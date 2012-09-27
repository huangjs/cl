(in-package :zip)

(defun default-external-format ()
  :utf-8)

(defun octets-to-string (octets ef)
  (with-output-to-string (out)
    (flexi-streams:with-input-from-sequence (in octets)
      (let ((in* (flexi-streams:make-flexi-stream in :external-format ef)))
	(loop
	    for c = (read-char in* nil nil)
	    while c
	    do (write-char c out))))))

(defun string-to-octets (string ef)
  (flexi-streams:with-output-to-sequence (out)
    (with-input-from-string (in string)
      (let ((out* (flexi-streams:make-flexi-stream out :external-format ef)))
	(loop
	    for c = (read-char in nil nil)
	    while c
	    do (write-char c out*))))))

(defclass buffer-output-stream
    (trivial-gray-stream-mixin fundamental-binary-output-stream)
    ((buf :initarg :buf :accessor buf)
     (pos :initform 0 :accessor pos)))

;; fallback method just in case the lisp doesn't have or doesn't use
;; stream-write-sequence:
(defmethod stream-write-byte
    ((stream buffer-output-stream) byte)
  (stream-write-sequence stream (vector byte) 0 1)
  byte)

(defmethod stream-write-sequence
    ((stream buffer-output-stream) seq start end &key)
  (replace (buf stream)
	   seq
	   :start1 (pos stream)
	   :start2 start
	   :end2 end)
  (incf (pos stream) (- end start))
  seq)

(defun make-buffer-output-stream (outbuf)
  (make-instance 'buffer-output-stream :buf outbuf))

(defclass truncating-stream
    (trivial-gray-stream-mixin fundamental-binary-input-stream)
    ((input-handle :initarg :input-handle :accessor input-handle)
     (size :initarg :size :accessor size)
     (pos :initform 0 :accessor pos)))

(defmethod stream-read-byte ((s truncating-stream))
  (if (< (pos s) (size s))
      (prog1
	  (read-byte (input-handle s))
	(incf (pos s)))
      nil))

(defmethod stream-read-sequence ((s truncating-stream) seq start end &key)
  (let* ((n (- end start))
        (max (- (size s) (pos s)))
        (result
         (read-sequence seq
                        (input-handle s)
                        :start start
                        :end (+ start (min n max)))))
    (incf (pos s) (- result start))
    result))
