;;; **********************************************************************
;;; Copyright (C) 2005 Todd Ingalls, Heinrich Taube
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; **********************************************************************


(in-package :cm)

       
(progn (defclass osc-stream (rt-stream)
         ((remote-port :initform nil :initarg :remote-port)
          (remote-host :initform nil :initarg :remote-host)
          (local-port :initform nil :initarg :local-port)
          (receive-data :initform (list nil nil) :accessor
           rt-stream-receive-data)
          (receive-mode :initform :message :initarg :receive-mode
           :accessor rt-stream-receive-mode)   ;; 
          (latency :initform 0.0 :initarg :latency)
          (buffer-size :initform 512 :initarg :buffer-size)
          (socket :initform nil))
         #+metaclasses
         (:metaclass io-class))
       (defparameter <osc-stream> (find-class 'osc-stream))
       (finalize-class <osc-stream>)
       (values))

(defmethod open-io ((obj osc-stream) dir &rest args)
  dir
  (let ((rh nil) (rp nil) (lp nil))
    (if (consp args)
        (progn (when (> (length args) 0) (setf rh (elt args 0)))
               (when (> (length args) 1) (setf rp (elt args 1)))
               (when (> (length args) 2) (setf lp (elt args 2)))))
    (unless (io-open obj)
      (setf (slot-value obj 'socket)
            (make-udp-socket (if rh rh (slot-value obj 'remote-host))
             (if rp rp (slot-value obj 'remote-port))
             (if lp lp (slot-value obj 'local-port))))
      (setf (slot-value obj 'open) t))
    (setf *out* obj)))

(defun osc-open (&rest args)
  (apply #'open-io "osc.udp" t args))

(defun osc-open? (&optional (osc (find-object "osc.udp")))
  (if osc
      (let ((io (io-open osc)))
	(if io 
	    osc
	    nil))))

(defmethod close-io ((obj osc-stream) &rest mode)
  mode
  (when (io-open obj)
    (udp-socket-close (slot-value obj 'socket))
    (setf (slot-value obj 'open) nil))
  (setf *out* nil))

(defun osc-close (&optional (osc (find-object "osc.udp")))
  (if (osc-open? osc)
      (cond ((receiver? osc)
             (error "osc-close: Can't close osc because a receiver is currently running."))
            (t (close-io osc ':force) osc))
      osc))

(defun send-osc (mess osc-stream len)
  (udp-socket-send (slot-value osc-stream 'socket) mess len))

(defmethod send-msg (message (io osc-stream))
  (multiple-value-bind (mess len)
      (format-osc message)
    (send-osc mess io len)))

(defmethod send-bundle (offset message (io osc-stream))
  (let ((arr (make-byte-vector "#bundle")) (mess-len 0))
    (setf arr (u8vector-append arr (make-osc-timetag offset io)))
    (if (listp (elt message 0))
        (progn (dolist (bundle-mess message)
                 (multiple-value-bind (mess len)
                     (format-osc bundle-mess)
                   (setf arr
                         (u8vector-append arr (make-byte-vector len)
                          mess))
                   (setf mess-len (+ mess-len len))))
               (setf mess-len (+ mess-len 24))
               (send-osc arr io mess-len))
        (multiple-value-bind (mess len)
            (format-osc message)
          (setf arr
                (u8vector-append arr (make-byte-vector len) mess))
          (setf mess-len (+ len 8 8 4))
          (send-osc arr io mess-len)))))

(defmethod set-receive-mode! ((str osc-stream) mode)
  (unless (member mode '(:message :raw))
    (error "receive: ~s is not a osc receive mode." mode))
  (setf (slot-value str 'receive-mode) mode))


;;;
#+openmcl
(defun u8vector->double (vec)
  (ccl::%stack-block ((d 8))
		     (dotimes (i 8)
		       (setf (ccl::%get-unsigned-byte d i)
			     (aref vec i)))
		     (ccl::%get-double-float d)))
  
#+sbcl 
(defun u8vector->double (vec)
  (let ((hb (u8vector->int (u8vector-subseq vec 0 4)))
	(lb (u8vector->int (u8vector-subseq vec 4 8))))
    (SB-KERNEL:MAKE-DOUBLE-FLOAT hb lb)))

(cffi:defcstruct timeval
  (tv-sec :long)
  (tv-usec :long))

(cffi:defcstruct timespec 
  (tv-sec  :long)
  (tv-nsec :long))

(cffi:defcfun ("gettimeofday" gettimeofday)
    :int
  (tp :pointer)
  (tzp :pointer))


#+openmcl
(defun get-current-time-of-day ()
  (ccl::rlet ((now :timeval))
    (#_gettimeofday now (ccl::%null-ptr))
    (+ (ccl::pref now :timeval.tv_sec)
       (/ (ccl::pref now :timeval.tv_usec)
          1000000.0D0))))
	
	
#+sbcl
(defun get-current-time-day ()
  (let ((x nil) (sec 0) (usec 0))
    (multiple-value-setq (x sec usec)
      (SB-UNIX:UNIX-GETTIMEOFDAY))
    (+ sec
       (/ usec
	  1000000.0D0))))

#|
(defun get-current-time-of-day ()
  (cffi:with-foreign-objects ((now 'timeval))
    (gettimeofday now (cffi:null-pointer))
    (+ (cffi:foreign-slot-value now 'timeval 'tv-sec)
       (/ (cffi:foreign-slot-value now 'timeval 'tv-usec)
          1000000.0D0))))
|#
(defun make-osc-timetag (offset out)
  (let ((vec nil)
	(target nil)
	(now (get-current-time-of-day)))
    (setf target (+ now (coerce offset 'double-float) (coerce (slot-value out 'latency) 'double-float)))
    (multiple-value-bind (secs fracs)
	(floor target)
      (setf vec (make-byte-vector (+ 2208988800 secs)))
      (u8vector-append vec (make-byte-vector (inexact->exact 
					      (* fracs
						 #xffffffff)))))))

;;;
;;; osc parsing
;;;
(defparameter *bundle-header-bytes* #(35 98 117 110 100 108 101 0))


(defun osc-bundle? (vec)
  (let ((res t))
    (if (arrayp vec)
	(dotimes (i 8)
	  (when (not (= (aref vec i) (aref *bundle-header-bytes* i)))
	    (setf res nil)
	    (return nil)))
      (setf res nil))
    res))

(defun osc-parse-timestamp (arr)
  (let ((ts nil))
    (setf ts (u8vector->uint (u8vector-subseq arr 0 4)))
    (setf ts (+ ts
             (exact->inexact
              (/ (u8vector->uint (u8vector-subseq arr 4 8))
                 4294967295))))))

(defun osc-parse-contents (arr)
  (let ((lst '())
	(mess nil)
	(pos nil)
	(sym-vector nil)
	(sym nil)
	(sym-len 0)
	(first-token-len nil)
	(type-list nil)
	(type-list-len nil))
    (setf first-token-len (position 0 arr))
    (if first-token-len
	(progn
	  (setf lst (append lst (list (intern (string-upcase (u8vector->string (subseq arr 0 first-token-len))) :cm))))
	  (setf mess (subseq arr (position 44 arr)))
	  (setf type-list (loop for i across (subseq mess 0 (position 0 mess))
				collect (code-char i)))
	  (setf type-list-len (length type-list))
	  (setf mess (subseq mess (+ type-list-len (- 4 (mod type-list-len 4)))))
	  (setf pos 0)
	  (dolist (j type-list)
	    (cond ((eq j #\i)
		   (setf lst (append lst (list (u8vector->int (subseq mess pos (+ pos 4))))))
		   (incf pos 4))
		  ((eq j #\f)
		   (setf lst (append lst (list (u8vector->float (subseq mess pos (+ pos 4))))))
		   (incf pos 4))
		  ((eq j #\d)
		   (setf lst (append lst (list (u8vector->double (subseq mess pos (+ pos 8))))))
		   (incf pos 8))
		  ((eq j #\s)
		   (if (= (u8vector->int (subseq mess pos)) 0)
		       (progn
			 (setf  pos (+ 4 pos))))
		   (setf sym-vector (subseq mess pos))
		   (setf sym-vector (subseq sym-vector 0 (position 0 sym-vector)))
		   (setf sym (coerce (loop for i across sym-vector collect (code-char i)) 'string))
		   (setf sym-len (length sym))
		   (setf lst (append lst (list sym)))
		   (if (= 0 (mod sym-len 4))
		       (setf pos (+ pos sym-len))
		       (setf pos (+ (+ sym-len (- 4 (mod sym-len 4))) pos)))))))
	(setf lst (append lst (list (string->symbol (u8vector->string arr))))))
    lst))

(defun osc-parse-message (arr)
  (osc-parse-contents arr))

(defun osc-parse-bundle (arr)
  (let ((msg-len nil)
	(pos 0)
	(arr-len (length arr))
	(bundle '()))
    (do ()
	((>= pos arr-len))
      (setf msg-len (u8vector->int (subseq arr pos (incf pos 4))))
      (setf bundle (append bundle (osc-parse-contents (subseq arr pos (+ pos msg-len)))))
      (incf pos msg-len))
    bundle))

(defun osc-vector->osc-message (arr)
  (let ((timestamp nil)
	(msg nil))
    (if (osc-bundle? arr)
	(progn 
	  (setf timestamp (osc-parse-timestamp (subseq arr 8 16)))
	  (setf msg (osc-parse-bundle (subseq arr 16))))
      (setf msg (osc-parse-contents arr)))
    (list msg timestamp)))

