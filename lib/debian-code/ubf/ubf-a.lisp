;; arch-tag: 7b6d2428-e297-11d7-ad53-000c76244c24

;; Written 2003-09-09 by Andreas Fuchs <asf@boinkor.net>, to celebrate
;; the end of the SOBIG.F crisis
;;
;; This code comes under the terms of the modified BSD license ("sans
;; advertising clause"). See the file COPYING for details.

(in-package #:ubf.a)

(defvar *recognition-stack* nil
  "The top of the recognition stack.")

(defvar *structure-stack* nil
  "Top of the (unspecified) structure stack.")

(defvar *registers* (make-hash-table :test 'eql)
  "Registers and their values")

(defconstant +control-chars+ "'\"~%-0123456789{}#&$>")
(defconstant +whitespace-chars+ '(#\Space #\Newline #\Return #\Tab #\,))

(defstruct tagged-object
  object
  type)

(deftype binary-data (&optional size)
  `(vector (unsigned-byte 8) ,size)) ; Type of 11,~binary data~

(defun read-until-char (delim-char stream)
  "Read from STREAM until you hit DELIM-CHAR and consume the
DELIM-CHAR."
  (declare (type base-char delim-char))
  (prog1
      (read-until (lambda (c) (eql delim-char c))
		  stream)
    (read-char stream)))

(defun read-until (predicate stream)
  "Read from STREAM until PREDICATE returns non-NIL. The last
character read (i.e. the one that triggered PREDICATE) is made
unread again."
  (coerce (loop for c = (read-char stream)
	     until (funcall predicate c)
	     collect c
	     finally (unread-char c stream))
	  'simple-string))

(defun push-recognized (elt)
  "Push ELT into the recognition stack and increment the current
structure count as necessary."
  (push elt *recognition-stack*)
  (when *structure-stack*
    (incf (car *structure-stack*)))
  elt)

(defun pop-recognized ()
  "Pop the recognition stack, decrement structure counts as necessary
and return the element popped."
  (prog1
      (pop *recognition-stack*)
    (when *structure-stack*
      (decf (car *structure-stack*)))))

(defun tag-object (object type)
  "Make a tagged object and return it. TYPE gets interned
case-insensitively in the UBF.A-TYPE package."
  (make-tagged-object :object object :type (intern type "UBF.A-TYPE")))

(defun read-token (&optional (stream *standard-input*))
  "Read a UBF token from STREAM and put it on the recognition
stack. May pop from the stack too, e.g. when reading. "
  (let ((c (read-char stream))
	(end-p nil))
    (loop while (position c +whitespace-chars+)
	  do (setf c (read-char stream)))
    (values
     (cond
       ((eql c #\#)
	(push-recognized nil))
       ((eql c #\&)
	(push-recognized
	 (cons (pop-recognized) (pop-recognized))))
       ((eql c #\')
	(push-recognized
	 (intern (read-until-char #\' stream)
		 "UBF.A-CONSTANT")))
       ((eql c #\`)
	(push-recognized
	 (tag-object (pop-recognized)
		     (read-until-char #\` stream))))
       ((eql c #\")
	(push-recognized (read-until-char #\" stream)))
       ((eql c #\%)
	(read-until-char #\% stream)
	(read-token stream))
       ((eql c #\$)
	(assert (= 1 (length *recognition-stack*)))
	(setf end-p t)
	(pop-recognized))
       ((eql c #\-)
	(read-token stream)
	(push-recognized (- 0 (pop-recognized))))
       ((eql c #\~)
	(let* ((length (first *recognition-stack*))
	       (sequence (make-array length :initial-element 0
				     :element-type '(unsigned-byte 8))))
	  (assert (and (integerp length)
		       (> length 0)))
	  (pop-recognized)
	  (assert (= (read-sequence sequence stream) length))
	  (assert (eql (read-char stream) #\~))
	  (push-recognized sequence)))
       ((eql c #\>)
	(assert (not (position (peek-char nil stream) +control-chars+)))
	(setf (gethash (read-char stream) *registers*) (pop-recognized)))
       ((eql c #\{)
	(push 0 *structure-stack*))
       ((eql c #\})
	(let ((struct-idx (pop *structure-stack*))
	      (rs-copy *recognition-stack*))
	  (setf *recognition-stack* (subseq rs-copy struct-idx))
	  (push-recognized
	   (make-array struct-idx :initial-contents
		       (nreverse (subseq rs-copy 0 struct-idx))))))
       ((digit-char-p c)
	(unread-char c stream)
	(push-recognized (parse-integer (read-until (lambda (c) (not (digit-char-p c)))
						    stream))))
       ((not (position c +control-chars+))
	(multiple-value-bind (val register-bound) (gethash c *registers*)
	  (assert register-bound)
	  (push-recognized val))))
     end-p)))

(defun read-message (stream)
  "Read an entire $-terminated message from STREAM."
  (let ((*registers* (make-hash-table :test 'eql))
	(*structure-stack* nil)
	(*recognition-stack* nil))
    (loop
       (multiple-value-bind (val end) 
	   (read-token stream)
	 ;; (format t "~A~%" val)
	 (when end
	   (return val))))))

;;; sending UBF data

(defmacro with-enclosing-char ((stream char) &body body)
  "Execute BODY and write CHAR to STREAM before and afterwards. Yields
 (+2 (progn ,@body)), so better make sure it returns a number."
  `(+ 2 (prog1
	    (progn (write-char ,char ,stream)
		   ,@body)
	  (write-char ,char ,stream))))

(defun write-stream (in-stream ubf-stream &optional length)
  "Write the contents of a IN-STREAM as UBF-encoded binary data to
UBF-STREAM.  If LENGTH is given, write exactly LENGTH octets into
UBF-STREAM."
  (let ((+buffer-length+ 1024))	; pseudo-constant, I know
    (cond
      (length
       (let ((buf (make-array +buffer-length+ :element-type '(unsigned-byte 8))))
	 (+ (write-message length ubf-stream :terminate nil)
	    ;; zero-copy is probably better if the os/impl supports it.
	    (with-enclosing-char (ubf-stream #\~)
	      (loop for read = (read-sequence buf in-stream)
		 sum read into total-length
		 do (write-sequence buf ubf-stream :end (if (>= total-length length)
							    (- read (- total-length length))
							    read))
		 until (or (< read +buffer-length+)
			   (>= total-length length)))
	      length))))
      (t
       ;; unknown length of the stream. cons madly to find out the
       ;; lengh
       (multiple-value-bind (total-length buffers last-read)
	   ;; count length and queue up buffers
	   (loop for buf = (make-array +buffer-length+ :element-type '(unsigned-byte 8))
	      for last-read = (read-sequence buf in-stream)
	      sum last-read into total-length
	      collect buf into buffers
	      if (< last-read +buffer-length+)
	      return (values total-length buffers last-read))
	 (+ (write-message total-length ubf-stream :terminate nil)
	    (with-enclosing-char (ubf-stream #\~)
	      (loop for buf in buffers
		 for next-buf on buffers
		 if (cdr next-buf)
		 do (write-sequence buf ubf-stream)
		 else
		 do (write-sequence buf ubf-stream :start 0 :end last-read))
	      total-length)))))))

;; FIXME: does this return the correct number for multibyte strings?
(defun write-message (object stream &key (terminate t))
  "UBF(A)-encode OBJECT and write it to STREAM. If TERMINATE is nil,
don't encode and write a #\$ character. Returns the length in bytes of
the data that were written."
  (+ 
   (cond
     ((null object)
      (write-char #\# stream)
      1)
     ((typep object 'binary-data)
      (+ (write-message (length object) stream :terminate nil)
	 (with-enclosing-char (stream #\~)
	   (length (write-sequence object stream)))))
     ((stringp object)
      (with-enclosing-char (stream #\")
	(length (write-string object stream))))
     ((vectorp object)
      (write-char #\{ stream)
      (+ 2
	 (prog1 (loop for elt across object
		   summing (write-message elt stream :terminate nil))
	   (write-char #\} stream))))
     ((consp object)
      (+ 2
	 (prog1 (write-message (cdr object) stream :terminate nil)
	   (write-char #\, stream))
	 (prog1 (write-message (car object) stream :terminate nil)
	   (write-char #\& stream))))
     ((tagged-object-p object)
      (+ (write-message (tagged-object-object object) stream :terminate nil)
	 (with-enclosing-char (stream #\`)
	   (length (write-string (symbol-name (tagged-object-type object)) stream)))))
     ((symbolp object)
      (with-enclosing-char (stream #\')
	(length (write-string (symbol-name object) stream))))
     ((integerp object)
      (write-char #\Space stream)
      (1+ (length (write-string (prin1-to-string object) stream))))
     ((typep object 'pathname)
      (with-open-file (f object :direction :input :element-type '(unsigned-byte 8))
	(write-stream f stream (file-length f))))
     ((streamp object) ; we destructively modify the stream state. hope that's ok.
      (write-stream object stream))
     (t
      (error 'simple-error
	     :format-control "dunno what I should do with ~S"
	     :format-arguments `(,object))))
   (if terminate
       (progn
	 (write-char #\$ stream)
	 1)
       0)))