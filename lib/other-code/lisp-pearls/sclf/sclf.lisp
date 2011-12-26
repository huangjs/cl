;;;  sclf.lisp --- miscellanea

;;;  Copyright (C) 2005, 2006, 2007 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: SCLF

#+cmu (ext:file-comment "$Module: sclf.lisp, Time-stamp: <2007-03-21 20:44:16 wcp> $")

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA

;;;  Commentary:

;;; This is a collection of Common Lisp functions of the most disparate
;;; uses and purposes.  These functions are too small or too unrelated
;;; to each other to deserve an own module.  AKA "Misc Utils".
;;;
;;; If you want to indent properly the following macros you should add
;;; the following lines to your .emacs file:
;;;
;;; (defun cl-indent-be (path state indent-point sexp-column normal-indent)
;;;   (let ((sexp-start (cadr state))
;;; 	(i 0))
;;;     (save-excursion
;;;       (goto-char sexp-start)
;;;       (forward-char)
;;;       (+ sexp-column
;;; 	 (block indentation
;;; 	   (condition-case nil
;;; 	       (while (< (point) indent-point)
;;; 		 (setq i (1+ i))
;;; 		 (when (and (= 0 (logand i 1))
;;; 			    (looking-at "[\t\n ]*\\s("))
;;; 		   (return-from indentation 2))
;;; 		 (forward-sexp))
;;; 	     (error nil))
;;; 	   (if (= 1 (logand i 1))
;;; 	       6 4))))))
;;;
;;; (put 'be 'common-lisp-indent-function 'cl-indent-be)
;;; (put 'be* 'common-lisp-indent-function 'cl-indent-be)
;;; (put 'awhen 'lisp-indent-function 1)
;;; (put 'gcase 'lisp-indent-function 1)
;;; (put 'acase 'lisp-indent-function 1)
;;; (put 'acond 'lisp-indent-function 1)
;;; (put 'until 'lisp-indent-function 1)



(cl:in-package :sclf)

(defvar *bourne-shell* "/bin/sh")

(defmacro be (&rest bindings-and-body)
  "Less-parenthetic let."
  (let ((bindings
	 (loop
	    while (and (symbolp (car bindings-and-body))
		       (cdr bindings-and-body))
	    collect (list (pop bindings-and-body)
			  (pop bindings-and-body)))))
    `(let ,bindings
       ,@bindings-and-body)))

(defmacro be* (&rest bindings-and-body)
  "Less-parenthetic let*."
  (let ((bindings
	 (loop
	    while (and (symbolp (car bindings-and-body))
		       (cdr bindings-and-body))
	    collect (list (pop bindings-and-body)
			  (pop bindings-and-body)))))
    `(let* ,bindings
       ,@bindings-and-body)))

(defmacro with-gensyms ((&rest symbols) &body body)
  "Gensym all SYMBOLS and make them available in BODY.
See also LET-GENSYMS."
  `(let ,(mapcar #'(lambda (s)
		     (list s '(gensym))) symbols)
     ,@body))

(defun s+ (&rest strings)
  (apply #'concatenate 'string strings))

(defun string-starts-with (prefix string &optional (compare #'string=))
  (be prefix-length (length prefix)
    (and (>= (length string) prefix-length)
	 (funcall compare prefix string :end2 prefix-length))))

(defun string-ends-with (postfix string &optional (compare #'string=))
  (let ((postfix-length (length postfix))
	(string-length (length string)))
    (and (>= string-length postfix-length)
	 (funcall compare postfix string :start2 (- string-length postfix-length)))))

(defun substitute-sequence (from to sequence &key (start 0) end (test #'eql))
  "Replace occurrences of FROM in SEQUENCE with TO.  FROM and TO
don't need to be the same length."
  (be from-length (length from)
      seqtype (type-of sequence)
      output-sequence (subseq sequence 0 start)
    ;; If the type specifier is a list, get rid of the extra
    ;; information as it may include even the size of the sequence,
    ;; which would be incompatible with this function does.
    (when (consp seqtype)
      (setf seqtype (car seqtype)))
    (loop
       for position = (search from sequence :start2 start :end2 end :test test)
       while position
       do
	 (setf output-sequence
	       (concatenate seqtype
			    output-sequence
			    (subseq sequence start position)
			    to))
	 (setf start (+ position from-length))
       finally (return
		 (concatenate seqtype output-sequence
			      (subseq sequence start))))))

(defun string-escape (string character &optional (escape-character #\\))
  "Prepend all occurences of CHARACTER in STRING with a
ESCAPE-CHARACTER."
  (with-output-to-string (stream)
    (loop
       for c across string
       when (char= c character)
       do (write-char escape-character stream)
       do (write-char c stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro aif (test then &optional else)
  `(be it ,test
     (if it
	 ,then
	 ,else)))

(defmacro awhen (test &body then)
  `(be it ,test
     (when it
       ,@then)))

(defmacro acond (&body forms)
  (when forms
    `(aif ,(caar forms)
	  (progn ,@(cdar forms))
	  (acond ,@(cdr forms)))))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acase (condition &body forms)
  `(be it ,condition
     (case it ,@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +whitespace+ '(#\return #\newline #\tab #\space #\page))

(defun string-trim-whitespace (string)
  (string-trim +whitespace+ string))

(defun string-right-trim-whitespace (string)
  (string-right-trim +whitespace+ string))

(defun string-left-trim-whitespace (string)
  (string-left-trim +whitespace+ string))

(defun whitespace-p (char)
  (member char +whitespace+))

(defun position-any (bag sequence &rest position-args)
  "Find any element of bag in sequence and return its position.
Accept any argument accepted by the POSITION function."
  (apply #'position-if #'(lambda (element)
			   (find element bag)) sequence position-args))

(defun find-any (bag sequence &rest find-args)
  "Find any element of bag in sequence.  Accept any argument
accepted by the FIND function."
  (apply #'find-if #'(lambda (element)
			   (find element bag)) sequence find-args))

(defun split-at (bag sequence)
  "Split SEQUENCE at occurence of any element from BAG.
Contiguous occurences of elements from BAG are considered atomic;
so no empty sequence is returned."
  (be len (length sequence)
    (labels ((split-from (start)
	       (unless (>= start len)
		 (be sep (position-any bag sequence :start start)
		   (cond ((not sep)
			  (list (subseq sequence start)))
			 ((> sep start)
			  (cons (subseq sequence start sep)
				(split-from (1+ sep))))
			 (:else
			  (split-from (1+ start))))))))
      (split-from 0))))

(defun split-string-at-char (string separator &key escape skip-empty)
  "Split STRING at SEPARATORs and return a list of the substrings.  If
SKIP-EMPTY is true then filter out the empty substrings.  If ESCAPE is
not nil then split at SEPARATOR only if it's not preceded by ESCAPE."
  (declare (type string string) (type character separator))
  (labels ((next-separator (beg)
             (be pos (position separator string :start beg)
	       (if (and escape
			pos
			(plusp pos)
			(char= escape (char string (1- pos))))
		   (next-separator (1+ pos))
		   pos)))
           (parse (beg)
             (cond ((< beg (length string))
                    (let* ((end (next-separator beg))
                           (substring (subseq string beg end)))
                      (cond ((and skip-empty (string= "" substring))
                             (parse (1+ end)))
                            ((not end)
                             (list substring))
                            (:else
			     (cons substring (parse (1+ end)))))))
                   (skip-empty
		    '())
                   (:else
		    (list "")))))
    (parse 0)))

(defun copy-stream (in out)
  (loop
     for c = (read-char in nil)
     while c
     do (write-char c out)))

(defun pathname-as-file (pathname)
  "Converts PATHNAME to file form and return it."
  (setf pathname (pathname pathname))
  (cond ((pathname-name pathname)
	 pathname)
	((stringp (car (last (pathname-directory pathname))))
	 (be name (parse-namestring (car (last (pathname-directory pathname))))
	   (make-pathname :directory (butlast (pathname-directory pathname))
			  :name (pathname-name name)
			  :type (pathname-type name)
			  :defaults pathname)))
	;; it can't be done?
	(t pathname)))

(defun copy-file (file copy-file &key (if-exists :error))
  (with-open-file (in file)
    (with-open-file (out copy-file :direction :output :if-exists if-exists)
      (copy-stream in out))))

(defun read-whole-stream (stream)
  "Read stream until the end and return it as a string."
  (with-output-to-string (string)
    (loop
       for line = (read-line stream nil)
       while line
       do (write-line line string))))

(defun read-file (pathname)
  "Read the whole content of file and return it as a string."
  (with-open-file (in pathname)
    (read-whole-stream in)))

(defun read-binary-file (pathname)
  "Read the whole content of file and return it as a vector."
  (be vector (make-array 10 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
    (with-open-file (in pathname :element-type '(unsigned-byte 8))
      (loop
	 for b = (read-byte in nil)
	 while b
	 do (vector-push-extend b vector)))
    vector))

(defun string-concat (list &optional (separator ""))
  "Concatenate the strings in LIST interposing SEPARATOR (default
a space) between them."
  (reduce #'(lambda (&rest args)
	      (if args
		  (s+ (car args) separator (cadr args))
		  ""))
	  list))

;; to indent it properly: (put 'gcase 'lisp-indent-function 1)
(defmacro gcase ((value &optional (test 'equalp)) &rest cases)
  "Generic CASE macro.  Match VALUE to CASES as if by the normal
CASE but use TEST as the comparison function (default EQUALP)."
  (with-gensyms (val)
    `(be ,val ,value
       ,(cons 'cond
	      (mapcar #'(lambda (case-desc)
			  (destructuring-bind (vals &rest forms) case-desc
			    `(,(cond ((consp vals)
				      (cons 'or (mapcar #'(lambda (v)
							    (list test val v))
							vals)))
				     ((or (eq vals 'otherwise)
					  (eq vals t))
				      t)
				     (t (list test val vals)))
			       ,@forms)))
		      cases)))))

(defun string-truncate (string max-length)
  "If STRING is longer than MAX-LENGTH, return a shorter version.
Otherwise return the same string unchanged."
  (if (> (length string) max-length)
      (subseq string 0 max-length)
      string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lazy primitives
;;;

(defstruct promise
  procedure
  value)

(defmacro lazy (form)
  `(make-promise :procedure #'(lambda () ,form)))


(defun force (promise)
  (if (promise-procedure promise)
      (prog1 (setf (promise-value promise)
		   (funcall (promise-procedure promise)))
	(setf (promise-procedure promise) nil))
      (promise-value promise)))

(defmacro deflazy (name value &optional documentation)
  `(defparameter ,name (lazy ,value)
     ,@(when documentation
	     (list documentation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to indent properly: (put 'until 'lisp-indent-function 1)
(defmacro until (test &body body)
  `(loop
      until ,test
      do (progn ,@body)))

(defun keywordify (string)
  (intern (string-upcase string) :keyword))

(defun run-pipe (direction fmt &rest args)
  "Run command made formatted by FMT and ARGS and according to
DIRECTION return the input and output streams and process object
of that process."
  #+cmu
  (be process (ext:run-program *bourne-shell* (list "-c" (apply #'format nil fmt args))
			       :wait nil
			       :pty nil
			       :input (when (member direction '(:output :input-output :io))
					:stream)
			       :output (when (member direction '(:input :input-output :io))
					 :stream)
			       :error nil)
    (values (ext:process-output process)
	    (ext:process-input process)
	    process))
  #+sbcl
  (be process (sb-ext:run-program *bourne-shell* (list "-c" (apply #'format nil fmt args))
				  :wait nil
				  :pty nil
				  :input (when (member direction '(:output :input-output :io))
					   :stream)
				  :output (when (member direction '(:input :input-output :io))
					    :stream)
				  :error nil)
    (values (sb-ext:process-output process)
	    (sb-ext:process-input process)
	    process))
  #+clisp
  (ext:make-pipe-input-stream
   (format nil "~A -c ~S" *bourne-shell*
	   (string-escape (string-escape (apply #'format nil fmt args) #\\) #\")))
  #-(or sbcl cmu clisp)
  (error "Unsupported Lisp system."))

(defun exit-code (process)
  #+cmu
  (progn (ext:process-wait process)
	 (ext:process-exit-code process))
  #+sbcl
  (progn (sb-ext:process-wait process)
	 (sb-ext:process-exit-code process))
  #-(or cmu sbcl)
  (error "exit-code primitive unsupported under this Lisp implementation"))

(defun locate-system-program (name)
  "Given the NAME of a system program try to find it through the
search of the environment variable PATH.  Return the full
pathname."
  (loop
     for dir in (split-string-at-char (getenv "PATH") #\:)
     for pathname = (merge-pathnames name (pathname-as-directory dir))
     when (probe-file pathname)
     return pathname))

(defun run-system-program (program &rest args)
  "Run PROGRAM system program with arguments ARGS.  Return the
exit status of the program."
  (flet ((stringify (items)
	   (mapcar #'(lambda (item)
		       (if (stringp item)
			   item
			   (format nil "~A" item)))
		   items)))
    #+cmu
    (be process (ext:run-program program (stringify args)
				 :wait t
				 :pty nil
				 :input nil
				 :output nil)
      (ext:process-exit-code process))
    #+sbcl
    (be process (sb-ext:run-program program (stringify args)
				    :wait t
				    :pty nil
				    :input nil
				    :output nil)
      (sb-ext:process-exit-code process))
    #+clisp
    (apply #'ext:execute (if (pathname-directory program)
			     program
			     (locate-system-program program))
	   (stringify args))
    #-(or cmu sbcl clisp)
    (error "Unsupported Lisp system.")))

(defun run-shell-command (fmt &rest args)
  "Run a Bourne Shell command."
  (run-system-program *bourne-shell* "-c" (apply #'format nil fmt args)))

(defmacro with-open-pipe ((in out program &rest args) &body body)
  (let ((direction (cond ((not in) :output)
			 ((and in out) :input-output)
			 (t :input)))
	(invar (or in (gensym)))
	(outvar (or out (gensym))))
    `(multiple-value-bind (,invar ,outvar) (run-pipe ,direction ,program ,@args)
       (declare (ignorable ,invar ,outvar))
       (unwind-protect (progn ,@body)
	 (when ,invar
	   (close ,invar))
	 (when ,outvar
	   (close ,outvar))))))

(defvar *tmp-file-defaults* #P"/tmp/")

(defun temp-file-name (&optional (default *tmp-file-defaults*))
  "Create a random pathname based on DEFAULT.  No effort is made
to make sure that the returned pathname doesn't identify an
already existing file.  If missing DEFAULT defaults to
*TMP-FILE-DEFAULTS*."
  (make-pathname :defaults default
		 :name (format nil "~36R" (random #.(expt 36 10)))))

(defun open-temp-file (&optional default-pathname &rest open-args)
  "Open a new temporary file and return a stream to it.  This
function makes sure the pathname of the temporary file is unique.
OPEN-ARGS are arguments passed verbatim to OPEN.  If
DEFAULT-PATHNAME is specified and not NIL it's used as defaults
to produce the pathname of the temporary file."
  (unless default-pathname
    (setf default-pathname *tmp-file-defaults*))
  (do* ((name #1=(temp-file-name default-pathname) #1#)
	(stream #2=(apply #'open name :direction :output :if-exists nil
			  :if-does-not-exist :create open-args) #2#))
       (stream stream)))

(defmacro with-temp-file ((stream &rest open-temp-args) &body body)
  "Execute BODY within a dynamic extent where STREAM is bound to
a STREAM open on a unique temporary file name.  OPEN-TEMP-ARGS are
passed verbatim to OPEN-TEMP-FILE."
  `(be ,stream (open-temp-file ,@open-temp-args)
     (unwind-protect
	  (progn ,@body)
       (close ,stream)
       ;; body may decide to rename the file so we must ignore the errors
       (ignore-errors
	 (delete-file (pathname ,stream))))))

(defmacro with-hidden-temp-file ((stream &rest open-args) &body body)
  "Just like WITH-TEMP-FILE but unlink (delete) the temporary
file before the execution of BODY.  As such BODY won't be able to
manipulate the file but through STREAM, and not other program is
able to see it.  Once STREAM is closed the temporary file blocks
are automatically relinquished by the operating system.  This
works at least on Unix filesystems.  I don't know about MS-OSs
where the system may likely decide to crash, take all your data
with it and, in the meanwhile, report you to the NSA as
terrorist."
  `(be ,stream (open-temp-file ,@open-args)
     (unwind-protect
	  (progn (delete-file (pathname ,stream))
		 ,@body)
       (close ,stream))))

(defun insert-in-order (item seq &key (test #'<) key)
  "Destructively insert ITEM in LIST in order by TEST.  Return
the new list.  This is a simple wrapper around MERGE."
  (merge (type-of seq) (list item) seq test :key key))

;; CL is a bit idiosyncratic in this respect
(defun file-size (pathname)
  (with-open-file (stream pathname)
    (file-length stream)))

(defun getenv (var)
  "Return the string associate to VAR in the system environment."
  #+cmu (cdr (assoc (if (symbolp var)
			var
			(intern var :keyword))
		    ext:*environment-list*))
  #+sbcl (sb-ext:posix-getenv (string var))
  #+lispworks (hcl:getenv var)
  #+clisp (ext:getenv (string var))
  #-(or cmu sbcl lispworks clisp)
  (error "GETENV not implemented for your Lisp system."))

(defun soundex (word &optional (key-length 4))
  "Knuth's Soundex algorithm.  Returns a string representing the
sound of a certain word (English).  Different words will thus
yield the same output string.  To compare two string by the
sound, simply do:

   (string= (soundex str1) (soundex str2))

Examples:

   (soundex \"Knuth\") => \"K530\"
   (soundex \"Kant\") => \"K530\"
   (soundex \"Lloyd\") => \"L300\"
   (soundex \"Ladd\") => \"L300\""
  (declare (type string word))
  (flet ((translate-char (char)
	   (awhen (position char "BFPVCGJKQSXZDTLMNR")
	     (elt "111122222222334556" it))))
    (let ((key (make-string key-length :initial-element #\0))
	  (word-length (length word)))
      (setf (elt key 0) (elt word 0))
      (loop
	 with previous-sound = (translate-char (char-upcase (elt word 0)))
	 with j = 1
	 for i from 1 by 1 below word-length
	 for c = (char-upcase (elt word i))
	 while (< j key-length)
	 do (be sound (translate-char c)
	      (cond ((not (eq sound previous-sound))
		     (unless (member c '(#\H #\W))
		       (setf previous-sound sound))
		     (when sound
		       (setf (elt key j) sound)
		       (incf j))))))
      key)))

(defun string-soundex= (string1 string2)
  (let ((l1 (split-at +whitespace+ string1))
	(l2 (split-at +whitespace+ string2)))
    (and (= (length l1) (length l2))
	 (every #'string= (mapcar #'soundex l1) (mapcar #'soundex l2)))))

#+nil
(defun soundex-test ()
  (let* ((words1 '("Euler" "Gauss" "Hilbert" "Knuth" "Lloyd" "Lukasiewicz" "Wachs"))
	 (words2 '("Ellery" "Ghosh" "Heilbronn" "Kant" "Ladd" "Lissajous" "Waugh"))
	 (results '("E460" "G200" "H416" "K530" "L300" "L222" "W200")))
    (mapc #'(lambda (w1 w2 r)
	      (let ((r1 (soundex w1))
		    (r2 (soundex w2)))
		(format t "~A = ~A, ~A = ~A => ~A~%" w1 r1 w2 r2
			(if (and (string= r1 r2)
				 (string= r r1))
			    "OK"
			    (format nil "ERROR (expected ~A)" r)))))
	  words1 words2 results)
    (values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defstruct cache-slot ()
;;   ((previous :type (or cache-slot null)
;; 	     :initarg :previous
;; 	     :initform nil
;; 	     :accessor cslot-previous)
;;    (key :initarg :key
;; 	:accessor cslot-key)
;;    (value :initarg :value
;; 	  :accessor cslot-value)
;;    (next :type (or cache-slot null)
;; 	 :initarg :next
;; 	 :initform nil
;; 	 :accessor cslot-next)))

;; (defmethod print-object ((object cache-slot) stream)
;;   (print-unreadable-object (object stream :type t)
;;     (if (slot-boundp object 'key)
;; 	(format stream "key=~S, value=~S" (cslot-key object) (cslot-value object))
;; 	(format stream "NULL"))))


(defstruct (double-linked-element (:conc-name dle-))
  (previous nil :type (or double-linked-element null))
  value
  (next nil :type (or double-linked-element null)))

(defmethod print-object ((object double-linked-element) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "value=~S" (dle-value object))))

(defun cons-dle (value previous next)
  (declare (type (or double-linked-element null) previous next))
  (be new-element (make-double-linked-element :previous previous :next next :value value)
    (when previous
      (setf (dle-next previous) new-element))
    (when next
      (setf (dle-previous next) new-element))
    new-element))

(defun dle-remove (dle-object)
  "Remove the DLE-OBJECT from its current position in the list of
elements agjusting the pointer of dle-objects before and after this
one (if any)."
  (declare (type double-linked-element dle-object))
  (awhen (dle-next dle-object)
    (setf (dle-previous it) (dle-previous dle-object)))
  (awhen (dle-previous dle-object)
    (setf (dle-next it) (dle-next dle-object))))

(defun dle-map (function dle-object)
  (when dle-object
    (make-double-linked-element :value (funcall function (dle-value dle-object))
				:previous (dle-previous dle-object)
				:next (dle-map function (dle-next dle-object)))))

(defmacro do-dle ((var dle &optional (result nil)) &body body)
  "Iterate over a list of DOUBLE-LINKED-ELEMENTs and map body to
each element's value.  Bind VAR to the value on each iteration."
  (be cursor (gensym)
    `(do ((,cursor ,dle (dle-next ,cursor)))
	 ((not ,cursor) ,result)
       (be ,var (dle-value ,cursor)
	 ,@body))))

(defmacro do-dle* ((var dle &optional (result nil)) &body body)
  "Same as DO-DLE but VAR is a symbol macro, so that BODY can
modify the element's value."
  (be cursor (gensym)
    `(symbol-macrolet ((,var (dle-value ,cursor)))
       (do ((,cursor ,dle (dle-next ,cursor)))
	   ((not ,cursor) ,result)
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass double-linked-list ()
  ((elements :type double-linked-element
	     :documentation "The actual list of elements held by this object.")
   (last-element :type double-linked-element))
  (:documentation
   "A double linked list where elements can be added or removed
from either end."))

(defmethod initialize-instance ((object double-linked-list) &rest rest)
  (declare (ignorable rest))
  (call-next-method)
  (with-slots (last-element elements) object
    (setf last-element (make-double-linked-element)
	  elements last-element)))

(defmethod print-object ((object double-linked-list) stream)
  (print-unreadable-object (object stream :type t)
    (be elements '()
      (do-dle (e (slot-value object 'elements))
	(push e elements))
      (format stream "elements=~S" (nreverse elements)))))

(defgeneric pop-first (double-linked-list)
  (:documentation
   "Pop the first element of a double-linked-list."))
(defgeneric pop-last (double-linked-list)
  (:documentation
   "Pop the last element of a double-linked-list."))
(defgeneric push-first (item double-linked-list)
  (:documentation
   "Push an item in front of a double-linked-list."))
(defgeneric push-last (item double-linked-list)
  (:documentation
   "Append an item to a double-linked-list."))
(defgeneric list-map (function double-linked-list)
  (:documentation
   "Map a function to a double-linked-list."))

(defmethod pop-last ((list double-linked-list))
  "Drop the last element in the sorted list."
  (with-slots (last-element) list
    (awhen (dle-previous last-element)
      (dle-remove it)
      (dle-value it))))

(defmethod pop-first ((list double-linked-list))
  "Drop the first element in the sorted list."
  (with-slots (elements) list
    (when (dle-next elements)
      (prog1 (dle-value elements)
	(setf (dle-previous (dle-next elements)) nil
	      elements (dle-next elements))))))

(defmethod push-first (value (list double-linked-list))
  (with-slots (elements) list
    (setf elements (cons-dle value nil elements)))
  list)

(defmethod push-last (value (list double-linked-list))
  (with-slots (last-element) list
    (cons-dle value (dle-previous last-element) last-element))
  list)

(defmethod list-map (function (list double-linked-list))
  (labels ((map-dll (dle)
	     (when (dle-next dle)
	       (make-double-linked-element
		:value (funcall function (dle-value dle))
		:previous (dle-previous dle)
		:next (map-dll (dle-next dle))))))
    (map-dll (slot-value list 'elements))))


(defmethod dll-find-cursor (object (list double-linked-list) &key (test #'eql) (key #'identity))
  (do ((cursor (slot-value list 'elements) (dle-next cursor)))
      ((not (dle-next cursor)))
    (be value (dle-value cursor)
      (when (funcall test (funcall key value) object)
	(return cursor)))))

(defmethod dll-find (object (list double-linked-list) &key (test #'eql) (key #'identity))
  (awhen (dll-find-cursor object list :test test :key key)
    (dle-value it)))

(defmethod dll-remove ((cursor double-linked-element) (list double-linked-list))
  (with-slots (elements) list
    (if (dle-previous cursor)
	(dle-remove cursor)
	(setf (dle-previous (dle-next elements)) nil
	      elements (dle-next elements))))
  list)

(defmacro do-dll ((var list &optional (result nil)) &body body)
  "Iterate over a sorted-list and map body to each element's
value.  Bind VAR to the value on each iteration."
  (be cursor (gensym)
    `(do ((,cursor (slot-value ,list 'elements) (dle-next ,cursor)))
	 ((not (dle-next ,cursor)) ,result)
       (be ,var (dle-value ,cursor)
	 ,@body))))

(defmacro do-dll* ((var list &optional (result nil)) &body body)
  "Same as DO-DLL but VAR is a symbol macro, so that BODY can
modify the element's value."
  (be cursor (gensym)
    `(symbol-macrolet ((,var (dle-value ,cursor)))
       (do ((,cursor (slot-value ,list 'elements) (dle-next ,cursor)))
	   ((not (dle-next ,cursor)) ,result)
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass limited-list (double-linked-list)
  ((max-size :initform nil
	     :initarg :size
	     :reader max-size
	     :type (or integer null)
	     :documentation "Size limit to which the list is allowed to grow to.  NIL = no limit.")
   (size :initform 0
	 :reader size
	 :type integer
	 :documentation "Current number of elements in the list."))
  (:documentation
   "A double linked list where the maximum number of elements can
be limited."))

(defun dll-member-p (dle list)
  (with-slots (elements size) list
    (do ((e elements (dle-next e)))
	((not e))
      (when (eq e dle)
	(return t)))))

(defmethod dll-remove ((cursor double-linked-element) (list limited-list))
  (with-slots (size) list
    (unless (zerop size)
      (decf size)
      (call-next-method)))
  list)

(defmethod pop-first ((list limited-list))
  (with-slots (size) list
    (unless (zerop size)
      (decf size)
      (call-next-method))))

(defmethod pop-last ((list limited-list))
  (with-slots (size) list
    (unless (zerop size)
      (decf size)
      (call-next-method))))

(defmethod push-first (value (list limited-list))
  "Add in front of the list and drop the last element if list is
full."
  (prog1 (call-next-method)
    (with-slots (max-size size last-element) list
      (if (or (not max-size)
	      (< size max-size))
	  (incf size)
	  (dle-remove (dle-previous last-element))))))

(defmethod push-last (value (list limited-list))
  "Add at the end of the list and drop the first element if list
is full."
  (prog1 (call-next-method)
    (with-slots (max-size size elements) list
      (if (or (not max-size)
	      (< size max-size))
	(incf size)
	(setf (dle-previous (dle-next elements)) nil
	      elements (dle-next elements))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sorted-list (limited-list)
  ((test :type function
	 :initarg :test))
  (:documentation
   "A double linked list where elements are inserted in a
sorted order."))

(defgeneric insert (item sorted-list)
  (:documentation
   "Insert an item in a sorted-list."))

(defmethod insert (item (sl sorted-list))
  "Insert ITEM in SL, which is a sorted double linked list,
before the item for which TEST is true or at the end of the list.
Returns two values, the modified list and the cursor to the new
element."
  (with-slots (max-size size elements test last-element) sl
    (do ((cursor elements (dle-next cursor)))
	((or (not (dle-next cursor))
	     (funcall test item (dle-value cursor)))
	 (if (dle-previous cursor)
	     (cons-dle item (dle-previous cursor) cursor)
	     (setf elements (cons-dle item nil cursor)))
	 (if (or (not max-size)
		  (< size max-size))
	     (incf size)
	     (dle-remove (dle-previous last-element)))
	 (values sl (dle-previous cursor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (lru-cache-slot (:include double-linked-element)
			   (:conc-name lruc-slot-))
  key)

(defmethod print-object ((object lru-cache-slot) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "key=~S value=~S" (lruc-slot-key object) (lruc-slot-value object))))

(defvar *default-cache-size* 100
  "Default size of a LRU cache if it's not specified at instantiation
time.")

(defclass lru-cache ()
  ((max-size :initform *default-cache-size*
	     :initarg :size
	     :reader max-size
	     :type (or integer null)
	     :documentation
	     "Maximum number of elements that the cache can fit.")
   (elements-list :type lru-cache-slot
		  :documentation "The list of elements held by the cache.")
   (elements-hash :type hash-table
		  :documentation "The hash table of the elements held bye the cache.")
   (last-element :type lru-cache-slot)
   (size :initform 0
	 :reader size
	 :type integer
	 :documentation "Current number of elements in the cache.")
   (finalizer :initform nil
	      :initarg :finalizer
	      :documentation
	      "Procedure to call when elements are dropped from cache."))
  (:documentation
   "An objects cache that keeps the elements used more often and
drops those that are used less often.  The usage is similar to an
hash table.  Elements are added to the list up to MAX-SIZE, then
any new element will drop the less used one in the cache.  Every
time an element is set or retrieved it goes in front of a list.
Those which get at the end of the list are dropped when more room
is required."))

(defmethod initialize-instance ((object lru-cache) &key test &allow-other-keys)
  (call-next-method)
  (with-slots (last-element elements-list elements-hash) object
    (setf last-element (make-lru-cache-slot)
	  elements-list last-element
	  elements-hash (if test
			    (make-hash-table :test test)
			    (make-hash-table)))))

(defgeneric getcache (key cache)
  (:documentation
   "Get an item with KEY from a CACHE."))

(defgeneric (setf getcache) (value key cache)
  (:documentation
   "Set or add an item with KEY in a CACHE."))

(defgeneric remcache (key cache)
  (:documentation
   "Remove an item with KEY from a CACHE."))

(defun move-in-front-of-cache-list (slot cache)
  "Relocate slot to the front of the elements list in cache.
This will stretch its lifespan in the cache."
  (declare (type lru-cache-slot slot)
	   (type lru-cache cache))
  ;; remove the slot from its original place...
  (dle-remove slot)
  ;; ... and add it in front of the list
  (with-slots (elements-list) cache
    (setf (lruc-slot-next slot) elements-list
	  (lruc-slot-previous slot) nil
	  (lruc-slot-previous elements-list) slot
	  elements-list slot)))

(defun drop-last-cache-element (cache)
  "Drop the last element in the list of the cache object."
  (declare (type lru-cache cache))
  (with-slots (last-element elements-hash finalizer) cache
    (awhen (lruc-slot-previous last-element)
      (when finalizer
	(funcall finalizer (lruc-slot-value it)))
      (dle-remove it)
      (remhash (lruc-slot-key it) elements-hash))))

(defun add-to-cache (slot cache)
  (declare (type lru-cache-slot slot)
	   (type lru-cache cache))
  (move-in-front-of-cache-list slot cache)
  (with-slots (max-size size elements-hash) cache
    (setf (gethash (lruc-slot-key slot) elements-hash) slot)
    (if (and max-size
	     (< size max-size))
	(incf size)
	(drop-last-cache-element cache))))

(defmethod getcache (key (cache lru-cache))
  (multiple-value-bind (slot found?) (gethash key (slot-value cache 'elements-hash))
    (when found?
      (move-in-front-of-cache-list slot cache)
      (values (lruc-slot-value slot) t))))

(defmethod (setf getcache) (value key (cache lru-cache))
  (with-slots (elements-hash elements-list) cache
    (multiple-value-bind (slot found?) (gethash key elements-hash)
      (if found?
	  (progn
	    (move-in-front-of-cache-list slot cache)
	    (setf (lruc-slot-value slot) value))
	  (add-to-cache (make-lru-cache-slot :key key :value value) cache))
      value)))

(defmethod remcache (key (cache lru-cache))
  (with-slots (elements-hash size finalizer) cache
    (multiple-value-bind (slot found?) (gethash key elements-hash)
      (when found?
	(remhash key elements-hash)
	(when finalizer
	  (funcall finalizer (lruc-slot-value slot)))
	(dle-remove slot)
	(decf size)
	t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun list->string (list)
  "Convert a list of characters into a string."
  (concatenate 'string list))

(defun getuid ()
  "Return the Unix user id.  This is an integer."
  #+sbcl (sb-unix:unix-getuid)
  #+cmu (unix:unix-getuid)
  #+clisp (posix:getuid)
  #-(or cmu sbcl clisp)
  (error "getuid unsupported under this Lisp implementation"))

(defun get-logname ()
  "Return the login id of the user.  This is a string and it is
not the Unix uid, which is a number."
  #+sbcl (sb-unix:uid-username (getuid))
  #+cmu (unix:user-info-name (unix:unix-getpwuid (getuid)))
  #+clisp (posix:user-info-login-id (posix:user-info (posix:getuid)))
  #-(or cmu sbcl clisp)
  (error "get-logname unsupported under this Lisp implementation"))

;; Rather stupid, but the mnemonic is worth it
(defun alist->plist (alist)
  "Convert an association list into a property list.  The alist
elements are assumed to be lists of just two elements: the key
and the value.  If the element list is longer this function
doesn't work."
  (mapcan #'identity alist))

(defun plist->alist (plist &optional pairs-p)
  "Convert a property list into an association list.  The alist
elements wiil be lists of just two elements: the key and the
value.  If PAIRS-P is true the alist elements will be pairs."
  (loop
     for (key val) on plist by #'cddr
     collect (if pairs-p
		 (cons key val)
		 (list key val))))

(defun string->byte-vector (string &key start end)
  "Convert a string of characters into a vector of (unsigned-byte
8) elements."
  (map '(vector (unsigned-byte 8)) #'char-code
       (if (or start end)
	   (subseq string (or start 0) end)
	   string)))

(defun byte-vector->string (vector &key start end)
  "Convert a vector of (unsigned-byte 8) elements into a string
of characters."
  (map 'string #'code-char
       (if (or start end)
	   (subseq vector (or start 0) end)
	   vector)))

(defun outdated-p (file dependencies)
  "Check if FILE has been modified before any of its
DEPENDENCIES."
  (be epoch (file-write-date file)
    ;; if file is missing altogether, we consider it outdated
    (or (not epoch)
	(loop
	   for dep in dependencies
	   for date = (file-write-date dep)
	   thereis (and date
			(> date epoch))))))

(defmacro let-slots (accessor/new-value-pairs object &body body)
  "Execute BODY with some OBJECT's slots temporary sets to new
values as described in ACCESSOR/NEW-VALUE-PAIRS.  The latter
should be an alist of accessor names and the value to be assigned
to that slot.  On exit from BODY, those slots are restored to
their original value."
  (let ((obj (gensym))
	(old-variables (loop
			  for x in accessor/new-value-pairs
			  collect (gensym))))
    `(be ,obj ,object
       (let ,(mapcar #'(lambda (old av-pair)
			 `(,old (,(car av-pair) ,obj)))
		     old-variables
		     accessor/new-value-pairs)
       
	 (unwind-protect
	      (progn
		;; as some slot values could raise an error, we assign
		;; the slots within the unwind-protect block so that we
		;; can always guarantee a consistent state of OBJECT
		,@(mapcar #'(lambda (av-pair)
			      `(setf (,(car av-pair) ,obj) ,(cadr av-pair)))
			  accessor/new-value-pairs)
		,@body)
	   ,@(mapcar #'(lambda (old av-pair)
			 `(setf (,(car av-pair) ,obj) ,old))
		     old-variables
		     accessor/new-value-pairs))))))

(defvar *decimal-point* #\.)
(defvar *thousands-comma* #\,)

(defun format-amount (number &optional (decimals 2))
  "Return a string formatted as fixed decimal point number of
DECIMALS adding commas every 3 places before the decimal
point."
  (be negative (< number 0)
    (when negative
      (setf number (- number)))
    (multiple-value-bind (int dec) (floor number)
      (let* ((integer-places (if (zerop int)
				 1
				 (1+ (log int 10))))
	     (length (+ decimals 1
			(if negative 1 0)
			(truncate integer-places)
			(truncate (1- integer-places) 3)))
	     (str (make-string length)))
	(when negative
	  (setf (char str 0) #\-))
	(setf (char str (- length decimals 1)) *decimal-point*)
	(flet ((digit (n)
		 (char "0123456789" n)))
	  (dotimes (i decimals)
	    (setf dec (* dec 10)
		  (char str (+ i (- length decimals))) (digit (mod (truncate dec) 10))))
	  (loop
	     for i from 1
	     if (zerop (mod i 4))
	     do (setf (char str (- length decimals 1 i)) *thousands-comma*)
	     else
	     do (setf (char str (- length decimals 1 i)) (digit (mod int 10))
		      int (truncate int 10))
	     until (zerop int)))
	str))))

(defmacro with-package (name &body body)
  `(let ((*package* (find-package ,name)))
     ,@body))

(defun bytes-simple-string (n &optional imply-bytes)
  "Return a string describing N using a unit of measure multiple
of a byte that is most apporpriate for the magnitude of N.  A
kilobyte is 1024 not 1000 bytes, everything follows."
  (let* ((kilo 1024)
	 (mega (* kilo kilo))
	 (giga (* kilo mega))
	 (tera (* mega mega))
	 (peta (* kilo tera)))
    (apply #'format nil "~,1F~A"
	   (cond ((> n (* 2 peta))
		  (list (/ n peta) (if imply-bytes "P" "PB")))
		 ((> n (* 2 tera))
		  (list (/ n tera) (if imply-bytes "T" "TB")))
		 ((> n (* 2 giga))
		  (list (/ n giga) (if imply-bytes "G" "GB")))
		 ((> n (* 2 mega))
		  (list (/ n mega) (if imply-bytes "M" "MB")))
		 ((> n (* 2 kilo))
		  (list (/ n kilo) (if imply-bytes "K" "KB")))
		 (t (list n (if imply-bytes "" " bytes")))))))

;; WARNING: This function may or may not work on your Lisp system.  It
;; all depends on how the OPEN function has been implemented regarding
;; the :IF-EXISTS option.  This function requires that OPEN be
;; implemented in a way so that the checking of the existence of file
;; and its open attempt be atomic.  If the Lisp OPEN first checks that
;; the file exists and then tries to open it, this function won't be
;; reliable.  CMUCL seems to use the O_EXCL open() flag in the right
;; way.  So at least on CMUCL this function will work.  Same goes for
;; SBCL.
(defun make-lock-files (pathnames &key (sleep-time 7) retries (suspend 13) expiration)
  "Create semaphore files.  If it can't create all the specified
files in the specified order, it waits SLEEP-TIME seconds and
retries the last file that didn't succeed.  You can specify the
number of RETRIES to do until failure is returned.  If the number
of retries is NIL this function will retry forever.

If it tries RETRIES times without success, this function signal
an error and removes all the lock files it created until then.

All files created by lock file will be read-only.

If you specify a EXPIRATION then an existing lock file will be
removed by force after EXPIRATION seconds have passed since the
lock file was last modified/created (most likely by some other
program that unexpectedly died without cleaning up its lock
files).  After a lock file has been removed by force, a
suspension of SUSPEND seconds is taken into account, in order to
prevent the inadvertent immediate removal of any newly created
lock file by another program."
  (be locked '()
    (flet ((lock (file)
	     (when (and expiration
			(> (get-universal-time)
			   (+ (file-write-date file) expiration)))
	       (delete-file file)
	       (when suspend
		 (sleep suspend)))
	     (do ((i 0 (1+ i))
		  (done nil))
		 (done)
	       (unless (or (not retries)
			   (< i retries))
		 (error "Can't create lock file ~S: tried ~A time~:P." file retries))
	       (with-open-file (out file :direction :output :if-exists nil)
		 (cond (out
			(format out "Lock file created on ~A~%" (time-string (get-universal-time)))
			(setf done t))
		       (sleep-time
			(sleep sleep-time)))))))
      (unwind-protect
	   (progn
	     (dolist (file pathnames)
	       (lock file)
	       (push file locked))
	     (setf locked '()))
	(mapc #'delete-file locked)))))

(defmacro with-lock-files ((lock-files &rest lock-args) &body body)
  "Execute BODY after creating LOCK-FILES.  Remove the lock files
on exit.  LOCK-ARGS are passed to MAKE-LOCK-FILES."
  (with-gensyms (files)
    `(be ,files (list ,@lock-files)
       (make-lock-files ,files ,@lock-args)
       (unwind-protect (progn ,@body)
	 (mapc #'delete-file ,files)))))

(defun getpid ()
  #+cmu (unix:unix-getpid)
  #+sbcl (sb-unix:unix-getpid)
  #+clisp (ext:process-id)
  #-(or cmu sbcl clisp)
   (error "getpid unsupported under this Lisp implementation"))

(defmacro on-error (form &body error-forms)
  "Execute FORM and in case of error execute ERROR-FORMS too.
This doesn't stop the error from propagating."
  (be done-p (gensym)
    `(be ,done-p nil
       (unwind-protect
	    (prog1
		,form
	      (setf ,done-p t))
	 (unless ,done-p
	   ,@error-forms)))))

(defun floor-to (x aim)
  "Round X down to the nearest multiple of AIM."
  (* (floor x aim) aim))

(defun round-to (x aim)
  "Round X to the nearest multiple of AIM."
  (* (round x aim) aim))

(defun ceiling-to (x aim)
  "Round X up to the nearest multiple of AIM."
  (* (ceiling x aim) aim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct queue
  first
  last)

(defmethod queue-append ((queue queue) (objects list))
  (cond ((null (queue-first queue))
	 (setf (queue-first queue) objects
	       (queue-last queue) (last objects)))
	(t
	 (setf (cdr (queue-last queue)) objects
	       (queue-last queue) (last objects))))
  queue)

(defmethod queue-append ((queue queue) object)
  (queue-append queue (list object)))

(defmethod queue-pop ((queue queue))
  (prog1 (car (queue-first queue))
    (setf (queue-first queue) (cdr (queue-first queue)))))

(defmethod queue-empty-p ((queue queue))
  (null (queue-first queue)))
