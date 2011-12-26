(in-package :cl-user)

(defpackage :hjs.meta.essential
  (:use :cl)
  (:export #:repeating
	   #:with-unique-names
	   #:with-gensyms
	   #:once-only
	   #:rotate-byte
	   #:install-hjs-lib
	   ))

(in-package :hjs.meta.essential)

(defmacro repeating (times &body expressions)
  `(loop repeat ,times
      do (progn
	   ,@expressions)))

(defmacro with-unique-names ((&rest bindings) &body body)
  "Executes a series of forms with each var bound to a fresh,
uninterned symbol. See http://www.cliki.net/WITH-UNIQUE-NAMES"
  `(let ,(mapcar #'(lambda (binding)
                     (multiple-value-bind (var prefix)
			 (%with-unique-names-binding-parts binding)
		       (check-type var symbol)
		       `(,var (gensym ,(format nil "~A"
					       (or prefix var))))))
                 bindings)
     ,@body))

(defun %with-unique-names-binding-parts (binding)
  "Return (values var prefix) from a WITH-UNIQUE-NAMES binding
form. If PREFIX is not given in the binding, NIL is returned to
indicate that the default should be used."
  (if (consp binding)
      (values (first binding) (second binding))
      (values binding nil)))

(define-condition list-binding-not-supported (warning)
  ((binding :initarg :binding :reader list-binding-not-supported-binding))
  (:report (lambda (condition stream)
	     (format stream "List binding ~S not supported by WITH-GENSYMS.
It will work, but you should use WITH-UNIQUE-NAMES instead."
		     (list-binding-not-supported-binding condition))))
  (:documentation "List bindings aren't supported by WITH-GENSYMS, and
if you want to use them you should use WITH-UNIQUE-NAMES instead. That
said, they will work; they'll just signal this warning to complain
about it."))


(defmacro with-gensyms ((&rest bindings) &body body)
  "Synonym for WITH-UNIQUE-NAMES, but BINDINGS should only consist of
atoms; lists are not supported. If you try to give list bindings, a
LIST-BINDING-NOT-SUPPORTED warning will be signalled, but it will work
the same way as WITH-UNIQUE-NAMES. Don't do it, though."
  ;; Signal a warning for each list binding, if there are any
  (dolist (binding (remove-if-not #'listp bindings))
    (warn 'list-binding-not-supported :binding binding))
  ;; Otherwise, this is a synonym for WITH-UNIQUE-NAMES
  `(with-unique-names ,bindings ,@body))

(defun %check-once-only-names (names)
  "Check that all of the NAMES are symbols. If not, raise an error."
  ;; This only raises an error for the first non-symbol argument
  ;; found. While this won't report multiple errors, it is probably
  ;; more convenient to only report one.
  (let ((bad-name (find-if-not #'symbolp names)))
    (when bad-name
      (error "ONCE-ONLY expected a symbol but got ~S" bad-name))))

(defmacro once-only (names &body body)
  ;; Check the NAMES list for validity.
  (%check-once-only-names names)
  ;; Do not touch this code unless you really know what you're doing.
  (let ((gensyms (loop for name in names collect (gensym (string name)))))
    `(let (,@(loop for g in gensyms
		for name in names
		collect `(,g (gensym ,(string name)))))
       `(let (,,@(loop for g in gensyms for n in names
		    collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms
		      collect `(,n ,g)))
		,@body)))))


;;; rotate-byte
#+sbcl
 (eval-when (:compile-toplevel :load-toplevel :execute)
    (require 'sb-rotate-byte))
#+sbcl
(defmacro rotate-byte (count bytespec integer)
  `(sb-rotate-byte:rotate-byte ,count ,bytespec ,integer))

#-sbcl
(defun rotate-byte (count bytespec integer)
  "Rotates a field of bits within INTEGER; specifically, returns an
integer that contains the bits of INTEGER rotated COUNT times
leftwards within the byte specified by BYTESPEC, and elsewhere
contains the bits of INTEGER. See http://www.cliki.net/ROTATE-BYTE"
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 1)))
  (let ((size (byte-size bytespec)))
    (when (= size 0)
      (return-from rotate-byte integer))
    (let ((count (mod count size)))
      (labels ((rotate-byte-from-0 (count size integer)
                 (let ((bytespec (byte size 0)))
                   (if (> count 0)
                       (logior (ldb bytespec (ash integer count))
                               (ldb bytespec (ash integer (- count size))))
                       (logior (ldb bytespec (ash integer count))
                               (ldb bytespec (ash integer (+ count size))))))))
        (dpb (rotate-byte-from-0 count size (ldb bytespec integer))
             bytespec
             integer)))))



(defun install-hjs-lib (&optional (package *package*))
  (use-package :hjs.meta.essential package)
  (use-package :hjs.meta.functional package)
  (use-package :hjs.meta.macro package)
  (use-package :hjs.meta.lisp package)
  (use-package :hjs.meta.type package)
  (use-package :hjs.data.sequence package)
  (use-package :hjs.data.tree package)
  (use-package :hjs.data.array package)
  (use-package :hjs.util.string package)
  (progn
    (unintern 'year package)
    (unintern 'month package)
    (unintern 'date package)
    (unintern 'day package)
    (unintern 'hour package)
    (unintern 'minute package)
    (unintern 'second package))
  (use-package :hjs.util.math package))


;;; FIXME:
(defmacro with-tests ()
  )
