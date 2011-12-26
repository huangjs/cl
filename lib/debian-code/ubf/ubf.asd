;; hey emacs, this is -*- lisp -*-

(defpackage #:ubf.system
  (:use #:asdf #:cl))

(in-package #:ubf.system)

(defsystem ubf
    :components ((:file "defpackage")
		 (:file "ubf-a" :depends-on ("defpackage"))))

;;; nicked from clx's asd:
#+sbcl
(defmethod perform :around (o (f cl-source-file))
  ;; SBCL signals an error if DEFCONSTANT is asked to redefine a
  ;; constant unEQLly.  For our purposes, however, we are defining
  ;; structured constants (lists and arrays) not for EQLity, but for
  ;; the purposes of constant-folding operations such as (MEMBER FOO
  ;; +BAR+), so it is safe to abort the redefinition provided the
  ;; structured data is sufficiently equal.
  (handler-bind
      ((sb-ext:defconstant-uneql
	(lambda (c)
	  ;; KLUDGE: this really means "don't warn me about
	  ;; efficiency of generic array access, please"
	  (let ((old (sb-ext:defconstant-uneql-old-value c))
		(new (sb-ext:defconstant-uneql-new-value c)))
	    (typecase old
	      (list (when (equal old new) (abort c)))
	      (string (when (and (typep new 'string)
				 (string= old new))
			(abort c)))
	      (simple-vector
	       (when (and (typep new 'simple-vector)
			  (= (length old) (length new))
			  (every #'eql old new))
		 (abort c)))
	      (array
	       (when (and (typep new 'array)
			  (equal (array-dimensions old)
				 (array-dimensions new))
			  (equal (array-element-type old)
				 (array-element-type new))
			  (dotimes (i (array-total-size old) t)
			    (unless (eql (row-major-aref old i)
					 (row-major-aref new i))
			      (return nil))))
		 (abort c))))))))
    (call-next-method)))


;;  arch-tag: "023273c4-e9e3-11d7-ad53-000c76244c24"
