;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :fortran-ffi-accessors; Base: 10 -*-

(in-package "FORTRAN-FFI-ACCESSORS")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +f77-lower-case+ t
  "Fortran names are lower case if non-NIL")
(defconstant +f77-underscore+ t
  "Fortran names have a trailing underscore if non-NIL")
(defconstant +f77-extra-underscore+ nil
  "Fortran names containing an underscore have an extra underscore appended if non-NIL")
)

(defun %cat% (prefix-string s &optional suffix-string)
  (concatenate 'string 
	       prefix-string
	       (string s)
	       suffix-string))

(defun scat (prefix-string s &optional suffix-string)
  (intern (%cat% prefix-string s suffix-string)))

;; If the Fortran function name is NAME, the Lisp FFI name prepends
;; "FORTRAN-"
(defun make-fortran-ffi-name (name)
  (scat "FORTRAN-" name))

(defun make-fortran-name (name)
  ;; Given the Fortran routine name NAME, this returns the real
  ;; underlying name.  This depends on the compiler conventions being
  ;; used.  Some Fortran compilers take the Fortran name NAME and
  ;; produce "name_" as the real routine name.  Others will prepend
  ;; the underscore.  Yet others might convert the name to all upper
  ;; case.
  (let* ((internal-underscore-p (position #\_ (symbol-name name)))
	 (name (concatenate 'string
			   (symbol-name name)
			   (if +f77-underscore+ "_" "")
			   (if (and +f77-extra-underscore+ internal-underscore-p)
			       "_" ""))))
    (if +f77-lower-case+
	(string-downcase name)
	name)))
  


