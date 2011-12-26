(defpackage :hjs.utils.asdf
  (:use :cl)
  (:export #:asdf-load
	   #:asdf-compile
	   #:asdf-file-loading-sequence
	   ))

(in-package :hjs.utils.asdf)

(defun asdf-load (package)
  (let ((*derive-function-types* t))
    (asdf:oos 'asdf:load-op package)))

(defun asdf-compile (package)
  (let ((*derive-function-types* t))
    (asdf:oos 'asdf:compile-op package)))

(defun asdf-file-loading-sequence (system-name)
  (let ((seq (remove-if-not
	      (lambda (c)
		(typep c 'asdf:source-file))
	      (mapcar
	       #'cdr
	       (asdf::traverse
		(make-instance 'asdf:load-op)
		(asdf:find-system system-name))))))
    (mapcar #'asdf:component-pathname seq)))
