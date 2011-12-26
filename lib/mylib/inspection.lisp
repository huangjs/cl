(in-package :cl-user)

(defpackage :hjs.sys.inspection
  (:use :cl)
  (:export #:methods-on-class
	   ))

(in-package :hjs.sys.inspection)

;;; from pjb
(defun methods-on-class (class-designator)
  (let ((class (if (typep class-designator 'class)
                   class-designator
                   (find-class class-designator)))
        (gfs '())
        (methods '()))
    (dolist (pack (list-all-packages))
      (do-symbols (s pack)
        (when (and (fboundp s) (typep (symbol-function s) 'generic-function))
          (push  (symbol-function s) gfs))
        (when (and (boundp s) (typep (symbol-value s) 'generic-function))
          (push  (symbol-value s) gfs))))
    (dolist (gf gfs)
      (dolist (m (CLOS:GENERIC-FUNCTION-METHODS gf))
        (when (member class (clos:method-specializers m))
          (push m methods))))
    (delete-duplicates methods)))
