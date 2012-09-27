
(defpackage :clim-graphic-forms-tests
  (:use :clim  :clim-lisp))

(in-package :clim-graphic-forms-tests)

;;;
;;; (run-frame-top-level (make-application-frame 'buttons))
;;;

(define-application-frame buttons () ()
  (:menu-bar nil)
  (:layouts
    (default
      (vertically (:equalize-width t)
        (make-pane 'push-button :label "First")))))
