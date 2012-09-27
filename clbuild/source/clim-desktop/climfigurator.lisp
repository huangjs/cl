;; -*- Mode: lisp -*-

;;(asdf:oos 'asdf:load-op :mcclim)
;;(asdf:oos 'asdf:load-op :clim-listener)
;;(asdf:oos 'asdf:load-op :climacs)

(in-package :climfigurator)

(define-application-frame config ()
  () ; no slots
  ;;options
  (:panes (application :application
		       :display-function #'display-commands
		       :display-after-commands nil))
  (:layouts
   (defaults application)))

(defvar *apps* '())

(defclass clim-app ()
  ((name :initarg :name :accessor name :initform "")
   (entry :initarg :entry :accessor entry :initform (lambda (x) (format t "~A was called~%" x)))))

(defmethod display-commands ((frame config) stream) 
  (loop for app in *apps*
     do (present app 'clim-app :stream stream)))

(define-presentation-method present (app (type clim-app) stream (view textual-view) &key)
  (format stream "~A~%" (name app)))

(define-config-command com-config-app ((appl 'clim-app))
		       )

(defun add-app (name entry)
  (push  (make-instance 'clim-app :name name :entry entry) *apps*))

(define-presentation-to-command-translator launch-app
    (clim-app com-launch-app config
	      :gesture :select)
    (object) (list object))

(add-app "beirc"  :package :beirc "" '())
(add-app "debugger" '(path))
(add-app "climacs" '())


(defun start ()
   #+:cmucl (multiprocessing::startup-idle-and-top-level-loops)
  (run-frame-top-level (make-application-frame 'climfigurator::config)))

