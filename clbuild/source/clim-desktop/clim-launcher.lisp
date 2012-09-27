;; -*- Mode: lisp -*-

(in-package :clim-launcher)

(define-application-frame launcher ()
  () ; no slots
  ;;options
  (:panes (application :application
		       :display-function #'display-commands
		       :display-after-commands nil))
  (:layouts
   (defaults application)))

(defvar *apps* (make-hash-table :test 'equal))

(defclass clim-app ()
  ((name :initarg :name :accessor name :initform "")
   (entry :initarg :entry :accessor entry :initform (lambda (x) (format t "~A was called~%" x)))))

(defmethod display-commands ((frame launcher) stream) 
  (loop for app being the hash-values of *apps*
     do (present app 'clim-app :stream stream)))

(define-presentation-method present
    (app (type clim-app) stream (view textual-view) &key)
  (format stream "~A~%" (name app)))

(define-launcher-command (com-refresh-list
                          :menu t)
    ()
  (redisplay-frame-panes *application-frame*))

(define-launcher-command
    com-launch-app
    ((appl 'clim-app))
  ;; KLUDGE: SBCL doesn't keep dynamic bindings from the parent thread
  ;; when invoking a new thread, so we'll have to create the threads
  ;; and the bindings ourselves.
  (flet ((run ()
           (let #+sbcl ((sb-ext:*invoke-debugger-hook* 'clim-debugger:debugger)
                        (*debugger-hook* 'clim-debugger:debugger))
                #-sbcl nil
                (funcall (entry appl)))))
    (clim-sys:make-process #'run :name (name appl))))

(define-launcher-command (com-remove-app)
    ((appl 'clim-app))
  ;; Remove from list.
  (remhash (name appl) *apps*)
  (redisplay-frame-panes *application-frame*))

(defun add-app (name entry)
  (setf (gethash name *apps*)
        (make-instance 'clim-app :name name :entry entry)))

(define-presentation-to-command-translator launch-app
    (clim-app com-launch-app launcher
	      :gesture :select
              :documentation "Launch application")
    (object) (list object))

(define-presentation-to-command-translator remove-app
    (clim-app com-remove-app launcher
	      :gesture :delete
              :documentation "Remove application")
    (object) (list object))

(add-app "Listener" (lambda () (clim-listener:run-listener)))
(add-app "Climacs" (lambda () (climacs:climacs)))
(add-app "Climacs (RV)" (lambda () (climacs:climacs-rv)))

(defun start ()
  "Start the CLIM Launcher program."
   #+:cmucl (multiprocessing::startup-idle-and-top-level-loops)
   (run-frame-top-level (make-application-frame 'clim-launcher::launcher)))

;; Get some support for launching apps into the CLIM Listener:

(defmethod display-applications ((frame clim-listener::listener) stream) 
  (loop for app being the hash-values of *apps*
     do (present app 'clim-app :stream stream)))

(define-command (com-list-applications
                 :name t
                 :command-table clim-listener::show-commands
                 :menu t)
    ()
  (display-applications *application-frame* (frame-standard-output *application-frame*)))

(define-command (com-launch-application
                 :name t
                 :command-table clim-listener::lisp-commands
                 :menu t)
    ((appl 'clim-app))
  ;; KLUDGE: SBCL doesn't inherit local dynamic bindings from the
  ;; parent thread, so we'll have to create the threads and the
  ;; bindings ourselves.
  (flet ((run ()
           (let #+sbcl ((sb-ext:*invoke-debugger-hook* #'clim-debugger:debugger)
                        (*debugger-hook* #'clim-debugger:debugger))
                #-sbcl nil
                (funcall (entry appl)))))
    (clim-sys:make-process #'run :name (name appl))))

(define-presentation-to-command-translator launch-application-translator
    (clim-app com-launch-application clim-listener::lisp-commands
	      :gesture :select
              :documentation "Launch Application")
    (object)
  (list object))

(define-presentation-to-command-translator edit-application-translator
    (clim-app climacs-gui::com-edit-function-definition clim-listener::lisp-commands
	      :gesture :edit
              :tester ((object presentation)
                       (declare (ignore presentation))
                       (symbolp (entry object)))
              :documentation "Edit Application")
    (object)
  (list (entry object)))

(define-presentation-method accept
    ((type clim-app) stream view &key (default nil defaultp)
     (default-type type))
  (multiple-value-bind (object success string)
      (complete-input stream
		      (lambda (so-far action)
			(complete-from-possibilities
			 so-far
                         (loop for val being the hash-values of *apps*
                            collecting val)
                         '()
                         :action action
			 :name-key #'name
			 :value-key #'identity))
		      :partial-completers '(#\Space)
		      :allow-any-input t)
    (cond (success
	   (values object type))
	  ((and (zerop (length string)) defaultp)
           (values default default-type))
	  (t (values string 'string)))))