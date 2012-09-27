;;; This is a playground for climacs editing.

(in-package :climacs-lisp-syntax)

(defun foobar (x)
 (with-slots (syntax) (buffer x)
      (compute-path syntax (offset (point x)))))
      
(in-package :climacs-gui)
(define-command (com-foobar :name "Foo" :command-table buffer-table) ()
		(let ((stream (climacs-gui::typeout-window "Foobar")))
		  (format stream "~A~%" (climacs-lisp-syntax::foobar (current-window)))))

(set-key 'com-foobar
	 'buffer-table
	 '((#\c :control) (#\d :control) (#\f :control)))

(define-command (com-offset :name "Offset" :command-table buffer-table) ()
		(display-message "~A~%" (offset (point (current-window)))))

(set-key 'com-offset
	 'buffer-table
	 '((#\c :control) (#\d :control) (#\o :control)))