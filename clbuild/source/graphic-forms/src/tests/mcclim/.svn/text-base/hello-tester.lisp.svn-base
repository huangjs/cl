
(defpackage :clim-graphic-forms-tests
  (:use :clim  :clim-lisp))

(in-package :clim-graphic-forms-tests)

(define-application-frame hello-frame ()
  ((message :initform "Foo!" :accessor message))
  (:menu-bar menubar-command-table)
  (:panes (some-pane :application :display-function 'display-some-pane))
  (:layouts (default
              (vertically (:height 500 :width 400)
                          (:fill some-pane)))))

(define-command com-hello ()
  #+graphic-forms (gfs::debug-print "com-hello called  ")
  (setf (message *application-frame*) "Hello there!"))

(define-command com-hi ()
  #+graphic-forms (gfs::debug-print "com-hi called  ")
  (setf (message *application-frame*) "Hi there!"))

(define-command-table menu-command-table
  :menu (("Hello" :command com-hello)
         ("Howdy" :command com-hi)))

(define-command-table menubar-command-table
  :menu (("Menu" :menu menu-command-table)
         ("Quit" :command com-quit-frame)))

(define-hello-frame-command (com-quit-frame :name "Quit" :menu t)
 ()
 (frame-exit *application-frame*))

(defmethod display-some-pane ((frame hello-frame) stream)
  #+graphic-forms (gfs::debug-print "display-some-pane called  ")
  (format stream (message frame)))
