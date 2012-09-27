;;; Short script for the Lisp side of clbuild:
;;; Argument parsing and application startup.
;;;
;;; Part of clbuild by Luke Gorrie and contributors:
;;;   Luke Gorrie <luke@member.fsf.org>
;;;   Anthony Chaumas-Pellet <achaumas@wispery.info>
;;;   Christophe Rhodes <csr21@cantab.net>
;;;   David Lichteblau <david@lichteblau.com>
;;;   Eric Marsden <eric.marsden@free.fr>

(defpackage :clbuild
  (:use :cl))

(in-package :clbuild)

#+openmcl
(setf *default-pathname-defaults*
      (pathname (concatenate 'string (ccl:getenv "PWD") "/")))

(defparameter *raw-args*
  #+sbcl (cdr sb-ext:*posix-argv*)
  #-sbcl (with-open-file (s cl-user::*clbuild-args*)
	   (loop
	      for line = (read-line s nil)
	      while line
	      collect line)))
(defparameter *cmd* (car *raw-args*))
(defparameter *args*
  (loop
     with args = (cdr *raw-args*)
     while args
     for arg = (pop args)
     if (eql (mismatch arg "--") 2)
     collect (intern (string-upcase (subseq arg 2)) :keyword) into keys
     and collect (pop args) into keys
     else collect arg into normals
     finally (return (append normals keys))))
(defparameter *clean-features* *features*)
(push (intern (string-upcase *cmd*) :clbuild) *features*)

(defun quit (rc)
  (or #+sbcl (sb-ext:quit :unix-status rc)
      #+openmcl (ccl:quit rc)
      (error "not implemented")))

(defun namify (arg)
  (if (listp arg)
      (car arg)
      arg))

(defun &p (x)
  (find x '(&key &optional)))

(defmacro with-application ((&rest args) &body body)
  (let* ((keypos (position '&key args))
	 (non-keys (mapcar #'namify (if keypos (subseq args 0 keypos) args)))
	 (keys (when keypos (mapcar #'namify (subseq args (1+ keypos))))))
    `(let ((non-keys ',non-keys)
	   (keys ',keys))
       #+sbcl (declare (optimize sb-ext:inhibit-warnings))
       (setf *features* *clean-features*)
       (flet ((usage ()
		(format t "Usage: clbuild ~A~:[~; [KEYS]~]~{ ~(~A~)~}~%"
			*cmd* keys non-keys)
		(when keys
		  (format t "Optional keys:~{ --~(~A~) VALUE~}~%"
			  keys))))
	 (when (find (car *args*) '("help" "--help" "-help" "-h" "-H")
		     :test #'equal)
	   (usage)
	   (quit 0))
	 (handler-case
	     (destructuring-bind ,args *args*
	       (declare (ignore ,@(mapcar #'namify (remove-if #'&p args)))))
	   (error ()
	     (format t "Error: Invalid command line arguments.~%~%")
	     (usage)
	     (quit 1)))) 
       (destructuring-bind ,args
	   *args*
	 ,@body)
       (quit 0))))

;; helper function instead of REQUIRE, for the benefit of non-SBCL lisps
(defun make (system)
  (let ((*package* (find-package :cl-user)))
    (asdf:operate 'asdf:load-op system)))

(defun make-clim (system)
  #-(or :clim-graphic-forms :clim-gtkairo)
  (unless (find-package :xlib)
    (make :clx))
  (make :mcclim)
  (make system))

#+sbcl
(declaim (optimize sb-ext:inhibit-warnings))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hunchentoot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::hunchentoot
#+clbuild::hunchentoot

(make :hunchentoot-test)

(with-application (&key (port "4242"))
  (setf port (parse-integer port))
  (setf tbnl:*catch-errors-p* nil)
  (hunchentoot:start-server :port port)
  (format t "~%Webserver is running at ~
               http://localhost:~D/hunchentoot/test~%~
             Type RET to quit~%"
	  port)
  (read-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLPython
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::clpython
#+clbuild::clpython

(make :clpython)

(with-application ()
  (setf common-lisp-user::*clpython-module-search-paths* "/usr/lib/python2.5/")
  (clpython:repl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WebDAV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::webdav
#+clbuild::webdav

(make :cl-webdav)

(with-application (directory &key (port "4242"))
  (setf port (parse-integer port))
  (setf tbnl:*catch-errors-p* nil)
  (setf dav:*file-resource-base-path-namestring* directory)
  (push (dav:create-dav-dispatcher 'dav:file-resource) tbnl:*dispatch-table*)
  (hunchentoot:start-server :port port)
  (format t "~%WebDAV server for ~A is running at http://localhost:~D/~%"
	  directory
	  port)
  (format t "Type RET to quit~%")
  (read-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eclipse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::eclipse
#+clbuild::eclipse

(unless (find-package :eclipse)
  (make :eclipse))

;;; (eclipse-system:compile-themes \"source/eclipse/themes/microGUI/\"
;;; 			       \"source/eclipse/themes/Step/\"
;;; 			       \"source/eclipse/themes/brushed-metal/\"
;;; 			       \"source/eclipse/themes/CoolClean/\")

(with-application (&optional display)
  (defparameter cl-user::*eclipse-initfile* ".eclipse.lisp")
  (defparameter cl-user::*eclipse-eclipsedir* nil)
  (load (compile-file "source/eclipse/lib/clx-ext/event.lisp"))
  (apply #'eclipse:eclipse
	 (when display
	   (list :display (parse-integer display)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLIM Listener
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::listener
#+clbuild::listener

(make-clim :clim-listener)

(with-application ()
  (clim-listener:run-listener))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLIM Desktop Launcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::clim-launcher
#+clbuild::clim-launcher

(progn
  (make-clim :clim-clx)			;closure build order issue
  (make-clim :clim-desktop))

(with-application ()
  #+clim-gtkairo (cerror "okay, I've been warned"
			 "this won't work well with gtkairo")
  (setf clim:*default-frame-manager*
	(make-instance 'climi::pixie/clx-look :port (clim:find-port)))
  (clim-launcher::start))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gsharp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::gsharp
#+clbuild::gsharp

(make-clim :gsharp)

(with-application ()
  (gsharp::gsharp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; demo demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::demodemo
#+clbuild::demodemo

(make-clim :clim-examples)

(with-application ()
  (clim-demo::demodemo))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLIM alerts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::clim-alerts
#+clbuild::clim-alerts

(make-clim :clim-alerts)

(with-application ()
  (clim-alerts::start :new-thread nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; climacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::climacs
#+clbuild::climacs

(make-clim :climacs)

(with-application ()
  (climacs:climacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; closure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::closure
#+clbuild::closure

(make-clim :climacs)

(with-application (&optional url)
  (setf gui:*home-page* url)
  (clim-user::run-closure :new-process nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; beirc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::beirc
#+clbuild::beirc

(make-clim :beirc)

(with-application ()
  (beirc:beirc :new-process nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; climplayer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::climplayer
#+clbuild::climplayer

(make-clim :climplayer)

(with-application ()
  (climplayer:climplayer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse-xml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::parse-xml
#+clbuild::parse-xml

(make :cxml)

(with-application (filename)
  (handler-case
      (cxml:parse (pathname filename) nil)
    (error (c)
      (format t "~A~%" c)
      (quit 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; validate-xml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::validate-xml
#+clbuild::validate-xml

(make :cxml)

(with-application (filename)
  (handler-case
      (cxml:parse (pathname filename) nil :validate t)
    (error (c)
      (format t "~A~%" c)
      (quit 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; validate-relax-ng
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::validate-relax-ng
#+clbuild::validate-relax-ng

(make :cxml-rng)

(with-application (xml-filename rng-filename &key (compact "no"))
  (handler-case
      (cxml:parse (pathname xml-filename)
		  (cxml-rng:make-validator
		   (cond
		     ((equal compact "yes")
		       (cxml-rng:parse-compact (pathname rng-filename)))
		     ((equal compact "no")
		       (cxml-rng:parse-schema (pathname rng-filename)))
		     (t
		      (error "invalid compact value, must be yes or no")))))
    (error (c)
      (format t "~A~%" c)
      (quit 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xuriella
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::xuriella
#+clbuild::xuriella

(make :xuriella)

(with-application (xsl-filename xml-filename &key (output nil))
  (handler-case
      (xuriella:apply-stylesheet (pathname xsl-filename)
				 (pathname xml-filename)
				 :output (if output
					     (pathname output)
					     *standard-output*))
    (error (c)
      (format t "~A~%" c)
      (quit 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-to-xhtml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::html-to-xhtml
#+clbuild::html-to-xhtml
#+clbuild::html-to-xhtml

(make :closure-html)
(make :cxml)

(with-application (html-filename output-filename)
  (with-open-file (out output-filename
		       :element-type '(unsigned-byte 8)
		       :if-exists :error
		       :direction :output)
    (chtml:parse (pathname html-filename)
		 (cxml:make-octet-stream-sink out))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xhtml-to-html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::xhtml-to-html
#+clbuild::xhtml-to-html
#+clbuild::xhtml-to-html

(make :closure-html)
(make :cxml)

(with-application (xml-filename output-filename)
  (with-open-file (out output-filename
		       :element-type '(unsigned-byte 8)
		       :if-exists :error
		       :direction :output)
    (cxml:parse (pathname xml-filename)
		(chtml:make-octet-stream-sink out))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vecto-demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::vecto-demo
#+clbuild::vecto-demo

(make :vecto)

(with-application
    (filename &key (text "Lisp")
	      (font "/usr/share/fonts/truetype/ttf-bitstream-vera/Vera.ttf"))
  (vecto:with-canvas (:width 90 :height 90)
    (let ((font (vecto:get-font font))
          (step (/ pi 7)))
      (vecto:set-font font 40)
      (vecto:translate 45 45)
      (vecto:draw-centered-string 0 -10 text)
      (vecto:set-rgb-stroke 1 0 0)
      (vecto:centered-circle-path 0 0 35)
      (vecto:stroke)
      (vecto:set-rgba-stroke 0 0 1.0 0.5)
      (vecto:set-line-width 4)
      (dotimes (i 14)
        (vecto:with-graphics-state
          (vecto:rotate (* i step))
          (vecto:move-to 30 0)
          (vecto:line-to 40 0)
          (vecto:stroke)))
      (format t "Writing ~A~%" filename)
      (vecto:save-png filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ltk-demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::ltk-demo
#+clbuild::ltk-demo

(make :ltk)

(with-application ()
  (setf ltk:*debug-tk* nil)
  (ltk:with-ltk ()
    (let ((b (make-instance
	      'ltk:button
	      :text "Hello World!"
	      :command (lambda ()
			 (ltk:do-msg "Bye!" "Hello World!")
			 (setf ltk:*exit-mainloop* t)))))
      (ltk:pack b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; qt-tutorial-14
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::qt-tutorial-14
#+clbuild::qt-tutorial-14

(make :qt-tutorial)

(with-application ()
  (qt-tutorial-14:main))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; adw-charting-demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::adw-charting-demo
#+clbuild::adw-charting-demo

(make :adw-charting)

(with-application (filename)
  (adw-charting:with-pie-chart (400 200)
    (let ((now (get-decoded-time)))
      (adw-charting:add-slice "seconds past" now)
      (adw-charting:add-slice "seconds left" (- 60 now))
      (adw-charting:save-file filename))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; perfectstorm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::perfectstorm
#+clbuild::perfectstorm

(make :perfectstorm)

(with-application ()
  (storm:storm))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SWANK (server part only)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::swank
#+clbuild::swank

(make :swank)

(with-application (&key (port "4005") (address "127.0.0.1"))
  (let ((swank::*loopback-interface* address))
    (swank:create-server :port (parse-integer port) :dont-close t))
  (format t "Swank is running on port ~A. Press RET to exit.~%" port)
  (read-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; record-dependencies (internal helper command)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+(or clbuild::record-dependencies clbuild::recompile-systems)
(progn
  ;; list of projects that hide their .asd files from us.
  ;; (We don't search recursively for .asd files unless it's really
  ;; necessary.)
  (defvar *hidden-projects* '("clg" "graphic-forms" "eclipse"))

  (defun project-to-systems (name)
    (remove-if (lambda (name)
		 ;; I've had it with broken test systems.
		 ;; Let's blacklist them unconditionally.
		 (search "-test" name))
	       (mapcar (lambda (x) (pathname-name x))
		       (directory
			(if (find name *hidden-projects* :test #'equal)
			    (format nil "source/~A/**/*.asd" name)
			    (format nil "source/~A/*.asd" name)))))))

#+clbuild::record-dependencies
#+clbuild::record-dependencies

(make :cl-ppcre)

(progn
  ;; Stop broken .asds from loading stuff without our permission.
  ;; ASDF extensions can be whitelisted here.
  (let ((*load-time-dependency-whitelist* '(#:cffi-grovel)))
    (defmethod asdf:perform :before ((op t) (system asdf:system))
      (unless (find (asdf:component-name system)
		    *load-time-dependency-whitelist*
		    :test #'string-equal)
	(error "Confusion between system definition an load time. Attempt to load ASDF systems during .asd parsing: ~A."
	       system))))

  (defun extra-system-dependencies (system)
    ;; add magic dependencies to ensure that clbuild configuration doesn't
    ;; leak into the dependency file too easily.  In this case, we don't
    ;; want clim-gtkairo's dependency on cffi to appear or disappear every time
    ;; some rebuilds dependencies with a different backend.
    ;;
    ;; (Better include spurious dependencies than too few.)
    ;;
    ;; OpenMCL-specific stuff like acl-compat for hunchentoot might also have
    ;; to be added here.
    (cdr (assoc system
		'(("mcclim" "cffi"))
		:test #'equal)))

  (defun extra-project-dependencies (project)
    (cdr (assoc project
		'(("redshank" "slime"))
		:test #'equal)))

  (defun system-to-project (name)
    (let* ((pathname (asdf:component-pathname (asdf:find-system name)))
	   (relative (enough-namestring pathname)))
      (cond
	((eq :absolute (car (pathname-directory relative)))
	 (error "found ~A outside of clbuild, can't translate to project"
		name))
	((equal "target" (second (pathname-directory relative)))
	 "sbcl")
	((equal "source" (second (pathname-directory relative)))
	 (third (pathname-directory relative)))
	(t
	 (error "found ~A outside of clbuild, can't translate to project"
		name)))))

  (defmacro without-errors ((error-value description) &body body)
    `(handler-case
	 (progn ,@body)
       (error (c)
	 (format t "Ignoring error ~A: ~A~%~%" ,description c)
	 ,error-value)))

  (defun system-dependencies (name)
    (let ((dependencies '())
	  (seen '()))
      (labels ((walk-dependency (sym)
		 (when (and (consp sym) (eq (car sym) :version))
		   (setf sym (second sym)))
		 (unless (find sym seen)
		   (push sym seen)
		   (let ((name (string-downcase sym)))
		     (when (asdf::system-definition-pathname name)
		       (pushnew sym dependencies))
		     (register-dependencies name))))
	       (register-dependencies (name)
		 (let* ((system (asdf:find-system name))
			(in-order-to
			 (slot-value system 'asdf::in-order-to)))
		   (loop
		      for (nil (nil . depends-on)) in in-order-to
		      do (dolist (next depends-on) 
			   (without-errors (nil "while walking dependencies")
			     (walk-dependency next)))))))
	(register-dependencies name))
      dependencies))

  (defun safe-project-dependencies (project)
    (let ((projects
	   (loop
	      for system in (project-to-systems project)
	      append (mapcar (lambda (system)
			       (without-errors (nil "when looking for system")
				 (system-to-project system)))
			     (without-errors
				 (nil "while scanning dependencies")
			       (append (extra-system-dependencies system)
				       (system-dependencies system)))))))
      (setf projects (append projects (extra-project-dependencies project)))
      (setf projects (remove nil projects))
      (setf projects (remove "sbcl" projects :test 'equal))
      (setf projects (remove project projects :test 'equal))
      (setf projects (remove-duplicates projects :test 'equal))
      (sort projects #'string-lessp)))

  (with-application (project-string dependency-file-name)
    (let ((projects (cl-ppcre:split "\\s+" project-string)))
      #+nil
      (setf projects
	    ;; requires clx in FIND-SYSTEM:
	    (remove "eclipse" projects :test 'equal))
      (setf projects (sort projects #'string-lessp))
      (dolist (project projects)
	(dolist (system (project-to-systems project))
	  (without-errors (nil (format nil "in find-system of ~A" system))
	    (asdf:find-system system nil))))
      (with-open-file (s dependency-file-name
			 :direction :output
			 :if-exists :rename-and-delete)
	(format s 
		"# Dependencies between projects known to clbuild.  If a~@
                 # project is not listed in this file, it can still be~@
                 # downloaded, but a warning will be issued.~@
                 # ~@
                 # THIS FILE IS AUTOGENERATED.  Feel free to edit it if you~@
                 # must, but never commit a hand-written version.  Use~@
                 # \"clbuild record-dependencies\" to recompute the official~@
                 # version.  Doing so requires a complete check out,~@
                 # including wnpp projects.  Please look out for unintended~@
                 # changes due to local configuration issues before~@
                 # committing~@
                 # ~@
                 # Quick dependency FAQ:~@
                 # ~@
                 # This file is an educated guess and sometimes not perfect.~@
                 # We prefer to err on the side of too many dependencies~@
                 # rather than too few.~@
                 # ~@
                 # However, the following situations are NOT necessarily bugs:~@
                 #   - circular dependencies~@
                 #     (cffi depends on trivial-features, which depends on~@
                 #     cffi itself.  That is NOT a problem.~@
                 #   - non-transitive dependencies~@
                 #     (if A depends on B, which depends on C, it does NOT~@
                 #     follow that A depends on C.)~@
                 # ~@
                 # For details, please refer to~@
                 # http://common-lisp.net/project/clbuild/#faq_dependency_details~@
                 # ~%")
	(dolist (project projects)
	  (format t "Looking for ~A's dependencies...~%" project)
	  (format s "~A~{ ~A~} ~%"
		  project
		  (safe-project-dependencies project)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recompile-systems (internal helper command)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::recompile-systems
#+clbuild::recompile-systems
#+clbuild::recompile-systems

(make :cl-ppcre)

(defparameter *blacklisted-systems*
    '(
      ;; need to rip out the theming fasls and configuration file stuff
      ;; to make it build reliably:
      "eclipse"

      ;; bogus #+sbcl reader conditional:
      #+sbcl "cl-typegraph"
      #+sbcl "cl-typesetting-test"

      ;; error due to constant folded reference to function object
      "cl-webdav"

      ;; don't want to depend on foreign libraries unnecessarily:
      ;; ... yes, this loads only clsql-postgresql-socket!
      ;; Good enough, I hope.
      "clsql-db2" "clsql-mysql" "clsql-aodbc" "clsql-oracle"
      "clsql-postgresql" "clsql-sqlite" "clsql-sqlite3" "clsql-odbc"

      ;; CFFI-GROVEL incompatibility?
      "trivial-features-tests"

      ;; assumes sb-bsd-sockets is already loaded instead of depending on it
      "ltk-remote"

      ;; .asd file assumes cells is already loaded
      "cells-test"

      ;; bogus dependencies in system
      "flexichain-test"

      ;; loads clsql from the asd file (argh!)
      "ele-clsql"

      ;; Require LIFT, which isn't needed otherwise
      "metabang-bind-test" "cl-containers-test"

      ;; windows only:
      #-(or windows mswindows win32) "graphic-forms"
      #-(or windows mswindows win32) "graphic-forms-tests"
      #-(or windows mswindows win32) "graphic-forms-uitoolkit"

      ;; random extra systems
      "mcclim-gif-bitmaps" "mcclim-jpeg-bitmaps" "mcclim-tiff-bitmaps"

      ;; blacklist funny test systems for no particular reason:
      "babel-tests" "clpython-test"))

(with-application (systems-string &key dump verbose force)
  (let ((systems (cl-ppcre:split "\\s+" systems-string))
	(*package* (find-package :cl-user)))
    (flet ((build ()
	     (dolist (system systems)
	       (format t "Loading ~A...~%" system)
	       (asdf:operate 'asdf:load-op system :verbose verbose))))
      (if verbose
	  (build)
	  (handler-bind
	      ;; make SBCL STFU
	      ((style-warning #'muffle-warning)
	       #+sbcl (sb-ext:compiler-note #'muffle-warning))
	    (let ((*compile-verbose* nil)
		  (*compile-print* nil)
		  (*load-verbose* nil)
		  (*load-print* nil))
	      (build)))))
    (when dump
      (format t "Dumping monster.core...~%")
      (force-output)
      #+sbcl (sb-ext:save-lisp-and-die "monster.core")
      #+clozure-common-lisp (ccl:save-application "monster.core")
      #-(or sbcl clozure-common-lisp)
      (error "don't know how to save this lisp"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hemlock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::hemlock

(with-application (&key file backend)
  (case backend
    (:qt
     (make :hemlock.qt))
    (:tty
     (make :hemlock.tty))
    ((nil)
     (make :hemlock.tty)
     (make :hemlock.qt)))
  (if file (ed file) (ed)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hemlock Slave
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::hemlock-slave
#+clbuild::hemlock-slave

(let ((*load-verbose* t))
  (make :hemlock.tty))

(with-application (&key editor slave-buffer background-buffer)
  #+ccl (setf ccl::*quiet-flag* nil)
  (format t "clbuild: Starting Slave...~%")
  (force-output)
  (hemlock::start-slave editor
			:slave-buffer slave-buffer
			:background-buffer background-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fell through
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(error "command not found")
