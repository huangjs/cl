;;; license declaration
;;; most of the code are copied from sclp.lisp by Walter C. Pelissero
;;; this file is published in LGPL license

(in-package :cl-user)

(defpackage :hjs.util.os
  (:use :cl :cl-fad :split-sequence :hjs.meta.lisp :hjs.meta.essential :hjs.util.time :hjs.meta.macro)
  (:export #:run-pipe
	   #:which-file
	   #:with-open-pipe
	   #:getenv
	   #:getuid
	   #:getpid
	   #:get-logname
	   #:with-temp-file
	   #:with-hidden-temp-file
	   #:with-lock-files
	   #:make-symlink))

(in-package :hjs.util.os)

(defvar *bourne-shell* "/bin/sh")

(defmacro be (&rest bindings-and-body)
  "Less-parenthetic let."
  (let ((bindings
	 (loop
	    while (and (symbolp (car bindings-and-body))
		       (cdr bindings-and-body))
	    collect (list (pop bindings-and-body)
			  (pop bindings-and-body)))))
    `(let ,bindings
       ,@bindings-and-body)))

(defmacro be* (&rest bindings-and-body)
  "Less-parenthetic let*."
  (let ((bindings
	 (loop
	    while (and (symbolp (car bindings-and-body))
		       (cdr bindings-and-body))
	    collect (list (pop bindings-and-body)
			  (pop bindings-and-body)))))
    `(let* ,bindings
       ,@bindings-and-body)))

(defun getenv (var)
  "Return the string associate to VAR in the system environment."
  #+cmu (cdr (assoc (if (symbolp var)
			var
			(intern var :keyword))
		    ext:*environment-list*))
  #+sbcl (sb-ext:posix-getenv (string var))
  #+lispworks (hcl:getenv var)
  #+clisp (ext:getenv (string var))
  #-(or cmu sbcl lispworks clisp)
  (error "GETENV not implemented for your Lisp system."))

(defun getuid ()
  "Return the Unix user id.  This is an integer."
  #+sbcl (sb-unix:unix-getuid)
  #+cmu (unix:unix-getuid)
  #+clisp (posix:getuid)
  #-(or cmu sbcl clisp)
  (error "getuid unsupported under this Lisp implementation"))

(defun get-logname ()
  "Return the login id of the user.  This is a string and it is
not the Unix uid, which is a number."
  #+sbcl (sb-unix:uid-username (getuid))
  #+cmu (unix:user-info-name (unix:unix-getpwuid (getuid)))
  #+clisp (posix:user-info-login-id (posix:user-info (posix:getuid)))
  #-(or cmu sbcl clisp)
  (error "get-logname unsupported under this Lisp implementation"))

(defun getpid ()
  #+cmu (unix:unix-getpid)
  #+sbcl (sb-unix:unix-getpid)
  #+clisp (ext:process-id)
  #-(or cmu sbcl clisp)
  (error "getpid unsupported under this Lisp implementation"))

(defun run-pipe (direction fmt &rest args)
  "Run command made formatted by FMT and ARGS and according to
DIRECTION return the input and output streams and process object
of that process."
  #+cmu
  (be process (ext:run-program *bourne-shell* (list "-c" (apply #'format nil fmt args))
			       :wait nil
			       :pty nil
			       :input (when (member direction '(:output :input-output :io))
					:stream)
			       :output (when (member direction '(:input :input-output :io))
					 :stream)
			       :error nil)
      (values (ext:process-output process)
	      (ext:process-input process)
	      process))
  #+sbcl
  (be process (sb-ext:run-program *bourne-shell* (list "-c" (apply #'format nil fmt args))
				  :wait nil
				  :pty nil
				  :input (when (member direction '(:output :input-output :io))
					   :stream)
				  :output (when (member direction '(:input :input-output :io))
					    :stream)
				  :error nil)
      (values (sb-ext:process-output process)
	      (sb-ext:process-input process)
	      process))
  #+clisp
  (ext:make-pipe-input-stream
   (format nil "~A -c ~S" *bourne-shell*
	   (string-escape (string-escape (apply #'format nil fmt args) #\\) #\")))
  #-(or sbcl cmu clisp)
  (error "Unsupported Lisp system."))


(defun which-file (name)
  "Given the NAME of a system program try to find it through the
search of the environment variable PATH.  Return the full
pathname."
  (loop
     for dir in (split-sequence #\: (getenv "PATH"))
     for pathname = (merge-pathnames name (pathname-as-directory dir))
     when (probe-file pathname)
     return pathname))

(defmacro with-open-pipe ((in out program &rest args) &body body)
  (let ((direction (cond ((not in) :output)
			 ((and in out) :input-output)
			 (t :input)))
	(invar (or in (gensym)))
	(outvar (or out (gensym))))
    `(multiple-value-bind (,invar ,outvar) (run-pipe ,direction ,program ,@args)
       (declare (ignorable ,invar ,outvar))
       (unwind-protect (progn ,@body)
	 (when ,invar
	   (close ,invar))
	 (when ,outvar
	   (close ,outvar))))))

(defvar *tmp-file-defaults*
  #+unix #P"/tmp/"
  #+windows #P"c:/windows/temp")

(defun temp-file-name (&key prefix postfix (default *tmp-file-defaults*))
  "Create a random pathname based on DEFAULT.  No effort is made
to make sure that the returned pathname doesn't identify an
already existing file.  If missing DEFAULT defaults to
*TMP-FILE-DEFAULTS*."
  (make-pathname :defaults default
		 :name (format nil "~@[~a-~]~36R~@[-~a~]" prefix (random #.(expt 36 10)) postfix))) ;

(defun open-temp-file (&rest open-args &key prefix postfix (default-pathname *tmp-file-defaults*) &allow-other-keys)
  "Open a new temporary file and return a stream to it.  This
function makes sure the pathname of the temporary file is unique.
OPEN-ARGS are arguments passed verbatim to OPEN.  If
DEFAULT-PATHNAME is specified and not NIL it's used as defaults
to produce the pathname of the temporary file."
  (unless default-pathname
    (setf default-pathname *tmp-file-defaults*))
  (let ((open-args (remove-keyword-args '(:prefix :postfix :default-pathname) open-args)))
    (do* ((name #1=(temp-file-name :prefix prefix
				   :postfix postfix
				   :default default-pathname) #1#)
	  (stream #2=(apply #'open name :direction :output :if-exists nil
			    :if-does-not-exist :create open-args) #2#))
	 (stream stream))))

(defmacro with-temp-file ((stream &rest open-temp-args) &body body)
  "Execute BODY within a dynamic extent where STREAM is bound to
a STREAM open on a unique temporary file name.  OPEN-TEMP-ARGS are
passed verbatim to OPEN-TEMP-FILE."
  `(be ,stream (open-temp-file ,@open-temp-args)
       (unwind-protect
	    (progn ,@body)
	 (close ,stream)
	 ;; body may decide to rename the file so we must ignore the errors
	 (ignore-errors
	   (delete-file (pathname ,stream))))))

(defmacro with-hidden-temp-file ((stream &rest open-args) &body body)
  "Just like WITH-TEMP-FILE but unlink (delete) the temporary
file before the execution of BODY.  As such BODY won't be able to
manipulate the file but through STREAM, and not other program is
able to see it.  Once STREAM is closed the temporary file blocks
are automatically relinquished by the operating system.  This
works at least on Unix filesystems.  I don't know about MS-OSs
where the system may likely decide to crash, take all your data
with it and, in the meanwhile, report you to the NSA as
terrorist."
  `(be ,stream (open-temp-file ,@open-args)
       (unwind-protect
	    (progn (delete-file (pathname ,stream))
		   ,@body)
	 (close ,stream))))


;; WARNING: This function may or may not work on your Lisp system.  It
;; all depends on how the OPEN function has been implemented regarding
;; the :IF-EXISTS option.  This function requires that OPEN be
;; implemented in a way so that the checking of the existence of file
;; and its open attempt be atomic.  If the Lisp OPEN first checks that
;; the file exists and then tries to open it, this function won't be
;; reliable.  CMUCL seems to use the O_EXCL open() flag in the right
;; way.  So at least on CMUCL this function will work.  Same goes for
;; SBCL.
(defun make-lock-files (pathnames &key (sleep-time 7) retries (suspend 13) expiration)
  "Create semaphore files.  If it can't create all the specified
files in the specified order, it waits SLEEP-TIME seconds and
retries the last file that didn't succeed.  You can specify the
number of RETRIES to do until failure is returned.  If the number
of retries is NIL this function will retry forever.

If it tries RETRIES times without success, this function signal
an error and removes all the lock files it created until then.

All files created by lock file will be read-only.

If you specify a EXPIRATION then an existing lock file will be
removed by force after EXPIRATION seconds have passed since the
lock file was last modified/created (most likely by some other
program that unexpectedly died without cleaning up its lock
files).  After a lock file has been removed by force, a
suspension of SUSPEND seconds is taken into account, in order to
prevent the inadvertent immediate removal of any newly created
lock file by another program."
  (be locked '()
      (flet ((lock (file)
	       (when (and expiration
			  (> (get-universal-time)
			     (+ (file-write-date file) expiration)))
		 (delete-file file)
		 (when suspend
		   (sleep suspend)))
	       (do ((i 0 (1+ i))
		    (done nil))
		   (done)
		 (unless (or (not retries)
			     (< i retries))
		   (error "Can't create lock file ~S: tried ~A time~:P." file retries))
		 (with-open-file (out file :direction :output :if-exists nil)
		   (cond (out
			  (format out "Lock file created on ~A~%" (time-string))
			  (setf done t))
			 (sleep-time
			  (sleep sleep-time)))))))
	(unwind-protect
	     (progn
	       (dolist (file pathnames)
		 (lock file)
		 (push file locked))
	       (setf locked '()))
	  (mapc #'delete-file locked)))))

(defmacro with-lock-files ((lock-files &rest lock-args) &body body)
  "Execute BODY after creating LOCK-FILES.  Remove the lock files
on exit.  LOCK-ARGS are passed to MAKE-LOCK-FILES."
  (with-gensyms (files)
    `(be ,files (list ,@lock-files)
	 (make-lock-files ,files ,@lock-args)
	 (unwind-protect (progn ,@body)
	   (mapc #'delete-file ,files)))))

(defun make-symlink (from to &rest more-to-links)
  (let ((from (format nil "~a" from))
	(to (format nil "~a" to))
	(more-to-links (mapcar (lambda (l) (format nil "~a" l)) more-to-links)))
    (loop for p in (cons to more-to-links)
       do #+sbcl (sb-posix:symlink from p)
       #-sbcl (error "not implemented on this platform."))))

