(in-package :beirc)

;;;---------------------------------------------------------------------------
;;; This is a rudimentary approach to having a permanently-running
;;; sound server to which you can dump sounds. [2006/04/06:rpg]
;;;---------------------------------------------------------------------------

;;;---------------------------------------------------------------------------
;;; To dos:
;;; 1.  figure out whether this is at all compatible with a
;;; single-threaded lisp, and if so, how to make it work out.
;;; 2.  Add cmucl and sbcl sound player forms.  SBCL added; needs to be checked.
;;;---------------------------------------------------------------------------

(defvar *sound-server-pid* NIL
  "What's the PID of the process to which you can dump sounds?
Should probably be moved to a slot of the application.")

(defvar *sound-server-stream* NIL
  "What's the stream into which you dump sound files?")

(defun start-sound-server (&optional (sound-player-cmd *default-sound-player*))
  (when sound-player-cmd
    (let (sound-stream pid)
      #+allegro
      (let (bogon)
	(declare (ignore bogon))
	(multiple-value-setq (sound-stream bogon pid)
	  (excl:run-shell-command sound-player-cmd :wait nil :input :stream :output "/dev/null" :if-output-exists :append
				  :error-output "/dev/null" :if-error-output-exists :append)))
      ;; the following is close to completely untested... [2006/04/06:rpg]
      #+sbcl
      (let ((p
	     (sb-ext:run-program  "/bin/sh"
				  (list  "-c" sound-player-cmd)
				  :input :stream :output nil :error nil :wait nil)))
	(setf sound-stream (sb-ext:process-input p)
	      pid p))
      #-(or allegro sbcl)
      (progn 
	(cerror "Just reset *default-sound-player* to NIL and run without sounds."
		"Don't know how to start a beirc sound server for this lisp.  Feel free to supply one.")
	(setf *default-sound-player* nil)
	(return-from start-sound-server nil))
      (setf *sound-server-pid* pid
	    *sound-server-stream* sound-stream))
    ))

(defun stop-sound-server ()
  "As the name suggests, shut down the sound server, killing the
OS subprocess."
  (when *sound-server-pid*
    #+sbcl
    (progn
      (sb-ext:process-kill *sound-server-pid* sb-posix:sigkill)
      (sb-ext:process-wait *sound-server-pid* t))
    #+allegro
    (progn
      (close *sound-server-stream*)
      (system:reap-os-subprocess :pid *sound-server-pid*))
    (setf *sound-server-pid* nil
	  *sound-server-stream* nil))
  (values))
	
(defun play-sound-file (filename &optional (stream *sound-server-stream*))
  "Play a sound file by dumping it into a stream opened by a sound server
program."
  (copy-to-stream filename stream))  

;;;---------------------------------------------------------------------------
;;; Helper function
;;;---------------------------------------------------------------------------
  
(defun copy-to-stream (from-file to-stream)
  "Dump the contents of the file FROM-FILE into the stream TO-STREAM."
  (with-open-file (from from-file)
    (cl-fad:copy-stream from to-stream)))

