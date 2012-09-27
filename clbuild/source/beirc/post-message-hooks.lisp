(in-package :beirc)

(defvar *post-message-hooks* (make-hash-table)
  "Table of hooks to be run when a message is posted to a receiver.")

(defun run-post-message-hooks (message frame receiver &rest args)
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (apply v message frame receiver args))
	   *post-message-hooks*)
  (values))

(defmacro define-post-message-hook (hook-name (message-var frame-var receiver-var &rest other-args) &body body)
  "Convenience macro for defining hooks that are run when a message is posted to a receiver."
  `(progn (defun ,hook-name (,message-var ,frame-var ,receiver-var ,@other-args &allow-other-keys) ,@body)
	  (setf (gethash ',hook-name *post-message-hooks*) ',hook-name)))

;;;---------------------------------------------------------------------------
;;; If you set *default-sound-player* and *sound-for-my-nick* this
;;; should work...  It leaves a lot to be desired.  This should
;;; probably turn into some kind of general noisemaking interface...
;;; But this should get us thinking. [2006/03/24:rpg]
;;;---------------------------------------------------------------------------
(define-post-message-hook noisemaker (msg frame receiver &key message-directed-to-me)
  (declare (ignore msg frame receiver))
  (when (and message-directed-to-me
	     *sound-server-stream*
	     *sound-for-my-nick*)
    (play-sound-file *sound-for-my-nick* *sound-server-stream*)))
