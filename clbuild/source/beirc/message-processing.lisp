(in-package :beirc)

;;; Functions and protocols related to message processing in beirc.

;;; Incoming IRC messages are caught by specializing
;;; irc:irc-message-event, which processes messages in this way:
;;;
;;;  1. The message is preprocessed by preprocess-message.
;;;  2. The message is posted to the application frame.
;;;  3. The message is processed by cl-irc's hooks.

(defvar *beirc-message-hooks* (make-hash-table))

(defclass beirc-connection (irc:connection)
     ())

(defmethod initialize-instance :after ((instance beirc-connection) &rest initargs)
  (declare (ignore initargs))
  (loop for hooks being the hash-values in *beirc-message-hooks* using (hash-key message-class)
        do (loop for hook in hooks
                 do (irc:add-hook instance message-class hook))))

(defmethod irc:irc-message-event :around ((connection beirc-connection) message)
  "Dispatch IRC messages to Beirc for display before cl-irc
mangles the channel/connection/user state."
  (preprocess-message connection message)
  (post-message *application-frame* message)
  (call-next-method))

;;; Message preprocessing

(defmethod preprocess-message ((connection beirc-connection) (message irc:irc-nick-message))
  "Handle various Nickname-change message cases:

 * change the connection's local user's nickname if it is the
   local user that changed its nickname.
 * rename queries that are open so that the nickname message gets
   posted there, too."
  (let ((receiver (find-receiver (irc:normalize-nickname connection (irc:source message))
                                 connection *application-frame*)))
    (cond
      ;; we changed our nick
      ((string= (irc:normalize-nickname connection (current-nickname connection))
                (irc:normalize-nickname connection (irc:source message)))
       (setf (irc:nickname (irc:user (irc:connection message)))
             (car (last (irc:arguments message)))
             
             (irc:normalized-nickname (irc:user (irc:connection message)))
             (irc:normalize-nickname connection (car (last (irc:arguments message))))))
      (receiver
       (rename-query-receiver receiver (car (last (irc:arguments message))))))))

(defmethod preprocess-message (connection message)
  (declare (ignore connection message))
  nil)

;;; Traditional cl-irc message hooks

(defmacro define-beirc-hook (hook-name ((message-var &rest message-types)) &body body)
  "Convenience macro for defining message hooks that are added at
connection instantiation time."
  `(progn (defun ,hook-name (,message-var) ,@body)
          ,@(loop for message-type in message-types
                  collect `(pushnew ',hook-name (gethash ',message-type *beirc-message-hooks*)))
          ',hook-name))

(define-beirc-hook update-away-status ((message irc:irc-rpl_noaway-message irc:irc-rpl_unaway-message))
  "Set/Unset away status."
  (setf (away-status *application-frame* (irc:connection message))
        (typep message 'irc:irc-rpl_noaway-message)))

(define-beirc-hook autojoin-hoook ((message cl-irc:irc-rpl_welcome-message))
  "When a connection is established, check the list of channels for autojoin
and set them up accordingly."
  (join-missing-channels *application-frame* (irc:connection message)))
