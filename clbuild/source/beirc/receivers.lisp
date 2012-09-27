(in-package :beirc)
#+(or)(declaim (optimize (debug 2)
                   (speed 0)
                   (space 0)))
(defclass receiver ()
     ((messages :accessor messages :initform nil)
      (unseen-messages :accessor unseen-messages :initform 0)
      (all-unseen-messages :accessor all-unseen-messages :initform 0)
      (messages-directed-to-me :accessor messages-directed-to-me :initform 0)
      (channel :reader channel :initform nil :initarg :channel)
      (connection :accessor connection :initarg :connection)
      (connection-open-p :accessor connection-open-p :initform t) ; used only on server receivers.
      (query :reader query :initform nil :initarg :query)
      (focused-nicks :accessor focused-nicks :initform nil)
      (title :reader title :initarg :title)
      (last-visited :accessor last-visited :initform 0)
      (incomplete-input :accessor incomplete-input :initform "")
      (positions-mentioning-user :accessor positions-mentioning-user :initform nil)
      (pane :reader pane)
      (tab-page :accessor tab-page)))

(defclass irc-connection-closed-message (irc:irc-message) ())

(defun slot-value-or-something (object &key (slot 'name) (something "without name"))
  (if (slot-boundp object slot)
      (slot-value object slot)
      something))

;;; (defmethod print-object ((receiver receiver) stream)
;;;   (print-unreadable-object (receiver stream :type t)
;;;     (write-string (slot-value-or-something receiver :slot 'title :something "without title")
;;;                   stream)))
;;; We have the needed accept-methods for the presentation-type receiver, so we can just write:
(defmethod print-object ((receiver receiver) stream)
  (write-string (slot-value-or-something receiver :slot 'title :something "#<RECEIVER without title>")
                stream))

(define-presentation-type receiver-pane ())

;;; KLUDGE: make-clim-application-pane doesn't return an application
;;; pane, but a pane that wraps the application pane. we need the
;;; application pane for redisplay, though.
(defun actual-application-pane (pane)
  "Find the actual clim:application-pane buried the layers and
  layers of wrapping panes that make-clim-application-pane
  returns."
  (if (typep pane 'clim:application-pane)
      pane
      (loop for child in (sheet-children pane)
            for found-pane = (actual-application-pane child)
            if found-pane do (return found-pane))))

(defun make-paneless-receiver (name &rest initargs)
  (apply 'make-instance 'receiver :title name initargs))

(defun initialize-receiver-with-pane (receiver frame pane &key (add-pane-p t))
  (setf (slot-value receiver 'pane) pane)
  (if (not add-pane-p)
      (setf (slot-value receiver 'tab-page) (sheet-to-page pane))
      (progn
        (setf (slot-value receiver 'tab-page)
              (make-instance 'tab-page
		:title (title receiver)
		:pane (pane receiver)
		:enabled-callback 'receiver-page-enabled-callback
		:presentation-type 'receiver-pane))
        (add-page (tab-page receiver) (find-pane-named frame 'query))
        ;; resize the pane to fit the tab container
        (change-space-requirements pane)))
  (setf (gethash (tab-page receiver) (tab-pages-to-receivers frame)) receiver))

(defun rename-query-receiver (receiver new-name)
  (let ((old-title (irc:normalize-nickname (connection receiver)
                                           (title receiver)))
        (normalized-name (irc:normalize-nickname (connection receiver)
                                                 new-name)))
    (with-slots (title query) receiver
       (setf title new-name
             query new-name
             (tab-page-title (tab-page receiver)) new-name)
       (remhash (list (connection receiver) old-title) (receivers *application-frame*))
       (setf (gethash (list (connection receiver) normalized-name) (receivers *application-frame*))
             receiver))))

(defun find-receiver (name connection frame)
  (gethash (list connection (irc:normalize-channel-name connection name))
           (receivers frame)))

(defun intern-receiver (name connection frame &rest initargs)
  (let* ((normalized-name (irc:normalize-channel-name connection name))
         (rec (find-receiver name connection frame)))
    (if rec
        rec
        (let* ((*application-frame* frame)
               (receiver (apply 'make-paneless-receiver normalized-name :connection connection
                                initargs))
               (creator (lambda (frame)
                          (initialize-receiver-with-pane receiver frame
                                                         (with-look-and-feel-realization
                                                             ((frame-manager *application-frame*) *application-frame*)
                                                           (make-clim-application-pane
                                                            :display-function
                                                            (lambda (frame pane)
                                                              (beirc-app-display frame pane receiver))
                                                            :display-time nil
                                                            :height 600
                                                            :min-width 400 :min-height 200
                                                            :incremental-redisplay t)))
                          (update-drawing-options receiver))))
          (if (equal (current-process) (ui-process frame))
              (funcall creator frame)
              (queue-beirc-event frame (make-instance 'new-sheet-event :sheet frame :creator creator)))
          (setf (gethash (list connection normalized-name) (receivers frame)) receiver
                (focused-nicks receiver) (cdr (assoc normalized-name
                                                     *auto-focused-alist*
                                                     :test #'equal)))
          receiver))))

(defun reinit-receiver-for-new-connection (server-receiver connection &optional (frame *application-frame*))
  (let ((old-connection (connection server-receiver)))
    (maphash (lambda (key receiver)
               (destructuring-bind (rec-connection name) key
                 (when (eql old-connection rec-connection)
                   (remhash key (receivers frame))
                   (setf (gethash (list connection name) (receivers frame)) receiver)
                   (setf (connection receiver) connection)
                   (dolist (message (messages receiver))
                     ;; KLUDGE: reset the connection of messages so
                     ;; that channel/user finding queries don't fail
                     ;; horribly
                     (setf (irc:connection message) connection)))))
             (receivers frame))))


(defun remove-receiver (receiver frame)
  (remove-page (tab-page receiver))
  (remhash (list (connection receiver) (title receiver)) (receivers frame)))

(defparameter *network-service-sources* '("nickserv" "memoserv" "chanserv" "")
  "Sources whose private messages (PRIVMSG, NOTICE, ...) should
  be treated as if they came from the connected server itself,
  unless the user has opened a query window to the source
  already.")

(defparameter *global-notice-targets* '("$*" "auth")
  "NOTICE message targets that should be treated as network
service targets.")

(defun nickname-comparator (connection)
  (lambda (nick1 nick2)
    (string= (irc:normalize-nickname connection nick1)
             (irc:normalize-nickname connection nick2))))

(defun from-network-service-p (source connection)
  (member source *network-service-sources*
          :test (nickname-comparator connection)))

(defun global-notice-p (message target)
  (and (typep message 'irc:irc-notice-message)
       (member target *global-notice-targets*
               :test (nickname-comparator (irc:connection message)))))

(macrolet ((define-privmsg-receiver-lookup (message-type)
               `(defmethod receiver-for-message ((message ,message-type) frame)
                  (let* ((mynick (current-nickname (irc:connection message)))
                             (nominal-target (irc:normalize-channel-name (irc:connection message)
                                                                         (first (irc:arguments message))))
                             (target (if (equal nominal-target mynick)
                                         (irc:source message)
                                         nominal-target)))
                    (cond ((find-receiver target (irc:connection message) frame)
                           (intern-receiver target (irc:connection message) frame :channel target))
                          ((or (global-notice-p message nominal-target)
                               (and (from-network-service-p (irc:source message) (irc:connection message))
                                    (equal nominal-target mynick)))
                           (server-receiver frame (irc:connection message)))
                          (t
                           (intern-receiver target (irc:connection message) frame :channel target)))))))
  (define-privmsg-receiver-lookup irc:irc-privmsg-message)
  (define-privmsg-receiver-lookup irc:ctcp-action-message)
  (define-privmsg-receiver-lookup irc:irc-notice-message))

(macrolet ((define-global-message-receiver-lookup (message-type)
               `(defmethod receiver-for-message ((message ,message-type) frame)
                  (remove nil
                          (mapcar (lambda (channel)
                                    (find-receiver channel (irc:connection message) frame))
                                  (let ((user (irc:find-user (irc:connection message)
                                                             (irc:source message))))
                                    (when user
                                      `(,@(mapcar (lambda (chan)
                                                    (irc:normalize-channel-name (irc:connection message)
                                                                                (irc:name chan)))
                                                  (irc:channels user))
                                          ,(irc:normalize-nickname (irc:connection message)
                                                                   (if (typep message 'irc:irc-quit-message)
                                                                       (irc:source message)
                                                                       (car (last (irc:arguments message)))))))))))))
  (define-global-message-receiver-lookup irc:irc-quit-message)
  (define-global-message-receiver-lookup irc:irc-nick-message))

(macrolet ((define-nth-arg-message-receiver-lookup (&rest clauses)
               "Defines receiver-for-message methods that return
               the receiver associated with the nth arg of the
               irc message or the last arg if NTH in the
               clauses is nil.

               Each clause must have this format:
               (nth message-type ...)"
               `(progn
                  ,@(loop for (nth . messages) in clauses
                          nconc (loop for message-type in messages
                                      collect
                                      `(defmethod receiver-for-message ((message ,message-type) frame)
                                         (let ((target ,(if (numberp nth)
                                                            `(nth ,nth (irc:arguments message))
                                                            `(first (last (irc:arguments message))))))
                                           (intern-receiver target (irc:connection message) frame :channel target))))))))
  (define-nth-arg-message-receiver-lookup
      (0 irc:irc-topic-message irc:irc-kick-message)
      (1 irc:irc-rpl_topic-message irc:irc-rpl_topicwhotime-message
         irc:irc-err_chanoprivsneeded-message irc:irc-rpl_banlist-message
         irc:irc-rpl_invitelist-message irc:irc-rpl_exceptlist-message)
      (2 irc:irc-rpl_namreply-message)
      (nil irc:irc-join-message)))

(defmethod receiver-for-message ((message irc:irc-part-message) frame)
  (let ((target (first (irc:arguments message))))
    (if (and
         (null (find-receiver target (irc:connection message) frame))
         (string= (irc:source message) (current-nickname (irc:connection message))))
        (server-receiver frame (irc:connection message)) ; don't re-open previously closed channels.
        (intern-receiver target (irc:connection message) frame :channel target))))

(defmethod receiver-for-message ((message irc:irc-mode-message) frame)
  (case (length (irc:arguments message))
    (2 (server-receiver frame (irc:connection message)))
    (t (destructuring-bind (channel modes &rest args) (irc:arguments message)
         (declare (ignore modes args))
         (intern-receiver channel (irc:connection message) frame :channel channel)))))

(macrolet ((define-current-receiver-or-server-message-types (&rest mtypes)
               `(progn
                  ,@(loop for mtype in mtypes
                          collect `(defmethod receiver-for-message ((message ,mtype) frame)
                                     (if (equal (connection (current-receiver frame)) (irc:connection message))
                                         (current-receiver frame)
                                         (server-receiver frame (irc:connection message))))))))
  (define-current-receiver-or-server-message-types
      irc:irc-rpl_whoisuser-message
      irc:irc-rpl_whoischannels-message
    irc:irc-rpl_whoisserver-message
    irc:irc-rpl_whoisidentified-message
    irc:irc-rpl_whoisidle-message
    irc:irc-rpl_away-message
    irc:irc-err_nosuchnick-message
    irc:irc-err_blocking_notid-message))

(macrolet ((define-ignore-message-types (&rest mtypes)
             `(progn
                ,@(loop for mtype in mtypes
                        collect `(defmethod receiver-for-message ((message ,mtype) frame)
                                   nil)))))
  (define-ignore-message-types cl-irc:irc-rpl_endofwhowas-message
    cl-irc:irc-rpl_endoflinks-message
    cl-irc:irc-rpl_endoptions-message
    cl-irc:irc-rpl_endofwhois-message
    cl-irc:irc-rpl_endsitelist-message
    cl-irc:irc-rpl_endofinvitelist-message
    cl-irc:irc-rpl_endofservices-message
    cl-irc:irc-rpl_endmode-message
    cl-irc:irc-rpl_endofmap-message
    cl-irc:irc-rpl_endofnames-message
    cl-irc:irc-rpl_endofusers-message
    cl-irc:irc-rpl_endofbanlist-message
    cl-irc:irc-rpl_endofmotd-message
    cl-irc:irc-rpl_endofinfo-message
    cl-irc:irc-rpl_endofstats-message
    cl-irc:irc-rpl_endofwho-message
    cl-irc:irc-rpl_endofexceptlist-message
    cl-irc:irc-ping-message))

;;; default receiver.
(defmethod receiver-for-message ((message irc:irc-message) frame)
  #+or                    ; comment out to debug on uncaught messages.
  (break)                   
  (server-receiver frame (irc:connection message)))

;; TODO: more receiver-for-message methods.

(macrolet ((define-delegate (function-name accessor &optional define-setter-p)
               `(progn
                  ,(when define-setter-p
                     `(defun (setf ,function-name) (new-value &optional (frame *application-frame*))
                        (when (current-receiver frame)
                          (setf (,accessor (current-receiver frame)) new-value))))
                  (defun ,function-name (&optional (frame *application-frame*))
                    (when (current-receiver frame)
                      (,accessor (current-receiver frame)))))))
  (define-delegate current-channel channel)
  (define-delegate current-query query)
  (define-delegate current-messages messages t)
  (define-delegate current-focused-nicks focused-nicks t))

(defun update-drawing-options (receiver)
  (when (and (slot-boundp receiver 'pane) (sheetp (pane receiver))
             (sheet-to-page (pane receiver)))
    (setf (tab-page-drawing-options (sheet-to-page (pane receiver)))
	  `(:ink ,(cond ((> (messages-directed-to-me receiver) 0) +green+)
		    ((> (unseen-messages receiver) 0) +red+)
		    (t +black+))))))

(defun receiver-page-enabled-callback (page)
  (let ((receiver (receiver-from-tab-page page)))
    (unless (null receiver)
      (setf (unseen-messages receiver) 0)
      (setf (all-unseen-messages receiver) 0)
      (setf (messages-directed-to-me receiver) 0)
      (setf (last-visited receiver) (get-universal-time))
      (update-drawing-options receiver))))

(defun raise-receiver (receiver)
  (setf (unseen-messages receiver) 0)
  (setf (all-unseen-messages receiver) 0)
  (setf (messages-directed-to-me receiver) 0)
  (setf (last-visited receiver) (get-universal-time))
  (switch-to-page (sheet-to-page (pane receiver))))
