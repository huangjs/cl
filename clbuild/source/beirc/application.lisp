;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BEIRC; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Beirc -- A simple IRC client based on CL-IRC
;;;   Created: 2005-07-24
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Gilbert Baumann

;;; 
;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;; 

(in-package :beirc)

#+(or)(declaim (optimize (debug 2)
                   (speed 0)
                   (space 0)))

;;;; Quick guide:
;;
;; Start with (beirc)
;; Say /connect <server name> <nick name>
;; Say /join <channel>
;; To raise your voice just type in what to say.
;; 

;;; Commands:

;; <mumble>
;; /focus <nickname>
;; /unfocus <nickname>
;; /ignore <nickname>
;; /unignore <nickname>
;; /say <mumble>
;; /query <channel-or-nick>
;; /nick <string>
;; /join <channel>
;; /connect <server> <nick>
;; /me <mumble>
;; /msg <nick> <mumble>
;;
;; <mumble> is just the rest of the input line.
;; <nickname> is a nickname of someone, with completion


;;; KLUDGE: workaround for mcclim bug "Application pane vertical
;;; scrolling does not work with table formatting"
(defclass redisplay-frame-mixin ()
   ())

(defmethod redisplay-frame-pane :after
    ((frame redisplay-frame-mixin) (pane application-pane) &key force-p)
  (declare (ignore force-p))
  (change-space-requirements
   pane :height (bounding-rectangle-height (stream-output-history pane))))

(define-application-frame beirc (redisplay-frame-mixin
                                 standard-application-frame)
    ((connection-processes :initform nil :accessor connection-processes)
     (ui-process :initform (current-process) :accessor ui-process)
     (ignored-nicks :initform nil)
     (receivers :initform (make-hash-table :test #'equal) :accessor receivers)
     (server-receivers :initform nil :reader server-receivers)
     (tab-pages-to-receivers :initform (make-hash-table :test #'equal) :accessor tab-pages-to-receivers)
     (presence :initform (make-hash-table :test #'equal) :reader presence))
  (:panes
   (io
    :interactor
    :height 72)
   (pointer-doc :pointer-documentation)

   (status-bar
    :application
    :display-function 'beirc-status-display
    :display-time :command-loop
    :incremental-redisplay t
    ;;:height '(1 :line)
    :height 20
    :scroll-bars nil
    :background +black+
    :foreground +white+)
   (server
    (make-clim-application-pane
     :display-function
     (lambda (frame pane)
       (beirc-app-display frame pane (server-receiver *application-frame*)))
     :display-time nil
     :width 400 :height 600
     ;; added this, in the hopes that overwriting the :height argument
     ;; would allow more freedom to resize the tab-pane
     ;; (query). [2006/04/05:rpg]
     :min-height 100
     :incremental-redisplay t)))
  (:geometry :width 800 :height 600)
  (:top-level (clim:default-frame-top-level :prompt 'beirc-prompt))
  (:layouts
   (default
       (vertically ()
         (with-tab-layout ('receiver-pane :name 'query)
           ("*Not Connected*" server :presentation-type 'receiver-pane))
         (make-pane 'clim-extensions:box-adjuster-gadget)
         io
         (20 pointer-doc)
         (20                            ;<-- Sigh! Bitrot!
          status-bar)))))

;;; addition of optional argument allows debugging from outside the frame process. [2006/03/16:rpg]
(defun receiver-from-tab-page (page &optional (frame *application-frame*))
  (gethash page (tab-pages-to-receivers frame)))

(defvar *current-receiver-override*)

(defmacro with-current-receiver ((var receiver) &body body)
  `(let* ((*current-receiver-override* ,receiver)
          (,var *current-receiver-override*))
     ,@body))

(defmethod current-receiver ((frame beirc))
  (let ((receiver  (if (boundp '*current-receiver-override*)
                       *current-receiver-override*
                       (receiver-from-tab-page (tab-layout-enabled-page (find-pane-named frame 'query)) frame))))
    (if (typep receiver 'receiver)
        receiver
        nil)))

(defmethod current-connection ((frame beirc))
  (when (current-receiver frame)
    (connection (current-receiver frame))))

(defmethod server-receiver ((frame beirc)
                            &optional (connection (current-connection *application-frame*)))
  (cdr (assoc connection (server-receivers frame) :test #'connection=)))

(defmethod server-receiver-from-args ((frame beirc) server-name port nickname)
  (loop for (connection . receiver) in (server-receivers frame)
        if (and (equal (irc:nickname (irc:user connection)) nickname)
                (equal (irc:server-name connection) server-name)
                ;; TODO: no port.
                )
          do (return receiver)))

(defmethod (setf server-receiver) (newval (frame beirc)
                                          &optional (connection (current-connection *application-frame*)))
  (pushnew (cons connection newval) (slot-value frame 'server-receivers)
           :key #'car :test #'connection=))

(defmethod connection-process ((frame beirc) connection)
  (cdr (assoc connection (connection-processes frame) :test #'connection=)))

(defmethod (setf connection-process) (newval (frame beirc) connection)
  (pushnew (cons connection newval) (slot-value frame 'connection-processes)
           :key #'car :test #'connection=))

(defmethod remove-connection-process ((frame beirc) connection)
  (setf (slot-value *application-frame* 'connection-processes)
        (delete connection (connection-processes *application-frame*) :key #'car)))

(defmethod away-status ((frame beirc) connection)
  (gethash connection (presence frame)))

(defmethod (setf away-status) (newval (frame beirc) connection)
  (setf (gethash connection (presence frame)) newval))

(defmethod current-nickname (&optional (connection (current-connection *application-frame*)))
  (let ((user (when connection
                (irc:user connection))))
    (when user
     (irc:nickname user))))

(defvar *gui-process* nil)

(defvar *beirc-frame*)

(defun beirc-status-display (*application-frame* *standard-output*)
  (multiple-value-bind (seconds minutes hours)
      (decode-universal-time (get-universal-time))
    seconds
    (with-text-family (t :sans-serif)
      (format t "~:[~;~2,'0D:~2,'0D    ~]~A~:[~;(away)~] ~@[on ~A~]~@[ speaking to ~A~]~100T~D messages"
              (processes-supported-p) ; don't display time if threads are not supported
              hours minutes
              (current-nickname)
              (away-status *application-frame* (current-connection *application-frame*))
              (current-channel)
              (current-query)
              (length (current-messages))))))

(defun beirc-prompt (*standard-output* *application-frame*)
  (stream-increment-cursor-position *standard-output* 3 4)
  (surrounding-output-with-border (*standard-output* :shape :drop-shadow :move-cursor nil)
    (write-string "Beirc" *standard-output*)
    (let ((receiver (current-receiver *application-frame*)))
      (when receiver
        (write-string " " *standard-output*)
        (with-output-as-presentation (*standard-output* receiver 'receiver)
          (write-string (title receiver) *standard-output*)))))
  (stream-increment-cursor-position *standard-output* 10 0)
  #+nil (write-string " => " *standard-output*))

;; (defun format-message (prefix mumble)
;;   (write-line
;;    (format nil (concatenate 'string
;;                             "~A ~@<"
;;                             (with-output-to-string (bag)
;;                               (loop for c across mumble do
;;                                     (if (char= c #\~) (princ #\~ bag))
;;                                     (princ c bag)))
;;                             "~:@>")
;;            prefix)))

;;;

(defun pane-scrolled-to-bottom-p (pane)
  (multiple-value-bind (x y) (transform-position (sheet-transformation pane)
                                                 0 0)
    (declare (ignore x))
    (with-bounding-rectangle* (x1 y1 x2 y2) pane
      (declare (ignore x1 y1 x2))
      (with-bounding-rectangle* (ax1 ay1 ax2 ay2) (sheet-parent pane)
        (declare (ignore ax1 ay1 ax2))
        (<= (+ y y2) ay2)))))

(defun scroll-pane-to-bottom (pane)
  (scroll-extent pane 0 (max 0 (- (bounding-rectangle-height pane)
                                  (bounding-rectangle-height (sheet-parent pane))))))

(defun redraw-receiver (receiver)
  (let ((pane (actual-application-pane (pane receiver))))
    (setf (pane-needs-redisplay pane) t)
    (redisplay-frame-pane *application-frame* pane)))

(defun redraw-all-receivers ()
  (when (and (boundp '*application-frame*)
             (not (null *application-frame*)))
   (maphash (lambda (name receiver)
              (declare (ignore name))
              (redraw-receiver receiver))
            (receivers *application-frame*))))

;;; event handling methods

(defmethod handle-event ((frame beirc) (event new-sheet-event))
  (funcall (sheet-creation-closure event) frame))

(defmacro with-pane-kept-scrolled-to-bottom ((pane-form) &body body)
  "Ensure that the pane in PANE-FORM has the same scroll state
after BODY terminates as it had before:

If the pane is scrolled to some position before the end, it is
kept there.  If the pane is at the bottom of the pane, the
viewport is reset to the then-current bottom after BODY is
finished."
  (let ((pane (gensym))
        (bottom-p (gensym)))
    `(let* ((,pane ,pane-form)
            (,bottom-p (pane-scrolled-to-bottom-p ,pane)))
       (multiple-value-prog1 (progn ,@body)
                             (when ,bottom-p (scroll-pane-to-bottom ,pane))))))

(defmethod handle-event ((frame beirc) (event foo-event))
  ;; Hack:
  ;; Figure out if we are scrolled to the bottom.
  (let* ((receiver (receiver event))
         (pane (actual-application-pane (pane receiver)))
         (next-event (and (processes-supported-p) (event-peek (frame-top-level-sheet frame)))))
    (with-pane-kept-scrolled-to-bottom (pane)
      (update-drawing-options receiver)
      ;; delay redisplay until this is the last event in the queue
      ;; (for this event's receiver).
      (unless (and (typep next-event 'foo-event)
                   (eql (receiver next-event) receiver))
        (setf (pane-needs-redisplay pane) t)
        (redisplay-frame-panes frame)))
    (medium-force-output (sheet-medium pane)) ;###
    ))

(defmethod handle-event ((frame beirc) (event bar-event))
  (let ((pane (get-frame-pane frame 'status-bar)))
    (redisplay-frame-pane frame pane)
    (when *auto-close-inactive-query-windows-p*
      (com-remove-inactive-queries))
    (medium-force-output (sheet-medium pane))))

;;;

(defun beirc (&key (new-process (processes-supported-p)) (load-init-file t))
  (let* ((syms '(*package* *trace-output*))
         (vals (mapcar #'symbol-value syms))
         (program (lambda ()
                    (progv syms vals
                      (let* ((frame (make-application-frame 'beirc))
                             (ticker-process (when (processes-supported-p)
                                               (clim-sys:make-process (lambda () (ticker frame))
                                                                      :name "Beirc Ticker"))))
                        (setf *beirc-frame* frame)
			(when load-init-file
			  (load-user-init-file))
                        (run-frame-top-level frame)
                        (when (processes-supported-p)
                          (clim-sys:destroy-process ticker-process))
                        (disconnect-all frame "Client Quit"))))))
    ;; will start up a sound player, if you've configured one. [2006/04/06:rpg]
    (start-sound-server)
    (cond
      (new-process
        (setf *gui-process*
              (clim-sys:make-process program
                                     ;; added process name for easier debug...
                                     :name "BEIRC GUI process")))
      (t (setf *gui-process* (clim-sys:current-process))
         (funcall program)))))


(defun message-directed-to-me-p (message)
  (let ((body (car (last (irc:arguments message))))
        (my-nick (current-nickname (irc:connection message))))
    (search my-nick (or body ""))))

(defun interesting-message-p (message)
  (typep message '(or irc:irc-privmsg-message irc:irc-notice-message irc:irc-topic-message irc:irc-kick-message irc:ctcp-action-message)))

(defun post-message-to-receiver (frame message receiver)
  (let ((message-to-me-p (message-directed-to-me-p message))
	(interesting-message-p (interesting-message-p message)))
    (setf (messages receiver)
          (nconc (messages receiver) (list message)))
    (unless (eql receiver (current-receiver frame))
      (when interesting-message-p
	(incf (unseen-messages receiver)))
      ;; why is this only done when the receiver is not the current
      ;; one? [2006/04/17:rpg]
      (when message-to-me-p
	(incf (messages-directed-to-me receiver)))
      (incf (all-unseen-messages receiver)))
    (when (and (slot-boundp receiver 'pane) (pane receiver))
      (let* ((pane (actual-application-pane (pane receiver)))
	     (current-insert-position (bounding-rectangle-height pane)))
	(when (and message-to-me-p
		   (not (eql current-insert-position
			     (first (positions-mentioning-user receiver)))))
	  (push current-insert-position
		(positions-mentioning-user receiver)))))
    (run-post-message-hooks message frame receiver :message-directed-to-me message-to-me-p
			    :message-interesting-p interesting-message-p)
    (queue-beirc-event frame
                       (make-instance 'foo-event :sheet frame :receiver receiver))
    ;; is this effectively the same as (values)? [2006/04/17:rpg]
    nil))

(defun post-message (frame message)
  (let ((receiver (receiver-for-message message frame)))
    (cond ((consp receiver)
           (loop for 1-receiver in receiver
                 do (post-message-to-receiver frame message 1-receiver)))
          ((null receiver) nil)
          (t (post-message-to-receiver frame message receiver)))))

(defun ticker (frame)
  (loop 
    (clim-internals::event-queue-prepend (climi::frame-event-queue frame)
                                         (make-instance 'bar-event :sheet frame))
    (sleep 1)))

(defun join-missing-channels (frame &optional (connection (current-connection frame)))
  (let* ((server (when connection (irc:server-name connection))))
    (when server
      (loop for join-channel in (cdr (assoc server *auto-join-alist* :test #'equal))
         do (unless (gethash join-channel (receivers frame))
              (irc:join connection join-channel))))))

(defun load-user-init-file (&key (pathname *beirc-user-init-file*))
  (when (probe-file pathname)
    (let ((*package* #.*package*))
     (load pathname))
    (when *application-frame*
      (join-missing-channels *application-frame*)
      (redraw-all-receivers))))
(defun auto-focus-nicks ()
  (dolist (channel *auto-focused-alist*)
    (let ((receiver (find-receiver (car channel)
                                   (current-connection *application-frame*)
                                   *application-frame*)))
      (when receiver
        (setf (focused-nicks receiver)
              (remove-duplicates (append (cdr channel)
                                         (focused-nicks receiver))
                                 :test #'equal))
        (redraw-receiver receiver)))))


(define-beirc-command (com-reload :name t) ()
  (load-user-init-file)
  (auto-focus-nicks))

(define-beirc-command (com-identify :name t) (&key
                                              (password 'string :prompt "Password"
                                                        :default (cdr (assoc (irc:server-name (current-connection *application-frame*))
                                                                             *nickserv-password-alist*
                                                                             :test #'equal)))
                                              (who 'nickname :prompt "Target" :default "NickServ"))
  (when (null password)
    (setf password (accept 'string :prompt "Password")))
  (irc:privmsg (current-connection *application-frame*) who
               (format nil "IDENTIFY ~A" password)))

(define-beirc-command (com-query :name t) ((nick 'nickname :prompt "who"))
  (raise-receiver (intern-receiver nick (current-connection *application-frame*)
                                   *application-frame* :query nick)))

(define-beirc-command (com-raise :name t) ((receiver 'receiver :prompt "receiver"))
  (raise-receiver receiver))

(macrolet ((define-window-switcher (name keystroke direction predicate)
               `(define-beirc-command (,name :name t :keystroke ,keystroke)
                    ()
                  (let* ((tab-layout
			  (find-pane-named *application-frame* 'query))
			 (current-page (tab-layout-enabled-page tab-layout))
                         (list-of-pages (tab-layout-pages tab-layout))
                         (n-pages (length list-of-pages))
                         (current-page-position (position current-page list-of-pages))
                         (position current-page-position)
                         (predicate ,predicate)
                         (step-by ,direction)
                         (start-position (- current-page-position (* step-by n-pages)))
                         (end-position (+ current-page-position (* step-by n-pages))))
                    (when list-of-pages
                      (setf position
                            (loop for i = (+ step-by start-position) then (+ i step-by)
                                  until (or (= i end-position)
                                            (funcall predicate (nth (mod (+ n-pages i) n-pages) list-of-pages)))
                                  finally (return i)))
                      (switch-to-page (nth (mod (+ n-pages position) n-pages) list-of-pages)))))))
  (labels ((page-interesting-p (page)
             (let ((receiver (receiver-from-tab-page page)))
               (or (> (messages-directed-to-me receiver) 0)
                   (> (unseen-messages receiver) 0)))))
    (define-window-switcher com-interesting-window-next (#\Tab :control) 1 #'page-interesting-p)
    (define-window-switcher com-interesting-window-previous (:iso-left-tab :control :shift) -1 #'page-interesting-p)
    (define-window-switcher com-window-next (:next :control) 1 (constantly t))
    (define-window-switcher com-window-previous (:prior :control) -1 (constantly t))))

(define-beirc-command (com-insert-input :name t) ((input 'bad-input))
  (setf (incomplete-input (current-receiver *application-frame*))
        (concatenate 'string (incomplete-input (current-receiver *application-frame*))
                     input)))

(define-beirc-command (com-close :name t) ((receivers '(sequence receiver) :prompt "tab" :default (list (current-receiver *application-frame*))))
  (dolist (receiver receivers)
    (let* ((connection (connection receiver))
           (channel (irc:find-channel connection (title receiver))))
      (cond
        ((member receiver (server-receivers *application-frame*) :key #'cdr)
         (disconnect connection *application-frame* "Client Quit")
         (setf (slot-value *application-frame* 'server-receivers)
               (delete receiver (server-receivers *application-frame*) :key #'cdr)))
        (channel
         (irc:part connection channel))))
    (remove-receiver receiver *application-frame*)))

(macrolet ((define-scroller-command ((com-name keystroke) (top-var bot-var) next-pos-form &optional fallback-position)
               `(define-beirc-command (,com-name :name t :keystroke ,keystroke) ()
                  (let* ((pane (actual-application-pane (pane (current-receiver *application-frame*))))
                         (,bot-var (max 0 (- (bounding-rectangle-height pane)
                                             (bounding-rectangle-height (sheet-parent pane)))))
                         (,top-var 0)
                         (next-y-position ,next-pos-form))
                    (declare (ignorable ,top-var ,bot-var))
                    (scroll-extent pane 0 ,(if fallback-position
                                               `(if next-y-position
                                                   (max 0 (min next-y-position bottom))
                                                   (progn
                                                     (beep)
                                                     ,fallback-position))
                                               `(max 0 (min next-y-position bottom))))))))
  (define-scroller-command (com-previous-highlighted-message (:prior :shift)) (top bottom)
    (find-if (lambda (position)
               (< position (bounding-rectangle-min-y (pane-viewport-region pane))))
             (positions-mentioning-user (current-receiver *application-frame*)))
    top)
  (define-scroller-command (com-next-highlighted-message (:next :shift)) (top bottom)
    (loop for (this prev . rest) on (positions-mentioning-user (current-receiver *application-frame*))
          until (null prev)
          if (<= prev (bounding-rectangle-min-y (pane-viewport-region pane)) this)
            do (return this))
    bottom)

  (define-scroller-command (com-previous-page (:prior)) (top bottom)
    (let* ((pane (actual-application-pane (pane (current-receiver *application-frame*))))
           (pane-min-y (rectangle-min-y (pane-viewport-region pane)))
           (scroll-by (* (rectangle-height (pane-viewport-region pane)) 19/20)))
      (- pane-min-y scroll-by)))
  (define-scroller-command (com-next-page (:next)) (top bottom)
    (let* ((pane (actual-application-pane (pane (current-receiver *application-frame*))))
           (pane-min-y (rectangle-min-y (pane-viewport-region pane)))
           (scroll-by (* (rectangle-height (pane-viewport-region pane)) 19/20)))
      (+ pane-min-y scroll-by)))
  (define-scroller-command (com-top (:home :control)) (top bottom)
    top)
  (define-scroller-command (com-bottom (:end :control)) (top bottom)
    bottom))

(define-beirc-command (com-remove-inactive-queries :name t) ()
  (let ((receivers-to-close nil))
    (maphash (lambda (name receiver)
               (declare (ignore name))
               (when (and (not (member receiver (server-receivers *application-frame*) :key #'cdr))
                          (not (eql receiver (current-receiver *application-frame*)))
                          (= 0
                             (unseen-messages receiver) (all-unseen-messages receiver)
                             (messages-directed-to-me receiver)
                             (length (incomplete-input receiver)))
                          (null (irc:find-channel (connection receiver) (title receiver)))
                          (> (- (get-universal-time) (last-visited receiver)) *max-query-inactive-time*))
                 (push receiver receivers-to-close)))
             (receivers *application-frame*))
    (loop for receiver in (remove-duplicates receivers-to-close)
          do (remove-receiver receiver *application-frame*))))

(define-beirc-command (com-part :name t) ()
  (irc:part (current-connection *application-frame*)
            (title (current-receiver *application-frame*))))

(define-beirc-command (com-focus :name t) ((who 'nickname :prompt "who"))
  (pushnew who (current-focused-nicks) :test #'string=)
  (redraw-receiver (current-receiver *application-frame*)))

(define-beirc-command (com-ignore :name t) ((who 'nickname :prompt "who"))
  (with-pane-kept-scrolled-to-bottom ((actual-application-pane
                                       (pane (current-receiver *application-frame*))))
    (pushnew who (slot-value *application-frame* 'ignored-nicks) :test #'string=)
    (redraw-all-receivers)))

(define-beirc-command (com-unignore :name t) ((who 'ignored-nickname :prompt "who"))
  (with-pane-kept-scrolled-to-bottom ((actual-application-pane
                                       (pane (current-receiver *application-frame*))))
    (setf (slot-value *application-frame* 'ignored-nicks)
          (remove who (slot-value *application-frame* 'ignored-nicks)  :test #'string=))
    (redraw-all-receivers)))

(define-beirc-command (com-unfocus :name t) ((who 'nickname :prompt "who"))
  (setf (current-focused-nicks)
        (remove who (current-focused-nicks) :test #'string=))
  (redraw-receiver (current-receiver *application-frame*)))

(define-beirc-command (com-whois :name t) ((who 'nickname :prompt "who"))
  (irc:whois (current-connection *application-frame*) who))

(define-beirc-command (com-eval :name t) ((command 'string :prompt "command")
                                          (args '(sequence string) :prompt "arguments"))
  (multiple-value-bind (symbol status) (find-symbol (string-upcase command) :irc)
    (when (eql status :external)
      (apply symbol (current-connection *application-frame*) (coerce args 'list)))))

(define-beirc-command (com-load :name t)
    ((pathname 'pathname :prompt "pathname")
     &key
     (remove-type-if-is-lisp-p 'boolean :default t))
  (when (probe-file pathname)
    (load (if (and remove-type-if-is-lisp-p
                   (string-equal (pathname-type pathname)
                                 "lisp"))
              (make-pathname :type nil :defaults pathname)
              pathname))))

(define-beirc-command (com-everywhere :name t) ((command 'command :prompt "command"))
  (mapc (lambda (server-receiver)
          (with-current-receiver (receiver (cdr server-receiver))
            (execute-frame-command *application-frame* command)))
        (server-receivers *application-frame*)))

(defun make-fake-irc-message (message-type &key command arguments
                              (source (current-nickname))
                              trailing-argument)
  (make-instance message-type
     :received-time (get-universal-time)
     :connection (current-connection *application-frame*)
     :arguments `(,@arguments ,trailing-argument)
     :command command
     :HOST "localhost"
     :USER "localuser"
     :SOURCE source))

(define-beirc-command (com-topic :name t) (&key (topic 'mumble :prompt "New topic"))
  (if (and (not (null topic))
           (not (equal topic "")))
        (irc:topic- (current-connection *application-frame*) (target) topic)
        (post-message *application-frame*
                      (make-fake-irc-message 'irc:irc-rpl_topic-message
                         :command "332"
                         :arguments `("=" ,(target))
                         :trailing-argument (irc:topic
                                             (irc:find-channel
                                              (current-connection *application-frame*)
                                              (target)))))))

(define-beirc-command (com-op :name t) ((who 'nickname :prompt "who"))
  (irc:op (current-connection *application-frame*) (target) who))

(define-beirc-command (com-deop :name t) ((who 'nickname :prompt "who"))
  (irc:deop (current-connection *application-frame*) (target) who))

(define-beirc-command (com-show-ban-list :name t) ()
  (irc:ban (current-connection *application-frame*) (target) ""))

(define-beirc-command (com-ban-nick :name t) ((who 'nickname :prompt "who"))
  (irc:ban (current-connection *application-frame*) (target) (format nil "~A!*@*" who)))

(define-beirc-command (com-ban-hostmask :name t) ((who 'hostmask :prompt "hostmask"))
  (irc:ban (current-connection *application-frame*) (target) who))

(define-beirc-command (com-unban-hostmask :name t) ((who 'hostmask :prompt "hostmask"))
  (irc:unban (current-connection *application-frame*) (target) who))

(define-beirc-command (com-unban-nick :name t) ((who 'nickname :prompt "who"))
  (irc:unban (current-connection *application-frame*) (target) (format nil "~A!*@*" who)))

(define-beirc-command (com-kick :name t) ((who 'nickname :prompt "who") (reason 'mumble :prompt "reason"))
  (irc:kick (current-connection *application-frame*) (target) who reason))

(define-beirc-command (com-names :name t) ()
  (irc:names (current-connection *application-frame*) (target)))

(define-beirc-command (com-away :name t) ((reason 'mumble
						  :prompt (if (away-status *application-frame* (current-connection *application-frame*))
							      "reason: to come back from away use /back instead of away"
							      "reason")))
  (irc:away (current-connection *application-frame*) reason))

(define-beirc-command (com-back :name t) ()
  (irc:away (current-connection *application-frame*) ""))

(defmethod command-enabled ((command-name (eql 'com-back)) frame)
  "Turn off the back command when it's not appropriate -- i.e., when you are 
not away."
  (away-status frame (current-connection frame)))

(defmethod command-enabled ((command-name (eql 'com-away)) frame)
  "Turn off the away command when you are already away."
  (not (away-status frame (current-connection frame))))

(define-beirc-command (com-quit :name t) (&key (reason 'mumble :prompt "reason" :default "Client Quit"))
  (disconnect-all *application-frame* reason)
  (frame-exit *application-frame*))

(define-beirc-command (com-disconnect :name t) ((reason 'mumble :prompt "reason"))
  (when (current-connection *application-frame*)
    (disconnect (current-connection *application-frame*) *application-frame* reason)))

(define-beirc-command (com-switch-timestamp-orientation :name t) ()
  (setf *timestamp-column-orientation* (if (eql *timestamp-column-orientation* :left)
                                           :right
                                           :left))
  (redraw-all-receivers))

(defun target (&optional (*application-frame* *application-frame*))
  (or (current-query)
      (current-channel)))

(defun send-private-message (target what)
  (unless (or (string= what "")
              (null target))
    (post-message *application-frame*
                  (make-fake-irc-message 'irc:irc-privmsg-message
                                         :trailing-argument what
                                         :arguments (list target)
                                         :command "PRIVMSG"))
    (irc:privmsg (current-connection *application-frame*) target what)))

(define-beirc-command (com-msg :name t)
    ((target '(OR nickname channel) :prompt "who")
     (what 'mumble :prompt "what"))
  (send-private-message target what))

(define-beirc-command (com-say :name t)
    ((what 'mumble))
  (com-msg (target) what))

(define-beirc-command (com-me :name t) ((what 'mumble :prompt nil))
  (let ((m (make-fake-irc-message 'irc:ctcp-action-message
                                    :trailing-argument
                                    (format nil "~AACTION ~A~A" (code-char 1) what (code-char 1))
                                    :arguments (list (target))
                                    :command "PRIVMSG"))) ;###
      (post-message *application-frame* m)
      (irc:privmsg (current-connection *application-frame*) (target)
                   (format nil "~AACTION ~A~A" (code-char 1) what (code-char 1)))))

(define-beirc-command (com-nick :name t) ((new-nick 'string :prompt "new nick"))
  (let ((connection (current-connection *application-frame*)))
    (cond
      (connection
	(irc:nick connection new-nick))
      (t
	(format *standard-input* "Default nickname set to ~A.~%" new-nick)
	(setf *default-nick* new-nick)))))

(define-beirc-command (com-browse-url :name t) ((url 'url :prompt "url"))
  (handler-case
      ;; this is probably not ENTIRELY The Right Thing for Allegro.  I
      ;; think for Allegro it would be handy to keep the process-id
      ;; around so that we can call the reap-os-subprocesses
      ;; function...  Not sure how to do this. [2006/03/14:rpg]
      ;; actually, this is true for all of these invocations. doesn't
      ;; bite us in sbcl, though. [2006/03/15:asf]
      #+allegro (excl:run-shell-command (format nil "~A \'~A\'" *default-web-browser* url) :wait nil)      
      #+sbcl (sb-ext:run-program *default-web-browser* `(,url) :wait nil)
      #+openmcl (ccl:run-program *default-web-browser* `(,url) :wait nil)
      #-(or sbcl openmcl allegro) (progn (format *debug-io* "Can't figure out how to browse to url ~A~%" url)
                                         (beep))
    #+sbcl (simple-error (e) (format t "~a" e))))

(define-presentation-to-command-translator incomplete-input-to-input-translator
    (bad-input com-insert-input beirc
                      :menu nil
                      :gesture :select
                      :documentation "Append this to the input line"
                      :pointer-documentation "Append this to the input line"
                      :priority 10)
    (object)
  (list object))

(define-presentation-to-command-translator nickname-to-ignore-translator
    (nickname com-ignore beirc
              :menu t
              :gesture nil
              :documentation "Ignore this user"
              :pointer-documentation "Ignore this user"
              :tester ((object)
                       (not (find object (slot-value *application-frame* 'ignored-nicks)
                                  :test 'string-equal))))

    (object)
  (list object))

(define-presentation-to-command-translator nickname-to-unignore-translator
    (nickname com-unignore beirc
              :menu t
              :gesture nil
              :documentation "Unignore this user"
              :pointer-documentation "Unignore this user"
              :tester ((object)
                       (find object (slot-value *application-frame* 'ignored-nicks)
                             :test 'string-equal)))
    (object)
  (list object))

(define-presentation-to-command-translator nickname-to-focus-translator
    (nickname com-focus beirc
              :menu t
              :gesture nil
              :documentation "Focus this user"
              :pointer-documentation "Focus this user"
              :tester ((object)
                       (not (find object (current-focused-nicks)
                                  :test 'string-equal))))
    (object)
  (list object))

(define-presentation-to-command-translator nickname-to-unfocus-translator
    (nickname com-unfocus beirc
              :menu t
              :gesture nil
              :documentation "Unfocus this user"
              :pointer-documentation "Unfocus this user"
              :tester ((object)
                       (find object (current-focused-nicks)
                             :test 'string-equal)))
    (object)
  (list object))

(define-presentation-to-command-translator nickname-to-query-translator
    (nickname com-query beirc
              :menu t
              :gesture :describe
              :documentation "Query this user"
              :pointer-documentation "Query this user")
    (object)
  (list object))

(define-presentation-to-command-translator nickname-to-kick-translator
    (nickname com-kick beirc
              :menu t
              :gesture nil
              :documentation "Kick this user"
              :pointer-documentation "Kick this user")
    (object)
  (list object
        ;; XXX: not the best way to do it. McCLIM should recognize
        ;; that this is a partial command and query for the rest of
        ;; the args itself.
        (accept 'mumble :prompt " Reason")))

(define-presentation-to-command-translator nickname-to-ban-nick-translator
    (nickname com-ban-nick beirc
              :menu t
              :gesture nil
              :documentation "Ban this user's nickname"
              :pointer-documentation "Ban this user's nickname")
    (object)
  (list object))

(define-presentation-to-command-translator hostmask-to-ban-translator
    (hostmask com-ban-hostmask beirc
              :menu t
              :gesture nil
              :documentation "Ban this hostmask"
              :pointer-documentation "Ban this hostmask")
    (object)
  (list object))

(define-presentation-to-command-translator hostmask-to-unban-translator
    (hostmask com-unban-hostmask beirc
              :menu t
              :gesture nil
              :documentation "Unban this hostmask"
              :pointer-documentation "Unban this hostmask")
    (object)
  (list object))

(define-presentation-to-command-translator nickname-to-ban-hostmask-translator
    (nickname com-ban-hostmask beirc
              :menu t
              :gesture nil
              :documentation "Ban this user's hostmask"
              :pointer-documentation "Ban this user's hostmask")
    (object)
  (list object))

(define-presentation-to-command-translator nickname-to-whois-translator
    (nickname com-whois beirc
              :gesture :select
              :menu t
              :documentation "Perform WHOIS query on user"
              :pointer-documentation "Perform WHOIS query on user"
              :priority 10)
    (object)
  (list object))

(define-presentation-to-command-translator channel-to-join-translator
    (channel com-join beirc
              :gesture :describe
              :menu t
              :documentation "Join this channel"
              :pointer-documentation "Join this channel"
              :priority 10)
    (object)
  (list object))

(define-presentation-to-command-translator url-to-browse-url-translator
    (url com-browse-url beirc :pointer-documentation "Browse URL"
	 :documentation "Browse URL")
   (presentation)
   (list (presentation-object presentation)))

;;; this translator refines the previous one, just giving a more
;;; precise pointer documentation.  If I were smarter about
;;; presentation types, I bet I could fold this into the previous
;;; translator. [2006/04/18:rpg]
(define-presentation-to-command-translator meme-url-to-browse-url-translator
    (meme-url com-browse-url beirc
              :documentation "Browse meme log"
              :pointer-documentation "Browse meme log"
	      ;; override url-to-browse-url-translator
	      :priority 1)
   (presentation)
  (list (presentation-object presentation)))


(define-presentation-translator receiver-pane-to-receiver-translator
    (receiver-pane receiver beirc
       :documentation ((object stream)
                       (format stream "Reiceiver: ~A"
                               (title (receiver-from-tab-page
                                       (sheet-to-page object))))))
    (object)
  (receiver-from-tab-page (sheet-to-page object)))

(define-presentation-translator receiver-pane-to-channel-translator
    (receiver-pane channel beirc
       :documentation ((object stream)
                       (format stream "Channel: ~A"
                               (channel (sheet-to-page object))))
       :tester ((object)
                (channel (receiver-from-tab-page (sheet-to-page object)))))
    (object)
  (channel (sheet-to-page object)))

(define-presentation-translator receiver-to-channel-translator
    (receiver channel beirc
       :documentation ((object stream)
                       (format stream "Channel: ~A"
                               (channel object)))
       :tester ((object)
                (channel object)))
    (object)
    (channel object))

;;;(define-presentation-to-command-translator close-via-tab-button
;;;    (tab-pane com-close beirc
;;;	      :menu t
;;;	      :gesture nil
;;;	      :documentation "Close this channel"
;;;	      :pointer-documentation "Close this channel")
;;;    (object)
;;;  (list (channel object)))

(define-presentation-translator nickname-to-hostmask-translator
    (nickname hostmask beirc
              :tester ((object context-type)
                       (declare (ignore object))
                       (presentation-subtypep context-type 'hostmask)))
    (object)
  (let ((hostname (irc:hostname (irc:find-user (current-connection *application-frame*) object))))
    (if (zerop (length hostname))
        (format nil "~A!*@*" object)
        (format nil "*!*@~A" hostname))))

(define-beirc-command (com-join :name t) ((channel 'channel :prompt "channel"))
  (raise-receiver (intern-receiver channel (current-connection *application-frame*)
                                   *application-frame* :channel channel))
  (irc:join (current-connection *application-frame*) channel))

(defun connection= (connection1 connection2)
  ;; TODO: should compare by network, not by server name.
  ;; TODO: also, there is no port that we could compare.
  (and (equal (irc:nickname (irc:user connection1)) (irc:nickname (irc:user connection2)))
       (equal (irc:server-name connection1) (irc:server-name connection2))))

(define-beirc-command (com-connect :name t)
    ((server 'string :prompt "Server")
     &key
     (nick 'string :prompt "Nick name" :default *default-nick*)
     (realname 'string :prompt "Real name (phrase)" :default *default-realname*)
     (pass 'string :prompt "Password" :default nil)
     (port 'number :prompt "Port" :default 6667))
  (let ((success nil)
        (maybe-server-receiver (server-receiver-from-args *application-frame* server port nick)))
    (or (and maybe-server-receiver (connection-open-p maybe-server-receiver))
        (let* ((frame *application-frame*)
               (connection (apply #'irc:connect
                                  :nickname nick :server server :connection-type 'beirc-connection :port (or port :default)
				  ;; this works because the default in
				  ;; cl-irc is NIL, so we don't have
				  ;; to handle this specially as with
				  ;; password.
				  :realname realname
                                  (if (null pass)
                                      nil
                                      `(:password ,pass))))
               (server-receiver (if maybe-server-receiver
                                    (prog1 maybe-server-receiver
                                           (reinit-receiver-for-new-connection maybe-server-receiver
                                                                               connection))
                                    (intern-receiver (format nil "~A on ~A:~A" nick server port) connection frame))))
          (unwind-protect
              (progn
                (setf (irc:client-stream connection) (make-broadcast-stream))
                (when (sheet-to-page (find-pane-named frame 'server))
                  (remove-page (sheet-to-page (find-pane-named frame 'server))))
                (setf (server-receiver frame connection) server-receiver)
                (setf (ui-process *application-frame*) (current-process))
                (if (processes-supported-p)
                    (setf (connection-process *application-frame* connection)
                          (clim-sys:make-process #'(lambda ()
                                                     (restart-case
                                                         (irc-event-loop frame connection)
                                                       (disconnect ()
                                                         :report "Terminate this connection"
                                                         (disconnect connection frame "Client Disconnect"))))
                                                 :name "IRC Message Muffling Loop"))
                    (irc:start-background-message-handler connection))
                (setf success t)
                connection)
            (unless success
              (disconnect connection frame "Client error.")))))
    ;; added auto-identify [2006/05/09:rpg]
    (when success
      (when (member server *auto-identify-list* :test 'equalp)
	(com-identify)))
    ))

(defun disconnect (connection frame reason)
  (let ((*application-frame* frame))
    (raise-receiver (server-receiver frame connection))
    (when (connection-process frame connection)
      (irc:quit connection reason)
      (when (not (eql (clim-sys:current-process)
                      (connection-process frame connection)))
        (destroy-process (connection-process frame connection)))
      (remove-connection-process frame connection))))

(defun disconnect-all (frame reason)
  (loop for (conn . receiver) in (server-receivers frame)
        do (disconnect (connection receiver) frame reason)))

;;; irc command and mumble reading

(defun save-input-line (stream frame)
  (when (current-receiver frame)
    (let ((buffer (stream-input-buffer stream)))
      (setf (incomplete-input (current-receiver frame))
            (with-output-to-string (s)
              (loop for elt across buffer
                    if (characterp elt)
                      do (write-char elt s))))
      (incomplete-input (current-receiver frame)))))

(define-condition invoked-command-by-clicking ()
  ()
  (:documentation "A condition that is invoked when the user
  clicked on a command or on a presentation that invokes a
  presentation-to-command translator. typically,
  read-frame-command will handle it and save the input line."))

#+mcclim
(defmethod frame-input-context-button-press-handler ((frame beirc) stream event)
  "Unportable method for saving the current input buffer in case
the user invokes a command while typing."
  (let* ((x (pointer-event-x event))
         (y (pointer-event-y event))
         (window (event-sheet event))
         (presentation (frame-find-innermost-applicable-presentation frame *input-context* stream x y :event event)))
    (if presentation
        (multiple-value-bind (p translator context)
            (climi::find-innermost-presentation-match *input-context*
                                                      presentation
                                                      *application-frame*
                                                      (event-sheet event)
                                                      x y
                                                      event
                                                      0
                                                      nil)
          (if p
              (multiple-value-bind (object ptype options)
                  (call-presentation-translator translator
                                                p
                                                (input-context-type context)
                                                *application-frame*
                                                event
                                                window
                                                x y)
                (when ptype
                  (when (and (presentation-subtypep ptype 'command)
                             (boundp 'climi::*current-input-stream*) climi::*current-input-stream*)
                    (restart-case (signal 'invoked-command-by-clicking)
                      (acknowledged ())))
                  (funcall (cdr context) object ptype event options)))
              (call-next-method)))
        (call-next-method))))

(defmethod read-frame-command ((frame beirc) &key (stream *standard-input*))
  (let ((bad-input nil))
    (unwind-protect
         (clim:with-input-editing (stream)
           (when (and (current-receiver frame) (incomplete-input (current-receiver frame))
                      (not (stream-rescanning-p stream)))
             (replace-input stream
                            (incomplete-input (current-receiver frame))
                            :rescan t))
           (with-input-context ('command) (object)
               (with-command-table-keystrokes (*accelerator-gestures* (frame-command-table frame))
                 (catch 'keystroke-command
                   (let ((force-restore-input-state nil))
                     (labels ((reset-saved-input ()
                                (when (current-receiver frame)
                                  (setf (incomplete-input (current-receiver frame)) ""))))
                       (handler-bind ((accelerator-gesture
                                       (lambda (gesture)
                                         (save-input-line stream frame)
                                         (throw 'keystroke-command (lookup-keystroke-command-item
                                                                    (accelerator-gesture-event gesture)
                                                                    (frame-command-table frame)))))
                                      (abort-gesture
                                       (lambda (gesture)
                                         (declare (ignore gesture))
                                         (reset-saved-input)
                                         (setf force-restore-input-state nil)))
                                      (invoked-command-by-clicking
                                       (lambda (cond)
                                         (declare (ignore cond))
                                         (save-input-line stream frame)
                                         (setf force-restore-input-state t)
                                         (invoke-restart 'acknowledged))))
                         (let ((c (clim:read-gesture :stream stream :peek-p t)))
                           (multiple-value-prog1
                               (cond ((eql c #\/)
                                      (handler-case
                                          (progn
                                            (clim:read-gesture :stream stream)
                                            (accept 'command :stream stream :prompt "" :prompt-mode :raw))
                                        (simple-completion-error (c)
                                          #+mcclim
                                          (let ((preliminary-line (save-input-line stream frame)))
                                            (when (current-receiver frame)
                                             (setf (incomplete-input (current-receiver frame))
                                                   (subseq preliminary-line 0
                                                           (search (climi::completion-error-input-so-far c)
                                                                   preliminary-line))
                                                   bad-input (subseq preliminary-line
                                                                     (search (climi::completion-error-input-so-far c)
                                                                             preliminary-line))
                                                   force-restore-input-state t)))
                                          (beep)
                                          nil)))
                                     (t
                                      (list 'com-say (accept 'mumble :history 'mumble :prompt nil :stream stream))))
                             (if force-restore-input-state
                                 (setf force-restore-input-state nil)
                                 (reset-saved-input)))))))))
             (command
              (save-input-line stream frame)
              object)))
      (window-clear stream)
      (when bad-input
        (format stream "Bad input \"")
        (with-drawing-options (stream :ink +red3+)
          (present bad-input 'bad-input :stream stream))
        (format stream "\"."))))) 

(defun irc-event-loop (frame connection)
  ;; keep unrecognized responses from crashing BEIRC [2006/04/21:rpg]
  (handler-bind ((cl-irc:no-such-reply #'(lambda (c)
					   (continue c))))
    (let ((*application-frame* frame))
      (unwind-protect (irc:read-message-loop connection)
	(setf (connection-open-p (server-receiver frame connection)) nil)
	(irc:remove-all-hooks connection)      
	(irc:irc-message-event connection
			       (make-fake-irc-message 'irc-connection-closed-message
						      :command "Connnection closed"
						      :source (irc:server-name connection)))))))

;;; proposed addition to auto-connect to servers in the
;;; *auto-connect-list* [2006/03/21:rpg]
(defmethod adopt-frame :after (frame-manager (frame beirc))
  (declare (ignore frame-manager))
  (loop for server in *auto-connect-list*
	do (execute-frame-command frame
		   `(com-connect ,server))))


(defmethod frame-exit :after ((frame beirc))
  "Shut off the sound server process, if necessary."
  (stop-sound-server))
