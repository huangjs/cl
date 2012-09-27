(in-package :beirc)

(declaim  (optimize (debug 2) (speed 0)
         (space 0)))

(defvar *max-preamble-length* 0)

(defvar *current-message*)

(defparameter *colors* `((0 . (:ink ,+white+))
                         (1 . (:ink ,+black+))
                         (2 . (:ink ,+blue+))
                         (3 . (:ink ,+green+))
                         (4 . (:ink ,+red+))
                         (5 . (:ink ,+brown+))
                         (6 . (:ink ,+purple+))
                         (7 . (:ink ,+orange+))
                         (8 . (:ink ,+yellow+))
                         (9 . (:ink ,+light-green+))
                         (10 . (:ink ,+dark-cyan+))
                         (11 . (:ink ,+cyan+))
                         (12 . (:ink ,+royal-blue+))
                         (13 . (:ink ,+pink+))
                         (14 . (:ink ,+grey+))
                         (15 . (:ink ,+light-grey+))
                         ("" . (normal))
                         ("" . (underline))
                         ("" . (inverse))
                         ("" . (bold))))

(defparameter *color-scanner* (cl-ppcre:create-scanner "[0-9]{1,2}(,[0-9]{1,2}){0,1}||||"))

(define-presentation-type url ()
  :inherit-from 'string)

(define-presentation-type meme-url ()
  :inherit-from 'url)


(defun present-url (url)
  (let* ((clhs-base "http://www.lispworks.com/reference/HyperSpec/")
         (start (search clhs-base url)))
    (cond (start
           (let* ((clhs-page (subseq url (+ start (length clhs-base))))
                  (new-url (concatenate 'string *hyperspec-base-url* clhs-page)))
             (write-string (subseq url 0 start))
             (with-output-as-presentation (t new-url 'url)
               (format t "clhs://~A" clhs-page))))
	  ((> (length url) *default-fill-column*)
	   (let ((new-url
		  (concatenate 'string
			       (subseq url 0 (floor *default-fill-column* 2))
			       "..."
			       (subseq url (- (length url)
					      (- (floor *default-fill-column* 2) 3))))))
	     (with-output-as-presentation (t url 'url)
	       (write-string new-url))))
	  (t (present url 'url)))))

(defun message-from-focused-nick-p (message receiver)
  (member (irc:source message) (focused-nicks receiver) :test #'string=))

(defun message-from-ignored-nick-p (message receiver)
  (declare (ignore receiver))
  (member (irc:source message) (slot-value *application-frame* 'ignored-nicks)
          :test #'string=))

(defun +boolean (initial-value &rest booleans)
  (loop for value = initial-value then (+ (ash value 1)
                                          (if boolean 1 0))
        for boolean in booleans
        finally (return value)))

(defun invoke-formatting-message (stream message receiver preamble-writer message-body-writer)
  (let* ((*current-message* message)
         (stream* (if (eql stream t) *standard-output* stream))
         (width (- (floor (bounding-rectangle-width (sheet-parent stream*))
                          (clim:stream-string-width stream* "X"))
                   2)))
    (labels ((make-meme-url (message)
               (format nil "http://meme.b9.com/cview.html?channel=~A&utime=~A#utime_requested"
                       (string-trim '(#\#) (channel receiver))
                       (irc:received-time message)))
             (format-timestamp (message)
               (format stream* "[~2,'0D:~2,'0D]"
                       (nth-value 2 (decode-universal-time (irc:received-time message)))
                       (nth-value 1 (decode-universal-time (irc:received-time message)))))
             (output-timestamp-column (position)
               (when (eql position *timestamp-column-orientation*)
                 (formatting-cell (stream* :align-x :left)
                   (with-drawing-options (stream* :ink +gray+)
                     (if (and *meme-log-bot-nick*
                              (irc:find-user (connection receiver) *meme-log-bot-nick*)
                              (member (title receiver)
                                      (irc:channels (irc:find-user (connection receiver) *meme-log-bot-nick*))
                                      :test #'equal
                                      :key #'irc:name))
                         (with-output-as-presentation (stream* (make-meme-url message) 'meme-url)
                           (format-timestamp message))
                         (format-timestamp message)))))))
      (updating-output (stream* 
                        :cache-value
                        (+boolean (sxhash (list message                                      
                                                width
                                                *max-preamble-length*
                                                *default-fill-column*))
                                  (message-from-focused-nick-p message receiver)
                                  (message-from-ignored-nick-p message receiver)
                                  (eql *timestamp-column-orientation* :left))                        
                        :cache-test #'eql)
        (formatting-row (stream*)
          (output-timestamp-column :left)
          (formatting-cell (stream* :align-x :right :min-width '(3 :character))
            (with-drawing-options (stream* :ink +dark-red+)
              (funcall preamble-writer)))
          (formatting-cell (stream* :align-x :left
                                    :min-width `(,*default-fill-column* :character))
            (funcall message-body-writer))
          (output-timestamp-column :right))))))

(defmacro formatting-message ((stream message receiver)
                              (&body preamble-column-body)
                              (&body message-body-column-body))
  ;; Fix me: This usage of UPDATING-OUTPUT is sub-optimal and ugly!
  ;; (asf 2005-09-17: is it still?)
  `(invoke-formatting-message ,stream ,message ,receiver
                              (lambda ()
                                ,@preamble-column-body)
                              (lambda ()
                                ,@message-body-column-body)))

;;; for optimal indentation, use (put 'formatting-message 'common-lisp-indent-function 1)

(defun strip-punctuation (word)
  (if (= (length word) 0)
      (values word "")
      (let ((last-char (char word (1- (length word)))))
       (case last-char
         ((#\: #\, #\. #\; #\) #\] #\} #\> #\? #\! #\" #\')
          (values (subseq word 0 (1- (length word)))
                  (string last-char)))
         (otherwise (values word ""))))))

(defun strip-preceding-punctuation (word)
  (if (= (length word) 0)
      (values word "")
      (let ((first-char (char word 0)))
       (case first-char
         ((#\@ #\+ #\< #\()
          (values (subseq word 1)
                  (string first-char)))
         (otherwise (values word ""))))))

(defun extract-color (string)
  (multiple-value-bind (start end)
      (cl-ppcre:scan *color-scanner*
                     string)
    (if start
        (let* ((message (subseq string end))
               (color-code (subseq string start end))
               (color-code (or (cl-ppcre:all-matches-as-strings "[0-9]{1,2}"
                                                                color-code)
                               (list (cl-ppcre:scan-to-strings "|||"
                                                               color-code))))
               (foreground (or (parse-integer (car color-code)
                                             :junk-allowed t)
                              (car color-code)))
               (background (when (cadr color-code)
                             (parse-integer (cadr color-code)
                                            :junk-allowed t)))
               (foreground (cdr (assoc foreground
                                       *colors*
                                       :test #'equal)))
               (background (cdr (assoc background
                                       *colors*
                                       :test #'equal))))
          (values message
                  foreground
                  background
                  ))
        string)))

(defun split-before (delimiter string)
  (let ((matches (cl-ppcre:all-matches delimiter string)))
    (if matches
        (loop for (a b c) on matches by #'cddr
              collecting (subseq string a c) into strings
              finally (return (if (zerop (car matches))
                                  strings
                                  (cons (subseq string
                                                0
                                                (car matches))
                                        strings))))
        (list string))))

(defmacro do-colored-string ((string-var str) &body body)
  `(dolist (part (split-before *color-scanner* ,str))
     (multiple-value-bind (message foreground background)
         (extract-color part)
       (cond (*filter-colors* nil)
             ((equal (car foreground)
                     'normal)
              (setf foreground-color +black+
                    background-color +white+))
             ((equal (car foreground)
                     :ink)
              (setf foreground-color
                    (cadr foreground))
              (when background
                (setf background-color (cadr background))))
             ((equal (car foreground)
                     'bold)
              (setf bold (if bold nil :bold)))
             ((equal (car foreground)
                     'underline)
              (setf underline (not underline)))
             ((equal (car foreground)
                     'inverse)
              (setf inverse (not inverse))))
       (with-drawing-options (t :text-face bold)
         (let ((,string-var message))
           (if inverse
               (with-irc-colors (background-color foreground-color underline)
                 ,@body)
               (with-irc-colors (foreground-color background-color underline)
                 ,@body)))))))

(defmacro with-irc-colors ((foreground background underlinep) &body body)
  `(with-sheet-medium (medium *standard-output*)
    (let ((record (with-new-output-record (t)
                    (with-drawing-options (t :ink ,foreground)
                      ,@body))))
      (with-bounding-rectangle* (left top right bottom)
          record
        (unless (equal left right)
          (unless (equal ,background +white+)
            (with-identity-transformation (medium)
              (draw-rectangle* *standard-output*
                               left
                               top
                               right
                               bottom
                               :filled t
                               :ink ,background)
              (replay-output-record record *standard-output*)
              (setf (stream-cursor-position *standard-output*)
                    (values right top))))
          (when ,underlinep
            (draw-line* *standard-output* left (- bottom 1)
                        (- right 1) (- bottom 1)
                        :ink ,foreground)))
        record))))

(defun format-message* (mumble &key (limit *default-fill-column*) (start-length 0))
  (let ((foreground-color (medium-ink *standard-output*))
        (background-color (medium-background *standard-output*))
        (bold nil)
        (underline nil)
        (inverse nil))
    (let ((column start-length))
      (loop for (word . rest) on (split-sequence:split-sequence #\Space mumble)
            do (do-colored-string (word word)
                 (incf column (length word))
                 (when (> column limit)
                   (setf column (length word))
                   (terpri))
                 (multiple-value-bind (%word stripped-preceding-punctuation) (strip-preceding-punctuation word)
                   (multiple-value-bind (word% stripped-punctuation) (strip-punctuation %word)
                     (write-string stripped-preceding-punctuation)
                     (cond
                       ((or (search "http://" word%) (search "https://" word%))
                        (present-url word%))
                       ((or
                         (nick-equals-my-nick-p word% (irc:connection *current-message*))
                         (and (current-connection *application-frame*)
                              (irc:find-user (current-connection *application-frame*) word%)))
                        (present word% 'nickname))
                       ((channelp word%) (present word% 'channel))
                       (t (write-string word%)))
                     (write-string stripped-punctuation))))
            do (unless (or (null rest) (>= column limit))
                 (do-colored-string (s " ")
                   (write-string s)
                   (incf column))))
      (terpri))))

;;; privmsg-like messages

(defmethod trailing-argument* (message)
  (car (last (irc:arguments message))))

(defmethod trailing-argument* ((message cl-irc:ctcp-action-message))
  (or
   (ignore-errors                       ;###
     (let ((p1 (position #\space (car (last (irc:arguments message))))))
       (subseq (car (last (irc:arguments message)))
	       (1+ p1)
	       (1- (length (car (last (irc:arguments message))))))))
   "#Garbage parsing message#"))

(defun print-privmsg-like-message (message receiver start-string end-string)
  (with-drawing-options
      (*standard-output*
       :ink (if (string-equal "localhost" (irc:host message))
		+blue4+
		+black+))
    (unless (message-from-ignored-nick-p message receiver)
      (with-text-face
	  (*standard-output*
	   (if (message-from-focused-nick-p message receiver) :bold :roman))
        (irc:destructuring-arguments (&rest :ignored &req body) message
          (formatting-message (t message receiver)
            ((write-string start-string *standard-output*)
             (present (irc:source message) 'unhighlighted-nickname)
             (write-string end-string *standard-output*))
            ((format-message* body))))))))

(defmethod print-message ((message irc:IRC-PRIVMSG-MESSAGE) receiver)
  (print-privmsg-like-message message receiver "<" ">"))

(defmethod print-message ((message irc:IRC-NOTICE-MESSAGE) receiver)
  (print-privmsg-like-message message receiver "-" "-"))

(defmethod print-message ((message irc:ctcp-action-message) receiver)
  (let ((source (cl-irc:source message)))
    (formatting-message (t message receiver)
        ((format t "*"))
        ((present source 'unhighlighted-nickname)
         (format t " ")
         (format-message* (trailing-argument* message)
                          :start-length (+ 2 (length source)))))))

(defmethod print-message ((message irc:ctcp-version-message) receiver)
  (let ((source (cl-irc:source message)))
    (formatting-message (t message receiver)
      ((format t "    "))
      ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
         (present source 'unhighlighted-nickname)
         (format t " ")
         (format-message* "asked for your IRC client version" :start-length (+ 2 (length source))))))))

;;; server messages

(macrolet ((define-server-message-printer ((&rest message-specs))
               `(progn
                  ,@(loop for (message-type . message-name) in message-specs
                          collect
                          `(defmethod print-message ((message ,message-type) receiver)
                             (irc:destructuring-arguments (:ignored &rest arguments &req body) message
                               (formatting-message (t message receiver)
                                 ((format t "~A" (irc:source message)))
                                 ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
                                    (format-message*
                                     (format nil "~@[~A: ~]~{~A ~}~A" ,message-name (butlast arguments) body)))))))))))
  (define-server-message-printer ((irc:irc-rpl_motd-message . "MODT")
                                  (irc:irc-rpl_motdstart-message . "MOTD")
                                  (irc:irc-rpl_isupport-message)
                                  (irc:irc-rpl_yourid-message . "Your id")
                                  (irc:irc-rpl_luserop-message)
                                  (irc:irc-rpl_luserclient-message)
                                  (irc:irc-rpl_luserme-message)
                                  (irc:irc-rpl_luserchannels-message)
                                  (irc:irc-rpl_luserunknown-message)
                                  (irc:irc-rpl_globalusers-message)
                                  (irc:irc-rpl_localusers-message)
                                  (irc:irc-rpl_created-message)
                                  (irc:irc-rpl_welcome-message)
                                  (irc:irc-rpl_yourhost-message)
                                  (irc:irc-rpl_myinfo-message)
                                  (irc:irc-rpl_hello-message)
                                  (irc:irc-rpl_statsdline-message)
                                  (irc:irc-rpl_statskline-message)
                                  (irc:irc-rpl_statshline-message)
                                  (irc:irc-rpl_statsvline-message)
                                  (irc:irc-rpl_noaway-message)
                                  (irc:irc-rpl_unaway-message))))

(defmethod print-message (message receiver)
  ;; default message if we don't know how to render a message.
  #+(or) (break "~S" message)   ; uncomment to debug
  (irc:destructuring-arguments (&whole args &rest :ignored &req body) message
   (formatting-message (t message receiver)
     ((format t "!!! ~A" (irc:source message)))
     ((with-drawing-options (*standard-output* :ink +red+ :text-size :small)
        (format t "~A ~A :~A" (irc:command message) (butlast args) body))))))

;;; user-related messages

(defmethod print-message ((message irc:irc-quit-message) receiver)
  (irc:destructuring-arguments (&optional body) message
    (formatting-message (t message receiver)
      ((format t "   "))
      ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
         (format t "Quit: ")
         (present (irc:source message) 'nickname)
         (unless (null body)
           (format t ": ")
           (format-message* body :start-length (+ 8 (length (irc:source message))))
           (when (string= (title receiver)
                          (irc:normalize-nickname (connection receiver) (irc:source message)))
             (offer-close receiver))))))))

(defun present-as-hostmask (user host)
  (write-char #\()
  (with-output-as-presentation (t (format nil "*!*@~A" host) 'hostmask)
    (format t "~A@~A" user host))
  (write-char #\)))

(defmethod print-message ((message irc:irc-nick-message) receiver)
  (irc:destructuring-arguments (&rest :ignored &req body) message
   (formatting-message (t message receiver)
     ((format t "   "))
     ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
        (format t "Nick change: ")
        (present (irc:source message) 'nickname)
        (write-string " ")
        (present-as-hostmask (irc:user message) (irc:host message))
        (write-string " is now known as ")
        (present body 'nickname))))))

(defmethod print-message ((message irc:irc-rpl_whoisuser-message) receiver)
  (formatting-message (t message receiver)
    ((format t "   "))
    ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
       (irc:destructuring-arguments (:ignored nickname user host &rest :ignored &req ircname) message
         (present nickname 'nickname)
         (format t " is ")
         (present-as-hostmask user host)
         (format t " (~A)" ircname))))))

(defmethod print-message ((message irc:irc-rpl_whoischannels-message) receiver)
  (irc:destructuring-arguments (:ignored nickname &rest :ignored &req body) message
   (formatting-message (t message receiver)
     ((format t "   "))
     ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
        (present nickname 'nickname)
        (format-message* (format nil " is in ~A" body) :start-length (length nickname)))))))

(defmethod print-message ((message irc:irc-rpl_whoisserver-message) receiver)
  (irc:destructuring-arguments (:ignored nickname server &rest :ignored &req server-callout) message
    (formatting-message (t message receiver)
      ((format t "   "))
      ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
         (present nickname 'nickname)
         (format-message* (format nil " is on ~A: ~A" server server-callout)
                          :start-length (length nickname)))))))

(defmethod print-message ((message irc:irc-rpl_away-message) receiver)
  (irc:destructuring-arguments (:ignored nickname &rest :ignored &req away-msg) message
    (formatting-message (t message receiver)
      ((format t "   "))
      ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
         (present nickname 'nickname)
         (format-message* (format nil " is away: ~A" away-msg)
                          :start-length (length nickname)))))))

(defmethod print-message ((message irc:irc-rpl_whoisidentified-message) receiver)
  (irc:destructuring-arguments (:ignored nickname body) message
    (formatting-message (t message receiver)
      ((format t "   "))
      ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
         (present nickname 'nickname)
         (write-char #\Space)
         (format-message* body :start-length (length nickname)))))))

(defun unix-epoch-to-universal-time (epoch-time)
  (+ epoch-time 2208988800 ; seconds between 1970-01-01 0:00 and 1900-01-01 0:00
     ))

(defun format-unix-epoch (unix-epoch)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (unix-epoch-to-universal-time unix-epoch))
    (format nil "~4,1,0,'0@A-~2,1,0,'0@A-~2,1,0,'0@A, ~2,1,0,'0@A:~2,1,0,'0@A:~2,1,0,'0@A"
            year month date hour minute second)))

(defmethod print-message ((message irc:irc-rpl_whoisidle-message) receiver)
  (irc:destructuring-arguments (:ignored nickname idle signon &rest :ignored) message
    (formatting-message (t message receiver)
      ((format t "   "))
      ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
         (present nickname 'nickname)
         (write-char #\Space)
         (format-message* (format nil "was idle ~A seconds, signed on: ~A"
                                  idle (format-unix-epoch (parse-integer signon)))
                          :start-length (length nickname)))))))

;;; channel management messages

(defun offer-close (receiver)
  (with-output-as-presentation (t `(com-close (,receiver)) 'command)
    (with-drawing-options (*standard-output* :ink +grey12+ :text-size :small)
      (format-message* "Click here to close this tab."))))

(defun offer-reconnect (receiver)
  (let* ((conn (connection receiver))
         (server (irc:server-name conn))
         (nickname (irc:nickname (irc:user conn)))
	 (realname (irc:realname (irc:user conn))))
    (with-output-as-presentation (t `(com-connect ,server :nick ,nickname :realname ,realname) 'command)
      (with-drawing-options (*standard-output* :ink +grey12+ :text-size :small)
        (format-message* (format nil "Click here to reconnect to ~A as ~A" server nickname))))))

(defmethod print-message ((message irc:irc-err_nosuchnick-message) receiver)
  (formatting-message (t message receiver)
    ((format t "    "))
    ((irc:destructuring-arguments (:ignored target &rest :ignored) message
       (with-drawing-options (*standard-output* :ink +red3+ :text-size :small)
         (format-message* (format nil "No such nick or channel \"~A\". "
                                  target)))
       (when (string= (title receiver)
                      (irc:normalize-nickname (connection receiver) target))
         (offer-close receiver))))))

(defmethod print-message ((message irc:irc-err_blocking_notid-message) receiver)
  (formatting-message (t message receiver)
    ((format t "    "))
    ((with-drawing-options (*standard-output* :ink +red3+ :text-size :small)
       (irc:destructuring-arguments (:ignored msg) message
         (format-message* msg)
         (with-drawing-options (*standard-output* :ink +grey12+ :text-size :small)
           (with-output-as-presentation (t `(com-identify) 'command)
             (format-message* "Click here to identify yourself."))))))))

(defmethod print-message ((message irc:irc-err_chanoprivsneeded-message) receiver)
  (irc:destructuring-arguments (:ignored body) message
   (formatting-message (t message receiver)
     ((format t "    "))
     ((with-drawing-options (*standard-output* :ink +red3+ :text-size :small)
        (format-message* (format nil "Not permitted: ~A" body)))))))

(defun print-topic (receiver message sender channel topic)
  (formatting-message (t message receiver)
    ((format t "    "))
    ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
       (cond
         ((and (null sender) (null topic))
          (format-message* (format nil "No topic for ~A" channel)))
         ((null sender)
          (format-message* (format nil "Topic for ~A: ~A" channel topic)))
         ((null topic)
          (present sender 'nickname)
          (format-message* (format nil " cleared the topic of ~A" channel)))
         (t
          (present sender 'nickname)
          (format-message* (format nil " set the topic for ~A to ~A" channel topic))))))))

(defmethod print-message ((message irc:irc-topic-message) receiver)
  (irc:destructuring-arguments (channel &rest :ignored &req topic) message
    (print-topic receiver message (irc:source message) channel topic)))

(defmethod print-message ((message irc:irc-rpl_topic-message) receiver)
  (irc:destructuring-arguments (:ignored channel &optional topic) message
    (print-topic receiver message nil channel topic)))

(defmethod print-message ((message irc:irc-rpl_topicwhotime-message) receiver)
  (formatting-message (t message receiver)
    ((format t "   "))
    ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
       (irc:destructuring-arguments (:ignored channel who time) message
         (format-message* (format nil "~A topic set by ~A on ~A" channel who
                                  (format-unix-epoch (parse-integer time)))))))))

(defmethod print-message ((message irc:irc-rpl_namreply-message) receiver)
  (irc:destructuring-arguments (:ignored ; me
                                :ignored ; privacy
                                channel &rest :ignored &req nicks) message
    (formatting-message (t message receiver)
      ((format t "   "))
      ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
         (format-message* (format nil "~A Names: ~A" channel nicks)))))))

(defmethod print-message ((message irc:irc-part-message) receiver)
  (irc:destructuring-arguments (channel &optional part-msg) message
    (formatting-message (t message receiver)
      ((format t "   "))
      ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
         (format t "Part: ")
         (present (irc:source message) 'nickname)
         (format t " left ~A" channel)
         (unless (null part-msg)
           (format-message* (format nil ": ~A" part-msg))))))))

(defmethod print-message ((message irc:irc-join-message) receiver)
  (formatting-message (t message receiver)
    ((format t "   "))
    ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
       (format t "Join: ")
       (present (irc:source message) 'nickname)
       (write-char #\Space)
       (present-as-hostmask (irc:user message) (irc:host message))))))

(defmethod print-message ((message irc:irc-kick-message) receiver)
  (irc:destructuring-arguments (:ignored victim &optional kick-msg) message
    (formatting-message (t message receiver)
      ((format t "   "))
      ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
         (present (irc:source message) 'nickname)
         (write-string " kicked ")
         (present victim 'nickname)
         (unless (null kick-msg)
           (format-message* (format nil ": ~A" kick-msg)
                            :start-length (+ 9 (length victim) (length (irc:source message))))))))))

;;; XXX: uses unexported symbols from cl-irc, but I think their
;;; unexportedness is accidental.
(defun mode-symbol-to-char (target mode)
  (irc::mode-desc-char
   (irc::mode-description (current-connection *application-frame*)
                          target mode)))

(defmethod print-mode-change (target op mode (user irc:user))
    (format t "~A~A:" op (mode-symbol-to-char target mode))
    (present (irc:nickname user) 'nickname))

(defmethod print-mode-change (target op (mode (eql :limit)) arg)
  (format t "~A~A" op (mode-symbol-to-char target mode))
  (when (not (null arg))
    (write-char #\:)
    (present arg 'number)))

(macrolet ((define-mode-change-with-hostmask-printer (&rest modes)
               `(progn
                  ,@(loop for mode in modes
                          collect `(defmethod print-mode-change (target op (mode (eql ,mode)) mask)
                                     (format t "~A~A:" op (mode-symbol-to-char target mode))
                                     (present mask 'hostmask))))))
  (define-mode-change-with-hostmask-printer :ban :invite :except))

(defmethod print-mode-change (target op mode arg)
  (format t "~A~A~:[~;:~A~]" op (mode-symbol-to-char target mode) arg arg))

(defmethod print-message ((message irc:irc-mode-message) receiver)
  (case (length (irc:arguments message))
    (2 (formatting-message (t message receiver)
         ((format t "   "))
         ((irc:destructuring-arguments (channel 1c-mode) message
            (with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
              (format-message* (format nil "~A set mode ~A ~A" (irc:source message)
                                       channel 1c-mode)))))))
    (t
     (irc:destructuring-arguments (target &rest args) message
       (let* ((connection (irc:connection message))
              (target (or (irc:find-user connection target)
                          (irc:find-channel connection target)))
              (mode-changes (irc:parse-mode-arguments connection target args
                                                      :server-p (irc:user connection))))
         (formatting-message (t message receiver)
           ((format t "   "))
           ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
              (present (irc:source message) 'nickname)
              (write-string " changes channel mode: ")
              (loop for (change . rest) on mode-changes
                    do (destructuring-bind (op mode &optional arg) change
                         (print-mode-change target op mode arg))
                    if (not (null rest))
                      do (write-string ", "))))))))))

(macrolet ((define-*list-printer (&rest message-types)
               `(progn
                  ,@(loop for (message-type prefix) in message-types
                          collect
                          `(defmethod print-message ((message ,message-type) receiver)
                             (formatting-message (t message receiver)
                               ((format t "    "))
                               ((with-drawing-options (*standard-output* :ink +gray33+ :text-size :small)
                                  (write-string ,prefix)
                                  (present (nth 2 (irc:arguments message)) 'hostmask)
                                  (when (find #\! (nth 3 (irc:arguments message)))
                                    (write-string " by ")
                                    (present (first (split-sequence:split-sequence #\! (nth 3 (irc:arguments message))))
                                             'nickname))))))))))
  (define-*list-printer
      (irc:irc-rpl_banlist-message "BANNED: ")
      (irc:irc-rpl_invitelist-message "INVITED: ")
      (irc:irc-rpl_exceptlist-message "UNBANNED: ")))

(defmethod print-message ((message irc-connection-closed-message) receiver)
  (formatting-message (t message receiver)
    ((format t "    "))
    ((with-drawing-options (*standard-output* :ink +red3+)
       (format-message* "Connection to server closed.")
       (offer-reconnect receiver)))))

;;; the display function (& utilities)

(defgeneric preamble-length (message)
  (:method ((message irc:irc-privmsg-message))
    (+ 2 (length (irc:source message))))
  (:method ((message irc:ctcp-action-message))
    1)
  (:method ((message irc:irc-message))
    3))

(defun beirc-app-display (*application-frame* *standard-output* receiver)
  (let* ((messages (and receiver (messages receiver)))
         (*max-preamble-length* (loop for message in messages
                                      maximize (preamble-length message))))
    (formatting-table (t)
      (loop for message in messages
            do (print-message message receiver)))))
