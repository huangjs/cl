(in-package :beirc)

(define-presentation-type mumble ())

(define-presentation-type nickname ())
(define-presentation-type unhighlighted-nickname () :inherit-from 'nickname)
(define-presentation-type ignored-nickname () :inherit-from 'nickname)
(define-presentation-type channel () :inherit-from 'string)
(define-presentation-type hostmask () :inherit-from 'string)

(define-presentation-type bad-input () :inherit-from 'string)

(defun hash-alist (hashtable &aux res)
  (maphash (lambda (k v) (push (cons k v) res)) hashtable)
  res)

;;; mumble

(defun split-input-line (so-far)
  (multiple-value-bind (word subseq-index)
      (split-sequence:split-sequence #\Space so-far
                                     :from-end t
                                     :remove-empty-subseqs nil
                                     :count 1)
    (values (first word)
            (if (= 0 subseq-index)
                ""
                (concatenate 'string (subseq so-far 0 subseq-index) " ")))))

(defun nickname-completer (so-far mode)
  (multiple-value-bind (word prefix) (split-input-line so-far)
    (labels ((prefixify (word &optional (success t))
               (concatenate 'string prefix word
                            (cond ((not success) "")
                                  ((zerop (length prefix)) ": ")
                                  (t " ")))))
      (if (eql mode :complete) ; the user entered an activation gesture. don't complete.
          (values so-far nil nil 0 nil)
          (multiple-value-bind (string success object nmatches possibilities)
              (complete-from-possibilities word
                                           (let ((channel (and
                                                           (current-channel)
                                                           (irc:find-channel
                                                            (current-connection *application-frame*)
                                                            (current-channel)))))
                                             (if (not (null channel))
                                                 (hash-alist (irc:users channel))
                                                 nil))
                                           '()
                                           :action mode
                                           :value-key #'cdr)
            (values (prefixify (if (not success)
                                   string
                                   (irc:nickname object))
                               success)
                    success object nmatches (mapcar (lambda (possibility)
                                                      (cons (prefixify (car possibility))
                                                            (cdr possibility)))
                                                    possibilities)))))))

;; FIXME/FIXMCCLIM: :possibility-printer is ignored in current
;; McCLIM's COMPLETE-INPUT implementation.
#+(or)
(defun nickname-completion-printer (string object stream)
  (declare (ignore string))
  (present (irc:nickname object) 'nickname :stream stream))


;; FIXME: complete-input here and (accept 'command) in
;; read-frame-command means that every command that takes a 'mumble
;; argument must be terminated by hitting RET twice. ugh.
(define-presentation-method accept ((type mumble) *standard-input* (view textual-view) &key)
  (with-delimiter-gestures (nil :override t)
    (let ((*completion-gestures* '(#\Tab)))
      (nth-value 2
                 (complete-input *standard-input* 'nickname-completer
                                 #+(or):possibility-printer #+(or) 'nickname-competion-printer
                                 :allow-any-input t
                                 :partial-completers '())))))

;;; nicknames

(define-presentation-method accept ((type nickname) *standard-input* (view textual-view) &key)
  (let* ((connection (current-connection *application-frame*))
         (users (let ((channel (and (not (null (current-channel)))
                                    (irc:find-channel connection (current-channel)))))
                  (if (not (null channel))
                      (mapcar #'car (hash-alist (irc:users (irc:find-channel connection (current-channel)))))))))
    (accept `(or (member ,@users) string) :prompt nil)))

(define-presentation-method accept ((type ignored-nickname) *standard-input* (view textual-view) &key)
  (with-slots (ignored-nicks) *application-frame*
    (accept `(member ,@ignored-nicks) :prompt nil)))

(defun nick-equals-my-nick-p (nickname connection)
  (and (not (null connection))
       (equal (current-nickname connection)
              (irc:normalize-nickname connection nickname))))

(define-presentation-method present (o (type unhighlighted-nickname) *standard-output* (view textual-view) &key)
  (write-string o))

(define-presentation-method present (o (type nickname) *standard-output* (view textual-view) &key)
  (if (nick-equals-my-nick-p o (if (boundp '*current-message*)
                                   (irc:connection *current-message*)
                                   (current-connection *application-frame*)))
      (with-drawing-options (t :ink +darkgreen+)
        (with-text-face (t :bold)
          (write-string o)))
      (write-string o)))

;;; receivers

(define-presentation-method accept ((type receiver) *standard-input* (view textual-view) &key)
  (completing-from-suggestions (*standard-input* :partial-completers '(#\Space))
    (maphash (lambda (key receiver)
               (suggest (second key) receiver))
             (receivers *application-frame*))))

;;; channels

(define-presentation-method presentation-typep (object (type channel))
  (channelp object))

(defun channelp (channel)
  (and (stringp channel)
       (> (length channel) 2)
       (< (length channel) 50)
       (member (char channel 0) '(#\# #\+ #\! #\&))
       (not (find-if (lambda (c)
                       (member c `(#\Space
                                   #\, ,(code-char 7)
                                   ;; XXX: #\: is used to separate the
                                   ;; channel name from the channel
                                   ;; mask, and so isn't a part of the
                                   ;; channel name. see rfc2811 for
                                   ;; details.
                                   #\:)))
                     channel))))

(define-presentation-method accept ((type channel) *standard-input* (view textual-view) &key)
  (let ((channel (accept 'string :view view :prompt nil)))
    (if (not (presentation-typep channel 'channel))
        (input-not-of-required-type channel 'channel)
        channel)))
