(in-package :sb-screen)

(defclass tty-screen () ())

(defvar *old-fd-0-handlers* nil)
(defvar *our-handler* nil)
(defvar *initialized* nil)

(defvar *last-key* nil)

(defvar *key-hook* nil)

(defparameter *special-keys*
  '((:escape . 27)
    (:rubout . 127)))

(defmethod encode-key ((screen tty-screen) key)
  (etypecase key
    (character (char-code key))
    (symbol (cond
              ((assoc key *special-keys*)
               (cdr (assoc key *special-keys*)))
              (t
               (error "Don't know ~S" key))))
    (list (cond
            ((eql (length key) 2)
             (if (eql (car key) :control)
                 (cond ((<= (char-code #\a) (char-code (second key))
                            (char-code #\z))
                        (+ 1 (- (char-code (second key))
                                (char-code #\a))))
                       ((<= (char-code #\A) (char-code (second key))
                            (char-code #\Z))
                        (+ 1 (- (char-code (second key))
                                (char-code #\A))))
                       (t (error "Can't encode key ~S" key)))
                 (error "Can't encode key ~S" key)))
            ((eql (length key) 1)
             (encode-key screen (car key)))
            (t
             (error "Can't encode key ~S" key))))))

(defmethod decode-key ((screen tty-screen) keysym)
  (if (< keysym 255)
      (let ((char (code-char keysym)))
        (if (graphic-char-p char) char
            (cond
              ((<= 1 keysym 26)
               (list :control (code-char (+ (char-code #\a) keysym -1))))
              ((rassoc keysym *special-keys*)
               (car (rassoc keysym *special-keys*)))
              (t
               :unknown))))
      :unknown))

(defun %input-function (screen)
  ;;; right now just do wacky stuff
  (loop while (not (zerop (slang-input-pending 0)))
        for key-hook = (or *key-hook*
                            (constantly nil))
        do (progn
             (funcall key-hook (decode-key screen (setf *last-key* (slang-getkey))))
             #+nil
             (push (decode-key screen *last-key*) *keys*))))

(defvar *color-hash*)

(defparameter *backgrounds*
  '("black" "red" "green" "brown" "blue" "magenta" "cyan" "lightgray"))

(defparameter *color-names*
  (append *backgrounds*
          '("gray" "brightred" "brightgreen" "yellow" "brightblue" "brightmagenta" "brightcyan" "white")))

(defun random-element (l &key not)
  (loop for e = (elt l (random (length l)))
       if (not (equalp e not))
       return e))

(defun initialize-color-hash ()
  (setf *color-hash* (make-hash-table :test #'equalp))
  (loop for i in *color-names* with c = 1 do
       (loop for j in *backgrounds*
          if (not (equalp i j))
          do (progn
               (incf c)
               (setf (gethash (cons i j) *color-hash*) c)
               (sltt-set-color c (symbol-name (gensym)) i j)))))

(defmethod initialize-screen ((screen tty-screen) &key (abort-char-code -1))
  (unless *initialized*
    (sltt-get-terminfo)
    (slkp-init)
    (slang-init-tty abort-char-code 0 1)
    (slsmg-init-smg)
    (initialize-color-hash)
    (setf *old-fd-0-handlers* (reverse (remove 0 sb-impl::*descriptor-handlers* :key #'sb-impl::handler-descriptor :test-not #'eql)))
    (mapc #'remove-fd-handler *old-fd-0-handlers*)
    (setf *our-handler* (add-fd-handler 0 :input (lambda (fd)
                                                   (declare (ignore fd))
                                                   (funcall '%input-function screen))))
    (setf *initialized* t)))

(defmethod key-hook (screen)
  *key-hook*)

(defmethod (setf key-hook) (hook (screen tty-screen))
  (setf *key-hook* hook))

(defmethod release-screen ((screen tty-screen))
  (slang-reset-tty)
  (slsmg-reset-smg)
  (if *our-handler*
      (remove-fd-handler *our-handler*))
  (setf *our-handler* nil)
  (mapc #'(lambda (handler)
            (push handler sb-impl::*descriptor-handlers*)) *old-fd-0-handlers*)
  (setf *old-fd-0-handlers* nil)
  (setf *initialized* nil))

(defun use-ansi-colors-p ()
  (not (zerop sltt-use-ansi-colors)))

(defmethod clear-screen ((screen tty-screen))
  (slsmg-cls))

(defmethod finish-screen ((screen tty-screen))
  (slsmg-refresh))

(defmethod set-cursor ((screen tty-screen) row col)
  (slsmg-gotorc row col))

(defmethod get-cursor ((screen tty-screen))
  (values
   (slsmg-get-row)
   (slsmg-get-column)))

(defmethod write-string-at-cursor ((screen tty-screen) string)
  (slsmg-write-string (coerce string 'simple-base-string)))

(defmethod erase-from-cursor-to-eol ((screen tty-screen))
  (slsmg-erase-eol))

(defmethod erase-from-cursor-to-eos ((screen tty-screen))
  (slsmg-erase-eos))

(defmethod draw-line-at-cursor ((screen tty-screen) direction length)
  (ecase direction
    (:horizontal
     (slsmg-draw-hline length))
    (:vertical
     (slsmg-draw-vline length))))

(defun get-color-pair-number (fg bg)
  (gethash (cons (symbol-name fg) (symbol-name bg)) *color-hash*))

(defmethod valid-color-p ((screen tty-screen) sym type)
  (and (typep sym 'symbol)
       (member (string-downcase (symbol-name sym))
               (ecase type
                 (:foreground *color-names*)
                 (:background *backgrounds*))
               :test #'string=)))

(defmethod valid-colors ((screen tty-screen) type)
  (mapcar #'(lambda (e) (intern (string-upcase e) :keyword))
          (ecase type
            (:foreground *color-names*)
            (:background *backgrounds*))))

(defmethod set-color ((screen tty-screen) foreground background)
  (if (and (valid-color-p screen foreground :foreground)
           (valid-color-p screen background :background)
           (not (equalp (symbol-name foreground)
                        (symbol-name background))))
      (progn
        (slsmg-set-color (gethash (cons (symbol-name foreground)
                                        (symbol-name background))
                                  *color-hash*))
        (slsmg-set-char-set 0))
      (error "~S / ~S are not valid foreground / background!"
             foreground background)))

(defmethod set-to-default-color ((screen tty-screen))
  (slsmg-set-color 0))

(defvar *winch* 0)

(defvar *winch-hook* nil)

(defun sigwinch-handler (&rest ignored)
  (declare (ignore ignored))
  (sltt-get-screen-size)
  (slsmg-reinit-smg)
  (if *winch-hook*
      (funcall *winch-hook*)))

(enable-interrupt sb-unix:sigwinch #'sigwinch-handler)

(defmethod get-screen-size ((screen tty-screen))
  (values sltt-screen-rows sltt-screen-cols))

(defmethod window-resize-hook ((screen tty-screen))
  *winch-hook*)

(defmethod (setf window-resize-hook) (new-hook (screen tty-screen))
  (setf *winch-hook* new-hook))

(defun clear-window-resize-hook ()
  (setf *winch-hook* nil))