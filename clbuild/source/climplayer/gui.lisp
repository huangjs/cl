;; climplayer -- a frontend for mplayer

;; Copyright (C) 2005  Thomas Persson

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :climplayer)

(defvar *climplayer-conf-directory* nil)

(defvar *climplayer-conf-file* nil)

(defvar *display-lock*)

(defvar *frame-process*)

(defvar *climplayer-frame* nil)

(defun player-loop (frame)
  (loop
   (sleep 0.1)
   (with-accessors ((currently-playing currently-playing)
                    (current-title current-title)
                    (mplayer-process mplayer-process)
                    (play-list play-list)
                    (next-to-play next-to-play)
                    (player-speed player-speed)
                    (play-list-tab play-list-tab)
                    (random-playbackp random-playbackp)
                    (continuous-playbackp continuous-playbackp)
                    (mplayer-file-length mplayer-file-length))
     frame
     (cond ((not play-list))
           ((and play-list-tab
                 (not (find play-list-tab
                            (tab-layout-pages (find-pane-named frame
                                                               'main-pane)))))
            (setf play-list-tab nil))
           ((and (timer frame)
                 (= (car (timer frame))
                    (caddr (multiple-value-list (get-decoded-time))))
                 (= (cadr (timer frame))
                    (cadr (multiple-value-list (get-decoded-time)))))
            (if (not (member (caddr (timer frame))
                             play-list))
                (setf (caddr (timer frame))
                      (nth (random (length play-list))
                           play-list)))
            (setf next-to-play
                  (caddr (timer frame))
                  continuous-playbackp
                  t
                  (timer frame)
                  nil)
            (stop t))
           ((and (cl-user::process-p mplayer-process)
                 (cl-user::process-alive-p mplayer-process))
            (get-stream-info))
           (next-to-play
            (when mplayer-process
              (close (cl-user::process-input mplayer-process))
              (close (cl-user::process-output mplayer-process)))
            (unless (find next-to-play (current-playlist frame))
              (setf (play-list-tab frame)
                    nil))
            (setf currently-playing
                  next-to-play
                  next-to-play
                  nil
                  player-speed
                  1
                  mplayer-process
                  (play currently-playing)
                  current-title
                  nil
                  mplayer-file-length
                  0)
            (redisplay-climplayer-frame frame))
           (currently-playing
            (unless (current-playlist frame)
              (setf (play-list-tab frame)
                    nil))
            (let ((current-playlist (current-playlist frame)))
              (if continuous-playbackp
                  (setf next-to-play
                        (nth (mod (+ (if random-playbackp
                                         (random (length current-playlist))
                                         1)
                                     (or (position currently-playing
                                                   current-playlist)
                                         -1))
                                  (length current-playlist))
                             current-playlist))
                  (progn (setf currently-playing
                               nil)
                         (redisplay-climplayer-frame frame))))))
     (let ((changed (get-changed-nodes))
           (*application-frame* frame))
       (when changed
         (mapcar #'refresh-node changed)
         (refresh-playlist)
         (redisplay-climplayer-frame frame)
         (setf (last-update *climplayer-frame*)
               (get-universal-time)))))))

(defun current-playlist (frame)
  (let ((current (play-list-tab frame)))
    (if current
        (reverse (find-node (play-list frame)
                            (gadget-value (search-field current))))
        (play-list frame))))

(defun redisplay-climplayer-frame (frame)
  (let ((info (find-pane-named frame 'info))
        (extended-info (find-pane-named frame 'extended-info))
        (browser (find-pane-named frame 'browser))
        (control (find-pane-named frame 'control)))
    (clim-sys:with-lock-held (*display-lock*)
      (redisplay-frame-pane frame browser)
      (redisplay-frame-pane frame info)
      (redisplay-frame-pane frame extended-info)
      (redisplay-frame-pane frame control)
      (redisplay-all-search-tabs frame)
      (queue-repaint browser
                     (make-instance 'window-repaint-event
                                    :region (pane-viewport-region browser)
                                    :sheet browser)))))

(defun redisplay-controls (frame)
  (let ((control (find-pane-named frame 'control)))
    (clim-sys:with-lock-held (*display-lock*)
      (redisplay-frame-pane frame control)
      (queue-repaint control
                     (make-instance 'window-repaint-event
                                    :region (sheet-region control)
                                    :sheet control)))))

(defmethod (setf currently-playing) :before (node
                                             frame)
  (let ((current (currently-playing frame)))
    (when current
      (setf (previously-played frame)
            current))))

(define-application-frame climplayer ()
  ((container-node :initform (make-container-node *media-directories*)
                   :accessor container-node)
   (net-nodes :initform (get-saved-urls)
              :accessor net-nodes)
   (play-list :accessor play-list)
   (play-list-tab :accessor play-list-tab
                  :initform nil)
   (previously-played :accessor previously-played
                       :initform nil)
   (currently-playing :accessor currently-playing
                      :initform nil)
   (current-title :accessor current-title
                  :initform nil)
   (next-to-play :accessor next-to-play
                 :initform nil)
   (player-process :accessor player-process)
   (in-stream :accessor in-stream)
   (mplayer-process :accessor mplayer-process
                    :initform nil)
   (currently-browsing :initform nil
                       :accessor currently-browsing)
   (player-speed :initform 1
                 :accessor player-speed)
   (monitor-process :accessor monitor-process
                    :initform nil)
   (last-update :accessor last-update
                :initform (get-universal-time))
   (random-playbackp :accessor random-playbackp
                     :initform nil)
   (continuous-playbackp :accessor continuous-playbackp
                         :initform t)
   (timer :accessor timer
          :initform nil)
   (volume :accessor volume
           :initform 0)
   (volume-updated :accessor volume-updated
                   :initform (get-universal-time))
   (mplayer-position :accessor mplayer-position
                     :initform 0)
   (mplayer-file-length :accessor mplayer-file-length
                        :initform 0))
  (:panes
   (info (make-pane 'application-pane
                    :scroll-bars nil
                    :incremental-redisplay t
                    :display-function 'display-info))
   (control (make-pane 'application-pane
                       :scroll-bars nil
                       :incremental-redisplay t
                       :display-function 'display-controls))
   (extended-info :application
                  :scroll-bars nil
                  :foreground +white+
                  :background +black+
                  :incremental-redisplay t
                  :display-function 'display-extended-info)
   (interactor :interactor
               :height 50)
   (pointer-doc :pointer-documentation)
   (browser :application
            :width 500
            :height 400
            :incremental-redisplay t
            :display-function 'display-file-browser))
  (:layouts
   (default (vertically ()
              (outlining ()
                (vertically ()
                  (40 info)
                  (45 control)))
              (with-tab-layout ('pane :name 'main-pane)
                ("Browser" browser :drawing-options `(:ink ,+dark-green+
                                                      :text-style ,(make-text-style nil :bold nil))))
              (make-pane 'clim-extensions:box-adjuster-gadget)
              interactor
              (18 (horizontally ()
                    pointer-doc
                    (120 extended-info)))))))

(defmethod (setf play-list-tab) :after (page
                                        frame)
  (let* ((main-pane (find-pane-named frame 'main-pane))
         (play-list-tab (or (play-list-tab frame)
                            (find-tab-page-named "Browser"
                                                 main-pane)))
         (rest-tabs (remove play-list-tab (tab-layout-pages main-pane))))
    (mapcar (lambda (page)
              (setf (tab-page-drawing-options page)
                    `(:ink ,+black+)))
            rest-tabs)
    (setf (tab-page-drawing-options play-list-tab)
          `(:ink ,+dark-green+
            :text-style ,(make-text-style nil :bold nil)))))

(defun climplayer (&key new-process (text-size :small))
  "Start climplayer."
  (flet ((run-climplayer ()
           (setf *climplayer-conf-directory*
                 (merge-pathnames (pathname ".climplayer/")
                                  (user-homedir-pathname))
                 *climplayer-text-size*
                 text-size
                 *climplayer-conf-file*
                 (merge-pathnames (pathname "climplayer-conf.lisp")
                                  *climplayer-conf-directory*)
                 *climplayer-conf-directories-file*
                 (merge-pathnames (pathname "climplayerdirs")
                                  *climplayer-conf-directory*))
           (ensure-directories-exist *climplayer-conf-directory*)
           (if (probe-file *climplayer-conf-file*)
               (load *climplayer-conf-file*))
           (when (probe-file *climplayer-conf-directories-file*)
             (setf *media-directories*
                   (with-open-file (s *climplayer-conf-directories-file*)
                     (setf *media-directories*
                           (remove-if-not #'probe-file (read s))))))
           (unless *media-directories*
             (run-frame-top-level (make-application-frame 'select)))
           (when *media-directories*
             (let ((frame (make-application-frame 'climplayer)))
               (setf *display-lock*
                     (clim-sys:make-lock)
                     (currently-browsing frame)
                     (container-node frame)
                     *frame-process*
                     (clim-sys:current-process)
                     (play-list frame)
                     (append (flatten (find-node (container-node frame) ""))
                             (nreverse (copy-list (net-nodes frame))))
                     (player-process frame)
                     (clim-sys:make-process (lambda ()
                                              (player-loop frame))))
               (setf *climplayer-frame* frame)
               (monitor-directories)
               (run-frame-top-level frame)
               (stop-monitor)
               (setf *climplayer-frame* nil)))))
    (if new-process
        (clim-sys:make-process #'run-climplayer
                               :name "climplayer-gui")
        (run-climplayer))))

(defmethod adopt-frame :after (frame-manager (frame climplayer))
  (declare (ignore frame-manager))
  (let ((tabs-file (merge-pathnames (pathname "tabs.lisp")
                                    *climplayer-conf-directory*))
        (tab-layout (find-pane-named frame
                                     'main-pane)))
    (when (probe-file tabs-file)
      (with-open-file (stream (merge-pathnames (pathname "tabs.lisp")
                                               *climplayer-conf-directory*)
                              :direction :input
                              :if-exists :supersede)
        (dolist (regexp (read stream))
          (let ((tab (new-search-tab)))
            (setf (gadget-value (search-field tab))
                  regexp)))
        (setf (tab-layout-enabled-page tab-layout)
              (car (tab-layout-pages tab-layout)))))))

(defun save-urls ()
  (with-open-file (stream (merge-pathnames (pathname "climplayer-state.lisp")
                                           *climplayer-conf-directory*)
                          :direction :output
                          :if-exists :supersede)
    (write (mapcar #'node-string-name
                   (net-nodes *application-frame*))
           :stream stream)))

(defun get-saved-urls ()
  (let ((path (merge-pathnames (pathname "climplayer-state.lisp")
                               *climplayer-conf-directory*)))
    (if (probe-file path)      
        (with-open-file (stream path
                                :direction :input)
          (mapcar (lambda (url)
                    (make-instance 'net-node
                                   :string-name url))
                  (read stream)))
        nil)))

(defmethod redisplay-frame-pane :around ((frame climplayer) pane
					 &key force-p)
  (declare (ignore force-p))
  (if (equal (clim-sys:current-process)
             *frame-process*)
      (clim-sys:with-lock-held (*display-lock*)
        (call-next-method))
      (call-next-method)))

(defun line-height (pane)
  (+ (text-style-height (make-text-style nil nil *climplayer-text-size*) pane)
     (if (equal pane
                (find-pane-named *climplayer-frame* 'browser))
         4
         2)))

(defun display-info (frame pane)
  (with-text-size (pane *climplayer-text-size*)
    (let ((node (currently-playing frame))
          (current-title (current-title frame)))
      (updating-output (pane :unique-id (list node current-title)
                             :id-test #'equal
                             :cache-value (list node current-title)
                             :cache-test #'equal)
        (with-drawing-options (pane :ink +darkgreen+)
          (with-text-face (pane :bold)
            (format pane " Playing: ")))
        (if node
            (with-output-as-presentation (pane node (type-of node))
              (write-string (extended-title node)
                            pane)
              (when current-title
                (with-drawing-options (pane :ink +black+)
                  (with-text-face (pane :bold)
                    (write-string "   -   "
                                  pane)))
                (write-string current-title
                              pane)))
            (write-string "-" pane))))))

(defun draw-stop-button (pane)
  (multiple-value-bind (x y)
      (stream-cursor-position pane)
    (write-string " " pane)
    (draw-rectangle* pane
                     x
                     (+ y 3)
                     (+ x 6)
                     (+ y 10))))

(defun seconds-to-hour-min-sec (seconds)
  (let* ((hour (floor (/ seconds 3600)))
         (min (floor (/ (- seconds
                           (* hour
                              3600))
                        60)))
         (seconds (- seconds
                     (* hour 3600)
                     (* min 60))))
    (format nil "~2,'0D:~2,'0D:~2,'0D"
            hour
            min
            seconds)))

(defun draw-timer (pane frame)
  (let ((timer (timer frame)))
    (write-string " Will play "
                  pane)
    (with-drawing-options (pane :ink +darkred+)
      (with-output-as-presentation (pane `(com-change-file-or-url)
                                         'command)
        (write-string (title (caddr timer))
                      pane)))
    (write-string " at " pane)
    (with-drawing-options (pane :ink +darkred+)
      (with-output-as-presentation (pane `(com-change-hour)
                                         'command)
        (format pane
                "~2,'0D"
                (car timer))))
    (write-string ":" pane)
    (with-drawing-options (pane :ink +darkred+)
      (with-output-as-presentation (pane `(com-change-minute)
                                         'command)
        (format pane
                "~2,'0D"
                (cadr timer))))
    (write-string "." pane)))

(defun display-controls (frame pane)
  (with-text-size (pane *climplayer-text-size*)
    (with-text-face (pane :bold)
      (if (and (cl-user::process-p (mplayer-process frame))
               (cl-user::process-alive-p (mplayer-process frame)))
          (format pane
                  " ~A / ~A"
                  (seconds-to-hour-min-sec (mplayer-position frame))
                  (seconds-to-hour-min-sec (mplayer-file-length frame)))
          (write-string " 00:00:00 / 00:00:00" pane))
      (terpri pane)
      (write-string " " pane)
      (with-output-as-presentation (pane `(com-previous) 'command)
        (write-string "<<" pane))
      (write-string " " pane)
      (with-output-as-presentation (pane `(com-play-or-pause) 'command)
        (write-string ">" pane))
      (write-string " " pane)
      (with-output-as-presentation (pane `(com-stop) 'command)
        (draw-stop-button pane))
      (write-string " " pane)
      (with-output-as-presentation (pane `(com-next) 'command)
        (write-string ">>" pane))
      (write-string "    " pane)
      (with-drawing-options (pane :ink (if (random-playbackp frame)
                                           +darkgreen+
                                           +lightgray+))
        (write-string " " pane)
        (with-output-as-presentation (pane `(com-random-playback) 'command)
          (write-string "R"
                        pane)))
      (with-drawing-options (pane :ink (if (continuous-playbackp frame)
                                           +darkgreen+
                                           +lightgray+))
        (write-string " " pane)
        (with-output-as-presentation (pane `(com-continuous-playback) 'command)
          (write-string "C"
                        pane))))
    (multiple-value-bind (x y)
        (stream-cursor-position pane)
      (format pane
              "     Volume: ~A"
              (if (and (cl-user::process-p (mplayer-process frame))
                       (cl-user::process-alive-p (mplayer-process frame)))
                  (format nil "~A%" (volume frame))
                  "-"))
      (stream-set-cursor-position pane 
                                  (+ x
                                     (stream-string-width pane "     Volume: 100% "))
                                  y))
    (with-text-face (pane :bold)
      (with-output-as-presentation (pane `(com-increase-volume) 'command)
        (write-string "+" pane))
      (write-string " " pane)
      (with-output-as-presentation (pane `(com-decrease-volume) 'command)
        (write-string "-" pane)))
    (terpri pane)
    (when (timer frame)
      (draw-timer pane frame))))

(define-climplayer-command (com-increase-volume :name nil)
    ()
  (setf (volume *application-frame*)
        (min (+ (volume *application-frame*)
                10)
             100))
  (mplayer-command (format nil
                           "set_property volume ~A"
                           (volume *application-frame*))))

(define-climplayer-command (com-decrease-volume :name nil)
    ()
  (setf (volume *application-frame*)
        (max (- (volume *application-frame*)
                10)
             0))
  (mplayer-command (format nil
                           "set_property volume ~A"
                           (volume *application-frame*))))

(define-climplayer-command (com-change-file-or-url :name nil)
    ()
  (when (timer *application-frame*)
    (setf (caddr (timer *application-frame*))
          (accept 'media-node :prompt "file or url"))))

(define-climplayer-command (com-change-hour :name nil)
    ()
  (when (timer *application-frame*)
    (setf (car (timer *application-frame*))
          (accept 'integer :prompt "hour"))))

(define-climplayer-command (com-change-minute :name nil)
    ()
  (when (timer *application-frame*)
    (setf (cadr (timer *application-frame*))
          (accept 'integer :prompt "minute"))))

(define-climplayer-command (com-random-playback :name t
                                                :keystroke (#\r :control))
    ()
  (setf (random-playbackp *application-frame*)
        (not (random-playbackp *application-frame*)))
  (redisplay-frame-panes *application-frame*))

(define-climplayer-command (com-continuous-playback :name t)
    ()
  (setf (continuous-playbackp *application-frame*)
        (not (continuous-playbackp *application-frame*))))

(defun display-extended-info (frame pane)
  (let ((speed (format nil "~A" (float (player-speed frame)))))
    (write-string "Speed: " 
                  pane)
    (write-string (if (< 5 (length speed))
                      (subseq speed 0 5)
                      speed)
                  pane)))

(defun node-status (node)
  (cond ((eq node (currently-playing *climplayer-frame*))
         'playing)
        ((eq node (next-to-play *climplayer-frame*))
         'next)
        (t nil)))

(defun node-icon (node stream &optional pattern)
  (let ((pattern (or pattern
                     (clim-listener::icon-of (typecase node
					       (net-node (node-string-name node))
					       (file (node-path-name node)))))))
    (multiple-value-bind (x y)
        (stream-cursor-position stream)
      (draw-pattern* stream pattern
                     x
                     (- y (ceiling (/ (- 16
                                         (text-style-height (make-text-style nil nil *climplayer-text-size*)
                                                            stream))
                                      2))))
      (stream-increment-cursor-position stream
                                        (+ (pattern-width pattern)
					   2)
					0))))

(define-climplayer-command (com-browse-up) ()
  (browse-upward))

(defun display-file-browser (frame pane)
  (with-end-of-line-action (pane :allow)
    (with-text-size (pane *climplayer-text-size*)
      (let ((contents (node-contents (currently-browsing frame))))
        (if (eq (type-of (currently-browsing frame))
                'container-node)
            (progn (with-output-as-presentation (pane
                                                 (currently-browsing frame)
                                                 'directory-node)
                     (with-text-face (pane :bold)
                       (write-line "Toplevel:" pane)))
                   (terpri pane)
                   (setf contents
                         (append contents
                                 (reverse (net-nodes frame)))))
            (progn
              (write-string "Contents of " pane)
              (do* ((node (currently-browsing *climplayer-frame*)
                          (node-parent node))
                    (node-list (list node)
                               (cons node node-list)))
                   ((eq (type-of node)
                        'container-node)
		    (when (cdr node-list)
                      (with-text-face (pane :bold)
                        (with-output-as-presentation (pane
                                                      (cadr node-list)
                                                      'directory-node)
                          (write-string (node-string-name (cadr node-list))
                                        pane))
                        (dolist (node (cddr node-list))
                          (with-output-as-presentation (pane
                                                        node
                                                        'directory-node)
                            (format pane "~A/"
                                    (car (reverse (pathname-directory (node-path-name node)))))))))))
              (terpri pane)
              (with-output-as-presentation (pane
                                            `(com-browse-up)
                                            'command)
                (node-icon (node-parent (currently-browsing frame))
                           pane
                           (clim-listener::standard-icon "up-folder.xpm"))
                (write-line ".."
                            pane))))
        (dolist (node contents)
          (updating-output (pane :unique-id node
                                 :id-test #'eq
                                 :cache-value (node-status node)
                                 :cache-test #'eq)
            (with-output-as-presentation (pane node (type-of node))
              (node-icon node pane)
              (cond ((eq node (currently-playing frame))
                     (with-drawing-options (pane :ink +darkgreen+)
                       (with-text-face (pane :bold)
                         (write-line (title node)
                                     pane))))
                    ((eq node (next-to-play frame))
                     (with-drawing-options (pane :ink +lightgray+)
                       (write-line (title node)
                                   pane)))
                    ((eq (type-of node)
                         'directory-node)
                     (write-line (pathname-to-string (node-path-name node))
                                 pane))
                    (t
                     (write-line (title node)
                                 pane)))
              (stream-increment-cursor-position pane 0 2))))))))

(define-climplayer-command (com-browse)
    ((node 'container-node))
  (setf (currently-browsing *application-frame*)
        node)
  (scroll-extent (find-pane-named *application-frame*
                                  'browser)
                 0
                 0))

(define-presentation-to-command-translator node-to-browse-translator
    (directory-node com-browse climplayer
                    :priority 2
                    :echo nil
                    :documentation "Browse"
                    :pointer-documentation "Browse")
    (object)
  (list object))

(defun browse-upward ()
  (let ((old (currently-browsing *application-frame*)))
    (unless (eq old (container-node *application-frame*))
      (setf (currently-browsing *application-frame*)
            (node-parent old))
      (if (eq (currently-browsing *application-frame*)
              (container-node *application-frame*))
          (scroll-extent (find-pane-named *application-frame*
                                          'browser)
                         0
                         0)
          (ensure-that-node-is-visible old :top t)))))

(defun mplayer-command (command)
  (let ((process (mplayer-process *application-frame*)))
    (when (and (cl-user::process-p process)
               (cl-user::process-alive-p process))
      (format (cl-user::process-input process)
              "~A~%" command)
      (force-output (cl-user::process-input process)))))

(defun stop (kill-process)
  (with-accessors ((current currently-playing)
                   (process mplayer-process))
      *climplayer-frame*
    (setf current
          nil)
    (if (and kill-process
             process)
        (cl-user::process-kill process 15))))

(defun scroll-y (pane y)
  (let* ((height (rectangle-height (pane-viewport-region pane)))
         (pane-max-y (bounding-rectangle-max-y pane))
         (new-y (cond ((< (- pane-max-y height)
                          y)
                       (- pane-max-y
                          height))
                      ((< y 0)
                       0)
                      (t y))))
    (if (and (<= y (* 19/20
                      (- height)))
             (equal (pane-name pane)
                    'browser))
        (browse-upward)
        (scroll-extent pane 0 new-y))))

(defun scroll-page (pane &optional up)
  (let ((scroll (* (rectangle-height (pane-viewport-region pane))
                   19/20)))
    (scroll-y pane
              (+ (rectangle-min-y (pane-viewport-region pane))
                 (if up
                     (- scroll)
                     scroll)))))

(defun regexify (string)
  (string-trim "."
               (string-downcase (format nil
                                        "~{~A.~}"
                                        (split-sequence-if
                                         (lambda (x)
                                           (not (alphanumericp x)))
                                         string)))))

(defun refresh-playlist ()
  (labels ((find-new-version (node test)
             (find node
                   (play-list *climplayer-frame*)
                   :test (lambda (n1 n2)
                           (equal (funcall test n1)
                                  (funcall test n2))))))
    (let* ((current (currently-playing *application-frame*))
           (next (next-to-play *application-frame*))
           (prev (previously-played *application-frame*)))
      (setf (play-list *application-frame*)
            (append (flatten (container-node *application-frame*))
                    (nreverse (copy-list (net-nodes *application-frame*))))
            (currently-playing *application-frame*)
            (when current
              (or (find-new-version current #'node-string-name)
                  (find-new-version current #'title)))
            (previously-played *application-frame*)
            (when prev
              (or (find-new-version prev #'node-string-name)
                  (find-new-version prev #'title)))
            (next-to-play *application-frame*)
            (when next
              (or (find-new-version next #'node-string-name)
                  (find-new-version next #'title)))
            (currently-browsing *application-frame*)
            (typecase (currently-browsing *application-frame*)
              (directory-node (or (car (remove-if-not (lambda (node)
                                                        (typep node 'directory-node))
                                                      (find-node (container-node *application-frame*)
                                                                 (regexify (node-string-name (currently-browsing *application-frame*))))))
                                  (container-node *application-frame*)))
              (container-node (container-node *application-frame*))))
      (when (timer *application-frame*)
        (setf (caddr (timer *application-frame*))
              (or (find-new-version (caddr (timer *application-frame*)) #'node-string-name)
                  (find-new-version (caddr (timer *application-frame*)) #'title)
                  (nth (random (length (play-list *application-frame*)))
                       (play-list *application-frame*))))))))

(defun play-relative-to-current (n)
  (with-accessors ((current currently-playing)
                   (next next-to-play)
                   (play-list play-list)
                   (prev previously-played)
                   (random-playbackp random-playbackp))
    *application-frame*
    (unless (current-playlist *application-frame*)
      (setf (play-list-tab *application-frame*)
            nil))
    (let ((current-playlist (current-playlist *application-frame*)))
      (when current
        (setf next
              (if (and random-playbackp
                       (< n 0)
                       prev)
                  prev
                  (nth (mod (+ (if random-playbackp
                                   (random (length current-playlist))
                                   n)
                               (or (position current
                                             current-playlist)
                                   -1))
                            (length current-playlist))
                       current-playlist)))
        (ensure-that-node-is-visible next)
        (stop t)))))
        

(defun ensure-that-node-is-visible (node &key top (tabs :all))
  (when node
    (macrolet ((scroll ()
                 `(cond ((or top
                             (< current-y min-y))
                         (scroll-y pane
                                   current-y))
                        ((> current-y (- max-y (line-height pane)))
                         (scroll-y pane
                                  (+ (line-height pane)
                                     (- current-y
                                        (- max-y
                                           min-y))))))))
      (when (and (or (eq tabs :all)
                     (eq tabs :browser))
                 (find node (node-contents (currently-browsing *application-frame*))))
        (let* ((pane (find-pane-named *application-frame*
                                      'browser))
               (min-y (rectangle-min-y (pane-viewport-region pane)))
               (max-y (rectangle-max-y (pane-viewport-region pane)))
               (current-y (+ (* (position node
                                          (node-contents (node-parent node)))
                                (line-height pane))
                             (- (* (line-height pane)
                                   2)
                                6))))
          (scroll)))
      (when (and (or (eq tabs :all)
                     (eq tabs :search))
                 (play-list-tab *application-frame*)
                 (find node (current-playlist *application-frame*))
                 (display-all (play-list-tab *application-frame*)))
        (let* ((pane (results (play-list-tab *application-frame*)))
               (min-y (rectangle-min-y (pane-viewport-region pane)))
               (max-y (rectangle-max-y (pane-viewport-region pane)))
               (current-y (* (position node
                                       (current-playlist *application-frame*))
                             (line-height pane))))
          (scroll))))))

(defun stream-to-string (stream)
  (do* ((line (read-line stream
                         nil
                         nil)
              (read-line stream
                         nil
                         nil))
        (text line
              (if line
                  (format nil "~A~%~A"
                          text
                          line)
                  text)))
       ((not line)
        text)))

(defun wget-url (url)
  (with-open-stream
      (stream (cl-user::process-output
               (cl-user::run-program "wget"
                                     (list "-q"
                                           "-O"
                                           "-"
                                           url)
                                     #+sbcl :search #+sbcl t
                                     :output
                                     :stream)))
    (stream-to-string stream)))

(defun head-url (url)
  (with-open-stream
      (stream (cl-user::process-error
               (cl-user::run-program "wget"
                                     (list "--spider"
                                           "--tries"
                                           "2"
                                           url)
                                     #+sbcl :search #+sbcl t
                                     :error
                                     :stream)))
    (stream-to-string stream)))

(defun parse-for-real-url (url)
  (let* ((head (head-url url))
         (media-content (scan "\\[((audio|video)/(x-ms-asf|x-ms-wax|x-ms-wvx|x-pn-realaudio|x-mpegurl|x-scpls)|(text/(html|plain)))\\]"
                              head))
         (length (scan-to-strings "Length:.([0-9](,)?)+" head))
         (length (if length
                     (parse-integer (remove #\, (subseq length 8)))
                     0)))
    (if (and media-content
             (< length 1000))
        (or (scan-to-strings "(?:https?://|ftp://|www\\.|rtsp://|mms://|ftp\\.)(?:(?:[a-zA-Z0-9@_\\-]+)\\.)+[a-zA-Z]{2,4}(?::[0-9]{1,5})?(?:/[a-z&A-Z0-9_.#-?=:%~]*)?"
                             (wget-url url))
            url)
        url)))

(defun add-urls-to-play-list (list)
  (let ((url-list (mapcar (lambda (url)
                            (let ((node (make-instance 'net-node
                                                       :string-name url)))
                              (push node (net-nodes *application-frame*))
                              node))
                          list)))
    (refresh-playlist)
    (save-urls)
    url-list))

(defmethod play ((node net-node))
  (cl-user::run-program "mplayer"
                        (append *mplayer-options*
                                (list "-slave"
                                      "-quiet"
                                      (parse-for-real-url (node-string-name node))))
                        #+sbcl :search #+sbcl t
                        :input :stream
                        :output :stream
                        :wait nil))

(defun get-stream-info ()
  (let ((mplayer-out-stream (cl-user::process-output (mplayer-process *climplayer-frame*)))
        (*application-frame* *climplayer-frame*))
    (when mplayer-out-stream
      (when (<= 1 (- (get-universal-time)
                     (volume-updated *climplayer-frame*)))
        (mplayer-command (format nil
                                 "pausing_keep_force get_property volume~%pausing_keep_force get_time_pos"))
        (when (zerop (mplayer-file-length *climplayer-frame*))
          (mplayer-command "pausing_keep_force get_property length"))
        (setf (volume-updated *climplayer-frame*)
              (get-universal-time)))
      (let ((line (read-line-no-hang mplayer-out-stream)))
        (multiple-value-bind (start end)
            (cl-ppcre:scan "ANS_volume=[^\.]*."
                           line)
          (when start
            (let ((new-volume (parse-integer (subseq line
                                                     (+ start 11)
                                                     (1- end)))))
              (when (not (equal new-volume
                                (volume *climplayer-frame*)))
                (setf (volume *climplayer-frame*)
                      new-volume)
                (redisplay-controls *climplayer-frame*)))))
        (when (eq (type-of (currently-playing *climplayer-frame*))
                  'net-node)
          (multiple-value-bind (start end)
              (cl-ppcre:scan "StreamTitle='[^;]*."
                             line)
                  (when start
                    (setf (current-title *climplayer-frame*)
                          (subseq line
                                  (+ start 13)
                                  (- end 2)))
                    (redisplay-climplayer-frame *climplayer-frame*))))
        (multiple-value-bind (start end)
            (cl-ppcre:scan "ANS_TIME_POSITION=[^\.]*."
                           line)
          (when start
            (let ((new-position (parse-integer (subseq line
                                                       (+ start 18)
                                                       (- end 1)))))
              (unless (= new-position
                         (mplayer-position *climplayer-frame*))
                (setf (mplayer-position *climplayer-frame*)
                      new-position)
                (redisplay-controls *climplayer-frame*)))))
        (multiple-value-bind (start end)
            (cl-ppcre:scan "ANS_length=[^\.]*."
                           line)
          (when start
            (setf (mplayer-file-length *climplayer-frame*)
                  (parse-integer (subseq line
                                         (+ start 11)
                                         (1- end))))))))))


;;;; commands

(define-climplayer-command (com-quit :name t)
    ()
  (let ((process (mplayer-process *application-frame*))
        (thread (player-process *application-frame*)))
    (if (cl-user::process-p process)
        (cl-user::process-kill process 15))
    (with-open-file (stream (merge-pathnames (pathname "tabs.lisp")
                                             *climplayer-conf-directory*)
                            :direction :output
                            :if-exists :supersede)
      (write (mapcar (lambda (page)
                       (gadget-value (search-field page)))
                     (remove-if-not (lambda (page)
                                      (typep page 'search-page))
                                    (tab-layout-pages (find-pane-named *application-frame*
                                                                       'main-pane))))
             :stream stream))
    (clim-sys:destroy-process thread)
    (frame-exit *application-frame*)))

(define-climplayer-command (com-next-tab :keystroke (:next :control))
    ()
  (switch-tab))

(define-climplayer-command (com-next-tab2 :keystroke (#\Tab :control))
    ()
  (switch-tab))

(define-climplayer-command (com-previous-tab :keystroke (:prior :control))
    ()
  (switch-tab t))

(defun switch-tab (&optional left)
  (let* ((main-pane (find-pane-named *climplayer-frame* 'main-pane))
         (current-page (tab-layout-enabled-page main-pane))
         (pages (tab-layout-pages main-pane))
         (n (if left
                -1
                1)))
    (switch-to-page (nth (mod (+ n (position current-page
                                            pages))
                              (length pages))
                         pages))
    (redisplay-frame-panes *application-frame*)))

(define-climplayer-command (com-config :name t)
    ()
  (stop-monitor)
  (let ((media-dirs (copy-list *media-directories*)))
    (run-frame-top-level (make-application-frame 'select))
    (unless (equal *media-directories*
                   media-dirs)
      (setf (container-node *application-frame*)
            (make-container-node *media-directories*)
            (currently-browsing *application-frame*)
            (container-node *application-frame*))
      (refresh-playlist)))
  (monitor-directories))

(define-climplayer-command (com-url :name t)
    ((url 'string :prompt "url")
     &key
     (parse 'boolean :default t))
  (ensure-that-node-is-visible
   (car (add-urls-to-play-list (list (if parse
                                         (parse-for-real-url url)
                                         url))))))

(define-climplayer-command (com-stop :name t :keystroke (:return :control))
    ()
  (stop t)
  (setf (previously-played *application-frame*)
        nil)
  (redisplay-frame-panes *application-frame*))

(define-climplayer-command (com-timer-on :name t)
    ((hour 'integer :prompt "hour")
     (min 'integer :prompt "minute")
     (node 'media-node :prompt "file or url"))
  (setf (timer *application-frame*)
        (list hour min node)))

(define-climplayer-command (com-timer-off :name t)
    ()
  (setf (timer *application-frame*) nil))

(define-climplayer-command (com-remove) ((node 'net-node))
  (setf (net-nodes *application-frame*)
        (delete node (net-nodes *application-frame*)))
  (refresh-playlist)
  (save-urls))

(define-presentation-to-command-translator node-to-remove-translator
    (net-node com-remove climplayer
              :echo t
              :documentation "Remove"
              :pointer-documentation "Remove")
    (object)
  (list object))

(defparameter *dont-monitor* nil)

(define-climplayer-command (com-delete) ((node 'file-node))
  (setf *dont-monitor* t)
  (unwind-protect
       (let ((path (node-path-name node)))
         (when (probe-file path)
           (delete-file path))
         (when (not (probe-file path))
           (setf (node-contents (node-parent node))
                 (delete node (node-contents (node-parent node))))
           (refresh-playlist)))
    (setf *dont-monitor* nil)))

(define-presentation-to-command-translator node-to-delete-translator
    (file-node com-delete climplayer
               :echo t
               :documentation "Delete file"
               :pointer-documentation "Delete file"
               :priority -1)
    (object)
  (list object))

(define-climplayer-command (com-next :name t :keystroke (:right :control))
    ()
  (play-relative-to-current 1))

(define-climplayer-command (com-previous :name t :keystroke (:left :control))
    ()
  (play-relative-to-current -1))

(define-climplayer-command (com-increase-speed :keystroke (#\+ :control))
    ()
  (setf (player-speed *application-frame*)
        (+ (player-speed *application-frame*)
           1/40))
  (mplayer-command (format nil "speed_set ~A"
                           (float (player-speed *application-frame*))))
  (redisplay-frame-panes *application-frame*))

(define-climplayer-command (com-decrease-speed :keystroke (#\- :control))
    ()
  (setf (player-speed *application-frame*)
        (- (player-speed *application-frame*)
           1/40))
  (mplayer-command (format nil "speed_set ~A"
                           (float (player-speed *application-frame*))))
  (redisplay-frame-panes *application-frame*))

(define-climplayer-command (com-listener :name t)
    ()
  (clim-listener:run-listener :new-process t))

(define-climplayer-command (com-seek-forward :keystroke (:right))
    ()
  (mplayer-command "seek 10"))

(define-climplayer-command (com-seek-back :keystroke (:left))
    ()
  (mplayer-command "seek -10"))

(define-climplayer-command (com-seek-forward-more :keystroke (:up))
    ()
  (mplayer-command "seek 60"))

(define-climplayer-command (com-seek-back-more :keystroke (:down))
    ()
  (mplayer-command "seek -60"))

(defun tab-pane-application-pane (pane)
  (if (eq (type-of pane)
          'clim:application-pane)
      pane
      (mapcan #'tab-pane-application-pane
              (sheet-children pane))))

(defgeneric pane-to-scroll (frame))

(defmethod pane-to-scroll ((frame climplayer))
  (let ((enabled (tab-layout-enabled-page (find-pane-named *climplayer-frame*
                                                           'main-pane))))
    (if (typep enabled 'search-page)
        (results enabled)
        (tab-pane-application-pane (tab-page-pane enabled)))))

(define-climplayer-command (com-scroll-up :keystroke (:prior))
    ()
  (let ((pane (pane-to-scroll *application-frame*)))
    (scroll-page pane t)))

(define-climplayer-command (com-scroll-down :keystroke (:next))
    ()
  (let* ((pane (pane-to-scroll *application-frame*)))
    (scroll-page pane)))

(define-climplayer-command (com-play-or-pause :keystroke (#\  :control))
    ()
  (let ((process (mplayer-process *application-frame*)))
    (if (and process
             (cl-user::process-alive-p process))
        (mplayer-command "pause")
        (com-play (if (and (random-playbackp *application-frame*)
                           (current-playlist *application-frame*))
                      (nth (random (length (current-playlist *application-frame*)))
                           (current-playlist *application-frame*))
                      (car (current-playlist *application-frame*)))))))

(define-climplayer-command (com-play :name t) ((node 'media-node :prompt "file or url"))
  (setf (next-to-play *application-frame*)
        node)
  (if (mplayer-process *application-frame*)
      (cl-user::process-kill (mplayer-process *application-frame*)
                             15))
  (ensure-that-node-is-visible node))

(define-presentation-to-command-translator node-to-play-translator
    (media-node com-play climplayer
                :echo nil
                :menu t
                :documentation "Play file"
                :pointer-documentation "Play file"
                :priority 3)
    (object)
  (list object))

(define-climplayer-command (com-play-next)
    ((node 'media-node))
  (setf (next-to-play *application-frame*)
        node))

(define-presentation-to-command-translator node-to-play-next-translator
    (media-node com-play-next climplayer
                :echo nil
                :menu t
                :documentation "Play file next"
                :pointer-documentation "Play file next"
                :priority 1)
    (object)
  (list object))

(define-climplayer-command (com-show :name t)
    ((node 'media-node :prompt "file or url"))
  (setf (currently-browsing *application-frame*)
        (if (eq (type-of node)
                'net-node)
            (container-node *application-frame*)
            (node-parent node)))
  (ensure-that-node-is-visible node :top t))

(define-presentation-to-command-translator node-to-show-translator
    (media-node com-show climplayer
                :echo nil
                :gesture :describe
                :menu t
                :documentation "show"
                :pointer-documentation "show in browser")
    (object)
  (list object))

(define-climplayer-command (com-show-current :keystroke (:home :control :shift))
    ()
  (when (currently-playing *application-frame*)
    (com-show (currently-playing *application-frame*))))

(define-climplayer-command (com-show-top :keystroke (:home :control))
    ()
  (setf (currently-browsing *application-frame*)
        (container-node *application-frame*))
  (scroll-extent (find-pane-named *application-frame*
                                  'browser)
                 0
                 0))


;;;; directory monitoring

(defun monitor-directories ()
  (with-accessors ((monitor-process monitor-process)
                   (container-node container-node))
    *climplayer-frame*
    (let ((dirs (mapcar #'node-string-name
                        (node-contents container-node))))
      (stop-monitor)
      (setf monitor-process
            (cl-user::run-program "fileschanged"
                                  (append (list "-r"
                                                "-s"
                                                "deleted,created")
                                          dirs)
                                  #+sbcl :search #+sbcl t
                                  :output :stream
                                  :wait nil)))))

(defun stop-monitor ()
  (with-accessors ((monitor-process monitor-process))
    *climplayer-frame*
    (when (and (cl-user::process-p monitor-process)
               (cl-user::process-alive-p monitor-process))
      (cl-user::process-kill monitor-process 15)
      (close (cl-user::process-output monitor-process)))))

(defun get-changed-nodes ()
  (when (and (cl-user::process-p (monitor-process *climplayer-frame*))
             (cl-user::process-alive-p (monitor-process *climplayer-frame*))
             (< 10 (- (get-universal-time)
                     (last-update *climplayer-frame*)))
             (not *dont-monitor*))
    (let ((stream (cl-user::process-output (monitor-process *climplayer-frame*))))
      (do ((dir (read-line-no-hang stream)
                (read-line-no-hang stream))
           (list nil
                 (cons dir list)))
          ((not dir)
           (setf (last-update *climplayer-frame*)
                 (get-universal-time))
           (remove-duplicates
            (mapcar (lambda (string)
                      (find-containing-node (container-node *climplayer-frame*)
                                            string))
                    (remove-duplicates list
                                       :test #'equal))))))))

(defun read-line-no-hang (stream)
  (do ((char (read-char-no-hang stream
                                nil
                                nil)
             (read-char-no-hang stream
                                nil
                                nil))
       (list nil
             (cons char list)))
      ((or (not char)
           (eq char #\Newline))
       (when list
         (coerce (reverse (remove #\Newline list))
                 'string)))))


(defun find-containing-node (node string)
  (let ((nodes (find-node node (regexify string))))
    (if nodes
        (if (or (not (typep (car nodes)
                            'directory-node))
                (> (length nodes)
                   1))
            (node-parent (car nodes))
            (car nodes))
        (find-containing-node node
                              (subseq string
                                      0
                                      (position #\/
                                                string
                                                :from-end t))))))


;;;; search-panes

(define-climplayer-command (com-search :name t :keystroke (#\s :control))
    ()
  (new-search-tab))

(define-climplayer-command (com-follow-list)
    ((page 'search-page))
  (setf (play-list-tab *application-frame*)
        (if (typep page 'search-page)
            page
            nil)))

(define-presentation-to-command-translator tab-page-to-follow-translator
    (tab-page com-follow-list climplayer
              :tester ((object) (or (typep object 'search-page)
                                    (and (typep object 'tab-page)
                                         (equal (tab-page-title object)
                                                "Browser"))))
              :echo nil
              :menu t
              :documentation "Follow list"
              :pointer-documentation "Follow list"
              :priority -1)
    (object)
  (list object))

(define-climplayer-command (com-follow-current-tab
                            :keystroke (#\p :control))
    ()
  (let ((page (tab-layout-enabled-page (find-pane-named *application-frame*
                                                        'main-pane))))
    (com-follow-list page)))

(define-climplayer-command (com-close)
    ((page 'search-page))
  (remove-page page))

(define-presentation-to-command-translator search-page-to-close-translator
    (tab-page com-close climplayer
              :tester ((object) (typep object 'search-page))
              :gesture :describe
              :echo nil
              :menu t
              :documentation "Close search tab"
              :pointer-documentation "Close search tab"
              :priority -1)
    (object)
  (list object))

(define-climplayer-command (com-close-current-search-tab
                            :keystroke (#\w :control))
    ()
  (let ((page (tab-layout-enabled-page (find-pane-named *application-frame*
                                                        'main-pane))))
    (when (typep page
                 'search-page)
      (remove-page page)
      (redisplay-frame-panes *application-frame*))))

(defclass search-page (tab-page)
  ((last-search :initform ""
                :accessor last-search)
   (display-all :initform nil
                :accessor display-all)
   (search-field :initarg :search-field
                 :accessor search-field)
   (results :initarg :results
            :accessor results)
   (text-field-focusedp :initform t
                        :accessor text-field-focusedp)))

(defun new-search-tab ()
  (with-look-and-feel-realization ((frame-manager *application-frame*)
                                   *application-frame*)
    (let* ((search-field (make-pane 'text-field
                                    :editable-p t
                                    :value-changed-callback #'update-results
                                    :armed-callback #'toggle-focusedp
                                    :disarmed-callback #'toggle-focusedp))
           (results (make-pane 'application-pane
                               :scroll-bars :vertical
                               :incremental-redisplay t
                               :display-function 'display-results))
           (tab-layout (find-pane-named *application-frame*
                                        'main-pane))
           (page (make-instance 'search-page
                                :title "Search"
                                :pane (vertically ()
                                        (outlining ()
                                          search-field)
                                        (outlining ()
                                          (scrolling (:scroll-bar :vertical)
                                            results)))
                                :search-field search-field
                                :results results)))
      (setf (tab-layout-pages tab-layout)
            (reverse (cons page
                           (reverse (tab-layout-pages tab-layout))))
            (tab-layout-enabled-page tab-layout)
            page)
      (redisplay-frame-panes *application-frame*)
      page)))

(defun toggle-focusedp (gadget)
  (let* ((parent (sheet-parent (sheet-parent (sheet-parent gadget))))
         (page (when (sheet-parent parent)
                 (sheet-to-page parent))))
    (when page
      (setf (text-field-focusedp page)
            (not (text-field-focusedp page))))))

(defun update-results (gadget value)
  (declare (ignore value))
  (when (sheet-parent gadget)
    (let* ((page (sheet-to-page (sheet-parent (sheet-parent (sheet-parent gadget)))))
           (results (results page)))
      (unless (equal (last-search page)
                     (gadget-value gadget))
        (setf (last-search page)
              (gadget-value gadget)
              (display-all page)
              nil)
        (redisplay-frame-pane *application-frame*
                              results)
        (scroll-extent results
                       0
                       0)))))

(define-climplayer-command (com-list-all :keystroke (#\l :control))
    ()
  (let ((enabled (tab-layout-enabled-page (find-pane-named *application-frame*
                                                           'main-pane))))
    (when (typep enabled 'search-page)
      (setf (display-all enabled)
            (not (display-all enabled)))
      (redisplay-frame-panes *application-frame*))))

(defun display-results (frame pane)
  (when (sheet-parent pane)
    (with-end-of-line-action (pane :allow)
      (with-text-size (pane *climplayer-text-size*)
        (let* ((page (sheet-to-page (sheet-parent (sheet-parent (sheet-parent (sheet-parent pane))))))
               (matching (find-node (play-list frame)
                                    (gadget-value (search-field page)))))
          (if (not matching)
              (format pane "No matches~%")
              (do* ((matching (reverse matching)
                              (cdr matching))
                    (n (if (display-all page)
                           (length matching)
                           50)
                       (1- n)))
                   ((or (not matching)
                        (= n 0))
                    (if matching
                        (with-output-as-presentation (pane `(com-list-all) 'command )
                          (format pane "~%~A more matches"
                                  (length matching)))))
                (let* ((node (car matching)))
                  (updating-output (pane :unique-id node
                                         :id-test #'eq
                                         :cache-value (node-status node)
                                         :cache-test #'eq)
                    (with-output-as-presentation (pane node (type-of node))
                      (cond ((eq node (currently-playing frame))
                             (with-drawing-options (pane :ink +darkgreen+)
                               (with-text-face (pane :bold)
                                 (write-line (title node)
                                             pane))))
                            ((eq node (next-to-play frame))
                             (with-drawing-options (pane :ink +lightgray+)
                               (write-line (title node)
                                           pane)))
                            (t
                             (write-line (title node)
                                         pane)))))))))))))

(defmethod (setf tab-layout-enabled-page) :after (page
                                                  (parent tab-layout))
  (when (and (equal (pane-name parent)
                    'main-pane)
             (not (typep page 'search-page))
             (typep (port-keyboard-input-focus (port *application-frame*))
                    'climi::drei-text-field-substrate))
    (let* ((new (find-pane-named *application-frame*
                                 'interactor))
           (previous (stream-set-input-focus new)))
      (disarmed-callback previous (gadget-client previous) (gadget-id previous))
      (toggle-focusedp previous))))

(defmethod (setf tab-layout-enabled-page) :after ((page search-page)
                                                  (parent tab-layout))
  (let ((gadget (climi::substrate (search-field page)))
        (previous-focus (port-keyboard-input-focus (port *application-frame*))))
    (cond ((text-field-focusedp page)
           (setf (port-keyboard-input-focus (port *application-frame*))
                 gadget)
           (armed-callback gadget (gadget-client gadget) (gadget-id gadget))
           (toggle-focusedp gadget))
          (t
           (stream-set-input-focus (find-pane-named *application-frame*
                                                    'interactor))))
    (when (typep previous-focus
                 'climi::drei-text-field-substrate)
      (disarmed-callback previous-focus
                         (gadget-client previous-focus)
                         (gadget-id previous-focus))
      (toggle-focusedp previous-focus))))

(defun redisplay-all-search-tabs (frame)
  (let* ((search-tabs (remove-if-not (lambda (page)
                                       (typep page 'search-page))
                                     (tab-layout-pages (find-pane-named frame
                                                                        'main-pane)))))
    (mapcar (lambda (pane)
              (redisplay-frame-pane frame
                                    pane))
            (mapcar #'results search-tabs))))
