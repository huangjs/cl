(in-package :climplayer)

(defvar *climplayer-text-size* :small)

(defvar *climplayer-conf-directories-file* nil)

(define-application-frame select ()
  ((current-directory :initform (user-homedir-pathname)
                      :accessor current-directory)
   (selected-directories :initform *media-directories*
                         :accessor selected-directories))
  (:panes
   (browser :application
            :width 500
            :height 300
            :incremental-redisplay t
            :display-function 'display-browser)
   (selected :application
             :width 500
             :height 200
             :incremental-redisplay t
             :display-function 'display-selected)
   (pointer-doc :pointer-documentation))
  (:layouts
   (default (vertically ()
              browser
              selected
              (horizontally ()
                (make-pane 'push-button
                           :label "ok"
                           :activate-callback
                           (lambda (button)
                             (declare (ignore button))
                             (set-media-directories
                              (selected-directories *application-frame*))
                             (frame-exit *application-frame*)))
                (make-pane 'push-button
                           :label "cancel"
                           :activate-callback
                           (lambda (button)
                             (declare (ignore button))
                             (frame-exit *application-frame*))))
              (20 pointer-doc)))))

(defun set-media-directories (list)
  (setf *media-directories*
        list)
  (with-open-file (s *climplayer-conf-directories-file*
                     :direction :output
                     :if-exists :supersede)
    (write *media-directories*
           :stream s)))

(defun print-pathname (path stream &optional pattern name)
  (let ((pattern (or pattern
		     (clim-listener::icon-of path)))
	(name (or name
		  (pathname-to-string path))))
    (with-output-as-presentation (stream path 'pathname)
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
					  0))
      (write-line name stream))))

(defun display-browser (frame pane)
  (with-end-of-line-action (pane :allow)
    (with-text-size (pane *climplayer-text-size*)
      (print-pathname (make-pathname
		       :directory (reverse (cdr (reverse (pathname-directory (current-directory *application-frame*))))))
		      pane
		      (clim-listener::standard-icon "up-folder.xpm")
		      "..")
      (dolist (dir (list-dirs (current-directory frame)))
        (unless (equal (char (car (last (pathname-directory dir)))
                             0)
                       #\.)
          (updating-output (pane :unique-id dir
                                 :id-test #'equal
                                 :cache-value dir
                                 :cache-test #'equal)
	    (print-pathname dir
			  pane)))))))

(defun display-selected (frame pane)
  (with-end-of-line-action (pane :allow)
    (with-text-size (pane *climplayer-text-size*)
      (write-string "selected:"
                    pane)
      (terpri pane)
      (dolist (dir (selected-directories frame))
        (updating-output (pane :unique-id dir
                               :id-test #'equal
                               :cache-value dir
                               :cache-test #'equal)
          (print-pathname dir
			  pane))))))

(define-select-command (com-show-directory :name t)
    ((path 'pathname))
  (setf (current-directory *application-frame*)
        path)
  (scroll-extent (find-pane-named *application-frame*
                                  'browser)
                 0
                 0))

(define-presentation-to-command-translator path-to-show-directory-translator
    (pathname com-show-directory select
              :menu t
              :documentation "Show directory"
              :pointer-documentation "Show directory")
    (object)
  (list object))

(define-select-command (com-select-directory :name t)
    ((path 'pathname))
  (setf (selected-directories *application-frame*)
        (nreverse (cons path
                        (nreverse (selected-directories *application-frame*))))))

(define-presentation-to-command-translator path-to-select-translator
    (pathname com-select-directory select
              :tester ((object)
                       (not (find object
                                  (selected-directories *application-frame*)
                                  :test #'equal)))
              :gesture :describe
              :menu t
              :documentation "Select directory"
              :pointer-documentation "Select directory")
    (object)
  (list object))

(define-select-command (com-remove-directory :name t)
    ((path 'pathname))
  (setf (selected-directories *application-frame*)
        (remove path
                (selected-directories *application-frame*))))


(define-presentation-to-command-translator path-to-remove-translator
    (pathname com-remove-directory select
              :tester ((object)
                       (find object
                             (selected-directories *application-frame*)
                             :test #'equal))
              :gesture :describe
              :menu t
              :documentation "Remove directory"
              :pointer-documentation "Remove directory")
    (object)
  (list object))