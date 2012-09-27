(in-package :tester)

(define-application-frame tester ()
  ((chain :initform (make-instance 'standard-cursorchain
                                   :element-type 'character
                                   :fill-element #\_)
          :reader chain)
   (cursors :initform (make-array 2) :reader cursors))
  (:panes
   (app :application :width 800 :height 300 :display-function 'display-app)
   (int :interactor :width 800 :height 500))
  (:layouts (default (vertically () app int))))

(defmethod initialize-instance :after ((frame tester) &rest args)
  (declare (ignore args))
  (with-slots (chain cursors) frame
     (setf (aref cursors 0)
           (make-instance 'left-sticky-flexicursor :chain chain))
     (setf (aref cursors 1)
           (make-instance 'right-sticky-flexicursor :chain chain))))

(defun run-tester ()
  (loop for port in climi::*all-ports*
        do (destroy-port port))
  (setq climi::*all-ports* nil)
  (run-frame-top-level (make-application-frame 'tester)))

(defun display-app (frame pane)
  (let* ((chain (chain frame))
         (buffer (slot-value chain 'flexichain::buffer))
         (length (length buffer))
         (cursors (cursors frame)))
    (format pane "nb elments: ~a~%~%" (nb-elements chain))
    (loop for i from 0 below (nb-elements chain)
          do (format pane " ~a" (element* chain i)))
    (format pane "~%")
    (loop for i from 0 below 2
          do (format pane (if (minusp (cursor-pos (aref cursors i)))
                              (make-string (* -2 (cursor-pos (aref cursors i)))
                                           :initial-element #\?)
                              (make-string (* 2 (cursor-pos (aref cursors i)))
                                           :initial-element #\space)))
             (format pane "~a~%" i))
    (format pane "~%~%")
    (format pane (if (minusp (slot-value chain 'flexichain::gap-start))
                     (make-string  (* -2 (slot-value chain 'flexichain::gap-start))
                                   :initial-element #\?)
                     (make-string  (* 2 (slot-value chain 'flexichain::gap-start))
                                   :initial-element #\space)))
    (format pane ">~%")
    (format pane (if (minusp (slot-value chain 'flexichain::gap-end))
                     (make-string (* -2 (slot-value chain 'flexichain::gap-end))
                                  :initial-element #\?)
                     (make-string (* 2 (slot-value chain 'flexichain::gap-end))
                              :initial-element #\space)))
    (format pane "<~%")
    (loop for i from 0 below length
          do (format pane "~a~a"
                     (if (= i (slot-value chain 'flexichain::data-start))
                         #\* #\Space)
                     (aref buffer i)))
    (format pane "~%")
    (loop for i from 0 below 2
          do (format pane (make-string (1+ (* 2 (slot-value (aref cursors i)
                                                            'flexichain::index)))
                               :initial-element #\space))
             (format pane "~a~a~%" i (at-end-p (aref cursors i))))
    (format pane "~%~%")))

(defmethod execute-frame-command :around ((frame tester) command)
  (declare (ignore command))
  (handler-case (call-next-method)
    (flexi-error (condition) (format (frame-standard-input *application-frame*)
                                     "~a~%" condition))))

(define-tester-command (com-empty :name t) ()
  (format (frame-standard-input *application-frame*)
          "~a~%" (flexi-empty-p (chain *application-frame*))))

(defun to-char (symbol)
  (char-downcase (aref (symbol-name symbol) 0)))

(define-tester-command (com-is :name t) ((pos 'integer) (object 'symbol))
  (insert* (chain *application-frame*) pos (to-char object)))

(define-tester-command (com-element* :name t) ((pos 'integer))
  (format (frame-standard-input *application-frame*)
          "~a~%" (element* (chain *application-frame*) pos)))

(define-tester-command (com-set-element* :name t) ((pos 'integer) (object 'symbol))
  (setf (element* (chain *application-frame*) pos) (to-char object)))

(define-tester-command (com-ds :name t) ((pos 'integer))
  (delete* (chain *application-frame*) pos))

(define-tester-command (com-push-start :name t) ((object 'symbol))
  (push-start (chain *application-frame*) (to-char object)))

(define-tester-command (com-push-end :name t) ((object 'symbol))
  (push-end (chain *application-frame*) (to-char object)))

(define-tester-command (com-pop-start :name t) ()
  (format (frame-standard-input *application-frame*)
          "~a~%" (pop-start (chain *application-frame*))))

(define-tester-command (com-pop-end :name t) ()
  (format (frame-standard-input *application-frame*)
          "~a~%" (pop-end (chain *application-frame*))))

(define-tester-command (com-rotate :name t) ((amount 'integer))
  (rotate (chain *application-frame*) amount))

(define-tester-command (com-move> :name t) ((cursor 'integer))
  (move> (aref (cursors *application-frame*) cursor)))

(define-tester-command (com-move< :name t) ((cursor 'integer))
  (move< (aref (cursors *application-frame*) cursor)))

(define-tester-command (com-ii :name t) ((cursor 'integer) (object 'symbol))
  (insert (aref (cursors *application-frame*) cursor)  (to-char object)))

(define-tester-command (com-d< :name t) ((cursor 'integer))
  (delete< (aref (cursors *application-frame*) cursor)))

(define-tester-command (com-d> :name t) ((cursor 'integer))
  (delete> (aref (cursors *application-frame*) cursor)))

(define-tester-command (com-clear :name t) ()
  (with-slots (chain cursors) *application-frame*
     (setf chain (make-instance 'standard-cursorchain
                    :element-type 'character :fill-element #\_))
     (setf (aref cursors 0)
           (make-instance 'left-sticky-flexicursor :chain chain))
     (setf (aref cursors 1)
           (make-instance 'right-sticky-flexicursor :chain chain))))

(define-tester-command (com-quit :name t) ()
  (frame-exit *application-frame*))
