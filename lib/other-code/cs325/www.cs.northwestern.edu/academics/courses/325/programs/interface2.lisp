;;; Example Macintosh Common Lisp interface code
;;; With named constants.
;;; Try (make-message-box "Pick a button" :continue :quit)

(defconstant *box-size* #@(300 150))
(defconstant *button-size* #@(70 20))
(defconstant *separation* 4)

(defconstant *border-space* (* 2 *separation*))

(defconstant *button-space*
  (add-points *button-size*
              (make-point *separation* *separation*)))

(defconstant *button-row*
  (point-v (subtract-points *box-size* *button-space*)))

(defconstant *text-size*
  (subtract-points *box-size*
                   (make-point *border-space*
                               (+ *border-space*
                                  (point-v *button-space*)))))


(defclass message-box (window) ()
  (:default-initargs
    :view-position :centered
    :view-size *box-size*
    :window-type :double-edge-box
    :close-box-p nil))

(defclass message-text (static-text-dialog-item) ()
  (:default-initargs
    :view-position
    (make-point *separation* *separation*)
    :view-size *text-size*))

(defclass message-button (button-dialog-item) ()
  (:default-initargs
    :view-size *button-size*
    :dialog-item-action 'print-and-close))

(defmethod print-and-close ((item message-button))
  (print (view-nick-name item))
  (window-close (view-window item)))

(defun make-message-box (text &rest labels)
  (make-instance 'message-box
    :view-subviews
    (cons (make-message-text text)
          (make-buttons labels))))

(defun make-message-text (text)
  (make-instance 'message-text
    :dialog-item-text text))

(defun make-buttons (labels)
  (loop for i from (length labels) downto 1
        for label in labels
        collect (make-message-button i label)))

(defun make-message-button (i label)
  (make-instance 'message-button
    :view-position
    (make-point (- (point-h *box-size*)
                   (* i (point-h *button-space*)))
                *button-row*)
    :view-nick-name label
    :dialog-item-text (string label)))

