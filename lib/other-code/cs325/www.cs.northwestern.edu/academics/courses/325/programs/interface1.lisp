;;; Example Macintosh Common Lisp interface code
;;; With anonymous constants.
;;; Try (make-message-box "Pick a button" :continue :quit)

(defclass message-box (window) ()
  (:default-initargs
    :view-position :centered
    :view-size #@(300 150)
    :window-type :double-edge-box
    :close-box-p nil))

(defclass message-text (static-text-dialog-item) ()
  (:default-initargs
    :view-position #@(4 4)
    :view-size #@(292 118)))

(defclass message-button (button-dialog-item) ()
  (:default-initargs
    :view-size #@(70 20)
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
    :view-position (make-point (- 300 (* 74 i)) 126)
    :view-nick-name label
    :dialog-item-text (string label)))
