(in-package :beirc)

;;; Here comes the trick:

;;; Although I would pretty much prefer an implementation of CLIM
;;; which is thread safe, I figure we better go through the central
;;; event loop. We define a new event class, subclass of
;;; WINDOW-MANAGER-EVENT, and whenever we want to update the display
;;; we send it to the frame.

(defclass foo-event (clim:window-manager-event)
  ((sheet :initarg :sheet :reader event-sheet)
   (receiver :initarg :receiver :reader receiver)))

;;for updating the time display, triggered from TICKER
(defclass bar-event (clim:window-manager-event)
  ((sheet :initarg :sheet :reader event-sheet)))

(defclass new-sheet-event (clim:window-manager-event)
     ((sheet :initarg :sheet :reader event-sheet)
      (closure :initarg :creator :reader sheet-creation-closure)
      (receiver :initarg :receiver :reader receiver)))
(defun processes-supported-p ()
  (processp (current-process)))

(defun queue-beirc-event (frame event)
  (if (processes-supported-p)
      (queue-event (frame-top-level-sheet frame)
                   event)
      (handle-event frame event)))