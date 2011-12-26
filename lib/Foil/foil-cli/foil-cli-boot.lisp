
;(setf foil:*fvm* (make-instance 'foil:foreign-vm
;             :stream
;             (sys:open-pipe "c:/dev/foil/FoilCLISvr/bin/debug/FoilCLISvr.exe")))

(eval-when (:compile-toplevel :load-toplevel)
(require "comm"))

(use-package :foil)

(defvar *ui-stream*)
(defvar *non-ui-stream*)
(defvar *display*)

;(setf *ui-stream* (comm:open-tcp-stream "localhost" 13479))
(setf *non-ui-stream* (comm:open-tcp-stream "localhost" 13478))
(setf *fvm* (make-instance 'foreign-vm :stream *non-ui-stream*))

;(close *ui-stream*)
;(close *non-ui-stream)

;(setf *fvm* (make-instance 'foreign-vm
;             :stream
;             (comm:open-tcp-stream "localhost" 13245)));



