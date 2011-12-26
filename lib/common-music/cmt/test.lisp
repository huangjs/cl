(in-package :cm)

(load "/usr/local/lisp/cmt/cmt.lisp")
(load "/usr/local/lisp/cmt/gnuplot.lisp")



(use-system :rts)

;;create file to write events
(defparameter cmt1 (new cmt-stream :name "/tmp/cmt1.txt"))


(defparameter *gc-stat-stream* (make-string-output-stream))

(defun gc-stat ()
  (let ((st *standard-output*)
	(res nil))
    (unwind-protect
	 (progn
	   (setf *standard-output* *gc-stat-stream*)
	   (room)
	   (setf res (get-output-stream-string *gc-stat-stream* )))
      (setf *standard-output* st))
    (remove #\newline res)))

(gc-stat)






(defun reps (num wai)
  (process repeat num
           output (new cmt-on :time (now) :args (time (gc-stat)))
           wait wai
           finally (format t "DONE!~%")))
  
;;;; run 1 

(rts cmt1 )

(progn
  (open-io cmt1 :output)
  (sprout (reps 100 .1)))

(close-io cmt1)
(rts-stop)

(defparameter t1 (import-events cmt1))

(map-objects #'print t1)



(begin
  (defparameter *data* (list))
  (defparameter *last-time* 0)
  (map-objects #'(lambda (x)
		   (begin
		    (push (- x *last-time*) *data*)
		    (if (> (- x *last-time*) .18)
			(print x))
		    (setf *last-time* x)))
               t1 :slot 'time))

(display-plot (new plot :data  (butlast *data* )))



;;;; run 2

(rts cmt1 :priority 47)

(progn
  (open-io cmt1 :output)
  (sprout (reps 1000 .1)))

(close-io cmt1)
(rts-stop)

(defparameter t1 (import-events cmt1))

(map-objects #'print t1)



(begin
  (defparameter *data* (list))
  (defparameter *last-time* 0)
  (map-objects #'(lambda (x)
		   (begin
		    (push (- x *last-time*) *data*)
		    (if (> (- x *last-time*) .12)
			(print x))
		    (setf *last-time* x)))
               t1 :slot 'time))


(display-plot (new plot :data  (reverse (butlast *data* ))))






;;; test sequence 1


;;;utility
(defun import-and-plot ( cmt)
  (let ((t1 (import-events cmt))
	(*data* '())
	(*last-time* 0))
    (map-objects #'(lambda (x)
		     (begin
		      (push (- x *last-time*) *data*)
		      (if (> (- x *last-time*) .12)
			  (print x))
		      (setf *last-time* x)))
		 t1 :slot 'time)
    (display-plot (new plot :data  (reverse (butlast *data* ))))))


;;;;test 1

(rts cmt1 :priority 38)

(defun reps (num wai)
  (process repeat num
           output (new cmt-on :time (now))
           wait wai
           finally (format t "DONE!~%")))


(progn
  (open-io cmt1 :output)
  (sprout (reps 1000 .1)))

(close-io cmt1)
(rts-stop)

(import-and-plot cmt1)



;;;test two

(rts cmt1 :priority 38)

(defun reps (num wai)
  (process repeat num
	   with x = (new cmt-on :time (now))
	   do (setf (slot-value x 'time) (now))
	   output x
           wait wai
           finally (format t "DONE!~%")))

(progn
  (open-io cmt1 :output)
  (sprout (reps 1000 .1)))

(close-io cmt1)
(rts-stop)

(import-and-plot cmt1)


;;test three 


(rts cmt1 :priority 38 :policy 1)

(defun reps (num wai)
  (process repeat num
	   with x = (new cmt-on :time (now))
	   do (setf (slot-value x 'time) (now))
	   output x
           wait wai
           finally (format t "DONE!~%")))

(progn
  (open-io cmt1 :output)
  (sprout (reps 1000 .1)))

(close-io cmt1)
(rts-stop)

(import-and-plot cmt1)



