
;(load "/usr/local/lisp/cm/src/cm.scm")
(load "/usr/local/lisp/cmt/cmt.scm")
(load "/usr/local/lisp/cmt/gnuplot.scm")


(use-system :rts)

;;create file to write events
(define cmt1 (new cmt-stream :name "/tmp/cmt1.txt"))


(rts cmt1)


(define (reps num wai)
  (process repeat num
           output (new cmt-on :time (now) :args (gc-stat))
           wait wai
           finally (format #t "DONE!~%")))
  

;;;; run 1 

(rts cmt1)

(begin
  (open-io cmt1 :output)
  (sprout (reps 1000 .1)))

(close-io cmt1)


(define t1 (import-events cmt1))

(map-objects print t1)
;;;should see entries like :
;; #i(cmt-on args ("(:total-heap-size" "19079168" ":free-bytes" "974848" ":bytes-since-gc" "7901008" ":total-bytes" "44029172)") time 134.92506)
;;#i(cmt-on args ("(:total-heap-size" "19079168" ":free-bytes" "962560" ":bytes-since-gc" "7916896" ":total-bytes" "44045060)") time 135.025097)
;;#i(cmt-on args ("(:total-heap-size" "19079168" ":free-bytes" "946176" ":bytes-since-gc" "7932856" ":total-bytes" "44061020)") time 135.12513)



(begin
  (define *data* (list))
  (define *last-time* 0)
  (map-objects (lambda (x)
                 (begin
                   (push (- x *last-time*) *data*)
                   (if (> (- x *last-time*) .18)
                       (print x))
                   (set! *last-time* x)))
               t1 :slot 'time))




(display-plot (new plot :data  (drop-right *data* 1)))

(rts-stop)

;;;;; run 2

(rts cmt1 :gc #t)

(begin
  (open-io cmt1 :output)
  (sprout (reps 1000 .1)))

(close-io cmt1)


(define t1 (import-events cmt1))

(map-objects print t1)

(begin
  (define *data* (list))
  (define *last-time* 0)
  (map-objects (lambda (x)
                 (begin
                   (push (- x *last-time*) *data*)
                   (if (> (- x *last-time*) .18)
                       (print x))
                   (set! *last-time* x)))
               t1 :slot 'time))

(display-plot (new plot :data  (drop-right *data* 1)))

(rts-stop)

;;; the one spike i see here when
;;; gc happens otherwise very stable

;;#i(cmt-on args ("(:total-heap-size" "19079168" ":free-bytes" "385024" ":bytes-since-gc" "8392352" ":total-bytes" "61545596)") time 61.215567)
;;#i(cmt-on args ("(:total-heap-size" "19079168" ":free-bytes" "6934528" ":bytes-since-gc" "14376" ":total-bytes" "61561524)") time 61.590558)
;;#i(cmt-on args ("(:total-heap-size" "19079168" ":free-bytes" "6934528" ":bytes-since-gc" "30264" ":total-bytes" "61577412)") time 61.690598)


;;;;; run 3

(rts cmt1 :gc 100)

(begin
  (open-io cmt1 :output)
  (sprout (reps 1000 .1)))

(close-io cmt1)


(define t1 (import-events cmt1))

(map-objects print t1)

(begin
  (define *data* (list))
  (define *last-time* 0)
  (map-objects (lambda (x)
                 (begin
                   (push (- x *last-time*) *data*)
                   (if (> (- x *last-time*) .18)
                       (print x))
                   (set! *last-time* x)))
               t1 :slot 'time))

;;;this plot should be much more erratic

(display-plot (new plot :data  (drop-right *data* 1)))


(rts-stop)

;;;

;;;utility

(define (import-and-plot cmt)
  (let ((t1 (import-events cmt))
	(*data* (list))
	(*last-time* 0))
    (map-objects (lambda (x)
                   (begin
		      (push (- x *last-time*) *data*)
		      (if (> (- x *last-time*) .12)
			  (print x))
		      (set! *last-time* x)))
		 t1 :slot 'time)
    (display-plot (new plot :data  (reverse (butlast *data* ))))))




;;; test sequence 1


;;;gauche test 1

(rts cmt1 :priority 38)

(define (reps num wai)
  (process repeat num
           output (new cmt-on :time (now))
           wait wai
           finally (format #t "DONE!~%")))


(begin
  (open-io cmt1 :output)
  (sprout (reps 1000 .1)))

(close-io cmt1)
(rts-stop)

(import-and-plot cmt1)

;;;gauche test 2

(rts cmt1 :priority 38)

(define (reps num wai)
  (process repeat num
	   with x = (new cmt-on :time (now))
	   do (slot-set! x 'time (now))
	   output x
           wait wai
           finally (format #t "DONE!~%")))

(begin
  (open-io cmt1 :output)
  (sprout (reps 1000 .1)))

(close-io cmt1)
(rts-stop)

(import-and-plot cmt1)


;;;gauche test 3

(rts cmt1 :priority 38 :policy 1)

(define (reps num wai)
  (process repeat num
	   with x = (new cmt-on :time (now))
	   do (slot-set! x 'time (now))
	   output x
           wait wai
           finally (format #t "DONE!~%")))

(begin
  (open-io cmt1 :output)
  (sprout (reps 1000 .1)))

(close-io cmt1)
(rts-stop)

(import-and-plot cmt1)
