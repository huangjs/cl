;; Copyright (c) 2008 Accelerated Data Works, Ryan Davis

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
(require 'cl-ppcre)
(defpackage #:net.acceleration.adw-charting-benchmarking
  (:use #:cl))

(in-package #:net.acceleration.adw-charting-benchmarking)

(defun timing-graph ()
  (adw-charting:with-line-chart (300 400)
    (adw-charting:set-axis :x "series")
    (adw-charting:set-axis :y "real time (ms)")

    (loop for x from 1 to 3
       do
	 (let ((dp (* 10 x)))
	   (adw-charting:add-series
	    (format nil "~ap" dp)
	    (loop for i from 1 to 20
	       collect (list i (first (timings i dp)))))
	   (format T "done with ~a run" dp)))

    (adw-charting:save-file "series-real-time.png")
    ))

(defun timings (series points)
  (let* ((trc (with-output-to-string (*trace-output*)
	       (time (lines series points))))
	 (rt (cl-ppcre:register-groups-bind (rt)
		 ("([\\d\\.]+) seconds of real" trc)
	       (parse-integer (cl-ppcre:regex-replace-all "\\." rt ""))))
	 (bc (cl-ppcre:register-groups-bind (rt)
		 ("([\\d\\,]+) bytes" trc)
	       (parse-integer (cl-ppcre:regex-replace-all "\\," rt "")))))
    (list rt bc)))

(defun lines (num-series points-per-series)
  (adw-charting:with-line-chart (300 400)
    (dotimes (s num-series)
      (adw-charting:add-series (format nil "s~a" s)
			       (random-series points-per-series
					      0 -100 100 100))
      )
    (adw-charting:save-file "benchmarking")))

(defun random-between (min max)
  (+ min
     (random (float (- max min)))))

(defun random-point (min-x min-y max-x max-y)
  (list (random-between min-x max-x)
	(random-between min-y max-y)))

(defun random-series (n min-x min-y max-x max-y)
  (sort 
   (loop for i from 1 to n
      collect (random-point min-x min-y max-x max-y))
   #'< :key #'first))