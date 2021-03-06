;;; **********************************************************************
;;; Copyright (C) 2003 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; generated by scheme->cltl from gnuplot.scm on 04-May-2006 23:05:26

(in-package :cm)


(defparameter *gnuplot-output-directory* "/tmp")

(defparameter *gnuplot-styles* (quote
                                (:lines
                                 :points
                                 :linespoints
                                 :impulses
                                 :dots
                                 :steps
                                 :fsteps
                                 :histeps
                                 :errorbars
                                 :xerrorbars
                                 :yerrorbars
                                 :xyerrorbars
                                 :errorlines
                                 :xerrorlines
                                 :yerrorlines
                                 :boxes
                                 :filledboxes
                                 :filledcurves
                                 :boxederrorbars
                                 :boxxyerrorbars
                                 :financebars
                                 :candlesticks
                                 :vector)))

(defun interleave (list1 list2)
  (cond ((null list1) list2)
        ((null list2) list1)
        (t
         (cons (car list1)
               (cons (car list2)
                     (interleave (cdr list1) (cdr list2)))))))

(defun swap (lst)
  (if (or (null lst) (null (cdr lst)))
      lst
      (cons (second lst) (cons (first lst) (swap (cddr lst))))))

(defun max-depth (lst)
  (if (null lst)
      0
      (if (not (listp (car lst)))
          (max 1 (max-depth (cdr lst)))
          (max (+ 1 (max-depth (car lst))) (max-depth (cdr lst))))))

(progn (defclass plot ()
         ((data :initform nil :initarg :data)
          (title :initform "untitled" :initarg :title)
          (style :initform :linespoints :initarg :style)
          (autoscale :initform nil :initarg :autoscale)
          (xrange :initform nil :initarg :xrange)
          (yrange :initform nil :initarg :yrange)
          (zrange :initform nil :initarg :zrange)
          (grid :initform nil :initarg :grid)
          (surface :initform nil :initarg :surface)
          (parametric :initform nil :initarg :parametric)
          (ticslevel :initform nil :initarg :ticslevel)
          (margin :initform nil :initarg :margin)
          (border :initform nil :initarg :border)
          (origin :initform nil :initarg :origin)
          (size :initform nil :initarg :size)))
       (defparameter <plot> (find-class 'plot))
       (finalize-class <plot>)
       (values))


(set-env-var "GNUTERMAPP" "/sw/Applications/AquaTerm.app")
(set-env-var "GNUTERM" "aqua")

(defmethod display-plot ((obj plot) &rest args)
  args
  (let ((dt 0)
        (depth (max-depth (slot-value obj 'data)))
        (data (slot-value obj 'data))
        (title (slot-value obj 'title))
        (autoscale (slot-value obj 'autoscale))
        (xrange (slot-value obj 'xrange))
        (yrange (slot-value obj 'yrange))
        (zrange (slot-value obj 'zrange))
        (grid (slot-value obj 'grid))
        (surface (slot-value obj 'surface))
        (parametric (slot-value obj 'parametric))
        (ticslevel (slot-value obj 'ticslevel))
        (margin (slot-value obj 'margin))
        (border (slot-value obj 'border))
        (origin (slot-value obj 'origin))
        (size (slot-value obj 'size))
        (style (slot-value obj 'style)))
    (with-open-file (plot-port 
		     (concatenate 'string *gnuplot-output-directory* "/tmp.dem")
		     :direction :output :if-exists :supersede)
      (when title (format plot-port "set title ~S~%" title))
      (when autoscale (format plot-port "set autoscale~%"))
      (when xrange
	(format plot-port
		"set xrange [~f:~f]~%"
		(first xrange)
		(second xrange)))
      (when yrange
	(format plot-port
		"set yrange [~f:~f]~%"
		(first yrange)
		(second yrange)))
      (when zrange
	(format plot-port
		"set yrange [~f:~f]~%"
		(first zrange)
		(second zrange)))
      (when grid
	(format plot-port
		"set grid xtics; set grid ytics; set grid ztics~%"))
      (when surface (format plot-port "set surface~%"))
      (when parametric (format plot-port "set parametric~%"))
      (when ticslevel
	(format plot-port "set ticslevel ~s~%" ticslevel))
      (when margin
	(format plot-port "set tmargin ~s~%" margin)
	(format plot-port "set lmargin ~s~%" margin)
	(format plot-port "set rmargin ~s~%" margin)
	(format plot-port "set bmargin ~s~%" margin))
      (when border (format plot-port "set border ~s~%" border))
      (when origin
	(format plot-port
		"set origin ~s,~s~%"
		(coerce (first origin) 'float)
		(coerce (second origin) 'float)))
      (when size
	(format plot-port
		"set size ~s,~s~%"
		(coerce (first size) 'float)
		(coerce (second size) 'float)))
      (with-open-file (data-port 
		       (concatenate 'string *gnuplot-output-directory* "/tmp.dat")
		       :direction :output :if-exists :supersede)
	(dotimes (k depth)
	  (progn (if (= depth 1)
		     (setf dt data)
		     (setf dt (elt data k)))
		 (when (and (> depth 1) (= k 0))
		   (format plot-port "set multiplot~%"))
		 (format plot-port
			 "plot '/tmp/tmp.dat' index ~A "
			 k)
		 (when style
		   (format plot-port
			   "with ~a ~%"
			   (keyword->string style)))
		 (loop for x from 0 by 0.238
		    for y in dt
		    do (format data-port "~s ~s ~%" x y)))
	  (format data-port "~% ~%"))))
      (ccl::run-program "/sw/bin/gnuplot" '("/tmp/tmp.dem") :wait nil)))
