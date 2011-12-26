
(use srfi-1)
(use file.util)
(use gauche.process)

(define *gnuplot-output-directory* (temporary-directory))

(define *gnuplot-styles* '(:lines :points :linespoints :impulses :dots :steps 
					:fsteps :histeps :errorbars :xerrorbars :yerrorbars
					:xyerrorbars :errorlines :xerrorlines :yerrorlines 
					:boxes :filledboxes :filledcurves :boxederrorbars
					:boxxyerrorbars :financebars :candlesticks :vector))

(define (interleave list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (#t (cons (car list1)
                  (cons (car list2) (interleave (cdr list1) (cdr list2)))))))

(define (swap lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
    (cons (second lst) (cons (first lst) (swap (cddr lst))))))

(define (max-depth lst)
  (if (null? lst) 0
    (if (not (list? (car lst)))
	(max 1 (max-depth (cdr lst)))
      (max (+ 1 (max-depth (car lst)))
	   (max-depth (cdr lst))))))

(define-class* <plot> ()
  ((data :init-value #f :init-keyword :data)
   (title :init-value "untitled" :init-keyword :title)
   (style :init-value :linespoints :init-keyword :style)
   (autoscale :init-value #f :init-keyword :autoscale)
   (xrange :init-value #f :init-keyword :xrange)
   (yrange :init-value #f :init-keyword :yrange)
   (zrange :init-value #f :init-keyword :zrange)
   (grid :init-value #f :init-keyword :grid)
   (surface :init-value #f :init-keyword :surface)
   (parametric :init-value #f :init-keyword :parametric)
   (ticslevel :init-value #f :init-keyword :ticslevel)
   (margin :init-value #f :init-keyword :margin)
   (border :init-value #f :init-keyword :border)
   (origin :init-value #f :init-keyword :origin)
   (size :init-value #f :init-keyword :size))
  :name 'plot)

(sys-putenv "GNUTERMAPP" "/sw/Applications/AquaTerm.app")
(sys-putenv "GNUTERM" "aqua")


(define-method* (display-plot (obj <plot>) . args)
  args
  (let ((dt 0) (depth (max-depth (slot-ref obj 'data))) (data (slot-ref obj 'data))
        (title (slot-ref obj 'title)) (autoscale (slot-ref obj 'autoscale))
        (xrange (slot-ref obj 'xrange)) (yrange (slot-ref obj 'yrange))
        (zrange (slot-ref obj 'zrange)) (grid (slot-ref obj 'grid))
        (surface (slot-ref obj 'surface)) (parametric (slot-ref obj 'parametric))
        (ticslevel (slot-ref obj 'ticslevel)) (margin (slot-ref obj 'margin))
        (border (slot-ref obj 'border)) (origin (slot-ref obj 'origin))
        (size (slot-ref obj 'size)) (style (slot-ref obj 'style)))
    (call-with-output-file (string-append *gnuplot-output-directory* "/tmp.dem")
      (lambda (plot-port)
        (when title
          (format plot-port "set title ~S~%" title))
        (when autoscale
          (format plot-port "set autoscale~%"))
        (when xrange
          (format plot-port "set xrange [~f:~f]~%"
                  (first xrange) (second xrange)))
        (when yrange
          (format plot-port "set yrange [~f:~f]~%"
                  (first yrange) (second yrange)))
        (when zrange
          (format plot-port "set yrange [~f:~f]~%"
                  (first zrange) (second zrange)))
        (when grid (format plot-port "set grid xtics; set grid ytics; set grid ztics~%"))
	(when surface (format plot-port "set surface~%"))
	(when parametric (format plot-port "set parametric~%"))
	(when ticslevel (format plot-port "set ticslevel ~s~%" ticslevel))
	(when margin
	  (format plot-port "set tmargin ~s~%" margin)
	  (format plot-port "set lmargin ~s~%" margin)
	  (format plot-port "set rmargin ~s~%" margin)
	  (format plot-port "set bmargin ~s~%" margin))
	(when border (format plot-port "set border ~s~%" border))
	(when origin (format plot-port "set origin ~s,~s~%" 
			     (coerce (first origin) 'float)
			     (coerce (second origin) 'float)))
	(when size (format plot-port "set size ~s,~s~%" 
			   (coerce (first size) 'float)
			   (coerce (second size) 'float)))
        (call-with-output-file (string-append *gnuplot-output-directory* "/tmp.dat") 
          (lambda (data-port)
            (dotimes (k depth)
              (begin
                (if (= depth 1)
                    (set! dt data)
                  (set! dt (list-ref data k)))
                (when (and (> depth 1) (= k 0))
                  (format plot-port "set multiplot~%"))
                (format plot-port "plot '/tmp/tmp.dat' index ~A " k)
                (when style (format plot-port "with ~a ~%" (keyword->string style)))
                (loop 
                 for x from 0 by .238
                 for y in dt
                 do 
                 (format data-port "~s ~s ~%" x y)))
              (format data-port "~% ~%")))     :if-exists :supersede) ) :if-exists :supersede)
    (run-process "/sw/bin/gnuplot" "/tmp/tmp.dem" :wait #f)))
      
      








