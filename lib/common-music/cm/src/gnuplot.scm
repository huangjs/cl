;;; **********************************************************************
;;; Copyright (C) 2007 Heinrich Taube, <taube (at) uiuc (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.9 $
;;; $Date: 2007/01/26 12:55:45 $

;;; Lisp wrapper to Gnuplot library. Should work in any Scheme with keywords
;;; and the following definitions in effect:
;;; (format ...) with ~a and ~s directives
;;; (os-name ) returns symbol name of os
;;; (shell <string>) exec string as shell command
;;; (list-prop plist sym) look up sym's value in plist
;;; most-negative-fixnum

(define *gnuplot* "gnuplot -persist")

(define *gnuplot-default-settings*
  (list :view #t :style :linespoints :points #f
	:terminal (if (member (os-name) '(darwin osx macosx))
		      "aqua" #f)))
			     
(define gnuplot-data-styles
  '(:lines :points :linespoints :impulses :dots :steps :fsteps :histeps
	   :errorbars :xerrorbars :yerrorbars :xyerrorbars :errorlines
	   :xerrorlines :yerrorlines :boxes :filledboxes :filledcurves
	   :boxederrorbars :boxxyerrorbars :financebars :candlesticks
	   :vector))

(define gnuplot-non-gnu-settings 
  ;; settings that dont belong to gnuplot
  '(:view :points :comment))

(define (thing->string x)
  (cond ((string? x) x)
	((keyword? x) (string-downcase (keyword->string x)))
	((symbol? x) (string-downcase (symbol->string x)))
	(else
	 (string-downcase (format #f "~a" x)))))

(define gnuplot-special-settings
  (list 
   (list ':title 
	 (lambda (f v) (format f " ~s" (thing->string v))))
   (list '(:origin :size)
	 (lambda (f v)
	   (format f " ~a,~a"  
		   (thing->string (car v)) (thing->string (cadr v)))))
   (list '(:xrange :yrange :zrange)
	 (lambda (f v) 
	   (format f " [~a:~a]" (thing->string (car v))
		   (thing->string (cadr v)))))
   (list ':style
	 (lambda (f v)
	   (if (member v gnuplot-data-styles)
	       (format f " data ~a" (thing->string v))
	       (format f " ~a" (thing->string v)))))
   ))

(define (find-gnuplot-setting k )
  (do ((tail gnuplot-special-settings (cdr tail))
       (spec #f))
      ((or (null? tail) spec)
       spec)
    (if (or (and (pair? (caar tail))
		 (member k (caar tail)))
	    (eq? k (caar tail)))
	(set! spec (car tail)))))

(define (print-gnuplot-setting f k v . p)
  ;; print global file setting or individual plot setting depending on
  ;; p. f is file, k is (keyword) setting, v is its value.  skip null
  ;; or non-gnu settings.
  (if (or (not v) (null? v) (member k gnuplot-non-gnu-settings))
      #f
      (let ((s (find-gnuplot-setting k)))
	;; global set command
	(if (null? p) (format f "set"))
	;; print setting name
	(format f " ~a" (thing->string k))
	;; print setting value
	(cond (s
	       ;; call special formatter
	       ( (cadr s) f v))
	      ((eq? v #t)
	       ;; simply print the setting
	       #f)
	      ((string? v)
	       (format f " ~a" v))
	      ((pair? v)
	       (do ((tail v (cdr tail)))
		   ((null? tail) #f)
		 (format f " ~a" (thing->string (car tail)))))
	      (else
	       (format f " ~a" (thing->string v))))
	(if (null? p) (format f "~%"))
	#f)))

; (setq foo '(0 0 25 .9 75 .9 100 0) bar (loop repeat 6 collect (random 1.0)))
; (guess-data-format foo)
; (guess-data-format bar)
; (guess-data-format (list (new midi)))

(define (guess-data-format dat)
  (let ((a (car dat)))
    (cond ((number? a)
	   (do ((l dat (cdr l))
		(i 0 (+ i 1))
		(x most-negative-fixnum)
		(f #f))
	       ((or (null? l) f)
		(if (not f)
		    (if (even? i) ':xy)
		    f))
	     (if (even? i) 
		 (if (>= (car l) x)
		     (set! x (car l))
		     (set! f ':y)))))
	  ((is-a? a <object>)
	   (let ((xslot #f)
		 (yslot #f)
		 (xtest '(time beg start startime begin begin-time 
			       start-time onset off x))
		 (ytest '(keynum frequency freq pitch frq note y)))
	     (do ((l xtest (cdr l)))
		 ((or (null? l) xslot) #f)
	       (if (slot-exists? a (car l)) (set! xslot (car l))))
	     (do ((l ytest (cdr l)))
		 ((or (null? l) yslot) #f)
	       (if (slot-exists? a (car l)) (set! yslot (car l))))
	     (if xslot
		 (if yslot (list xslot yslot)
		     xslot)
		 yslot)))
	  (else #f))))

; (slot-data? (new midi) '(keynum amplitude))

(define (slot-data? x sl)
  ;; test object x for valid slots
  (cond ((null? sl) #t)
	((pair? sl) 
	 (and (slot-data? x (car sl))
	      (slot-data? x (cdr sl))))
	(else
	 (and (slot-exists? x sl)
	      (slot-bound? x sl)
	      (number? (slot-ref x sl))))))

(define (slot-data x sl)
  (cond ((null? sl) '())
	((pair? sl) 
	 (cons (slot-data x (car sl))
	       (slot-data x (cdr sl))))
	(else
	 (slot-ref x sl))))

(define (print-gnuplot-plot file data fmat)
  (cond ((not fmat)
	 (err "Missing :points format for ~s." data))
	((eq? fmat #t)
	 ;; each sublist is a point record (single line )
	 (do ((tail data (cdr tail)))
	     ((null? tail) #f)
	   (do ((e (car tail) (cdr e)))
	       ((null e) #f)
	     (format file " ~s" (car e)))
	   (format file "~%")))
	((number? fmat)
	 (do ()
	     ((null? data) #f)
	   (do ((i 0 (+ i 1)))
	       ((or (not (< i fmat))
		    (null? data)))
		(format file "~%"))
	     (format file " ~s" (car data))
	     (set! data (cdr data))))
	((eq? fmat ':y)
	 (do ((tail data (cdr tail)))
	     ((null? tail) #f)
	   (format file " ~s~%" (car tail))))
	((eq? fmat ':xy)
	 (do ((tail data (cddr tail)))
	     ((null? tail) #f)
	   (format file " ~s ~s~%" (car tail) (cadr tail))))
	((is-a? (car data) <object>)
	 (if (slot-data? (car data) fmat)
	     (letrec ((slotdata (lambda (f x sl)
				  (cond ((null? sl) #f)
					((pair? sl) 
					 (slotdata f x (car sl))
					 (slotdata f x (cdr sl)))
					(else
					 (format f " ~s" (slot-ref x sl)))))))
               (do ((tail data (cdr tail)))
                   ((null? tail) #f)
                 (slotdata file (car tail) fmat)
		 (format file "~%")))
	     (err "Missing slot data for ~s in  ~s." 
		  fmat (car data))))
	(else
	 (err "Unknown :points format ~s." fmat)))
  (format file "e~%"))

; (gnuplot t :title "asdf" :margin 1 :xrange '(-pi pi))
; (gnuplot t :title "asdf" :margin 1 )

(define (gnuplot path . args)
    ;; file is open file or stream
  (let ((plots (list))
        (infos (list-copy *gnuplot-default-settings*)))
    (call-with-output-file path
      (lambda (file)
	;; write optional comment line
	(let ((str (member ':comment args)))
	  (if str (format file "#~A~%" (caddr str))))
	;; handle global file settings until first plot
	(do ((tail args (cddr tail)))
	    ((or (null? tail)
		 (not (keyword? (car tail))))
	     (if (null? tail)
		 (err "Missing plot data in: ~s" args)
		 (set! args tail)))
	  (cond ((null? (cdr tail))
		 (err "Missing value for keyword ~s." (car tail)))
		((member (car tail) *gnuplot-default-settings*)
		 ;; update default infos with user's info
		 (set-car! (cdr (member (car tail) infos))
			   (cadr tail))
		 )
		(else
		 (print-gnuplot-setting file (car tail) (cadr tail)))))
	;; now iterate infos and output (possibly updated) values
	(do ((tail infos (cddr tail)))
	    ((null? tail) #f)
	  (if (not (member (car tail) gnuplot-non-gnu-settings))
	      (print-gnuplot-setting file (car tail) (cadr tail))))
	;; args now series of plot specs: ({<plotdata> {key val}*}+ )
	(do ((plotd #f))
	    ((null? args)
	     (set! plots (reverse! plots)))
	  ;; plot data can be list or seq
	  (set! plotd (if (is-a? (car args) <seq>)
			  (subobjects (car args))
			  (car args)))
	  (unless (and (pair? plotd)
		       (or (number? (car plotd)) ; env/data list
			   (and (pair? (car plotd)) ; point records
				(number? (caar plotd)))
			   (is-a? (car plotd) <object>))) ; objects
	    (err "Not a list or seq containing plot data: ~s"
		 (car args)))
	  ;; plotd is: (<plotdata> <format> (ranges..) settings...)
	  (set! plotd (list plotd
			    (list-prop infos ':points)
			    (list :trange #f :xrange #f :yrange #f)
			    ))
	  ;; gather keyargs for current plot until next plot. ranges and
	  ;; data args are handled specially
	  (do ((tail (cdr args) (cddr tail)))
	      ((or (null? tail)
		   (not (keyword? (car tail))))
	       (set! args tail))
	    (cond ((null? (cdr tail))
		   (err "Missing value for keyword ~s." (car tail)))
		  ((eq? (car tail) ':points)
		   ;; update plot-specific data format
		   (list-set! plotd 1 (cadr tail)))
		  (else
		   (let ((rng (member (car tail) (caddr plotd))))
		     (if (not rng)
			 (append! plotd (list (car tail) (cadr tail)))
			 (set-car! (cdr rng) (cadr tail))))))
	    )
	  ;; if data is point data (lists) ignore :points spec else try to
	  ;; guess data format if not explicitly provided.
	  (cond ((pair? (caar plotd))
		 (list-set! plotd 1 #t))
		((not (cadr plotd))	; user did not indicate format
		 (list-set! plotd 1 (guess-data-format (car plotd)))))
	  (push plotd plots)
	  )
	;;(print (list :plots-> plots))
	;; plots now list of (<plotdata> <format> (ranges..) settings...)
	;; print plot command and args for each plot. 
	(format file "plot")
	;; print settings for each plot...
	(do ((tail plots (cdr tail)))
	    ((null? tail) 
	     (format file "~%"))
	  (let* ((plotd (car tail))
		 (range (list-ref plotd 2)))
	    ;; , separates individual plot clauses
	    (unless (eq? tail plots)
	      (format file ","))
	    ;; print plot ranges...
	    (do ((tail range (cddr tail))
		 (func (find-gnuplot-setting ':xrange))) ; get printer
		((null? tail) #f)
	      (if (cadr tail)
		  ( func file (cadr tail))))
	    (format file " '-'")
	    ;; map over and print plot-specific settings
	    (do ((tail (list-tail plotd 3) (cddr tail)))
		((null? tail) #f)
	      (print-gnuplot-setting file (car tail) (cadr tail) #t))))
	;; print individual data blocks
	(do ((tail plots (cdr tail)))
	    ((null? tail) #f)
	  (let* ((plotd (car tail))
		 (pdata (car plotd))
		 (dataf (cadr plotd)))
	    (print-gnuplot-plot file pdata dataf)))

	#t))
      (if (list-prop infos ':view)
	  (shell (format #f "~a ~a" *gnuplot* path)))
      path))

; (gnuplot "test.plt" :title "hiho" :style ':linespoints (list 0 0 50 .9 100 0) (list 0 1 100 0))
; (gnuplot "test.plt" :title "hiho"  (list 0 0 50 .9 100 0) :with :linespoints (list 0 1 100 0))
; (gnuplot "test.plt" :points :xy (list 0 0 50 .9 100 0) )
; (gnuplot "test.plt" :title "hiho" :points :xy (list 0 0 50 .9 100 0) (list 0 1 100 0) )

;;;
;;; eof
;;;

