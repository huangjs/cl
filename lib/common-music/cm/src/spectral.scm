;;; **********************************************************************
;;; Copyright (C) 2003 Heinrich Taube (taube@uiuc.edu) 
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.24 $
;;; $Date: 2005/12/03 17:49:58 $

;;;
;;; harmonics
;;;

; (harmonics 1 8)
; (harmonics 8 16 :hertz 100)

(define (harmonics h1 h2 . args)
  (with-args (args &key ((:hertz hz) 1) invert 
                   undertones ((:keynum knum) #f)
                   ((:rescale scaler) #f)
                   scale-order harmonic)
    ;; calculate overtones from h1 to h2. if fundamental is 1 (the
    ;; default) then freq ratios are returned.
    (unless (< h1 h2)
      (err "harmonics: ~s not less than ~s."
           h1 h2))
    (when harmonic
      (unless (<= h1 harmonic h2)
        (err "harmonics: :harmonic ~s not between ~s and ~s."
             harmonic h1 h2)))
    (let* ((freq (if knum (hertz knum) hz))
           (spec
            (if invert
                (if undertones
                    (if harmonic
                        (/ freq (/ h2 harmonic))
                        (loop for h from h2 downto h1
                           collect (/ freq (/ h2 h ))))
                    (if harmonic
                        (* freq (/ h2 harmonic))
                        (loop for h from h2 downto h1
                           collect (* freq (/ h2 h)))))
                (if undertones
                    (if harmonic
                        (/ freq (/ harmonic h1))
                        (loop for h from h1 to h2
                           collect (/ freq (/ h h1))))
                    (if harmonic
                        (* freq (/ harmonic h1))
                        (loop for h from h1 to h2
                           collect (* freq (/ h h1))))))))
      (when scaler
        (if knum (set! scaler (hertz scaler :hz #t)))
        (do ((root (car spec))
             (term (list-ref spec (- h2 h1)))
             (tail (cdr spec) (cdr tail)))
            ((null? tail) #f)
          (set-car! tail (rescale (car tail) root term root scaler))))
      (when scale-order
        (when harmonic
          (error "harmonics: :scale-order and :harmonic are exclusive."))
        (case scale-order
          ((:reverse reverse) (set! spec (reverse! spec)))
          ((:up up)
           (if undertones (set! spec (reverse! spec))))
          ((:down down)
           (if (not undertones) (set! spec (reverse! spec))))
          ((:random random) (set! spec (shuffle spec)))
          (else
           (err "harmonics: not a :scale-order value: ~S"
                scale-order))))
      (if knum
          (keynum spec :hz #t)
          spec))))

;; rm-spectrum performs ring modulation on two input specta f1 and f2
;; to return a spectrum (list of frequencies) consisting of the
;; difference and sum tones between pairwise combinations of the
;; paritals in each input specta. The partials in the input spects can
;; be hertz values, key numbers or note names but lists must be the
;; same type.

(define (rm-spectrum set1 set2 . args)
  (with-args (args &key (spectrum #f) (minimum #f) (maximum #f)
                   (hz #f) (scale-order :up) (remove-duplicates #f))
    (let* ((sums-and-diffs 
            (lambda (f1 f2)
              (if (= f1 f2) (list)
                  (list (abs (- f1 f2)) (+ f1 f2)))))
           (type (cond ((not spectrum)
                        (if hz ':hertz
                            (if (pair? set1)
                                (if (symbol? (car set1)) ':note ':keynum)
                                (if (symbol? set1) ':note ':keynum))))
                       ((member spectrum '(:note :keynum :hertz))
                        spectrum)
                       (else
                        (err "Spectrum ~S not one of: :note :keynum :hertz."
                             spectrum))))
           (spec
            (if (pair? set1)
              (if (pair? set2)
                (loop with l2 = (hertz set2 :hz hz)
                   for f1 in (hertz set1 :hz hz)
                   append (loop for f2 in l2
                             append (sums-and-diffs f1 f2)))
                (loop with f2 = (hertz set2 :hz hz)
                   for f1 in (hertz set1 :hz hz)
                   append (sums-and-diffs f1 f2)))
              (if (pair? set2)
                (loop with f1 = (hertz set1 :hz hz)
                   for f2 in (hertz set2 :hz hz)
                   append (sums-and-diffs f1 f2))
                (sums-and-diffs (hertz set1 :hz hz)
                                (hertz set2 :hz hz))))))
      (when minimum
        (do ((freq (hertz minimum :hz (eq? type ':hertz)))
             (tail spec (cdr tail)))
            ((null? tail) #f)
          (do ()
              ((not (< (car tail) freq)))
            (set-car! tail (* (car tail) 2)))))
      (when maximum
        (do ((freq (hertz maximum :hz (eq? type ':hertz)))
             (tail spec (cdr tail)))
            ((null? tail) #f)
          (do ()
              ((not (> (car tail) freq)))
            (set-car! tail (/ (car tail) 2)))))
      (cond ((eq? scale-order ':up)
             (set! spec (sort! spec (function <))))
            ((eq? scale-order ':down)
             (set! spec (sort! spec (function >))))
            ((eq? scale-order ':random)
             (set! spec (shuffle spec))))
      (cond ((eq? type ':note)
             (set! spec (note spec :hz #t)))
            ((eq? type ':keynum)
             (set! spec (keynum spec :hz #t)))
            ((eq? type ':hertz)
             #f))
      (if (and remove-duplicates
               (do ((tail spec (cdr tail))
                    (flag #f))
                   ((or (null? tail) flag) flag)
                 (set! flag (member (car tail) (cdr tail)))))
        (do ((tail spec (cdr tail))
             (resl (list #f)))
            ((null? tail) (cdr resl))
          (if (member (car tail) resl)
            #f
            (set! resl (append! resl (list (car tail))))))
        spec))))

; (rm-spectrum 'd5 'e5)
; (rm-spectrum 'a4 'ds5 :spectrum ':keynum)
; (rm-spectrum '(300 400) 550 :hz t)
; (rm-spectrum '(300 400) 550 :hz t :spectrum ':note)
; (rm-spectrum '(300 400) 550 :hz t :spectrum ':note :minimum 'c4)
; (rm-spectrum '(300 400) 550 :hz t :spectrum ':note :maximum 'c5)
; (rm-spectrum '(300 400) 550 :hz t :spectrum ':note :minimum 'c4 :maximum 'c5 :scale-order ':random)
; (rm-spectrum '(300 400) 550 :hz t :spectrum ':note :scale-order ':down)
; (rm-spectrum '(300 400) 550 :hz t :spectrum ':keynum)
; (rm-spectrum '(c4 e4) '(c4 e4 g4))
; (rm-spectrum '(c4 e4) '(c4 e4 g4) :remove-duplicates t)

;;; fm-spectrum returns a spectrum (frequencies and optional amplitudes)
;;; of sidebands calculated from a specified carrier, mratio and
;;; index. the type of values returned in the spectrum are determined
;;; by the value of the :spectrum arg. if :spectrum is nil, then the
;;; exact sidebands are returned. if :spectrum is :hertz (the default)
;;; then negative sidebands are relected into the positive frequency
;;; domain with their phases inverted. if :spectrum is :keynum then
;;; sidebands are returned as floating point keynums kkk.nn where nn
;;; is interpreted as that number of cents above the integer keynum,
;;; (ie 60.99 is 99 cents above middle c). If :spectrum is :notes then
;;; the sidebands are rounded to the closest note name. :spectrum can 
;;; also be a list like (:notes c4 c6) in which case the second two 
;;; values are min and max boundaries for the spectral components to be
;;; fit between regardless of where the sidebands were actually located.
;;;
;;; If :sideband-scaler is nil (the default) then the number of sidebands
;;; generated is 1+round(index). If :sideband-scaler is a number then the
;;; number of sidebands generated is round(index*scaler). According to
;;; Dick Moore (pg 325, Elements of Computer Music) a :sideband-scaler 
;;; of 1.7 will give all sidebands down to 60dB, use 1.5 for 40dB down.
;;;
;;; the :amps keyword says whether or not side band amplitudes are to
;;; returned as well as the frequencies. the default value is nil.
;;; if :amps is T then each sideband is represented in the spectrum as
;;; a list (frq amp). if the value of :amps is ':normalized then all
;;; amps are normalized to lie between 0 and 1. if :amps is :weight the
;;; spectrum returned is suitable to pass directly to a RANDOM pattern,
;;; ie the spectum becomes a list elements where amplitude is used as
;;; probability weight. 
;;;
;;; if the :invert keyword is T the spectrum is inverted before it is
;;; returned. if :ignore-zero is T; then side bands with zero amp are
;;; ignored, othwise their frequency is 0.0 or R (rest) if for notes. 
;;;

; (fm-spectrum 100 1.4 3 )
; (fm-spectrum 100 1.4 3 :amplitudes #f)
; (fm-spectrum 100 1.4 3 :spectrum :hertz)
; (fm-spectrum 100 1.4 3 :spectrum :hertz :amplitudes #t)
; (fm-spectrum 100 1.4 3 :spectrum :note)
; (fm-spectrum 100 1.4 3 :spectrum :keynum)
; (fm-spectrum 100 1.4 3 :spectrum :note :minimum 'c4 :maximum 'b4)
; (fm-spectrum 100 1.4 3 :spectrum :note :minimum 'c4 :maximum 'b4 :amplitudes :weight )

(define (fm-spectrum carrier mratio index . args)
  (with-args (args &key (spectrum #f) (minimum #f) (maximum #f)
                   invert (ignore-zero #f izp)
                   (amplitudes (eq? spectrum ':raw))
                   all-sidebands scale-order
                   (remove-duplicates #t))
    (let ((data #f)
          (type #f)
          (bot #f)
          (top #f))
      ;; :ignore-zero defaults to #t if generating
      ;; notes, hertz or keynums.
      (case spectrum
        ((#f )
         (set! type spectrum))
        ((:hertz )
         (set! type spectrum)
         (unless izp (set! ignore-zero #t)))
        ((:keynum )
         (set! type spectrum)
         (unless izp (set! ignore-zero #t)))
        ((:note)
         (set! type spectrum)
         (unless izp (set! ignore-zero #t)))
        (else
         (err "Spectrum ~s not one of: false :note :keynum :hertz."
              spectrum)))
      (when spectrum
        (if minimum (set! bot (hertz minimum :hz (eq? type ':hertz))))
        (if maximum (set! top (hertz maximum :hz (eq? type ':hertz)))))
      ;(car mrat ind negfreq? posamp? ss)
      (set! data (fm-spectrum1 carrier mratio index 
                               (eq? type #f)
                               #f
                               all-sidebands))
      ;; sidebands at zero Hz can be included or removed
      (when ignore-zero
        (set! data (remove! (lambda (x) (zero? (car x))) data)))
      
      ;; optionally flip spectrum around carrier for undertones
      (when invert
        (dolist (s data)
          (let ((f (first s)))
            (unless (= f 0.0)
              (set-car! s (abs (hz-invert (hz-ify carrier) f))))))
        (set! data (sort! data (lambda (x y) 
                                 (< (first x) (first y))))))

      (when (or (eq? type ':note)
                (eq? type ':keynum))
        (if (or top bot)
          ;; compress spectrum to lie between bot and top 
          ;; only valid for :keynum or :note
          (let ((f #f)
                (k #f)
                (e #f)
                (l (list)))
            (dolist (s data)
              ;; s is a list (freq amp)
              (set! f (abs (first s))) 
              (when (not (= f 0.0))
                (do () 
                    ((<= f top) #f)
                  (set! f (* f 0.5)))
                (do ()
                    ((>= f bot) #f)
                  (set! f (* f 2.0))))
              (set! k (if (= f 0.0)
                        'r
                        (if (eq? type ':keynum)
                          (keynum f :hz #t)
                          (note f :hz #t))))
              (set! e (find (lambda (x) (equal? k (car x))) l))
              ;(set! e (find k l :key #'first))
              (if (and e remove-duplicates)
                ;; just update amplitude
                (set-car! (cdr e)
                          (+ (cadr e)
                             (second s)))
                (begin (set-car! s k)
                       (push s l))))
            (set! data (reverse! l)))
          (do ((low (hertz 0))
               (tail data (cdr tail)))
              ((null? tail) #f)
            ;; ignore partials lower than lowest note
            ;; in standard scale
            (unless (< (caar tail) low) 
              (set! (caar tail)
                    (if (eq? type ':note)
                      (note (caar tail) :hz #t)
                      (keynum (caar tail) :hz #t)))))))
      (unless (eq? type #f)
        (set! data
              (cond 
                ((not scale-order)
                 ;; only have to reorder if we compressed
                 (if (or top bot)
                   (sort! data (lambda (a b)
                                 (scale< (first a) (first b))))
                   data))
                ((eq? scale-order :up )
                 (sort! data (lambda (a b)
                               (scale< (first a) (first b)))))
                ((eq? scale-order :down )
                 (sort! data (lambda (a b)
                               (scale> (first a) (first b)))))
                ((eq? scale-order :random )
                 (shuffle data))
                (else
                 (err "Not a valid :scale-order: ~s."
                      scale-order)))))

      (cond ((not amplitudes)
             (do ((tail data (cdr tail)))
                 ((null? tail) #f)
               (set-car! tail (car (car tail)))))
            ((eq? amplitudes :normalized)
             (dolist (s data) 
               (set-car! (cdr s) (abs (second s)))))
            ((eq? amplitudes :weights)
             (dolist (s data)
               (set-car! (cdr s) (abs (second s)))
               (set-cdr! s (cons ':weight (cdr s)))))
            ((eq? amplitudes #t)
             #f)
            (else
             (err "Not an amplitude value: ~s." 
                  amplitudes)))
      data)))

;;;
;;; returns C M and C/M for low and high freq and low and hig side band
;;; numbers.from james daschow.

(define (sidebands-to-cm low high lb ub)
  (let* ((m (/ (- high low) (- ub lb)))
         (c (- high (* ub m))))
    (values c m (/ m c))))

;; macro version has compile problem in sbcl 0.9.4 and up

(define (bes-jn unn ux)
  ;;return Jn(x) for any integer n, real x
  (let ((nn unn) ;macro version was `(let ((nn ,unn) (x ,ux)) ...
        (x ux)) 
    (let* ((n (inexact->exact (floor (abs nn))))
           (besn (if (= n 0) 
                   (bes-j0 x)
                   (if (= n 1) 
                     (bes-j1 x)
                     (if (zero? x) 
                       0.0
                       (let ((iacc 40)
                             (ans 0.0)
                             (bigno 1.0e10)
                             (bigni 1.0e-10))
                         (if (> (abs x) n)
                           (do ((tox (/ 2.0 (abs x)))
                                (bjm (bes-j0 (abs x)))
                                (bj (bes-j1 (abs x)))
                                (j 1 (+ j 1))
                                (bjp 0.0))
                               ((= j n) (set! ans bj))
                             (set! bjp (- (* j tox bj) bjm))
                             (set! bjm bj)
                             (set! bj bjp))
                           (let ((tox (/ 2.0 (abs x)))
                                 (m (* 2 
                                       (inexact->exact
                                        (floor
                                         (/ (+ n (sqrt (* iacc n)))
                                            2)))))
                                 (jsum 0.0)
                                 (bjm 0.0)
                                 (sum 0.0)
                                 (bjp 0.0)
                                 (bj 1.0))
                             (do ((j m (- j 1)))
                                 ((= j 0))
                               (set! bjm (- (* j tox bj) bjp))
                               (set! bjp bj)
                               (set! bj bjm)
                               (when (> (abs bj) bigno)
                                 (set! bj (* bj bigni))
                                 (set! bjp (* bjp bigni))
                                 (set! ans (* ans bigni))
                                 (set! sum (* sum bigni)))
                               (if (not (= 0 jsum)) (incf sum bj))
                               (set! jsum (- 1 jsum))
                               (if (= j n) (set! ans bjp)))
                             (set! sum (- (* 2.0 sum) bj))
                             (set! ans (/ ans sum))))
                         (if (and (negative? x) 
                                  (odd? n)) 
                           (- ans)
                           ans)))))))
      (if (and (negative? nn)
               (odd? nn))
        (- besn)
        besn))))

(define (fm-spectrum1 carrier mratio index neg-freq? pos-amp?
                      sideband-scaler)
  (set! carrier (hz-ify carrier))
  (let ((mfreq (* carrier mratio))
        (nsides (if sideband-scaler
                  (inexact->exact
                   (round (* sideband-scaler index))) ;; Moore p. 325
                  (+ (inexact->exact (round index)) 1)))
        (spectrum '()))
    
    ;; spectrum is a list of sidebands, each sideband is (freq amp)
    (set! spectrum
          (loop for k from (- nsides) to nsides
                for a = (bes-jn k index)
                unless (= a 0.0) ; ignore sidebands with no energy
                collect (list (+ carrier (* mfreq k)) a)))
    (unless neg-freq?
      ;; fold negative freqs into positive side, combine sideband amps
      (do ((neg #f)
           (pos #f))
          ((not (negative? (caar spectrum))) #f)
        (set! neg (pop spectrum))
        (set! pos (abs (first neg)))
        ;; wrap around to positive freq, invert phase
        (set-car! neg pos)
        (set-car! (cdr neg) (- (second neg)))
        ;; combine with existing sideband
        ;; or insert at appropriate position.
        (let ((side (assoc (first neg) spectrum)))
          (if side
            (set-car! (cdr side)
                      (+ (cadr side) 
                         (second neg)))
            ;;(position-if #'(lambda (x) (< x pos)) spectrum :key #'first :from-end #t)
            (let ((p (do ((i (- (length spectrum) 1) (- i 1))
                          (f #f)
                          (x #f))
                         ((or f (< i 0)) f)
                       (set! x (car (list-ref spectrum i)))
                       (if (< x pos) (set! f i)))))
              (if (not p)
                (push neg spectrum) 
                (let ((tail (list-tail spectrum p)))
                  (set! neg (list neg))
                  (set-cdr! neg (cdr tail))
                  (set-cdr! tail neg))))))))
    ;; normalize sideband amps.
    (when pos-amp?
      (dolist (s spectrum)
        (set-car! (cdr s) (abs (second s)))))
    spectrum))

;;;
;;; fm-plot displays a plot of the spectrum. In mcl it uses the plotter 
;;; window
;;;

; #+MCL
; (defun fm-plot (carrier mratio index &key (spectrum 'hertz)
;                         (amps t) invert ignore-zero)
;   (set! carrier (hz-ify carrier))
;   (plotter
;    (new plot 
;         :data (loop for d in (fm-spectrum carrier mratio index
;                                           :spectrum spectrum
;                                           :amps amps :invert invert
;                                           :ignore-zero ignore-zero)
;                     collect
;                     (new point :y (second d) 
;                          :x (if (eql spectrum 'note)
;                               (keynum (first d))
;                               (first d))))
;         :y-axis (new axis :minimum -1 :maximum 1 :increment .25 :ticks 1)
;         :x-axis (new axis :minimum 0)
;         :graph-style ':histogram)))


; #-MCL
; (defun fm-plot1 (carrier mratio index &key (spectrum 'hertz)
;                         (amps t) invert ignore-zero)
;   (set! carrier (hz-ify carrier))
;   (let* ((mfreq (* carrier mratio))
;          (cmrat (rationalize (/ carrier mfreq)))
;          (data (fm-spectrum carrier mratio index :spectrum spectrum
;                             :amps amps :invert invert
;                             :ignore-zero ignore-zero))
;          fmat)
;     (if (typep (first (first data)) 'float)
;       (set! fmat  "~10,3f")
;       (set! fmat "~4@A" carrier (note carrier) mfreq (note mfreq)))
;     (format t (format nil "~~%C: ~A, M: ~A, C/M: ~~S" fmat fmat)
;             carrier mfreq cmrat)
;     (dolist (s data)
;       (format t (format nil "~~%~A~~10,3f" fmat)
;               (first s) (second s)))))

;;;
;;; support routines.
;;;

(define (hz-ify x)
  (if (symbol? x)
    (hertz x)
    (* x 1.0)))

(define (hz-invert f1 f2)
  ;; invert f2 around f1.
  (let ((l1 (log f1))
        (l2 (log f2)))
    (exp (+ l1 (- l1 l2)))))

;;;
;;; Bill's bessel functions from snd/run-tests.scm and clm-3/bessel.lisp
;;;

(define (bes-j0 x)                      ;returns J0(x) for any real x
  (if (< (abs x) 8.0)                   ;direct rational function fit
    (let* ((y (* x x))
           (ans1 (+ 57568490574.0
                    (* y (+ -13362590354.0 
                            (* y (+ 651619640.7
                                    (* y (+ -11214424.18 
                                            (* y (+ 77392.33017
                                                    (* y -184.9052456)))))))))))
           (ans2 (+ 57568490411.0 
                    (* y (+ 1029532985.0 
                            (* y (+ 9494680.718
                                    (* y (+ 59272.64853
                                            (* y (+ 267.8532712 y)))))))))))
      (/ ans1 ans2))
    (let* ((ax (abs x))
	   (z (/ 8.0 ax))
	   (y (* z z))
	   (xx (- ax 0.785398164))
	   (ans1 (+ 1.0 
		    (* y (+ -0.1098628627e-2 
			    (* y (+ 0.2734510407e-4
				    (* y (+ -0.2073370639e-5
					    (* y 0.2093887211e-6)))))))))
	   (ans2 (+ -0.1562499995e-1
		    (* y (+ 0.1430488765e-3
			    (* y (+ -0.6911147651e-5
				    (* y (+ 0.7621095161e-6
					    (* y -0.934945152e-7))))))))))
      (* (sqrt (/ 0.636619772 ax))
	 (- (* (cos xx) ans1)
	    (* z (sin xx) ans2))))))

(define (signum x) (if (= x 0.0) 0 (if (< x 0.0) -1 1)))

(define (bes-j1 x)                      ;returns J1(x) for any real x
  (if (< (abs x) 8.0)
    (let* ((y (* x x))
           (ans1 (* x 
                    (+ 72362614232.0
                       (* y (+ -7895059235.0
                               (* y (+ 242396853.1
                                       (* y (+ -2972611.439
                                               (* y (+ 15704.48260
                                                       (* y -30.16036606))))))))))))
           (ans2 (+ 144725228442.0 
                    (* y (+ 2300535178.0 
                            (* y (+ 18583304.74
                                    (* y (+ 99447.43394
                                            (* y (+ 376.9991397 y)))))))))))
      (/ ans1 ans2))
    (let* ((ax (abs x))
	   (z (/ 8.0 ax))
	   (y (* z z))
	   (xx (- ax 2.356194491))
	   (ans1 (+ 1.0
		    (* y (+ 0.183105e-2
			    (* y (+ -0.3516396496e-4
				    (* y (+ 0.2457520174e-5
					    (* y -0.240337019e-6)))))))))
	   (ans2 (+ 0.04687499995
		    (* y (+ -0.2002690873e-3
			    (* y (+ 0.8449199096e-5
				    (* y (+ -0.88228987e-6
					    (* y 0.105787412e-6))))))))))
      (* (signum x)
	 (sqrt (/ 0.636619772 ax))
	 (- (* (cos xx) ans1)
	    (* z (sin xx) ans2))))))


; (fm-plot 400 1 3 :spectrum nil )
; (fm-spectrum 400 1 3 :spectrum :hertz )
; (fm-spectrum 400 1 3 :spectrum nil)
; (fm-plot 400 1 3 :spectrum :hertz )
; (fm-plot 400 1 3 :spectrum :hertz :amps :normalize)
; (fm-plot 400 1 3    :amps :normalize  :ignore-zero t)
; (fm-plot 400 1 3 :spectrum '(keynum g4 g5)
;          :amps :normalize :ignore-Zero t)
; (fm-plot 400 1 3 :spectrum '(g4 g5) :amps :normalize  :ignore-zero t)
; (fm-spectrum 400 1 3 :spectrum :note )
; (fm-spectrum 400 1 3 :spectrum :note :amps :normalize)
; (fm-spectrum 400 1 3 :spectrum :note :amps :weight)
; (fm-spectrum 400 1 3 :spectrum :note :amps :weight :ignore-zero t)
; (fm-spectrum 400 1 3 :spectrum :note :invert t )
; (fm-spectrum 400 1 3 :spectrum '(c4 b4) :invert t )


;;
;; spear data
;;
;; (import-spear-data file &key start end point-format freq-scaler
;;                              amp-scaler time-scaler)
;;   Imports spear frame or partial data from file
;;   :point-format   {:raw | :hertz | :keynum | :note
;;                   ({:hertz|:keynum|:note} :amplitude)
;;                   (:time {:hertz|:keynum|:note})
;;   :start          starting frame/partial to import, default 0
;;   :end            ending frame/partial to import, default all
;;   :freq-scaler    scaler for :hertz points, semitone shift for :keynum :note
;;   :amp-scaler     scaler on amp values
;;   :time-scaler    scaler on time values

;; (convert-spectrum spec fmat)
;;   Convert spectrum freqs to new fmat {:hertz|:keynum|:note}
;; (rescale-spectrum spec . minf maxf mina maxa)
;;   Rescale specturm to lie within optional new boundaries minf maxf mina maxa
;;   if a boundary is false that boundary remains unchanged. minf and maxf
;;   should agree with actual frequency format of spec
;; (invert-spectrum spec . amps?)
;;   Inverts spectral components and optionally amps
;; (plot-spectrum spec)
;;  Draws spec in plotter. spec can be list of spectra.
;; (spectrum-maxfreq spec . fmat)
;;   Return max freq of spec optionally converted to {:hertz|:keynum|:note}
;; (spectrum-minfreq spec . fmat)
;;   Return min freq of spec optionally converted to {:hertz|:keynum|:note}
;; (spectrum-maxamp spec)
;;   Return max amp of spec
;; (spectrum-minamp spec)
;;   Return min amp of spec
;; (spectrum-freqs spec . fmat)
;;   Return list of all freqs optionally converted to {:hertz|:keynum|:note}
;; (spectrum-amps spec)
;;   Return list of all amps
;; (spectrum->midi spec . slotinits)
;;   Return list of midis whose keynums & optionally amps are from spec.

(define (import-spear-data path . args)
  (with-args (args &key (start 0) end point-format
                   freq-scaler amp-scaler time-scaler)
    (let ((ftype #f)
          (count #f)
          (file (open-file path ':input))
          (data (list))
          (err? #t)
          (para #f)
          (parb #f))
      (dynamic-wind
       (lambda () #f)
       (lambda () 
         ;; parse "header" lines scheme string parsing is dreadful!
         (loop 
            for s = (file-line file)
            until (or (file-eof? s)
                      (equal? s "frame-data")
                      (equal? s "partials-data"))
            do
            (cond ((equal? s "par-text-frame-format")
                   (set! ftype ':frames))
                  ((equal? s "par-text-partials-format")
                   (set! ftype ':partials))
                  ((equal? s "point-type time frequency amplitude")
                   ;; fix when i see what other points are available
                   #f)
                  ((equal? s "point-type index frequency amplitude")
                   ;; fix when i see what other points are available
                   #f)
                  (else
                   (let ((x (string-substrings s)))
                     (cond ((null? x) #f)
                           ((or (equal? (car x) "partials-count")
                                (equal? (car x) "frame-count"))
                            (set! count (string->number (cadr x)))))))))
         ;; see if headers has stuff
         (cond ((eq? ftype ':frames)
                (unless point-format (set! point-format '(:hertz :amplitude)))
                (cond ((pair? point-format)
                       (if (member (car point-format) '(:hertz :keynum :note))
                           (set! para (car point-format))
                           (err "import-spear-data: illegal point-format: ~s."
                                point-format))
                       (if (and (pair? (cdr point-format))
                                (eq? (car (cdr point-format)) ':amplitude))
                           (set! parb #t)
                           (err "import-spear-data: illegal point-format: ~s."
                                point-format)))
                      ((member point-format '(:hertz :keynum :note :raw))
                       (set! para point-format))
                      (else
                       (err "import-spear-data: ~s is not a valid point-format."
                            point-format))))
               ((eq? ftype ':partials)
                (unless point-format (set! point-format '(:time :hertz)))
                (cond ((pair? point-format)
                       (if (eq? (car point-format) ':time)
                           (set! para ':time)
                           (err "import-spear-data: illegal point-format ~s."
                                point-format))
                       (if (and (pair? (cdr point-format))
                                (member (car (cdr point-format)) 
                                        '(:hertz :keynum :note)))
                           (set! parb (car (cdr point-format)))
                           (err "import-spear-data: illegal point-format ~s."
                                point-format)))
                      ((member point-format '(:hertz :keynum :note :raw))
                       (set! parb point-format))
                      (else
                       (err "import-spear-data: ~s is not a valid point-format."
                            point-format))))
               (else
                (err "import-spear-data: no frames or partials.")))
         (unless count
           (err "import-spear-data: file does not contain frame/partial count."))
         ;; collect of data
         (loop with d
            for s = (file-line file)
            for i from 0
            until (file-eof? s)
            do
            (when (or (and (>= i start) (or (not end) (<= i end))))
              (if (eq? ftype ':partials)
                  (let ((hdr s))
                    (set! s (file-line file))
                    (set! d (read-spear-partial hdr s para parb
                                                freq-scaler amp-scaler)))
                  (set! d (read-spear-frame s para parb
                                            time-scaler freq-scaler)))
              (if (pair? d) (push d data))))
         (set! err? #f))
       (lambda () (if file (close-file file ':input)))
       )
      (if (not err?) (set! data (reverse! data)))
      data)))
       
(define (read-spear-frame str ftyp amp? freq-scaler amp-scaler )
  (let ((data (string->expr str :multiok #t)))
    (if (eq? ftyp ':raw)
        data
        (do ((tail (cddr data) (cdddr tail))
             (head (list))
             (freq #f)
             (ampl #f))
            ((null? tail)
             (reverse! head))
          (set! freq (car (cdr tail)))
          (set! ampl (car (cddr tail)))
          (cond ((eq? ftyp ':hertz) 
                 (if freq-scaler
                     (set! freq (* freq freq-scaler))))
                (else
                 (if freq-scaler
                     (set! freq (+ (keynum freq :hz #t) freq-scaler))
                     (set! freq (keynum freq :hz #t)))
                 (if (eq? ftyp ':note)
                     (set! freq (note freq)))))
          (push freq head)
          (if amp?
              (push (if amp-scaler (* ampl amp-scaler) ampl)
                    head))))))

(define (read-spear-partial hdr str tim? ftyp time-scaler freq-scaler)
  hdr
  (let ((data (string->expr str :multiok #t)))
    (if (eq? ftyp ':raw)
        data
        (do ((tail data (cdddr tail))
             (head (list))
             (time #f)
             (freq #f))
            ((null? tail)
             (reverse! head))
          (set! time (car tail))
          (set! freq (car (cdr tail)))
          (if tim? (push (if time-scaler (* time time-scaler) time)
                         head))
          (cond ((eq? ftyp ':hertz) 
                 (if freq-scaler
                     (set! freq (* freq freq-scaler))))
                (else
                 (if freq-scaler
                     (set! freq (+ (keynum freq :hz #t) freq-scaler))
                     (set! freq (keynum freq :hz #t)))
                 (if (eq? ftyp ':note)
                     (set! freq (note freq)))))
          (push freq head)))))

(define (spectrum-maxfreq spec . args)
  (with-args (args &optional fmat)
    (let ((freq (list-ref spec (- (length spec) 2))))
      (if (not fmat) freq
          (let ((type (spectrum-guess-type spec)) )
            (case fmat
              ((:hertz :hz) 
               (if (eq? type ':hertz) freq
                   (hertz freq)))
              ((:keynum)
               (if (eq? fmat type) freq
                   (keynum freq :hz (eq? type ':hertz))))
              ((:note )
               (if (eq? fmat type) freq 
                   (note freq :hz (eq? type ':hertz))))
              (else (err "spectrum-maxfreq: ~s is not a frequency format."
                         fmat))))))))

(define (spectrum-minfreq spec . args)
  (with-args (args &optional fmat)
    (let ((freq (car spec)))
      (if (not fmat) freq
          (let ((type (spectrum-guess-type spec)))
            (case fmat
              ((:hertz :hz) 
               (if (eq? type ':hertz) freq
                   (hertz freq)))
              ((:keynum)
               (if (eq? fmat type) freq
                   (keynum freq :hz (eq? type ':hertz))))
              ((:note )
               (if (eq? fmat type) freq 
                   (note freq :hz (eq? type ':hertz))))
              (else (err "spectrum-minfreq: ~s is not a frequency format."
                         fmat))))))))

(define (spectrum-minamp spec)
  (do ((tail (cdr spec) (cddr tail))
       (mina most-positive-fixnum))
      ((null? tail)
       (if (eq? mina most-positive-fixnum) #f mina))
    (set! mina (min mina (car tail)))))

(define (spectrum-maxamp spec)
  (do ((tail (cdr spec) (cddr tail))
       (maxa most-negative-fixnum))
      ((null? tail)
       (if (eq? maxa most-negative-fixnum) #f maxa))
    (set! maxa (max maxa (car tail)))))

(define (spectrum-freqs spec . args)
  (with-args (args &optional fmat)
    (let* ((frqs (list #f))
           (type (spectrum-guess-type spec)))
      (if (not fmat)
          (set! fmat type)
          (case fmat
            ((:note :notes) (set! fmat ':note))
            ((:keynum :keynums) (set! fmat ':keynum))
            ((:hertz :hz) (set! fmat ':hertz))
            (else (err "spectrum-freqs: ~s is not a frequency format." fmat))))
      (do ((next spec (cddr next))
           (tail frqs))
          ((null? next)
           (cdr frqs))
        (set! (cdr tail)
              (list (cond ((eq? type fmat) (car next))
                          ((eq? fmat ':hertz) (hertz (car next)))
                          ((eq? fmat ':keynum)
                           (keynum (car next) :hz (eq? type ':hertz)))
                          (else (note (car next) :hz (eq? type ':hertz))))))
        (set! tail (cdr tail))))))

(define (spectrum-amps spec)
  (let ((amps (list #f)))
    (do ((next (cdr spec) (cddr next))
         (tail amps))
        ((null? next)
         (cdr amps))
      (set! (cdr tail) (list (car next)))
      (set! tail (cdr tail)))))

;;; convert spectrum from one point-format to another. want to keep
;;; specta just simple env lists so we have to distinuish between
;;; keynum and hertz values.

(define (spectrum-guess-type spec)
  ;; if maxfreq is less then 138 assume keynum
  (if (symbol? (car spec)) ':note
      (let ((max (spectrum-maxfreq spec)))
        (if (< max 138) ':keynum ':hertz)))) ; 138=22050

(define (convert-spectrum spec fmat)
  (let ((head (list #f))
        (mode 0)
        (type (spectrum-guess-type spec))
        )
    ;; 0 hk 1 hn 2 kh 3 kn 4 nh 5 nk
    ;; mode 0000=h->h 0100=h->k, 1000=h->n,
    ;;      0101=k->k 0001=k->h, 1001=k->n
    ;;      1010=n->n 0010=n->h, 0110=n->k
    (case type
      ((:keynum) (set! mode 1))
      ((:note) (set! mode 2)))
    (case fmat
      ((:keynum :keynums) (set! mode (logior mode #b0100)))
      ((:note :notes) (set! mode (logior mode #b1000)))
      ((:hertz :hz) #f)
      (else
       (err "convert-spectrum: ~s is not :note :keynum or :hertz" fmat)))
    (if (member mode '(#b0000 #b0101 #b1010))
        spec
    (do ((next spec (cdr next))
         (tail head (cdr tail))
         (amp? #f (not amp?)))
        ((null? next)
         (cdr head))
      (set! (cdr tail)
            (list (if amp? (car next)
                      (case mode
                        ((#b0100) (keynum (car next) :hz #t))
                        ((#b1000) (note (car next) :hz #t))
                        ((#b0001 #b0010) (hertz (car next)))
                        ((#b1001) (note (car next)))
                        ((#b0110) (keynum (car next)))
                        (else
                         (err "convert-spectrum: Shouldn't: mode is ~s"
                              mode))))))))))

(define (rescale-spectrum  spec . args)
  (with-args (args &optional f1 f2 a1 a2)
    (let* ((fmat (spectrum-guess-type spec))
           (amp? (or a1 a2))
           (next (if (eq? fmat ':hertz)
                     (append spec (list)) ; new copy
                     (convert-spectrum spec ':hertz))))
      ;; next is now copy of spec in hertz
      (when amp?
        (let ((mina (spectrum-minamp next))
              (maxa (spectrum-maxamp next)))
          (if (not a1) (set! a1 mina))
          (if (not a2) (set! a2 maxa))
          (do ((tail (cdr next) (cddr tail)))
              ((null? tail) #f)
            (set! (car tail) (rescale (car tail) mina maxa a1 a2)))))
      (unless (and (eq? fmat ':hertz) (not f1) (not f2))
        (let ((oldmin (spectrum-minfreq next))
              (oldmax (spectrum-maxfreq next))
              (newmin #f)
              (newmax #f))
          (cond ((eq? fmat ':hertz)
                 (set! newmin (or f1 oldmin))
                 (set! newmax (or f2 oldmax)))
                (else
                 (set! newmin (if f1 (hertz f1) oldmin))
                 (set! newmax (if f2 (hertz f2) oldmax))))
          (do ((tail next (cddr tail)))
              ((null? tail) #f)
            (set! (car tail)
                  (rescale (car tail) oldmin oldmax newmin newmax))
            (if (eq? fmat ':keynum)
                (set! (car tail) (keynum (car tail) :hz #t))
                (if (eq? fmat ':note)
                    (set! (car tail) (note (car tail) :hz #t)))))))
      next)))

(define (invert-spectrum spec . args)
  (with-args (args &optional amps?)
    (let* ((isa (spectrum-guess-type spec))
           (new (if (eq? isa ':note)
                    (convert-spectrum spec ':keynum)
                    (append spec (list))))
           (bot (spectrum-minfreq new))
           (top (spectrum-maxfreq new))
           (len (length new))
           )
      (do ((tail new (cddr tail))
           (soft (if amps? (spectrum-minamp new) #f))
           (loud (if amps? (spectrum-maxamp new) #f)))
          ((null? tail) #f)
        (set-car! tail (rescale (car tail) bot top top bot))
        (if soft
            (set-car! (cdr tail) (rescale (cadr tail) soft loud loud soft)))
        (if (eq? isa ':note) (set-car! tail (note (car tail)))))
      ;; freqs are now high to low
      (do ((head 0 (+ head 2))
           (tail (- len 2) (- tail 2))
           (temp #f))
          ((not (< head tail)) #f)
        (set! temp (list-ref new head))
        (list-set! new head (list-ref new tail))
        (list-set! new tail temp))
      new)))

(define (spectrum->midi spec . args)
  (let* ((type (spectrum-guess-type spec))
         (amps (if (eq? (list-prop args ':amplitude) #t)
                   (spectrum-amps spec)
                   #f))
         (keys (spectrum-freqs spec (if (eq? type ':hertz) ':keynum #f)))
         (head (list #f)))
    (if (not amps)
        (do ((klis keys (cdr klis))
             (tail head))
            ((null? klis)
             (cdr head))
          (set! (cdr tail) (list (apply (function make)
                                        (find-class* 'midi)
                                        :keynum (car klis)
                                        args)))
          (set! tail (cdr tail)))
        (do ((ampl (member ':amplitude args) ) ; get pos in args to set
             (klis keys (cdr klis))
             (alis amps (cdr alis))
             (tail head))
            ((null? klis) (cdr head))
          (set! (car (cdr ampl)) (car alis)) ; set amp in args
          (set! (cdr tail)
                (list (apply (function make) 
                             (find-class* 'midi)
                             :keynum (car klis) 
                             args)))
          (set! tail (cdr tail))))))
               


; (setq h (loop for h from 1 to 10 collect (* h 100) collect (* h .1)))
; (setq n (spectrum-hertz->note h))
; (setq k (spectrum-hertz->keynum h))
; (setq h2 (rescale-spectrum h :h1 50))
; (setq n2 (rescale-spectrum n :n1 'c2))
; (setq foo (import-spear-data "/Users/hkt/spear/log-drum-1+9db.txt"))
; (plot-spectrum (car foo) :keynum)
; (plot-spectrum foo)





; (cd "/Users/hkt/spear")
; (import-spear-data "test-frames.txt")
; (pprint (import-spear-data "test-frames.txt" :point-format ':keynum))
; (pprint (import-spear-data "test-frames.txt" :point-format ':note))
; (import-spear-data "test-frames.txt" :point-format ':keynum :freq-scaler -12)
; (import-spear-data "test-frames.txt" :point-format ':note)
; (import-spear-data "va4-partials.txt")
; (import-spear-data "va4-partials.txt" :point-format ':keynum)
; (import-spear-data "va4-partials.txt" :point-format ':note)
; (import-spear-data "va4-partials.txt" :point-format '(:time :note))


