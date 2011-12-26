;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; percussion symbols taken from "Music Notation" by Read
;;;
;;; included here are:
;;;
;;;    cymbal (a big circle)
;;;    gong (same circle with another inside it)
;;;    suspended-cymbal
;;;    triangle
;;;    cow-bells
;;;    hi-hat
;;;    maracas
;;;    tambourine
;;;
;;;    mallet symbols (bass-drum, hard-stick, soft-stick, metal-stick, rubber-stick, triangle-stick, wire-brush, wood-stick)
;;;
;;;    harp-setting (to display Salzedo's diagram of the harp pedal settings)
;;;    woodblock
;;;
;;; all these symbols here assume they're attached to some object, but beyond
;;; that it's up to the user to place them correctly using dx and dy or whatever

(in-package :cmn)

#|
 (cmn (size 60) staff (staff-lines 1) (start-line 2) percussion (meter 4 4) 
    (b4 w (cymbal (dy 1.1) (dx .2))) (b4 w (gong (dy 1.1) (dx .2))) 
    (b4 w (suspended-cymbal (dy 1.1) (dx .2))) (b4 w (hi-hat (dy 1.1) (dx .2))) 
    (b4 w (tambourine (dy 1.1) (dx .2))) (b4 w (maracas (dy 1.1) (dx .2))) 
    (b4 w (cow-bells (dy 1.1) (dx .2)))  (b4 w (triangle (dy 1.1) (dx .2))) )

 (cmn (size 60) staff (staff-lines 1) (start-line 2) percussion (meter 4 4) 
    (b4 w (cymbal (dy 1.1) (dx .2)) wood-stick) (b4 w (gong (dy 1.1) (dx .2)) bass-drum)
    (b4 w (suspended-cymbal (dy 1.1) (dx .2)) metal-stick) (b4 w (hi-hat (dy 1.1) (dx .2)) triangle-stick) 
    (b4 w (tambourine (dy 1.1) (dx .2)) soft-stick) (b4 w (maracas (dy 1.1) (dx .2)) hard-stick) 
    (b4 w (cow-bells (dy 1.1) (dx .2)) wire-brush)  (b4 w (triangle (dy 1.1) (dx .2)) rubber-stick))
|#



;;; cymbal

(defun display-cymbal (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "cymbal")
    (with-thickness score mark .025
      (circle score 0 0 .25))
    (matrix-back score)))

(define-accent cymbal #'display-cymbal nil '(-.15 -.15 .15 .15))


;;; ------------------------------------------------
;;; gong

(defun display-gong (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "gong")
    (with-thickness score mark .025
      (circle score 0 0 .25)
      (circle score 0 0 .075))
    (matrix-back score)))

(define-accent gong #'display-gong nil '(-.25 -.25 .25 .25))


;;; ------------------------------------------------
;;; maracas

(defun display-maracas (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "maracas")
    (with-thickness score mark .025
      (circle score 0 0 .25)
      (moveto score 0 -.05)
      (rlineto score 0 -.35)
      (draw score))
    (matrix-back score)))

(define-accent maracas #'display-maracas nil '(-.15 -.4 .15 .15))


;;; ------------------------------------------------
;;; tambourine

(defun display-tambourine (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (dis .1)
	 (len .15)
	 (x-off (+ (box-x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "tambourine")
    (with-thickness score mark .025
      (circle score 0 0 .25)
      (moveto score (- dis) dis)
      (rlineto score (- len) len)
      (moveto score (- dis) (- dis))
      (rlineto score (- len) (- len))
      (moveto score dis dis)
      (rlineto score len len)
      (moveto score dis (- dis))
      (rlineto score len (- len))
      (draw score))
    (matrix-back score)))

(define-accent tambourine #'display-tambourine nil '(-.25 -.25 .25 .25))


;;; ------------------------------------------------
;;; suspended-cymbal

(defun display-suspended-cymbal (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "suspended cymbal")
    (with-thickness score mark .05
      (moveto score -.2 0)
      (rlineto score .4 0)
      (draw score))
    (moveto score 0 .1)
    (rlineto score 0 -.1)
    (draw score)
    (matrix-back score)))

(define-accent suspended-cymbal #'display-suspended-cymbal nil '(-.2 -.1 .2 .1))


;;; ------------------------------------------------
;;; hi-hat

(defun display-hi-hat (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "hi hat")
    (with-thickness score mark .05
      (moveto score -.2 0)
      (rlineto score .4 0)
      (moveto score -.2 -.1)
      (rlineto score .4 0)
      (draw score))
    (moveto score 0 .1)
    (rlineto score 0 -.1)
    (moveto score 0 -.1)
    (rlineto score 0 -.1)
    (draw score)
    (matrix-back score)))

(define-accent hi-hat #'display-hi-hat nil '(-.2 -.1 .2 .1))


;;; ------------------------------------------------
;;; triangle

(defun display-triangle (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "triangle")
    (with-thickness score mark .01
      (moveto score -.2 -.2)
      (rlineto score .4 0)
      (rlineto score -.2 .25)
      (rlineto score -.2 -.25)
      (draw score))
    (matrix-back score)))

(define-accent triangle #'display-triangle nil '(-.2 -.2 .2 .2))


;;; ------------------------------------------------
;;; cow-bells

(defun display-cow-bells (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "cow bells")
    (with-thickness score mark .01
      (circle score 0 0 .25 0 180)
      (moveto score -.25 0)
      (rlineto score 0 -.15)
      (rlineto score .5 0)
      (rlineto score 0 .15)
      (draw score))
    (matrix-back score)))

(define-accent cow-bells #'display-cow-bells nil '(-.25 -.5 .25 .25))


;;; ------------------------------------------------
;;; mallets
;;;
;;; I am blindly copying "Music Notation" of Gardner Read here
;;;

;;; -------- bass-drum

(defun display-bass-drum (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark)))
	 (r .1)
	 (r2 .075)
	 (cr (coerce (cos (/ pi 4)) 'single-float))
	 (ri (* r cr))
	 (ri2 (* r2 cr)))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "bass drum")
    (with-thickness score mark .01
      (circle score 0 0 r -45 135)
      (circle score -.05 -.05 r 135 315)
      (moveto score (- ri) ri)
      (rlineto score -.05 -.05)
      (moveto score ri (- ri))
      (rlineto score -.05 -.05)
      (moveto score ri ri)
      (rlineto score (* r 1.5) (* r 1.5))
      (draw score)
      (circle score (+ (* r 1.5) ri ri2) (+ (* r 1.5) ri ri2) r2))
    (matrix-back score)))

(define-accent bass-drum #'display-bass-drum nil '(-.25 -.25 .25 .25))


;;; -------- hard-stick

(defun display-hard-stick (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark)))
	 (r .075))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "hard stick")
    (with-thickness score mark .02
      (moveto score 0 0)
      (rlineto score 0 .2)
      (draw score)
      (circle score 0 (+ r .2) r 0 360 t))
    (matrix-back score)))

(define-accent hard-stick #'display-hard-stick nil '(0 0 .2 .25))


;;; -------- soft-stick

(defun display-soft-stick (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark)))
	 (r .075))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "soft stick")
    (with-thickness score mark .02
      (moveto score 0 0)
      (rlineto score 0 .2)
      (draw score)
      (circle score 0 (+ r .2) r 0 360 nil))
    (matrix-back score)))

(define-accent soft-stick #'display-soft-stick nil '(0 0 .2 .25))


;;; -------- metal-stick

(defun display-metal-stick (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark)))
	 (r .075))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "metal stick")
    (with-thickness score mark .02
      (moveto score 0 0)
      (rlineto score 0 .2)
      (draw score))
    (moveto score (- r) .2)
    (lineto score (- .15 r) .2)
    (lineto score (- .15 r) .35)
    (lineto score (- r) .35)
    (lineto score (- r) .2)
    (fill-in score)
    (matrix-back score)))

(define-accent metal-stick #'display-metal-stick nil '(0 0 .15 .15))


;;; -------- rubber-stick

(defun display-rubber-stick (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark)))
	 (r .075))
    (matrix-front score (translate-matrix score mark x-off y-off)) 
    (comment score "rubber stick")
    (with-thickness score mark .02
      (moveto score 0 0)
      (rlineto score 0 .2)
      (draw score))
    (circle score 0 (+ r .2) r)
    (circle score -.025 (+ r .175) .01 0 360 t)
    (circle score 0 (+ r .225) .01 0 360 t)
    (circle score .025 (+ r .175) .01 0 360 t)
    (matrix-back score)))

(define-accent rubber-stick #'display-rubber-stick nil '(-.1 -.1 .2 .2))


;;; -------- wood-stick

(defun display-wood-stick (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark)))
	 (r .075))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "wood stick")
    (circle score 0 (+ r .2) r 0 360 t)
    (moveto score -.02 .2)
    (rlineto score .04 0)
    (rlineto score .04 -.2)
    (rlineto score -.12 0)
    (rlineto score .04 .2)
    (fill-in score)
    (matrix-back score)))

(define-accent wood-stick #'display-wood-stick nil '(-.1 -.1 .2 .2))


;;; -------- wire-brush

(defun display-wire-brush (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "wire brush")
    (with-thickness score mark .02
      (moveto score 0 0)
      (rlineto score 0 .3)
      (moveto score 0 .2)
      (rlineto score .15 .04)
      (moveto score 0 .2)
      (rlineto score .1 .0875)
      (moveto score 0 .2)
      (rlineto score -.1 .0875)
      (moveto score 0 .2)
      (rlineto score -.15 .04)
      (draw score))
    (matrix-back score)))

(define-accent wire-brush #'display-wire-brush nil '(0 0 .35 .3))


;;; -------- triangle-stick

(defun display-triangle-stick (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (x-off (+ (box-x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "triangle stick")
    (with-thickness score mark .01
      (moveto score -.2 -.2)
      (rlineto score .4 0)
      (rlineto score -.2 .25)
      (rlineto score -.2 -.25)
      (moveto score 0 -.2)
      (rlineto score 0 .1)
      (draw score))
    (circle score 0 -.1 .04 0 360 t)
    (matrix-back score)))

(define-accent triangle-stick #'display-triangle-stick nil '(-.25 -.25 .25 .25))


;;; ------------------------------------------------
;;; harp set-up diagram

(defun harp-setting (D-ped C-ped B-ped E-ped F-ped G-ped A-ped &rest args)
  ;; pedals can be :sharp :flat :natural 
  (apply #'mark #'(lambda (mark note score &optional justifying)
		    (declare (ignore justifying))
		    (let* ((ped-height .3)
			   (x0 (+ (x0 note) (vis-dx mark)))
			   (y0 (+ (staff-y0 note) (vis-dy mark))))
		      (flet ((pedup (n) (if (eq n :flat) (+ ped-height .05)
					  (if (eq n :natural) (* .5 ped-height) 
					    -.05))))
			(matrix-front score (translate-matrix score mark x0 y0))
			(comment score "harp diagram")
			(with-thickness score mark .025
			  (moveto score -.05 ped-height)
			  (rlineto score 2.3 0)
			  (rmoveto score -1.2 -.2)
			  (rlineto score 0 .4)
			  (draw score))
			(setf (line-width score) .1)
			(moveto score .05 (pedup D-ped))
			(rlineto score 0 ped-height)
			(moveto score (+ .05 .3) (pedup C-ped))
			(rlineto score 0 ped-height)
			(moveto score (+ .05 .6) (pedup B-ped))
			(rlineto score 0 ped-height)
			(moveto score (+ .05 1.2) (pedup E-ped))
			(rlineto score 0 ped-height)
			(moveto score (+ .05 1.5) (pedup F-ped))
			(rlineto score 0 ped-height)
			(moveto score (+ .05 1.8) (pedup G-ped))
			(rlineto score 0 ped-height)
			(moveto score (+ .05 2.1) (pedup A-ped))
			(rlineto score 0 ped-height)
			(draw score)
			(setf (line-width score) 0)
			(matrix-back score))))
	 :harp-setting
	 args))

;;; (cmn staff treble c4 w (harp-setting :sharp :flat :natural :flat :sharp :natural :flat (dy -1.5)))

;;; ------------------------------------------------
;;; woodblock(s) (Anders Vinjar)

(defun display-woodblock (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note)))
	 (width .5)
         (halfwidth (/ width 2))
	 (height halfwidth)
	 (x-off (+ (box-x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "woodblock")
    (with-thickness score mark .025
                    (moveto score halfwidth 0)
                    (rlineto score 0 height)
                    (rlineto score (- width) 0)
                    (rlineto score 0 (- height))
                    (rlineto score width 0)
                    (draw score))
    (with-thickness score mark .05
                    (moveto score  (- (* 0.5 halfwidth)) (* height 3/5))
                    (rlineto score halfwidth 0)
                    (draw score))
    (matrix-back score)))

(define-accent woodblock #'display-woodblock nil '(-.2 -.1 .2 .1))
