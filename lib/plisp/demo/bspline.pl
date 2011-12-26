
;;  bspline.pl        Author: John Peterson (peterson-john@cs.yale.edu)

;;  The following routine constructs a BSpline from a set of control points.
;;  The input is an array of any length containing points.  Each point is
;;  an array of length 2.

;;  Source: foggy memories of Bob Barnhill's CAGD class.

;;  Make a triple knot at each end by repeating the first and last point
;;  two extra times.  A closed figure would be obtained by instead 
;;  repeating the last points before the first and first after the last.

(defun bspline (points) 0
   (let ((a (array (+ (length points) 4)))
	 (n (length points)))
     (setf (elt a 0) (elt points 0))
     (setf (elt a 1) (elt points 0))
     (dotimes (i n)
	 (setf (elt a (+ i 2)) (elt points i)))
     (setf (elt a (+ n 2)) (elt points (1- n)))
     (setf (elt a (+ n 3)) (elt points (1- n)))
     (moveto (elt (elt points 0) 0) (elt (elt points 0) 1))
     (internal-bspline a)))

;; Draw the bspline by picking up 6 points at a time from the control

(defun internal-bspline (pts) 0
   (let ((n (length pts)))
     (dotimes (i (- n 5))
	  (bspline-curve
		(elt pts i)
		(elt pts (+ i 1))
		(elt pts (+ i 2))
		(elt pts (+ i 3))
		(elt pts (+ i 4))
		(elt pts (+ i 5))))))

;; Generate a single bezier segment.  Lots of interpolation goes on here.

(defun bspline-curve (p1 p2 p3 p4 p5 p6) 0
  (let ((l1 (seg-length p1 p2))
	(l2 (seg-length p2 p3))
	(l3 (seg-length p3 p4))
	(l4 (seg-length p4 p5))
	(l5 (seg-length p5 p6)))
    (let* (
	   (tp1 (interp p3 p4 (/ l2 (+ l2 l3 l4))))
	   (tp2 (interp p3 p4 (/ (+ l2 l3) (+ l2 l3 l4))))
	   (tp3 (interp p4 p5 (/ l3 (+ l3 l4 l5))))
	   (tp4 (interp tp2 tp3 (/ l3 (+ l3 l4)))))
      (curveto (elt tp1 0) (elt tp1 1) (elt tp2 0) (elt tp2 1)
	       (elt tp4 0) (elt tp4 1)))))

;; Compute the distance between two points

(defun seg-length (p1 p2) 1
  (sqrt (+ (* (- (elt p1 0) (elt p2 0)) (- (elt p1 0) (elt p2 0)))
	   (* (- (elt p1 1) (elt p2 1)) (- (elt p1 1) (elt p2 1))))))

;; Interpolate between 2 points.  The interval (0,1) in alpha is
;; mapped onto (p1,p2).

(defun interp (p1 p2 alpha) 1
  (vector (+ (elt p1 0) (* alpha (- (elt p2 0) (elt p1 0))))
          (+ (elt p1 1) (* alpha (- (elt p2 1) (elt p1 1))))))

