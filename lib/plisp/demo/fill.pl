
;;; A routine to allow arbitrary fill patterns.  The arg to
;;; setpattern is a square array of arrays, containing 1/0

(defvar resmatrix (defaultmatrix (matrix)))

(defun findresolution () 1
    (let (((dx dy) (dtransform-matrix 72 0 resmatrix)))
	(sqrt (+ (* dx dx) (* dy dy)))))

(defun setuserscreen () 0
    (let ((frequency (/ (findresolution) (* (length bstring) pixels-per-bit))))
	(setscreen frequency 0 (load 'bitpatternspotfunction))))

(defun bitison (x y) 1
    (= (get (get bstring y) x) 1))

(defvar totalbits 0)
(defvar onbits 0)

(defun bitpatternspotfunction (x y) 1
    (let ((xindex (cvi (* (/ (1+ x) 2) bpside)))
	  (yindex (cvi (* (/ (1+ y) 2) bpside))))
	(incf totalbits)
	(if (bitison xindex yindex)
	    1
	    (progn (incf offbits) 0))))

(defun setpattern (pixels-per-bit bstring) 0
    (setf totalbits 0)
    (setf offbits 0)
    (setf bpside (length bstring))
    (setuserscreen)
    (settransfer #'(lambda () (values)))
    (setgray (/ offbits totalbits)))

(defun fill-pattern (pixels-per-bit pattern) 0
    (gsave)
    (setpattern pixels-per-bit pattern)
    (fill)
    (grestore))

