;;; The wedge program as in the PLisp document.

(defun inch (x) 1 (* x 72))

(defun wedge () 0
    (newpath)
    (moveto 0 0)
    (translate 1 0)
    (rotate 15)
    (translate 0 (sin 15))
    (arc 0 0 (sin 15) -90 90)
    (closepath))

(gsave)
(translate (inch 3.75) (inch 7.25))
(scale (inch 1) (inch 1))
(wedge)
(setlinewidth 0.02)
(stroke)
(grestore)

(gsave)
(translate (inch 4.25) (inch 4.25))
(scale (inch 1.74) (inch 1.75))
(setlinewidth 0.02)
(dotimes (i 12)
         (setgray (/ (1+ i)12))
	 (gsave)
	 (wedge)
	 (gsave) (fill) (grestore)
         (setgray 0)
	 (stroke)
	 (grestore)
	 (rotate 30))

(grestore)
(showpage)
