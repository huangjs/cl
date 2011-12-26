
;;; This is a sample Postscript library

(defconstant page-right (* 72 8.5))        ; Right margin of paper
(defconstant page-center (/ page-right 2)) ; Center of paper

(defun center (str &optional (x 0 x-supplied) (y 0 y-supplied)) 0
   (cond (y-supplied (moveto x y))        ; When both are supplied
         (x-supplied (moveto page-center x))); One extra supplied
   (let (((lx ly) (stringwidth str)))
         ;; Stringwidth returns 2 values to lx and ly 
      (rmoveto (- (/ lx 2)) (- (/ ly 2)))) ; relative move half way
   (show str))

;; gpreserve wraps a gsave and grestore around
;; a set of statements

(defmacro gpreserve (&rest statements)
   `(progn (gsave) ,@statements (grestore)))

(defun concat-str (s1 s2) 1
  (let* ((l1 (length s1))
         (l2 (length s2))
         (res (string (+ l1 l2))))
     (putinterval res 0 s1)
     (putinterval res l1 s2)
     res))

;;; To define a function in the compilation environment
;;; for use in macro expansion, the PLisp eval
;;; executes the defun at compile time.

(eval (defun get-font (style)
    (let ((family (cond
		        ((member 'times style)
			 '(|Times-Roman| |Times-Bold| |Times-Italic|
			   |Times-BoldItalic|))
			((or (member 'avant style) (member 'avantgarde style))
			 '(|AvantGarde-Book| |AvantGarde-Demi|
			   |AvantGarde-BookOblique| |AvantGarde-DemiOblique|))
			((or (member 'schoolbook style)
			    (member 'newcenturyschoolbook style))
			 '(|NewCenturySchbk-Roman| |NewCenturySchlbk-Bold|
		   |NewCenturySchbk-Italic| |NewCenturySchlbk-BoldItalic|))
			((member 'helvetica style)
			 '(|Helvetica| |Helvetica-Bold|
			   |Helvetica-Oblique| |Helvetica-BoldOblique|))
			((member 'helvetica-narrow style)
			 '(|Helvetica-Narrow| |Helvetica-Narrow-Bold|
		   |Helvetica-Narrow-Oblique| |Helvetica-Narrow-BoldOblique|))
			((member 'bookman style)
			 '(|Bookman-Light| |Bookman-Demi|
			   |Bookman-LightItalic| |Bookman-DemiItalic|))
			((member 'courier style)
			 '(|Courier| |Courier-Bold| 
			   |Courier-Oblique| |Courier-BoldOblique|))
			((member 'palatino style)
			 '(|Palatino-Roman| |Palatino-Bold|
			   |Palatino-Italic| |Palatino-BoldItalic|))
			((member 'chancery style)
			 '(|ZapfChancery-MediumItalic|
			   |ZapfChancery-MediumItalic|
			   |ZapfChancery-MediumItalic|
			   |ZapfChancery-MediumItalic|))
			((member 'symbol style)
			 '(|Symbol| |Symbol| |Symbol| |Symbol|))
			((member 'bats style)
			 '(|ZapfDingbats|
			   |ZapfDingbats|
			   |ZapfDingbats|
			   |ZapfDingbats|))
		        (T
			 '(|Times-Roman| |Times-Bold|
			   |Times-Italic| |Times-BoldItalic|)))))
      (if (member 'italic style)
	  (setf family (cddr family)))
      (if (member 'bold style)
	  (setf family (cdr family)))
      (car family))))

;;; This macro sets the current font.  italic and bold
;;; modifiers are allowed.

(defmacro font (size &rest style)
  `(setfont (scalefont (findfont ',(get-font style)) ,size)))
 
(defmacro fontname (&rest style)
  `',(get-font style))

(defmacro mkfont (size &rest style)
  `(scalefont (findfont ',(get-font style)) ,size))

