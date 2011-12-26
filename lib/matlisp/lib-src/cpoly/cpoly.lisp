(in-package "MATLISP")

(def-fortran-routine cpoly :void
  "CPOLY"
  (opr (* :double-float) :input)	; Coefficients of polynomial (realpart)
  (opi (* :double-float) :input)	; (imaginary part)
  (degree :integer :input)
  (zeror (* :double-float) :input)	; Roots are returned here (realpart)
  (zeroi (* :double-float) :input)	; (imaginary part)
  (info :integer :output))		; Number of roots found

(defgeneric polyroots (polynomial)
  (:documentation
   "
  Syntax
  ======
  (POLYROOTS x)
 
  Purpose
  =======
  Compute all roots of the polynomial X.

  The coefficients of the polynomial are stored in X in order of
  decreasing powers.  The leading coefficient should be non-zero.

  The result is an array of complex numbers containing the roots
  found, roughly in order of magnitude.

  Notes
  =====
  The number of roots found may not be the degree of the polynomial
  due to difficulties in computing the roots.  You may want to check
  for that.  If no roots can found, then NIL is returned.

"))
   
(defmethod polyroots ((coef array))
  (let* ((len (length coef))
	 (degree (1- len))
	 (opr (make-array len :element-type 'double-float))
	 (opi (make-array len :element-type 'double-float))
	 (zeror (make-array degree :element-type 'double-float))
	 (zeroi (make-array degree :element-type 'double-float)))
    ;; CPOLY wants the real and imaginary parts of the coefficients in
    ;; separate arrays
    (map-into opr #'(lambda (x)
			(coerce (realpart x) 'double-float))
	      coef)
    (map-into opi #'(lambda (x)
			(coerce (imagpart x) 'double-float))
	      coef)
    
    (multiple-value-bind (info)
	(cpoly opr opi degree zeror zeroi 0)
      (unless (minusp info)
	(let ((roots (make-array info :element-type '(complex double-float))))
	  (dotimes (k info)
	    (setf (aref roots k) (complex (aref zeror k)
					  (aref zeroi k))))
	  (make-complex-matrix roots))))))

(defmethod polyroots :before ((coef standard-matrix))
  (assert (row-or-col-vector-p coef)))

(defmethod polyroots ((coef real-matrix))
  (polyroots (store coef)))

(defmethod polyroots ((coef complex-matrix))
  ;; Need to convert the elements of the matrix into a complex-valued
  ;; array (assuming we want to use the polyroots method for arrays.
  (let* ((len (floor (length (store coef)) 2))
	 (p (make-array len :element-type '(complex double-float))))
    (dotimes (k len)
      (setf (aref p k) (matrix-ref coef k)))
    (polyroots p)))
