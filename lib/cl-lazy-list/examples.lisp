(defun integers (start &optional end)
  (if (or (null end) (<= start end))
	  (ll:cons start (integers (1+ start) end))
	  nil))

(defun fibgen (a b)
  (ll:cons a (fibgen b (+ a b))))

(defun fibs ()
  (fibgen 0 1))

(defun divisible? (x y) 
  (zerop (rem x y)))

(defun sieve (stream)
  (ll:cons (ll:car stream)
		   (sieve (ll:filter
				   #'(lambda (x) (not (divisible? x (ll:car stream))))
				   (ll:cdr stream)))))

(defun primes ()
  (sieve (integers 2)))

(defun merge2 (list1 list2)
  (cond ((null list1) list2)
		((null list2) list1)
		(t
		 (let ((x (ll:car list1))
			   (y (ll:car list2)))
		   (cond ((< x y)
				  (ll:cons x (merge2 (ll:cdr list1) list2)))
				 ((= x y)
				  (ll:cons x (merge2 (ll:cdr list1) (ll:cdr list2))))
				 (t
				  (ll:cons y (merge2 list1 (ll:cdr list2)))))))))

(defun hamming ()
  (let ((x '(1)))
	(setf (cdr x)
		  (merge2 (ll:mapcar (lambda (e) (* e 2)) x)
				  (merge2 (ll:mapcar (lambda (e) (* e 3)) x)
						  (ll:mapcar (lambda (e) (* e 5)) x))))
	x))

(defun take (n list)
  (ll:subseq list 0 n))

(defun take-when (pred list)
  (let ((head (ll:first list)))
	(if (funcall pred head)
		(ll:cons head (take-when pred (ll:cdr list)))
		nil)))

(defun sum (list)
  (apply #'+ (ll:enumerate-all list)))
