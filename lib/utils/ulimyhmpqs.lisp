;;;* ulimyhmpqs
;;;* common lisp implementation of the
;;;* hypercube multipolynomial quadratic sieve with one large prime
;;;* (hmpqs) factorization algorithm
;;;*
;;;* Copyright (c) 2007, Ulrich Meyer
;;;* All rights reserved.
;;;*
;;;* Redistribution and use in source and binary forms, with or without
;;;* modification, are permitted provided that the following conditions are met:
;;;*     * Redistributions of source code must retain the above copyright
;;;*       notice, this list of conditions and the following disclaimer.
;;;*     * Redistributions in binary form must reproduce the above copyright
;;;*       notice, this list of conditions and the following disclaimer in the
;;;*       documentation and/or other materials provided with the distribution.
;;;*     * Neither the name of Ulrich Meyer nor the
;;;*       names of its contributors may be used to endorse or promote products
;;;*       derived from this software without specific prior written permission.
;;;*
;;;* THIS SOFTWARE IS PROVIDED BY ULRICH MEYER ``AS IS'' AND ANY
;;;* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;* DISCLAIMED. IN NO EVENT SHALL ULRICH MEYER BE LIABLE FOR ANY
;;;* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;;* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;;* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; there are two safe and one direct access to the hmpqs:
;;; 
;;; (function) factors n &key (report nil)       returns a sorted list of the prime factors of n
;;;                                              small prime factors will be found by probe-division
;;;                                              if n is a power, the appropriate root will be taken
;;;                                              if necessary, thereafter the hmpqs is invoked
;;;                                              you might want to toggle the report on to see the progress
;;; 
;;; (function) efactors n &key (report nil)      like factors, but returns ((p1 e1) (p2 e2) ... (pn en))                           
;;;                                              factorization of n = p1^e1 * p2^e2 * ... * pn^en
;;; 
;;; (function) qs n                              direct access to hmpqs factorization of n
;;;               &key                           a variety of parameters may but don't need to be specified:
;;;               m                              half the sieve size (default increases with the size of n)
;;;               b                              factor base size (default increases with the size of n)
;;;               delta-t                        threshold adjustment (default 0, should be increased for large n)
;;;               omit-below                     primes below (default 25) will not be sieved in
;;;               slp-log2                       log2 of single-large-prime-table (default 16, table-size 65536)
;;;               alfa                           single-large-primes > alfa * pmax will be discarded
;;;               report                         report on calculation progress (default t)
;;;               return                         a variety of values may be returned by the qs function
;;;                                              default is 'factors, the list of all prime factors of n
;;;                                              '(factors time) would return a list of the factor-list and the
;;;                                              run-time in seconds as floating-point number
;;;                                              see got-them-all for further options
;;; 
;;; the following stuff is used by the hmpqs, but might also be interesting for other purpose:
;;; 
;;; (function) binoc n k                         binomial coefficient
;;; (function) square? x                         returns (isqrt x) if x is square, otherwise nil
;;; (function) exptmod a n p                     a^n mod p for big numbers
;;; (function) invmod x m                        multiplicative inverse of x modulo m, nil if not exists
;;; (function) invmod! x m                       multiplicative inverse of x modulo m, error if not exists
;;; (function) divmod a b m                      a times multiplicative inverse of b modulo m, nil if not exists
;;; (function) jacobisymbol p q                  the Jacobi-Symbol (p/q)
;;; (function) quadratic-residue? n p            is n quadratic residue modulo p?
;;; (function) sqrtmodprime n p                  modular square root x of n, x*x=n (mod p), p prime
;;; (function) pprime? p &optional(trials 30)    probabilistic test for primality of p (solovay-strassen)
;;; (function) nextpprime p                      next (probabilistic) prime > p
;;; (function) make-primes n                     generates an array of the first n primes
;;; (variable) *stored-primes*                   self-explanatory
;;; (variable) *nr-of-stored-primes*             a default of the first 100000 primes will be generated
;;; (variable) *max-stored-prime*                self-explanatory
;;; (macro) with-all-stored-primes p &rest body  with p bound to 2, 3, ... *max-stored-prime* body will be executed
;;; (macro) with-all-primes p &rest body         as above, but continues with probabilistic primes
;;; (function) prime i                           the i-th prime
;;; (function) prime? x                          is x prime? (probabilistic if x>*max-stored-prime*)
;;; (function) pi_ n                             approximation to pi(n), number of primes p, p <= n
;;; (function) hard d                            a hard-to-factor number of approximately d digits
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; miscellaneous number theory
;;; most of these functions are used by the hmpqs.


;;; some short cuts concerning effectivity/readability/debugging

(defmacro div2 (x) `(ash ,x -1))                ; this one's faster than `(truncate ,x 2)

(defmacro div (a b)                             ; in some places we want only one
  (let ((d (gensym)))                           ; value to be returned
    `(let ((,d (truncate ,a ,b))) ,d)))
(defmacro expt2 (i) `(ash 1 ,i))
(defmacro 2* (i) `(ash ,i 1))
(defmacro odd-up (i) `(logior ,i 1))            ; next odd number >= i etc.
(defmacro odd-down (i) `(logior (1- ,i) 1))
(defmacro even-up (i) `(logand (1+ ,i) -2))
(defmacro even-down (i) `(logand ,i -2))

(defmacro dfloat (x) `(coerce ,x 'double-float))
(defmacro ? (&rest xxx)
  `(progn
     ,@ (mapcar #'(lambda (x)
                    (let ((s (gensym)))
                      `(let ((,s ,x))
                         (format t "~s = ~s~%" ',x ,s) ,s)))
                xxx)))


;;; binomial coefficient

(defun binoc (n k)
  (labels
    ((binoc-aux (n k)
       (if (zerop k) 1 (/ (* n (binoc-aux (1- n) (1- k))) k))))
    (cond ((> k n) 0)
          ((= k n) 1)
          (t (binoc-aux n (min k (- n k)))))))



;;; is a number square?
;;; if a number x is square, then it is also square modulo a number m.
;;; to make this a cheap test, we take a power of 2 as m and use logand
;;; as a cheap division. we have precalculated all squares modulo m:
#|  (defun quadratic-residues-bit-vector (m)
      (let ((q (make-array m :element-type 'bit :initial-element 0)))
        (dotimes (i m) (setf (sbit q (mod (* i i) m)) 1))
        q))
    (defconstant *qr256* (quadratic-residues-bit-vector 256))   |#
;;; and now need to calculate the expensive isqrt in only one out of five cases

(defconstant *qr256* #*1100100001000000110000000100000001001000010000000100000001000000110010000100000001000000010000000100100001000000010000000100000001001000010000001100000001000000010010000100000001000000010000000100100001000000010000000100000001001000010000000100000001000000)

(defun square? (x)
  (and (= (sbit *qr256* (logand x 255)) 1) 
       (let ((r (isqrt x))) (and (= x (* r r)) r))))



;;; calculate a^n mod p for big numbers

(defun exptmod (base power modulo)
  (do ((ai base (mod (* ai ai) modulo))
       (an 1 (if (oddp ni) (mod (* an ai) modulo) an))
       (ni power (div2 ni)))
      ((= ni 0) an)))



;;; find multiplicative inverse of x modulo m
;;; invmod returns nil if it does not exist, invmod! errors
;;; set x to x mod m
;;; x=0:  no inverse exists
;;; x=1:  x'= 1
;;; else:                   let       x'= 1/x (mod m)
;;;         now find v, so that       x'= (v*m+1)/x (mod m)
;;; v*m+1 must be multiple of x   v*m+1 = 0 (mod x)
;;;                       hence       v = -1/m = -m' = x-m' (mod x)
;;; recursion ends because m mod x < x < m.

(defun invmod (x m)
  (labels ((invmod1 (x m)
             (setf x (mod x m))
             (cond ((= x 0) (throw 'noexist nil))
                   ((= x 1) 1)
                   (t (div (1+ (* m (- x (invmod1 m x)))) x)))))
    (catch 'noexist (invmod1 x m))))

(defun invmod! (x m)
  (setf x (mod x m))
  (cond ((= x 0) (error "There exists no multiplicative inverse"))
        ((= x 1) 1)
        (t (div (1+ (* m (- x (invmod! m x)))) x))))

(defun divmod (a b modulo)
  (let ((b1 (invmod b modulo)))
    (and b1 (mod (* a b1) modulo))))



;;;  Jacobi-Symbol
;;;  J(p/q), a generalization of the Legendre-Symbol L(p/q)
;;;  q must not be prime, but odd. L(p/q)=J(p/q) for odd primes q.
;;;  +1 if p is quadratic residue modulo q
;;;  -1 if p is quadratic non residue modulo q
;;;   0 if q divides p

(defun jacobisymbol (p q)
  (macrolet ((-1^ (n) `(if (oddp ,n) -1 1)))
    (let ((pmod16 (logand p 15)) (qmod16 (logand q 15)))
    (cond ((= p 0) 0)	
	  ((= p 1) 1)
	  ((oddp pmod16)
	   (* (jacobisymbol (mod q p) p)
	      (-1^ (truncate (* (- pmod16 1) (- qmod16 1)) 4))))
	  (t (* (jacobisymbol (floor p 2) q)
                (-1^ (truncate (- (* qmod16 qmod16) 1) 8))))))))

;;; ulimy: the above version does not work for q being square
;;; note that L(x/2) is always +1 because 0*0=0 and 1*1=1.
;;; for the setup of a QS factorbase we need to know if n
;;; is a quadratic residue mod p, p prime.

(defun quadratic-residue? (n p)
  (or (= p 2) (= (jacobisymbol n p) 1)))


;;; The Shanks-Tonelli Algorithm
;;; solves the congruence x^2 = n (mod p) where p is an odd prime
;;; note, that (p-x)^2 = n (mod p) is another solution!
;;; p must be prime and n must be quadratic residue modulo p,
;;; otherwise it hangs or gives wrong results!  
;;; ulimy 06/2001
;;; see the file sqrtmod for the general case (p not necessarily prime)



(defun sqrtmodprime (n p)
  (if (= p 2) (mod n 2)                 ; see note above
    (do ((s 0 (+ s 1))
         (q (- p 1) (truncate q 2)))
        ((oddp q)
         (let ((r (exptmod n (div2 (+ q 1)) p)))
           (if (= s 1) r
               (do ((w 2 (+ 1 w)))
                   ((= (JacobiSymbol w p) -1)
                    (let ((v (exptmod w q p))
                          (ninv (invmod n p))) ;(exptmod n (- p 2) p)
                      (loop
                        (let ((r2ninv (mod (* r r ninv) p)) (i 0))
                          (if (zerop
                               (do ((rn2i r2ninv (mod (* rn2i rn2i) p)))
                                   ((= rn2i 1) i)
                                 (incf i)))
                            (return r)
                            (setf r
                                  (mod (* r
                                          (let ((si1 (- s i 1)) (vv v))
                                            (loop (if (zerop si1) (return vv))
                                                  (setf vv (mod (* vv vv) p))
                                                  (decf si1))))
                                       p)) ))))))))))))



;;; probabilistic test for primality of p
;;; the solovay-strassen prime test: let a be a random integer 1<a<p
;;; if a^((p-1)/2) != (a/p) (mod p) then p is definitely not prime,
;;; otherwise the probability for p being prime is at least 0.5
;;; after t trials the chance for a mistake is less than 2^-t
;;; with 30 trials less than 1E-9, should be good enough....
;;; caution! (pprime? 2) => NIL

(defun pprime? (p &optional (trials 30))
  (and (oddp p) 
       (let* ((p-1 (1- p)) (p2 (div2 p-1)))
         (dotimes (i trials t)
           (let* ((a (1+ (random p-1)))
                  (a^p2 (exptmod a p2 p)))
             (unless (cond ((= a^p2 1) (= (jacobisymbol a p) 1))
                           ((= a^p2 p-1) (= (jacobisymbol a p) -1)))
               (return nil)))))))
                           

(defun nextpprime (p)
  (do ((x (+ 1 (even-up p)) (+ x 2))) ((pprime? x) (return x))))

;;; prime base
;;; we definitely need a base of the first let's say 100000 primes.
;;; calculating it takes a few seconds

(defun make-primes (n)
  (let ((p (make-array n)) (i 2) (pmax 3) s j)
    (setf (aref p 0) 2 (aref p 1) 3)
    (loop (setf s (isqrt (incf pmax 2)) j 1)
          (when (loop (let ((x (aref p j)))
                      (cond ((> x s) (return t))
                            ((= (mod pmax x) 0) (return nil))
                            (t (incf j)))))
            (setf (aref p i) pmax)
            (if (= (incf i) n) (return p))))))

(defvar *nr-of-stored-primes* 100000)
(defvar *stored-primes*)
(defvar *max-stored-prime*)
(defvar *max-divide-out*)   ; see primeq
(unless (boundp '*stored-primes*)
  (format t "generating ~d primes...~%" *nr-of-stored-primes*)
  (setf *stored-primes* (make-primes *nr-of-stored-primes*))
  (setf *max-stored-prime* (aref *stored-primes* (- *nr-of-stored-primes* 1)))
  (setf *max-divide-out* (* (min 5000 *max-stored-prime*) *max-stored-prime*)))

(defmacro with-all-stored-primes (p &rest body)
  (let ((i (gensym)))
    `(let ((,i 0))
       (loop (let ((,p (aref *stored-primes* ,i)))
               ,@body
               (if (= (incf ,i) *nr-of-stored-primes*) 
                   (return nil)))))))

;;; with-all-primes, prime and primeq
;;; enable the user to forget about whether a prime is stored.

(defmacro with-all-primes (p &rest body)
  (let ((i (gensym)))
    `(let ((,i 0))
       (loop
         (let ((,p (aref *stored-primes* ,i)))
           ,@body
           (if (= (incf ,i) *nr-of-stored-primes*)
             (return
              (let ((,p *max-stored-prime*))
                (loop
                  (if (pprime? (incf ,p 2)) (progn ,@body)))))))))))
           
(defun prime (i)
  (if (< (decf i) *nr-of-stored-primes*)
    (aref *stored-primes* i)
    (let ((x *max-stored-prime*)
          (j (- *nr-of-stored-primes* 1)))
      (loop (if (pprime? (incf x 2))                 ; this can take a while...
                (if (= (incf j) i) (return x)))))))


;;; prime? x uses the stored primes to check primality
;;; and if x > *max-divide-out* the Euler-test pprime?    
;;; *max-divide-out* is the estimated run time cross-over-point 
;;; for probe division vs probabilistic prime test, if x is prime

(defun prime? (x)
  (labels
    ((primeq-aux (x i1 i3)             ; binary search
       (if (>= i1 i3) (= x (aref *stored-primes* i1))
           (let* ((i2 (div2 (+ i1 i3))) (pi2 (aref *stored-primes* i2)))
             (cond
              ((= x pi2) t)
              ((< x pi2) (primeq-aux x i1 (- i2 1)))
              (t (primeq-aux x (+ i2 1) i3)))))))
    (cond
     ((< x 4) t)
     ((evenp x) nil)
     ((> x *max-divide-out*) (pprime? x))
     ((> x *max-stored-prime*)
      (let ((s (isqrt x)))
        (with-all-stored-primes p (if (> p s) (return t))                  
                                (if (= (mod x p) 0) (return nil)))))
     (t (primeq-aux x 2 *nr-of-stored-primes*)))))



;;; pi(x) number of primes p, p <= x
;;; only the approximation pi_ is provided
#| exact version
(defun pi (x)
  (let ((n 0))
    (with-all-primes p
      (if (> p x) (return n) (incf n)))))
simple approximation (too big) but quite ok
(defun pi__ (x) (/ x (- (log x) 1.08366)))
first derivative of approximation below
(defun pi1 (x)
  (let ((logx (log x))) (- (/ 1 logx) (/ 1 logx logx logx))))

(defun factorial (x)
  (do* ((i x (- i 1)) (f x (* f i))) ((<= i 1) f)))

(defun factorial_ (x)
  (* (sqrt (* 2 pi x)) (expt (/ x (exp 1)) x)))         ; Stirling formula

(defun psi_ (n k)
  "approx #{x | x<=n & x is k-smooth} for small k"
  (let ((pp 1) (j 0) (f 1))
    (with-all-primes p
      (if (> p k) (return (/ pp j))
          (setf f (* f (incf j))
                pp (* pp (/ (log n) (log p))))))))
|#

(defun pi_ (n)
  "approx #{p | p<=n & p prime}"
  (let ((logn (log n))) (/ (* n (+ 1 (/ 1 logn))) logn)))

;;; a hard-to-factor integer of approximately d decimal digits

(defun hard (d)
  (* (nextpprime (random (expt 10 (ceiling d 2))))
     (nextpprime (random (expt 10 (floor d 2))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hmpqs
;;; not there exists an .pdf file "hmpqs for dummies" which explains
;;; the math stuff on this algorithm. in most places I tried to make
;;; identifiers similar to those in the text. unfortunately this text
;;; is for the time beeing in german language ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; factor-base


(defstruct factor 
  (prime 0 :type fixnum)       ; the prime p itself, jacobi(n,p)=1...
  (sqrtnmodp 0 :type fixnum)   ; ... so there exists sqrtnmodp^2 = n (mod p)
  (log2p 0 :type fixnum)       ; approximately [log2(p)], used for sieving
  (log10p 0.0 :type float)     ; log10(p), used to build a
  (1/amodp 0 :type fixnum)     ; inverse of a (mod p)
  (dp 0 :type fixnum)          ; the "zeros", s with g(s)=0 (mod p)
  (ep 0 :type fixnum))         ; and s' with g(s')=0 (mod p)

(defmacro fb-p (q) `(factor-prime (aref fb ,q)))
(defmacro fb-log10 (q) `(factor-log10p (aref fb ,q))) 

  
(defun make-factor-base (n fb-size)
  (let ((fb (make-array fb-size :element-type 'factor))
        (i 1))
    (setf (aref fb 0) (make-factor :prime -1))
    (with-all-primes x
      (when (quadratic-residue? n x)
        (setf (aref fb i)
              (make-factor :prime     x
                           :sqrtnmodp (let ((s (sqrtmodprime n x))) (min s (- x s)))
                           :log2p     (integer-length x)
                           :log10p    (log x 10)
                           :1/amodp   -1))
          (if (= (incf i) fb-size) (return fb))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; solver
;;;
;;; the solver uses solutions of the eliminator to find factors
;;; of n. it aborts the complete factorization process, when n has
;;; been completely broken into prime factors.
;;;
;;; the result of the gaussian elimination is a list (i1 ... im)
;;; which, once mapped onto xarray, turns out to be (x1 ... xm)
;;; and we know v^2 = (x1^2-n) * ... * (xm^2-n) is a square.
;;; furthermore u^2 = x1^2 * ... * xm^2 is a square.
;;; these squares are obviously kongruent modulo n
;;; and so gcd(u-v, n) and gcd(u+v, n) divide n
;;; with a good chance of non-triviality.
;;; Note: u and v are large, around 2000 digits if n has 30 digits.
;;; around 10000 digits if n has 40 digits...


(defun try-to-solve (ix-list)
  (declare (special n xarray))
  (declare (special nroftrivials nrofnontrivials))
  (let* ((u 1) 
         (v^2 1)
         (v (dolist (i ix-list (isqrt v^2))
              (dolist (xi (aref xarray i))
                (setf u (* u xi))
                (setf v^2 (* v^2 (- (* xi xi) n)))))))
    (if (/= (* v v) v^2)
      (error "a q-sieve-solution is not square") ;;; this must never happen
      (let ((f (gcd (- u v) n)))
        (if (or (= f 1) (= f n))
          (incf nroftrivials)
          (progn (incf nrofnontrivials) (new-factor f)))))))


;;; (power-breaker x) checks, if an integer x is an e-th power of y.
;;; it assumes, that x has no divisor y <= max-factor.
;;; if no factors have been divided out, (max-factor 1.5),
;;; be aware that it will be much slower then...
;;; it returns two values, e and y, which means x = (expt y e) and
;;; there is no higher exponent than e, so y is not a power itself.

(defun power-breaker (x &optional (max-factor 1.5))
  (let ((emax (/ (log x) (log max-factor))))
    (with-all-stored-primes e
      (if (>= e emax) 
        (return (values 1 x))
        (labels ((binsearch (r0 r1)
                   (if (<= (- r1 r0) 1) nil
                       (let* ((r (div2 (+ r0 r1)))
                              (rrr (expt r e)))
                         (cond ((> rrr x) (binsearch r0 r))
                               ((< rrr x) (binsearch r r1))
                               (t r))))))
          (let ((e-th-root (binsearch 0 x)))
            (when e-th-root 
              (return 
               (multiple-value-bind 
                 (e1 x1) (power-breaker e-th-root max-factor)
                 (values (* e e1) x1))))))))))


;;; new-factor splits n down to prime-factors
;;; during progress it holds non-prime-factors in n-leftovers
;;; and returns prime-factors in factors-of-n


(defun multiple-cons (x nrofcopies xcopies)
  (if (= 0 nrofcopies) xcopies 
      (multiple-cons x (- nrofcopies 1) (cons x xcopies))))

(defun multiple-append (x nrofcopies xcopies)
  (if (= 0 nrofcopies) xcopies 
      (multiple-append x (- nrofcopies 1) (append x xcopies))))

(defun new-factor (f)
  (declare (special n-leftover factors-of-n))
  (labels ((check-prime (p)
             (if (prime? p) 
               (progn (push p factors-of-n) nil)
               (multiple-value-bind (e e-th-root-of-p) (power-breaker p *max-stored-prime*)
                 (let ((ppp (multiple-cons e-th-root-of-p e nil)))
                   (if (prime? e-th-root-of-p)
                     (progn (setf factors-of-n (append factors-of-n ppp)) nil)
                     ppp))))))
    (setf n-leftover
          (mapcan 
           #'(lambda (f1)
               (let ((g (gcd f f1)))
                 (if (or (= g 1) (= g f1))
                   (list f1)
                   (append (check-prime g)
                           (check-prime (truncate f1 g))))))
           n-leftover))
    (when (null n-leftover) (got-them-all))))


;;;;;;;;;;;;; eliminator

(defun xor-merge (u v)
  (let* ((uv (copy-list '(nil))) (uvtail uv))
    (loop
      (if (null u) (return (nconc uvtail v)))
      (if (null v) (return (nconc uvtail u)))
      (let ((u1 (car u)) (v1 (car v)))
        (if (= u1 v1) (setf u (cdr u) v (cdr v))
            (if (> u1 v1)
              (progn (setf uvtail (cdr (nconc uvtail (list u1))))
                     (setf u (cdr u)))
              (progn (setf uvtail (cdr (nconc uvtail (list v1))))
                     (setf v (cdr v)))))))
    (cdr uv)))


;;; continuous gaussian elimination
(defun put-into-its-bucket (r)
  (declare (special buckets))
  (let ((p-list (car r)))
    (if (null p-list) 
      (try-to-solve (cadr r))
      (let* ((p0 (car p-list)) 
             (p0-r (aref buckets p0)))
        (if (null p0-r)
          (setf (aref buckets p0) r)
          (put-into-its-bucket
           (list (xor-merge (cdr p-list) (cdar p0-r))
                 (xor-merge (cadr r) (cadr p0-r)))))))))

;;; if a new relation is found, it will be normally inserted into
;;; the continuous gaussian elimination algorithm, which will throw
;;; a solution, if it had been fed enough relations to find one.
;;; however, for experimenting with the hmpqs it is useful to have
;;; other options here (define-relation-handling experimental).
;;; the three possible settings for the experimental mode are:
;;; (setf *how-to-handle-new-relations* 'continuous-elimination)
;;;   behaves normally.
;;; (setf *how-to-handle-new-relations* 'count-b)
;;;   new relations are counted but not inserted into the
;;;   solving process, the sieving is terminated when there have
;;;   been found as many relations, as there are factors in the factor
;;;   base, which is approximately what is required to find a solution.
;;;   This was useful to find out the run-time-ratio between sieving 
;;;   and eliminating.
;;; (setf *how-to-handle-new-relations* 'count-x)
;;;   as count-b, but sieving stops when *count-x* relations
;;;   have been found. this mode was featured for preparing the
;;;   factoring of a very large n, which might be considerably sped
;;;   up by factoring k*n instead of n, to test the performance of
;;;   various k in advance.
;;; if you use count-b or count-x, the hmpqs should be directly
;;; addressed through the qs function, that returns two values
;;; and the second value is a float giving the run-time in minutes.
;;; on-top functions, like factors, will error, as in these cases
;;; no list of factors will be returned as first value.

(defvar *how-to-handle-new-relations* 'continuous-elimination)
(defvar *count-x* 100)


(defmacro define-relation-handling (mode)
  `(defun divide-out-mod-2 (x hx a-factors)
     (declare (special fb fb-size))
     (declare (special nrofrelations duetopartials xarray))
     (declare (special slp-hash slp-table slp-max))
     (labels 
       ((new-relation (factor-list x-list)
          ,(let ((normal-behaviour `(let ((xindex nrofrelations))
                                   ;   (statistic nrofp/rel (length factor-list))
                                      (incf nrofrelations)
                                      (setf (aref xarray xindex) x-list)
                                      (put-into-its-bucket 
                                       (list (xor-merge factor-list a-factors)
                                             (list xindex))))))
             (if (eq mode 'experimental)
               `(case *how-to-handle-new-relations*
                  ((continuous-elimination) ,normal-behaviour)
                  ((count-b)
                   (when (= (incf nrofrelations) fb-size) (got-them-all)))
                  ((count-x)
                   (when (= (incf nrofrelations) *count-x*) (got-them-all))))
               normal-behaviour))))
       (let* ((i 1) 
              (p (fb-p 1))
              (hxrest hx) 
              (oddexp nil)
              (factors (and (< hx 0) (progn (setf hxrest (- hx)) '(0)))))
         (loop
           (multiple-value-bind (quo rem) (truncate hxrest p)
             (if (zerop rem)
               (progn (setf oddexp (not oddexp)) (setf hxrest quo))
               (progn (if oddexp (push i factors))
                      (when (= hxrest 1)
                        (new-relation factors (list x))
                        (return))
                      (when (= (incf i) fb-size)
                   ;     (statistic hxrest hxrest)
                        (when (<= hxrest slp-max)
                          (let* ((slp-index (logand (div2 hxrest) slp-hash))
                                 (slp (aref slp-table slp-index)))
                            (if (or (null slp)  (< hxrest (car slp)))
                              (setf (aref slp-table slp-index) 
                                    (list hxrest (xor-merge factors a-factors) x))
                              (when (= hxrest (car slp))
                       ;         (statistic hxrest* hxrest)
                                (incf duetopartials)
                                (new-relation (xor-merge factors (cadr slp))
                                              (list x (caddr slp)))))))
                        (return))
                      (setf p (fb-p i))
                      (setf oddexp nil)))))))))


(define-relation-handling high-end-usage)
;(define-relation-handling experimental)
;(setf *how-to-handle-new-relations* 'continuous-elimination)
;(setf *how-to-handle-new-relations* 'count-b)
;(setf *how-to-handle-new-relations* 'count-x)
;(setf *count-x* 1000)



;;;;;;;;;;;;; sieve
;
; g(x)  = (ax+b)^2 - N                        b^2-N = 0 (mod a)
; h(x)  = g(x)/a                             -M <= x <= +M    y = x+M
; h1(y) = h(y-M) = a y^2 + beta y + gamma     0 <= y <= 2M    x = y-M
; beta  = 2(b-Ma)
; gamma = aM^2 - 2bM + (b^2-N)/a



#| 
these functions have been defined for the very fishy development 
of a fast sieve initialization routine. we leave them here, because
the slow function init-sieve-1 explains in a very simple fashion
what the fast function init-sieve below does...
(defun ss (from to) 
  (declare (special sieve))
  (subseq sieve from to))
(defun clear-sieve ()
  (declare (special sieve sieve-size))
  (dotimes (y sieve-size) (setf (aref sieve y) -999)))
(defun check-sieve ()
  (declare (special sieve sieve-size))
  (declare (special a beta gamma))
  (let ((nrofdevs 0)
        (avgdev 0))
    (dotimes (y sieve-size)
      (let* ((sy (aref sieve y))
             (dev (- sy (ilog2 (h1 y)))))
        (if (= sy -999) (debug "no init at" y)
            (when (/= 0 dev)
              (incf nrofdevs) (incf avgdev (abs dev))))))
    (if (zerop nrofdevs) (format t "super sieve!~%")
        (setf avgdev (float (/ avgdev nrofdevs))))
    (debug nrofdevs avgdev)))
(defun init-sieve-1 (a beta gamma)
  (declare (special sieve sieve-size))
  (dotimes (y sieve-size)
    (setf (aref sieve y) (list y (ilog2 (h1 y)))))) 
|#

            
(defmacro h1 (y) `(+ (* (+ (* a ,y) beta) ,y) gamma))
(defmacro ilog2 (x) `(integer-length (abs ,x)))
(defmacro 2^i (i) `(ash 1 ,i))
(defmacro ilog2h1y (y) `(ilog2 (h1 ,y)))



(defmacro yofh (h quadrant)
  `(,(case quadrant ((1 2) `-) ((3 4) `+))
    y2 (sqrt (max (+ y2^2 (/ (- ,(case quadrant ((1 4) h) ((2 3) `(- ,h)))
                                gamma) a)) 0))))

        
(defmacro init-subsieve (y ystop quadrant)
  `(let ((y ,y) (ystop ,ystop))
    (loop
      (let* ((ilog (ilog2h1y y))
             (yto (min ,(case quadrant
                          ((1 3) `(floor (yofh (2^i (- ilog 1)) ,quadrant)))
                          ((2 4) `(floor (yofh (- (2^i ilog) 1) ,quadrant))))
                       ystop)))
        (loop (setf (aref sieve y) ilog)
              (if (> (incf y) yto) (return)))
        (if (> y ystop) (return))))))
  
(defun init-sieve (a beta gamma)
  (declare (special sieve sieve-size))
  (let* ((y2 (/ (float (- beta)) (2* a)))
         (y2^2 (* y2 y2))
         (w (sqrt (- y2^2 (/ gamma a))))
         (y1 (- y2 w))
         (y3 (+ y2 w)))
    (init-subsieve            0 (floor y1)       1)
    (init-subsieve (ceiling y1) (floor y2)       2)
    (init-subsieve (ceiling y2) (floor y3)       3)
    (init-subsieve (ceiling y3) (- sieve-size 1) 4)))


(defun sieve-for-ab (a b a-factors)
  ;  (declare (special a b))
  (declare (special m n fb fb-start fb-size))
  (declare (special sieve sieve-size threshold))
  (declare (special nrofpolynoms nrofrelations))
  (incf nrofpolynoms)
  (let* ((beta (2* (- b (* m a))))
         (gamma (+  (* (- (* a m) (2* b)) m)  (/ (- (* b b) N) a))))
    ;    (declare (special beta gamma))
    (init-sieve a beta gamma)
    (labels ((sieve-in (p dp ilogp)
               (let ((y (+ dp m (* p (ceiling (- 0 m dp) p)))))
                 (if (< y sieve-size)
                   (loop (decf (aref sieve y) ilogp)
                         (if (>= (incf y p) sieve-size) (return)))))))
      (do ((i fb-start (+ i 1))) ((= i fb-size))
        (let* ((fbi (aref fb i)) 
               (p (factor-prime fbi)) 
               (ilogp (factor-log2p fbi)))
          (when (plusp (factor-1/amodp fbi))
            (sieve-in p (factor-dp fbi) ilogp)
            (sieve-in p (factor-ep fbi) ilogp)))))
    (dotimes (y sieve-size)
      (when (< (aref sieve y) threshold)
        (divide-out-mod-2 (+ (* a (- y m)) b)
                          (h1 y)
                          a-factors)))))



;;;;;;;;;;;;; B-generator

(defun b-generator (a a-factors d)
  (declare (special fb fb-start fb-size kip))
  (let ((a-max (car a-factors)) (a-fax (cdr a-factors)))
    (do ((px (- fb-size 1) (- px 1))) ((< px fb-start))
      (setf (factor-1/amodp (aref fb px))
            (if (= px a-max) 
              (progn (setf a-max (if (null a-fax) -1 (pop a-fax))) -1)
              ; a-max = -1 flags a-fax is done
              ; 1/amodp = -1 means p is an a-factor, not to be sieved
              (invmod! a (fb-p px))))))
  (when (null kip)        
    (initial-report)                ; allocated, as the nr of dimensions is known
    (setf kip (make-array (list d fb-size) :element-type 'fixnum)))
  (let ((myi (make-array d :initial-element +1))
        (bi (make-array d))
        (a-fax a-factors)
        (b 0))
    (dotimes (i d)
      (let* ((fbi (aref fb (pop a-fax)))
             (ai (factor-prime fbi))
             (a/ai (div a ai))
             (a/ai^-1 (invmod! a/ai ai))
             (s1 (factor-sqrtnmodp fbi))
             (s2 (- ai s1))
             (gamma (min (mod (* a/ai^-1 s1) ai) (mod (* a/ai^-1 s2) ai)))
             ; not necessary, but keeps b half the size
             (bii (* a/ai gamma)))
        (setf (aref bi i) bii)
        (incf b bii)))
    (do ((px fb-start (+ px 1))) ((= px fb-size))
      (let* ((fbpx (aref fb px))
             (1/amodp (factor-1/amodp fbpx)))
        (when (plusp 1/amodp)              ; if factor px is a sieving prime
          (let* ((p (factor-prime fbpx))
                 (bmodp (mod b p))
                 (sp (factor-sqrtnmodp fbpx)))
            (setf (factor-dp (aref fb px)) (mod (* 1/amodp (- sp bmodp)) p))
            (setf (factor-ep (aref fb px)) (mod (* 1/amodp (- 0 sp bmodp)) p))
            (dotimes (i d)
              (setf (aref kip i px) 
                    (mod  (* 1/amodp -2 (aref bi i))  p)))))))
    ;;; at this point the hypercube is ready for traversion....
    (labels ((pathfinder (i)
               (when (> i 0)
                 (pathfinder (- i 1))
                 (let ((my (- (aref myi i))))
                   (setf (aref myi i) my)
                   (setf b  (+ b (2* (* my (aref bi i)))) )
                   (do ((px fb-start (+ px 1))) ((= px fb-size))
                     (when (plusp (factor-1/amodp (aref fb px)))
                       (let ((p (fb-p px))
                             (delta (* my (aref kip i px))))
                         (setf (factor-dp (aref fb px)) 
                               (mod (+ (factor-dp (aref fb px)) delta) p))
                         (setf (factor-ep (aref fb px)) 
                               (mod (+ (factor-ep (aref fb px)) delta) p))))))
                 (sieve-for-ab a b a-factors)
                 (pathfinder (- i 1)))))
      (sieve-for-ab a b a-factors)
      (pathfinder (- d 1)))))

           
;;;;;;;;;;;;; A-generator

(defun fb-log-sum (i0 i1)
  (declare (special fb))
  (do* ((i i0 (+ i 2))
        (x 0))
       ((> i i1) x)
    (incf x (fb-log10 i))))

(defmacro topoff-index (log-sum)
  `(truncate (* topoff-table-size (- ,log-sum min-log10-topoff))
             (- max-log10-topoff min-log10-topoff)))
(defmacro topoff-bucket (log-sum)
  `(aref topoff-table (topoff-index ,log-sum)))

(defmacro make-topoff-n-cubes (dimension)
  (labels ((n-symbols (var d)
             (if (= d 0) nil (cons (gensym var) (n-symbols var (- d 1))))))
    (let* ((qstop (cons 'q1 (n-symbols "q1-" (- dimension 1))))
           (reverseqstop (reverse qstop))
           (r (n-symbols "r" dimension))
           (x (n-symbols "x" dimension)))
      (labels ((qstoplets (q delta)
                 (if q (cons `(,(car q) (- q1 ,delta))
                             (qstoplets (cdr q) (+ delta 2)))))
               (nested-loops (r0 r123 q321 x0 x123 r3210)
                 (let* ((r1 (car r123)) 
                        (r23 (cdr r123))
                        (x1 (car x123))
                        (fblogr1 `(fb-log10 ,r1)))
                   `(do ((,r1 ,r0 (+ ,r1 2)))
                        ((> ,r1 ,(car q321)))
                      (let ((,x1 ,(if (null x0) fblogr1 `(+ ,x0 ,fblogr1))))
                        ,(if (null r23)
                           `(push (list ,@r3210) (topoff-bucket ,x1))
                           (nested-loops `(+ ,r1 2) r23 (cdr q321)
                                         x1 (cdr x123) r3210)))))))
        `(let* ((q0 (odd-up q-min))
                (q1 (odd-down q-max))
                ,@(qstoplets (cdr qstop) 2))
           (setf nroftopoffcubes (binoc (+ 1 (div2 (- q1 q0))) ,dimension))
           (setf topoff-dim ,dimension)
           (setf topoff-table-size
                 ,(if (= dimension 1)
                    `nroftopoffcubes
                    `(min 20000 (round nroftopoffcubes 8))))
           (setf topoff-table 
                 (make-array (1+ topoff-table-size) :initial-element nil))
           (setf min-log10-topoff (fb-log-sum q0 (+ q0 ,(* 2 (- dimension 1)))))
           (setf max-log10-topoff (fb-log-sum ,(car reverseqstop) q1))
           ,(nested-loops 'q0 r reverseqstop nil x (reverse r)))))))


(defun merge-cubes (a1 a2)
  (cond ((null a2) a1)
        ((null a1) a2)
        (t (let ((c1 (car a1)) (c2 (car a2)))
             (if (> c1 c2) (cons c1 (merge-cubes (cdr a1) a2))
                 (cons c2 (merge-cubes (cdr a2) a1)))))))

(defun make-cubes (a-list log-rest q qmax d)
  (declare (special fb))
  (declare (special topoff-table topoff-table-size
                    min-log10-topoff max-log10-topoff))
  (declare (special ideal-a nrofhypercubes a-error))
  (let (q-rest)
    (loop
      (if (> q qmax) (return))
      (setf q-rest (- log-rest (fb-log10 q)))
      (if (< q-rest min-log10-topoff) (return))
      (if (> q-rest max-log10-topoff)
        (make-cubes (cons q a-list) q-rest (+ q 2) qmax (+ d 1))
        (if (> q-rest min-log10-topoff)
          (let ((bottom-cube (cons q a-list)))
            ; this loop runs zero times if the topoff-bucket is empty:
            (dolist (topoff-cube (topoff-bucket q-rest))
              (let* ((a-factors (merge-cubes topoff-cube bottom-cube))
                     (a (let ((a 1))
                          (dolist (i a-factors a) 
                            (setf a (* a (fb-p i)))))))
                (setf a-error (abs (/ (- a ideal-a) (float ideal-a))))
                (incf nrofhypercubes)
                (b-generator a a-factors d)
                (report?)
                )))))
      (incf q 2))))


(defun make-cublets (a-list log-rest q qmax d)
  (declare (special fb))
  (declare (special ideal-a nrofhypercubes a-error))
  (declare (special min-log10-topoff max-log10-topoff))
  (let (q-rest)
    (loop
      (if (> q qmax) (return))
      (setf q-rest (- log-rest (fb-log10 q)))
      (if (< q-rest min-log10-topoff) (return))
      (if (> q-rest max-log10-topoff)
        (make-cublets (cons q a-list) q-rest (+ q 1) qmax (+ d 1))
        (let* ((a-factors (cons q a-list))
               (a (let ((a 1))
                    (dolist (i a-factors a) 
                      (setf a (* a (fb-p i)))))))
          (setf a-error (abs (/ (- a ideal-a) (float ideal-a))))
          (incf nrofhypercubes)
          (b-generator a a-factors d)
          (report?)
          ))
      (incf q 1))))
  

; a is constructed by multiplying
; permutations of factors (with even indices) called bottom-cube
; by the n factors (odd indices) of topoff-n-cubes
; topoff-cubes are indexed by their log in a table, so its easy to find
; those that make approximately log(bottom)+log(topoff) = log(ideal-a)

(defun a-generator ()
  (declare (special sieve-size n log10n m fb fb-size report))
  (let* (
         (ideal-a (ceiling (isqrt (* 2 n)) m))
         (log10-ideal-a (log ideal-a 10))
         (q-min 10)
         (q-max (min 210 (- fb-size 1)))
         topoff-table topoff-table-size 
         (topoff-dim 0)
         min-log10-topoff max-log10-topoff
         nroftopoffcubes
         (nrofhypercubes 0)
         (nrofpolynoms 0)
         a-error
         )
    (declare (special ideal-a log10-ideal-a))
    (declare (special q-min q-max))
    (declare (special topoff-table topoff-dim topoff-table-size
                      nroftopoffcubes
                      min-log10-topoff max-log10-topoff))
    (declare (special nrofhypercubes nrofpolynoms a-error))
    (when (> log10n 42) 
      (make-topoff-n-cubes 3)
      (make-cubes nil log10-ideal-a (even-up q-min) (even-down q-max) (+ topoff-dim 1)))
    (when (> log10n 23)
      (make-topoff-n-cubes 2)
      (make-cubes nil log10-ideal-a (even-up q-min) (even-down q-max) (+ topoff-dim 1)))
    (setf q-min 2 min-log10-topoff -0.1 max-log10-topoff 0.1)
    (loop (make-cublets nil log10-ideal-a 2 q-max 1)
          (setf max-log10-topoff min-log10-topoff)
          (setf min-log10-topoff (* 2 min-log10-topoff)))
    ; archaic genom leftover protected by infinite loop.....
    (if report (format t "Out of hypercubes at ~S~%" log10n))
    (let* ((help (prime (- *nr-of-stored-primes* 1 (random 10000)))))
      (multiple-value-bind (helpf ttlt) (qs (* n help) :report report)
        (let ((f (remove help helpf))) ;;; i am not sure if its safe
          (centered-report f)
          (values f ttlt))))))


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; reporting & timing
;
; time0 is the very beginning
; time1 is the beginning of the sieving, #equ/min refers to it!
; time2 is the time of the last report, #new/min refers to it
;       
(defconstant minutes-per-internal-time-unit
  (/ 1.0 60.0 internal-time-units-per-second))

(defun minutes () 
  (* (get-internal-run-time) minutes-per-internal-time-unit))

(defun nice-time (mins)
  (cond
   ((< mins 0.017) (format nil "~Dms" (round (* mins 60000.0))))
   ((< mins 1.0) (format nil "~,1Fs" (* mins 60.0)))
   ((< mins 60.0) 
    (multiple-value-bind (min s) (truncate mins)
      (setf s (round (* 60.0 s)))
      (when (= s 60) (setf s 0) (incf min))
      (if (= 0 s) (format nil "~Dmin" min)
          (format nil "~Dmin~Ds" min s))))
   (t (multiple-value-bind (h min) (truncate (/ mins 60.0))
        (setf min (round (* 60.0 min)))
        (when (= min 60) (setf min 0) (incf h))
        (if (= 0 min) (format nil "~Dh" h)
            (format nil "~Dh~Dmin" h min))))))
   

(defmacro concat (&rest strngs) `(concatenate 'string ,@strngs))
(defmacro str$ (x) `(format nil "~A" ,x))

(defconstant *qs-line-width* 74)
(defconstant *qs-line* "--------------------------------------------------------------------------")

(defun centered-title (x)
  (let* ((x (format nil "- ~A -" x))
         (lenx (length x))
         (lenmissing (- *qs-line-width* lenx)))
    (concat (if (< lenmissing 1) x
                (concat (subseq *qs-line* 0 (ceiling lenmissing 2)) x
                        (subseq *qs-line* 0 (floor lenmissing 2))))
            "~%")))

(defun centered-report (x)
  (declare (special report))
  (when report (format t (centered-title x))))

(defun fb-picture (wid)
  (declare (special fb fb-start fb-size))
  (let* ((left "{-1") (right (concat " ... " (str$ (fb-p (- fb-size 1))) "}"))
         (w (- wid (length left) (length right)))  (q 0))
    (labels ((add-if-you-can? (x)
               (let ((l (length x)))
                 (if (<= l w) (progn (setf left (concat left x)) (decf w l) t)))))
      (loop 
        (if (= (incf q) fb-start) (unless (add-if-you-can? " ;") (return)))
        (unless (add-if-you-can? (concat " " (str$ (fb-p q)))) (return)))
      (concat left right))))


(defun factors-of-n-report ()
  (declare (special factors-of-n report))
  (when report (format t (centered-title factors-of-n))))


(defun initial-report ()
  (declare (special log10n m fb fb-size threshold delta-t
                    report time0 time1 time2
                    ideal-a log10-ideal-a
                    q-min q-max 
                    topoff-dim topoff-table-size nroftopoffcubes
                    min-log10-topoff max-log10-topoff))
  (setf time1 (minutes) time2 time1)
  (when report
    (format t "log10(N) (M~7D) (h(x*)) (a_ideal) (a_primes) (~6D/~5A~A) thr~%"
            m nroftopoffcubes topoff-table-size
            (case topoff-dim ((0) "cublets") ((1) "  lines") ((2) "squares") ((3) "  cubes")))
    (format t "~6,2F ~9,2F ~9,2F ~8,2F ~6,2F ~5,2F ~6,2F ~6,2F  d~5,1@F ~3D~%"
            log10n (log m 10) (- log10n log10-ideal-a) log10-ideal-a
            (fb-log10 q-min) (fb-log10 q-max) min-log10-topoff max-log10-topoff
            delta-t threshold)
    (format t "fctrbase (B~7D) ~54@A~%" fb-size (fb-picture 54))
    (format t "cub a-error #h(x)  equ/min new/min  #equ  #due done%    est/min ~10,3F~%"
          (- time1 time0))))


(defun report ()
  (declare (special report time0 time1 time2 nrofr2))
  (declare (special nrofhypercubes a-error nrofpolynoms))
  (declare (special nrofrelations duetopartials fb-size))
  (labels
      ((save/ (a b) (if (= b 0) 999999 (/ a b))))
    (let* ((now (minutes))
           (since-last-report (- now time2))
           (elapsed (- now time1))
           (elapsed-total (- now time0))
           (total-rate (save/ nrofrelations elapsed))
           (current-rate (save/ (- nrofrelations nrofr2) since-last-report))
           (estimate (+ elapsed-total (/ (- fb-size nrofrelations)
                                         current-rate)))
           )
    ;cub a-error #h(x) equ/min new/min  #equ  #due done%   est/minn
      (format t "~4A~6F% ~5D  ~7F ~7F ~5D ~5D ~5,1F ~10,3F ~10,3F~%"
              nrofhypercubes (* a-error 100.0) nrofpolynoms
              total-rate current-rate
              nrofrelations duetopartials (/ nrofrelations fb-size 0.01)
              estimate elapsed-total)
      (setf time2 now)
      (setf nrofr2 nrofrelations))))

(defun report? ()
  (declare (special report))
  (when report (report)))

(defun report?sieving-time ()
  (declare (special report time1))
  (when report (report))
  (- (minutes) time1))

(defun report?total-time ()
  (declare (special report time0 factors-of-n slp-hash slp-table
                    sieve-size fb-size nroftrivials nrofnontrivials
                    nrofpolynoms nrofrelations duetopartials))
  (let ((total-time (- (minutes) time0)))
    (when report 
      (report)
      (let* ((slp-size (+ slp-hash 1))
             (slp-found (let ((sum 0))
                          (dotimes (i slp-size sum)
                            (if (aref slp-table i) (incf sum)))))
             (sieved (* nrofpolynoms sieve-size)))
        (format t "#h(x)*2M/equation  non/triv   #slp~6D  due% saved%~%"
                slp-found)
        (format t "~8E ~8E  ~2D +~2D   ~5,2F%~6D  ~4,1F  ~4,1F  ~20@A~%"
                sieved (/ sieved nrofrelations)
                nrofnontrivials nroftrivials
                (/ slp-found slp-size 0.01) slp-size
                (/ duetopartials nrofrelations 0.01)
                (/ (- fb-size nrofrelations) fb-size 0.01)
                (nice-time total-time)))
      (centered-report factors-of-n))
    total-time))

#|
------------- 6103758287276125231264105636697360331257720953 -------------
log10(N) (M  32866) (h(x*)) (a_ideal) (a_primes) (161700/20000  cubes) thr
 45.79      4.52     27.26    18.53   1.67  3.47   5.83  10.39  d +0.0  20
fctrbase (B   2527)   {-1 2 3 7 11 13 17 19 23 ; 37 47 73 79 89 ... 47837}
cub a-error #h(x)  equ/min new/min  #equ  #due done%    est/min      0.018
1   .02954%   128  14006.7 14006.7   416     2  16.5      0.198      0.048
2   .02820%   256  13878.8 13748.6   817     6  32.3      0.201      0.077
3   .01661%   384  13965.7 14144.3  1222    17  48.4      0.198      0.105
4   .00163%   512  13983.5 14032.5  1668    33  66.0      0.198      0.137
5   .00407%   640  11961.1 7185.75  2031    41  80.4      0.257      0.188
6   .01911%   728  8597.32 2914.94  2324    52  92.0      0.358      0.288
#h(x)*2M/equation  non/triv   #slp  1001  due% saved%
4.785E+7 2.059E+4   1 + 1    1.53% 65536   2.2   8.0                 17.3s
----------- (137526383686732454899213 44382453196615039261981) -----------
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; parameters

(defun m-opt (log10n) (round (* 4804 (exp (* 0.042 log10n)))))

(defun b-opt (n)
  ;;; exp( sqrt(2)/4 * sqrt(log n * log log n) )
  (let ((logn (log n))) (round (exp (sqrt (* 0.125 logn (log logn)))))))

(defun factor-base-size (n) (min 9000 (b-opt n)))

(defun a_ideal (n m) (/ (isqrt (* 2 n)) m))

;;; multiplying n by a small factor p, might improve
;;; our factor-base. this frequently results in faster
;;; factorizations, because this measure can heavily
;;; counteract the fact, that n*p is greater than n.
;;; how to perform a time-critical factorization:
;;; type (densities n) and choose those p, which
;;; have large f or d.
;;; sieve for one hypercube, using (* n p) for
;;; these p, use the same factor base size for
;;; comparability, and then factor (* n p) using
;;; the p that had the best equ/min-rate during
;;; the test-sieving.

#|
(defun density (n &optional (b 100))
  (let ((d 0) (bcount 0))
    (with-all-stored-primes p
      (if (= bcount b) (return d))
      (when (quadratic-residue? n p)
        (incf bcount)
        (incf d (* (log p) (+ (/ 1 p) )))))))

(defun densities (n &optional (b 100))
  (let ((bestd 0) (bestp 0) d f)
    (dolist (p '(1 2 3 5 7 11 13 17 19 23) bestp)
      (setf d (density (* n p) b))
      (setf f (- d (* 0.3 (log p))))
      (format t "p=~d d=~d f=~d~%" p d f)
      (if (> d bestd) (setf bestd d bestp p)))))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; QS-shell
;;; the QS function allows a variety of results to be returned
;;; specified by the :return parameter, that defaults to the
;;; list of factors of n. additionally the let-qs macro provides
;;; a shortcut to bind these return-values parameters locally,
;;; which is quite useful for experimentals


(defun got-them-all ()
  (declare (special return n factors-of-n))
  (declare (special threshold sieve-size fb fb-size ideal-a))
  (declare (special nrofpolynoms nrofrelations duetopartials sieve-size))
  (let ((time (report?total-time)))
    (labels 
        ((return-value (id)
           (case id
             ((factors)    factors-of-n)
             ((n)          n)
             ((time)       time)
             ((m)          (div2 sieve-size))
             ((b)          fb-size)
             ((pmax)       (fb-p (- fb-size 1)))
             ((threshold)  threshold)
             ((a)          ideal-a)
             ((tau)        (float (/ (- nrofrelations duetopartials) 
                                     (* nrofpolynoms sieve-size))))
             ((tau-err%)   (/ 100.0 nrofpolynoms))
             (t            `(,id ???)))))
      (throw 'got-them-all 
             (if (atom return)
                 (return-value return)
                 (mapcar #'return-value return))))))


(defmacro let-qs (return-ids n &rest stuff)
  (labels ((option-symbol-p (s) (and (eq (aref (format nil "~s" s) 0) #\:) s)))
    (let ((qs-results (gensym)))
      `(let* ((,qs-results
               (qs ,n :return (quote ,return-ids)
                   ,@(do ((opts nil (cons (pop stuff) (cons (pop stuff) opts))))
                         ((not (option-symbol-p (car stuff))) opts))))
              ,@(mapcar #'(lambda (x) `(,x (pop ,qs-results))) return-ids))
         ,@stuff))))


(defun qs (n &key (m 0)                 ; [-M...+M] is the sieve
                  (b 0)                 ; factor base size
                  (delta-t 0)           ; threshold adjustment
                  (omit-below 25)       ; don't sieve in primes below this
                  (slp-log2 16)         ; log2 of single-large-prime-table
                  (alfa 32)             ; slp < alfa * pmax
                  (report t)            ; 
                  (return 'factors))    ; see got-them-all above
  (declare (special n m fb-start report delta-t return))
  (centered-report n)
  (let* ((time0 (minutes)) time1 time2 (nrofr2 0)

         (n-leftover (list n))
         (factors-of-n nil)
         (nrofnontrivials 0)
         (nroftrivials 0)
         (log10n (log n 10))
         (fb-size (max 20 (if (= b 0) (truncate (factor-base-size n)) b)))
         (fb (make-factor-base n fb-size))
         (pmax (fb-p (- fb-size 1)))
         (kip nil) ; defined here to avoid reallocation of this big array
         (fb-start 1)                                ; see below
         
         (sieve-size (+ 1 (* 2 (if (= m 0) (setf m (m-opt log10n)) m))))
         (sieve (make-array sieve-size))
         (threshold (+ delta-t (log pmax 2) -0.5))   ; see below
         
         (nrofrelations 0)
         (duetopartials 0)
         (buckets (make-array fb-size :initial-element nil))
         (xarray (make-array (+ fb-size 1000)))      ; should be safe!?!
         
         (slp-hash-size (expt2 slp-log2))
         (slp-hash (- slp-hash-size 1))
         (slp-table (make-array slp-hash-size :initial-element nil))
         (slp-max (* alfa pmax))
         )
    (declare (special time0 time1 time2 nrofr2))
    (declare (special n-leftover factors-of-n log10n))
    (declare (special nroftrivials nrofnontrivials))
    (declare (special fb fb-size fb-start kip))
    (declare (special sieve sieve-size threshold))
    (declare (special nrofrelations duetopartials buckets xarray))
    (declare (special slp-hash slp-table slp-max))
    (setf threshold
          (loop (let ((p (fb-p fb-start)))
                  (if (>= p omit-below) (return (floor threshold)))
                  (incf threshold (* 2 (/ (log p 2) p)))
                  (incf fb-start))))
    (catch 'got-them-all (a-generator))))


;;; redefinition from the factorization module
;;; (qs n) does not work if n is a prime or power of a prime.
;;; this is the nature of the quadratic sieve. additionally
;;; the solver will fail if n is divisible by a power of prime
;;; which is less or equal to a limit, which for perfomance
;;; reasons and common sense has been set to *max-stored-prime*.
;;; therefore (factors n) is a safe all-purpose access to the 
;;; hypercube multiple polynomial quadratic sieve, that returns
;;; a sorted list of primes, whose product is n.

(defun factors (n &key (report nil))
  (let* ((primefactors (when (< n 0) (setf n (- n)) (list -1)))
         (i 0) (p 2))
    (cond
     ((zerop n) (error "0 has no factors"))
     ((= n 1) primefactors)
     (t (loop 
          (multiple-value-bind (d m) (truncate n p)
            (cond
             ((zerop m) 
              (push p primefactors) (setf n d)
              (if (= n 1) (return (reverse primefactors))))
             ((< (incf i) *nr-of-stored-primes*)
              (setf p (aref *stored-primes* i)))
             (t (return
                 (multiple-value-bind
                   (e e-th-root-of-n) (power-breaker n *max-stored-prime*)
                   (if (prime? e-th-root-of-n)
                     (reverse (multiple-cons e-th-root-of-n e primefactors))
                     (sort (multiple-append (qs e-th-root-of-n :report report)
                                            e primefactors) #'<))))))))))))

;;; (efactors n) returns ((p1 e1) (p2 e2) ... (pn en))
;;; factorization of n = p1^e1 * p2^e2 * ... * pn^en

(defun efactors (n &key (report nil))
  (let* ((f (factors n :report report))
         p e (ef nil))
    (loop (if (null f) (return (reverse ef)))
          (setf p (pop f) e 1)
          (loop (when (or (null f) (/= (car f) p))
                  (push (list p e) ef)
                  (return))
                (incf e) (pop f)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; that's it
;;;;;; here is my email adress: ulimy at freenet dot de ;;;;;;;;;;;;;;;;;;;;;;

