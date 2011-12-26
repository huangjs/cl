(in-package "ACL2")

;; (local (include-book "merge"))
;; (include-book "ireps") ;make local?
;; (local (include-book "rnd"))
;; (local (include-book "bias"))
;; (local (include-book "sgn"))
;; (local (include-book "bits"))
;; (local (include-book "trunc"))
;; (local (include-book "away"))
;; (local (include-book "near"))
;; (local (include-book "near+"))
;; (local (include-book "sticky"))
;; (local (include-book "drnd-original"))
;; ;; (local (include-book "../../arithmetic/top"))

;; (local (in-theory (enable evenp))) ;yuck
;; (local (in-theory (disable EXPT-2-TYPE-LINEAR))) ;yuck!

;; Necessary functions:

(defun natp (x)
  (declare (xargs :guard t))
  (and (integerp x)
       (<= 0 x)))

(defund fl (x)
  (declare (xargs :guard (real/rationalp x)))
  (floor x 1))

(defund cg (x)
  (declare (xargs :guard (real/rationalp x)))
  (- (fl (- x))))

(defun expo-measure (x)
;  (declare (xargs :guard (and (real/rationalp x) (not (equal x 0)))))
  (cond ((not (rationalp x)) 0)
	((< x 0) '(2 . 0))
	((< x 1) (cons 1 (fl (/ x))))
	(t (fl x))))

(defund expo (x)
  (declare (xargs :guard t
                  :measure (expo-measure x)))
  (cond ((or (not (rationalp x)) (equal x 0)) 0)
	((< x 0) (expo (- x)))
	((< x 1) (1- (expo (* 2 x))))
	((< x 2) 0)
	(t (1+ (expo (/ x 2))))))

;could redefine to divide by the power of 2 (instead of making it a negative power of 2)...
(defund sig (x)
  (declare (xargs :guard t))
  (if (rationalp x)
      (if (< x 0)
          (- (* x (expt 2 (- (expo x)))))
        (* x (expt 2 (- (expo x)))))
    0))

;make defund?
(defun sgn (x)
  (declare (xargs :guard t))
  (if (or (not (rationalp x)) (equal x 0))
      0
    (if (< x 0)
        -1
      1)))

(defund exactp (x n)
;  (declare (xargs :guard (and (real/rationalp x) (integerp n))))
  (integerp (* (sig x) (expt 2 (1- n)))))

(defun fp+ (x n)
  (+ x (expt 2 (- (1+ (expo x)) n))))

(defund trunc (x n)
  (declare (xargs :guard (integerp n)))
  (* (sgn x) (fl (* (expt 2 (1- n)) (sig x))) (expt 2 (- (1+ (expo x)) n))))

(defund away (x n)
  (* (sgn x) (cg (* (expt 2 (1- n)) (sig x))) (expt 2 (- (1+ (expo x)) n))))


(defund re (x)
  (- x (fl x)))

(defund near (x n)
  (let ((z (fl (* (expt 2 (1- n)) (sig x))))
	(f (re (* (expt 2 (1- n)) (sig x)))))
    (if (< f 1/2)
	(trunc x n)
      (if (> f 1/2)
	  (away x n)
	(if (evenp z)
	    (trunc x n)
	  (away x n))))))

(defund near+ (x n)
  (if (< (re (* (expt 2 (1- n)) (sig x)))
	 1/2)
      (trunc x n)
    (away x n)))

(defund sticky (x n)
  (cond ((exactp x (1- n)) x)
	(t (+ (trunc x (1- n))
              (* (sgn x) (expt 2 (1+ (- (expo x) n))))))))

(defund inf (x n)
  (if (>= x 0)
      (away x n)
    (trunc x n)))

(defund minf (x n)
  (if (>= x 0)
      (trunc x n)
    (away x n)))

(defund rnd (x mode n)
  (case mode
    (away (away x n)) ;added by eric in august, 2001
    (near+ (near+ x n))
    (trunc (trunc x n))
    (inf (inf x n))
    (minf (minf x n))
    (near (near x n))
    (otherwise 0)))

(defund IEEE-MODE-P (mode)
  (member mode '(trunc inf minf near)))

(defund common-rounding-mode-p (mode)
  (or (IEEE-mode-p mode) (equal mode 'away) (equal mode 'near+)))

(defund flip (m)
  (case m
    (inf 'minf)
    (minf 'inf)
    (t m)))

; bias of a q bit exponent field is 2^(q-1)-1 
(defund bias (q) (- (expt 2 (- q 1)) 1) )



;; New stuff:
;;
;;;**********************************************************************
;;;                         Denormal Rounding 
;;;**********************************************************************

;;; because of the drnd definition changed, we abandon all the proofs in
;;; rel5/support/drnd.lisp moved to rel5/support/drnd-original.lisp

;; (i-am-here) ;; Sun Oct 15 15:54:19 2006

(defund drnd (x mode p q)
  (rnd x mode (+ p (expo x) (- (expo (spn q))))))

(defthmd drnd-minus
  (equal (drnd (* -1 x) mode p q)
         (* -1 (drnd x (flip mode) p q)))
  :hints (("Goal" :in-theory (enable drnd expo-minus  rnd-minus))))

;----------------------------------------------------------------------




(local 
 (encapsulate ()
       (local (include-book "../../arithmetic/fl"))
       (local 
          (defthm fl-1/2-sig-x-is-zero-lemma
            (implies (and (rationalp x)
                          (rationalp y)
                          (< 0 y)
                          (<= y 1/2))
                     (equal (fl (* (sig x) y))
                            0))
            :hints (("Goal" :in-theory (disable sig-upper-bound)
                     :use ((:instance sig-upper-bound)
                           (:instance sig-lower-bound))))))

       ;; we really need these two lemma
       (defthm fl-1/2-sig-x-is-zero-lemma-2
         (implies (and (rationalp x)
                       (rationalp y)
                       (not (equal x 0))
                       (< 0 y)
                       (<= y 1/2))
                  (equal (fl (* -1 (sig x) y))
                         -1))
         :hints (("Goal" :in-theory (enable sig fl-minus)
                  :use ((:instance fl-1/2-sig-x-is-zero-lemma)))))

       (defthm expt-2-no-greater-than-1 
            (implies (and (<= (+ p (expo x))
                              (expo (spn q)))
                          (integerp p))
                     (<= (* 2
                            (EXPT 2
                                  (+ -1 P (EXPO X)
                                     (* -1 (EXPO (SPN Q))))))
                         1))
            :hints (("Goal" :use ((:instance expt-weak-monotone-linear
                                             (n (+ -1 P (EXPO X)
                                                   (* -1 (EXPO (SPN Q)))))
                                             (m -1)))))
            :rule-classes :linear)

       (defthm fl-1/2-sig-x-is-zero
           (implies (and (rationalp x)
                         (case-split (not (equal x 0)))
                         (integerp p)
                         (<= (+ p (expo x))
                             (expo (spn q))))
                    (equal (FL (* (SIG X)
                                  (EXPT 2
                                        (+ -1 P (EXPO X)
                                           (* -1 (EXPO (SPN Q)))))))
                           0))
           :hints (("Goal" :use ((:instance fl-1/2-sig-x-is-zero-lemma
                                            (y (EXPT 2
                                                     (+ -1 P (EXPO X) 
                                                        (* -1 (EXPO (SPN q)))))))))))


       (defthm fl-1/2-sig-x-is-zero-2
           (implies (and (rationalp x)
                         (case-split (not (equal x 0)))
                         (integerp p)
                         (<= (+ p (expo x))
                             (expo (spn q))))
                    (equal (FL (* -1 (SIG X)
                                  (EXPT 2
                                        (+ -1 P (EXPO X)
                                           (* -1 (EXPO (SPN Q)))))))
                           -1))
           :hints (("Goal" :use ((:instance fl-1/2-sig-x-is-zero-lemma-2
                                            (y (EXPT 2
                                                     (+ -1 P (EXPO X) 
                                                        (* -1 (EXPO (SPN q)))))))))))))


;----------------------------------------------------------------------

(encapsulate () 
;;; prove the first condition in drepp 
             ;;
             ;;L d            (DEFUN DREPP (X P Q)
             ;;                      (AND (RATIONALP X)
             ;;                           (NOT (= X 0))
             ;;                           (<= (- 2 P) (+ (EXPO X) (BIAS Q)))
             ;;                           (<= (+ (EXPO X) (BIAS Q)) 0)
             ;;                           (EXACTP X (+ -2 P (EXPT 2 (- Q 1)) (EXPO X)))))

             (local (encapsulate ()
;;;;
;;;;          (<= (+ (EXPO X) (BIAS Q)) 0)  if x < (spn q)
;;;; 
                                 (local
                                  (defthm expo-less-than-minus-1-lemma
                                    (IMPLIES (AND (< N (EXPO X))
                                                  (< 0 X)
                                                  (integerp n)
                                                  (RATIONALP X))
                                             (<= (EXPT 2 (+ 1 N)) X))
                                    :hints (("Goal" :use ((:instance expt-weak-monotone-linear
                                                                     (n (+ 1 n))
                                                                     (m (expo x)))
                                                          (:instance expo-lower-bound))))))

                                 (local
                                  (defthm expo-less-than-minus-1
                                    (implies (and (< 0 x)
                                                  (integerp n)
                                                  (rationalp x)
                                                  (< X (EXPT 2 (+ 1 n))))
                                             (<= (expo x) n))
                                    :hints (("Goal" :cases ((> (expo x) n))))
                                    :rule-classes :linear))

                                 (defthm less-than-spn-implies-expo-less
                                   (implies (and (< (abs x) (spn q))
                                                 (> q 0)
                                                 (> x 0)
                                                 (integerp q)
                                                 (rationalp x))
                                            (>= 0 (+ (bias q) (expo x))))
                                   :hints (("Goal" :in-theory (enable spn expo-minus)
                                            :use ((:instance expo-monotone (x (abs x)) (y (spn q))))))
                                   :rule-classes :linear))

                    ) ;;; END OF    (<= (+ (EXPO X) (BIAS Q)) 0)  if x < (spn q)


             (local (encapsulate () 

;;;
;;;     (EXACTP X (+ -2 P (EXPT 2 (- Q 1)) (EXPO X)))))
;;;

                                 (defthm exactp-drnd-specific
                                   (implies (and (rationalp x)
                                                 (> (+ p (expo x))
                                                    (expo (spn q)))
                                                 (integerp p)
                                                 (integerp q)
                                                 (> q 0))
                                            (EXACTP (DRND X MODE P Q)
                                                    (+ -2 P (EXPO X) (EXPT 2 (+ -1 Q)))))
                                   :hints (("Goal" :in-theory (enable drnd spn bias)
                                            :use ((:instance RND-EXACTP-A
                                                             (X x) (mode MODE)
                                                             (n (+ -1 P (BIAS Q) (EXPO X)))))))))
                    ) ;;; END OF  (EXACTP X (+ -2 P (EXPT 2 (- Q 1)) (EXPO X)))))



             (local (encapsulate () 

                                 (local
                                  (defthm expt-equal-specific-lemma
                                    (implies (and (EQUAL 0 (+ y x))
                                                  (integerp x)
                                                  (integerp y))
                                             (equal (expt 2 (+ 1 x))
                                                    (expt 2 (+ 1 (* -1 y)))))
                                    :hints (("Goal" :cases ((equal x (* -1 y)))))))

;;                                  (local 
;;                                   (defthm integerp-expo
;;                                     (integerp (expo x))))
                                  


                                 (defthm expt-equal-specific
                                   (implies (and (EQUAL 0 (+ (BIAS Q) (EXPO X)))
                                                 (rationalp x)
                                                 (integerp q)
                                                 (> q 0))
                                            (equal (expt 2 (+ 1 (expo x)))
                                                   (expt 2 (+ 1 (* -1 (bias q))))))
                                   :hints (("Goal"  
                                            :cases ((equal (expo x)
                                                           (* -1 (bias q)))))
                                           ("Subgoal 1" :in-theory (disable expt-equal-specific-lemma)
                                            :use ((:instance
                                                               expt-equal-specific-lemma
                                                               (y (bias q))
                                                               (x (expo x)))))))
                                 )) ;; don't know why we need this. 




             (local (encapsulate () 

                                 (defthm minus-expt-reduce
                                   (implies (and (integerp p)
                                                 (integerp q)
                                                 (> q 0)
                                                 (rationalp x))
                                            (equal (+ -1 P (EXPO X) (EXPT 2 (+ -1 Q)))
                                                   (+ 1 p (expo x) (* -1 (expo (spn q))))))
                                   :hints (("Goal" :in-theory (enable spn bias expo-2**n))))


                                 ))

             (local (encapsulate ()
;;;
;;;           (<= (- 2 P) (+ (EXPO X) (BIAS Q)))
;;;
                                 (defthm p-expo-x-expo-spn
                                   (implies (and (> (+ p (expo x))
                                                    (expo (spn q)))
                                                 (rationalp x)
                                                 (integerp p)
                                                 (integerp q)
                                                 (> q 0))
                                            (>= (+ (BIAS Q) (EXPO x))
                                                (+ 2 (* -1 p))))
                                   :hints (("Goal" :in-theory (enable spn)))
                                   :rule-classes :linear))

                    ) ;;;  END OF          (<= (- 2 P) (+ (EXPO X) (BIAS Q)))



             (local
              (defthm drnd-exactp-a-lemma
                (implies (and (rationalp x)
                              (< (EXPO (SPN Q)) (+ P (EXPO X)))
                              (> x 0)
                              (< (abs x) (spn q))
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (common-rounding-mode-p mode))
                         (or (drepp (drnd x mode p q) p q)
                             (= (drnd x mode p q) 0)
                             (= (drnd x mode p q) (* (sgn x) (spn q)))))
                :rule-classes ()
                :hints (("Goal"  :in-theory (e/d (drepp) (REARRANGE-NEGATIVE-COEFS-<))
                         :do-not '(fertilize)
                         :cases ((not (equal (expo (drnd x mode p q)) (expo x)))))
                        ("Subgoal 2" :use ((:instance less-than-spn-implies-expo-less)))
                        ("Subgoal 1" :in-theory (enable drepp exactp-2**n)
                         :cases ((not (equal (drnd x mode p q) (expt 2 (+ 1 (expo x)))))))
                        ("Subgoal 1.2" :cases ((not (equal (expo x) (* -1 (bias q))))))
                        ("Subgoal 1.2.2" :in-theory (enable sgn spn))
                        ("Subgoal 1.2.1" :use ((:instance less-than-spn-implies-expo-less)))
                        ("Subgoal 1.1" :in-theory (enable drnd)
                         :use ((:instance expo-rnd
                                          (n (+ P (EXPO X) (- (EXPO (SPN Q)))))))))))


;;              (defthm drepp-minus
;;                (implies (and (rationalp x)
;;                              (integerp p)
;;                              (integerp q))
;;                         (equal (drepp (* -1 x) p q)
;;                                (drepp x p q)))
;;                :hints (("Goal" :in-theory (enable expo-minus drepp))))

             (encapsulate ()
                          (local 
                           (defthm bias-expo-reduce
                             (implies (and (integerp q)
                                           (> q 0))
                                      (equal (+ (bias q) (expo (spn q)))
                                             1))
                             :hints (("Goal" :in-theory (enable spn)))))
         
                          (local
                           (defthm integerp-less-than
                             (implies (and (integerp p)
                                           (integerp q)
                                           (> q 0)
                                           (> p 1))
                                      (<= (+ 1 (BIAS Q) (* -1 P) (EXPO (SPN Q))) 0))
                             :hints (("Goal" :in-theory (enable spn)))
                             :rule-classes :linear))

                          (local
                           (defthm exactp-fact
                             (implies (and (integerp p)
                                           (integerp q)
                                           (> q 0)
                                           (> p 1))
                                      (EXACTP (EXPT 2 (+ 1 (* -1 P) (EXPO (SPN Q))))
                                              (+ -1 (EXPO (SPN Q))
                                                 (EXPT 2 (+ -1 Q)))))
                             :hints (("Goal" :in-theory (enable spn exactp-2**n bias)))))



                          (local 
                           (defthm expt-2-no-greater-than-2
                             (implies (and (integerp q)
                                           (> q 0))
                                      (<= (EXPT 2
                                                (+ 1 (* -1 q)))
                                          1))
                             :hints (("Goal" :use ((:instance expt-weak-monotone-linear
                                                              (n (+ 1 (* -1 q)))
                                                              (m 0)))))
                             :rule-classes :linear))

             (defthm exactp-spn-p
               (implies (and (integerp p)
                             (integerp q)
                             (> q 0)
                             (> p 1))
                        (exactp (spn q) p))
               :hints (("Goal" :in-theory (enable spn
                                                  exactp-2**n))))




             
             (defthm local-rewrite-hack
               (implies (and (equal (+ x (spn q)) 0)
                             (< (EXPO (SPN Q)) (+ P (EXPO X)))
                             (common-rounding-mode-p mode)
                             (integerp p)
                             (integerp q)
                             (> p 1)
                             (> q 0))
                        (EQUAL (+ (SPN Q)
                                  (RND X MODE
                                       (+ P (EXPO X)
                                          (* -1 (EXPO (SPN Q))))))
                               0))
               :hints (("Goal" :cases ((not (equal x (* -1 (spn
                                                            q)))))
                        :in-theory (enable rnd-exactp-b
                                           expo-minus
                                           rnd-minus))))



               (defthm drnd-exactp-a1
                 (implies (and (rationalp x)
                               (<= (abs x) (spn q))
                               (integerp p)
                               (> p 1)
                               (integerp q)
                               (> q 0)
                               (common-rounding-mode-p mode))
                          (or (drepp (drnd x mode p q) p q)
                              (= (drnd x mode p q) 0)
                              (= (drnd x mode p q) (* (sgn x) (spn q)))))
                 :hints (("Goal" :in-theory (enable rnd-minus drepp-minus
                                                    sgn 
                                                    flip drnd rnd-exactp-b
                                                    expo-minus sgn-minus)
                          :cases ((not (<= (+ p (expo x) (- (expo (spn q))))
                                           0))))
                         ("Subgoal 2" :in-theory (e/d (drepp  expo-minus sgn
                                                       drnd near near+ 
                                                       away cg rnd)
                                                      (REARRANGE-NEGATIVE-COEFS-<)))
                         ("Subgoal 1" :cases ((not (equal x 0))))
                         ("Subgoal 1.2" :in-theory (enable drnd))
                         ("Subgoal 1.1" :cases ((not (> x 0))))
                         ("Subgoal 1.1.2" :use ((:instance drnd-exactp-a-lemma)))
                         ("Subgoal 1.1.1" :use ((:instance drnd-exactp-a-lemma
                                                           (x (* -1 x))
                                                           (mode (flip
                                                                  mode)))
                                                (:instance rnd-exactp-b
                                                           (x (* -1 x))
                                                           (mode (flip mode))))))
                 :rule-classes ()))



         (defthm drnd-exactp-a
           (implies (and (rationalp x)
                         (<= (abs x) (spn q))
                         (integerp p)
                         (> p 1)
                         (integerp q)
                         (> q 0)
                         (common-rounding-mode-p mode))
                    (or (drepp (drnd x mode p q) p q)
                        (= (drnd x mode p q) 0)
                        (= (drnd x mode p q) (* (sgn x) (spn q)))))
           :hints (("Goal" :cases ((not (equal (abs x) (spn q))))
                    :in-theory (enable sgn drnd rnd-minus expo-minus))
                   ("Subgoal 2" :cases ((equal x (spn q))
                                        (equal x (* -1 (spn q)))))
                   ("Subgoal 1" :use drnd-exactp-a1))
           :rule-classes ())


             ) ;; end of drnd-exactp-a

;;; 
;;; extremely bad proof!! 
;;;
;;; We could resolve to mid-range, small-range, large range. 
;;;

(defthmd drnd-exactp-b
     (implies (and (rationalp x)
   	        (drepp x p q)
                   (integerp p)
                   (> p 1)
                   (integerp q)
                   (> q 0)
                   (common-rounding-mode-p mode))
              (equal (drnd x mode p q)
                     x))
     :hints (("Goal" :in-theory (e/d (drepp spn bias drnd)
                                     (common-rounding-mode-p))
              :use ((:instance rnd-exactp-b
                               (n (+ P (EXPO X) (- (EXPO (SPN Q))))))))))


;----------------------------------------------------------------------



(defthm drnd-trunc
  (implies (and (integerp p)
                (> p 1)
                (integerp q)
                (> q 0)
                (rationalp x)
                (<= (abs x) (spn q)))
           (<= (abs (drnd x 'trunc p q))
               (abs x)))
  :hints (("Goal" :in-theory (enable drnd rnd)
           :use ((:instance trunc-upper-bound 
                            (x x)
                            (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))))))

(defthm drnd-away
  (implies (and (integerp p)
                (> p 1)
                (integerp q)
                (> q 0)
                (rationalp x)
                (<= (abs x) (spn q)))
           (>= (abs (drnd x 'away p q))
               (abs x)))
  :hints (("Goal" :in-theory (enable drnd rnd)
           :use ((:instance away-lower-bound
                            (x x)
                            (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))))))

(defthm drnd-minf
  (implies (and (integerp p)
                (> p 1)
                (integerp q)
                (> q 0)
                (rationalp x)
                (<= (abs x) (spn q)))
           (<= (drnd x 'minf p q)
               x))
    :hints (("Goal" :in-theory (enable drnd rnd)
           :use ((:instance minf-lower-bound
                            (x x)
                            (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))))))



(defthm drnd-inf
  (implies (and (integerp p)
                (> p 1)
                (integerp q)
                (> q 0)
                (rationalp x)
                (<= (abs x) (spn q)))
           (>= (drnd x 'inf p q)
               x))
    :hints (("Goal" :in-theory (enable drnd rnd)
           :use ((:instance inf-lower-bound
                            (x x)
                            (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))))))



;----------------------------------------------------------------------


(local 
 (defthm exactp-c-lemma-1
   (IMPLIES (AND (RATIONALP X)
                 (< 0 X)
                 (<= X (SPN Q))
                 (RATIONALP A)
                 (DREPP A P Q)
                 (<= X A)
                 (INTEGERP P)
                 (< 1 P)
                 (INTEGERP Q)
                 (< 0 Q))
            (<= (TRUNC X (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))
                A))
      :hints (("Goal" 
               :use ((:instance trunc-upper-bound 
                                (x x)
                                (n (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))))
      :rule-classes :linear))


(local 
   (encapsulate ()

      ;; (defthmd spd-mult
      ;;   (implies (and (integerp p)
      ;;                 (> p 1)
      ;;                 (integerp q)
      ;;                 (> q 0)
      ;;                 (> r 0)
      ;; 		(rationalp r)
      ;; 		(= m (/ r (spd p q))))
      ;; 	   (iff (drepp r p q)
      ;; 		(and (natp m)
      ;; 		     (<= 1 m)
      ;; 		     (< m (expt 2 (1- p))))))
      ;;   :hints (("Goal" :in-theory (e/d () (am spd drepp
      ;;                                          spd-mult-1-specific-further
      ;;                                          spd-mult-2-specific-further)))
      ;;           ("Subgoal 3" :use ((:instance spd-mult-1-specific-further
      ;;                                         (m (/ r (spd p q))))))
      ;;           ("Subgoal 2" :use ((:instance smallest-spd)))
      ;;           ("Subgoal 1" :use ((:instance spd-mult-2-specific-further)))))

      (local 
         (defthm equal-spd
           (implies (and (integerp p)
                         (integerp q)
                         (> p 1)
                         (> q 0))
                    (equal (spd p q)
                           (EXPT 2 (+ 1 (* -1 P) (EXPO (SPN Q))))))
           :hints (("Goal" :in-theory (enable spd spn bias)))))

      (local 
           (defund denormal-norm (r p q)
             (/ r (spd p q))))

      (local
          (defthm spd-mult-specific
            (implies (and (integerp p)
                          (> p 1)
                          (integerp q)
                          (> q 0)
                          (> r 0)
                          (rationalp r))
                     (= r (* (denormal-norm r p q) (spd p q))))
            :hints (("Goal" :in-theory (enable denormal-norm)))
            :rule-classes nil))

      (local 
          (defthm drepp-implies-denormal-norm-integerp
            (implies (and (drepp r p q)
                          (integerp p)
                          (> p 1)
                          (integerp q)
                          (> q 0)
                          (> r 0)
                          (rationalp r))
                     (integerp (denormal-norm r p q)))
            :hints (("Goal" :use ((:instance spd-mult
                                             (m (denormal-norm r p q)))
                                  (:instance spd-mult-specific))))
            :rule-classes :type-prescription))


      (local 
          (defthm drepp-implies-denormal-norm-less-than
            (implies (and (drepp r p q)
                          (integerp p)
                          (> p 1)
                          (integerp q)
                          (> q 0)
                          (> r 0)
                          (rationalp r))
                     (<= (denormal-norm r p q)
                         (+ -1 (expt 2 (+ -1 p)))))
            :hints (("Goal" :use ((:instance spd-mult
                                             (m (denormal-norm r p q)))
                                  (:instance spd-mult-specific))))
            :rule-classes :linear))

      (local
          (defthm denormal-normal-monotone
            (implies (and (< r1 r2)
                          (integerp (denormal-norm r1 p q))
                          (integerp (denormal-norm r2 p q)))
                     (<= (+ 1 (denormal-norm r1 p q))
                         (denormal-norm r2 p q)))
            :hints (("Goal" :in-theory (enable spd denormal-norm)))
            :rule-classes :linear))

      (local 
      (defthm drepp-diff
           (implies (and (rationalp r1)
                         (rationalp r2)
                         (> r1 r2)
                         (> r2 0)
                         (integerp p)
                         (integerp q)
                         (> p 1)
                         (> q 0)
                         (drepp r1 p q)
                         (drepp r2 p q))
                    (<= (+ r2 (EXPT 2 (+ 1 (* -1 P) (EXPO (SPN Q)))))
                        r1))
           :hints (("Goal" :use ((:instance spd-mult-specific
                                            (r r1))
                                 (:instance spd-mult-specific
                                            (r r2))
                                 (:instance denormal-normal-monotone
                                            (r1 r2)
                                            (r2 r1)))))
           :rule-classes nil))


      (local 
       (defthm expt-merge
         (implies (and (integerp p)
                       (integerp q)
                       (> q 0))
                  (equal (* (EXPT 2 (+ -1 P))
                            (EXPT 2 (+ 2 (* -1 P) (* -1 (BIAS Q)))))
                         (expt 2 (+ 1 (* -1 (bias q))))))
         :hints (("Goal" :in-theory (enable a15)))))


      (local 
       (defthm arithm-hack-specific
        (implies (and (<= (DENORMAL-NORM R P Q)
                          (+ -1 (EXPT 2 (+ -1 P))))
                      (rationalp r)
                      (integerp p)
                      (integerp q)
                      (> q 0))
                 (<= (+ (EXPT 2 (+ 1 (* -1 P) (EXPO (SPN Q))))
                        (* (EXPT 2 (+ 1 (* -1 P) (EXPO (SPN Q))))
                           (denormal-norm r p q)))
                     (spn q)))
        :hints (("Goal" :in-theory (enable spn denormal-norm 
                                           spd)))
        :rule-classes nil))


      (defthm maximal-drepp
         (implies (and (drepp r p q)
                       (integerp p)
                       (> p 1)
                       (integerp q)
                       (> q 0)
                       (> r 0)
                       (rationalp r))
                  (<= (+ r (EXPT 2 (+ 1 (* -1 P) (EXPO (SPN Q)))))
                      (spn q)))
         :hints (("Goal" :use ((:instance drepp-implies-denormal-norm-less-than)
                               (:instance spd-mult-specific)
                               (:instance arithm-hack-specific))))
         :rule-classes :linear)


      (defthm drepp-diff
           (implies (and (rationalp r1)
                         (rationalp r2)
                         (> r1 r2)
                         (> r2 0)
                         (integerp p)
                         (integerp q)
                         (> p 1)
                         (> q 0)
                         (drepp r1 p q)
                         (drepp r2 p q))
                    (<= (+ r2 (EXPT 2 (+ 1 (* -1 P) (EXPO (SPN Q)))))
                        r1))
           :hints (("Goal" :use ((:instance spd-mult-specific
                                            (r r1))
                                 (:instance spd-mult-specific
                                            (r r2))
                                 (:instance denormal-normal-monotone
                                            (r1 r2)
                                            (r2 r1)))))
           :rule-classes nil)



   ))



(local 
 (encapsulate ()
              (local 
               (defthm spd-spd-less-than
                 (implies (and (integerp p)
                               (integerp q)
                               (> p 1)
                               (> q 0))
                          (iff (<= (SPD P Q) A)
                               (<= (EXPT 2 (+ 1 (* -1 P) (EXPO (SPN Q))))
                                   A)))
                 :hints (("Goal" :in-theory (enable spn spd)))))

              (defthm exactp-c-lemma-2
                (implies (and (integerp p)
                              (> p 1)
                              (> x 0)
                              (rationalp a)
                              (integerp q)
                              (> q 0)
                              (rationalp x)
                              (>= a x)
                              (drepp a p q)
                              (<= (abs x) (spn q)))
                         (<= (AWAY X (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))
                             a))
                :hints (("Goal" :cases ((not (>= (+ p (expo x)) (expo (spn q))))))
                        ("Subgoal 2"           
                         :in-theory (enable drnd rnd sgn positive-spd)
                         :use ((:instance drnd-exactp-a
                                          (mode 'away))
                               (:instance away-upper-bound
                                          (x x)
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                               (:instance drepp-diff
                                          (r2 a)
                                          (r1 (AWAY X (+ P (EXPO X) 
                                                         (* -1 (EXPO (SPN Q)))))))))
                        ("Subgoal 1" :in-theory (enable drnd rnd away cg sgn)
                         :use ((:instance smallest-spd (r a)))))
                :rule-classes :linear)))


(local 
   (defthmd drnd-exactp-c-lemma
     (implies (and (rationalp x)
                   (> x 0)
                   (<= (abs x) (spn q))
   		(rationalp a)
                   (drepp a p q)
   		(>= a x)
                   (integerp p)
                   (> p 1)
                   (integerp q)
                   (> q 0)
                   (common-rounding-mode-p mode))
              (>= a (drnd x mode p q)))
     :hints (("Goal" :in-theory (enable sgn drnd rnd))
             ("Subgoal 5" 
              :use ((:instance near-choice
                               (x x)
                               (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))))
             ("Subgoal 2" 
              :use ((:instance near+-choice
                               (x x)
                               (n (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))))))

              


(local 
    (defthm exactp-d-lemma-1
      (IMPLIES (AND (RATIONALP X)
                    (< 0 X)
                    (<= X (SPN Q))
                    (RATIONALP A)
                    (DREPP A P Q)
                    (<= A X)
                    (INTEGERP P)
                    (< 1 P)
                    (INTEGERP Q)
                    (< 0 Q))
            (<= A (AWAY X (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))
      :hints (("Goal" 
               :use ((:instance away-lower-bound
                                (x x)
                                (n (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))))
      :rule-classes :linear))



;;    (local 
;;     (defthm never-zero-drepp
;;       (not (DREPP 0 P Q))
;;       :hints (("Goal" :in-theory (enable drepp)))))
;;

(local 
    (defthm x-less-than-spd-if-negative
      (implies (and (<= (+ P (EXPO X) (* -1 (EXPO (SPN Q)))) 0)
                    (> x 0)
                    (rationalp x)
                    (integerp p)
                    (integerp q)
                    (> q 0))
               (< x (spd p q)))
      :hints (("Goal" :in-theory (enable spd spn)
               :use ((:instance expo-monotone
                                (x (spd p q))
                                (y x)))))))

(local 
    (defthm exactp-d-lemma-2  
      (IMPLIES (AND (RATIONALP X)
                    (<= X (SPN Q))
                    (< 0 X)
                    (RATIONALP A)
                    (DREPP A P Q)
                    (<= A X)
                    (INTEGERP P)
                    (< 1 P)
                    (INTEGERP Q)
                    (< 0 Q))
            (<= A
                (TRUNC X
                       (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))
      :hints (("Goal" :cases ((not (> (+ p (expo x)) (expo (spn q))))))
              ("Subgoal 2" 
               :in-theory (enable drnd rnd sgn positive-spd)
               :use ((:instance drnd-exactp-a
                                (mode 'trunc))
                     (:instance trunc-lower-bound
                               (x x)
                               (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                     (:instance drepp-diff
                               (r1 a)
                               (r2 (trunc X (+ P (EXPO X)
                                               (* -1 (EXPO (SPN Q)))))))))
              ("Subgoal 1" :in-theory (enable drnd rnd spd trunc sgn)
               :use ((:instance smallest-spd (r a))
                     (:instance x-less-than-spd-if-negative))))
     :rule-classes :linear))


(defthmd drnd-exactp-d-lemma
     (implies (and (rationalp x)
                   (<= (abs x) (spn q))
                   (> x 0)
   		(rationalp a)
                   (drepp a p q)
   		(<= a x)
                   (integerp p)
                   (> p 1)
                   (integerp q)
                   (> q 0)
                   (common-rounding-mode-p mode))
              (<= a (drnd x mode p q)))
     :hints (("Goal" :in-theory (enable ieee-mode-p drnd rnd))
             ("Subgoal 2" 
              :use ((:instance near+-choice
                               (x x)
                               (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))))
             ("Subgoal 1" 
              :use ((:instance near-choice
                               (x x)
                               (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))))))




(defthmd drnd-exactp-c
     (implies (and (rationalp x)
                   (<= (abs x) (spn q))
                   (rationalp a)
                   (drepp a p q)
                   (>= a x)
                   (integerp p)
                   (> p 1)
                   (integerp q)
                   (> q 0)
                   (common-rounding-mode-p mode))
              (>= a (drnd x mode p q)))
     :hints (("Goal" :cases ((not (equal x 0)))
              :in-theory (enable drnd-minus flip drepp-minus))
             ("Subgoal 2" :in-theory (enable drnd rnd))
             ("Subgoal 1" :cases ((not (> x 0))))
             ("Subgoal 1.2" :use ((:instance drnd-exactp-c-lemma)))
             ("Subgoal 1.1" :use ((:instance drnd-exactp-d-lemma
                                             (x (* -1 x))
                                             (a (* -1 a))                                         
                                             (mode (flip mode)))))))



(defthmd drnd-exactp-d
     (implies (and (rationalp x)
                   (<= (abs x) (spn q))
   		(rationalp a)
                   (drepp a p q)
   		(<= a x)
                   (integerp p)
                   (> p 1)
                   (integerp q)
                   (> q 0)
                   (common-rounding-mode-p mode))
              (<= a (drnd x mode p q)))
     :hints (("Goal" :cases ((not (equal x 0)))
              :in-theory (enable drnd-minus flip drepp-minus))
             ("Subgoal 2" :in-theory (enable drnd rnd))
             ("Subgoal 1" :cases ((not (> x 0))))
             ("Subgoal 1.2" :use ((:instance drnd-exactp-d-lemma)))
             ("Subgoal 1.1" :use ((:instance drnd-exactp-c-lemma
                                             (x (* -1 x))
                                             (a (* -1 a))                                         
                                             (mode (flip mode)))))))



;----------------------------------------------------------------------

(local
   (encapsulate ()
    
       (local 
        (defthm equal-spd
          (implies (and (integerp p)
                        (integerp q)
                        (> p 1)
                        (> q 0))
                   (equal (spd p q)
                          (EXPT 2 (+ 1 (* -1 P) (EXPO (SPN Q))))))
          :hints (("Goal" :in-theory (enable spd spn bias)))))

       (local 
        (defthm x-less-than-spd-if-negative
          (implies (and (<= (+ P (EXPO X) (* -1 (EXPO (SPN Q)))) 0)
                        (> x 0)
                        (rationalp x)
                        (integerp p)
                        (integerp q)
                        (> q 0))
                   (< x (spd p q)))
          :hints (("Goal" :in-theory (enable spd spn)
                   :use ((:instance expo-monotone
                                    (x (spd p q))))))))


       (defthm drnd-non-negative
         (implies (and (< 0 x)
                       (rationalp x)
                       (integerp p)
                       (integerp q)
                       (> p 1)
                       (> q 0)
                       (common-rounding-mode-p mode))
                  (>= (drnd x mode p q) 0))
         :hints (("Goal" :in-theory (enable ieee-mode-p near near+ drnd rnd)))
         :rule-classes (:type-prescription :linear))



       (defthm drnd-diff-lemma
         (implies (and (rationalp x)
                       (<= x (spn q))
                       (> x 0)
                       (integerp p)
                       (> p 1)
                       (integerp q)
                       (> q 0)
                       (common-rounding-mode-p mode))
                  (< (abs (- x (drnd x mode p q))) (spd p q)))
         :hints (("Goal" :cases ((not (> (+ p (expo x)) (expo (spn q))))))
                 ("Subgoal 2" :in-theory (enable drnd)
                  :use ((:instance rnd-diff
                                   (n (+ P (EXPO X) (* -1 (EXPO (SPN
                                                                 Q))))))))
                 ("Subgoal 1" 
                  :use ((:instance drnd-exactp-c
                                   (a (spd p q)))
                        (:instance drepp-spd)
                        (:instance x-less-than-spd-if-negative)))))))

(defthm drnd-diff
        (implies (and (rationalp x)
                   (<= (abs x) (spn q))
                   (integerp p)
                   (> p 1)
                   (integerp q)
                   (> q 0)
                   (common-rounding-mode-p mode))
              (< (abs (- x (drnd x mode p q))) (spd p q)))
     :hints (("Goal" :cases ((not (equal x 0))))
             ("Subgoal 2" :in-theory (enable drnd rnd spd))
             ("Subgoal 1" :cases ((not (> x 0))))
             ("Subgoal 1.2" :use ((:instance drnd-diff-lemma)))
             ("Subgoal 1.1" :in-theory (enable flip drnd drnd-minus)
              :use ((:instance drnd-diff-lemma
                               (x (* -1 x))
                               (mode (flip mode)))))))



;----------------------------------------------------------------------

(encapsulate () 

             (local
              (defthm drnd-near-est-lemma-1
                (implies (and (rationalp x)
                              (equal (expo a) (expo x))
                              (<= x (spn q))
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near p q)))))
                :hints (("Goal" 
                         :in-theory (enable rnd drnd bias DREPP spn)
                         :use ((:instance near2
                                          (y a)
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))))))



             (local
              (defthm rationalp-drepp
                (implies (drepp a p q)
                         (rationalp a))
                :hints (("Goal" :in-theory (enable drepp)))
                :rule-classes :forward-chaining))

             (local
              (defthm drnd-near-est-lemma-2-1
                (implies (and (rationalp x)
                              (<= x (spn q))
                              (equal (- x (trunc x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                                     (- (away x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))) x))
                              (> a 0)
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near p q)))))
                :hints (("Goal" :in-theory (enable drnd rnd)
                         :use ((:instance near-choice (x x)
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))))))


             (local
              (defthm drnd-near-est-lemma-2-2
                (implies (and (rationalp x)
                              (<= x (spn q))
                              (not (equal (expo x) (expo a)))
                              (<  (- x (trunc x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                                  (- (away x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))) x))
                              (> a 0)
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near p q)))))
                :hints (("Goal" :in-theory (enable drnd rnd)
                         :do-not '(fertilize)
                         :use ((:instance near1-a
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                               (:instance trunc-upper-bound
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))))))



                               
             (local
              (defthm drnd-near-est-lemma-2-3
                (implies (and (rationalp x)
                              (<= x (spn q))
                              (not (equal (expo x) (expo a)))
                              (>  (- x (trunc x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                                  (- (away x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))) x))
                              (> a 0)
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near p q)))))
                :hints (("Goal" :in-theory (enable drnd rnd)
                         :do-not '(fertilize)
                         :use ((:instance near1-b
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                               (:instance away-lower-bound
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))))))





             (local
              (defthm drnd-near-est-lemma-2
                (implies (and (rationalp x)
                              (<= x (spn q))
                              (not (equal (expo a) (expo x)))
                              (> a 0)
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near p q)))))
                :hints (("Goal" :cases ((not (equal (- x (trunc x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                                                    (- (away x (+ P (EXPO X) (* -1 (EXPO (SPN
                                                                                          Q))))) x))))
                         :in-theory (disable abs drnd))
                        ("Subgoal 2" :use ((:instance drnd-near-est-lemma-2-1)))
                        ("Subgoal 1" :cases ((not (< (- x (trunc x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                                                     (- (away x (+ P (EXPO X) (* -1 (EXPO (SPN
                                                                                           Q))))) x)))))
                        ("Subgoal 1.2" :use ((:instance drnd-near-est-lemma-2-2)))
                        ("Subgoal 1.1" :use ((:instance drnd-near-est-lemma-2-3))))))




             (local
              (defthm drnd-near-est-lemma-3
                (implies (and (rationalp x)
                              (<= x (spn q))
                              (not (equal (expo a) (expo x)))
                              (< a 0)
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near p q)))))
                :hints (("Goal" :use ((:instance smallest-spd (r a))
                                      (:instance drnd-diff
                                                 (mode 'near)))))))
                                   


             (local 
              (defthm never-zero-drepp
                (not (DREPP 0 P Q))
                :hints (("Goal" :in-theory (enable drepp)))))

             (local
              (defthm drnd-near-est-lemma
                (implies (and (rationalp x)
                              (<= x (spn q))
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near p q)))))
                :hints (("Goal" :cases ((not (equal (expo a) (expo x)))))
                        ("Subgoal 2" :use ((:instance drnd-near-est-lemma-1)))
                        ("Subgoal 1":cases ((not (equal a 0))))
                        ("Subgoal 1.1":cases ((not (> a 0))))
                        ("Subgoal 1.1.2":use ((:instance drnd-near-est-lemma-2)))
                        ("Subgoal 1.1.1":use ((:instance drnd-near-est-lemma-3))))))

    (defthm drnd-near-est
      (implies (and (rationalp x)
                    (<= (abs x) (spn q))
                    (integerp p)
                    (> p 1)
                    (integerp q)
                    (> q 0)
                    (drepp a p q))
               (>= (abs (- x a)) (abs (- x (drnd x 'near p q)))))
      :hints (("Goal" :cases ((not (equal x 0))))
              ("Subgoal 2" :in-theory (enable drnd rnd))
              ("Subgoal 1" :cases ((not (> x 0))))
              ("Subgoal 1.2" :use ((:instance drnd-near-est-lemma)))
              ("Subgoal 1.1" :use ((:instance drnd-near-est-lemma
                                              (x (* -1 x))
                                              (a (* -1 a))))
               :in-theory (enable drnd-minus))))

             )

;----------------------------------------------------------------------

(encapsulate () 

             (local
              (defthm drnd-near+-est-lemma-1
                (implies (and (rationalp x)
                              (equal (expo a) (expo x))
                              (<= x (spn q))
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near+ p q)))))
                :hints (("Goal" 
                         :in-theory (enable rnd drnd bias DREPP spn)
                         :use ((:instance near+2
                                          (y a)
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))))))




             (local
              (defthm rationalp-drepp
                (implies (drepp a p q)
                         (rationalp a))
                :hints (("Goal" :in-theory (enable drepp)))
                :rule-classes :forward-chaining))



             (local
              (defthm drnd-near+-est-lemma-2-1
                (implies (and (rationalp x)
                              (<= x (spn q))
                              (equal (- x (trunc x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                                     (- (away x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))) x))
                              (> a 0)
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near+ p q)))))
                :hints (("Goal" :in-theory (enable drnd rnd)
                         :use ((:instance near+-choice (x x)
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))))))


;----------------------------------------------------------------------



             (local
              (defthm drnd-near+-est-lemma-2-2
                (implies (and (rationalp x)
                              (<= x (spn q))
                              (not (equal (expo x) (expo a)))
                              (<  (- x (trunc x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                                  (- (away x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))) x))
                              (> a 0)
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near+ p q)))))
                :hints (("Goal" :in-theory (enable drnd rnd)
                         :do-not '(fertilize)
                         :use ((:instance near+1-a
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                               (:instance trunc-upper-bound
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))))))
                               
             (local
              (defthm drnd-near+-est-lemma-2-3
                (implies (and (rationalp x)
                              (<= x (spn q))
                              (not (equal (expo x) (expo a)))
                              (>  (- x (trunc x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                                  (- (away x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))) x))
                              (> a 0)
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near+ p q)))))
                :hints (("Goal" :in-theory (enable drnd rnd)
                         :do-not '(fertilize)
                         :use ((:instance near+1-b
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                               (:instance away-lower-bound
                                          (n (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))))))
                               

             (local
              (defthm drnd-near+-est-lemma-2
                (implies (and (rationalp x)
                              (<= x (spn q))
                              (not (equal (expo a) (expo x)))
                              (> a 0)
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near+ p q)))))
                :hints (("Goal" :cases ((not (equal (- x (trunc x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                                                    (- (away x (+ P (EXPO X) (* -1 (EXPO (SPN
                                                                                          Q))))) x))))
                         :in-theory (disable abs drnd))
                        ("Subgoal 2" :use ((:instance drnd-near+-est-lemma-2-1)))
                        ("Subgoal 1" :cases ((not (< (- x (trunc x (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))
                                                     (- (away x (+ P (EXPO X) (* -1 (EXPO (SPN
                                                                                           Q))))) x)))))
                        ("Subgoal 1.2" :use ((:instance drnd-near+-est-lemma-2-2)))
                        ("Subgoal 1.1" :use ((:instance drnd-near+-est-lemma-2-3))))))


;----------------------------------------------------------------------

             (local
              (defthm drnd-near+-est-lemma-3
                (implies (and (rationalp x)
                              (<= x (spn q))
                              (not (equal (expo a) (expo x)))
                              (< a 0)
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near+ p q)))))
                :hints (("Goal" :use ((:instance smallest-spd (r a))
                                      (:instance drnd-diff
                                                 (mode 'near+)))))))
                                   

             (local 
              (defthm never-zero-drepp
                (not (DREPP 0 P Q))
                :hints (("Goal" :in-theory (enable drepp)))))



             (local
              (defthm drnd-near+-est-lemma
                (implies (and (rationalp x)
                              (<= x (spn q))
                              (> x 0)
                              (integerp p)
                              (> p 1)
                              (integerp q)
                              (> q 0)
                              (drepp a p q))
                         (>= (abs (- x a)) (abs (- x (drnd x 'near+ p q)))))
                :hints (("Goal" :cases ((not (equal (expo a) (expo x)))))
                        ("Subgoal 2" :use ((:instance drnd-near+-est-lemma-1)))
                        ("Subgoal 1":cases ((not (equal a 0))))
                        ("Subgoal 1.1":cases ((not (> a 0))))
                        ("Subgoal 1.1.2":use ((:instance drnd-near+-est-lemma-2)))
                        ("Subgoal 1.1.1":use ((:instance drnd-near+-est-lemma-3))))))

     (defthm drnd-near+-est
       (implies (and (rationalp x)
                     (<= (abs x) (spn q))
                     (integerp p)
                     (> p 1)
                     (integerp q)
                     (> q 0)
                     (drepp a p q))
                (>= (abs (- x a)) (abs (- x (drnd x 'near+ p q)))))
       :hints (("Goal" :cases ((not (equal x 0))))
               ("Subgoal 2" :in-theory (enable drnd rnd))
               ("Subgoal 1" :cases ((not (> x 0))))
               ("Subgoal 1.2" :use ((:instance drnd-near+-est-lemma)))
               ("Subgoal 1.1" :use ((:instance drnd-near+-est-lemma
                                               (x (* -1 x))
                                               (a (* -1 a))))
                :in-theory (enable drnd-minus))))

             )

;;
;; Sat Feb  4 12:35:01 2006 finally! 
;;
;----------------------------------------------------------------------


(encapsulate () 


   (local (encapsulate () 

          (defthm fl-expt-n-minus-1-minus-1
            (implies (and (rationalp x)
                          (case-split (not (equal x 0)))
                          (integerp n)
                          (<= n 0))
                     (equal (fl (* -1 (sig x) (expt 2 (+ -1 n))))
                            -1))
            :hints (("Goal" :use ((:instance fl-1/2-sig-x-is-zero-lemma-2
                                             (y (expt 2 (+ -1 n))))
                                  (:instance expt-weak-monotone-linear
                                             (n (+ -1 n))
                                             (m -1))))))


         (defthm n-zero-away-reduce
           (implies (and (rationalp x)
                         (> x 0)
                         (integerp n)
                         (<= n 0))
                    (equal (away x n)
                           (EXPT 2 (+ 1 (EXPO X) (* -1 n)))))
           :hints (("Goal" :in-theory (enable sgn away cg))))


         (defthm drnd-lemma-trunc-small
           (implies (and (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (rationalp x)
                         (<= (+ p (expo x)) (expo (spn q))))
                    (equal (drnd x 'trunc p q)  0))
           :hints (("Goal" :in-theory (enable drnd rnd))))


         (defthm drnd-lemma-away-small
           (implies (and (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (rationalp x)
                         (<= (+ p (expo x)) (expo (spn q))))
                    (equal (drnd x 'away p q)  
                           (expt 2 (+ 1 (EXPO (SPN Q)) (* -1 p)))))
           :hints (("Goal" :in-theory (enable drnd rnd))))



         (defthm drnd-lemma-minf-small
           (implies (and (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (rationalp x)
                         (<= (+ p (expo x)) (expo (spn q))))
                    (equal (drnd x 'minf p q)  0))
           :hints (("Goal" :in-theory (enable drnd rnd))))



         (defthm drnd-lemma-inf-small
           (implies (and (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (rationalp x)
                         (<= (+ p (expo x)) (expo (spn q))))
                    (equal (drnd x 'inf p q)
                           (expt 2 (+ 1 (EXPO (SPN Q)) (* -1 p)))))
           :hints (("Goal" :in-theory (enable drnd rnd))))



         (local 
          (defthm local-expt-expand 
            (implies (and (integerp p)
                          (integerp q)
                          (> q 0))
                     (equal (EXPT 2 (+ 1 (* -1 P) (EXPO (SPN Q))))
                            (* 2 (expt 2 (+  (* -1 p) (expo (spn q)))))))
            :hints (("Goal" :use ((:instance a15 (i 2) (j1 1)
                                             (j2 (+ (* -1 P) (EXPO (SPN Q))))))))))


         (defthm drnd-lemma-near-small-1
           (implies (and (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (rationalp x)
                         (< x (expt 2 (+ (* -1 p) (expo (spn q)))))
                         (<= (+ p (expo x)) (expo (spn q))))
                    (equal (drnd x 'near p q)
                           0))
           :hints (("Goal" :in-theory (enable drnd rnd)
                    :use ((:instance near1-a (n (+ p (expo x) (* -1 (expo (spn
                                                                           q))))))))))


         (defthm drnd-lemma-near-small-2
           (implies (and (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (rationalp x)
                         (> x (expt 2 (+ (* -1 p) (expo (spn q)))))
                         (<= (+ p (expo x)) (expo (spn q))))
                    (equal (drnd x 'near p q)
                           (expt 2 (+ 1 (EXPO (SPN Q)) (* -1 p)))))
           :hints (("Goal" :in-theory (enable drnd rnd)
                    :use ((:instance near1-b (n (+ p (expo x) (* -1 (expo (spn q))))))))))




         (defthm drnd-lemma-near+-small-1
           (implies (and (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (rationalp x)
                         (< x (expt 2 (+ (* -1 p) (expo (spn q)))))
                         (<= (+ p (expo x)) (expo (spn q))))
                    (equal (drnd x 'near+ p q)
                           0))
           :hints (("Goal" :in-theory (enable drnd rnd)
                    :use ((:instance near+1-a (n (+ p (expo x) (* -1 (expo (spn
                                                                            q))))))))))


         (defthm drnd-lemma-near+-small-2
           (implies (and (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (rationalp x)
                         (> x (expt 2 (+ (* -1 p) (expo (spn q)))))
                         (<= (+ p (expo x)) (expo (spn q))))
                    (equal (drnd x 'near+ p q)
                           (expt 2 (+ 1 (EXPO (SPN Q)) (* -1 p)))))
           :hints (("Goal" :in-theory (enable drnd rnd)
                    :use ((:instance near+1-b (n (+ p (expo x) (* -1 (expo (spn q))))))))))


         (encapsulate ()
                      (local 
                       (defthm spd-/2-rewrite
                         (implies (and (integerp p)
                                       (integerp q)
                                       (> q 0))
                                  (equal (/ (spd p q) 2)
                                         (expt 2 (+ (* -1 p) (expo (spn q))))))
                         :hints (("Goal" :in-theory (enable spd spn)
                                  :use ((:instance a15 (i 2) (j1 1)
                                                   (j2 (+ 1 (* -1 P) (* -1 (BIAS Q))))))))))
                   
                      (local 
                       (defthm less-than-1/2-spd-implies-expo-x-small
                         (implies (and (< x (expt 2 (+ (* -1 p) (expo (spn q)))))
                                       (> x 0)
                                       (rationalp x)
                                       (integerp p)
                                       (integerp q)
                                       (> q 0))
                                  (<= (+ p (expo x)) (expo (spn q))))
                         :hints (("Goal" :use ((:instance expo-monotone
                                                          (x x)
                                                          (y (expt 2 (+ (* -1 p) (expo (spn
                                                                                        q)))))))
                                  :in-theory (enable expo-2**n)))))

                      (defthm drnd-tiny-equal-lemma
                        (implies (and (common-rounding-mode-p mode)
                                      (natp p)
                                      (> p 1)
                                      (natp q)
                                      (> q 0)
                                      (rationalp x)
                                      (< 0 x)
                                      (< x (/ (spd p q) 2))
                                      (rationalp y)
                                      (< 0 y)
                                      (< y (/ (spd p q) 2)))
                                 (equal (drnd x mode p q)
                                        (drnd y mode p q)))
                        :hints (("Goal" :in-theory (enable ieee-mode-p)))
                        :rule-classes nil))


         (defthm sticky-never-increase-over-expt
           (implies (and (< x (expt 2 k))
                         (integerp k)
                         (rationalp x)
                         (> x 0)
                         (> n 0)
                         (integerp n))
                    (< (sticky x n) 
                       (expt 2 k)))
           :hints (("Goal" :use ((:instance expo-sticky)
                                 (:instance expo-monotone
                                            (x (expt 2 k))
                                            (y (sticky x n)))
                                 (:instance expt-weak-monotone-linear
                                            (n k)
                                            (m (expo x)))
                                 (:instance expo-lower-bound
                                            (x x))))))

         (defthm sticky-preserves-inequality
           (implies  (and (< x (expt 2 (+ (* -1 p) (expo (spn q)))))
                          (rationalp x)
                          (> x 0)
                          (> n 0)
                          (integerp n)
                          (integerp p)
                          (integerp q)
                          (> p 1)
                          (> q 0))
                     (< (sticky x n)
                        (expt 2 (+ (* -1 p) (expo (spn q))))))
           :hints (("Goal"  :use ((:instance sticky-never-increase-over-expt
                                             (k (+ (* -1 p)
                                                   (expo (spn q)))))))))

         (defthm greater-than-1/2-spd-implies-n-no-less-than-2
           (implies (and (> x (expt 2 (+ (* -1 p) (expo (spn q)))))
                         (rationalp x)
                         (> x 0)
                         (> n 0)
                         (integerp n)
                         (integerp p)
                         (integerp q)
                         (>= n (+ p (expo x) (- (expo (spn q))) 2))
                         (> p 1)
                         (> q 0))
                    (>= n 2))
           :hints (("Goal" :use ((:instance expo-monotone
                                            (x (expt 2 (+ (* -1 p) (expo (spn q)))))
                                            (y x)))
                    :in-theory (enable expo-2**n)))
           :rule-classes nil)

         (local 
          (defthm trunc-1-m-is-1
            (implies (and (integerp n)
                          (> n 0))
                     (equal (trunc 1 n)
                            1))
            :hints (("Goal" :in-theory (enable trunc a15)))))

         (defthm trunc-2**n
           (implies (and (integerp n)
                         (integerp m)
                         (> m 0))
                    (equal (trunc (expt 2 n) m)
                           (expt 2 n)))
           :hints (("Goal" :use ((:instance trunc-shift
                                            (x 1)
                                            (k n)
                                            (n m)))
                    :in-theory (enable trunc))))


         (defthm sticky-preserves-inequality-2-strong
           (implies  (and (> x (expt 2 (+ (* -1 p) (expo (spn q)))))
                          (rationalp x)
                          (> x 0)
                          (> n 0)
                          (integerp n)
                          (integerp p)
                          (integerp q)
                          (>= n (+ p (expo x) (- (expo (spn q))) 2))
                          (> p 1)
                          (> q 0))
                     (> (sticky x n)
                        (expt 2 (+ (* -1 p) (expo (spn q))))))
           :hints (("Goal" :in-theory (enable sticky trunc-shift sgn)
                    :use ((:instance trunc-monotone
                                     (x (expt 2 (+ (* -1 p) (expo (spn q)))))
                                     (y x)
                                     (n (+ -1 n)))
                          (:instance greater-than-1/2-spd-implies-n-no-less-than-2)))))


         (defthm exactp-expt-2-1
           (implies (and (integerp n)
                         (integerp m)
                         (> n 0))
                    (exactp (expt 2 m) n))
           :hints (("Goal" :in-theory (enable a15 sig exactp))))


         (defthm equal-x-1/2-spd-sticky-n-1/2-spd
           (implies (and (integerp p)
                         (integerp n)
                         (integerp q)
                         (> p 1)
                         (> q 0)
                         (> n 0))
                    (equal (sticky (expt 2 (+ (* -1 p) (expo (spn q)))) n)
                           (expt 2 (+ (* -1 p) (expo (spn q))))))
           :hints (("Goal" :in-theory (e/d (expo-2**n sticky)
                                           (exactp-expt-2-1))
                    :use ((:instance exactp-expt-2-1
                                     (m (+ (* -1 P) (EXPO (SPN Q)))) 
                                     (n (+ -1 n)))))))


         (defthm expo-sticky-strong
           (implies (and (rationalp x)
                         (integerp n) (> n 0))
                    (= (expo (sticky x n))
                       (expo x)))
           :hints (("Goal" :cases ((not (> x 0)))
                    :in-theory (enable expo-minus sticky-minus))
                   ("Subgoal 2" :use ((:instance expo-sticky)))
                   ("Subgoal 1" :use ((:instance expo-sticky
                                                 (x (* -1 x)))))))


;----------------------------------------------------------------------

         (defthm n-equal-zero-implies-ultra-small
           (implies (and (>= 0 (+ p (expo x) (- (expo (spn q))) 2))
                         (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (rationalp x))
                    (< x (expt 2 (+ -1 (* -1 p) (expo (spn q))))))
           :hints (("Goal" :use ((:instance expo-upper-bound)
                                 (:instance expt-weak-monotone-linear
                                            (n (+ 1 (expo x)))
                                            (m (+ -1 (* -1 p) (expo (spn q)))))))))



         (defthm sticky-0-reduce
           (implies (and (> x 0)
                         (rationalp x))
                    (equal (sticky x 0)
                           (EXPT 2 (1+ (EXPO X)))))
           :hints (("Goal" :in-theory (enable sticky trunc sgn exactp))))



         (defthm small-fl-is-minus-1
           (implies (and (rationalp x)
                         (> x 0)
                         (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (< x (expt 2 (+ -1 (* -1 p) (expo (spn q))))))
                    (equal (FL (* -1 (SIG X)
                                  (EXPT 2
                                        (+ -1 P (EXPO X)
                                           (* -1 (EXPO (SPN Q)))))))
                           -1))
           :hints (("Goal" :use ((:instance fl-1/2-sig-x-is-zero-2)
                                 (:instance expo-monotone
                                            (x x)
                                            (y (expt 2 (+ -1 (* -1 p) (expo (spn q))))))))))
                                   

         (defthm small-fl-is-zero-1
           (implies (and (rationalp x)
                         (> x 0)
                         (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (< x (expt 2 (+ -1 (* -1 p) (expo (spn q))))))
                    (equal (FL (* (SIG X)
                                  (EXPT 2
                                        (+ -1 P (EXPO X)
                                           (* -1 (EXPO (SPN Q)))))))
                           0))
           :hints (("Goal" :use ((:instance fl-1/2-sig-x-is-zero)
                                 (:instance expo-monotone
                                            (x x)
                                            (y (expt 2 (+ -1 (* -1 p) (expo (spn q))))))))))
                                   


                             
         (defthm small-fl-is-minus-1-v2
           (implies (and (rationalp x)
                         (> x 0)
                         (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (< x (expt 2 (+ -1 (* -1 p) (expo (spn q))))))
                    (equal (FL (* -1 (SIG (EXPT 2 (+ 1 (EXPO X))))
                                  (EXPT 2
                                        (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))
                           -1))
           :hints (("Goal" :use ((:instance fl-1/2-sig-x-is-zero-2
                                            (x (expt 2 (+ 1 (expo x)))))
                                 (:instance expo-monotone
                                            (x x)
                                            (y (expt 2 (+ -1 (* -1 p) (expo (spn q))))))))))
           
  
                             
         (defthm small-fl-is-zero-1-v2
           (implies (and (rationalp x)
                         (> x 0)
                         (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (< x (expt 2 (+ -1 (* -1 p) (expo (spn q))))))
                    (equal (FL (* (SIG (EXPT 2 (+ 1 (EXPO X))))
                                  (EXPT 2
                                        (+ P (EXPO X) (* -1 (EXPO (SPN Q)))))))
                           0))
           :hints (("Goal" :use ((:instance fl-1/2-sig-x-is-zero
                                            (x (expt 2 (+ 1 (expo x)))))
                                 (:instance expo-monotone
                                            (x x)
                                            (y (expt 2 (+ -1 (* -1 p) (expo (spn q))))))))))
           

         ;; (defthm expo-monotone-strong
         ;;   (implies (and (< x (expt 2 n))
         ;;                 (equal 
         ;;             (rationalp x)

           
         (defthm small-small-lemma
           (implies (<= (+ 2 P (EXPO X)) (EXPO (SPN Q)))
                    (<= (+ -1 p (expo x) (* -1 (expo (spn q))))
                        -3)))

         (defthm small-small-lemma-2
           (implies (<= (+ 2 P (EXPO X)) (EXPO (SPN Q)))
                    (<= (+ p (expo x) (* -1 (expo (spn q))))
                        -2)))

         (defthm small-is-small
           (implies (and (>= 0 (+ p (expo x) (- (expo (spn q))) 2))
                         (rationalp x)
                         (> x 0)
                         (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0))
                    (> 1
                       (* 2 (SIG X)
                          (EXPT 2
                                (+ -1 P (EXPO X)
                                   (* -1 (EXPO (SPN q))))))))
           :hints (("Goal" :use ((:instance sig-upper-bound)
                                 (:instance expt-weak-monotone-linear
                                            (n (+ -1 P (EXPO X)
                                                  (* -1 (EXPO (SPN q)))))
                                            (m -3)))))
           :rule-classes :linear)
           
         (encapsulate () 
                      (local      
                       (defthm sig-expt-fact
                         (implies (integerp n)
                                  (equal (sig (expt 2 n)) 1))
                         :hints (("Goal" :in-theory (enable sig a15)))))

                      (defthm small-is-small-v2
                        (implies (and (>= 0 (+ p (expo x) (- (expo (spn q))) 2))
                                      (rationalp x)
                                      (> x 0)
                                      (natp p)
                                      (> x 0)
                                      (> p 1)
                                      (natp q)
                                      (> q 0))
                                 (> 1
                                    (* 2 (SIG (EXPT 2 (+ 1 (EXPO X))))
                                       (EXPT 2
                                             (+ P (EXPO X) (* -1 (EXPO (SPN Q))))))))
                        :hints (("Goal" :use ((:instance expt-weak-monotone-linear
                                                         (n (+ P (EXPO X)
                                                               (* -1 (EXPO (SPN q)))))
                                                         (m -2)))))
                        :rule-classes :linear))
                



         (defthm extra-small-drnd-is-equal
           (implies (and (< x (expt 2 (+ -1 (* -1 p) (expo (spn q)))))
                         (>= 0 (+ p (expo x) (- (expo (spn q))) 2))
                         (> x 0)
                         (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (rationalp x))
                    (equal (drnd (sticky x 0) mode p q)
                           (drnd x mode p q)))
           :hints (("Goal" :in-theory (enable drnd trunc sgn cg near+ near away rnd sticky))))


         (defthm drnd-sticky-lemma
           (implies (and (common-rounding-mode-p mode)
                         (natp p)
                         (> x 0)
                         (> p 1)
                         (natp q)
                         (> q 0)
                         (rationalp x)
                         (<= x (spn q))
                         (>= n 0)
                         (integerp n)
                         (>= n (+ p (expo x) (- (expo (spn q))) 2)))
                    (equal (drnd (sticky x n) mode p q)
                           (drnd x mode p q)))
           :hints (("Goal" :cases ((not (> (+ p (expo x)) (expo (spn q))))))
                   ("Subgoal 2" :cases ((not (equal n 0))))
                   ("Subgoal 2.1"  :use ((:instance rnd-sticky
                                                    (m (+ p (expo x) 
                                                          (- (expo (spn q)))))))
                    :in-theory (enable drnd))
                   ("Subgoal 1" :in-theory (e/d (common-rounding-mode-p
                                                 sticky-positive ieee-mode-p)
                                                (drnd rnd))
                    :cases ((not (equal x (expt 2 (+ (* -1 p)
                                                     (expo (spn q))))))))
                   ("Subgoal 1.1"  :cases ((not (equal n 0))))
                   ("Subgoal 1.1.2" :use ((:instance extra-small-drnd-is-equal)
                                          (:instance
                                           n-equal-zero-implies-ultra-small)))
                   ("Subgoal 1.1.1" :cases ((not (> x (expt 2 (+ (* -1 p)
                                                                 (expo (spn q)))))))))
           :rule-classes nil)))


  (defthm drnd-sticky
    (implies (and (common-rounding-mode-p mode)
		  (natp p)
		  (> p 1)
		  (natp q)
		  (> q 0)
		  (rationalp x)
                  (<= (abs x) (spn q))
		  (natp n)
		  (>= n (+ p (expo x) (- (expo (spn q))) 2)))
	     (equal (drnd (sticky x n) mode p q)
		    (drnd x mode p q)))
    :hints (("Goal" :cases ((not (equal x 0)))
             :in-theory (enable sticky-minus expo-minus
                                drnd-minus flip))
            ("Subgoal 1" :cases ((not (> x 0))))
            ("Subgoal 1.2" :use ((:instance drnd-sticky-lemma)))
            ("Subgoal 1.1" :use ((:instance drnd-sticky-lemma
                                            (x (* -1 x))
                                            (mode (flip mode)))))))




  (defthm drnd-tiny-equal
    (implies (and (common-rounding-mode-p mode)
                  (natp p)
                  (> p 1)
                  (natp q)
                  (> q 0)
                  (rationalp x)
                  (< 0 x)
                  (< (abs x) (/ (spd p q) 2))
                  (rationalp y)
                  (< 0 y)
                  (< (abs y) (/ (spd p q) 2)))
             (equal (drnd x mode p q)
                    (drnd y mode p q)))
    :hints (("Goal" :use ((:instance drnd-tiny-equal-lemma))))
    :rule-classes nil)

)

;----------------------------------------------------------------------
(encapsulate ()

 (local (encapsulate () 

                     ;; (defthm plus-rnd
                     ;;   (implies (and (rationalp x)
                     ;;                 (>= x 0)
                     ;;                 (rationalp y)
                     ;;                 (>= y 0)
                     ;;                 (integerp k)
                     ;;                 (exactp x (+ -1 k (- (expo x) (expo y))))
                     ;;                 (common-rounding-mode-p mode))
                     ;;            (= (+ x (rnd y mode k))
                     ;;               (rnd (+ x y)
                     ;;                    mode
                     ;;                    (+ k (- (expo (+ x y)) (expo y))))))
                     ;;   :hints (("Goal" :use ((:instance plus-rnd---rtl-rel5-support))))
                     ;;   :rule-classes ())

                     (defthm exactp-spn-fact
                       (implies (and (integerp p)
                                     (> p 1)
                                     (integerp q)
                                     (> q 0))
                                (EXACTP (SPN Q) (+ -1 P)))
                       :hints (("Goal" :in-theory (enable spn exactp-2**n))))

                     (defthm exactp-spn-fact-2
                       (implies (and (integerp p)
                                     (> p 1)
                                     (integerp q)
                                     (> q 0))
                                (EXACTP (SPN Q) P))
                       :hints (("Goal" :in-theory (enable spn exactp-2**n))))

                     (defthm exactp-spn-fact-3
                       (implies (and (integerp p)
                                     (> p 1)
                                     (integerp q)
                                     (> q 0))
                                (EXACTP (* 2 (SPN Q)) P))
                       :hints (("Goal" :in-theory (enable spn exactp-2**n)
                                :use ((:instance a15 (i 2) (j1 1) (j2 (+ 1 (* -1 (BIAS Q)))))))))


                     ;; (defthm expo-unique
                     ;;   (implies (and (<= (expt 2 n) (abs x))
                     ;;                 (< (abs x) (expt 2 (1+ n)))
                     ;;                 (rationalp x)
                     ;;                 (integerp n))
                     ;;            (equal n (expo x)))
                     ;;   :rule-classes ())


                     (encapsulate () 
                                  (local 
                                   (defthm local-expt-expand 
                                     (implies (integerp n)
                                              (equal (EXPT 2 (+ 1 n))
                                                     (* 2 (expt 2 n))))
                                     :hints (("Goal" :use ((:instance a15 (i 2) (j1 1)
                                                                      (j2 n)))))))

                                  (defthm expo-x-plus-spn-equal-expo-spn-lemma
                                    (implies (and (rationalp x)
                                                  (> x 0)
                                                  (< x (expt 2 n))
                                                  (integerp n))
                                             (equal (expo (+ x (expt 2 n)))
                                                    n))
                                    :hints (("Goal" :use ((:instance expo-unique
                                                                     (x (+ x (expt 2 n)))
                                                                     (n n)))
                                             :in-theory (enable expo-2**n
                                                                spn)))
                                    :rule-classes nil))


                     (defthm expo-x-plus-spn-equal-expo-spn
                       (implies (and (rationalp x)
                                     (> x 0)
                                     (< x (spn q))
                                     (integerp q)
                                     (> q 0))
                                (equal (expo (+ x (spn q)))
                                       (expo (spn q))))
                       :hints (("Goal" :in-theory (e/d (spn expo-2**n) ())
                                :use ((:instance expo-x-plus-spn-equal-expo-spn-lemma
                                                 (n (expo (spn q))))))))




                     (defthmd drnd-rewrite-lemma
                       (implies (and (rationalp x)
                                     (>= x 0)
                                     (<= x (spn q))
                                     (common-rounding-mode-p mode)
                                     (integerp p)
                                     (> p 1)
                                     (integerp q)
                                     (> q 0))
                                (equal (drnd x mode p q)
                                       (- (rnd (+ x (* (sgn x) (spn q))) mode p)
                                          (* (sgn x) (spn q)))))
                       :hints (("Goal" :cases ((not (equal x (spn q)))))
                               ("Subgoal 2" :in-theory (e/d (drnd sgn)
                                                            (rnd-exactp-b))
                                :use ((:instance rnd-exactp-b (x (spn q))
                                                 (n p))
                                      (:instance rnd-exactp-b (x (* 2 (spn q)))
                                                 (n p))))
                               ("Subgoal 1" :use ((:instance plus-rnd
                                                             (x (spn q))
                                                             (y x)
                                                             (k (+ p (expo x) (* -1 (expo (spn q)))))))
                                :in-theory (e/d (drnd sgn spn bias) (common-rounding-mode-p)))))


                      (defthm collect-neg-specific
                        (equal (+ (* -1 X) (* -1 (SGN X) (SPN Q)))
                               (* -1 (+ x (* (sgn x) (spn q))))))))

        (defthmd drnd-rewrite
          (implies (and (rationalp x)
                        (<= (abs x) (spn q))
                        (common-rounding-mode-p mode)
                        (integerp p)
                        (> p 1)
                        (integerp q)
                        (> q 0))
                   (equal (drnd x mode p q)
                          (- (rnd (+ x (* (sgn x) (spn q))) mode p)
                             (* (sgn x) (spn q)))))
          :hints (("Goal" :cases ((not (>= x 0))))
                  ("Subgoal 2" :use ((:instance drnd-rewrite-lemma)))
                  ("Subgoal 1" :use ((:instance drnd-rewrite-lemma
                                                (x (* -1 x))
                                                (mode (flip mode))))
                   :in-theory (enable drnd-minus sgn-minus
                                      rnd-minus expo-minus flip))))

        )

;----------------------------------------------------------------------
