;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          finance.lisp
;;;; Purpose:       Financial Load Utilities.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: finance.lisp,v 1.3 2003/10/03 17:36:37 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.finance)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


(defvar *check-for-plus* t
  "Variable that controls whether the calc functions should check their args for
positive numbers.")

(define-condition non-solveable ()
  ((message :reader message :initform "" :initarg :message))
  (:report (lambda (condition stream)
             (format stream "Warning: ~a~%" (message condition))))
  (:documentation "Condition to be signaled when a financial problem 
cannot be solved. This may be because the problem has no solution, or that 
a solution may exist but be numerically unstable to compute."))


;;; Round a <number> to <digit> digits.
(defun round-to (number digit)
  "Round <number> to <digit> digits after the decimal point."
  (let ((level (/ 1.0d0 (expt 10.0d0 digit))))
    (* (ftruncate (+ number (/ level 2.0d0)) level) level)))


(defun check-for-plusp (args)
  "Check that the odd elements of the list, <args>, are valid positive
numbers. Use the even elements as identifiers. If an argument is found that is
not a positive number throw an error."
  (if *check-for-plus*
      (loop for (val val-string) on args by #'cddr do
            (unless (and (numberp val) (plusp val))
              (error "The ~a, ~s, is not a positive number.~%" 
                     val-string val)))))

(defun compute-linear-difference-eqn (x0 a b n)
  "Solve (by iteration) the linear difference equation: 
x_(k+1) = a * x_k + b; x_0 = x0.
Returns x_n."
  (let ((acc (coerce x0 'double-float)))
    (dotimes (i n acc)
      (setf acc (+ b (* acc a))))))

(defun solve-linear-diff-eqn (x0 a b n)
  "Solve (by formula) the linear difference equation: 
x_(k+1) = a * x_k + b; x_0 = x0.
Returns x_n."
  (/ (+ b (* (- (* x0 (- 1 a)) b) (expt a n))) (- 1 a)))


(defun debt-calc (initial-debt annual-interest-rate 
                  monthly-payment number-of-payments)
  "Compute debt at the end of n (number-of-payments) payments, 
based on the initial amount; the (annual) interest rate; and the 
monthly payment. Compounding is assumed to be monthly."
  (solve-linear-diff-eqn initial-debt 
                         (+ 1.0d0 (/ annual-interest-rate 1200.0d0)) 
                         (- monthly-payment) number-of-payments))

(defun iteration-number-when-zero (x0 a b)
  "Finds the iteration at which the solution to the difference equation: 
x_(k+1) = a * x_k + b; x_0 = x0 is 0. 
Returns a number which may be a fraction. Raises a signal of type 
non-solveable if the equation is not solveable."
  (let ((iteration-num
         (/ (log (/ b (- b (* x0 (- 1 a))))) (log a))))
    (if (complexp iteration-num)
        (signal (make-condition 'non-solveable :message 
                                "Can't solve difference equation."))
      iteration-num)))

(defun calc-num-payments (initial-debt annual-interest-rate monthly-payment)
  "Calculates the number of payments to pay off an initial debt with 
a given annual interest rate and monthly payment. The debt is assumed 
to compound monthly. Returns a multiple value: The number of payments; 
the number of whole years this represents; the number of months after the 
number of whole years; the total pay-out; and the last payment."
  (check-for-plusp (list initial-debt "debt"                         
                         annual-interest-rate "annual interest rate"
                         monthly-payment "monthly payment"))
  (handler-case 
    (let ((num (floor 
                (iteration-number-when-zero 
                 initial-debt 
                 (+ 1.0d0 
                    (/ annual-interest-rate 1200.0d0)) 
                 (- monthly-payment)))))
      (let* ((debt (debt-calc initial-debt 
                              annual-interest-rate 
                              monthly-payment 
                              num))
             (pay-out (+ (* num monthly-payment) debt)))
        (when (> debt 0.0d0)
          (incf num))
        (multiple-value-bind (years months) 
            (truncate num 12)
          (values num years (floor months) pay-out debt))))
    (non-solveable () 
      (signal (make-condition 'non-solveable
                :message
                (format nil 
                        "A monthly payment of 
\"~s\" is too low, a solution does not exist." monthly-payment))))
    (:no-error (num years months pay-out debt) 
      (return-from calc-num-payments
        (values num years months pay-out debt)))))
              
(defun display-num-payments (initial-debt annual-interest-rate monthly-payment)
  "Displays the number of payments to pay off an initial debt with a given
annual interest rate and monthly payment. The debt is assumed to compound
monthly."
  (check-for-plusp (list initial-debt "debt"                         
                         annual-interest-rate "annual interest rate"
                         monthly-payment "monthly payment"))
  (let ((*check-for-plus* nil))    
    (handler-case 
        (multiple-value-bind (num-payments years months 
                              total-payout last-payment) 
            (calc-num-payments initial-debt 
                               annual-interest-rate 
                               monthly-payment)
          (format t "Number of payments = ~a " num-payments)
          (if (> years 0)
              (if (= years 1)
                  (format t "(That's ~a year" years)
                (format t "(That's ~a years" years)))
          (if (> months 0)
              (if (= months 1)
                  (format t " and ~a month.)~%" months)
                (format t " and ~a months.)~%" months))
            (format t ".)~%"))
          (format t "Total pay-out      = $~2$~%" total-payout)
          (when (> last-payment 0.0)
            (format t "Last payment       = $~2$~%" last-payment))
          (values))
      (non-solveable (obj) (format t "~a~%" (message obj)) (values))
      (:no-error () (return-from display-num-payments (values))))))



(defun calc-initial-debt (years-to-pay annual-interest-rate monthly-payment)
  "Calculates the initial debt that one can pay off in <years-to-pay> years at a
monthly payment rate of <monthly-payment> with an annual interest rate of
<annual-interest-rate>. Compounding is assumed to occur monthly."
  (check-for-plusp (list years-to-pay "years to pay"                         
                         annual-interest-rate "annual interest rate"
                         monthly-payment "monthly payment"))
  (let ((n (* years-to-pay 12.0d0))
        (a (+ 1.0d0 (/ annual-interest-rate 1200.0d0)))
        (b (- (coerce monthly-payment 'double-float))))
    (round-to (* b (/ (- 1.0d0 (expt a (- n))) (- 1 a))) 2.0d0)))

(defun display-initial-debt (years-to-pay annual-interest-rate monthly-payment)
  "Displays the initial debt that one can pay off in <years-to-pay> years at a
monthly payment rate of <monthly-payment> with an annual interest rate of
<annual-interest-rate>. Compounding is assumed to occur monthly."
    (check-for-plusp (list years-to-pay "years to pay"                         
                           annual-interest-rate "annual interest rate"
                           monthly-payment "monthly payment"))
    (let ((*check-for-plus* nil))    
      (format t "Initial debt = $~$~%" 
              (calc-initial-debt years-to-pay 
                                 annual-interest-rate monthly-payment))
      (values)))


(defun calc-monthly-payment (debt years-to-pay annual-interest-rate)
  "Calculates the monthly payment that one needs in order to pay off a debt of
<debt> in <years-to-pay> years at an interest rate of <annual-interest-rate>.
Compounding is assumed to occur monthly."
  (check-for-plusp (list debt "initial debt"
                         years-to-pay "years to pay"
                         annual-interest-rate "annual interest rate"))
  (let ((a (+ 1.0d0 (/  annual-interest-rate 1200.0d0)))
        (x0 (coerce debt 'double-float))
        (n (* years-to-pay 12.0d0)))
    (round-to (/ (* (expt a n) x0 (- a 1.0d0)) (- (expt a n) 1.0d0)) 2.0d0)))
  
(defun display-monthly-payment (debt years-to-pay annual-interest-rate)
  "Displays the monthly payment that one needs in order to pay off a debt of
<debt> in <years-to-pay> years at an interest rate of <annual-interest-rate>.
Compounding is assumed to occur monthly."
  (check-for-plusp (list debt "initial debt"
                         years-to-pay "years to pay"
                         annual-interest-rate "annual interest rate"))
  (let ((*check-for-plus* nil))    
    (format t "Monthly payment = $~$~%" 
            (calc-monthly-payment debt years-to-pay annual-interest-rate))
    (values)))



(defun calc-interest-rate (initial-debt monthly-payment years)
  "Calculates the interest rate at which an initial debt of <initial-debt> will
be paid off in <years> years with monthly payment, <monthly-payment>.
Compounding is assumed to occur monthly."
  (check-for-plusp (list initial-debt "initial debt"
                         monthly-payment "monthly payment"
                         years "years"))
  (when (> (- initial-debt (* monthly-payment years 12.0d0)) 0.0d0)
    (return-from calc-interest-rate 
      (signal (make-condition 'non-solveable 
                :message 
                "Monthly payment is too low to obtain a solution."))))
  (let ((n (* (coerce years 'double-float) 12.0d0))
        (b (coerce monthly-payment 'double-float))
        (x0 (coerce initial-debt 'double-float)))
    (let ((r (/ (+ n (/ x0 b)) (+ n (/ (* x0 x0) (* b b))))))
      (do ((r-last 0.0d0)
           (iter-count 0 (incf iter-count)))
          ((< (abs (- r r-last)) 1.0d-6))
        (when (> iter-count 100)
          (return-from calc-interest-rate
            (signal (make-condition 'non-solveable 
                      :message "Algorithm not converging to a solution."))))
        (setf r-last r)
        (setf r (- r (/ (+ (- (* (expt (1+ r) (1+ n)) 
                                 x0) 
                              (* (expt (1+ r) n) 
                                 (+ x0 b)))
                           b)
                        (- (* (1+ n) 
                              (expt (1+ r) n) 
                              x0) 
                           (* n 
                              (expt (1+ r) (1- n)) 
                              (+ x0 b)))))))
      (if (< (/ (abs (+ (* (- x0 (/ b r)) 
                           (expt (+ 1.0d0 r) n))
                        (/ b r))) b) 1.0d0)
          (* r 1200.0d0)
        (signal (make-condition 'non-solveable 
                  :message "Monthly payment is too low. 
Cannot accurately compute the annual interest rate."))))))


(defun display-interest-rate (initial-debt monthly-payment years)
  "Displays the interest rate at which an initial debt of <initial-debt> will be
paid off in <years> years with monthly payment, <monthly-payment>.
Compounding is assumed to occur monthly."
  (check-for-plusp (list initial-debt "initial debt"
                         monthly-payment "monthly payment"
                         years "years"))
  (handler-case 
      (let ((*check-for-plus* nil))
        (let ((rate (calc-interest-rate initial-debt monthly-payment years)))
          rate))
    (non-solveable (obj)
      (format t "~a~%" (message obj))
      (values))
    (:no-error (rate)
      (format t "Annual Interest Rate = ~3$~%" rate)    
      (values))))
