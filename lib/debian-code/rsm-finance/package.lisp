;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for rsm.finance.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.2 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)


(defpackage rsm.finance
  (:use #:cl)
  (:documentation
   "Functions to compute loan financing. Loans are assumed to compound monthly.

Export Summary:

calc-num-payments    : Calculate the number of payments to make for a loan 
                       given the initial debt; the annual interest rate; 
                       and the monthly payment.

display-num-payments : Display the number of payments to make for a loan.
                       (uses calc-num-payments)


calc-monthly-payment   : Calculate the monthly payments for a loan 
                         given initial debt; years to pay; and the 
                         annual interest rate.

display-monthly-payment: Display the monthly payment for a loan.
                         (uses calc-monthly-payment)


calc-initial-debt    : Calculate the initial debt given: years to pay;
                       the annual interest rate; and the monthly payment.

display-initial-debt : Display the initial debt of a loan.
                       (uses calc-initial-debt)


calc-interest-rate   : Calculate the annual interest rate given: 
                       the years to pay; the annual interest rate; 
                       and the monthly payment.

display-interest-rate: Display the interest rate of a loan.
                       (uses calc-initial-debt)

non-solvable: A Condition that represents an inablity to solve 
              a financial problem. This may be because the problem
              is impossible, or that the solution is numerically unstable.
")
  (:export
   #:calc-num-payments
   #:display-num-payments
   #:calc-initial-debt
   #:display-initial-debt
   #:calc-monthly-payment
   #:display-monthly-payment
   #:calc-interest-rate
   #:display-interest-rate
   #:non-solveable))
             
