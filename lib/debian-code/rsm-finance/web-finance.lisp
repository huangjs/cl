;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.web-finance -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          web-finance.lisp
;;;; Purpose:       Web Interface For financial Loan Utilities.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Sep 2003
;;;;
;;;; $Id: web-finance.lisp,v 1.1 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

;;;;
;;;; ACCESS THROUGH WEB BROWSER: MAIN WEB PAGE http://localhost/loan
;;;;

(in-package #:cl-user)

(defpackage rsm.web-finance 
  (:use :common-lisp rsm.finance :excl :net.html.generator :net.aserve))

(in-package rsm.web-finance)


(defvar *bad-pos-num* nil)

(defvar *rate* nil)
(defvar *debt* nil)
(defvar *payment* nil)
(defvar *years* nil)

(defconstant +param-strings+ (list "years" "rate" "debt" "payment"))

(defvar *param-hash* (make-hash-table :test #'equal :size 4))


(eval-when (:toplevel-compile :toplevel-load :execute)
  (dolist (param +param-strings+)
    (setf (gethash param *param-hash*) "")))

(defun clear-loan-params ()
  (dolist (param +param-strings+)
    (setf (gethash param *param-hash*) "")))


(defun set-loan-params (assoc-list)
  (dolist (param +param-strings+)
    (let ((pval (cadr (assoc param assoc-list :test #'equal))))
      (when pval
        (setf (gethash param *param-hash*) pval)))))

(defun get-debt ()
  (gethash "debt" *param-hash*))

(defun get-rate ()
  (gethash "rate" *param-hash*))

(defun get-payment ()
  (gethash "payment" *param-hash*))

(defun get-years ()
  (gethash "years" *param-hash*))

(defun check-pos-num (param-string val val-string)
  (unless (and (numberp val) 
               (> val 0))
    (setf *bad-pos-num* t)
    (html
     (:br)
     ((:b :style "color=red")
      (:princ-safe (format nil "Unable to calculate. REASON: 
Parameter, ~s, has value, ~s, which is not a positive number." 
                           param-string val-string))))))


(defun display-monthly-payment-html (initial-debt 
                                     years-to-pay 
                                     annual-interest-rate)
  (let ((debt (read-from-string initial-debt nil nil))
        (years (read-from-string years-to-pay nil nil))
        (rate (read-from-string annual-interest-rate nil nil)))
    
    (let ((*bad-pos-num* nil))
      (check-pos-num "Initial Debt" debt initial-debt)
      (check-pos-num "Years to Pay" years years-to-pay)
      (check-pos-num "Annual Interest Rate" rate annual-interest-rate)
      (when *bad-pos-num*
        (return-from display-monthly-payment-html)))
    (let ((payment 
           (calc-monthly-payment (read-from-string initial-debt) 
                                 (read-from-string years-to-pay)
                                 (read-from-string annual-interest-rate))))
      (set-loan-params (list (list "years" years-to-pay)
                             (list "rate" annual-interest-rate)
                             (list "payment" (format nil "~2$" payment))
                             (list "debt" initial-debt)))
      (html
       (:br)
       (:b
        (:princ-safe
         (format nil "Monthly payment: ")))
       (:princ-safe (format nil "$~2$" payment))))
      (values)))

(defun display-num-payments-html (initial-debt 
                                  annual-interest-rate 
                                  monthly-payment)
  
  (let ((debt (read-from-string initial-debt nil nil))
        (rate (read-from-string annual-interest-rate nil nil))
        (payment (read-from-string monthly-payment nil nil)))
    
    (let ((*bad-pos-num* nil))
      (check-pos-num "Initial Debt" debt initial-debt)
      (check-pos-num "Annual Interest Rate" rate annual-interest-rate)
      (check-pos-num "Monthly Payment" payment monthly-payment)
      (when *bad-pos-num*
        (return-from display-num-payments-html)))
      
    (handler-case 
        (multiple-value-bind (num-payments years months 
                              total-payout last-payment) 
            (calc-num-payments (read-from-string initial-debt)
                               (read-from-string annual-interest-rate)
                               (read-from-string monthly-payment))
          (set-loan-params (list (list "years" (format nil "~3$"
                                                       (/ num-payments 12.0)))
                                 (list "rate" annual-interest-rate)
                                 (list "payment" monthly-payment)
                                 (list "debt" initial-debt)))
          (html
           (:br)
           (:b (:princ-safe "Number of payments: "))
           ((:em :style "color=red") 
            (:princ-safe (format nil "~a" num-payments))))
          (if (> years 0)
              (if (= years 1)
                  (html
                   (:br)
                   (:princ-safe (format nil "That's 1 year")))
                (html
                 (:br)
                 (:princ-safe (format nil "That's ~a years" years)))))
          (if (> months 0)
              (if (= months 1)
                  (html
                   (:princ-safe " and 1 month"))
                (html
                 (:princ-safe (format nil " and ~a months." months)))))
          (html
           (:br)
           (:princ-safe (format nil "Total payout: $~2$" total-payout)))
          (when (> last-payment 0.0)
            (html
             (:br)
             (:princ-safe (format nil "Last Payment: $~2$" last-payment)))) 
          (values))
      (non-solveable (obj) 
        (html
         (:br)
         ((:b :style "color=red")
          (:princ-safe (format nil "~a" obj)))))
      (:no-error () (values)))))


(defun display-initial-debt-html (years-to-pay 
                                  annual-interest-rate 
                                  monthly-payment)
  (let ((years (read-from-string years-to-pay nil nil))
        (rate (read-from-string annual-interest-rate nil nil))
        (payment (read-from-string monthly-payment nil nil)))
    
    (let ((*bad-pos-num* nil))
      (check-pos-num "Years to Pay" years years-to-pay)
      (check-pos-num "Annual Interest Rate" rate annual-interest-rate)
      (check-pos-num "Monthly Payment" payment monthly-payment)
      (when *bad-pos-num*
        (return-from display-initial-debt-html)))
    (html
     (:br)
     (:b 
      (:princ-safe
       (format nil "Initial debt: ")))
     (let ((debt (format nil "~$~%" 
                          (calc-initial-debt 
                           (read-from-string years-to-pay)
                           (read-from-string annual-interest-rate)
                           (read-from-string monthly-payment)))))
       (html
        (:princ-safe (format nil "$~$" debt)))
       (set-loan-params (list (list "years" years-to-pay)
                              (list "rate" annual-interest-rate)
                              (list "payment" monthly-payment)
                              (list "debt" (format nil "~2$" debt)))))
     (values))))


  
(defun display-annual-rate-html (initial-debt
                                 monthly-payment
                                 years-to-pay)
  (let ((debt (read-from-string initial-debt nil nil))
        (payment (read-from-string monthly-payment nil nil))
        (years (read-from-string years-to-pay nil nil)))

    (let ((*bad-pos-num* nil))
      (check-pos-num "Initial Debt" debt initial-debt)
      (check-pos-num "Monthly Payment" payment monthly-payment)
      (check-pos-num "Years to Pay" years years-to-pay)
      (when *bad-pos-num*
        (return-from display-annual-rate-html)))
    
    (handler-case
        (let ((rate 
               (calc-interest-rate
                               (read-from-string initial-debt)
                               (read-from-string monthly-payment)
                               (read-from-string years-to-pay))))
          (set-loan-params (list (list "years" years-to-pay)
                                 (list "rate" (format nil "~3$" rate))
                                 (list "payment" monthly-payment)
                                 (list "debt" initial-debt)))
          (html
           (:br)
           (:b 
            (:princ-safe
             (format nil "Annual Interest Rate: ")))
           (:princ-safe (format nil "~3$" rate))))
      (non-solveable (obj) 
        (html
         (:br)
         ((:b :style "color=red")
          (:princ-safe (format nil "~a" obj)))))
      (:no-error ()))))


(publish :path "/loan"
         :content-type "text/html"
         :function
         #'(lambda (req ent)
             (clear-loan-params)
             (with-http-response (req ent)
               (with-http-body (req ent)
                 (html
                  (:html
                   (:head (:title "Financial Loan Utilities"))
                   ((:body :background "c:/shared/lisp/serv/background.jpg")
                    (:center (:h1 "Financial Loan Utilities"))
                    (:h2 "Loan Operations")
                    (:p 
                     (:b "Choose from one of the following:"))
                    (:ul
                     (:li ((:a :href "/calcnpay") 
                           "Calculate Number of Payments"))
                     (:li ((:a :href "/calcmpay") 
                           "Calculate Monthly Payment"))
                     (:li ((:a :href "/calcdebt")
                           "Calculate Initial Debt"))
                     (:li ((:a :href "/calcrate")
                           "Calculate Annual Interest Rate"))))))))))

(publish :path "/calcnpay"
         :content-type "text/html"
         :function
         #'(lambda (req ent)
             (let* ((body (get-request-body req))
                    (query (form-urlencoded-to-query body))
                    (debt (cdr (assoc "debt" query :test #'equal)))
                    (rate (cdr (assoc "rate" query :test #'equal)))
                    (payment (cdr (assoc "payment" query :test #'equal))))
               (with-http-response (req ent)
                 (with-http-body (req ent)
                   (if (or debt rate payment)
                       (html 
                        (:html
                         (:head (:title "Financial Loan Utilities"))
                         ((:body :background 
                                 "c:/shared/lisp/serv/background.jpg")
                          (:center (:h1 "Financial Loan Utilities"))
                          (:h2 "Calculate Number of Payments")
                          ((:form :action "/calcnpay"
                                  :method "post")
                           ((:table :cols "2")
                            (:tr
                             (:th "Description")
                             (:th "Values"))
                            (:tr
                             (:td "Initial Debt ")
                             (:td
                              ((:input :type "text" :size "18" 
                                       :name "debt" 
                                       :value (format nil "~a" debt)))))
                            (:tr
                             (:td "Annual Interest Rate ")
                             (:td 
                              ((:input :type "text" :size "18" 
                                       :name "rate"
                                       :value (format nil "~a" rate)))))
                            (:tr
                             (:td "Monthly Payment ")
                             (:td
                              ((:input :type "text" :size "18" 
                                       :name "payment"
                                       :value (format nil "~a" payment)))))
                            (:tr
                             ((:td :colspan 2)
                              (:center
                               ((:input :type "submit")))))))
                          (:br)
                          ((:a :href "/loan") "Main Loan Utility Page")
                          (:br)
                          (:br)
                          ((:a :href "/calcmpay") "Calculate Monthly Payment")
                          (:br)
                          (:br)
                          ((:a :href "/calcdebt") "Calculate Initial Debt")
                          (:br)
                          (:br)
                          ((:a :href "/calcrate") 
                           "Calculate Annual Interest Rate")
                          (:br)
                          (:br)
                          (display-num-payments-html debt rate payment))))
                     (html 
                      (:html
                       (:head (:title "Financial Loan Utilities"))
                       ((:body :background "c:/shared/lisp/serv/background.jpg")
                        (:center (:h1 "Financial Loan Utilities"))
                        (:h2 "Calculate Number of Payments")
                        ((:form :action "/calcnpay"
                                :method "post")
                         ((:table :cols "2")
                          (:tr
                           (:th "Description")
                           (:th "Values"))
                          (:tr
                           (:td "Initial Debt ")
                           (:td
                            ((:input :type "text" :size "18" 
                                     :name "debt"
                                     :value (get-debt)))))
                          (:tr
                           (:td "Annual Interest Rate ")
                           (:td 
                            ((:input :type "text" :size "18" 
                                     :name "rate"
                                     :value (get-rate)))))
                          (:tr
                           (:td "Monthly Payment ")
                           (:td
                            ((:input :type "text" :size "18" 
                                     :name "payment"
                                     :value (get-payment)))))
                          (:tr
                           ((:td :colspan 2)
                            (:center
                             ((:input :type "submit")))))))
                        (:br)
                        ((:a :href "/loan") "Main Loan Utility Page")
                        (:br)
                        (:br)
                        ((:a :href "/calcmpay") "Calculate Monthly Payment")
                        (:br)
                        (:br)
                        ((:a :href "/calcdebt") "Calculate Initial Debt")
                        (:br)
                        (:br)
                        ((:a :href "/calcrate") 
                         "Calculate Annual Interest Rate"))))))))))


(publish :path "/calcmpay"
         :content-type "text/html"
         :function
         #'(lambda (req ent)
             (let* ((body (get-request-body req))
                    (query (form-urlencoded-to-query body))
                    (debt (cdr (assoc "debt" query :test #'equal)))
                    (years (cdr (assoc "years" query :test #'equal)))
                    (rate (cdr (assoc "rate" query :test #'equal))))
               (with-http-response (req ent)
                 (with-http-body (req ent)
                   (if (or debt years rate)
                       (html 
                        (:html
                         (:head (:title "Financial Loan Utilities"))
                         ((:body :background 
                                 "c:/shared/lisp/serv/background.jpg")
                          (:center (:h1 "Financial Loan Utilities"))
                          (:h2 "Calculate Monthly Payment")
                          ((:form :action "/calcmpay"
                                  :method "post")
                           ((:table :cols "2")
                            (:tr
                             (:th "Description")
                             (:th "Values"))
                            (:tr
                             (:td "Initial Debt ")
                             (:td
                              ((:input :type "text" :size "18" 
                                       :name "debt" 
                                       :value (format nil "~a" debt)))))
                            (:tr
                             (:td "Years to Pay ")
                             (:td
                              ((:input :type "text" :size "18" 
                                       :name "years"
                                       :value (format nil "~a" years)))))
                            (:tr
                             (:td "Annual Interest Rate ")
                             (:td 
                              ((:input :type "text" :size "18" 
                                       :name "rate"
                                       :value (format nil "~a" rate)))))
                            (:tr
                             ((:td :colspan 2)
                              (:center
                               ((:input :type "submit")))))))
                          (:br)
                          ((:a :href "/loan") "Main Loan Utility Page")
                          (:br)
                          (:br)
                          ((:a :href "/calcnpay") 
                           "Calculate Number of Payments")
                          (:br)
                          (:br)
                          ((:a :href "/calcdebt") "Calculate Initial Debt")
                          (:br)
                          (:br)
                          ((:a :href "/calcrate") 
                           "Calculate Annual Interest Rate")
                          (:br)
                          (:br)
                          (display-monthly-payment-html debt years rate))))
                     (html 
                      (:html
                       (:head (:title "Financial Loan Utilities"))
                       ((:body :background "c:/shared/lisp/serv/background.jpg")
                        (:center (:h1 "Financial Loan Utilities"))
                        (:h2 "Calculate Monthly Payment")
                        ((:form :action "/calcmpay"
                                :method "post")
                         ((:table :cols "2")
                          (:tr
                           (:th "Description")
                           (:th "Values"))
                          (:tr
                           (:td "Initial Debt ")
                           (:td
                            ((:input :type "text" :size "18" 
                                     :name "debt"
                                     :value (get-debt)))))
                          (:tr
                           (:td "Years to Pay ")
                           (:td 
                            ((:input :type "text" :size "18" 
                                     :name "years"
                                     :value (get-years)))))
                          (:tr
                           (:td "Annual Interest Rate ")
                           (:td
                            ((:input :type "text" :size "18" 
                                     :name "rate"
                                     :value (get-rate)))))
                          (:tr
                           ((:td :colspan 2)
                            (:center
                             ((:input :type "submit")))))))
                        (:br)
                        ((:a :href "/loan") "Main Loan Utility Page")
                        (:br)
                        (:br)                        
                        ((:a :href "/calcnpay") "Calculate Number of Payments")
                        (:br)
                        (:br)
                        ((:a :href "/calcdebt") "Calculate Initial Debt")
                        (:br)
                        (:br)
                        ((:a :href "/calcrate") 
                         "Calculate Annual Interest Rate"))))))))))


(publish :path "/calcdebt"
         :content-type "text/html"
         :function
         #'(lambda (req ent)
             (let* ((body (get-request-body req))
                    (query (form-urlencoded-to-query body))
                    (years (cdr (assoc "years" query :test #'equal)))
                    (rate (cdr (assoc "rate" query :test #'equal)))
                    (payment (cdr (assoc "payment" query :test #'equal))))
               (with-http-response (req ent)
                 (with-http-body (req ent)
                   (if (or years rate payment)
                       (html 
                        (:html
                         (:head (:title "Financial Loan Utilities"))
                         ((:body :background 
                                 "c:/shared/lisp/serv/background.jpg")
                          (:center (:h1 "Financial Loan Utilities"))
                          (:h2 "Calculate Initial Debt")
                          ((:form :action "/calcdebt"
                                  :method "post")
                           ((:table :cols "2")
                            (:tr
                             (:th "Description")
                             (:th "Values"))
                            (:tr
                             (:td "Years to Pay ")
                             (:td
                              ((:input :type "text" :size "18" 
                                       :name "years" 
                                       :value (format nil "~a" years)))))
                            (:tr
                             (:td "Annual Interest Rate ")
                             (:td 
                              ((:input :type "text" :size "18" 
                                       :name "rate"
                                       :value (format nil "~a" rate)))))
                            (:tr
                             (:td "Monthly Payment ")
                             (:td
                              ((:input :type "text" :size "18" 
                                       :name "payment"
                                       :value (format nil "~a" payment)))))
                            
                            (:tr
                             ((:td :colspan 2)
                              (:center
                               ((:input :type "submit")))))))
                          (:br)
                          ((:a :href "/loan") "Main Loan Utility Page")
                          (:br)
                          (:br)
                          ((:a :href "/calcnpay") 
                           "Calculate Number of Payments")
                          (:br)
                          (:br)
                          ((:a :href "/calcmpay") "Calculate Monthly Payment")
                          (:br)
                          (:br)
                          ((:a :href "/calcrate") 
                           "Calculate Annual Interest Rate")
                          (:br)
                          (:br)
                           (display-initial-debt-html years rate payment))))
                     (html 
                      (:html
                       (:head (:title "Financial Loan Utilities"))
                       ((:body :background "c:/shared/lisp/serv/background.jpg")
                        (:center (:h1 "Financial Loan Utilities"))
                        (:h2 "Calculate Initial Debt")
                        ((:form :action "/calcdebt"
                                :method "post")
                         ((:table :cols "2")
                          (:tr
                           (:th "Description")
                           (:th "Values"))
                          (:tr
                           (:td "Year to Pay ")
                           (:td
                            ((:input :type "text" :size "18" 
                                     :name "years"
                                     :value (get-years)))))
                          (:tr
                           (:td "Annual Interest Rate ")
                           (:td
                            ((:input :type "text" :size "18" 
                                     :name "rate"
                                     :value (get-rate)))))
                          (:tr
                           (:td "Monthly Payment ")
                           (:td 
                            ((:input :type "text" :size "18" 
                                     :name "payment"
                                     :value (get-payment)))))
                          
                          (:tr
                           ((:td :colspan 2)
                            (:center
                             ((:input :type "submit")))))))
                        (:br)
                        ((:a :href "/loan") "Main Loan Utility Page")
                        (:br)
                        (:br)
                        ((:a :href "/calcnpay") "Calculate Number of Payments")
                        (:br)
                        (:br)
                        ((:a :href "/calcmpay") "Calculate Monthly Payment")
                        (:br)
                        (:br)
                        ((:a :href "/calcrate") 
                         "Calculate Annual Interest Rate"))))))))))


(publish :path "/calcrate"
         :content-type "text/html"
         :function
         #'(lambda (req ent)
             (let* ((body (get-request-body req))
                    (query (form-urlencoded-to-query body))
                    (debt (cdr (assoc "debt" query :test #'equal)))
                    (payment (cdr (assoc "payment" query :test #'equal)))
                    (years (cdr (assoc "years" query :test #'equal))))
               (with-http-response (req ent)
                 (with-http-body (req ent)
                   (if (or debt payment years)
                       (html 
                        (:html
                         (:head (:title "Financial Loan Utilities"))
                         ((:body :background 
                                 "c:/shared/lisp/serv/background.jpg")
                          (:center (:h1 "Financial Loan Utilities"))
                          (:h2 "Calculate Annual Interest Rate")
                          ((:form :action "/calcrate"
                                  :method "post")
                           ((:table :cols "2")
                            (:tr
                             (:th "Description")
                             (:th "Values"))
                            (:tr
                             (:td "Initial Debt ")
                             (:td
                              ((:input :type "text" :size "18" 
                                       :name "debt" 
                                       :value (format nil "~a" debt)))))
                            (:tr
                             (:td "Monthly Payment ")
                             (:td 
                              ((:input :type "text" :size "18" 
                                       :name "payment"
                                       :value (format nil "~a" payment)))))
                            (:tr
                             (:td "Years to Pay ")
                             (:td
                              ((:input :type "text" :size "18" 
                                       :name "years"
                                       :value (format nil "~a" years)))))
                            (:tr
                             ((:td :colspan 2)
                              (:center
                               ((:input :type "submit")))))))
                          (:br)
                          ((:a :href "/loan") "Main Loan Utility Page")
                          (:br)
                          (:br)
                          ((:a :href "/calcnpay") 
                           "Calculate Number of Payments")
                          (:br)
                          (:br)
                          ((:a :href "/calcmpay") "Calculate Monthly Payment")
                          (:br)
                          (:br)
                          ((:a :href "/calcdebt") "Calculate Initial Debt")
                          (:br)
                          (:br)
                          (display-annual-rate-html debt payment years))))
                     (html 
                      (:html
                       (:head (:title "Financial Loan Utilities"))
                       ((:body :background "c:/shared/lisp/serv/background.jpg")
                        (:center (:h1 "Financial Loan Utilities"))
                        (:h2 "Calculate Annual Interest Rate")
                        ((:form :action "/calcrate"
                                :method "post")
                         ((:table :cols "2")
                          (:tr
                           (:th "Description")
                           (:th "Values"))
                          (:tr
                           (:td "Initial Debt ")
                           (:td
                            ((:input :type "text" :size "18" 
                                     :name "debt"
                                     :value (get-debt)))))
                          (:tr
                           (:td "Monthly Payment ")
                           (:td
                            ((:input :type "text" :size "18" 
                                     :name "payment"
                                     :value (get-payment)))))
                          (:tr
                           (:td "Years to Pay ")
                           (:td 
                            ((:input :type "text" :size "18" 
                                     :name "years"
                                     :value (get-years)))))
                          (:tr
                           ((:td :colspan 2)
                            (:center
                             ((:input :type "submit")))))))
                        (:br)
                        ((:a :href "/loan") "Main Loan Utility Page")
                        (:br)
                        (:br)
                        ((:a :href "/calcnpay") "Calculate Number of Payments")
                        (:br)
                        (:br)
                        ((:a :href "/calcmpay") "Calculate Monthly Payment")
                        (:br)
                        (:br)
                        ((:a :href "/calcdebt") 
                         "Calculate Initial Debt"))))))))))

