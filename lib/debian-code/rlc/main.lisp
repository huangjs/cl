;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10; Package: rlc -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rlc.lisp
;;;; Purpose:       RLC Functions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Jan 2003
;;;;
;;;; $Id: kboot.lisp 8414 2003-12-28 19:46:57Z kevin $
;;;; *************************************************************************

(in-package #:rlc)

(defun plot-series-rlc-current (v r l c t-inc t-end &optional (t-start 0)
                                (graph-function 'run-xgraph))
  (let ((path (make-pathname
               :directory '(:absolute "tmp")
               :name
               (concatenate 'string "rlc-data-"
                            (write-to-string (get-universal-time)))
               :type "dat")))
    (write-series-rlc-current-graph path v r l c t-inc t-end t-start)
    (funcall graph-function path)
    (sleep 2)
    (delete-file path)))

(defun run-xgraph (path)
  (kl::run-shell-command "xgraph ~A" (namestring path)))

(defun write-series-rlc-current-graph (path v r l c t-inc t-end
                                       &optional (t-start 0))
  (with-open-file (out path :direction :output :if-exists :supersede
                   :if-does-not-exist :create)
    (write-series-rlc-current-graph-stream out v r l c t-inc t-end t-start)))

(defun write-series-rlc-current-graph-stream (out v r l c t-inc t-end
                                       &optional (t-start 0))
  (multiple-value-bind (x y)
      (series-rlc-current-graph-data v r l c t-inc t-end t-start)
    (dotimes (i (length x))
      (format out "~D ~D~%" (aref x i) (aref y i)))))

(defun series-rlc-current-graph-data (v r l c t-inc t-end &optional (t-start 0))
  (let* ((formula-list (series-rlc-current-formula v r l c))
         (formula-eval (eval formula-list))
         (formula (compile nil formula-eval))
         (n (ceiling (- t-end t-start) t-inc)))
    (do ((i 0 (1+ i))
         (tm t-start (+ tm t-inc))
         (x-pts (make-array n))
         (y-pts (make-array n)))
        ((= i n)
         (values x-pts y-pts))
      (setf (aref x-pts i) tm)
      (setf (aref y-pts i) (funcall formula tm)))))

(defun series-rlc-current-time (v r l c tm)
  (let* ((formula-list (series-rlc-current-formula v r l c))
         (formula (eval formula-list)))
    (funcall formula tm)))

(defun circuit-type (r l c)
   (cond
    ((and (zerop r) (zerop l) (zerop c))
     :null)
    ((and (/= 0 r) (zerop l) (zerop c))
     :r)
    ((and (zerop r) (/= 0 l) (zerop c))
     :l)
    ((and (zerop r) (zerop l) (/= 0 c))
     :c)
    ((and (/= 0 r) (/= 0 l) (zerop c))
     :rl)
    ((and (/= 0 r) (zerop l) (/= 0 c))
     :rc)
    ((and (zerop r) (/= 0 l) (/= 0 c))
     :lc)
    (t
     :rlc)))

(defun series-rlc-current-formula (v r l c)
  "Returns formula for currrent through a series RLC circuit with a step-voltage applied at time 0."
  (ecase (circuit-type r l c)
    (:null
     `(lambda (tm) (declare (ignore tm)) ,v))
    (:r
     `(lambda (tm) (declare (ignore tm)) ,(/ v r)))
    (:c
     `(lambda (tm)
        (if (zerop tm)
            ,(* v c)
          0)))
    (:l
     `(lambda (tm)
        (* ,(/ v l) tm)))
    (:rl
      `(lambda (tm)
         (* ,(/ v r) (- 1 (exp (- (* tm ,(/ r l))))))))
    (:rc
     `(lambda (tm) (* ,(/ v r) (exp (- (* tm ,(/ 1 r c)))))))
     (:lc
      (let ((lc-root (sqrt (* l c))))
        `(lambda (tm)
           (* ,(/ (* v lc-root) l) (sin (* tm ,(/ 1 lc-root)))))))
     (:rlc
      (let* ((r/2l (when (/= 0 l) (/ r (+ l l))))
             (rr/4ll (when r/2l (* r/2l r/2l)))
             (v/l (when (/= 0 l) (/ v l)))
             (1/lc (when (and (/= 0 l) (/= 0 c)) (/ 1 (* l c)))))

        (cond
         ;; RLC over-damped
         ((> rr/4ll 1/lc)
          (let* ((root (sqrt (- rr/4ll 1/lc)))
                 (p1 (+ (- r/2l) root))
                 (p2 (- (- r/2l) root)))
            `(lambda (tm)
               (* ,(/ v/l (- p1 p2))
                  (- (exp (* ,p1 tm)) (exp (* ,p2 tm)))))))
         ;; RLC critcally-damped
         ((= rr/4ll 1/lc)
          `(lambda (tm)
             (* tm
                ,v/l
                (exp (- (* tm ,r/2l))))))
         ;; RLC under-damped
         (t
          (let ((diff (- 1/lc rr/4ll)))
            `(lambda (tm)
               (* ,(/ v/l (sqrt diff))
                  (exp (- (* tm ,r/2l)))
                  (sin (* tm ,(sqrt diff))))))))))))



