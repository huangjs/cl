(in-package :fnwrap)

(defun fact (n)
  (declare (notinline fact))
  (if (= n 1) 1 (* n (fact (1- n)))))


(defwrapper fact (fn n)
  (when (zerop (rem n 5))
    (break "~S is a multiple of 5" n))
  (funcall fn n))

(defwrapper fact (fn n)
  (when (oddp n) 
    (print (list 'fact n)))
  (let ((result (funcall fn n)))
    (when (oddp n)
      (print (list 'result result)))
    result))
      

(defwrapper fact (fn n)
  (trace-call (fact n) (funcall fn n) (oddp n)))

(defwrapper fact (fn n)
  (break-call (fact n) (funcall fn n) (oddp n)))

