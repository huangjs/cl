;;; Alternative: The standard approach would use CLOS. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage "GENERATOR"
  (:use "COMMON-LISP")
  (:export "MAKE-LIST-GEN" "MAKE-NUMBER-GEN" "MAKE-STREAM-GEN"
           "EMPTY-GEN-P" "GEN-ELT" "ADVANCE-GEN"
           "MAP-GEN" "EXTRUDE"))

(in-package "GENERATOR")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some generator makers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-number-gen (m &optional n)
  #'(lambda (cmd)
      (ecase cmd
        (:test (and (numberp n) (> m n)))
        (:get m)
        (:next (prog1 m
                    (unless (and (numberp n) (> m n))
                      (incf m)))))))

(defun make-list-gen (l)
  #'(lambda (cmd)
      (ecase cmd
        (:test (null l))
        (:get (car l))
        (:next (pop l)))))

(defun make-stream-gen (stream &key (reader #'read))
  (let* ((eof (list 'eof))
         (exp (funcall reader stream nil eof)))
    #'(lambda (cmd)
        (ecase cmd
          (:test (eq exp eof))
          (:get exp)
          (:next (prog1 exp
                   (setq exp
                         (funcall reader stream nil eof))))))))

#|
Exercise for reader: why doesn't the following work?

(defun make-file-gen (pathname)
  (with-open-file (stream pathname :direction :input)
    (make-stream-gen stream)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Generator Calling Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; In CLOS, these would be methods.

(defun empty-gen-p (gen)
  (funcall gen :test))

(defun gen-elt (gen)
  (funcall gen :get))

(defun advance-gen (gen)
  (funcall gen :next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advanced Generator Calling Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun extrude (n gen)
  (do ((l nil (cons (advance-gen gen) l))
       (i 0 (1+ i)))
      ((or (empty-gen-p gen) (= i n))
       (nreverse l))))

(defun map-gen (fn gen)
  (do ()
      ((empty-gen-p gen) nil)
    (funcall fn (advance-gen gen))))
    
(provide "generators")