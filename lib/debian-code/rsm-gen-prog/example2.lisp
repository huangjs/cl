(in-package :cl-user)


;;; The actual function we are trying to reproduce.
(defun real-func (x)
  (cos (* 2.0 x)))

(defun % (x y)
  (if (= y 0.0)
      0.0
    (/ x y)))


;;; The actual function we are trying to reproduce.
(defparameter *correct-func* #'real-func)

;;; The metric used to determine how far a given program tree and the 
;;; correct solution differ at a point.
(defparameter *distance-metric* #'(lambda (x y) (abs (- x y))))

;;; The norm used on the sum of all the errors(differences from correct to 
;;; program tree).
(defparameter *sum-norm* #'sqrt)


;;; Function and variable building blocks used to form program trees. 
;;; A list of pairs; each pairs represents a 
;;; function and the number of arguments it takes. 
(defparameter *funcs-vars* '((+ . 2) (- . 2) (* . 2) (% . 2) (sin . 1) 
                             (?x . 0) (R . 0)))

;;; Function building blocks used to form program trees. 
;;; A list of pairs; each pairs represents a 
;;; function and the number of arguments it takes. 
(defparameter *funcs* '((+ . 2) (- . 2) (* . 2) (% . 2) (sin . 1)))


;;; The initial terminals (leafs from the initial population).
(defparameter *init-terminals* '(?x R))

;;; The terminals (leafs from populations after the first).
(defparameter *terminals* '(?x))

;;; The variables of the program trees.
(defparameter *vars* '(?x))

;;; The conversion of functions in the program trees.
(defparameter *func-conversions* nil)

;;; The conversion of initial terminals(from the first population).
(defparameter *init-terminal-conversions* `((R . ,#'(lambda () (random 10.0)))))

;;; Points used to compare the gene pool to the real (correct) function.
(defparameter *points* '((0.0) (0.1) (0.2) (0.3) (0.4) (0.5) (0.6) (0.7) (0.8)
                         (0.9) (1.0) (1.1) (1.2) (1.3) (1.4) (1.5) (1.6) (1.7)
                         (1.8) (1.9) (2.0)
                         (2.1) (2.2) (2.3) (2.4) (2.5) (2.6) (2.7) (2.8)
                         (2.9) (3.0) (3.1) (3.2) (3.3) (3.4) (3.5) (3.6) (3.7)
                         (3.8) (3.9) (4.0)))

(defparameter *population-size* 150
  "The number of program trees in each generation.")

(defvar *total-fitness*  0)
(defvar *first-fitness* 0)

(setf *gen-params* (rsm.gen-prog:make-gen-params *population-size*
                                        *vars* *terminals* *init-terminals*
                                        *funcs* *funcs-vars* *correct-func* 
                                        *func-conversions*
                                        *init-terminal-conversions*
                                        *distance-metric* *sum-norm*
                                        *points*))

(setf *total-fitness* (rsm.gen-prog:make-initial-population *gen-params*))
(setf *first-fitness* *total-fitness*)

(loop :for gen :from 1 :below 51 :do
  (setf *total-fitness* (rsm.gen-prog:form-next-generation gen 
                                                  *total-fitness* 
                                                  *gen-params*))
  (format t "Generation ~a:~cTotal fitness = ~a~%" 
          gen 
          #\Tab
          *total-fitness*))
(format t "~%First fitness = ~a~%" *first-fitness*)



;;; Get the best programs, compare the first best function against
;;; the real function.
(let ((programs (rsm.gen-prog:get-best-programs *gen-params*)))
  (format t "best-programs = ~a~%" programs)
  (let* ((first (caadr programs))
         (best-one (rsm.gen-prog::pt-program first))
         (best-fit (rsm.gen-prog::pt-fitness first))
         (correct-sum 0))
    
    
    ;;; Loop over all the points and compare the real function 
    ;;; with the best one we found.
    (format t "COMPARE the BEST PROGRAM TREE with the REAL FUNCTION~%~%")
    (loop :for point :in *points* :do
      (let ((correct (apply #'real-func point))
            (best (apply best-one point)))
        (format t "best = ~a ~c real = ~a~%" 
                best
                #\Tab
                correct)))))


  
