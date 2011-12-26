(in-package #:cl-user)


(defun xor (x y)
  (not (eql x y)))


;;; The actual function we are trying to reproduce.
(defun real-func (x0 x1 x2 x3 x4 x5)
  (or (and x1 (not x2)) (not x3) (xor x4 (not x5))))


;;; The actual function we are trying to reproduce.
(defparameter *correct-func* #'real-func)

;;; The metric used to determine how far a given program tree and the 
;;; correct solution differ at a point.
(defparameter *distance-metric* #'(lambda (x y) (if (eql x y) 0 1)))

;;; The norm used on the sum of all the errors(differences from correct to 
;;; program tree).
(defparameter *sum-norm* #'sqrt)

;;; Function and variable building blocks used to form program trees. 
;;; A list of pairs; each pairs represents a 
;;; function and the number of arguments it takes. 
(defparameter *funcs-vars* '((and . 2) (or . 2) (not . 1) 
                             (?x0 . 0) (?x1 . 0) (?x2 . 0)
                             (?x3 . 0) (?x4 . 0) (?x5 . 0)))

;;; Function building blocks used to form program trees. 
;;; A list of pairs; each pairs represents a 
;;; function and the number of arguments it takes. 
(defparameter *funcs* '((and . 2) (or . 2) (not . 1)))


;;; The initial terminals (leafs from the initial population).
(defparameter *init-terminals* '(?x0 ?x1 ?x2 ?x3 ?x4 ?x5))

;;; The terminals (leafs from populations after the first).
(defparameter *terminals* '(?x0 ?x1 ?x2 ?x3 ?x4 ?x5))

;;; The variables of the program trees.
(defparameter *vars* '(?x0 ?x1 ?x2 ?x3 ?x4 ?x5))

;;; The conversion of functions in the program trees.
(defparameter *func-conversion* nil)

;;; The conversion of initial terminals(from the first population).
(defparameter *init-terminal-conversions* nil)

;;; Points used to compare the gene pool to the real (correct) function.
(defparameter *points* '((t t t t t t) (t nil t t t t) (t t nil t t t)
                         (t t t nil t t) (t t t t nil t) (t t t t t nil)
                         (t t nil t nil t) (t nil t nil t nil)
                         (t t t nil nil t) (t t t t nil nil) (t t nil nil t t)
                         (t nil t t t nil) (t nil nil t nil nil)
                         (t nil nil t t nil)
                         (nil t t t t t) (nil nil t t t t) (nil t nil t nil t)
                         (nil t t nil t nil) (nil nil t t nil t) 
                         (nil t t t nil nil)
                         (nil t nil t nil t) (nil nil t nil t nil)
                         (nil t t nil nil t) (nil t nil t nil nil) 
                         (nil t nil nil t t)
                         (nil nil t t t nil) (nil nil nil t nil nil)
                         (nil nil nil t t nil)))

(defparameter *population-size* 150
  "The number of program trees in each generation.")

;;; Used below.
(defvar *total-fitness* 0)




;;;; MAIN PROGRAM

;;; Bundle up all the genetic programming parameters.
(setf *gen-params* (rsm.gen-prog:make-gen-params *population-size* 
                                        *vars* *terminals* *init-terminals*
                                        *funcs* *funcs-vars* *correct-func* 
                                        *func-conversion* 
                                        *init-terminal-conversions* 
                                        *distance-metric* *sum-norm*
                                        *points*))


;;; Make the initial population - get the fitness of the initial population.
(setf *total-fitness* (rsm.gen-prog:make-initial-population *gen-params*))

;;(rsm.gen-prog:examin-population *gen-params*)

;;; Evolve 50 generations.
(loop :for gen :from 1 :below 51 :do
  (setf *total-fitness* (rsm.gen-prog:form-next-generation gen 
                                                  *total-fitness* 
                                                  *gen-params*))
  (format t "Generation ~a:~cTotal fitness = ~a~%" 
          gen 
          #\Tab
          *total-fitness*))
(format t "~%")



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
        (when (eql correct best)
          (incf correct-sum))
        (format t "best = ~a~creal = ~a~%" 
                best
                #\Tab
                correct
                )))
    (format t "~%Correct = ~a ~c Total = ~a~%" 
            correct-sum #\Tab (length *points*))
    (format t "Best fitness = ~s~%" best-fit)))
  
