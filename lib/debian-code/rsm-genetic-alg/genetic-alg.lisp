;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          genetic-alg.lisp
;;;; Purpose:       Genetic Algorithms.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: genetic-alg.lisp,v 1.4 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.genetic-alg)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;;;;
;;;; Parameters
;;;;


(defparameter *genetic-problem-hash* (make-hash-table :test #'equalp)
  "Hash table to store genetic problems by their names.")


(defparameter *cache-size* 10
  "Default cache size of the gene cache.")


(defparameter *base-fitness-value* 0
  "Fitness functions should have values that are >= to this value.")


;;; Gene cache.
(defparameter *gene-cache* (rsm.cache:make-standard-cache 
                      "Gene cache" 10 :cache-list-limit 10)
  "A gene cache of best genes that have occurred through out the 
simulated evolution.")

;;;;
;;;; Utility Functions.
;;;;

(defun bin-search (vec-obj val obj-acc val-acc)
  "Perform a binary search to find the first object in a vector of objects
<vec-obj> (which are ordered in an increasing fashion based on the
value of the objects) that satisfies <val> <= obj-val. If val is greater
than all object values, the last object is returned. Likewise, if 
val is less than all objects the first object is returned."
  (let* ((n0 0)
         (len (length vec-obj))
         (n1 (1- len))
         mid)
    (let ((idx
           (loop 
             :repeat len :do
             (when (= (1+ n0) n1)
               (return
                 (if (<= val (funcall val-acc (aref vec-obj n0)))
                     n0
                   n1)))
             (setf mid (truncate (+ n0 n1) 2))
             (if (<= val (funcall val-acc (aref vec-obj mid)))
                 (setf n1 mid)
               (setf n0 mid)))))
      (funcall obj-acc (aref vec-obj idx)))))



(defun listify (n val)
"listifies a value, <val>, if not already a list. That is, if <val> is a 
scalar return a list with <val> repeated <n> times. If <val> is a vector, 
convert it to a list. If the length of <val> is less than n, extend it
to length n cyclically repeating the values from <val>. If the length
of <val> is greater than <n>, just use the values from <val> up to <n>."
  (let ((que (rsm.queue:create)))
    (if (or (vectorp val) (listp val))
        (let ((l-val (copy-list (coerce val 'list))))
          (cond ((< (length val) n)
                 (setf que (rsm.queue:list->queue l-val))
                 (setf (cdr (last l-val)) l-val)
                 (do ((cursor l-val (cdr cursor))
                      (count (- n (length val)) (decf count)))
                     ((= count 0))
                   (rsm.queue:enqueue (car cursor) que)))
                ((= (length val) n)
                 (setf que (rsm.queue:list->queue l-val)))
                (t                      ; (length val) > n
                 (loop :for elem :in l-val 
                   :as count :from 1 :to n :do
                             (rsm.queue:enqueue elem que)))))
      (loop :repeat n :do
        (rsm.queue:enqueue val que)))
    (rsm.queue:nget-list que)))



;;;;
;;;; Representation Layer. (This section defines structures and functions
;;;;                        that manipulate genes.)
;;;;


;;; Structure which defines the genetic parameters needed to 
;;; run a genetic simulation.
(defstruct (genetic 
            (:conc-name g-))
  name
  (mutation-rate 5)
  fitness-function
  alphabet
  pool
  pool-length
  initial-gene-length)


(defun make-vec-pool (gene-pool-list)
  "Take a list of genes represented as lists and return a list of 
genes represented as vectors."
    (mapcar #'(lambda (gene)
                (coerce gene 'vector))
            gene-pool-list))



(defmacro defgenetic (name &key
                           (mutation-rate 5)
                           fitness-function 
                           alphabet 
                           pool)
  "Define a genetic problem (store it by its name in a hash.)"
  `(progn
     
     ;; Make sure the user has provided the key words fitness-function,
     ;; alphabet, and pool.
     (unless ,fitness-function
       (error "defgenetic: No fitness function provided for 
genetic problem ~s." ,(symbol-name name)))
     (unless ,alphabet
       (error "defgenetic: No alphabet provided for genetic problem ~s." 
              ,(symbol-name name)))
     (unless ,pool
       (error "defgenetic: No initial gene pool provided for 
genetic problem ~s." ,(symbol-name name)))

     (unless ,(if (eql (car pool) 'quote)
                  (apply #'= (mapcar #'length (cadr pool)))
                (apply #'= (mapcar #'length pool)))
       (error "defgenetic: Gene pool has different length lists."))
     
     (unless (functionp ,fitness-function)
       (error "defgenetic: Fitness-function value is not a function."))

     (unless (or (listp ,alphabet) (vectorp ,alphabet))
       (error "defgenetic: Alphabet value is not a sequence."))

     (unless (and (numberp ,mutation-rate)
                  (<= ,mutation-rate 100)
                  (>= ,mutation-rate 0))
       (error "defgenetic: Mutation rate (representing percent) is not 
a number in the range [0,100])."))
     
     (let ((pl ',(if (eql (car pool) 'quote)
                    (make-vec-pool (cadr pool))
                  (make-vec-pool pool)))
           (alpha ,(cond ((and (listp alphabet) 
                               (eql (car alphabet) 'quote))
                          (coerce (copy-seq (cadr alphabet)) 'vector))
                         ((listp alphabet)
                          (coerce (copy-seq alphabet) 'vector))
                         (t
                          (copy-seq alphabet)))))
       (handler-case 
           (loop for gene in pl do
                 (funcall ,fitness-function gene))
         (serious-condition ()
           (error "defgenetic: There is an error with 
        either the fitness function or one of the genes of the gene pool in 
        the genetic problem ~s~%." ,(symbol-name name))))
       
       (setf (gethash ,(symbol-name name) *genetic-problem-hash*)
         (make-genetic :name ,(symbol-name name)
                       :mutation-rate (coerce ,mutation-rate 'single-float)
                       :fitness-function ,fitness-function
                       :alphabet alpha
                       :pool pl
                       :pool-length (length pl)
                       :initial-gene-length (length (car pl)))))))



(defun splice (g1 g2)
  "Returns a gene formed by concatenating two genes together."
  (let ((new-g (make-array (+ (gene-length g1) (gene-length g2)) 
                           :adjustable t)))
    (loop for g-val across g1 
        as i from 0 do
          (setf (aref new-g i) g-val))
    
    (loop for g-val across g2 
        as i from (gene-length g1) do
          (setf (aref new-g i) g-val))
    new-g))



(declaim (inline gene-length))
(defun gene-length (gene)
  "Get the length of a gene."
  (length gene))

(defun split-gene (gene point)
  "Splits a gene at point <point> (zero based index assumed) and returns
two pieces."
  (let ((g1 (make-array point :adjustable t))
        (g2 (make-array (- (gene-length gene) point) :adjustable t)))
    (loop for i from 0 below (gene-length g1) 
        as g-val across gene do
          (setf (aref g1 i) g-val))
    (loop for i from 0 below (gene-length g2) 
        as g-val across gene do
          (setf (aref g2 i) g-val))
    (values g1 g2)))


(defun pick-random-alpha (gene-alphabet)
  "Randomly (uniformly) picks a letter from the gene-alphabet 
<gene-alphabet>."
  (let ((rnd (random (length gene-alphabet))))
    (svref gene-alphabet rnd)))


(defun mutate-gene (gene mutation-rate 
                    gene-alphabet 
                    gene-length-constant?)
  "Returns a possibly mutated gene from <gene> given a mutation rate, 
<mutation-rate>. If <gene-length-constant?> is true, then the 
mutated gene will be the same length as <gene>; otherwise, its 
length may vary."
  (let ((rnd (random 100)))
    
    ;; If true we mutate and return a new gene.
    (if (and (> (gene-length gene) 0) (< rnd mutation-rate))
        (let ((mutate-point (random (gene-length gene))))
          
          ;; If gene length is to remain constant, pick a letter 
          ;; uniformly at random and add it to the new gene.
          (if gene-length-constant?
              (progn
                (setf (aref gene mutate-point) 
                  (pick-random-alpha gene-alphabet))
                gene)
            (let ((op (random 3))
                  (len (gene-length gene)))
              (if (= op 2)
                  ;; Just add a uniformly randomly selected letter. This
                  ;; effectively replaces the current letter.
                  (progn
                    (setf (aref gene mutate-point) 
                      (pick-random-alpha gene-alphabet))
                    gene)
                ;; Otherwise, add or delete a letter...
                (let ((new-gene 
                       (make-array 
                        (case op
                          (0 (1- len))
                          (1 (1+ len))))))
                  (case op
                    ;; Don't add existing letter at mutate-point to new gene.
                    ;; That is, shrink gene by one character.
                    (0 
                     (loop for i from 0 below mutate-point do
                           (setf (aref new-gene i) (aref gene i)))
                     (loop for i from (1+ mutate-point) below len 
                         as j from 0 do
                           (setf (aref new-gene (+ j mutate-point))
                             (aref gene i))))
                    ;; Add the current letter at mutate-point plus 
                    ;; a new uniformly randomly selected letter.
                    (1 
                     (loop for i from 0 upto mutate-point do
                           (setf (aref new-gene i) (aref gene i)))
                     (setf (aref new-gene (1+ mutate-point))
                       (pick-random-alpha gene-alphabet))
                     (loop for i from (1+ mutate-point) below len 
                         as j from 2 do
                           (setf (aref new-gene (+ j mutate-point))
                             (aref gene i)))))
                  new-gene)))))
      gene)))



;;;;
;;;; Abstraction Layer (Genes and operations on genes are abstract 
;;;;                    however gene pools which are used below
;;;;                    are explicitly lists of genes)


(defun mate-genes (gene1 gene2 gene-length-constant?)
  "Given two genes, randomly split them and cross-join the pieces returning
two new genes."
  (if gene-length-constant?
      ;; Split each gene at the same point determined randomly (uniformly), 
      ;; then splice returning two new genes.
      (let ((split-point1 (random (gene-length gene1))))
        (multiple-value-bind (g11 g12) (split-gene gene1 split-point1)
          (multiple-value-bind (g21 g22) (split-gene gene2 split-point1)
            (values (splice g11 g22) (splice g21 g12)))))
    
    ;; Else gene length not constant. Split each gene at a point
    ;; that can occur on both genes and then and then splice returning
    ;; two new genes.
    (let ((len1 (gene-length gene1))
          (len2 (gene-length gene2)))
      (cond ((= len1 0) gene2)
            ((= len2 0) gene1)
            (t
             (let* ((split-point1 (random len1))
                    (split-point2 (random len2))
                    (split-point (min split-point1 split-point2)))
               (multiple-value-bind (g11 g12) (split-gene gene1 split-point)
                 (multiple-value-bind (g21 g22) (split-gene gene2 split-point)
                   (values (splice g11 g22) (splice g21 g12))))))))))


(defun make-gene-prob-dist (gene-list fitness-function)
  "Given a list of genes and a fitness function, return what
effectively amounts to a gene cumulative probability distribution
(not a density).  That is, a vector of pairs: (gene . fitness)
along with the total fitness of this vector of genes."
  (let ((cum 0)
        (prob-dist (rsm.queue:create)))
    (dolist (gene gene-list)
      (let ((fitness (funcall fitness-function gene)))
        (rsm.cache:cache-if-large *gene-cache* gene fitness)
        (incf cum fitness)
        (rsm.queue:enqueue (cons gene cum) prob-dist)))
    (values (coerce (rsm.queue:nget-list prob-dist) 'vector) cum)))


(defun select-gene-from-dist (fitness-dist total-fitness)
  "Randomly select a gene from a gene pool distribution (a vector of
gene/fitness pairs) with total-fitness, <total-fitness>. The
selection is based on the cumulative probability distribution
inherit in the gene/fitness pairs in <fitness-dist>."
  (if (<= total-fitness 0)
      (error "select-gene-from-dist: Bad total fitness of gene pool.
Either a bad fitness function or a poor gene pool.
The fitness of the gene pool is ~s. It should be greater than 0.
If fitness function is acceptable, try running again." 
             total-fitness))
  (let ((fitness (random total-fitness)))
    (bin-search fitness-dist fitness #'car #'cdr)))


(defun next-gen (gene-pool fitness-function mutation-rate 
                 gene-alphabet gene-length-constant?)
  "Form the next generation; that is, the next gene pool (a list of
genes) given the current gene pool, <gene-pool>, fitness function,
and mutation rate."
  
  ;; Form the cumulative probability distribution from the gene-pool list.
  (multiple-value-bind (fitness-dist total-fitness)
      (make-gene-prob-dist gene-pool fitness-function)
    (let (next-gene-pool
          (gene-pool-len/2 (truncate (length gene-pool) 2)))
      
      ;; Based on the probability distribution, form the next gene-pool list
      ;; by selecting and mating members of the current gene-pool.
      ;; The pool will be the same size from generation to generation.
      ;; Note: when gene-length-constant? is false, it is possible that 
      ;;       a gene may have zero length as a mutation. We will not allow
      ;;       this to occur. If it does we will continue with selection 
      ;;       and mutation until we get a full set of genes with positive
      ;;       length.
      (loop 
        with i = 0 
          do
            (let ((g1 (select-gene-from-dist fitness-dist total-fitness))
                  (g2 (select-gene-from-dist fitness-dist total-fitness)))
              (multiple-value-bind (ng1 ng2) 
                  (mate-genes g1 g2 gene-length-constant?)
                (setf ng1 (mutate-gene ng1 mutation-rate 
                                       gene-alphabet
                                       gene-length-constant?))
                (setf ng2 (mutate-gene ng2 mutation-rate 
                                       gene-alphabet 
                                       gene-length-constant?))
                (when (or gene-length-constant?
                          (and (> (gene-length ng1) 0)
                               (> (gene-length ng2) 0)))
                  (incf i)
                  (push ng1 next-gene-pool)
                  (push ng2 next-gene-pool))))
          until (= i gene-pool-len/2))
      next-gene-pool)))




(defun print-gene-pool (gene-pool)
  "Print the gene pool, <gene-pool>."
  (dolist (gene gene-pool)
    (format t "~%~a~%" gene)))




(defun ga-sim (n 
               gene-pool
               fitness-function
               mutation-rate
               gene-alphabet
               &key (gene-length-constant? t) 
                    (print-pools?          nil)
                    (print-intermediate?   nil)
                    (k 1))
  "Run a genetic algorithm simulation <n> times with initial gene pool
<gene-pool> which is a list of genes (each gene is a list from
alphabet <gene-alphabet); a fitness function <fitness-function> which
determines the fitness of a given gene (fitness values produced should be 
greater than *base-fitness-value*; a mutation rate
<mutation-rate> which determines the rate at which gene mutation
occurs; <gene-length-constant?> which determines if the length of a
gene is constant; and, <print-intermediate?> which determines whether
to print intermediate gene pools. If <print-pools?> is nil, no
generations are printed regardless of the value of <print-intermediate?>. 
Store the best k solutions in a cache."

  ;; Clear out the gene cache and set the size to <k>.
  (rsm.cache:clear-and-set-obj-cache-size *gene-cache* k)

  ;; Print with print parameters at their default settings.
  (with-standard-io-syntax
    
    ;; Print the initial gene pool.
    (when print-pools?
      (format t "Generation 0:~%")
      (print-gene-pool gene-pool))
    
    ;; Generate <n> gene pools.
    (dotimes (i n)
      (let ((genes
             (next-gen gene-pool fitness-function
                       mutation-rate 
                       gene-alphabet 
                       gene-length-constant?)))
        
        ;; Set the new genes to the symbol gene-pool.
        (setf gene-pool genes)
        
        ;; When true print the intermediate gene pools.
        (when (and print-pools? print-intermediate?)
          (format t "~%Generation ~a:~%" (1+ i))
          (print-gene-pool gene-pool))))
    
    ;; Print the last generation.
    (when print-pools?
      (format t "~%Generation ~a:~%" n)
      (print-gene-pool gene-pool))
    
    ;; Compute and possibly store (in a gene cache) genes from this 
    ;; last generation.
    (multiple-value-bind (dist cum)
        (make-gene-prob-dist gene-pool fitness-function)
      (declare (ignore dist cum)))
    
    (values)))


(defun solve-genetic-problem (n g-name &key (gene-length-constant? t) 
                                            (print-pools? nil)
                                            (print-intermediate? nil)
                                            (mutation-rate nil)
                                            (k 1))
  "Solve a genetic problem defined by structure genetic named by <g-name>.
Return the <k> best solutions. See ga-sim for an explanation of the 
keyword parameters."

  (let ((g-info (gethash g-name *genetic-problem-hash*)))
    (when (not g-info)
      (error "Could not find genetic problem named ~s~%" g-name))
    
    ;; Run the simulation <n> times.
    ;; The best <k> solutions will be stored in a gene cache.
    (ga-sim n 
            (g-pool g-info)
            (g-fitness-function g-info)
            (or mutation-rate (g-mutation-rate g-info))
            (g-alphabet g-info)
            :gene-length-constant? gene-length-constant?
            :print-pools? print-pools?
            :print-intermediate? print-intermediate?
            :k k)
    
    ;; Retrieve the genes of the best k solutions.
    (list g-name (rsm.cache:retrieve-obj-cache *gene-cache*))))


(defun clear-genetic-problems ()
  "Clear out the genetic problem definitions introduced by defgenetic."
  (clrhash *genetic-problem-hash*)
  (values))



(defun solve-all-genetic-problems (n &key (gene-length-constant? t) 
                                          (print-pools? nil)
                                          (print-intermediate? nil)
                                          (mutation-rate nil)
                                          (k 1))
  "Solve the list of genetic problems introduced by defgenetic.
Run the simulation <n> times. Return the best <k> solutions for
each problem. If <k> is a vector or list, then the ith value of <k>
is used with the ith value of the genetic problem 
(ordered alphabetically by name). The same is true of <n> and the 
genetic problems. See ga-sim for an explanation of the keyword parameters."
  (let (names)
    (maphash #'(lambda (key val)
                 (declare (ignore val))
                 (push key names)) *genetic-problem-hash*)
    (setf names (sort names #'(lambda (x y)
                                (string<= x y))))
    (solve-genetic-problems n names 
                            :gene-length-constant? gene-length-constant?
                            :print-pools? print-pools?
                            :print-intermediate? print-intermediate?
                            :mutation-rate mutation-rate
                            :k k)))


(defun solve-genetic-problems (n names &key (gene-length-constant? t) 
                                            (print-pools? nil)
                                            (print-intermediate? nil)
                                            (mutation-rate nil)
                                            (k 1))
  "Solve the list of genetic problems introduced by defgenetic named by <names>.
Run the simulation <n> times. Return the best <k> solutions for
each problem. If <k> is a vector or list, then the ith value of <k>
is used with the ith value of the genetic problem 
(ordered alphabetically by name). The same is true of <n> and the 
genetic problems. See ga-sim for an explanation of the keyword parameters."
  (let* ((len (length names))
         (ks (listify len k))
         (ns (listify len n)))    
    (let ((solutions (rsm.queue:create)))
      (loop :for name :in names
        :as k :in ks 
        :as n :in ns :do 
        (when print-pools?
          (format t "genetic problem name: ~s~%" name))
        (let ((k-solutions
               (solve-genetic-problem 
                n name
                :gene-length-constant? gene-length-constant?
                :print-pools? print-pools?
                :print-intermediate? print-intermediate?
                :mutation-rate mutation-rate
                :k k)))
          (when print-pools?
            (format t "Solution(s): ~s~%" (cadr k-solutions)))
          (rsm.queue:enqueue k-solutions solutions)))
      (rsm.queue:nget-list solutions))))



(defun display-solutions (solutions)
  "Display the solutions."
  (loop :for solution :in solutions :do
    (display-solution solution))
  (values))


(defun display-solution (solution)
  "Display a solution."
  (format t "Genetic problem name: ~a~%" (car solution))
  (format t "Solution(s): ~s~%" (cdr solution))
  (values))
