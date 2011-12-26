;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          gen-prog.lisp
;;;; Purpose:       Genetic Programming.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: gen-prog.lisp,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.gen-prog)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;;;; PARAMETERS AND THEIR DEFAULT VALUES.

(defparameter *cross-over-func-prob* 0.9
  "This probability of selecting a subtree (as opposed to a leaf) of a
program tree crossing over program trees.")

(defparameter *cross-over-prob* 0.9
  "This probability is used to determine what part of the gene pool will 
be used in the cross over process to produce the next generation.")

(defparameter *max-tree-depth* 17
  "The maximum depth of any program tree in a gene pool.")

(defparameter *max-initial-tree-depth* 6
  "The maximum depth of the initial program trees.")

(defparameter *cross-over-attempts* 6
  "The number of attempts to make at crossing two trees.")

(defparameter *retry-count* 50
  "When generating random trees, the maximum number of tries one 
should make in order to get a tree of depth greater than 1.")

(defparameter *raw-fitness-increasing* nil
  "Is a larger raw fitness better.")

(defparameter *raw-fitness-bound* 0
  "The best raw fitness value one can have.")
 
(defparameter *obj-cache-size* 10
  "Default cache size of the program cache.")


(defparameter *cache-list-limit* 10
  "Default maximum length of any list in the program cache.
If nil is specified, then there will be no maximum length of any 
list in the program cache.")



(defstruct (genetic-params (:conc-name gp-))
  "A structure that contains all the parameters necessary for 
genetic programming."
  correct-func
  distance-metric
  sum-norm
  vars
  terminals
  init-terminals
  funcs
  funcs-vars
  func-conversions
  raw-fitness-increasing 
  raw-fitness-bound
  init-terminal-conversions
  cross-over-func-prob
  cross-over-prob
  cross-over-attempts
  max-initial-tree-depth
  max-tree-depth
  retry-count
  program-cache
  program-ary
  points
  population-size
  prog-idx
  obj-cache-size
  )

(defun make-gen-params (population-size vars terminals init-terminals 
                        funcs funcs-vars correct-func func-conversions 
                        init-terminal-conversions
                        distance-metric sum-norm points
                        &key (cross-over-func-prob *cross-over-func-prob*)
                             (cross-over-prob *cross-over-prob*)
                             (cross-over-attempts *cross-over-attempts*)
                             (max-initial-tree-depth *max-initial-tree-depth*)
                             (max-tree-depth *max-tree-depth*)
                             (retry-count *retry-count*)
                             (raw-fitness-increasing *raw-fitness-increasing*)
                             (raw-fitness-bound *raw-fitness-bound*)
                             (cache-list-limit *cache-list-limit*)
                             (obj-cache-size *obj-cache-size*)
                             )
  "A constructor for structure genetic-params."
  (make-genetic-params :vars vars 
                       :terminals terminals 
                       :init-terminals init-terminals
                       :funcs funcs
                       :funcs-vars funcs-vars
                       :func-conversions func-conversions
                       :raw-fitness-increasing raw-fitness-increasing
                       :raw-fitness-bound raw-fitness-bound
                       :correct-func correct-func
                       :init-terminal-conversions init-terminal-conversions
                       :distance-metric distance-metric
                       :sum-norm sum-norm
                       :cross-over-func-prob cross-over-func-prob
                       :cross-over-prob cross-over-prob
                       :cross-over-attempts cross-over-attempts
                       :max-initial-tree-depth max-initial-tree-depth
                       :max-tree-depth max-tree-depth
                       :retry-count retry-count
                       :points points
                       :population-size population-size
                       :program-cache (rsm.cache:make-standard-cache 
                                       "GP" 10 
                                       :threshold 0 
                                       :cache-list-limit cache-list-limit)
                       :program-ary (make-array 
                                     2
                                     :initial-element 
                                     (make-array population-size
                                                 :initial-element nil))
                       :obj-cache-size obj-cache-size
                       :prog-idx 0
                       )
  )



(defstruct (program-tree (:conc-name pt-))
  "A structure which encapsulates the information of a program tree."
  
  tree                                  ; The program tree.
  program                               ; The program of the tree.
  dist                                  ; A random number generator 
                                        ;  that generates nodes of the tree.
  fitness                               ; The fitness value of the tree.
  generation                            ; The generation this tree was found.
  )


(defun cp-program-tree (program-tree)
  "A constructor for a program tree."
  
  (make-program-tree :tree (copy-tree (pt-tree program-tree))
                     :dist (rsm.rand:clone (pt-dist program-tree))
                     :generation (pt-generation program-tree)))



;;; Get the next value of the program index.
(defun next-prog-idx (gen-params)
  "Get the next index used to get at the program array 
parameter of <gen-params>."
  (floor (+ (gp-prog-idx gen-params) 1) 2))

;;; Increment the program index, return the next index.
(defun incf-prog-idx (gen-params)
  "Increment the index used to get at the program array 
parameter of <gen-params>."
  (let ((curr-idx (gp-prog-idx gen-params)))
    (setf (gp-prog-idx gen-params) 
      (floor (+ curr-idx 1) 2))))


;;; Get the current program index.
(defun curr-prog-idx (gen-params)
  "Get the current index used to get at the program array 
parameter of <gen-params>."
  (gp-prog-idx gen-params))


(declaim (inline node-number))
(defun node-number (val)
  (car val))

(declaim (inline depth-in))
(defun depth-in (val)
  (caadr val))

(declaim (inline depth-out))
(defun depth-out (val)
  (cdadr val))


(defun examine-population (gen-params)
  "Print the current gene population."
  (let ((program-ary (gp-program-ary gen-params)))
    (let ((prog-idx (curr-prog-idx gen-params)))
      (loop :for tree-pair :across (aref program-ary prog-idx) 
        :as i :from 0 :do
        (format t "program-tree ~a = ~a~%" i (car tree-pair))))))



(defun bin-search (vec-obj val obj-acc val-acc)
  "Perform a binary search to find the first object in a vector of 
object/values, <vec-obj>, which are ordered in an increasing fashion 
based on the value of the objects that satisfies <val> <= obj-val. 
If val is greater than all object values, the last object is returned. 
Likewise, if val is less than all objects the first object is returned.
An object is returned from the object/value vector based on the value
of obj-acc."
  
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


(defmacro make-func-from-pairs (func-sym func-pairs-vec &key (dotted-p nil))
  "Makes a step function attached to the symbol, <func-sym>, that has 
values the step beginnings and step values defined by <func-pairs-vec>. 
If dotted-p is true the pairs in <func-pairs-vec> are dotted pairs.
Note: It is assumed that the start values of the steps in <func-pairs-vec> are 
      in increasing order."
  (let ((x (gensym)))
    `(defun ,func-sym (,x)
       (if ,dotted-p
           (bin-search ,func-pairs-vec ,x #'cdr #'car)
         (bin-search ,func-pairs-vec ,x #'cadr #'car)))))



(defun insert-program (program-tree fitness-function total-fitness
                       prog-idx slot-idx gen-params)
  "Inserts a '(program-tree . fitness) into the program array.
 Returns the fitness. Here, fitness is the cumulative fitness of 
the programs inserted into the program array."
  
  (let ((program-ary (gp-program-ary gen-params))
        (program-cache (gp-program-cache gen-params)))
    (let ((fitness (funcall fitness-function)))
      (setf (pt-fitness program-tree) fitness)
      (rsm.cache:cache-if-large program-cache 
                            program-tree 
                            fitness
                            :test #'(lambda (x y)
                                      (equal (pt-tree x) (pt-tree y))))
      (setf (aref (aref program-ary prog-idx) slot-idx)
        (cons program-tree (+ total-fitness fitness)))
      fitness)))


(defun select-program-tree (total-fitness prog-idx gen-params)
  "Select at random a program tree (with replacement) from the 
current generation using the probability distribution based on the 
fitness of the current generation. The current generation is found by 
indexing (using prog-idx) into gen-params to find the current program tree
array."
  
  (let ((program-ary (gp-program-ary gen-params)))
    (let ((fitness (* total-fitness (random 1.0f0))))
      (bin-search (aref program-ary prog-idx) fitness #'car #'cdr))))


(defun form-tree-internal-dist (tree)
  "Computes a discrete random number generator with 
values of the form: (node-number (depth-in . depth-to-bottom)).
The node number is the number of a node of the tree (depth first ordering).
The <*cross-over-func-prob*> probability is then evenly distributed over all 
sub-branches and the remaining probability is distributed evenly over the 
leafs. Returns the random number generator object, the number of nodes of 
the tree and the maximum depth of the tree, <tree>."
  
  (let ((count 0)
        (max-depth 1)
        (dist-que (rsm.queue:create)))
    (labels 
        ((rec (tree depth)
           (when (< max-depth depth)
             (setf max-depth depth))
           (do ((cursor (cdr tree) (cdr cursor)))
               ((null cursor))
             (cond ((consp (car cursor))
                    (rsm.queue:enqueue (list count depth :f) dist-que)
                    (incf count)
                    (rec (car cursor) (1+ depth)))
                   (t
                    (rsm.queue:enqueue (list count depth :l) dist-que)
                    (incf count))))))
      (rec tree 1)
      (let ((dist (rsm.queue:nget-list dist-que))
            (f-count 0))        
        (mapc #'(lambda (lst)
                  (when (eq :f (caddr lst))
                    (incf f-count)))
              dist)
        (let (f-prob l-prob)
          (cond ((= f-count count)
                 (setf f-prob (/ 1.0 count))
                 (setf l-prob 0.0))
                ((= 0 f-count)
                 (setf l-prob (/ 1.0 count))
                 (setf f-prob 0.0))
                (t
                 (setf f-prob (/ *cross-over-func-prob* f-count))
                 (setf l-prob (/ (- 1.0 *cross-over-func-prob*)
                                 (- count f-count)))))
          (setf dist 
            (mapcar #'(lambda (lst)
                        (list
                         (list
                          (car lst) 
                          (cons (cadr lst) (- max-depth (cadr lst))))
                         (if (eq :f (caddr lst))
                             f-prob
                           l-prob)))
                    dist))
          (values (rsm.rand:make-standard-randgen dist) count max-depth))))))



(defun select-subtree (tree n)
  "Select node <n> (depth first ordering) from tree <tree>."
  
  (let ((count 0))
    (labels 
        ((rec (tree)
           (do ((cursor (cdr tree) (cdr cursor)))
               ((null cursor))
             (when (= count n)
               (return-from select-subtree cursor))
             (incf count)
             (when (consp (car cursor))
               (rec (car cursor))))))
      (rec tree))))


(defun cross-over (program-tree-1 program-tree-2 gen-params)
  "<tree-program-1> and <tree-program-2> are structures of type program-tree.
Returns a pair of ORDINARY trees or nil if unable to cross-over in 
<cross-over-attempts> attempts."
  
  (let ((max-tree-depth (gp-max-tree-depth gen-params))
        (cross-over-attempts (gp-cross-over-attempts gen-params)))
    (let ((tree1 (pt-tree program-tree-1))
          (tree2 (pt-tree program-tree-2))
          (dist1 (pt-dist program-tree-1))
          (dist2 (pt-dist program-tree-2)))
      (let ((r1 (rsm.rand:next-rand dist1))
            (r2 (rsm.rand:next-rand dist2))
            (t1 (copy-tree tree1))
            (t2 (copy-tree tree2)))
        (let ((success (< (max 
                           (+ (depth-in r1) (depth-out r2))
                           (+ (depth-out r1) (depth-in r2))) 
                          max-tree-depth)))
          (loop 
            :repeat cross-over-attempts :until success :do
            (setf r1 (rsm.rand:next-rand dist1))
            (setf r2 (rsm.rand:next-rand dist2))
            (setf success (< (max 
                              (+ (depth-in r1) (depth-out r2))
                              (+ (depth-in r1) (depth-out r2))) 
                             max-tree-depth)))
          (unless success
            (return-from cross-over (values nil nil)))
          (let* ((cur1 (select-subtree t1 (node-number r1)))
                 (cur2 (select-subtree t2 (node-number r2)))
                 (tmp (car cur1)))
            (setf (car cur1) (car cur2))
            (setf (car cur2) tmp)
            (values t1 t2)))))))


(defun convert-init-tree (tree terminal-conversions)
  "Convert certain leafs of a tree, <tree>, using the conversions found 
in parameter <terminal-conversions>."
  (labels 
      ((rec (tree acc)
         (cond ((null tree) (nreverse acc))
               ((consp (car tree))
                (rec (cdr tree) (cons (rec (car tree) nil) acc)))
               (t
                (let ((pair (assoc (car tree) terminal-conversions)))
                  (if pair
                      (rec (cdr tree) (cons (funcall (cdr pair)) acc))
                    (rec (cdr tree) (cons (car tree) acc))))))))
    (rec tree nil)))


(defun make-random-tree-full (depth &key 
                                    vars
                                    terminals
                                    init-terminal-conversions
                                    funcs
                                    func-conversions)
  "Make a random tree where each branch has depth <depth>.
Returns a program-tree structure."

  (let ((term-len (length terminals))
        (func-len (length funcs)))
    (labels 
        ((rec (depth)
           (if (= depth 1)
               (nth (random term-len) terminals)
             (let* ((func-args (nth (random func-len) funcs))
                    (func (car func-args))
                    (arg-count (cdr func-args)))
               (unless (numberp arg-count)
                 (let* ((smallest (car arg-count))
                        (largest (car arg-count)))
                   (setf arg-count (+ smallest 
                                      (random 
                                       (1+ (- largest smallest)))))))
               (cons func (loop 
                            :repeat arg-count 
                            :collect (rec (1- depth))))))))
      (let ((tree (rec depth)))
        (when init-terminal-conversions
          (setf tree (convert-init-tree tree init-terminal-conversions)))
        (let ((lambda-tree (tree->func tree 
                                       :vars vars
                                       :func-conversions func-conversions)))
          (make-program-tree :tree tree 
                             :dist (form-tree-internal-dist tree)
                             :program lambda-tree
                             :generation 0))))))


(defun make-random-tree-grow (depth &key 
                                    vars
                                    terminals
                                    init-terminal-conversions 
                                    funcs
                                    func-conversions
                                    retry-count)
  "Make a random tree that has a maximum depth of <depth>.
Returns a program-tree structure."

  (let ((term-len (length terminals))
        (func-len (length funcs))
        (initial-depth depth)
        (r-count 0))
    (when (< depth 2)
      (error "make-random-tree-grow: depth must be greater than 1."))
    (labels
        ((rec (depth)
           (if (= depth 1)
               (nth (random term-len) terminals)
             (let* ((func-args (nth (random func-len) funcs))
                    (func (car func-args))
                    (arg-count (cdr func-args)))
               (cond ((and (numberp arg-count)
                           (= arg-count 0) 
                           (= initial-depth depth))
                      (incf r-count)
                      (if (>= r-count retry-count)
                          (error "Make-random-tree-grow: Unable to make a tree 
of depth greater than 1.")
                        (rec depth)))
                     ((and (numberp arg-count)
                           (= arg-count 0))
                      func)
                     (t
                      (unless (numberp arg-count)
                        (let* ((smallest (car arg-count))
                               (largest (car arg-count)))
                          (setf arg-count (+ smallest 
                                             (random 
                                              (1+ (- largest smallest)))))))
                      (cons func (loop 
                                   :repeat arg-count 
                                   :collect (rec (1- depth))))))))))
      (let ((tree (rec depth)))
        (when init-terminal-conversions
          (setf tree (convert-init-tree tree init-terminal-conversions)))
        (let ((lambda-tree (tree->func tree 
                                       :vars vars
                                       :func-conversions func-conversions)))
          (make-program-tree :tree tree 
                             :dist (form-tree-internal-dist tree)
                             :program lambda-tree
                             :generation 0))))))



(defun make-fitness-function (program-tree 
                              &key raw-fitness-increasing?
                                   raw-fitness-bound 
                                   points
                                   correct-func
                                   distance-metric
                                   sum-norm)
  "Take the raw fitness function from the program tree, <program-tree>, 
which gives a value for a single point; form a new <raw-fitness> based on 
the collection of points supplied, <points>; then form a standard fitness
function. That is we take the program tree considered as a raw fitness function
and return a new fitness function which is standardized - meaning goes from 0.0
to 1.0 with 1.0 best. This returned fitness function is also compiled."
  (compile nil
           #'(lambda ()
               (let* ((raw-point-fitness-func (pt-program program-tree))
                      (sum 
                       (loop :for point :in points :sum
                         (funcall distance-metric 
                                  (apply raw-point-fitness-func point)
                                  (apply correct-func point)))))
                 (let ((raw-fitness (funcall sum-norm sum)))
                   (cond ((and raw-fitness-increasing? raw-fitness-bound)
                          (/ 1.0 (+ 1.0 (- raw-fitness-bound raw-fitness))))
                         ((and (not raw-fitness-increasing?) raw-fitness-bound)
                          (/ 1.0 (+ 1.0 (- raw-fitness raw-fitness-bound))))
                         (t (error "make-fitness-function: Bad fitness parameter
combination, raw-fitness-increasing? and raw-fitness-bound."))))))))


(defun make-initial-population (gen-params)
  "From <gen-params> get <program-ary> and <population-size> and then
fill in the array <program-ary> of size <population-size> with elements 
of the type program-tree. The keyword, cache-list-limit, limits the 
maximum length of a list in the cache of programs (stored in <gen-params>) 
to its value. If cache-list-limit is nil, then there will be no 
maximum length to any list in the cache of programs.
Return the total fitness of the population."

  (let ((vars (gp-vars gen-params))
        (terminals (gp-init-terminals gen-params))
        (init-terminal-conversions (gp-init-terminal-conversions gen-params))
        (funcs (gp-funcs gen-params))
        (points (gp-points gen-params))
        (distance-metric (gp-distance-metric gen-params))
        (sum-norm (gp-sum-norm gen-params))
        (correct-func (gp-correct-func gen-params))
        (funcs-vars (gp-funcs-vars gen-params))
        (max-depth (gp-max-initial-tree-depth gen-params))
        (initial-population (gp-population-size gen-params))
        (raw-fitness-increasing? (gp-raw-fitness-increasing gen-params))
        (raw-fitness-bound (gp-raw-fitness-bound gen-params))
        (retry-count (gp-retry-count gen-params))
        (func-conversions (gp-func-conversions gen-params)))
    (let ((half-pop/depth (floor (/ initial-population (1- max-depth)) 2))
          (total-fitness 0)
          (prog-idx (curr-prog-idx gen-params))
          (slot-idx 0))
      (loop :for i :from 2 :to max-depth :do
        (loop :repeat half-pop/depth :do
          (let ((program-tree
                 (make-random-tree-grow i 
                                        :vars vars 
                                        :funcs funcs-vars
                                        :terminals terminals
                                        :init-terminal-conversions 
                                        init-terminal-conversions
                                        :func-conversions func-conversions
                                        :retry-count retry-count)))
            (incf total-fitness 
                  (insert-program program-tree
                                  (make-fitness-function 
                                   program-tree
                                   :raw-fitness-increasing? 
                                   raw-fitness-increasing? 
                                   :raw-fitness-bound raw-fitness-bound
                                   :distance-metric distance-metric
                                   :points points
                                   :correct-func correct-func
                                   :sum-norm sum-norm)
                                  total-fitness
                                  prog-idx slot-idx gen-params))
            (incf slot-idx)))
        (loop :repeat half-pop/depth :do
          (let ((program-tree
                 (make-random-tree-full i 
                                        :vars vars 
                                        :funcs funcs
                                        :terminals terminals
                                        :init-terminal-conversions 
                                        init-terminal-conversions
                                        :func-conversions func-conversions)))
            (incf total-fitness 
                  (insert-program program-tree
                                  (make-fitness-function 
                                   program-tree
                                   :raw-fitness-increasing? 
                                   raw-fitness-increasing?
                                   :raw-fitness-bound raw-fitness-bound
                                   :distance-metric distance-metric
                                   :points points
                                   :correct-func correct-func
                                   :sum-norm sum-norm)
                                  total-fitness
                                  prog-idx slot-idx gen-params))
            (incf slot-idx))))
      (loop :for idx :from slot-idx :below initial-population :do
        (let ((program-tree
               (make-random-tree-full (floor max-depth 2) 
                                      :vars vars 
                                      :funcs funcs
                                      :terminals terminals
                                      :init-terminal-conversions 
                                      init-terminal-conversions
                                      :func-conversions func-conversions)))
          (incf total-fitness 
                (insert-program program-tree
                                (make-fitness-function 
                                 program-tree
                                 :raw-fitness-increasing? 
                                 raw-fitness-increasing?
                                 :raw-fitness-bound raw-fitness-bound
                                 :distance-metric distance-metric
                                 :points points
                                 :correct-func correct-func
                                 :sum-norm sum-norm)
                                total-fitness
                                prog-idx idx gen-params))))
      total-fitness)))




(defun form-next-generation (gen-number total-fitness gen-params)
  "Form the next generation from the current one (contained in <gen-params>).
Use the fitness of the present population and the total-fitness to select 
randomly (with replacement) the next generation. The parameter, 
cache-list-limit sets a limit on the length of lists in the program cache
which is stored in <gen-params>. If nil, there will be no restrictions on 
the length of any list in the program cache."
  
  (let ((vars (gp-vars gen-params))
        (points (gp-points gen-params))
        (raw-fitness-increasing? (gp-raw-fitness-increasing gen-params))
        (raw-fitness-bound (gp-raw-fitness-bound gen-params))
        (distance-metric (gp-distance-metric gen-params))
        (sum-norm (gp-sum-norm gen-params))
        (cross-over-prob (gp-cross-over-prob gen-params))
        (population-size (gp-population-size gen-params))
        (correct-func (gp-correct-func gen-params))
        (func-conversions (gp-func-conversions gen-params)))

    (let ((cross-over-num (floor (/ (* population-size cross-over-prob) 2))))
      (let ((curr-prog-idx (curr-prog-idx gen-params))
            (next-prog-idx (next-prog-idx gen-params))
            (new-total-fitness 0)
            (slot-idx 0))
        (loop :for i :from 0 :below cross-over-num :do
          (let ((program-tree-1
                 (select-program-tree 
                  total-fitness curr-prog-idx gen-params))
                (program-tree-2
                 (select-program-tree 
                  total-fitness curr-prog-idx gen-params)))
            (multiple-value-bind (t1 t2)
                (cross-over program-tree-1 program-tree-2 gen-params)
              (unless (null (or t1 t2))
                (let ((dist1 (form-tree-internal-dist t1))
                      (dist2 (form-tree-internal-dist t2)))
                  (let ((lambda-tree-1 (tree->func 
                                        t1 
                                        :vars vars
                                        :func-conversions func-conversions))
                        (lambda-tree-2 (tree->func 
                                        t2
                                        :vars vars
                                        :func-conversions func-conversions)))
                    (let ((new-program-tree-1
                           (make-program-tree :tree t1 
                                              :dist dist1
                                              :program lambda-tree-1
                                              :generation gen-number))
                          (new-program-tree-2
                           (make-program-tree :tree t2
                                              :dist dist2
                                              :program lambda-tree-2
                                              :generation gen-number)))
                      (incf new-total-fitness
                            (insert-program new-program-tree-1
                                            (make-fitness-function 
                                             new-program-tree-1
                                             :raw-fitness-increasing? 
                                             raw-fitness-increasing? 
                                             :raw-fitness-bound 
                                             raw-fitness-bound
                                             :distance-metric distance-metric
                                             :points points
                                             :correct-func correct-func
                                             :sum-norm sum-norm)
                                            new-total-fitness
                                            next-prog-idx slot-idx 
                                            gen-params))
                      (incf slot-idx)
                      (incf new-total-fitness
                            (insert-program new-program-tree-2
                                            (make-fitness-function 
                                             new-program-tree-2
                                             :raw-fitness-increasing? 
                                             raw-fitness-increasing? 
                                             :raw-fitness-bound 
                                             raw-fitness-bound
                                             :distance-metric distance-metric
                                             :points points
                                             :correct-func correct-func
                                             :sum-norm sum-norm)
                                            new-total-fitness
                                            next-prog-idx slot-idx 
                                            gen-params))))
                  (incf slot-idx))))))
        (loop :for idx :from slot-idx :below population-size :do
          (let ((program-tree
                 (select-program-tree
                  total-fitness curr-prog-idx gen-params)))
            (let ((new-program-tree
                   (cp-program-tree program-tree)))
              (let ((lambda-tree (tree->func 
                                  (pt-tree new-program-tree)
                                  :vars vars
                                  :func-conversions func-conversions)))
                (setf (pt-program new-program-tree)
                  lambda-tree)
                (incf new-total-fitness 
                      (insert-program new-program-tree
                                      (make-fitness-function 
                                       new-program-tree
                                       :raw-fitness-increasing? 
                                       raw-fitness-increasing? 
                                       :raw-fitness-bound raw-fitness-bound
                                       :distance-metric distance-metric
                                       :points points
                                       :correct-func correct-func
                                       :sum-norm sum-norm)
                                      new-total-fitness
                                      next-prog-idx idx
                                      gen-params))))))
        new-total-fitness))))


(defun get-program-cache (gen-params)
  "Get the best lists of program trees."
  
  (let ((cache (gp-program-cache gen-params)))
    (rsm.cache:retrieve-obj-cache cache)))

(defun get-best-programs (gen-params)
  "Get a list of the best program trees."
  
  (let ((cache (get-program-cache gen-params)))
    (car cache)))

(defun tree->func (tree &key vars func-conversions)
  "Turn a tree into a lambda form."
  
  (if func-conversions
      `(lambda ,vars
         ,(sublis func-conversions tree))
    `(lambda ,vars
       ,tree)))


(defun raw-increasing->adjusted (raw raw-max)
  "Takes a raw (larger is better) and converts to an adjusted value."
  
  (/ 1.0 (+ 1.0 (- raw-max raw))))

(defun raw-descreasing->adjusted (raw raw-min)
  "Takes a raw (smaller is better) and converts to an adjusted value."
  
  (/ 1.0 (+ 1.0 (- raw raw-min))))



(defun get-num-part (x)
  "Get the number part of a variable. 
Example: (get-num-part x1)
returns 1."
  
  (let ((var (format nil "~a" x)))
    (parse-integer (subseq var (position-if #'digit-char-p var)))))

(defun get-args (func)
  "Gets the leafs (variables) of a tree."
  (let (args)
    (labels 
        ((rec (tree)
           (cond ((null tree) nil)
                 ((atom tree)
                  (pushnew tree args))
                 (t
                  (mapc #'rec (cdr tree))))))
      (rec func)
      (sort args #'(lambda (x1 x2)
                     (< (get-num-part x1) (get-num-part x2)))))))

