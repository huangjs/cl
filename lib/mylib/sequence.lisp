(in-package :cl-user)

(defpackage :hjs.data.sequence
  (:use #:common-lisp #:iterate #:hjs.meta.functional)
  (:export #:flatten
	   #:flatten1
	   #:split
	   #:split1
	   #:substitute-sequence
	   #:shuffle
	   #:nshuffle
	   #:random-elt
	   #:position-any
	   #:find-any
	   #:print-list-table
	   #:range
	   #:outer
	   #:foldr
	   #:foldl
	   #:binary-search
	   #:pack
	   #:span
	   #:take
	   #:take-when
	   #:appendf
	   #:removef
	   #:deletef
	   #:unionf
	   #:nunionf
	   #:emptyp
	   #:make-circular-list
	   #:circular-list
	   #:circular-list-p
	   #:proper-list
	   #:proper-list-p
	   #:proper-sequence
	   #:mappend
	   #:map-product
	   ))

(in-package :hjs.data.sequence)


;;; modifier
(define-modify-macro appendf (&rest lists) append
  "Modify-macro for APPEND. Appends LISTS to the place designated by the first
argument.")

(define-modify-macro removef (item &rest remove-keywords)
  (lambda (seq item &rest keyword-arguments)
    (apply #'remove item seq keyword-arguments))
  "Modify-macro for REMOVE. Sets place designated by the first argument to
the result of calling REMOVE with ITEM, place, and the REMOVE-KEYWORDS.")

(define-modify-macro deletef (item &rest remove-keywords)
  (lambda (seq item &rest keyword-arguments)
    (apply #'delete item seq keyword-arguments))
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the REMOVE-KEYWORDS.")

(define-modify-macro unionf (list) union
  "Modify-macro for UNION. Saves the union of LIST and the contents of the
place designated by the first argument to the designated place.")

(define-modify-macro nunionf (list) nunion
  "Modify-macro for NUNION. Saves the union of LIST and the contents of the
place designated by the first argument to the designated place. May modify
either argument.")


;;; constructor & predicates
(defun emptyp (sequence)
  "Returns true if SEQUENCE is an empty sequence. Signals an error if SEQUENCE
is not a sequence"
  (etypecase sequence
    (list (null sequence))
    (sequence (zerop (length sequence)))))

(defun make-circular-list (length &key initial-element)
  "Creates a circular list of LENGTH with the given INITIAL-ELEMENT."
  (let ((cycle (make-list length :initial-element initial-element)))
    (nconc cycle cycle)))

(defun circular-list (&rest elements)
  "Creates a circular list of ELEMENTS."
  (let ((cycle (copy-list elements)))
    (nconc cycle cycle)))

(defun circular-list-p (object)
  "Returns true if OBJECT is a circular list, NIL otherwise."
  (and (listp object)
       (do ((fast object (cddr fast))
            (slow (cons (car object) (cdr object)) (cdr slow)))
           (nil)
         (unless (and (consp fast) (listp (cdr fast)))
           (return nil))
         (when (eq fast slow)
           (return t)))))

(defun proper-list-p (object)
  "Returns true if OBJECT is a proper list."
  (cond ((not object)
         t)
        ((consp object)
         (do ((fast object (cddr fast))
              (slow (cons (car object) (cdr object)) (cdr slow)))
             (nil)
           (unless (and (listp fast) (consp (cdr fast)))
             (return (and (listp fast) (not (cdr fast)))))
           (when (eq fast slow)
             (return nil))))
        (t
         nil)))

(deftype circular-list () `(satisfies circular-list-p))

(deftype proper-list () `(satisfies proper-list-p))

(deftype proper-sequence ()
  "Type designator for proper sequences, that is proper lists and sequences
that are not lists."
  `(or proper-list
       (and (not list) sequence)))


;;; flatten/split/shuffle

(defun flatten1 (llists)
  "Flatten a list of lists into a list. (flatten but only one level)"
  (iter outer (for i in llists)
	(iter (for j in i)
	      (in outer
		  (collect j)))))

(defun flatten (tree &optional accumulator)
  "Flatten the tree into a list, depth-first iteration."
  (cond ((null tree) accumulator)
        ((atom tree) (cons tree accumulator))
        (t (flatten (first tree)
                    (flatten (rest tree) accumulator)))))

(defun split1 (list)
  "split list into 2 non-empty lists, give all the possibilities."
  (iter (with orig = list)
	(for left-tail initially orig then (rest left-tail))
	(for right-head = (rest left-tail))
	(while right-head)
	(progn
	  (setf (cdr left-tail) nil)
	  (collect (list (copy-list orig) right-head))
	  (setf (cdr left-tail) right-head))))

(defun split (list)
  "split list into 2, give all the possibilities. empty part is allowed."
  (cons (list '() list)
	(append (split1 list) (list (list list '())))))

(defgeneric shuffle (sequence &optional start)
  (:documentation "Shuffle a sequence/list."))

(defgeneric nshuffle (sequence &optional start)
  (:documentation "Shuffle a sequence/list, destructive."))

(defmethod nshuffle ((vector vector) &optional (start 0))
  (loop for idx downfrom (1- (length vector)) to (1+ start)
     for other = (+ start (random (1+ (- idx start))))
     do (unless (= idx other)
	  (rotatef (aref vector idx) (aref vector other))))
  vector)

(defmethod nshuffle ((list list) &optional (start 0))
  (coerce (nshuffle (coerce list 'simple-vector) start) 'list))

(defmethod shuffle ((list list) &optional (start 0))
  (nshuffle (copy-list list) start))

(defmethod shuffle ((vector vector) &optional (start 0))
  (nshuffle (copy-seq vector) start))

(defun random-elt (sequence &key (start 0) end)
  (declare (sequence sequence) (fixnum start) ((or fixnum null) end))
  (let ((i (+ start (random (- (or end  (if (listp sequence)
                                            (list-length sequence)
                                            (length sequence)))
                               start)))))
    (elt sequence i)))

;;; TODO: list is not optimized. use pattern matching
(defun substitute-sequence (new old sequence
			    &key (start 0) end count (test #'eql) key)
  "Replace occurrences of NEW in SEQUENCE with OLD.  NEW and OLD
don't need to be the same length."
  (let ((length-of-new (length new))
	(seq-type (type-of sequence))
	(result (subseq sequence 0 start))
	(count (if (numberp count) count most-positive-fixnum)))
    (iter (for pos = (search old sequence :test test :start2 start :end2 end :key key))
	  (repeat count)
	  (while pos)
	  (progn
	    (setf result
		  (concatenate seq-type
			       result
			       (subseq sequence start pos)
			       new))
	    (setf start (+ pos length-of-new)))
	  (finally
	   (return (concatenate seq-type result (subseq sequence start)))))))


;;; position-any/find-any

(defun position-any (bag sequence &rest position-args)
  "Find any element of bag in sequence and return its position.
Accept any argument accepted by the POSITION function."
  (apply #'position-if #'(lambda (element)
			   (find element bag)) sequence position-args))

(defun find-any (bag sequence &rest find-args)
  "Find any element of bag in sequence.  Accept any argument
accepted by the FIND function."
  (apply #'find-if #'(lambda (element)
		       (find element bag)) sequence find-args))

;;; print a table made from a list of lists
;;; fax's functional version seems to be faster than mine...
(defun print-list-table (table &optional (stream *standard-output*))
  (labels ((print-first-border (width)
	     (format stream "~%.~v{~A~}.~&" width '#1=(#\- . #1#)))
	   (print-intermediate-border (width)
	     (format stream "~%|~v{~A~}|~&" width '#2=(#\- . #2#)))
	   (print-last-border (width)
	     (format stream "~%'~v{~A~}'~&" width '#3=(#\- . #3#)))
	   (get-table-info ()
	     (let ( ;; indicating the width of each column
		   (width-vector (make-array (length (first table))
					     :element-type 'integer
					     :initial-element 0)))
	       (iter (for row in table)
		     (for i from 0)
		     (collect
			 (iter (for col in row)
			       (for j from 0)
			       (for s = (prin1-to-string col))
			       (for l = (length s))
			       (when (> l (aref width-vector j))
				 (setf (aref width-vector j) l))
			       (collect s))
		       into table-as-string)
		     (finally
		      (return (values
			       table-as-string
			       width-vector
			       (reduce #'+ width-vector :initial-value (1- (length width-vector))))))))))
    (multiple-value-bind (table-as-string width-vector row-width)
	(get-table-info)
      ;; start printing
      (print-first-border row-width)
      (iter (for row in table-as-string)
	    (with printed-first-row = nil)
	    (with width-list = (coerce width-vector 'list))
	    (progn
	      (if printed-first-row
		  (print-intermediate-border row-width)
		  (setq printed-first-row t))
	      (mapc (lambda (item column-width)
		      (format stream (format nil "|~a~a~a" "~a~" (- column-width (length item)) ",1@t") item))
		    row
		    width-list)
	      (format stream "|")))
      (print-last-border row-width))))


;;; range
(defun range (&rest args)
  (ecase (length args)
    (1 (iter (for i from 1 to (first args)) (collect i)))
    (2 (iter (with (start end) = args)
	     (for i from start to end)
	     (collect i)))
    (3 (iter (with (start end step) = args)
	     (for i from start to end by step)
	     (collect i)))))


;;; foldl/foldr
(defun foldl (op initial list)
  (reduce op list :initial-value initial))

(defun foldr (op initial list)
  (if (null list)
      initial
      (funcall op (car list)
	       (foldr op initial (cdr list)))))


;;; note: used by ascedent sorted vectors
(defun binary-search (item vector &key (test #'eql) (< #'<))
  "Search item in the vector, the vector is a sequence of ascedent ordered elements."
  (declare (type vector vector))
  (let ((i 0)
	(j (1- (length vector))))
    (iter (while (> (- j i) 1))
	  (for k = (floor (+ i j) 2))
	  (let ((mid (aref vector k)))
	    (if (funcall test item mid)
		(return-from binary-search k)
		(if (funcall < item mid)
		    (setf j k)
		    (setf i k)))))
    (cond ((funcall test item (aref vector i)) i)
	  ((funcall test item (aref vector j)) j)
	  (t nil))))

;;; pack/span
(defun pack (list &key (test #'eql))
  (iter (for i in (cdr list))
	(for count from 1)
	(with cur-item = (car list))
	(when (not(funcall test i cur-item))
	  (progn
	    (collect (cons cur-item count) into result)
	    (setf count 0
		  cur-item i)))
	(finally 
	 (return (append result (list (cons cur-item (1+ count))))))))

(defun span (packed-list)
  (iter outer (for (i . n) in packed-list)
	(iter (repeat n)
	      (in outer
		  (collect i)))))


;;; take/take-when
(defun take (n seq &key (result-type 'list))
  (coerce (subseq seq 0 n) result-type))

(defun take-when (pred seq &key (result-type 'list))
  (coerce (iter (for i in-sequence seq)
		(while (funcall pred i))
		(collect i))
	  result-type))


;;; mapping
(defun mappend (function &rest lists)
  "nondestructive appending version of mapcan."
  (loop for results in (apply #'mapcar function lists)
     append results))


(defun map-product (function list &rest more-lists)
  "Returns a list containing the results of calling FUNCTION with one argument
from LIST, and one from each of MORE-LISTS for each combination of arguments.
In other words, returns the product of LIST and MORE-LISTS using FUNCTION.

Example:
  
 (map-product 'list '(1 2) '(3 4) '(5 6)) => ((1 3 5) (1 3 6) (1 4 5) (1 4 6) 
                                              (2 3 5) (2 3 6) (2 4 5) (2 4 6))
"
  (labels ((%map-product (f lists)
             (let ((more (cdr lists))
                   (one (car lists)))
               (if (not more)
                   (mapcar f one)
                   (mappend (lambda (x)
                              (%map-product (curry f x) more))
                            one)))))
    (%map-product (if (functionp function) 
                      function
                      (fdefinition function))
                  (cons list more-lists))))


;;; combination
;;; note: isomorphic with collect-list, but can be applied in functional way
(defun outer (combinator &rest lists) 
  (cond ((null lists)
	 nil)
	((null (cdr lists))
	 (car lists))
	(t
	 (iter (with (1st 2nd . rst) = lists)
	       (return
		 (apply #'outer
			combinator
			(iter outer-col (for e1 in 1st)
			      (iter (for e2 in 2nd)
				    (in outer-col
					(collect (funcall combinator e1 e2)))))
			rst))))))


;;; todo: add inner
