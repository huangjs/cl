;;; 2007/03/22
;;; by Jianshi Huang <jianshi.huang@gmail.com>
;;; MIT license

(in-package :lazy-list)

(declaim (optimize speed))
(declaim (inline make-delay delay-closure delayp force))
(declaim (inline cdr cadr cddr rest second third fourth fifth
		 sixth seventh eighth ninth tenth nth
		 pop subseq))

;;; building block
(defstruct (delay
	     (:constructor make-delay (closure))
	     (:predicate delayp))
  (closure nil :type function))

(defmethod print-object ((thing delay) stream)
  (princ "..." stream))

(defmacro delay (form)
  `(make-delay #'(lambda () ,form)))

(defun force (thing)
  (if (delayp thing)
      (funcall (delay-closure thing))
      thing))

;;; constructors
(defmacro cons (head tail)
  `(cl:cons ,head (delay ,tail)))

(defun make-list (size &key initial-element)
  (declare ((integer 0 #.most-positive-fixnum) size))
  (if (= size 0)
      nil
      (cons initial-element
	    (make-list (1- size) :initial-element initial-element))))

;;; accessor
(defun cdr (list)
  (setf (cl:rest list) (force (cl:rest list))))

(defun cadr (list)
  (car (cdr list)))

(defun cddr (list)
  (cdr (cdr list)))

(defun rest (list)
  (cdr list))

(defun second (list)
  (cadr list))

(defun third (list)
  (car (cddr list)))

(defun fourth (list)
  (cadr (cddr list)))

(defun fifth (list)
  (car (cddr (cddr list))))

(defun sixth (list)
  (cadr (cddr (cddr list))))

(defun seventh (list)
  (car (cddr (cddr (cddr list)))))

(defun eighth (list)
  (cadr (cddr (cddr (cddr list)))))

(defun ninth (list)
  (car (cddr (cddr (cddr (cddr list))))))

(defun tenth (list)
  (cadr (cddr (cddr (cddr (cddr list))))))

(defun nthcdr (n list)
  (declare ((integer 0 #.most-positive-fixnum) n))
  (if (= n 0)
      list
      (nthcdr (1- n) (rest list))))

(defun nth (n list)
  (car (nthcdr n list)))

(defun enumerate-all (list)
  (loop with rest = (rest list)
     while rest
     do (setf rest (rest rest))
     finally (return list)))


;;; helper function
(defun append-two (l1 l2)
  (if (null l1)
      l2
      (cons (car l1)
	    (append-two (cdr l1) l2))))

(defun %check-once-only-names (names)
  "Check that all of the NAMES are symbols. If not, raise an error."
  ;; This only raises an error for the first non-symbol argument
  ;; found. While this won't report multiple errors, it is probably
  ;; more convenient to only report one.
  (let ((bad-name (find-if-not #'symbolp names)))
    (when bad-name
      (error "ONCE-ONLY expected a symbol but got ~S" bad-name))))

(defmacro once-only (names &body body)
  ;; Check the NAMES list for validity.
  (%check-once-only-names names)
  ;; Do not touch this code unless you really know what you're doing.
  (let ((gensyms (loop for name in names collect (gensym (string name)))))
    `(let (,@(loop for g in gensyms
		for name in names
		collect `(,g (gensym ,(string name)))))
       `(let (,,@(loop for g in gensyms for n in names
		    collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms
		      collect `(,n ,g)))
		,@body)))))


;;; iterations
(defun mapcar (function list &rest morelists)
  (labels ((map-one (fn list)
	     (declare (function fn))
	     (if (null list)
		 nil
		 (cons (funcall fn (car list))
		       (map-one fn (cdr list)))))
	   (map-mult (fn lists)
	     (declare (function fn))
	     (if (some #'null lists)
		 nil
		 (cons (apply fn (cl:mapcar #'car lists))
		       (map-mult fn (cl:mapcar #'cdr lists))))))
    (if morelists
	(map-mult function (cl:cons list morelists))
	(map-one function list))))

(defun mapcan (function list &rest morelists)
  (labels ((map-one (fn list)
	     (declare (function fn))
	     (if (null list)
		 nil
		 (let ((x (funcall fn (car list))))
		   (cons (car x)
			 (append-two (cdr x)
				     (map-one fn (cdr list)))))))
	   (map-mult (fn lists)
	     (declare (function fn))
	     (if (some #'null lists)
		 nil
		 (let ((x (apply fn (cl:mapcar #'car lists))))
		   (cons (car x)
			 (append-two (cdr x)
				     (map-mult fn (cl:mapcar #'cdr lists))))))))
    (if morelists
	(map-mult function (cl:cons list morelists))
	(map-one function list))))

(defun maplist (function list &rest morelists)
  (labels ((map-one (fn list)
	     (declare (function fn))
	     (if (null list)
		 nil
		 (cons (funcall fn list)
		       (map-one fn (cdr list)))))
	   (map-mult (fn lists)
	     (declare (function fn))
	     (if (some #'null lists)
		 nil
		 (cons (apply fn lists)
		       (map-mult fn (cl:mapcar #'cdr lists))))))
    (if morelists
	(map-mult function (cl:cons list morelists))
	(map-one function list))))

(defun remove-if-not (predicate list &key (start 0) end count key)
  (declare ((integer 0 #.most-positive-fixnum) start)
	   ((or null (integer 0 #.most-positive-fixnum)) end count)
	   (function predicate)
	   ((or null function) key))
  (labels ((rec (list start end count)
	     (declare ((integer 0 #.most-positive-fixnum) start)
		      ((or null (integer 0 #.most-positive-fixnum)) end count))
	     (if (null list)
		 nil
		 (if (<= count 0)
		     list
		     (if (> start 0)
			 (cons (car list)
			       (rec (cdr list) (1- start) (1- end) count))
			 (if (<= end 0)
			     list
			     (let ((x (if key (funcall key (car list)) (car list))))
			       (if (funcall predicate x)
				   (cons (car list)
					 (rec (cdr list) 0 (1- end) count))
				   (rec (cdr list) 0 (1- end) (1- count))))))))))
    (when (null end)
      (setf end most-positive-fixnum))
    (when (null count)
      (setf count most-positive-fixnum))
    (when (> start end)
      (error ":start must be smaller than or equal to :end"))
    (rec list start end count)))

(defun filter (predicate list)
  "a simple case of remove-if-not, exported."
  (declare (function predicate))
  (if (null list)
      nil
      (let ((x (car list)))
	(if (funcall predicate x)
	    (cons x (filter predicate (cdr list)))
	    (filter predicate (cdr list))))))

(defun remove-if (predicate list &key (start 0) end count key)
  (declare (function predicate)
	   ((or null function) key))
  (remove-if-not (complement predicate) list
		 :start start :end end :count count :key key))

(defun remove (item sequence &key (test #'eql) (start 0) end count key)
  (declare (function test))
  (remove-if #'(lambda (e) (funcall test e item))
	     sequence :start start :end end :count count :key key))

;;; list functions
(defun append (&rest lists)
  (if (null (rest lists))
      (first lists)
      (apply #'append (cl:cons (append-two (first lists) (second lists))
			       (rest (rest lists))))))

(defun length (list)
  (warn "The length of the list is not correct if the list is a lazy list.")
  (labels ((rec (list acc)
	     (declare ((integer 0 #.(1- most-positive-fixnum)) acc))
	     (if (or (null list) (delayp list))
		 acc
		 (rec (cl:cdr list) (1+ acc)))))
    (rec list 0)))

(defun revappend (x y)
  (let ((revx (cl:reverse (enumerate-all x))))
    (setf (cl:cdr (cl:last revx)) y)
    revx))

(defun subst (new old tree &key key (test #'eql))
  (declare (function test)
	   ((or null function) key))
  (mapcar #'(lambda (e)
	      (if (consp e)
		  (subst new old e :key key :test test)
		  (let ((x (if key (funcall key e) e)))
		    (if (funcall test old x)
			new
			e))))
	  tree))

(defun subst-if (new test tree &key key)
  (declare (function test)
	   ((or null function) key))
  (subst new t tree :key #'(lambda (e) (funcall test (if key (funcall key e) e)))))

(defun subst-if-not (new test tree &key key)
  (declare (function test)
	   ((or null function) key))
  (subst new nil tree :key #'(lambda (e) (funcall test (if key (funcall key e) e)))))

(defun pop (place)
  (cdr place)				; peek
  (cl:pop place))

(defun substitute (new old list &key (test #'eql) (start 0) end count key)
  (declare ((integer 0 #.most-positive-fixnum) start)
	   ((or null (integer 0 #.most-positive-fixnum)) end count)
	   (function test)
	   ((or null function) key))
  (labels ((rec (list start end count)
	     (declare ((integer 0 #.most-positive-fixnum) start end count))
	     (if (null list)
		 nil
		 (if (<= count 0)
		     list
		     (if (> start 0)
			 (cons (car list)
			       (rec (cdr list) (1- start) (1- end) count))
			 (if (<= end 0)
			     list
			     (let ((x (if key (funcall key (car list)) (car list))))
			       (if (funcall test x old)
				   (cons new
					 (rec (cdr list) 0 (1- end) count))
				   (cons (car list)
					 (rec (cdr list) 0 (1- end) (1- count)))))))))))
    (when (null end)
      (setf end most-positive-fixnum))
    (when (null count)
      (setf count most-positive-fixnum))
    (when (> start end)
      (error ":start must be smaller than or equal to :end"))
    (rec list start end count)))

(defun substitute-if (new predicate list &key (start 0) end count key)
  (declare (function predicate)
	   ((or null function) key))
  (substitute new t list :start start :end end :count count
	      :key #'(lambda (e) (funcall predicate (if key (funcall key e) e)))))

(defun substitute-if-not (new predicate list &key (start 0) end count key)
  (declare (function predicate)
	   ((or null function) key))
  (substitute new nil list :start start :end end :count count
	      :key #'(lambda (e) (funcall predicate (if key (funcall key e) e)))))

(defun remove-duplicates (list &key (test #'eql) (start 0) end key)
  (declare (function test)
	   ((or null function) key))
  (labels ((memberp (item list)
	     (cond ((null list) nil)
		   ((delayp (cl:cdr list)) nil)
		   (t
		    (if (funcall test (if key (funcall key (car list)) (car list)) item)
			t
			(memberp item (cl:cdr list)))))))
    (remove-if #'(lambda (e) (memberp e list)) list
	       :start start :end end :key key)))

(defun subseq (list start &optional end)
  (declare ((integer 0 #.most-positive-fixnum) start)
	   ((or null (integer 0 #.most-positive-fixnum)) end))
  (labels ((before (list end)
	     (declare ((integer 0 #.most-positive-fixnum) end))
	     (if (<= end 0)
		 nil
		 (cons (car list) (before (cdr list) (1- end))))))
    (when (and end (> start end))
      (error "start must be smaller than or equal to end"))
    (let ((after (nthcdr start list)))
      (if (null end)
	  after
	  (before after (- end start))))))

(defun merge (list1 list2 predicate &key key)
  (declare (function predicate)
	   ((or null function) key))
  (cond ((null list1) list2)
	((null list2) list1)
	(t
	 (let ((e1 (car list1))
	       (e2 (car list2)))
	   (if (funcall predicate (if key (funcall key e1) e1)
			(if key (funcall key e2) e2))
	       (cons e1 (merge (cdr list1) list2 predicate :key key))
	       (cons e2 (merge list1 (cdr list2) predicate :key key)))))))

