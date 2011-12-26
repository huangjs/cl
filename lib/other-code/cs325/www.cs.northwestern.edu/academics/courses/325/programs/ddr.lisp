;;; Simple (and CONSful) Deductive Retriever
;;; ----------------------------------------------------------------------
;;; - File: ddr.lisp
;;; - Author: Chris Riesbeck
;;;
;;; Updates
;;; 06/12/06: replaced make-empty-bindings with more general make-bindings [CKR]
;;; 06/07/06: add with-context [CKR]
;;; 03/23/06: added *compile-file-pathname* to require call [CKR]
;;; 03/21/06: replaced clear-rule-base with init-kb [CKR]
;;; 03/1/05: removed <- and -> exports [CKR]
;;; 05/12/04: fixed bug with hyphenated variable names [CKR]
;;; 05/12/04: changed tell to accept multiple assertions [CKR]
;;; 05/11/04: fixed apply-assertion bug (missing uniquify) [CKR]
;;; 05/03/04: renamed extend-blist to bind-var to reduce confusion [CKR]
;;; 04/30/04: added ask-trace, show-trace [CKR]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "tables" (cl:merge-pathnames "tables" (or *compile-file-pathname* *load-pathname*)))
  )

(defpackage #:ddr
  (:use #:common-lisp #:tables)
  (:export #:ask #:ask-trace #:find-blists #:init-kb #:show-trace #:tell 
           #:with-context
           #:bind 
           #:replace-variables #:unify #:var-p #:var-name #:var-bound-p 
           #:var-value #:make-bindings
           #:define-retrieval-method #:define-storage-method
           #:pattern-args #:pattern-head)
  )

(in-package #:ddr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fact and rule storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftable get-assertions)
(deftable get-backward-rules)
(deftable get-forward-rules)

;;; *CONTEXT*: a dynamically bound list of additional facts
;;; and rules, for use in what-if reasoning and parsing.
(defvar *context* nil)

(defmacro with-context (context &body body)
  `(let ((*context* ,context)) ,@body))

(defun init-kb (&rest rule-sets)
  (clear-rule-base)
  (dolist (rule-set rule-sets)
    (apply #'tell rule-set)))

(defun clear-rule-base ()
  (clear-table (get-assertions))
  (clear-table (get-backward-rules))
  (clear-table (get-forward-rules)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic pattern and rule structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rule-p (pat)
  (and (consp pat)
       (member (pattern-head pat) '(<- ->) :test #'string-equal)))

(defun rule-type (rule) (car rule))
(defun rule-head (rule) (cadr rule))
(defun rule-body (rule) (cddr rule))

(defun rule-index (rule)
  (let ((head (rule-head rule)))
    (if (eql (car head) 'and)
        (car (cadr head))
      (car head))))

(defun pattern-head (pat) (car pat))
(defun pattern-args (pat) (cdr pat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexing and retrieving rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun index-pat (pat)
  (cond ((rule-p pat) (index-rule pat))
	(t (index-assertion pat))))

(defun index-rule (rule)
  (let ((index (rule-index rule))
        (type (rule-type rule)))
    (cond ((string-equal type '->)
           (push rule (get-forward-rules index)))
          ((string-equal type '<-)
           (push rule (get-backward-rules index))))))

(defun index-assertion (pat)
  (push pat (get-assertions (pattern-head pat))))

(defun fetch-assertions (pat)
  (get-assertions (pattern-head pat)))

(defun fetch-backward-rules (pat)
  (get-backward-rules (pattern-head pat)))

(defun fetch-forward-rules (pat)
  (get-forward-rules (pattern-head pat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Storing facts and rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tell (&rest pats)
  (mapcar #'safe-store pats))

(defun safe-store (pat)
  (let ((safe-pat (uniquify-variables pat)))
    (funcall (get-storage-method safe-pat) safe-pat)
    safe-pat))

(defun default-storage-method (pat)
  (unless (stored-p pat)
    (index-pat pat)
    (forward-chain pat)))

(defun stored-p (pat)
  (find pat (fetch-assertions pat) :test #'variants-p))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing if one pattern is a variant of another
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun variants-p (pat1 pat2 &optional (pairs (list nil)))
  (cond ((null pairs) nil)
	((var-p pat1) (update-pairs pat1 pat2 pairs))
	((eql pat1 pat2) pairs)
	((or (atom pat1) (atom pat2)) nil)
	(t (variants-p (rest pat1) (rest pat2)
	     (variants-p (first pat1) (first pat2) pairs)))))

(defun update-pairs (var pat pairs)
  (cond ((null pairs) nil)
	((not (var-p pat)) nil)
	(t (add-pair var pat pairs))))

(defun add-pair (var1 var2 pairs)
  (let ((alist (first pairs)))
    (let ((pair1 (assoc var1 alist))
	  (pair2 (rassoc var2 alist)))
      (cond ((not (eq pair1 pair2)) nil)
	    ((null pair1)
	     (list (cons (cons var1 var2) alist)))
	    (t pairs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forward chaining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A forward-chaining rule has the form
;;;
;;;  (-> head sentence1 sentence2 ...)
;;;
;;; where head is either
;;;
;;;   (and trigger precondition1 precondition2 ...)
;;;
;;; or just trigger, which is equivalent to (and trigger).
;;; When a sentence is asserted that matches the trigger
;;; and the preconditions are all true,then sentence1,
;;; sentence2, ... are asserted.

(defun forward-chain (pat)
  (unless (rule-p pat)
    (dolist (rule (fetch-forward-rules pat))
      (dolist (blist (match-preconditions (rule-head rule) pat))
	(and-store
	 (replace-variables (rule-body rule) blist))))))

(defun match-preconditions (head pat)
  (and-retrieve (head-preconditions head)
                (unify (head-trigger head) pat)))

(defun head-preconditions (head)
  (if (eql (car head) 'and)
      (cddr head)
    nil))

(defun head-trigger (head)
  (if (eql (car head) 'and)
      (cadr head)
    head))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieving facts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *ddr-debug* nil)
(defvar *ddr-debug-tree* nil)

(defun ask (pat &optional (form pat))
  (replace-blists form (find-blists pat)))

(defun ask-trace (pat &optional (form pat))
  (clear-trace)
  (let* ((*ddr-debug* t)
         (results (ask pat form)))
    (show-tree-statistics)
    results))

(defun show-trace ()
  (pprint (get-trace-tree)))

(defun find-blists (pat)
  (start-subtrace pat)
  (let ((blists (funcall (get-retrieval-method pat) pat)))
    (end-subtrace pat blists)
    blists))

(defun replace-blists (form blists)
  (mapcar #'(lambda (blist)
              (replace-variables form blist))
          blists))

(defun default-retrieval-method (pat)
  (append (apply-context pat)
          (apply-assertions pat)
          (apply-rules pat)))

(defun apply-context (pat)
  (collect-blists #'(lambda (assertion)
                      (apply-assertion assertion pat))
                  *context*))

(defun apply-assertions (pat)
  (collect-blists #'(lambda (assertion)
                      (apply-assertion assertion pat))
                  (fetch-assertions pat)))

(defun apply-assertion (assertion pat)
  (let ((blists (unify pat (uniquify-variables assertion))))
    (ddr-trace (not (null blists)) assertion)
    blists))


(defun apply-rules (pat)
  (collect-blists #'(lambda (rule) (apply-rule rule pat))
		  (fetch-backward-rules pat)))

(defun collect-blists (fn l)
  (reduce #'append (mapcar fn l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-rule (rule pat)
  (let ((applies? (rule-applies-p rule pat)))
    (ddr-trace applies? rule)
    (when applies?
      (retrieve-from-rule rule pat))))

(defun rule-applies-p (rule pat)
  (unify pat (rule-head rule)))

(defun retrieve-from-rule (rule pat)
  (let ((new-rule (uniquify-variables rule)))
    (and-retrieve (rule-body new-rule)
                  (unify pat (rule-head new-rule)))))

(defun uniquify-variables (pat)
  (replace-variables pat (rename-list pat)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unifier
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unify (pat1 pat2 &optional (blists (make-bindings)))
  (cond ((null blists) nil)
        ((var-p pat1) (var-unify pat1 pat2 blists))
        ((var-p pat2) (var-unify pat2 pat1 blists))
        ((atom pat1) (when (eql pat1 pat2) blists))
        ((atom pat2) nil)
        (t (unify (cdr pat1) (cdr pat2)
                  (unify (car pat1) (car pat2) blists)))))

(defun var-unify (var pat blists)
  (mapcan #'(lambda (blist) (bind-var var pat blist))
          blists))

(defun bind-var (var pat blist)
  (if (and (var-p pat) 
           (var-equalp var pat blist))
      (list blist)
      (let ((bdg (var-binding var blist)))
        (cond (bdg (unify (binding-value bdg) pat (list blist)))
              ((contained-in-p var pat blist) nil)
              (t (list (add-var-binding var pat blist)))))))
              
(defun var-equalp (var1 var2 blist)
  (and (var-p var2)
       (or (eql (var-name var1) (var-name var2))
           (var-equalp var1 (var-value var2 blist) blist))))

(defun contained-in-p (var pat blist)
  (if (var-p pat)
      (or (eql (var-name var) (var-name pat))
          (contained-in-p var (var-value pat blist) blist))
      (and (consp pat)
           (or (contained-in-p var (car pat) blist)
               (contained-in-p var (cdr pat) blist)))))

(defun add-var-binding (var val blist)
  (cons (list (var-name var) val) blist))

(defun var-p (x) (and x (symbolp x) (eq (char (symbol-name x) 0) #\?)))
(defun var-name (x) x)

(defun var-value (var blist) (binding-value (var-binding var blist)))
(defun var-binding (var blist) (assoc (var-name var) blist))
(defun binding-value (bdg) (cadr bdg))

(defun var-bound-p (var blist) (not (null (var-binding var blist))))

(defun make-bindings (&optional a-list) 
  (list (mapcar #'(lambda (pair) (list (car pair) (cdr pair)))
                a-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variable replacement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun replace-variables (pat blist)
  (cond ((null blist) pat)
        ((var-p pat) (replace-variable pat blist))
        ((atom pat) pat)
        (t (mapcar #'(lambda (x) (replace-variables x blist))
		   pat))))

(defun replace-variable (var blist)
  (cond ((not (var-bound-p var blist)) var)
	(t (let ((value (var-value var blist)))
	     (cond ((eql var value) var)
		   (t (replace-variables value blist)))))))

(defun rename-list (pat)
  (mapcar #'(lambda (var)
              (list var (rename-variable var)))
          (pattern-variables pat)))

(defun rename-variable (var)
  (multiple-value-bind (base counter) (parse-name-counter var)
    (make-variable-name base (1+ counter))))

(defun pattern-variables (pat &optional vars)
  (cond ((var-p pat) (adjoin pat vars))
        ((atom pat) vars)
        (t
         (pattern-variables (cdr pat)
                            (pattern-variables (car pat) vars)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variable name generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; New variable names are generated by tagging the existing name with
;;; -nn. If the old variable already ends with -nn, the nn is
;;; incremented, otherwise the variable is extended with -1.

(defun parse-name-counter (var)
  (let* ((name (symbol-name (var-name var)))
         (base-end (and (internal-variable-p var)
                        (position #\- name :from-end t))))
    (cond ((null base-end) (values name 0))
          (t (values (subseq name 0 base-end)
                     (read-from-string name nil nil
                                       :start (1+ base-end)))))))

(defun internal-variable-p (var)
  (and (symbolp var)
       (null (symbol-package var))))
       
(defun make-variable-name (base counter)
  (make-symbol (format nil "~A-~S" base counter)))
		       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defining and getting retrieval methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftable retrieval-method)

(defmacro define-retrieval-method (head args &rest body)
  `(progn
     (setf (retrieval-method ',head)
           #'(lambda ,args ,@body))
     ',head))

(defun get-retrieval-method (pat)
  (or (retrieval-method (pattern-head pat))
      #'default-retrieval-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defining and getting storage methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftable storage-method)

(defmacro define-storage-method (head args &rest body)
  `(progn
     (setf (storage-method ',head)
           #'(lambda ,args ,@body))
     ',head))

(defun get-storage-method (pat)
  (or (storage-method (pattern-head pat))
      #'default-storage-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AND methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-storage-method and (pat)
  (and-store (pattern-args pat)))

(defun and-store (args) 
  (apply #'tell args))

(define-retrieval-method and (pat)
  (and-retrieve (pattern-args pat)))

(defun and-retrieve (args &optional (blists (make-bindings)))
  (do ((blists blists (retrieve-conjunct (first pats) blists))
       (pats args (rest pats)))
      ((or (null blists) (null pats)) blists)))

(defun retrieve-conjunct (pat blists)
  (collect-blists #'(lambda (blist)
		      (extend-blists
		       (find-blists (replace-variables pat blist))
		       blist))
		  blists))

(defun extend-blists (blists new-blist)
  (mapcar #'(lambda (old-blist)
	      (append new-blist old-blist))
	  blists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOT methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To deduce (not sentence), first look for (not sentence)
;;; using the default retrieval method, otherwise look for
;;; sentence and return success if not found
;;;
;;; No special storage method is needed.

(define-retrieval-method not (pat)
  (or (default-retrieval-method pat)
      (if (find-blists (car (pattern-args pat))) 
	  nil
	  (make-bindings))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BIND methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (bind var val) returns binding lists that bind var to
;;; (eval val), e.g., (bind ?x (+ ?y 1)).
;;;
;;; It's an error if val contains pattern variables.

(define-retrieval-method bind (pat)
  (destructuring-bind (bind var val) pat
    (declare (ignore bind))
    (unify var (eval val))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debug tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-trace ()
  (setq *ddr-debug-tree* nil))

(defun start-subtrace (pat)
  (when *ddr-debug*
    (format t "~&~VTGoal: ~S" (* 2 (get-node-depth *ddr-debug-tree*)) pat)
    (push :goal *ddr-debug-tree*)
    (push pat *ddr-debug-tree*)))

(defun end-subtrace (pat blists)
  (when *ddr-debug*
    (setq *ddr-debug-tree*
          (collect-subtree *ddr-debug-tree*))
    (format t "~&~VT" (* 2 (get-node-depth *ddr-debug-tree*)))
    (if (null blists)
        (format t "FAILED")
      (format t "Results:~{ ~S~}" 
              (replace-blists pat blists)))))

(defun ddr-trace (use? item)
  (when *ddr-debug*
    (push (list (if use? :use :skip) item)
          *ddr-debug-tree*)))

(defun collect-subtree (tree)
  (let ((l (member-if #'keywordp tree)))
    (cond ((null l)
           (error "No keyword in tree ~S" tree))
          (t
           (cons (reverse (ldiff tree (cdr l)))
                 (cdr l))))))

;;; The trace tree should be one node, but if the stack
;;; overflows, it will be incomplete. This will return either
;;; the one node if the tree looks OK, or the whole thing.

(defun get-trace-tree ()
  (if (and (consp *ddr-debug-tree*)
           (trace-node-p (car *ddr-debug-tree*))
           (null (cdr *ddr-debug-tree*)))
      (car *ddr-debug-tree*)
    *ddr-debug-tree*))

;;; calculates depth of current node in partial tree
(defun get-node-depth (tree)
  (count-if #'keywordp tree))
  
;;; Calculates the maximum depth of tree.
(defun get-tree-depth (tree)
  (cond ((trace-node-p tree)
         (1+ (reduce #'max (cdr tree)
                     :key #'get-tree-depth :initial-value 0)))
        (t 0)))

;;; Counts the number of tree nodes.
(defun get-tree-size (tree)
  (cond ((trace-node-p tree)
         (1+ (reduce #'+ tree
                     :key #'get-tree-size :initial-value 0)))
        (t 0)))

;;; A node is assumed to be any list starting with a keyword
(defun trace-node-p (x)
  (and (consp x) (keywordp (car x))))


(defun show-tree-statistics ()
  (let ((tree (get-trace-tree)))
    (format t "~%Nodes: ~S Depth: ~S"
            (get-tree-size tree)
            (get-tree-depth tree))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wrap up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide "ddr")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
5/3/03 [CKR]
Problem: No way to do forward stores for things like "if you turn
         left and you were heading east, now you're heading north"
Cause: forward-chain rule form was limited to (-> sentence ....)
Change: Allow (-> (and sentence condition ...) ...) which forward stores
        items if sentence matches a newly stored item, and the 
        conditions can be retrieved.

5/3/03 [CKR]
Problem: No way to do arithmetic, e.g., if you do something at
         time t, then at t + 1, x will be true
Change: Added (BIND pat lisp-form) which unifies pat with 
        (EVAL lisp-form)


4/30/03 [CKR]
Problem: Old package and module setup
Change: Updated to be compatible with Allegro and LispWorks,
        store and retrieve named to tell and ask to fit AIMA

2/27/95 [CKR]
Problem: Forward chaining broken.
Cause: Forward chainer didn't expect rule-body to return a list.
Change: Fixed forward chainer to use AND's storage method.

2/24/95 [CKR]
Problem: (<- head pat pat ...) not allowed.
Change: Rearranged code to use and-retrieve on CDFR of rule.

2/23/95 [CKR] 
Problem: AIP 2 code used old FOR macro, poor names, etc.
Change: Updated to C25 coding standards.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#
