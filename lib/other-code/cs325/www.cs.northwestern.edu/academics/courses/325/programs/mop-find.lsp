(defun find-element (tag &optional (nodes *cs351*))
  (nodes-walker
    #'(lambda (node)
         (if (eql tag (node-tag node))
            (return-from find-element node)))
    nodes)
  nil)

(defun find-elements (tag &optional (nodes *cs351*))
  (let ((l nil))
    (nodes-walker
      #'(lambda (node)
          (if (eql tag (node-tag node))
            (push node l)))
      nodes)
    l))

(defun find-matching-node (pat &optional (nodes *cs351*))
  (nodes-walker
    #'(lambda (node)
         (if (pat-match pat node)
            (return-from find-matching-node node)))
    nodes)
  nil)

(defun find-matching-nodes (pat &optional (nodes *cs351*))
  (let ((l nil))
    (nodes-walker
      #'(lambda (node)
          (if (pat-match pat node)
            (push node l)))
      nodes)
    l))

(defun nodes-walker (fn nodes)
  (and (consp nodes)
    (let ((node (car nodes)))
      (funcall fn node)
      (nodes-walker fn (node-contents node))
      (nodes-walker fn (cdr nodes)))))


(defun node-p (x) t)

(defun node-tag (node)
  (cond ((atom node) node)
        ((atom (car node)) (car node))
        ((atom (caar node)) (caar node))
        (t nil)))

(defun node-attributes (node)
  (cond ((atom node) nil)
        ((atom (car node)) nil)
        ((atom (caar node)) (cdar node))
        (t nil)))

(defun node-contents (node)
  (cond ((atom node) nil)
        (t (cdr node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package :tables)

(deftable get-rule)

(defmacro defrule (name pat mop)
  `(add-rule ',name ',pat ',mop))

(defun add-rule (name pat mop)
  (setf (get-rule name) (list name pat mop))
  name)

(defun all-rules ()
  (let ((rules nil))
    (map-table
      #'(lambda (name rule)
          (push rule rules))
      (get-rule))
    rules))

(defun all-rule-names ()
  (let ((names nil))
    (map-table
      #'(lambda (name rule)
          (push name names))
      (get-rule))
    names))

(defun mop-markup (nodes &optional (rules (all-rules)))
  (let ((answers nil))
     (nodes-walker
       #'(lambda (node)
           (setq answers
             (append (mop-markup-node node rules)
                     answers)))
       nodes)
     answers))

(defun mop-markup-node (node &optional (rules (all-rules)))
  (loop for rule in rules
        append (apply-rule rule node)))

(defun apply-rule (rule node)
  (let ((blists (pat-match (rule-pattern rule) node)))
    (if (null blists)
      nil
      (loop for blist in blists
            append
             (instantiate-pattern (rule-mops rule) blist)))))


(defun rule-name (rule) (first rule))
(defun rule-pattern (rule) (second rule))
(defun rule-mops (rule) (cddr rule))


(defrule title-rule
   (:title (? title) (?*))
   ((:mop-instance :id (? title))
     ((:isa :value "cs-course"))
     ))





