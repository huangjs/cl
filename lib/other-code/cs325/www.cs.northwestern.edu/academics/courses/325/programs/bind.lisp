;;; BIND from Chapter 3, AIP, redefined using parser and
;;; expander functions. A little longer, but more
;;; maintainable.

(defmacro bind (locs-n-vals &body body)
  (let ((setf-methods (get-setf-methods locs-n-vals))
        (vals (get-values locs-n-vals))
        (symbols (get-symbols locs-n-vals)))
    (expand-bind (collect-lists #'first setf-methods)
		 (collect-lists #'second setf-methods)
		 (collect-cars #'third setf-methods)
		 (collect-items #'fourth setf-methods)
		 (collect-items #'fifth setf-methods)
		 vals symbols body)))

(defun get-setf-methods (locs-n-vals)
  (mapcar #'(lambda (loc-n-val)
	      (multiple-value-list (get-setf-method (first loc-n-val))))
          locs-n-vals))

(defun get-symbols (locs-n-vals)
  (mapcan #'(lambda (loc-n-val)
              (if (symbolp (first loc-n-val))
                  (list (first loc-n-val))
                  nil))
          locs-n-vals))

(defun collect-items (accessor l)
  (mapcar accessor l))

(defun collect-lists (accessor l)
  (mapcan #'(lambda (x) (copy-list (funcall accessor x))) l))

(defun collect-cars (accessor l)
  (mapcar #'(lambda (x) (car (funcall accessor x))) l))

(defun expand-bind (temp-vars temp-vals store-vars store-forms
		    access-forms vals symbols body)
  `(let* (,@(pair-up temp-vars temp-vals)
	  ,@(pair-up store-vars access-forms))
    ,@(make-declares symbols)
    (unwind-protect
	 (let* ,(pair-up store-vars vals)
	   ,@store-forms
	   ,@body)
      ,@store-forms)))

(defun pair-up (l1 l2) (mapcar #'list l1 l2))

(defun make-declares (symbols)
  (if (null symbols) nil
      `((declare (special ,@symbols)))))
  
