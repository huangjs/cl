(cl:in-package simple-match)

(defun split-declare-body (code)
  (let ((declares nil))
    (labels ((inner (code)
	       (if (and (consp (first code))
			(eq (first (first code)) 'declare))
		   (progn
		     (push (first code) declares)
		     (inner (rest code)))
		   (values (nreverse declares)
			   code))))
      (inner code))))

(defun push-assoc (value key assoc &key (test #'eql))
  (let ((val (cons value (cdr (assoc key assoc :test test)))))
    (acons key val (remove key assoc :test test :key #'car))))

(defun scan-pattern (pattern)
  (let ((name-vars nil)
	(equalities nil)
	(ignores nil))
    (labels ((inner (pattern)
	       (cond ((and (consp pattern)
			   (symbolp (cdr (last pattern)))
			   (not (null (cdr (last pattern)))))
		      (let ((tmp (copy-list pattern)))
			(setf (cdr (last tmp))
			      (list '&rest (cdr (last pattern))))
			(inner tmp)))
		     ((eq pattern t) (let ((name (gensym "WILD")))
				       (push name ignores)
				       name))
		     ((or (member pattern '(&rest &optional nil))
			  (keywordp pattern))                    pattern)
		     ((atom pattern) (let ((name (gensym (symbol-name pattern))))
				       (setf name-vars
					     (push-assoc name pattern name-vars))
				       name))
		     ((eq (first pattern)
			  'quote)   (let ((name (gensym "LIT")))
				      (push (cons name pattern)
					    equalities)
				      name))
		     (t (mapcar #'inner pattern)))))
      (values (inner pattern)
	      name-vars
	      ignores
	      equalities))))

(defmacro match ((value &key default (flag 'fail)) &body clauses)
  (let ((value-var (gensym "VALUE"))
	(match-flag (gensym "FLAG")))
    (labels ((inner (clauses)
	       (if (null clauses)
		   (list default)
		   (destructuring-bind (pattern &rest clause)
		       (first clauses)
		     (multiple-value-bind (pattern name-vars ignores equalities)
			 (scan-pattern pattern)
		       (multiple-value-bind (declares body)
			   (split-declare-body clause)
			 (cons `(catch ',flag
				  (handler-case
				      (destructuring-bind ,pattern
					  ,value-var
					,@ (when ignores
					     `((declare (ignore ,@ignores))))
					   (locally
					       #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
					       ,@ (when equalities
						    `((unless (and
							       ,@ (mapcar (lambda (eq)
									    `(equal ,(car eq) ,(cdr eq)))
									  equalities))
							(throw ',flag nil))))
						  (unless (and
							   ,@ (mapcan (lambda (name-var)
									(let ((vars (cdr name-var)))
									  (mapcar (lambda (var)
										    `(equal ,(first vars) ,var))
										  (rest vars))))
								      name-vars))
						    (throw ',flag nil)))
					(setf ,match-flag t)
					(let ,(mapcar (lambda (name-vars)
							 `(,(car name-vars) ,(first (cdr name-vars))))
						       name-vars)
					  ,@declares
					  (return ,@body)))
				    (error (condition)
				      (when ,match-flag
					(error condition)))))
			       (inner (rest clauses)))))))))
      `(let ((,value-var ,value)
	     (,match-flag nil))
	 (block nil ,@(inner clauses))))))

