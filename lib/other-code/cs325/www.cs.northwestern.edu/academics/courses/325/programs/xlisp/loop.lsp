;;; LOOP.LSP
;;; Author: Chris Riesbeck
;;; Last modified: 10/21/94

;;; Small subset of the Common Lisp LOOP macro for
;;; XlispStat 3.39
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (load compile eval)
  (unless (find-package :loop)
    (make-package :loop :use '(#+cltl2 :common-lisp
                               #-cltl2 :lisp))))

(in-package :loop)

(export '(loop))


;;; The subkeys say what parts a clause can have, e.g.,
;;; FOR has subkeys FROM, TO, BY, = , and so on
;;; The parse-fn returns the pieces of code to put into the loop.

(defstruct loop-info subkeys parse-fn)

;;; Keys are tested with string-equal (see the Common Lisp manual).
;;; This avoids problems with packages.

;;; For this reason, it's handy to define a few functions and forms
;;; based on string-equal. name-equal is the "safe" version -- it
;;; doesn't blow up on numbers, for example. It's always NIL if symbols
;;; are not involved.

(defun name-equal (x y)
  (and (symbolp x) (symbolp y)
       (string-equal (symbol-name x) (symbol-name y))))

(defun name-member (x l)
  (member x l :test #'name-equal))

(defvar *loop-key-info* nil)

(defun get-info-entry (key)
  (assoc key *loop-key-info* :test #'name-equal))

(defun force-info-entry (key)
  (or (get-info-entry key)
      (let ((entry (list key nil)))
        (push entry *loop-key-info*)
        entry)))

(defun key->loop-info (key)
  (second (get-info-entry key)))

(defun set-key->loop-info (key subkeys parse-fn)
  (let ((entry (force-info-entry key)))
    (setf (second entry)
          (make-loop-info :subkeys subkeys :parse-fn parse-fn))
    key))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct loop-parts
  initializers terminators pre-body body post-body finalizers)

(defun expand-loop (l)
  (if (old-style-loop-p l)
      `(do () () ,@l)
      (let ((parts (make-loop-parts
                    :initializers (list (list 'loop-var nil)))))
        (do ((clauses (collect-clauses l) (rest clauses)))
            ((endp clauses) nil)
            (update-parts parts (first clauses)))
        (parts->loop parts))))

(defun old-style-loop-p (l)
  (and (not (null l))
       (null (key->loop-info (first l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-clauses (l)
  (do ((clauses nil) (clause nil))
      ((null l) (nreverse clauses))
    (destructuring-setq (clause l) (collect-clause l))
    (when clause
          (setq clauses (cons clause clauses)))))

(defun collect-clause (l)
  (cond ((null l) nil)
    ((name-member (car l) '(if unless when))
     (collect-conditional-clause l))
    ((name-member (car l) '(do finally))
     (collect-multi-clause l))
    (t (collect-simple-clause l))))

(defun collect-simple-clause (l)
  (let ((info (key->loop-info (car l))))
    (if (null info)
        (error "~S is not a LOOP keyword" (car l))
        (let ((clause-end 
               (find-clause-end (loop-info-subkeys info)
                                (cddr l)
                                #'cddr)))
          (list (ldiff l clause-end) clause-end)))))

(defun collect-multi-clause (l)
  (let ((clause-end 
         (find-clause-end nil (cddr l) #'cdr)))
    (list (ldiff l clause-end) clause-end)))
  
		
(defun collect-conditional-clause (l)
  (let ((type (first l))
        (test (second l))
        (clause nil))
    (destructuring-setq (clause l) (collect-clause (cddr l)))
    (if (name-equal (first l) 'else)
        (let ((else-clause nil))
          (destructuring-setq (else-clause l) (collect-clause (rest l)))
          (list (list type test clause else-clause)
                l))
        (list (list type test clause) l))))

(defun find-clause-end (subkeys l stepfn)
  (do ((ll l (funcall stepfn ll)))
      ((or (endp ll)
           (clause-end-p subkeys (first ll)))
       ll)))

(defun clause-end-p (subkeys x)
  (and (atom x)
       (or (name-member x '(if when unless else))
           (not (name-member x subkeys)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-parts (parts clause)
  (funcall (loop-info-parse-fn (key->loop-info (car clause)))
           parts
           clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parts->loop (parts)
  `(let* ,(loop-parts-initializers parts)
    (block nil
      (tagbody
       loop-start
       ,@(loop-parts-terminators parts)
       ,@(loop-parts-pre-body parts)
       ,@(loop-parts-body parts)
       ,@(loop-parts-post-body parts)
       (go loop-start)
       loop-end)
           ,@(loop-parts-finalizers parts)
           (return loop-var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; doesn't optimize CAR/CDR combos

(defmacro destructuring-setq (vars form)
  (expand-destructuring-setq vars form))

(defun expand-destructuring-setq (vars form)
  (if (atom vars)
      `(setq ,vars ,form)
      (let ((pairs (destructuring-pairs vars form)))
        `(let (,(first pairs))
           ,@(mapcar #'(lambda (var-val) `(setq ,@var-val))
                     (rest pairs))))))

(defun destructuring-pairs (vars form)
  (let ((var (gensym)))
    `((,var ,form)
      ,@(destructuring-forms vars var))))

(defun destructuring-forms (vars form)
  (cond ((null vars) '())
        ((atom vars) `((,vars ,form)))
        (t (append (destructuring-forms (car vars) `(car ,form))
                   (destructuring-forms (cdr vars) `(cdr ,form))))))

(defun destructuring-vars (form)
  (cond ((null form) '())
    ((atom form) `((,form nil)))
    (t (append (destructuring-vars (car form))
               (destructuring-vars (cdr form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-parts (parts
                  &key initializers terminators 
                  pre-body body post-body 
                  finalizers)
  (setf (loop-parts-initializers parts) 
        (merge-initializations
         (loop-parts-initializers parts) 
         (copy-list initializers))
        (loop-parts-terminators parts) 
        (nconc (loop-parts-terminators parts) 
               (copy-list terminators))
        (loop-parts-pre-body parts) 
        (nconc (loop-parts-pre-body parts) 
               (copy-list pre-body))
        (loop-parts-body parts) 
        (nconc (loop-parts-body parts) 
               (copy-list body))
        (loop-parts-post-body parts) 
        (nconc (loop-parts-post-body parts) 
               (copy-list post-body))
        (loop-parts-finalizers parts) 
        (nconc (loop-parts-finalizers parts) 
               (copy-list finalizers))
        ))

(defun merge-initializations (old-inits new-inits)
  (cond ((null old-inits) new-inits)
    ((assoc (caar old-inits) new-inits)
     (merge-initializations (cdr old-inits) new-inits))
    (t
     (cons (car old-inits)
           (merge-initializations (cdr old-inits) new-inits)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (DEFLOOP key subkeys variables . body) => key	[Macro]
;;;   Defines a loop clause keyword.
;;; The subkeys is a list of all the keys that can follow the main
;;; keyword in the clause.

(defmacro defloop (key subkeys vars &rest body)
  `(set-key->loop-info ',key ',subkeys #'(lambda ,vars ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defloop always () (parts clause)
  (add-parts parts
             :initializers
             `((loop-var t))
             :body
             `((unless ,(second clause)
                       (return nil)))))

(defloop append (into) (parts clause)
  (list-collector parts `(copy-list ,(second clause)) (cddr clause)))

(defloop collect (into) (parts clause)
  (list-collector parts `(list ,(second clause)) (cddr clause)))

(defun list-collector (parts expr into)
  (let ((var (or (second into) 'loop-var)))
    (add-parts parts
               :initializers
               `((,var nil))
               :body
               `((setq ,var 
                       (nconc ,var ,expr))))))

(defloop count (into) (parts clause)
  (let ((expr (second clause))
        (var (or (fourth clause) 'loop-var)))
    (add-parts parts
               :initializers
               `((,var 0))
               :body
               `((when ,expr (incf ,var))))))

(defloop do () (parts clause)
  (add-parts parts :body (rest clause)))

(defloop finally () (parts clause)
  (add-parts parts :finalizers (rest clause)))

(defloop for (from to downto by in on = then) (parts clause)
  (let ((for-var (second clause))
        (for-type (third clause))
        (options (cdddr clause)))
    (cond 
      ((name-equal for-type 'from)
       (for-range parts for-var options))
      ((name-equal for-type '=)
       (for-assign parts for-var options))
      ((name-equal for-type 'in)
       (for-list parts for-var options))
      ((name-equal for-type 'on)
       (for-tails parts for-var options))
      (t (error "~S is an unknown FOR type" for-type)))))

(defun for-range (parts for-var options)
  (let* ((end-var (gensym))
         (by-var (gensym))
         (start (pop options))
         (direction (cond ((name-member (first options) '(to downto))
                           (pop options))
                      (t nil)))
         (end (if direction (pop options) nil))
         (by (cond ((name-equal (first options) 'by)
                    (second options))
               (t 1)))
         (inc-fn (if (or (null direction)
                         (name-equal direction 'to))
                     '+ '-))
         (end-pred (if  (or (null direction)
                            (name-equal direction 'to))
                        '> '<)))
    (when end
          (add-parts parts
                     :initializers
                     `((,end-var ,end)
                       ,@(destructuring-vars for-var))
                     :terminators
                     `((when (,end-pred ,for-var ,end-var)
                             (go loop-end)))))
    (add-parts parts
               :initializers
               `((,by-var ,by)
                 (,for-var ,start))
               :post-body
               `((setq ,for-var (,inc-fn ,for-var ,by-var))))))

(defun for-list (parts for-var options)
  (let ((list-var (gensym))
        (elt-var (if (consp for-var) (gensym) nil)))
    (add-parts parts
               :initializers
               `((,list-var ,(first options))
                 ,@(destructuring-vars for-var))
               :terminators
               `((when (null ,list-var) (go loop-end)))
               :pre-body
               `(,(expand-destructuring-setq for-var `(car ,list-var)))
               :post-body
               `((setq ,list-var (cdr ,list-var))))))

(defun for-tails (parts for-var options)
  (let ((list-var (gensym)))
    (add-parts parts
               :initializers
               `((,list-var ,(first options))
                 ,@(destructuring-vars for-var))
               :terminators
               `((when (null ,list-var) (go loop-end)))
               :pre-body
               `(,(expand-destructuring-setq for-var list-var))
               :post-body
               `((setq ,list-var (cdr ,list-var))))))

(defun for-assign (parts for-var options)
  (let ((first-val (first options))
        (next-val (third options))
        (first-time-var (gensym)))
    (add-parts parts
               :initializers
               `((,first-time-var t)
                 ,@(destructuring-vars for-var))
               :pre-body
               `(,(expand-destructuring-setq
                   for-var
                   (if next-val
                       `(cond (,first-time-var
                               (setq ,first-time-var nil)
                               ,first-val)
                          (t ,next-val))
                       first-val))))))

(defloop if (else) (parts args)
  (apply #'conditional-clause parts args))

(defloop never () (parts clause)
  (add-parts parts
             :initializers
             `((loop-var t))
             :body
             `((when ,(second clause)
                     (return nil)))))

(defloop sum (into) (parts clause)
  (let ((var (or (fourth clause) 'loop-var)))
    (add-parts parts
               :initializers
               `((,var 0))
               :body
               `((setq ,var 
                       (+ ,var ,(second clause)))))))

(defloop thereis () (parts clause)
  (add-parts parts
             :initializers
             `((loop-var nil))
             :body
             `((when (setq loop-var ,(second clause))
                     (return loop-var)))))

(defloop unless (else) (parts args)
  (apply #'conditional-clause parts args))

(defloop until () (parts clause)
  (add-parts parts
             :body
             `((when ,(second clause) (go loop-end)))))

(defloop when (else) (parts args)
  (apply #'conditional-clause parts args))

(defloop while () (parts clause)
  (add-parts parts
             :body
             `((unless ,(second clause) (go loop-end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun conditional-clause (parts type test clause &optional else-clause)
  (update-parts parts clause) 
  (when else-clause 
        (update-parts parts else-clause))
  (let ((else-body (if else-clause (pop-body parts) nil))
        (body (pop-body parts)))
    (add-parts parts 
               :body
               (cond ((name-member type '(if when))
                      `((if ,test ,body ,else-body)))
                 (t `((if ,test ,else-body ,body)))))))

(defun pop-body (parts)
  (let* ((body (loop-parts-body parts))
         (last-exps (last body)))
    (setf (loop-parts-body parts)
          (ldiff body last-exps))
    (car last-exps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (LOOP clause clause ...) => value		[Macro]
;;;   Iterates through the clauses.

(defmacro loop (&body clauses) (expand-loop clauses))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)
  
(provide "loop")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
10/24/94 [CKR]
Problem: loop keywords inside clauses not recognized.
Cause: code looking for loop keywords with eql, member, and case
Change: use name-equal to check for loop keywords

10/24/94 [CKR]
Problem: loop depended on tables package.
Change: store loop keywords in assoc list instead.

10/21/94 [CKR]
Problem: No :loop package.
Change:  Add package calls.
|#

