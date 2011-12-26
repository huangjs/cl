;;; Based on generated lists in Charniak, Riesbeck, McDermott and Meehan's
;;; AI Programming, 2nd ed. (Google "lisp lazy lists" for similar efforts.)

;;; Updates:
;;;
;;; 01/15/06 Fixed some comments, added GCONS [CKR]
;;; 05/01/04 Added GLIST-LIST, generalized GPOP to handle places [CKR]

;;; (DELAY exp ...) => generator  [macro]
;;;   Returns a list generator. When accessed by one of the
;;;   functions below, the expressions will be evaluated (once).
;;;   The value of the last exp will be used as the list.
;;;
;;; (GCONS x y) => generated list [macro]
;;;   Same as (CONS x (DELAY y)).
;;;   
;;; GAPPEND, GCAR, GCDR, GNULL, GPOP
;;;   Equivalents of Lisp functions that handle list generators.
;;;   GCAR and GPOP always return an actual element or NIL.
;;;   GCDR and GAPPEND may return generators. 
;;;   Always use GAPPEND to combine generators without
;;;   expanding unnecessarily.
;;;
;;; (GLIST-LIST generator) => list
;;;   Returns the list of all elements in generator. Don't use
;;;   on infinite lists!


(defpackage #:glist
  (:use #:common-lisp)
  (:export #:delay #:gappend #:gcar #:gcdr #:gcons #:glist-list #:gnull #:gpop))

(in-package #:glist)


;;; External

(defmacro delay (&rest exp)
  `(make-gen :closure #'(lambda () ,@exp)))

(defmacro gcons (x y)
  `(cons ,x (delay ,y)))

;;; Based on XPOP example in Hyperspec (with "new" bug fixed)
(defmacro gpop (place &environment env)
  (multiple-value-bind (dummies vals new setter getter)
      (get-setf-expansion place env)
    (if (cdr new) (error "Can't expand (GPOP ~S)." place)
      `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter))
         (prog1 (gcar ,(car new))
           (setq ,(car new) (gcdr ,(car new)))
           ,setter)))))
        

(defun gcar (glist) (car (normalize glist)))
(defun gcdr (glist) (cdr (normalize glist)))
(defun gnull (glist) (null (normalize glist)))

(defun gappend (glist1 glist2)
  (cond ((gnull glist1) glist2)
        (t
         (cons (gcar glist1)
               (delay (gappend (gcdr glist1) glist2))))))

(defun glist-list (glist)
  (do ((l nil (cons (gpop glist) l)))
      ((gnull glist) (nreverse l))))


;;; Internal

;;; GEN -- structure
;;;   A generator that holds either a closure or the value
;;;   resulting from calling that closure. The closure is
;;;   removed after calling so that it's never called more
;;;   than once.
;;;
;;; (NORMALIZE l) => list
;;;   Given a list, generator, or list beginning with a 
;;;   generator, repeatedly uses FORCE until there is a
;;;   "real" list, either null, or starting with at least
;;;   one non-generator.
;;;
;;; (FORCE generator) => value
;;;   Gets the value of the generator, calling in the 
;;;   generator's closure at most once.

(defstruct (gen (:print-function print-gen))
  closure value)

(defun print-gen (gen stream depth)
  (declare (ignore depth))
  (format stream "#<gen ~S>"
          (or (gen-closure gen) (gen-value gen))))

(defun normalize (glist)
  (cond ((null glist) nil)
        ((gen-p glist) (normalize (force glist)))
        ((gen-p (car glist))
         (normalize (append (normalize (car glist)) (cdr glist))))
        (t glist)))


(defun force (gen)
  (cond ((not (gen-p gen)) gen)
        ((null (gen-closure gen)) (gen-value gen))
        (t
         (psetf
           (gen-value gen) (funcall (gen-closure gen))
           (gen-closure gen) nil)
         (gen-value gen))))

(provide "glist")
