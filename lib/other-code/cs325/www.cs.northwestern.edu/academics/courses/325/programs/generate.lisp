;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load eval compile)
  (unless (find-package :generate)
    (make-package :generate
                  :use (list (or (find-package :common-lisp)
                                 (find-package :lisp))))))

(in-package :generate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "tables")
(use-package :tables)

(require "mops")
(use-package :mops)

(require "extend-match")
(use-package :extend-match)
(import '(extend-match::binding-variable extend-match::pat-extension-p 
          extend-match::var-type-name-p))

(require "recognize")
(use-package :recognize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(generate add-expresser
          ?role ?path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pattern expresser functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(deftable pat-expresser)

(defun add-expresser (name function)
  (unless (var-type-name-p name)
    (error "Not a valid variable type name: ~S" name))
  (setf (pat-expresser name) function))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (GENERATE spec-concept [pattern]) => expression
;;;   Given a specialized concept, such as that produced by
;;;   RECOGNIZE, "expresses" that concept, using the 
;;;   patterns attached to the concepts and role fillers.
;;;
;;; Example:
;;;
;;;  > (setq p (first (recognize 'm-if-expression '(if 
;;;  (null l) nil (if (consp l) (car l) l)))))
;;;  ....
;;;  > (generate p)
;;;  (IF (NULL L) NIL (IF (CONSP L) (CAR L) L))

(defun generate (spec &optional (pat (select-pattern spec)))
  (express-pat pat spec))

(defun express-pat (pat spec)
  (cond ((pat-extension-p pat)
         (express-extension pat spec))
        ((atom pat) pat)
        (t (cons (express-pat (car pat) spec)
                 (express-pat (cdr pat) spec)))))

(defun express-extension (pat spec)
  (let ((expresser (pat-expresser (first pat))))
    (if (null expresser)
      pat
      (funcall expresser (rest pat) spec))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SELECT-PATTERN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (SELECT-PATTERN patterns specialized-concept) => pattern
;;;   Given one or more patterns, tries to find one that
;;;   expresses all the roles bound in specialized-concept.
;;;
;;; Deficiencies: 
;;;
;;;  Doesn't recognize when two patterns work
;;;  and one is better because it doesn't refer to roles
;;;  not mentioned in the specialized concept.
;;;
;;;  Has a hard-wired reference to ?role.

(defun select-pattern (spec)
  (let ((pats (concept-patterns (spec-concept-abst spec))))
    (cond ((null pats) (first spec))
          ((null (rest pats)) (first pats))
          (t (or (find-good-pat pats spec)
                 (first pats))))))

(defun find-good-pat (pats spec)
  (find-if #'(lambda (pat) (good-pat-p pat spec))
           pats))

(defun good-pat-p (pat spec)
  (every #'(lambda (binding)
             (expresses-role-p pat (binding-variable binding)))
         (spec-concept-bindings spec)))

(defun expresses-role-p (pat role)
  (cond ((role-extension-p pat)
         (eql (pat-extension-role pat) role))
        ((atom pat) nil)
        (t (or (expresses-role-p (first pat) role)
               (expresses-role-p (rest pat) role)))))

(defun role-extension-p (pat)
  (and (consp pat)
       (member (first pat) '(?role ?path))))

(defun pat-extension-role (pat)
  (second pat))

(defun pat-extension-path (pat)
  (rest pat))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expressers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-expresser '?role 'express-role)


(defun express-role (args spec)
  (destructuring-bind (role) args
    (generate (spec-concept-filler spec role))))
  

(add-expresser '?path 'express-path)


(defun express-path (path spec)
  (generate (spec-concept-path-filler spec path)))
  

(provide "generate")