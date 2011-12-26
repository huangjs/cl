(defpackage :our-match
  (:use :common-lisp)
  (:export #:our-match)
  )

(in-package :our-match)

(defparameter *extensions* (make-hash-table :test 'equal))
(defparameter *segment-extensions*
  (make-hash-table :test 'equal))

(defun our-match (pat input &optional (blists (list nil)))
  (cond ((null blists) nil)
        ((not (null (get-extension pat)))
         (funcall (get-extension pat)
                  pat input blists))
        ((eql pat input) blists)
        ((atom pat) nil)
        ((not (null (get-segment-extension (car pat))))
         (funcall (get-segment-extension (car pat))
                  pat input blists))
        ((atom input) nil)
        (t
         (our-match (cdr pat) (cdr input)
                    (our-match (car pat) (car input) blists)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-extension (name fn)
  (setf (gethash (string name) *extensions*) fn))

(defun get-extension (pat)
  (and (consp pat)
       (symbolp (car pat))
       (gethash (string (car pat)) *extensions*)))

(defun add-segment-extension (name fn)
  (setf (gethash (string name) *segment-extensions*) fn))

(defun get-segment-extension (pat)
  (and (consp pat)
       (symbolp (car pat))
       (gethash (string (car pat)) *segment-extensions*)))

(defun var-name (var)
  (cadr var))

(defun variable-p (pat)
  (and (consp pat)
       (symbolp (car pat))
       (string= (symbol-name (car pat)) "?")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-extension '? 'match-variable)

(defun match-variable (pat input blists)
  (bind-variable (var-name pat) input blists))

(defun bind-variable (var input blists)
  (and (not (null blists))
       (if (null var)
           blists
         (let ((binding (assoc var (car blists))))
           (cond ((null binding) 
                  (list (cons (cons var input)
                              (car blists))))
                 ((equal (cdr binding) input)
                  blists)
                 (t nil))))))



;;; (?and pat1 pat2 ...) matches if all patterns match

(add-extension '?and 'match-and)

(defun match-and (pat input blists)
  (match-and-list (cdr pat) input blists))

(defun match-and-list (pats input blists)
  (cond ((null blists) nil)
        ((null pats) blists)
        (t (match-and-list
            (cdr pats)
            input
            (our-match (car pats) input blists)))))
             

;;; (?is function) matches if (function input) is true

(add-extension '?is 'match-predicate)

(defun match-predicate (pat input blists)
  (if (funcall (cadr pat) input)
      blists
    nil))

;;; ((?* var) . more-pats)
;;;
;;; (our-match '((?* l1) (?* l2)) '(a b c))

(add-segment-extension '?* 'match-star)

(defun match-star (pat input blists)
  (destructuring-bind ((marker &optional var) &rest pats)
      pat
    (match-star-loop var pats input blists)))

(defun match-star-loop (var pats input blists)
  (cond ((null blists) nil)
        (t
         (match-tail var pats input blists))))

(defun match-tail (var pats input blists)
  (do ((tail input (cdr tail))
       (new-blists nil
                   (append*
                    (our-match pats tail
                               (bind-variable var (ldiff input tail) blists))
                    new-blists)))
      ((null tail) 
       (append*
        (our-match pats nil
                   (bind-variable var input blists))
        new-blists))))

(defun append* (x y) (append x y))

;;; (?not pattern) matches if pattern does not match

(add-extension '?not 'match-not)

(defun match-not (pat input blists)
  (if (not (our-match (cadr pat) input blists))
      blists
    nil))

