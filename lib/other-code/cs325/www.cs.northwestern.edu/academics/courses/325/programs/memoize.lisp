;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simple function memo-izer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; USAGE
;;; -----

;;;> (require "memoize")
;;;> (use-package :memoize)
;;;
;;; (MEMOIZE 'function-name) 
;;; (MEMOIZE '(function-name equality-test equality-test ...))
;;;   Starts tracking each call to function-name and the value
;;;   returned. When the same arguments are passed to function-name
;;;   the previous value is returned rather than actually calling
;;;   the function.
;;; (UNMEMOIZE 'function-name)
;;;   Stops tracking calls to function-name. If re-memoized, a new
;;;   table of values is generaated.
;;;
;;; (CLEAR-MEMOIZE 'function-name)
;;;   Clears the table of stored values for function-name.
;;;
;;;
;;; RESTRICTIONS
;;; ------------
;;;
;;; Since most Lisp's won't let you redefine built-in CL functions,
;;; you can't memo-ize them either.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages, globals, structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)
  (unless (find-package :memoize)
    (make-package :memoize
                  :use (list (or (find-package :common-lisp)
                                 (find-package :lisp))))))

(in-package :memoize)

(export '(memoize unmemoize clear-memoize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; memo-info holds the original function, the memo version (to avoid
;;; problems if the function is redefined while memoized) and the
;;; top table of values

(defstruct memo-info fn memo-fn table)

;;; first-test, rest-tests, and length-tests handle (x x x . x) lists.

(defun first-test (tests) (if (consp tests) (car tests) tests))
(defun rest-tests (tests) (if (consp tests) (cdr tests) tests))
(defun length-tests (tests) 
  (and (consp tests) (null (cdr (last tests))) (length tests)))

#|
(defun memoize (fn-form)
  "Replace fn-name's global definition with a memoized version."

  (let* ((fn-name (if (atom fn-form) fn-form (car fn-form)))
         (fn (symbol-function (unmemoize fn-name)))
         (tests (if (atom fn-form) 'eq (cdr fn-form)))
         (table (make-hash-table :test (first-test tests)))
         (memo-fn (make-memo-fn fn table tests)))
    (setf (get fn-name 'memo-info) 
          (make-memo-info :fn fn :table table :memo-fn memo-fn)
          (symbol-function fn-name) 
          memo-fn)
    fn-name))
|#

(defun memoize (fn-name &key (key #'identity) (test nil test-p))
  "Replace fn-name's global definition with a memoized version."

  (let* ((fn (symbol-function (unmemoize fn-name)))
         (test (if test-p test (guess-test key)))
         (table (make-hash-table :test test))
         (memo-fn (make-memo-fn fn table key)))
    (setf (get fn-name 'memo-info) 
          (make-memo-info :fn fn :table table :memo-fn memo-fn)
          (symbol-function fn-name) 
          memo-fn)
    fn-name))


(defun unmemoize (fn-name)
  "Restore fn-name's pre-memoized global definition."
  (let ((info (get fn-name 'memo-info)))
    (when (and info
               (eq (symbol-function fn-name) (memo-info-memo-fn info)))
      (setf (symbol-function fn-name) (memo-info-fn info))
      (remprop fn-name 'memo-info))
    fn-name))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((info (get fn-name 'memo-info)))
    (when info (clrhash (memo-info-table info)))))

#|
(defun make-memo-fn (fn table tests)
  "Returns a memoized version of a function, given a table and tests."
  (case (length-tests tests)
    ((1) #'(lambda (x) (memo-put-1 fn x table)))
    ((2) #'(lambda (x y) (memo-put-2 fn x y table (cadr tests))))
    (t
     #'(lambda (&rest args)
         (memo-put-n fn args table (rest-tests tests))))))
|#

(defun guess-test (key)
  (cond ((or (eql key #'identity) (eql key 'identity))
         #'equal)
    (t #'eql)))

(defvar *hits* 0)
         
(defun make-memo-fn (fn table key)
  #'(lambda (&rest args)
      (let ((hash-key (funcall key args)))
        (multiple-value-bind (value found-p)
                             (gethash hash-key table)
          (if found-p (prog2 (incf *hits*) value)
              (setf (gethash hash-key table)
                    (apply fn args)))))))

(defun force-table (x table test)
  "Put a nested hashtable under x in table, if necessary."
  (or (gethash x table)
      (setf (gethash x table)
            (make-hash-table :test test))))

(defun memo-put-1 (fn x table)
  "Re-use past values of (fn x) in table."
  (multiple-value-bind (val found)
      (gethash x table)
    (if found val (setf (gethash x table) (funcall fn x)))))

(defun memo-put-2 (fn x y x-table test)
  "Re-use past values of (fn x y) in nested tables."
  (let ((y-table (force-table x x-table test)))
    (multiple-value-bind (val found)
        (gethash y y-table)
      (if found val (setf (gethash y y-table) (funcall fn x y))))))

(defun memo-put-n (fn arg-list table test-list)
  "Re-use past values of (fn x y ...) in nested tables."
  (do ((args arg-list (cdr args))
       (tests test-list (rest-tests tests))
       (n-table table
                (force-table (car args) n-table (first-test tests))))
      ((null (cdr args))
       (multiple-value-bind (val found)
           (gethash (car args) n-table)
         (if found val
             (setf (gethash (car args) n-table) (apply fn arg-list)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (print-cache fn-name)
;;;   Shows what the internal memo hashtables contain.

(defun print-cache (fn-name &aux (info (get fn-name 'memo-info)))
  (when info (print-all-hash (memo-info-table info))))

(defun print-all-hash (table &optional (tabs 0))
  (maphash #'(lambda (item value)
               (if (hash-table-p value)
                 (progn
                   (format t "~%~VT~S = <HASH-TABLE>"  tabs item)
                   (print-all-hash value (+ 5 tabs)))
                 (format t "~%~VT~S = ~S"  tabs item value)))
           table))
       
(provide "memoize")
