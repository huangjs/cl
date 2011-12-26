(in-package :cs325-user)

(define-test-suite set-grades
  (clear-grades)
  (set-grades 'smith '(12 0 20))
  (assert-equal (get-grades 'smith) '(12 0 20))
  (assert-equal (get-grades 'jones) nil)
  (set-grades 'jones '(5 6 10))
  (assert-equal (get-grades 'jones) '(5 6 10))
  (set-grades 'smith '(8 9 5))
  (assert-equal (get-grades 'smith) '(8 9 5))
  )

(define-test-suite define-grades
  (clear-grades)
  (define-grades smith 12 0 B)
  (define-grades jones 8 9 C+)
  (assert-equal (get-grades 'smith) '(12 0 B))
  (assert-equal (get-grades 'jones) '(8 9 C+))
  )

(defvar *grades* (make-hash-table))

(defun clear-grades ()
  (clrhash *grades*))

(defun get-grades (name)
  (gethash name *grades*))

(defun set-grades (name grades)
  (setf (gethash name *grades*) grades))

;;; (define-grades jones 8 9 C+)
;;; => (set-grades 'jones '(8 9 C+))

(defmacro define-grades (name &rest grades)
  `(set-grades ',name ',grades))
