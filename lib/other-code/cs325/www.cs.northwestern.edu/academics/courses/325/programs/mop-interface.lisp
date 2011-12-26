;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; support users adding concepts
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (compile load eval)
  (unless (find-package :mop-interface)
    (make-package :mop-interface
                  :use (list (or (find-package :common-lisp)
                                 (find-package :lisp))))))


(in-package :mop-interface)

(require "mops")
(use-package :mops)

(require "frames")
(use-package :frames)

(export '(add-new-concept m-root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-new-concept (&optional (concept 'm-root))
  (let* ((specs (specs-of concept))
         (selection (menu-select specs)))
    (cond ((null selection)
           (add-concept-here concept))
          (t 
           (add-new-concept selection)))))

(defun add-concept-here (root)
  (let ((name (get-concept-name)))
    (format t "Creating ~S" name)
    (add-mop name (list root) nil)))

(defun add-mop (name absts slots)
  (add-frame name
             :type :mop
             :abstractions absts
             :slots slots))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-concept-name ()
  (format t "~&Give a name for the new concept: ")
  (loop for name = (read-mop-name)
        while (mop-p name)
        do (format t "~&That's in use -- try again: ")
        finally (return name)))

(defun read-mop-name ()
  (make-mop-name (string-trim " " (read-line))))

(defun make-mop-name (name-string)
  (read-from-string (fix-name-string name-string)))

(defun fix-name-string (name-string)
  (if (string-prefix-match-p name-string "M-")
    name-string
    (concatenate 'string "M-" name-string)))

(defun string-prefix-match-p (string pat)
  (string-equal (subseq string 0 (length pat))
                pat))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun menu-select (l &key (allow-none t))
  (print-menu l allow-none)
  (read-selection l))

(defun print-menu (l allow-none)
  (format t "~&Here are your choices:")
  (loop for x in l
        for i from 1
        do (format t "~&~S. ~S" i x))
  (print-menu-prompt (length l) allow-none)
  )

(defun print-menu-prompt (n allow-none)
  (format t "~%Enter a number between 1 and ~S to select an item"
            n)
  (when allow-none
    (format t " (enter 0 to select none)"))
  (format t ": "))

(defun read-selection (l)
  (let ((n (read)))
    (cond ((= n 0) (values nil nil))
          (t (values (nth (1- n) l) t)))))