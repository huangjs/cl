(in-package :cs325-user)

(unuse-package :frames)
(unuse-package :mops)

(defvar *code-kb*)

(setq *code-kb* 
'(
  (programming-language)
  (lisp-language (programming-language)
                 (name "Lisp")
                 (run-time-types "yes"))
  (common-lisp (lisp-language)
               (name "Common Lisp")
               (compile-time-types "optional"))
  (allegro-lisp (common-lisp)
                (name "Allegro"))
  (allegro-windows (allegro-lisp)
                   (platform "windows"))
  ))

(defun get-name (concept)
  (car concept))

(defun get-concept (name)
  (assoc name *code-kb*))

(defun get-absts (name)
  (cadr (get-concept name)))

(defun get-parent (name)
  (car (get-absts name)))

(defun get-slots (name)
  (cddr (get-concept name)))

;;; (get-filler name role) => filler

(defun get-filler (name role)
  (cadr (assoc role (get-slots name))))

(defun inherit-filler (name role)
  (and (not (null name))
       (or (get-filler name role)
           (inherit-filler (get-parent name) role))))

;;; (add-concept concept) => concept

(defun add-concept (concept)
  (remove-concept (get-name concept))
  (push concept *code-kb*)
  concept)

;;; (remove-concept name) => concept

(defun remove-concept (name)
  (let ((concept (get-concept name)))
    (setq *code-kb* (delete concept  *code-kb*))
    concept))


(defun test-kb-code ()
  (unless (equal (get-filler 'allegro-lisp 'name) "Allegro")
    (format t "~&Failure: ~S" '(get-filler 'allegro-lisp 'name)))
  (unless (equal (get-filler 'allegro-lisp 'compile-time-types) nil)
    (format t "~&Failure: ~S" '(get-filler 'allegro-lisp 'compile-time-types)))
  (unless (equal (get-filler 'foo 'name) nil)
    (format t "~&Failure: ~S" '(get-filler 'foo 'name)))
  (unless (equal (inherit-filler 'allegro-lisp 'compile-time-types) "optional")
    (format t "~&Failure: ~S" '(inherit-filler 'allegro-lisp 'compile-time-types))) 
  (unless (equal (inherit-filler 'allegro-lisp 'run-time-types) "yes")
    (format t "~&Failure: ~S" '(inherit-filler 'allegro-lisp 'run-time-types)))
  (unless (equal (inherit-filler 'allegro-lisp 'platform) nil)
    (format t "~&Failure: ~S" '(inherit-filler 'allegro-lisp 'platform)))
  (unless (equal (inherit-filler 'allegro-lisp 'name) "Allegro")
    (format t "~&Failure: ~S" '(inherit-filler 'allegro-lisp 'name)))
  (unless (equal (inherit-filler 'allegro-windows 'name) "Allegro")
    (format t "~&Failure: ~S" '(inherit-filler 'allegro-windows 'name)))

  (unless
      (test-add-concept
       '(xanalysis-lisp (common-lisp) (name "lispworks") (platform "windows")))
    (format t "~&Failure: ~S" '(adding xanalysis-lisp)))

  (progn
    (add-concept '(foo () (name "foo")))
    (remove-concept 'foo)
    (unless (null (get-concept 'foo))
      (format t "~%Failure: ~S" '(remove-concept foo))))

  (progn
    (add-concept '(foo () (name "foo")))
    (add-concept '(foo () (name "baz")))
    (unless (equal (get-filler 'foo 'name) "baz")
      (format t "~%Failure: ~S" '(replace-concept foo)))
    (remove-concept 'foo))

  t
 )


(defun test-add-concept (concept)
  (add-concept concept)
  (let ((new-concept (get-concept (get-name concept))))
    (and (not (null new-concept))
         (equal concept new-concept))))