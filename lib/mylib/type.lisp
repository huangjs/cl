(in-package :cl-user)

(defpackage :hjs.meta.type
  (:use :cl :hjs.meta.essential :hjs.meta.macro :iter :parse-number)
  (:export #:string->symbol
	   #:symbol->string
	   #:hash-table->alist
	   #:alist->hash-table
	   #:hash-table->plist
	   #:plist-hash->table
	   #:char->num
	   #:num->char
	   #:char->string
	   #:string->char
	   #:num->string
	   #:string->num
	   #:of-type
	   #:type=
	   ))

(in-package :hjs.meta.type)


;;; type conversion
(defalias symbol->name symbol-name :space :function)
(declaim (inline symbol->name))

(defun string->symbol (string)
  (make-symbol string))


(defun hash-table->alist (table)
  "Returns an association list containing the keys and values of hash table
TABLE."
  (let ((alist nil))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    alist))

(defun alist->hash-table (alist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the association list
ALIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (dolist (cons alist)
      (setf (gethash (car cons) table) (cdr cons)))
    table))


(defun hash-table->plist (table)
  "Returns a property list containing the keys and values of hash table
TABLE."
  (let ((plist nil))
    (maphash (lambda (k v)
               (setf plist (list* k v plist)))
             table)
    plist))

(defun plist-hash->table (plist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the property list
PLIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (do ((tail plist (cddr tail)))
        ((not tail))
      (setf (gethash (car tail) table) (cadr tail)))
    table))


(defun char->num (char)
  (let ((code (char-code char)))
    (when (and (<= code 57) (>= code 48))
      (- code 48))))

(defun num->char (num)
  (when (and (>= num 0) (<= num 9))
    (code-char (+ num 48))))

(defun char->string (char)
  (make-string 1 :initial-element char))

(defun string->char (string)
  (when (= (length string) 1)
    (char string 0)))

(defun num->string (num)
  (write-to-string num))

(defun string->num (string)
  (parse-number string))



;;; predicates
(defun of-type (type)
  "Returns a function of one argument, which returns true when its argument is
of TYPE."
  (lambda (thing) (typep thing type)))

(define-compiler-macro of-type (&whole form type &environment env)
  ;; This can yeild a big benefit, but no point inlining the function
  ;; all over the place if TYPE is not constant.
  (if (constantp type env)
      (with-gensyms (thing)
        `(lambda (,thing)
           (typep ,thing ,type)))
      form))

(declaim (inline type=))
(defun type= (type1 type2)
  "Returns a primary value of T is TYPE1 and TYPE2 are the same type,
and a secondary value that is true is the type equality could be reliably
determined: primary value of NIL and secondary value of T indicates that the
types are not equivalent."
  (multiple-value-bind (sub ok) (subtypep type1 type2)
    (cond ((and ok sub)
           (subtypep type2 type1))
          (ok
           (values nil ok))
          (t
           (multiple-value-bind (sub ok) (subtypep type2 type1)
             (declare (ignore sub))
             (values nil ok))))))
