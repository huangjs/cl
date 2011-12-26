;;; **********************************************************************
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.20 $
;;; $Date: 2006/04/14 09:54:33 $

;;;
;;; file system and OS interface.
;;;

(use-modules (srfi srfi-1 ))
(use-modules (srfi srfi-4 ))
(use-modules (oop goops))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 pretty-print)) ; remove  at some point...


;;; Lisp environment

(define (lisp-version)
  (string-append "Guile " (version)))

(define (err msg . args)
  (let ((str (apply format #f msg args)))
    (error str)))

(define (read-macro-set! char func)
  (read-hash-extend char
                    (lambda (arg port)
                      (func (read port)))))

(define pprint pretty-print)

;;; File system, files and ports

(define (pwd )
  (getcwd))

(define (cd . args)
  (let ((dir (if (null? args)
               (passwd:dir (getpw (getuid)))
               (car args))))
    (chdir dir)
    (getcwd)))

(define (read-byte fp)
  (char->integer (read-char fp)))

(define (file-byte fp)
  ;; gad what a crock! ill have to look at srfi56
  (let ((c (read-char fp)))
    (if (eof-object? c) c
        (char->integer c))))

;;;;changed to char->integer 5/13/05 tmi
;; (define (file-byte fp)
;;   ;; gad what a crock! ill have to look at srfi56
;;   (let ((c (read-char fp)))
;;     (if (eof-object? c) c
;;         (integer->char c))))


(define (write-byte by fp)
  (write-char (integer->char by) fp))

(define (set-file-position file pos set?)
  (if (= pos 0)
    (seek file 0 SEEK_CUR)
    (if set?
      (seek file pos SEEK_SET)
      (seek file pos SEEK_CUR))))

(define (port-position prt)
  (ftell prt))


;;; System calls

(define (os-name )
  (string->symbol (string-downcase (vector-ref (uname) 0))))

(define (env-var name)
  (getenv name))

(define (set-env-var name value)
  (setenv name value))

(define (shell cmd . args)
  (system cmd))

(define (get-current-time)
  (let ((vec (localtime (current-time))))
    (vector-set! vec 5
                 (+ 1900 (vector-ref vec 5)))
    vec))

;;; Hashtables. all ok

(define (make-equal?-hash-table  size)
  (make-hash-table size))

;;;
;;; numbers

(define (integer-decode-float num)
  (if (zero? num) 
    (values 0 0 1)
    (let ((base 2)
          (mant-size 23)
          (exp-size 8)
          (sign 1))
      (if (negative? num) 
        (begin (set! sign -1) (set! num (- num))))
      (let* ((bot (expt base mant-size))
             (top (* base bot)))
        (let loopy ((n num) (e 0))
             (cond
               ((>= n top)
                (loopy (quotient n base) (+ e 1)))
               ((< n bot)
                (loopy (* n base) (- e 1)))
               (else
                (values (inexact->exact (round n))
                        e sign))))))))


;;; Strings



;;;
;;; Keywords. Guile keywords are #:foo but you can set
;;; the reader to parse :foo as #:
;;; guile has keyword?, keyword->symbol and symbol->keyword

(read-set! keywords 'prefix)

(define (keyword->string kw)
  (symbol->string (keyword->symbol kw)))

(define (string->keyword s)
  (symbol->keyword (string->symbol s)))

;;;
;;; OOP functions
;;;

(define class-direct-superclasses class-direct-supers)

(define (class-subclasses cls)
  (let ((subs (class-direct-subclasses cls)))
    (append subs
            (loop for s in (class-direct-subclasses cls)
                  append (class-subclasses s)))))

(define slot-definition-initform slot-definition-init-value)

(define (slot-definition-initargs slotd)
  (let* ((inits (list #f))
	 (tail inits))
    (do ((l (cdr slotd) (cddr l)))
	((null? l) (cdr inits))
      (if (eq? (car l) ':init-keyword)
	(begin (set-cdr! tail (list (cadr l)))
	       (set! tail (cdr tail)))))))

(define (finalize-class class) #t)

;;;
;;; CM OOP functions that must be defined:

(define-macro (define-class* name supers slots . options)
  `(define-class ,name ,supers ,@ (append slots options)))

(define-macro (define-method* formals . body)
  `(define-method ,formals ,@ body))

(define-macro (define-generic* formals)
  (if (pair? formals)
    `(define-generic ,(car formals))
    `(define-generic ,formals)))

(define (find-class* name . root)
  ;; returns a class given its name or #f. optional
  ;; root class defaults to <top>.
  (letrec ((fc
	    (lambda (name class)
	      (if (null? class)
		#f
		(if (pair? class)
		  (or (fc name (car class))
		      (fc name (cdr class)))
		  (if (and (slot-bound? class 'name)
			   (eq? name (class-name class)))
		    class
		    (fc name (class-direct-subclasses class))))))))
    (fc name (if (null? root) <top> (car root)))))

(define-macro (define-object-printer* args . body)
  `(define-method ,(cons 'write args) ,@ body))

(define (slot-getter-form obj slot)
  `(slot-ref ,obj ',slot))

(define (slot-setter-form obj slot val)
  `(slot-set! ,obj ',slot ,val))

;;;
;;; stub out thread support [srfi-18]
;;;

(define (nothreads )
  (error "Threads are not supported in Guile."))

(define <thread> <object>)

(define (current-thread )
  (nothreads))

(define (thread? obj)
  obj
  (nothreads))

(define (make-thread thunk . name)
 (nothreads))

(define (thread-name thread) 
  thread
  (nothreads))

;(thread-specific thread)
;(thread-specific-set! thread obj)

(define (thread-start! thread)
  thread
  (nothreads))

(define (thread-yield! )
  (nothreads))

(define (thread-sleep! timeout)
  timeout
  (nothreads))

(define (thread-terminate! thread)
  thread
  (nothreads))

;(thread-join! thread [timeout [timeout-val]])

(define (mutex? obj)
  obj
  (nothreads))

(define (make-mutex . name)
  name
  (nothreads))

(define (mutex-name mutex)
  mutex
  (nothreads))

;(mutex-specific mutex)
;(mutex-specific-set! mutex obj)
;(mutex-state mutex)
;(mutex-lock! mutex [timeout [thread]])
;(mutex-unlock! mutex [condition-variable [timeout]])
;(condition-variable? obj)
;(make-condition-variable [name])
;(condition-variable-name condition-variable)
;(condition-variable-specific condition-variable)
;(condition-variable-specific-set! condition-variable obj)
;(condition-variable-signal! condition-variable)
;(condition-variable-broadcast! condition-variable)

;(define (current-time )
;  (nothreads))

(define (time? obj)
  obj
  (nothreads))

(define (time->seconds time) 
  time
  (nothreads))

(define (seconds->time sec)
  sec
  (nothreads))

;(current-exception-handler)
;(with-exception-handler handler thunk)
;(raise obj)
;(join-timeout-exception? obj)
;(abandoned-mutex-exception? obj)
;(terminated-thread-exception? obj)
;(uncaught-exception? obj)
;(uncaught-exception-reason exc)

;;;
;;; periodic task support
;;;

(define (set-periodic-task! . args)
  args
  (err "set-periodic-task!: no periodic tasks in Guile."))

