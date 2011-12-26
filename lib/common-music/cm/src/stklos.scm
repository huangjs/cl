;;; **********************************************************************
;;; Copyright (C) 2005 Heinrich Taube (taube@uiuc.edu) This program is
;;; free software; you can redistribute it and/or modify it under the
;;; terms of the LLGPL (Lisp Lesser General Public License) as described
;;; at http://www.cliki.net/LLGPL. This program is distributed in the
;;; hope that it will be useful, but WITHOUT ANY WARRANTY; without
;;; even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.9 $
;;; $Date: 2005/07/09 12:38:20 $

#,(pragma :undefined-symbol-warning #f)

(require "srfi-1")
(require "srfi-4")

;;; Lisp environment

(define (lisp-version)
  (string-append "STklos " (version)))

(define quit exit)

(define pprint pp)

(define err error)

(define macroexpand macro-expand)

(define (read-macro-set! char func)
  #f)

;;; file system, file and ports

(define (pwd)
  (getcwd))

(define (cd . dir)
  (if (null? dir)
      (chdir "~/")
      (chdir (car dir)))
  (pwd))

;(define (set-file-position fil pos set?)
;  (if (not set?)
;      (port-current-position fil)
;      (if (= pos 0)
;          (port-seek fil pos ':start)
;          (port-seek fil pos ':current))))

(define (set-file-position fil pos set?)
  (if (= pos 0)
    (port-seek fil 0 :current) ; return the current position
    (if set?
      (port-seek fil pos :start)
      (port-seek fil pos :current))))

(define (delete-file fil)
  (system (string-append "rm " fil)))

(define (read-byte fp)
  ;; gad what a crock! ill have to look at srfi56
  (let ((c (read-char fp)))
    (if (eof-object? c) c
        (char->integer c))))

(define file-byte read-byte)

(define (write-byte by fp)
  (write-char (integer->char by) fp))

(define (port-position prt)
  (port-current-position prt))


;;; System calls

(define (os-name )
  (let ((str (machine-type)))
    (string->symbol
     (string-downcase
      (substring str 0 
                 (let loopit ((i 0)
                            (l (string-length str)))
                      (if (not (< i l)) #f
                          (if (not (char-alphabetic?
                                    (string-ref str i)))
                              i
                              (loopit (+ i 1) l)))))))))

(define (env-var var)
  (getenv var))

(define (set-env-var var val)
  (setenv! var val))

(define (shell cmd . args)
  (system cmd))

(define (get-current-time)
  (let ((d (current-date)))
    (vector (date-second d)
            (date-minute d)
            (date-hour d)
            (date-day d)
            (date-month d)
            (date-year d))))

;;; Lists

(define (list-set! lis pos val)
  (set-car! (list-tail lis pos) val)
  val)

(define sort! sort)

;;; Hashtables

(define (make-equal?-hash-table size)
  (make-hash-table equal?))

(define (hash-ref tbl key)
  (hash-table-get tbl key #f))

(define hash-set! hash-table-put!)

(define hash-remove! hash-table-remove!)

(define (hash-clear! tbl)
  (hash-table-map tbl (lambda (k v) (hash-table-remove! tbl k)))
  tbl)

(define (hash-fold fn x tbl)
  (hash-table-map tbl (lambda (k v) (set! x (fn k v x))))
  x)

;;; Strings

(define (string-trim-both str fn)
  (let ((len (string-length str))
        (beg #f)
        (end #f))
    (do ((i 0 (+ i 1)))
        ((not (< i len)) #f)
      (if ( fn (string-ref str i))
          (set! beg (+ i 1))
          (set! i len)))
    (do ((i (- len 1) (- i 1)))
        ((< i 0) #f)
      (if ( fn (string-ref str i))
          (set! end i)
          (set! i -1)))
    (if beg
        (if (= beg len)
            ""
            (if end (substring str beg end)
                (substring str beg len)))
        (if end (substring str 0 end)
            str))))

; NEEDS: string-read


;;; Keywords

(define (string->keyword str)
  (make-keyword str))

(define (symbol->keyword sym) 
  (string->keyword (symbol->string sym)))

(define (keyword->symbol kw)
  (string->symbol (keyword->string kw)))

;;; Numbers

(define logand bit-and)

(define logior bit-or)

(define lognot bit-not)

(define ash bit-shift)

(define (logtest i1 i2)
  (not (zero? (logand i1 i2))))

(define (logbit? ind j)
  (logtest (expt 2 ind) j))

(define (integer-length n)
  ;; from http://www-2.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/code/library/cl2scm/0.html
  (let ((leng
	 (inexact->exact (ceiling (/ (log (if (< n 0)
					      (- n)
					      (+ n 1)))
				     (log 2))))))
    (if (= (expt 2 (+ -1 leng)) (+ n 1))
	(+ -1 leng)  ;; correction needed - a little fudging to fix the round-off error
	leng)))

(define most-positive-fixnum (- (expt 2 29) 1))
(define most-negative-fixnum (- (expt 2 29)))

(define *random-state* #f)

(define (random n . args)
  ;; dont check for rationals etc
  (if (exact? n)
    (random-integer n)
    (if (= n 1.0)
        (random-real )
        (* (random-real) n))))

;(define (random n . args)
;  (if (integer? n)
;    (random-integer n)
;    (if (inexact? n)
;      (if (= n 1.0)
;        (random-real )
;        (* (random-real) n))
;      (error "random bounds not integer or real: ~s." n))))

(define integer-decode-float decode-float)
;; (define (integer-decode-float num)
;;   ;; see srfi 56...
;;   (if (zero? num) 
;;     (values 0 0 1)
;;     (let ((base 2)
;;           (mant-size 23)
;;           (exp-size 8)
;;           (sign 1))
;;       (if (negative? num) 
;;         (begin (set! sign -1) (set! num (- num))))
;;       (let* ((bot (expt base mant-size))
;;              (top (* base bot)))
;;         (let loopy ((n num) (e 0))
;;              (cond
;;                ((>= n top)
;;                 (loopy (quotient n base) (+ e 1)))
;;                ((< n bot)
;;                 (loopy (* n base) (- e 1)))
;;                (else
;;                 (values (round (inexact->exact n))
;;                         e sign))))))))

;;; Object system, interface is almost exactly the same as Gauche

(define-macro (define-generic* . args)
  `(define-generic ,(car args)))

(define-macro (define-method* formals . body)
  `(define-method ,(car formals) ,(cdr formals) ,@body))

(define *named-classes* (make-hash-table eq?))

(define-macro (define-class* class supers slots . options)
  (let ((cname #f)
        (metac #f)
        (csets (list)))
    ;; substitute :init-form for :init-value
    (do ((tail slots (cdr tail))
         (head (list )))
        ((null? tail) 
         (set! slots (reverse! head)))
      (set! head 
            (cons (if (member ':init-value (cdr (car tail)))
                      (map (lambda (x) (if (eq? x ':init-value)
                                           ':init-form x))
                          (car tail))
                      (car tail))
                  head)))
    (do ((tail options (cddr tail)))
        ((null? tail) #f)
      ;; parse the various metaclass options we use (in guile these
      ;; appear in prop list format after slotdefs)
      (case (car tail)
        ((:name) (set! cname (cadr tail)))
        ((:metaclass) (set! metac (cadr tail)))
        ((:file-types )  ; cm metaclass slot
         (set! csets (list* `(slot-set! ,class 'handles
                                        ,(cadr tail)) csets)))
        ((:output-hook) ; cm metaclass slot
         (set! csets (list* `(slot-set! ,class 'output-hook
                                        ,(cadr tail)) csets)))
        ((:definer) ; cm metaclass slot
         (set! csets (list* `(slot-set! ,class 'definer
                                        ,(cadr tail)) csets)))
        ((:versions) ; cm metaclass slot
         (set! csets (list* `(slot-set! ,class 'versions
                                        ,(cadr tail)) csets)))
        ))
    `(begin
      (define-class ,class ,supers ,slots
                    ,@ (if metac (list :metaclass metac) (list)))
      (slot-set! ,class 'name ,cname)
      ;; add named class to the table
      (hash-table-put! *named-classes* ,cname ,class)
      ;; set slots from metaclass
      ,@csets)))

(define (find-class* name . args)
  (hash-table-get *named-classes* name #f))

(define-macro (define-object-printer* args . body)
  `(define-method write-object , args ,@ body))

;; has class-subclasses

(define (slot-definition-initargs slot)
  (do ((opts (slot-definition-options slot) (cddr opts))
       (args (list)))
      ((null? opts)
       (reverse! args))
    (if (eq? (car opts) ':init-keyword)
      (set! args (cons (cadr opts) args)))))

(define class-direct-superclasses class-direct-supers)

(define slot-definition-initform slot-definition-init-form)

(define (slot-getter-form obj slot)
  `(slot-ref ,obj ',slot))

(define (slot-setter-form obj slot val)
  `(slot-set! ,obj ',slot ,val))

; (define-class <foo> () ((a :accessor foo-a :init-keyword :a :init-keyword :z)))
; (describe (make <foo> :z 22))
; (describe (make <foo> :z 22))   ; allows multi keywords but others do not set
; (slot-ref <foo> 'name)
; (slot-set! <foo> 'name 'freddy)  ; can change class name
; (find-class 'freddy)
; (describe <foo>)

;;;
;;; stub out thread support [srfi-18]
;;;

(define (nothreads )
  (error "Threads are not supported in STklos."))

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
  (err "set-periodic-task!: no periodic tasks in STklos."))

