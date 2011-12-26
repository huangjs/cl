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
;;; $Revision: 1.23 $
;;; $Date: 2006/05/03 15:52:13 $

;;;
;;; porting code for Gauche Scheme:
;;; http://www.shiro.dreamhost.com/scheme/gauche/index.html
;;; 

(use srfi-1)     ; list library
(use srfi-4)     ; u8vector
(use srfi-13)     ; extra string support
(use srfi-27)    ; random bits
(use file.util)  ; current-directory, home-directory
(use gauche.threads) ; for rt threads

;; Lisp environment normalization

(define (lisp-version )
  (string-append "Gauche " (gauche-version)))

(define quit exit)  

(define err errorf)

(define (read-macro-set! char fn) #f)

;; Directories, files and ports

(define (pwd)
  (current-directory))

(define (cd . args)
  (if (null? args)
    (current-directory (home-directory))
    (current-directory (car args)))
  (pwd))

(define (set-file-position fil pos set?)
  (if (= pos 0)
    (port-seek fil 0 SEEK_CUR) ; return the current position
    (if set?
      (port-seek fil pos SEEK_SET)
      (port-seek fil pos SEEK_CUR))))

(define (delete-file f)  (sys-unlink f))

(define file-byte read-byte)

(define (port-position prt)
  (port-tell prt))


;; System calls

(define (os-name )
  (string->symbol (string-downcase (car (sys-uname )))))

(define (env-var var)
  (sys-getenv var))

(define (set-env-var var val)
  (sys-putenv var val))

(define (shell . args)
  (sys-system (car args)))

(define (get-current-time)
  (let ((now (sys-localtime (sys-gettimeofday))))
    (vector (slot-ref now 'sec)
            (slot-ref now 'min)
            (slot-ref now 'hour)
            (slot-ref now 'mday)
            (slot-ref now 'mon)
            (+ 1900 (slot-ref now 'year)))))

;;; lists

(define (list-set! lis pos val)
  (set-car! (list-tail lis pos) val)
  val)

;;; Hashtables

(define (make-equal?-hash-table size)
  (make-hash-table 'equal?))

(define (hash-ref table key)
  (hash-table-get table key #f))

(define (hash-set! table key value)
  (hash-table-put! table key value))

(define (hash-remove! table key)
  (hash-table-delete! table key))

(define (hash-fold fn x table)
  (hash-table-fold table fn x))    
  
;;; Strings

(define (string-downcase str)
  (let* ((len (string-length str))
         (new (make-string len)))
    (do ((i 0 (+ i 1)))
        ((= i len) new)
      (string-set! new i (char-downcase (string-ref str i))))))

(define (string-read str . args)
  (if (null? args)
      (read-from-string str)
      (read-from-string str (car args))))

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


;;; Keywords. Gauche keywords are cltl keywords, Yay! 

(define (keyword->symbol kw)
  (string->symbol (keyword->string kw)))

(define (symbol->keyword sym)
  (make-keyword (symbol->string sym)))

(define (string->keyword s)
  (make-keyword s))

;; numbers

(define most-positive-fixnum (- (expt 2 29) 1))

(define most-negative-fixnum (- (expt 2 29)))

(define *random-state* default-random-source)

(define (random n . args)
  (if (exact? n)
    (random-integer n)
    (if (inexact? n)
      (if (= n 1.0)
          (random-real)
          (* (random-real) n))
      (errorf "random bounds not integer or real: ~s." n))))

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

;                                   Chicken Gauche  Guile  Stklos
;(make class . args)                        y       y       y
;(initialize class inits)                   y       y       y
;(class-of obj)                             y       y       y
;(is-a? obj class)                          y       y       y
;(slot-ref obj slot)                        y       y       y
;(slot-set obj slot val)                    y       y       y
;(class-name class)                         y       y       y
;(class-slots class)                        y       y       x
;(class-direct-subclasses class)            y       y       y
;(class-direct-superclasses class)          *       y       *
;(slot-definition-name slot)                y       y       y
;(slot-definition-initargs slot)            n       n       n
;(slot-definition-initform slot)            n       n       n

; SCHEMES MUST PROVIDE THESE OOP METHODS, starred methods are to
; avoid overriding an existing implementation and/or normalize syntax
; issues between schemes
;
;(define-class* ...)
;(define-method* ...)
;(define-generic* ...)
;(find-class* name)                
;(define-object-printer* object port)
;(class-subclasses class) 
;(slot-getter-form obj slot)
;(slot-setter-form obj slot val)

(define-macro (define-generic* . args)
  `(define-generic ,(car args)))

;;; Gauche methods are like Guile methods, inclusing next-

(define-macro (define-method* formals . body)
  `(define-method ,(car formals) ,(cdr formals) ,@body))

;;; define-class* expansion for Gauche.  Gauche expands define-class
;;; into a really hairy expr.  rather than try to deal with that, the
;;; current macro expansion simply re-sets the class name after the
;;; class has been defined, ie (define-class* <foo> ... :name 'foo)
;;; becomes: (begin (define-class <foo> ...) (slot-set! <foo> 'name
;;; 'foo)) Luckily, the superclass list for Gauche's define-class
;;; expects variables, not names.

(define *named-classes* (make-hash-table 'eq?))

(define-macro (define-class* class supers slots . options)
  (let ((cname #f)
        (metac #f)
        (csets (list)))
    (do ((tail options (cddr tail)))
        ((null? tail) #f)
      (case (car tail)
        ((:name) (set! cname (cadr tail)))
        ((:metaclass) (set! metac (cadr tail)))
        ((:file-types )  ; cm metaclass slot
         (set! csets (cons* `(slot-set! ,class 'handles
                                        ,(cadr tail)) csets)))
        ((:output-hook) ; cm metaclass slot
         (set! csets (cons* `(slot-set! ,class 'output-hook
                                        ,(cadr tail)) csets)))
        ((:definer) ; cm metaclass slot
         (set! csets (cons* `(slot-set! ,class 'definer
                                        ,(cadr tail)) csets)))
        ((:versions) ; cm metaclass slot
         (set! csets (cons* `(slot-set! ,class 'versions
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

;; default method for instances.
;(define-method write-object ((obj <object>) port)
;  (format port "~s" obj))

(define-macro (define-object-printer* args . body)
  `(define-method write-object , args ,@ body))

(define (class-subclasses cls)
  (let ((subs (class-direct-subclasses cls)))
    (do ((tail subs (cdr tail)))
        ((null? tail) subs)
      (set! subs (append subs (class-subclasses (car tail)))))))

;;; implement clos slot mop accessors

(define (slot-definition-initargs slot)
  (do ((opts (slot-definition-options slot) (cddr opts))
       (args (list)))
      ((null? opts)
       (reverse! args))
    (if (eq? (car opts) ':init-keyword)
      (set! args (cons (cadr opts) args)))))

(define class-direct-superclasses class-direct-supers)

(define (slot-definition-initform slot)
  (list-prop (slot-definition-options slot) ':init-value))

;;; CM functions for expanding slot access in write-event methods.

(define (slot-getter-form obj slot)
  `(slot-ref ,obj ',slot))

(define (slot-setter-form obj slot val)
  `(slot-set! ,obj ',slot ,val))

; (define-class* <foo> () ((a :init-value 1)) :name 'foo)
; (find-class* 'foo)
; (make <foo>)
; (class-name <foo>)
;; test if supers list holds names or variables:
;      (define-class <bar> (freddy) ((b :init-value 2)))
; (define-class <bar> (<foo>) ((b :init-value 2)))
; (describe <bar>)
; (describe (make <bar>))

; (define-class* <foo> () ((a :init-value 1 :init-keyword :a :accessor foo-a)) :name 'foo)
; (define-class* <bar> (<foo>) ((b :init-value 2 :init-keyword :b :accessor bar-b) (c :init-value 3 :accessor bar-c :init-keyword :c :init-keyword :cc)) :name 'bar)
; (find-class* 'foo)
; (find-class* 'bar)
; (class-direct-subclasses (find-class* 'foo))
; (class-slots <bar>)
; (define sd (find (lambda (x) (eq? 'c (slot-definition-name x) )) (class-slots <bar>)))
; (apropos 'slot-definition)
; (slot-definition-initargs sd)
; (slot-definition-initform sd)

;;;
;;;
;;;

(define (use-system sys . args)
  (let ((dir (get-keyword ':directory args #f))
	(ver (get-keyword ':verbose args #t))
	(fil (format #f "~a.scm" sys))
	(tst #f))
    (if (not dir)
	(set! dir (format #f "~a/~a/" (sys-dirname *cm-directory*)
			  sys)))
    (set! tst (string-append dir fil))
    (cond ((file-exists? tst)
	   (let ((old *load-path*))
	     (dynamic-wind
		 (lambda () #f)
		 (lambda ()
		   (set! *load-path* (cons dir *load-path*))
		   (if ver (format #t "; loading ~a~%" tst))
		   (load tst))
		 (lambda () (set! *load-path* old)))
	     sys))
	  (else
	   (format #t "Can't locate system file \"~a.scm\" in ~s. Use :directory arg to use-system." fil dir)
	   #f))))

;;;
;;; eof
;;;


