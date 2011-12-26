;;; **********************************************************************
;;; Copyright (C) 2003 Heinrich Taube (taube@uiuc.edu) 
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.37 $
;;; $Date: 2007/07/16 12:09:44 $

(define %months #("Jan" "Feb" "Mar" "Apr" "May" "Jun" 
		  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define (date-and-time)
  (let ((vect (get-current-time))
	(fnum (lambda (n)
		(if (< n 10)
		  (string-append "0" (number->string n))
		  (number->string n)))))
    (string-append (fnum (vector-ref vect 3))               ; day
                   " "
                   (vector-ref %months (vector-ref vect 4)) ; month
                   " "
                   (fnum (vector-ref vect 5))               ; year
                   ", "
                   (fnum (vector-ref vect 2))               ; hour
                   ":" 
                   (fnum (vector-ref vect 1))               ; min
                   ":" 
                   (fnum (vector-ref vect 0)))))            ; sec

;;;
;;; list utilities
;;;

(define-macro (dopairs decl . body)
  (let* ((m "dopairs: (v1 v2 list [return]) . body")
	 (s (if (pair? decl) (pop decl) (err m)))
	 (v (if (pair? decl) (pop decl) (err m)))
	 (l (if (pair? decl) (pop decl) (err m)))
	 (x (if (pair? decl) (pop decl) #f))
	 (a (gensym))
	 (h (gensym)))
    `(let ((,h ,l))
      (do ((,a ,h (cddr ,a))
	   (,s #f)
	   (,v #f))
	  ((null? ,a) ,x)
	(set! ,s (car ,a))
	(if (null? (cdr ,a))
	  (err "Uneven pair list: ~s" ,h)
	  (set! ,v (cadr ,a)))
	,@ body))))

(define-macro (make-cycl) 
  `(make-list 2))

(define-macro (cycl-data cycl)
  `(car ,cycl))

(define-macro (cycl-data-set! cycl data)
  `(set-car! ,cycl ,data))

(define-macro (cycl-last cycl)
  `(cadr ,cycl)) 

(define-macro (cycl-last-set! cycl data)
  `(set-car! (cdr ,cycl) ,data))

(define-macro (cycl-tail cycl)
  `(cddr ,cycl))

(define-macro (cycl-tail-set! cycl tail)
  `(set-cdr! (cdr ,cycl) ,tail)) ; set-cddr!

(define-macro (pop-cycl cycl)
  `(cdr-pop (cdr ,cycl)))

(define-macro (reset-cycl cycl)
  (let ((c (gensym)))
    `(let ((,c ,cycl))
       (cycl-tail-set! ,c (car ,c)))))

;(define-macro (cycl-append x cycl)
;  ;; append new thing to data.
;  (let ((var (gensym))
;        (new (gensym)))
;    `(let ((,var ,cycl)
;           (,new (list ,x)))
;       (if (null? (cycl-data ,var))
;	 (begin
;           (cycl-data-set! ,var ,new)
;	   (cycl-last-set! ,var ,new))
;	 (begin 
;	  (set-cdr! (cycl-last ,var) ,new)
;	  (cycl-last-set! ,var (cdr (cycl-last ,var)))))
;       ,var)))
;
;(define-macro (cycl-insert cycl element . args)
;  ;; accessor is object-time normally, but midifiles enqueue pending
;  ;; note offs as cons cells (time . message)
;  ;; if earliest is NIL then the object is placed at the last possible
;  ;; time avaliable so that objects of the same time are run in the
;  ;; order the user specified.  noteOff sheduling, on the other hand,
;  ;; wants to place a future note off at the earliest time possible
;  (let ((accessor (if (null? args) 'object-time (pop args)))
;	(earliest (if (null? args) #f (car args)))
;	(evar (gensym))
;        (bvar (gensym)))
;    `(let ((,evar ,element)
;           (,bvar ,cycl))
;       (cond ((null? (cycl-tail ,bvar))
;              (cycl-tail-set! ,bvar (list ,evar ))
;              (cycl-last-set! ,bvar (cycl-tail ,bvar)))
;             (else
;	      (let ((time (,accessor ,evar)))
;                (cond ((, (if earliest '> '>=) 
;                          time 
;                          (,accessor (car (cycl-last ,bvar))))
;		       (set-cdr! (cycl-last ,bvar) (list ,evar))
;                       (cycl-last-set! ,bvar (cdr (cycl-last ,bvar))))
;                      ((, (if earliest '<= '< )
;                          time 
;                          (,accessor (car (cycl-tail ,bvar))))
;		       (cycl-tail-set! ,bvar (cons ,evar (cycl-tail ,bvar))))
;                      (else
;		       (let ((tail (cycl-tail ,bvar)))
;			 (do ((head (cdr tail) (cdr tail)))
;			     ((or (null? head)
;				  (not (,(if earliest '< '<=)
;					 (,accessor (car head))
;					 time)))
;			      (set-cdr! tail (cons ,evar head)))
;			   (set! tail head))))))))
;       ,evar)))

;;;
;;;----------------------------------------------------
;;; printing

(define (format-integer int field pad)
  ;; if field is negative then left justify
  (let* ((str (number->string int))
	 (len (string-length str))
	 (wid (abs field)))
    (if (< len wid)
      (let* ((d (- wid len))
	     (s (make-string wid pad)))
	(if (< field 0)			; left justify
	  (dotimes (i len)
	    (string-set! s i (string-ref str i)))
	  (dotimes (i len)
	    (string-set! s d (string-ref str i))
	    (set! d (+ d 1))))
	s)
      str)))

;(define (address->string w) ; uses 32 bit word
;  (do ((s (make-string 8))
;       (n 28 (- n 4))
;       (i 0 (+ i 1))
;       (c #f))
;      ((< n 0) s)
;    (set! c (ash (logand w (ash #xf n)) (- n)))
;    (string-set! s i (integer->char (if (< c 10) (+ 48 c) (+ 55 c))))))

(define (quotify token)
  (string-append "\""
		 (if (string? token) token
		     (symbol->string token))
		 "\""))

(define (quote-if-necessary x)
  (if (or (number? x)
	  (string? x)
	  (vector? x)
	  (and (pair? x)
	       (eq? (car x) 'quote)))
    x
    `(quote ,x)))

;;;
;;; this is referenced by with-args in level1.scm 

(define (parse-lambda-list pars)
  ;;(format #t "args=~s" pars)
  ;; parse a cltl2 parameter declaration into seperate lists. modified 
  ;; to allow either cltl2 or guile style type decls, ie &key or #:key
  (let ((mode '&required)
        (reqs '())
        (opts '())
        (rest '())
        (keys '())
        (auxs '())
        (aok? #f)                       ; allow other keys
        (bind
	 (lambda (par type maxlen)
	   (if (pair? par)
               (begin
                (if (eq? type '&key)
                    (cond ((pair? (car par))
                           (unless (and (keyword? (car (car par)))
                                        (= (length (car par)) 2)
                                        (symbol? (cadr (car par))))
                             (err "Malformed ~s parameter: ~s"
                                  type par)))
                          ((symbol? (car par))
                           #t)
                          (else
                           (err "Malformed ~s parameter: ~s."
                                type par)))
                    (unless (symbol? (car par))
                      (err "Malformed ~s parameter: ~s." type par)))
                (unless (<= (length par) maxlen)
                  (err "Malformed ~s parameter: ~s." type par))
                par)
               (if (symbol? par)
                   (list par #f)
                   (err "Not a lambda parameter: ~s." par)))))
        (this #f)
        (head pars))
    (do ()
        ((null? pars) )
      (set! this (car pars))
      (set! pars (cdr pars))
      ;; recognize cltl2 or guile names
      (if (member this '(&optional &rest &key &aux &allow-other-keys))
          (cond ((eq? this '&optional)
                 (unless (eq? mode '&required)
                   (err "Malformed lambda list: ~s." head))
                 (set! mode '&optional))
                ((eq? this '&rest)
                 (unless (member mode '(&required &optional))
                   (err "Malformed lambda list: ~s." head))
                 (set! mode '&rest))
                ((eq? this '&key)
                 (unless (member mode '(&required &optional !rest))
                   (err "Malformed lambda list: ~s." head))
                 (set! mode '&key))
                ((eq? this '&allow-other-keys)
                 (unless (eq? mode '&key)
                   (err "Malformed lambda list: ~s." head))
                 (set! mode '&allow-other-keys)
                 (set! aok? #t))
                ((eq? this '&aux)
                 (set! mode '&aux)))
          (case mode
            ((&required )
             (if (not (symbol? this))
                 (err "Required argument not symbol: ~s" this))
             (push this reqs))
            ((&optional )
             (push (bind this mode 3) opts))
            ((&rest )
             (if (not (symbol? this))
                 (err "&rest argument not symbol: ~s" this))
             (push this rest)
             (set! mode '!rest))
            ((&key )
             (push (bind this mode 3) keys))
            ((&aux )
             (push (bind this mode 2) auxs))
            (else
             (err "Malformed lambda list: ~s." head)))))
    (values (reverse reqs)
            (reverse opts)
            rest                        ; only one
            (reverse keys)
            aok? 
            (reverse auxs))))

;;;
;;; cm-version
;;;   major:       incompatible (not backwards-compatible) API
;;;   minor:       backwards-compatible API change, i.e. "feature-add"
;;;   maintenance: no API change, bug fix only.

(define %cm-version% #x2B1)

(define (cm-version-number . arg)
  ;; return raw 3 byte version number or version list
  arg
  %cm-version%)

(define (cm-version-name)
  (format #f "~a.~a.~a" 
          (ldb (byte 4 8) %cm-version%)
          (ldb (byte 4 4) %cm-version%)
          (ldb (byte 4 0) %cm-version%)))

(define (cm-version . fmat)
  (cond ((null? fmat)
	 (format #f "Common Music ~a" (cm-version-name)))
	((not (null? (cdr fmat)))
	 (err "cm-version: more than one arg: ~s." fmat))
	((eq? (car fmat) ':number)
	 %cm-version%)
	((eq? (car fmat) ':string)
	 (cm-version-name))
	((eq? (car fmat) ':list)
	 (list (ldb (byte 4 8) %cm-version%)
	       (ldb (byte 4 4) %cm-version%)
	       (ldb (byte 4 0) %cm-version%)))
	(else (err "cm-version: Bad format: ~s." (car fmat)))))

;;;
;;; the ultimate in algorithmic iconic artware
;;;

(define *cm-logo* #t)

(define (cm-logo )
  (if *cm-logo*
    (begin
     (format #t "~%")
     (do ((e "~%")
          (v (make-string 15))
          (y 0 (+ y 1)))
         ((= y 7) #f)
       (format #t
               (do ((x 0 (+ x 1)))
                   ((= x 15)
                    (if (= y 3)
                      (string-append v " " (cm-version) e)
                      (string-append v e)))
                 (string-set! v x
                              (if (<= 2 (- x y) 4) #\\
                                  (if (= (- x (- 4 (modulo (+ 13 y) 15))) 1)
                                    #\/
                                    (if (<= 1 y 5) #\-
                                        (if (= (* (- x 6) (- y 3)) 15) #\/
                                            #\space))))))))
     (format #t "~%")))
  (values))

;;;
;;; string hacks
;;;

(define (string-substrings string . args)
  (with-args (args &key (delimiters '(#\space #\tab)) 
		   (start 0) (end (string-length string))
		   key)
    (let* ((pos1 start)
           (pos2 #f)
           (head (list #f))
           (tail head))
      (do ()
          ((> pos1 end) (cdr head))
        (set! pos2 (or (do ((i pos1 (+ i 1))
                            (f #f))
                           ((or f (= i end)) f)
                         (if (member (string-ref string i) delimiters)
                             (set! f i)))
                       end))
        (unless (= pos1 pos2)
          (set-cdr! tail 
                    (list (if key
                              (key (substring string pos1 pos2))
                              (substring string pos1 pos2))))
          (set! tail (cdr tail)))
        (set! pos1 (+ pos2 1))))))


; (string-substrings "A B :FOO (BASD     ASD) 123")

; (string-readable? "")
; (string-readable? "    ")
; (string-readable? "()")
; (string-readable? "())")
; (string-readable? "(()")
; (string-readable? "(\"1\")")
; (string-readable? "(\"1)")
; (string-readable? " 1 2 () 3   ")
; (string-readable? " \"bif buf\"   ")
; (string-readable? "#.foo")
; (string-readable? "'foo")
; (string-readable? "'(foo bar)")

(define (string-readable? string . args)
  ;; do some simple checks on strings before reading for lisp expressions.
  ;; return nil or the number of forms in string.
  (with-args (args &optional (start 0) (end (string-length string)))
    (do ((pos start (+ pos 1))
         (tok #f)
         (num 0)
         (lev 0)
         (str #f)
         (chr #f))
        ((not (< pos end))
         (if (and (= pos end) (= lev 0) (not str))
           (if tok (+ num 1) num)
           #f))
      (set! chr (string-ref string pos))
      (cond ((char=? chr #\()
             (if (= lev 0) (incf num))
             (incf lev)
             (set! tok #f))
            ((char=? chr #\))
             (decf lev)
             (if (< lev 0) (set! pos end))
             (set! tok #f))
            ((char=? chr #\") 
             (set! str (not str))
             (if (and (= lev 0) str) (incf num))
             (set! tok #f))
            ((member chr '(#\, #\`))
             ;; backquote is hopeless...
             (if (not str) (set! pos end)))
            ((member chr '(#\space #\return #\tab))
             (if (and tok (= lev 0) (not str))
               (incf num))
             (set! tok #f))
            ((member chr '(#\' #\#))
             (set! tok #f))
            (else (set! tok #t))))))

;;;
;;; string->expr reads one or more exprs from a string.
;;; returns two values, the expr(s) and an error flag that, if not nil
;;; is one of the following errorcodes

(define +se-nullstring+ 0)
(define +se-unreadable+ 1)
(define +se-multiple+   2)
(define +se-incorrect+  3)
(define +se-not-number+ 4)
(define +se-not-symbol+ 5)
(define +se-not-cons+   6)

(define (string->expr str . args)
  (with-args (args &key (read #t) (test #f)
                   (nullok #t) (multiok #f) errval)
    ;; parse input value from a gtk entry or a string
    (let ((text str)
          (trim '(#\space #\newline #\tab))
          (expr #f)
          (err? #f))
      (if (string=? text "")
        (if nullok (values #f #f) (values "" +se-nullstring+))
        (let ((len (string-length text))
              (raw text))
          (if (or (member (string-ref text 0) trim)
                  (member (string-ref text (- len 1)) trim))
            (set! text (strip-chars text))) ; remove whitespace
          (if (string=? text "")
            (if nullok (values #f #f) (values raw +se-nullstring+))
            (begin
              (if (not read)
                (begin (set! expr text) (set! err? #f))
                (if (not (eq? read #t))
                  (multiple-value-setq (expr err?) ( read text))
                  (let ((num (string-readable? text)))
                    (cond ((not num)
                           (set! expr text)
                           (set! err? +se-unreadable+))
                          ((= num 1)
                           ;; dont care about num
                           (multiple-value-setq (expr num)
                                                (string-read text))
                           (set! err? #f))
                          ((not multiok)
                           (set! expr text)
                           (set! err? +se-multiple+))
                          (else
                           (do ((n 0)
                                (x #f)
                                (l (list)))
                               ((eq? x ':eof)
                                (set! expr (reverse! l))
                                (set! err? #f))
                             (multiple-value-setq (x n)
                               (string-read text n))
                             (unless (eq? x ':eof)
                               (push x l))))))))
              (if err? 
                (values expr err?) 
                (if test
                  (if ( test expr) (values expr #f)
                      (values test (or errval +se-incorrect+)))
                  (values expr err?))))))))))

; (string->expr "")
; (string->expr "    ")
; (string->expr "(")
; (string->expr "()")
; (string->expr "1 2 3")
; (string->expr "(list 1 2 3)")
; (string->expr "1 2 3" :multiok t)

;;; accessing slot values

(define-macro (sv obj slot . args)
  (if (null? args)
    (slot-getter-form obj (if (keyword? slot) 
                              (keyword->symbol slot)
                              slot))
    (let ((o (gensym)))
      `(let ((,o ,obj))
         ,(slot-setter-form o (if (keyword? slot) (keyword->symbol slot) slot)
                            (car args))
         ,@ (if (null? (cdr args))
              (list)
              (let ((res '())) 
                (dopairs (x y (cdr args))
                  (push (slot-setter-form o (if (keyword? x)
                                                (keyword->symbol x)
                                                x)
                                          y) 
                        res))
                (reverse res)))
            ;(values)
            ))))

(define (svaux obj op slot val others)
  ;; args={slot val}+
  (let* ((ob (gensym))
         (args (list ob))
         (done #f))
    (do ()
        (done 
         `(let ((,ob ,obj))
            (sv ,@ args)))
      (append! args (list slot `(, op (sv ,ob ,slot) ,val)))
      (if (null? others)
          (set! done #t)
          (begin (set! slot (car others))
                 (set! val (cadr others))
                 (set! others (cddr others)))))))

(define-macro (sv+ obj slot val . more)
  (svaux obj '+ slot val more))

(define-macro (sv* obj slot val . more)
  (svaux obj '* slot val more))

;;;
;;; u8vector addition for both scheme and cltl
;;;

(define (u8vector-copy! vec1 vec2 . args)
  ;; args is (<vec1start> <vec2length>)
  (let ((p (if (null? args) 0 (car args)))
        (l (if (or (null? args) (null? (cdr args)))
             (u8vector-length vec2) (cadr args))))
    (do ((i p (+ i 1))
         (j 0 (+ j 1)))
        ((= j l) vec1)
      (u8vector-set! vec1 i (u8vector-ref vec2 j)))))

(define (u8vector-append . vecs)
  (let ((len 0))
    (for-each (lambda (x) (set! len (+ len (u8vector-length x)))) 
              vecs)
    (do ((tail vecs (cdr tail))
         (v (make-u8vector len 0))
         (p 0 (+ p l))
         (l #f))
        ((null? tail) v)
      (set! l (u8vector-length (car tail)))
      (u8vector-copy! v (car tail) p l))))

; (u8vector-append #() #(0 1) #(2 3) #(4 5 6) #())

(define (u8vector->uint vec)
  (logior (ash (u8vector-ref vec 0) 24)
          (ash (u8vector-ref vec 1) 16)
          (ash (u8vector-ref vec 2) 8)
          (u8vector-ref vec 3)))

(define (u8vector->int vec)
  (let ((int (u8vector->uint vec)))
    (if (> int (ash #xffffffff -1))
	(- int #xffffffff 1 )
      int)))
    
(define (u8vector->float vec)
  (let ((i (u8vector->uint vec)))
    (if (zero? i)
      0.0
      (let ((val (* (if (zero? (ash i -31)) 1.0 -1.0) 
                    (expt 2.0 (- (logand (ash i -23) #xff) 127)) 
                    (logior #x800000 (logand i #x7fffff)) 
                    (expt 2 -23))))
        val))))

(define (u8vector->string vec)
  (let* ((vec-len (u8vector-length vec))
         (str-list '()))
    (do ((i 0 (+ i 1))
         (j #f))
        ((or (= i vec-len) (equal? j 0)))
      (set! j (u8vector-ref vec i))
      (unless (= j 0)
        (set! str-list (append! str-list (list (integer->char j))))))
    (list->string str-list)))

;;; changed 5/13/05 tmi
;;;in guile at least the loop was not working properly
;;;because vector-length was being called and
;;;getting wrong-type-arg error with a u8vector
;;;
;; (define (u8vector->string vec)
;;   (list->string (loop for i across vec
;;                    until (= i 0)
;;                    collect (integer->char i))))



(define (u8vector-subseq vec beg . arg)
  (let* ((end (if (null? arg) (u8vector-length vec)
                  (car arg)))
         (sub (make-u8vector (- end beg))))
    (do ((i 0 (+ i 1))
         (j beg (+ j 1)))
        ((= j end) sub)
      (u8vector-set! sub i (u8vector-ref vec j)))))


