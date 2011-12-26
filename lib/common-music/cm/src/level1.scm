;;; **********************************************************************
;;; Copyright (C) 2001 Heinrich Taube (taube@uiuc.edu) 
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
;;; $Revision: 1.21 $
;;; $Date: 2005/12/03 13:45:29 $

;;; level1 for scheme. most of this is either cltl functionality or
;;; noop expansions for generated cl code.

;;;
;;; scheme noops that are either needed to generate cl code or for cl
;;; compile/loading

(define-macro (function fn) fn)

(define (funcall fn . args)
  (apply fn args))

(define (in-package name) #f)

(define-macro (eval-when decl . body) ; noop in scheme
  (if (null? (cdr body))
    (car body)
    `(begin ,@ body)))

;;;
;;; useful cltl features.

(define-macro (when test . forms)
  `(if ,test (begin ,@forms)))

(define-macro (unless test . forms)
  `(if (not ,test)
       (begin ,@forms)))

(define-macro (multiple-value-bind vars form . body)
  `(call-with-values
    (lambda () ,form)
    (lambda (,@vars) ,@body)))

(define-macro (multiple-value-list form)
    `(call-with-values
      (lambda () , form)
      (lambda args args)))

(define-macro (multiple-value-setq vars form)
  (let ((lst (map (lambda (x) (gensym (symbol->string x))) 
		  vars)))
    `(call-with-values 
      (lambda () ,form)
      (lambda (,@lst)
	,@(map (lambda (x y) `(set! ,x ,y))
	       vars lst)))))

(define-macro (push val sym)
  `(begin (set! ,sym (cons ,val ,sym)) ,sym))

(define-macro (pop sym)
  (let ((v (gensym)))
    `(let ((,v (car ,sym)))
       (set! ,sym (cdr ,sym))
       ,v)))

(define-macro (incf sym . val)
  `(begin
    (set! ,sym (+ ,sym ,(if (null? val) 1 (car val))))
    ,sym))

(define-macro (decf sym . val)
  `(begin
    (set! ,sym (- ,sym ,(if (null? val) 1 (car val))))
    ,sym))

(define-macro (rotatef sym1 sym2)
  (let ((v (gensym)))
    `(let ((,v ,sym1))
      (set! ,sym1 ,sym2)
      (set! ,sym2 ,v))))

(define-macro (cdr-pop x)
    ;; an equivalent for (pop (cdr ..)) 
  (let ((h (gensym))
	(t (gensym))
	(v (gensym)))
    `(let* ((,h ,x)
	    (,t (cdr ,h))
	    (,v (car ,t)))
      (set-cdr! ,h (cdr ,t))
      ,v)))

(define-macro (dolist spec . body)
  ;; spec = (var list . return)
  (let ((v (gensym)))
    `(do ((,v ,(cadr spec) (cdr ,v))
          (,(car spec) #f))
         ((null? ,v) ,@ (cddr spec))
       (set! ,(car spec) (car ,v))
       ,@body)))

(define-macro (dotimes spec . body)
  ;; spec = (var end . return)
  (let ((e (gensym))
        (n (car spec)))
    `(do ((,e ,(cadr spec))
          (,n 0))
         ((>= ,n ,e) ,@ (cddr spec))
       ,@body
      (set! ,n (+ ,n 1)))))

;;;
;;; true and false provided for example portability
;;;

(define true #t)
(define false #f)

;;;
;;; list operations not in srfi-1
;;;

(define rest cdr)

(define (list-position x lis . arg)
  (let ((test (if (null? arg) (function eq?) (car arg))))
    (do ((tail lis (cdr tail))
         (indx 0 (+ indx 1))
         (flag #f))
        ((or (null? tail) flag) flag)
      (if ( test x (car tail)) (set! flag indx)))))

(define (copy-tree lis)
  (if (pair? lis)
    (cons (copy-tree (car lis))
          (copy-tree (cdr lis)))
    lis))

(define (butlast lis)
  (if (or (null? lis) 
          (null? (cdr lis)))
    '()
    (let ((h (list '())))
      (do ((l h))
          ((null? (cdr lis))
           (cdr h))
        (set-cdr! l (list (car lis)))
        (set! l (cdr l))
        (set! lis (cdr lis))))))


;;;
;;; property list getting and setting
;;;

(define (list-prop lis prop . def)
  (if (null? lis)
      (if (null? def) #f (car def))
      (if (eq? (car lis) prop)
          (cadr lis)
          (apply list-prop (cddr lis) prop def))))

(define (list-prop-set! lis prop val)
  (if (eq? (car lis) prop)
      (set-car! (cdr lis) val)
      (if (null? (cddr lis))
          (set-cdr! (cdr lis) (list prop val))
          (list-prop-set! (cddr lis) prop val))))
  
;;;
;;; define-list-struct defines a list struct implemented by a
;;; constructor and getters/setters for each slot:
;;; (define-list-struct foo a (b 1)) 
;;; => (make-foo &key a b)
;;;    (foo-a s) (foo-b s) (foo-a-set! s v) (foo-b-set! s v)
;;;

(define-macro (define-list-struct name . slotspecs)
  (expand-list-struct name slotspecs))

(define (expand-list-struct name slotspecs)
  (letrec ((tailform
            (lambda (pos)
              (case pos
                ((0) 'arg)
                ((1) '(cdr arg))
                ((2) '(cddr arg))
                ((3) '(cdddr arg))
                ((4) '(cddddr arg))
                (else `(list-tail arg ,pos)))))
           (setslotform 
            (lambda (pos) `(set-car! ,(tailform pos) val)))
           (getslotform 
            (lambda (pos) `(car ,(tailform pos))))
           (fillslotform
            (lambda (slot)
              (let ((value #f))
                (when (pair? slot)
                  (set! value (cadr slot))
                  (set! slot (car slot)))
                `(list-prop args ',(symbol->keyword slot) ,value))))
           (make-name
            (lambda (str1 . strs)
              (string->symbol (apply string-append str1 strs)))))

  (let  ((name (symbol->string name))
         (slots (map (lambda (s) (if (symbol? s) s (car s)))
                    slotspecs)))
    `(begin
      ;; create constructor
       (define (, (make-name "make-" name) . args)
        , (cons 'list (map fillslotform slotspecs)))
      ;; create accessor functions
       ,@ (do ((i 0 (+ i 1))
	       (l '())
	       (s slots (cdr s)))
	      ((null? s) (reverse l))
	    (push `(define (,(make-name name "-" 
					 (symbol->string (car s)))
			     arg)
		     ,(getslotform i))
		   l))
      ;; create setter functions
      ,@ (do ((i 0 (+ i 1))
	       (l '())
	       (s slots (cdr s)))
	      ((null? s) (reverse l))
	    (push `(define (,(make-name name "-"
					 (symbol->string (car s))
					 "-set!") 
			     arg val)
		     ,(setslotform i))
		   l))))))

;;;
;;; hash tables
;;;

(define (hash-clear! tabl)
  (hash-fold (lambda (k v r)
               (hash-remove! tabl k)
               r)
             #t
             tabl))


;(define-macro (ecase datum . rest)
;  (let ((var (gensym))
;        (keys '())
;        (case '()))
;    (for-each (lambda (x)
;                (if (pair? (car x))
;                  (begin (push (car x) keys)
;                         (push x case))
;                  (let ((e (list (car x))))
;                    (push e keys)
;                    (push `(,e ,@ (cdr x)) case))))
;              rest)
;    `(let ((,var ,datum))
;       (case ,var
;         ,@ case
;         (else
;          (err "case: key ~s is not one of the expected keys: ~s"
;	       ,var ',(apply append keys)))))))

;;;
;;; Numbers and bit twiddling. requires ash, logand, logior, lognot
;;;

(define pi 3.141592653589793)

;;; common lisp floor and round. 

(define (clfloor n . arg)
  (if (null? arg)
    (let ((v (floor n)))
      (values (inexact->exact v) (- n v)))
    (let* ((d (car arg))
	   (v (/ n d))
	   (i (inexact->exact (floor v)))
	   (r (- n (* i d))))
      (values i r))))

(define (clround n . arg)
  (if (null? arg)
    (let ((v (round n)))
      (values (inexact->exact v) (- n v)))
    (let* ((d (car arg))
	   (v (/ n d))
	   (i (inexact->exact (round v)))
	   (r (- n (* i d))))
      (values i r))))

(define (mod num div)
  (if (and (exact? num)
           (exact? div))
    (modulo num div)
    (let* ((res (/ num div))
           (flo (floor res)))
      (- num (* flo div)))))

(define (rem num div)
  (if (and (exact? num)
           (exact? div))
    (remainder num div)
    (let* ((res (/ num div))
           (flo (truncate res)))
      (- num (* flo div)))))

(define %log2 (log 2))

(define (log2 n) (/ (log n) %log2))

(define (logn num base) (/ (log num) (log base)))

(define (signum n)
  (cond ((= n 0) 0)
        ((< n 0) -1)
        (else +1)))

;;;
;;; byte spec
;;;

(define (byte siz pos)
  ;; cache size, position and mask.
  (vector siz pos (ash (- (expt 2 siz) 1) pos)))

(define (byte-size bytespec)
  (vector-ref bytespec 0))

(define (byte-position bytespec)
  (vector-ref bytespec 1))

(define (byte-mask bytespec)
  (vector-ref bytespec 2))

(define (ldb bytespec integer)
  (ash (logand integer (byte-mask bytespec))
       (- (byte-position bytespec))))

(define (dpb integer bytespec into)
  (let ((val (logand integer (ash (byte-mask bytespec) 
                                  (- (byte-position bytespec))))))
    (logior (logand into (lognot (byte-mask bytespec)))
	    (ash val (byte-position bytespec)))))

;;;
;;; cltl2 lambda parameters
;;;
;;;       
;;; with-args (list . decl) . body)
;;; binds variables to values from a list according to cltl2's lambda
;;; parameter declaration syntax. any &optional, &key &rest and &aux
;;; parameters without default values are initiaized to #f. Example: 
;;; cltl2:
;;; (defun foo (a b &optional (c 3) &key d (e a) &aux (f -99))
;;;   (list a b c d e f))
;;; scheme:
;;; (define (foo . args) 
;;;   (with-args (args a b &optional (c 3) &key d (e a) &aux (f a)) 
;;;     (list a b c d e f)))
;;;
;;; (foo 0 1 2 :d 3 :e 4)
;;;

(define-macro (with-args spec . body) 
  ;; spec is (list . lambda-decl)
  (let ((args (gensym ))
        (reqs '())
        (opts '())
        (rest '())
        (keys '())
        (auxs '())
        (aok? #f)			; allow-other-keys
        (vars #f)
        (setk #f)
        (keyc #f)
        (seta #f))

    ;; parse-lambda-list defined in utils.scm...
    (multiple-value-setq (reqs opts rest keys aok? auxs )
                         (parse-lambda-list (cdr spec)))
    ;; each &key entry is represented by a four element list:
    ;; (var default passed? keyword) where var is the variable,
    ;; default is its default value, passed? is a flag set to
    ;; #t if key is passed and keyword is the keyword.
    (do ((tail keys (cdr tail))
         (head (list))
         (b #f)
         (v #f)
         (k #f)
         (l #f))
        ((null? tail)
         (set! keys (reverse! head)))
      (set! b (car tail))
      ;; binding b is ( {var | (key var)} val [var2])
      (cond ((pair? (car b))
             (set! k (caar b))
             (set! v (cadar b)))
            (else
             (set! v (car b))
             (set! k (symbol->keyword v))))
      (set! l (length b)) ;; 2 or 3
      (cond ((= l 2)
             (push (list v (cadr b) (gensym ) k) head))
            ((= l 3)
             (push (list v (cadr b) (caddr b) k) head))
            (else (err "Malformed &key binding: ~s" b))))
    ;; create required arg bindings
    (set! reqs (map (lambda (r)		; r is required par
                      (unless (symbol? r)
                        (err "Required arg not symbol: ~s" r))
		      (let ((v (gensym )))
			`(,r (if (null? ,args)
			       (err "Missing value for required arg ~s"
				    ',r)
			       (let ((,v (car ,args)))
				 (set! ,args (cdr ,args))
				 ,v)))))
		reqs))
    ;; create optional args bindings. optimize the common case of a
    ;; single optional arg
    (set! opts (if (and (null? rest) (null? keys)
			(= (length opts) 1))
		 ;; skip the let and cdring if single optional arg
		 `((, (car (car opts))
                      (if (null? ,args) ,(cadr (car opts)) (car ,args))))
		 (map (lambda (b)
			;; b is (<var> <val>)
			(let ((v (gensym )))
			  `(,(car b)
                             (if (null? ,args)
                               ,(cadr b)
                               (let ((,v (car ,args)))
                                 (set! ,args (cdr ,args))
                                 ,v)))))
		      opts)))
    ;; vars is list of all parameter var bindings.
    (set! vars
          (append! reqs
                   opts
                   ;; rest arg is already a binding. hmmm is this true?
                   rest
                   ;; bind all keyword vars, key-exist flags and aux vars to false
                   (map (lambda (b) (list (car b) #f)) keys)
                   (map (lambda (b) (list (caddr b) #f)) keys)
                   (map (lambda (b) (list (car b) #f)) auxs)))
    ;; setk is a list of setting forms for setting default keyword
    ;; values after keyword processing.
    (set! setk
          (apply append
                 (map (lambda (b) 
                        ;; b is (<var> <val> <v?>) only set <var> to
                        ;; <val> if <v?> is #f, ie user didnt pass the
                        ;; arg
                        (if (eq? (cadr b) #f)
                          (list)
                          `((if (not ,(caddr b))
                              (set! ,(car b) ,(cadr b))))))
                      keys)))
    ;; keyc is a list of case clauses for each keyword:
    ;; ((<keyword>) (set! <var> (cadr <args>) (set! <flag> #t)))
    (set! keyc (map (lambda (b)
                      ;; b is (<var> <val> <flag> <keyw>)
                      `((,(cadddr b) )
                        (set! ,(car b) (cadr ,args))
                        (set! ,(caddr b) #t)))
                    keys))
    ;; seta is a list of aux var setting forms. spliced in just before
    ;; body of with-args
    (set! seta
          (apply append (map (lambda (b)
			      (if (eq? (cadr b) #f)
				(list)
				`((set! ,(car b) ,(cadr b)))))
			auxs)))
    ;; let* so lambda param bindings can reference earlier ones
    `(let* ((,args ,(car spec))
	    ;; splice in all var bindings
	    ,@ vars)
       ;; splice in keyword processing loop.
       ,@ 
       (if (pair? keys)
         (let ((head (gensym)))
           ;; this do loop parses keyword args, each keword has its
           ;; own case clause. signals error for incorrect keys.
           `((do ((,head ,args))
		 ((null? ,args) 
                  ;; loop termination clause sets default values for
                  ;; all keys that were not passed in args and whose
                  ;; default value is not #f.
                  ,@ setk
                  )
               ;; do actions. first is make sure a value exists for
               ;; each keyword
               (if (null? (cdr ,args))
                 (err "Args not keyword format: ~s." ,head))
               ;; current keyword must match a case clause else its bogus
               (case (car ,args)
                 ,@ keyc
                    ;; splice in error trap unless &allow-other-keys.
                    ;; error message includes list of valid keywords.
                    ,@
                    (if (not aok?)
                      `((else
                         (err 
                          "Illegal keyword '~s' in: ~s.~%Valid keywords: ~s"
                          (car ,args) ,head ',(map cadddr keys))))
                      (list)))
               (set! ,args (cddr ,args)))))
         (list))
       ;; spice in &aux params if default value not #f.
       ,@seta
       ;; splice in body of with-args.
       ,@body)))

; (define pprint display)
; (let ((l '(22 2 3))) (with-args (l a b c) (list a b c)))
; (let ((l '()) ) (with-args (l &optional (a 0) b c) (list a b c)))
; (let ((l '(100)) ) (with-args (l  &optional a (b 5) c) (list a b c)))
; (let ((l '(:a 100 :b 200 :c 300))) (with-args (l &key  (a 0) b c) (list a b c)))
; (pprint (macroexpand '(with-args (l &key (a 0) b c) (list a b c))))
; (let ((l '()) ) (with-args (l &key (a 0) b c) (list a b c)))
;;errors:
; (let ((l '(:c 200 b -9)) ) (with-args (l a (b 5) c) (list a b c)))
; (let ((l '(:c 200 z -9)) ) (with-args (l &key a (b 5) c) (list a b c)))
; (let ((l '(1 :c 100)) ) (with-args (l &key a (b 5) c) (list a b c)))
; (let ((l '(:c)) ) (with-args (l &key a (b 5) (c b)) (list a b c)))

;;;
;;; find, position, find-if, position-if, find-if-not, position-if-not
;;; for list strings and vectors
;
;(define (find-aux mode obj seq test key start end from-end)
;  (let ((lim (if from-end < >=))
;	(inc (if from-end - +))
;	(get #f))
;    (cond ((vector? seq)
;           (set! get vector-ref)
;           (set! end (or end (vector-length seq))))
;          ((list? seq)
;           (set! get list-ref)
;           (set! end (or end (length seq))))
;          ((string? seq)
;           (set! get string-ref)
;           (set! end (or end (string-length seq))))
;          (else
;           (err "~s is not a vector, pair or string." seq)))
;
;    (do ((i (if from-end (- end 1) start))
;	 (z (if from-end start end))
;         (j #f)
;         (k #f))
;        ((or k (lim i z)) k)
;      (set! j (get seq i))
;      (if (test obj (key j))
;        (if (eq? mode 'find) (set! k j) (set! k i))
;	)
;      (set! i (inc i 1)))))
;
;(define (find obj seq . args)
;  (with-args (args &key (test eq?) (key identity) (start 0) end
;                   from-end )
;    (find-aux 'find obj seq test key start end from-end)))
;
;(define (position obj seq . args)
;  (with-args (args &key (test eq?) (key identity) (start 0) end
;                   from-end )
;    (find-aux 'position obj seq test key start end from-end)))
;(define (find-if fn seq . args) (apply find #t seq ':test (lambda (x y) (if (fn y) #t #f)) args))
;(define (find-if-not fn seq . args) (apply find #f seq ':test (lambda (x y) (if (fn y) #t #f)) args))
;(define (position-if fn seq . args) (apply position #t seq ':test (lambda (x y) (if (fn y) #t #f)) args))
; (find 1 '(a b c 1 2 3))
; (position-if (lambda (x) (eq? x 1)) '(a b v 1 2))
; (find 1 #(a b c 1 2 3))
; (find #\1 "abc123")
; (find 1 '((a 1) (b 2) (c 2) (1 a) (2 b) (3 c)) ':key cadr)
; (find 9 '((a 1) (b 2) (c 2) (1 a) (2 b) (3 c)) ':key cadr)
; (find #\1 "")
; (find 1 #())
; (find 1 '())
; (position 1 '((a a) (b b) (c c) (1 1) (2 2) (3 3))  ':key car ':from-end #t)

(define (strip-chars str . args)
  (let ((chars (if (null? args) '(#\space #\tab #\return)
                   (car args))))
    (string-trim-both str (lambda (c) (member c chars)))))

(define (string-read str . args)
  ;; args is: start eoftok
  (let ((len (string-length str))
        (beg (if (null? args) 0 (car args)))
        (eof (if (or (null? args) (null? (cdr args)))
               ':eof
               (car (cdr args)))))
    (call-with-input-string 
     str
     (lambda (sp) ; string port
       ;; advance to starting pos
       (do ((p 0 (+ p 1)))
           ((not (< p beg)) #f)
         (read-char sp))
       (if (not (< beg len))
         (values eof 0)
         (let ((val (read sp)))
           (values (if (eof-object? val) eof val)
                   (port-position sp))))))))

;;;
;;; unix filename twiddling. filenames are just strings.
;;; the level0 files must set the directory character.
;;;

(define (namestring p) p)

(define (filename p) p)

(define (filename-charpos str char)
  (do ((i (- (string-length str) 1) (- i 1))
       (d #f))
      ((or d (< i 0)) d)
    (if (char=? (string-ref str i) char)
      (set! d  i))))

(define (filename-directory file)
  (let ((dir (filename-charpos file #\/)))
    (if dir
      (substring file 0 (+ dir 1))
      #f)))

(define (filename-name file)
  (let ((dir (or ;;(position directory-delimiter  file :from-end #t)
              (filename-charpos file #\/)
              -1))
	(dot (or ;;(position #\. file ':from-end #t)
              (filename-charpos file #\.)
              (string-length file))))
    ;; name from dir+1 to dot-1
    (if (= dot (+ dir 1))
      (substring file dot (string-length file))
      (if (> dot (+ dir 1))
	(substring file (+ dir 1) dot)
	#f))))

(define (filename-type file)
  (let ((dot ;;(position #\. file :from-end #t)
         (filename-charpos file #\.)
          ))
    (if dot
      (let ((dir (or ;;(position directory-delimiter file ':from-end #t)
                  (filename-charpos file #\/)
                  -1))
	    (len (string-length file)))
	(if (and dot (< dir (- dot 1) dot (- len 1)))
	  (substring file (+ dot 1) len)
	  #f))
      #f)))

(define (merge-filenames p1 p2)
  (let ((pd (filename-directory p1))
	(pn (filename-name p1))
	(pt (filename-type p1)))
    (if (not pd)
      (set! pd (filename-directory p2)))
    (if (not pn)
      (set! pn (filename-name p2)))
    (if (not pt)
      (set! pt (filename-type p2)))
    (apply string-append (or pd "") (or pn "") 
	   (if pt (list "." pt) '()))))

(define (open-file name direction . type)
  (if (eq? direction :output)
    (open-output-file name)
    (open-input-file name)))

(define (close-file fp dir) 
  (if (eq? dir :output)
    (close-output-port fp)
    (close-input-port fp)))

(define (file-form fil)
  (read fil))

(define (file-line fil)
  (read-line fil))

(define (file-eof? x) (eof-object? x))

;(define-macro (with-open-output-file args . body)
;  (let ((var (car args)))
;    `(let ((,var (open-output-file ,(cadr args))))
;       (dynamic-wind (lambda () #f)
;                     (lambda () ,@body 
;                             )
;                     (lambda () (close-output-port ,var))))))
;
;(define-macro (with-open-input-file args . body)
;  (let ((var (car args)))
;    `(let ((,var (open-input-file ,(cadr args))))
;       (dynamic-wind (lambda () #f)
;                     (lambda () ,@body ;(close-input-port ,var)
;                             )
;                     (lambda () (close-input-port ,var))))))

;;;
;;; Scheme expansion for defobject
;;;

(define (expand-defobject name gvar supers decl pars methods streams)
  ;; slots must be parsed into goops format.
  (let ((slts (map (lambda (s) (parse-slot-spec name s)) decl))
        (opts (if (or pars (not (null? streams)))
                `(:metaclass <parameterized-class>
;                             :event-streams (quote ,streams)
;                             :parameters (quote ,pars)
                             )
                '())))
    `(begin 
      (define-class* ,gvar ,(map class-name->class-var supers)
	, slts :name ',name ,@ opts )
      
      ,@(if (null? pars) (list)
            `((set! (class-parameters ,gvar) (quote ,pars))))
      ,@(if (null? streams) (list)
            `((set! (class-event-streams ,gvar) (quote ,streams))))
      
      (define-method* (make-load-form (obj ,gvar))
        (cons* 'make ', gvar (slot-init-forms obj :eval #t)))
      ,@methods
      (values))))

(define (parse-slot-spec cname spec)
  (let ((acc (lambda (b)
	       (string->symbol (format #f "~a-~a" cname b))))
	(key (lambda (b) (symbol->keyword b)))
	(val #f)
	(name (if (pair? spec) (car spec) spec))
	(spec (if (pair? spec) (list-copy (cdr spec)) (list))))
    
    ;; convert :initarg to :init-keyword. in cltl the slot
    ;; can have any number of initargs and initargs can
    ;; be symbols or keywords. guile only supports a single
    ;; keyword initarg.
    (set! val (memq ':initarg spec))
    (if val
      (do ((tail val (memq ':initarg tail))
           (sofar '() sofar)
           (key #f))
          ((not tail)  #f)
        (if (keyword? (cadr tail))
          (set! key (cadr tail))
          (set! key (and (cadr tail) ;; not #f
                         (symbol->keyword (cadr tail)))))
        ;; remove duplicate initarg or ':initarg #f'
        (if (or (memq key sofar)
                (not key))
          (begin
           ; remove warning
           ;(when (cadr tail)
           ;  (warning "Ignoring duplicate initarg for ~a." 
           ;           name))
           ;; remove from spec. 
           ;(format #t "~%spec=~s tail=~s" spec tail)
           (if (eq? spec tail)
             (begin (set! spec (cddr spec))
                    (set! tail spec))
             (do ((edit spec (cdr edit)))
                 ((eq? (cdr edit) tail)
                  (set-cdr! edit (cdddr edit))))))
          (begin (set-car! tail ':init-keyword)
                 (set-car! (cdr tail) key)
                 (push key sofar)))
        (set! tail (cddr tail)))
      (set! spec (cons ':init-keyword (cons (key name) spec))))
    ;; add accessor if not supplied
    (unless (memq ':accessor spec)
      (set! spec (cons ':accessor (cons (acc name) spec))))
    ;; convert :initform to :init-value
    (set! val (memq ':initform spec))
    (when val
      (set-car! val ':init-value))
    (cons name spec)))

;;;
;;; scheme expansion for write-event
;;;

(define (define-output-method objclassname objclassvar objvar
          fileclassname fileclassvar
          filevar timevar body)
  `(define-method* (write-event (,objvar ,objclassvar)
                               (,filevar ,fileclassvar)
                               ,timevar)
     ,@body))

;;;
;;; Scheme expansion for process macro
;;;

(define (process-stop expr)
  ;; stopprocess is lexical var holding continuation 
  ;; return false
  '(stopprocess #f))

(define (expand-process forms ops)
  (let ((parsed (parse-iteration 'process forms ops))
	(code '())
	(func #f)
	(tests '())
	(done #f))
    (set! tests (loop-end-tests parsed))
    (set! done (process-stop #f))
    (if (loop-finally parsed)
      (set! done `(begin ,@(loop-finally parsed) ,done)))
    (if (not (null? tests))
      (begin
       (if (null? (cdr tests))
	 (set! tests (car tests))
	 (set! tests (cons 'or tests)))
       (set! tests `((if ,tests ,done))))
      (unless (process-code-terminates? (loop-looping parsed)
					(process-stop #f))
	(format #t "Warning: A non-terminating process may have been defined.")
        ))
    (set! func `(lambda ()
		  (call-with-current-continuation
		   (lambda (stopprocess)
		     ,@ tests
		        ,@ (loop-looping parsed)
		           ,@ (loop-stepping parsed)
                              ;;(enqueue *process* *qnext* *qstart* )
		              #t
                              ))))
    (if (and (null? (loop-bindings parsed))
	     (null? (loop-initially parsed)))
      func
      ;; use let* sequential binding
      `(let* ,(loop-bindings parsed)
	 ,@(loop-initially parsed)
	 ,func))))

(define (expand-defprocess forms)
  (let ((args (second forms)))
    (if (not (list? args))
      (err "defprocess arguments not list: ~S" args))
    `(define (,(first forms) ,@args) 
       ,@(cddr forms))))

;;;
;;; scheme expansion for make-midi-message-set!
;;;

(define (make-midi-message-set! getter bytespec)
  (let ((setter (string->symbol
                 (string-append (symbol->string getter)
                                "-set!"))))
    `(define-macro (,setter message value)
       (if (symbol? message)
         (let ((val (gensym)))
           `(let ((,val ,value )) ;
              (set! ,message (dpb ,val ,',bytespec ,message))
              ,val))
         `(dpb ,value ,',bytespec ,message)))))

;;;
;;;
;;;

(define (cm . verbose)
  ;; a no-op for now, 
  (if (or (null? verbose)
          (not (eq? (car verbose) #f)))
    (cm-logo))
  (values))

;;;
;;; u8
;;;

(define (u8vector-write vec fd)
  (do ((i 0 (+ i 1))
       (e (u8vector-length vec)))
      ((= i e) vec)
    (write-byte (u8vector-ref vec i) fd)))


