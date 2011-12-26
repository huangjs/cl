;Generated from Scheme source by scm2cl, v. 0k1,
;(c) Dorai Sitaram, http://www.cs.rice.edu/~dorai/scm2cl

(defparameter *pregexp-comment-char* #\;)

(defparameter *pregexp-nul-char-int* (- (char-code #\a) 97))

(defparameter *pregexp-return-char* (code-char (+ 13 *pregexp-nul-char-int*)))

(defparameter *pregexp-tab-char* (code-char (+ 9 *pregexp-nul-char-int*)))

(defparameter *pregexp-space-sensitive?* t)

(defun pregexp-reverse! (s)
  (let ((s s) (r 'nil))
    (flet ((loop! (s r) (throw 'loop! (values s r))))
      (loop
        (multiple-value-setq (s r)
          (let ((s s) (r r))
            (catch 'loop!
              (return (if (null s) r (let ((d (cdr s))) (rplacd s r) (loop! d s))))
) ) ) ) ) ) )

(defun pregexp-error (where &rest what) (princ "Error: ") (princ where) (princ ": ")
  (mapc #'princ what) (terpri) (error)
)

(defun pregexp-read-pattern (s i n)
  (if (>= i n) (list (list ':or (list ':seq)) i)
    (let ((branches 'nil) (i i))
      (flet ((loop! (branches i) (throw 'loop! (values branches i))))
        (loop
          (multiple-value-setq (branches i)
            (let ((branches branches) (i i))
              (catch 'loop!
                (return
                  (if (or (>= i n) (char= (char s i) #\)))
                    (list (cons ':or (pregexp-reverse! branches)) i)
                    (let
                      ((vv (pregexp-read-branch s (if (char= (char s i) #\|) (+ i 1) i) n)))
                      (loop! (cons (car vv) branches) (cadr vv))
) ) ) ) ) ) ) ) ) ) )

(defun pregexp-read-branch (s i n)
  (let ((pieces 'nil) (i i))
    (flet ((loop! (pieces i) (throw 'loop! (values pieces i))))
      (loop
        (multiple-value-setq (pieces i)
          (let ((pieces pieces) (i i))
            (catch 'loop!
              (return
                (if (>= i n) (list (cons ':seq (pregexp-reverse! pieces)) i)
                  (if (let ((c (char s i))) (or (char= c #\|) (char= c #\))))
                    (list (cons ':seq (pregexp-reverse! pieces)) i)
                    (if t
                      (let ((vv (pregexp-read-piece s i n)))
                        (loop! (cons (car vv) pieces) (cadr vv))
) ) ) ) ) ) ) ) ) ) ) )

(defun pregexp-read-piece (s i n)
  (let ((c (char s i)))
    (case c ((#\^) (list ':bos (+ i 1))) ((#\$) (list ':eos (+ i 1)))
      ((#\.) (pregexp-wrap-quantifier-if-any (list ':any (+ i 1)) s n))
      ((#\[)
        (pregexp-wrap-quantifier-if-any
          (case (char s (+ i 1))
            ((#\^)
              (let ((vv (pregexp-read-char-list s (+ i 2) n)))
                (list (list ':neg-char (car vv)) (cadr vv))
            ) )
            (t (pregexp-read-char-list s (+ i 1) n))
          )
          s n
      ) )
      ((#\() (pregexp-wrap-quantifier-if-any (pregexp-read-subpattern s (+ i 1) n) s n))
      ((#\\)
        (pregexp-wrap-quantifier-if-any
          (let ((__test__ (pregexp-read-escaped-number s i n)))
            (if __test__
              (funcall #'(lambda (num-i) (list (list ':backref (car num-i)) (cadr num-i)))
                __test__
              )
              (let ((__test__ (pregexp-read-escaped-char s i n)))
                (if __test__
                  (funcall #'(lambda (char-i) (list (car char-i) (cadr char-i))) __test__)
                  (if t (pregexp-error 'pregexp-read-piece "backslash"))
          ) ) ) )
          s n
      ) )
      (t
        (if
          (or *pregexp-space-sensitive?*
            (and
              (not
                (let ((|Scheme-to-CL-1| c))
                  (or (char= |Scheme-to-CL-1| #\space) (char= |Scheme-to-CL-1| #\tab)
                    (not (graphic-char-p |Scheme-to-CL-1|))
              ) ) )
              (not (char= c *pregexp-comment-char*))
          ) )
          (pregexp-wrap-quantifier-if-any (list c (+ i 1)) s n)
          (let ((i i) (in-comment? nil))
            (flet ((loop! (i in-comment?) (throw 'loop! (values i in-comment?))))
              (loop
                (multiple-value-setq (i in-comment?)
                  (let ((i i) (in-comment? in-comment?))
                    (catch 'loop!
                      (return
                        (if (>= i n) (list ':empty i)
                          (let ((c (char s i)))
                            (if in-comment? (loop! (+ i 1) (not (char= c #\newline)))
                              (if
                                (let ((|Scheme-to-CL-2| c))
                                  (or (char= |Scheme-to-CL-2| #\space)
                                    (char= |Scheme-to-CL-2| #\tab)
                                    (not (graphic-char-p |Scheme-to-CL-2|))
                                ) )
                                (loop! (+ i 1) nil)
                                (if (char= c *pregexp-comment-char*) (loop! (+ i 1) t)
                                  (if t (list ':empty i))
) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )

(defun pregexp-read-escaped-number (s i n)
  (and (< (+ i 1) n)
    (let ((c (char s (+ i 1))))
      (and (digit-char-p c)
        (let ((i (+ i 2)) (r (list c)))
          (flet ((loop! (i r) (throw 'loop! (values i r))))
            (loop
              (multiple-value-setq (i r)
                (let ((i i) (r r))
                  (catch 'loop!
                    (return
                      (if (>= i n)
                        (list
                          (let
                            ((|Scheme-to-CL-3| (concatenate 'string (pregexp-reverse! r))))
                            (if (position #\: |Scheme-to-CL-3| :test #'char=) nil
                              (let
                                ((|Scheme-to-CL-4| (read-from-string |Scheme-to-CL-3| nil)))
                                (if (numberp |Scheme-to-CL-4|) |Scheme-to-CL-4| nil)
                          ) ) )
                          i
                        )
                        (let ((c (char s i)))
                          (if (digit-char-p c) (loop! (+ i 1) (cons c r))
                            (list
                              (let
                                ((|Scheme-to-CL-5|
                                    (concatenate 'string (pregexp-reverse! r))
                                ))
                                (if (position #\: |Scheme-to-CL-5| :test #'char=) nil
                                  (let
                                    ((|Scheme-to-CL-6|
                                        (read-from-string |Scheme-to-CL-5| nil)
                                    ))
                                    (if (numberp |Scheme-to-CL-6|) |Scheme-to-CL-6| nil)
                              ) ) )
                              i
) ) ) ) ) ) ) ) ) ) ) ) ) ) )

(defun pregexp-read-escaped-char (s i n)
  (and (< (+ i 1) n)
    (let ((c (char s (+ i 1))))
      (case c ((#\b) (list ':wbdry (+ i 2))) ((#\B) (list ':not-wbdry (+ i 2)))
        ((#\d) (list ':digit (+ i 2))) ((#\D) (list '(:neg-char :digit) (+ i 2)))
        ((#\n) (list #\newline (+ i 2))) ((#\r) (list *pregexp-return-char* (+ i 2)))
        ((#\s) (list ':space (+ i 2))) ((#\S) (list '(:neg-char :space) (+ i 2)))
        ((#\t) (list *pregexp-tab-char* (+ i 2))) ((#\w) (list ':word (+ i 2)))
        ((#\W) (list '(:neg-char :word) (+ i 2))) (t (list c (+ i 2)))
) ) ) )

(defun pregexp-read-posix-char-class (s i n)
  (let ((neg? nil))
    (let ((i i) (r (list #\:)))
      (flet ((loop! (i r) (throw 'loop! (values i r))))
        (loop
          (multiple-value-setq (i r)
            (let ((i i) (r r))
              (catch 'loop!
                (return
                  (if (>= i n) (pregexp-error 'pregexp-read-posix-char-class)
                    (let ((c (char s i)))
                      (if (char= c #\^) (progn (setq neg? t) (loop! (+ i 1) r))
                        (if (alpha-char-p c) (loop! (+ i 1) (cons c r))
                          (if (char= c #\:)
                            (if (or (>= (+ i 1) n) (not (char= (char s (+ i 1)) #\])))
                              (pregexp-error 'pregexp-read-posix-char-class)
                              (let
                                ((posix-class
                                    (let
                                      ((|Scheme-to-CL-7|
                                          (map 'string
                                            #'(lambda (c)
                                                (cond ((upper-case-p c) (char-downcase c))
                                                  ((lower-case-p c) (char-upcase c)) (t c)
                                              ) )
                                            (concatenate 'string (pregexp-reverse! r))
                                      ))  )
                                      (if
                                        (or (string= |Scheme-to-CL-7| "")
                                          (not (char= (char |Scheme-to-CL-7| 0) #\:))
                                        )
                                        (intern |Scheme-to-CL-7|)
                                        (intern (subseq |Scheme-to-CL-7| 1) :keyword)
                                ))  ) )
                                (list (if neg? (list ':neg-char posix-class) posix-class)
                                  (+ i 2)
                            ) ) )
                            (if t (pregexp-error 'pregexp-read-posix-char-class))
) ) ) ) ) ) ) ) ) ) ) ) ) )

(defun pregexp-read-cluster-type (s i n)
  (let ((c (char s i)))
    (case c
      ((#\?)
        (let ((i (+ i 1)))
          (case (char s i) ((#\:) (list 'nil (+ i 1))) ((#\=) (list '(:lookahead) (+ i 1)))
            ((#\!) (list '(:neg-lookahead) (+ i 1))) ((#\>) (list '(:no-backtrack) (+ i 1)))
            ((#\<)
              (list
                (case (char s (+ i 1)) ((#\=) '(:lookbehind)) ((#\!) '(:neg-lookbehind))
                  (t (pregexp-error 'pregexp-read-cluster-type))
                )
                (+ i 2)
            ) )
            (t
              (let ((i i) (r 'nil) (inv? nil))
                (flet ((loop! (i r inv?) (throw 'loop! (values i r inv?))))
                  (loop
                    (multiple-value-setq (i r inv?)
                      (let ((i i) (r r) (inv? inv?))
                        (catch 'loop!
                          (return
                            (let ((c (char s i)))
                              (case c ((#\-) (loop! (+ i 1) r t))
                                ((#\i)
                                  (loop! (+ i 1)
                                    (cons (if inv? ':case-sensitive ':case-insensitive) r)
                                    nil
                                ) )
                                ((#\x) (setq *pregexp-space-sensitive?* inv?)
                                  (loop! (+ i 1) r nil)
                                )
                                ((#\:) (list r (+ i 1)))
                                (t (pregexp-error 'pregexp-read-cluster-type))
      ) ) ) ) ) ) ) ) ) ) ) ) )
      (t (list '(:sub) i))
) ) )

(defun pregexp-read-subpattern (s i n)
  (let ((remember-space-sensitive? *pregexp-space-sensitive?*))
    (let ((ctyp-i (pregexp-read-cluster-type s i n)))
      (let ((ctyp (car ctyp-i)))
        (let ((i (cadr ctyp-i)))
          (let ((vv (pregexp-read-pattern s i n)))
            (setq *pregexp-space-sensitive?* remember-space-sensitive?)
            (let ((vv-re (car vv)) (vv-i (cadr vv)))
              (if (and (< vv-i n) (char= (char s vv-i) #\)))
                (list
                  (let ((ctyp ctyp) (re vv-re))
                    (flet ((loop! (ctyp re) (throw 'loop! (values ctyp re))))
                      (loop
                        (multiple-value-setq (ctyp re)
                          (let ((ctyp ctyp) (re re))
                            (catch 'loop!
                              (return
                                (if (null ctyp) re (loop! (cdr ctyp) (list (car ctyp) re)))
                  ) ) ) ) ) ) )
                  (+ vv-i 1)
                )
                (pregexp-error 'pregexp-read-subpattern)
) ) ) ) ) ) ) )

(defun pregexp-wrap-quantifier-if-any (vv s n)
  (let ((re (car vv)))
    (let ((i (cadr vv)))
      (flet ((loop! (i) (throw 'loop! (values i))))
        (loop
          (multiple-value-setq (i)
            (let ((i i))
              (catch 'loop!
                (return
                  (if (>= i n) vv
                    (let ((c (char s i)))
                      (if
                        (and
                          (let ((|Scheme-to-CL-8| c))
                            (or (char= |Scheme-to-CL-8| #\space)
                              (char= |Scheme-to-CL-8| #\tab)
                              (not (graphic-char-p |Scheme-to-CL-8|))
                          ) )
                          (not *pregexp-space-sensitive?*)
                        )
                        (loop! (+ i 1))
                        (case c
                          ((#\* #\+ #\? #\{)
                            (let ((new-re (list ':between 'minimal? 'at-least 'at-most re)))
                              (let ((new-vv (list new-re 'next-i)))
                                (case c
                                  ((#\*) (rplaca (cddr new-re) 0)
                                    (rplaca (cdddr new-re) nil)
                                  )
                                  ((#\+) (rplaca (cddr new-re) 1)
                                    (rplaca (cdddr new-re) nil)
                                  )
                                  ((#\?) (rplaca (cddr new-re) 0) (rplaca (cdddr new-re) 1))
                                  ((#\{)
                                    (let ((pq (pregexp-read-nums s (+ i 1) n)))
                                      (if (not pq)
                                        (pregexp-error 'pregexp-wrap-quantifier-if-any
                                          "left bracket must be followed by number"
                                      ) )
                                      (rplaca (cddr new-re) (car pq))
                                      (rplaca (cdddr new-re) (cadr pq)) (setq i (caddr pq))
                                ) ) )
                                (let ((i (+ i 1)))
                                  (flet ((loop! (i) (throw 'loop! (values i))))
                                    (loop
                                      (multiple-value-setq (i)
                                        (let ((i i))
                                          (catch 'loop!
                                            (return
                                              (if (>= i n)
                                                (progn (rplaca (cdr new-re) nil)
                                                  (rplaca (cdr new-vv) i)
                                                )
                                                (let ((c (char s i)))
                                                  (if
                                                    (and
                                                      (let ((|Scheme-to-CL-9| c))
                                                        (or (char= |Scheme-to-CL-9| #\space)
                                                          (char= |Scheme-to-CL-9| #\tab)
                                                          (not
                                                            (graphic-char-p |Scheme-to-CL-9|
                                                      ) ) ) )
                                                      (not *pregexp-space-sensitive?*)
                                                    )
                                                    (loop! (+ i 1))
                                                    (if (char= c #\?)
                                                      (progn (rplaca (cdr new-re) t)
                                                        (rplaca (cdr new-vv) (+ i 1))
                                                      )
                                                      (when t (rplaca (cdr new-re) nil)
                                                        (rplaca (cdr new-vv) i)
                                ) ) ) ) ) ) ) ) ) ) ) )
                                new-vv
                          ) ) )
                          (t vv)
) ) ) ) ) ) ) ) ) ) ) ) )

(defun pregexp-read-nums (s i n)
  (let ((p 'nil) (q 'nil) (k i) (reading 1))
    (flet ((loop! (p q k reading) (throw 'loop! (values p q k reading))))
      (loop
        (multiple-value-setq (p q k reading)
          (let ((p p) (q q) (k k) (reading reading))
            (catch 'loop!
              (return
                (progn (if (>= k n) (pregexp-error 'pregexp-read-nums))
                  (let ((c (char s k)))
                    (if (digit-char-p c)
                      (if (= reading 1) (loop! (cons c p) q (+ k 1) 1)
                        (loop! p (cons c q) (+ k 1) 2)
                      )
                      (if
                        (and
                          (let ((|Scheme-to-CL-10| c))
                            (or (char= |Scheme-to-CL-10| #\space)
                              (char= |Scheme-to-CL-10| #\tab)
                              (not (graphic-char-p |Scheme-to-CL-10|))
                          ) )
                          (not *pregexp-space-sensitive?*)
                        )
                        (loop! p q (+ k 1) reading)
                        (if (and (char= c #\,) (= reading 1)) (loop! p q (+ k 1) 2)
                          (if (char= c #\})
                            (let
                              ((p
                                  (let
                                    ((|Scheme-to-CL-11|
                                        (concatenate 'string (pregexp-reverse! p))
                                    ))
                                    (if (position #\: |Scheme-to-CL-11| :test #'char=) nil
                                      (let
                                        ((|Scheme-to-CL-12|
                                            (read-from-string |Scheme-to-CL-11| nil)
                                        ))
                                        (if (numberp |Scheme-to-CL-12|) |Scheme-to-CL-12|
                                          nil
                               )  ) ) ) )
                                (q
                                  (let
                                    ((|Scheme-to-CL-13|
                                        (concatenate 'string (pregexp-reverse! q))
                                    ))
                                    (if (position #\: |Scheme-to-CL-13| :test #'char=) nil
                                      (let
                                        ((|Scheme-to-CL-14|
                                            (read-from-string |Scheme-to-CL-13| nil)
                                        ))
                                        (if (numberp |Scheme-to-CL-14|) |Scheme-to-CL-14|
                                          nil
                              ) ) ) ) ) )
                              (if (and (not p) (= reading 1)) (list 0 nil k)
                                (if (= reading 1) (list p p k) (if t (list p q k)))
                            ) )
                            (if t nil)
) ) ) ) ) ) ) ) ) ) ) ) ) )

(defun pregexp-invert-char-list (vv) (rplaca (car vv) ':none-of-chars) vv)

(defun pregexp-read-char-list (s i n)
  (let ((r 'nil) (i i))
    (flet ((loop! (r i) (throw 'loop! (values r i))))
      (loop
        (multiple-value-setq (r i)
          (let ((r r) (i i))
            (catch 'loop!
              (return
                (if (>= i n)
                  (pregexp-error 'pregexp-read-char-list "character class ended too soon")
                  (let ((c (char s i)))
                    (case c
                      ((#\])
                        (if (null r) (loop! (cons c r) (+ i 1))
                          (list (cons ':one-of-chars (pregexp-reverse! r)) (+ i 1))
                      ) )
                      ((#\\)
                        (let ((char-i (pregexp-read-escaped-char s i n)))
                          (if char-i (loop! (cons (car char-i) r) (cadr char-i))
                            (pregexp-error 'pregexp-read-char-list "backslash")
                      ) ) )
                      ((#\-)
                        (let ((c-prev (car r)))
                          (if (characterp c-prev)
                            (loop!
                              (cons (list ':char-range c-prev (char s (+ i 1))) (cdr r))
                              (+ i 2)
                            )
                            (loop! (cons c r) (+ i 1))
                      ) ) )
                      ((#\[)
                        (if (char= (char s (+ i 1)) #\:)
                          (let
                            ((posix-char-class-i (pregexp-read-posix-char-class s (+ i 2) n)
                            ))
                            (loop! (cons (car posix-char-class-i) r)
                              (cadr posix-char-class-i)
                          ) )
                          (loop! (cons c r) (+ i 1))
                      ) )
                      (t (loop! (cons c r) (+ i 1)))
) ) ) ) ) ) ) ) ) ) )

(defun pregexp-string-match (s1 s i n sk fk)
  (let ((n1 (length s1)))
    (if (> n1 n) (funcall fk)
      (let ((j 0) (k i))
        (flet ((loop! (j k) (throw 'loop! (values j k))))
          (loop
            (multiple-value-setq (j k)
              (let ((j j) (k k))
                (catch 'loop!
                  (return
                    (if (>= j n1) (funcall sk k)
                      (if (>= k n) (funcall fk)
                        (if (char= (char s1 j) (char s k)) (loop! (+ j 1) (+ k 1))
                          (if t (funcall fk))
) ) ) ) ) ) ) ) ) ) ) ) )

(defun pregexp-char-word? (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\_)))

(defun pregexp-at-word-boundary? (s i n)
  (or (= i 0) (>= i n)
    (let ((c/i (char s i)) (c/i-1 (char s (- i 1))))
      (let
        ((c/i/w? (pregexp-check-if-in-char-class? c/i ':word))
          (c/i-1/w? (pregexp-check-if-in-char-class? c/i-1 ':word))
        )
        (or (and c/i/w? (not c/i-1/w?)) (and (not c/i/w?) c/i-1/w?))
) ) ) )

(defun pregexp-check-if-in-char-class? (c char-class)
  (case char-class ((:any) (not (char= c #\newline)))
    ((:alnum) (or (alpha-char-p c) (digit-char-p c))) ((:alpha) (alpha-char-p c))
    ((:ascii) (< (char-code c) 128))
    ((:blank) (or (char= c #\space) (char= c *pregexp-tab-char*)))
    ((:cntrl) (< (char-code c) 32)) ((:digit) (digit-char-p c))
    ((:graph)
      (and (>= (char-code c) 32)
        (not
          (let ((|Scheme-to-CL-15| c))
            (or (char= |Scheme-to-CL-15| #\space) (char= |Scheme-to-CL-15| #\tab)
              (not (graphic-char-p |Scheme-to-CL-15|))
    ) ) ) ) )
    ((:lower) (lower-case-p c)) ((:print) (>= (char-code c) 32))
    ((:punct)
      (and (>= (char-code c) 32)
        (not
          (let ((|Scheme-to-CL-16| c))
            (or (char= |Scheme-to-CL-16| #\space) (char= |Scheme-to-CL-16| #\tab)
              (not (graphic-char-p |Scheme-to-CL-16|))
        ) ) )
        (not (alpha-char-p c)) (not (digit-char-p c))
    ) )
    ((:space)
      (let ((|Scheme-to-CL-17| c))
        (or (char= |Scheme-to-CL-17| #\space) (char= |Scheme-to-CL-17| #\tab)
          (not (graphic-char-p |Scheme-to-CL-17|))
    ) ) )
    ((:upper) (upper-case-p c))
    ((:word) (or (alpha-char-p c) (digit-char-p c) (char= c #\_)))
    ((:xdigit)
      (or (digit-char-p c) (char-equal c #\a) (char-equal c #\b) (char-equal c #\c)
        (char-equal c #\d) (char-equal c #\e) (char-equal c #\f)
    ) )
    (t (pregexp-error 'pregexp-check-if-in-char-class?))
) )

(defun pregexp-list-ref (s i)
  (let ((s s) (k 0))
    (flet ((loop! (s k) (throw 'loop! (values s k))))
      (loop
        (multiple-value-setq (s k)
          (let ((s s) (k k))
            (catch 'loop!
              (return (if (null s) nil (if (= k i) (car s) (if t (loop! (cdr s) (+ k 1))))))
) ) ) ) ) ) )

(defun pregexp-match-positions-aux (re s start n i)
  (let ((case-sensitive? t))
    (labels
      ((sub (re i backrefs sk fk)
          (if (eql re ':bos) (if (= i start) (funcall sk i backrefs) (funcall fk))
            (if (eql re ':eos) (if (>= i n) (funcall sk i backrefs) (funcall fk))
              (if (eql re ':empty) (funcall sk i backrefs)
                (if (eql re ':wbdry)
                  (if (pregexp-at-word-boundary? s i n) (funcall sk i backrefs) (funcall fk)
                  )
                  (if (eql re ':not-wbdry)
                    (if (pregexp-at-word-boundary? s i n) (funcall fk)
                      (funcall sk i backrefs)
                    )
                    (if (and (characterp re) (< i n))
                      (if (funcall (if case-sensitive? #'char= #'char-equal) (char s i) re)
                        (funcall sk (+ i 1) backrefs) (funcall fk)
                      )
                      (if (and (not (consp re)) (< i n))
                        (if (pregexp-check-if-in-char-class? (char s i) re)
                          (funcall sk (+ i 1) backrefs) (funcall fk)
                        )
                        (if (and (consp re) (eql (car re) ':char-range) (< i n))
                          (let ((c (char s i)))
                            (if
                              (let ((c< (if case-sensitive? #'char<= #'char-not-greaterp)))
                                (and (funcall c< (cadr re) c) (funcall c< c (caddr re)))
                              )
                              (funcall sk (+ i 1) backrefs) (funcall fk)
                          ) )
                          (if (consp re)
                            (case (car re)
                              ((:char-range)
                                (if (>= i n) (funcall fk)
                                  (pregexp-error 'pregexp-match-positions-aux)
                              ) )
                              ((:one-of-chars)
                                (if (>= i n) (funcall fk)
                                  (labels
                                    ((loup-one-of-chars (chars)
                                        (if (null chars) (funcall fk)
                                          (sub (car chars) i backrefs sk
                                            #'(lambda nil (loup-one-of-chars (cdr chars)))
                                    ))  ) )
                                    (loup-one-of-chars (cdr re))
                              ) ) )
                              ((:neg-char)
                                (if (>= i n) (funcall fk)
                                  (sub (cadr re) i backrefs
                                    #'(lambda (i1 backrefs1) (funcall fk))
                                    #'(lambda nil (funcall sk (+ i 1) backrefs))
                              ) ) )
                              ((:seq)
                                (labels
                                  ((loup-seq (res i backrefs)
                                      (if (null res) (funcall sk i backrefs)
                                        (sub (car res) i backrefs
                                          #'(lambda (i1 backrefs1)
                                              (loup-seq (cdr res) i1 backrefs1)
                                            )
                                          fk
                                  ))  ) )
                                  (loup-seq (cdr re) i backrefs)
                              ) )
                              ((:or)
                                (labels
                                  ((loup-or (res)
                                      (if (null res) (funcall fk)
                                        (sub (car res) i backrefs
                                          #'(lambda (i1 backrefs1)
                                              (or (funcall sk i1 backrefs1)
                                                (loup-or (cdr res))
                                            ) )
                                          #'(lambda nil (loup-or (cdr res)))
                                  ))  ) )
                                  (loup-or (cdr re))
                              ) )
                              ((:backref)
                                (let ((backref (pregexp-list-ref backrefs (cadr re))))
                                  (if backref
                                    (pregexp-string-match
                                      (subseq s (car backref) (cdr backref)) s i n
                                      #'(lambda (i) (funcall sk i backrefs)) fk
                                    )
                                    (funcall sk i backrefs)
                              ) ) )
                              ((:sub)
                                (let ((sub-backref (cons i i)))
                                  (let ((backrefs (append backrefs (list sub-backref))))
                                    (sub (cadr re) i backrefs
                                      #'(lambda (i1 backrefs1) (rplacd sub-backref i1)
                                          (funcall sk i1 backrefs1)
                                        )
                                      fk
                              ) ) ) )
                              ((:lookahead)
                                (let
                                  ((found-it?
                                      (sub (cadr re) i backrefs #'list #'(lambda nil nil))
                                  ))
                                  (if found-it? (funcall sk i backrefs) (funcall fk))
                              ) )
                              ((:neg-lookahead)
                                (let
                                  ((found-it?
                                      (sub (cadr re) i backrefs #'list #'(lambda nil nil))
                                  ))
                                  (if found-it? (funcall fk) (funcall sk i backrefs))
                              ) )
                              ((:lookbehind)
                                (let ((n-actual n)) (setq n i)
                                  (let
                                    ((found-it?
                                        (sub
                                          (list ':seq '(:between nil 0 nil :any) (cadr re)
                                            ':eos
                                          )
                                          0 backrefs #'list #'(lambda nil nil)
                                    ))  )
                                    (setq n n-actual)
                                    (if found-it? (funcall sk i backrefs) (funcall fk))
                              ) ) )
                              ((:neg-lookbehind)
                                (let ((n-actual n)) (setq n i)
                                  (let
                                    ((found-it?
                                        (sub
                                          (list ':seq '(:between nil 0 nil :any) (cadr re)
                                            ':eos
                                          )
                                          0 backrefs #'list #'(lambda nil nil)
                                    ))  )
                                    (setq n n-actual)
                                    (if found-it? (funcall fk) (funcall sk i backrefs))
                              ) ) )
                              ((:no-backtrack)
                                (let
                                  ((found-it?
                                      (sub (cadr re) i backrefs #'list #'(lambda nil nil))
                                  ))
                                  (if found-it?
                                    (funcall sk (car found-it?) (cadr found-it?))
                                    (funcall fk)
                              ) ) )
                              ((:case-sensitive :case-insensitive)
                                (let ((old case-sensitive?))
                                  (setq case-sensitive? (eql (car re) ':case-sensitive))
                                  (sub (cadr re) i backrefs
                                    #'(lambda (i1 backrefs1) (setq case-sensitive? old)
                                        (funcall sk i1 backrefs1)
                                      )
                                    #'(lambda nil (setq case-sensitive? old) (funcall fk))
                              ) ) )
                              ((:between)
                                (let ((maximal? (not (cadr re))))
                                  (let ((p (caddr re)))
                                    (let ((q (cadddr re)))
                                      (let ((re (car (cddddr re))))
                                        (let
                                          ((subpat? (and (consp re) (eql (car re) ':sub))))
                                          (labels
                                            ((loup-p (k i cbackrefs)
                                                (if (< k p)
                                                  (sub re i backrefs
                                                    #'(lambda (i1 backrefs1)
                                                        (loup-p (+ k 1) i1 backrefs1)
                                                      )
                                                    fk
                                                  )
                                                  (let ((q (and q (- q p))))
                                                    (labels
                                                      ((loup-q (k i cbackrefs)
                                                          (let
                                                            ((fk
                                                                #'(lambda nil
                                                                    (funcall sk i
                                                                      (if
                                                                        (eql cbackrefs
                                                                          'no-match-yet
                                                                        )
                                                                        (if subpat?
                                                                          (append backrefs
                                                                            (list nil)
                                                                          )
                                                                          backrefs
                                                                        )
                                                                        cbackrefs
                                                            ))    ) ) )
                                                            (if (and q (>= k q))
                                                              (funcall fk)
                                                              (if maximal?
                                                                (sub re i backrefs
                                                                  #'(lambda (i1 backrefs1)
                                                                      (or
                                                                        (loup-q (+ k 1) i1
                                                                          backrefs1
                                                                        )
                                                                        (funcall fk)
                                                                    ) )
                                                                  fk
                                                                )
                                                                (or (funcall fk)
                                                                  (sub re i backrefs
                                                                    #'(lambda (i1 backrefs1)
                                                                        (loup-q (+ k 1) i1
                                                                          backrefs1
                                                                      ) )
                                                                    fk
                                                      ))  ) ) ) ) )
                                                      (loup-q 0 i cbackrefs)
                                            ))  ) ) )
                                            (loup-p 0 i 'no-match-yet)
                              ) ) ) ) ) ) )
                              (t (pregexp-error 'pregexp-match-positions-aux))
                            )
                            (if (>= i n) (funcall fk)
                              (if t (pregexp-error 'pregexp-match-positions-aux))
      ))  ) ) ) ) ) ) ) ) ) )
      (sub re i 'nil #'list #'(lambda nil nil))
) ) )

(defun pregexp-replace-aux (str ins n backrefs)
  (let ((i 0) (r ""))
    (flet ((loop! (i r) (throw 'loop! (values i r))))
      (loop
        (multiple-value-setq (i r)
          (let ((i i) (r r))
            (catch 'loop!
              (return
                (if (>= i n) r
                  (let ((c (char ins i)))
                    (if (char= c #\\)
                      (let ((br-i (pregexp-read-escaped-number ins i n)))
                        (let
                          ((br
                              (if br-i (car br-i) (if (char= (char ins (+ i 1)) #\&) 0 nil))
                          ))
                          (let ((i (if br-i (cadr br-i) (if br (+ i 2) (+ i 1)))))
                            (if (not br)
                              (let ((c2 (char ins i)))
                                (loop! (+ i 1)
                                  (if (char= c2 #\$) r
                                    (concatenate 'string r (concatenate 'string (list c2)))
                              ) ) )
                              (loop! i
                                (let ((backref (pregexp-list-ref backrefs br)))
                                  (if backref
                                    (concatenate 'string r
                                      (subseq str (car backref) (cdr backref))
                                    )
                                    r
                      ) ) ) ) ) ) )
                      (loop! (+ i 1) (concatenate 'string r (concatenate 'string (list c))))
) ) ) ) ) ) ) ) ) ) )

(defun pregexp (s) (setq *pregexp-space-sensitive?* t)
  (list ':sub (car (pregexp-read-pattern s 0 (length s))))
)

(defun pregexp-match-positions (pat str &rest opt-args)
  (let ((pat (if (stringp pat) (pregexp pat) pat)))
    (let
      ((start
          (if (null opt-args) 0
            (let ((start (car opt-args))) (setq opt-args (cdr opt-args)) start)
      ))  )
      (let ((end (if (null opt-args) (length str) (car opt-args))))
        (let ((i start))
          (flet ((loop! (i) (throw 'loop! (values i))))
            (loop
              (multiple-value-setq (i)
                (let ((i i))
                  (catch 'loop!
                    (return
                      (and (<= i end)
                        (let ((vv (pregexp-match-positions-aux pat str start end i)))
                          (if vv (cadr vv) (loop! (+ i 1)))
) ) ) ) ) ) ) ) ) ) ) ) )

(defun pregexp-match (pat str &rest opt-args)
  (let ((ix-prs (apply #'pregexp-match-positions pat str opt-args)))
    (and ix-prs
      (mapcar #'(lambda (ix-pr) (and ix-pr (subseq str (car ix-pr) (cdr ix-pr)))) ix-prs)
) ) )

(defun pregexp-split (pat str)
  (let ((n (length str)))
    (let ((i 0) (r 'nil) (picked-up-one-undelimited-char? nil))
      (flet
        ((loop! (i r picked-up-one-undelimited-char?)
            (throw 'loop! (values i r picked-up-one-undelimited-char?))
        ))
        (loop
          (multiple-value-setq (i r picked-up-one-undelimited-char?)
            (let
              ((i i) (r r) (picked-up-one-undelimited-char? picked-up-one-undelimited-char?)
              )
              (catch 'loop!
                (return
                  (if (>= i n) (pregexp-reverse! r)
                    (let ((__test__ (pregexp-match-positions pat str i n)))
                      (if __test__
                        (funcall
                          #'(lambda (y)
                              (let ((jk (car y)))
                                (let ((j (car jk)) (k (cdr jk)))
                                  (if (= j k)
                                    (loop! (+ k 1) (cons (subseq str i (+ j 1)) r) t)
                                    (if (and (= j i) picked-up-one-undelimited-char?)
                                      (loop! k r nil)
                                      (if t (loop! k (cons (subseq str i j) r) nil))
                            ) ) ) ) )
                          __test__
                        )
                        (if t (loop! n (cons (subseq str i n) r) nil))
) ) ) ) ) ) ) ) ) ) ) )

(defun pregexp-replace (pat str ins)
  (let ((n (length str)))
    (let ((pp (pregexp-match-positions pat str 0 n)))
      (if (not pp) str
        (let ((ins-len (length ins)) (m-i (caar pp)) (m-n (cdar pp)))
          (concatenate 'string (subseq str 0 m-i) (pregexp-replace-aux str ins ins-len pp)
            (subseq str m-n n)
) ) ) ) ) )

(defun pregexp-replace* (pat str ins)
  (let ((pat (if (stringp pat) (pregexp pat) pat)) (n (length str)) (ins-len (length ins)))
    (let ((i 0) (r ""))
      (flet ((loop! (i r) (throw 'loop! (values i r))))
        (loop
          (multiple-value-setq (i r)
            (let ((i i) (r r))
              (catch 'loop!
                (return
                  (let ((pp (pregexp-match-positions pat str i n)))
                    (if pp
                      (loop! (cdar pp)
                        (concatenate 'string r (subseq str i (caar pp))
                          (pregexp-replace-aux str ins ins-len pp)
                      ) )
                      (concatenate 'string r (subseq str i n))
) ) ) ) ) ) ) ) ) ) )
