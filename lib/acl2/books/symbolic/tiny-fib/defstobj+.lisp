(in-package "ACL2")

;This book was created by Daron Vroon on August 10, 2004

; Oct. 2005
; Modified by Lee Pike, Galois Connections, Inc. <leepike@galois.com>
; Modifications include:
;
;   * It was infeasible to complete the necessary proofs from this
;     file for large stobj's (20+ fields).  Large stobj's now 
;     prove in a reasonable amount of time.
;
;   * Defstobj+'s can now be made with all the keys that are valid for
;     defstobj (:doc, :rename, and :inline)
;
; What a great book, Daron.  Enjoy!

;The purpose of this book is to define a macro, defstobj+, which
;defines a defstobj, functions that copy to and from the defstobj from
;and to a non-stobj of similar shape, and theorems that these functions
;are logically the identity.

;This gets challenging if the stobj has arrays in it. These must be
;copied element by element using the element accessor defined by the
;defstobj. For example, consider the following stobj from the tiny
;machine (developed at Rockwell Collins, and published in the ACL2 case
;studies book):

;(defstobj tiny-state
;  (progc :type (unsigned-byte 10)
;         :initially 0)
;  (mem :type (array (signed-byte 32) (1024))
;       :initially 0)
;  (dtos :type (unsigned-byte 10)
;        :initially 0)
;  (ctos :type (unsigned-byte 10)
;        :initially 0)
;  :inline t)

;Copying the non-array elements is simple, using the update-progc and
;progc functions. The mem field only has accessors for its elements, so
;it must be copied one element at a time:

;(defun copy-to-tiny-state-mem
;  (n mem tiny-state)
;  (declare (xargs :stobjs (tiny-state)
;                  :measure (acl2-count (1+ n))
;                  :guard
;                  (and (memp mem)
;                       (equal (length mem) 1024)
;                       (natp n)
;                       (<= n 1024))))
;  (if (or (zp n) (< 1024 n))
;      tiny-state
;    (let* ((n (1- n))
;           (tiny-state (update-memi n (nth n mem) tiny-state)))
;      (copy-to-tiny-state-mem n mem tiny-state))))

;and

;(defun copy-from-tiny-state-mem (n tiny-state)
;         (declare (xargs :stobjs (tiny-state)
;                         :guard (and (natp n) (<= n 1024))))
;         (if (zp n)
;             nil
;             (cons (memi (- 1024 n) tiny-state)
;                   (copy-from-tiny-state-mem (1- n)
;                                             tiny-state))))

;Proving that each of these behaves correctly requires a 2 step
;process. First we need to prove a more general theorem proven by
;induction over n. Then we can prove the overall theorem that the
;functions work correctly when n = (len (nth *memi* tiny-state)). 
;Rather than do this two step process for every array field of every
;stobj defined using defstobj+, we define generic versions of array
;copying functions that are very similar to the ones defined
;above. These are called copy-to-stobj-array and
;copy-from-stobj-array. See their definitions below. Once they are
;defined, we prove that they do what we want (basically, that they are
;noops). The defstobj+ macro then simply defines array copy functions
;such as the ones above, and proves that they are logically equivalent
;to the generic functions.

;The only other difficulty of this macro is the guard proofs for the
;functions it generates (such as the ones above). In order to prove
;these, we need to prove that each array field is a true-list and that
;every element of those arrays is of the appropriate type. However,
;these are useful theorems in general, so we export them from defstobj+
;as well. Another "bonus" of this macro is a function that is logically
;identical to the stobj recognizer predicate, but works on
;non-stobjs. In our running example, the defstobj above creates a
;function called tiny-statep that is a recognizer predicate for the
;stobj. Our macro also includes a function called logical-tiny-statep
;which is logically equivalent to tiny-statep, but works on
;non-stobjs. We then immediately prove this fact and turn off the
;definition.

;USAGE

;Simply call defstobj+ like you would defstobj. It should work in any
;package. The only problem we have found is if you define the stobj to
;have a name in a package different than the current package. For
;example

;(defstobj+ FOO::tiny-state
;  (progc :type (unsigned-byte 10)
;         :initially 0)
;  (mem :type (array (signed-byte 32) (1024))
;       :initially 0)
;  (dtos :type (unsigned-byte 10)
;        :initially 0)
;  (ctos :type (unsigned-byte 10)
;        :initially 0)
;  :inline t)

;will break if you are in package BAR. This is due to the fact that
;defstobj (which is called by this macro) puts all of the functions

;it generates into the current package instead of the package of the
;stobj name. Since we haven't found a way of getting at the current
;package in a macro, we cannot follow suit. When we try to use
;functions generated by the defstobj, we call them in the package of
;the name of the stobj, and everything breaks.

;When you have successfully run defstobj+, you get everything that a
;defstobj generates. In addition, you get the following:

;(DEFUN COPY-TO-c (COPY c) ...) ;puts all the data in COPY into the stobj c.

;(DEFTHM COPY-TO-c-NOOP
;  (IMPLIES (AND (cP X)
;                (cP Y))
;           (EQUAL (COPY-TO-c X Y)
;                  X)))

;(DEFTHM COPY-TO-c-IGNORES-SECOND-ARG
;  (IMPLIES (AND (cP X)
;                (CP Y))
;           (EQUAL (EQUAL (COPY-TO-c COPY X)
;                         (COPY-TO-c COPY Y))
;                  T)))

;(DEFUN COPY-FROM-c (c) ...) ;creates a non-stobj that is logically identical to c.

;(DEFTHM COPY-FROM-c-NOOP
;  (IMPLIES (cP c)
;           (EQUAL (COPY-FROM-c c) c)))

;(DEFUN LOGICAL-cP (X) ...) ;logically equivalent to cP, but x doesn't need to
;                           ;be a stobj. 
;(DEFTHM LOGICAL-cP-cP
;  (EQUAL (LOGICAL-cP X)
;         (cP X)))

;where c is the name of your stobj. In addition, for each array field,
;a of c, you get the following:

;(DEFTHM aP-TRUE-LISTP
;  (IMPLIES (aP X) (TRUE-LISTP X)))

;(DEFTHM aP-NTH-TYPE
;  (IMPLIES (AND (aP X) (NATP N) (< N (LEN X)))
;           (typep(NTH N X))))

;(DEFUN COPY-TO-c-a (N a c) ...) ;called by COPY-TO-c
;(DEFUN COPY-FROM-c-a (N c) ...) ;called by COPY-FROM-c

;where typep is the predicate associated with the type of elements in
;a. To see what the predicate is for a given type, see the ACL2
;documentation topic type-spec.

;Including this book also provides some useful theorems about
;update-nth, nth, and a couple other functions. To turn these off,
;simply run the form:

;(in-theory (disable defstobj+-theory))

;Don't worry about the macro, it turns on everything it needs locally.

;MODIFIED by Daron Vroon on October 18, 2004:

;I've added a with-copy-of-stobj macro. The usage is identical to that
;of with-local-stobj (see ACL2 documentation). However, instead of
;creating a blank copy of the stobj for the form, it creates a copy of
;the current version of the stobj.

;For our purposes, this is useful since we can take the current state
;of our machine and run it forward to see what happens without
;actually changing the state of the machine.

;Basically, the macro uses copy-from- and copy-to- functions as
;follows. First, the macro creates a non-stobj copy of the stobj. Then
;it calls with-local-stobj to create a local copy of the stobj. Before
;computing anything, it puts the contents of the non-stobj copy into
;the local stobj. Finally, it performs the computation given by the
;user using the local copy.

(local (include-book "arithmetic-2/meta/top" :dir :system))

;we only want the stuff in this book + ground-zero theory for our
;proofs. so we have defstobj+-before and defstobj+-after.
(deflabel defstobj+-before)

;;;firstn

(local
 (defun firstn (n l)
   (declare (xargs :guard (and (true-listp l)
                               (integerp n)
                               (<= 0 n))))
   (cond ((endp l) nil)
         ((zp n) nil)
         (t (cons (car l)
                  (firstn (1- n) (cdr l)))))))

;;;lastn

(local
 (defun lastn (n a)
   (declare (xargs :guard (and (natp n))))
   (if (or (atom a) (zp n))
       a (lastn (1- n) (cdr a)))))


(local
  (defthm car-lastn
    (equal (car (lastn n x))
           (nth n x))
    :hints (("Goal" :in-theory (enable nth)))))

(local
 (defthm cdr-lastn
   (implies (and (natp n)
                 (true-listp x))
            (equal (cdr (lastn n x))
                   (lastn (1+ n) x)))))

(local
 (defthm consp-lastn
   (implies (and (< n (len x))
                 (<= 0 n))
            (consp (lastn n x)))))

(local
 (defthm lastn-alt-def
   (implies (and (natp n)
                 (true-listp x)
                 (< n (len x)))
            (equal (lastn n x)
                   (cons (nth n x)
                         (lastn (1+ n) x))))
   :hints (("goal" 
            :in-theory (disable car-lastn cdr-lastn lastn)
            :use (car-lastn cdr-lastn)))))

(local
 (defthm lastn-nil
   (implies (and (true-listp x)
                 (<= (len x) n)
                 (natp n))
            (equal (lastn n x)
                   nil))))

(local
 (defthm lastn-0
   (equal (lastn 0 x) x)))

(local (in-theory (disable lastn)))

;;;;;;;;;;;;;;;update-nth/nth stuff;;;;;;;;;;;;;;;;;;;;;;;
;i'm not sure exactly how much of this is necessary. i just grabbed
;these theorems from tiny.lisp

(defthm nth-update-nth2
   (equal
    (nth n1 (update-nth n2 v l))
    (if (equal (nfix n1) (nfix n2))
	v
      (nth n1 l)))
  :hints (("goal" :in-theory (enable update-nth nth))))

(defthm nth-update-nth-1
  (implies
    (not (equal (nfix n1) (nfix n2)))
   (equal
    (nth n1 (update-nth n2 v l))
    (nth n1 l))))

(defthm nth-update-nth-2
  (implies
    (equal (nfix n1) (nfix n2))
   (equal
    (nth n1 (update-nth n2 v l))
    v)))

(defun nth-repeat2 (n v)
  (if (zp n)
      nil
    (cons v (nth-repeat2 (1- n) v))))

(defthm update-nth-nil2
  (equal (update-nth i v nil)
	 (append (nth-repeat2 i nil) (list v)))
  :hints (("goal" :in-theory (enable update-nth))))

(defthm update-nth-nth-noop-helper
  (implies
   (and (<= 0 n) (< n (len l)))
   (equal (update-nth n (nth n l) l) l))
  :hints (("goal" :in-theory (enable nth update-nth))))

(defthm update-nth-nth-noop
  (implies
   (and (<= 0 n) (< n (len l1)) (equal (nth n l1) (nth n l2)))
   (equal (update-nth n (nth n l2) l1) l1)))

;; order update-nth terms based on update field value
(defthm update-nth-update-nth-diff
  (implies
   (not (equal (nfix i1) (nfix i2)))
   (equal (update-nth i1 v1 (update-nth i2 v2 l))
	  (update-nth i2 v2 (update-nth i1 v1 l))))
  :hints (("goal" :in-theory (enable update-nth nfix)))
  :rule-classes ((:rewrite :loop-stopper ((i1 i2)))))

(defthm update-nth-update-nth-same
  (equal (update-nth i v1 (update-nth i v2 l))
	 (update-nth i v1 l))
  :hints (("goal" :in-theory (enable update-nth))))

(defthm len-repeat2
  (equal (len (nth-repeat2 i v)) (nfix i))
  :hints (("goal" :in-theory (enable nfix))))

(defthm len-update-nth-better
  (equal (len (update-nth i v l)) (max (1+ (nfix i)) (len l)))
  :hints (("goal" :in-theory (enable update-nth max))))

(defthm car-update-nth
  (equal (car (update-nth i v l)) (if (zp i) v (car l)))
  :hints (("goal" :in-theory (enable update-nth))))

(defthm cdr-update-nth
 (equal (cdr (update-nth n x y))
        (if (zp n) (cdr y) (update-nth (1- n) x (cdr y)))))

;;;copy-from-stobj stuff

(defun copy-from-stobj-array (n array)
  (if (zp n)
      nil
    (cons (nth (- (len array) n) array)
          (copy-from-stobj-array (1- n) array))))

(local
 (defthm copy-from-stobj-array-noop-l1
   (implies (and (natp n)
                 (true-listp array)
                 (<= n (len array)))
            (equal (copy-from-stobj-array n array)
                   (lastn (- (len array) n) array)))))

(in-theory (disable copy-from-stobj-array))

(defthm copy-from-stobj-array-noop
  (implies (and (equal n (len array))
                (true-listp array))
           (equal (copy-from-stobj-array n array)
                  array))
  :hints (("goal" :use ((:instance copy-from-stobj-array-noop-l1
                                   (n (len array)))))))

;;;copy-to-stobj stuff;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-to-stobj-array (n mem1 mem2)
  (if (or (zp n)
          (< (len mem2) n))
      mem2
    (let* ((n (1- n))
           (mem2 (update-nth n (nth n mem1) mem2)))
      (copy-to-stobj-array n mem1 mem2))))

(local
 (defthm cdr-copy-to-stobj-array
   (implies (and (consp mem2)
                 (<= n (len mem2))
                 (natp n))
            (equal (cdr (copy-to-stobj-array n mem1 mem2))
                   (copy-to-stobj-array (1- n) (cdr mem1) (cdr mem2))))
   :hints (("Goal" :in-theory (enable update-nth nth)))))

(local
 (defthm car-copy-to-stobj-array
   (implies (and (consp mem2)
                 (<= n (len mem2))
                 (posp n))
            (equal (car (copy-to-stobj-array n mem1 mem2))
                   (car mem1)))
   :hints (("Goal" :in-theory (enable update-nth nth)))))

(in-theory (disable copy-to-stobj-array))

(local
 (defun sub1-cdr-cdr-induction (n x1 x2)
   (if (zp n)
       (cons x1 x2)
     (sub1-cdr-cdr-induction (1- n) (cdr x1) (cdr x2)))))

(local
 (defthm copy-to-stobj-array-firstn
   (implies (and (consp mem1)
                 (consp mem2)
                 (equal (len mem1)
                        (len mem2))
                 (<= n (len mem1))
                 (natp n))
            (equal (firstn n (copy-to-stobj-array n mem1 mem2))
                   (firstn n mem1)))
   :hints (("Goal" :in-theory (enable update-nth nth)
            :induct (sub1-cdr-cdr-induction n mem1 mem2)))))

(local
 (defthm firstn-with-large-index
   (implies (and (<= (len l) (nfix n))
                 (true-listp l))
            (equal (firstn n l) l))
   :hints (("goal" :in-theory (enable nfix)))))

(local
 (defthm true-listp-copy-to-stobj-array
   (implies (true-listp x)
            (true-listp (copy-to-stobj-array n y x)))
   :hints (("Goal" :in-theory (enable copy-to-stobj-array)))))

(local
 (defthm len-copy-to-stobj-array
   (implies (<= n (len x))
            (equal (len (copy-to-stobj-array n y x))
                   (len x)))
   :hints (("Goal" :in-theory (enable copy-to-stobj-array)))))

(defthm copy-to-stobj-array-noop
  (implies (and (equal n (len mem1))
                (equal n (len mem2))
                (consp mem1)
                (consp mem2)
                (true-listp mem1)
                (true-listp mem2))
           (equal (copy-to-stobj-array n mem1 mem2)
                  mem1))
  :hints (("goal" :use copy-to-stobj-array-firstn)))

;;;copy-to-stobj-array ignores 2nd argument.

(local
 (defun sub1-cdr-cdr-cdr-induction (n x1 x2 x3)
   (if (zp n)
       (list x1 x2 x3)
     (sub1-cdr-cdr-cdr-induction (1- n) (cdr x1) (cdr x2) (cdr x3)))))

(local
 (defthm copy-to-stobj-array-ignores-second-arg-l1
   (implies (and (natp n)
                 (<= n (len mem2))
                 (equal (len mem2) (len mem3)))
            (equal (equal (firstn n (copy-to-stobj-array n mem1 mem2))
                          (firstn n (copy-to-stobj-array n mem1 mem3)))
                   t))
   :hints (("Goal" :induct (sub1-cdr-cdr-cdr-induction n mem1 mem2 mem3)))))

(defthm copy-to-stobj-array-ignores-second-arg
  (implies (and (true-listp mem2)
                (true-listp mem3)
                (equal n (len mem2))
                (equal n (len mem3)))
           (equal (equal (copy-to-stobj-array n mem1 mem2)
                         (copy-to-stobj-array n mem1 mem3))
                  t))
  :hints (("Goal" :use copy-to-stobj-array-ignores-second-arg-l1)))

;;;other helpful theorems:

(defthm len-0-atom
 (equal (equal 0 (len x))
          (atom x)))

;(defthmd nth-constant-open
;  (implies (and (syntaxp (quotep n))
;                (natp n))
;           (equal (nth n x)
;                  (if (equal n 0) (car x) (nth (1- n) (cdr x))))))

(defthmd cons-equal-car
  (equal (equal x (cons (car x) rest))
         (and (consp x)
              (equal (cdr x) rest))))

(defthm len>0-consp
  (implies (< 0 (len x))
           (consp x)))

(defthm add-cancel
  (implies (and (syntaxp (quotep x))
                (syntaxp (quotep z))
                (rationalp y)
                (rationalp z))
           (equal (equal (+ x y) z)
                  (equal y (+ (- x) z)))))


(deflabel defstobj+-after)

(deftheory defstobj+-theory
  (set-difference-theories (current-theory 'defstobj+-after)
                           (current-theory 'defstobj+-before)))

;; To disable the theory.
;;(in-theory (disable defstobj+-theory))

;and now, the macro:

;snagged this from Dave Greve's work. It turns a list of symbols into
;a single string that is the concatination of the names of those symobols
(defun symbol-list-to-string1 (list)
  (declare (xargs :guard (symbol-listp list)))
  (if (consp list)
      (concatenate 'string 
                   (symbol-name (car list))
                   (symbol-list-to-string1 (cdr list)))
    ""))
                   
;again, snagged from Dave Greve. This concatinates symbols. The
;resulting symbol is put in the same package as the witness.
(defmacro join-symbols1 (witness &rest rst)
  `(intern-in-package-of-symbol (symbol-list-to-string1 (list ,@rst))
                                ,witness))

;LP get the new name, if any, from the renaming lst
(defun get-new-name (old-name rename-lst)
  (cond ((endp rename-lst) old-name)
	((equal (caar rename-lst) old-name) (cadar rename-lst))
	(T (get-new-name old-name (cdr rename-lst)))))

;this generates all the stuff necessary for each array field
(defun generate-array-theorems (stobj-name field-name rename-lst elt-type len)
  (declare (xargs :mode :program))
  (let* ((x (join-symbols1 stobj-name 'x))
         (n (join-symbols1 stobj-name 'n))
         (field-predicate (join-symbols1 stobj-name field-name 'p))
         (field-get (join-symbols1 stobj-name field-name 'i))
         (old-field-update (join-symbols1 stobj-name 'update- field-get))
	 (field-update (get-new-name old-field-update rename-lst))
         (field-index (join-symbols1 stobj-name '* field-name 'i*))
         (stobj-predicate (join-symbols1 stobj-name stobj-name 'p))
         (thm1name (join-symbols1 stobj-name 
                                 field-predicate 
                                 '-true-listp))
         (thm2name (join-symbols1 stobj-name 
                                 field-predicate 
                                 '-nth-type))
         (copy-to-name (join-symbols1 stobj-name
                                     'copy-to-
                                     stobj-name
                                     '-
                                     field-name))
         (thm3name (join-symbols1 stobj-name
                                 copy-to-name
                                 '-copy-to-stobj-array))
         (copy-from-name (join-symbols1 stobj-name
                                       'copy-from-
                                       stobj-name
                                       '-
                                       field-name))
         (thm4name (join-symbols1 stobj-name
                                 copy-from-name
                                 '-copy-from-stobj-array)))
    (list
     `(defthm ,thm1name
        (implies (,field-predicate ,x)
                 (true-listp ,x)))
     `(defthm ,thm2name
        (implies (and (,field-predicate ,x)
                      (natp ,n)
                      (< ,n (len ,x)))
                 ,(translate-declaration-to-guard elt-type `(nth ,n ,x) nil)))
     `(defun ,copy-to-name (,n ,field-name ,stobj-name)
        (declare (xargs :stobjs (,stobj-name)
                        :measure (acl2-count (1+ ,n)) ;needed?
                        :guard (and (,field-predicate ,field-name)
                                    (equal (length ,field-name) ,len)
                                    (natp ,n)
                                    (<= ,n ,len))))
        (if (or (zp ,n)
                (< ,len ,n))
            ,stobj-name
          (let* ((,n (1- ,n))
                 (,stobj-name (,field-update ,n (nth ,n ,field-name) ,stobj-name)))
            (,copy-to-name ,n ,field-name ,stobj-name))))
     `(local
       (defthm ,thm3name
         (implies (and (< ,field-index (len ,stobj-name))
                       (equal (len (nth ,field-index ,stobj-name)) ,len))
                  (equal (,copy-to-name ,n ,field-name ,stobj-name)
                         (update-nth 
                          ,field-index
                          (copy-to-stobj-array ,n 
                                               ,field-name 
                                               (nth ,field-index ,stobj-name))
                          ,stobj-name)))
         :hints (("goal" :in-theory (enable copy-to-stobj-array)))))
     `(in-theory (disable ,copy-to-name))
     `(defun ,copy-from-name (,n ,stobj-name)
        (declare (xargs :stobjs (,stobj-name)
                        :guard (and (natp ,n)
                                    (<= ,n ,len))))
        (if (zp ,n)
            nil
          (cons (,field-get (- ,len ,n) ,stobj-name)
                (,copy-from-name (1- ,n) ,stobj-name))))
     `(local
       (defthm ,thm4name
         (implies (,stobj-predicate ,stobj-name)
                  (equal (,copy-from-name ,n ,stobj-name)
                         (copy-from-stobj-array ,n 
                                                (nth ,field-index 
                                                     ,stobj-name))))
         :hints (("goal" :in-theory (enable copy-from-stobj-array))))))))

;this gets the type of a field from its definition.
(defun get-type-from-field-declaration (fd)
  (cond ((endp fd) t)
        ((equal (car fd) :type) (cadr fd))
        (t (get-type-from-field-declaration (cdr fd)))))

;this generates all the theorems and defuns necessary.
;stobj-name is the name of the stobj (duh)
;field-defs is the list of field definitions left to be processed
;rename-lst is the a-list of possible renamings for the generated
;stobj defs.
;lsp is the list of predicates for the logical stobj predicate (aka lsp)
;ct keeps track of the actions necessary to copy to a stobj
;cf keeps track of the actions necessary to copy from a stobj
;array-thms keeps track of all the events generated for all the array fields
;len keeps track of the number of field-defs processed so far. at the end, it gives
;    us the length of the stobj.
(defun generate-defstobj+-aux (stobj-name field-defs rename-lst lsp 
					  ct cf array-thms len)
  (declare (xargs :mode :program))
  (let ((x (join-symbols1 stobj-name 'x))
        (y (join-symbols1 stobj-name 'y))
        (copy (join-symbols1 stobj-name 'copy)))
    (if (endp field-defs) ;when we are done, put everything together.
        (let* ((old-name (join-symbols1 stobj-name stobj-name 'p))
	       (stobj-predicate (get-new-name old-name rename-lst))
               (lsp-name (join-symbols1 stobj-name 'logical- stobj-predicate))
               (thm1name (join-symbols1 stobj-name lsp-name '- stobj-predicate))
               (copy-to-name (join-symbols1 stobj-name 'copy-to- stobj-name))
               (copy-from-name (join-symbols1 stobj-name 'copy-from- stobj-name))
               (thm2name (join-symbols1 stobj-name copy-to-name '-noop))
               (thm3name (join-symbols1 stobj-name 
                                       copy-to-name 
                                       '-ignores-second-arg))
               (thm4name (join-symbols1 stobj-name copy-from-name '-noop)))
          `((defun ,lsp-name (,x)
              (declare (xargs :guard t))
              (and (true-listp ,x)
                   (equal (len ,x) ,len)
                   ,@lsp))
            (defthm ,thm1name
              (equal (,lsp-name ,x)
                     (,stobj-predicate ,x)))
            (in-theory (disable ,lsp-name))
            ,@array-thms
            (defun ,copy-to-name (,copy ,stobj-name)
              (declare (xargs :stobjs (,stobj-name)
                              :guard (,lsp-name ,copy)))
              (let* ,ct ,stobj-name))
            (defthm ,thm2name
              (implies (and (,stobj-predicate ,x)
                            (,stobj-predicate ,y))
                       (and (equal (,copy-to-name ,x ,y)
				   ,x)))
              :hints (("goal" 
		       :do-not `(eliminate-destructors)
		       :in-theory (enable cons-equal-car))))
            (defthm ,thm3name
              (implies (and (,stobj-predicate ,x)
                            (,stobj-predicate ,y)
			    (,stobj-predicate ,copy)) ;;LP--is this assump. okay??
                       (equal (equal (,copy-to-name ,copy ,x)
                                     (,copy-to-name ,copy ,y))
                              t))
	      :hints (("goal" :use ((:instance ,thm2name (,x ,copy) (,y ,x))
				    (:instance ,thm2name (,x ,copy) (,y ,y))))))
            (defun ,copy-from-name (,stobj-name)
              (declare (xargs :stobjs (,stobj-name)))
              (list ,@(reverse cf)))
            (defthm ,thm4name
              (implies (,stobj-predicate ,stobj-name)
                       (equal (,copy-from-name ,stobj-name)
                              ,stobj-name))
              :hints (("goal" :in-theory (enable cons-equal-car))))
            (in-theory (disable ,copy-to-name ,copy-from-name))))
      (if (not (consp (car field-defs))) ;if the item is :inline, t, or nil, just continue.
          (generate-defstobj+-aux stobj-name (cdr field-defs) rename-lst lsp ct cf array-thms len)
        (let* ((fd (car field-defs))
               (fname (car fd))
               (type (get-type-from-field-declaration (cdr fd)))
               (old-field-predicate (join-symbols1 stobj-name fname 'p))
	       (field-predicate (get-new-name old-field-predicate rename-lst)))
          (if (and (consp type)
                   (eq (car type) 'array))
              (let ((flen (caaddr type))
                    (elt-type (cadr type))
                    (field-index (join-symbols1 stobj-name '* fname 'i*))
                    (copy-to-name (join-symbols1 stobj-name 
                                                'copy-to- 
                                                stobj-name
                                                '-
                                                fname))
                    (copy-from-name (join-symbols1 stobj-name
                                                  'copy-from-
                                                  stobj-name
                                                  '-
                                                  fname)))
                (generate-defstobj+-aux 
                 stobj-name
                 (cdr field-defs)
		 rename-lst
                 (cons `(,field-predicate (nth ,field-index ,x))
                       (cons `(equal (len (nth ,field-index ,x)) ,flen)
                             lsp))
                 (cons `(,stobj-name (,copy-to-name ,flen (nth ,field-index ,copy) ,stobj-name))
                       ct)
                 (cons `(,copy-from-name ,flen ,stobj-name) cf)
                 (append (generate-array-theorems stobj-name fname rename-lst elt-type flen)
                         array-thms)
                 (1+ len)))
            (let* ((field-index (join-symbols1 stobj-name '* fname '*))
                  (old-field-update (join-symbols1 stobj-name 'update- fname))
		  (field-update (get-new-name old-field-update rename-lst)))
              (generate-defstobj+-aux
               stobj-name
               (cdr field-defs)
	       rename-lst
               (cons `(,field-predicate (nth ,field-index ,x))
                     lsp)
               (cons `(,stobj-name (,field-update (nth ,field-index ,copy) ,stobj-name))
                     ct)
               (cons `(,fname ,stobj-name) cf)
               array-thms
               (1+ len)))))))))


;; To get rid of the keys in the macro arg list.
(defun fields (lst)
  (cond ((endp lst) lst)
	((or (equal ':renaming (car lst))
	     (equal ':doc (car lst))
	     (equal ':inline (car lst)))
	 nil)
	(T (cons (car lst) (fields (cdr lst))))))

;; Get the renames
(defun renames (lst)
  (cond ((endp lst) lst)
	((equal ':renaming (car lst)) (cadr lst))
	(T (renames (cdr lst)))))

;the final product:
;LP: macro now matches defstobj+ arg-list exactly (I think...)
(defmacro defstobj+ (name &rest rst)
  `(encapsulate
    ()
    (local (in-theory (union-theories (theory 'ground-zero)
                                      (theory 'defstobj+-theory))))
    (defstobj ,@(cons name rst))
    ,@(generate-defstobj+-aux name (fields rst) (renames rst) 
			      nil nil nil nil 0)))

;added October 18, 2004 by Daron Vroon:

(defmacro with-copy-of-stobj (stobj mv-let-form)
  (let ((copy-from-stobj (join-symbols1 stobj 'copy-from- stobj))
	(copy-to-stobj (join-symbols1 stobj 'copy-to- stobj)))
    `(let ((stobj (,copy-from-stobj ,stobj)))
       (with-local-stobj
	,stobj
	(mv-let ,(nth 1 mv-let-form)
		(let ((,stobj (,copy-to-stobj stobj ,stobj)))
		  ,(nth 2 mv-let-form))
		,(nth 3 mv-let-form))))))

