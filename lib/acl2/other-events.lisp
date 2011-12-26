; ACL2 Version 3.1 -- A Computational Logic for Applicative Common Lisp
; Copyright (C) 2006  University of Texas at Austin

; This version of ACL2 is a descendent of ACL2 Version 1.9, Copyright
; (C) 1997 Computational Logic, Inc.  See the documentation topic NOTE-2-0.

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

; Written by:  Matt Kaufmann               and J Strother Moore
; email:       Kaufmann@cs.utexas.edu      and Moore@cs.utexas.edu
; Department of Computer Sciences
; University of Texas at Austin
; Austin, TX 78712-1188 U.S.A.

(in-package "ACL2")

;  We permit macros under the following constraints on the args.

;  1.  No destructuring.  (Maybe some day.)
;  2.  No &aux.           (LET* is better.)
;  3.  Initforms must be quotes.  (Too hard for us to do evaluation right.)
;  4.  No &environment.   (Just not clearly enough specified in CLTL.)
;  5.  No nonstandard lambda-keywords.  (Of course.)
;  6.  No multiple uses of :allow-other-keys.  (Implementations differ.)

;  There are three nests of functions that have the same view of
;  the subset of macro args that we support:  macro-vars...,
;  chk-macro-arglist..., and bind-macro-args...  Of course, it is
;  necessary to keep them all with the same view of the subset.

(defun macro-vars-key (args)

;  We have passed &key.

  (cond ((null args) nil)
        ((eq (car args) '&allow-other-keys)
         (cond ((null (cdr args))
                nil)
               (t (er hard nil "macro-vars-key"))))
        ((atom (car args))
         (cons (car args) (macro-vars-key (cdr args))))
        (t (let ((formal (cond
                          ((atom (car (car args)))
                           (car (car args)))
                          (t (cadr (car (car args)))))))
             (cond ((int= (length (car args)) 3)
                    (cons formal
                          (cons (caddr (car args))
                                (macro-vars-key (cdr args)))))
                   (t (cons formal (macro-vars-key (cdr args)))))))))

(defun macro-vars-after-rest (args)

;  We have just passed &rest or &body.

  (cond ((null args) nil)
        ((eq (car args) '&key)
         (macro-vars-key (cdr args)))
        (t (er hard nil "macro-vars-after-rest"))))

(defun macro-vars-optional (args)

;  We have passed &optional but not &key or &rest or &body.

  (cond ((null args) nil)
        ((eq (car args) '&key)
         (macro-vars-key (cdr args)))
        ((member (car args) '(&rest &body))
         (cons (cadr args) (macro-vars-after-rest (cddr args))))
        ((symbolp (car args))
         (cons (car args) (macro-vars-optional (cdr args))))
        ((int= (length (car args)) 3)
         (cons (caar args)
               (cons (caddr (car args))
                     (macro-vars-optional (cdr args)))))
        (t (cons (caar args)
                 (macro-vars-optional (cdr args))))))

(defun macro-vars (args)
  (cond ((null args)
         nil)
        ((eq (car args) '&whole)
         (cons (cadr args) (macro-vars (cddr args))))
        ((member (car args) '(&rest &body))
         (cons (cadr args) (macro-vars-after-rest (cddr args))))
        ((eq (car args) '&optional)
         (macro-vars-optional (cdr args)))
        ((eq (car args) '&key)
         (macro-vars-key (cdr args)))
        ((or (not (symbolp (car args)))
             (lambda-keywordp (car args)))
         (er hard nil "macro-vars"))
        (t (cons (car args) (macro-vars (cdr args))))))

(defun chk-legal-defconst-name (name state)
  (cond ((legal-constantp name) (value nil))
        ((legal-variable-or-constant-namep name)
         (er soft (cons 'defconst name)
             "The symbol ~x0 may not be declared as a constant because ~
              it does not begin and end with the character *."
             name))
        (t (er soft (cons 'defconst name)
               "Constant symbols must ~*0.  Thus, ~x1 may not be ~
                declared as a constant.  See :DOC name and :DOC ~
                defconst."
               (tilde-@-illegal-variable-or-constant-name-phrase name)
               name))))

(defun defconst-fn1 (name val doc doc-pair w state)
  (let ((w (update-doc-data-base
            name doc doc-pair
            (putprop name 'const (kwote val) w))))
    (value w)))

(defun defconst-fn (name form state doc event-form)

; Important Note:  Don't change the formals of this function without
; reading the *initial-event-defmacros* discussion in axioms.lisp.

  (with-ctx-summarized
   (if (output-in-infixp state) event-form (cons 'defconst name))
   (let ((wrld1 (w state))
         (event-form (or event-form (list* 'defconst name form
                                           (if doc (list doc) nil)))))
     (er-progn
      (chk-all-but-new-name name ctx 'const wrld1 state)
      (chk-legal-defconst-name name state)
      (let ((const-prop (getprop name 'const nil 'current-acl2-world wrld1)))
        (cond
         ((and const-prop
               (equal event-form (get-event name wrld1)))

; We stop the redundant event even before evaluating the form.  We believe
; that this is merely an optimization, even if the form calls compress1 or
; compress2 (which will not update the 'acl2-array property when supplied the
; same input as the last time the compress function was called).

          (stop-redundant-event state))
         (t
          (er-let*
           ((pair (state-global-let*
                   ((safe-mode

; Why do we need to bind safe-mode to t?  Otherwise, if we certify book
; char-bug-sub with a GCL image then we can certify char-bug with an Allegro
; image, thus proving nil.  The problem is that f1 is not properly guarded, yet
; we go directly into the raw Lisp version of f1 when evaluating the defconst.
; That is just the sort of problem that safe-mode prevents.  See also :doc
; note-2-9-3 for another example.

 #|
 ;;; char-bug-sub.lisp

 (in-package "ACL2")

 (defun f1 ()
   (declare (xargs :mode :program))
   (char-upcase (code-char 224)))

 (defconst *b* (f1))

 (defthm gcl-not-allegro
   (equal (code-char 224) *b*)
   :rule-classes nil)

 ;;; char-bug.lisp

 (in-package "ACL2")

 (include-book "char-bug-sub")

 (defthm ouch
   nil
   :hints (("Goal" :use gcl-not-allegro))
   :rule-classes nil)

 |#

; However, it is not practical to bind safe-mode to t during the boot-strap
; with user::*fast-acl2-gcl-build*, because we have not yet compiled the *1*
; functions (see add-trip).  For the sake of uniformity, we go ahead and allow
; raw Lisp calls, avoiding safe mode during the boot-strap, even for other
; lisps.

                     (not (global-val 'boot-strap-flg (w state)))))
                   (simple-translate-and-eval form nil
                                              nil
                                              "The second argument of defconst"
                                              ctx wrld1 state)))
            (val (value (cdr pair))))
           (cond
            ((and (consp const-prop)
                  (equal (cadr const-prop) val))

; When we store the 'const property, we kwote it so that it is a term.
; Thus, if there is no 'const property, we will getprop the nil and
; the consp will fail.

             (stop-redundant-event state))
            (t
             (enforce-redundancy
              event-form ctx wrld1
              (er-let*
               ((wrld2 (chk-just-new-name name 'const nil ctx wrld1 state))
                (doc-pair (translate-doc name doc ctx state))
                (wrld3 (defconst-fn1 name val doc doc-pair wrld2 state)))
               (install-event name
                              event-form
                              'defconst
                              name
                              nil
                              (list 'defconst name form val)
                              nil nil wrld3 state)))))))))))))

(defun chk-legal-init (x ctx state)

; See the note in chk-macro-arglist before changing this fn to
; translate the init value.

  (cond ((and (consp x)
              (true-listp x)
              (int= 2 (length x))
              (eq (car x) 'quote))
         (value nil))
        (t (er soft ctx
               "Illegal initial value.  In ACL2 we require that ~
                initial values be quoted forms and you used ~x0.~
                ~#1~[  You should just write '~x0 instead.  Warren ~
                Teitelman once remarked that it was really dumb of a ~
                Fortran compiler to say ``missing comma!''  ``If it ~
                knows a comma is missing, why not just put one in?''  ~
                Indeed.~/~]  See :DOC macro-args."
               x
               (if (or (eq x nil)
                       (eq x t)
                       (acl2-numberp x)
                       (stringp x)
                       (characterp x))
                   0
                   1)))))

(defun chk-macro-arglist-keys (args keys-passed ctx state)
  (cond ((null args) (value nil))
        ((eq (car args) '&allow-other-keys)
         (cond ((null (cdr args)) (value nil))
               (t (er soft ctx
                      "&ALLOW-OTHER-KEYS may only occur as the last ~
                       member of an arglist so it is illegal to ~
                       follow it with ~x0.  See :DOC macro-args."
                      (cadr args)))))
        ((atom (car args))
         (cond ((symbolp (car args))
                (let ((new (intern (symbol-name (car args)) "KEYWORD")))
                  (cond ((member new keys-passed)
                         (er soft ctx
                             "The symbol-name of each keyword ~
                              parameter specifier must be distinct.  ~
                              But you have used the symbol-name ~s0 ~
                              twice.  See :DOC macro-args."
                             (symbol-name (car args))))
                        (t (chk-macro-arglist-keys
                            (cdr args)
                            (cons new
                                  keys-passed)
                            ctx state)))))
               (t (er soft ctx
                      "Each keyword parameter specifier must be ~
                       either a symbol or a list.  Thus, ~x0 is ~
                       illegal.  See :DOC macro-args."
                      (car args)))))
        ((or (not (true-listp (car args)))
             (> (length (car args)) 3))
         (er soft ctx
             "Each keyword parameter specifier must be either a ~
              symbol or a truelist of length 1, 2, or 3.  Thus, ~x0 ~
              is illegal.  See :DOC macro-args."
             (car args)))
        (t (er-progn
            (cond ((symbolp (caar args)) (value nil))
                  (t (cond ((or (not (true-listp (caar args)))
                                (not (equal (length (caar args))
                                            2))
                                (not (keywordp (car (caar args))))
                                (not (symbolp (cadr (caar args)))))
                            (er soft ctx
                                "Keyword parameter specifiers in ~
                                 which the keyword is specified ~
                                 explicitly, e.g., specifiers of the ~
                                 form ((:key var) init svar), must ~
                                 begin with a truelist of length 2 ~
                                 whose first element is a keyword and ~
                                 whose second element is a symbol.  ~
                                 Thus, ~x0 is illegal.  See :DOC ~
                                 macro-args."
                                (car args)))
                           (t (value nil)))))
            (let ((new (cond ((symbolp (caar args))
                              (intern (symbol-name (caar args))
                                      "KEYWORD"))
                             (t (car (caar args))))))
              (er-progn
               (cond ((member new keys-passed)
                      (er soft ctx
                          "The symbol-name of each keyword parameter ~
                           specifier must be distinct.  But you have ~
                           used the symbol-name ~s0 twice.  See :DOC ~
                           macro-args."
                          (symbol-name new)))
                     (t (value nil)))
               (cond ((> (length (car args)) 1)
                      (chk-legal-init (cadr (car args)) ctx state))
                     (t (value nil)))
               (cond ((> (length (car args)) 2)
                      (cond ((symbolp (caddr (car args)))
                             (value nil))
                            (t (er soft ctx
                                   "~x0 is an illegal keyword ~
                                    parameter specifier because the ~
                                    ``svar'' specified, ~x1, is not a ~
                                    symbol.  See :DOC macro-args."
                                   (car args)
                                   (caddr (car args))))))
                     (t (value nil)))
               (chk-macro-arglist-keys (cdr args) (cons new keys-passed)
                                       ctx state)))))))

(defun chk-macro-arglist-after-rest (args ctx state)
  (cond ((null args) (value nil))
        ((eq (car args) '&key)
         (chk-macro-arglist-keys (cdr args) nil ctx state))
        (t (er soft ctx
               "Only keyword specs may follow &REST or &BODY.  See ~
                :DOC macro-args."))))

(defun chk-macro-arglist-optional (args ctx state)
  (cond ((null args) (value nil))
        ((member (car args) '(&rest &body))
         (cond ((and (cdr args)
                     (symbolp (cadr args))
                     (not (lambda-keywordp (cadr args))))
                (chk-macro-arglist-after-rest (cddr args) ctx state))
               (t (er soft ctx
                      "~x0 must be followed by a variable symbol.  ~
                       See :DOC macro-args."
                      (car args)))))
        ((eq (car args) '&key)
         (chk-macro-arglist-keys (cdr args) nil ctx state))
        ((symbolp (car args))
         (chk-macro-arglist-optional (cdr args) ctx state))
        ((or (atom (car args))
             (not (true-listp (car args)))
             (not (< (length (car args)) 4)))
         (er soft ctx
             "Each optional parameter specifier must be either a ~
              symbol or a true list of length 1, 2, or 3.  ~x0 is ~
              thus illegal.  See :DOC macro-args."
             (car args)))
        (t (er-progn
            (cond ((symbolp (car (car args))) (value nil))
                  (t (er soft ctx
                         "~x0 is an illegal optional parameter ~
                          specifier because the ``variable symbol'' ~
                          used is not a symbol.  See :DOC macro-args."
                         (car args))))
            (cond ((> (length (car args)) 1)
                   (chk-legal-init (cadr (car args)) ctx state))
                  (t (value nil)))
            (cond ((int= (length (car args)) 3)
                   (cond ((symbolp (caddr (car args)))
                          (value nil))
                         (t (er soft ctx
                                "~x0 is an illegal optional parameter ~
                                 specifier because the ``svar'' ~
                                 specified, ~x1, is not a symbol.  ~
                                 See :DOC macro-args."
                                (car args)
                                (caddr (car args))))))
                  (t (value nil)))
            (chk-macro-arglist-optional (cdr args) ctx state)))))

(defun chk-macro-arglist1 (args ctx state)
  (cond ((null args) (value nil))
        ((not (symbolp (car args)))
         (er soft ctx
             "~x0 is illegal as the name of a required formal ~
              paramter.  See :DOC macro-args."
             (car args)))
        ((member (car args) '(&rest &body))
         (cond ((and (cdr args)
                     (symbolp (cadr args))
                     (not (lambda-keywordp (cadr args))))
                (chk-macro-arglist-after-rest (cddr args)
                                              ctx state))
               (t (er soft ctx
                      "~x0 must be followed by a variable symbol.  ~
                       See :DOC macro-args."
                      (car args)))))
        ((eq (car args) '&optional)
         (chk-macro-arglist-optional (cdr args) ctx state))
        ((eq (car args) '&key)
         (chk-macro-arglist-keys (cdr args) nil ctx state))
        (t (chk-macro-arglist1 (cdr args) ctx state))))

(defun subsequencep (lst1 lst2)

; We return t iff lst1 is a subsequence of lst2, in the sense that
; '(a c e) is a subsequence of '(a b c d e f) but '(a c b) is not.

  (cond ((null lst1) t)
        (t (let ((tl (member (car lst1) lst2)))
             (cond ((null tl) nil)
                   (t (subsequencep (cdr lst1) (cdr tl))))))))

(defun collect-lambda-keywordps (lst)
  (cond ((null lst) nil)
        ((lambda-keywordp (car lst))
         (cons (car lst) (collect-lambda-keywordps (cdr lst))))
        (t (collect-lambda-keywordps (cdr lst)))))

(defun chk-macro-arglist (args ctx state)

; Any modification to this function and its subordinates must cause
; one to reflect on the two function nests bind-macro-args...  and
; macro-vars... because they assume the presence of the structure that
; this function checks for.  See the comment before macro-vars for the
; restrictions we impose on macros.

; The subordinates of this function do not check that symbols that
; occur in binding spots are non-keywords and non-constants and
; without duplicates.  That check is performed here, with chk-arglist,
; as a final pass.

; Important Note:  If ever we change this function so that instead of
; just checking the args it "translates" the args, so that it returns
; the translated form of a proper arglist, then we must visit a similar
; change on the function primordial-event-macro-and-fn, which currently
; assumes that if a defmacro will be processed without error then
; the macro-args are exactly as presented in the defmacro.

; The idea of translating macro args is not ludicrous.  For example,
; the init-forms in keyword parameters must be quoted right now.  We might
; want to allow naked numbers or strings or t or nil.  But then we'd
; better go look at primordial-event-macro-and-fn.

; It is very suspicious to think about allowing the init forms to be
; anything but quoted constants because Common Lisp is very vague about
; when you get the bindings for free variables in such expressions
; or when such forms are evaluated.

  (er-progn
   (cond ((not (true-listp args))
          (er soft ctx
              "The arglist ~x0 is not a true list.  See :DOC ~
               macro-args."
              args))
         (t (value nil)))
   (let ((lambda-keywords (collect-lambda-keywordps args)))
     (cond
      ((or (subsequencep lambda-keywords
                         '(&whole &optional &rest &key &allow-other-keys))
           (subsequencep lambda-keywords
                         '(&whole &optional &body &key &allow-other-keys)))
       (cond (args
              (cond ((eq (car args) '&whole)
;  &whole can only appear at the very beginning.
                     (cond ((and (consp (cdr args))
                                 (symbolp (cadr args))
                                 (not (lambda-keywordp (cadr args))))
                            (chk-macro-arglist1 (cddr args) ctx state))
                           (t (er soft ctx
                                  "When the &whole lambda-list ~
                                   keyword is used it must be the ~
                                   first element of the lambda-list ~
                                   and it must be followed by a variable ~
                                   symbol.  This is not the case in ~
                                   ~x0.  See :DOC macro-args."
                                  args))))
                    (t (chk-macro-arglist1 args ctx state))))
             (t (value nil))))
      (t (er soft ctx
             "The lambda-list keywords allowed by ACL2 are &WHOLE, ~
              &OPTIONAL, &REST, &BODY, &KEY, and &ALLOW-OTHER-KEYS.  ~
              These must occur (if at all) in that order, with no ~
              duplicate occurrences and at most one of &REST and ~
              &BODY.  The argument list ~x0 is thus illegal."
             args))))
   (chk-arglist (macro-vars args) t ctx (w state) state)))

(defun defmacro-fn1 (name args doc doc-pair guard body w state)
  (let ((w (update-doc-data-base
            name doc doc-pair
            (putprop
             name 'macro-args args
             (putprop
              name 'macro-body body

; Below we store the guard. We currently store it in unnormalized form.
; If we ever store it in normalized form -- or in any form other than
; the translated user input -- then reconsider redundant-defmacrop
; below.

              (putprop-unless name 'guard guard *t* w))))))
    (value w)))

(defun chk-defmacro-width (rst ctx state)
  (cond ((or (not (true-listp rst))
             (not (> (length rst) 2)))
         (er soft ctx
             "Defmacro requires at least 3 arguments.  ~x0 is ~
              ill-formed.  See :DOC defmacro."
             (cons 'defmacro rst)))
        (t
         (let ((name (car rst))
               (args (cadr rst))
               (value (car (last rst)))
               (dcls-and-docs (butlast (cddr rst) 1)))
           (value (list name args dcls-and-docs value))))))

(defun redundant-defmacrop (name args guard body w)

; We determine whether there is already a defmacro of name with the
; given args, guard, and body.  We know that body is a term.  Hence,
; it is not nil.  Hence, if name is not a macro and there is no 
; 'macro-body, the first equal below will fail.

  (and (getprop name 'absolute-event-number nil 'current-acl2-world w)

; You might think the above test is redundant, given that we look for
; properties like 'macro-body below and find them.  But you would be wrong.
; Certain defmacros, in particular, those in *initial-event-defmacros* have
; 'macro-body and other properties but haven't really been defined yet!

       (equal (getprop name 'macro-body nil 'current-acl2-world w) body)
       (equal (macro-args name w) args)
       (equal (guard name nil w) guard)))

(defun defmacro-fn (mdef state event-form)

; Important Note:  Don't change the formals of this function without
; reading the *initial-event-defmacros* discussion in axioms.lisp.

  (with-ctx-summarized
   (if (output-in-infixp state) event-form (cons 'defmacro (car mdef)))
   (let ((wrld1 (w state))
         (event-form (or event-form (cons 'defmacro mdef))))
     (er-let*
      ((four (chk-defmacro-width mdef ctx state)))
      (let ((name (car four))
            (args (cadr four))
            (dcls (caddr four))
            (body (cadddr four)))
        (er-progn
         (chk-all-but-new-name name ctx 'macro wrld1 state)

; Important Note:  In chk-macro-arglist there is a comment warning us about
; the idea of "translating" the args to a macro to obtain the "internal"
; form of acceptable args.  See that comment before implementing any such
; change.

         (chk-macro-arglist args ctx state)
         (er-let*
          ((edcls (collect-declarations
                   dcls (macro-vars args)
                   'defmacro state ctx)))
          (let ((doc (if (stringp (car edcls)) (car edcls) nil))
                (edcls (if (stringp (car edcls)) (cdr edcls) edcls)))
            (er-let*
             ((tguard (translate
                       (conjoin (get-guards1 edcls wrld1))
                       '(nil) nil t ctx wrld1 state))

; known-stobjs = t, above and below.  But it doesn't matter because we
; know, from chk-macro-arglist above, that no stobjs occur in the
; formals of the macro and we check below, in
; chk-free-and-ignored-vars, that tguard and tbody use only those
; vars.

              (tbody (translate body '(nil) nil t ctx wrld1 state)))
             (cond
              ((redundant-defmacrop name args tguard tbody wrld1)
               (stop-redundant-event state))
              (t
               (enforce-redundancy
                event-form ctx wrld1
                (er-let*
                 ((wrld2 (chk-just-new-name name 'macro nil ctx wrld1 state))
                  (ignored (value (ignore-vars edcls)))
                  (ignorables (value (ignorable-vars edcls)))
                  (doc-pair (translate-doc name doc ctx state)))
                 (er-progn
                  (chk-xargs-keywords1 edcls '(:guard) ctx state)
                  (chk-free-and-ignored-vars name (macro-vars args) tguard *0*
                                             ignored ignorables tbody ctx
                                             state)
                  (er-let*
                   ((wrld3 (defmacro-fn1 name args doc doc-pair
                             tguard tbody wrld2 state)))
                   (install-event name
                                  event-form
                                  'defmacro
                                  name
                                  nil
                                  (cons 'defmacro mdef)
                                  nil nil wrld3 state))))))))))))))))

; The following functions support boot-strapping.  Consider what
; happens when we begin to boot-strap.  The first form is read.
; Suppose it is (defconst nil 'nil).  It is translated wrt the
; initial world.  Unless 'defconst has a macro definition in that
; initial world, we won't get off the ground.  The same remark holds
; for the other primitive event functions encountered in axioms.lisp.
; Therefore, before we first call translate we have got to construct a
; world with certain properties already set.

; We compute those properties with the functions below, from the
; following constant.  This constant must be the quoted form of the
; event defmacros found in axioms.lisp!  It was obtained by
; going to the axioms.lisp buffer, grabbing all of the text in the
; "The *initial-event-defmacros* Discussion", moving it over here,
; embedding it in "(defconst *initial-event-defmacros* '(&))" and
; then deleting the #+acl2-loop-only commands, comments, and documentation
; strings.

(defconst *initial-event-defmacros*
  '((defmacro in-package (str)
      (list 'in-package-fn
            (list 'quote str)
            'state))
    (defmacro defpkg (&whole event-form name form &optional doc book-path)
      (list 'defpkg-fn
            (list 'quote name)
            (list 'quote form)
            'state
            (list 'quote doc)
            (list 'quote book-path)
            (list 'quote event-form)))
    (defmacro defchoose (&whole event-form &rest def)
      (list 'defchoose-fn
            (list 'quote def)
            'state
            (list 'quote event-form)))
    (defmacro defun (&whole event-form &rest def)
      (list 'defun-fn
            (list 'quote def)
            'state
            (list 'quote event-form)
            #+:non-standard-analysis ; std-p
            nil))
    (defmacro defuns (&whole event-form &rest def-lst)
      (list 'defuns-fn
            (list 'quote def-lst)
            'state
            (list 'quote event-form)
            #+:non-standard-analysis ; std-p
            nil))
    (defmacro verify-termination (&whole event-form &rest lst)
      (list 'verify-termination-fn
            (list 'quote lst)
            'state
            (list 'quote event-form)
            #+:non-standard-analysis ; std-p
            nil))
    (defmacro verify-guards (&whole event-form name
                                    &key hints otf-flg doc)
      (list 'verify-guards-fn
            (list 'quote name)
            'state
            (list 'quote hints)
            (list 'quote otf-flg)
            (list 'quote doc)
            (list 'quote event-form)))
    (defmacro defmacro (&whole event-form &rest mdef)
      (list 'defmacro-fn
            (list 'quote mdef)
            'state
            (list 'quote event-form)))
    (defmacro defconst (&whole event-form name form &optional doc)
      (list 'defconst-fn
            (list 'quote name)
            (list 'quote form)
            'state
            (list 'quote doc)
            (list 'quote event-form)))
    (defmacro defstobj (&whole event-form name &rest args)
      (list 'defstobj-fn
            (list 'quote name)
            (list 'quote args)
            'state
            (list 'quote event-form)))
    (defmacro defthm (&whole event-form
                             name term
                             &key (rule-classes '(:REWRITE))
                             instructions
                             hints
                             otf-flg
                             doc)
      (list 'defthm-fn
            (list 'quote name)
            (list 'quote term)
            'state
            (list 'quote rule-classes)
            (list 'quote instructions)
            (list 'quote hints)
            (list 'quote otf-flg)
            (list 'quote doc)
            (list 'quote event-form)
            #+:non-standard-analysis ; std-p
            nil))
    (defmacro defaxiom (&whole event-form
                               name term
                               &key (rule-classes '(:REWRITE))
                               doc)
      (list 'defaxiom-fn
            (list 'quote name)
            (list 'quote term)
            'state
            (list 'quote rule-classes)
            (list 'quote doc)
            (list 'quote event-form)))
    (defmacro deflabel (&whole event-form name &key doc)
      (list 'deflabel-fn
            (list 'quote name)
            'state
            (list 'quote doc)
            (list 'quote event-form)))
    (defmacro defdoc (&whole event-form name doc)
      (list 'defdoc-fn
            (list 'quote name)
            'state
            (list 'quote doc)
            (list 'quote event-form)))
    (defmacro deftheory (&whole event-form name expr &key doc)
      (list 'deftheory-fn
            (list 'quote name)
            (list 'quote expr)
            'state
            (list 'quote doc)
            (list 'quote event-form)))
    (defmacro in-theory (&whole event-form expr &key doc)
      (list 'in-theory-fn
            (list 'quote expr)
            'state
            (list 'quote doc)
            (list 'quote event-form)))
    (defmacro in-arithmetic-theory (&whole event-form expr &key doc)
      (list 'in-arithmetic-theory-fn
            (list 'quote expr)
            'state
            (list 'quote doc)
            (list 'quote event-form)))
    (defmacro push-untouchable (&whole event-form name fn-p &key doc)
      (list 'push-untouchable-fn
            (list 'quote name)
            (list 'quote fn-p)
            'state
            (list 'quote doc)
            (list 'quote event-form)))
    (defmacro reset-prehistory (&whole event-form &optional permanent-p doc)
      (list 'reset-prehistory-fn
            (list 'quote permanent-p)
            'state
            (list 'quote doc)
            (list 'quote event-form)))
    (defmacro set-body (&whole event-form fn name-or-rune)
      (list 'set-body-fn
            (list 'quote fn)
            (list 'quote name-or-rune)
            'state
            (list 'quote event-form)))
    (defmacro table (&whole event-form name &rest args)
      (list 'table-fn
            (list 'quote name)
            (list 'quote args)
            'state
            (list 'quote event-form)))
    (defmacro progn (&rest r)
      (list 'progn-fn
            (list 'quote r)
            'state))
    (defmacro encapsulate (&whole event-form signatures &rest cmd-lst)
      (list 'encapsulate-fn
            (list 'quote signatures)
            (list 'quote cmd-lst)
            'state
            (list 'quote event-form)))
    (defmacro include-book (&whole event-form user-book-name
                                   &key
                                   (load-compiled-file ':warn)
                                   (uncertified-okp 't)
                                   (defaxioms-okp 't)
                                   (skip-proofs-okp 't)
                                   (ttags nil)
                                   dir
                                   doc)
      (list 'include-book-fn
            (list 'quote user-book-name)
            'state
            (list 'quote load-compiled-file)
            (list 'quote :none)
            (list 'quote uncertified-okp)
            (list 'quote defaxioms-okp)
            (list 'quote skip-proofs-okp)
            (list 'quote ttags)
            (list 'quote doc)
            (list 'quote dir)
            (list 'quote event-form)))
    (defmacro local (x)
      (list 'if
            '(equal (ld-skip-proofsp state) 'include-book)
            '(mv nil nil state)
            (list 'if 
                  '(equal (ld-skip-proofsp state) 'initialize-acl2)
                  '(mv nil nil state)
                  (list 'state-global-let*
                        '((in-local-flg t))
                        (list 'when-logic "LOCAL" x)))))
    ))

; Because of the Important Boot-Strapping Invariant noted in axioms.lisp,
; we can compute from this list the following things for each event:

; the macro name
; the macro args
; the macro body
; the -fn name corresponding to the macro
; the formals of the -fn

; The macro name and args are easy.  The macro body must be obtained
; from the list above by translating the given bodies, but we can't use
; translate yet because the world is empty and so, for example, 'list
; is not defined as a macro in it.  So we use the following boot-strap
; version of translate that is capable (just) of mapping the bodies above
; into their translations under a properly initialized world.

(defun boot-translate (x)
  (cond ((atom x)
         (cond ((eq x nil) *nil*)
               ((eq x t) *t*)
               ((keywordp x) (kwote x))
               ((symbolp x) x)
               (t (kwote x))))
        ((eq (car x) 'quote) x)
        ((eq (car x) 'if)
         (list 'if
               (boot-translate (cadr x))
               (boot-translate (caddr x))
               (boot-translate (cadddr x))))
        ((eq (car x) 'equal)
         (list 'equal
               (boot-translate (cadr x))
               (boot-translate (caddr x))))
        ((eq (car x) 'ld-skip-proofsp)
         (list 'ld-skip-proofsp
               (boot-translate (cadr x))))
        ((or (eq (car x) 'list)
             (eq (car x) 'mv))
         (cond ((null (cdr x)) *nil*)
               (t (list 'cons
                        (boot-translate (cadr x))
                        (boot-translate (cons 'list (cddr x)))))))
        ((eq (car x) 'when-logic)
         (list 'if
               '(eq (default-defun-mode-from-state state) ':program)
               (list 'skip-when-logic (list 'quote (cadr x)) 'state)
               (boot-translate (caddr x))))
        (t (er hard 'boot-translate
               "Boot-translate was called on ~x0, which is ~
                unrecognized.  If you want to use such a form in one ~
                of the *initial-event-defmacros* then you must modify ~
                boot-translate so that it can translate the form."
               x))))

; The -fn name corresponding to the macro is easy.  Finally to get the
; formals of the -fn we have to walk through the actuals of the call of
; the -fn in the macro body and unquote all the names but 'STATE.  That
; is done by:

(defun primordial-event-macro-and-fn1 (actuals)
  (cond ((null actuals) nil)
        ((equal (car actuals) '(quote state))
         (cons 'state (primordial-event-macro-and-fn1 (cdr actuals))))
        #+:non-standard-analysis
        ((or (equal (car actuals) nil)
             (equal (car actuals) t))

; Since nil and t are not valid names for formals, we need to transform (car
; actuals) to something else.  Up until the non-standard extension this never
; happened.  We henceforth assume that values of nil and t correspond to the
; formal std-p.

         (cons 'std-p (primordial-event-macro-and-fn1 (cdr actuals))))
        ((and (consp (car actuals))
              (eq (car (car actuals)) 'list)
              (equal (cadr (car actuals)) '(quote quote)))
         (cons (caddr (car actuals))
               (primordial-event-macro-and-fn1 (cdr actuals))))
        (t (er hard 'primordial-event-macro-and-fn1
               "We encountered an unrecognized form of actual, ~x0, ~
                in trying to extract the formals from the actuals in ~
                some member of *initial-event-defmacros*.  If you ~
                want to use such a form in one of the initial event ~
                defmacros, you must modify ~
                primordial-event-macro-and-fn1 so that it can recover ~
                the corresponding formal name from the actual form."
               (car actuals)))))

(defun primordial-event-macro-and-fn (form wrld)

; Given a member of *initial-event-defmacros* above, form, we check that
; it is of the desired shape, extract the fields we need as described,
; and putprop them into wrld.

  (case-match form
              (('defmacro 'local macro-args macro-body)
               (putprop
                'local 'macro-args macro-args
                (putprop
                 'local 'macro-body (boot-translate macro-body)
                 (putprop
                  'ld-skip-proofsp 'symbol-class :common-lisp-compliant
                  (putprop
                   'ld-skip-proofsp 'formals '(state)
                   (putprop
                    'ld-skip-proofsp 'stobjs-in '(state)
                    (putprop
                     'ld-skip-proofsp 'stobjs-out '(nil)

; See the fakery comment below for an explanation of this infinite
; recursion!  This specious body is only in effect during the
; processing of the first part of axioms.lisp during boot-strap.  It
; is overwritten by the accepted defun of ld-skip-proofsp.  Similarly
; for default-defun-mode-from-state and skip-when-logic.

                     (putprop
                      'ld-skip-proofsp 'def-bodies
                      (list (make def-body
                                  :formals '(state)
                                  :hyp nil
                                  :concl '(ld-skip-proofsp state)
                                  :rune *fake-rune-for-anonymous-enabled-rule*
                                  :nume 0 ; fake
                                  :recursivep nil
                                  :controller-alist nil))
                      (putprop
                       'default-defun-mode-from-state 'symbol-class
                       :common-lisp-compliant
                       (putprop
                        'default-defun-mode-from-state 'formals '(state)
                        (putprop
                         'default-defun-mode-from-state 'stobjs-in '(state)
                         (putprop
                          'default-defun-mode-from-state 'stobjs-out '(nil)
                          (putprop
                           'default-defun-mode-from-state 'def-bodies
                           (list (make def-body
                                       :formals '(str state)
                                       :hyp nil
                                       :concl '(default-defun-mode-from-state
                                                 state)
                                       :rune
                                       *fake-rune-for-anonymous-enabled-rule*
                                       :nume 0 ; fake
                                       :recursivep nil
                                       :controller-alist nil))
                           (putprop
                            'skip-when-logic 'symbol-class
                            :common-lisp-compliant
                            (putprop
                             'skip-when-logic 'formals '(str state)
                             (putprop
                              'skip-when-logic 'stobjs-in '(nil state)
                              (putprop
                               'skip-when-logic 'stobjs-out '(nil nil state)
                               (putprop
                                'skip-when-logic 'def-bodies
                                (list (make def-body
                                            :formals '(str state)
                                            :hyp nil
                                            :concl '(skip-when-logic str state)
                                            :rune
                                            *fake-rune-for-anonymous-enabled-rule*
                                            :nume 0 ; fake
                                            :recursivep nil
                                            :controller-alist nil))
                                wrld))))))))))))))))))
              (('defmacro name macro-args
                 ('list ('quote name-fn) . actuals))
               (let* ((formals (primordial-event-macro-and-fn1 actuals))
                      (stobjs-in (compute-stobj-flags formals t wrld))

; known-stobjs = t but, in this case it could just as well be
; known-stobjs = '(state) because we are constructing the primordial world
; and state is the only stobj.

                      (macro-body (boot-translate (list* 'list
                                                         (kwote name-fn)
                                                         actuals))))

; We could do a (putprop-unless name 'guard *t* *t* &) and a
; (putprop-unless name-fn 'guard *t* *t* &) here, but it would be silly.

                 (putprop
                  name 'macro-args macro-args
                  (putprop
                   name 'macro-body macro-body
                   (putprop
                    name-fn 'symbol-class :common-lisp-compliant
                    (putprop
                     name-fn 'formals formals
                     (putprop
                      name-fn 'stobjs-in stobjs-in
                      (putprop
                       name-fn 'stobjs-out '(nil nil state)

; The above may make sense, but the following act of fakery deserves
; some comment.  In order to get, e.g. defconst-fn, to work before
; it is defined in a boot-strap, we give it a body, which makes
; ev-fncall think it is ok to take a short cut and use the Common Lisp
; definition.  Of course, we are asking for trouble by laying down
; this recursive call!  But it never happens.

                       (putprop
                        name-fn 'def-bodies
                        (list (make def-body
                                    :formals formals
                                    :hyp nil
                                    :concl (cons name-fn formals)
                                    :rune
                                    *fake-rune-for-anonymous-enabled-rule*
                                    :nume 0 ; fake
                                    :recursivep nil
                                    :controller-alist nil))
                        wrld)))))))))
              (& (er hard 'primordial-event-macro-and-fn
                     "The supplied form ~x0 was not of the required ~
                      shape.  Every element of ~
                      *initial-event-defmacros* must be of the form ~
                      expected by this function.  Either change the ~
                      event defmacro or modify this function."
                     form))))

(defun primordial-event-macros-and-fns (lst wrld)

; This function is given *initial-event-defmacros* and just sweeps down it,
; putting the properties for each event macro and its corresponding -fn.

  (cond
   ((null lst) wrld)
   (t (primordial-event-macros-and-fns
       (cdr lst)
       (primordial-event-macro-and-fn (car lst) wrld)))))

; We need to declare the 'type-prescriptions for those fns that are
; referenced before they are defined in the boot-strapping process.
; Actually, apply is such a function, but it has an unrestricted type
; so we leave its 'type-prescriptions nil.

(defconst *initial-type-prescriptions*
  (list (list 'o-p
              (make type-prescription
                    :rune *fake-rune-for-anonymous-enabled-rule*
                    :nume nil
                    :term '(o-p x)
                    :hyps nil
                    :basic-ts *ts-boolean*
                    :vars nil
                    :corollary '(booleanp (o-p x))))
        (list 'o<
              (make type-prescription
                    :rune *fake-rune-for-anonymous-enabled-rule*
                    :nume nil
                    :term '(o< x y)
                    :hyps nil
                    :basic-ts *ts-boolean*
                    :vars nil
                    :corollary '(booleanp (o< x y))))))

(defun strip-caddrs (x)
  (declare (xargs :guard (all->=-len x 3)))
  (cond ((null x) nil)
        (t (cons (caddar x) (strip-caddrs (cdr x))))))

(defun collect-world-globals (wrld ans)
  (cond ((null wrld) ans)
        ((eq (cadar wrld) 'global-value)
         (collect-world-globals (cdr wrld)
                                (add-to-set-eq (caar wrld) ans)))
        (t (collect-world-globals (cdr wrld) ans))))

(defun primordial-world-globals (operating-system)

; This function is the standard place to initialize a world global.
; Among the effects of this function is to set the global variable
; 'world-globals to the list of all variables initialized.  Thus,
; it is very helpful to follow the discipline of initializing all
; globals here, whether their initial values are important or not.

; Historical Note: Once upon a time, before we kept a stack of
; properties on the property lists representing installed worlds, it
; was necessary, when retracting from a world, to scan the newly
; exposed world to find the new current value of any property removed.
; This included the values of world globals and it often sent us all
; the way back to the beginning of the primordial world.  We then
; patched things up by using this collection of names at the end of
; system initialization to "float" to the then-top of the world the
; values of all world globals.  That was the true motivation of
; collecting the initialization of all globals into one function: so
; we could get 'world-globals so we knew who to float.

  (let ((wrld
         (global-set-lst
          (list*
           (list 'event-landmark (make-event-tuple -1 0 nil nil 0 nil))
           (list 'command-landmark (make-command-tuple -1 :logic nil nil))
           (list 'known-package-alist *initial-known-package-alist*)
           (list 'well-founded-relation-alist
                 (list (cons 'o<
                             (cons 'o-p
                                   *fake-rune-for-anonymous-enabled-rule*))))
           (list 'recognizer-alist *initial-recognizer-alist*)
           (list 'built-in-clauses
                 (classify-and-store-built-in-clause-rules
                  *initial-built-in-clauses*
                  nil
; The value of wrld supplied below, nil, just means that all function symbols
; of initial-built-in-clauses will seem to have level-no 0.
                  nil))
           (list 'half-length-built-in-clauses
                 (floor (length *initial-built-in-clauses*) 2))
           (list 'type-set-inverter-rules *initial-type-set-inverter-rules*)
           (list 'global-arithmetic-enabled-structure
                 (initial-global-enabled-structure
                  "ARITHMETIC-ENABLED-ARRAY-"))
           (let ((globals
                  '((event-index nil)
                    (command-index nil)
                    (event-number-baseline 0)
                    (command-number-baseline-info
                     (make command-number-baseline-info
                           :current 0
                           :permanent-p t
                           :original 0))
                    (embedded-event-lst nil)
                    (cltl-command nil)
                    (include-book-alist nil)
                    (include-book-path nil)
                    (certification-tuple nil)
                    (documentation-alist nil)
                    (proved-functional-instances-alist nil)
                    (nonconstructive-axiom-names nil)
                    (standard-theories (nil nil nil nil))
                    (current-theory nil)
                    (current-theory-augmented nil)
                    (current-theory-index -1)
                    (generalize-rules nil)
                    (boot-strap-flg t)
                    (boot-strap-pass-2 nil)
                    (skip-proofs-seen nil)
                    (redef-seen nil)
                    (free-var-runes-all nil)
                    (free-var-runes-once nil)
                    (chk-new-name-lst
                     (if iff implies not
                         in-package
                         defpkg defun defuns mutual-recursion defmacro defconst
                         defstobj defthm defaxiom progn encapsulate include-book 
                         deflabel defdoc deftheory
                         in-theory in-arithmetic-theory
                         push-untouchable remove-untouchable set-body table
                         reset-prehistory verify-guards verify-termination
                         local defchoose ld-skip-proofsp
                         in-package-fn defpkg-fn defun-fn defuns-fn
                         mutual-recursion-fn defmacro-fn defconst-fn
                         defstobj-fn
                         defthm-fn defaxiom-fn progn-fn encapsulate-fn
                         include-book-fn deflabel-fn defdoc-fn
                         deftheory-fn in-theory-fn in-arithmetic-theory-fn
                         push-untouchable-fn remove-untouchable-fn
                         reset-prehistory-fn set-body-fn
                         table-fn verify-guards-fn verify-termination-fn
                         defchoose-fn apply o-p o<
                         default-defun-mode-from-state skip-when-logic

; The following names are here simply so we can deflabel them for
; documentation purposes:

                         state
                         declare apropos
                         enter-boot-strap-mode exit-boot-strap-mode
                         lp acl2-defaults-table let let*
                         complex complex-rationalp

                         ))
                    (ttags-seen nil)
                    (untouchable-fns nil)
                    (untouchable-vars nil))))
             (list* `(operating-system ,operating-system)
                    globals)))
          nil)))
    (global-set 'world-globals
                (collect-world-globals wrld '(world-globals))
                wrld)))

;; RAG - I added the treatment of *non-standard-primitives*

(defun primordial-world (operating-system)
  (let ((names (strip-cars *primitive-formals-and-guards*))
        (arglists (strip-cadrs *primitive-formals-and-guards*))
        (guards (strip-caddrs *primitive-formals-and-guards*))
        (ns-names #+:non-standard-analysis *non-standard-primitives*
                  #-:non-standard-analysis nil))

    (add-command-landmark
     :logic
     (list 'enter-boot-strap-mode operating-system)
     nil
     (add-event-landmark
      (list 'enter-boot-strap-mode operating-system)
      'enter-boot-strap-mode
      (append (strip-cars *primitive-formals-and-guards*)
              (strip-non-hidden-package-names *initial-known-package-alist*))
      (putprop
       'equal
       'coarsenings
       '(equal)
       (putprop-x-lst1
        names 'absolute-event-number 0
        (putprop-defun-runic-mapping-pairs
         names nil
         (putprop-x-lst1
          ns-names
          'classicalp nil
          (putprop-x-lst1
           ns-names
           'constrainedp t
           (putprop-x-lst1
            names
            'symbol-class :common-lisp-compliant
            (putprop-x-lst2-unless
             names 'guard guards *t*
             (putprop-x-lst2
              names 'formals arglists
              (putprop-x-lst2
               (strip-cars *initial-type-prescriptions*)
               'type-prescriptions
               (strip-cdrs *initial-type-prescriptions*)
               (putprop-x-lst1
                names 'coarsenings nil
                (putprop-x-lst1
                 names 'congruences nil
                 (primordial-event-macros-and-fns
                  *initial-event-defmacros*

; This putprop must be here, into the world seen by
; primordial-event-macros-and-fns!

                  (putprop
                   'state 'stobj '(*the-live-state*)
                   (primordial-world-globals
                    operating-system))))))))))))))))))

(defun same-name-twice (l)
  (cond ((null l) nil)
        ((null (cdr l)) nil)
        ((equal (symbol-name (car l))
                (symbol-name (cadr l)))
         (list (car l) (cadr l)))
        (t (same-name-twice (cdr l)))))

(defun conflicting-imports (l)

; We assume that l is sorted so that if any two elements have the same
; symbol-name, then two such are adjacent.

  (same-name-twice l))

(defun chk-new-stringp-name (ev-type name ctx w state)
  (cond
   ((not (stringp name))
    (er soft ctx
        "The first argument to ~s0 must be a string.  You provided ~
         the object ~x1.  See :DOC ~s."
        (cond
         ((eq ev-type 'defpkg) "defpkg")
         (t "include-book"))
        name))
   (t (let ((entry
             (find-package-entry name (global-val 'known-package-alist w))))
        (cond
         ((and entry
               (not (and (eq ev-type 'defpkg)
                         (package-entry-hidden-p entry))))
          (er soft ctx
              "The name ~x0 is in use as a package name.  We do not permit ~
               package names~s1 to participate in redefinition.  If you must ~
               redefine this name, use :ubt to undo the existing definition."
              name
              (if (package-entry-hidden-p entry)
                  " (even those that are hidden; see :DOC hidden-death-package"
                "")))
         ((assoc-equal name (global-val 'include-book-alist w))

; Name is thus a full-book-name.

          (cond
           ((eq ev-type 'include-book)
            (value name))
           (t (er soft ctx
                  "The name ~x0 is in use as a book name.  You are trying to ~
                   redefine it as a package.  We do not permit package names ~
                   to participate in redefinition.  If you must redefine this ~
                   name, use :ubt to undo the existing definition."
                  name))))
         (t (value nil)))))))

(deflabel package-reincarnation-import-restrictions
  :doc
  ":Doc-Section Miscellaneous

   re-defining undone ~ilc[defpkg]s~/

   Suppose ~c[(defpkg \"pkg\" imports)] is the most recently executed
   successful definition of ~c[\"pkg\"] in this ACL2 session and that it
   has since been undone, as by ~c[:]~ilc[ubt].  Any future attempt in this
   session to define ~c[\"pkg\"] as a package must specify an identical
   imports list.~/

   The restriction stems from the need to implement the reinstallation
   of saved logical ~il[world]s as in error recovery and the ~c[:]~ilc[oops] ~il[command].
   Suppose that the new ~ilc[defpkg] attempts to import some symbol, ~c[a::sym],
   not imported by the previous definition of ~c[\"pkg\"].  Because it was
   not imported in the original package, the symbol ~c[pkg::sym], different
   from ~c[a::sym], may well have been created and may well be used in some
   saved ~il[world]s.  Those saved ~il[world]s are Common Lisp objects being held
   for you ``behind the scenes.''  In order to import ~c[a::sym] into
   ~c[\"pkg\"] now we would have to unintern ~c[pkg::sym], rendering those
   saved ~il[world]s ill-formed.  It is because of saved ~il[world]s that we do
   not actually clear out a package when it is undone.

   At one point we thought it was sound to allow the new ~ilc[defpkg] to
   import a subset of the old.  But that is incorrect.  Suppose the old
   definition of ~c[\"pkg\"] imported ~c[a::sym] but the new one does not.
   Suppose we allowed that and implemented it simply by setting the
   imports of ~c[\"pkg\"] to the new subset.  Then consider the conjecture
   ~c[(eq a::sym pkg::sym)].  This ought not be a theorem because we did
   not import ~c[a::sym] into ~c[\"pkg\"].  But in fact in AKCL it is a theorem
   because ~c[pkg::sym] is read as ~c[a::sym] because of the old imports."

#| Once upon a time the documentation included the following additional
   text.  We deleted it on the grounds that we shouldn't tell the user
   how to go behind our backs.  But we might want to recall this hack
   in the future for our own use.

   If you really must change the imports list of a previously defined
   but now undone package, we recommend that you either invent a new
   package name for subsequent use in this session or that you save
   your state and reconstruct it in a new ACL2 session.  If you wish to
   try behind the scenes surgery to allow the new ~ilc[defpkg] to succeed ~-[]
   at the expense of ACL2's soundness in the rest of the session ~-[]
   exit ~ilc[lp] and type the following to raw Lisp:
   ~bv[]
   (let ((name \"pkg\")           ; fill in pkg name
         (new-imports '(...))     ; fill in new imports
         (p (find-package name)))
     (do-symbols (sym p) (unintern sym p))
     (import new-imports p)
     (setq *ever-known-package-alist*
           (cons (make-package-entry :name name :imports new-imports)
                 (remove-package-entry name *ever-known-package-alist*)))
     name)
   ~ev[]
   This will render ill-formed any saved ~il[world]s involving symbols in
   ~c[\"pkg\"] and it may be impossible to recover from certain errors.  In
   addition, because ACL2 is probably unsound after this hack we
   recommend that you treat the rest of the session as merely
   exploratory.
|#
)

(defun chk-package-reincarnation-import-restrictions (name proposed-imports)

; Logically, this function always returns t, but it may cause a hard
; error because we cannot create a package with the given name and imports.
; See :DOC package-reincarnation-import-restrictions.

  #+acl2-loop-only
  (declare (ignore name proposed-imports))
  #-acl2-loop-only
  (chk-package-reincarnation-import-restrictions2 name proposed-imports)
  t)

(defun convert-book-name-to-cert-name (x)

; X is assumed to satisfy chk-book-name.  We generate the
; corresponding certification file name.

; The cddddr below chops of the "lisp" but leaves the dot.

  (coerce (append (reverse (cddddr (reverse (coerce x 'list))))
                  '(#\c #\e #\r #\t))
          'string))

(defun tilde-@-defpkg-error-phrase (name package-entry new-not-old old-not-new
                                         book-path defpkg-book-path w)
  (list
   "The proposed defpkg conflicts with a previously-executed defpkg for name ~
    ~x0~@1.  ~#a~[For example, symbol ~s2::~s3 is in the list of imported ~
    symbols for the ~s4 definition but not for the other.~/The two have the ~
    same lists of imported symbols, but not in the same order.~]  The previous ~
    defpkg is ~#5~[at the top level.~/in the .cert file ~x6 for the book ~x7, ~
    which is included at the top level~/in the .cert file ~x6 for the book ~
    ~x7, which is included via the following path, from top-most book down to ~
    the above file.~|  ~F8~]~@9~@b"
   (cons #\0 name)
   (cons #\1 (if (package-entry-hidden-p package-entry)
                 " that no longer exists in the current ACL2 logical world ~
                  (see :DOC hidden-death-package)"
               ""))
   (cons #\a (if (or new-not-old old-not-new) 0 1))
   (cons #\2 (symbol-package-name (if new-not-old
                                      (car new-not-old)
                                    (car old-not-new))))
   (cons #\3 (symbol-name (if new-not-old
                              (car new-not-old)
                            (car old-not-new))))
   (cons #\4 (if new-not-old "current" "previous"))
   (cons #\5 (zero-one-or-more book-path))
   (cons #\6 (convert-book-name-to-cert-name (car book-path)))
   (cons #\7 (car book-path))
   (cons #\8 (reverse book-path))
   (cons #\9 (if defpkg-book-path
                 "~|This previous defpkg event appears to have been created ~
                  because of a defpkg that was hidden by a local include-book; ~
                  see :DOC hidden-death-package."
               ""))
   (cons #\b (let ((include-book-path
                    (global-val 'include-book-path w)))
               (if (or include-book-path
                       defpkg-book-path)
                   (msg "~|The new proposed defpkg event may be found by ~
                         following the sequence of include-books below, from ~
                         top-most book down to the book whose portcullis ~
                         contains the following defpkg form.~|  ~F0"
                        (reverse (append defpkg-book-path include-book-path)))
                 "")))))

(defconst *1*-pkg-prefix*

; Unfortunately, *1*-package-prefix* is defined in raw Lisp only, early in the
; boot-strap.  We mirror that constant here for use below.

  (let ((result "ACL2_*1*_"))
    #-acl2-loop-only
    (or (equal result *1*-package-prefix*)
        (er hard '*1*-pkg-prefix*
            "Implementation error:  Failed to keep *1*-package-prefix* and ~
             *1*-pkg-prefix* in sync."))
    result))

(defun chk-acceptable-defpkg (name form defpkg-book-path ctx w state)

; We return an error triple.  The non-error value is either 'redundant or a
; triple (tform value . package-entry), where tform and value are a translated
; form and its value, and either package-entry is nil in the case that no
; package with name name has been seen, or else is an existing entry for name
; in known-package-alist with field hidden-p=t (see the Essay on Hidden
; Packages).

  (mv-let
    (all-names state)
    (list-all-package-names state)
    (cond
     ((not (stringp name))
      (er soft ctx
          "Package names must be string constants and ~x0 is not.  See :DOC ~
          defpkg."
          name))
     ((equal name "")

; In Allegro CL, "" is prohibited because it is already a nickname for the
; KEYWORD package.  But in GCL we could prove nil up through v2-7 by certifying
; the following book with the indicated portcullis:

; (in-package "ACL2")
;
; Portcullis:
; (defpkg "" nil)
;
; (defthm bug
;   nil
;   :hints (("Goal" :use ((:instance intern-in-package-of-symbol-symbol-name
;                                    (x '::abc) (y 17)))))
;   :rule-classes nil)

      (er soft ctx
          "The empty string is not a legal package name for defpkg."
          name))
     ((not (standard-char-listp (coerce name 'list)))
      (er soft ctx
          "~x0 is not a legal package name for defpkg, which requires the name ~
           to contain only standard characters."
          name))
     ((not (equal (string-upcase name) name))
      (er soft ctx
          "~x0 is not a legal package name for defpkg, which disallows lower ~
           case characters in the name."
          name))
     ((equal name "LISP")
      (er soft ctx
          "~x0 is disallowed as a a package name for defpkg, because this ~
           package name is used under the hood in some Common Lisp ~
           implementations."
          name))
     ((let ((len (length *1*-pkg-prefix*)))
        (and (<= len (length name))
             (string-equal (subseq name 0 len) *1*-pkg-prefix*)))

; The use of string-equal could be considered overkill; probably equal provides
; enough of a check.  But we prefer not to consider the possibility that some
; Lisp has case-insensitive package names.  Probably we should similarly use
; member-string-equal instead of member-equal below.

      (er soft ctx
          "It is illegal for a package name to start (even ignoring case) ~
           with the string \"~@0\".  ACL2 makes internal use of package names ~
           starting with that string."
          *1*-pkg-prefix*))
     ((and (member-equal name all-names)
           (not (global-val 'boot-strap-flg w))
           (not (member-equal name

; It seems very likely that the following value contains the same names as
; (strip-cars (known-package-alist state)), except for built-in packages, now
; that we keep hidden packages around.  But it seems harmless enough to keep
; this state global around.

                              (f-get-global 'packages-created-by-defpkg
                                            state))))
      (er soft ctx
          "The package named ~x0 already exists and was not created by ACL2's ~
           defpkg.  We cannot (re)define an existing package.  See :DOC defpkg."
          name))
     ((not (true-listp defpkg-book-path))
      (er soft ctx
          "The book-path argument to defpkg, if supplied, must be a ~
           true-listp.  It is not recommended to supply this argument, since ~
           the system makes use of it for producing useful error messages.  ~
           The defpkg of ~x0 is thus illegal."
          name))
     (t (state-global-let*
         ((safe-mode

; In order to build a profiling image for GCL, we have observed a need to avoid
; going into safe-mode when building the system.

           (not (global-val 'boot-strap-flg w))))
         (er-let*
          ((pair (simple-translate-and-eval form nil nil
                                            "The second argument to defpkg"
                                            ctx w state)))
          (let ((tform (car pair))
                (imports (cdr pair)))
            (cond
             ((not (symbol-listp imports))
              (er soft ctx
                  "The second argument of defpkg must eval to a list of ~
                  symbols.  See :DOC defpkg."))
             (t (let* ((imports (sort-symbol-listp imports))
                       (conflict (conflicting-imports imports))
                       (base-symbol (packn (cons name '("-PACKAGE")))))

; Base-symbol is the the base symbol of the rune for the rule added by
; defpkg describing the properties of symbol-package-name on interns
; with the new package.

                  (cond
                   ((member-symbol-name *pkg-witness-name* imports)
                    (er soft ctx
                        "It is illegal to import symbol ~x0 because its name ~
                         has been reserved for a symbol in the package being ~
                         defined."
                        (car (member-symbol-name *pkg-witness-name* imports))))
                   (conflict
                    (er soft ctx
                        "The value of the second (imports) argument of defpkg ~
                        may not contain two symbols with the same symbol name, ~
                        e.g. ~&0.  See :DOC defpkg."
                        conflict))
                   (t (let ((package-entry
                             (and (not (global-val 'boot-strap-flg w))
                                  (find-package-entry
                                   name
                                   (global-val 'known-package-alist w)))))
                        (cond
                         ((and package-entry
                               (not (equal imports
                                           (package-entry-imports package-entry))))
                          (er soft ctx
                              "~@0"
                              (tilde-@-defpkg-error-phrase
                               name package-entry
                               (set-difference-eq
                                imports
                                (package-entry-imports package-entry))
                               (set-difference-eq
                                (package-entry-imports package-entry)
                                imports)
                               (package-entry-book-path package-entry)
                               defpkg-book-path w)))
                         ((and package-entry
                               (not (package-entry-hidden-p package-entry)))
                          (prog2$ (chk-package-reincarnation-import-restrictions
                                   name imports)
                                  (value 'redundant)))
                         (t (er-progn
                             (chk-new-stringp-name 'defpkg name ctx w state)
                             (chk-all-but-new-name base-symbol ctx nil w state)

; Note:  Chk-just-new-name below returns a world which we ignore because
; we know redefinition of 'package base-symbols is disallowed, so the
; world returned is w when an error isn't caused.

; Warning: In maybe-push-undo-stack and maybe-pop-undo-stack we rely
; on the fact that the symbol name-PACKAGE is new!

                             (chk-just-new-name base-symbol
                                                'package nil ctx w state)
                             (prog2$
                              (chk-package-reincarnation-import-restrictions
                               name imports)
                              (value (list* tform
                                            imports
                                            package-entry ; hidden-p is true
                                            )))))))))))))))))))

(defun defpkg-fn (name form state doc book-path event-form)

; Important Note:  Don't change the formals of this function without
; reading the *initial-event-defmacros* discussion in axioms.lisp.

; Like defconst, defpkg evals its second argument.

; We forbid interning into a package before its imports are set once and for
; all.  In the case of the main Lisp package, we assume that we have no control
; over it and simply refuse requests to intern into it.

  (with-ctx-summarized
   (if (output-in-infixp state) event-form (cons 'defpkg name))
   (let ((w (w state))
         (event-form (or event-form
                         (list* 'defpkg name form
                                (if (or doc book-path) (list doc) nil)
                                (if book-path (list book-path) nil)))))
     (er-let* ((doc-pair (translate-doc name doc ctx state))
               (tform-imports-entry
                (chk-acceptable-defpkg name form book-path ctx w state)))
              (cond
               ((eq tform-imports-entry 'redundant)
                (stop-redundant-event state))
               (t
                (let* ((imports (cadr tform-imports-entry))
                       (w1 (global-set
                            'known-package-alist
                            (cons (make-package-entry
                                   :name name
                                   :imports imports
                                   :hidden-p nil
                                   :book-path
                                   (append book-path
                                           (global-val
                                            'include-book-path
                                            w))
                                   :defpkg-event-form event-form
                                   :tterm (car tform-imports-entry))
                                  (if (cddr tform-imports-entry)
                                      (remove-package-entry
                                       name
                                       (known-package-alist state))
                                    (global-val 'known-package-alist w)))
                            w))

; Defpkg adds an axiom, labelled ax below.  We make a :REWRITE rule out of ax.
; Warning: If the axiom added by defpkg changes, be sure to consider the
; initial packages that are not defined with defpkg, e.g., "ACL2".  In
; particular, for each primitive package in *initial-known-package-alist*
; (except for the "COMMON-LISP" package) there is a defaxiom in axioms.lisp
; exactly analogous to the add-rule below.  So if you change this code, change
; that code.

                       (ax
                        `(implies
                          ,(if (null imports)
                               `(if (stringp x)
                                    (if (symbolp y)
                                        (equal (symbol-package-name y)
                                               (quote ,name))
                                      ,*nil*)
                                  ,*nil*)
                             `(if (stringp x)
                                  (if (not (member-symbol-name
                                            x (quote ,imports)))
                                      (if (symbolp y)
                                          (equal (symbol-package-name y)
                                                 (quote ,name))
                                        ,*nil*)
                                    ,*nil*)
                                ,*nil*))
                          (equal (symbol-package-name
                                  (intern-in-package-of-symbol x y))
                                 (quote ,name))))
                       (w2
                        (add-rules
                         (packn (cons name '("-PACKAGE")))
                         `((:REWRITE :COROLLARY ,ax))
                         ax ax (ens state) w1 state))
                       (w3 (update-doc-data-base name doc doc-pair w2)))
                  (pprogn
                   (f-put-global 'packages-created-by-defpkg
                                 (cons name
                                       (f-get-global 'packages-created-by-defpkg
                                                     state))
                                 state)
                   (install-event name
                                  event-form
                                  'defpkg
                                  name
                                  nil
                                  (list 'defpkg name form)
                                  :protect ctx w3 state)))))))))

; We now start the development of deftheory and theory expressions.

; First, please read the Essay on Enabling, Enabled Structures, and
; Theories for a refresher course on such things as runes, common
; theories, and runic theories.  Roughly speaking, theory expressions
; are terms that produce common theories as their results.  Recall
; that a common theory is a truelist of rule name designators.  A rule
; name designator is an object standing for a set of runes; examples
; include APP, which might stand for {(:DEFINITION app)}, (APP), which
; might stand for {(:EXECUTABLE-COUNTERPART app)}, and LEMMA1, which
; might stand for the set of runes {(REWRITE lemma1 . 1) (REWRITE
; lemma1 . 2) (ELIM lemma1)}.  Of course, a rune is a rule name designator
; and stands for the obvious: the singleton set containing that rune.

; To every common theory there corresponds a runic theory, obtained
; from the common theory by unioning together the designated sets of
; runes and then ordering the result by nume.  Runic theories are
; easier to manipulate (e.g., union together) because they are
; ordered.

; To define deftheory we need not define any any "theory manipulation
; functions" (e.g., union-theories, or universal-theory) because
; deftheory just does a full-blown eval of whatever expression the
; user provides.  We could therefore define deftheory now.  But there
; are a lot of useful theory manipulation functions and they are
; generally used only in deftheory and in-theory, so we define them
; now.

; Calls of these functions will be typed by the user in theory
; expressions.  Those expressions will be executed to obtain new
; theories.  Furthermore, the user may well define his own theory
; producing functions which will be mixed in with ours in his
; expressions.  How do we know a "theory expression" will produce a
; theory?  We don't.  We just evaluate it and check the result.  But
; this raises a more serious question: how do we know our theory
; manipulation functions are given theories as their arguments?
; Indeed, they may not be given theories because of misspellings, bugs
; in the user's functions, etc.  Because of the presence of
; user-defined functions in theory expressions we can't syntactically
; check that an expression is ok.  And at the moment we don't see that
; it is worth the trouble of making the user prove "theory theorems"
; such as (THEORYP A W) -> (THEORYP (MY-FN A) W) that would let us so
; analyze his expressions.

; So we have decided to put run-time checks into our theory functions.
; We have two methods available to us: we could put guards on them or
; we could put checks into them.  The latter course does not permit us
; to abort on undesired arguments -- because we don't want theory
; functions to take STATE and be multi-valued.  Thus, once past the
; guards all we can do is coerce unwanted args into acceptable ones.

; There are several sources of tension.  It was such tensions that
; led to the idea of "common" v. "runic" theories and, one level deeper,
; "rule name designators" v. runes.

; (1) When our theory functions are getting input directly from the
;     user we wish they did a throrough job of checking it and were
;     forgiving about such things as order, e.g., sorted otherwise ok
;     lists, so that the user didn't need to worry about order.

; (2) When our theory functions are getting input produced by one of
;     our functions, we wish they didn't check anything so they could
;     just fly.

; (3) These functions have to be admissible under the definitional principle
;     and not cause errors when called on the utter garbage that the user
;     might type.

; (4) Checking the well-formedness of a theory value requires access to
;     wrld.

; We have therefore chosen the following strategy.

; First, all theory manipulation functions take wrld as an argument.
; Some need it, e.g., the function that returns all the available rule
; names.  Others wouldn't need it if we made certain choices on the
; handling of run-time checks.  We've chosen to be uniform: all have
; it.  This uniformity saves the user from having to remember which
; functions do and which don't.

; Second, all theory functions have guards that check that their
; "theory" arguments "common theories."  This means that if a theory
; function is called on utter garbage the user will get an error
; message.  But it means we'll pay the price of scanning each theory
; value on each function entry in his expression to check
; rule-name-designatorp.

; To compute on theories we will convert common theories to runic ones
; (actually, all the way to augmented runic theories) and we will
; always return runic theories because they can be verified faster.
; This causes a second scan every time but in general will not go into
; sorting because our intermediate results will always be ordered.
; This gives us "user-friendliness" for top-level calls of the theory
; functions without (too much?)  overhead.

; Now we define union, intersect, and set-difference for lists of rule
; names.

(defun theory-fn-callp (x)

; We return t or nil.  If t, and the evaluation of x does not cause an error,
; then the result is a runic-theoryp.  Here x is an untranslated term; see also
; theory-fn-translated-callp for translated terms x.  It would be sound to
; return non-nil here if theory-fn-translated-callp returns non-nil, but that
; doesn't seem useful for user-level terms (though we may want to reconsider).

  (and (consp x)
       (member-eq (car x)
                  '(current-theory
                    disable
                    e/d
                    enable
                    executable-counterpart-theory
                    function-theory
                    intersection-theories
                    set-difference-theories
                    theory
                    union-theories
                    universal-theory))
       t))

(defun intersection-augmented-theories-fn1 (lst1 lst2 ans)

; Let lst1 and lst2 be augmented theories: descendingly ordered lists
; of pairs mapping numes to runes.  We return the intersection of the
; two theories -- as a runic theory, not as an augmented runic theory.
; That is, we strip off the numes as we go.  This is unesthetic: it
; would be more symmetric to produce an augmented theory since we take
; in augmented theories.  But this is more efficient because we don't
; have to copy the result later to strip off the numes.

  (cond
   ((null lst1) (revappend ans nil))
   ((null lst2) (revappend ans nil))
   ((= (car (car lst1)) (car (car lst2)))
    (intersection-augmented-theories-fn1 (cdr lst1) (cdr lst2)
                                         (cons (cdr (car lst1)) ans)))
   ((> (car (car lst1)) (car (car lst2)))
    (intersection-augmented-theories-fn1 (cdr lst1) lst2 ans))
   (t (intersection-augmented-theories-fn1 lst1 (cdr lst2) ans))))

(defmacro check-theory (lst wrld ctx form)
  `(cond ((theoryp! ,lst ,wrld)
          ,form)
         (t (er hard ,ctx
                "A theory function has been called on an argument that does ~
                 not represent a theory.  See the **NOTE**s above and see ~
                 :DOC theories."))))

(defun intersection-theories-fn (lst1 lst2 wrld)
  (check-theory
   lst1 wrld 'intersection-theories-fn
   (check-theory
    lst2 wrld 'intersection-theories-fn
    (intersection-augmented-theories-fn1 (augment-theory lst1 wrld)
                                         (augment-theory lst2 wrld)
                                         nil))))

(defmacro intersection-theories (lst1 lst2)

; Warning: The resulting value must be a runic-theoryp.  See theory-fn-callp.

  ":Doc-Section Theories

  intersect two ~il[theories]~/
  ~bv[]
  Example:
  (intersection-theories (current-theory :here)
                         (theory 'arith-patch))~/

  General Form:
  (intersection-theories th1 th2)
  ~ev[]
  where ~c[th1] and ~c[th2] are theories (~pl[theories]).  To each of
  the arguments there corresponds a runic theory.  This function
  returns the intersection of those two runic ~il[theories], represented as
  a list and ordered chronologically.

  This ``function'' is actually a macro that expands to a term
  mentioning the single free variable ~ilc[world].  When theory expressions
  are evaluated by ~ilc[in-theory] or the ~c[:]~ilc[in-theory] hint, ~ilc[world] is bound to
  the current ACL2 ~il[world].~/

  :cited-by theory-functions"

  (list 'intersection-theories-fn
        lst1
        lst2
        'world))

(defun union-augmented-theories-fn1 (lst1 lst2 ans)

; Let lst1 and lst2 be augmented theories: descendingly ordered lists
; of pairs mapping numes to runes.  We return their union as an
; unagumented runic theory.  See intersection-augmented-theories-fn1.

  (cond ((null lst1) (revappend ans (strip-cdrs lst2)))
        ((null lst2) (revappend ans (strip-cdrs lst1)))
        ((int= (car (car lst1)) (car (car lst2)))
         (union-augmented-theories-fn1 (cdr lst1) (cdr lst2)
                                       (cons (cdr (car lst1)) ans)))
        ((> (car (car lst1)) (car (car lst2)))
         (union-augmented-theories-fn1 (cdr lst1) lst2
                                       (cons (cdr (car lst1)) ans)))
        (t (union-augmented-theories-fn1 lst1 (cdr lst2)
                                         (cons (cdr (car lst2)) ans)))))

(defun union-theories-fn1 (lst1 lst2 nume wrld ans)

; Lst2 is an augmented runic theory: descendingly ordered list of pairs mapping
; numes to runes.  Lst1 is an unaugmented runic theory, which may be thought of
; as the strip-cdrs of an augmented runic theory.  Nume is either nil or else
; is the nume of the first element of lst1.  We accumulate into ans and
; ultimately return the result of adding all runes in lst2 to lst1, as an
; unaugmented runic theory.

  (cond ((null lst1) (revappend ans (strip-cdrs lst2)))
        ((null lst2) (revappend ans lst1))
        (t (let ((nume (or nume (runep (car lst1) wrld))))
             (assert$
              nume
              (cond
               ((int= nume (car (car lst2)))
                (union-theories-fn1
                 (cdr lst1) (cdr lst2) nil wrld (cons (car lst1) ans)))
               ((> nume (car (car lst2)))
                (union-theories-fn1
                 (cdr lst1) lst2 nil wrld (cons (car lst1) ans)))
               (t (union-theories-fn1
                   lst1 (cdr lst2) nume wrld (cons (cdar lst2) ans)))))))))

(defun union-theories-fn (lst1 lst2 lst1-known-to-be-runic wrld)

; We make some effort to share structure with lst1 if it is a runic theory,
; else with lst2 if it is a runic theory.  Argument lst1-known-to-be-runic is
; an optimization: if it is true, then lst1 is known to be a runic theory, so
; we can skip the runic-theoryp check.

  (cond
   ((or lst1-known-to-be-runic
        (runic-theoryp lst1 wrld))
    (check-theory lst2 wrld 'union-theories-fn
                  (union-theories-fn1 lst1
                                      (augment-theory lst2 wrld)
                                      nil
                                      wrld
                                      nil)))
   ((runic-theoryp lst2 wrld)
    (check-theory lst1 wrld 'union-theories-fn
                  (union-theories-fn1 lst2
                                      (augment-theory lst1 wrld)
                                      nil
                                      wrld
                                      nil)))
   (t
    (check-theory
     lst1 wrld 'union-theories-fn
     (check-theory
      lst2 wrld 'union-theories-fn
      (union-augmented-theories-fn1

; We know that lst1 is not a runic-theoryp, so we open-code for a call of
; augment-theory, which should be kept in sync with the code below.

       (duplicitous-sort-car
        nil
        (convert-theory-to-unordered-mapping-pairs lst1 wrld))
       (augment-theory lst2 wrld)
       nil))))))

(defmacro union-theories (lst1 lst2)

; Warning: The resulting value must be a runic-theoryp.  See theory-fn-callp.

  ":Doc-Section Theories

  union two ~il[theories]~/
  ~bv[]
  Example:
  (union-theories (current-theory 'lemma3)
                  (theory 'arith-patch))~/

  General Form:
  (union-theories th1 th2)
  ~ev[]
  where ~c[th1] and ~c[th2] are theories (~pl[theories]).  To each of
  the arguments there corresponds a runic theory.  This function
  returns the union of those two runic ~il[theories], represented as a list
  and ordered chronologically.

  This ``function'' is actually a macro that expands to a term
  mentioning the single free variable ~ilc[world].  When theory expressions
  are evaluated by ~ilc[in-theory] or the ~c[:]~ilc[in-theory] hint, ~ilc[world] is bound to
  the current ACL2 ~il[world].~/

  :cited-by theory-functions"

  (cond ((theory-fn-callp lst1)
         (list 'union-theories-fn
               lst1
               lst2
               t
               'world))
        ((theory-fn-callp lst2)
         (list 'union-theories-fn
               lst2
               lst1
               t
               'world))
        (t
         (list 'union-theories-fn
               lst1
               lst2
               nil
               'world))))

(defun set-difference-augmented-theories-fn1 (lst1 lst2 ans)

; Let lst1 and lst2 be augmented theories: descendingly ordered lists
; of pairs mapping numes to runes.  We return their set-difference as
; an unagumented runic theory.  See intersection-augmented-theories-fn1.

  (cond ((null lst1) (revappend ans nil))
        ((null lst2) (revappend ans (strip-cdrs lst1)))
        ((= (car (car lst1)) (car (car lst2)))
         (set-difference-augmented-theories-fn1 (cdr lst1) (cdr lst2) ans))
        ((> (car (car lst1)) (car (car lst2)))
         (set-difference-augmented-theories-fn1
          (cdr lst1) lst2 (cons (cdr (car lst1)) ans)))
        (t (set-difference-augmented-theories-fn1 lst1 (cdr lst2) ans))))

(defun set-difference-theories-fn1 (lst1 lst2 nume wrld ans)

; Lst2 is an augmented runic theory: descendingly ordered list of pairs mapping
; numes to runes.  Lst1 is an unaugmented runic theory, which may be thought of
; as the strip-cdrs of an augmented runic theory.  Nume is either nil or else
; is the nume of the first element of lst1.  We accumulate into ans and
; ultimately return the result of removing all runes in lst2 from lst1, as an
; unaugmented runic theory.

  (cond ((null lst1) (reverse ans))
        ((null lst2) (revappend ans lst1))
        (t (let ((nume (or nume (runep (car lst1) wrld))))
             (assert$
              nume
              (cond
               ((int= nume (car (car lst2)))
                (set-difference-theories-fn1
                 (cdr lst1) (cdr lst2) nil wrld ans))
               ((> nume (car (car lst2)))
                (set-difference-theories-fn1
                 (cdr lst1) lst2 nil wrld (cons (car lst1) ans)))
               (t (set-difference-theories-fn1
                   lst1 (cdr lst2) nume wrld ans))))))))

(defun set-difference-theories-fn (lst1 lst2 lst1-known-to-be-runic wrld)

; We make some effort to share structure with lst1 if it is a runic theory.
; Argument lst1-known-to-be-runic is an optimization: if it is true, then lst1
; is known to be a runic theory, so we can skip the runic-theoryp check.

  (cond
   ((or lst1-known-to-be-runic
        (runic-theoryp lst1 wrld))
    (check-theory lst2 wrld 'set-difference-theories-fn
                  (set-difference-theories-fn1 lst1
                                               (augment-theory lst2 wrld)
                                               nil
                                               wrld
                                               nil)))
   (t
    (check-theory
     lst1 wrld 'set-difference-theories-fn
     (check-theory
      lst2 wrld 'set-difference-theories-fn
      (set-difference-augmented-theories-fn1

; We know that lst1 is not a runic-theoryp, so we open-code for a call of
; augment-theory, which should be kept in sync with the code below.

       (duplicitous-sort-car
        nil
        (convert-theory-to-unordered-mapping-pairs lst1 wrld))
       (augment-theory lst2 wrld)
       nil))))))

(defmacro set-difference-theories (lst1 lst2)

; Warning: The resulting value must be a runic-theoryp.  See theory-fn-callp.

  ":Doc-Section Theories

  difference of two ~il[theories]~/
  ~bv[]
  Example:
  (set-difference-theories (current-theory :here)
                           '(fact (fact)))~/

  General Form:
  (set-difference-theories th1 th2)
  ~ev[]
  where ~c[th1] and ~c[th2] are ~il[theories] (~pl[theories]).  To each of
  the arguments there corresponds a runic theory.  This function
  returns the set-difference of those two runic ~il[theories], represented
  as a list and ordered chronologically.  That is, a ~il[rune] is in the
  result iff it is in the first runic theory but not in the second.

  The standard way to ``disable'' a theory, ~c[lst], is:
  ~c[(in-theory (set-difference-theories (current-theory :here) lst))].

  This ``function'' is actually a macro that expands to a term
  mentioning the single free variable ~ilc[world].  When theory expressions
  are evaluated by ~ilc[in-theory] or the ~c[:]~ilc[in-theory] hint, ~ilc[world] is bound to
  the current ACL2 ~il[world].~/

  :cited-by theory-functions"

  (list 'set-difference-theories-fn
        lst1
        lst2
        (theory-fn-callp lst1)
        'world))

; Now we define a few useful theories.

(defun universal-theory-fn1 (lst ans redefined)

; Lst is a cdr of the current world.  We scan down lst accumulating onto ans
; every rune in every 'runic-mapping-pairs property.  Our final ans is
; descendingly ordered.  We take advantage of the fact that the world is
; ordered reverse-chronologically, so the runes in the first
; 'runic-mapping-pairs we see will have the highest numes.

; If at any point we encounter the 'global-value for the variable
; 'standard-theories then we assume the value is of the form (r-unv r-fn1 r-fn2
; r-fn3), where r-unv is the reversed universal theory as of that world, r-fn1
; is the reversed function symbol theory, r-fn2 is the reversed executable
; counterpart theory, and r-fn3 is the reversed function theory.  If we find
; such a binding we stop and revappend r-unv to our answer and quit.  By this
; hack we permit the precomputation of a big theory and save having to scan
; down world -- which really means save having to swap world into memory.

; At the end of the bootstrap we will save the standard theories just to
; prevent the swapping in of prehistoric conses.

; Note: :REDEF complicates matters.  If a name is redefined the runes based on
; its old definition are invalid.  We can tell that sym has been redefined when
; we encounter on lst a triple of the form (sym RUNIC-MAPPING-PAIRS
; . :ACL2-PROPERTY-UNBOUND).  This means that all runes based on sym
; encountered subsequently must be ignored or deleted (ignored when encountered
; as RUNIC-MAPPING-PAIRS and deleted when seen in the stored standard theories.
; The list redefined contains all such syms encountered.

  (cond ((null lst)
         #+acl2-metering (meter-maid 'universal-theory-fn1 500)
         (reverse ans)) ; unexpected, but correct
        ((eq (cadr (car lst)) 'runic-mapping-pairs)
         #+acl2-metering (setq meter-maid-cnt (1+ meter-maid-cnt))
         (cond
          ((eq (cddr (car lst)) *acl2-property-unbound*)
           (universal-theory-fn1 (cdr lst) ans
                                 (add-to-set-eq (car (car lst)) redefined)))
          ((member-eq (car (car lst)) redefined)
           (universal-theory-fn1 (cdr lst) ans redefined))
          (t (universal-theory-fn1 (cdr lst)
                                   (append-strip-cdrs (cddr (car lst)) ans)
                                   redefined))))
        ((and (eq (car (car lst)) 'standard-theories)
              (eq (cadr (car lst)) 'global-value))
         #+acl2-metering (meter-maid 'universal-theory-fn1 500)
         (revappend-delete-runes-based-on-symbols (car (cddr (car lst)))
                                                  redefined
                                                  ans))
        (t
         #+acl2-metering (setq meter-maid-cnt (1+ meter-maid-cnt))
         (universal-theory-fn1 (cdr lst) ans redefined))))

(defun universal-theory-fn (logical-name wrld)

; Return the theory containing all of the rule names in the world created
; by the event that introduced logical-name.

  (declare (xargs :guard (logical-namep logical-name wrld)))

; It is possible that wrld starts with a triple of the form (name REDEFINED
; . mode) in which case that triple is followed by an arbitrary number of
; triples "renewing" various properties of name.  Among those properties is,
; necessarily, RUNIC-MAPPING-PAIRS.  This situation only arises if we are
; evaluating a theory expression as part of an event that is in fact redefining
; name.  These "mid-event" worlds are odd precisely because they do not start
; on event boundaries (with appropriate interpretation given to the occasional
; saving of worlds and theories).

; Now we are asked to get a theory as of logical-name and hence must decode
; logical name wrt wrld, obtaining some tail of wrld, wrld1.  If we are in the
; act of redefining name then we add to wrld1 the triple unbinding
; RUNIC-MAPPING-PAIRS of name.  Why not add all the renewing triples?  The
; reason is that this is the only renewed property that is relevant to
; universal-theory1, the workhorse here.


  (let* ((wrld1 (decode-logical-name logical-name wrld))
         (wrld2 (if (eq (cadr (car wrld)) 'redefined)
                    (cons (list* (car (car wrld))
                                 'runic-mapping-pairs
                                 *acl2-property-unbound*)
                          wrld1)
                  wrld1)))
    (assert$-runic-theoryp (universal-theory-fn1 wrld2 nil nil)
                           wrld)))

(defmacro universal-theory (logical-name)

; Warning: The resulting value must be a runic-theoryp.  See theory-fn-callp.

  ":Doc-Section Theories

  all rules as of logical name~/
  ~bv[]
  Examples:
  (universal-theory :here)
  (universal-theory 'lemma3)
  ~ev[]
  ~l[logical-name].~/
  ~bv[]
  General Form:
  (universal-theory logical-name)
  ~ev[]
  Returns the theory consisting of all the ~il[rune]s that existed
  immediately after ~ilc[logical-name] was introduced.  ~l[theories]
  and ~pl[logical-name].  The theory includes ~ilc[logical-name] itself
  (if there is a rule by that name).  (Note that since some ~il[events] do
  not introduce rules (e.g., ~ilc[defmacro], ~ilc[defconst] or ~ilc[defthm] with
  ~c[:]~ilc[rule-classes] ~c[nil]), the universal-theory does not necessarily
  include a ~il[rune] for every event name.)  The universal-theory is very
  long and you will probably regret printing it.

  You may experience a fencepost problem in deciding which
  ~il[logical-name] to use.  ~ilc[Deflabel] can always be used to mark
  unambiguously for future reference a particular point in the
  development of your theory.  This is convenient because ~ilc[deflabel]
  does not introduce any rules and hence it doesn't matter if you
  count it as being in the interval or not.  The order of ~il[events] in
  the vicinity of an ~ilc[encapsulate] is confusing.  ~l[encapsulate].

  This ``function'' is actually a macro that expands to a term
  mentioning the single free variable ~ilc[world].  When theory expressions
  are evaluated by ~ilc[in-theory] or the ~c[:]~ilc[in-theory] hint, ~ilc[world] is bound to
  the current ACL2 ~il[world].

  Also ~pl[current-theory].  ~c[Current-theory] is much more commonly used than
  ~c[universal-theory].  The former includes only the ~il[enable]d ~il[rune]s
  as of the given ~ilc[logical-name], which is probably what you want, while
  the latter includes ~il[disable]d ones as well.~/

  :cited-by theory-functions"

  (list 'universal-theory-fn
        logical-name
        'world))

(defun function-theory-fn1 (token lst ans redefined)

; Token is either :DEFINITION, :EXECUTABLE-COUNTERPART or something
; else.  Lst is a cdr of the current world.  We scan down lst and
; accumulate onto ans all of the runes of the indicated type (or both
; if token is neither of the above).

; As in universal-theory-fn1, we also look out for the 'global-value of
; 'standard-theories and for *acl2-property-unbound*.  See the comment there.

  (cond ((null lst)
         #+acl2-metering (meter-maid 'function-theory-fn1 500)
         (reverse ans)) ; unexpected, but correct
        ((eq (cadr (car lst)) 'runic-mapping-pairs)
         #+acl2-metering (setq meter-maid-cnt (1+ meter-maid-cnt))
         (cond
          ((eq (cddr (car lst)) *acl2-property-unbound*)
           (function-theory-fn1 token (cdr lst) ans
                                (add-to-set-eq (car (car lst)) redefined)))
          ((member-eq (car (car lst)) redefined)
           (function-theory-fn1 token (cdr lst) ans redefined))
          ((eq (car (cdr (car (cddr (car lst))))) :DEFINITION)

; The test above extracts the token of the first rune in the mapping pairs and
; this is a function symbol iff it is :DEFINITION.

           (function-theory-fn1 token (cdr lst)
                                (case token
                                      (:DEFINITION
                                       (cons (cdr (car (cddr (car lst)))) ans))
                                      (:EXECUTABLE-COUNTERPART

; Note that we might be looking at the result of storing a :definition rule, in
; which case there will be no :executable-counterpart rune.  So, we check that
; we have something before accumulating it.

                                       (let ((x (cdr (cadr (cddr (car lst))))))
                                         (if (null x)
                                             ans
                                           (cons x ans))))
                                      (otherwise
                                       (cons (cdr (car (cddr (car lst))))
                                             (cons (cdr (cadr (cddr (car lst))))
                                                   ans))))
                                redefined))
          (t (function-theory-fn1 token (cdr lst) ans redefined))))
        ((and (eq (car (car lst)) 'standard-theories)
              (eq (cadr (car lst)) 'global-value))
         #+acl2-metering (meter-maid 'function-theory-fn1 500)
         (revappend-delete-runes-based-on-symbols
          (case token
                (:DEFINITION (cadr (cddr (car lst))))
                (:EXECUTABLE-COUNTERPART (caddr (cddr (car lst))))
                (otherwise (cadddr (cddr (car lst)))))
          redefined
          ans))
        (t
         #+acl2-metering (setq meter-maid-cnt (1+ meter-maid-cnt))
         (function-theory-fn1 token (cdr lst) ans redefined))))

(defun function-theory-fn (logical-name wrld)

; Return the theory containing all of the function names in the world
; created by the user event that introduced logical-name.

  (declare (xargs :guard (logical-namep logical-name wrld)))

; See universal-theory-fn for an explanation of the production of wrld2.

  (let* ((wrld1 (decode-logical-name logical-name wrld))
         (wrld2 (if (eq (cadr (car wrld)) 'redefined)
                    (cons (list* (car (car wrld))
                                 'runic-mapping-pairs
                                 *acl2-property-unbound*)
                          wrld1)
                  wrld1)))
    (assert$-runic-theoryp (function-theory-fn1 :DEFINITION wrld2 nil nil)
                           wrld)))

(defmacro function-theory (logical-name)

; Warning: The resulting value must be a runic-theoryp.  See theory-fn-callp.

  ":Doc-Section Theories

  function symbol rules as of logical name~/
  ~bv[]
  Examples:
  (function-theory :here)
  (function-theory 'lemma3)
  ~ev[]
  ~l[logical-name].~/
  ~bv[]
  General Form:
  (function-theory logical-name)
  ~ev[]
  Returns the theory containing all the ~c[:]~ilc[definition] ~il[rune]s, whether
  ~il[enable]d or not, that existed immediately after ~ilc[logical-name] was
  introduced.  See the documentation for ~il[theories],
  ~il[logical-name] and ~ilc[executable-counterpart-theory].

  You may experience a fencepost problem in deciding which logical
  name to use.  ~ilc[Deflabel] can always be used to mark unambiguously for
  future reference a particular point in the development of your
  theory.  The order of ~il[events] in the vicinity of an ~ilc[encapsulate] is
  confusing.  ~l[encapsulate].

  This ``function'' is actually a macro that expands to a term
  mentioning the single free variable ~ilc[world].  When theory expressions
  are evaluated by ~ilc[in-theory] or the ~c[:]~ilc[in-theory] hint, ~ilc[world] is bound to
  the current ACL2 ~il[world].~/

  :cited-by theory-functions"

  (list 'function-theory-fn
        logical-name
        'world))

(defun executable-counterpart-theory-fn (logical-name wrld)

; Return the theory containing all of the executable-counterpart names
; in the world created by the event that introduced logical-name.

  (declare (xargs :guard (logical-namep logical-name wrld)))

; See universal-theory-fn for an explanation of the production of wrld2.

  (let* ((wrld1 (decode-logical-name logical-name wrld))
         (wrld2 (if (eq (cadr (car wrld)) 'redefined)
                    (cons (list* (car (car wrld))
                                 'runic-mapping-pairs
                                 *acl2-property-unbound*)
                          wrld1)
                  wrld1)))
    (function-theory-fn1 :executable-counterpart wrld2 nil nil)))

(defmacro executable-counterpart-theory (logical-name)

; Warning: The resulting value must be a runic-theoryp.  See theory-fn-callp.

  ":Doc-Section Theories

  executable counterpart rules as of logical name~/
  ~bv[]
  Examples:
  (executable-counterpart-theory :here)
  (executable-counterpart-theory 'lemma3)
  ~ev[]
  ~l[logical-name].~/
  ~bv[]
  General Form:
  (executable-counterpart-theory logical-name)
  ~ev[]
  Returns the theory containing all the ~c[:]~ilc[executable-counterpart]
  ~il[rune]s, whether ~il[enable]d or not, that existed immediately after
  ~ilc[logical-name] was introduced.  See the documentation for
  ~il[theories], ~il[logical-name], ~il[executable-counterpart] and
  ~ilc[function-theory].

  You may experience a fencepost problem in deciding which logical
  name to use.  ~ilc[Deflabel] can always be used to mark unambiguously for
  future reference a particular point in the development of your
  theory.  The order of ~il[events] in the vicinity of an ~ilc[encapsulate] is
  confusing.  ~l[encapsulate].

  This ``function'' is actually a macro that expands to a term
  mentioning the single free variable ~ilc[world].  When theory expressions
  are evaluated by ~ilc[in-theory] or the ~c[:]~ilc[in-theory] hint, ~ilc[world] is bound to
  the current ACL2 ~il[world].~/

  :cited-by theory-functions"

  (list 'executable-counterpart-theory-fn
        logical-name
        'world))

; Having defined the functions for computing the standard theories,
; we'll now define the function for precomputing them.

(defun standard-theories (wrld)
  (list (universal-theory-fn1 wrld nil nil)
        (function-theory-fn1 :definition wrld nil nil)
        (function-theory-fn1 :executable-counterpart wrld nil nil)
        (function-theory-fn1 :both wrld nil nil)))

(defun current-theory-fn (logical-name wrld)

; We return the theory that was enabled in the world created by the
; event that introduced logical-name.

; See universal-theory-fn for an explanation of the production of wrld2.

  (let* ((wrld1 (decode-logical-name logical-name wrld))
         (wrld2 (if (eq (cadr (car wrld)) 'redefined)
                    (cons (list* (car (car wrld))
                                 'runic-mapping-pairs
                                 *acl2-property-unbound*)
                          wrld1)
                  wrld1)))
    (assert$-runic-theoryp (current-theory1 wrld2 nil nil)
                           wrld)))

(defmacro current-theory (logical-name)

; Warning: The resulting value must be a runic-theoryp.  See theory-fn-callp.

  ":Doc-Section Theories

  currently ~il[enable]d rules as of logical name~/
  ~bv[]
  Examples:
  (current-theory :here)
  (current-theory 'lemma3)
  ~ev[]
  ~l[logical-name].~/
  ~bv[]
  General Form:
  (current-theory logical-name)
  ~ev[]
  Returns the current theory as it existed immediately after the
  introduction of ~ilc[logical-name] provided it is evaluated in
  an environment in which the variable symbol WORLD is bound to the
  current ACL2 logical world, ~c[(w state)].  Thus,
  ~bv[]
  ACL2 !>(current-theory :here)
  ~ev[]
  will cause an (unbound variable) error while
  ~bv[]
  ACL2 !>(let ((world (w state))) (current-theory :here))
  ~ev[]
  will return the current theory in world.

  ~l[theories] and ~pl[logical-name] for a discussion of
  theories in general and why the commonly used ``theory functions''
  such as ~c[current-theory] are really macros that expand into terms
  involving the variable ~c[world].  

  The theory returned by ~c[current-theory] is in fact the theory selected by
  the ~ilc[in-theory] event most recently preceding logical name, extended by
  the rules introduced up through ~ilc[logical-name].

  You may experience a fencepost problem in deciding which logical
  name to use.  ~ilc[Deflabel] can always be used to mark unambiguously for
  future reference a particular point in the development of your
  theory.  The order of ~il[events] in the vicinity of an ~ilc[encapsulate] is
  confusing.  ~l[encapsulate].

  This ``function'' is actually a macro that expands to a term
  mentioning the single free variable ~ilc[world].  When theory expressions
  are evaluated by ~ilc[in-theory] or the ~c[:]~ilc[in-theory] hint, ~ilc[world] is bound to
  the current ACL2 ~il[world].~/

  :cited-by theory-functions"

  (list 'current-theory-fn logical-name
        'world))

; Essay on Theory Manipulation Performance

; Below we show some statistics on our theory manipulation functions.
; These are recorded in case we someday change these functions and
; wish to compare the old and new implementations.  The expressions
; shown should be executed in raw lisp, not LP, because they involve
; the time function.  These expressions were executed in a newly
; initialized ACL2.  The times are on a Sparc 2 (Rana).

; The following expression is intended as a "typical" heavy duty
; theory expression.  For the record, the universal theory at the time
; of these tests contained 1307 runes.

#|(let ((world (w *the-live-state*)))
  (time
   (length
    (union-theories
     (intersection-theories (current-theory :here)
                            (executable-counterpart-theory :here))
     (set-difference-theories (universal-theory :here)
                              (function-theory :here))))))|#

; Repeated runs were done.  Typical results were:
;   real time : 0.350 secs
;   run time  : 0.233 secs
;   993

; The use of :here above meant that all the theory functions involved
; just looked up their answers in the 'standard-theories at
; the front of the initialized world.  The following expression forces
; the exploration of the whole world.  In the test, "ACL2-USER" was
; the event printed by :pc -1, i.e., the last event before ending the
; boot.

#|(let ((world (w *the-live-state*)))
  (time
   (length
    (union-theories
     (intersection-theories (current-theory "ACL2-USER")
                            (executable-counterpart-theory "ACL2-USER"))
     (set-difference-theories (universal-theory "ACL2-USER")
                              (function-theory "ACL2-USER"))))))|#

; Repeated tests produced the following typical results.
;   real time : 0.483 secs
;   run time  : 0.383 secs
;   993
; The first run, however, had a real time of almost 10 seconds because
; wrld had to be paged in.

; The final test stresses sorting.  We return to the :here usage to
; get our theories, but we reverse the output every chance we get so
; as force the next theory function to sort.  In addition, we
; strip-cadrs all the input runic theories to force the reconstruction
; of runic theories from the wrld.

#|(let ((world (w *the-live-state*)))
  (time
   (length
    (union-theories
     (reverse
      (intersection-theories
        (reverse (strip-base-symbols (current-theory :here)))
        (reverse (strip-base-symbols (executable-counterpart-theory :here)))))
     (reverse
      (set-difference-theories
        (reverse (strip-base-symbols (universal-theory :here)))
        (reverse (strip-base-symbols (function-theory :here)))))))))|#

; Typical times were
;   real time : 1.383 secs
;   run time  : 0.667 secs
;   411
; The size of the result is smaller because the strip-cadrs identifies
; several runes, e.g., (:DEFINITION fn) and (:EXECUTABLE-COUNTERPART
; fn) both become fn which is then understood as (:DEFINITION fn).

; End of performance data.

(defun end-prehistoric-world (wrld)
  (let* ((wrld1 (global-set-lst
                 (list (list 'untouchable-fns
                             (append *initial-untouchable-fns*
                                     (global-val 'untouchable-fns wrld)))
                       (list 'untouchable-vars
                             (append *initial-untouchable-vars*
                                     (global-val 'untouchable-vars wrld)))
                       (list 'standard-theories
                             (standard-theories wrld))
                       (list 'boot-strap-flg nil)
                       (list 'boot-strap-pass-2 nil)
                       (list 'command-number-baseline-info
                             (let ((command-number-baseline
                                    (next-absolute-command-number wrld)))
                               (make command-number-baseline-info
                                     :current command-number-baseline
                                     :permanent-p t
                                     :original command-number-baseline)))
                       (list 'event-number-baseline
                             (next-absolute-event-number wrld))
                       (list 'skip-proofs-seen nil)
                       (list 'redef-seen nil))
                 (putprop 'acl2-defaults-table
                          'table-alist
                          *initial-acl2-defaults-table*
                          wrld)))
         (wrld2 (update-current-theory (current-theory1 wrld nil nil) wrld1)))
    (add-command-landmark
     :logic
     '(exit-boot-strap-mode)
     nil
     (add-event-landmark
      '(exit-boot-strap-mode)
      'exit-boot-strap-mode
      0
      wrld2))))

(defun theory-namep (name wrld)

; We return t or nil according to whether name is the name of a theory,
; i.e., a name introduced by deftheory.

  (and (symbolp name)
       (not (eq (getprop name 'theory t 'current-acl2-world wrld)
                t))))

(defun theory-fn (name wrld)

; We deliver the value of the defined theory named name.

  (declare (xargs :guard (theory-namep name wrld)))
  (getprop name 'theory nil 'current-acl2-world wrld))

(defmacro theory (name)

; Warning: The resulting value must be a runic-theoryp.  See theory-fn-callp.

  ":Doc-Section Theories

  retrieve named theory~/
  ~bv[]
  Example:
  (theory 'ground-zero)
  ~ev[]
  In the example above, the theory returned is the one in force when ACL2 is
  started up (~pl[ground-zero]).~/

  ~bv[]
  General Form:
  (theory name)
  ~ev[]
  where ~c[name] is the name of a previously executed ~ilc[deftheory] event.
  Returns the named theory.  ~l[theories].

  This ``function'' is actually a macro that expands to a term
  mentioning the single free variable ~ilc[world].  When theory expressions
  are evaluated by ~ilc[in-theory] or the ~c[:]~ilc[in-theory] hint, ~ilc[world] is bound to
  the current ACL2 ~il[world].~/

  :cited-by theory-functions"

  (list 'theory-fn name 'world))

(defun deftheory-fn (name expr state doc event-form)

; Historical Note:  Once upon a time deftheory-fn did not exist even
; though deftheory did.  We defined deftheory as a macro which expanded
; into a defconstant-fn expression.  In particular,

; (deftheory *a* (union *b* (universe w)))

; was mapped to

; (er-let* ((lst (translate-in-theory-hint
;                   '(union *b* (universe w))
;                   nil
;                   '(deftheory . *a*)
;                   (w state)
;                   state)))
;          (defconstant-fn '*a*
;            (list 'quote lst)
;            state
;            nil))

; Thus, the "semantics" of a successful execution of deftheory was that of
; defconstant.  This suffered from letting theories creep into formulas.  For
; example, one could later write in a proposed theorem (member 'foo *a*) and
; the truth of that proposition depended upon the particular theory computed
; for *a*.  This made it impossible to permit either the use of state in
; "theory expressions" (since different theories could be computed for
; identical worlds, depending on ld-skip-proofsp) or the use of deftheory in
; encapsulate (see below).  The state prohibition forced upon us the ugliness
; of permitting the user to reference the current ACL2 world via the free
; variable W in theory expressions, which we bound appropriately before evaling
; the expressions.

; We abandoned the use of defconstant (now defconst) for these reasons.

; Here is a comment that once illustrated why we did not allow deftheory
; to be used in encapsulate:

; We do not allow deftheory expressions in encapsulate.  This may be a
; severe restriction but it is necessary for soundness given the current
; implementation of deftheory.  Consider the following:

; (encapsulate nil
;   (local (defun foo () 1))
;   (deftheory *u* (all-names w))
;   (defthm foo-thm (member 'foo *u*)))

; where all-names is a user defined function that computes the set of
; all names in a given world.  [Note: Intuitively, (all-names w) is
; (universal-theory nil w).  Depending on how event descriptors are
; handled, that may or may not be correct.  In a recent version of
; ACL2, (universal-theory nil w), if used in an encapsulate, had the
; effect of computing all the names in the theory as of the last
; world-chaning form executed by the top-level loop.  But because
; encapsulate did not so mark each term as it executed them,
; universal-theory backed up to the point in w just before the
; encapsulate.  Thus, universal-theory could not be used to get the
; effect intended here.  However, (all-names w) could be defined by
; the user to get what is intended here.]

; When the above sequence is processed in pass 1 of encapsulate *u*
; includes 'foo and hence the defthm succeeds.  But when it is processed
; in pass 2 *u* does not include 'foo and so the assumption of the
; defthm is unsound!  In essence, permitting deftheory in encapsulate is
; equivalent to permitting (w state) in defconst forms.  That is
; disallowed too (as is the use of any variable in an defconst form).
; If you can set a constant as a function of the world, then you can use
; the constant to determine which encapsulate pass you are in.

  (when-logic
   "DEFTHEORY"
   (with-ctx-summarized
    (if (output-in-infixp state) event-form (cons 'deftheory name))
    (let ((wrld (w state))
          (event-form (or event-form
                          (list* 'deftheory name expr
                                 (if doc
                                     (list :doc doc)
                                   nil)))))
      (er-progn
       (chk-all-but-new-name name ctx nil wrld state)
       (er-let*
        ((wrld1 (chk-just-new-name name 'theory nil ctx wrld state))
         (doc-pair (translate-doc name doc ctx state))
         (theory0 (translate-in-theory-hint expr nil ctx wrld1 state)))
        (mv-let (theory theory-augmented-ignore)

; The following call is similar to the one in update-current-theory.  But here,
; our aim is just to create an appropriate theory, without extending the
; world.

                (extend-current-theory
                 (global-val 'current-theory wrld)
                 theory0
                 :none
                 wrld)
                (declare (ignore theory-augmented-ignore))
                (let ((wrld2 (update-doc-data-base
                              name doc doc-pair
                              (putprop name 'theory theory wrld1))))

; Note:  We do not permit DEFTHEORY to be made redundant.  If this
; is changed, change the text of the :doc for redundant-events.

                  (install-event (length theory)
                                 event-form
                                 'deftheory
                                 name
                                 nil
                                 nil
                                 nil ; global theory is unchanged
                                 nil
                                 wrld2 state)))))))))

; And now we move on to the in-theory event, in which we process a theory
; expression into a theory and then load it into the global enabled
; structure.

(defun in-theory-fn (expr state doc event-form)
  (when-logic
   "IN-THEORY"
   (with-ctx-summarized
    (if (output-in-infixp state)
        event-form
      (cond ((atom expr)
             (cond ((null doc)
                    (msg "( IN-THEORY ~x0)" expr))
                   (t (cons 'in-theory expr))))
            ((symbolp (car expr))
             (cond ((null doc)
                    (msg "( IN-THEORY (~x0 ...))"
                         (car expr)))
                   (t (msg "( IN-THEORY (~x0 ...) ...)"
                           (car expr)))))
            ((null doc) "( IN-THEORY (...))")
            (t "( IN-THEORY (...) ...)")))
    (let ((wrld (w state))
          (event-form (or event-form
                          (list* 'in-theory expr
                                 (if doc
                                     (list :doc doc)
                                   nil)))))
      (er-let*
       ((doc-pair (translate-doc nil doc ctx state))
        (theory0 (translate-in-theory-hint expr t ctx wrld state)))
       (let* ((ens1 (ens state))
              (force-xnume-en1 (enabled-numep *force-xnume* ens1))
              (imm-xnume-en1 (enabled-numep *immediate-force-modep-xnume* ens1))
              (wrld1 (update-current-theory theory0 wrld)))

; Note:  We do not permit IN-THEORY to be made redundant.  If this
; is changed, change the text of the :doc for redundant-events.

         (er-let*
          ((val (install-event (length theory0)
                                event-form
                                'in-theory
                                0
                                nil
                                nil
                                :protect
                                nil
                                wrld1 state)))
          (pprogn (if (member-equal
                       expr
                       '((enable (:EXECUTABLE-COUNTERPART
                                  force))
                         (disable (:EXECUTABLE-COUNTERPART
                                   force))
                         (enable (:EXECUTABLE-COUNTERPART
                                  immediate-force-modep))
                         (disable (:EXECUTABLE-COUNTERPART
                                   immediate-force-modep))))
                      state
                    (maybe-warn-about-theory
                     ens1 force-xnume-en1 imm-xnume-en1
                     (ens state) ctx wrld state))
                  (value val)))))))))

(defun in-arithmetic-theory-fn (expr state doc event-form)

; After Version_3.0, the following differs from the fancier in-theory-fn.  The
; latter calls update-current-theory to deal with the 'current-theory and
; related properties, 'current-theory-augmented and 'current-theory-index.
; Someday we may want to make analogous changes to the present function.

  (when-logic
   "IN-ARITHMETIC-THEORY"
   (with-ctx-summarized
    (if (output-in-infixp state)
        event-form
      (cond ((atom expr)
             (cond ((null doc)
                    (msg "( IN-ARITHMETIC-THEORY ~x0)" expr))
                   (t (cons 'in-arithmetic-theory expr))))
            ((symbolp (car expr))
             (cond ((null doc)
                    (msg "( IN-ARITHMETIC-THEORY (~x0 ...))"
                         (car expr)))
                   (t (msg "( IN-ARITHMETIC-THEORY (~x0 ...) ...)"
                           (car expr)))))
            ((null doc) "( IN-ARITHMETIC-THEORY (...))")
            (t "( IN-ARITHMETIC-THEORY (...) ...)")))
    (let ((wrld (w state))
          (event-form (or event-form
                          (list* 'in-arithmetic-theory expr
                                 (if doc
                                     (list :doc doc)
                                   nil)))))
      (cond
       ((not (quotep expr))
        (er soft ctx
            "Arithmetic theory expressions must be quoted constants.  ~
             See :DOC in-arithmetic-theory."))
       (t
        (er-let*
          ((doc-pair (translate-doc nil doc ctx state))
           (theory (translate-in-theory-hint expr t ctx wrld state))
           (ens (load-theory-into-enabled-structure
                 expr theory nil
                 (global-val 'global-arithmetic-enabled-structure wrld)
                 nil nil wrld ctx state)))
          (let ((wrld1 (global-set 'global-arithmetic-enabled-structure ens
                                   wrld)))

; Note:  We do not permit IN-THEORY to be made redundant.  If this
; is changed, change the text of the :doc for redundant-events.

            (install-event (length theory)
                           event-form
                           'in-arithmetic-theory
                           0
                           nil
                           nil
                           nil ; handles its own invariants checking
                           nil
                           wrld1 state)))))))))

(defmacro disable (&rest rst)

; Warning: The resulting value must be a runic-theoryp.  See theory-fn-callp.

  ":Doc-Section Theories

  deletes names from current theory~/
  ~bv[]
  Example:
  (disable fact (fact) associativity-of-app)~/

  General Form:
  (disable name1 name2 ... namek)
  ~ev[]
  where each ~c[namei] is a runic designator; ~pl[theories].  The
  result is the theory that contains all the names in the current
  theory except those listed.  Note that this is merely a function
  that returns a theory.  The result is generally a very long list of
  ~il[rune]s and you will probably regret printing it.

  The standard way to ``disable'' a fixed set of names, is:
  ~bv[]
  (in-theory (disable name1 name2 ... namek)) ; globally
  :in-theory (disable name1 name2 ... namek)  ; locally
  ~ev[]
  Note that all the names are implicitly quoted.  If you wish to
  disable a computed list of names, ~c[lst], use the theory expression
  ~c[(set-difference-theories (current-theory :here) lst)].~/

  :cited-by theory-functions"

  (list 'set-difference-theories-fn
        '(current-theory :here)
        (kwote rst)
        t
        'world))

(defmacro enable (&rest rst)

; Warning: The resulting value must be a runic-theoryp.  See theory-fn-callp.

  ":Doc-Section Theories

  adds names to current theory~/
  ~bv[]
  Example:
  (enable fact (fact) associativity-of-app)~/

  General Form:
  (enable name1 name2 ... namek)
  ~ev[]
  where each ~c[namei] is a runic designator; ~pl[theories].  The
  result is the theory that contains all the names in the current
  theory plus those listed.  Note that this is merely a function that
  returns a theory.  The result is generally a very long list of ~il[rune]s
  and you will probably regret printing it.

  The standard way to ``enable'' a fixed set of names, is
  ~bv[]
  (in-theory (enable name1 name2 ... namek)) ; globally
  :in-theory (enable name1 name2 ... namek)  ; locally
  ~ev[]
  Note that all the names are implicitly quoted.  If you wish to
  enable a computed list of names, ~c[lst], use the theory expression
  ~c[(union-theories (current-theory :here) lst)].~/

  :cited-by theory-functions"

  (list 'union-theories-fn
        '(current-theory :here)
        (kwote rst)
        t
        'world))

; The theory-invariant-table maps arbitrary keys to translated terms
; involving only the variables THEORY and STATE:

(table theory-invariant-table nil nil
       :guard (and (consp val)
                   (consp (cdr val))
                   (let ((tterm (access theory-invariant-record val
                                        :tterm)))
                     (and (termp tterm world)
                          (booleanp (access theory-invariant-record val
                                            :error))
                          (subsetp-eq (all-vars tterm) '(ens state))))))

(defmacro theory-invariant (&whole event-form term &key key (error 't))

  ":Doc-Section Events

  user-specified invariants on ~il[theories]~/
  ~bv[]
  Examples:
  (theory-invariant (not (and (active-runep '(:rewrite left-to-right))
                              (active-runep '(:rewrite right-to-left))))
                    :key my-invariant
                    :error nil)

  ; Equivalent to the above:
  (theory-invariant (incompatible '(:rewrite left-to-right)
                                  '(:rewrite right-to-left))
                    :key my-invariant
                    :error nil)~/

  General Form:
  (theory-invariant term &key key error)
  ~ev[]
  where:~bq[]

  o ~c[term] is a term that uses no variables other than ~c[ens] and
  ~ilc[state];

  o ~c[key] is an arbitrary ``name'' for this invariant (if omitted, an integer
  is generated and used); and

  o ~c[:error] specifies the action to be taken when an invariant is violated
  ~-[] either ~c[nil] if a warning is to be printed, else ~c[t] (the default)
  if an error is to be caused.

  ~eq[]~c[Theory-invariant] is an event that adds to or modifies the ~il[table]
  of user-supplied theory invariants that are checked each time a theory
  expression is evaluated.

  The theory invariant mechanism is provided via a table
  (~pl[table]) named ~c[theory-invariant-table].  In fact, the
  ~c[theory-invariant] ``event'' is just a macro that expands into a use of the
  ~ilc[table] event.  More general access to the ~c[theory-invariant]
  ~il[table] is provided by ~ilc[table] itself.  For example, the ~il[table]
  can be inspected or cleared with ~ilc[table]; you can clear an individual
  theory invariant by setting the invariant to ~c[t], or eliminate all theory
  invariants with the command ~c[(table theory-invariant-table nil nil :clear)].

  ~c[Theory-invariant-table] maps arbitrary keys to records containing terms
  that mention, at most, the variables ~c[ens] and ~ilc[state].  Every time an
  alleged theory expression is evaluated, e.g., in the ~ilc[in-theory] event or
  ~c[:]~ilc[in-theory] hint, each of the terms in ~c[theory-invariant-table] is
  evaluated with ~c[ens] bound to a so-called ``enabled structure'' obtained
  from the theory expression and ~ilc[state] bound to the current ACL2 state
  (~pl[state]).  Users generally need not know about the enabled structure,
  other than that it can be accessed using the macros ~c[active-runep] and
  ~c[incompatible]; ~pl[active-runep] and ~pl[incompatible].  If the result is
  ~c[nil], a message is printed and an error occurs (except, only a warning
  occurs if ~c[:error nil] is specified).  Thus, the ~il[table] can be thought
  of as a list of conjuncts.  Each ~c[term] in the ~il[table] has a ``name,''
  which is just the key under which the term is stored.  When a theory violates
  the restrictions specified by some term, both the name and the term are
  printed.  By calling ~c[theory-invariant] with a new term but the same name,
  you can overwrite that conjunct of the theory invariant; but see the Local
  Redefinition Caveat at the end of this note.  You may want to avoid using
  explicit names, since otherwise the subsequent inclusion of another book that
  defines a theory invariant with the same name will override your theory
  invariant.

  Theory invariants are particularly useful in the context of large rule sets
  intended for re-use.  Such sets often contain conflicting rules, e.g., rules
  that are to be ~il[enable]d when certain function symbols are ~il[disable]d,
  rules that rewrite in opposite directions and thus loop if simultaneously
  ~il[enable]d, groups of rules which should be ~il[enable]d in concert, etc.
  The developer of such rule sets understands these restrictions and probably
  documents them.  The theory invariant mechanism allows the developer to
  codify his restrictions so that the user is alerted when they are violated.

  Since theory invariants are arbitrary terms, macros may be used to
  express commonly used restrictions.  For example, executing the event
  ~bv[]
  (theory-invariant (incompatible (:rewrite left-to-right)
                                  (:rewrite right-to-left)))
  ~ev[]
  would subsequently cause an error any time the current theory contained both
  of the two ~il[rune]s shown.  Of course, ~il[incompatible] is just defined as
  a macro.  Its definition may be inspected with ~c[:pe incompatible].

  In order for a ~c[theory-invariant] event to be accepted, the proposed theory
  invariant must be satisfied by the current theory (~pl[current-theory]).  The
  value returned upon successful execution of the event is the key (whether
  user-supplied or generated).

  Local Redefinition Caveat.  Care needs to be taken when redefining a theory
  invariant in a ~il[local] context.  Consider the following example.

  ~bv[]
  (theory-invariant
   (active-runep '(:definition binary-append))
   :key app-inv)

  (encapsulate
   ()
   (local (theory-invariant t :key app-inv))
   (in-theory (disable binary-append))
   (defthm ...))
  ~ev[]
  The second pass of the ~ilc[encapsulate] will fail, because the
  ~ilc[in-theory] event violates the original ~c[theory-invariant] and the
  ~ilc[local] ~c[theory-invariant] is skipped in the second pass of the
  ~ilc[encapsulate].  Of course, ~ilc[local] ~ilc[theory-invariant]s in
  ~il[books] can cause the analogous problem in the second (~ilc[include-book])
  pass of a ~ilc[certify-book].  In both cases, though, the theory invariants
  are only checked at the conclusion of the (~c[include-book] or
  ~c[encapsulate]) event.  Indeed, theory invariants are checked at the end of
  every event related to ~il[theories], including ~ilc[defun], ~ilc[defthm],
  ~ilc[in-theory], ~ilc[encapsulate], and ~ilc[include-book], except for events
  executed on behalf of an ~ilc[include-book] or the second pass of an
  ~ilc[encapsulate].~/"

; Note: This macro "really" expands to a TABLE event (after computing
; the right args for it!) and hence it should inherit the TABLE event's
; semantics under compilation, which is to say, is a noop.  This
; requirement wasn't noticed until somebody put a THEORY-INVARIANT
; event into a book and then the compiled book compiled the logical
; code below and thus loading the .o file essentially tried to
; reexecute the table event after it had already been executed by the
; .lisp code in the book.  A hard error was caused.

  #-acl2-loop-only
  (declare (ignore event-form term key))
  #-acl2-loop-only

; The clisp compiler (version 2.27) complains whether or not error is declared
; ignored above or not, if the next form is simply nil, perhaps because it is
; assigned a value above as an (&key) arg.  So we go ahead and "use" it here.

  (and (not (equal error error)) nil)

  #+acl2-loop-only
  `(when-logic
    "THEORY-INVARIANT"
    (with-ctx-summarized
     'theory-invariant
     (er-let* ((tterm
                (translate ',term '(nil) nil '(state)
                           'theory-invariant (w state) state)))

; known-stobjs ='(state).  All other variables in term are treated as
; non- stobjs.  This is ok because the :guard on the
; theory-invariant-table will check that the only variables involved
; in tterm are THEORY and STATE and when we ev the term THEORY will be
; bound to a non-stobj (and STATE to state, of course).

              (let* ((inv-table (table-alist 'theory-invariant-table
                                             (w state)))
                     (key ,(if key
                               `(quote ,key)
                             '(1+
                               (length inv-table)))))
                (er-let*
                 ((val
                   (with-output
                    :off summary
                    (table-fn1 'theory-invariant-table
                               key
                               (make theory-invariant-record
                                     :tterm tterm
                                     :error ',error
                                     :untrans-term ',term)
                               :put
                               nil
                               'theory-invariant
                               (w state)
                               state
                               ',event-form))))
                 (cond
                  ((eq val :redundant)
                   (value val))
                  (t
                   (pprogn
                    (cond ((assoc-equal key inv-table)
                           (warning$ 'theory-invariant "Theory"
                                     "An existing theory invariant, named ~
                                      ~x0, is being overwritten by a new ~
                                      theory invariant with that name.~@1"
                                     key
                                     (cond ((f-get-global 'in-local-flg state)
                                            "  Moreover, this override is ~
                                             being done LOCALly; see :DOC ~
                                             theory-invariant (in particular, ~
                                             the Local Redefinition Caveat ~
                                             there), especially if an error ~
                                             occurs.")
                                           (t ""))))
                          (t state))
                    (mv-let (erp val state)
                            (with-output
                             :off summary
                             (in-theory (current-theory :here)))
                            (declare (ignore val))
                            (cond
                             (erp
                              (er soft 'theory-invariant
                                  "The specified theory invariant fails for ~
                                   the current ACL2 world, and hence is ~
                                   rejected.  This failure can probably be ~
                                   overcome by supplying an appropriate ~
                                   in-theory event first."))
                             (t (value key)))))))))))))

(defmacro incompatible (rune1 rune2)
  ":Doc-Section Theories

  declaring that two rules should not both be ~il[enable]d~/
  ~bv[]
  Example:
  (theory-invariant (incompatible (:rewrite left-to-right)
                                  (:rewrite right-to-left)))~/

  General Form:
  (incompatible rune1 rune2)
  ~ev[]
  where ~c[rune1] and ~c[rune2] are two specific ~il[rune]s.  The arguments are
  not evaluated.  ~c[Invariant] is just a macro that expands into a term
  that checks that not both ~il[rune]s are enabled.  ~l[theory-invariant].~/"

  (cond ((and (consp rune1)
              (consp (cdr rune1))
              (symbolp (cadr rune1))
              (consp rune2)
              (consp (cdr rune2))
              (symbolp (cadr rune2)))

; The above condition is similar to conditions in runep and active-runep.

         `(not (and (active-runep ',rune1)
                    (active-runep ',rune2))))
        (t (er hard 'incompatible
               "Each argument to ~x0 should have the shape of a rune, ~
                (:KEYWORD BASE-SYMBOL), unlike ~x1."
               'incompatible
               (or (and (consp rune1)
                        (consp (cdr rune1))
                        (symbolp (cadr rune1))
                        rune2)
                   rune1)))))

; We now begin the development of the encapsulate event.  Often in this
; development we refer to the Encapsulate Essay.  See the comment in
; the function encapsulate-fn, below.

(deflabel signature
  :doc
  ":Doc-Section Miscellaneous

  how to specify the arity of a constrained function~/
  ~bv[]
  Examples:
  ((hd *) => *)
  ((printer * state) => (mv * * state))
  ((mach * mach-state * state) => (mv * mach-state)

  General Form:
  ((fn ...) => *)
  ((fn ...) => stobj)
  or
  ((fn ...) => (mv ...))
  ~ev[]

  where ~c[fn] is the constrained function symbol, ~c[...] is a list
  of asterisks and/or the names of single-threaded objects and
  ~c[stobj] is a single-threaded object name.  ACL2 also supports an
  older style of signature described below after we describe the
  preferred style.~/

  Signatures specify three syntactic aspects of a function symbol: (1)
  the ``arity'' or how many arguments the function takes, (2) the
  ``multiplicity'' or how many results it returns via ~c[MV], and (3)
  which of those arguments and results are single-threaded objects and
  which objects they are.

  For a discussion of single-threaded objects, ~pl[stobj].  For
  the current purposes it is sufficient to know that every single-
  threaded object has a unique symbolic name and that ~ilc[state] is
  the name of the only built-in single-threaded object.  All other
  stobjs are introduced by the user via ~ilc[defstobj].  An object that
  is not a single-threaded object is said to be ``ordinary.''

  The general form of a signature is ~c[((fn x1 ... xn) => val)].  So
  a signature has two parts, separated by the symbol ``=>''.  The
  first part, ~c[(fn x1 ... xn)], is suggestive of a call of the
  constrained function.  The number of ``arguments,'' ~c[n], indicates
  the arity of ~c[fn].  Each ~c[xi] must be a symbol.  If a given
  ~c[xi] is the symbol ``*'' then the corresponding argument must be
  ordinary.  If a given ~c[xi] is any other symbol, that symbol must
  be the name of a single-threaded object and the corresponding
  argument must be that object.  No stobj name may occur twice among the
  ~c[xi].

  The second part, ~c[val], of a signature is suggestive of a term and
  indicates the ``shape'' of the output of ~c[fn].  If ~c[val] is a
  symbol then it must be either the symbol ``*'' or the name of a
  single-threaded object.  In either case, the multiplicity of ~c[fn]
  is 1 and ~c[val] indicates whether the result is ordinary or a
  stobj.  Otherwise, ~c[val] is of the form ~c[(mv y1 ... yk)], where
  ~c[k] > 1.  Each ~c[yi] must be either the symbol ``*'' or the name
  of a stobj.  Such a ~c[val] indicates that ~c[fn] has multiplicity
  ~c[k] and the ~c[yi] indicate which results are ordinary and which
  are stobjs.  No stobj name may occur twice among the ~c[yi].

  Finally, a stobj name may appear in ~c[val] only if appears among the
  ~c[xi].

  Before ACL2 supported user-declared single-threaded objects there
  was only one single-threaded object: ACL2's built-in notion of
  ~ilc[state].  The notion of signature supported then gave a special
  role to the symbol ~c[state] and all other symbols were considered
  to denote ordinary objects.  ACL2 still supports the old form of
  signature, but it is limited to functions that operate on ordinary
  objects or ordinary objects and ~c[state].

  ~bv[]
  Old-Style General Form:
  (fn formals result)
  ~ev[]

  where ~c[fn] is the constrained function symbol, ~c[formals] is a
  suitable list of formal parameters for it, and ~c[result] is either
  a symbol denoting that the function returns one result or else
  ~c[result] is an ~ilc[mv] expression, ~c[(mv s1 ... sn)], where
  ~c[n>1], each ~c[si] is a symbol, indicating that the function
  returns ~c[n] results.  At most one of the formals may be the symbol
  ~c[STATE], indicating that corresponding argument must be ACL2's
  built-in ~ilc[state].  If ~c[state] appears in ~c[formals] then
  ~c[state] may appear once in ~c[result].  All ``variable symbols''
  other than ~c[state] in old style signatures denote ordinary
  objects, regardless of whether the symbol has been defined to be a
  single-threaded object name!

  We also support a variation on old style signatures allowing the user
  to declare which symbols (besides ~c[state]) are to be considered
  single-threaded object names.  This form is
  ~bv[]
  (fn formals result :stobjs names)
  ~ev[]
  where ~c[names] is either the name of a single-threaded object or else
  is a list of such names.  Every name in ~c[names] must have been
  previously defined as a stobj via ~c[defstobj].~/")

(defun gen-formals-from-pretty-flags1 (pretty-flags i avoid)
  (cond ((endp pretty-flags) nil)
        ((eq (car pretty-flags) '*)
         (let ((xi (pack2 'x i)))
           (cond ((member-eq xi avoid)
                  (let ((new-var (genvar 'genvar ;;; ACL2 package
                                         "GENSYM"
                                         1
                                         avoid)))
                    (cons new-var 
                          (gen-formals-from-pretty-flags1
                           (cdr pretty-flags)
                           (+ i 1)
                           (cons new-var avoid)))))
                 (t (cons xi
                          (gen-formals-from-pretty-flags1
                           (cdr pretty-flags)
                           (+ i 1)
                           avoid))))))
        (t (cons (car pretty-flags)
                 (gen-formals-from-pretty-flags1
                  (cdr pretty-flags)
                  (+ i 1)
                  avoid)))))

(defun gen-formals-from-pretty-flags (pretty-flags)

; Given a list of prettyified stobj flags, e.g., '(* * $S * STATE) we
; generate a proposed list of formals, e.g., '(X1 X2 $S X4 STATE).  We
; guarantee that the result is a list of symbols as long as
; pretty-flags.  Furthermore, a non-* in pretty-flags is preserved in
; the same slot in the output.  Furthermore, the symbol generated for
; each * in pretty-flags is unique and not among the symbols in
; pretty-flags.  Finally, STATE is not among the symbols we generate.

  (gen-formals-from-pretty-flags1 pretty-flags 1 pretty-flags))

(defconst *generic-bad-signature-string*
  "The object ~x0 is not a legal signature.  It should be of one of the ~
   following three forms:  ((fn sym1 ... symn) => val) or (fn (var1 ... varn) ~
   val) or (fn (var1 ... varn) val :stobjs names).  But it is of none of these ~
   forms!  See :DOC signature.")

(defun chk-signature (x ctx wrld state)

; Warning: If you change the acceptable form of signatures, change the
; raw lisp code for encapsulate in axioms.lisp and change
; signature-fns.

; X is supposed to be the external form of a signature of a function,
; fn.  This function either causes an error (if x is ill-formed) or
; returns a pair of the form (insig .  wrld1), where insig is the
; internal form of the signature of fn.

; The preferred external form of a signature is of the form:

; ((fn . pretty-flags) => pretty-flag)
; ((fn . pretty-flags) => (mv . pretty-flags))

; where fn is a new or redefinable name, pretty-flag is an asterisk or
; stobj name, and pretty-flags is a true list of pretty flags.  Note
; that this ``preferred form'' is deficient because it does not allow
; the specification of the formals of fn.

; We thus support the old style:

; (fn formals val)

; which is deficient because it does not allow the inclusion of a
; stobj other than STATE.  So we also support a generalization of the
; old style:

; (fn formals val :stobjs names)

; that is the most general of the forms supported and allows a
; specification of the formals of fn while also allowing, in a
; context-free sense, the naming of whatever stobjs are required.

; If we do not cause an error, we return (insig . wrld1), where wrld1
; is the world in which we are to perform the constraint of fn and
; insig is of the form:

; (fn formals' stobjs-in stobjs-out)

; where formals' is an appropriately generated arglist.

  (mv-let
   (msg fn formals val stobjs)
   (case-match
    x
    (((fn . pretty-flags1) arrow val)
     (cond
      ((not (and (symbolp arrow) (equal (symbol-name arrow) "=>")))
       (mv (msg *generic-bad-signature-string* x) nil nil nil nil))
      ((not (and (symbol-listp pretty-flags1)
                 (no-duplicatesp-equal
                  (collect-non-x '* pretty-flags1))))
       (mv (msg
            "The object ~x0 is not a legal signature because ~x1 is ~
             not applied to a true-list of distinct symbols but to ~
             ~x2 instead."
            x fn pretty-flags1)
           nil nil nil nil))
      ((not (or (symbolp val)
                (and (consp val)
                     (eq (car val) 'mv)
                     (symbol-listp (cdr val))
                     (no-duplicatesp-equal
                      (collect-non-x '* (cdr val))))))
       (mv (msg
            "The object ~x0 is not a legal signature because the ~
             result, ... => ~x1, is not a symbol or an MV form ~
             containing distinct symbols."
            x val)
           nil nil nil nil))
      ((or (member-eq t pretty-flags1)
           (member-eq nil pretty-flags1)
           (eq val t)
           (eq val nil)
           (and (consp val)
                (or (member-eq t (cdr val))
                    (member-eq nil (cdr val)))))
       (mv (msg
            "The object ~x0 is not a legal signature because it mentions ~
             T or NIL in places that must be filled by asterisks (*) ~
             or single-threaded object names."
            x)
           nil nil nil nil))
      ((not (subsetp-eq (collect-non-x '* (if (consp val)
                                              (cdr val)
                                            (list val)))
                        pretty-flags1))
       (mv (msg
            "The object ~x0 is not a legal signature because the ~
             result, ~x1, refers to one or more single-threaded ~
             objects, ~&2, not displayed among the inputs in ~x3."
            x
            val
            (set-difference-eq (if (consp val)
                                   (cdr val)
                                 (list val))
                               (cons '* pretty-flags1))
            (cons fn pretty-flags1))
           nil nil nil nil))
      (t
       (let* ((formals (gen-formals-from-pretty-flags pretty-flags1))

; Note:  Stobjs will contain duplicates iff formals does.  Stobjs will
; contain STATE iff formals does.

              (stobjs (collect-non-x '* pretty-flags1)))
         (mv nil fn formals val stobjs)))))
    ((fn formals val)
     (cond ((true-listp formals)
            (let ((stobjs (if (member-eq 'state formals) '(state) nil)))
              (mv nil fn formals val stobjs)))
           (t (mv (msg
                   "The object ~x0 is not a legal signature.  It ~
                    appears to be in the form (fn (var1 ... varn) ~
                    val) but is actually of the form (fn (var1 ... . ~
                    varn) val)!"
                   x)
                  nil nil nil nil))))
    ((fn formals val ':stobjs stobjs)
     (cond ((and (true-listp formals)
                 (or (symbolp stobjs)
                     (true-listp stobjs)))
            (let ((stobjs (if (and (member-eq 'state formals)
                                   (not (member-eq 'state
                                                   (if (symbolp stobjs)
                                                       (list stobjs)
                                                     stobjs))))
                              (cons 'state
                                    (if (symbolp stobjs)
                                        (list stobjs)
                                      stobjs))
                            (if (symbolp stobjs)
                                (list stobjs)
                              stobjs))))
              (mv nil fn formals val stobjs)))
           (t (mv (msg
                   "The object ~x0 is not a legal signature, either ~
                    because the proffered formals are not a true-list ~
                    or because the proffered stobj names are ~
                    ill-formed.  The stobj names are expected to be ~
                    either a single symbol or a true list of symbols."
                   x)
                  nil nil nil nil))))
    (& (mv (msg *generic-bad-signature-string* x) nil nil nil nil)))
   (cond
    (msg (er soft ctx "~@0" msg))
    (t
     (er-progn
      (chk-all-but-new-name fn ctx 'constrained-function wrld state)
      (chk-arglist formals t ctx wrld state)
      (chk-all-stobj-names stobjs
                           (msg "~x0" x)
                           ctx wrld state)
      (er-let*
        ((wrld1 (chk-just-new-name fn 'function nil ctx wrld state)))
        (er-progn
         (cond ((not (or (symbolp val)
                         (and (consp val)
                              (eq (car val) 'mv)
                              (symbol-listp (cdr val))
                              (> (length val) 2))))
                (er soft ctx
                    "The purported signature ~x0 is not a legal ~
                      signature because ~x1 is not a legal output ~
                      description.  Such a description should either ~
                      be a symbol or of the form (mv sym1 ... symn), ~
                      where n>=2."
                    x val))
               (t (value nil)))
         (let* ((syms (cond ((symbolp val) (list val))
                            (t (cdr val))))
                (stobjs-in (compute-stobj-flags formals
                                                stobjs
                                                wrld))
                (stobjs-out (compute-stobj-flags syms
                                                 stobjs
                                                 wrld)))
           (cond
            ((not (subsetp (collect-non-x nil stobjs-out)
                           (collect-non-x nil stobjs-in)))
             (er soft ctx
                 "It is impossible to return single-threaded objects ~
                 (such as ~&0) that are not among the formals!  Thus, ~
                 the input signature ~x1 and the output signature ~x2 ~
                 are incompatible."
                 (set-difference-eq (collect-non-x nil stobjs-out)
                                    (collect-non-x nil stobjs-in))
                 formals
                 val))
            ((not (no-duplicatesp (collect-non-x nil stobjs-out)))
             (er soft ctx
                 "It is illegal to return the same single-threaded ~
                 object in more than one position of the output ~
                 signature.  Thus, ~x0 is illegal because ~&1 ~
                 ~#1~[is~/are~] duplicated."
                 val
                 (duplicates (collect-non-x nil stobjs-out))))
            (t (value (cons (list fn
                                  formals
                                  stobjs-in
                                  stobjs-out)
                            wrld1))))))))))))

(defun chk-signatures (signatures ctx wrld state)

; We return a pair containing the list of internal signatures and the
; final wrld in which we are to do the introduction of these fns, or
; else cause an error.

  (cond ((atom signatures)
         (cond ((null signatures) (value (cons nil wrld)))
               (t (er soft ctx
                      "The list of the signatures of the functions ~
                       constrained by an encapsulation is supposed to ~
                       be a true list, but yours ends in ~x0.  See ~
                       :DOC encapsulate."
                      signatures))))
        ((and (consp (cdr signatures))
              (symbolp (cadr signatures))
              (equal (symbol-name (cadr signatures)) "=>"))

; This clause is meant as an optimization helpful to the user.  It is
; an optimization because if we didn't have it here we would proceed
; to apply chk-signature first the (car signatures) -- which will
; probably fail -- and then to '=> -- which would certainly fail.
; These error messages are less understandable than the one we
; generate here.

         (er soft ctx
             "The signatures argument of ENCAPSULATE is supposed to ~
              be a list of signatures.  But you have provided ~x0, ~
              which might be a single signature.  Try writing ~x1."
             signatures
             (list signatures)))
        (t (er-let* ((pair1 (chk-signature (car signatures)
                                           ctx wrld state))
                     (pair2 (chk-signatures (cdr signatures)
                                            ctx (cdr pair1) state)))
                    (let ((insig (car pair1))
                          (insigs (car pair2))
                          (wrld1 (cdr pair2)))
                      (cond ((assoc-eq (car insig) insigs)
                             (er soft ctx
                                 "The name ~x0 is mentioned twice in the ~
                                  signatures of this encapsulation. See :DOC ~
                                  encapsulate."
                                 (car insig)))
                            (t (value (cons (cons insig insigs) wrld1)))))))))

(defun chk-acceptable-encapsulate1 (signatures form-lst ctx wrld state)

; This function checks that form-lst is a plausible list of forms to evaluate
; and that signatures parses into a list of function signatures for new
; function symbols.  We return the internal signatures and the world in which
; they are to be introduced, as a pair (insigs . wrld1).  This function is
; executed before the first pass of encapsulate.

  (er-progn
   (cond ((not (and (true-listp form-lst)
                    (consp form-lst)
                    (consp (car form-lst))))

; Observe that if the car is not a consp then it couldn't possibly be an
; event.  We check this particular case because we fear the user might get
; confused and write an explicit (progn expr1 ...  exprn) or some other
; single expression and this will catch all but the open lambda case.

          (er soft ctx
              "The arguments to encapsulate, after the first, are ~
               each supposed to be embedded event forms.  There must ~
               be at least one form.  See :DOC encapsulate and :DOC ~
               embedded-event-form."))
         (t (value nil)))
   (chk-signatures signatures ctx wrld state)))

; The following is a complete list of the macros that are considered
; "primitive event macros".  This list includes every macro that calls
; install-event except for defpkg, which is omitted as
; explained below.  In addition, the list includes defun (which is
; just a special call of defuns).  Every name on this list has the
; property that while it takes state as an argument and possibly
; changes it, the world it produces is a function only of the world in
; the incoming state and the other arguments.  The function does not
; change the world as a function of, say, some global variable in the
; state.

; The claim above, about changing the world, is inaccurate for include-book!
; It changes the world as a function of the contents of some arbitrarily
; named input object file.  How this can be explained, I'm not sure.

; All event functions have the property that they install into state
; the world they produce, when they return non-erroneously.  More
; subtly they have the property that when the cause an error, they do
; not change the installed world.  For simple events, such as DEFUN
; and DEFTHM, this is ensured by not installing any world until the
; final STOP-EVENT.  But for compound events, such as ENCAPSULATE and
; INCLUDE-BOOK, it is ensured by the more expensive use of
; REVERT-WORLD-ON-ERROR.

(defconst *primitive-event-macros*

; Warning:  See the warnings below!

  '(defun
     #+:non-standard-analysis
     defun-std
     mutual-recursion
     defuns
     defthm
     #+:non-standard-analysis
     defthm-std
     defaxiom
     defconst
     defstobj
;    defpkg                   ; We prohibit defpkgs except in very
                              ; special places.  See below.
     deflabel
     defdoc
     deftheory
     defchoose
     verify-guards
     defmacro
     in-theory
     in-arithmetic-theory
     push-untouchable
     remove-untouchable
     reset-prehistory
     set-body
     table
     progn
     progn!
     encapsulate
     include-book
     add-include-book-dir
     delete-include-book-dir
     comp
     verify-termination
     add-match-free-override
     theory-invariant
     logic program
     add-default-hints!
     remove-default-hints!
     set-match-free-default
     set-enforce-redundancy
     set-verify-guards-eagerness
     set-non-linearp
     set-compile-fns set-measure-function set-well-founded-relation
     set-invisible-fns-table
     set-backchain-limit
     set-bogus-mutual-recursion-ok
     set-irrelevant-formals-ok
     set-ignore-ok
     set-inhibit-warnings set-state-ok
     set-let*-abstractionp
     set-nu-rewriter-mode
     set-case-split-limitations
     set-default-hints!
     set-rewrite-stack-limit
     defttag))

; Warning: If a symbol is on this list then it is allowed into books.
; If it is allowed into books, it will be compiled.  Thus, if you add a
; symbol to this list you must consider how compile will behave on it
; and what will happen when the .o file is loaded.  Most of the symbols
; on this list have #-acl2-loop-only definitions that make them
; no-ops.  At least one, defstub, expands into a perfectly suitable
; form involving the others and hence inherits its expansion's
; semantics for the compiler.

; Warning: If this list is changed, inspect the following definitions,
; down through CHK-EMBEDDED-EVENT-FORM.  Also consider modifying the
; list *fmt-ctx-spacers* as well.

; We define later the notion of an embedded event.  Only such events
; can be included in the body of an ENCAPSULATE or a file named by
; INCLUDE-BOOK.

; We do not allow defpkg as an embedded event.  In fact, we do not allow
; defpkg anywhere in a blessed set of files except in files that contain
; nothing but top-level defpkg forms (and those files must not be compiled).
; The reason is explained in deflabel embedded-event-form below.

; Once upon a time we allowed in-package expressions inside of
; encapsulates, in a "second class" way.  That is, they were not
; allowed to be hidden in LOCAL forms.  But the whole idea of putting
; in-package expressions in encapsulated event lists is silly:
; In-package is meant to change the package into which subsequent
; forms are read.  But no reading is being done by encapsulate and the
; entire encapsulate event list is read into whatever was the current
; package when the encapsulate was read.

; Here is an example of why in-package should never be hidden (i.e.,
; in LOCAL), even in a top-level list of events in a file.

; Consider the following list of events:

; (DEFPKG ACL2-MY-PACKAGE '(DEFTHM SYMBOL-PACKAGE-NAME EQUAL))

; (LOCAL (IN-PACKAGE "ACL2-MY-PACKAGE"))

; (DEFTHM GOTCHA (EQUAL (SYMBOL-PACKAGE-NAME 'IF) "ACL2-MY-PACKAGE"))

; When processed in pass 1, the IN-PACKAGE is executed and thus
; the subsequent form (and hence the symbol 'IF) is read into package
; ACL2-MY-PACKAGE.  Thus, the equality evaluates to T and GOTCHA is a
; theorem.  But when processed in pass 2, the IN-PACKAGE is not
; executed and the subsequent form is read into the "ACL2" package.  The
; equality evaluates to NIL and GOTCHA is not a theorem.

(deflabel embedded-event-form  :doc
  ":Doc-Section Miscellaneous

  forms that may be embedded in other ~il[events]~/
  ~bv[]
  Examples:
  (defun hd (x) (if (consp x) (car x) 0))
  (local (defthm lemma23 ...))
  (progn (defun fn1 ...)
         (local (defun fn2 ...))
         ...)~/

  General Form:
  An embedded event form is a term, x, such that:~ev[]~bq[]

    ~c[x] is a call of an event function other than ~ilc[DEFPKG] (~pl[events] for
    a listing of the event functions);

    ~c[x] is of the form ~c[(]~ilc[LOCAL]~c[ x1)] where ~c[x1] is an embedded
    event form;

    ~c[x] is of the form ~c[(]~ilc[SKIP-PROOFS]~c[ x1)] where ~c[x1] is an
    embedded event form;

    ~c[x] is of the form ~c[(]~ilc[MAKE-EVENT]~c[ &)], where ~c[&] is any term
    whose expansion is an embedded event (~pl[make-event]);

    ~c[x] is of the form ~c[(]~ilc[WITH-OUTPUT]~c[ ... x1)] where ~c[x1] is an
    embedded event form;

    ~c[x] is of the form ~c[(VALUE-TRIPLE &)], where ~c[&] is any term;

    ~c[x] is a call of ~ilc[ENCAPSULATE], ~ilc[PROGN], ~ilc[PROGN!], or
    ~ilc[INCLUDE-BOOK];

    ~c[x] macroexpands to one of the forms above; or

    [intended only for the implementation] ~c[x] is
    ~c[(RECORD-EXPANSION x1 x2)], where ~c[x1] and ~c[x2] are embedded event
    forms.

  ~eq[]
  An exception: an embedded event form may not set the
  ~ilc[acl2-defaults-table] when in the context of ~ilc[local].  Thus for example,
  the form
  ~bv[]
  (local (table acl2-defaults-table :defun-mode :program))
  ~ev[]
  is not an embedded event form, nor is the form ~c[(local (program))],
  since the latter sets the ~ilc[acl2-defaults-table] implicitly.  An
  example at the end of the discussion below illustrates why there is
  this restriction.

  Only embedded event forms are allowed in a book after its initial
  ~ilc[in-package] form.  ~l[books].  However, you may find that
  ~ilc[make-event] allows you to get the effect you want for a form that is not
  an embedded event form.  For example, you can put the following into a book,
  which assigns the value 17 to ~ilc[state] global variable ~c[x]:
  ~bv[]
  (make-event (er-progn (assign x 17)
                        (value '(value-triple nil)))
              :check-expansion t)
  ~ev[]

  When an embedded event is executed while ~ilc[ld-skip-proofsp] is
  ~c[']~ilc[include-book], those parts of it inside ~ilc[local] forms are ignored.
  Thus,
  ~bv[]
     (progn (defun f1 () 1)
            (local (defun f2 () 2))
            (defun f3 () 3))
  ~ev[]
  will define ~c[f1], ~c[f2], and ~c[f3] when ~ilc[ld-skip-proofsp] is ~c[nil] but will
  define only ~c[f1] and ~c[f3] when ~ilc[ld-skip-proofsp] is ~c[']~ilc[include-book].

  ~em[Discussion:]

  ~ilc[Encapsulate], ~ilc[progn], and ~ilc[include-book] place restrictions on
  the kinds of forms that may be processed.  These restrictions ensure that the
  non-local ~il[events] are indeed admissible provided that the sequence of
  ~ilc[local] and non-local ~il[events] is admissible when proofs are done,
  i.e., when ~c[ld-skip-proofs] is ~c[nil].  But ~ilc[progn!] places no such
  restrictions, hence is potentially dangerous and should be avoided unless you
  understand the ramifications; so it is illegal unless there is an active
  trust tag (~pl[defttag]).

  ~ilc[Local] permits the hiding of an event or group of ~il[events] in the
  sense that ~ilc[local] ~il[events] are processed when we are trying to
  establish the admissibility of a sequence of ~il[events] embedded in
  ~ilc[encapsulate] forms or in ~il[books], but are ignored when we are
  constructing the ~il[world] produced by assuming that sequence.  Thus, for
  example, a particularly ugly and inefficient ~c[:]~ilc[rewrite] rule might be
  made ~ilc[local] to an ~il[encapsulate] that ``exports'' a desirable theorem
  whose proof requires the ugly lemma.

  To see why we can't allow just anything in as an embedded event,
  consider allowing the form
  ~bv[]
  (if (ld-skip-proofsp state)
      (defun foo () 2)
      (defun foo () 1))
  ~ev[]
  followed by
  ~bv[]
  (defthm foo-is-1 (equal (foo) 1)).
  ~ev[]
  When we process the ~il[events] with ~ilc[ld-skip-proofsp], ~c[nil] the second
  ~ilc[defun] is executed and the ~ilc[defthm] succeeds.  But when we process the
  ~il[events] with ~ilc[ld-skip-proofsp] ~c[']~ilc[include-book], the second ~ilc[defun] is
  executed, so that ~c[foo] no longer has the same definition it did when
  we proved ~c[foo-is-1].  Thus, an invalid formula is assumed when we
  process the ~ilc[defthm] while skipping proofs.  Thus, the first form
  above is not a legal embedded event form.

  If you encounter a situation where these restrictions seem to prevent you
  from doing what you want to do, then you may find ~c[make-event] to be
  helpful.  ~l[make-event].

  ~ilc[Defpkg] is not allowed because it affects how things are read after
  it is executed.  But all the forms embedded in an event are read
  before any are executed.  That is,
  ~bv[]
  (encapsulate nil
               (defpkg \"MY-PKG\" nil)
               (defun foo () 'my-pkg::bar))
  ~ev[]
  makes no sense since ~c[my-pkg::bar] must have been read before the
  ~ilc[defpkg] for ~c[\"MY-PKG\"] was executed.

  Finally, let us elaborate on the restriction mentioned earlier
  related to the ~ilc[acl2-defaults-table].  Consider the following form.
  ~bv[]
  (encapsulate
   ()
   (local (program))
   (defun foo (x)
     (if (equal 0 x)
         0
       (1+ (foo (- x))))))
  ~ev[]
  ~l[local-incompatibility] for a discussion of how ~ilc[encapsulate]
  processes event forms.  Briefly, on the first pass through the
  ~il[events] the definition of ~c[foo] will be accepted in ~ilc[defun] mode
  ~c[:]~ilc[program], and hence accepted.  But on the second pass the form
  ~c[(local (program))] is skipped because it is marked as ~ilc[local], and
  hence ~c[foo] is accepted in ~ilc[defun] mode ~c[:]~ilc[logic].  Yet, no proof has been
  performed in order to admit ~c[foo], and in fact, it is not hard to
  prove a contradiction from this definition!~/")

; One can imagine adding new event forms.  The requirement is that
; either they not take state as an argument or else they not be
; sensitive to any part of state except the current ACL2 world.

(defun name-introduced (trip functionp)

; Trip is a triple from a world alist.  We seek to determine whether
; this triple introduces a new name, and if so, which name.  We return
; the name or nil.  If functionp is T we only return function names.
; That is, we return nil if the name introduced is not the name of a
; function, e.g., is a theorem or constant.  Otherwise, we return any
; logical name introduced.  The event functions are listed below.
; Beside each is listed the triple that we take as the unique
; indication that that event introduced name.  Only those having
; FORMALS are considered to be function names.

; event function            identifying triple

; defun-fn                   (name FORMALS . &)
; defuns-fn                  (name FORMALS . &)
; defthm-fn                  (name THEOREM . &)
; defaxiom-fn                (name THEOREM . &)
; defconst-fn                (name CONST . &)
; defstobj-fn                (name STOBJ . names)
;                                [Name is a single-threaded
;                                 object, e.g., $st, and has the
;                                 associated recognizers, accessors
;                                 and updaters.  But those names are
;                                 considered introduced by their
;                                 associated FORMALS triples.]
; deflabel-fn                (name LABEL . T)
; defdoc-fn                  ---
; deftheory-fn               (name THEORY . &)
; defchoose-fn               (name FORMALS . &)
; verify-guards-fn           ---
; defmacro-fn                (name MACRO-BODY . &)
; in-theory-fn               ---
; in-arithmetic-theory-fn    ---
; push-untouchable-fn        ---
; remove-untouchable-fn      ---
; reset-prehistory           ---
; set-body-fn                ---
; table-fn                   ---
; encapsulate-fn             --- [However, the signature functions
;                                 are introduced with (name FORMALS . &)
;                                 and those names, along with any others
;                                 introduced by the embedded events, are
;                                 returned.]
; include-book-fn            (CERTIFICATION-TUPLE GLOBAL-VALUE 
;                              ("name" "user name" "short name"
;                               cert-annotations . chk-sum))

; Those marked "---" introduce no names.

; If redefinition has occurred we have to avoid being fooled by trips such
; as (name FORMALS . *acl2-property-unbound*) and
; (name THEOREM . *acl2-property-unbound*).

  (cond ((eq (cddr trip) *acl2-property-unbound*)
         nil)
        ((eq (cadr trip) 'formals)
         (car trip))
        (functionp nil)
        ((member-eq (cadr trip) '(theorem const macro-body label theory stobj))
         (car trip))
        ((and (eq (car trip) 'certification-tuple)
              (eq (cadr trip) 'global-value)
              (cddr trip))

; The initial value of 'certification-tuple is nil (see initialize-
; world-globals) so we filter it out.  Observe that name is a string
; here.  This name is not the name that occurs in the include-book
; event -- that name is called "user name" in the identifying triple
; column above -- but is in fact the full name of the book, complete
; with the current-book-directory.

         (car (cddr trip)))
        (t nil)))

(defun chk-embedded-event-form-orig-form-msg (orig-form state)
  (cond (orig-form
         (msg "  Note: the above form was encountered during processing of ~X01."
              orig-form
              (term-evisc-tuple t state)))
        (t "")))

(defun chk-embedded-event-form
  (form orig-form wrld ctx state names portcullisp in-local-flg
        in-encapsulatep make-event-chk)

; Note: For a test of this function, see the reference to foo.lisp
; below.

; Orig-form is used for error reporting.  It is either nil, indicating
; that errors should refer to form, or else it is a form from a
; superior call of this function.  So it is typical, though not
; required, to call this with orig-form = nil at the top level.  If we
; encounter a macro call and orig-form is nil, then we set orig-form
; to the macro call so that the user can see that macro call if the
; check fails.

; This function checks that form is a tree whose tips are calls of the
; symbols listed in names, and whose interior nodes are each of one of
; the forms:

; (local &)
; (value-triple #)
; (skip-proofs &)
; (with-output ... &)
; (make-event #)

; where each & is checked.  The # forms above are unrestricted,
; although the the result of expanding the argument of make-event (by
; evaluation) is checked.  Note that both 'encapsulate and 'progn are
; typically in names, and their sub-events aren't checked by this
; function until evaluation time.

; In addition, if portcullisp is t we are checking that the forms are
; acceptable as the portcullis of some book and we enforce the
; additional restriction noted below.

;   (local &) is illegal because such a command would be skipped
;   when executing the portcullis during the subsequent include-book.

; Formerly we also checked here that include-book is only applied to
; absolute pathnames.  That was important for insuring that the book
; that has been read into the certification world is not dependent
; upon :cbd.  Remember that (include-book "file") will find its way
; into the portcullis of the book we are certifying and there is no
; way of knowing in the portcullis which directory that book comes
; from if it doesn't explicitly say.  However, we now use
; fix-portcullis-cmds to modify include-book forms that use relative
; pathnames so that they use absolute pathnames instead, or cause an
; error trying.

; We allow defaxioms skip-proofs, and defttags in the portcullis, but
; we mark the book's certificate appropriately.

; If in-local-flg is t, we enforce the restriction that (table
; acl2-defaults-table ...) is illegal, even if table is among names,
; because we do not permit acl2-defaults-table to be changed locally.
; Similarly, defun-mode events and set-compile-fns events are illegal.
; (We used to make these restrictions when portcullisp is t, because
; we restored the initial acl2-defaults-table before certification,
; and hence it was misguided for the user to be setting the defun-mode
; or the compile flag in the certification world since they were
; irrelevant to the world in which the certification is done.)  Note
; that a value of 'dynamic for in-local-flg means that we are locally
; including a book but are not in the lexical scope of a local within
; that book, in which case it is fine to set the acl2-defaults-table.

; Moreover, we do not allow local defaxiom events.  Imagine locally
; including a book that has nil as a defaxiom.  You can prove anything
; you want in your book, and then when you later include the book,
; there will be no trace of the defaxiom in your logical world!

; We do not check that the tips are well-formed calls of the named
; functions (though we do ensure that they are all true lists).

; If names is *primitive-event-macros* and form can be translated and
; evaluated without error, then it is in fact an embedded event form
; as described in :DOC embedded-event-form.

; We sometimes call this function with names extended by the addition
; of 'DEFPKG.

; If form is rejected, the error message is that printed by str, with
; #\0 bound to the subform (of form) that was rejected.

; We return a value triple (mv erp val state).  If erp is nil then val
; is the form to be evaluated.  Generally that is the result of
; macroexpanding the input form.  However, if (perhaps after some
; macroexpansion) form is a call of local that should be skipped, then
; val is nil.

  (let* ((er-str

; Below, the additional er arguments are as follows:
; ~@1: a reason specific to the context, or "" if none is called for.
; ~@2: original form message.
; ~@3: additional explanation, or "".

          (if portcullisp
              "The command ~x0, used in the construction of the current ~
               world, cannot be included in the portcullis of a certified ~
               book~@1.  See :DOC portcullis.~@2~@3"
            "The form ~x0 is not an embedded event form~@1.  See :DOC ~
             embedded-event-form.~@2~@3"))
         (local-str "The form ~x0 is not an embedded event form in the ~
                     context of LOCAL~@1.  See :DOC embedded-event-form.~@2~@3")
         (encap-str "The form ~x0 is not an embedded event form in the ~
                     context of ENCAPSULATE~@1.  See :DOC ~
                     embedded-event-form.~@2~@3"))
    (cond ((or (atom form)
               (not (symbolp (car form)))
               (not (true-listp (cdr form))))
           (er soft ctx er-str
               form
               ""
               (chk-embedded-event-form-orig-form-msg orig-form state)
               ""))
          ((and (eq (car form) 'local)
                (consp (cdr form))
                (null (cddr form)))
           (cond
            (portcullisp

; We will miss this case if we have an ill-formed call of local:
; (not (and (consp (cdr form)) (null (cddr form)))).  However, macroexpansion
; of local will fail later, so that isn't a problem.

             (er soft ctx er-str
                 form
                 " because LOCAL commands are not executed by include-book"
                 (chk-embedded-event-form-orig-form-msg orig-form state)
                 ""))
            ((eq (ld-skip-proofsp state) 'include-book)

; Keep this in sync with the definition of the macro local; if we evaluate the
; cadr of the form there, then we need to check it here.

             (value nil))
            (t
             (er-let* ((new-form (chk-embedded-event-form
                                  (cadr form) orig-form wrld ctx state names
                                  portcullisp t in-encapsulatep
                                  make-event-chk)))
                      (value (and new-form (list (car form) new-form)))))))
          ((and (eq in-local-flg t)
                (consp form)
                (eq (car form) 'table)
                (consp (cdr form))
                (eq (cadr form) 'acl2-defaults-table))
           (er soft ctx local-str
               form
               " because it sets the acl2-defaults-table in a local context.  ~
                A local context is not useful when setting this table, since ~
                the acl2-defaults-table is restored upon completion of ~
                encapsulate, include-book, and certify-book forms; that is, ~
                no changes to the acl2-defaults-table are exported" 
               (chk-embedded-event-form-orig-form-msg orig-form state)
               ""))
          ((and (eq in-local-flg t)
                (consp form)
                (member-eq (car form)
                           '(add-include-book-dir
                             add-match-free-override
                             delete-include-book-dir
                             logic
                             program
                             set-backchain-limit
                             set-bogus-mutual-recursion-ok
                             set-case-split-limitations
                             set-compile-fns
                             set-default-backchain-limit
                             set-enforce-redundancy
                             set-ignore-ok
                             set-inhibit-warnings
                             set-irrelevant-formals-ok
                             set-let*-abstractionp
                             set-match-free-default
                             set-measure-function
                             set-non-linearp
                             set-nu-rewriter-mode
                             set-rewrite-stack-limit
                             set-state-ok
                             set-verify-guards-eagerness
                             set-well-founded-relation
                             defttag)))
           (er soft ctx local-str
               form
               " because it implicitly sets the acl2-defaults-table in a ~
                local context.  A local context is not useful when setting ~
                this table, since the acl2-defaults-table is restored upon ~
                completion of encapsulate, include-book, and certify-book ~
                forms; that is, no changes to the acl2-defaults-table are ~
                exported"
               (chk-embedded-event-form-orig-form-msg orig-form state)
               ""))
          ((and in-local-flg (eq (car form) 'defaxiom))
           (er soft ctx local-str
               form
               " because it adds an axiom whose traces will disappear"
               (chk-embedded-event-form-orig-form-msg orig-form state)
               ""))
          ((and in-encapsulatep (eq (car form) 'defaxiom))
           (er soft ctx encap-str
               form
               " because we do not permit defaxiom events in the scope of an ~
                encapsulate"
               (chk-embedded-event-form-orig-form-msg orig-form state)
               ""))
          ((and in-encapsulatep

; Note that in-local-flg could be 'dynamic from (encapsulate (local
; (include-book ...)) where we are inside the ... but not in an encapsulate in
; the present book -- so we do not insist that in-local-flg be equal to t.
; That lets us get away with (encapsulate (include-book ...)) inside a locally
; included book foo.lisp, but we would catch that mistake here when certifying
; foo.lisp.

                (not in-local-flg)
                (eq (car form) 'include-book))
           (er soft ctx encap-str
               form
               " because we do not permit non-local include-book forms in the ~
                scope of an encapsulate.  We fear that such forms will ~
                generate unduly large constraints that will impede the ~
                successful use of :functional-instance lemma instances.  ~
                Consider moving your include-book form outside the ~
                encapsulates, or else making it local"
               (chk-embedded-event-form-orig-form-msg orig-form state)
               ""))
          ((member-eq (car form) names)

; Names is often *primitive-event-macros* or an extension, and hence
; contains encapsulate and include-book.  This is quite reasonable,
; since they do their own checking.  And because they restore the
; acl2-defaults-table when they complete, we don't have to worry that
; they are sneaking in a ``local defun-mode.''

           (value form))
          ((and (eq (car form) 'skip-proofs)
                (consp (cdr form))
                (null (cddr form)))
           (pprogn
            (warning$ ctx "Skip-proofs"
                      "ACL2 has encountered a SKIP-PROOFS form, ~x0, in the ~
                       context of a book or an encapsulate event.  Therefore, ~
                       no logical claims may be soundly made in this context.  ~
                       See :DOC SKIP-PROOFS."
                      form)
            (er-let* ((new-form (chk-embedded-event-form
                                 (cadr form) orig-form wrld ctx state names
                                 portcullisp in-local-flg in-encapsulatep
                                 make-event-chk)))
                     (value (and new-form (list (car form) new-form))))))
          ((and (eq (car form) 'with-output)
                (consp (cdr form)))

; We let the with-output macro check the details of the form structure.

           (er-let* ((new-form (chk-embedded-event-form
                                (car (last form)) orig-form wrld ctx state
                                names portcullisp in-local-flg
                                in-encapsulatep make-event-chk)))
                    (value (and new-form
                                (append (butlast form 1)
                                        (list new-form))))))
          ((eq (car form) 'value-triple) (value form))
          ((eq (car form) 'make-event)
           (cond ((and make-event-chk
                       (not (and (true-listp form)
                                 (consp (cadr (member-eq :check-expansion
                                                         form))))))
                  (er soft ctx
                      "The :check-expansion argument of make-event should be ~
                       a consp in the present context.  This error can occur ~
                       when including an uncertified book.  If you see the ~
                       error during other normal use of ACL2, then you may ~
                       have uncovered an ACL2 bug; please contact the ACL2 ~
                       implementors.  Current form:~|~%~X01"
                      form
                      nil))
                 (t (value form))))
          ((eq (car form) 'record-expansion) ; a macro, but we handle specially
           (cond ((not (and (cdr form)
                            (cddr form)
                            (null (cdddr form))))
                  (er soft ctx
                      "The macro ~x0 takes two arguments, so ~x1 is illegal."
                      'record-expansion
                      form))
                 (t (er-progn
                     (chk-embedded-event-form (cadr form)
                                              nil
                                              wrld ctx state names
                                              portcullisp in-local-flg
                                              in-encapsulatep nil)
                     (chk-embedded-event-form (caddr form)
                                              (or orig-form form)
                                              wrld ctx state names
                                              portcullisp in-local-flg
                                              in-encapsulatep t)))))
          ((getprop (car form) 'macro-body nil 'current-acl2-world wrld)
           (cond
            ((member-eq (car form) (global-val 'untouchable-fns wrld))
             (er soft ctx er-str
                 form
                 ""
                 (chk-embedded-event-form-orig-form-msg orig-form state)
                 (msg "~|The macro ~x0 may not be used to generate an event, ~
                       because it has been placed on untouchable-fns.  See ~
                       :DOC push-untouchable."
                      (car form))))
            ((member-eq (car form)
                        '(mv mv-let translate-and-test with-local-stobj))
             (er soft ctx er-str
                 form
                 ""
                 (chk-embedded-event-form-orig-form-msg orig-form state)
                 (msg "~|Calls of the macro ~x0 do not generate an event, ~
                       because this macro has special meaning that is not ~
                       handled by ACL2's event-generation mechanism.  Please ~
                       contact the implementors if this seems to be a ~
                       hardship."
                      (car form))))
            (t
             (er-let*
              ((expansion (macroexpand1 form ctx state)))
              (chk-embedded-event-form expansion
                                       (or orig-form form)
                                       wrld ctx state names
                                       portcullisp in-local-flg
                                       in-encapsulatep make-event-chk)))))
          (t (er soft ctx er-str
                 form
                 ""
                 (chk-embedded-event-form-orig-form-msg orig-form state)
                 "")))))

; We have had a great deal of trouble correctly detecting embedded defaxioms!
; Tests for this have been incorporated into
; books/make-event/embedded-defaxioms.lisp.

(defun destructure-expansion (form)
  (declare (xargs :guard (true-listp form)))
  (cond ((member-eq (car form) '(local skip-proofs with-output))
         (mv-let (wrappers base-form)
                 (destructure-expansion (car (last form)))
                 (mv (cons (butlast form 1) wrappers)
                     base-form)))
        (t (mv nil form))))

(defun rebuild-expansion (wrappers form)
  (cond ((endp wrappers) form)
        (t (append (car wrappers)
                   (list (rebuild-expansion (cdr wrappers) form))))))

(defun set-raw-mode-on (state)
  (pprogn
   (cond ((raw-mode-p state)
          (fms "No change: raw mode is already on.~|"
               nil (standard-co state) state nil))
         (t
          (pprogn (fms "Entering raw-mode.~|" nil (standard-co state) state nil)
                  (f-put-global 'acl2-raw-mode-p t state))))
   (value :invisible)))

(defun set-raw-mode-off (state)
  (pprogn
   (cond ((raw-mode-p state)
          (pprogn (fms "Leaving raw-mode.~|" nil (standard-co state) state nil)
                  (f-put-global 'acl2-raw-mode-p nil state)))
         (t
          (fms "No change: raw mode is already off.~|"
               nil (standard-co state) state nil)))
   (value :invisible)))

(defmacro set-raw-mode-on! ()

  ":Doc-Section Other

  enter ``raw mode,'' a raw Lisp environment~/

  This is the same as ~c[(]~ilc[set-raw-mode]~c[ t)] except that it first
  introduces a so-called ``trust tag'' (``ttag'') so that ~c[set-raw-mode] will
  be legal.  ~l[defttag] for a discussion of ttags and how they affect
  ~ilc[certify-book] and ~ilc[include-book].~/~/"

  '(er-progn (ld '((defttag :raw-mode-hack)
                   (set-raw-mode-on state))
                 :ld-prompt nil :ld-verbose nil :ld-post-eval-print nil)
             (value :invisible)))

(defmacro set-raw-mode (flg)
  (declare (xargs :guard (member-equal flg '(t 't nil 'nil))))

  ":Doc-Section Other

  enter or exit ``raw mode,'' a raw Lisp environment~/

  ACL2 users often find its careful syntax checking to be helpful during code
  development.  Sometimes it is even useful to do code development in
  ~c[:]~ilc[logic] mode, where ACL2 can be used to check termination of
  (mutually) recursive functions, verify guards, or even prove properties of
  the functions.

  However, loading code using ~ilc[include-book] is much slower than using
  Common Lisp ~c[load] in raw Lisp, and in this sense ACL2 can get in the way
  of efficient execution.  Unfortunately, it is error-prone to use ACL2 sources
  (or their compilations) in raw Lisp, primarily because a number of ACL2
  primitives will not let you do so.  Perhaps you have seen this error message
  when trying to do so:
  ~bv[]
  HARD ACL2 ERROR in ACL2-UNWIND-PROTECT:  Apparently you have tried
  to execute a form in raw Lisp that is only intended to be executed
  inside the ACL2 loop.
  ~ev[]
  Even without this problem it is important to enter the ACL2 loop (~pl[lp]),
  for example in order to set the ~ilc[cbd] and (to get more technical) the
  readtable.

  ACL2 provides a ``raw mode'' for execution of raw Lisp forms.  In this mode,
  ~ilc[include-book] reduces essentially to a Common Lisp ~c[load].  More
  generally, the ACL2 logical ~ilc[world] is not routinely extended in raw mode
  (some sneaky tricks are probably required to make that happen).  To turn raw
  mode off or on:
  ~bv[]
  :set-raw-mode t   ; turn raw mode on
  :set-raw-mode nil ; turn raw mode off
  ~ev[]~/

  The way you can tell that you are in raw mode is by looking at the prompt
  (~pl[default-print-prompt]), which uses a capital ``~c[P]'' (suggesting
  something like program mode, but more so).
  ~bv[]
  ACL2 P>
  ~ev[]

  Typical benefits of raw mode are fast loading of source and compiled files
  and the capability to hack arbitrary Common Lisp code in an environment with
  the ACL2 sources loaded (and hence with ACL2 primitives available).  In
  addition, ACL2 hard errors will put you into the Lisp debugger, rather than
  returning you to the ACL2 loop, and this may be helpful for debugging;
  ~pl[hard-error] and ~pl[illegal], but also ~pl[break-on-error].  However, it
  probably is generally best to avoid raw mode unless these advantages seem
  important.  We expect the main benefit of raw mode to be in deployment of
  applications, where load time is much faster than the time required for a
  full-blown ~ilc[include-book], although in certain cases the fast loading of
  books and treatment of hard errors discussed above may be useful during
  development.

  Raw mode is also useful for those who want to build extensions of ACL2.  For
  example, the following form can be put into a certifiable book to load an
  arbitrary Common Lisp source or compiled file.
  ~bv[]
  (progn! (defttag my-application)
          (set-raw-mode t)
          (load \"some-file\"))
  ~ev[]
  Also see ~c[with-raw-mode] defined in ~c[books/misc/hacker.lisp],
  ~pl[defttag], and ~pl[progn!].

  Below are several disadvantages to raw mode.  These should discourage users
  from using it for general code development, as ~c[:]~ilc[program] mode is
  generally preferable.
  ~bf[]
  -- Forms are in essence executed in raw Lisp.  Hence:
     -- Syntax checking is turned off; and
     -- Guard checking is completely disabled.
  -- Table events, including ~ilc[logic], are ignored, as are many
     other ~ilc[events], including ~ilc[defthm] and ~ilc[comp].
  -- Soundness claims are weakened for any ACL2 session in which raw
     mode was ever entered; ~pl[defttag].
  -- The normal undoing mechanism (~pl[ubt]) is not supported.
  ~ef[]

  We conclude with some details.

  ~em[Printing results].  The rules for printing results are unchanged for raw
  mode, with one exception.  If the value to be printed would contain any Lisp
  object that is not a legal ACL2 object, then the ~c[print] routine is used
  from the host Lisp, rather than the usual ACL2 printing routine.  The
  following example illustrates the printing used when an illegal ACL2 object
  needs to be printed.  Notice how that ``command conventions'' are observed
  (~pl[ld-post-eval-print]); the ``~c[[Note]'' occurs one space over in the
  second example, and no result is printed in the third example.
  ~bv[]
  ACL2 P>(find-package \"ACL2\")
  [Note:  Printing non-ACL2 result.]
  #<The ACL2 package> 
  ACL2 P>(mv nil (find-package \"ACL2\") state)
   [Note:  Printing non-ACL2 result.]
  #<The ACL2 package> 
  ACL2 P>(mv t (find-package \"ACL2\") state)
  ACL2 P>(mv 3 (find-package \"ACL2\"))
  [Note:  Printing non-ACL2 result.]
  (3 #<The ACL2 package>) 
  ACL2 P>
  ~ev[]
  If you have trouble with large structures being printed out, you might want
  to execute appropriate Common Lisp forms in raw mode, for example,
  ~c[(setq *print-length* 5)] and ~c[(setq *print-level* 5)].

  ~em[Packages].  Raw mode disallows the use of ~ilc[defpkg].  If you want to
  create a new package, first exit raw mode with ~c[:set-raw-mode nil];
  you can subsequently re-enter raw mode with ~c[:set-raw-mode t] if you
  wish.~/"

  (if (or (null flg)
          (equal flg '(quote nil)))
      '(set-raw-mode-off state)
    '(set-raw-mode-on state)))

#-acl2-loop-only
(defun-one-output stobj-out (val)

; Warning:  This function assumes that we are not in the context of a local
; stobj.  As of this writing, it is only used in raw mode, so this does not
; concern us too much.  With raw mode, there are no guarantees.

  (if (eq val *the-live-state*)
      'state
    (car (rassoc val *user-stobj-alist* :test 'eq))))

#-(or acl2-loop-only acl2-mv-as-values)
(defun mv-ref! (i)

; This silly function is just mv-ref, but without the restriction that the
; argument be an explicit number.

  (case i
    (1 (mv-ref 1))
    (2 (mv-ref 2))
    (3 (mv-ref 3))
    (4 (mv-ref 4))
    (5 (mv-ref 5))
    (6 (mv-ref 6))
    (7 (mv-ref 7))
    (8 (mv-ref 8))
    (9 (mv-ref 9))
    (10 (mv-ref 10))
    (11 (mv-ref 11))
    (12 (mv-ref 12))
    (13 (mv-ref 13))
    (14 (mv-ref 14))
    (15 (mv-ref 15))
    (16 (mv-ref 16))
    (17 (mv-ref 17))
    (18 (mv-ref 18))
    (19 (mv-ref 19))
    (20 (mv-ref 20))
    (21 (mv-ref 21))
    (22 (mv-ref 22))
    (23 (mv-ref 23))
    (24 (mv-ref 24))
    (25 (mv-ref 25))
    (26 (mv-ref 26))
    (27 (mv-ref 27))
    (28 (mv-ref 28))
    (29 (mv-ref 29))
    (30 (mv-ref 30))
    (31 (mv-ref 31))
    (otherwise (error "Illegal value for mv-ref!"))))

(defmacro add-raw-arity (name val)
  (declare (xargs :guard (and (symbolp name)
                              (or (and (integerp val) (<= 0 val))
                                  (eq val :last)))))

  ":Doc-Section Set-raw-mode

  add arity information for raw mode~/

  Technical note: This macro is a no-op, and is not necessary, when ACL2 is
  built with #-acl2-mv-as-values.

  Users of raw mode (~pl[set-raw-mode]) can use arbitrary raw Lisp functions
  that are not known inside the usual ACL2 loop.  In such cases, ACL2 may not
  know how to display a multiple value returned by ACL2's ~ilc[mv] macro.  The
  following example should make this clear.
  ~bv[]
  ACL2 P>(defun foo (x y) (mv y x))
  FOO
  ACL2 P>(foo 3 4)

  Note: Unable to compute number of values returned by this evaluation
  because function FOO is not known in the ACL2 logical world.  Presumably
  it was defined in raw Lisp or in raw mode.  Returning the first (perhaps
  only) value for calls of FOO.
  4
  ACL2 P>(add-raw-arity foo 2)
   RAW-ARITY-ALIST
  ACL2 P>(foo 3 4)
  (4 3)
  ACL2 P>
  ~ev[]
  The first argument of ~c[add-raw-arity] should be a symbol, representing the
  name of a function, macro, or special form, and the second argument should
  either be a non-negative integer (denoting the number of values returned by
  ACL2) or else the symbol ~c[:LAST], meaning that the number of values
  returned by the call is the number of values returned by the last
  argument.~/

  The current arity assignments can be seen by evaluating
  ~c[(@ raw-arity-alist)].  ~l[remove-raw-arity] for how to undo a call of
  ~c[add-raw-arity].~/"

  #+acl2-mv-as-values (declare (ignore name val))
  #+acl2-mv-as-values '(value nil)
  #-acl2-mv-as-values
  `(pprogn (f-put-global 'raw-arity-alist
                         (put-assoc-eq ',name
                                       ,val
                                       (f-get-global 'raw-arity-alist state))
                         state)
           (value 'raw-arity-alist)))

(defmacro remove-raw-arity (name)
  (declare (xargs :guard (symbolp name)))

  ":Doc-Section Set-raw-mode

  remove arity information for raw mode~/

  Technical note: This macro is a no-op, and is not necessary, when ACL2 is
  built with #-acl2-mv-as-values.

  The form ~c[(remove-raw-arity fn)] undoes the effect of an earlier
  ~c[(remove-raw-arity fn val)].  ~l[add-raw-arity].~/~/"

  #+acl2-mv-as-values (declare (ignore name))
  #+acl2-mv-as-values '(value nil)
  #-acl2-mv-as-values
  `(pprogn (f-put-global 'raw-arity-alist
                         (delete-assoc-eq ',name
                                          (f-get-global 'raw-arity-alist
                                                        state))
                         state)
           (value 'raw-arity-alist)))

#-(or acl2-loop-only acl2-mv-as-values)
(defun raw-arity (form wrld state)
  (cond
   ((atom form) 1)
   ((eq (car form) 'mv)
    (length (cdr form)))
   ((eq (car form) 'if)
    (let ((arity1 (raw-arity (caddr form) wrld state)))
      (if (cdddr form)
          (let ((arity2 (raw-arity (cadddr form) wrld state)))
            (if (eql arity1 arity2)
                arity1
              (let ((min-arity (min arity1 arity2)))
                (prog2$
                 (warning$ 'top-level "Raw"
                           "Unable to compute arity of the following ~
                            IF-expression in raw mode because the true branch ~
                            has arity ~x0 but the false branch has arity ~x1, ~
                            so we assume an arity of ~x2 ~
                            (see :DOC add-raw-arity):~%  ~x3."
                           arity1 arity2 min-arity form)
                 min-arity))))
        arity1)))
   (t (let ((arity (cdr (assoc-eq (car form)
                                  (f-get-global 'raw-arity-alist state)))))
        (cond
         ((eq arity :last)
          (raw-arity (car (last form)) wrld state))
         ((and (integerp arity)
               (<= 0 arity))
          arity)
         (arity
          (error "Ill-formed value of *raw-arity-alist*."))
         (t
          (let ((stobjs-out
                 (getprop (car form) 'stobjs-out t 'current-acl2-world wrld)))
            (cond
             ((eq stobjs-out t)
              (multiple-value-bind
               (new-form flg)
               (macroexpand-1 form)
               (cond ((null flg)

; Remember that our notion of multiple value here is ACL2's notion, not Lisp's
; notion.  So the arity is 1 for calls of Common Lisp functions.

                      (when (not (member-eq
                                  (car form)
                                  *common-lisp-symbols-from-main-lisp-package*))
                        (fms "Note: Unable to compute number of values ~
                              returned by this evaluation because function ~x0 ~
                              is not known in the ACL2 logical world.  ~
                              Presumably it was defined in raw Lisp or in raw ~
                              mode.  Returning the first (perhaps only) value ~
                              for calls of ~x0.  See :DOC add-raw-arity.~|"
                             (list (cons #\0 (car form)))
                             *standard-co* state nil))
                      1)
                     (t (raw-arity new-form wrld state)))))
             (t (length stobjs-out))))))))))

(defun alist-to-bindings (alist)
  (cond
   ((endp alist) nil)
   (t (cons (list (caar alist) (kwote (cdar alist)))
            (alist-to-bindings (cdr alist))))))

#-acl2-loop-only
(defun-one-output acl2-raw-eval-form-to-eval (form)
  `(let ((state *the-live-state*)
         ,@(alist-to-bindings *user-stobj-alist*))

; OpenMCL prints "Unused lexical variable" warnings unless we take some
; measures, which we do now.  We notice that we need to include #+cmu for the
; second form, so we might as well include it for the first, too.

     #+(or openmcl cmu sbcl)
     ,@(mapcar #'(lambda (x) `(declare (ignorable ,(car x))))
               *user-stobj-alist*)
     #+(or openmcl cmu sbcl)
     (declare (ignorable state))
     ,(cond ((and (consp form)
                  (eq (car form) 'in-package)
                  (or (and (consp (cdr form))
                           (null (cddr form)))
                      (er hard 'top-level
                          "IN-PACKAGE takes one argument.  The form ~p0 is ~
                           thus illegal."
                          form)))

; The package must be one that ACL2 knows about, or there are likely to be
; problems involving the prompt and the ACL2 reader.  Also, we want the
; in-package form to reflect in the prompt.

             (list 'in-package-fn (list 'quote (cadr form)) 'state))
            (t form))))

#-(or acl2-loop-only acl2-mv-as-values)
(defun acl2-raw-eval (form state)
  (or (eq state *the-live-state*)
      (error "Unexpected state in acl2-raw-eval!"))
  (if (or (eq form :q) (equal form '(EXIT-LD STATE)))
      (mv nil '((NIL NIL STATE) NIL :Q REPLACED-STATE) state)
    (let ((val (eval (acl2-raw-eval-form-to-eval form)))
          (index-bound (raw-arity form (w state) state)))
      (if (<= index-bound 1)
          (mv nil (cons (list (stobj-out val)) val) state)
        (let ((ans nil)
              (stobjs-out nil))
          (do ((i (1- index-bound) (1- i)))
              ((eql i 0))
              (let ((x (mv-ref! i)))
                (push x ans)
                (push (stobj-out x)
                      stobjs-out)))
          (mv nil
              (cons (cons (stobj-out val) stobjs-out)
                    (cons val ans))
              state))))))

#+(and (not acl2-loop-only) acl2-mv-as-values)
(defun acl2-raw-eval (form state)
  (or (eq state *the-live-state*)
      (error "Unexpected state in acl2-raw-eval!"))
  (if (or (eq form :q) (equal form '(EXIT-LD STATE)))
      (mv nil '((NIL NIL STATE) NIL :Q REPLACED-STATE) state)
    (let* ((vals (multiple-value-list
                  (eval (acl2-raw-eval-form-to-eval form))))
           (arity (length vals)))
      (if (<= arity 1)
          (let ((val (car vals)))
            (mv nil (cons (list (stobj-out val)) val) state))
        (mv nil
            (loop for val in vals
                  collect (stobj-out val) into stobjs-out
                  finally (return (cons stobjs-out vals)))
            state)))))

#+acl2-loop-only
(defun acl2-raw-eval (form state)
  (trans-eval form 'top-level state))

(defun get-and-chk-last-make-event-expansion (form wrld ctx state names)
  (let ((expansion (f-get-global 'last-make-event-expansion state)))
    (cond
     (expansion
      (mv-let
       (erp val state)
       (state-global-let*
        ((inhibit-output-lst *valid-output-names*))
        (chk-embedded-event-form form
                                 nil ; orig-form
                                 wrld ctx state names
                                 nil ; portcullisp
                                 nil ; in-local-flg
                                 nil ; in-encapsulatep
                                 nil ; make-event-chk
                                 ))
       (declare (ignore val))
       (cond (erp (er soft ctx
                      "Make-event is only legal in event contexts, where it ~
                       can be tracked properly; see :DOC embedded-event-form.  ~
                       The form ~p0 has thus generated an illegal call of ~
                       make-event.  This form's evaluation will have no ~
                       effect on the ACL2 logical world."
                      form))
             (t (value expansion)))))
     (t (value nil)))))

(defun eval-event-lst (index expansion-alist ev-lst quietp in-encapsulatep
                             in-local-flg last-val other-control ctx channel
                             state)

; This function takes a true list of forms, ev-lst, and successively evals each
; one, cascading state through successive elements.  However, it insists that
; each form is an embedded-event-form.  We return a tuple (mv erp value
; expansion-alist state), where erp is 'non-event if some member of ev-lst is
; not an embedded event form and otherwise is as explained below.  If erp is
; nil, then value is the final value (or nil if ev-lst is empty), and
; expansion-alist associates the (+ index n)th member E of ev-lst with its
; expansion if there was any make-event expansion subsidiary to E, ordered by
; index from smallest to largest (accumulated in reverse order).  If erp is not
; nil, then let n be the (zero-based) index of the event in ev-lst that
; translated or evaluated to some (mv erp0 ...) with non-nil erp0.  Then we
; return (mv t (+ index n) state) if the error was during translation, else (mv
; (list erp0) (+ index n) state).  Except, in the special case that there is no
; error but we find that make-event was called under some non-embedded-event
; form, we return (mv 'make-event-problem (+ index n) state).

; Other-control is either :non-event-ok, used for progn!, or else t or nil for
; the make-event-chk in chk-embedded-event-form.

; Channel is generally (proofs-co state), but doesn't have to be.

; A non-nil value of quietp suppresses printing of the event and the result.

  (cond
   ((null ev-lst) (mv nil last-val (reverse expansion-alist) state))
   (t (pprogn
       (cond
        (quietp state)
        (t
         (io? event nil state
              (channel ev-lst)
              (fms "~%~@0~sr ~@1~*2~#3~[~q4~/~]~|"
                   (list
                    (cons #\0 (f-get-global 'current-package state))
                    (cons #\1 (defun-mode-prompt-string state))
                    (cons #\2 (list "" ">" ">" ">"
                                    (make-list-ac
                                     (1+ (f-get-global 'ld-level state))
                                     nil nil)))
                    (cons #\3 (if (eq (ld-pre-eval-print state) :never)
                                  1
                                0))
                    (cons #\4 (car ev-lst))
                    (cons #\r
                          #+:non-standard-analysis "(r)"
                          #-:non-standard-analysis ""))
                   channel state nil))))
       (mv-let
        (erp form state)
        (cond ((eq other-control :non-event-ok)
               (mv nil (car ev-lst) state))
              (t (chk-embedded-event-form (car ev-lst)
                                          nil
                                          (w state)
                                          ctx state
                                          *primitive-event-macros*
                                          nil
                                          in-local-flg
                                          in-encapsulatep
                                          other-control)))
        (cond
         (erp (mv 'non-event index nil state))
         ((null form)
          (eval-event-lst (1+ index) expansion-alist (cdr ev-lst) quietp
                          in-encapsulatep in-local-flg nil other-control ctx
                          channel state))
         (t
          (mv-let
           (erp trans-ans state)
           (pprogn (f-put-global 'last-make-event-expansion nil state)
                   (if (raw-mode-p state)
                       (acl2-raw-eval form state)
                     (trans-eval form ctx state)))

; If erp is nil, trans-ans is 
; ((nil nil state) . (erp' val' replaced-state))
; because ev-lst contains nothing but embedded event forms.

           (let* ((tuple
                   (cond ((eq other-control :non-event-ok)
                          (let* ((stobjs-out (car trans-ans))
                                 (result (replace-stobjs stobjs-out (cdr trans-ans))))
                            (if (null (cdr stobjs-out)) ; single value
                                (list nil result)
                              result)))
                         (t (cdr trans-ans))))
                  (erp-prime (car tuple))
                  (val-prime (cadr tuple)))
             (cond
              (erp
               (mv t index nil state))
              (erp-prime
               (mv (list erp-prime) index nil state))
              (t
               (pprogn
                (cond (quietp state)
                      (t (pprogn (ppr val-prime 0 channel state nil)
                                 (newline channel state))))
                (mv-let
                 (erp expansion0 state)

; We need to cause an error if we have an expansion but are not properly
; tracking expansions.  For purposes of seeing if such tracking is being done,
; it should suffice to do the check in the present world rather than the world
; present before evaluating the form.

                 (get-and-chk-last-make-event-expansion
                  (car ev-lst) (w state) ctx state *primitive-event-macros*)
                 (cond
                  (erp (mv 'make-event-problem index nil state))
                  (t
                   (eval-event-lst
                    (1+ index)
                    (cond
                     (expansion0
                      (acons index
                             (list 'record-expansion
                                   (car ev-lst)
                                   (mv-let (wrappers base-form)
                                           (destructure-expansion form)
                                           (declare (ignore base-form))
                                           (rebuild-expansion wrappers
                                                              expansion0)))
                             expansion-alist))
                     (t expansion-alist))
                    (cdr ev-lst) quietp
                    in-encapsulatep in-local-flg val-prime
                    other-control ctx channel
                    state))))))))))))))))

; After we have evaluated the event list and obtained wrld2, we
; will scrutinize the signatures and exports to make sure they are
; appropriate.  We will try to give the user as much help as we can in
; detecting bad signatures and exports, since it may take him a while
; to recreate wrld2 after fixing an error.  Indeed, he has already
; paid a high price to get to wrld2 and it is a real pity that we'll
; blow him out of the water now.  The guilt!  It's enough to make us
; think about implementing some sort of interactive version of
; encapsulate, when we don't have anything else to do.  (We have since
; implemented redo-flat, which helps with the guilt.)

(defun equal-insig (insig1 insig2)

; Suppose insig1 and insig2 are both internal form signatures, (fn
; formals stobjs-in stobjs-out).  We return t if they are ``equal.''
; But by equal we mean only that the fn, stobjs-in and stobjs-out are
; the same.  If the user has declared that fn has formals (x y z) and
; then witnessed fn with a function with formals (u v w), we don't
; care -- as long as the stobjs among the two lists are the same in
; corresponding positions.  But that information is captured in the
; stobjs-in.

  (and (equal (car insig1) (car insig2))
       (equal (caddr insig1) (caddr insig2))
       (equal (cadddr insig1) (cadddr insig2))))

;; RAG - I changed this so that non-classical witness functions are
;; not allowed.  The functions introduced by encapsulate are
;; implicitly taken to be classical, so a non-classical witness
;; function presents a (non-obvious) signature violation.

(defun bad-signature-alist (insigs udf-fns wrld)
  (cond ((null insigs) nil)
        ((member-eq (caar insigs) udf-fns)
         (bad-signature-alist (cdr insigs) udf-fns wrld))
        (t (let* ((declared-insig (car insigs))
                  (fn (car declared-insig))
                  (actual-insig (list fn
                                      (formals fn wrld)
                                      (stobjs-in fn wrld)
                                      (stobjs-out fn wrld))))
             (cond
              ((and (equal-insig declared-insig actual-insig)
                    #+:non-standard-analysis
                    (classical-fn-list-p (list fn) wrld))
               (bad-signature-alist (cdr insigs) udf-fns wrld))
              (t (cons (list fn declared-insig actual-insig)
                       (bad-signature-alist (cdr insigs) udf-fns wrld))))))))

(defmacro if-ns (test tbr fbr ctx)

; This is just (list 'if test tbr fbr), except that we expect test always to be
; false in the standard case.

  #+:non-standard-analysis
  (declare (ignore ctx))
  #-:non-standard-analysis
  (declare (ignore tbr))
  (list 'if
        test
        #+:non-standard-analysis
        tbr
        #-:non-standard-analysis
        `(er hard ,ctx
             "Unexpected intrusion of non-standard analysis into standard ~
              ACL2!  Please contact the implementors.")
        fbr))

(defun tilde-*-bad-insigs-phrase1 (alist)
  (cond ((null alist) nil)
        (t (let* ((fn (caar alist))
                  (dcl-insig (cadar alist))
                  (act-insig (caddar alist)))
             (cons
              (if-ns (equal-insig dcl-insig act-insig)
                     (msg
                      "The signature you declared for ~x0 is ~x1, but ~
                       your local witness for the function is not classical."
                      fn
                      (unparse-signature dcl-insig))
                     (msg
                      "The signature you declared for ~x0 is ~x1, but ~
                       the signature of your local witness for it is ~
                       ~x2."
                      fn
                      (unparse-signature dcl-insig)
                      (unparse-signature act-insig))
                     'tilde-*-bad-insigs-phrase1)
              (tilde-*-bad-insigs-phrase1 (cdr alist)))))))

(defun tilde-*-bad-insigs-phrase (alist)

; Each element of alist is of the form (fn insig1 insig2), where
; insig1 is the internal form of the signature presented by the user
; in his encapsulate and insig2 is the internal form signature of the
; witness.  For each element we print a sentence of the form "The
; signature for your local definition of fn is insig2, but the
; signature you declared for fn was insig1."

  (list "" "~@*" "~@*" "~@*"
        (tilde-*-bad-insigs-phrase1 alist)))

(defun union-eq-cars (alist)
  (cond ((null alist) nil)
        (t (union-eq (caar alist) (union-eq-cars (cdr alist))))))

(defun chk-acceptable-encapsulate2 (insigs wrld ctx state)

; Wrld is a world alist created by the execution of an event list.
; Insigs is a list of internal form function signatures.  We verify
; that they are defined as functions in wrld and have the signatures
; listed.

; This is an odd little function because it may generate more than one
; error message.  The trouble is that this wrld took some time to
; create and yet will have to be thrown away as soon as we find one of
; these errors.  So, as a favor to the user, we find all the errors we
; can.

  (let ((udf-fns (collect-non-function-symbols insigs wrld)))
    (mv-let
     (erp1 val state)
     (cond
      (udf-fns
       (er soft ctx
           "You provided signatures for ~&0, but ~#0~[that function ~
           was~/those functions were~] not defined by the ~
           encapsulated event list.  See :DOC encapsulate."
           (merge-sort-symbol-< udf-fns)))
      (t (value nil)))
     (declare (ignore val))
     (mv-let
      (erp2 val state)
      (let ((bad-sig-alist (bad-signature-alist insigs udf-fns wrld)))
        (cond
         (bad-sig-alist
          (er soft ctx
              "The signature~#0~[~/s~] provided for the ~
               function~#0~[~/s~] ~&0 ~#0~[is~/are~] incorrect.  See ~
               :DOC encapsulate.  ~*1"
              (strip-cars bad-sig-alist)
              (tilde-*-bad-insigs-phrase bad-sig-alist)))
         (t (value nil))))
      (declare (ignore val))
      (mv (or erp1 erp2) nil state)))))

(defun conjoin-into-alist (fn thm alist)

; Alist is an alist that maps function symbols to terms.  Fn is a function
; symbol and thm is a term.  If fn is not bound in alist we add (fn . thm)
; to it.  Otherwise, we change the binding (fn . term) in alist to
; (fn . (if thm term *nil*)).

  (cond ((null alist)
         (list (cons fn thm)))
        ((eq fn (caar alist))
         (cons (cons fn (conjoin2 thm (cdar alist)))
               (cdr alist)))
        (t (cons (car alist) (conjoin-into-alist fn thm (cdr alist))))))

(defun classes-theorems (classes)

; Classes is the 'classes property of some symbol.  We return the list of all
; corollary theorems from these classes.

  (cond
   ((null classes) nil)
   (t (let ((term (cadr (assoc-keyword :corollary (cdr (car classes))))))
        (if term
            (cons term (classes-theorems (cdr classes)))
          (classes-theorems (cdr classes)))))))

(defun constraints-introduced1 (thms fns ans)
  (cond
   ((endp thms) ans)
   ((ffnnamesp fns (car thms))

; We use add-to-set-equal below because an inner encapsulate may have both an
; 'unnormalized-body and 'constraint-lst property, and if 'unnormalized-body
; has already been put into ans then we don't want to include that constraint
; when we see it here.

    (constraints-introduced1 (cdr thms) fns (add-to-set-equal (car thms) ans)))
   (t (constraints-introduced1 (cdr thms) fns ans))))

(defun new-trips (wrld3 proto-wrld3 seen acc)

; Important:  This function returns those triples in wrld3 that are after
; proto-wrld3, in the same order they have in wrld3. See the comment labeled
; "Important" in the definition of constrained-functions.

; As with the function actual-props, we are only interested in triples
; that aren't superseded by *acl2-property-unbound*.  We therefore do
; not copy to our answer any *acl2-property-unbound* triple or any
; chronologically earlier bindings of the relevant symbol and key!
; That is, the list of triples returned by this function contains no
; *acl2-property-unbound* values and makes it appear as though the
; property list was really erased when that value was stored.

; Note therefore that the list of triples returned by this function
; will not indicate when a property bound in proto-wrld3 becomes
; unbound in wrld3.  However, if a property was stored during the
; production of wrld3 and the subsequently in the production of wrld3
; that property was set to *acl2-property-unbound*, then the property
; is gone from the new-trips returned here.

; Warning: The value of this function is sometimes used as though it
; were the 'current-acl2-world!  It is a legal property list world.
; If it gets into a getprop on 'current-acl2-world the answer is
; correct but slow.  Among other things, we use new-trips to compute
; the ancestors of a definition defined within an encapsulate --
; knowing that functions used in those definitions but defined outside
; of the encapsulate (and hence, outside of new-trips) will be treated
; as primitive.  That way we do not explore all the way back to ground
; zero when we are really just looking for the subfunctions defined
; within the encapsulate.

; Note on this recursion: The recursion below is potentially
; disastrously slow.  Imagine that proto-wrld3 is a list of 10,000
; repetitions of the element e.  Imagine that wrld3 is the extension
; produced by adding 1000 more copies of e.  Then the equal below will
; fail the first 1000 times, but it will only fail after confirming
; that the first 10,000 e's in wrld3 are the same as the corresponding
; ones in proto-wrld3, i.e., the equal will do a root-and-branch walk
; through proto-wrld3 1000 times.  When finally the equal succeeds it
; potentially does another root-and-branch exploration of proto-wrld3.
; However, this worst-case scenario is not likely.  More likely, if
; wrld3 is an extension of proto-wrld3 then the first element of wrld3
; differs from that of proto-wrld3 -- because either wrld3 begins with
; a putprop of a new name or a new list of lemmas or some other
; property.  Therefore, most of the time the equal below will fail
; immediately when the two worlds are not equal.  When the two worlds
; are in fact equal, they will be eq, because wrld3 was actually
; constructed by adding triples to proto-wrld3.  So the equal will
; succeed on its initial eq test and avoid a root-and-branch
; exploration.  This analysis is crucial to the practicality of this
; recursive scheme.  Our worlds are so large we simply cannot afford
; root-and-branch explorations.

; In fact, we did see performance issues when seen was kept as a list
; of triples.  So, we have restructured it as an alist, whose values
; are alists, in which triple (key1 key2 . val) is found in the alist
; associated with key1.

  (cond ((equal wrld3 proto-wrld3)
         (reverse acc))
        ((let ((key-alist (assoc-eq (caar wrld3) seen)))
            (and key-alist ; optimization
                 (assoc-eq (cadar wrld3) (cdr key-alist))))
         (new-trips (cdr wrld3) proto-wrld3 seen acc))
        ((eq (cddr (car wrld3)) *acl2-property-unbound*)
         (new-trips (cdr wrld3) proto-wrld3
                    (put-assoc-eq (caar wrld3)
                                  (cons (cdar wrld3)
                                        (cdr (assoc-eq (caar wrld3) seen)))
                                  seen)
                    acc))
        (t
         (new-trips (cdr wrld3) proto-wrld3
                    (put-assoc-eq (caar wrld3)
                                  (cons (cdar wrld3)
                                        (cdr (assoc-eq (caar wrld3) seen)))
                                  seen)
                    (cons (car wrld3) acc)))))

(defun constraints-introduced (new-trips fns wrld defs-flg ans)

; Warning:  Keep this function in sync with definitional-constraints.

; New-trips is a list of triples from a property list world, none of them with
; cddr *acl2-property-unbound*.  We return the list of all formulas represented
; in new-trips that mention any function symbol in the list fns (each of which
; is in :logic mode), either restricted to or excluding definitional (defuns,
; defchoose) axioms according to defs-flg.  We may skip properties such as
; 'congruences and 'lemmas that can only be there if some other property has
; introduced a formula for which the given property's implicit formula is a
; consequence.  However, we must include 'type-prescriptions that were computed
; under the assumption that a given axiom was a total recursive definition.  We
; can omit 'induction-machine because we have checked for subversive
; inductions.  Actually, a good way to look at this is that the only events
; that can introduce axioms are defuns, defthm, encapsulate, defaxiom, and
; include-book, and we have ruled out the last two.  Encapsulate is covered by
; 'constraint-lst.

; We could probably optimize by avoiding definitions of symbols whose
; 'constraint-lst property has already been seen, since those definitional
; axioms are already included in the 'constraint-lst properties.  However, we
; prefer not to rely this way on the order of properties.

  (cond
   ((endp new-trips) ans)
   (t (constraints-introduced
       (cdr new-trips)
       fns
       wrld
       defs-flg
       (let ((trip (car new-trips)))
         (case (cadr trip)
           (constraint-lst
            (cond
             (defs-flg ans)
             ((symbolp (cddr trip))

; Then the constraint list for (car trip) is held in the 'constraint-lst
; property of (cddr trip).  We know that this kind of "pointing" is within the
; current encapsulate, so it is safe to ignore this property, secure in the
; knowledge that we see the real constraint list at some point.

              ans)
             (t (constraints-introduced1 (cddr trip) fns ans))))
           (theorem
            (cond
             (defs-flg ans)
             ((ffnnamesp fns (cddr trip))
              (add-to-set-equal (cddr trip) ans))
             (t ans)))
           (defchoose-axiom
             (cond
              ((not defs-flg) ans)
              ((member-eq (car trip) fns)
               (add-to-set-equal (cddr trip) ans))
              (t ans)))
           (classes
            (cond
             (defs-flg ans)
             (t (constraints-introduced1
                 (classes-theorems (cddr trip)) fns ans))))
           (unnormalized-body
            (cond
             ((not defs-flg) ans)
             ((member-eq (car trip) fns)
              (add-to-set-equal
               (mcons-term* 'equal
                            (cons-term (car trip) (formals (car trip) wrld))
                            (cddr trip))
               ans))
             (t ans)))
           (type-prescriptions
            (cond
             ((and defs-flg
                   (member-eq (car trip) fns))

; We are only interested in type prescriptions put by the system at defun time.
; (The others are theorems or corollaries of theorems, which we handle
; elsewhere.)  Those type prescriptions have the property that the only
; non-primitive function symbol in their corresponding formulas is the one
; being defined.  Hence, we can catch all of those type prescriptions if we
; simply look for type prescriptions hung on function symbols that belong to
; fns.

              (let* ((tp (find-runed-type-prescription
                          (list :type-prescription (car trip))
                          (cddr trip))))
                (cond
                 ((null tp)
                  ans)
                 (t (add-to-set-equal (access type-prescription tp :corollary)
                                      ans)))))
             (t ans)))
           (otherwise ans)))))))

(defun putprop-constraints (fn constrained-fns constraint-lst wrld3)

; Wrld3 is almost wrld3 of the encapsulation essay.  We have added all the
; exports, but we have not yet stored the 'constraint-lst properties of the
; functions in the signature of the encapsulate.  Fn is the first function
; mentioned in the signature, while constrained-fns includes the others as well
; as all functions that have any function in the signature as an ancestor.  We
; have determined that the common constraint for all these functions is
; constraint-lst, which has presumably been obtained from all the new theorems
; introduced by the encapsulate that mention any functions in (fn
; . constrained-fns).

; We actually store the symbol fn as the value of the 'constraint-lst property
; for every function in constrained-fns.  For fn, we store a 'constraint-lst
; property of constraint-lst.  It is crucial that we store the 'constraint-lst
; property for fn before we store any other 'constraint-lst properties; see the
; comment in constrained-functions-save-one.

; Note that we store a 'constrain-lst property for every function in (fn
; . constrained-fns).  The function constraint-info will find this property
; rather than looking for an 'unnormalized-body or 'defchoose-axiom.

  (putprop-x-lst1 constrained-fns 'constraint-lst fn
                  (putprop fn 'constraint-lst constraint-lst wrld3)))

(deflabel local-incompatibility
  :doc
  ":Doc-Section Miscellaneous

  when non-local ~il[events] won't replay in isolation~/

  Sometimes a ``~ilc[local] incompatibility'' is reported while attempting
  to embed some ~il[events], as in an ~ilc[encapsulate] or ~ilc[include-book].  This is
  generally due to the use of a locally defined name in a non-local
  event or the failure to make a witnessing definition ~ilc[local].~/

  ~ilc[local] incompatibilities may be detected while trying to execute the
  strictly non-local ~il[events] of an embedding.  For example, ~ilc[encapsulate]
  operates by first executing all the ~il[events] (~ilc[local] and non-local)
  with ~ilc[ld-skip-proofsp] ~c[nil], to confirm that they are all admissible.
  Then it attempts merely to assume the non-local ones to create the
  desired theory, by executing the ~il[events] with ~ilc[ld-skip-proofsp] set to
  ~c[']~ilc[include-book].  Similarly, ~ilc[include-book] assumes the non-local ones,
  with the understanding that a previously successful ~ilc[certify-book] has
  performed the admissiblity check.

  How can a sequence of ~il[events] admitted with ~ilc[ld-skip-proofsp] ~c[nil] fail
  when ~ilc[ld-skip-proofsp] is ~c[']~ilc[include-book]?  The key observation is that
  in the latter case only the non-local ~il[events] are processed.  The
  ~ilc[local] ones are skipped and so the non-local ones must not depend
  upon them.

  Two typical mistakes are suggested by the detection of a ~ilc[local]
  incompatibility: (1) a locally defined function or macro was used in
  a non-~ilc[local] event (and, in the case of ~ilc[encapsulate], was not included
  among the ~il[signature]s) and (2) the witnessing definition of a
  function that was included among the ~il[signature]s of an ~ilc[encapsulate]
  was not made ~ilc[local].

  An example of mistake (1) would be to include among your
  ~il[encapsulate]d ~il[events] both ~c[(local (defun fn ...))] and
  ~c[(defthm lemma (implies (fn ...) ...))].  Observe that ~c[fn] is
  defined locally but a formula involving ~c[fn] is defined
  non-locally.  In this case, either the ~ilc[defthm] should be made
  ~ilc[local] or the ~ilc[defun] should be made non-local.

  An example of mistake (2) would be to include ~c[(fn (x) t)] among your
  ~il[signature]s and then to write ~c[(defun fn (x) ...)] in your ~il[events],
  instead of ~c[(local (defun fn ...))].

  One subtle aspect of ~ilc[encapsulate] is that if you constrain any member
  of a mutually recursive clique you must define the entire clique
  locally and then you must constrain those members of it you want
  axiomatized non-locally.

  Errors due to ~ilc[local] incompatibility should never occur in the
  assumption of a fully certified book.  Certification ensures against
  it.  Therefore, if ~ilc[include-book] reports an incompatibility, we
  assert that earlier in the processing of the ~ilc[include-book] a warning
  was printed advising you that some book was uncertified.  If this is
  not the case ~-[] if ~ilc[include-book] reports an incompatibility and there
  has been no prior warning about lack of certification ~-[] please
  report it to us.

  When a ~ilc[local] incompatibility is detected, we roll-back to the ~il[world]
  in which we started the ~ilc[encapsulate] or ~ilc[include-book].  That is, we
  discard the intermediate ~il[world] created by trying to process the
  ~il[events] skipping proofs.  This is clean, but we realize it is very
  frustrating because the entire sequence of ~il[events] must be processed
  from scratch.  Assuming that the embedded ~il[events] were, once upon a
  time, processed as top-level ~il[command]s (after all, at some point you
  managed to create this sequence of ~il[command]s so that the ~ilc[local] and
  non-local ones together could survive a pass in which proofs were
  done), it stands to reason that we could define a predicate that
  would determine then, before you attempted to embed them, if ~ilc[local]
  incompatibilities exist.  We hope to do that, eventually.

  We conclude with a subtle example of ~ilc[local] incompatibility.  The problem
  is that in order for ~c[foo-type-prescription] to be admitted using the
  specified ~c[:typed-term] ~c[(foo x)], the conclusion ~c[(my-natp (foo x))]
  depends on ~c[my-natp] being a ~il[compound-recognizer].  This is fine on the
  first pass of the ~ilc[encapsulate], during which lemma ~c[my-natp-cr] is
  admitted.  But ~c[my-natp-cr] is skipped on the second pass because it is
  marked ~ilc[local], and this causes ~c[foo-type-prescription] to fail on the
  second pass.
  ~bv[]
  (defun my-natp (x)
    (declare (xargs :guard t))
    (and (integerp x)
         (<= 0 x)))

  (defun foo (x)
    (nfix x))

  (encapsulate
   ()
   (local (defthm my-natp-cr
            (equal (my-natp x)
                   (and (integerp x)
                        (<= 0 x)))
            :rule-classes :compound-recognizer))
   (defthm foo-type-prescription
     (my-natp (foo x))
     :hints ((\"Goal\" :in-theory (enable foo)))
     :rule-classes ((:type-prescription :typed-term (foo x)))))
  ~ev[]")

(defun maybe-install-acl2-defaults-table (acl2-defaults-table ctx state)
  (cond
   ((equal acl2-defaults-table
           (table-alist 'acl2-defaults-table (w state)))
    (value nil))
   (t (mv-let (erp val expansion-alist state)
              (eval-event-lst
               0 nil
               `((table acl2-defaults-table nil
                        ',acl2-defaults-table :clear))
               (ld-skip-proofsp state)
               t ; use strict value of in-encapsulatep, but shouldn't matter
               (f-get-global 'in-local-flg state)
               nil t ctx (proofs-co state) state)
              (assert$ (and (null erp) (null expansion-alist))
                       (value val))))))

(defun in-encapsulatep (embedded-event-lst non-trivp)

; This function determines if we are in the scope of an encapsulate.
; If non-trivp is t, we restrict the interpretation to mean ``in the
; scope of a non-trivial encapsulate'', i.e., in an encapsulate that
; introduces a constrained function symbol.

  (cond
   ((endp embedded-event-lst) nil)
   ((and (eq (car (car embedded-event-lst)) 'encapsulate)
         (if non-trivp
             (cadr (car embedded-event-lst))
           t))
    t)
   (t (in-encapsulatep (cdr embedded-event-lst) non-trivp))))

(defun update-for-redo-flat (n ev-lst state)

; Here we update the state globals 'redo-flat-succ and 'redo-flat-fail on
; behalf of a failure of progn or encapsulate.

  (assert$ (and (natp n)
                (< n (length ev-lst)))
           (pprogn
            (f-put-global 'redo-flat-succ
                          (append? (take n ev-lst)
                                   (f-get-global 'redo-flat-succ state))
                          state)
            (if (null (f-get-global 'redo-flat-fail state))
                (f-put-global 'redo-flat-fail
                              (nth n ev-lst)
                              state)
              state))))

(defmacro redo-flat (&key (succ-ld-skip-proofsp 't)
                          (label 'r)
                          (succ 't)
                          (fail 't)
                          (pbt 't)
                          (show 'nil))

  ":Doc-Section Events

  redo up through a failure in an ~ilc[encapsulate] or ~ilc[progn]~/

  When one submits an ~ilc[encapsulate] or ~ilc[progn] event and one of its
  sub-events fails, ACL2 restores its logical ~il[world] as though the
  ~c[encapsulate] or ~c[progn] had not been run.  But sometimes one would like
  to debug the failure by re-executing all sub-events that succeeded up to the
  point of failure, and then re-executing the failed sub-event.  Said
  differently, imagine that the top-level ~c[encapsulate] or ~c[progn] form, as
  well as all such sub-forms, were flattened into a list of events that were
  then submitted to ACL2 up to the point of failure.  This would put us in the
  state in which the original failed event had failed, so we could now submit
  that failed event and try modifying it, or first proving additional events,
  in order to get it admitted.

  ~c[Redo-flat] is provided for this purpose.  Consider the following (rather
  nonsensical) example, in which the ~ilc[defun] of ~c[f3] fails (the body is
  ~c[y] but the formal parameter list is ~c[(x)]).
  ~bv[]
  (encapsulate
   ()
   (defun f1 (x) x)
   (encapsulate ()
                (local (defthm hack (equal (car (cons x y)) x))))
   (encapsulate ()
                (local (defthm hack (equal (+ x y) (+ y x)))))
   (encapsulate ()
                (make-event '(defun f2 (x) x))
                (progn (defthm foo (equal x x) :rule-classes nil)
                       (defun f3 (x) y)))
   (defun f4 (x) x)
   (defun f5 (x) y))
  ~ev[]
  After this attempt fails, you can evaluate the following form.
  ~bv[]
  (redo-flat)
  ~ev[]
  This will first lay down a ~ilc[deflabel] event, ~c[(deflabel r)], so that
  you can eventually remove your debugging work with ~c[(:ubt! r)].  Then the
  successful sub-events that preceded the failure will be executed with proofs
  skipped (so that this execution is fast).  Then, the failed event will be
  executed.  Finally, a ~c[:]~ilc[pbt] command is executed so that you can see
  a summary of the events that executed successfully.

  You can eliminate some of the steps above by supplying keyword values, as
  follows.
  ~bv[]
  (redo-flat
   :succ  succ ; Skip the successful sub-events if val is nil.
   :fail  fail ; Skip the failed sub-event if val is nil.
   :label lab  ; Skip deflabel if lab or succ is nil, else use (deflabel lab).
   :pbt   val  ; Skip the final :pbt if val, lab, or succ is nil.
   )
  ~ev[]
  Also, you can avoid skipping proofs for the successful sub-events by
  supplying keyword ~c[:succ-ld-skip-proofsp] with a valid value for
  ~c[ld-skip-proofsp]; ~pl[ld-skip-proofsp].

  If you prefer only to see the successful and failed sub-events, without any
  events being re-executed, you may evaluate the following form instead.
  ~bv[]
  (redo-flat :show t)
  ~ev[]
  For the example above, this command produces the following output.
  ~bv[]

  List of events (from encapsulate or progn) preceding the failure:

  ((DEFUN F1 (X) X)
   (ENCAPSULATE NIL
                (LOCAL (DEFTHM HACK (EQUAL (CAR (CONS X Y)) X))))
   (ENCAPSULATE NIL
                (LOCAL (DEFTHM HACK (EQUAL (+ X Y) (+ Y X)))))
   (MAKE-EVENT '(DEFUN F2 (X) X))
   (DEFTHM FOO (EQUAL X X)
           :RULE-CLASSES NIL))

  Failed event:

  (DEFUN F3 (X) Y)
  ACL2 !>
  ~ev[]

  ~c[Redo-flat] uses a scheme that should not cause spurious name conflicts for
  ~ilc[local] events.  Above, it is mentioned that events are ``flattened'';
  now we clarify this notion.  Each sub-event that succeeds and is an
  ~ilc[encapsulate] or ~ilc[progn] is left intact.  Only such events that fail
  are replaced by their component events.  Thus, in the example above, there is
  no conflict between the two ~ilc[local] sub-events named ``~c[hack],''
  because these are contained in successful ~c[encapsulate] sub-events, which
  are therefore not flattened.  The ~ilc[progn] and two ~ilc[encapsulate]
  events surrounding the definition of ~c[f3] are, however, flattened, because
  that definition failed to be admitted.

  Unfortunately, an event must actually fail in order for ~c[redo-flat] to
  work.  So if the system is ``stuck'' on an event, then you may find it
  helpful to insert an illegal event just in front of it before submitting the
  ~ilc[encapsulate] or ~ilc[progn].~/~/"

  `(if (null (f-get-global 'redo-flat-fail state))
       (pprogn (fms "There is no failure saved from an encapsulate or progn.~|"
                    nil (standard-co state) state nil)
               (value :invisible))
     ,(if show
          `(pprogn (fms "List of events (from encapsulate or progn) preceding ~
                         the failure:~|~%~x0~|"
                        (list (cons #\0 (f-get-global 'redo-flat-succ state)))
                        (standard-co state) state nil)
                   (fms "Failed event:~|~%~x0~|"
                        (list (cons #\0 (f-get-global 'redo-flat-fail state)))
                        (standard-co state) state nil)
                   (value :invisible))
        `(let ((redo-flat-succ (f-get-global 'redo-flat-succ state))
               (redo-flat-fail (f-get-global 'redo-flat-fail state)))
           (state-global-let*
            ((redo-flat-succ redo-flat-succ)
             (redo-flat-fail redo-flat-fail))
            (ld (list ,@(and succ label `('(deflabel ,label)))
                      ,@(and succ (list (list 'list ''ld
                                              (list 'cons
                                                    ''list
                                                    (list 'kwote-lst 'redo-flat-succ))
                                              :ld-skip-proofsp succ-ld-skip-proofsp)))
                      ,@(and fail (list (list 'list ''ld
                                              (list 'list
                                                    ''list
                                                    (list 'list ''quote 'redo-flat-fail))
                                              :ld-error-action :continue
                                              :ld-pre-eval-print t)))
                      ,@(and pbt succ label
                             `('(pprogn (newline (proofs-co state)
                                                 state)
                                        (pbt ',label)))))))))))

(defun process-embedded-events
  (caller acl2-defaults-table skip-proofsp pkg ee-entry ev-lst index
  make-event-chk ctx state)

; Warning: This function uses set-w and hence may only be called within a
; revert-world-on-error.  See the statement of policy in set-w.

; This function is the heart of the second pass of encapsulate, include-book,
; and certify-book.  Caller is in fact one of the five symbols
; 'encapsulate-pass-1, 'encapsulate-pass-2, 'include-book, 'certify-book, or
; 'defstobj.  Note: There is no function encapsulate-pass-1, but it is still a
; ``caller.''

; Acl2-defaults-table is either a legal alist value for acl2-defaults-table or
; else is :do-not-install.  If the former, then that alist is installed as the
; acl2-defaults-table (if it is not already there) after executing the events
; in ev-lst.

; The name ee-entry stands for ``embedded-event-lst'' entry.  It is
; consed onto the embedded-event-lst for the duration of the processing
; of ev-lst.  The length of that list indicates how deep these evs are.
; For example, if the embedded-event-lst were:
;   ((defstobj ...)
;    (encapsulate nil) 
;    (include-book ...)
;    (encapsulate ((p (x y) (nil nil) (nil)) ...)))
; Then the ev-lst is the ``body'' of a defstobj, which occurs in the body of
; an encapsulate, which is in an include-book, which is in an encapsulate.

; The shape of an ee-entry is entirely up to the callers and the customers
; of the embedded-event-lst, with three exceptions:
; (a) the ee-entry must always be a consp;
; (b) if the car of the ee-entry is 'encapsulate then the cadr
;     is the internal form signatures of the functions being constrained; and
; (c) if the car of the ee-entry is 'include-book then the cadr is the
;     full-book-name.
; We refer to the signatures in (b) as insigs below and think of insigs as nil
; for all ee-entries other than encapsulates.

; Ev-lst is the list of alleged events.  Pkg is the value we should use for
; current-package while we are processing the events.  This affects how forms
; are prettyprinted.  It also affects how the prompt looks.

; We first extend the current world of state by insigs (if caller is
; 'encapsulate-pass-2) and extend the embedded event list by ee-entry.  We then
; extend further by doing each of events in ev-lst while ld-skip-proofsp is set
; to skip-proofsp, checking that they are indeed embedded-event-forms.  If that
; succeeds, we restore embedded-event-lst, install the world, and return.

; If caller is not 'encapsulate-pass-2, then we return an expansion-alist that
; records the result of expanding away every make-event call encountered in the
; course of processing the given ev-lst.  Each pair (n . ev) in expansion-alist
; asserts that ev is the result of expanding away every make-event call during
; evaluation of the nth member of ev-lst (starting with index for the initial
; member of ev-lst), though if no such expansion took place then this pair is
; omitted.

; If caller is 'encapsulate-pass-2, then since the final world is in STATE, we
; use the value component of the non-erroneous return triple to return the
; world extended by the signatures (and the incremented depth).  That world,
; called proto-wrld3 in the encapsulate essay and below, is useful only for
; computing (via difference) the names introduced by the embedded events.  We
; still need the expansion-alist described in the preceding paragraph, so the
; value returned for 'encapsulate-pass-2 is the cons of that expansion-alist
; with this proto-wrld3.

; If an error is caused by the attempt to embed the events, we print a warning
; message explaining and pass the error up.

; The world names used here are consistent with the encapsulate essay.

  (let* ((wrld1 (w state))
         (insigs (if (eq (car ee-entry) 'encapsulate)
                     (cadr ee-entry)
                   nil))
         (old-embedded-event-lst
          (global-val 'embedded-event-lst wrld1))
         (new-embedded-event-lst
          (cons ee-entry old-embedded-event-lst))

; We now declare the signatures of the hidden functions (when we're in
; pass 2 of encapsulate), producing what we here call proto-wrld3.  We
; also extend the embedded event list by ee-entry.  After installing that
; world in state we'll execute the embedded events on it to produce
; the wrld3 of the encapsulation essay.

         (proto-wrld3
          (global-set 'embedded-event-lst new-embedded-event-lst
                      (cond ((eq caller 'encapsulate-pass-2)
                             (intro-udf-lst insigs wrld1))
                            (t wrld1)))))
    (let ((state (set-w 'extension proto-wrld3 state)))
      (er-progn
       (cond ((not (find-non-hidden-package-entry pkg
                                                  (known-package-alist state)))
              (er soft 'in-package
                  "The argument to IN-PACKAGE must be a known package ~
                   name, but ~x0 is not.  The known packages are~*1"
                  pkg
                  (tilde-*-&v-strings
                   '&
                   (strip-non-hidden-package-names (known-package-alist state))
                   #\.)))
             (t (value nil)))

; If we really executed an (in-package-fn pkg state) it would do the
; check above and cause an error if pkg was unknown.  But we just bind
; current-package to pkg (with "unwind protection") and so we have to
; make the check ourselves.

       (mv-let (erp expansion-alist state)
               (state-global-let*
                ((current-package pkg)
                 (ld-skip-proofsp skip-proofsp))
                (er-progn

; Once upon a time, under the same conditions on caller as shown
; below, we added '(logic) to the front of ev-lst before doing the
; eval-event-lst below.  But if the caller is an include-book inside a
; LOCAL, then the (LOGIC) event at the front is rejected by
; chk-embedded-event-form.  One might wonder whether an erroneous
; ev-lst would have left us in a different state than here.  The
; answer is no.  If ev-lst causes an error, eval-event-lst returns
; whatever the state was at the time of the error and does not do any
; cleanup.  The error is passed up to the revert-world-on-error we
; know is above us, which will undo the (logic) as well as anything
; else we changed.

                 (if (or (eq caller 'include-book)
                         (eq caller 'defstobj))

; The following is equivalent to (logic), without the PROGN (value
; :invisible).  The PROGN is illegal in Common Lisp code because its
; ACL2 semantics differs from its CLTL semantics.  Furthermore, we
; can't write (TABLE acl2-defaults-table :defun-mode :logic) because,
; like PROGN, its CLTL semantics is different.

                     (state-global-let*
                      ((inhibit-output-lst (cons 'summary (@ inhibit-output-lst))))
                      (table-fn 'acl2-defaults-table
                                '(:defun-mode :logic)
                                state
                                '(table acl2-defaults-table :defun-mode :logic)))
 
                   (value nil))
                 (mv-let
                  (erp val expansion-alist state)
                  (pprogn
                   (cond ((eq caller 'encapsulate-pass-1)
                          (pprogn (f-put-global 'redo-flat-succ nil state)
                                  (f-put-global 'redo-flat-fail nil state)))
                         (t state))
                   (eval-event-lst index nil
                                   ev-lst
                                   (ld-skip-proofsp state)
                                   (in-encapsulatep new-embedded-event-lst nil)
                                   (f-get-global 'in-local-flg state)
                                   nil make-event-chk ctx (proofs-co state)
                                   state))
                  (cond (erp (pprogn
                              (cond ((eq caller 'encapsulate-pass-1)
                                     (update-for-redo-flat val ev-lst state))
                                    (t state))
                              (mv erp val state)))
                        (t (er-progn
                            (if (eq acl2-defaults-table :do-not-install)
                                (value nil)
                              (maybe-install-acl2-defaults-table
                               acl2-defaults-table ctx state))
                            (value expansion-alist)))))))
               (cond
                (erp

; The evaluation of the embedded events caused an error.  If
; skip-proofsp is t, then we have a local incompatibility (because we
; know the events were successfully processed while not skipping proofs
; earlier).  If skip-proofsp is nil, we simply have an inappropriate
; ev-lst.

                 (cond
                  ((eq caller 'defstobj)
                   (value (er hard ctx
                              "An error has occurred while DEFSTOBJ was ~
                               defining the supporting functions.  This is ~
                               supposed to be impossible!  Please report this ~
                               error to the ACL2 implementors.")))
                  (t
                   (pprogn
                    (warning$ ctx nil
                              (cond
                               ((or (eq skip-proofsp nil)
                                    (eq skip-proofsp t))
                                "The attempted ~x0 has failed while ~
                                 trying to establish the ~
                                 admissibility of one of the (local ~
                                 or non-local) forms in ~#1~[the body ~
                                 of the ENCAPSULATE~/the book to be ~
                                 certified~].")
                               ((eq caller 'encapsulate-pass-2)
                                "The error reported above is the ~
                                 manifestation of a local ~
                                 incompatibility.  See :DOC ~
                                 local-incompatibility.  The ~
                                 attempted ~x0 has failed.")
                               (t "The error reported above indicates ~
                                   that this book is incompatible ~
                                   with the current logical world.  ~
                                   The attempted ~x0 has failed."))
                              (if (or (eq caller 'encapsulate-pass-1)
                                      (eq caller 'encapsulate-pass-2))
                                  'encapsulate
                                caller)
                              (if (eq caller 'encapsulate-pass-1) 0 1))
                    (mv t nil state)))))
                (t 

; The evaluation caused no error.  The world inside state is the current one
; (because nothing but events were evaluated and they each install the world).
; Pop the embedded event list and install that world.  We let our caller extend
; it with constraints if that is necessary.  We return proto-wrld3 so the
; caller can compute the difference attributable to the embedded events.  This
; is how the constraints are determined.

                 (let ((state
                        (set-w 'extension
                               (global-set 'embedded-event-lst
                                           old-embedded-event-lst
                                           (w state))
                               state)))
                   (cond ((eq caller 'encapsulate-pass-2)
                          (value (cons expansion-alist proto-wrld3)))
                         (t (value expansion-alist)))))))))))

(defun constrained-functions (exported-fns sig-fns new-trips)

; New-trips is the list of triples introduced into wrld3 from proto-wrld3,
; where wrld3 is the world created from proto-wrld3 by the second pass of an
; encapsulate, the one in which local events have been skipped.  (See the
; encapsulate essay.)  We return all the functions in exported-fns that,
; according to the world segment represented by new-trips, have a member of
; sig-fns among their ancestors.  We include sig-fns in the result as well.

; Important:  The new-trips needs to be in the same order as in wrld3, because
; of the call of instantiable-ancestors below.

  (cond
   ((endp exported-fns) sig-fns)
   (t (let ((ancestors
             (instantiable-ancestors (list (car exported-fns)) new-trips nil)))
        (cond
         ((intersectp-eq sig-fns ancestors)
          (cons (car exported-fns)
                (constrained-functions (cdr exported-fns) sig-fns new-trips)))
         (t (constrained-functions (cdr exported-fns) sig-fns new-trips)))))))

(defun collect-logicals (names wrld)

; Names is a list of function symbols.  Collect the :logic ones.

  (cond ((null names) nil)
        ((logicalp (car names) wrld)
         (cons (car names) (collect-logicals (cdr names) wrld)))
        (t (collect-logicals (cdr names) wrld))))

(defun exported-function-names (new-trips)
  (cond ((endp new-trips)
         nil)
        (t (let ((new-name (name-introduced (car new-trips) t)))

; Because of the second argument of t, above, new-name is known to be
; a function name.

             (cond (new-name
                    (cons new-name (exported-function-names (cdr new-trips))))
                   (t (exported-function-names (cdr new-trips))))))))

(defun get-unnormalized-bodies (names wrld)
  (cond ((endp names) nil)
        (t (cons (getprop (car names) 'unnormalized-body nil
                          'current-acl2-world wrld)
                 (get-unnormalized-bodies (cdr names) wrld)))))

(defun collect-t-machines (fns wrld seen)

; Given a list of functions, this function partitions it into an
; alist, each pair in which is of the form

; ((fn1 ... fnk) . (t-machine1 ... t-machinek))

; with the proviso that in the case of a non-recursive function fn, the
; pair looks like

; ((fn) . nil)

  (cond ((endp fns) nil)
        ((member-eq (car fns) seen)
         (collect-t-machines (cdr fns) wrld seen))
        (t (let* ((recp (getprop (car fns) 'recursivep nil
                                 'current-acl2-world wrld))
                  (names (if recp
                             recp
                           (list (car fns)))))
             (cond
              (recp
               (cons (cons names
                           (termination-machines names
                                                 (get-unnormalized-bodies
                                                  names
                                                  wrld)))

; We put the entire clique of names into seen, to skip the other members.
; This is actually unnecessary if names is a singleton.

                     (collect-t-machines (cdr fns) wrld (append names seen))))
              (t (cons (cons names nil)
                       (collect-t-machines (cdr fns) wrld seen))))))))

(defun subversivep (fns t-machine)

; See subversive-cliquep for conditions (1) and (2).

  (cond ((endp t-machine) nil)
        (t (or (intersectp-eq fns ; Condition (1)
                              (all-fnnames-lst (access tests-and-call
                                                       (car t-machine)
                                                       :tests)))
               (intersectp-eq fns ; Condition (2)
                              (all-fnnames-lst
                               (fargs (access tests-and-call
                                              (car t-machine)
                                              :call))))
               (subversivep fns (cdr t-machine))))))

(defun subversive-cliquep (fns t-machines)

; Here, fns is a list of functions introduced in an encapsulate.  If
; we are using [Front] to move some functions forward, then fns is the
; list of ones that are NOT moved: they all use the signature
; functions somehow.  T-machines is a list of termination machines for
; some clique of functions defined within the encapsulate.  The clique
; is subversive if some function defined in the clique is has a
; subversive t-machine.

; Intuitively, a t-machine is subversive if its admission depended on
; properties of the witnesses for signature functions.  That is, the
; definition uses signature functions in a way that affects the
; termination argument.

; Technically a t-machine is subversive if some tests-and-call record
; in it has either of the following properties:

; (1) a test mentions a function in fns

; (2) an argument of a call mentions a function in fns.

; Observe that if a clique is not subversive then every test and
; argument to every recursive call uses functions defined outside the
; encapsulate.  If we are in a top-level encapsulate, then a
; non-subversive clique is a ``tight'' clique wrt the functions in the
; initial world of the encapsulate.

  (cond ((endp t-machines) nil)
        (t (or (subversivep fns (car t-machines))
               (subversive-cliquep fns (cdr t-machines))))))

(defun contains-non-trivial-encapsulatep (new-trips)

; We return t if new-trips contains an encapsulate event with
; non-empty signature.  This function is used when we decided whether
; to ``rearrange'' the events inside an encapsulate, i.e., to use the
; theorems [Front] and [Back] of the encapsulate paper to move some
; events outside of the encapsulate.  New-trips is known not to
; contain *acl2-property-unbound* values.

  (cond ((endp new-trips) nil)
        (t (or (let ((trip (car new-trips)))
                 (and (eq (car trip) 'event-landmark)
                      (eq (cadr trip) 'global-value)
                      (eq (access-event-tuple-type (cddr trip))
                          'encapsulate)
                      (cadr (access-event-tuple-form (cddr trip)))))
               (contains-non-trivial-encapsulatep (cdr new-trips))))))

(defun definitional-constraints (fn wrld)

; Warning: Keep this function in sync with constraints-introduced.  We
; return a list of formulas consisting of the defchoose-axiom or the
; definitional equation and type-prescription.

  (let ((formula (formula fn nil wrld))
        (tp (find-runed-type-prescription
             (list :type-prescription fn)
             (getprop fn 'type-prescriptions nil 'current-acl2-world wrld))))

; Formula is either the definining equation of a definition, the
; defchoose-axiom of a defchoose, or nil for a primitive function like
; car or an constrained function (even one with a non-trivial
; constraints-lst).

; Tp is either a type-prescription or nil.  If the former, it is a
; type-prescription computed by the system for a defined function --
; because it has the name of the fn, not some user-supplied name like
; consp-fn.
        
    (cond (formula
           (cond (tp
                  (list formula
                        (access type-prescription tp :corollary)))
                 (t (list formula))))
          (tp (list (access type-prescription tp :corollary)))
          (t nil))))

(defun definitional-constraints-list (fns wrld)
  (cond ((endp fns) nil)
        (t (append (definitional-constraints (car fns) wrld)
                   (definitional-constraints-list (cdr fns) wrld)))))

(defun iteratively-grow-constraint1
  (fns                               ;;; the list of all fns not moved forward
   t-machine-alist                   ;;; a tail of collect-t-machines output
   formula-lst                       ;;; the list of constraints so far
   fns-in-formula-lst                ;;; all fns ``involved'' in formula-lst
                                     ;;; (including ancestors of involved fns)
   subversive-fns                    ;;; subversive members of fns, so far
   new-trips                         ;;; the new trips added by this encap
                                     ;;;  used to compute ancestors of defs
   wrld                              ;;; the world, starting with new-trips
   no-action-lst                     ;;; pairs of t-machine-alist already
                                     ;;; processed and thought to be irrelevant
   subversive-enlargementp           ;;; t if we have enlarged the formula with
                                     ;;; a subversive function
   everythingp                       ;;; t if we are to sweep everything into
                                     ;;; the constraint; nil if we can use
                                     ;;; [Back].
   infectious-fns)                   ;;; list of (non-subversive) fns infecting
                                     ;;; the constraint.

  (cond
   ((endp t-machine-alist)
    (cond
     (subversive-enlargementp

; We added to the formula some subversive function and suitably extended
; the fns-in-formula-lst.  But what about the elements of t-machine-alist
; that we had previously processed and found to be irrelevant?  We must
; go around again.

      (iteratively-grow-constraint1
       fns
       no-action-lst
       formula-lst
       fns-in-formula-lst
       subversive-fns
       new-trips
       wrld
       nil
       nil
       everythingp
       infectious-fns))
     (t
      (mv formula-lst                  ;;; final list of constraint formulas
          (intersection-eq             ;;; all fns constrained -- the inter-
           fns-in-formula-lst fns)     ;;;  section eliminates fns from before
                                       ;;;  the encapsulate started
          subversive-fns               ;;; all subversive fns
          infectious-fns               ;;; other fns infecting constraint.
          ))))
   (t (let ((names (car (car t-machine-alist)))
            (t-machines (cdr (car t-machine-alist))))
           
        (cond
         ((null t-machines)

; Names is a singleton and contains a non-recursive function.  It
; cannot be subversive.  The only question is whether this function is
; involved in the constraint.  If so, we want to enlarge the
; constraint.

          (cond
           ((or everythingp
                (member-eq (car names) fns-in-formula-lst))
            (let ((additional-constraints
                   (definitional-constraints (car names) wrld)))
              (iteratively-grow-constraint1
               fns
               (cdr t-machine-alist)
               (union-equal additional-constraints formula-lst)
               (if (member-eq (car names) fns-in-formula-lst)
                   fns-in-formula-lst
                 (instantiable-ffn-symbs-lst additional-constraints
                                             new-trips
                                             fns-in-formula-lst      
                                             nil))
               subversive-fns
               new-trips
               wrld
               no-action-lst
               subversive-enlargementp
               everythingp
               (if additional-constraints
                   (cons (car names) infectious-fns)
                 infectious-fns))))
           (t (iteratively-grow-constraint1
               fns
               (cdr t-machine-alist)
               formula-lst
               fns-in-formula-lst
               subversive-fns
               new-trips
               wrld
               (cons (car t-machine-alist) no-action-lst)
               subversive-enlargementp
               everythingp
               infectious-fns))))
         ((subversive-cliquep fns t-machines)
          (let* ((additional-constraints
                  (definitional-constraints-list names wrld)))
            (iteratively-grow-constraint1
             fns
             (cdr t-machine-alist)
             (union-equal additional-constraints formula-lst)
             (instantiable-ffn-symbs-lst additional-constraints
                                         new-trips
                                         fns-in-formula-lst      
                                         nil)
             (append names subversive-fns)
             new-trips
             wrld
             no-action-lst
             t
             everythingp
             infectious-fns)))
         ((or everythingp
              (intersectp-eq names fns-in-formula-lst))
          (let ((additional-constraints
                 (definitional-constraints-list names wrld)))
            (iteratively-grow-constraint1
             fns
             (cdr t-machine-alist)
             (union-equal additional-constraints formula-lst)
             (if (intersectp-eq names fns-in-formula-lst)
                 fns-in-formula-lst
               (instantiable-ffn-symbs-lst additional-constraints
                                           new-trips
                                           fns-in-formula-lst      
                                           nil))
             subversive-fns
             new-trips
             wrld
             no-action-lst
             subversive-enlargementp
             everythingp
             (if additional-constraints
                 (append names infectious-fns)
               infectious-fns))))
         (t (iteratively-grow-constraint1
               fns
               (cdr t-machine-alist)
               formula-lst
               fns-in-formula-lst
               subversive-fns
               new-trips
               wrld
               (cons (car t-machine-alist) no-action-lst)
               subversive-enlargementp
               everythingp
               infectious-fns)))))))

(defun iteratively-grow-constraint (sig-fns exported-names new-trips wrld)

; Sig-fns is the list of functions appearing in the signature of an
; encapsulate.  Exported-names is the list of all functions introduced
; (non-locally) in the body of the encapsulate (it doesn't include
; sig-fns).  New-trips is the list of property list triples added to
; the initial world to form wrld.  Wrld is the result of processing
; the non-local events in body.

; We return (mv constraints constrained-fns subversive-fns
; infectious-fns), where constraints is a list of the formulas that
; constrain all of the functions listed in constrained-fns.
; Subversive-fns is a list of exported functions which are not tight
; wrt the initial world.  Infectious-fns is the list of fns (other
; than subversives) whose defuns are in the constraint.  This could
; happen either because we were not allowed to rearrange but
; encountered a defun, or because some non-subversive definition is
; ancestral in the constraint.

; We do not actually rearrange anything.  Instead, we compute the constraint
; formula generated by this encapsulate as though we had pulled certain defuns
; and defchooses out before generating it.

  (let* ((rearrange-eventsp
          (and (not (in-encapsulatep
                     (global-val 'embedded-event-lst wrld)
                     t))

; Note: 'embedded-event-lst of this wrld does not include the current
; sig-fns.  That entry was chopped off at the very end of
; process-embedded-events.  The check above ensures that we are not in
; the scope of a non-trivial encapsulate.

               sig-fns
               (not
                (contains-non-trivial-encapsulatep new-trips))))
         (fns 
          (if rearrange-eventsp

; Imagine moving all definitions (defuns and defchooses) that we can,
; so that they are in front of the encapsulate, as described in :DOC
; constraint.  What's left is the list we define here: the function
; symbols introduced by the encapsulate for which the signature
; functions are ancestral.  Fns includes the signature functions.  It
; is empty if sig-fns is empty, i.e., all defuns and defchooses got
; moved up front.

              (constrained-functions
               (collect-logicals exported-names wrld)
               sig-fns
               new-trips)
            (append (collect-logicals exported-names wrld)
                    sig-fns)))
         (formula-lst (constraints-introduced new-trips fns wrld nil nil)))
    (iteratively-grow-constraint1
     fns
     (collect-t-machines fns wrld nil)
     formula-lst
     (instantiable-ffn-symbs-lst formula-lst new-trips sig-fns nil)
     nil
     new-trips
     wrld
     nil
     nil

; The last argument to iterative-grow-constraint1 is everythingp:  It should
; be t if we are not allowed to rearrange things, and should be nil if we
; are allowed to rearrange things.

     (not rearrange-eventsp)
     nil)))

(defun erase-induction-properties (subversive-fns wrld)
  
; We remove the 'induction-machine property of each fn in
; subversive-fns.  In addition, we remove the 'quick-block-info.  This
; makes these functions look just like mutually-recursive functions,
; i.e., of the properties put by put-induction-info, they have only
; 'recursivep, 'justification, and 'symbol-class.

  (putprop-x-lst1 subversive-fns
                  'induction-machine
                  *acl2-property-unbound*
                  (putprop-x-lst1 subversive-fns
                                  'quick-block-info
                                  *acl2-property-unbound*
                                  wrld)))

(defun encapsulate-pass-2 (insigs ev-lst saved-acl2-defaults-table only-pass-p
                                  ctx state)

; Warning: This function uses set-w and hence may only be called
; within a revert-world-on-error.  See the statement of policy in
; set-w.

; This is the second pass of the encapsulate event.  We assume that
; the installed world in state is wrld1 of the encapsulate essay.  We
; assume that chk-acceptable-encapsulate1 has approved of wrld1 and
; chk-acceptable-encapsulate2 has approved of the wrld2 generated in
; with ld-skip-proofsp nil.  Insigs is the internal form
; signatures list.  We either cause an error and return a state in
; which wrld1 is current or else we return normally and return a state
; in which wrld3 of the essay is current.  In the case of normal
; return and only-pass-p = nil, the value is a list containing

; * constrained-fns - the functions for which a new constraint-lst will
;   be stored

; * constraints - the corresponding list of constraints

; * exported-names - the exported names

; * subversive-fns - the subversive (non-tight) functions encountered

; * infectious-fns - list of (non-subversive) fns whose defun equations were
;   moved into the constraint (possibly because we were not allowed to
;   rearrange).

; However, if only-pass-p = t, then the value returned is an expansion-alist
; mapping, in reverse increasing order, indices of events in ev-lst to the
; result of expanding away make-event calls.

; This information is used by the output routines.

; Note:  The function could be declared to return five values, but we would
; rather use the standard state and error primitives and so it returns three
; and lists together the three "real" answers.

  (let ((wrld1 (w state)))
    (er-let* ((expansion-alist-and-proto-wrld3

; The following process-embedded-events, which requires world reversion
; on errors, is protected by virtue of being in encapsulate-pass-2, which
; also requires such reversion.

; Note: The proto-wrld3 returned below is wrld1 above extended by the
; signatures.  The installed world after this process-embedded-events
; has the non-local events of ev-lst in it.

               (state-global-let*
                ((in-local-flg

; As we start processing the events in the encapsulat, we are no longer in the
; lexical scope of LOCAL for purposes of disallowing setting of the
; acl2-defaults-table.

                  (and (f-get-global 'in-local-flg state)
                       'dynamic)))
                (process-embedded-events 'encapsulate-pass-2
                                         saved-acl2-defaults-table
                                         'include-book
                                         (current-package state)
                                         (list 'encapsulate insigs)
                                         ev-lst 0

; If only-pass-p is t then we need to allow make-event with :check-expansion
; that is not a cons.  The reason is that a make-event with non-nil
; :check-expansion can cause us not to blow away a make-event, which will
; result in a call of make-event-fn, whose expansion can create a make-event
; with :check-expansion t.  If this latter make-event is under an encapsulate,
; then this call of process-embedded-events will cause a call of
; chk-embedded-event-form, which will cause an error if the following argument
; is t.

                                         (not only-pass-p)
                                         ctx state))))
             (let* ((expansion-alist (car expansion-alist-and-proto-wrld3))
                    (proto-wrld3 (cdr expansion-alist-and-proto-wrld3))
                    (wrld (w state))
                    (new-trips (new-trips wrld proto-wrld3 nil nil))
                    (exported-names (exported-function-names new-trips)))
               (cond
                ((and expansion-alist (not only-pass-p))
                 (value (er hard ctx
                            "Implementation error: Unexpected expansion-alist ~
                            ~x0 for second pass of encapsulate.  Please ~
                            contact the ACL2 implementors.")))
                ((null insigs)
                 (value (if only-pass-p
                            expansion-alist
                          (list nil nil exported-names))))
                (t

; We are about to collect the constraint generated by this encapsulate
; on the signature functions.  We ``optimize'' one common case: if
; this is a top-level encapsulation that has a non-empty signature (so
; it introduces some constrained functions) and no encapsulate in its
; body introduces any constrained functions, then we may use the
; theorems [Front] and [Back] of the encapsulate paper to
; ``rearrange'' the events within this encapsulate.  If, on the other
; hand, this encapsulate is contained in another or it contains an
; encapsulate with a non-empty signature, we do not rearrange things.
; Of course, the whole point is moot if this encapsulate has an empty
; signature -- there will be no constraints anyway.

                 (let* ((new-trips (new-trips wrld wrld1 nil nil))
                        (sig-fns (strip-cars insigs)))
                   (mv-let
                    (constraints constrained-fns
                                 subversive-fns infectious-fns)
                    (iteratively-grow-constraint sig-fns exported-names
                                                 new-trips wrld)
                    (let ((state
                           (set-w 'extension
                                  (putprop-constraints
                                   (car sig-fns)
                                   (remove1-eq (car sig-fns) constrained-fns)
                                   constraints
                                   (erase-induction-properties subversive-fns
                                                               wrld))
                                  state)))
                      (value (if only-pass-p
                                 expansion-alist
                               (list constrained-fns
                                     constraints
                                     exported-names
                                     subversive-fns
                                     infectious-fns))))))))))))

#|

; Here I have collected a sequence of encapsulates to test the implementation.
; After each is an undo.  They are not meant to co-exist.  Just eval each
; of the forms in this comment.  You should never get an error.

(set-state-ok t)

(defun test (val)
  (declare (xargs :mode :program))
  (if val
      'ok
    (er hard 'test "This example failed!")))
                                            
; I start with a collection of simple encapsulates, primarily to test the
; handling of signatures in their three forms.  I need a stobj.  

(defstobj $s x y)

; Here is a simple, typical encapsulate.
(encapsulate ((p (x) t))
  (local (defun p (x) (declare (ignore x)) t))
  (defthm booleanp-p (booleanp (p x))))

(test
 (equal
  (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
  '((booleanp (P X)))))

(u)

; The next set just look for errors that should never happen.

(encapsulate (((p *) => *))
             (local (defun p (x) x)))


#|
The following all cause errors.

(encapsulate (((p x) => x))
             (local (defun p (x) x)))

(encapsulate ((p x) => x)
             (local (defun p (x) x)))

(encapsulate (((p x $s) => (mv x $s)))
             (local (defun p (x $s) (declare (xargs :stobjs ($s))) (mv x $s))))

(encapsulate (((p * state $s) => state))
             (local (defun p (x state $s)
                      (declare (xargs :stobjs nil) (ignore x $s))
                      state)))

(encapsulate (((p * state *) => $s))
             (local (defun p (x state $s)
                      (declare (xargs :stobjs $s) (ignore x state))
                      $s)))

; Here are some of the "same" errors provoked in the old notation.


(encapsulate ((p (x $s) (mv * $s) :stobjs *))
             (local (defun p (x $s) (declare (xargs :stobjs ($s))) (mv x $s))))

(encapsulate ((p (* state $s) state))
             (local (defun p (x state $s)
                      (declare (xargs :stobjs nil) (ignore x $s))
                      state)))

(encapsulate ((p (y state $s) $s))
             (local (defun p (x state $s)
                      (declare (xargs :stobjs $s) (ignore x state))
                      $s)))

(encapsulate ((p (x state y) $s))
             (local (defun p (x state $s)
                      (declare (xargs :stobjs $s) (ignore x state))
                      $s)))

(encapsulate ((p (x state $s) $s :stobjs $s))
             (local (defun p (x state $s)
                      (declare (xargs :stobjs $s) (ignore x state))
                      $s)))
|#

; The rest of my tests are concerned with the constraints produced.

; Here is one that contains a function that can be moved forward out
; of encapsulate, even though it is used in the constraint.  Note that
; not every theorem proved becomes a constraint.  The theorem evp-+ is
; moved forward too.

(encapsulate ((p (x) t))
  (local (defun p (x) (declare (ignore x)) 2))
  (defun evp (n) (if (zp n) t (if (zp (- n 1)) nil (evp (- n 2)))))
  (defthm evp-+ (implies (and (integerp i)
                              (<= 0 i)
                              (evp i)
                              (integerp j)
                              (<= 0 j)
                              (evp j))
                         (evp (+ i j))))
  (defthm evp-p (evp (p x))))

(test
 (equal
  (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
  '((EVP (P X)))))

(u)

; This illustrates a function which uses the signature function p but
; which can be moved back out of the encapsulate.  The only constraint
; on p is (EVP (P X)).

; But if the function involves the constrained function, it cannot
; be moved forward.  It may be moved back, or it may become part of the
; constraint, depending on several things.

; Case 1.  The function uses p in a benign way and nothing is proved
; about the function.

(encapsulate ((p (x) t))
  (local (defun p (x) (ifix x)))
  (defun mapp (x)
    (if (consp x)
        (cons (p (car x)) (mapp (cdr x)))
      nil))
  (defthm integerp-p (integerp (p x))))

(test
 (and (equal (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
             '((integerp (p x))))
      (equal (getprop 'mapp 'constraint-lst nil 'current-acl2-world (w state))
             nil)))

(u)

; The constraint, above, on p is (INTEGERP (P X)).

; Case 2.  The function is subversive, i.e., uses p in a way critical to
; its termination.

(encapsulate ((p (x) t))
  (local (defun p (x) (cdr x)))
  (defthm len-p (implies (consp x) (< (len (p x)) (len x))))
  (defun bad (x)
    (if (consp x)
        (not (bad (p x)))
      t)))

(test
 (and (equal (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
             '((EQUAL (BAD X)
                      (IF (CONSP X)
                          (IF (BAD (P X)) 'NIL 'T)
                          'T))
               (IF (EQUAL (BAD X) 'T)
                   'T
                   (EQUAL (BAD X) 'NIL))
               (IMPLIES (CONSP X)
                        (< (LEN (P X)) (LEN X)))))
      (equal (getprop 'bad 'constraint-lst nil 'current-acl2-world (w state))
             'p)))

(u)

; The constraint, above, is 
; (AND (EQUAL (BAD X)
;            (OR (NOT (CONSP X))
;                (AND (NOT (BAD (P X))) T)))
;     (OR (EQUAL (BAD X) T)
;         (EQUAL (BAD X) NIL))
;     (IMPLIES (CONSP X)
;              (< (LEN (P X)) (LEN X))))
; 
; and it is associated both with p and bad.  That is, if you functionally
; instantiate p, the new function must satisfy the axiom for bad too,
; which means you must instantiate bad.  Similarly, if you instantiate
; bad, you must instantiate p.

; It would be better if you did this:

(encapsulate ((p (x) t))
  (local (defun p (x) (cdr x)))
  (defthm len-p (implies (consp x) (< (len (p x)) (len x)))))

(test
 (equal (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
        '((IMPLIES (CONSP X)
                   (< (LEN (P X)) (LEN X))))))

; The only constraint on p is 
; (IMPLIES (CONSP X) (< (LEN (P X)) (LEN X))).
; Now you can define bad outside:

(defun bad (x)
  (declare (xargs :measure (len x)))
  (if (consp x)
      (not (bad (p x)))
    t))

(u)
(u)

; Case 3.  The function uses p in a benign way but something is proved
; about the function, thus constraining p.

(encapsulate ((p (x) t))
  (local (defun p (x) (ifix x)))
  (defun mapp (x)
    (if (consp x)
        (cons (p (car x)) (mapp (cdr x)))
      nil))
  (defthm mapp-is-a-list-of-ints
    (integer-listp (mapp x))))

(test
 (and (equal (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
             '((EQUAL (MAPP X)
                      (IF (CONSP X)
                          (CONS (P (CAR X)) (MAPP (CDR X)))
                          'NIL))
               (TRUE-LISTP (MAPP X))
               (INTEGER-LISTP (MAPP X))))
      (equal (getprop 'mapp 'constraint-lst nil 'current-acl2-world (w state))
             'p)))

(u)

; The constraint above, on both p and mapp, is
; (AND (EQUAL (MAPP X)
;             (AND (CONSP X)
;                  (CONS (P (CAR X)) (MAPP (CDR X)))))
;      (TRUE-LISTP (MAPP X))
;      (INTEGER-LISTP (MAPP X)))

; Here is another case of a subversive definition, illustrating that
; we do not just check whether the function uses p but whether it uses
; p ancestrally.

(encapsulate ((p (x) t))
  (local (defun p (x) (cdr x)))
  (defun bad1 (x) (p x))
  (defun bad2 (x)
    (if (consp x)
        (not (bad2 (bad1 x)))
      t)))

(test
 (and (equal (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
             '((EQUAL (BAD1 X) (P X))
               (EQUAL (BAD2 X)
                      (IF (CONSP X)
                          (IF (BAD2 (BAD1 X)) 'NIL 'T)
                          'T))
               (IF (EQUAL (BAD2 X) 'T)
                   'T
                   (EQUAL (BAD2 X) 'NIL))))
      (equal (getprop 'bad1 'constraint-lst nil 'current-acl2-world (w state))
             'p)
      (equal (getprop 'bad2 'constraint-lst nil 'current-acl2-world (w state))
             'p)
      (equal (getprop 'bad2 'induction-machine nil
                      'current-acl2-world (w state))
             nil)))


(u)

(encapsulate ((p (x) t))
  (local (defun p (x) (cdr x)))
  (defun bad1 (x)
    (if (consp x) (bad1 (cdr x)) (p x)))
  (defun bad2 (x)
    (if (consp x)
        (not (bad2 (bad1 x)))
      t)))

(test
 (and (equal (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
             '((EQUAL (BAD1 X)
                      (IF (CONSP X)
                          (BAD1 (CDR X))
                          (P X)))
               (EQUAL (BAD2 X)
                      (IF (CONSP X)
                          (IF (BAD2 (BAD1 X)) 'NIL 'T)
                          'T))
               (IF (EQUAL (BAD2 X) 'T)
                   'T
                   (EQUAL (BAD2 X) 'NIL))))
      (equal (getprop 'bad1 'constraint-lst nil 'current-acl2-world (w state))
             'p)
      (equal (getprop 'bad2 'constraint-lst nil 'current-acl2-world (w state))
             'p)
      (not (equal (getprop 'bad1 'induction-machine nil
                           'current-acl2-world (w state))
                  nil))
      (equal (getprop 'bad2 'induction-machine nil
                      'current-acl2-world (w state))
             nil)))

(u)

; Once up a time we had a bug in encapsulate, because subversiveness was
; based on the induction machine rather than the termination machine
; and no induction machine is constructed for mutually recursive definitions.
; Here is an example that once led to unsoundness:

(encapsulate
 ((fn1 (x) t))
 (local (defun fn1 (x)
          (cdr x)))
 (mutual-recursion
  (defun fn2 (x)
    (if (consp x)
        (not (fn3 (fn1 x)))
      t))
  (defun fn3 (x)
    (if (consp x)
        (not (fn3 (fn1 x)))
      t))))

(test
 (and (equal (getprop 'fn1 'constraint-lst nil 'current-acl2-world (w state))
             '((EQUAL (FN2 X)
                      (IF (CONSP X)
                          (IF (FN3 (FN1 X)) 'NIL 'T)
                          'T))
               (IF (EQUAL (FN2 X) 'T)
                   'T
                   (EQUAL (FN2 X) 'NIL))
               (EQUAL (FN3 X)
                      (IF (CONSP X)
                          (IF (FN3 (FN1 X)) 'NIL 'T)
                          'T))
               (IF (EQUAL (FN3 X) 'T)
                   'T
                   (EQUAL (FN3 X) 'NIL))))
      (equal (getprop 'fn2 'constraint-lst nil 'current-acl2-world (w state))
             'fn1)
      (equal (getprop 'fn3 'constraint-lst nil 'current-acl2-world (w state))
             'fn1)
      (equal (getprop 'fn2 'induction-machine nil
                      'current-acl2-world (w state))
             nil)
      (equal (getprop 'fn3 'induction-machine nil
                      'current-acl2-world (w state))
             nil)))

; Now, fn1, fn2, and fn3 share both definitional constraints.

; It is possible to prove the following lemma

(defthm lemma
  (not (equal (fn1 '(a)) '(a)))
  :rule-classes nil
  :hints (("Goal" :use (:instance fn3 (x '(a))))))

; But in the unsound version it was then possible to functionally
; instantiate it, choosing the identity function for fn1, to derive
; a contradiction.  Here is the old killer:

; (defthm bad
;   nil
;   :rule-classes nil
;   :hints (("Goal" :use (:functional-instance lemma (fn1 identity)))))

(u)
(u)

; Now when you do that you have to prove an impossible theorem about
; fn3, namely

; (equal (fn3 x) (if (consp x) (not (fn3 x)) t))

; The only way to prove this is to show that nothing is a cons.

; This examples shows that a function can call a subversive one and
; not be subversive.

(encapsulate ((p (x) t))
  (local (defun p (x) (cdr x)))
  (defun bad1 (x) (p x))            ; tight: non-recursive

  (defun bad2 (x)                   ; not tight: recursive call involves
    (if (consp x)                   ; a fn (bad1) defined inside the encap
        (not (bad2 (bad1 x)))
      t))
  (defun bad3 (x)
    (if (consp x)
        (bad2 (bad3 (cdr x)))
      nil)))                        ; tight: even though it calls bad2

; Bad2 is swept into the constraint because it is not tight (subversive).  Bad1
; is swept into it because it introduces a function (bad1) used in the enlarged
; constraint.  Bad3 is not swept in.  Indeed, bad3 is moved [Back].

(test
 (and (equal (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
             '((EQUAL (BAD1 X) (P X))
               (EQUAL (BAD2 X)
                      (IF (CONSP X)
                          (IF (BAD2 (BAD1 X)) 'NIL 'T)
                          'T))
               (IF (EQUAL (BAD2 X) 'T)
                   'T
                   (EQUAL (BAD2 X) 'NIL))))
      (equal (getprop 'bad1 'constraint-lst nil 'current-acl2-world (w state))
             'p)
      (equal (getprop 'bad2 'constraint-lst nil 'current-acl2-world (w state))
             'p)
      (equal (getprop 'bad3 'constraint-lst nil 'current-acl2-world (w state))
             nil)
      (equal (getprop 'bad2 'induction-machine nil
                      'current-acl2-world (w state))
             nil)
      (not (equal (getprop 'bad3 'induction-machine nil
                           'current-acl2-world (w state))
                  nil))))

(u)

; Now what about nested encapsulates?

; Let us first consider the two simplest cases:

(encapsulate ((p (x) t))
  (local (defun p (x) (declare (ignore x)) 23))
  (encapsulate nil
     (defthm lemma1 (equal x x) :rule-classes nil)
     (defthm main (equal x x) :rule-classes nil))
  (defthm integerp-p (integerp (p x))))

; We are permitted to rearrange this, because the inner encap has a nil
; signature.  So we get what we expect:

(test
 (equal
  (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
  '((integerp (P X)))))

(u)

; The other simple case is

(encapsulate nil
   (defthm lemma1 (equal x x) :rule-classes nil)
   (defthm main (equal x x) :rule-classes nil)
   (encapsulate ((p (x) t))
                (local (defun p (x) (declare (ignore x)) 23))
                (defun benign (x)
                  (if (consp x) (benign (cdr x)) x))
                (defthm integerp-p (integerp (p x)))))

; Note that benign doesn't constrain p, because the containing encap
; contains no sig fns.

(test
 (equal
  (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
  '((integerp (P X)))))

(u)

; But if we have a pair of encaps, each of which introduces a sig fn,
; we lose the ability to rearrange things:

(encapsulate ((p1 (x) t))
             (local (defun p1 (x) x))             
             (defun benign1 (x)
               (if (consp x) (benign1 (cdr x)) t))
             (defthm p1-constraint (benign1 (p1 x)))
             (encapsulate  ((p2 (x) t))
                           (local (defun p2 (x) x))             
                           (defun benign2 (x)
                             (if (consp x) (benign2 (cdr x)) t))
                           (defthm p2-constraint (benign2 (p2 x)))))

(test
 (and (equal (getprop 'p1 'constraint-lst nil 'current-acl2-world (w state))
             '((EQUAL (BENIGN1 X)
                      (IF (CONSP X) (BENIGN1 (CDR X)) 'T))
               (BENIGN1 X)
               (BENIGN1 (P1 X))
               (BENIGN2 (P2 X))
               (BENIGN2 X)
               (EQUAL (BENIGN2 X)
                      (IF (CONSP X) (BENIGN2 (CDR X)) 'T))))
      (equal
       (getprop 'p2 'constraint-lst nil 'current-acl2-world (w state))
       'p1)
      (equal
       (getprop 'benign2 'constraint-lst nil 'current-acl2-world (w state))
       'p1)
      (equal
       (getprop 'benign1 'constraint-lst nil 'current-acl2-world (w state))
       'p1)))

(u)

(encapsulate ((f1 (x) t))
             (local (defun f1 (x) (declare (ignore x)) 0))
             (defun bad (x)
               (if (consp x)
                   (if (and (integerp (bad (cdr x)))
                            (<= 0 (bad (cdr x)))
                            (< (bad (cdr x)) (acl2-count x)))
                       (bad (bad (cdr x)))
                     (f1 x))
                 0)))

(test
 (and (equal (getprop 'f1 'constraint-lst nil 'current-acl2-world (w state))
             '((EQUAL (BAD X)
                      (IF (CONSP X)
                          (IF (INTEGERP (BAD (CDR X)))
                              (IF (< (BAD (CDR X)) '0)
                                  (F1 X)
                                  (IF (< (BAD (CDR X)) (ACL2-COUNT X))
                                      (BAD (BAD (CDR X)))
                                      (F1 X)))
                              (F1 X))
                          '0))))
      (equal
       (getprop 'bad 'constraint-lst nil 'current-acl2-world (w state))
       'f1)
      (equal
       (getprop 'bad 'induction-machine nil 'current-acl2-world (w state))
       nil)))

(u)



; Here is a sample involving defchoose.  In this example, the signature
; function is ancestral in the defchoose axiom.

(encapsulate ((p (y x) t))
             (local (defun p (y x) (member-equal y x)))
             (defchoose witless x (y) (p y x))
             (defthm consp-witless
               (consp (witless y))
               :rule-classes :type-prescription
               :hints (("Goal" :use (:instance witless (x (cons y nil)))))))

(test
 (and (equal (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
             '((IMPLIES (P Y X)
                        ((LAMBDA (X Y) (P Y X)) (WITLESS Y) Y))
               (CONSP (WITLESS Y))))
      (equal
       (getprop 'witless 'constraint-lst nil 'current-acl2-world (w state))
       'p)
      (equal
       (getprop 'witless 'defchoose-axiom nil 'current-acl2-world (w state))
       '(IMPLIES (P Y X)
                 ((LAMBDA (X Y) (P Y X)) (WITLESS Y) Y)))))

(u)

; and in this one it is not, indeed, the defchoose function can be
; moved to the [Front] even though it is used in the constraint of p.

(encapsulate ((p (y x) t))
             (local (defun p (y x) (member-equal y x)))
             (defchoose witless x (y) (member-equal y x))
             (defthm p-constraint (p y (witless y))
               :hints (("Goal" :use (:instance witless (x (cons y nil)))))))

(test
 (and (equal (getprop 'p 'constraint-lst nil 'current-acl2-world (w state))
             '((p y (witless y))))
      (equal
       (getprop 'witless 'constraint-lst nil 'current-acl2-world (w state))
       nil)
      (equal
       (getprop 'witless 'defchoose-axiom nil 'current-acl2-world (w state))
       '(IMPLIES (member-equal Y X)
                 ((LAMBDA (X Y) (member-equal Y X)) (WITLESS Y) Y)))))

(u)

(quote (the end of my encapsulate tests -- there follow two undo commands))
(u)
(u)

|#

(defun tilde-@-abbreviate-object-phrase (x)

; This function produces a tilde-@ phrase that describes the
; object x, especially if it is a list.  This is just a hack
; used in error reporting.

  (cond ((atom x) (msg "~x0" x))
        ((symbol-listp x)
         (cond ((< (length x) 3)
                (msg "~x0" x))
               (t
                (msg "(~x0 ... ~x1)"
                     (car x)
                     (car (last x))))))
        ((atom (car x))
         (cond ((and (consp (cdr x))
                     (atom (cadr x)))
                (msg "(~x0 ~x1 ...)"
                     (car x)
                     (cadr x)))
               (t
                (msg "(~x0 ...)"
                     (car x)))))
        ((atom (caar x))
         (cond ((and (consp (cdar x))
                     (atom (cadar x)))
                (msg "((~x0 ~x1 ...) ...)"
                     (caar x)
                     (cadar x)))
               (t
                (msg "((~x0 ...) ...)"
                     (caar x)))))
        (t "(((...) ...) ...)")))

(defun encapsulate-ctx (signatures form-lst)

; This function invents a suitable error context, ctx, for an
; encapsulate with the given signatures and form-lst.  The args have
; not been translated or checked.  Thus, this function is rough.
; However, we have to have some way to describe to the user which
; encapsulation is causing the problem, since we envision them often
; being nested.  Our guess is that the signatures, if non-nil, will be
; the most recognizable aspect of the encapsulate.  Otherwise, we'll
; abbreviate the form-lst.

  (cond
   (signatures
    (cond ((and (consp signatures)
                (consp (car signatures))
                (consp (caar signatures)))
           (msg "( ENCAPSULATE (~@0 ...) ...)"
                (tilde-@-abbreviate-object-phrase (car signatures))))
          (t
           (msg "( ENCAPSULATE ~@0 ...)"
                (tilde-@-abbreviate-object-phrase signatures)))))
   (form-lst
    (msg "( ENCAPSULATE NIL ~@0 ...)"
         (tilde-@-abbreviate-object-phrase (car form-lst))))
   (t "( ENCAPSULATE NIL)")))

(defun print-encapsulate-msg1 (insigs form-lst state)
  (declare (ignore insigs))
  (cond
   ((ld-skip-proofsp state) state)
   (t
    (io? event nil state
         (form-lst)
         (fms "To verify that the ~#0~[~/~n1 ~]encapsulated event~#0~[~/s~] ~
               correctly extend~#0~[s~/~] the current theory we will evaluate ~
               ~#0~[it~/them~].  The theory thus constructed is only ~
               ephemeral.~|~#2~[~%Encapsulated Event~#0~[~/s~]:~%~/~]"
              (list (cons #\0 form-lst)
                    (cons #\1 (length form-lst))
                    (cons #\2 (if (eq (ld-pre-eval-print state) :never) 1 0)))
              (proofs-co state)
              state nil)))))

(defun print-encapsulate-msg2 (insigs form-lst state)
  (declare (ignore insigs))
  (cond
   ((ld-skip-proofsp state) state)
   (t
    (io? event nil state
         (form-lst)
         (fms "End of Encapsulated Event~#0~[~/s~].~%"
              (list (cons #\0 form-lst))
              (proofs-co state)
              state nil)))))

(defun print-encapsulate-msg3/exported-names (insigs lst)

; This returns a list of tilde-@ phrases.  The list always has either
; 0 or 1 things in it.  The single element describes the exports of
; an encapsulation (if any).  Insigs is the list of internal form
; signatures of the constrained fns.

  (cond ((null lst)

; Say nothing if there are no additional names.

         nil)
        (insigs
         (list (msg "In addition to ~&0, we export ~&1.~|~%"
                    (strip-cars insigs)
                    lst)))
        (t (list (msg "We export ~&0.~|~%"
                      lst)))))

(defun print-encapsulate-msg3/constraints (constrained-fns constraints wrld)
  (cond
   ((null constraints)

; It's tempting in this case to say something like, "No new constraints are
; associated with any function symbols."  However, one could argue with that
; statement, since DEFUN introduces constraints in some sense, for example.
; This problem does not come up if there are constrained functions, since in
; that case (below), we are honestly reporting all of the constraints on the
; indicated functions.  So, we simply print nothing in the present case.

    nil)
   ((null constrained-fns)
    (er hard 'print-encapsulate-msg3/constraints
        "We had thought that the only way that there can be constraints is if ~
         there are constrained functions.  See ~
         print-encapsulate-msg3/constraints."))
   (t (list
       (msg "The following constraint is associated with ~#0~[the ~
             function~/both of the functions~/every one of the functions~] ~
             ~&1:~|~%~p2~|"
            (let ((n (length constrained-fns)))
              (case n
                    (1 0)
                    (2 1)
                    (otherwise 2)))
            constrained-fns
            (untranslate (conjoin constraints) t wrld))))))

(defun print-encapsulate-msg3 (ctx insigs form-lst exported-names
                                   constrained-fns constraints-introduced
                                   subversive-fns infectious-fns
                                   wrld state)

; This function prints a sequence of paragraphs, one devoted to each
; constrained function (its arities and constraint) and one devoted to
; a summary of the other names created by the encapsulation.

  (cond
   ((ld-skip-proofsp state) state)
   (t
    (io? event nil state
         (infectious-fns ctx subversive-fns wrld constraints-introduced
                         constrained-fns exported-names insigs form-lst)
         (pprogn
          (fms "Having verified that the encapsulated event~#0~[ ~
                validates~/s validate~] the signatures of the ~
                ENCAPSULATE event, we discard the ephemeral theory ~
                and extend the original theory as directed by the ~
                signatures and the non-LOCAL events.~|~%~*1"
               (list
                (cons #\0 form-lst)
                (cons #\1
                      (list "" "~@*" "~@*" "~@*"
                            (append
                             (print-encapsulate-msg3/exported-names
                              insigs exported-names)
                             (print-encapsulate-msg3/constraints
                              constrained-fns constraints-introduced wrld)
                             ))))
               (proofs-co state)
               state nil)
          (print-defun-msg/signatures (strip-cars insigs) wrld state)
          (if subversive-fns
              (warning$ ctx "Infected"
                        "Note that ~&0 ~#0~[is~/are~] ``subversive.'' ~
                         See :DOC subversive-recursions.  Thus, ~
                         ~#0~[its definitional equation ~
                         infects~/their definitional equations ~
                         infect~] the constraint of this ~
                         en~-cap~-su~-la~-tion.  Furthermore, ~#0~[this ~
                         function~/these functions~] will not suggest ~
                         any induction schemes to the theorem prover. ~
                          If possible, you should remove ~#0~[this ~
                         definition~/these definitions~] from the ~
                         encapsulate and introduce ~#0~[it~/them~] ~
                         afterwards.  A constraint containing a ~
                         definitional equation is often hard to use in ~
                         subsequent functional instantiations."
                        subversive-fns)
            state)
          (if infectious-fns
              (warning$ ctx "Infected"
                        "Note that the definitional ~
                         equation~#0~[~/s~] for ~&0 infect~#0~[s~/~] ~
                         the constraint of this ~
                         en~-cap~-su~-la~-tion.  That can be caused ~
                         either because we are not allowed to move a ~
                         defun out of nested non-trivial encapsulates ~
                         or because a function ancestrally involves ~
                         the constrained functions of an encapsulate ~
                         and is ancestrally involved in the ~
                         constraining theorems of those functions. In ~
                         any case, if at all possible, you should ~
                         move ~#0~[this definition~/these ~
                         definitions~] out of the encapsulation.  A ~
                         constraint containing a definitional ~
                         equation is often hard to use in subsequent ~
                         functional instantiations.  See ~
                         :DOC subversive-recursions for a discussion of ~
                         related issues."
                        infectious-fns)
            state))))))

(defun redundant-event-tuplep (event-form mode wrld)

; We return t iff the non-prehistoric (if that's where we start) part of wrld
; contains an event-tuple whose form is equal to event-form.

  (cond ((or (null wrld)
             (and (eq (caar wrld) 'command-landmark)
                  (eq (cadar wrld) 'global-value)
                  (equal (access-command-tuple-form (cddar wrld))
                         '(exit-boot-strap-mode))))
         nil)
        ((and (eq (caar wrld) 'event-landmark)
              (eq (cadar wrld) 'global-value)
              (equal (access-event-tuple-form (cddar wrld))
                     event-form))
         (eq (default-defun-mode wrld) mode))
        (t (redundant-event-tuplep event-form mode (cdr wrld)))))

(mutual-recursion

(defun find-first-non-local-name (x)

; X is allegedly an embedded event form.  It may be a call of some
; user macro and thus completely unrecognizable to us.  But it could
; be a call of one of our primitive fns.  We are interested in the
; question "If x is successfully executed, what is a logical name it
; will introduce?"  Since no user event will introduce nil, we use nil
; to indicate that we don't know about x (or, equivalently, that it is
; some user form we don't recognizer, or that it introduces no names,
; or that it is ill-formed and will blow up).  Otherwise, we return a
; logical name that x will create.

  (case-match x
              (('local . &) nil)
              (('defun name . &) name)
              (('defuns (name . &) . &) name)
              (('defthm name . &) name)
              (('defaxiom name . &) name)
              (('skip-proofs ev) (find-first-non-local-name ev))
              (('defconst name . &) name)
              (('deflabel name . &) name)
              (('deftheory name . &) name)
              (('defstobj name . &) name)
              (('defmacro name . &) name)
              (('mutual-recursion ('defun name . &) . &) name)
              (('encapsulate (((name . &) arrow &) . &) . &)
               (and (symbolp arrow)
                    (equal (symbol-name arrow) "=>")
                    name))
              (('encapsulate ((name . &) . &) . &) name)
              (('encapsulate nil . ev-lst)
               (find-first-non-local-name-lst ev-lst))
              (('include-book name . &) name)
              (& nil)))               

(defun find-first-non-local-name-lst (lst)

; Challenge: If lst is a true list of embedded event forms that is
; successfully processed with ld-skip-proofsp nil, name one name that
; would be created.  Now lst might not be a list of embedded event
; forms.  Or the forms might be doomed to cause errors or might be
; unrecognizable user macro calls.  So we return nil if we can't spot a
; suitable name.  Otherwise we return a name.  The only claim made is
; this: if we return non-nil and lst were successfully processed, then
; that name is a logical name that would be created.  Consequently, if
; that name is new in a world, we know that this lst has not been
; processed before.

  (cond ((atom lst) nil)
        (t (or (find-first-non-local-name (car lst))
               (find-first-non-local-name-lst (cdr lst))))))

)

(defun corresponding-encap-events (old-evs new-evs ans)
  (cond
   ((endp old-evs)
    (and (null new-evs)
         ans))
   ((endp new-evs)
    nil)
   (t (let ((old-ev (car old-evs))
            (new-ev (car new-evs)))
        (cond ((equal old-ev new-ev)
               (corresponding-encap-events (cdr old-evs) (cdr new-evs) ans))
              ((and (eq (car old-ev) 'record-expansion)
                    (equal (cadr old-ev) new-ev))
               (corresponding-encap-events (cdr old-evs) (cdr new-evs) :expanded))
              (t nil))))))

(defun corresponding-encaps (old new)
  (assert$
   (eq (car new) 'encapsulate)
   (and (eq (car old) 'encapsulate)
        (true-listp new)
        (equal (cadr old) (cadr new))
        (corresponding-encap-events (cddr old) (cddr new) t))))

(defun redundant-encapsulate-tuplep (event-form mode wrld)

; We return non-nil iff the non-prehistoric (if that's where we start) part of wrld
; contains an event-tuple whose form is essentially equal to event-form.  We
; return t if they are equal, else we return the old form.  See also the Essay
; on Make-event.

  (cond ((or (null wrld)
             (and (eq (caar wrld) 'command-landmark)
                  (eq (cadar wrld) 'global-value)
                  (equal (access-command-tuple-form (cddar wrld))
                         '(exit-boot-strap-mode))))
         nil)
        ((and (eq (caar wrld) 'event-landmark)
              (eq (cadar wrld) 'global-value)
              (let* ((old-event-form (access-event-tuple-form (cddar wrld)))
                     (equal?
                      (and (eq (car old-event-form) 'encapsulate)
                           (corresponding-encaps old-event-form event-form))))
                (and equal?
                     (eq (default-defun-mode wrld) mode)
                     (if (eq equal? :expanded)
                         old-event-form
                       t)))))
        (t (redundant-encapsulate-tuplep event-form mode (cdr wrld)))))

(defun redundant-encapsulatep (signatures ev-lst event-form wrld)

; We wish to know whether (redundant-event-tuplep event-form wrld) is t or nil.
; That is, is there an event-tuple in wrld that has event-form as its form?  We
; know, however, that event-form is really an encapsulate with the given two
; arguments.  We don't know if event-form will execute without error -- i.e.,
; the args may be screwed up.  But suppose we could find a name among
; signatures and ev-lst that is guaranteed to be created if event-form were
; successful.  Then if that name is new, we know we won't find event-form in
; wrld and needn't bother looking.  If the name is old and was introduced by an
; corresponding encapsulate (in the sense that the signatures agree and each
; form of the new encapsulate either equals the corresponding form of the old
; encapsulate or else does so before expansion of the old form), then the event
; is redundant.  Otherwise, if this correspondence test fails or if we can't
; even find a name -- e.g., because signatures is nil and all the events in
; ev-lst are user macros -- then we suffer the search through wrld.  How bad is
; this?  We expect most encapsulates to have a readily recognized name among
; their new args and most encapsulates are not redundant, so we think most of
; the time, we'll find a name and it will be new.

; If we find that the current encapsulate is redundant, then we return t unless
; the earlier corresponding encapsulate is not equal to it, in which case we
; return that earlier encapsulate, which is stored in expanded form.  See also
; the Essay on Make-event.  Otherwise we return nil.

  (let ((name
         (find-first-non-local-name
          (list* 'encapsulate signatures ev-lst))))
    (cond ((and name
                (stringp name)
                (not (find-non-hidden-package-entry
                      name
                      (global-val 'known-package-alist wrld)))
                (not (assoc-equal name (global-val 'include-book-alist wrld))))

; If the name we find is a string then it can only be a full-book-name, e.g.,
; the first non-local event in the encapsulate was an include-book.  However,
; just to remind us that stringp names can be package names we look there too,
; even though a defpkg couldn't occur in an encapsulate.  Note that if we do
; not find the name in the 'include-book-alist or the 'known-package-alist
; (non-hidden; see the Essay on Hidden Packages) then this encapsulate could
; not have been executed so it is not redundant.

; It actually is tempting to cause an error here, since we believe that both
; defpkg and non-local include-book forms are impossible.  But we haven't
; checked that we have a legal embedded event form, so a string is possible;
; consider for example (encapsulate () (defun "foo" (x) t)).

           nil)
          ((and name
                (symbolp name)
                (new-namep name wrld))
           nil)
          (t
           (or (and name
                    (symbolp name)
                    (let* ((wrld-tail (lookup-world-index
                                       'event
                                       (getprop name 'absolute-event-number 0
                                                'current-acl2-world wrld)
                                       wrld))
                           (event-tuple (cddr (car wrld-tail)))
                           (old-event-form (access-event-tuple-form
                                            event-tuple))
                           (equal? (corresponding-encaps old-event-form
                                                         event-form)))
                      (and equal?
                           (eq (default-defun-mode wrld-tail)
                               (default-defun-mode wrld))
                           (if (eq equal? :expanded)
                               old-event-form
                             t))))
               (redundant-encapsulate-tuplep event-form

; We could probably replace the two uses of wrld with wrld-tail below.  But
; default-defun-mode ultimately invokes getprop, so it's probably preferable to
; use wrld since it is installed (or a small extension of an installed world).

                                             (default-defun-mode wrld)
                                             wrld))))))

(defun mark-missing-as-hidden-p (a1 a2)

; A1 and a2 are known-package-alists.  Return the result of marking each
; package-entry in a1 that is missing in a2 with hidden-p equal to t.

  (cond ((endp a1) nil)
        ((or (find-package-entry (package-entry-name (car a1)) a2)
             (package-entry-hidden-p (car a1)))
         (cons (car a1)
               (mark-missing-as-hidden-p (cdr a1) a2)))
        (t (cons (change-package-entry-hidden-p (car a1) t)
                 (mark-missing-as-hidden-p (cdr a1) a2)))))

(defun known-package-alist-included-p (a1 a2)

; Return true if every package-entry in a1 is present in a2, and moveover, is
; present non-hidden in a2 if present non-hidden in a1.

  (cond ((endp a1) t)
        (t (and (let ((a2-entry (find-package-entry
                                 (package-entry-name (car a1)) a2)))
                  (and a2-entry
                       (or (package-entry-hidden-p (car a1))
                           (not (package-entry-hidden-p a2-entry)))))
                (known-package-alist-included-p (cdr a1) a2)))))

(defun encapsulate-fix-known-package-alist (pass1-k-p-alist wrld)

; Pass1-k-p-alist is the known-package-alist from the end of the first pass of
; an encapsulate, and we are now at the end of the second pass in the given
; world, wrld.  The known-package-alist of wrld may be missing some
; package-entries from pass1-k-p-alist because of defpkg events that were only
; executed under locally included books in the first pass.  We return the
; result of setting the known-package-alist of the given world by marking each
; package-entry in pass1-k-p-alist that is missing in the current world's
; known-package-alist with hidden-p equal to t.

; The call of known-package-alist-included-p below checks that the second pass
; does not introduce any packages beyond those introduced in the first pass,
; nor does the second pass "promote" any package to non-hidden that was hidden
; in the first pass.  We rely on this fact in order to use the
; known-package-alist from the first pass as a basis for the alist returned, so
; that any package-entry present in the second pass's alist is present in the
; result alist, and moveover is non-hidden in the result if non-hidden in the
; second pass's alist.

; In fact we believe that the known-package-alist at the end of the second pass
; of an encapsulate is the same as at the beginning of the encapsulate, since
; local events are all skipped and include-books are all local.  However, we do
; not rely on this belief.

  (let ((pass2-k-p-alist (global-val 'known-package-alist wrld)))
    (cond ((equal pass1-k-p-alist pass2-k-p-alist) ; optimize for a common case
           wrld)
          (t (assert$
              (known-package-alist-included-p pass2-k-p-alist pass1-k-p-alist)
              (global-set 'known-package-alist
                          (mark-missing-as-hidden-p pass1-k-p-alist
                                                    pass2-k-p-alist)
                          wrld))))))

(defun subst-by-position1 (alist lst index acc)

; See the comment in subst-by-position.

  (cond ((endp alist)
         (revappend acc lst))
        ((endp lst)
         (cond ((endp alist) nil)
               (t
                (er hard 'subst-by-position1
                    "Implementation error: lst is an atom, so unable to ~
                     complete call ~x0."
                    `(subst-by-position1 ,alist ,lst ,index ,acc)))))
        ((eql index (caar alist))
         (subst-by-position1 (cdr alist) (cdr lst) (1+ index)
                             (cons (cdar alist) acc)))
        (t
         (subst-by-position1 alist (cdr lst) (1+ index)
                             (cons (car lst) acc)))))

(defun subst-by-position (alist lst index)

; Alist associates index-based positions in lst with values.  We
; return the result of replacing each element of lst with its corresponding
; value from alist.  Alist should have indices in increasing order and should 
; only have indices i for which index+i is less than the length of lst.

  (cond (alist
         (cond ((< (caar alist) index)
                (er hard 'subst-by-position
                    "Implementation error: The alist in subst-by-position ~
                       must not start with an index less than its index ~
                       argument, so unable to compute ~x0."
                    `(subst-by-position ,alist ,lst ,index)))
               (t (subst-by-position1 alist lst index nil))))
        (t ; optimize for common case
         lst)))

(defun encapsulate-fn (signatures ev-lst state event-form)

; Important Note:  Don't change the formals of this function without reading
; the *initial-event-defmacros* discussion in axioms.lisp.

; The Encapsulate Essay

; The motivation behind this event is to permit one to extend the theory by
; introducing function symbols, and theorems that describe their properties,
; without completely tying down the functions or including all of the lemmas
; and other hacks necessary to lead the system to the proofs.  Thus, this
; mechanism replaces the CONSTRAIN event of Nqthm.  It also offers one way of
; getting some name control, comparable to scopes.  However, it is better than
; just name control because the "hidden" rules are not just apparently hidden,
; they simply don't exist.

; Encapsulate takes two main arguments.  The first is a list of
; "signatures" that describe the function symbols to be hidden.  By
; signature we mean the formals, stobjs-in and stobjs-out of the
; function symbol.  The second is a list of events to execute.  Some
; of these events are tagged as "local" events and the others are not.
; Technically, each element of ev-lst is either an "event form" or
; else an s-expression of the form (LOCAL ev), where ev is an "event
; form."  The events of the second form are the local events.
; Informally, the local events are present only so that we can justify
; (i.e., successfully prove) the non-local events.  The local events
; are not visible in the final world constructed by an encapsulation.

; Suppose we execute an encapsulation starting with ld-skip-proofsp nil in
; wrld1.  We will actually make two passes through the list of events.  The
; first pass will execute each event, proving things, whether it is local or
; not.  This will produce wrld2.  In wrld2, we check that every function symbol
; in signatures is defined and has the signature alleged.  Then we back up to
; wrld1, declare the hidden functions with the appropriate signatures
; (producing what we call proto-wrld3) and replay only the non-local events.
; (Note: if redefinitions are allowed and are being handled by query, the user
; will be presented with two queries for each redefining non-local event.
; There is no assurance that he answers the same way both times and different
; worlds may result.  C'est la vie avec redefinitions.)  During this replay we
; skip proofs.  Having constructed that world we then collect all of the
; theorems that mention any of the newly-introduced functions and consider the
; resulting list as the constraint for all those functions.  (This is a
; departure from an earlier, unsound implementation, in which we only collected
; theorems mentioning the functions declared in the signature.)  However, we
; "optimize" by constructing this list of theorems using only those
; newly-introduced functions that have as an ancestor at least one function
; declared in the signature.  In particular, we do not introduce any
; constraints if the signature is empty, which is reasonable since in that
; case, we may view the encapsulate event the same as we view a book.  At any
; rate, the world we obtain by noting this constraint on the appropriate
; functions is called wrld3, and it is the world produced by a successful
; encapsulation.  By putting enough checks on the kinds of events executed we
; can guarantee that the formulas assumed to create wrld3 from wrld1 are
; theorems that were proved about defined functions in wrld2.

; This is a non-trivial claim and will be the focus of much of our discussion
; below.  This discussion could be eliminated if the second pass consisted of
; merely adding to wrld1 the formulas of the exported names, obtained from
; wrld2.  We do not do that because we want to be able to execute an
; encapsulation quickly if we process one while skipping proofs.  That is,
; suppose the user has produced a script of some session, including some
; encapsulations, and the whole thing has been processed with ld-skip-proofsp
; nil, once upon a time.  Now the user wants to assume that script and and
; continue -- i.e., he is loading a "book".

; Suppose we hit the encapsulation when skipping proofs.  Suppose we are
; again in wrld1 (i.e., processing the previous events of this script
; while skipping proofs has inductively left us in exactly the same
; state as when we did them with proofs).  We are given the event list
; and the signatures.  We want to do here exactly what we did in the
; second pass of the original proving execution of this encapsulate.
; Perhaps more informatively put, we want to do in the second pass of
; the proving execution exactly what we do here -- i.e., the relative
; paucity of information available here (we only have wrld1 and not
; wrld2) dictates how we must handle pass two back there.  Remember, our
; goal is to ensure that the final world we create, wrld3, is absolutely
; identical to that created above.

; Our main problem is that the event list is in untranslated form.
; Two questions arise.

; (1) If we skip an event because it is tagged LOCAL, how will we know
; we can execute (or even translate) the subsequent events without
; error?  For example, suppose one of the events skipped is the
; defmacro of deflemma, and then we see a (deflemma &).  We will have
; to make sure this doesn't happen.  The key here is that we know that
; the second pass of the proving execution of this encapsulate did
; whatever we're doing and it didn't cause an error.  But this is an
; important point about the proving execution of an encapsulate: even
; though we make a lot of checks before the first pass, it is possible
; for the second pass to fail.  When that happens, we'll revert back
; to wrld1 for sanity.  This is unfortunate because it means the user
; will have to suffer through the re-execution of his event list
; before seeing if he has fixed the last error.  We should eventually
; provide some sort of trial encapsulation mechanism so the user can
; see if he's got his signatures and exports correctly configured.

; (2) How do we know that the formulas generated during the second
; pass are exactly the same as those generated during the first pass?
; For example, one of the events might be:

; (if (ld-skip-proofsp state)
;     (defun foo () 3)
;     (defun foo () 2))

; In this case, (foo) would be 2 in wrld2 but 3 in wrld3.

; The key to the entire story is that we insist that the event list
; consist of certain kinds of events.  For lack of a better name, we
; call these "embedded event forms".  Not everything the user might
; want to type in an interactive ACL2 session is an embedded event
; form!  Roughly speaking, an event form translates to a PROGN of
; "primitive events", where the primitive events are appropriate calls
; of such user-level functions as defun and defthm.  By "appropriate"
; we mean STATE only appears where specified by the stobjs-in for each
; event.  The other arguments, e.g., the name of a defthm, must be
; occupied by state free terms -- well, almost.  We allow uses of w so
; that the user can compute things like gensyms wrt the world.  In a
; rough analogy with Lisp, the events are those kinds of commands that
; are treated specially when they are seen at the top-level of a file
; to be compiled.

; Events have the property that while they take state as an argument
; and change it, their changes to the world are a function only of the
; world (and their other arguments).  Because of this property, we
; know that if s1 and s1' are states containing the same world, and s2
; and s2' are the states obtained by executing an event on the two
; initial states, respectively, then the worlds of s2 and s2' are
; equal.

; Thus ends the encapsulate essay.

  (let ((ctx (encapsulate-ctx signatures ev-lst)))
    (with-ctx-summarized
     (if (output-in-infixp state) event-form ctx)
     (let* ((wrld1 (w state))
            (saved-acl2-defaults-table
             (table-alist 'acl2-defaults-table wrld1))
            (event-form (or event-form
                            (list* 'encapsulate signatures ev-lst))))
       (revert-world-on-error
        (let ((r (redundant-encapsulatep signatures ev-lst event-form wrld1)))
          (cond
           (r
            (pprogn
             (if (eq r t)
                 state
               (f-put-global 'last-make-event-expansion r state))
             (stop-redundant-event state)))
           ((and (not (eq (ld-skip-proofsp state) 'include-book))
                 (not (eq (ld-skip-proofsp state) 'include-book-with-locals))
                 (not (eq (ld-skip-proofsp state) 'initialize-acl2)))

; Ld-skip-proofsp is either t or nil.  But whatever it is, we will be
; processing the LOCAL events.

            (er-let*
             ((pair (chk-acceptable-encapsulate1 signatures ev-lst
                                                 ctx wrld1 state)))
             (let ((insigs (car pair))
                   (wrld1 (cdr pair)))
               (pprogn
                (set-w 'extension wrld1 state)
                (print-encapsulate-msg1 insigs ev-lst state)
                (er-let*
                 ((expansion-alist
                   (state-global-let*
                    ((in-local-flg

; As we start processing the events in the encapsulate, we are no longer in the
; lexical scope of LOCAL for purposes of disallowing setting of the
; acl2-defaults-table.

                      (and (f-get-global 'in-local-flg state)
                           'dynamic)))
                    (process-embedded-events
                     'encapsulate-pass-1
                     saved-acl2-defaults-table
                     (ld-skip-proofsp state)
                     (current-package state)
                     (list 'encapsulate insigs)
                     ev-lst 0 nil ctx state))))
                 (let* ((wrld2 (w state))
                        (post-pass-1-ttags-seen (global-val 'ttags-seen
                                                            wrld2)))
                   (pprogn
                    (print-encapsulate-msg2 insigs ev-lst state)
                    (er-progn
                     (chk-acceptable-encapsulate2 insigs wrld2 ctx state)
                     (let* ((pass1-known-package-alist
                             (global-val 'known-package-alist wrld2))
                            (new-ev-lst
                             (subst-by-position expansion-alist ev-lst 0))
                            (state (set-w 'retraction wrld1 state)))
                       (er-let*
                        ((temp

; The following encapsulate-pass-2 is protected by the revert-world-on
; error above.
                          (encapsulate-pass-2
                           insigs
                           new-ev-lst
                           saved-acl2-defaults-table nil ctx state)))
                        (let ((wrld3 (w state))
                              (constrained-fns (nth 0 temp))
                              (constraints-introduced (nth 1 temp))
                              (exports (nth 2 temp))
                              (subversive-fns (nth 3 temp))
                              (infectious-fns (nth 4 temp))
                              (new-event-form
                               (and expansion-alist
                                    (list* 'encapsulate signatures
                                           new-ev-lst))))
                          (pprogn
                           (print-encapsulate-msg3
                            ctx insigs new-ev-lst exports
                            constrained-fns constraints-introduced
                            subversive-fns infectious-fns wrld3 state)
                           (f-put-global 'last-make-event-expansion
                                         new-event-form
                                         state)
                           (install-event
                            t
                            (or new-event-form event-form)
                            'encapsulate
                            (strip-cars insigs)
                            nil nil
                            t
                            ctx
                            (let ((wrld4 (encapsulate-fix-known-package-alist
                                          pass1-known-package-alist
                                          wrld3)))
                              (global-set? 'ttags-seen
                                           post-pass-1-ttags-seen
                                           wrld4
                                           (global-val 'ttags-seen wrld3)))
                            state)))))))))))))

           (t ; (ld-skip-proofsp state) = 'include-book
          ;;;                         'include-book-with-locals or
          ;;;                         'initialize-acl2

; We quietly execute our second pass.

            (er-let*
             ((pair (chk-signatures signatures ctx wrld1 state)))
             (let ((insigs (car pair))
                   (wrld1 (cdr pair)))
               (pprogn
                (set-w 'extension wrld1 state)
                (er-let*

; The following encapsulate-pass-2 is protected by the revert-world-on
; error above.

                 ((expansion-alist
                   (encapsulate-pass-2
                    insigs ev-lst saved-acl2-defaults-table t ctx state)))
                 (let ((wrld3 (w state))
                       (new-event-form
                        (and expansion-alist
                             (list* 'encapsulate signatures
                                    (subst-by-position expansion-alist
                                                       ev-lst
                                                       0)))))
                   (pprogn
                    (f-put-global 'last-make-event-expansion
                                  new-event-form
                                  state)
                    (install-event t
                                   (if expansion-alist
                                       new-event-form
                                     event-form)
                                   'encapsulate
                                   (signature-fns signatures)
                                   nil nil
                                   nil ; irrelevant, since we are skipping proofs
                                   ctx

; We have considered calling encapsulate-fix-known-package-alist on wrld3, just
; as we do in the first case (when not doing this on behalf of include-book).
; But we do not see a need to do so, both because all include-books are local
; and hence skipped (hence the known-package-alist has not changed from before
; the encapsulate), and because we do not rely on tracking packages during
; include-book, :puff (where ld-skip-proofsp is include-book-with-locals), or
; initialization.

                                   wrld3
                                   state)))))))))))))))

(defun progn-fn1 (ev-lst progn!p state)

; Important Note:  Don't change the formals of this function without reading
; the *initial-event-defmacros* discussion in axioms.lisp.

  (let ((ctx (cond (ev-lst
                    (msg "( PROGN~s0 ~@1 ...)"
                         (if progn!p "!" "")
                         (tilde-@-abbreviate-object-phrase (car ev-lst))))
                   (t (if progn!p "( PROGN~)" "( PROGN)"))))
        (in-encapsulatep
         (in-encapsulatep (global-val 'embedded-event-lst (w state)) nil)))
    (with-ctx-summarized
     ctx
     (revert-world-on-error
      (mv-let
       (erp val expansion-alist state)
       (pprogn
        (f-put-global 'redo-flat-succ nil state)
        (f-put-global 'redo-flat-fail nil state)
        (eval-event-lst
         0 nil
         ev-lst
         t ; quietp
         in-encapsulatep
         (f-get-global 'in-local-flg state)
         nil
         (if progn!p
             :non-event-ok

; It is unknown here whether make-event must have a consp :check-expansion, but
; if this progn is in such a context, chk-embedded-event-form will check that
; for us.

           nil)
         ctx (proofs-co state) state))
       (pprogn
        (if erp
            (update-for-redo-flat val ev-lst state)
          state)
        (cond ((eq erp 'non-event)
               (er soft ctx
                   "PROGN may only be used on legal event forms (see :DOC ~
                   embedded-event-form).  Consider using ER-PROGN instead."))
              (erp (er soft ctx
                       "~x0 failed!~@1"
                       (if progn!p 'progn! 'progn)
                       (if (and progn!p
                                (consp erp))
                           (msg "  Note that the ~n0 form evaluated to a ~
                                 multiple value (mv erp ...) with non-nil ~
                                 erp, ~X12; see :DOC progn!."
                                (list (1+ val))
                                (car erp)
                                (default-evisc-tuple state))
                         "")))
              (t (pprogn (f-put-global 'last-make-event-expansion
                                       (and expansion-alist
                                            (cons (if progn!p 'progn! 'progn)
                                                  (subst-by-position
                                                   expansion-alist
                                                   ev-lst
                                                   0)))
                                       state)
                         (value val))))))))))

(defun progn-fn (ev-lst state)
  (progn-fn1 ev-lst nil state))

(defun progn!-fn (ev-lst state)
  (state-global-let* ((acl2-raw-mode-p (f-get-global 'acl2-raw-mode-p state)))
                     (progn-fn1 ev-lst t state)))

(defun make-event-ctx (event-form)
  (msg "( MAKE-EVENT ~@0~@1)"
       (tilde-@-abbreviate-object-phrase (cadr event-form))
       (if (cddr event-form) " ..." "")))

(defun state-global-bindings (names)
  (cond ((endp names)
         nil)
        (t (cons `(,(car names) (f-get-global ',(car names) state))
                 (state-global-bindings (cdr names))))))

(defconst *initial-ld-special-bindings*

; This alist is used by initialize-acl2 to set the initial values of the LD
; specials.  It is assumed by reset-ld-specials that the first three are the
; channels.

  `((standard-oi . ,*standard-oi*)
    (standard-co . ,*standard-co*)
    (proofs-co . ,*standard-co*)
    (ld-skip-proofsp . nil)
    (ld-redefinition-action . nil)
    (ld-prompt . t)
    (ld-keyword-aliases . nil)
    (ld-pre-eval-filter . :all)
    (ld-pre-eval-print . nil)
    (ld-post-eval-print . :command-conventions)
    (ld-evisc-tuple . nil)
    (ld-error-triples . t)
    (ld-error-action . :continue)
    (ld-query-control-alist . nil)
    (ld-verbose . "~sv.  Level ~Fl.  Cbd ~xc.~%Type :help for help.~%~
                   Type (good-bye) to quit completely out of ACL2.~%~%")))

(defconst *protected-state-globals-for-make-event*
  (let ((val
         (set-difference-eq
          (union-eq (strip-cars *initial-ld-special-bindings*)
                    (strip-cars *initial-global-table*))
          '(acl2-raw-mode-p            ;;; keep raw mode status
            certify-book-disabledp     ;;; preserve disable of certify-book
            bddnotes                   ;;; for feedback after expansion failure

; We handle world and enabled structure installation ourselves, with set-w! and
; revert-world-on-error.  We do not want to rely just on state globals because
; the world protection/modification functions do pretty fancy things.

            current-acl2-world global-enabled-structure
            keep-tmp-files             ;;; allow user to modify this in a book
            make-event-debug           ;;; allow user to modify this in a book
            packages-created-by-defpkg ;;; keep around traces of defpkg forms
            saved-output-reversed      ;;; for feedback after expansion failure
            saved-output-p             ;;; for feedback after expansion failure
            ttags-allowed              ;;; propagate changes outside expansion

; Note that tainted-okp is deliberately omitted from this list of exceptions,
; since its global value is the one that should be used during event
; processing.

            ))))
    (assert$

; We rely on the protection of ld-skip-proofsp in the definition of
; protected-eval-with-proofs.

     (member-eq 'ld-skip-proofsp val)
     val)))

(defmacro protect-state-globals-for-make-event (form)
  `(state-global-let*
    ,(state-global-bindings *protected-state-globals-for-make-event*)
    ,form))

(defun protected-eval-with-proofs (form on-behalf-of ctx state)

; We assume that this is executed under a revert-world-on-error, so that we do
; not have to protect the world here.  Form should evaluate either to an
; ordinary value, val, or to (mv nil val state stobj1 ... stobjk), where k may
; be 0.  If so, we return (value val), and if not, we return a soft error.

  (let ((original-wrld (w state)))
    (protect-state-globals-for-make-event
     (pprogn ; setting 'ld-skip-proofsp is protected by protect-xxx just above
      (f-put-global 'ld-skip-proofsp nil state)
      (er-let*
       ((result

; It would be nice to add
; (state-global-let* ((safe-mode t))
; here.  But some *1* functions need always to call their raw Lisp
; counterparts.  Although we have made progress in oneify-cltl-code to that end
; by keeping functions like certify-book-fn from being replaced by their *1*
; counterparts, still that process is not complete, so we play it safe here by
; avoiding safe-mode.

; If we bind safe-mode to t here, visit occurrences of comments "; Note that
; safe-mode for make-event will require addition".  Those comments are
; associated with membership tests that, for now, we avoid for efficiency.

         (trans-eval form ctx state)))
       (let* ((stobjs-out (car result))
              (vals (cdr result))
              (safep (equal stobjs-out '(nil))))
         (cond (safep (value vals))
               ((or (null (cdr stobjs-out))
                    (not (eq (caddr stobjs-out) 'state))
                    (member-eq nil (cdddr stobjs-out)))
                (er soft ctx
                    "The expansion of a make-event form must either return a ~
                     single ordinary value or else should return a tuple (mv ~
                     erp val state stobj1 stobj2 ... stobjk) for some k >= ~
                     0.  But the shape of ~x0 is ~x1."
                    form
                    (prettyify-stobjs-out stobjs-out)))
               ((stringp (car vals))
                (er soft ctx
                    (car vals)))
               ((and (true-listp (car vals))
                     (stringp (caar vals))) ; a message
                (er soft ctx
                    "~@0"
                    (car vals)))
               ((car vals)
                (er soft ctx
                    "Error in MAKE-EVENT ~@0from expansion of:~|  ~Y12"
                    (cond (on-behalf-of
                           (msg "on behalf of~|  ~Y01~|"
                                on-behalf-of
                                nil))
                          (t ""))
                    form
                    nil))
               (t (pprogn (set-w! original-wrld state)
                          (value (cadr vals)))))))))))

(defun make-event-debug-pre (form on-behalf-of state)
  (cond
   ((null (f-get-global 'make-event-debug state))
    (value nil))
   (t
    (let ((depth (f-get-global 'make-event-debug-depth state)))
      (pprogn (fms "~x0> Expanding for MAKE-EVENT~@1~|  ~y2~|"
                   (list (cons #\0 depth)
                         (cons #\1 (if on-behalf-of
                                       (msg " on behalf of~|  ~y0:"
                                            on-behalf-of)
                                     ":"))
                         (cons #\2 form))
                   (proofs-co state) state nil)
              (value depth))))))

(defun make-event-debug-post (debug-depth expansion0 state)
  (cond ((null debug-depth) state)
        (t
         (fms "<~x0 Returning MAKE-EVENT expansion:~|  ~y1~|"
              (list (cons #\0 debug-depth)
                    (cons #\1 expansion0))
              (proofs-co state) state nil))))

(defmacro do-proofs? (do-proofsp form)
  `(if ,do-proofsp
       (state-global-let*
        ((ld-skip-proofsp nil))
        ,form)
     ,form))

(defun make-event-fn (form check-expansion on-behalf-of whole-form ctx state)
  (cond
   ((not (or (eq check-expansion nil)
             (eq check-expansion t)
             (consp check-expansion)))
    (er soft ctx
        "The check-expansion flag of make-event must be t, nil, or a cons ~
         pair.  The following check-expansion flag is thus illegal: ~x0.  See ~
         :DOC make-event."
        check-expansion))
   (t
    (revert-world-on-error
     (state-global-let*
      ((make-event-debug-depth (1+ (f-get-global 'make-event-debug-depth
                                                 state))))
      (let ((wrld (w state)))
        (er-let*
         ((debug-depth (make-event-debug-pre form on-behalf-of state))
          (expansion0 ; expansion result for form
           (protected-eval-with-proofs form on-behalf-of ctx state))
          (expansion1 ; apply macroexpansion to get actual embedded event form
           (pprogn
            (make-event-debug-post debug-depth expansion0 state)
            (do-proofs?

; This wrapper of do-proofs? avoids errors in checking expansions when
; ld-skip-proofsp is 'include-book.  See the "Very Technical Remark" in
; books/make-event/read-from-file.lisp.

             check-expansion
             (chk-embedded-event-form
              expansion0 whole-form wrld ctx state *primitive-event-macros*
              nil ; portcullisp
              (f-get-global 'in-local-flg state)
              (in-encapsulatep (global-val 'embedded-event-lst wrld) nil)
              nil))))
          (stobjs-out-and-result
           (trans-eval

; Note that expansion1 is guaranteed to be an embedded event form, which (as
; checked just below) must evaluate to an error triple.

            expansion1
            ctx state)))
         (let ((stobjs-out (car stobjs-out-and-result))
               (result (cdr stobjs-out-and-result))
               (expansion2
                (cond
                 ((f-get-global 'last-make-event-expansion state)
                  (mv-let
                   (wrappers base)
                   (destructure-expansion expansion1)
                   (cond
                    ((member-eq (car base)
                                '(make-event progn progn! encapsulate))
                     (rebuild-expansion
                      wrappers
                      (f-get-global 'last-make-event-expansion state)))
                    (t expansion1))))
                 (t expansion1))))
           (assert$
            (equal stobjs-out '(nil nil state)) ; evaluated an event form
            (cond ((car result)
                   (silent-error state))
                  ((and (consp check-expansion)
                        (not (equal check-expansion expansion2)))
                   (er soft ctx
                       "The current MAKE-EVENT expansion differs from the ~
                        expected (original or specified) expansion.  See :DOC ~
                        make-event.~|~%~|~%Make-event ~
                        argument:~|~%~Y01~|~%Expected ~
                        expansion:~|~%~Y21~|~%Current expansion:~|~%~Y31~|"
                       form
                       nil
                       check-expansion
                       expansion2))
                  (t (let ((actual-expansion
                            (cond
                             ((consp check-expansion)

; The original make-event form is already expanded (see :doc
; make-event-details).

                              nil)
                             (check-expansion
                              (assert$ (eq check-expansion t) ; from macro guard
                                       (list* 'make-event form
                                              :check-expansion expansion2
                                              (and on-behalf-of
                                                   `(:on-behalf-of
                                                     ,on-behalf-of)))))
                             (t expansion2))))
                       (pprogn
                        (f-put-global 'last-make-event-expansion
                                      actual-expansion
                                      state)
                        (value (cadr result)))))))))))))))

; Now we develop the book mechanism, which shares a lot with what
; we've just done.  In the discussion that follows, Unix is a
; trademark of Bell Laboratories.

; First, a broad question:  how much security are we trying to provide?
; After all, one could always fake a .cert file, say by calling checksum
; onesself.  Our claim is simply that we only fully "bless" certification runs,
; from scratch, of entire collections of books, without intervention.  Thus,
; there is no soundness problem with using (include-book "hd:ab.lisp") in a
; book certified in a Unix file system and having it mean something completely
; different on the Macintosh.  Presumably the attempt to certify this
; collection on the Macintosh would simply fail.

; How portable do we intend book names to be?  Suppose that one has a
; collection of books, some of which include-book some of the others, where all
; of these include-books use relative path names.  Can we set things up so that
; if one copies all of these .lisp and .cert files to another file system,
; preserving the hierarchical directory relationship, then we can guarantee
; that this collection of books is certifiable (modulo resource limitations)?
; The answer is yes: We use Unix-style pathnames within ACL2.  See :doc
; pathname, and see the Essay on Pathnames in interface-raw.lisp.  (Before
; Version_2.5 we also supported a notion of structured pathnames, similar to
; the "structured directories" concept in CLtL2.  However, the CLtL2 notion was
; just for directories, not file names, and we "deprecated" structured
; pathnames by deleting their documentation around Version_2.5.  We continued
; to support structured pathnames through Version_2.8 for backwards
; compatibility, but no longer.)

; Note.  It is important that regardless of what initial information we store
; in the state that is based on the surrounding operating system, this
; information not be observable in the logical theory.  For example, it would
; really be unfortunate if we did something like:

;  (defconst *directory-separator*
;    #+apple #\:
;    #-apple #\/)

; because then we could certify a book in one ACL2 that contains a theorem
; (equal *directory-separator* #\/), and include this book in another world
; where that theorem fails, thus deriving a contradiction.  In fact, we make
; the operating-system part of the state (as a world global), and figure
; everything else out about book names using that information.

(deflabel books
  :doc
  ":Doc-Section  Books

  files of ACL2 event forms~/

  This documentation topic is about ACL2 input files.  However, there are two
  traditional (paper) books published about ACL2:  a textbook and a case
  studies book.  Further information is available by following links from the
  ACL2 home page, ~c[http://www.cs.utexas.edu/users/moore/acl2/].  Now, on to
  the real content of this topic:

  A ``book'' is a file of ACL2 ~il[events] that have been certified as
  admissible.  Using ~ilc[include-book] you can construct a new logical
  ~il[world] by assuming the ~il[events] in any number of mutually compatible
  books.  Relevant documented topics are listed below.  Following this list
  is a ``guided tour'' through the topics.
  ~terminal[You may start guided tour by typing :more.]~/

  ~em[Introduction.]

  A ``book'' is a file of ACL2 forms.  Books are prepared entirely by
  the user of the system, i.e., they are ``source'' files not
  ``object'' files.  Some of the forms in a book are marked ~ilc[local]
  and the others are considered ``non-local.''

  ~ilc[Include-book] lets you load a book into any ACL2 ~il[world].  If
  completed without error, the inclusion of a book extends the logic
  of the host ~il[world] by the addition of just the non-local ~il[events] in
  the book.  You may extend the ~il[world] by successively including a
  variety of books to obtain the desired collection of definitions and
  rules.  Unless name conflicts occur (which are detected and
  signalled) inclusion of a book is consistency preserving provided
  the book itself is consistent as discussed later.  However,
  ~ilc[include-book] merely assumes the validity of the ~il[events] in a book;
  if you include a book that contains an inconsistency (e.g., an
  inadmissible definition) then the resulting theory is inconsistent.

  It is possible to ``certify'' a book, with ~ilc[certify-book],
  guaranteeing that the error-free inclusion of the certified forms
  will produce a consistent extension of a consistent logic.
  Certification processes both the ~ilc[local] and non-local forms, so
  you can mark as ~ilc[local] those ~il[events] you need for certification
  that you want to hide from users of the book (e.g., hacks, crocks,
  and kludges on the way to a good set of ~c[:]~ilc[rewrite] rules).
  Certification can also ``compile'' a book, thereby speeding up the
  execution of the functions defined within it.  The desire to compile
  books is largely responsible for the restrictions we put on the
  forms allowed in books.

  Extensive ~il[documentation] is available on the various aspects of
  books.  We recommend that you read it all before using books.  It
  has been written so as to make sense when read in a certain linear
  sequence, called the ``guided tour'', though in general you may
  browse through it randomly.  If you are on the guided tour, you
  should next read the ~il[documentation] on book-example
  (~pl[book-example]~terminal[ and use :more to read through it]).~/

  :cite include-book")

(defun chk-book-name (book-name full-book-name ctx state)

; Book-name is something submitted by the user as a book name.
; Full-book-name is the first result of calling parse-book-name on
; book-name and state.  We check that full-book-name is a string
; ending in ".lisp" or cause an error.  But the error reports
; book-name as the offender.

; This check is important because to form the certification extension we strip
; off the "lisp" and replace it by "cert".  So if this is changed, change
; convert-book-name-to-cert-name and also the raw lisp function
; convert-book-name-to-compiled-name.

; Note: Because it is our own code, namely parse-book-name, that tacks on the
; ".lisp" extension, this check is now redundant.  Once upon a time, the user
; was expected to supply the .lisp extension, but that made the execution of
; (include-book "arith.lisp") in raw lisp load the .lisp file rather than the
; .o file.  We've left the redundant check in because we are not sure that
; parse-book-name will be kept in its current form; it has changed a lot
; lately.

  (cond
   ((and (stringp full-book-name)
         (let ((n (length full-book-name)))
           (and (> n 5)
                (eql (char full-book-name (- n 5)) #\.) 
                (eql (char full-book-name (- n 4)) #\l) 
                (eql (char full-book-name (- n 3)) #\i) 
                (eql (char full-book-name (- n 2)) #\s) 
                (eql (char full-book-name (- n 1)) #\p))))
    (value nil))
   ((null full-book-name)
    (er soft ctx
        "~x0 is not a legal book name.  See :DOC book-name."
        book-name))
   (t (er soft ctx
          "~x0 is not a legal book name because it does not specify the ~
           ``.lisp'' extension.  See :DOC book-name."
          book-name))))

; The portcullis of a book consists of two things, a sequence of
; commands which must be executed with ld-skip-proofs nil without error
; and an include-book-alist-like structure which must be a subset of
; include-book-alist afterwards.  We describe the structure of an
; include-book-alist below.

(defun include-book-alist-subsetp (alist1 alist2)

; The include-book-alist contains elements of the
; general form         example value

; (full-book-name     ; "/usr/home/moore/project/arith.lisp"
;  user-book-name     ; "project/arith.lisp"
;  familiar-name      ; "arith"
;  cert-annotations   ; ((:SKIPPED-PROOFSP . sp)
;                        (:AXIOMSP . axp)
;                        (:TTAGS . ttag-alistp))
;  . ev-lst-chk-sum)  ; 12345678

; The include-book-alist becomes part of the certificate for a book, playing a
; role in both the pre-alist and the post-alist.  In the latter role some
; elements may be marked (LOCAL &).  When we refer to parts of the
; include-book-alist entries we have tried to use the tedious names above, to
; help us figure out what is used where.  Please try to preserve this
; convention.

; Cert-annotations is an alist.  The alist has three possible keys:
; :SKIPPED-PROOFSP, :AXIOMSP, and :TTAGS.  The possible values of the first two
; are t, nil, or ?, indicating the presence, absence, or possible presence of
; skip-proof forms or defaxioms, respectively.  The forms in question may be
; either LOCAL or non-LOCAL and are in the book itself (not just in some
; subbook).  Even though the cert-annotations is an alist, we compare
; include-book-alists with equality on that component, not ``alist equality.''
; So we are NOT free to drop or rearrange keys in these annotations.

; If the book is uncertified, the chk-sum entry is nil.

; Suppose the two alist arguments are each include-book-alists from different
; times.  We check that the first is a subset of the second, in the sense that
; the (familiar-name cert-annotations . chk-sum) parts of the first are all
; among those of the second.  We ignore the full names and the user names
; because they may change as the book or connected book directory moves around.

  (subsetp-equal (strip-cddrs alist1)
                 (strip-cddrs alist2)))

(defun get-portcullis-cmds (wrld ans wrld-segs wrld-list names ctx state)

; When certify-book is called, we scan down wrld to collect all the user
; commands into ans.  This answer is part of the portcullis of the certificate,
; once it has been cleaned up by fix-portcullis-cmds and new-defpkg-list.
; Second, we also accumulate corresponding world segments into wrld-segs.  Each
; command in ans will be "fixed" using its corresponding world segment.
; Finally, we also collect the entire world corresponding to each command,
; except the result has an extra world on the front of that list corresponding
; to the final world observed (probably the boot-strap world).

  (cond
   ((null wrld) (mv nil ans wrld-segs (cons wrld wrld-list) state))
   ((and (eq (caar wrld) 'command-landmark)
         (eq (cadar wrld) 'global-value))
    (let ((form
           (or (access-command-tuple-last-make-event-expansion (cddar wrld))
               (access-command-tuple-form (cddar wrld)))))
      (cond ((equal form '(exit-boot-strap-mode))
             (mv nil ans wrld-segs (cons wrld wrld-list) state))
            (t (mv-let
                (erp val state)
                (chk-embedded-event-form form nil
                                         wrld ctx state names t nil nil t)
                (declare (ignore val))
                (cond
                 (erp (mv erp nil nil nil state))
                 (t
                  (get-portcullis-cmds
                   (cdr wrld)
                   (cons form ans)
                   (cons (world-to-next-command (cdr wrld) nil)
                         wrld-segs)
                   (cons wrld wrld-list)
                   names ctx state))))))))
   (t (get-portcullis-cmds (cdr wrld) ans wrld-segs wrld-list names ctx
                           state))))

; We now develop code to "fix" the commands in the certification world before
; placing them in the portcullis of the certificate, in order to eliminate
; relative pathnames in include-book forms.  See the comment in
; fix-portcullis-cmds.

(mutual-recursion

(defun make-include-books-absolute (form book-name-alist names
                                         make-event-parent os ctx state)

; Form is a command from the current ACL2 world that is known to be an embedded
; event form with respect to names.  Keep this function in sync with
; chk-embedded-event-form in chk-acceptable-certify-book1).  We pattern the
; code after find-a-skip-proofs.  Like that function, this one returns an error
; triple because we use macroexpand1, but never causes an error.

; If make-event-parent is non-nil, then it is a make-event form whose expansion
; is being considered, and we cause an error rather than converting.

  (cond
   ((atom form) (value form)) ; This should never happen.
   ((eq (car form) 'skip-proofs)
    (er-let* ((x (make-include-books-absolute (cadr form) book-name-alist names
                                              make-event-parent os ctx
                                              state)))
             (value (list (car form) x))))
   ((eq (car form) 'local)

; Include-books in the context of LOCAL will not generate 'certification-tuple
; entries in the world, hence will not contribute to book-name-alist.  If we
; were to explore (cadr form), we could then find an include-book of a relative
; pathname that is not found in book-name-alist.

    (value form))
   ((eq (car form) 'encapsulate)
    (er-let* ((x (make-include-books-absolute-lst (cddr form) book-name-alist
                                                  names make-event-parent os
                                                  ctx state)))
             (value (list* (car form)
                           (cadr form)
                           x))))
   ((eq (car form) 'progn)
    (er-let* ((rest (make-include-books-absolute-lst (cdr form) book-name-alist
                                                     names make-event-parent
                                                     os ctx state)))
             (value (cons (car form)
                          rest))))
   ((eq (car form) 'value)
    (value form))
   ((and (eq (car form) 'include-book)
         (stringp (cadr form))
         (not (absolute-pathname-string-p
               (cadr form)
               nil ; no directory check necessary here
               os)))
    (cond
     (make-event-parent
      (er soft ctx

; Suppose the following error occurs.  A solution could be to treat
; include-book kind of like make-event, in the sense that its expansion has an
; absolute pathname, and its expansion is recorded immediately.  Then we could
; presumably eliminate make-include-books-absolute entirely.  If we decide to
; consider such a change, then we should also consider whether storing an
; absolute pathname for *every* include-book could cause some sort of
; portability problem, for example when one wishes to move certificates (an
; operation we don't condone but which is occasionally performed).

          "Each include-book form in the certification world must have an ~
           absolute pathname.  ACL2 generally figures out a suitable absolute ~
           pathname when the pathname is relative.  But the present form, ~
           ~x0, comes from the expansion of a make-event form with (first) ~
           argument ~x1 and non-nil :check-expansion argument.  Consider ~
           changing this make-event form to produce an include-book with an ~
           absolute pathname instead.  If this is a hardship, please feel ~
           free to report the issue to the ACL2 implementors, who may be able ~
           to come up with a better solution."
          form (cadr make-event-parent)))
     (t
      (let ((full-book-names
             (or (cdr (assoc-equal (cadr form) book-name-alist))
                 (er hard ctx
                     "Implementation error (did not find book name, function ~
                      make-include-books-absolute).  Please contact the ACL2 ~
                      implementors."))))
        (if (null (cdr full-book-names))
            (value (list* 'include-book
                          (car full-book-names)
                          (cddr form)))
          (er soft ctx
              "The certification world has include-book command ~x0 that ~
               corresponds to more than one possible absolute pathname, ~
               namely:  ~&1. ACL2 cannot currently certify a book in such a ~
               world.  To work around this problem, you can use an absolute ~
               pathname in this include-book command (see :DOC pathname) or ~
               rename one of the conflicting books."
              form
              full-book-names))))))
   ((member-eq (car form) names)
    (value form))
   ((eq (car form) 'make-event)
    (let ((expansion (cadr (assoc-keyword :check-expansion (cddr form)))))
      (cond ((not (consp expansion))
             (er soft ctx
                 "Implementation error: we had thought that every make-event ~
                  form in the certification world would have a consp ~
                  :check-expansion field, yet we found the following.  Please ~
                  contact the ACL2 implementors.~|~x0"
                 form))
            (t (er-progn (make-include-books-absolute expansion book-name-alist
                                                      names
                                                      form
                                                      os ctx state)
                         (value form))))))
   ((and (eq (car form) 'with-output)
         (consp (cdr form)))
    (er-let* ((form1 (make-include-books-absolute (car (last form))
                                                  book-name-alist names
                                                  make-event-parent os ctx
                                                  state)))
             (value (append (butlast form 1) (list form1)))))
   ((getprop (car form) 'macro-body nil 'current-acl2-world (w state))
    (er-let*
     ((form1 (macroexpand1 form ctx state)))
     (make-include-books-absolute form1 book-name-alist names
                                  make-event-parent os ctx state)))
   (t (value (er hard ctx
                 "Implementation error in make-include-books-absolute:  ~
                  unrecognized event type, ~x0.  Make-include-books-absolute ~
                  needs to be kept in sync with chk-embedded-event-form.  ~
                  Please send this error message to the implementors."
                 (car form))))))

(defun make-include-books-absolute-lst (forms book-name-alist names
                                              make-event-parent os ctx state)
  (if (endp forms)
      (value nil)
    (er-let* ((first (make-include-books-absolute (car forms) book-name-alist
                                                  names make-event-parent os
                                                  ctx state))
              (rest (make-include-books-absolute-lst (cdr forms)
                                                     book-name-alist names
                                                     make-event-parent os
                                                     ctx state)))
             (value (cons first rest)))))
)

(defun relative-book-name-alist (wrld-segment acc os ctx state)

; We return an alist that associates, for each relative pathname p for which
; (include-book p) was executed in the given world segment, the corresponding
; full book names that are (also) without the .lisp extension.

  (cond
   ((null wrld-segment)
    (value acc))
   ((and (eq (caar wrld-segment) 'certification-tuple)
         (eq (cadar wrld-segment) 'global-value))
    (let* ((cert-tuple (cddar wrld-segment))

; Cert-tuple is of the following form.

; (list full-book-name user-book-name familiar-name cert-annotations
;       ev-lst-chk-sum) 

           (full-book-name-raw
            (if (eq cert-tuple *acl2-property-unbound*)

; We do not expect to find *acl2-property-unbound* here.  If we do find it,
; then we cause an error.

                (er hard ctx
                    "Implementation error!  Unexpected find of unbound ~
                     certification tuple value in command-cbd!  Please ~
                     contact the ACL2 implementors and send this message.")
              (car cert-tuple)))
           (full-book-name
            (subseq full-book-name-raw 0 (- (length full-book-name-raw) 5)))
           (user-book-name (cadr cert-tuple)))
      (cond

; Since this code is being introduced in Version_2.6, we do not need to
; support structured pathnames.  We check for them just in case.

       ((not (stringp user-book-name))
        (er soft ctx
            "Structured book names, such as ~x0, are no longer supported by ~
             ACL2.  Please modify your include-book events accordingly."
            user-book-name))
       ((absolute-pathname-string-p user-book-name nil os)
        (relative-book-name-alist (cdr wrld-segment) acc os ctx state))
       (t
        (let ((pair (assoc-equal user-book-name acc)))
          (cond
           ((null pair)
            (relative-book-name-alist (cdr wrld-segment)
                                      (cons (list user-book-name
                                                  full-book-name)
                                            acc)
                                      os ctx state))
           ((member-equal full-book-name (cdr pair)) ; Can this happen?
            (relative-book-name-alist (cdr wrld-segment) acc os ctx state))
           (t
            (relative-book-name-alist (cdr wrld-segment)
                                      (put-assoc-equal
                                       (car pair)
                                       (cons full-book-name (cdr pair))
                                       acc)
                                      os ctx state))))))))
   (t
    (relative-book-name-alist (cdr wrld-segment) acc os ctx state))))

(defun first-known-package-alist (wrld-segment)
  (cond
   ((null wrld-segment)
    nil)
   ((and (eq (caar wrld-segment) 'known-package-alist)
         (eq (cadar wrld-segment) 'global-value))
    (let* ((kpa  (cddar wrld-segment)))
      (if (eq kpa *acl2-property-unbound*)

; We do not expect to find *acl2-property-unbound* here.  If we do find it,
; then we cause an error.

          (er hard 'first-known-package-alist
              "Implementation error!  Unexpected find of unbound ~
               known-package-alist value!  Please contact the ACL2 ~
               implementors and send this message.")
        kpa)))
   (t
    (first-known-package-alist (cdr wrld-segment)))))

(defun defpkg-items-rec (new-kpa old-kpa ctx w state acc)

; For background on the discussion below, see the Essay on Hidden Packages.

; We are given a world w (for example, the certification world of a
; certify-book command).  Old-kpa is the known-package-alist of w.  New-kpa is
; another known-package-alist, which may include entries not in old-kpa (for
; example, the known-package-alist after executing each event in the
; admissibility pass of certify-book).  We return a list of "defpkg items" for
; names of new-kpa, where each item is of the form (list name imports body doc
; book-path).  The intention is that the item can be used to form a defpkg
; event with indicated name, body, doc and book-path, where body may have been
; modified from a corresponding defpkg event so that it is suitable for
; evaluation in w.  Here, book-path is the book-path to be used if such an
; event is to be added to the end of the portcullis commands in the certificate
; of a book being certified.

; As a minor optimization we completely omit names of new-kpa from the result
; when they correspond to packages in old-kpa.

; It is helpful for efficiency if w is the current-acl2-world or a reasonably
; short extension of it, since we call termp and untranslate on that world.

; Finally, note that the order of packages in new-kpa is in the reverse of the
; order from the resulting list of defpkg events.  Thus, the newest packages
; are at the end of the returned list.

  (cond
   ((endp new-kpa) (value acc))
   (t (let* ((e (car new-kpa))
             (n (package-entry-name e)))
        (cond
         ((find-package-entry n old-kpa)
          (defpkg-items-rec (cdr new-kpa) old-kpa ctx w state acc))
         (t
          (let* ((imports (package-entry-imports e))
                 (event (package-entry-defpkg-event-form e))
                 (name (cadr event))
                 (body (caddr event))
                 (doc (cadddr event))
                 (tterm (package-entry-tterm e))
                 (book-path (package-entry-book-path e)))
            (mv-let (erp pair state)
              (simple-translate-and-eval body nil nil
                                         "The second argument to defpkg"
                                         ctx w state)
              (defpkg-items-rec
                (cdr new-kpa) old-kpa ctx w state
                (cons (list name
                            imports
                            (assert$
                             event
                             (assert$
                              (equal n name)
                              (cond ((and (not erp)
                                          (equal (cdr pair) imports) ; always?
                                          (equal tterm (car pair)))
                                     body)
                                    ((termp tterm w)
                                     tterm)
                                    (t
                                     (kwote imports)))))
                            doc
                            book-path)
                      acc))))))))))

(defun defpkg-items (new-kpa ctx w state)

; This is just a wrapper for defpkg-items-rec, with error output turned off
; (because of calls of translate).  See the comment for defpkg-items-rec.

  (state-global-let*
   ((inhibit-output-lst (cons 'error
                              (f-get-global 'inhibit-output-lst state))))
   (defpkg-items-rec new-kpa (global-val 'known-package-alist w) ctx w state
     nil)))

(defun new-defpkg-list2 (imports discarded-items flg seen)
  (cond
   ((endp imports)
    (mv flg discarded-items))
   (t
    (let ((p (symbol-package-name (car imports))))
      (cond
       ((member-equal p seen)
        (new-defpkg-list2 (cdr imports) discarded-items
                          flg seen))
       (t (let ((item (assoc-equal p discarded-items)))
            (cond (item (new-defpkg-list2
                         (cdr imports)
                         (delete-assoc-equal p discarded-items)
                         t
                         (cons p seen)))
                  (t (new-defpkg-list2
                      (cdr imports)
                      discarded-items
                      flg
                      (cons p seen)))))))))))

(defun make-defpkg (name imports/doc/book-path)
  (let ((imports (car imports/doc/book-path))
        (doc (cadr imports/doc/book-path))
        (book-path (caddr imports/doc/book-path)))
    (cond
     (book-path `(defpkg ,name ,imports ,doc ,book-path))
     (doc       `(defpkg ,name ,imports ,doc))
     (t         `(defpkg ,name ,imports)))))

(defun new-defpkg-list1 (defpkg-items base-kpa added-defpkgs discarded-items)

; See the comment in new-defpkg-list.  Here, we maintain an accumulator,
; added-defpkgs, that contains the defpkg that need to be added based on what
; we have already processed in defpkg-items, in reverse order.  We also
; maintain a list discarded-items, that contains members of defpkg-items not
; added to added-defpkgs.

  (cond
   ((endp defpkg-items)
    (mv added-defpkgs discarded-items))
   (t
    (mv-let (added-defpkgs discarded-items)
      (new-defpkg-list1 (cdr defpkg-items) base-kpa added-defpkgs
                        discarded-items)
      (let* ((item (car defpkg-items))
             (name (car item)))
        (cond
         ((find-package-entry name base-kpa)
          (mv added-defpkgs (cons item discarded-items)))
         (t ; we want to add event, so may need to add some already "discarded"
          (let ((imports (cadr item)))
            (mv-let (flg remaining-discarded-items)
              (new-defpkg-list2 imports discarded-items nil nil)
              (mv-let (added-defpkgs discarded-items)
                (cond
                 (flg ; (not (equal discarded-items remaining-discarded-items))
                  (new-defpkg-list1
                   (set-difference-equal discarded-items
                                         remaining-discarded-items)
                   base-kpa added-defpkgs remaining-discarded-items))
                 (t (mv added-defpkgs discarded-items)))
                (mv (cons (make-defpkg name (cddr item))
                          added-defpkgs)
                    discarded-items)))))))))))
                
(defun new-defpkg-list (defpkg-items base-kpa)

; For background on the discussion below, see the Essay on Hidden Packages.

; Defpkg-items is a list of "defpkg items" each of the form (list name imports
; body doc book-path) representing a list of package definitions.  We return a
; list of defpkg events that can be executed in a world whose
; known-package-alist is base-kpa.  Defpkg-items corresponds to defpkg events
; in reverse order of their introduction, i.e., the newest ones are at the end
; of the list; and the returned list has a corresponding order.  The primary
; reason a defpkg is in the returned list is that its package is not in
; base-kpa (not even hidden).  The second reason is that we need to define a
; package P1 if we add another package P2 whose import list contains a symbol
; in package P1; we close under this process.

; This function is called at the end of the include-book phase of certify-book.
; In that case, base-kpa is the known-package-alist at that point, and
; defpkg-items contains an item for each name of a package in the
; known-package-alist at the end of the earlier, admissibility pass of
; certify-book that was not defined in the certification world.  To illustrate
; the "second reason" above, let us suppose that the book being certified
; contains forms (include-book "book1") and (local (include-book "book2")),
; where book1 defines (defpkg "PKG1" ...) and book2 defines (defpkg "PKG2"
; '(PKG1::SYM)).  Then we want to add the definition of "PKG2" to the
; portcullis, but in order to do so, we need to add the definition of "PKG1" as
; well, even though it will eventually be included by way of book1.  And, we
; need to be sure to add the defpkg of "PKG1" before that of "PKG2".

; This function is also called on behalf of puff-fn1, where defpkg-items
; corresponds to the packages in known-package-alist in the world at completion
; of the command about to be puffed, and base-kpa corresponds to the
; known-package-alist just before that command.  In that case there is no ned
; for the "second reason" above, but for simplicity we call this same function.

  (mv-let (added-defpkgs discarded-items)
    (new-defpkg-list1 defpkg-items base-kpa nil nil)
    (declare (ignore discarded-items))
    added-defpkgs))

(defun fix-portcullis-cmds (cmds wrld-segs wrld-list old-kpa ans names os ctx
                                 state)

; This function modifies cmds by making relative pathnames absolute, and also
; by adding defpkg events as explained in the Essay on Hidden Packages.  We
; explain these two aspects in turn.

; Certify-book needs to insist that any include-books in the portcullis refer
; to absolute pathnames for the operating system in which the book was
; certified.  This ensures that the actual file read is not dependent upon cbd.
; Suppose this restriction were dropped, and consider:

; :set-cbd "/usr/home/moore/"
; (include-book "prelude")
; :set-cbd "/usr/local/src/library/"
; (certify-book "user")

; A naive implementation would provide a portcullis for "user" that contains
; (include-book "prelude").  But there is no clue as to the directory on which
; "prelude" resides.  Note that "prelude" does not represent an absolute
; pathname.  If it did represent an absolute pathname, then it would have to be
; the full book name because parse-book-name returns x when x represents an
; absolute pathname.

; We deal with the issue above by allowing relative pathnames for include-book
; commands in the certification world, but modifying them to be appropriate
; absolute pathnames.  This function takes the original cmds, a list of
; embedded event forms, together with wrld-segs, a corresponding list of world
; segments.  Each member of wrld-segs contains the tuples laid down as a result
; of its corresponding command in cmds (up to but not including the command
; landmark).  We return a list of commands that is guaranteed to be free of
; include-books of relative pathnames, that nevertheless is equivalent to the
; original cmds from the standpoint of subsequent embedded events.  (Or, we
; return an error, but in fact we believe that that will not happen.)

; Our algorithm uses the 'certification-tuple tuples laid down in each element
; of wrld-segs to make necessary adjustments.  We will thus miss local
; include-book events.  But we do not need to make those absolute, since we
; trust the portcullis and hence do not need to pay attention to local events.

; As mentioned at the outset above, this function also adds defpkg events.  Our
; approach is heavy-handed (it seemed easiest to code this way), in that it
; adds a defpkg event before each command for every defpkg admitted under that
; command.  if the command is a defkpg, then we add the identical defpkg in
; front of that defpkg!  But we take care of that and any other duplication in
; the call of remove-duplicates-equal-from-end in certify-book-fn.  If for some
; reason we fail to remove the duplicate for trivial reasons (say we fail to
; match the doc string or the book-path argument of a defpkg when adding an
; allegedly identical new on), the worst that can happen is that we get a
; surprising error; barring that, the worst is that we have duplicate defpkg
; events in .cert files.  Neither is a soundness issue.

; Call this function using the same names parameter as that used when verifying
; that cmds is a list of embedded event forms.

  (cond
   ((null cmds) (value (reverse ans)))
   (t (er-let* ((book-name-alist
                 (relative-book-name-alist (car wrld-segs) nil os ctx state))
                (cmd
                 (if (null book-name-alist) ; optimization:  nothing to fix
                     (value (car cmds))
                   (make-include-books-absolute (car cmds) book-name-alist
                                                names nil os ctx state)))
                (new-kpa (value (first-known-package-alist (car wrld-segs))))
                (defpkg-items (if new-kpa
                                  (defpkg-items
                                    new-kpa
                                    ctx (car wrld-list) state)
                                (value nil))))
               (fix-portcullis-cmds (cdr cmds)
                                    (cdr wrld-segs)
                                    (cdr wrld-list)
                                    new-kpa
                                    (cons cmd (revappend (new-defpkg-list
                                                          defpkg-items
                                                          old-kpa)
                                                         ans))
                                    names os ctx state)))))

(defun collect-uncertified-books (alist)

; Alist is an include-book-alist and thus contains elements of the
; form described in include-book-alist-subsetp.  A typical element is
; (full-book-name user-book-name familiar-name cert-annotations
; . ev-lst-chk-sum) and ev-lst-chk-sum is nil if the book has not been
; certified.

  (cond ((null alist) nil)
        ((null (cddddr (car alist)))  ; ev-lst-chk-sum
         (cons (caar alist)           ; full-book-name
               (collect-uncertified-books (cdr alist))))
        (t (collect-uncertified-books (cdr alist)))))

(defun certificate-filep (file1 state)
  (let ((file2 (convert-book-name-to-cert-name file1)))
    (mv-let
     (ch state)
     (open-input-channel file2 :object state)
     (cond
      ((null ch) (value nil))
      (t (pprogn (close-input-channel ch state)
                 (value t)))))))

(defun chk-in-package (channel file ctx state)

; Channel must be an open input object channel.  We assume (for error
; reporting purposes) that it is associated with the file named file.
; We read the first form in it and cause an error unless that form is
; an in-package.  If it is an in-package, we return the package name.

  (state-global-let*
   ((current-package "ACL2"))
   (mv-let (eofp val state)
           (read-object channel state)
           (cond
            (eofp (er soft ctx
                      "The file ~x0 is empty.  An IN-PACKAGE form, at ~
                       the very least, was expected."
                      file))
            ((and (true-listp val)
                  (= (length val) 2)
                  (eq (car val) 'in-package)
                  (stringp (cadr val)))
             (cond
              ((find-non-hidden-package-entry (cadr val)
                                              (known-package-alist state))
               (value (cadr val)))
              (t (er soft ctx
                     "The argument to IN-PACKAGE must be a known ~
                      package name, but ~x0, used in the first form ~
                      in ~x1, is not.  The known packages are~*2"
                     (cadr val)
                     file
                     (tilde-*-&v-strings
                      '&
                      (strip-non-hidden-package-names
                       (known-package-alist state))
                      #\.)))))
            (t (er soft ctx
                   "The first form in ~x0 was expected to be ~
                    (IN-PACKAGE \"pkg\") where \"pkg\" is a known ~
                    ACL2 package name.  See :DOC book-contents.  The first ~
                    form was, in fact, ~x1."
                   file val))))))

(defconst *ill-formed-certificate-msg*
  "The certificate for the book ~x0 is ill-formed.  Delete or rename ~
   the file ~x1 and recertify ~x0.  Remember that the certification ~
   world for ~x0 is described in the portcullis of ~x1 (see :DOC ~
   portcullis) so you might want to look at ~x1 to remind yourself of ~
   ~x0's certification world.") 

(defun include-book-er (file1 file2 str
                              keyword
                              suspect-book-action-alist
                              ctx state)

; Depending on various conditions we either do nothing, print a
; warning, or cause an error.  File1 and file2 are the full book name
; and its .cert file, respectively.  (Well, sometimes file2 is nil --
; we never use it ourselves but str might and supplies it when
; needed.)  Str is an arbitrary fmt string used as the error message
; and used in the warning message.  Suspect-book-action-alist is the
; alist manufactured by include-book, specifying the values of its
; keyword arguments.  Among these are arguments that control our
; behavior on these errors.  Keyword specifies the kind of error this
; is, using the convention that it is either t, meaning cause an
; error, or the keyword used by include-book to specify the behavior.
; I.e., if this error reports the lack of a certificate, then keyword
; is :uncertified-okp.

  (let* ((keyword-string
          (case keyword
            (:uncertified-okp "Uncertified")
            (:skip-proofs-okp "Skip-proofs")
            (:defaxioms-okp "Defaxioms")
            (t (if (eq keyword t)
                   nil
                 (er hard 'include-book-er
                     "Include-book-er does not know the include-book keyword ~
                      argument ~x0."
                     keyword)))))
         (warning-summary
          (cond
           ((eq keyword t) nil)
           ((assoc-eq keyword suspect-book-action-alist)
            (cond
             ((cdr (assoc-eq keyword suspect-book-action-alist))
              (cond
               ((if (eq keyword :skip-proofs-okp)
                    (not (f-get-global 'skip-proofs-okp-cert state))
                  (and (eq keyword :defaxioms-okp)
                       (not (f-get-global 'defaxioms-okp-cert state))))

; Although suspect-book-action-alist allows this (implicit) include-book, we
; are attempting this include-book underneath a certify-book that disallows
; this keyword.  We signify this case by overloading warning-summary to be this
; keyword.

                keyword)
               (t keyword-string)))
             (t nil)))
           (t (er hard 'include-book-er
                  "There is a discrepancy between the keywords in the ~
                   suspect-book-action-alist, ~x0, and the keyword, ~x1, ~
                   supplied to include-book-er."
                  suspect-book-action-alist
                  keyword)))))

; If warning-summary is nil, we cause an error.  Otherwise, it is summary
; of the desired warning.

    (cond
     ((null warning-summary)
      (er soft ctx "~@2" file1 file2 str))
     ((symbolp warning-summary) ; keyword
      (er soft ctx "~@0  This is illegal because we are currently attempting a ~
                    book certification with ~x1 set to NIL.  You can avoid ~
                    this error by calling certify-book with a value of T for ~
                    ~x1; see :DOC certify-book."
          (list "~@2" (cons #\0 file1) (cons #\1 file2) (cons #\2 str))
          keyword))
     (t
      (pprogn
       (warning$ ctx warning-summary "~@2" file1 file2 str)
       (value nil))))))

(defun tilde-*-book-check-sums-phrase1 (flg reqd-alist actual-alist)

; The two alists are strip-cddrs of include-book-alists.  Thus, each
; entry in each is of the form (familiar-name cert-annotations
; . ev-lst-chk-sum).  For each entry in reqd-alist we either find an
; identical entry in actual-alist or else we print a message.

  (cond
   ((null reqd-alist) nil)
   (t (let* ((reqd-entry (car reqd-alist))
             (familiar-name (car reqd-entry))
             (actual-entry (assoc-equal familiar-name actual-alist)))

; We know there is an entry for familiar-name because otherwise we would have
; caused an error.  The question is only whether we found a cert file
; for it, etc.

        (cond
         ((equal reqd-entry actual-entry)
          (tilde-*-book-check-sums-phrase1 flg (cdr reqd-alist) actual-alist))
         (flg
          (cons
           (msg "the book \"~s0\" with certificate annotations ~x1 ~
                 and check sum ~x2"
                familiar-name
                (cadr reqd-entry)  ;;; cert-annotations
                (cddr reqd-entry)) ;;; ev-lst-chk-sum
           (tilde-*-book-check-sums-phrase1 flg
                                            (cdr reqd-alist)
                                            actual-alist)))
         (t
          (cons
           (cond
            ((null (cddr actual-entry))
             (msg "an uncertified version of ~x0 with certificate ~
                   annotations ~x1"
                  familiar-name
                  (cadr actual-entry) ; cert-annotations
                  ))
            (t (msg "a version of ~x0 with certificate annotations ~
                     ~x1 and check sum ~x2"
                    familiar-name
                    (cadr actual-entry)  ; cert-annotations
                    (cddr actual-entry))))
           (tilde-*-book-check-sums-phrase1 flg
                                            (cdr reqd-alist)
                                            actual-alist))))))))

(defun tilde-*-book-check-sums-phrase (flg reqd-alist actual-alist)

; Flg is t or nil and the two alists each contain pairs of the form
; (full-book-name user-book-name familiar-name cert-annotations
; . ev-lst-chk-sum).  Reqd-Alist shows what is required and
; actual-alist shows that is actual.  We know reqd-alist ought to be a
; `include-book alist subset' of actual-alist but it is not.  If flg
; is t we complete the phrase "this book requires ..."  and if flg is
; nil we complete "but we have ...".

  (list "" "~@*" "~@* and " "~@*, "
        (tilde-*-book-check-sums-phrase1 flg
                                         (strip-cddrs reqd-alist)
                                         (strip-cddrs actual-alist))))

(defun get-cmds-from-portcullis (file1 file2 ch ctx state ans)

; Keep this in sync with chk-raise-portcullis2.

; We read successive forms from ch, stopping when we get to
; :END-PORTCULLIS-CMDS and returning the list of forms read, which we
; accumulate onto ans as we go.  Ans should be nil initially.

  (mv-let (eofp form state)
          (state-global-let*
           ((infixp nil))
           (read-object ch state))
          (cond
           (eofp (er soft ctx *ill-formed-certificate-msg* file1 file2))
           ((eq form :END-PORTCULLIS-CMDS)
            (value (reverse ans)))
           (t (get-cmds-from-portcullis file1 file2 ch ctx state
                                        (cons form ans))))))

(defun chk-raise-portcullis2 (file1 file2 ch ctx state ans)

; Keep this in sync with get-cmds-from-portcullis.

; We read successive forms from ch and trans-eval them.  We stop when
; we get to :END-PORTCULLIS-CMDS.  We may cause an error.  It is
; assumed that each form evaluated is a DEFPKG or an event form and is
; responsible for installing its world in state.  This assumption is
; checked by chk-acceptable-certify-book, before a .cert file is
; written.  We return the list of forms read, which we accumulate onto
; ans as we go.  Ans should be nil initially.

  (mv-let (eofp form state)
          (state-global-let*
           ((infixp nil))
           (read-object ch state))
          (cond
           (eofp (er soft ctx *ill-formed-certificate-msg* file1 file2))
           ((eq form :END-PORTCULLIS-CMDS)
            (value (reverse ans)))
           (t (mv-let
               (error-flg trans-ans state)
               (trans-eval form
                           (msg "the portcullis for ~x0"
                                file1)
                           state)

; If error-flg is nil, trans-ans is of the form
; ((nil nil state) . (erp' val' replaced-state))
; because form is a DEFPKG or event form.

               (let ((erp-prime (car (cdr trans-ans))))
                 (cond
                  ((or error-flg erp-prime) ;erp'
                   (pprogn
                    (warning$ ctx "Portcullis"
                              "The error reported above was caused ~
                               while trying to raise the portcullis ~
                               for the book ~x0.  In particular, we ~
                               were trying to execute ~x1 when the ~
                               error occurred.  Because we cannot ~
                               raise the portcullis, we cannot ~
                               include this book in this world.  ~
                               There are two standard responses to ~
                               this situation.  Either change the ~
                               current logical world so that this ~
                               error does not occur, e.g., redefine ~
                               one of your functions, or recertify ~
                               the book in a different environment."
                              file1 form)
                    (mv t nil state)))
                  (t (chk-raise-portcullis2 file1 file2 ch ctx state
                                            (cons form ans))))))))))

(defun chk-raise-portcullis1 (file1 file2 ch ctx state)

; We read each of the forms in ch until we get to :END-PORTCULLIS-CMDS
; and eval them.  However, we temporarily skip proofs (in an error
; protected way).  We return the list of command forms in the
; portcullis.

  (state-global-let*
   ((ld-skip-proofsp 'include-book)
    (in-local-flg

; As we start processing events on behalf of including a book, we are no longer
; in the lexical scope of LOCAL for purposes of disallowing setting of the
; acl2-defaults-table.

     (and (f-get-global 'in-local-flg state)
          'dynamic)))
   (er-progn
    (maybe-install-acl2-defaults-table

; The point here is to re-create the environment in which the book to be
; included was originally certified.  If we do not install the original
; acl2-defaults-table then we can, for example, certify a book definining (foo
; x) = (car x), then in a new session include this book after
; (set-verify-guards-eagerness 2), and then get a hard error with (foo 3).

     *initial-acl2-defaults-table*
     'chk-raise-portcullis1 state)
    (chk-raise-portcullis2 file1 file2 ch ctx state nil))))

(defun mark-local-included-books (post-alist1 post-alist2)

; See make-certificate-file for an explanation of this function.  Roughly
; speaking, we copy post-alist1 (which is the include-book-alist after the
; events in the main book were successfully proved) and every time we find a
; non-local book in it that is not in post-alist2 (which is the
; include-book-alist after the main book was included by certify-book's second
; pass), we mark that element LOCAL.  We know that post-alist2 is a subset of
; post-alist1.  Thus, if we then throw out all the elements marked LOCAL we get
; post-alist2.

; One might ask why we mark post-alist1 this way rather than just put
; post-alist2 into the certificate object in the first place.  One reason
; is to allow a hand inspection of the certificate to see exactly what
; versions of the local subbooks participated in the certification.  But a more
; critical reason is to note the use of skip-proofs in locally included
; subbooks; see the Essay on Skip-proofs.

; Recall that each element of an include-book-alist is (full-book-name
; user-book-name familiar-name cert-annotations . ev-lst-chk-sum).  We
; only look at the full-book-name components below.

  (cond ((null post-alist1) nil)
        ((eq (caar post-alist1) 'local)
         (cons (car post-alist1)
               (mark-local-included-books (cdr post-alist1) post-alist2)))
        ((assoc-equal (caar post-alist1) post-alist2)
         (cons (car post-alist1)
               (mark-local-included-books (cdr post-alist1) post-alist2)))
        (t (cons (list 'local (car post-alist1))
                 (mark-local-included-books (cdr post-alist1) post-alist2)))))

(defun unmark-and-delete-local-included-books (post-alist3)

; See make-certificate-file for an explanation of this function.  Roughly
; speaking, this function undoes what mark-local-included-books does.  If
; post-alist3 is the result of marking post-alist1 and post-alist2, then this
; function produces post-alist2 from post-alist3.  Given our use of it, it
; produces the include-book-alist you should have after any successful
; inclusion of the main book.

  (cond ((null post-alist3) nil)
        ((eq (caar post-alist3) 'LOCAL)
         (unmark-and-delete-local-included-books (cdr post-alist3)))
        (t (cons (car post-alist3)
                 (unmark-and-delete-local-included-books (cdr post-alist3))))))

; Essay on Tainted Books

; A notion of incremental version number, or incrl, was introduced in ACL2
; Version_2.9.1, in order to support incremental releases.  We wanted to make
; it easy for the ACL2 user to experiment with incremental releases without
; being required to recertify all books.  See :DOC set-tainted-okp for more
; high-level discussion of this issue and how it is dealt with from a user
; perspective.  In a nutshell, we mark a book as "tainted" by putting
; "(tainted)" into the version string of its certificate, and we do this when
; we detect the possibility that the book depends on the inclusion of at least
; one book with a mismatch of the incrl version number (see :DOC version).  We
; disallow certification of tainted books, and treat include-book of tainted
; books as uncertified, except when the user has evaluated (set-tainted-okp t),

; Two state globals support handling of tainted books.  State global
; 'certify-book-info is generally nil, but is set to the full book name while
; certifying a book and transitions from that to (list full-book-name) if we
; encounter inclusion of a book that is either explicitly tainted or has a
; version mismatch of its incrl field.  State global 'tainted-okp is initially
; nil, which disallows certification of tainted books and treates included
; tainted books as uncertified.  The user may call set-tainted-okp to set this
; variable to t in order to allow include-book and certify book to accept
; tainted books fully, albeit with "Tainted" warnings.

(defun decimal-string-to-number (s bound expo)

; Returns 10^expo times the integer represented by the digits of string s from
; 0 up through bound-1 (most significant digit at position 0), but returns a
; hard error if any of those "digits" are not digits.

  (declare (xargs :guard (and (stringp s)
                              (natp expo)
                              (<= bound (length s)))))
  (cond ((zp bound) 0)
        (t (let* ((pos (1- bound))
                  (ch (char s pos)))
             (cond ((member ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                    (let ((digit (case ch
                                   (#\0 0)
                                   (#\1 1)
                                   (#\2 2)
                                   (#\3 3)
                                   (#\4 4)
                                   (#\5 5)
                                   (#\6 6)
                                   (#\7 7)
                                   (#\8 8)
                                   (otherwise 9))))
                      (+ (* (expt 10 expo) digit)
                         (decimal-string-to-number s pos (1+ expo)))))
                   (t (er hard 'decimal-string-to-number
                          "Found non-decimal digit in position ~x0 of string ~
                           \"~s1\"."
                          pos s)))))))

(defun tainted-stringp (s)

; Keep this in sync with taint-string.

; This function recognizes acl2-version strings that are "tainted".  Books with
; certificates containing such acl2-version strings are not to be fully
; trusted.

; We could just look in s for "(tainted)".  But this function allows strings
; such as "(tainted[some-other-info])", which may somehow be of use for us down
; the road.

  (let ((p1 (position #\( s)))
    (and p1
         (let ((p2 (position #\) s)))
           (and p2
                (<= 8 (- p2 p1))
                (equal (subseq s (+ 1 p1) (+ 8 p1)) "tainted"))))))

(defun taint-string (v)

; Keep this in sync with tainted-stringp.

  (let ((p (position #\( v))
        (tnt "(tainted)"))
    (cond (p (concatenate 'string
                          (subseq v 0 p)
                          tnt
                          (subseq v p (length v))))
          (t (concatenate 'string v tnt)))))

(defun parse-version (version)

; Version is an ACL2 version string, as in state global 'acl2-version.  We
; return (mv major minor incrl taintedp rest), where either major is nil,
; indicating an ill-formed version; or else major, minor, and incrl are natural
; numbers indicating the major, minor, and incrl version, and rest is the part
; of the string starting with #\(, if any, with its initial "(...)"  removed if
; "..." begins with "tainted".  For example, (parse-version "ACL2 Version
; 2.10") is (mv 2 10 0 nil "") and (parse-version "ACL2 Version
; 2.10.1(tainted)(r)") is (mv 2 10 1 t "(r)").

  (let* ((root "ACL2 Version")
         (pos0 (if (and (stringp version)
                        (<= 13 (length version))
                        (equal (subseq version 0 12) root)
                        (or (eql (char version 12) #\Space)
                            (eql (char version 12) #\_)))
                   13
                 nil))
         (pos-lparen (position #\( version))
         (end0 (or pos-lparen
                   (length version)))
         (rest0 (subseq version end0 (length version)))
         (taintedp (and pos-lparen ; optimization
                        (tainted-stringp version)))
         (rest (if taintedp
                   (subseq rest0 (1+ (position #\) rest0)) (length rest0))
                 rest0))
         (from-pos0 (and pos0 (subseq version pos0 end0)))
         (pos1-from-pos0 (and pos0 (position #\. from-pos0)))
         (pos1 (and pos1-from-pos0 (+ pos0 pos1-from-pos0)))
         (major (and pos1 (decimal-string-to-number
                           (subseq version pos0 pos1)
                           (- pos1 pos0) 0)))
         (from-pos1 (and pos1 (subseq version (1+ pos1) end0)))
         (pos2-from-pos1 (and pos1 (position #\. from-pos1)))
         (pos2 (if pos2-from-pos1
                   (+ (1+ pos1) pos2-from-pos1)
                 (and pos1 end0)))
         (minor (and pos2 (decimal-string-to-number
                           (subseq version (1+ pos1) pos2)
                           (1- (- pos2 pos1)) 0)))
         (incrl (if (and pos2 (< pos2 end0))
                    (decimal-string-to-number 
                     (subseq version (1+ pos2) end0)
                     (1- (- end0 pos2))
                     0)
                  0)))
    (mv major minor incrl taintedp rest)))

#-acl2-loop-only
(defun-one-output latest-release-note-string ()
  (mv-let (major minor incrl taintedp rest)
    (parse-version (f-get-global 'acl2-version *the-live-state*))
    (declare (ignore taintedp rest))
    (if (zerop incrl)
        (format nil "note-~s-~s" major minor)
      (format nil "note-~s-~s-~s" major minor incrl))))

(defun earlier-acl2-versionp (version1 version2)

; This function ignores the part of each version string after the first
; parenthesis (if any), including "(tainted)".

  (mv-let (major1 minor1 incrl1 taintedp1 rest1)
    (parse-version version1)
    (declare (ignore taintedp1 rest1))
    (mv-let (major2 minor2 incrl2 taintedp2 rest2)
      (parse-version version2)
      (declare (ignore taintedp2 rest2))
      (cond
       ((or (null major1) (null major2))
        (er hard 'earlier-acl2-versionp
            "We are surprised to find an ACL2 version string, ~x0, that ~
               cannot be parsed."
            (if (null major1)
                version1
              version2)))
       (t
        (or (< major1 major2)
            (and (int= major1 major2)
                 (assert$ (and (natp minor1) (natp minor2))
                          (or (< minor1 minor2)
                              (and (int= minor1 minor2)
                                   (< incrl1 incrl2)))))))))))

(defun acl2-version-r-p (version)
  (let ((p (position #\( version)))
    (and p
         (< (+ p 2) (length version))
         (equal (subseq version p (+ p 3)) "(r)"))))

; We need to check whether an include-book-alist found in a .cert file
; is legal.  It will be checked with respect to the version number
; stored in the file, since we sometimes change the shape of the
; alist.  We also need to convert old-style alists into new-style
; ones.  We do the same thing for every entry in the alist, depending
; on the version.  Rather than repeatedly asking whether the stored
; version is before or after some fixed landmark version number, we
; ask that question once and for all and convert the answer to an integer
; indicating which ``era'' the version was in.  Then we just case split
; on the era.

(defun acl2-version-era (version)
  (cond
   ((earlier-acl2-versionp version "ACL2 Version_2.5")
    0)
   (t 1)))

(defun ttag-alistp (x)

; We don't check that pathnames are absolute, but that isn't important here.

  (cond ((atom x)
         (null x))
        (t (and (consp (car x))
                (symbolp (caar x))
                (string-listp (remove1 nil (cdar x)))
                (ttag-alistp (cdr x))))))

(defun cert-annotationsp (x)
  (case-match x
    (((':SKIPPED-PROOFSP . sp)
      (':AXIOMSP . ap)
      . ttags-singleton)
     (and (member-eq sp '(t nil ?))
          (member-eq ap '(t nil ?))
          (or (null ttags-singleton)
              (case-match ttags-singleton
                (((':TTAGS . ttags))
                 (ttag-alistp ttags))
                (& nil)))))
    (& nil)))

(defun include-book-alist-entryp (era entry)
  (case era
    (0 (and (consp entry)
            (stringp (car entry))         ;;; full-book-name
            (consp (cdr entry))
            (stringp (cadr entry))        ;;; user-book-name
            (consp (cddr entry))
            (stringp (caddr entry))       ;;; familiar-name
            (or (integerp (cdddr entry))  ;;; ev-lst-chk-sum
                (eq (cdddr entry) nil))))
    (t (and (consp entry)
            (stringp (car entry))         ;;; full-book-name
            (consp (cdr entry))
            (stringp (cadr entry))        ;;; user-book-name
            (consp (cddr entry))
            (stringp (caddr entry))       ;;; familiar-name
            (consp (cdddr entry))
            (cert-annotationsp (cadddr entry)) ;;; cert-annotations
            (or (integerp (cddddr entry))      ;;; ev-lst-chk-sum
                (eq (cddddr entry) nil))))))

(defun include-book-alistp1 (era x local-markers-allowedp)
  (cond
   ((atom x) (equal x nil))
   ((and local-markers-allowedp
         (consp (car x))
         (eq (car (car x)) 'local)
         (consp (cdr (car x)))
         (equal (cddr (car x)) nil))
    (and (include-book-alist-entryp era (cadr (car x)))
         (include-book-alistp1 era
                               (cdr x)
                               local-markers-allowedp)))
   (t (and (include-book-alist-entryp era (car x))
           (include-book-alistp1 era
                                 (cdr x)
                                 local-markers-allowedp)))))

(defun include-book-alistp (version x local-markers-allowedp)

; We check whether x is a legal include-book-alist in the given
; version.  If local-markers-allowedp we consider entries of the form
; (LOCAL e) to be legal if e is legal; otherwise, LOCAL is given no
; special meaning.

  (or (include-book-alistp1 (acl2-version-era version)
                            x local-markers-allowedp)

; If it fails to be an alist of the proper shape, but the version is
; 2.5, it could be an old-style one.  We use the strange code below to
; recognize "ACL2 Version 2.5" simply to avoid having that string in
; our code during version bump.  Because we sometimes pass underscore
; versions (as we do with "ACL2 Version_1.8" in chk-certificate-file1)
; we might someday pass "ACL2 Version_2.5" into this code, even though
; it never occurs in books.  So we look for it too.

      (if (or (equal version "ACL2 Version_2.5")
              (and (stringp version)
                   (equal (length version) 16)
                   (equal (subseq version 0 13) "ACL2 Version ")
                   (equal (subseq version 13 16) "2.5")))
          (include-book-alistp1 0 x local-markers-allowedp)
        nil)))

; Now we repeat that for the function that modernizes an alist
; into the current era.

(defun modernize-include-book-alist-entry (era entry)
  (case era
    (0 (list* (car entry)               ;;; full-book-name
              (cadr entry)              ;;; user-book-name
              (caddr entry)             ;;; familiar-name
              '((:SKIPPED-PROOFSP . ?)  ;;; cert-annotations
                (:AXIOMSP . ?)
                (:TTAGS))
              (cdddr entry)))           ;;; ev-lst-chk-sum
    (t entry)))

(defun modernize-include-book-alist1 (era alist)
  (cond
   ((endp alist) nil)
   (t (cons (if (eq (car (car alist)) 'local)
                (list 'local
                      (modernize-include-book-alist-entry
                       era
                       (cadr (car alist))))
              (modernize-include-book-alist-entry era (car alist)))
            (modernize-include-book-alist1 era (cdr alist))))))

(defun modernize-include-book-alist (version alist)

; We modernize the alist to the current ACL2 version.  We assume it has passed
; include-book-alistp with this same version and whatever the appropriate
; LOCAL-allowed flag.  It may or may not contain LOCAL markers.  If so, we
; modernize them.

  (let ((era (acl2-version-era version)))
    (if (include-book-alistp1 era alist t)
        (modernize-include-book-alist1 era alist)
      (modernize-include-book-alist1 0 alist))))

(defrec cert-obj
  ((cmds . pre-alist) . (post-alist . expansion-alist))
  t)

(defun chk-raise-portcullis (version file1 file2 ch skip-pre-alist-chkp
                                     ctx state
                                     suspect-book-action-alist evalp)

; File1 is a book and file2 is its certificate file.  The version string
; recorded with the file is version.  Ch is an open object input channel to the
; certificate.  We have already read past the initial (in-package "ACL2"),
; acl2-version and the :BEGIN-PORTCULLIS-CMDS in ch.  We now read successive
; commands and, if evalp is true, evaluate them in state.  Ld-skip-proofsp is
; 'include-book for this operation because these commands have all been
; successfully carried out in a boot strap world.  If this doesn't cause an
; error, then we read the optional :expansion-alist, the pre- and post- check
; sum alists, and the final check sum.  If these objects are (except the
; optional :expansion-alist) not present or are of the wrong type, or there is
; additional text in the file, or the final check sum is inaccurate, we cause
; an error.

; Unless we are told to ignore the pre-alist, we check that it is a subset of
; the current include-book-alist.  Failure of this check may lead either to an
; error or to the assumption that the book is uncertified, according to the
; suspect-book-action-alist.  If we don't cause an error we return either the
; certificate object, which is a cert-obj record, or else we return nil,
; indicating that the book is presumed uncertified.

  (er-let*
    ((portcullis-cmds
      (if evalp
          (chk-raise-portcullis1 file1 file2 ch ctx state)
        (get-cmds-from-portcullis file1 file2 ch ctx state nil))))
    (mv-let
     (eofp pre-alist state)
     (state-global-let*
      ((infixp nil))
      (read-object ch state))
     (er-let*
      ((expansion-alist
        (cond
         (eofp (er soft ctx *ill-formed-certificate-msg* file1 file2))
         ((eq pre-alist :expansion-alist)
          (mv-let
           (eofp expansion-alist state)
           (state-global-let*
            ((infixp nil))
            (read-object ch state))
           (cond
            (eofp
             (er soft ctx *ill-formed-certificate-msg* file1 file2))
            (t (value expansion-alist)))))
         (t (value nil))))
       (pre-alist
        (cond
         ((eq pre-alist :expansion-alist)
          (mv-let
           (eofp pre-alist state)
           (state-global-let*
            ((infixp nil))
            (read-object ch state))
           (cond
            (eofp (er soft ctx *ill-formed-certificate-msg* file1 file2))
            (t (value pre-alist)))))
         (t (value pre-alist))))
       (modern-pre-alist
        (cond ((include-book-alistp version pre-alist nil)
               (value (modernize-include-book-alist version pre-alist)))
              (t (er soft ctx *ill-formed-certificate-msg* file1 file2)))))
      (let ((actual-alist (global-val 'include-book-alist (w state))))
        (mv-let
         (eofp post-alist3 state)
         (state-global-let*
          ((infixp nil))
          (read-object ch state))
         (er-let*
          ((modern-post-alist3
            (cond
             ((include-book-alistp version post-alist3 t)
              (value
               (modernize-include-book-alist version post-alist3)))
             (t (er soft ctx *ill-formed-certificate-msg* file1 file2)))))
          (cond
           (eofp
            (er soft ctx *ill-formed-certificate-msg* file1 file2))
           (t
            (mv-let
             (eofp chk-sum1 state)
             (state-global-let*
              ((infixp nil))
              (read-object ch state))
             (cond
              ((or eofp (not (integerp chk-sum1)))
               (er soft ctx *ill-formed-certificate-msg* file1 file2))
              (t
               (mv-let
                (eofp temp state)
                (state-global-let*
                 ((infixp nil))
                 (read-object ch state))
                (declare (ignore temp))
                (cond
                 ((not eofp)
                  (er soft ctx *ill-formed-certificate-msg* file1 file2))
                 (t
                  (let ((certificate-object
                         (make cert-obj
                               :cmds portcullis-cmds
                               :pre-alist pre-alist
                               :post-alist post-alist3
                               :expansion-alist expansion-alist))
                        (modern-certificate-object
                         (make cert-obj
                               :cmds portcullis-cmds
                               :pre-alist modern-pre-alist
                               :post-alist modern-post-alist3
                               :expansion-alist expansion-alist)))
                    (mv-let
                     (chk-sum2 state)
                     (check-sum-obj certificate-object state)
                     (cond
                      ((or (not (integerp chk-sum2))
                           (not (int= chk-sum1 chk-sum2)))
                       (er soft ctx *ill-formed-certificate-msg*
                           file1 file2))
                      ((and (not skip-pre-alist-chkp)
                            (not (include-book-alist-subsetp
                                  modern-pre-alist
                                  actual-alist)))

; Note: Sometimes I have wondered how the expression above deals with
; LOCAL entries in the alists in question, because
; include-book-alist-subsetp does not handle them.  The answer is:
; there are no LOCAL entries in a pre-alist because we prohibit local
; events in the portcullis commands.

                       (include-book-er
                        file1 file2
                        (cons
                         "The portcullis for ~x0 requires ~*3 but we have ~*4."
                         (list
                          (cons #\3 (tilde-*-book-check-sums-phrase
                                     t
                                     modern-pre-alist
                                     actual-alist))
                          (cons #\4 (tilde-*-book-check-sums-phrase
                                     nil
                                     modern-pre-alist
                                     actual-alist))))
                        :uncertified-okp
                        suspect-book-action-alist
                        ctx state))
                      (t (value modern-certificate-object)))))))))
              )))))))))))

(defun chk-certificate-file1 (file1 file2 ch skip-pre-alist-chkp
                                    ctx state suspect-book-action-alist
                                    evalp)

; File1 is a book name and file2 is its associated certificate file name.  Ch
; is a channel to file2.  We assume we have read the initial (in-package
; "ACL2") and temporarily slipped into that package.  Our caller will restore
; it.  We now read the rest of file2 and either open the portcullis (skipping
; evaluation if evalp is nil) and return a cert-obj record or nil if we are
; assuming the book, or we cause an error.

; The code below is tedious and we here document it.  The first thing we look
; for is the ACL2 Version number printed immediately after the in-package.
; This function is made more complicated by four facts.  First, until Version
; 1.9, certificates did not include the version but we will treat such early
; books as though they were certified with Version 1.8.  Second, we do not know
; for sure that the certificate file is well-formed in any version.  Third, we
; do not know whether include-book-er causes an error or just prints a warning
; (because that is determined by suspect-book-action-alist and the values of
; the state globals defaxioms-okp-cert and skip-proofs-okp-cert).  Suppose we
; read a purported version string, val, that does not match the current
; acl2-version.  Then we cause an include-book-er which may or may not signal
; an error.  If it does not then we are to assume the uncertified book so we
; must proceed with the certificate check as though the version were ok.
; Basically this means we want to call chk-raise-portcullis, but we must first
; make sure we've read to the beginning of the portcullis.  If val is
; :BEGIN-PORTCULLIS-CMDS, then this file is probably a well-formed Version 1.8
; file and we are properly positioned.  If val looks like an ACL2 Version
; string, then this file is probably a well-formed Version 1.9+ file and we
; must read the :BEGIN-PORTCULLIS-CMDS before proceeding.  Otherwise, this
; isn't well-formed and we cause an error.

; The fourth complication is that until late in the development of
; Version 2.5, include-book-alist entries were of the form
; (full-book-name user-book-name familiar-name . ev-lst-chk-sum)
; instead of the current (full-book-name user-book-name familiar-name
; cert-annotations . ev-lst-chk-sum).  Thus, when we read an alist, we
; must modernize it.

  (mv-let
   (eofp version state)
   (state-global-let* ((infixp nil)) (read-object ch state))
   (cond
    (eofp (er soft ctx *ill-formed-certificate-msg* file1 file2))
    (t (let* ((version-okp (equal version (f-get-global 'acl2-version state)))
              (version-okp-tainted
               (and (not version-okp)
                    (mv-let (major1 minor1 incrl1 taintedp1 rest1)
                      (parse-version version)
                      (declare (ignore taintedp1))
                      (mv-let (major2 minor2 incrl2 taintedp2 rest2)
                        (parse-version (f-get-global 'acl2-version state))
                        (declare (ignore taintedp2))
                        (and (eql major1 major2)
                             (eql minor1 minor2)
                             (equal rest1 rest2)
                             (if (eql incrl1 incrl2)
                                 'tainted
                               'incrl-mismatch)))))))
         (cond
          ((or version-okp
               (and version-okp-tainted
                    (f-get-global 'tainted-okp state)))
           (mv-let
             (eofp key state)
             (state-global-let* ((infixp nil)) (read-object ch state))
             (cond
              ((or eofp (not (eq key :begin-portcullis-cmds)))
               (er soft ctx *ill-formed-certificate-msg* file1 file2))
              (t (er-let* ((cert-obj
                            (chk-raise-portcullis version file1 file2 ch
                                                  skip-pre-alist-chkp
                                                  ctx state
                                                  suspect-book-action-alist
                                                  evalp)))
                   (cond (version-okp-tainted
                          (pprogn (let ((certify-book-info (f-get-global
                                                            'certify-book-info
                                                            state)))
                                    (if (stringp certify-book-info)
                                        (f-put-global 'certify-book-info
                                                      (list certify-book-info)
                                                      state)
                                      state))
                                  (warning$ ctx "Tainted"
                                            "The book ~x0 is being treated as ~
                                             certified even though ~
                                             ~#1~[~@2~/~@3~].  See :DOC ~
                                             set-tainted-okp."
                                            file1
                                            (if (eq version-okp-tainted
                                                    'tainted)
                                                0
                                              1)
                                            "some potential version ~
                                             discrepancies were encountered ~
                                             during its certification"
                                            `("the ``incrl'' field of its ~
                                              acl2-version, ~xa, differs from ~
                                              that of the current ~
                                              acl2-version, ~xb; see :DOC ~
                                              version"
                                              (#\a . ,version)
                                              (#\b . ,(f-get-global
                                                       'acl2-version 
                                                       state))))
                                  (value cert-obj)))
                         (t (value cert-obj))))))))
          ((not (equal (acl2-version-r-p (f-get-global 'acl2-version state))
                       (acl2-version-r-p version)))
           (er soft ctx
               "We do not permit ACL2 books to be processed by ACL2(r) or vice ~
                versa.  The book ~x0 was last certified with ~s1 but this is ~
                ~s2."
               file1
               version
               (f-get-global 'acl2-version state)))
          (t
           (mv-let
             (erp val state)
             (include-book-er
              file1 file2
              (cons "~x0 was apparently certified with ~sa.  The inclusion of ~
                      this book in the current ACL2 may render this ACL2 ~
                      sesion unsound!  We recommend you recertify the book ~
                      with the current version, ~sb.  See :DOC version.  No ~
                      compiled file will be loaded with this book.~@c"
                    (list (cons #\a (if (eq version :begin-portcullis-cmds)
                                        "ACL2 Version 1.8"
                                      version))
                          (cons #\b (f-get-global 'acl2-version state))
                          (cons #\c
                                (case version-okp-tainted
                                  (incrl-mismatch
                                   "~|  NOTE that only the ``incrl'' version ~
                                    fields disagree.  See :DOC set-tainted-okp ~
                                    for an untrusted workaround for this ~
                                    problem.")
                                  (tainted
                                   "~|  NOTE that the above book was certified ~
                                    in an ACL2 world in which some version ~
                                    discrepancies were ignored.  See :DOC ~
                                    set-tainted-okp for an untrusted ~
                                    workaround for this problem.")
                                  (otherwise "")))))
              :uncertified-okp
              suspect-book-action-alist
              ctx state)

; Because the book was certified under a different version of ACL2, we
; consider it uncertified and, at best, return nil rather than a
; certificate object below.  Of course, we might yet cause an error.

             (cond
              (erp (mv erp val state))
              ((eq version :begin-portcullis-cmds)
               (er-progn
                (chk-raise-portcullis "ACL2 Version_1.8"
                                      file1 file2 ch skip-pre-alist-chkp
                                      ctx state
                                      suspect-book-action-alist
                                      t)
                (value nil)))
              ((and (stringp version)
                    (<= 13 (length version))
                    (equal (subseq version 0 13) "ACL2 Version "))
               (mv-let
                 (eofp key state)
                 (state-global-let* ((infixp nil)) (read-object ch state))
                 (cond
                  ((or eofp (not (eq key :begin-portcullis-cmds)))
                   (er soft ctx *ill-formed-certificate-msg* file1 file2))
                  (t (er-progn
                      (chk-raise-portcullis version file1 file2 ch
                                            skip-pre-alist-chkp ctx state
                                            suspect-book-action-alist t)
                      (value nil))))))
              (t (er soft ctx *ill-formed-certificate-msg* file1
                     file2)))))))))))

(defun chk-certificate-file (file1 skip-pre-alist-chkp ctx state
                                   suspect-book-action-alist evalp)

; File1 is a full book name.  We see whether there is a certificate on file for
; it.  If so, and we can get past the portcullis (evaluating it if evalp is
; true), we return the certificate object, a cert-obj record, or nil if we
; presume the book is uncertified.

; This function may actually execute some events or even some DEFPKGs as part
; of the raising of the portcullis in the case that evalp is true.  If
; skip-pre-alist-chkp is t, we do not enforce the requirement that the books
; included by the portcullis commands have the specified check sums.  This
; feature is used when we use this function to recover from an old certificate
; the portcullis commands to recertify the file.

; We make the convention that if a file has no certificate or has an invalid
; certificate, we will either assume it anyway or cause an error depending on
; suspect-book-action-alist.  In the case that we pronouce this book
; uncertified, we return nil.

  (let ((file2 (convert-book-name-to-cert-name file1)))
    (mv-let
     (ch state)
     (open-input-channel file2 :object state)
     (cond
      ((null ch)
       (include-book-er file1 file2
                        "There is no certificate on file for ~x0."
                        :uncertified-okp
                        suspect-book-action-alist
                        ctx state))
      (t (er-let*
          ((pkg 
            (state-global-let*
             ((infixp nil))
             (chk-in-package ch file2 ctx state))))
          (cond
           ((not (equal pkg "ACL2"))
            (er soft ctx *ill-formed-certificate-msg* file1 file2))
           (t
            (state-global-let*
             ((current-package "ACL2"))
             (mv-let (error-flg val state)
                     (chk-certificate-file1 file1 file2 ch
                                            skip-pre-alist-chkp
                                            ctx state
                                            suspect-book-action-alist evalp)
                     (pprogn (close-input-channel ch state)
                             (mv error-flg val state))))))))))))

; All of the above is used during an include-book to verify that a
; certificate is well-formed and to raise the portcullis of the book.
; It happens that the code is also used by certify-book to recover the
; portcullis of a book from an old certificate.  We now continue with
; certify-book's checking, which next moves on to the question of
; whether the environment in which certify-book was called is actually
; suitable for a certification.

(defun chk-acceptable-certify-book1 (file k cmds wrld-segs wrld-list names
                                          wrld ctx state)

; This function is checking the appropriateness of the environment in which
; certify-book is called.

; This subroutine is called after we have the k proposed portcullis commands
; and wrld.  It must be the case that cmds is returned by (get-portcullis-cmds
; wrld nil nil nil names ctx state).  We supply cmds simply because the caller
; already has it.

; Unless we cause an error, we return the portcullis of the file, namely (cmds
; . pre-alist).

  (let* ((pre-alist (global-val 'include-book-alist wrld))
         (uncert-books (collect-uncertified-books pre-alist)))
    (cond
     ((not (eq (default-defun-mode wrld) :logic))
      (er soft ctx
          "Books must be certified in :LOGIC mode.  The current mode is ~x0."
          (default-defun-mode wrld)))
     ((and (not (integerp k))
           (not (eq k '?)))
      (er soft ctx
          "The second argument to certify-book must be either ~x0, ~x1, or an ~
           integer.  You supplied ~x2.  See :DOC certify-book."
          t '? k))
     ((and (not (equal k (length cmds)))
           (not (eq k '?)))
      (er soft ctx
          "You indicated that the portcullis for ~x0 would be of ~
           length ~x1 but it is actually of length ~x2.  Perhaps you ~
           had better inspect the world and call certify-book again."
          file k (length cmds)))
     ((assoc-equal file pre-alist)

; Why do we do this?  By insuring that file is not in the include-book-alist
; initially, we ensure that it gets into the alist only at the end when we
; include-book the book.  This lets us cdr it off.  If it happened to be
; the alist initially, then the include-book would not add it and the cdr
; wouldn't remove it.  See the end of the code for certify-book.

      (er soft ctx
          "We cannot certify ~x0 in a world in which it has already ~
           been included."
          file))
     (t (cond
         (uncert-books
          (er soft ctx
              "It is impossible to certify any book in the current world ~
               because it is built upon ~*0 which ~#1~[is~/are~] uncertified."
              (tilde-*-&v-strings '& uncert-books #\,)
              uncert-books))
         ((eq wrld-segs :omitted)
          (value (cons cmds pre-alist)))

; Now that we know we have a list of embedded event forms, we are ready to
; replace relative pathnames by absolute pathnames.  See fix-portcullis-cmds.

         (t (er-let* ((fixed-cmds
                       (fix-portcullis-cmds cmds wrld-segs wrld-list
                                            (global-val 'known-package-alist
                                                        (car wrld-list))
                                            nil names
                                            (os wrld) ctx state)))
                     (value (cons fixed-cmds pre-alist)))))))))

; We next develop chk-well-formed-ttags.  But first we need to develop
; extend-pathname, which is called by translate-book-names, which supports
; chk-well-formed-ttags.

(defun remove-after-last-directory-separator (p)
  (let* ((p-rev (reverse p))
         (posn (position *directory-separator* p-rev)))
    (if posn
        (subseq p 0 (1- (- (length p) posn)))
      (er hard 'remove-after-last-directory-separator
          "Implementation error!  Unable to handle a directory string."))))

(defun merge-using-dot-dot (p s)

; P is a directory pathname without the final "/".  S is a pathname (for a file
; or a directory) that may start with any number of sequences "../" and "./".
; We want to "cancel" the leading "../"s in s against directories at the end of
; p, and eliminate leading "./"s from s (including leading "." if that is all
; of s).  The result should syntactically represent a directory (end with a "/"
; or "."  or be "") if and only if s syntactically represents a directory.

; This code is intended to be simple, not necessarily efficient.

  (cond
   ((equal p "") s)
   ((equal s "..")
    (concatenate 'string
                 (remove-after-last-directory-separator p)
                 *directory-separator-string*))
   ((equal s ".")
    (concatenate 'string
                 p
                 *directory-separator-string*))
   ((and (>= (length s) 3)
         (eql (char s 0) #\.)
         (eql (char s 1) #\.)
         (eql (char s 2) #\/))
    (merge-using-dot-dot (remove-after-last-directory-separator p)
                         (subseq s 3 (length s))))
   ((and (>= (length s) 2)
         (eql (char s 0) #\.)
         (eql (char s 1) #\/))
    (merge-using-dot-dot p (subseq s 2 (length s))))
   (t
    (concatenate 'string p *directory-separator-string* s))))

(defun our-merge-pathnames (p s)

; This is something like the Common Lisp function merge-pathnames.  P and s are
; (Unix-style) pathname strings, where s is a relative pathname.  (If s may be
; an absolute pathname, use extend-pathname instead.)  We allow p to be nil,
; which is a case that arises when p is (f-get-global 'connected-book-directory
; state) during boot-strapping; otherwise p should be an absolute directory
; pathname (though we allow "" as well).

  (cond
   ((and (not (equal s ""))
         (eql (char s 0) *directory-separator*))
    (er hard 'our-merge-pathnames
        "Attempt to merge with an absolute filename, ~p0.  Please contact the ~
         ACL2 implementors."
        s))
   ((or (null p) (equal p ""))
    s)
   ((stringp p) ; checked because of structured pathnames before Version_2.5
    (merge-using-dot-dot
     (if (eql (char p (1- (length p)))
              *directory-separator*)
         (subseq p 0 (1- (length p)))
       p)
     s))
   (t
    (er hard 'our-merge-pathnames
        "The first argument of our-merge-pathnames must be a string, ~
         but the following is not:  ~p0."
        p))))

(defun extend-pathname (dir file-name os)

; Dir is a string representing an absolute directory name, and file-name is a
; string representing a file or directory name.  We want to extend dir by
; file-name if subdir is relative, and otherwise return file-name.

  (cond
   ((absolute-pathname-string-p file-name nil os)
    file-name)
   (t
    (our-merge-pathnames dir file-name))))

(defun registered-full-book-name (filename ctx state)

; Returns an error triple whose value is the name of the file corresponding to
; filename, a full book name, that is found in the filename's certificate file,
; if such exists.  Otherwise the value is nil.

  (state-global-let*
   ((inhibit-output-lst *valid-output-names*))
   (mv-let (erp cert-obj state)
           (chk-certificate-file filename t ctx state
                                 '((:uncertified-okp . t)
                                   (:defaxioms-okp t)
                                   (:skip-proofs-okp t))
                                 nil)
           (value (and (null erp)
                       cert-obj
                       (caar (access cert-obj cert-obj :post-alist)))))))

(defun translate-book-names (filenames ctx cbd os state acc)
  (declare (xargs :guard (true-listp filenames))) ; one member can be nil
  (cond ((endp filenames)
         (value (reverse acc)))
        ((null (car filenames))
         (translate-book-names (cdr filenames) ctx cbd os state
                               (cons nil acc)))
        (t (let ((file0 (extend-pathname cbd
                                         (possibly-add-lisp-extension
                                          (car filenames))
                                         os)))
             (er-let* ((file1 (registered-full-book-name file0 ctx state)))
                      (translate-book-names (cdr filenames) ctx cbd os state
                                            (cons (or file1 file0) acc)))))))

(defun fix-ttags (ttags ctx cbd os state seen acc)

; Seen is a list of symbols, nil at the top level.  We use this argument to
; enforce the lack of duplicate ttags.  Acc is the accumulated list of ttags to
; return, which may include symbols and lists (sym file1 ... filek).

  (declare (xargs :guard (true-listp ttags)))
  (cond ((endp ttags)
         (value (reverse acc)))
        (t (let* ((ttag (car ttags))
                  (sym (if (consp ttag) (car ttag) ttag)))
             (cond
              ((not (and (symbolp sym)
                         sym
                         (or (atom ttag)
                             (string-listp (remove1-eq nil (cdr ttag))))))
               (er soft ctx
                   "A :ttags value for certify-book or include-book must ~
                    either be the keyword :ALL or else a list, each of whose ~
                    members is one of the following: a non-nil symbol, or the ~
                    CONS of a non-nil symbol onto a true list consisting of ~
                    strings and at most one nil.  The value ~x0 is thus an ~
                    illegal member of such a list."
                   ttag))
              ((member-eq sym seen)
               (er soft ctx
                   "A :ttags list may not mention the same ttag symbol more ~
                    than once, but the proposed list mentions ~x0 more than ~
                    once."
                   sym))
              ((symbolp ttag)
               (fix-ttags (cdr ttags) ctx cbd os state (cons sym seen)
                          (cons sym acc)))
              (t
               (er-let* ((full-book-names
                          (translate-book-names (cdr ttag) ctx cbd os state
                                                nil)))
                        (fix-ttags (cdr ttags) ctx cbd os state (cons sym seen)
                                   (cons (cons sym full-book-names)
                                         acc)))))))))

(defun chk-well-formed-ttags (ttags cbd ctx state)
  (cond ((or (null ttags) ; optimization
             (eq ttags :all))
         (value ttags))
        ((not (true-listp ttags))
         (er soft ctx
             "A valid list of ttags must be a true list, unlike: ~x0."
             ttags))
        (t (fix-ttags ttags ctx cbd (os (w state)) state nil nil))))

(defun cbd-fn (state)
  (or (f-get-global 'connected-book-directory state)
      (er hard 'cbd
          "The connected book directory has apparently not yet been set.  ~
           This could be a sign that the top-level ACL2 loop, generally ~
           entered using (LP), has not yet been entered.")))

(defmacro cbd nil
  ":Doc-Section Books

  connected book directory string~/
  ~bv[]
  Example:
  ACL2 !>:cbd
  \"/usr/home/smith/\"
  ~ev[]
  The connected book directory is a nonempty string that specifies a
  directory as an absolute pathname.  (~l[pathname] for a
  discussion of file naming conventions.)  When ~ilc[include-book] is given
  a relative book name it elaborates it into a full book name,
  essentially by appending the connected book directory string to the
  left and ~c[\".lisp\"] to the right.  (For details,
  ~pl[book-name] and also ~pl[full-book-name].)  Furthermore,
  ~ilc[include-book] temporarily sets the connected book directory to the
  directory string of the resulting full book name so that references
  to inferior ~il[books] in the same directory may omit the directory.
  ~l[set-cbd] for how to set the connected book directory string.~/
  ~bv[]
  General Form:
  (cbd)
  ~ev[]
  This is a macro that expands into a term involving the single free
  variable ~ilc[state].  It returns the connected book directory string.

  The connected book directory (henceforth called the ``~c[cbd]'') is
  used by ~ilc[include-book] to elaborate the supplied book name into a
  full book name (~pl[full-book-name]).  For example, if the ~c[cbd]
  is ~c[\"/usr/home/smith/\"] then the elaboration of the ~il[book-name]
  ~c[\"project/task-1/arith\"] (to the ~c[\".lisp\"] extension) is
  ~c[\"/usr/home/smith/project/task-1/arith.lisp\"].  That
  ~il[full-book-name] is what ~il[include-book] opens to read the
  source text for the book.

  The ~c[cbd] may be changed using ~ilc[set-cbd] (~pl[set-cbd]).
  Furthermore, during the processing of the ~il[events] in a book,
  ~ilc[include-book] sets the ~c[cbd] to be the directory string of the
  ~il[full-book-name] of the book.  Thus, if the ~c[cbd] is
  ~c[\"/usr/home/smith/\"] then during the processing of ~il[events] by
  ~bv[]
  (include-book \"project/task-1/arith\")
  ~ev[]
  the ~c[cbd] will be set to ~c[\"/usr/home/smith/project/task-1/\"].
  Note that if ~c[\"arith\"] recursively includes a subbook, say
  ~c[\"naturals\"], that resides on the same directory, the
  ~ilc[include-book] event for it may omit the specification of that
  directory.  For example, ~c[\"arith\"] might contain the event
  ~bv[]
    (include-book \"naturals\").
  ~ev[]
  In general, suppose we have a superior book and several inferior
  ~il[books] which are included by ~il[events] in the superior book.  Any
  inferior book residing on the same directory as the superior book
  may be referenced in the superior without specification of the
  directory.

  We call this a ``relative'' as opposed to ``absolute'' naming.  The
  use of relative naming is preferred because it permits ~il[books]
  (and their accompanying inferiors) to be moved between directories
  while maintaining their ~il[certificate]s and utility.  Certified
  ~il[books] that reference inferiors by absolute file names are unusable
  (and rendered uncertified) if the inferiors are moved to new
  directories.

  ~em[Technical Note and a Challenge to Users:]

  After elaborating the book name to a full book name, ~ilc[include-book]
  opens a channel to the file to process the ~il[events] in it.  In some
  host Common Lisps, the actual file opened depends upon a notion of
  ``connected directory'' similar to our connected book directory.
  Our intention in always elaborating book names into absolute
  filename strings (~pl[pathname] for terminology) is to
  circumvent the sensitivity to the connected directory.  But we may
  have insufficient control over this since the ultimate file naming
  conventions are determined by the host operating system rather than
  Common Lisp (though, we do check that the operating system
  ``appears'' to be one that we ``know'' about).  Here is a question,
  which we'll pose assuming that we have an operating system that
  calls itself ``Unix.''  Suppose we have a file name, filename, that
  begins with a slash, e.g., ~c[\"/usr/home/smith/...\"].  Consider two
  successive invocations of CLTL's
  ~bv[]
  (open filename :direction :input)
  ~ev[]
  separated only by a change to the operating system's notion of
  connected directory.  Must these two invocations produce streams to
  the same file?  A candidate string might be something like
  ~c[\"/usr/home/smith/*/usr/local/src/foo.lisp\"] which includes some
  operating system-specific special character to mean ``here insert
  the connected directory'' or, more generally, ``here make the name
  dependent on some non-ACL2 aspect of the host's state.''  If such
  ``tricky'' name strings beginning with a slash exist, then we have
  failed to isolate ACL2 adequately from the operating system's file
  naming conventions.  Once upon a time, ACL2 did not insist that the
  ~c[cbd] begin with a slash and that allowed the string
  ~c[\"foo.lisp\"] to be tricky because if one were connected to
  ~c[\"/usr/home/smith/\"] then with the empty ~c[cbd] ~c[\"foo.lisp\"]
  is a full book name that names the same file as
  ~c[\"/usr/home/smith/foo.lisp\"].  If the actual file one reads is
  determined by the operating system's state then it is possible for
  ACL2 to have two distinct ``full book names'' for the same file, the
  ``real'' name and the ``tricky'' name.  This can cause ACL2 to
  include the same book twice, not recognizing the second one as
  redundant."

  `(cbd-fn state))

(defun chk-acceptable-certify-book (book-name full-book-name k ctx state
                                              suspect-book-action-alist)

; This function determines that it is ok to run certify-book on
; full-book-name and k.  Unless an error is caused we return a pair
; (cmds . pre-alist) that contains the two parts of the portcullis.
; If k is t it means that the existing certificate file specifies the
; intended portcullis.  It also means that there must be such a file
; and that we are in the ground zero state.  If all those things check
; out, we will actually carry out the portcullis to get into the right
; state by the time we return.

  (let ((names               

; Warning: If you change the list of names below, be sure to change it
; in the call of note-certification-world in certify-book-fn.

         (cons 'defpkg *primitive-event-macros*))
        (wrld (w state)))
    (er-progn
     (cond ((ld-skip-proofsp state)
            (er soft ctx
                "Certify-book must be called with ld-skip-proofsp set to nil."))
           ((f-get-global 'in-local-flg state)
            (er soft ctx
                "Certify-book may not be called inside a LOCAL command."))
           ((global-val 'skip-proofs-seen wrld)
            (er soft ctx
                "At least one command in the current ACL2 world was executed ~
                 while the value of state global variable '~x0 was not ~
                 nil:~|~%  ~y1~%(If you did not explicitly use ~
                 set-ld-skip-proofsp or call ld with :ld-skip-proofsp not ~
                 nil, then some other function did so, for example, rebuild.) ~
                 Certification is therefore not allowed in this world.  If ~
                 the intention was for proofs to be skipped for one or more ~
                 events in the certification world, consider wrapping those ~
                 events explicitly in skip-proofs forms.  See :DOC ~
                 skip-proofs."
                'ld-skip-proofsp
                (global-val 'skip-proofs-seen wrld)))
           ((global-val 'redef-seen wrld)
            (er soft ctx
                "At least one command in the current ACL2 world was executed ~
                 while the value of state global variable '~x0 was not ~
                 nil:~|~%  ~y1~%Certification is therefore not allowed in ~
                 this world.  You can use :ubt to undo back through this ~
                 command; see :DOC ubt."
                'ld-redefinition-action
                (global-val 'redef-seen wrld)))
           ((ttag wrld)

; We disallow active ttag at certification time because we don't want to think
; about certain oddly redundant defttag events.  Consider for example executing
; (defttag foo), and then certifying a book containing the following forms,
; (certify-book "foo" 1 nil :ttags ((foo nil))), indicating that ttag foo is
; only active at the top level, not inside a book.

; (defttag foo)

; (defun f ()
;   (declare (xargs :mode :program))
;   (sys-call "ls" nil))

; The defttag expands to a redundant table event, hence would be allowed.
; Perhaps this is OK, but it is rather scary since we then have a case of a
; book containing a defttag of which there is no evidence of this in any "TTAG
; NOTE" string or in the book's certificate.  While we see no real problem
; here, since the defttag really is ignored, still it's very easy for the user
; to work around this situation by executing (defttag nil) before
; certification; so we take this conservative approach.

            (er soft ctx
                "It is illegal to certify a book while there is an active ~
                 ttag, in this case, ~x0.  Consider undoing the corresponding ~
                 defttag event (see :DOC ubt) or else executing ~x1.  See ~
                 :DOC defttag."
                (ttag wrld)
                '(defttag nil)))
           (t (value nil)))
     (chk-book-name book-name full-book-name ctx state)
     (er-let*
      ((certp (certificate-filep full-book-name state)))
      (mv-let
       (erp cmds wrld-segs wrld-list state)
       (get-portcullis-cmds wrld nil nil nil names ctx state)
       (cond
        (erp (silent-error state))
        ((eq k t)
         (cond
          (cmds
           (er soft ctx
               "When you tell certify-book to recover the certification world ~
                from the old certificate, you must call certify-book in the ~
                initial ACL2 logical world -- so we don't have to worry about ~
                the certification world  clashing with the existing logical ~
                world.  But you are not in the initial logical world.  Use ~
                :pbt 1 to see the world."))
          ((not certp)
           (er soft ctx
               "There is no certificate on file for ~x0.  But you told ~
                certify-book to recover the certi~-fication world from the ~
                old certificate.  You will have to construct the ~
                certi~-fication world by hand (by executing the desired ~
                commands in the current logical world) and then call ~
                certify-book again."
               full-book-name))
          (t

; So k is t, we are in the initial world, and there is a certificate file
; from which we can recover the portcullis.  Do it.

           (er-let*
            ((cert-obj
              (chk-certificate-file full-book-name t ctx state
                                    (cons '(:uncertified-okp . nil)
                                          suspect-book-action-alist)
                                    t))
             (cert-obj-cmds (value (and cert-obj
                                        (access cert-obj cert-obj :cmds)))))
            (chk-acceptable-certify-book1 full-book-name
                                          (length cert-obj-cmds) ;; k
                                          cert-obj-cmds ;; cmds
                                          :omitted ;; wrld-segs
                                          wrld-list
                                          names
                                          (w state)
                                          ctx state)))))
        (t (chk-acceptable-certify-book1 full-book-name k cmds wrld-segs
                                         wrld-list names wrld ctx
                                         state))))))))

(defun print-objects (lst ch state)
  (cond ((null lst) state)
        (t (pprogn (print-object$ (car lst) ch state)
                   (print-objects (cdr lst) ch state)))))

(defun new-post-alist3 (alist old old-length new)

; Search through alist, viewed as a tree, and for any string with prefix old
; (which has length old-length), replace that prefix with new.

  (cond ((atom alist)
         alist)
        (t (let* ((a (car alist))
                  (b (cond ((consp a)
                            (new-post-alist3 a old old-length new))
                           ((and (stringp a)
                                 (> (length a) old-length)
                                 (equal old (subseq a 0 old-length)))
                            (concatenate 'string new (subseq a old-length
                                                             (length a))))
                           (t a))))
             (cons b (new-post-alist3 (cdr alist) old old-length new))))))

(defun make-certificate-file1 (file portcullis certification-file post-alist3
                                    expansion-alist ctx state)

; See make-certificate-file.

  (mv-let
    (chk-sum state)
    (check-sum-obj (make cert-obj
                         :cmds (car portcullis)
                         :pre-alist (cdr portcullis)
                         :post-alist post-alist3
                         :expansion-alist expansion-alist)
                   state)
    (cond
     ((not (integerp chk-sum))
      (value (er hard ctx
                 "Check-sum-obj returned a non-integerp value on the ~
                  portcullis and post-alist3!")))
     (t
      (mv-let
        (ch state)
        (open-output-channel certification-file :object state)
        (cond
         ((null ch)
          (er soft ctx
              "We cannot open a certificate file for ~x0.  The file ~
               we tried to open for output was ~x1."
              file
              certification-file))
         (t (state-global-let*
             ((current-package "ACL2"))
             (pprogn
              (print-object$ '(in-package "ACL2") ch state)
              (print-object$
               (if (consp (f-get-global 'certify-book-info state))
                   (taint-string (f-get-global 'acl2-version state))
                 (f-get-global 'acl2-version state))
               ch state)
              (print-object$ :BEGIN-PORTCULLIS-CMDS ch state)
              (print-objects (car portcullis) ch state)
              (print-object$ :END-PORTCULLIS-CMDS ch state)
              (cond (expansion-alist
                     (pprogn (print-object$ :EXPANSION-ALIST ch state)
                             (print-object$ expansion-alist ch state)))
                    (t state))
              (print-object$ (cdr portcullis) ch state)
              (print-object$ post-alist3 ch state)
              (print-object$ chk-sum ch state)
              (close-output-channel ch state)
              (value certification-file))))))))))

(defun make-certificate-file (file portcullis post-alist1 post-alist2
                                   expansion-alist ctx state)

; We assume file satisfies chk-book-name.  The portcullis is a pair (cmds
; . pre-alist), where cmds is the list of portcullis commands that created the
; world in which the certification was done, and pre-alist is the
; include-book-alist just before certification was done.  Post-alist1 is the
; include-book-alist after proving the events in file and post-alist2 is the
; include-book-alist after just including the events in file.  If they are
; different it is because the book included some subbooks within LOCAL forms
; and those subbooks did not get loaded for post-alist2.

; To verify that a subsequent inclusion is ok, we really only need
; post-alist2.  That is, if the book included some LOCAL subbook then it is
; not necessary that that subbook even exist when we include the main book.
; On the other hand, it might be useful to know what version of the subbook we
; used during certification, although the code at the moment makes no use of
; that.  So we massage post-alist1 so that any subbook in it that is not in
; post-alist2 is marked LOCAL.  Thus, post-alist3, below, will be of the form

; ((full1 user1 familiar1 cert-annotations1 . chk-sum1)
;  ...
;  (LOCAL (fulli useri familiari cert-annotationsi . chk-sumi))
;  ...
;  (fullk userk familiark cert-annotationsk . chk-sumk))

; and thus is not really an include-book-alist.  By deleting the LOCAL
; elements from it we obtain post-alist2.

; We write a certificate file for file.  The certificate file has the
; following form:

; (in-package "ACL2")
; "ACL2 Version x.y"      ; but see below, regarding tainting
; :BEGIN-PORTCULLIS-CMDS  ; this is here just to let us check that the file
; cmd1                    ; is not a normal list of events.
; ...
; cmdk
; :END-PORTCULLIS-CMDS
; pre-alist
; post-alist3
; chk-sum

; where chk-sum is the check sum of ((cmds . pre-alist) . post-alist3).

; The reason the portcullis commands are written this way, rather than
; as a single object, is that we can't read them all at once since
; they may contain DEFPKGs.  We have to read and eval the cmdi
; individually.

; Optionally, create .cert.final file as well; see comment below.

; Finally, if the book is to be tainted, we taint the acl2-version in the
; certificate.  However, this function is not responsible for checking if
; tainting is legal; such a check should already have been made if we are to
; taint the book.

  (let ((certification-file (convert-book-name-to-cert-name file))
        (post-alist3 (mark-local-included-books post-alist1 post-alist2)))
    (er-progn

; For Debian release:

; A .cert.final file is created if state globals 'old-certification-dir and
; 'new-certification-dir are set to strings.  For example, in
; acl2-customization.lisp you might put:

; (f-put-global 'old-certification-dir "/fix/debian/acl2/acl2-2.9.4/books" state)
; (f-put-global 'new-certification-dir "/usr/share/acl2-2.9.4/books" state)

; This will create extension
; .cert.final instead of .cert, with post-alist3 fixed up so that for each
; string with prefix equal to the value of state global 'old-certification-dir,
; that prefix is replaced by the value of state global 'new-certification-dir.

     (let ((old-dir
            (and (f-boundp-global 'old-certification-dir state)
                 (f-get-global 'old-certification-dir state)))
           (new-dir
            (and (f-boundp-global 'new-certification-dir state)
                 (f-get-global 'new-certification-dir state))))
       (cond (old-dir
              (cond ((and (stringp old-dir)
                          (stringp new-dir)
                          (not (equal old-dir ""))
                          (not (equal new-dir ""))
                          (not (equal (char old-dir (1- (length old-dir)))
                                      *directory-separator*))
                          (not (equal (char new-dir (1- (length new-dir)))
                                      *directory-separator*)))
                     (make-certificate-file1
                      file portcullis
                      (concatenate 'string certification-file ".final")
                      (new-post-alist3 post-alist3 old-dir (length old-dir)
                                       new-dir)
                      expansion-alist
                      ctx state))
                    (t (er soft ctx
                           "Attempted to create ~x0 because state global ~
                            'old-certification-dir is bound to a non-nil ~
                            value, ~x1.  However, in this case we require that ~
                            both this variable and 'new-certification-dir are ~
                            bound to non-empty strings not terminating in ~s2; ~
                            but this is not the case."
                           (concatenate 'string certification-file ".final")
                           old-dir
                           *directory-separator-string*))))
             (t (value :irrelevant-value))))
     (make-certificate-file1 file portcullis certification-file post-alist3
                             expansion-alist ctx state))))
                           
; We now develop a general-purpose read-object-file, which expects
; the given file to start with an IN-PACKAGE and then reads into that
; package all of the remaining forms of the file, returning the list
; of all forms read.

(defun open-input-object-file (file ctx state)

; If this function returns without error, then a channel is returned.
; In our use of this function in INCLUDE-BOOK we know file is a string.
; Indeed, it is a book name.  But we write this function slightly more
; ruggedly so that read-object-file, below, can be used on an
; arbitrary alleged file name.

  (cond ((stringp file)
         (mv-let (ch state)
                 (open-input-channel file :object state)
                 (cond ((null ch)
                        (er soft ctx
                            "There is no file named ~x0 that can be ~
                             opened for input."
                            file))
                       (t (value ch)))))
        (t (er soft ctx
               "File names in ACL2 must be strings, so ~x0 is not a ~
                legal file name."
               file))))

(defun read-object-file1 (channel state ans)

; Channel is an open input object channel.  We have verified that the
; first form in the file is an in-package and we are now in that
; package.  We read all the remaining objects in the file and return
; the list of them.

  (mv-let (eofp val state)
          (read-object channel state)
          (cond (eofp (value (reverse ans)))
                (t (read-object-file1 channel state (cons val ans))))))

(defun read-object-file (file ctx state)

; We open file for object input (causing an error if file is
; inappropriate).  We then get into the package specified by the
; (in-package ...) at the top of file, read all the objects in file,
; return to the old current package, close the file and exit,
; returning the list of all forms read (including the IN-PACKAGE).

  (er-let* ((ch (open-input-object-file file ctx state))
            (new-current-package (chk-in-package ch file ctx state)))
           (state-global-let*
            ((current-package new-current-package))
            (er-let* ((lst (read-object-file1 ch state nil)))
                     (let ((state (close-input-channel ch state)))
                       (value (cons (list 'in-package new-current-package)
                                    lst)))))))

(defun maybe-add-separator (str)
  (if (and (not (equal str ""))
           (eql (char str (1- (length str))) *directory-separator*))
      str
    (string-append str *directory-separator-string*)))

(defun set-cbd-fn (str state)
  (cond ((not (stringp str))
         (er soft (cons 'set-cbd str)
             "~x0 does not have the syntax of an ~
                ACL2 directory name.  See :DOC cbd."
             str))
        ((absolute-pathname-string-p str nil (os (w state)))
         (assign connected-book-directory (maybe-add-separator str)))
        ((not (absolute-pathname-string-p
               (f-get-global 'connected-book-directory state)
               nil
               (os (w state))))
         (er soft (cons 'set-cbd str)
             "An attempt was made to set the connected book directory (cbd) ~
              using relative pathname ~p0, but surprisingly, the existing cbd ~
              is ~p1, which is not an absolute pathname.  See :DOC pathname."
             str
             (f-get-global 'connected-book-directory state)))
        (t
         (assign connected-book-directory
                 (maybe-add-separator
                  (our-merge-pathnames
                   (f-get-global 'connected-book-directory state)
                   str))))))

(defmacro set-cbd (str)

  ":Doc-Section books

  to set the connected book directory~/
  ~bv[]
  Example Forms:
  ACL2 !>:set-cbd \"/usr/home/smith/\"
  ACL2 !>:set-cbd \"my-acl2/books\"
  ~ev[]
  ~l[cbd] for a description of the connected book directory.~/
  ~bv[]
  General Form:
  (set-cbd str)
  ~ev[]

  where ~c[str] is a nonempty string that represents the desired
  directory (~pl[pathname]).  This command sets the connected book
  directory (~pl[cbd]) to the string representing the indicated
  directory.  Thus, this command may determine which files are
  processed by ~ilc[include-book] and ~ilc[certify-book] ~il[command]s typed at the
  top-level.  However, the ~ilc[cbd] is also temporarily set by those two
  book processing ~il[command]s.

  ~sc[Important]:  Pathnames in ACL2 are in the Unix (trademark of AT&T)
  style.  That is, the character ``~c[/]'' separates directory components
  of a pathname, and pathnames are absolute when they start with this
  character, and relative otherwise.  ~l[pathname]."

  `(set-cbd-fn ,str state))

(defun parse-book-name (dir x extension os)

; This function takes a directory name, dir, and a user supplied book name, x,
; which is a string, and returns (mv full dir familiar), where full is the full
; book name string, dir is the directory name, and familiar is the familiar
; name string.  Extension is either nil or a string such as ".lisp" and the
; full book name is given the extension if it is non-nil.

; Given dir                and x with extension=".lisp"
; "/usr/home/moore/"           "nasa-t3/arith"       ; user name
; this function produces
; (mv "/usr/home/moore/nasa-t3/arith.lisp"           ; full name
;     "/usr/home/moore/nasa-t3/"                     ; directory name
;     "arith")                                       ; familiar name

; On the other hand, if x is "/usr/home/kaufmann/arith" then the result is
; (mv "/usr/home/kaufmann/arith.lisp"
;     "/usr/home/kaufmann/"
;     "arith")

; We work with Unix-style pathnames.

; Note that this function merely engages in string processing.  It does not
; actually guarantee that the named file exists or that the various names are
; in any sense well-formed.  It does not change the connected book directory.
; If x is not a string and not well-formed as a structured pathname, the result
; is (mv nil nil x).  Thus, if the full name returned is nil, we know something
; is wrong and the short name returned is whatever junk the user supplied.

  (cond
   ((stringp x)
    (let* ((lst (coerce x 'list))
           (rlst (reverse lst))
           (temp (member *directory-separator* rlst)))

; If x is "project/task3/arith.lisp" then temp is "project/task3/" except is a
; list of chars and is in reverse order (!).

      (let ((familiar (coerce (reverse (first-n-ac
                                        (- (length x) (length temp))
                                        rlst nil))
                              'string))
            (dir1 (extend-pathname dir
                                   (coerce (reverse temp) 'string)
                                   os)))
        (mv (if extension
                (concatenate 'string dir1 familiar extension)
              (concatenate 'string dir1 familiar))
            dir1
            familiar))))
   (t (mv nil nil x))))

(defun chk-cert-annotations
  (cert-annotations portcullis-cmds full-book-name suspect-book-action-alist
                    ctx state)

; Warning: Chk-cert-annotations and chk-cert-annotations-post-alist are nearly
; duplicates of one another.  If you change one, e.g., to add a new kind of
; annotation and its checker, change the other.

  (er-progn
   (cond
    ((eq (cdr (assoc :skipped-proofsp cert-annotations)) nil)
     (value nil))
    ((eq (cdr (assoc :skipped-proofsp cert-annotations)) t)
     (include-book-er full-book-name nil
                      (if portcullis-cmds
                          "The book ~x0 (including events from its portcullis) ~
                           contains one or more SKIP-PROOFS events."
                        "The book ~x0 contains one or more SKIP-PROOFS events.")
                      :skip-proofs-okp
                      suspect-book-action-alist ctx state))
    (t (include-book-er full-book-name nil
                        (if portcullis-cmds
                            "The book ~x0 (including events from its ~
                             portcullis) may contain SKIP-PROOFS events."
                          "The book ~x0 may contain SKIP-PROOFS events.")
                        :skip-proofs-okp
                        suspect-book-action-alist ctx state)))
   (cond
    ((eq (cdr (assoc :axiomsp cert-annotations)) nil)
     (value nil))
    ((eq (cdr (assoc :axiomsp cert-annotations)) t)
     (include-book-er full-book-name nil
                      (if portcullis-cmds
                          "The book ~x0 (including events from its portcullis) ~
                           contains one or more DEFAXIOM events."
                        "The book ~x0 contains one or more DEFAXIOM events.")
                      :defaxioms-okp
                      suspect-book-action-alist ctx state))
    (t (include-book-er full-book-name nil
                        (if portcullis-cmds
                            "The book ~x0 (including events from its ~
                             portcullis) may contain DEFAXIOM events."
                          "The book ~x0 may contain DEFAXIOM events.")
                        :defaxioms-okp
                        suspect-book-action-alist ctx state)))))

(defun chk-cert-annotations-post-alist
  (post-alist portcullis-cmds full-book-name suspect-book-action-alist ctx
              state)
  
; Warning: Chk-cert-annotations and chk-cert-annotations-post-alist are nearly
; duplicates of one another.  If you change one, e.g., to add a new kind of
; annotation and its checker, change the other.

; We are in the process of including the book full-book-name.  Post-alist is
; its locally-marked include-book alist as found in the .cert file.  We look
; at every entry (LOCAL or not) and check that its cert annotations are
; consistent with the suspect-book-action-list.

  (cond
   ((endp post-alist) (value nil))
   (t 

; An entry in the post-alist is (full user familiar cert-annotations . chk).
; It may optionally be embedded in a (LOCAL &) form.

      (let* ((localp (eq (car (car post-alist)) 'local))
             (full-subbook (if localp
                               (car (cadr (car post-alist)))
                             (car (car post-alist))))
             (cert-annotations (if localp
                                   (cadddr (cadr (car post-alist)))
                                 (cadddr (car post-alist)))))
        (er-progn
         (cond
          ((eq (cdr (assoc :skipped-proofsp cert-annotations)) nil)
           (value nil))
          ((eq (cdr (assoc :skipped-proofsp cert-annotations)) t)
           (include-book-er
            full-book-name nil
            (cons "The book ~x0~sp~#a~[~/ locally~] includes ~xb, which ~
                   contains one or more SKIP-PROOFS events."
                  (list (cons #\a (if localp 1 0))
                        (cons #\b full-subbook)
                        (cons #\p (if portcullis-cmds
                                      " (including events from its portcullis)"
                                    ""))))
            :skip-proofs-okp
            suspect-book-action-alist ctx state))
          (t (include-book-er
              full-book-name nil
              (cons "The book ~x0~sp~#a~[~/ locally~] includes ~xb, which ~
                     may contain SKIP-PROOFS events."
                    (list (cons #\a (if localp 1 0))
                          (cons #\b full-subbook)
                          (cons #\p (if portcullis-cmds
                                        " (including events from its portcullis)"
                                      ""))))
              :skip-proofs-okp
              suspect-book-action-alist ctx state)))
         (cond
          ((eq (cdr (assoc :axiomsp cert-annotations)) nil)
           (value nil))
          ((eq (cdr (assoc :axiomsp cert-annotations)) t)
           (include-book-er
            full-book-name nil
            (cons "The book ~x0~sp~#a~[~/ locally~] includes ~xb, which ~
                   contains one or more DEFAXIOM events."
                  (list (cons #\a (if localp 1 0))
                        (cons #\b full-subbook)
                        (cons #\p (if portcullis-cmds
                                      " (including events from its portcullis)"
                                    ""))))
            :defaxioms-okp
            suspect-book-action-alist ctx state))
          (t (include-book-er
              full-book-name nil
              (cons "The book ~x0~sp~#a~[~/ locally~] includes ~xb, which ~
                     may contain DEFAXIOM events."
                    (list (cons #\a (if localp 1 0))
                          (cons #\b full-subbook)
                          (cons #\p (if portcullis-cmds
                                        " (including events from its ~
                                         portcullis)"
                                      ""))))
              :defaxioms-okp
              suspect-book-action-alist ctx state)))
         (chk-cert-annotations-post-alist (cdr post-alist)
                                          portcullis-cmds
                                          full-book-name
                                          suspect-book-action-alist
                                          ctx state))))))

(defun chk-input-object-file (file ctx state)

; This checks that an object file named file can be opened for input.
; It either causes an error or returns t.  It changes the state --
; because it opens and closes a channel to the file -- and it may well
; be that the file does not exist in the state returned!  C'est la
; guerre.  The purpose of this function is courtesy to the user.  It
; is nice to rather quickly determine, in include-book for example,
; whether an alleged file exists.

  (er-let* ((ch (open-input-object-file file ctx state)))
           (let ((state (close-input-channel ch state)))
             (value t))))

(defmacro include-book-dir (dir)
  `(if (eq ,dir :system)
       (f-get-global 'distributed-books-dir state)
     (cdr (assoc-eq ,dir
                    (cdr (assoc-eq :include-book-dir-alist
                                   (table-alist 'acl2-defaults-table
                                                (w state))))))))

(defmacro include-book-dir-with-chk (soft-or-hard ctx dir)
  `(let ((ctx ,ctx)
         (dir ,dir))
     (let ((dir-value (include-book-dir dir)))
       (cond ((null dir-value) ; hence, dir is not :system
              (er ,soft-or-hard ctx
                  "The legal values for the :DIR argument are keywords that ~
                   include :SYSTEM as well as those added by a call of ~
                   add-include-book-dir.  However, that argument is ~x0, which ~
                   is not among the list of those legal values, ~x1."
                  dir
                  (cons :system
                        (strip-cars
                         (cdr (assoc-eq :include-book-dir-alist
                                        (table-alist 'acl2-defaults-table
                                                     (w state))))))))
             (t ,(if (eq soft-or-hard 'soft) '(value dir-value)
                   'dir-value))))))

(defun newly-defined-top-level-fns-rec (k wrld path acc)

; We accumulate into acc (which is eventually returned) the list of function
; symbols defined in the top k tuples of wrld whose definition does not come
; from an included book.  Path is the the most recent value of world global
; 'include-book-path as we recur backwards through wrld (initially, nil).

; We do not mind if acc contains duplicates (say, because a function was
; defined in :program mode in wrld and then verify-termination was executed for
; it in wrld).

  (declare (type (unsigned-byte 28) k))
  (cond ((eql k 0)
         acc)
        ((and (eq (caar wrld) 'include-book-path)
              (eq (cadar wrld) 'global-value))
         (newly-defined-top-level-fns-rec (1-f k) (cdr wrld) (cddar wrld) acc))
        (path
         (newly-defined-top-level-fns-rec (1-f k) (cdr wrld) path acc))
        ((and (eq (caar wrld) 'cltl-command)
              (eq (cadar wrld) 'global-value)
              (equal (caddar wrld) 'defuns))
         (newly-defined-top-level-fns-rec
          (1-f k) (cdr wrld) path
          (append (strip-cars (cdddr (cddar wrld))) acc)))
        (t
         (newly-defined-top-level-fns-rec (1-f k) (cdr wrld) path acc))))

(defun newly-defined-top-level-fns (old-wrld new-wrld)
  
; New-wrld is an extension of old-wrld.

  (let ((old-len (len old-wrld))
        (new-len (len new-wrld)))
    (assert$
     (<= old-len new-len)
     (newly-defined-top-level-fns-rec (- new-len old-len) new-wrld nil nil))))

(defun include-book-fn (user-book-name state
                                       load-compiled-file
                                       expansion-alist
                                       uncertified-okp
                                       defaxioms-okp
                                       skip-proofs-okp
                                       ttags
                                       doc
                                       dir
                                       event-form)

; Expansion-alist is :none if this is not called by certify-book-fn.
; Otherwise, it is an expansion-alist generated from make-event calls.

  (with-ctx-summarized
   (if (output-in-infixp state) event-form (cons 'include-book user-book-name))
   (let* ((wrld0 (w state))
          (active-book-name0 (active-book-name wrld0 state))
          (old-ttags-seen (global-val 'ttags-seen wrld0))
          (behalf-of-certify-flg (not (eq expansion-alist :none)))
          #-acl2-loop-only (*inside-include-book-fn* t)
          (old-include-book-path
           (global-val 'include-book-path wrld0))
          (saved-acl2-defaults-table
           (table-alist 'acl2-defaults-table wrld0))
          (cddr-event-form
           (if event-form
               (cddr event-form)
             (append 
              (if (not (eq load-compiled-file :warn))
                  (list :load-compiled-file
                        load-compiled-file)
                nil)
              (if (not (eq uncertified-okp t))
                  (list :uncertified-okp
                        uncertified-okp)
                nil)
              (if (not (eq defaxioms-okp t))
                  (list :defaxioms-okp
                        defaxioms-okp)
                nil)
              (if (not (eq skip-proofs-okp t))
                  (list :skip-proofs-okp
                        skip-proofs-okp)
                nil)
              (if doc
                  (list :doc doc)
                nil))))
          #+(and clisp (not acl2-loop-only))
          (custom::*suppress-check-redefinition* t)
          #+(and allegro (not acl2-loop-only))
          (excl:*redefinition-warnings* nil)
          )
     (er-let*
      ((dir-value
        (cond (dir (include-book-dir-with-chk soft ctx dir))
              (t (value (cbd))))))
      (mv-let
       (full-book-name directory-name familiar-name)
       (parse-book-name dir-value user-book-name ".lisp" (os (w state)))

; If you add more keywords to the suspect-book-action-alist, make sure you do
; the same to the list constructed by certify-book-fn.  You might wish to
; handle the new warning summary in warning1.

       (let ((suspect-book-action-alist
              (list (cons :uncertified-okp
                          (if (assoc-eq 'certify-book
                                        (global-val 'embedded-event-lst
                                                    wrld0))
                              nil
                            uncertified-okp))
                    (cons :defaxioms-okp defaxioms-okp)
                    (cons :skip-proofs-okp skip-proofs-okp)))
             (include-book-alist0 (global-val 'include-book-alist wrld0)))
         (er-progn
          (chk-book-name user-book-name full-book-name ctx state)
          (chk-input-object-file full-book-name ctx state)
          (revert-world-on-error
           (cond
            ((and (not (global-val 'boot-strap-flg wrld0))
                  full-book-name
                  (assoc-equal full-book-name include-book-alist0))
             (stop-redundant-event state))
            (t
             (let ((wrld1 (global-set
                           'include-book-path
                           (cons full-book-name old-include-book-path)
                           wrld0)))
               (pprogn
                (set-w 'extension wrld1 state)
                (er-let*
                 ((redef (chk-new-stringp-name 'include-book full-book-name
                                               ctx wrld1 state))
                  (doc-pair (translate-doc full-book-name doc ctx state))
                  (cert-obj (if behalf-of-certify-flg
                                (value nil)
                              (chk-certificate-file full-book-name nil ctx state
                                                    suspect-book-action-alist
                                                    t)))
                  (wrld2 (value (w state)))
                  (expansion-alist (value (if behalf-of-certify-flg
                                              expansion-alist
                                            (and cert-obj
                                                 (access cert-obj cert-obj
                                                         :expansion-alist)))))
                  (post-alist (value (and cert-obj
                                          (access cert-obj cert-obj
                                                  :post-alist))))
                  (cert-full-book-name (value (car (car post-alist)))))
                 (cond

; We try the redundancy check again, because it will be cert-full-book-name
; that is stored on the world's include-book-alist, not full-book-name (if the
; two book names differ).

                  ((and (not (equal full-book-name cert-full-book-name))
                        (not (global-val 'boot-strap-flg wrld2))
                        cert-full-book-name 
                        (assoc-equal cert-full-book-name
                                     include-book-alist0))

; Chk-certificate-file calls chk-certificate-file1, which calls
; chk-raise-portcullis, which calls chk-raise-portcullis1, which evaluates, for
; example, maybe-install-acl2-defaults-table.  So we need to revert the world
; here.

                   (pprogn (set-w 'retraction wrld0 state)
                           (stop-redundant-event state)))
                  (t
                   (er-let*
                    ((ev-lst (read-object-file full-book-name ctx state)))

; Cert-obj above is either nil, indicating that the file is uncertified, or is
; a cert-obj record, which contains the now raised portcullis and the check sum
; alist of the files that should be brought in by this inclusion.  The first
; element of post-alist is the one for this book.  It should look like this:
; (full-book-name' user-book-name' familiar-name cert-annotations
; . ev-lst-chk-sum), where the first two names are irrelevant here because they
; reflect where the book was when it was certified rather than where the book
; resides now.  However, the familiar-name, cert-annotations and the
; ev-lst-chk-sum ought to be those for the current book.

                    (mv-let
                     (ev-lst-chk-sum state)
                     (check-sum-obj (append expansion-alist ev-lst) state)
                     (cond
                      ((not (integerp ev-lst-chk-sum))

; This error should never arise because check-sum-obj is only called on
; something produced by read-object, which checks that the object is ACL2
; compatible, and perhaps make-event expansion.  The next form causes a soft
; error, assigning proper blame.

                       (mv-let
                        (raw-ev-lst-chk-sum state)
                        (check-sum-obj ev-lst state)
                        (cond ((not (integerp raw-ev-lst-chk-sum))
                               (er soft ctx
                                   "The file ~x0 is not a legal list of ~
                                    embedded event forms because it contains ~
                                    an object, ~x1, which check sum was ~
                                    unable to handle."
                                   full-book-name raw-ev-lst-chk-sum))
                              (t
                               (mv-let
                                (expansion-chk-sum state)
                                (check-sum-obj expansion-alist state)
                                (cond
                                 ((not (integerp expansion-chk-sum))
                                  (er soft ctx
                                      "The expansion-alist (from make-event) ~
                                       for file ~x0 is not a legal list of ~
                                       embedded event forms because it ~
                                       contains an object, ~x1, which check ~
                                       sum was unable to handle."
                                      full-book-name expansion-chk-sum))
                                 (t (er soft ctx
                                        "The append of expansion-alist (from ~
                                         make-event) and command for file ~x0 ~
                                         is not a legal list of embedded ~
                                         event forms because it contains an ~
                                         object, ~x1, which check sum was ~
                                         unable to handle.  This is very ~
                                         surprising,because check sums were ~
                                         computed successfully for the ~
                                         expansion-alist and the commands.  ~
                                         It would be helpful for you to send ~
                                         a replayable example of this ~
                                         behavior to the ACL2 implementors."
                                        full-book-name ev-lst-chk-sum))))))))
                      (t (er-progn

; Notice that we are reaching inside the certificate object to retrieve
; information about the book from the post-alist.  (Car post-alist)) is in fact
; of the form (full-book-name user-book-name familiar-name cert-annotations
; . ev-lst-chk-sum).


                          (cond
                           ((and cert-obj
                                 (not (equal (caddr (car post-alist))
                                             familiar-name)))
                            (include-book-er
                             full-book-name nil
                             (cons
                              "The cer~-ti~-fi~-cate on file for ~x0 lists ~
                               the book under the name ~x3 whereas we were ~
                               expecting it to give the name ~x4.  While we ~
                               allow a certified book to be moved from one ~
                               directory to another after ~
                               cer~-ti~-fi~-ca~-tion, we insist that it keep ~
                               the same familiar name.  This allows the ~
                               cer~-ti~-fi~-cate file to contain the familiar ~
                               name, making it easier to identify which ~
                               cer~-ti~-fi~-cates go with which files and ~
                               inspiring a little more confidence that the ~
                               cer~-ti~-fi~-cate really does describe the ~
                               alleged file.  In the present case, it looks ~
                               as though the familiar book name was changed ~
                               after cer~-ti~-fi~-ca~-tion.  For what it is ~
                               worth, the check sum of the file at ~
                               cer~-ti~-fi~-ca~-tion was ~x5.  Its check sum ~
                               now is ~x6."
                              (list (cons #\3 (caddr (car post-alist)))
                                    (cons #\4 familiar-name)
                                    (cons #\5 (cddddr (car post-alist)))
                                    (cons #\6 ev-lst-chk-sum)))
                             :uncertified-okp
                             suspect-book-action-alist
                             ctx state))
                           (t (value nil)))

                          (cond
                           ((and cert-obj
                                 (not (equal (cddddr (car post-alist))
                                             ev-lst-chk-sum)))
                            (include-book-er
                             full-book-name nil
                             (cons
                              "The certificate on file for ~x0 lists the ~
                               check sum of the certified book as ~x3.  But ~
                               the check sum of the events now in the file is ~
                               ~x4. This generally indicates that the file ~
                               has been modified since it was last certified."
                              (list (cons #\3 (cddddr (car post-alist)))
                                    (cons #\4 ev-lst-chk-sum)))
                             :uncertified-okp
                             suspect-book-action-alist
                             ctx state))
                           (t (value nil)))

                          (let* ((cert-annotations
                                  (cadddr (car post-alist)))
                                 (cert-ttags
                                  (cdr (assoc-eq :ttags cert-annotations))))

; It is possible for cert-annotations to be nil now.  That is because cert-obj
; was nil.  But we never use it if cert-obj is nil, except for cert-ttags.
; Now, cert-obj is nil when we are including an uncertified book; so the fact
; that the calls of chk-ttags-for-include-book and chk-acceptable-ttags are
; trivial, in this case, is not a problem.

                            (er-let*
                             ((ttags (chk-well-formed-ttags
                                      ttags directory-name ctx state))
                              (ttags-info
                               (er-progn
                                (cond
                                 ((and cert-obj
                                       (or (cdr (assoc-eq
                                                 :skipped-proofsp
                                                 cert-annotations))
                                           (cdr (assoc-eq
                                                 :axiomsp
                                                 cert-annotations))))
                                  (chk-cert-annotations
                                   cert-annotations
                                   (access cert-obj cert-obj :cmds)
                                   full-book-name
                                   suspect-book-action-alist
                                   ctx state))
                                 (t (value nil)))

; The following two calls of chk-acceptable-ttags1 can presumably be skipped if
; state global 'skip-notify-on-defttag has a non-nil value.  However, they are
; probably cheap so we go ahead and make them anyhow, for robustness.
; 'Skip-notify-on-defttag will prevent needless subsidiary notification
; messages.

                                (chk-acceptable-ttags1
                                 cert-ttags
                                 nil ; active-book-name is irrelevant
                                 ttags
                                 nil ; ttags-seen is irrelevant
                                 :quiet ctx state)
                                (chk-acceptable-ttags1
                                 cert-ttags active-book-name0
                                 (f-get-global 'ttags-allowed state)
                                 old-ttags-seen t ctx state))))

; The following process-embedded-events is protected by the revert-world-
; on-error above.  See the Essay on Guard Checking for a discussion of the
; binding of guard-checking-on below.

                             (er-let*
                              ((ttags-allowed1
                                (state-global-let*
                                 ((skipped-proofsp nil)
                                  (axiomsp nil)
                                  (ttags-allowed (car ttags-info))
                                  (skip-notify-on-defttag cert-obj)
                                  (connected-book-directory directory-name)
                                  (match-free-error nil)
                                  (guard-checking-on nil)
                                  (in-local-flg

; As we start processing the events in the book, we are no longer in the
; lexical scope of LOCAL for purposes of disallowing setting of the
; acl2-defaults-table.

                                   (and (f-get-global 'in-local-flg state)
                                        'dynamic)))
                                 (let ((skip-proofsp

; At one time we bound this variable to 'initialize-acl2 if (or cert-obj
; behalf-of-certify-flg) is false.  But cert-obj is non-nil even if the
; check-sum is wrong, so we were distinguishing between two kinds of
; uncertified books: those with bad certificates and those with no
; certificates.  And inclusion of either sort of uncertified book is an "all
; bets are off" situation.  So it seems fine to use 'include-book here in all
; cases.  But why do we want to do so?  Eric Smith sent a nice example of a
; book with forms (local (include-book "bar")) and (local (my-macro)), where
; my-macro is defined in bar.lisp.  With 'initialize-acl2,
; chk-embedded-event-form recurs through the local calls and reports that
; (my-macro) is not an embedded event form (because the local inclusion of
; "bar" prevent my-macro from being defined).  With 'include-book, we can
; include the book.  More generally, Eric would like uncertified books to be
; treated by include-book much like certified books, in order to assist his
; development process.  That seems reasonable.

                                        'include-book))
                                   (er-progn
                                    (process-embedded-events
                                     'include-book

; We do not allow process-embedded-events-to set the ACL2 defaults table at the
; end.  For, consider the case that (defttag foo) has been executed just before
; the (include-book "bar") being processed.  At the start of this
; process-embedded-events we clear the acl2-defaults-table, removing any :ttag.
; If we try to restore the acl2-defaults-table at the end of this
; process-embedded-events, we will fail because the include-book-path was
; extended above to include full-book-name (for "bar"), and the restoration
; installs a :ttag of foo, yet in our example there is no :ttags argument for
; (include-book "bar").  So, instead we directly set the 'table-alist property
; of 'acl2-defaults-table directory for the install-event call below.

                                     :do-not-install
                                     skip-proofsp
                                     (cadr (car ev-lst))
                                     (list 'include-book full-book-name)
                                     (subst-by-position expansion-alist
                                                        (cdr ev-lst)
                                                        1)
                                     1
                                     (eq skip-proofsp 'include-book)
                                     ctx state)
                                    (value (f-get-global 'ttags-allowed
                                                         state)))))))

; The above process-embedded-events call returns what might be called
; proto-wrld3, which is equivalent to the current world of state before the
; process-embedded-events (since the insigs argument is nil), but it has an
; incremented embedded-event-depth.  We don't care about this world.  The
; interesting world is the one current in the state returned by by
; process-embedded-events.  It has all the embedded events in it and we are
; done except for certification issues.

                              (let* ((wrld3 (w state))
                                     (actual-alist (global-val 'include-book-alist
                                                               wrld3)))
                                (er-progn
                                 (cond
                                  ((and cert-obj
                                        (not (include-book-alist-subsetp
                                              (unmark-and-delete-local-included-books
                                               (cdr post-alist))
                                              actual-alist)))
                                   (include-book-er
                                    full-book-name nil
                                    (cons "The certified book ~x0 requires ~
                                           ~*3 but we have ~*4."
                                          (list
                                           (cons #\3
                                                 (tilde-*-book-check-sums-phrase
                                                  t
                                                  (unmark-and-delete-local-included-books
                                                   (cdr post-alist))
                                                  actual-alist))
                                           (cons #\4
                                                 (tilde-*-book-check-sums-phrase
                                                  nil
                                                  (unmark-and-delete-local-included-books
                                                   (cdr post-alist))
                                                  actual-alist))))
                                    :uncertified-okp
                                    suspect-book-action-alist
                                    ctx state))
                                  (t (value nil)))

; Now we check that all the subbooks of this one are also compatible with the
; current settings of suspect-book-action-alist.  The car of post-alist is the
; part that deals with full-book-name itself.  So we deal below with the cdr,
; which lists the subbooks.  The cert-obj may be nil, which makes the test
; below a no-op.

                                 (chk-cert-annotations-post-alist
                                  (cdr post-alist)
                                  (and cert-obj
                                       (access cert-obj cert-obj :cmds))
                                  full-book-name
                                  suspect-book-action-alist
                                  ctx state)

                                 (let* ((cert-annotations
                                         (cadddr (car post-alist)))

; If cert-obj is nil, then cert-annotations is nil.  If cert-obj is
; non-nil, then cert-annotations is non-nil.  Cert-annotations came
; from a .cert file, and they are always non-nil.  But in the
; following, cert-annotations may be nil.

                                        (certification-tuple
                                         (cond
                                          ((and cert-obj
                                                (equal (caddr (car post-alist))
                                                       familiar-name)
                                                (equal (cddddr (car post-alist))
                                                       ev-lst-chk-sum)
                                                (include-book-alist-subsetp
                                                 (unmark-and-delete-local-included-books
                                                  (cdr post-alist))
                                                 actual-alist))

; Below we use the full book name from the certificate, cert-full-book-name,
; rather than full-book-name (from the parse of the user-book-name), in
; certification-tuple, Intuitively, cert-full-book-name is the unique
; representative of the class of all legal full book names (including those
; that involve soft links).  Before Version_2.7 we used full-book-name rather
; than cert-full-book-name, and this led to problems as shown in the example
; below.

#|
    % ls temp*/*.lisp
    temp1/a.lisp  temp2/b.lisp  temp2/c.lisp
    % cat temp1/a.lisp
    (in-package "ACL2")
    (defun foo (x) x)
    % cat temp2/b.lisp
    (in-package "ACL2")
    (defun goo (x) x)
    % cat temp2/c.lisp
    (in-package "ACL2")
    (defun hoo (x) x)
    % 

  Below, two absolute pathnames are abbreviated as <path1> and <path2>.

  In temp2/ we LD a file with the following forms.

    (certify-book "<path1>/a")
    :u
    (include-book "../temp1/a")
    (certify-book "b" 1)
    :ubt! 1
    (include-book "b")
    (certify-book "c" 1)

  We then see the following error.  The problem is that <path1> involved symbolic
  links, and hence did not match up with the entry in the world's
  include-book-alist made by (include-book "../temp1/a") which expanded to an
  absolute pathname that did not involve symbolic links.

    ACL2 Error in (CERTIFY-BOOK "c" ...):  During Step 3 , we loaded different
    books than were loaded by Step 2!  Perhaps some other user of your
    file system was editing the books during our Step 3?  You might think
    that some other job is recertifying the books (or subbooks) and has
    deleted the certificate files, rendering uncertified some of the books
    needed here.  But more has happened!  Some file has changed!

    Here is the include-book-alist as of the end of Step 2:
    (("<path2>/temp2/c.lisp"
          "c" "c" ((:SKIPPED-PROOFSP) (:AXIOMSP))
          . 48180423)
     ("<path2>/temp2/b.lisp"
          "b" "b" ((:SKIPPED-PROOFSP) (:AXIOMSP))
          . 46083312)
     (LOCAL ("<path1>/a.lisp"
                 "<path1>/a"
                 "a" ((:SKIPPED-PROOFSP) (:AXIOMSP))
                 . 43986201))).

    And here is the alist as of the end of Step 3:
    (("<path2>/temp2/c.lisp"
          "c" "c" ((:SKIPPED-PROOFSP) (:AXIOMSP))
          . 48180423)
     ("<path2>/temp2/b.lisp"
          "b" "b" ((:SKIPPED-PROOFSP) (:AXIOMSP))
          . 46083312)
     ("<path2>/temp1/a.lisp"
          "<path2>/temp1/a"
          "a" ((:SKIPPED-PROOFSP) (:AXIOMSP))
          . 43986201)).

    Frequently, the former has more entries than the latter because the
    former includes LOCAL books. So compare corresponding entries, focusing
    on those in the latter.  Each entry is of the form (name1 name2 name3
    alist . chk-sum).  Name1 is the full name, name2 is the name as written
    in an include-book event, and name3 is the ``familiar'' name of the
    file. The alist indicates the presence or absence of problematic forms
    in the file, such as DEFAXIOM events.  For example, (:AXIOMSP . T)
    means there were defaxiom events; (:AXIOMSP . NIL) -- which actually
    prints as (:AXIOMSP) -- means there were no defaxiom events. Finally,
    chk-sum is either an integer check sum on the contents of the file
    at the time it was certified or else chk-sum is nil indicating that
    the file is not certified.  Note that if the chk-sum is nil, the entry
    prints as (name1 name2 name3 alist).  Go figure.


    Summary
    Form:  (CERTIFY-BOOK "c" ...)
    Rules: NIL
    Warnings:  Guards
    Time:  0.01 seconds (prove: 0.00, print: 0.00, other: 0.01)

    ******** FAILED ********  See :DOC failure  ******** FAILED ********
     :ERROR
    ACL2 !>

|#

                                         (list* cert-full-book-name
                                                user-book-name
                                                familiar-name
                                                cert-annotations
                                                ev-lst-chk-sum))
                                        (t 

; The certification tuple below is marked as uncertified because the
; ev-lst-chk-sum is nil.  What about cert-annotations?  It may or may
; not correctly characterize the file, it may even be nil.  Is that
; bad?  No, the check sum will always save us.

                                         (list* full-book-name
                                                user-book-name
                                                familiar-name
                                                cert-annotations
                                                nil)))))
                                 (pprogn
                                  #-acl2-loop-only
                                  (progn
                                    (state-global-let*
                                     ((ld-skip-proofsp 'include-book))
                                     (progn
                                       (load-compiled-file-if-more-recent
                                        ctx
                                        (and (eq load-compiled-file :comp!)
                                             (newly-defined-top-level-fns
                                              wrld2 wrld3))
                                        load-compiled-file full-book-name
                                        directory-name expansion-alist ev-lst)
                                       (value nil)))
                                    state)
                                  (redefined-warning redef ctx state)
                                  (f-put-global 'include-book-alist-state
                                                (add-to-set-equal
                                                 certification-tuple
                                                 (union-equal
                                                  (cdr post-alist)
                                                  (f-get-global
                                                   'include-book-alist-state
                                                   state)))
                                                state)
                                  (f-put-global 'ttags-allowed
                                                ttags-allowed1
                                                state)
                                  (install-event
                                   (if behalf-of-certify-flg
                                       ev-lst-chk-sum
                                     (or cert-full-book-name
                                         full-book-name))
                                   (list* 'include-book

; We use the the unique representative of the full book name provided by the
; one in the .cert file, when the certificate is valid before execution of this
; event), namely, cert-full-book-name; otherwise, we use the full-book-name
; parsed from what the user supplied.  Either way, we have an absolute path
; name, which is useful for the :puff and :puff* commands.  These could fail
; before Version_2.7 because the relative path name stored in the event was not
; sufficient to find the book at :puff/:puff* time.

                                          (or cert-full-book-name
                                              full-book-name)
                                          cddr-event-form)
                                   'include-book
                                   full-book-name
                                   nil nil t ctx
                                   (let ((wrld4
                                          (global-set?
                                           'ttags-seen
                                           (cdr ttags-info)
                                           (global-set
                                            'include-book-path
                                            old-include-book-path
                                            (update-doc-data-base
                                             full-book-name doc doc-pair
                                             (global-set
                                              'certification-tuple
                                              certification-tuple
                                              (global-set 'include-book-alist
                                                          (add-to-set-equal
                                                           certification-tuple
                                                           (global-val
                                                            'include-book-alist
                                                            wrld3))
                                                          wrld3))))
                                           old-ttags-seen)))
                                     (if (equal (table-alist
                                                 'acl2-defaults-table
                                                 wrld3)
                                                saved-acl2-defaults-table)
                                         wrld4
                                       (putprop 'acl2-defaults-table
                                                'table-alist
                                                saved-acl2-defaults-table
                                                wrld4)))
                                   state))))))))))))))))))))))))))))

(defun spontaneous-decertificationp1 (ibalist alist files)

; Ibalist is an include-book alist, while alist is the strip-cddrs of
; an include-book alist.  Thus, an entry in ibalist is of the form
; (full-book-name user-book-name familiar-name cert-annotations
; . ev-lst-chk-sum), while an entry in alist is (familiar-name
; cert-annotations . ev-lst-chk-sum).  We know, from context, that
; (subsetp-equal (strip-cddrs ibalist) alist) fails.  Thus, there are
; entries in ibalist that are not ``in'' alist, where ``in'' compares
; (familiar-name cert-annotations . ev-lst-chk-sum) tuples.  We
; determine whether each such entry fails only because the chk-sum in
; the ibalist is nil while that in a corresponding entry in the alist
; is non-nil.  If so, then the most likely explanation is that a
; concurrent process is recertifying certain books and deleted their
; .cert files.  We return the list of all files which have been
; decertified.

  (cond ((endp ibalist) files)
        (t (let* ((familiar-name1 (caddr (car ibalist)))
                  (cert-annotations1 (cadddr (car ibalist)))
                  (ev-lst-chk-sum1 (cddddr (car ibalist)))
                  (temp (assoc-equal familiar-name1 alist))
                  (cert-annotations2 (cadr temp))
                  (ev-lst-chk-sum2 (cddr temp)))
             (cond
              (temp
               (cond
                ((equal (cddr (car ibalist)) temp)

; This entry is identical to its mate in alist.  So we keep
; looking.
                 (spontaneous-decertificationp1 (cdr ibalist) alist files))
                ((and (or (null cert-annotations1)
                          (equal cert-annotations1 cert-annotations2))
                      (equal ev-lst-chk-sum1 nil)
                      ev-lst-chk-sum2)

; The full-book-name (car (car ibalist)) spontaneously decertified.
; So we collect it and keep looking.

                 (spontaneous-decertificationp1 (cdr ibalist) alist
                                                (cons (car (car ibalist))
                                                      files)))
                (t nil)))
              (t nil))))))

(defun spontaneous-decertificationp (alist1 alist2)

; We know that alist1 is not an include-book-alist-subset of alist2.
; We check whether this is precisely because some files which were
; certified in alist2 are not certified in alist1.  If so, we return
; the list of all such files.  But if we find any other kind of
; discrepancy, we return nil.

  (spontaneous-decertificationp1 alist1 (strip-cddrs alist2) nil))

; The following code is used to determine whether the portcullis
; contains a skip-proofs and to note include-books.

(mutual-recursion

(defun note-certification-world (form wrld ctx state names
                                      suspect-book-action-alist)

; We know that form has passed the chk-embedded-event-form in
; chk-acceptable-certify-book1.  This function returns an error triple with a
; state in which state globals include-book-alist-state and skipped-proofsp
; have been updated to reflect form.  An error only occurs if we detect an
; uncertified included book.  The value component of the returned error triple
; is irrelevant.  Before Version_2.6 there was an analogous but inadequate
; function, find-a-skip-proofs; see the Essay on Skip-proofs.

  (cond ((atom form) (value nil)) ; This should never happen.
        ((eq (car form) 'skip-proofs)
         (pprogn (set-skipped-proofsp state)
                 (value nil)))
        ((eq (car form) 'encapsulate)
         (note-certification-world-lst (cddr form) wrld ctx state names
                                       suspect-book-action-alist))
        ((eq (car form) 'progn)
         (note-certification-world-lst (cdr form) wrld ctx state names
                                       suspect-book-action-alist))
        ((eq (car form) 'local)
         (note-certification-world (cadr form) wrld ctx state names
                                   suspect-book-action-alist))
        ((eq (car form) 'value) (value nil))
        ((eq (car form) 'include-book)

; Why do we need to deal with include-book?  After all, if this include-book
; was executed, then can't we expect the include-book-alist entries for that
; book and all its subbooks, even locally-included subbooks, to be represented
; in the world global include-book-alist?  The problem is that the include-book
; form at hand may have been locally included inside an encapsulate.  In that
; case the information from its certificate will not show up in the world.  In
; fact we may not even catch a locally included uncertified book with our check
; in chk-acceptable-certify-book1.  But, we will catch that here.

         (mv-let
          (full-book-name directory-name familiar-name)
          (parse-book-name (cbd) (cadr form) ".lisp" (os wrld))
          (declare (ignore directory-name familiar-name))
          (mv-let (erp cert-obj state)
                  (chk-certificate-file full-book-name t ctx state
                                        suspect-book-action-alist
                                        nil)
                  (if erp
                      (er soft ctx
                          "Note:  The error reported just above is due to the ~
                          form ~x0 in the certification world.  ~
                          See :DOC certify-book."
                          form)
                    (pprogn (f-put-global 'include-book-alist-state
                                          (union-equal
                                           (and cert-obj
                                                (access cert-obj cert-obj
                                                        :post-alist))
                                           (f-get-global
                                            'include-book-alist-state
                                            state))
                                          state)
                            (value nil))))))
        ((member-eq (car form) names) (value nil))
        ((eq (car form) 'make-event)
         (cond ((consp (cadr (assoc-keyword :check-expansion
                                            (cddr form)))) ; use the expansion
                (note-certification-world (cadr (assoc-keyword :check-expansion
                                                               (cddr form)))
                                          wrld ctx state names
                                          suspect-book-action-alist))
               (t (er soft ctx
                      "Implementation error: we had thought that every ~
                       make-event form in the certification world would have ~
                       a consp :check-expansion field, yet we found the ~
                       following.  Please contact the ACL2 implementors.~|~x0"
                      form))))
        ((getprop (car form) 'macro-body nil 'current-acl2-world wrld)
         (er-let* ((expansion (macroexpand1 form ctx state)))
           (note-certification-world expansion wrld ctx state names
                                     suspect-book-action-alist)))
        (t (er soft ctx

; Since form has passed the chk-embedded-event-form in
; chk-acceptable-certify-book1, we should not be here.

               "Unexpected form in certification world:~%  ~x0"
               form))))

(defun note-certification-world-lst (forms wrld ctx state names
                                           suspect-book-action-alist)
  (cond
   ((null forms) (value nil))
   (t (er-progn
       (note-certification-world (car forms) wrld ctx state names
                                 suspect-book-action-alist)
       (note-certification-world-lst (cdr forms) wrld ctx state names
                                     suspect-book-action-alist)))))

)

(defun remove-duplicates-equal-from-end (lst acc)
  (cond ((endp lst) (reverse acc))
        ((member-equal (car lst) acc)
         (remove-duplicates-equal-from-end (cdr lst) acc))
        (t (remove-duplicates-equal-from-end (cdr lst) (cons (car lst) acc)))))

(defun include-book-alist-subsetp-failure-witnesses (alist1 strip-cddrs-alist2 acc)

; We accumulate into acc all members of alist1 that serve as counterexamples to
; (include-book-alist-subsetp alist1 alist2), where strip-cddrs-alist2 =
; (strip-cddrs alist2).

  (cond ((endp alist1) acc)
        (t (include-book-alist-subsetp-failure-witnesses
            (cdr alist1)
            strip-cddrs-alist2
            (if (member-equal (cddr (car alist1)) strip-cddrs-alist2)
                acc
              (cons (car alist1) acc))))))

; Essay on Guard Checking

; We bind the state global variable guard-checking-on to nil in certify-book-fn
; and in include-book-fn (using state-global-let*), and also in
; pc-single-step-primitive.  We bind guard-checking-on to t in prove,
; translate-in-theory-hint, and value-triple.  We do not bind guard-checking-on
; in defconst-fn.  Here we explain these decisions.

; We prefer to bind guard-checking-on to a predetermined fixed value when
; certifying or including books.  Why?  Book certification is a logical act.
; :Set-guard-checking is intended to be extra-logical, giving the user control
; over evaluation in the interactive loop, and hence we do not want it to
; affect how books are processed, either during certification or during
; inclusion.

; So the question now is whether to bind guard-checking-on to t or to nil for
; book certification and for book inclusion.  (We reject :none and :all because
; they can be too inefficient.)

; We want it to be the case that if a book is certified, then subsequently it
; can be included.  In particular, it would be unfortunate if certification is
; done in an environment with guard checking off, and then later we get a guard
; violation when including the book with guard checking on.  So if we bind
; guard-checking-on to nil in certify-book, then we should also bind it to nil
; in include-book.

; We argue now for binding guard-checking-on to nil in certify-book-fn (and
; hence, as argued above, in include-book-fn as well).  Note that we already
; allow book certification without requiring guard verification, which drives
; home the position that guards are extra-logical.  Thus, a high-level argument
; for binding guard-checking-on to nil during certification is that if we bind
; instead to t, then that position is diluted.  Now we give a more practical
; argument for binding guard-checking-on to nil during certification.  Suppose
; someone writes a macro with a guard, where that guard enforces some
; intention.  Do we want to enforce that guard, and when?  We already have
; safe-mode to enforce the guard as necessary for Common Lisp.  If a user
; executes :set-guard-checking nil in the interactive loop, then function
; guards are not checked, and thus it is reasonable not to check macro guards
; either.  Now suppose the same user attempts to certify a book that contains a
; top-level guard-violating macro call.  What a rude surprise it would be if
; certification fails due to a guard violation during expansion of that macro
; call, when the interactive evaluation of that same call had just succeeded!
; (One might argue that it is a sophisticated act to turn off guard checking in
; the interactive loop, hence a user who does that should be able to handle
; that rude surprise.  But that argument seems weak; even a beginner could find
; out how to turn off guard checking after seeing a guard violation.)

; We have argued for turning off guard checking during certify-book.  But a
; concern remains.  Suppose one user has written a macro with a guard, and now
; suppose a second user creates a book containing a top-level call of that
; macro with a guard violation.  Safe-mode will catch any Common Lisp guard
; violation.  But the macro writer may have attached the guard in order to
; enforce some intention that is not related to Common Lisp.  In the case of
; functions, one can ensure one's compliance with existing guards by verifying
; all guards, for example with (set-verify-guards-eagerness 2) at the top of
; the file.  A similar "complete guard checking" mechanism could enforce one's
; compliance with macro guards as well, say, (set-verify-guards-eagerness 3).
; The same concern applies to defconst, not only for macro expansion but also
; for function calls, and could be handled in the case of complete guard
; checking in the same way as for top-level macro expansion, by binding
; guard-checking-on to t with state-global-let*.  In practice, users may not
; demand the capability for complete guard checking, so it might not be
; important to provide this capability.

; Having decided to bind guard-checking-on to nil in certify-book-fn and
; (therefore) include-book-fn, let us turn to the other cases in which we bind
; guard-checking-on.

; We discussed defconst briefly above.  We note that raw Lisp evaluation should
; never take place for the body of a defconst form (outside the boot-strap),
; because the raw Lisp evaluation of defconst avoids such evaluation when the
; name is already bound, which should be the case from prior evaluation in the
; logic.  However, value-triple does not assign to a variable, so its
; evaluation in the logic should be done with guard checking on in order to
; avoid a subsequent hard lisp error when the form is later evaluated in raw
; Lisp.  It would likely suffice, actually, to bind safe-mode to t instead of
; binding guard-checking-on to t, but we take the safer (stronger) route of
; imposing guard checking since we do not expect any adverse consequences.

; We bind guard-checking-on to nil in prove, because proofs can use evaluation
; and such evaluation should be done in the logic, without regard to guards.

; It can be important to check guards during theory operations like
; union-theory, not only during certify-book but in the interactive loop.  For
; example, with guard checking off in Version_2.9, one gets a hard Lisp error
; upon evaluation of the following form.

; (in-theory (union-theories '((:rewrite no-such-rule))
;                            (current-theory 'ground-zero)))

; (Aside.  One does not get such an error in Version_2.8, because *1* functions
; checked guards of system functions regardless of the value of
; guard-checking-on; but we have abandoned that aggressive approach, relying
; instead on safe-mode.)  Our solution is to bind guard-checking-on to t in
; translate-in-theory-hint, which calls simple-translate-and-eval and hence
; causes the guards to be checked.

; Note that guard-checking-on is bound to nil in pc-single-step-primitive.  We
; no longer recall why, but we may as well preserve that binding.

(defun expansion-filename (full-book-name)

; We use a .lsp suffix instead of .lisp for benefit of the makefile system,
; which by default looks for .lisp files to certify.

  (let ((len (length full-book-name)))
    (assert$ (equal (subseq full-book-name (- len 5) len) ".lisp")
             (concatenate 'string
                          (subseq full-book-name 0 (- len 5))
                          "@expansion.lsp"))))

(defun write-expansion-file (new-fns-exec expansion-filename expansion-alist
                                          ev-lst ctx state)

; Expansion-filename is the expansion file for a certified book (or, a book
; whose certification is nearly complete) that has been included.  (We call
; set-current-package below instead of the corresponding f-put-global as a
; partial check that this inclusion has taken place.)  We write out that
; expansion file, instead causing an error if we cannot open it.

  #+acl2-loop-only
  (declare (ignore new-fns-exec))
  (mv-let
   (ch state)
   (open-output-channel expansion-filename :object state)
   (cond
    ((null ch)
     (er soft ctx
         "We cannot open expansion file ~s0 for output."
         expansion-filename))
    (t
     (state-global-let*
      ((current-package "ACL2"))
      (pprogn
       (io? event nil state
            (expansion-filename)
            (fms "Writing book expansion file, ~s0.~|"
                 (list (cons #\0 expansion-filename))
                 (proofs-co state) state nil))
       (print-object$ (car ev-lst) ch state) ; in-package form
       (er-progn
        (set-current-package (cadr (car ev-lst)) state)
        (pprogn
         (print-objects (subst-by-position expansion-alist (cdr ev-lst) 1)
                        ch state)
         (newline ch state)
         #-acl2-loop-only
         (progn (when new-fns-exec
                  (newline ch state)
                  (princ$ ";;; *1* function definitions to compile:" ch state)
                  (newline ch state)
                  (compile-uncompiled-*1*-defuns nil ; irrelevant filename
                                                 new-fns-exec nil ch))
                state)
         (close-output-channel ch state)
         (value expansion-filename)))))))))

(defun collect-ideal-user-defuns1 (tl wrld ans)
  (cond
   ((or (null tl)
        (and (eq (caar tl) 'command-landmark)
             (eq (cadar tl) 'global-value)
             (equal (access-command-tuple-form (cddar tl))
                    '(exit-boot-strap-mode))))
    ans)
   ((and (eq (caar tl) 'cltl-command)
         (eq (cadar tl) 'global-value)
         (equal (caddar tl) 'defuns))
    (collect-ideal-user-defuns1
     (cdr tl)
     wrld
     (cond
      ((null (cadr (cddar tl)))

 ; Defun-mode-flg = nil means encapsulate or :non-executable.  In this case we
 ; do not pick up the function, but that's OK because we don't care if it is
 ; executed efficiently.

       ans)
      ((eq (symbol-class (caar (cdddr (cddar tl))) wrld) :ideal)
       (append (strip-cars (cdddr (cddar tl))) ans))
      (t ans))))
   (t (collect-ideal-user-defuns1 (cdr tl) wrld ans))))

(defun collect-ideal-user-defuns (wrld)

; We scan wrld down to command 0 (but not into prehistory), collecting those
; fns which were (a) introduced with defun or defuns and (b) are :ideal.

  (collect-ideal-user-defuns1 wrld wrld nil))

(defun set-difference-eq-sorted (lst1 lst2 ans)

; Lst1 and lst2 are sorted by symbol-<.  If ans is nil, then we return the
; difference of lst1 and lst2, sorted by symbol-<.

  (cond ((null lst1) (reverse ans))
        ((null lst2) (revappend ans lst1))
        ((eq (car lst1) (car lst2))
         (set-difference-eq-sorted (cdr lst1) (cdr lst2) ans))
        ((symbol-< (car lst1) (car lst2))
         (set-difference-eq-sorted (cdr lst1) lst2 (cons (car lst1) ans)))
        (t (set-difference-eq-sorted lst1 (cdr lst2) ans))))

(defun certify-book-disabledp (state)
  (f-get-global 'certify-book-disabledp state))

(defun certify-book-fn (user-book-name k compile-flg
                                       defaxioms-okp
                                       skip-proofs-okp
                                       ttags
                                       save-expansion
                                       state event-form)

; We assume that all the referenced books in user-book-name have already been
; certified (we will check this assumption).

; Note that we want to inhibit proof-tree output in the summary, which is why
; we bind inhibit-output-lst so early.

  (declare (ignore event-form))
  (with-ctx-summarized
   (if (output-in-infixp state)
       (list* 'certify-book user-book-name
              (if (and (equal k 0) (eq compile-flg t))
                  nil
                '(irrelevant)))
     (cons 'certify-book user-book-name))
   (cond
    ((certify-book-disabledp state)
     (er soft ctx
         "Certify-book has been disabled in this session because ~@0."
         (certify-book-disabledp state)))
    ((and (stringp user-book-name)
          (let ((len (length user-book-name)))
            (and (<= 10 len) ; 10 = (length "@expansion")
                 (equal (subseq user-book-name (- len 10) len)
                        "@expansion"))))
     (er soft ctx
         "Book names may not end in \"@expansion\"."))
    (t
     (er-let*
      ((ttags (chk-well-formed-ttags ttags (cbd) ctx state))
       (pair0 (let ((wrld (w state)))
                (chk-acceptable-ttags1

; We check whether the ttags in the certification world are legal for the given
; ttags, and if so we refine ttags, as described in chk-acceptable-ttag1.

                 (global-val 'ttags-seen wrld)
                 nil ; correct active-book-name, but irrelevant
                 ttags
                 nil ; irrelevant value for ttags-seen
                 :quiet ; defttags in certification world were already reported
                 ctx state))))
      (mv-let
       (full-book-name directory-name familiar-name)
       (parse-book-name (cbd) user-book-name ".lisp" (os (w state)))
       (state-global-let*
        ((certify-book-info full-book-name)
         (inhibit-output-lst
          (add-to-set-eq 'proof-tree
                         (f-get-global 'inhibit-output-lst state)))
         (match-free-error nil)
         (defaxioms-okp-cert defaxioms-okp)
         (skip-proofs-okp-cert skip-proofs-okp)
         (guard-checking-on nil)) ; see the Essay on Guard Checking
        (let ((saved-acl2-defaults-table
               (table-alist 'acl2-defaults-table (w state)))

; If you add more keywords to this list, make sure you do the same to
; the list constructed by include-book-fn.  Also, you might wish to
; handle the new warning summary in warning1.

              (suspect-book-action-alist
               (list '(:uncertified-okp . nil)
                     (cons :defaxioms-okp defaxioms-okp)
                     (cons :skip-proofs-okp skip-proofs-okp)))
              #-acl2-loop-only
              (*inside-include-book-fn* t)
              #+(and clisp (not acl2-loop-only))
              (custom::*suppress-check-redefinition* t)
              #+(and allegro (not acl2-loop-only))
              (excl:*redefinition-warnings* nil))
          (er-let*
           ((portcullis ; (cmds . pre-alist).
             (chk-acceptable-certify-book user-book-name
                                          full-book-name k ctx state
                                          suspect-book-action-alist)))
           (let ((wrld1 (w state)))
             (pprogn
              (io? event nil state
                   (full-book-name)
                   (fms "CERTIFICATION ATTEMPT FOR ~x0~%~s1~%~%"
                        (list (cons #\0 full-book-name)
                              (cons #\1 (f-get-global 'acl2-version state)))
                        (proofs-co state) state nil))
              (io? event nil state
                   (full-book-name)
                   (fms "* Step 1:  Read ~x0 and compute its check sum.~%"
                        (list (cons #\0 full-book-name))
                        (proofs-co state)
                        state nil))
              (er-let*
               ((ev-lst (read-object-file full-book-name ctx state)))
               (mv-let
                (ev-lst-chk-sum state) ; later we'll include
                (check-sum-obj ev-lst state)
                (cond
                 ((not (integerp ev-lst-chk-sum))
                  (er soft ctx
                      "The file ~x0 is not a legal list of embedded event ~
                        forms because it contains an object, ~x1, which check ~
                        sum was unable to handle."
                      full-book-name ev-lst-chk-sum))
                 (t
                  (pprogn
                   (io? event nil state
                        (ev-lst)
                        (fms "* Step 2:  There ~#0~[were no forms in the ~
                              file. Why are you making such a silly ~
                              book?~/was one form in the file.~/were ~n1 ~
                              forms in the file.~]  We now attempt to ~
                              establish that each form, whether local or ~
                              non-local, is indeed an admissible embedded ~
                              event form in the context of the previously ~
                              admitted ones.  Note that proof-tree output is ~
                              inhibited during this check; see :DOC ~
                              proof-tree.~%"
                             (list (cons #\0 (zero-one-or-more ev-lst))
                                   (cons #\1 (length ev-lst)))
                             (proofs-co state) state nil))
                   (er-let*
                    ((pass1-result
                      (state-global-let*
                       ((ttags-allowed (car pair0))

; We ``accumulate'' into the flag skipped-proofsp whether there are any
; skip-proofs in sight.  See the Essay on Skip-proofs.

                        (skipped-proofsp nil)
                        (include-book-alist-state nil)

; We will accumulate into the flag axiomsp whether any axioms have been added,
; starting with those in the portcullis.  We can identify axioms in the
; portcullis by asking if the current nonconstructive axioms are different from
; those at the end of boot-strap.

                        (axiomsp (not
                                  (equal
                                   (global-val   ;;; axioms as of boot-strap
                                    'nonconstructive-axiom-names
                                    (scan-to-landmark-number
                                     'event-landmark
                                     (global-val 'event-number-baseline
                                                 wrld1)
                                     wrld1))
                                   (global-val   ;;; axioms now.
                                    'nonconstructive-axiom-names
                                    wrld1))))
                        (ld-redefinition-action nil)
                        (connected-book-directory directory-name))
                       (revert-world-on-error
                        (er-progn
                         (note-certification-world-lst
                          (car portcullis)
                          (w state)
                          ctx
                          state

; This list of names must be the same as in chk-acceptable-certify-book.

                          (cons 'defpkg
                                *primitive-event-macros*)
                          suspect-book-action-alist)

; The fact that we are under 'certify-book means that all calls of
; include-book will insist that the :uncertified-okp action is nil, meaning
; errors will be caused if uncertified books are read.

                         (er-let*
                          ((expansion-alist
                            (process-embedded-events
                             'certify-book
                             saved-acl2-defaults-table
                             nil (cadr (car ev-lst))
                             (list 'certify-book full-book-name)
                             (cdr ev-lst)
                             1 nil 'certify-book state)))
                          (value (list (f-get-global 'skipped-proofsp state)
                                       (f-get-global 'axiomsp state)
                                       (global-val 'ttags-seen (w state))
                                       (f-get-global
                                        'include-book-alist-state
                                        state)
                                       expansion-alist))))))))
                    (let* ((pass1-known-package-alist
                            (global-val 'known-package-alist (w state)))
                           (skipped-proofsp (car pass1-result))
                           (axiomsp (cadr pass1-result))
                           (ttags-seen (caddr pass1-result))
                           (new-include-book-alist-state
                            (cadddr pass1-result))
                           (expansion-alist (cadddr (cdr pass1-result)))
                           (cert-annotations
                            (list 
                                
; We set :skipped-proofsp in the certification annotations to t or nil
; according to whether there were any skipped proofs in either the
; portcullis or the body of this book.

                             (cons :skipped-proofsp skipped-proofsp)

; We similarly set :axiomsp to t or nil.  Note that axioms in subbooks 
; are not counted as axioms in this one.

                             (cons :axiomsp axiomsp)
                             (cons :ttags ttags-seen))))
                      (er-let*
                       ((chk-sum
                         (cond
                          (expansion-alist
                           (mv-let
                            (chk-sum state)
                            (check-sum-obj (append expansion-alist
                                                   ev-lst)
                                           state)
                            (value chk-sum)))
                          (t (value ev-lst-chk-sum))))
                        (post-alist1
                         (value (cons (list* full-book-name
                                             user-book-name
                                             familiar-name
                                             cert-annotations
                                             chk-sum)
                                      new-include-book-alist-state))))
                       (er-progn
                        (chk-cert-annotations cert-annotations
                                              (car portcullis)
                                              full-book-name
                                              suspect-book-action-alist
                                              ctx state)
                        (pprogn
                         (io? event nil state
                              nil
                              (fms "* Step 3:  That completes the ~
                                    admissibility check.  Each form read was ~
                                    an embedded event form and was ~
                                    admissible. We now retract back to the ~
                                    initial world and try to include the ~
                                    book.  This may expose local ~
                                    incompatibilities.~%"
                                   nil
                                   (proofs-co state) state nil))
                         (set-w 'retraction wrld1 state)
                         (er-let*
                          ((defpkg-items
                             (defpkg-items
                               pass1-known-package-alist
                               ctx wrld1 state))
                           (new-chk-sum
                            (state-global-let*
                             ((ld-redefinition-action nil))

; Note that we do not bind connected-book-directory before calling
; include-book-fn, because it will bind it for us.  We leave the directory set
; as it was when we parsed user-book-name to get full-book-name, so that
; include-book-fn will parse user-book-name the same way again.

                             (include-book-fn user-book-name state nil
                                              expansion-alist
                                              nil
                                              defaxioms-okp skip-proofs-okp
                                              ttags-seen
                                              nil nil nil))))
                          (let* ((wrld2 (w state))
                                 (new-defpkg-list
                                  (new-defpkg-list
                                   defpkg-items
                                   (global-val 'known-package-alist wrld2)))
                                 (new-fns (and (or (not (warning-disabled-p
                                                         "Guards"))
                                                   (eq compile-flg :all))

; The test above is an optimization; we don't need new-fns if it's false.

                                               (newly-defined-top-level-fns
                                                wrld1 wrld2)))
                                 (new-fns-exec (and (eq compile-flg :all)
                                                    new-fns))
                                 (expansion-filename ; nil for "do not write it"
                                  (and (or (eq save-expansion :save)
                                           new-fns-exec
                                           expansion-alist)
                                       (expansion-filename full-book-name)))
                                 (post-alist2
                                  (cons (list* full-book-name
                                               user-book-name
                                               familiar-name

; We use the cert-annotations from the first pass.  They are the ones that
; include the LOCAL events too.

                                               cert-annotations
                                               new-chk-sum)
                                        (cdr (global-val 'include-book-alist
                                                         wrld2)))))

; The cdr above removes the certification tuple stored by the
; include-book-fn itself.  That pair is guaranteed to be last one
; because it is the most recently added one (with add-to-set-equal)
; and we know it was not already a member of the list because
; chk-acceptable-certify-book1 checked that.  Could a file include
; itself?  It could try.  But if (include-book file) is one of the
; events in file, then the attempt to (include-book file) will cause
; infinite recursion -- because we don't put file on the list of files
; we've included (and hence recognize as redundant) until after we've
; completed the processing.

                            (pprogn
                             (mv-let
                              (new-bad-fns all-bad-fns)
                              (cond ((not (warning-disabled-p "Guards"))
                                     (mv (collect-ideals new-fns wrld2 nil)
                                         (collect-ideal-user-defuns wrld2)))
                                    (t (mv nil nil)))
                              (cond
                               ((or new-bad-fns all-bad-fns)
                                (let* ((new-bad-fns
                                        (sort-symbol-listp
                                         new-bad-fns))
                                       (all-bad-fns
                                        (sort-symbol-listp
                                         all-bad-fns))
                                       (extra-bad-fns
                                        (set-difference-eq-sorted
                                         all-bad-fns
                                         new-bad-fns
                                         nil)))
                                  (warning$
                                   ctx ("Guards")
                                   "~#1~[~/The book ~x0 defines the ~
                                    function~#2~[ ~&2, which has not had ~
                                    its~/s ~&2, which have not had their~] ~
                                    guards verified.  ~]~#3~[~/~#1~[For the ~
                                    book ~x0, its~/Moreover, this book's~] ~
                                    included sub-books ~#4~[~/and/or its ~
                                    certification world ~]define ~
                                    function~#5~[ ~&5, which has not had ~
                                    its~/s ~&5, which have not had their~] ~
                                    guards verified.  ~]See :DOC guards."
                                   full-book-name
                                   (if new-bad-fns 1 0)
                                   new-bad-fns
                                   (if extra-bad-fns 1 0)
                                   (if (eql k 0) 0 1)
                                   extra-bad-fns)))
                               (t state)))
                             (cond
                              ((not (include-book-alist-subsetp post-alist2
                                                                post-alist1))
                               (let ((files (spontaneous-decertificationp
                                             post-alist2
                                             post-alist1)))
                                 (cond
                                  (files
                                   (er soft ctx
                                       "During Step 3, we loaded the ~
                                        uncertified ~#0~[book ~&0.  This book ~
                                        was certified when we looked at ~
                                        it~/books ~&0. These books were ~
                                        certified when we looked at them~] in ~
                                        Step 2!  The most likely explanation ~
                                        is that some concurrent job, possibly ~
                                        by another user of your file system, ~
                                        is currently recertifying ~#0~[this ~
                                        book~/these books~] (or subbooks of ~
                                        ~#0~[it~/them~]).  That hypothetical ~
                                        job might have deleted the ~
                                        certificate files of the books in ~
                                        question, rendering ~#0~[this ~
                                        one~/these~] uncertified.  If this ~
                                        explanation seems likely, we ~
                                        recommend that you identify the other ~
                                        job and wait until it has ~
                                        successfully completed."
                                       files))
                                  (t
                                   (er soft ctx
                                       "During Step 3, we loaded different ~
                                        books than were loaded by Step 2!  ~
                                        Here are the tuples produced by Step ~
                                        3 of the form ~X04 whose CDDRs are ~
                                        not in the list of tuples produced by ~
                                        Step 2:~|~%~X14~|~%Perhaps some other ~
                                        user of your file system was editing ~
                                        the books during our Step 3? You ~
                                        might think that some other job is ~
                                        recertifying the books (or subbooks) ~
                                        and has deleted the certificate ~
                                        files, rendering uncertified some of ~
                                        the books needed here.  But more has ~
                                        happened!  Some file has changed (as ~
                                        indicated above)!~%~%DETAILS.  Here ~
                                        is the include-book-alist as of the ~
                                        end of Step 2:~%~X24.~|~%And here is ~
                                           the alist as of the end of Step ~
                                           3:~%~X34.~|~%Frequently, the former ~
                                        has more entries than the latter ~
                                        because the former includes LOCAL ~
                                        books. So compare corresponding ~
                                        entries, focusing on those in the ~
                                        latter.  Each entry is of the form ~
                                        (name1 name2 name3 alist . chk-sum). ~
                                        Name1 is the full name, name2 is the ~
                                        name as written in an include-book ~
                                        event, and name3 is the ``familiar'' ~
                                        name of the file. The alist indicates ~
                                        the presence or absence of ~
                                        problematic forms in the file, such ~
                                        as DEFAXIOM events.  For example, ~
                                        (:AXIOMSP . T) means there were ~
                                        defaxiom events; (:AXIOMSP . NIL) -- ~
                                        which actually prints as (:AXIOMSP) ~
                                        -- means there were no defaxiom ~
                                        events. Finally, chk-sum is either an ~
                                        integer check sum based on the ~
                                        contents of the file at the time it ~
                                        was certified or else chk-sum is nil ~
                                        indicating that the file is not ~
                                        certified.  Note that if the chk-sum ~
                                        is nil, the entry prints as (name1 ~
                                        name2 name3 alist).  Go figure."
                                       '(:full-book-name
                                         :user-book-name
                                         :familiar-name
                                         :cert-annotations
                                         . :chk-sum-for-events)
                                       (include-book-alist-subsetp-failure-witnesses
                                        post-alist2
                                        (strip-cddrs post-alist1)
                                        nil)
                                       post-alist1
                                       post-alist2
                                       nil)))))
                              (t
                               (pprogn
                                (io? event nil state
                                     (post-alist1 full-book-name
                                                  expansion-filename)
                                     (fms "* Step 4:  Write the certificate ~
                                           for ~x0 in ~x1~@2.  The final ~
                                           check sum alist is ~x3.~%"
                                          (list
                                           (cons #\0 full-book-name)
                                           (cons
                                            #\1
                                            (convert-book-name-to-cert-name
                                             full-book-name))
                                           (cons
                                            #\2
                                            (if expansion-filename
                                                (msg ", as well as the ~
                                                      expansion file, ~@0"
                                                     expansion-filename)
                                              ""))
                                           (cons #\3 post-alist1))
                                          (proofs-co state) state nil))
                                (er-progn
                                 (if expansion-filename
                                     (write-expansion-file
                                      new-fns-exec expansion-filename
                                      expansion-alist ev-lst ctx state)
                                   (value nil))
                                 (make-certificate-file
                                  full-book-name
                                  (cons
                                   (remove-duplicates-equal-from-end
                                    (append (car portcullis) new-defpkg-list)
                                    nil)
                                   (cdr portcullis))
                                  post-alist1
                                  post-alist2
                                  expansion-alist
                                  ctx
                                  state)
                                 (pprogn
                                  (cond
                                   (compile-flg
                                    (pprogn
                                     (io? event nil state
                                          (full-book-name)
                                          (fms "* Step 5:  Compile the ~
                                                functions defined in ~x0.~%"
                                               (list (cons #\0 full-book-name))
                                               (proofs-co state) state nil))
                                     #-acl2-loop-only
                                     (compile-certified-file
                                      new-fns-exec
                                      expansion-filename
                                      full-book-name expansion-alist
                                      state)
                                     state))
                                   (t
                                    (pprogn
                                     #-acl2-loop-only
                                     (progn
                                       (delete-compiled-file
                                        (pathname-unix-to-os full-book-name
                                                             state))
                                       state)
                                     state)))
                                  #-acl2-loop-only
                                  (progn
                                    (when (and expansion-filename
                                               (not save-expansion))
                                      (delete-file expansion-filename)
                                      (io? event nil state
                                           (expansion-filename)
                                           (fms "Note: Deleting book ~
                                                 expansion file,~%~s0.~|"
                                                (list
                                                 (cons
                                                  #\0
                                                  expansion-filename))
                                                (proofs-co state) state
                                                nil)))
                                    state)
                                  (value
                                   full-book-name))))))))))))))))))))))))))))))

#+acl2-loop-only
(defmacro certify-book (&whole event-form
                               user-book-name
                               &optional
                               (k '0)
                               (compile-flg 't)
                               &key
                               (defaxioms-okp 'nil)
                               (skip-proofs-okp 'nil)
                               (ttags 'nil)
                               (save-expansion 'nil))

  ":Doc-Section Books

  how to produce a ~il[certificate] for a book~/
  ~bv[]
  Examples:
  (certify-book \"my-arith\" 3)      ;certify in a world with 3 commands
  (certify-book \"my-arith\")        ;certify in a world with 0 commands
  (certify-book \"my-arith\" 0 nil)  ;as above, but do not compile
  (certify-book \"my-arith\" 0 t)    ;as above, but compile
  (certify-book \"my-arith\" 0 :all) ;as above, but compile exececutable
                                     ;  counterparts too
  (certify-book \"my-arith\" t)      ;certify from world of old certificate~/

  General Form:
  (certify-book book-name k compile-flg
                :defaxioms-okp t/nil        ; [default nil]
                :skip-proofs-okp t/nil      ; [default nil]
                :save-expansion :save/t/nil ; [default nil]
                :ttags ttags                ; [default nil]
                )
  ~ev[]
  where ~c[book-name] is a book name (~pl[book-name]), ~c[k] is
  either ~c[t] or an integer used to indicate your approval of the
  ``certification ~il[world].''  ~c[Compile-flg] indicates whether you
  wish to compile the (functions in the) book.  ~c[Compile-flg]
  defaults to ~c[t], meaning to compile; ~c[nil] means do not compile.

  The second argument ~c[k] is optional as well; it defaults to ~c[0].

  Two keyword arguments, ~c[:defaxioms-okp] and ~c[:skip-proofs-okp], determine
  how the system handles the inclusion of ~ilc[defaxiom] events and
  ~ilc[skip-proofs] events, respectively, in the book.  The value ~c[t] allows
  such events, but prints a warning message.  The value ~c[nil] is the default,
  and causes an error if such an event is found.

  The keyword argument ~c[:ttags] may normally be omitted.  A few constructs,
  used for example if you are building your own system based on ACL2, may
  require it.  ~l[defttag] for an explanation of this argument.

  To advanced users only: in the rare case that you are willing to add to
  compilation time in return for compiling the executable counterparts of
  functions defined in the book, you may supply a value of ~c[:all] for
  ~c[compile-flg].  This setting is useful for compiling a book whose functions
  are called during macroexpansion, because evaluation during macroexpansion is
  done in a ``safe mode'' that avoids calling raw Lisp functions
  (~pl[guards-and-evaluation]).

  The keyword argument ~c[:save-expansion] controls whether or not a so-called
  ``book expansion'' file is written, obtained by appending the string
  \"@expansion.lsp\" to the end of the book name.  ~l[make-event] for
  discussion of the book expansion; in a nutshell, ~ilc[make-event] calls
  generate forms that replace them in the book expansion.  Book expansion is
  skipped if ~c[compile-flg] and ~c[:save-expansion] are both ~c[nil].
  Otherwise, the values of ~c[nil] and ~c[t] for ~c[:save-expansion] cause the
  book expansion to be created only when a ~ilc[make-event] form occurs in a
  book (i.e., only if there is some expansion), or if at least one executable
  counterpart is to be compiled (see preceding paragraph).  If the book
  expansion is created, then it is deleted after compilation if
  ~c[:save-expansion] is ~c[nil].  Finally, if ~c[:save-expansion] is
  ~c[:save], then the book expansion file is created in all cases, and is not
  deleted.

  For a general discussion of books, ~pl[books].  ~c[Certify-book]
  is akin to what we have historically called a ``proveall'': all the
  forms in the book are ``proved'' to guarantee their admissibility.
  More precisely, ~c[certify-book] (1) reads the forms in the book,
  confirming that the appropriate packages are defined in the
  certification ~il[world]; (2) does the full admissibility checks on
  each form (proving termination of recursive functions, proving
  theorems, etc.), checking as it goes that each form is an embedded
  event form (~pl[embedded-event-form]); (3) rolls the ~il[world]
  back to the initial certification ~il[world] and does an
  ~ilc[include-book] of the book to check for ~ilc[local] incompatibilities
  (~pl[local-incompatibility]); (4) writes a ~il[certificate]
  recording not only that the book was certified but also recording
  the ~il[command]s necessary to recreate the certification ~il[world] (so
  the appropriate packages can be defined when the book is included in
  other ~il[world]s) and the check sums of all the ~il[books] involved
  (~pl[certificate]); (5) compiles the book if so directed (and
  then loads the object file in that case).  The result of executing a
  ~c[certify-book] ~il[command] is the creation of a single new event, which
  is actually an ~ilc[include-book] event.  If you don't want its
  included ~il[events] in your present ~il[world], simply execute ~c[:]~ilc[ubt]
  ~c[:here] afterwards.

  ~c[Certify-book] requires that the default ~il[defun-mode]
  (~pl[default-defun-mode]) be ~c[:]~ilc[logic] when certification is
  attempted.  If the mode is not ~c[:]~ilc[logic], an error is signalled.

  An error will occur if ~c[certify-book] has to deal with any
  uncertified book other than the one on which it was called.  For
  example, if the book being certified includes another book, that
  subbook must already have been certified.

  Certification occurs in some logical ~il[world], called the
  ``certification ~il[world].'' That ~il[world] must contain the ~ilc[defpkg]s
  needed to read and execute the forms in the book.  The ~il[command]s
  necessary to recreate that ~il[world] from the ACL2 initial
  ~il[world] will be copied into the ~il[certificate] created for the
  book.  Those ~il[command]s will be re-executed whenever the book is
  included, to ensure that the appropriate packages (and all other
  names used in the certification ~il[world]) are correctly defined.  The
  certified book will be more often usable if the certification
  ~il[world] is kept to a minimal extension of the ACL2 initial
  ~il[world].  Thus, before you call ~c[certify-book] for the first
  time on a book, you should get into the initial ACL2 ~il[world]
  (e.g., with ~c[:ubt 1] or just starting a new version of ACL2),
  ~ilc[defpkg] the desired packages, and then invoke ~c[certify-book].

  The ~c[k] argument to ~c[certify-book] must be either a nonnegative integer
  or else one of the symbols ~c[t] or ~c[?] in the ~c[ACL2] package.  If ~c[k]
  is an integer, then it must be the number of ~il[command]s that have been
  executed after the initial ACL2 ~il[world] to create the ~il[world] in which
  ~c[certify-book] was called.  One way to obtain this number is by doing
  ~c[:pbt :start] to see all the ~il[command]s back to the first one.

  If ~c[k] is ~c[t] it means that ~c[certify-book] should use the same
  ~il[world] used in the last certification of this book.  ~c[K] may be ~c[t]
  only if you call ~c[certify-book] in the initial ACL2 ~il[world] and there is
  a ~il[certificate] on file for the book being certified.  (Of course, the
  ~il[certificate] is probably invalid.)  In this case, ~c[certify-book] reads
  the old ~il[certificate] to obtain the ~il[portcullis] ~il[command]s and
  executes them to recreate the certification ~il[world].

  Finally, ~c[k] may be ~c[?], in which case there is no check made on the
  certification world.  That is, if ~c[k] is ~c[?] then no action related to
  the preceding two paragraphs is performed, which can be a nice convenience
  but at the cost of eliminating a potentially valuable check that the
  certification ~il[world] may be as expected.

  If you have a certified book that has remained unchanged for some
  time you are unlikely even to remember the appropriate ~ilc[defpkg]s
  for it.  If you begin to change the book, don't throw away its
  ~il[certificate] file just because it has become invalid!  It is an
  important historical document until the book is re-certified.

  When ~c[certify-book] is directed to produce a compiled file, it
  calls the Common Lisp function ~c[compile-file] on the original source
  file.  This creates a compiled file with an extension known to ACL2,
  e.g., if the book is named ~c[\"my-book\"] then the source file is
  ~c[\"my-book.lisp\"] and the compiled file under AKCL will be
  ~c[\"my-book.o\"] while under Lucid it will be ~c[\"my-book.lbin\"] or
  ~c[\"my-book.sbin\".]  The compiled file is then loaded.  When
  ~ilc[include-book] is used later on ~c[\"my-book\"] it will
  automatically load the compiled file, provided the compiled file has
  a later write date than the source file.  The only effect of such
  ~il[compilation] and loading is that the functions defined in the
  book execute faster.  ~l[guard] for a discussion of the issues.

  When ~c[certify-book] is directed not to produce a compiled file, it
  will delete any existing compiled file for the book, so as not to
  mislead ~ilc[include-book] into loading the now outdated compiled file.

  After execution of a ~c[certify-book] form, the value of
  ~ilc[acl2-defaults-table] is restored to what it was immediately before
  that ~c[certify-book] form was executed.
  ~l[acl2-defaults-table].

  This completes the tour through the ~il[documentation] of ~il[books].~/

  :cited-by other
  :cited-by Programming"

  (declare (xargs :guard (and (member-eq compile-flg
                                         '(nil t :all))
                              (member-eq save-expansion
                                         '(nil t :save)))))
  (list 'certify-book-fn
        (list 'quote user-book-name)
        (list 'quote k)
        (list 'quote compile-flg)
        (list 'quote defaxioms-okp)
        (list 'quote skip-proofs-okp)
        (list 'quote ttags)
        (list 'quote save-expansion)
        'state
        (list 'quote event-form)))

(defmacro certify-book! (user-book-name &optional (k '0) (compile-flg 't)
                                        &rest args)
  (declare (xargs :guard (and (integerp k) (<= 0 k))))
  
  ":Doc-Section Other

  a variant of ~ilc[certify-book]~/
  ~bv[]
  Examples:
  (certify-book! \"my-arith\" 3)     ;Certify in a world with 3
                                     ; commands, starting in a world
                                     ; with at least 3 commands.
  (certify-book! \"my-arith\")       ;Certify in the initial world.
  (certify-book! \"my-arith\" 0 nil) ;As above, but do not compile.~/

  General Form:
  (certify-book! book-name k compile-flg)
  ~ev[]
  where ~c[book-name] is a book name (~pl[book-name]), ~c[k] is a
  nonnegative integer used to indicate the ``certification ~il[world],''
  and ~c[compile-flg] indicates whether you wish to compile the
  (functions in the) book.

  This ~il[command] is identical to ~ilc[certify-book], except that the second
  argument ~c[k] may not be ~c[t] in ~c[certify-book!] and if ~c[k]
  exceeds the current ~il[command] number, then an appropriate ~ilc[ubt!] will
  be executed first.  ~l[certify-book] and ~pl[ubt!].~/"

  `(er-progn (ubt! ,(1+ k))
             (certify-book ,user-book-name ,k ,compile-flg ,@args)))

(deflabel pathname
  :doc
  ":Doc-Section acl2::Books

  introduction to filename conventions in ACL2~/

  The notion of pathname objects from Common Lisp is not supported in
  ACL2, nor is the function ~c[pathname].  However, ACL2 supports file
  operations, using conventions for naming files based on those of the
  Unix (trademark of AT&T) operating system, so that the character ~c[/]
  is used to terminate directory names.  Some file names are ``absolute''
  (complete) descriptions of a file or directory; others are
  ``relative'' to the current working directory or to the connected
  book directory (~pl[cbd]).  We emphasize that even for users of
  Windows-based systems or Macintosh computers, ACL2 file names are in
  the Unix style.  We will call these ~em[ACL2 pathnames], often
  omitting the ``ACL2.''~/

  Pathnames starting with the directory separator (~c[/]) are absolute
  pathnames.  All other pathnames are relative pathnames.  An
  exception is in the Microsoft Windows operating system, where the
  drive may be included, e.g., ~c[\"c:/home/smith/acl2/book-1.lisp\"].
  In fact, the drive ~em[must] be included in the portcullis of a book;
  ~pl[portcullis].

  Consider the following examples.  The filename string
  ~bv[]
  \"/home/smith/acl2/book-1.lisp\"
  ~ev[]
  is an absolute pathname, with top-level directory ~c[\"home\"],
  under that the directory ~c[\"smith\"] and then the directory
  ~c[\"acl2\"], and finally, within that directory the file
  ~c[\"book-1.lisp\"].  If the connected book directory is
  ~c[\"/home/smith/\"] (~pl[cbd]), then the filename string above
  also corresponds to the relative filename string \"acl2/book1.lisp\".~/")

(deflabel book-example
  :Doc
  ":Doc-Section Books

  how to create, certify, and use a simple book~/

  Suppose you have developed a sequence of admissible ~il[events] which you
  want to turn into a book.  We call this ``publishing'' the book.
  This note explains how to do that.~/

  A key idea of ~il[books] is that they are ``incremental'' in the
  sense that when you include a book in a host logical ~il[world], the
  ~il[world] is incrementally extended by the results established in that
  book.  This is allowed only if every name defined by the incoming
  book is either new or is already identically defined.
  ~l[redundant-events].  This is exactly the same problem faced
  by a programmer who wishes to provide a utility to other people: how
  can he make sure he doesn't create name conflicts?  The solution, in
  Common Lisp, is also the same: use packages.  While ~il[books] and
  packages have a very tenuous formal connection (every book must
  start with an ~ilc[in-package]), the creation of a book is intimately
  concerned with the package issue.  Having motivated what would
  otherwise appear as an unnecessary fascination with packages below,
  we now proceed with a description of how to publish a book.

  Just to be concrete, let's suppose you have already gotten ACL2 to
  accept the following sequence of ~il[command]s, starting in the ACL2
  initial ~il[state].
  ~bv[]
     (defpkg \"ACL2-MY-BOOK\"
             (union-eq *common-lisp-symbols-from-main-lisp-package*
                       *acl2-exports*))
     (in-package \"ACL2-MY-BOOK\")
     (defun app (x y)
       (if (consp x) (cons (car x) (app (cdr x) y)) y))
     (defun rev (x)
       (if (consp x) (app (rev (cdr x)) (list (car x))) nil))
     (defthm rev-app-hack
       (equal (rev (app a (list x))) (cons x (rev a))))
     (defthm rev-rev 
       (implies (acl2::true-listp x) (equal (rev (rev x)) x)))
  ~ev[]
  Observe that the first form above defines a package (which imports
  the symbols defined in CLTL such as ~ilc[if] and ~ilc[cons] and the
  symbols used to ~il[command] ACL2 such as ~ilc[defun] and ~ilc[defthm]).  The
  second form selects that package as the current one.  All subsequent
  forms are read into that package.  The remaining forms are just
  event forms: ~ilc[defun]s and ~ilc[defthm]s in this case.

  Typically you would have created a file with Emacs containing these
  forms and you will have submitted each of them interactively to ACL2
  to confirm that they are all admissible.  That interactive
  verification should start in ACL2's initial ~il[world] ~-[] although
  you might, of course, start your sequence of ~il[events] with some
  ~ilc[include-book]s to build a more elaborate ~il[world].

  The first step towards publishing a book containing the results
  above is to create a file that starts with the ~ilc[in-package] and
  then contains the rest of the forms.  Let's call that file
  ~c[\"my-book.lisp\"].  The name is unimportant, except it must end
  with ~c[\".lisp\"].  If there are ~il[events] that you do not wish to be
  available to the user of the book ~-[] e.g., lemmas you proved on your
  way toward proving the main ones ~-[] you may so mark them by
  enclosing them in ~ilc[local] forms.  ~l[local].  Let us suppose
  you wish to hide ~c[rev-app-hack] above.  You may also add standard Lisp
  comments to the file.  The final content of ~c[\"my-book.lisp\"]
  might be:
  ~bv[]
   ; This book contains my app and rev functions and the theorem
   ; that rev is its own inverse.

     (in-package \"ACL2-MY-BOOK\")
     (defun app (x y)
       (if (consp x) (cons (car x) (app (cdr x) y)) y))
     (defun rev (x)
       (if (consp x) (app (rev (cdr x)) (list (car x))) nil))

   ; The following hack is not exported.
     (local (defthm rev-app-hack
       (equal (rev (app a (list x))) (cons x (rev a)))))

     (defthm rev-rev 
       (implies (acl2::true-listp x) (equal (rev (rev x)) x)))
  ~ev[]
  The file shown above ~st[is] the book.  By the time this note is
  done you will have seen how to certify that the book is correct, how
  to compile it, and how to use it in other host ~il[world]s.  Observe that
  the ~ilc[defpkg] is not in the book.  It cannot be: Common Lisp
  compilers disagree on how to treat new package definitions appearing
  in files to be compiled.

  Since a book is just a source file typed by the user, ACL2 provides
  a mechanism for checking that the ~il[events] are all admissible and then
  marking the file as checked.  This is called certification.  To
  certify ~c[\"my-book.lisp\"] you should first get into ACL2 with an
  initial ~il[world].  Then, define the package needed by the book, by
  typing the following ~ilc[defpkg] to the ACL2 ~il[prompt]:
  ~bv[]
  ACL2 !>(defpkg \"ACL2-MY-BOOK\"
                 (union-eq *common-lisp-symbols-from-main-lisp-package*
                           *acl2-exports*))
  ~ev[]
  Then execute the ~il[command]:
  ~bv[]
  ACL2 !>(certify-book \"my-book\" 1 t) ; the `t' is in fact the default
  ~ev[]
  Observe that you do not type the ~c[\".lisp\"] part of the file
  name.  For purposes of ~il[books], the book's name is ~c[\"my-book\"] and
  by the time all is said and done, there will be several extensions
  in addition to the ~c[\".lisp\"] extension associated with it.

  The ~c[1] tells ~ilc[certify-book] that you acknowledge that there is
  one command in this ``certification ~il[world]'' (namely the ~ilc[defpkg]).
  To use the book, any prospective host ~il[world] must be extended by
  the addition of whatever ~il[command]s occurred before certification.  It
  would be a pity to certify a book in a ~il[world] containing junk because
  that junk will become the ``~il[portcullis]'' guarding entrance to
  the book.  The ~c[t] above tells ~ilc[certify-book] that you wish to
  compile ~c[\"my-book.lisp\"] also.  ~ilc[Certify-book] makes many checks
  but by far the most important and time-consuming one is that it
  ``proves'' every event in the file.

  When ~ilc[certify-book] is done it will have created two new files.
  The first will be called ~c[\"my-book.cert\"] and contains the
  ``~il[certificate]'' attesting to the admissibility of the ~il[events] in
  ~c[\"my-book.lisp\"].  The ~il[certificate] contains the ~ilc[defpkg] and any
  other forms necessary to construct the certification ~il[world].  It also
  contains various check sums used to help you keep track of which
  version of ~c[\"my-book.lisp\"] was certified.

  The second file created by ~ilc[certify-book] is the compiled version
  of ~c[\"my-book.lisp\"] and will have a name that is assigned by the
  host compiler (e.g., ~c[\"my-book.o\"] in AKCL, ~c[\"my-book.lbin\"]
  or ~c[\"my-book.sbin\"] in Lucid).  ~ilc[Certify-book] will also load
  this object file.  When ~ilc[certify-book] is done, you may throw away
  the logical ~il[world] it created, for example by executing the
  ~il[command] ~c[:u].

  To use the book later in any ACL2 session, just execute the event
  ~c[(include-book \"my-book\")].  This will do the necessary
  ~ilc[defpkg], load the non-~ilc[local] ~il[events] in ~c[\"my-book.lisp\"] and
  then load the compiled code for the non-local functions defined in
  that file.  Checks are made to ensure that the ~il[certificate] file
  exists and describes the version of ~c[\"my-book.lisp\"] that is
  read.  The compiled code is loaded if and only if it exists and has
  a later write date than the source file.

  Since ~ilc[include-book] is itself an event, you may put such forms
  into other ~il[books].  Thus it is possible for the inclusion of a single
  book to lead to the inclusion of many others.  The check sum
  information maintained in ~il[certificate]s helps deal with the
  version control problem of the referenced ~il[books].  I.e., if this
  version of ~c[\"my-book\"] is used during the certification of
  ~c[\"your-book\"], then the ~il[certificate] for ~c[\"your-book\"] includes
  the check sum of this version of ~c[\"my-book\"].  If a later
  ~c[(include-book \"your-book\")] finds a version of ~c[\"my-book\"]
  with a different check sum, an error is signalled.  But check sums
  are not perfect and the insecurity of the host file system prevents
  ACL2 from guaranteeing the logical soundness of an ~ilc[include-book]
  event, even for a book that appears to have a valid ~il[certificate]
  (they can be forged, after all).  (~l[certificate] for further
  discussion.)

  This concludes the example of how to create, certify and use a book.
  If you wish, you could now review the ~il[documentation] for book-related
  topics (~pl[books]) and browse through them.  They'll probably
  make sense in this context.  Alternatively, you could continue the
  ``guided tour'' through the rest of the ~il[documentation] of ~il[books].
  ~l[book-name], following the pointer given at the conclusion.")

(deflabel full-book-name
  :doc
  ":Doc-Section Books

  book naming conventions assumed by ACL2~/

  For this discussion we assume that the resident operating system is
  Unix (trademark of AT&T), but analogous remarks apply to other
  operating systems supported by ACL2, in particular, the Macintosh
  operating system where `~c[:]' plays roughly the role of `~c[/]' in
  Unix; ~pl[pathname].

  ACL2 defines a ``full book name'' to be an ``absolute filename
  string,'' that may be divided into contiguous sections:  a
  ``directory string'', a ``familiar name'' and an ``extension''.
  ~l[pathname] for the definitions of ``absolute,'' ``filename
  string,'' and other notions pertaining to naming files.  Below we
  exhibit the three sections of one such string:
  ~bv[]
  \"/usr/home/smith/project/arith.lisp\"

  \"/usr/home/smith/project/\"           ; directory string
                          \"arith\"      ; familiar name
                               \".lisp\" ; extension~/
  ~ev[]
  The sections are marked by the rightmost slash and rightmost dot,
  as shown below.
  ~bv[]
  \"/usr/home/smith/project/arith.lisp\"
                          |     |
                          slash dot
                          |     |
  \"/usr/home/smith/project/\"           ; directory string
                          \"arith\"      ; familiar name
                               \".lisp\" ; extension
  ~ev[]
  The directory string includes (and terminates with) the rightmost
  slash.  The extension includes (and starts with) the rightmost dot.
  The dot must be strictly to the right of the slash so that the
  familiar name is well-defined and nonempty.

  If you are using ACL2 on a system in which file names do not have
  this form, please contact the authors and we'll see what we can do
  about generalizing ACL2's conventions.")

(deflabel book-name
  :doc
  ":Doc-Section  Books

  conventions associated with book names~/
  ~bv[]
  Examples:
  \"list-processing\"
  \"/usr/home/smith/my-arith\"
  ~ev[]
  Book names are string constants that can be elaborated into file
  names.  We elaborate book names by concatenating the ``connected
  book directory'' (~pl[cbd]) string on the left and some
  ``extension,'' such as ~c[\".lisp\"], on the right.  However, the
  connected book directory is not added if the book name itself
  already represents an absolute file name.  Furthermore,
  ~ilc[include-book] and ~ilc[certify-book] temporarily reset the connected
  book directory to be the directory of the book being processed.
  This allows ~ilc[include-book] forms to use file names without explicit
  mention of the enclosing book's directory.  This in turn allows
  ~il[books] (together with those that they include, using
  ~ilc[include-book]) to be moved between directories while maintaining
  their certification and utility.

  You may wish to read elsewhere for details of ACL2 file name
  conventions (~pl[pathname]), for a discussion of the filename
  that is the result of the elaboration described here
  (~pl[full-book-name]), and for details of the concept of the
  connected book directory (~pl[cbd]).  For details of how
  ~ilc[include-book] (~pl[include-book]) and ~ilc[certify-book]
  (~pl[certify-book]) use these concepts, see below.~/

  Often a book name is simply the familiar name of the file.
  (~l[full-book-name] for discussion of the notions of
  ``directory string,'' ``familiar name,'' and ``extension''.  These
  concepts are not on the guided tour through ~il[books] and you
  should read them separately.)  However, it is permitted for book
  names to include a directory or part of a directory name.  Book
  names never include the extension, since ACL2 must routinely tack
  several different extensions onto the name during ~ilc[include-book].
  For example, ~ilc[include-book] uses the ~c[\".lisp\"], ~c[\".cert\"] and
  possibly the ~c[\".o\"] or ~c[\".lbin\"] extensions of the book name.

  Book names are elaborated into full file names by ~ilc[include-book]
  and ~ilc[certify-book].  This elaboration is sensitive to the
  ``connected book directory.'' The connected book directory is an
  absolute filename string (~pl[pathname]) that is part of the
  ACL2 ~ilc[state].  (You may wish to ~pl[cbd] and to
  ~pl[set-cbd] ~-[] note that these are not on the guided tour).
  If a book name is an absolute filename string, ACL2 elaborates it
  simply by appending the desired extension to the right.
  If a book name is a relative filename string, ACL2 appends the
  connected book directory on the left and the desired extension on
  the right.

  Note that it is possible that the book name includes some partial
  specification of the directory.  For example, if the connected book
  directory is ~c[\"/usr/home/smith/\"] then the book name
  ~c[\"project/task-1/arith\"] is a book name that will be elaborated
  to
  ~bv[]
  \"/usr/home/smith/project/task-1/arith.lisp\".
  ~ev[]

  Observe that while the ~il[events] in this ~c[\"arith\"] book are being
  processed the connected book directory will temporarily be set to
  ~bv[]
  \"/usr/home/smith/project/task-1/\".
  ~ev[]
  Thus, if the book requires other ~il[books], e.g.,
  ~bv[]
  (include-book \"naturals\")
  ~ev[]
  then it is not necessary to specify the directory on which they
  reside provided that directory is the same as the superior book.

  This inheritance of the connected book directory and its use to
  elaborate the names of inferior ~il[books] makes it possible to move
  ~il[books] and their inferiors to new directories, provided they maintain
  the same relative relationship.  It is even possible to move with
  ease whole collections of ~il[books] to different filesystems that use
  a different operating system than the one under which the original
  certification was performed.

  The ~c[\".cert\"] extension of a book, if it exists, is presumed to
  contain the most recent ~il[certificate] for the book.
  ~l[certificate] (or, if you are on the guided tour, wait until
  the tour gets there).

  ~l[book-contents] to continue the guided tour.")

(deflabel book-contents
  :doc
  ":Doc-Section  Books

  restrictions on the forms inside ~il[books]~/
  ~bv[]
  Example Book:

  ; This book defines my app function and the theorem that it is
  ; associative.  One irrelevant help lemma is proved first but
  ; it is local and so not seen by include-book.  I depend on the
  ; inferior book \"weird-list-primitives\" from which I get
  ; definitions of hd and tl.

  (in-package \"MY-PKG\")

  (include-book \"weird-list-primitives\")

  (defun app (x y) (if (consp x) (cons (hd x) (app (tl x) y)) y))

  (local
   (defthm help-lemma
     (implies (true-listp x) (equal (app x nil) x))))

  (defthm app-is-associative
    (equal (app (app a b) c) (app a (app b c))))~/

  ~ev[]
  The first form in a book must be ~c[(in-package \"pkg\")] where
  ~c[\"pkg\"] is some package name known to ACL2 whenever the book is
  certified.  The rest of the forms in a book are embedded event
  forms, i.e., ~ilc[defun]s, ~ilc[defthm]s, etc., some of which may be
  marked ~ilc[local].  ~l[embedded-event-form].  The usual Common
  Lisp commenting conventions are provided.  Note that since a book
  consists of embedded event forms, we can talk about the
  ``~ilc[local]'' and ``non-local'' ~il[events] of a book.

  Because ~ilc[in-package] is not an embedded event form, the only
  ~ilc[in-package] in a book is the initial one.  Because ~ilc[defpkg] is
  not an embedded event form, a book can never contain a ~ilc[defpkg]
  form.  Because ~ilc[include-book] is an embedded event form, ~il[books] may
  contain references to other ~il[books].  This makes ~il[books] structured
  objects.

  When the forms in a book are read from the file, they are read with
  ~ilc[current-package] set to the package named in the ~ilc[in-package]
  form at the top of the file.  The effect of this is that all symbols
  are ~il[intern]ed in that package, except those whose packages are given
  explicitly with the ``::'' notation.  For example, if a book begins
  with ~c[(in-package \"ACL2-X\")] and then contains the form
  ~bv[]
    (defun fn (x)
      (acl2::list 'car x))
  ~ev[]
  then ~ilc[defun], ~c[fn], ~c[x], and ~ilc[car] are all ~il[intern]ed in the
  ~c[\"ACL2-X\"] package.  I.e., it is as though the following form
  were read instead:
  ~bv[]
    (acl2-x::defun acl2-x::fn (acl2-x::x)
        (acl2::list 'acl2-x::car acl2-x::x)).
  ~ev[]
  Of course, ~c[acl2-x::defun] would be the same symbol as
  ~c[acl2::defun] if the ~c[\"ACL2-X\"] package imported ~c[acl2::defun].

  If each book has its own unique package name and all the names
  defined within the book are in that package, then name clashes
  between ~il[books] are completely avoided.  This permits the construction
  of useful logical ~il[world]s by the successive inclusion of many ~il[books].
  Although it is often too much trouble to manage several packages,
  their judicious use is a way to minimize name clashes.  Often, a
  better way is to use ~c[local]; ~pl[local].

  How does ~ilc[include-book] know the definitions of the packages used in a
  book, since ~ilc[defpkg]s cannot be among the forms?  More generally,
  how do we know that the forms in a book will be admissible in the
  host logical ~il[world] of an ~ilc[include-book]?  ~l[certificate] for
  answers to these questions.")

(deflabel certificate
  :doc
  ":Doc-Section Books

  how a book is known to be admissible and where its ~ilc[defpkg]s reside~/

  A book, say ~c[\"arith\"], is said to have a ``certificate'' if there
  is a file named ~c[\"arith.cert\"].  Certificates are created by the
  function ~ilc[certify-book] and inspected by ~ilc[include-book].  Check
  sums are used to help ensure that certificates are legitimate and
  that the corresponding book has not been modified since
  certification.  But because the file system is insecure and check
  sums are not perfect it is possible for the inclusion of a book to
  cause inconsistency even though the book carries an impeccable
  certificate.

  The certificate includes the version number of the certifying ACL2.
  A book is considered uncertified if it is included in an ACL2
  with a different ~il[version] number.~/

  The presence of a ``valid'' certificate file for a book attests to
  two things: all of the ~il[events] of the book are admissible in a
  certain extension of the initial ACL2 logic, and the non-~ilc[local]
  ~il[events] of the book are independent of the ~ilc[local] ones
  (~pl[local-incompatibility]).  In addition, the certificate
  contains the ~il[command]s used to construct the ~il[world] in which
  certification occurred.  Among those ~il[command]s, of course, are the
  ~ilc[defpkg]s defining the packages used in the book.  When a book is
  included into a host ~il[world], that ~il[world] is first extended
  by the ~il[command]s listed in the certificate for the book.  Unless that
  causes an error due to name conflicts, the extension ensures that
  all the packages used by the book are identically defined in the
  host ~il[world].

  ~em[Security:]

  Because the host file system is insecure, there is no way ACL2 can
  guarantee that the contents of a book remain the same as when its
  certificate was written.  That is, between the time a book is
  certified and the time it is used, it may be modified.  Furthermore,
  certificates can be counterfeited.  Check sums (~pl[check-sum])
  are used to help detect such problems.  But check sums provide
  imperfect security: two different files can have the same check sum.

  Therefore, from the strictly logical point of view, one must
  consider even the inclusion of certified ~il[books] as placing a burden
  on the user:~bq[]

  The non-erroneous inclusion of a certified book is consistency
  preserving provided (a) the objects read by ~ilc[include-book] from the
  certificate were the objects written there by a ~ilc[certify-book] and
  (b) the forms read by ~ilc[include-book] from the book itself are the
  forms read by the corresponding ~ilc[certify-book].

  ~eq[]We say that a given execution of ~ilc[include-book] is ``certified''
  if a certificate file for the book is present and well-formed and
  the check sum information contained within it supports the
  conclusion that the ~il[events] read by the ~ilc[include-book] are the ones
  checked by ~ilc[certify-book].  When an uncertified ~ilc[include-book]
  occurs, warnings are printed or errors are caused.  But even if no
  warning is printed, you must accept burdens (a) and (b) if you use
  ~il[books].  These burdens are easier to live with if you protect your
  ~il[books] so that other users cannot write to them, you abstain from
  running concurrent ACL2 jobs, and you abstain from counterfeiting
  certificates.  But even on a single user uniprocessor, you can shoot
  yourself in the foot by using the ACL2 ~il[io] primitives to fabricate an
  inconsistent book and the corresponding certificate.

  Note that part (a) of the burden described above implies, in
  particular, that there are no guarantees when a certificate is
  copied.  When ~il[books] are renamed (as by copying them), it is
  recommended that their certificates be removed and the ~il[books] be
  recertified.  The expectation is that recertification will go
  through without a hitch if relative ~il[pathname]s are used.
  ~l[pathname], which is not on the guided tour.

  Certificates essentially contain two parts, a ~il[portcullis] and a
  ~il[keep].  There is a third part, an ~c[expansion-alist], in order
  to record expansions if ~ilc[make-event] has been used, but the user
  need not be concerned with that level of detail.

  ~l[portcullis] to continue the guided tour through ~il[books].")

(deflabel portcullis

; This documentation string formerly concluded (just before "~l[keep] to
; continue...") with the following discussion, until Version  2.6.  Now that we
; change include-book forms in the portcuillis to use absolute pathnames, we do
; not need this.

#|
  Recall that we disallow ~ilc[include-book] ~il[events] from the portcullis
  unless the included book's name is an absolute filename
  (~l[pathname]).  Thus, for example, under the Unix operating
  system it is impossible to certify a book if the certification
  ~il[world] was created with
  ~bv[]
  ACL2 !>(~il[include-book] \"arith\")
  ~ev[]
  The problem here is that the file actually read on behalf of such
  an ~ilc[include-book] depends upon the then current setting of the
  connected book directory (~pl[cbd]).  That setting could be
  changed before the certification occurs.  If we were to copy
  ~c[(include-book \"arith\")] into the portcullis of the book being
  certified, there is no assurance that the ~c[\"arith\"] book included
  would come from the correct directory.  However, by requiring that
  the ~ilc[include-book]s in the certification ~il[world] give book names
  that begin with slash we effectively require you to specify the full
  file name of each book involved in creating your certification
  ~il[world].  Observe that the execution of
  ~bv[]
  (~il[include-book] \"/usr/local/src/acl2/library/arith\")
  ~ev[]
  does not depend on the current book directory.  On the other hand,
  this requirement ~-[] effectively that absolute file names be used in
  the certification ~il[world] ~-[] means that a book that requires
  another book in its certification ~il[world] will be rendered
  uncertified if the required book is removed to another directory.
  If possible, any ~ilc[include-book] ~il[command] required for a book ought
  to be placed in the book itself and not in the certification
  ~il[world].  The only time this cannot be done is if the required
  book is necessary to some ~ilc[defpkg] required by your book.  Of
  course, this is just the same advice we have been giving: keep the
  certification ~il[world] as elementary as possible.
|#

  :doc
  ":Doc-Section Books

  the gate guarding the entrance to a certified book~/

  The certificate (~pl[certificate] for general information) of a
  certified file is divided into two parts, a portcullis and a
  ~il[keep].  These names come from castle lore.  The portcullis of a
  castle is an iron grate that slides up through the ceiling of the
  tunnel-like entrance.  The portcullis of a book ensures that
  ~ilc[include-book] does not start to read the book until the
  appropriate context has been created.~/

  Technically, the portcullis consists of the ~il[version] number of
  the certifying ACL2, a list of ~il[command]s used to create the
  ``certification ~il[world]'' and an alist specifying the check sums
  of all the ~il[books] included in that ~il[world].  The portcullis
  is constructed automatically by ~ilc[certify-book] from the ~il[world]
  in which ~ilc[certify-book] is called, but that ~il[world] must have
  certain properties described below.  After listing the properties we
  discuss the issues in a more leisurely manner.

  Each ~il[command] in the portcullis must be either a ~ilc[defpkg] form or an
  embedded event form (~pl[embedded-event-form]).

  Consider a book to be certified.  The book is a file containing
  event forms.  Suppose the file contains references to such symbols
  as ~c[my-pkg::fn] and ~c[acl2-arith::cancel], but that the book itself
  does not create the packages.  Then a hard Lisp error would be
  caused merely by the attempt to read the expressions in the book.
  The corresponding ~ilc[defpkg]s cannot be written into the book itself
  because the book must be compilable and Common Lisp compilers differ
  on the rules concerning the inline definition of new packages.  The
  only safe course is to make all ~ilc[defpkg]s occur outside of compiled
  files.

  More generally, when a book is certified it is certified within some
  logical ~il[world].  That ``certification ~il[world]'' contains not only
  the necessary ~ilc[defpkg]s but also, perhaps, function and constant
  definitions and maybe even references to other ~il[books].  When
  ~ilc[certify-book] creates the ~il[certificate] for a file it recovers
  from the certification ~il[world] the ~il[command]s used to create that
  ~il[world] from the initial ACL2 ~il[world].  Those ~il[command]s become
  part of the portcullis for the certified book.  In addition,
  ~ilc[certify-book] records in the portcullis the check sums
  (~pl[check-sum]) of all the ~il[books] included in the certification
  ~il[world].

  ~ilc[Include-book] presumes that it is impossible even to read the
  contents of a certified book unless the portcullis can be
  ``raised.'' To raise the portcullis we must be able to execute
  (possibly redundantly, but certainly without error), all of the
  ~il[command]s in the portcullis and then verify that the ~il[books] thus
  included were identical to those used to build the certification
  ~il[world] (up to check sum).  This raising of the portcullis must
  be done delicately since ~ilc[defpkg]s are present: we cannot even read
  a ~il[command] in the portcullis until we have successfully executed the
  previous ones, since packages are being defined.

  Clearly, a book is most useful if it is certified in the most
  elementary extension possible of the initial logic.  If, for
  example, your certification ~il[world] happens to contain a
  ~ilc[defpkg] for ~c[\"MY-PKG\"] and the function ~c[foo], then those
  definitions become part of the portcullis for the book.  Every time
  the book is included, those names will be defined and will have to
  be either new or redundant (~pl[redundant-events]).  But if
  those names were not necessary to the certification of the book,
  their presence would unnecessarily restrict the utility of the book.

  ~l[keep] to continue the guided tour of ~il[books].")

(deflabel version
  :doc
  ":Doc-Section Miscellaneous

  ACL2 Version Number~/

  To determine the version number of your copy of ACL2, evaluate the form
  ~c[(@ acl2-version)].  The value will be a string.  For example,
  ~bv[]
  ACL2 !>(@ acl2-version)
  \"ACL2 Version 3.1\"
  ~ev[]
  ~/

  The part of the string after ~c[\"ACL2 Version \"] is of the form ~c[x.y] or
  ~c[x.y.z], optionally followed by a succession of values in parentheses,
  where ~c[x], ~c[y], and ~c[z] are natural numbers.  If ~c[z] is omitted then
  it is implicitly 0.  We refer to ~c[X], ~c[y], and ~c[z] as the ``major'',
  ``minor'', and ``incrl'' fields, respectively.  The incrl field is used for
  incremental releases.  The discussion just below assumes that incremental
  releases are not employed at the user's site, i.e., the incrl fields are
  always 0.  We remove this assumption when we discuss incremental releases at
  the end of this documenttation topic.

  ~il[Books] are considered certified only in the same version of ACL2
  in which the certification was done.  The ~il[certificate] file
  records the version number of the certifying ACL2 and
  ~il[include-book] considers the book uncertified if that does not
  match the current version number.  Thus, each time we release a new
  version of ACL2, previously certified books should be recertified.

  Note that there are over 150 constants in the system, most having to do with
  the fact that ACL2 is coded in ACL2.  Many of these, for example
  ~c[*common-lisp-specials-and-constants*] and ~c[*acl2-exports*], may change
  from version to version, and this can cause unsoundness.  For example, the
  symbol ~c['set-difference-eq] was added to ~c[*acl2-exports*] in Version_2.9,
  so we can certify a book in Version_2.8 containing the following theorem,
  which is false in Version_2.9.
  ~bv[]
  (null (member 'set-difference-eq *acl2-exports*))
  ~ev[]
  Therefore, we need to disallow inclusion of such a book in a Version_2.9
  session, which otherwise would allow us to prove ~c[nil].  Furthermore, it is
  possible that from one version of the system to another we might change, say,
  the default values on some system function or otherwise make ``intentional''
  changes to the axioms.  It is even possible one version of the system is
  discovered to be unsound and we release a new version to correct our error.

  Therefore we adopted the draconian policy that books are certified
  by a given version of ACL2 and ``must'' be recertified to be used
  in other versions.  We put ``must'' in quotes because in fact, ACL2
  allows a book that was certified in one ACL2 version to be included
  in a later version, using ~ilc[include-book].  But ACL2 does not allow
  ~ilc[certify-book] to succeed when such an ~ilc[include-book] is executed on its
  behalf.  Also, you may experience undesirable behavior if you avoid
  recertification when moving to a different version.  (We try to
  prevent some undesirable behavior by refusing to load the compiled
  code for an uncertified book, but this does not guarantee good
  behavior.)  Hence we recommend that you stick to the draconion
  policy of recertifying books when updating to a new ACL2 version.

  The string ~c[(@ acl2-version)] can contain implementation-specific
  information in addition to the version number.  For example, in
  Macintosh Common Lisp (MCL) ~c[(char-code #\Newline)] is 13, while as
  far as we know, it is 10 in every other Common Lisp.  Our concern is
  that one could certify a book in an MCL-based ACL2 with the theorem
  ~bv[]
  (equal (char-code #\Newline) 13)
  ~ev[]
  and then include this book in another Lisp and thereby prove ~c[nil].
  So, when a book is certified in an MCL-based ACL2, the book's
  ~il[certificate] mentions ``MCL'' in its version string.  Moreover,
  ~c[(@ acl2-version)] similarly mentions ``MCL'' when the ACL2 image has
  been built on top of MCL.  Thus, an attempt to include a book in an
  MCL-based ACL2 that was certified in a non-MCL-based ACL2, or
  vice-versa, will be treated like an attempt to include an
  uncertified book.

  ~em[Incremental releases.]

  From time to time, so-called ``incremental releases'' of ACL2 are made
  available.  These releases are thoroughly tested on at least two platforms;
  ``normal'' releases, on the other hand, are thoroughly tested on many more
  platforms (perhaps a dozen or so) and are accompanied by updates to the ACL2
  home page.  We provide incremental releases in order to provide timely
  updates for ACL2 users who want them, without imposing unnecessary burdens on
  either on the ACL2 implementors or on ACL2 users who prefer to update less
  frequently.  The implementors expect users to update their copies of ACL2
  when normal releases are made available, but not necessarily when incremental
  releases are made available.

  Incremental releases are accompanied by a bump in the incrl field of the
  version field, while normal releases are accompanied by a bump in the minor
  or (much less frequently) major field and zeroing out of the incrl field.

  Note that LOGICALLY SPEAKING, INCREMENTAL RELEASES ARE FULL-FLEDGE RELEASES.
  However, ACL2 users may wish to experiment with incremental releases without
  recertifying all of their existing ACL2 ~il[books] (~pl[certify-book]).  In
  order to learn how to avoid such recertification, ~pl[set-tainted-okp].  The
  basic idea is that if certification may depend on including books from an
  ACL2 version with a different incrl field, the book's ~il[certificate] is
  marked with a ``tainted'' version, i.e., a version with ~c[\"(tainted\"] as a
  substring.  Subsequent inclusion of any such book is restricted to sessions
  in which the user explicitly invokes ~c[(]~ilc[set-tainted-okp]~c[ t)], which
  is intended as an acknowledgment that including such a book may render the
  ACL2 session unsound.~/")

(deflabel keep
  :doc
  ":Doc-Section Books

  how we know if ~ilc[include-book] read the correct files~/

  The certificate (~pl[certificate] for general information) of a
  certified file is divided into two parts, a ~il[portcullis] and a
  keep.  These names come from castle lore.  The keep is the strongest
  and usually tallest tower of a castle from which the entire
  courtyard can be surveyed by the defenders.  The keep of a book is a
  list of file names and check sums used after the book has been
  included, to determine if the files read were (up to check sum)
  those certified.~/

  Once the ~il[portcullis] is open, ~ilc[include-book] can enter the book
  and read the event forms therein.  The non-~ilc[local] event forms are
  in fact executed, extending the host theory.  That may read in other
  ~il[books].  When that has been finished, the keep of the
  ~il[certificate] is inspected.  The keep is a list of the book names
  which are included (hereditarily through all subbooks) in the
  certified book (including the certified book itself) together with
  the check sums of the objects in those ~il[books] at the time of
  certification.  We compare the check sums of the ~il[books] just included
  to the check sums of the ~il[books] stored in the keep.  If differences
  are found then we know that the book or one of its subbooks has been
  changed since certification.

  ~l[include-book] to continue the guided tour through ~il[books].")

; The documentation for include-book is in axioms.lisp, where the
; include-book event is defined.

(deflabel uncertified-books
  :doc
  ":Doc-Section Books

  invalid ~il[certificate]s and uncertified ~il[books]~/

  ~ilc[Include-book] has a special provision for dealing with uncertified
  ~il[books]: If the file has no ~il[certificate] or an invalid
  ~il[certificate] (i.e., one whose check sums describe files other
  than the ones actually read), a warning is printed and the book is
  otherwise processed as though it were certified and had an open
  ~il[portcullis].  (For details ~pl[books], ~pl[certificate],
  and ~pl[portcullis].)

  This can be handy, but it can have disastrous consequences.~/

  The provision allowing uncertified ~il[books] to be included can
  have disastrous consequences, ranging from hard lisp errors, to
  damaged memory, to quiet logical inconsistency.

  It is possible for the inclusion of an uncertified book to render
  the logic inconsistent.  For example, one of its non-~ilc[local] ~il[events]
  might be ~c[(defthm t-is-nil (equal t nil))].  It is also possible
  for the inclusion of an uncertified book to cause hard errors or
  ~il[breaks] into raw Common Lisp.  For example, if the file has been
  edited since it was certified, it may contain too many open
  parentheses, causing Lisp to read past ``end of file.'' Similarly,
  it might contain non-ACL2 objects such as ~c[3.1415] or ill-formed
  event forms that cause ACL2 code to break.

  Even if a book is perfectly well formed and could be certified (in a
  suitable extension of ACL2's initial ~il[world]), its uncertified
  inclusion might cause Lisp errors or inconsistencies!  For example,
  it might mention packages that do not exist in the host ~il[world].
  The ~il[portcullis] of a certified book ensures that the correct
  ~ilc[defpkg]s have been admitted, but if a book is read without
  actually raising its ~il[portcullis], symbols in the file, e.g.,
  ~c[acl2-arithmetic::fn], could cause ``unknown package'' errors in
  Common Lisp.  Perhaps the most subtle disaster occurs if the host
  ~il[world] does have a ~ilc[defpkg] for each package used in the book
  but the host ~ilc[defpkg] imports different symbols than those required
  by the ~il[portcullis].  In this case, it is possible that formulas
  which were theorems in the certified book are non-theorems in the
  host ~il[world], but those formulas can be read without error and
  will then be quietly assumed.

  In short, if you include an uncertified book, ~st[all bets are off]
  regarding the validity of the future behavior of ACL2.

  That said, it should be noted that ACL2 is pretty tough and if
  errors don't occur, the chances are that deductions after the
  inclusion of an uncertified book are probably justified in the
  (possibly inconsistent) logical extension obtained by assuming the
  admissibility and validity of the definitions and conjectures in the
  book.")

(deflabel book-makefiles
  :Doc
  ":Doc-Section Books

  makefile support provided with the ACL2 distribution~/

  This topic describes the ACL2 methodology for using makefiles to assist in
  the automation of the certification of collections of ACL2 ~il[books].  We
  assume here a familiarity with Unix/Linux ~c[make].  We also assume that you
  are using GNU ~c[make] rather than some other flavor of ~c[make].~/

  ACL2's regression suite is run using ~c[Makefile]s that include
  ~c[books/Makefile-generic].  You can look at existing ~c[Makefile]s to
  understand how to create your own ~c[Makefile]s.  Here are the seven steps to
  follow to create a ~c[Makefile] for a directory that contains books to be
  certified, and certify them using that ~c[Makefile].  Below these steps we
  conclude with discussion of other capabilties provided by
  ~c[books/Makefile-generic].

  1. Include the file ~c[books/Makefile-generic].  For example, if you look at
  ~c[books/misc/Makefile] then you'll see that it starts with this line:
  ~bv[]
  include ../Makefile-generic
  ~ev[]
  Note that ~c[../] should be replaced by the appropriate path to
  ~c[books/Makefile-generic].  AND PLEASE NOTE:  This ~c[include] line should
  precede the lines mentioned below.

  2. Define the ~c[ACL2] variable.  For example, file
  ~c[books/arithmetic-3/pass1/Makefile] starts as follows.
  ~bv[]
  include ../../Makefile-generic
  ACL2 = ../../../saved_acl2
  ~ev[]
  Note that you will need to provide the appropriate path to your ACL2
  executable.

  3. (Optional; usually skipped.)  Set the ~c[INHIBIT] variable if you want to
  see more than the summary output.  For example, if you want to see the same
  output as you would normally see at the terminal, put this line in your
  Makefile after the ~c[include] and ~c[ACL2] lines.
  ~bv[]
  INHIBIT = (assign inhibit-output-lst (list (quote proof-tree)))
  ~ev[]
  For other values to use for ~c[INHIBIT], ~pl[set-inhibit-output-lst] and see
  the original setting of ~c[INHIBIT] in ~c[books/Makefile-generic].

  4. Specify the books to be certified.  If every file with extension ~c[.lisp]
  is a book that you want to certify, you can skip this step.  Otherwise, put a
  line in your ~c[Makefile] after the ones above that specifies the books to be
  certified.  The following example, from
  ~c[books/finite-set-theory/osets/Makefile], should make this clear.
  ~bv[]
  BOOKS = computed-hints fast instance map membership outer primitives \\
          quantify set-order sets sort
  ~ev[]  

  5. Create ~c[.acl2] files for books that are to be certified in other than
  the initial ACL2 world (~pl[portcullis]).  For example, if you look in
  ~c[books/arithmetic/equalities.acl2] you will see ~ilc[defpkg] forms followed
  by a ~ilc[certify-book] command, because it was determined that ~ilc[defpkg]
  forms were necessary in the certification world in order to certify the
  ~c[equalities] book.  In general, for each ~c[<book-name>.lisp] whose
  certification requires a non-initial certification world, you will need a
  corresponding ~c[<book-name>.acl2] file that ends with the appropriate
  ~ilc[certify-book] command.  Of course, you can also use ~c[.acl2] files with
  initial certification worlds, for example if you want to pass optional
  arguments to ~ilc[certify-book].

  You also have the option of creating a file ~c[cert.acl2] that has a special
  role.  When file ~c[<book-name>.lisp] is certified, if there is no file
  ~c[<book-name>.acl2] but there is a file ~c[cert.acl2], then ~c[cert.acl2]
  will be used as ~c[<book-name>.acl2] would have been used, as described in
  the preceding paragraph, except that the appropriate ~ilc[certify-book]
  command will be generated automatically ~-[] no ~c[certify-book] command
  should occur in ~c[cert.acl2].

  It is actually allowed to put raw lisp forms in a ~c[.acl2] file (presumably
  preceded by ~c[:q] or ~c[(value :q)] and followed by ~c[(lp)]).  But this is
  not recommended; we make no guarantees about certification performed any time
  after raw Lisp has been entered in the ACL2 session.

  6. Run the following command:
  ~bv[]
  make dependencies
  ~ev[]
  This will generate dependency information.  If you try it in ~c[books/misc/],
  the result should agree with what you find in ~c[books/misc/Makefile].  If
  you run this in the directory you are developing, you will want to insert the
  output at the end of your ~c[Makefile].

  7. Run ~c[make].  This will generate a ~c[<book-name>.out] file for each
  ~c[<book-name>.lisp] file being certified, which is the result of redirecting
  ACL2's standard output.  Note that ~c[make] will stop at the first failure,
  but you can use ~c[make -i] to force make to continue past failures.  You can
  also use the ~c[-j] option to speed things up if you have a multi-core
  machine.

  That concludes the basic instructions for creating a ~c[Makefile] in a
  directory including books.  Here are some other capabilities offered by
  ~c[books/Makefile-subdirs].

  ~st[Subdirectory support.]  There is support for the case that there are no
  books in the current directory, but there are subdirectories that include
  books (or themselves have no books but contain subdirectories with books,
  etc.)  For example, file ~c[books/arithmetic-3/Makefile] has the following
  contents.
  ~bv[]
  DIRS = pass1 bind-free floor-mod
  include ../Makefile-subdirs
  ~ev[]
  This indicates that we are to run ~c[make] in subdirectories ~c[pass1/],
  ~c[bind-free/], and ~c[floor-mod] of the current directory
  (namely, ~c[books/arithmetic-3/]).  Use ~c[Makefile-psubdirs] instead of
  ~c[Makefile-subdirs] if certitification of a book in a subdirectory never
  depends on certification of a book in a different subdirectory, because then
  ~c[make]'s ~c[-j] option can allow subdirectories to be processed in
  parallel.

  ~st[Cleaning up.]  We note that there is a ~c[clean] target.  Thus,
  ~bv[]
  make clean
  ~ev[]
  will remove all ~c[.cert] files, files resulting from compilation, and other
  ``junk''; see the full list under ``~c[clean:]'' in
  ~c[books/Makefile-generic].

  ~st[Compilation support.]  Finally, ~c[books/Makefile-generic] provides
  support for compiling books that are already certified.  For example, suppose
  that you have certified books in GCL, resulting in compiled files with the
  ~c[.o] extension.  Now suppose you would like to compile the books for
  Allegro Common Lisp, whose compiled files have the ~c[.fasl] extension.  The
  following command will work if you have included ~c[books/Makefile-generic]
  in your ~c[Makefile].
  ~bv[]
  make fasl
  ~ev[]
  In general, the compiled file extension for a Lisp supported by ACL2 will be
  a target name for building compiled files for all your books (after
  certifying the books, if not already up-to-date on certification).")

(link-doc-to makefiles books book-makefiles)

; We now use encapsulate to implement defstub.

(defun defstub-ignores (formals body)

; The test below is sufficient to ensure that the set-difference-equal
; used to compute the ignored vars will not cause an error.  We return
; a true list.  The formals and body will be checked thoroughly by the
; encapsulate, provided we generate it!  Provided they check out, the
; result returned is the list of ignored formals.

  (if (and (symbol-listp formals)
           (or (symbolp body)
               (and (consp body)
                    (symbol-listp (cdr body)))))
      (set-difference-equal
       formals
       (if (symbolp body)
           (list body)
         (cdr body)))
    nil))

(defun defstub-body (output)

; This strange little function is used to turn an output signature
; spec (in either the old or new style) into a term.  It never causes
; an error, even if output is ill-formed!  What it returns in that
; case is irrelevant.  If output is well-formed, i.e., is one of:

;       output               result
; *                           nil
; x                           x
; state                       state
; (mv * state *)              (mv nil state nil)
; (mv x state y)              (mv x state y)

; it replaces the *'s by nil and otherwise doesn't do anything.

  (cond ((atom output)
         (cond ((equal output '*) nil)
               (t output)))
        ((equal (car output) '*)
         (cons nil (defstub-body (cdr output))))
        (t (cons (car output) (defstub-body (cdr output))))))

; The following function is used to implement a slighly generalized
; form of macro args, namely one in which we can provide an arbitrary
; number of ordinary arguments terminated by an arbitrary number of
; keyword argument pairs.

(defun partition-rest-and-keyword-args1 (x)
  (cond ((endp x) (mv nil nil))
        ((keywordp (car x))
         (mv nil x))
        (t (mv-let (rest keypart)
                   (partition-rest-and-keyword-args1 (cdr x))
                   (mv (cons (car x) rest)
                       keypart)))))

(defun partition-rest-and-keyword-args2 (keypart keys alist)

; We return t if keypart is ill-formed as noted below.  Otherwise, we
; return ((:keyn . vn) ... (:key1 . v1)).

  (cond ((endp keypart) alist)
        ((and (keywordp (car keypart))
              (consp (cdr keypart))
              (not (assoc-eq (car keypart) alist))
              (member (car keypart) keys))
         (partition-rest-and-keyword-args2 (cddr keypart)
                                           keys
                                           (cons (cons (car keypart)
                                                       (cadr keypart))
                                                 alist)))
        (t t)))

(defun partition-rest-and-keyword-args (x keys)

; X is assumed to be a list of the form (a1 ... an :key1 v1 ... :keyk
; vk), where no ai is a keyword.  We return (mv erp rest alist), where
; erp is t iff the keyword section of x is ill-formed.  When erp is
; nil, rest is '(a1 ... an) and alist is '((:key1 . v1) ... (:keyk
; . vk)).

; The keyword section is ill-formed if it contains a non-keyword in an
; even numbered element, if it binds the same keyword more than once,
; or if it binds a keyword other than those listed in keys.

  (mv-let (rest keypart)
          (partition-rest-and-keyword-args1 x)
          (let ((alist (partition-rest-and-keyword-args2 keypart keys nil)))
            (cond
             ((eq alist t) (mv t nil nil))
             (t (mv nil rest alist))))))

(defmacro defstub (name &rest rst)

  ":Doc-Section Events

  stub-out a function symbol~/
  ~bv[]
  Examples:
  ACL2 !>(defstub subr1 (* * state) => (mv * state))
  ACL2 !>(defstub add-hash (* * hash-table) => hash-table)~/

  General Forms:
  (defstub name args-sig => output-sig)
  (defstub name args-sig => output-sig :doc doc-string)
  ~ev[]

  ~c[Name] is a new function symbol and ~c[(name . args-sig) => output-sig)]
  is a ~il[signature].  If the optional ~ilc[doc-string] is supplied
  it should be a documentation string.  See also the ``Old Style''
  heading below.

  ~c[Defstub] macro expands into an ~ilc[encapsulate] event
  (~pl[encapsulate]).  Thus, no axioms are available about ~c[name]
  but it may be used wherever a function of the given signature is
  permitted.

  Old Style:
  ~bv[]
  Old Style General Form:
  (defstub name formals output)
  (defstub name formals output :doc doc-string)
  ~ev[] 
  where ~c[name] is a new function symbol, ~c[formals] is its list of
  formal parameters, and ~c[output] is either a symbol (indicating
  that the function returns one result) or a term of the form
  ~c[(mv s1 ... sn)], where each ~c[si] is a symbol (indicating that the
  function returns ~c[n] results).  Whether and where the symbol
  ~ilc[state] occurs in ~c[formals] and ~c[output] indicates how the
  function handles ~il[state].  It should be the case that 
  ~c[(name formals output)] is in fact a signature (~pl[signature]).

  Note that with the old style notation it is impossible to stub-out
  a function that uses any single-threaded object other than state.
  The old style is preserved for compatibility with earlier versions of
  ACL2."

  (mv-let (erp args key-alist)
          (partition-rest-and-keyword-args rst '(:doc))
          (cond
           ((or erp
                (not (or (equal (length args) 2)
                         (and (equal (length args) 3)
                              (symbol-listp (car args))
                              (symbolp (cadr args))
                              (equal (symbol-name (cadr args)) "=>")))))
            `(er soft 'defstub
                 "Defstub must be of the form (defstub name formals ~
                  body) or (defstub name args-sig => body-sig), where ~
                  args-sig is a true-list of symbols.  Both ~
                  forms permit an optional, final :DOC doc-string ~
                  argument.  See :DOC defstub."))
           (t
            (let ((doc (cdr (assoc-eq :doc key-alist))))
              (cond
               ((equal (length args) 2)

; Old style
                (let* ((formals (car args))
                       (body (cadr args))
                       (ignores (defstub-ignores formals body)))
                  `(encapsulate
                    ((,name ,formals ,body))
                    (logic)
                    (local
                     (defun ,name ,formals
                       (declare (ignore ,@ignores))
                       ,body))
                    ,@(if doc `((defdoc ,name ,doc)) nil))))
               (t (let* ((args-sig (car args))
                         (body-sig (caddr args))
                         (formals (gen-formals-from-pretty-flags args-sig))
                         (body (defstub-body body-sig))
                         (ignores (defstub-ignores formals body))
                         (stobjs (collect-non-x '* args-sig)))
                    `(encapsulate
                      (((,name ,@args-sig) => ,body-sig))
                      (logic)
                      (local
                       (defun ,name ,formals
                         (declare (ignore ,@ignores)
                                  (xargs :stobjs ,stobjs))
                         ,body))
                      ,@(if doc `((defdoc ,name ,doc)) nil))))))))))

; Next we implement defchoose and defun-sk.

(defun redundant-defchoosep (name event-form wrld)
  (let* ((old-ev (get-event name wrld)))
    (and
     old-ev
     (case-match old-ev
       (('defchoose !name old-bound-vars old-free-vars old-body . old-rest)
        (case-match event-form
          (('defchoose !name new-bound-vars new-free-vars new-body . new-rest)
           (and (equal old-bound-vars new-bound-vars)
                (equal old-free-vars new-free-vars)
                (equal old-body new-body)
                (eq (cadr (assoc-keyword :strengthen old-rest))
                    (cadr (assoc-keyword :strengthen new-rest)))))))))))

(defun chk-arglist-for-defchoose (args bound-vars-flg ctx state)
  (cond ((arglistp args) (value nil))
        ((not (true-listp args))
         (er soft ctx
             "The ~#0~[bound~/free~] variables of a DEFCHOOSE event must be a ~
              true list but ~x1 is not."
             (if bound-vars-flg 0 1)
             args))
        (t (mv-let (culprit explan)
                   (find-first-bad-arg args)
                   (er soft ctx
                       "The ~#0~[bound~/free~] variables of a DEFCHOOSE event ~
                        must be a true list of distinct, legal variable names.  ~
                        ~x1 is not such a list.  The element ~x2 violates the ~
                        rules because it ~@3."
                       (if bound-vars-flg 0 1)
                       args culprit explan)))))

(defun defchoose-constraint-basic (fn bound-vars formals tbody ctx wrld state)

; It seems a pity to translate tbody, since it's already translated, but that
; seems much simpler than the alternatives.

  (cond
   ((null (cdr bound-vars))
    (er-let*
     ((consequent (translate
                   `(let ((,(car bound-vars) ,(cons fn formals)))
                      ,tbody)
                   t t t ctx wrld state)))
     (value (fcons-term*
             'implies
             tbody
             consequent))))
   (t
    (er-let*
     ((consequent (translate
                   `(mv-let ,bound-vars
                            ,(cons fn formals)
                            ,tbody)
                   t t t ctx wrld state)))
     (value (fcons-term*
             'if

; We need this true-listp axiom in order to prove guard conjectures generated
; by mv-nth in defun-sk.

             (fcons-term*
              'true-listp
              (cons-term fn formals))
             (fcons-term*
              'implies
              tbody
              consequent)
             *nil*))))))

(defun defchoose-constraint-extra (fn bound-vars formals tbody ctx wrld state)

; WARNING: If the following comment is removed, then eliminate the reference to
; it in :doc defchoose.

; Note that :doc conservativity-of-defchoose contains an argument showing that
; we may assume that there is a definable enumeration, enum, of the universe.
; Thus, for any definable property that is not always false, there is a "least"
; witness, i.e., a least n for which (enum n) satisfies that property.  Thus, a
; function defined with defchoose is definable: pick the least witness if there
; is one, else nil.  From this definition it is clear that the following
; formula holds, where formals2 is a copy of formals that is disjoint both from
; formals and from bound-vars, and where tbody2 is the result of replacing
; formals by formals2 in tbody, the translated body of the defchoose.  (If
; bound-vars is a list of length 1, then we use let rather than mv-let in this
; formula.)

; (or (equal (fn . formals)
;            (fn . formals2))
;     (mv-let (bound-vars (fn . formals))
;       (and tbody
;            (not tbody2)))
;     (mv-let (bound-vars (fn . formals2))
;       (and tbody2
;            (not tbody1))))

; We now outline an argument for the :non-standard-analysis case, which in fact
; provides justification for both defchoose axioms.  The idea is to assume that
; there is a suitable well-ordering for the ground-zero theory and that the
; ground-zero theory contains enough "invisible" functions so that this
; property is preserved by extensions (as discussed in the JAR paper "Theory
; Extensions in ACL2(r) by Gamboa and Cowles).  Here is a little more detail,
; but a nice challenge is to work this out completely.

; The idea of the proof is first to start with what the above paper calls an
; "r-complete" GZ: basically, a ground-zero theory satisfying induction and
; transfer that contains a function symbol for each defun and defun-std.  We
; can preserve r-completeness as we add defun, defun-std, encapsulate, and
; defchoose events (again, as in the above paper).  The key idea for defchoose
; is that GZ should also have a binary symbol, <|, that is axiomatized to be a
; total order.  That is, <| is a "definable well order", in the sense that
; there are axioms that guarantee for each phi(x) that (exists x phi) implies
; that (exists <|-least x phi).  The trick is to add the well-ordering after
; taking a nonstandard elementary extension of the standard reals MS, where
; every function over the reals is represented in MS as the interpretation of a
; function symbol.

; Still as in the above paper, there is a definable fn for the above defchoose,
; obtained by picking the least witness.  Moreover, if body is classical then
; we can first conjoin it with (standard-p bound-var), choose the <|-least
; bound-var with a classical function using defun-std, and then show by
; transfer that this function witnesses the original defchoose.

  (let* ((formals2 (generate-variable-lst formals (append bound-vars formals)
                                          nil (ens state) wrld))
         (equality (fcons-term* 'equal (cons fn formals) (cons fn formals2)))
         (tbody2 (subcor-var formals formals2 tbody))
         (raw-disjunct

; It seems a pity to translate tbody, since it's already translated, but that
; seems much simpler than the alternatives.

          (cond
           ((null (cdr bound-vars))
            `(or (let ((,(car bound-vars) (,fn ,@formals)))
                   (and ,tbody
                        (not ,tbody2)))
                 (let ((,(car bound-vars) (,fn ,@formals2)))
                   (and ,tbody2
                        (not ,tbody)))))
           (t
            `(or (mv-let ,bound-vars
                         (,fn ,@formals)
                         (and ,tbody
                              (not ,tbody2)))
                 (mv-let ,bound-vars
                         (,fn ,@formals2)
                         (and ,tbody2
                              (not ,tbody))))))))
    (er-let* ((disjunct
               (translate raw-disjunct t t t ctx wrld state)))
             (value (disjoin2 equality disjunct)))))

(defun defchoose-constraint (fn bound-vars formals tbody strengthen ctx wrld state)
  (er-let* ((basic (defchoose-constraint-basic fn bound-vars formals tbody ctx
                     wrld state)))
           (cond
            (strengthen
             (er-let* ((extra (defchoose-constraint-extra fn bound-vars formals
                                tbody ctx wrld state)))
                      (value (conjoin2 basic extra))))
            (t (value basic)))))

(defun defchoose-fn (def state event-form)
  (declare (xargs :guard (true-listp def))) ; def comes from macro call
  (when-logic
   "DEFCHOOSE"
   (with-ctx-summarized
    (if (output-in-infixp state) event-form (cons 'defchoose (car def)))
    (let* ((wrld (w state))
           (event-form (or event-form (cons 'defchoose def)))
           (raw-bound-vars (cadr def))
           (valid-keywords '(:doc :strengthen))
           (ka (nthcdr 4 def)) ; def is the argument list of a defchoose call
           (doc (cadr (assoc-keyword :doc ka)))
           (strengthen (cadr (assoc-keyword :strengthen def))))
      (er-progn
       (chk-all-but-new-name (car def) ctx 'constrained-function wrld state)
       (cond
        ((not (and (keyword-value-listp ka)
                   (null (strip-keyword-list valid-keywords ka))))
         (er soft ctx
             "Defchoose forms must have the form (defchoose fn bound-vars ~
              formals body), with optional keyword arguments ~&0. However, ~
              ~x1 does not have this form.  See :DOC defchoose."
             valid-keywords
             event-form))
        ((and doc
              (not (doc-stringp doc)))
         (er soft ctx
             "Illegal doc string has been supplied in ~x0.  See :DOC ~
              doc-string."
             event-form))
        ((not (booleanp strengthen))
         (er soft ctx
             "The :strengthen argument of a defchoose event must be t or nil. ~
              The event ~x0 is thus illegal."
             event-form))
        ((redundant-defchoosep (car def) event-form wrld)
         (stop-redundant-event state))
        (t
         (enforce-redundancy
          event-form ctx wrld
          (cond
           ((null raw-bound-vars)
            (er soft ctx
                "The bound variables of a defchoose form must be non-empty.  ~
                 The form ~x0 is therefore illegal."
                event-form))
           (t
            (let ((fn (car def))
                  (bound-vars (if (atom raw-bound-vars)
                                  (list raw-bound-vars)
                                raw-bound-vars))
                  (formals (caddr def))
                  (body (cadddr def)))
              (er-progn
               (chk-arglist-for-defchoose bound-vars t ctx state)
               (chk-arglist-for-defchoose formals nil ctx state)
               (er-let*
                ((tbody (translate body t t t ctx wrld state))
                 (wrld (chk-just-new-name fn 'function nil ctx wrld state))
                 (doc-pair (translate-doc fn doc ctx state)))
                (cond
                 ((intersectp-eq bound-vars formals)
                  (er soft ctx
                      "The bound and free variables of a defchoose form must ~
                       not intersect, but their intersection for the form ~x0 ~
                       is ~x1."
                      event-form
                      (intersection-eq bound-vars formals)))
                 (t
                  (let* ((body-vars (all-vars tbody))
                         (bound-and-free-vars (append bound-vars formals))
                         (diff (set-difference-eq bound-and-free-vars
                                                  body-vars))
                         (ignore-ok (cdr (assoc-eq
                                          :ignore-ok
                                          (table-alist 'acl2-defaults-table
                                                       wrld)))))
                    (cond
                     ((not (subsetp-eq body-vars bound-and-free-vars))
                      (er soft ctx
                          "All variables in the body of a defchoose form must ~
                           appear among the bound or free variables supplied ~
                           in that form.  However, the ~#0~[variable ~x0 ~
                           does~/variables ~&0 do~] not appear in the bound or ~
                           free variables of the form ~x1, even though ~#0~[it ~
                           appears~/they appear~] in its body."
                          (set-difference-eq body-vars
                                             (append bound-vars formals))
                          event-form))
                     ((and diff
                           (null ignore-ok))
                      (er soft ctx
                          "The variable~#0~[ ~&0~ occurs~/s ~&0 occur~] in the ~
                           body of the form ~x1.  However, ~#0~[this variable ~
                           does~/these variables do~] not appear either in the ~
                           bound variables or the formals of that form.  In ~
                           order to avoid this error, see :DOC set-ignore-ok."
                          diff
                          event-form))
                     (t
                      (pprogn
                       (cond
                        ((eq ignore-ok :warn)
                         (warning$ ctx "Ignored-variables"
                                   "The variable~#0~[ ~&0 occurs~/s ~&0 ~
                                    occur~] in the body of the following ~
                                    defchoose form:~|~x1~|However, ~#0~[this ~
                                    variable does~/these variables do~] not ~
                                    appear either in the bound variables or ~
                                    the formals of that form.  In order to ~
                                    avoid this warning, see :DOC set-ignore-ok."
                                   diff
                                   event-form))
                        (t state))
                       (let* ((stobjs-in
                               (compute-stobj-flags formals nil wrld))
                              (stobjs-out
                               (compute-stobj-flags bound-vars nil wrld))
                              (wrld
                               (putprop
                                fn 'constrainedp t
                                (putprop
                                 fn 'symbol-class
                                 :common-lisp-compliant
                                 (putprop-unless
                                  fn 'stobjs-out stobjs-out nil
                                  (putprop-unless
                                   fn 'stobjs-in stobjs-in nil
                                   (putprop
                                    fn 'formals formals
                                    (update-doc-data-base
                                     fn doc doc-pair wrld))))))))
                         (er-let*
                          ((constraint
                            (defchoose-constraint
                              fn bound-vars formals tbody strengthen
                              ctx wrld state)))
                          (install-event fn
                                         event-form
                                         'defchoose
                                         fn
                                         nil
                                         `(defuns nil nil

; Keep the following in sync with intro-udf-lst2.

                                            (,fn
                                             ,formals
                                             (declare (ignore ,@formals))
                                             (throw-raw-ev-fncall
                                              '(ev-fncall-null-body-er ,fn))))
                                         :protect
                                         ctx
                                         (putprop
                                          fn 'defchoose-axiom constraint wrld)
                                         state))))))))))))))))))))))

(defun non-acceptable-defun-sk-p (name args body doc quant-ok rewrite exists-p)

; Since this is just a macro, we only do a little bit of vanilla checking,
; leaving it to the real events to implement the most rigorous checks.

  (declare (ignore doc))
  (let ((bound-vars (and (true-listp body) ;this is to guard cadr
                         (cadr body)
                         (if (atom (cadr body))
                             (list (cadr body))
                           (cadr body)))))
    (cond
     ((and rewrite exists-p)
      (msg "It is illegal to supply a :rewrite argument for a defun-sk form ~
            that uses the exists quantifier.  See :DOC defun-sk."))
     ((and (keywordp rewrite)
           (not (member-eq rewrite '(:direct :default))))
      (msg "The only legal keyword values for the :rewrite argument of a ~
            defun-sk are :direct and :default.  ~x0 is thus illegal."
           rewrite))
     ((not (true-listp args))
      (msg "The second argument of DEFUN-SK must be a true list of legal ~
            variable names, but ~x0 is not a true-listp."
           args))
     ((not (arglistp args))
      (mv-let
       (culprit explan)
       (find-first-bad-arg args)
       (msg "The formal parameters (second argument) of a DEFUN-SK form must ~
             be a true list of distinct, legal variable names.  ~x0 is not ~
             such a list.  The element ~x1 violates the rules because it ~@2."
            args culprit explan)))
     ((not (and (true-listp body)
                (equal (length body) 3)
                (symbolp (car body))
                (member-equal (symbol-name (car body))
                              '("FORALL" "EXISTS"))
                (true-listp bound-vars)
                (null (collect-non-legal-variableps bound-vars))))
      (msg "The body (last argument) of a DEFUN-SK form must be a true list of ~
            the form (Q vars term), where Q is FORALL or EXISTS and vars is a ~
            variable or a true list of variables.  The body ~x0 is therefore ~
            illegal."
           body))
     ((member-eq 'state bound-vars)
      (msg "The body (last argument) of a DEFUN-SK form must be a true list of ~
            the form (Q vars term), where vars represents the bound ~
            variables.  The bound variables must not include STATE.  The body ~
            ~x0 is therefore illegal."
           body))
     ((null (cadr body))
      (msg "The variables of the body of a DEFUN-SK, following the quantifier ~
            EXISTS or FORALL, must be a non-empty list.  However, in DEFUN-SK ~
            of ~x0, they are empty."
           name))
     ((intersectp-eq bound-vars args)
      (msg "The formal parameters of a DEFN-SK form must be disjoint from the ~
            variables bound by its body.  However, the ~#0~[variable ~x0 ~
            belongs~/variables ~&0 belong~] to both the formal parameters, ~
            ~x1, and the bound variables, ~x2."
           (intersection-eq bound-vars args)
           args bound-vars))
     ((and (not quant-ok)
           (or (symbol-name-tree-occur 'forall (caddr body))
               (symbol-name-tree-occur 'exists (caddr body))))
      (msg "The symbol ~x0 occurs in the term you have supplied to DEFUN-SK, ~
            namely, ~x1.  By default, this is not allowed.  Perhaps you ~
            believe that DEFUN-SK can appropriately handle quantifiers other ~
            than one outermost quantifier; sadly, this is not yet the case ~
            (though you are welcome to contact the implementors and request ~
            this capability).  If however you really intend this DEFUN-SK form ~
            to be executed (because, for example, ~x0 is in the scope of a ~
            macro that expands it away), simply give a non-nil :quant-ok ~
            argument.  See :DOC defun-sk."
           (if (symbol-name-tree-occur 'forall (caddr body))
               'forall
             'exists)
           body))
     (t nil))))

(defmacro defun-sk (name args body
                         &key doc quant-ok skolem-name thm-name rewrite
                         (witness-dcls
                          '((declare (xargs :non-executable t)))))

  ":Doc-Section Events

  define a function whose body has an outermost quantifier~/
  ~bv[]
  Examples:
  (defun-sk exists-x-p0-and-q0 (y z)
    (exists x
            (and (p0 x y z)
                 (q0 x y z))))

  (defun-sk exists-x-p0-and-q0 (y z) ; equivalent to the above
    (exists (x)
            (and (p0 x y z)
                 (q0 x y z))))

  (defun-sk forall-x-y-p0-and-q0 (z)
    (forall (x y)
            (and (p0 x y z)
                 (q0 x y z))))~/

  General Form:
  (defun-sk fn (var1 ... varn) body
    &key rewrite doc quant-ok skolem-name thm-name witness-dcls)
  ~ev[]
  where ~c[fn] is the symbol you wish to define and is a new symbolic
  name (~pl[name]), ~c[(var1 ... varn)] is its list of formal
  parameters (~pl[name]), and ~c[body] is its body, which must be
  quantified as described below.  The ~c[&key] argument ~ilc[doc] is an optional
  ~il[documentation] string to be associated with ~c[fn]; for a description
  of its form, ~pl[doc-string].  In the case that ~c[n] is 1, the list
  ~c[(var1)] may be replaced by simply ~c[var1].  The other arguments are
  explained below.

  For a simple example, ~pl[defun-sk-example].  For a more elaborate example,
  ~pl[Tutorial4-Defun-Sk-Example].  Also ~pl[quantifiers] for an example
  illustrating how the use of recursion, rather than explicit quantification
  with ~c[defun-sk], may be preferable.

  Below we describe the ~c[defun-sk] event precisely.  First, let us
  consider the examples above.  The first example, again, is:
  ~bv[]
  (defun-sk exists-x-p0-and-q0 (y z)
    (exists x
            (and (p0 x y z)
                 (q0 x y z))))
  ~ev[]
  It is intended to represent the predicate with formal parameters ~c[y]
  and ~c[z] that holds when for some ~c[x], ~c[(and (p0 x y z) (q0 x y z))]
  holds.  In fact ~c[defun-sk] is a macro that adds the following two
  ~il[events], as shown just below.  The first event guarantees that if
  this new predicate holds of ~c[y] and ~c[z], then the term shown,
  ~c[(exists-x-p0-and-q0-witness y z)], is an example of the ~c[x] that is
  therefore supposed to exist.  (Intuitively, we are axiomatizing
  ~c[exists-x-p0-and-q0-witness] to pick a witness if there is one.)
  Conversely, the second event below guarantees that if there is any
  ~c[x] for which the term in question holds, then the new predicate does
  indeed hold of ~c[y] and ~c[z]. 
  ~bv[]
  (defun exists-x-p0-and-q0 (y z)
    (declare (xargs :non-executable t))
    (let ((x (exists-x-p0-and-q0-witness y z)))
      (and (p0 x y z) (q0 x y z))))
  (defthm exists-x-p0-and-q0-suff
    (implies (and (p0 x y z) (q0 x y z))
             (exists-x-p0-and-q0 y z)))
  ~ev[]
  Now let us look at the third example from the introduction above:
  ~bv[]
  (defun-sk forall-x-y-p0-and-q0 (z)
    (forall (x y)
            (and (p0 x y z)
                 (q0 x y z))))
  ~ev[]
  The intention is to introduce a new predicate
  ~c[(forall-x-y-p0-and-q0 z)] which states that the indicated conjunction
  holds of all ~c[x] and all ~c[y] together with the given ~c[z].  This time, the
  axioms introduced are as shown below.  The first event guarantees
  that if the application of function ~c[forall-x-y-p0-and-q0-witness] to
  ~c[z] picks out values ~c[x] and ~c[y] for which the given term
  ~c[(and (p0 x y z) (q0 x y z))] holds, then the new predicate
  ~c[forall-x-y-p0-and-q0] holds of ~c[z].  Conversely, the (contrapositive
  of) the second axiom guarantees that if the new predicate holds of
  ~c[z], then the given term holds for all choices of ~c[x] and ~c[y] (and that
  same ~c[z]). 
  ~bv[]
  (defun forall-x-y-p0-and-q0 (z)
    (declare (xargs :non-executable t))
    (mv-let (x y)
            (forall-x-y-p0-and-q0-witness z)
            (and (p0 x y z) (q0 x y z))))
  (defthm forall-x-y-p0-and-q0-necc
    (implies (not (and (p0 x y z) (q0 x y z)))
             (not (forall-x-y-p0-and-q0 z))))
  ~ev[]
  The examples above suggest the critical property of ~c[defun-sk]:  it
  indeed does introduce the quantified notions that it claims to
  introduce.

  Notice that the ~ilc[defthm] event just above, ~c[forall-x-y-p0-and-q0-necc],
  may not be of optimal form as a rewrite rule.  Users sometimes find that when
  the quantifier is ~c[forall], it is useful to state this rule in a form where
  the new quantified predicate is a hypothesis instead.  In this case that form
  would be as follows:
  ~bv[]
  (defthm forall-x-y-p0-and-q0-necc
    (implies (forall-x-y-p0-and-q0 z)
             (and (p0 x y z) (q0 x y z))))
  ~ev[]
  ACL2 will turn this into one ~c[:]~ilc[rewrite] rule for each conjunct,
  ~c[(p0 x y z)] and ~c[(q0 x y z)], with hypothesis
  ~c[(forall-x-y-p0-and-q0 z)] in each case.  In order to get this effect, use
  ~c[:rewrite :direct], in this case as follows.
  ~bv[]
  (defun-sk forall-x-y-p0-and-q0 (z)
    (forall (x y)
            (and (p0 x y z)
                 (q0 x y z)))
    :rewrite :direct)
  ~ev[]

  We now turn to a detailed description ~c[defun-sk], starting with a
  discussion of its arguments as shown in the \"General Form\" above.

  The third argument, ~c[body], must be of the form
  ~bv[]
  (Q bound-vars term)
  ~ev[]
  where:  ~c[Q] is the symbol ~ilc[forall] or ~ilc[exists] (in the \"ACL2\"
  package), ~c[bound-vars] is a variable or true list of variables
  disjoint from ~c[(var1 ... varn)] and not including ~ilc[state], and
  ~c[term] is a term.  The case that ~c[bound-vars] is a single variable
  ~c[v] is treated exactly the same as the case that ~c[bound-vars] is
  ~c[(v)].

  The result of this event is to introduce a ``Skolem function,'' whose name is
  the keyword argument ~c[skolem-name] if that is supplied, and otherwise is
  the result of modifying ~c[fn] by suffixing \"-WITNESS\" to its name.  The
  following definition and one of the following two theorems (as indicated) are
  introduced for ~c[skolem-name] and ~c[fn] in the case that ~c[bound-vars]
  (see above) is a single variable ~c[v].  The name of the ~ilc[defthm] event
  may be supplied as the value of the keyword argument ~c[:thm-name]; if it is
  not supplied, then it is the result of modifying ~c[fn] by suffixing
  \"-SUFF\" to its name in the case that the quantifier is ~ilc[exists], and
  \"-NECC\" in the case that the quantifier is ~ilc[forall].
  ~bv[]
  (defun fn (var1 ... varn)
    (declare (xargs :non-executable t))
    (let ((v (skolem-name var1 ... varn)))
      term))

  (defthm fn-suff ;in case the quantifier is EXISTS
    (implies term
             (fn var1 ... varn)))

  (defthm fn-necc ;in case the quantifier is FORALL
    (implies (not term)
             (not (fn var1 ... varn))))
  ~ev[]

  In the ~c[forall] case, however, the keyword pair ~c[:rewrite :direct] may be
  supplied after the body of the ~c[defun-sk] form, in which case the
  contrapositive of the above form is used instead:
  ~bv[]
  (defthm fn-necc ;in case the quantifier is FORALL
    (implies (fn var1 ... varn)
             term))
  ~ev[]
  This is often a better choice for the \"-NECC\" rule, provided ACL2 can parse
  ~c[term] as a ~c[:]~ilc[rewrite] rule.  A second possible value of the
  ~c[:rewrite] argument of ~c[defun-sk] is ~c[:default], which gives the same
  behavior as when ~c[:rewrite] is omitted.  Otherwise, the value of
  ~c[:rewrite] should be the term to use as the body of the ~c[fn-necc] theorem
  shown above; ACL2 will attempt to do the requisite proof in this case.  If
  that term is weaker than the default, the properties introduced by
  ~c[defun-sk] may of course be weaker than they would be otherwise.  Finally,
  note that the ~c[:rewrite] keyword argument for ~c[defun-sk] only makes sense
  if the quantifier is ~c[forall]; it is thus illegal if the quantifier is
  ~c[exists].  Enough said about ~c[:rewrite]!

  In the case that ~c[bound-vars] is a list of at least two variables, say
  ~c[(bv1 ... bvk)], the definition above (with no keywords) is the following
  instead, but the theorem remains unchanged.
  ~bv[]
  (defun fn (var1 ... varn)
    (declare (xargs :non-executable t))
    (mv-let (bv1 ... bvk)
            (skolem-name var1 ... varn)
            term))
  ~ev[]

  In order to emphasize that the last element of the list, ~c[body], is a
  term, ~c[defun-sk] checks that the symbols ~ilc[forall] and ~ilc[exists] do
  not appear anywhere in it.  However, on rare occasions one might
  deliberately choose to violate this convention, presumably because
  ~ilc[forall] or ~ilc[exists] is being used as a variable or because a
  macro call will be eliminating ``calls of'' ~ilc[forall] and ~ilc[exists].
  In these cases, the keyword argument ~c[quant-ok] may be supplied a
  non-~c[nil] value.  Then ~c[defun-sk] will permit ~ilc[forall] and
  ~ilc[exists] in the body, but it will still cause an error if there is
  a real attempt to use these symbols as quantifiers.

  Note the form ~c[(declare (xargs :non-executable t))] that appears in each
  ~ilc[defun] above.  These forms disable certain checks that are required for
  execution, in particular the single-threaded use of ~ilc[stobj]s.  However,
  there is a price: calls of these defined functions cannot be evaluated.
  Normally that is not a problem, since these notions involve quantifiers.  But
  you are welcome to replace this ~ilc[declare] form with your own, as follows:
  if you supply a list of ~c[declare] forms to keyword argument
  ~c[:witness-dcls], these will become the declare forms in the generated
  ~ilc[defun].

  ~c[Defun-sk] is a macro implemented using ~ilc[defchoose], and hence should
  only be executed in ~il[defun-mode] ~c[:]~ilc[logic]; ~pl[defun-mode] and
  ~pl[defchoose].

  If you find that the rewrite rules introduced with a particular use of
  ~c[defun-sk] are not ideal, even when using the ~c[:rewrite] keyword
  discussed above (in the ~c[forall] case), then at least two reasonable
  courses of action are available for you.  Perhaps the best option is to prove
  the ~ilc[rewrite] rules you want.  If you see a pattern for creating rewrite
  rules from your ~c[defun-sk] events, you might want to write a macro that
  executes a ~c[defun-sk] followed by one or more ~ilc[defthm] events.  Another
  option is to write your own variant of the ~c[defun-sk] macro, say,
  ~c[my-defun-sk], for example by modifying a copy of the definition of
  ~c[defun-sk] from the ACL2 sources.

  If you want to represent nested quantifiers, you can use more than one
  ~c[defun-sk] event.  For example, in order to represent
  ~bv[]
  (forall x (exists y (p x y z)))
  ~ev[]
  you can use ~c[defun-sk] twice, for example as follows.
  ~bv[]
  (defun-sk exists-y-p (x z)
    (exists y (p x y z)))

  (defun-sk forall-x-exists-y-p (z)
    (forall x (exists-y-p x z)))
  ~ev[]

  Some distracting and unimportant warnings are inhibited during
  ~c[defun-sk].

  Note that this way of implementing quantifiers is not a new idea.  Hilbert
  was certainly aware of it 60 years ago!  Also
  ~pl[conservativity-of-defchoose] for a technical argument that justifies the
  logical conservativity of the ~ilc[defchoose] event in the sense of the paper
  by Kaufmann and Moore entitled ``Structured Theory Development for a
  Mechanized Logic'' (Journal of Automated Reasoning 26, no. 2 (2001),
  pp. 161-203).~/"

  (let* ((exists-p (and (true-listp body)
                        (symbolp (car body))
                        (equal (symbol-name (car body)) "EXISTS")))
         (bound-vars (and (true-listp body)
                          (or (symbolp (cadr body))
                              (true-listp (cadr body)))
                          (cond ((atom (cadr body))
                                 (list (cadr body)))
                                (t (cadr body)))))
         (body-guts (and (true-listp body) (caddr body)))
         (skolem-name
          (or skolem-name
              (intern-in-package-of-symbol
               (concatenate 'string (symbol-name name) "-WITNESS")
               name)))
         (thm-name
          (or thm-name
              (intern-in-package-of-symbol
               (concatenate 'string (symbol-name name)
                            (if exists-p "-SUFF" "-NECC"))
               name)))
         (msg (non-acceptable-defun-sk-p name args body doc quant-ok rewrite
                                         exists-p)))
    (if msg
        `(er soft '(defun-sk . ,name)
             "~@0"
             ',msg)
      `(encapsulate
        ()
        (set-match-free-default :all)
        (set-inhibit-warnings "Theory" "Use" "Free" "Non-rec" "Infected")
        (encapsulate
         ((,skolem-name ,args
                         ,(if (= (length bound-vars) 1)
                              (car bound-vars)
                            (cons 'mv bound-vars))))
         (local (in-theory '(implies)))
         (local
          (defchoose ,skolem-name ,bound-vars ,args
            ,(if exists-p
                 body-guts
               `(not ,body-guts))))

         ; A :type-prescription lemma is needed in the case of more than one bound
         ; variable, in case we want to do guard proofs.

         ,@(cond
            ((null (cdr bound-vars)) nil)
            (t
             `((local (defthm ,(intern-in-package-of-symbol
                                (concatenate 'string
                                             (symbol-name skolem-name)
                                             "-TYPE-PRESCRIPTION")
                                skolem-name)
                        (true-listp ,(cons skolem-name args))
                        :rule-classes :type-prescription
                        :hints (("Goal" :by ,skolem-name)))))))
         (defun ,name ,args
           ,@witness-dcls
           ,(if (= (length bound-vars) 1)
                `(let ((,(car bound-vars) (,skolem-name ,@args)))
                   ,body-guts)
              `(mv-let (,@bound-vars)
                       (,skolem-name ,@args)
                       ,body-guts)))
         (in-theory (disable (,name)))
         (defthm ,thm-name
           ,(cond (exists-p
                   `(implies ,body-guts
                             (,name ,@args)))
                  ((eq rewrite :direct)
                   `(implies (,name ,@args)
                             ,body-guts))
                  ((member-eq rewrite '(nil :default))
                   `(implies (not ,body-guts)
                             (not (,name ,@args))))
                  (t rewrite))
           :hints (("Goal"
                     :use (,skolem-name ,name)
                     :in-theory (theory 'minimal-theory))))
         ,@(if doc
               `((defdoc ,name ,doc))
             nil))))))

(deflabel forall
  :doc
  ":Doc-Section Defun-sk

  universal quantifier~/~/

  The symbol ~c[forall] (in the ACL2 package) represents universal
  quantification in the context of a ~ilc[defun-sk] form.
  ~l[defun-sk] and ~pl[exists].

  ~l[quantifiers] for an example illustrating how the use of
  recursion, rather than explicit quantification with ~ilc[defun-sk], may be
  preferable.")

(deflabel exists
  :doc
  ":Doc-Section Defun-sk

  existential quantifier~/~/

  The symbol ~c[exists] (in the ACL2 package) represents existential
  quantification in the context of a ~ilc[defun-sk] form.
  ~l[defun-sk] and ~pl[forall].

  ~l[quantifiers] for an example illustrating how the use of
  recursion, rather than explicit quantification with ~ilc[defun-sk], may be
  preferable.")

(deflabel defun-sk-example
  :doc
  ":Doc-Section Defun-sk

  a simple example using ~ilc[defun-sk]~/~/

  The following example illustrates how to do proofs about functions defined
  with ~ilc[defun-sk].  The events below can be put into a certifiable book
  (~pl[books]).  The example is contrived and rather silly, in that it shows
  how to prove that a quantified notion implies itself, where the antecedent
  and conclusion are defined with different ~ilc[defun-sk] events.  But it
  illustrates the formulas that are generated by ~ilc[defun-sk], and how to use
  them.  Thanks to Julien Schmaltz for presenting this example as a challenge.
  ~bv[]
  (in-package \"ACL2\")

  (encapsulate
   (((p *) => *)
    ((expr *) => *))

   (local (defun p (x) x))
   (local (defun expr (x) x)))

  (defun-sk forall-expr1 (x)
    (forall (y) (implies (p x) (expr y))))

  (defun-sk forall-expr2 (x)
    (forall (y) (implies (p x) (expr y)))))

  ; We want to prove the theorem my-theorem below.  What axioms are there that
  ; can help us?  If you submit the command

  ; :pcb! forall-expr1

  ; then you will see the following two key events.  (They are completely
  ; analogous of course for FORALL-EXPR2.)

  #|
  (DEFUN FORALL-EXPR1 (X)
    (LET ((Y (FORALL-EXPR1-WITNESS X)))
         (IMPLIES (P X) (EXPR Y))))

  (DEFTHM FORALL-EXPR1-NECC
    (IMPLIES (NOT (IMPLIES (P X) (EXPR Y)))
             (NOT (FORALL-EXPR1 X)))
    :HINTS
    ((\"Goal\" :USE FORALL-EXPR1-WITNESS)))
  |#

  ; We see that the latter has value when FORALL-EXPR1 occurs negated in a
  ; conclusion, or (therefore) positively in a hypothesis.  A good rule to
  ; remember is that the former has value in the opposite circumstance: negated
  ; in a hypothesis or positively in a conclusion.

  ; In our theorem, FORALL-EXPR2 occurs positively in the conclusion, so its
  ; definition should be of use.  We therefore leave its definition enabled,
  ; and disable the definition of FORALL-EXPR1.

  #|
  (thm
    (implies (and (p x) (forall-expr1 x))
             (forall-expr2 x))
    :hints ((\"Goal\" :in-theory (disable forall-expr1))))

  ; which yields this unproved subgoal:

  (IMPLIES (AND (P X) (FORALL-EXPR1 X))
           (EXPR (FORALL-EXPR2-WITNESS X)))
  |#

  ; Now we can see how to use FORALL-EXPR1-NECC to complete the proof, by
  ; binding y to (FORALL-EXPR2-WITNESS X).

  ; We use defthmd below so that the following doesn't interfere with the
  ; second proof, in my-theorem-again that follows.
  (defthmd my-theorem
    (implies (and (p x) (forall-expr1 x))
             (forall-expr2 x))
    :hints ((\"Goal\"
             :use ((:instance forall-expr1-necc
                              (x x)
                              (y (forall-expr2-witness x)))))))

  ; The following illustrates a more advanced technique to consider in such
  ; cases.  If we disable forall-expr1, then we can similarly succeed by having
  ; FORALL-EXPR1-NECC applied as a :rewrite rule, with an appropriate hint in how
  ; to instantiate its free variable.  See :doc hints.

  (defthm my-theorem-again
    (implies (and (P x) (forall-expr1 x))
             (forall-expr2 x))
    :hints ((\"Goal\"
             :in-theory (disable forall-expr1)
             :restrict ((forall-expr1-necc
                         ((y (forall-expr2-witness x))))))))
  ~ev[]")

(deflabel quantifiers
  :doc
  ":Doc-Section Defun-sk

  issues about quantification in ACL2~/

  ACL2 supports first-order quantifiers ~ilc[exists] and ~ilc[forall] by way of
  the ~ilc[defun-sk] event.  However, proof support for quantification is
  quite limited.  Therefore, we recommend using recursion in place of
  ~c[defun-sk] when possible (following common ACL2 practice).~/

  For example, the notion ``every member of ~c[x] has property ~c[p]'' can be
  defined either with recursion or explicit quantification, but proofs
  may be simpler when recursion is used.  We illustrate this point
  with two proofs of the same informal claim, one of which uses
  recursion which the other uses explicit quantification.  Notice that
  with recursion, the proof goes through fully automatically; but this
  is far from true with explicit quantification (especially notable is
  the ugly hint).

  The informal claim for our examples is:  If every member ~c[a] of each
  of two lists satisfies the predicate ~c[(p a)], then this holds of their
  ~ilc[append]; and, conversely.

  ~l[quantifiers-using-recursion] for a solution to this example
  using recursion.

  ~l[quantifiers-using-defun-sk] for a solution to this example
  using ~ilc[defun-sk].  Also ~l[quantifiers-using-defun-sk-extended]
  for an elaboration on that solution.")

(deflabel quantifiers-using-recursion
  :doc
  ":Doc-Section Quantifiers

  recursion for implementing quantification~/

  The following example illustrates the use of recursion as a means of
  avoiding proof difficulties that can arise from the use of explicit
  quantification (via ~ilc[defun-sk]).  ~l[quantifiers] for more about
  the context of this example.~/
  ~bv[]
  (in-package \"ACL2\")

  ; We prove that if every member A of each of two lists satisfies the
  ; predicate (P A), then this holds of their append; and, conversely.

  ; Here is a solution using recursively-defined functions.

  (defstub p (x) t)

  (defun all-p (x)
    (if (atom x)
        t
      (and (p (car x))
           (all-p (cdr x)))))

  (defthm all-p-append
    (equal (all-p (append x1 x2))
           (and (all-p x1) (all-p x2))))
  ~ev[]")

(deflabel quantifiers-using-defun-sk
  :doc
  ":Doc-Section Quantifiers

  quantification example~/

  ~l[quantifiers] for the context of this example.  It should be
  compared to a corresponding example in which a simpler proof is
  attained by using recursion in place of explicit quantification;
  ~pl[quantifiers-using-recursion].~/
  ~bv[]
  (in-package \"ACL2\")

  ; We prove that if every member A of each of two lists satisfies the
  ; predicate (P A), then this holds of their append; and, conversely.

  ; Here is a solution using explicit quantification.

  (defstub p (x) t)

  (defun-sk forall-p (x)
    (forall a (implies (member a x)
                       (p a))))

  (defthm member-append
    (iff (member a (append x1 x2))
         (or (member a x1) (member a x2))))

  (defthm forall-p-append
    (equal (forall-p (append x1 x2))
           (and (forall-p x1) (forall-p x2)))
    :hints ((\"Goal\" ; ``should'' disable forall-p-necc, but no need
             :use
             ((:instance forall-p-necc
                         (x (append x1 x2))
                         (a (forall-p-witness x1)))
              (:instance forall-p-necc
                         (x (append x1 x2))
                         (a (forall-p-witness x2)))
              (:instance forall-p-necc
                         (x x1)
                         (a (forall-p-witness (append x1 x2))))
              (:instance forall-p-necc
                         (x x2)
                         (a (forall-p-witness (append x1 x2))))))))
  ~ev[]

  Also ~pl[quantifiers-using-defun-sk-extended] for an
  elaboration on this example.") 

(deflabel quantifiers-using-defun-sk-extended
  :doc
  ":Doc-Section Quantifiers

  quantification example with details~/

  ~l[quantifiers-using-defun-sk] for the context of this example.~/
  ~bv[]
  (in-package \"ACL2\")

  ; We prove that if every member A of each of two lists satisfies the
  ; predicate (P A), then this holds of their append; and, conversely.

  ; Here is a solution using explicit quantification.

  (defstub p (x) t)

  (defun-sk forall-p (x)
    (forall a (implies (member a x)
                       (p a))))

  ; The defun-sk above introduces the following axioms.  The idea is that
  ; (FORALL-P-WITNESS X) picks a counterexample to (forall-p x) if there is one.

  #|
  (DEFUN FORALL-P (X)
    (LET ((A (FORALL-P-WITNESS X)))
         (IMPLIES (MEMBER A X) (P A))))

  (DEFTHM FORALL-P-NECC
    (IMPLIES (NOT (IMPLIES (MEMBER A X) (P A)))
             (NOT (FORALL-P X)))
    :HINTS ((\"Goal\" :USE FORALL-P-WITNESS)))
  |#

  ; The following lemma seems critical.

  (defthm member-append
    (iff (member a (append x1 x2))
         (or (member a x1) (member a x2))))

  ; The proof of forall-p-append seems to go out to lunch, so we break into
  ; directions as shown below.

  (defthm forall-p-append-forward
    (implies (forall-p (append x1 x2))
             (and (forall-p x1) (forall-p x2)))
    :hints ((\"Goal\" ; ``should'' disable forall-p-necc, but no need
             :use
             ((:instance forall-p-necc
                         (x (append x1 x2))
                         (a (forall-p-witness x1)))
              (:instance forall-p-necc
                         (x (append x1 x2))
                         (a (forall-p-witness x2)))))))

  (defthm forall-p-append-reverse
    (implies (and (forall-p x1) (forall-p x2))
             (forall-p (append x1 x2)))
    :hints ((\"Goal\"
             :use
             ((:instance forall-p-necc
                         (x x1)
                         (a (forall-p-witness (append x1 x2))))
              (:instance forall-p-necc
                         (x x2)
                         (a (forall-p-witness (append x1 x2))))))))

  (defthm forall-p-append
    (equal (forall-p (append x1 x2))
           (and (forall-p x1) (forall-p x2)))
    :hints ((\"Goal\" :use (forall-p-append-forward
                          forall-p-append-reverse))))

  ~ev[]")

; Here is the defstobj event.

; We start with the problem of finding the arguments to the defstobj event.
; The form looks likes 

; (defstobj name ... field-descri ... 
;           :renaming alist
;           :doc string)
;           :inline flag)

; where the :renaming, :doc, and :inline keyword arguments are
; optional.  This syntax is not supported by macros because you can't
; have an &REST arg and a &KEYS arg without all the arguments being in
; the keyword style.  So we use &REST and implement the new style of
; argument recovery.

; Once we have partitioned the args for defstobj, we'll have recovered
; the field-descriptors, a renaming alist, and a doc string.  Our next
; step is to check that the renaming alist is of the correct form.

(defun doublet-style-symbol-to-symbol-alistp (x)
  (cond ((atom x) (equal x nil))
        (t (and (consp (car x))
                (symbolp (caar x))
                (consp (cdar x))
                (symbolp (cadar x))
                (null (cddar x))
                (doublet-style-symbol-to-symbol-alistp (cdr x))))))

; Then, we can use the function defstobj-fnname to map the default
; symbols in the defstobj to the function names the user wants us to
; use.  (It is defined elsewhere because it is needed by translate.)

(defun chk-legal-defstobj-name (name state)
  (cond ((eq name 'state)
         (er soft (cons 'defstobj name)
             "STATE is an illegal name for a user-declared ~
              single-threaded object."))
        ((legal-variablep name)
         (value nil))
        (t
         (er soft (cons 'defstobj name)
             "The symbol ~x0 may not be declared as a single-threaded object ~
              name because it is not a legal variable name."
             name))))

(defun chk-unrestricted-guards-for-user-fns (names wrld ctx state)
  (cond
   ((null names) (value nil))
   ((or (acl2-system-namep (car names) wrld)
        (equal (guard (car names) nil wrld) *t*))
    (chk-unrestricted-guards-for-user-fns (cdr names) wrld ctx state))
   (t (er soft ctx
          "The guard for ~x0 is ~p1.  But in order to use ~x0 in the ~
           type-specification of a single-threaded object it must ~
           have a guard of T."
          (car names)
          (untranslate (guard (car names) nil wrld) t wrld)))))

(defconst *expt2-28* (expt 2 28))

(defun chk-stobj-field-descriptor (name field-descriptor ctx wrld state)

; See the comment just before chk-acceptable-defstobj1 for an
; explanation of our handling of Common Lisp compliance.

   (cond
    ((symbolp field-descriptor) (value nil))
    (t
     (er-progn
      (if (and (consp field-descriptor)
               (symbolp (car field-descriptor))
               (keyword-value-listp (cdr field-descriptor))
               (member-equal (length field-descriptor) '(1 3 5 7))
               (let ((keys (odds field-descriptor)))
                 (and (no-duplicatesp keys)
                      (subsetp-eq keys '(:type :initially :resizable)))))
          (value nil)
          (er soft ctx
              "The field descriptors of a single-threaded object ~
               definition must be a symbolic field-name or a list of ~
               the form (field-name :type type :initially val), where ~
               field-name is a symbol.  The :type and :initially ~
               keyword assignments are optional and their order is ~
               irrelevant.  The purported descriptor ~x0 for a field ~
               in ~x1 is not of this form."
              field-descriptor
              name))
      (let ((field (car field-descriptor))
            (type (if (assoc-keyword :type (cdr field-descriptor))
                      (cadr (assoc-keyword :type (cdr field-descriptor)))
                    t))
            (init (if (assoc-keyword :initially (cdr field-descriptor))
                      (cadr (assoc-keyword :initially (cdr field-descriptor)))
                    nil))
            (resizable (if (assoc-keyword :resizable (cdr field-descriptor))
                           (cadr (assoc-keyword :resizable (cdr field-descriptor)))
                         nil)))
        (cond
         ((and resizable (not (eq resizable t)))
          (er soft ctx
              "The :resizable value in the ~x0 field of ~x1 is ~
               illegal:  ~x2.  The legal values are t and nil."
              field name resizable))
         ((and (consp type)
               (eq (car type) 'array))
          (cond
           ((not (and (true-listp type)
                      (equal (length type) 3)
                      (true-listp (caddr type))
                      (equal (length (caddr type)) 1)))
            (er soft ctx
                "When a field descriptor specifies an ARRAY :type, ~
                 the type must be of the form (ARRAY etype (n)).  ~
                 Note that we only support single-dimensional arrays. ~
                  The purported ARRAY :type ~x0 for the ~x1 field of ~
                 ~x2 is not of this form."
                type field name))
          (t (let* ((etype (cadr type))
                    (etype-term (translate-declaration-to-guard
                                 etype 'x wrld))
                    (n (car (caddr type))))
               (cond
                ((null etype-term)
                 (er soft ctx
                     "The element type specified for the ~x0 field of ~
                      ~x1, namely ~x0, is not recognized by ACL2 as a ~
                      type-spec.  See :DOC type-spec."
                     field name type))
                ((not (and (integerp n)
                           (<= 0 n)))
                 (er soft ctx
                     "Array dimensions must be non-negative integers.  ~
                      The :type ~x0 for the ~x1 field of ~x2 is thus ~
                      illegal."
                     type field name))
                (t
                 (er-let*
                   ((pair (simple-translate-and-eval etype-term
                                                     (list (cons 'x init))
                                                     nil
                                                     (msg
                                                      "The type ~x0"
                                                      etype-term)
                                                     ctx
                                                     wrld
                                                     state)))

; pair is (tterm . val), where tterm is a term and val is its value
; under x<-init.

                   (er-progn
                    (chk-common-lisp-compliant-subfunctions
                     nil (list field) (list (car pair))
                     wrld "auxiliary function" ctx state)
                    (chk-unrestricted-guards-for-user-fns
                     (all-fnnames (car pair))
                     wrld ctx state)
                    (cond
                     ((not (cdr pair))
                      (er soft ctx
                          "The value specified by the :initially ~
                           keyword, namely ~x0, fails to satisfy the ~
                           declared type ~x1 in the array ~
                           specification for the ~x2 field of ~x3."
                          init etype field name))
                     (t (value nil)))))))))))
         ((assoc-keyword :resizable (cdr field-descriptor))
          (er soft ctx
              "The :resizable keyword is only legal for array types, hence is ~
               illegal for the ~x0 field of ~x1."
              field name))
         (t (let ((type-term (translate-declaration-to-guard
                              type 'x wrld)))
              (cond
               ((null type-term)
                (er soft ctx
                    "The :type specified for the ~x0 field of ~x1, ~
                     namely ~x2, is not recognized by ACL2 as a ~
                     type-spec.  See :DOC type-spec."
                    field name type))
               (t
                (er-let*
                  ((pair (simple-translate-and-eval type-term
                                                    (list (cons 'x init))
                                                    nil
                                                    (msg
                                                     "The type ~x0"
                                                     type-term)
                                                    ctx
                                                    wrld
                                                    state)))

; pair is (tterm . val), where tterm is a term and val is its value
; under x<-init.

                  (er-progn
                   (chk-common-lisp-compliant-subfunctions
                    nil (list field) (list (car pair))
                    wrld "body" ctx state)
                   (chk-unrestricted-guards-for-user-fns
                     (all-fnnames (car pair))
                     wrld ctx state)
                   (cond
                    ((not (cdr pair))
                     (er soft ctx
                         "The value specified by the :initially ~
                          keyword, namely ~x0, fails to satisfy the ~
                          declared :type ~x1 for the ~x2 field of ~x3."
                         init type field name))
                    (t (value nil)))))))))))))))

(defun chk-acceptable-defstobj-renaming
  (name field-descriptors renaming ctx state default-names)

; We collect up all the default names and then check that the domain
; of renaming contains no duplicates and is a subset of the default
; names.  We already know that field-descriptors is well-formed and
; that renaming is a doublet-style symbol-to-symbol alist.

  (cond
   ((endp field-descriptors)
    (let ((default-names (list* name
                                (defstobj-fnname name :recognizer :top nil)
                                (defstobj-fnname name :creator :top nil)
                                (reverse default-names)))
          (domain (strip-cars renaming)))
      (cond
       ((null renaming)

; In this case, the default-names are the names the user intends us to use.

        (cond
         ((not (no-duplicatesp default-names))
          (er soft ctx
              "The field descriptors are illegal because they require ~
               the use of the same name for two different functions.  ~
               The duplicated name~#0~[ is~/s are~] ~&0.  You must ~
               change the component names so that no conflict occurs. ~
                You may then wish to use the :RENAMING option to ~
               introduce your own names for these functions.  See ~
               :DOC defstobj."
              (duplicates default-names)))
         (t (value nil))))
       ((not (no-duplicatesp default-names))
        (er soft ctx
            "The field descriptors are illegal because they require ~
             the use of the same default name for two different ~
             functions.  The duplicated default name~#0~[ is~/s are~] ~
             ~&0.  You must change the component names so that no ~
             conflict occurs.  Only then may you use the :RENAMING ~
             option to rename the default names."
            (duplicates default-names)))
       ((not (no-duplicatesp domain))
        (er soft ctx
            "No two entries in the :RENAMING alist may mention the ~
             same target symbol.  Your alist, ~x0, contains ~
             duplications in its domain."
            renaming))
       ((not (subsetp domain default-names))
        (er soft ctx
            "Your :RENAMING alist, ~x0, mentions ~#1~[a function ~
             symbol~/function symbols~] in its domain which ~
             ~#1~[is~/are~] not among the default symbols to be ~
             renamed.  The offending symbol~#1~[ is~/s are~] ~&1.  ~
             The default defstobj names for this event are ~&2."
            renaming
            (set-difference-equal domain default-names)
            default-names))
       (t (value nil)))))
   (t (let* ((field (if (atom (car field-descriptors))
                        (car field-descriptors)
                      (car (car field-descriptors))))
             (type (if (consp (car field-descriptors))
                       (or (cadr (assoc-keyword :type
                                                (cdr (car field-descriptors))))
                           t)
                     t))
             (key2 (if (and (consp type)
                            (eq (car type) 'array))
                       :array
                     :non-array)))
        (chk-acceptable-defstobj-renaming
         name (cdr field-descriptors) renaming ctx state
         (list* (defstobj-fnname field :updater key2 nil)
                (defstobj-fnname field :accessor key2 nil)
                (defstobj-fnname field :recognizer key2 nil)
                (cond ((eq key2 :array)
                       (list* (defstobj-fnname field :length key2 nil)
                              (defstobj-fnname field :resize key2 nil)
                              default-names))
                      (t default-names))))))))

; The functions introduced by defstobj are all defined with
; :VERIFY-GUARDS T.  This means we must ensure that their guards and
; bodies are compliant.  Most of this stuff is mechanically generated
; by us and is guaranteed to be compliant.  But there is a way that a
; user defined function can sneak in.  The user might use a type-spec
; such as (satisfies foo), where foo is a user defined function.

; To discuss the guard issue, we name the functions introduced by
; defstobj, following the convention used in the comment in
; defstobj-template.  The recognizer for the stobj itself will be
; called namep, and the creator will be called create-name.  For each
; field, the following names are introduced: recog-name - recognizer
; for the field value; accessor-name - accessor for the field;
; updater-name - updater for the field; length-name - length of array
; field; resize-name - resizing function for array field.

; We are interested in determining the conditions we must check to
; ensure that each of these functions is Common Lisp compliant.  Both
; the guard and the body of each function must be compliant.
; Inspection of defstobj-axiomatic-defs reveals the following.

; Namep is defined in terms of primitives and the recog-names.  The
; guard for namep is T.  The body of namep is always compliant, if the
; recog-names are compliant and have guards of T.

; Create-name is a constant with a guard of T.  Its body is always
; compliant.

; Recog-name has a guard of T.  The body of recog-name is interesting
; from the guard verification perspective, because it may contain
; translated type-spec such as (satisfies foo) and so we must check
; that foo is compliant.  We must also check that the guard of foo is
; T, because the guard of recog-name is T and we might call foo on
; anything.

; Accessor-name is not interesting:  its guard is namep and its body is
; primitive.  We will have checked that namep is compliant.

; Updater-name is not interesting:  its guard may involve translated
; type-specs and will involve namep, but we will have checked their
; compliance already.

; Length-name and resize-name have guards that are calls of namep, and
; their bodies are known to satisfy their guards.

; So it all boils down to checking the compliance of the body of
; recog-name, for each component.  Note that we must check both that
; the type-spec only involves compliant functions and that every
; non-system function used has a guard of T.

(defun defconst-name (name)
  (intern-in-package-of-symbol
   (concatenate 'string "*" (symbol-name name) "*")
   name))

(defun chk-acceptable-defstobj1
  (name field-descriptors ftemps renaming ctx wrld state names const-names)

; We check whether it is legal to define name as a single-threaded
; object with the description given in field-descriptors.  We know
; name is a legal (and new) stobj name and we know that renaming is an
; symbol to symbol doublet-style alist.  But we know nothing else.  We
; either signal an error or return the world in which the event is to
; be processed (thus implementing redefinitions).  Names is, in
; general, the actual set of names that the defstobj event will
; introduce.  That is, it contains the images of the default names
; under the renaming alist.  We accumulate the actual names into it as
; we go and check that it contains no duplicates at the termination of
; this function.  All of the names in names are to be defined as
; functions with :VERIFY-GUARDS T.  See the comment above about
; Common Lisp compliance.

  (cond
   ((endp ftemps)
    (let* ((recog-name (defstobj-fnname name :recognizer :top renaming))
           (creator-name (defstobj-fnname name :creator :top renaming))
           (names (list* recog-name creator-name names)))
      (er-progn
       (chk-all-but-new-name recog-name ctx 'function wrld state)
       (chk-all-but-new-name creator-name ctx 'function wrld state)
       (chk-acceptable-defstobj-renaming name field-descriptors renaming
                                         ctx state nil)

; Note: We insist that all the names be new.  In addition to the
; obvious necessity for something like this, we note that this does
; not permit us to have redundantly defined any of these names.  For
; example, the user might have already defined a field recognizer,
; PCP, that is identically defined to what we will lay down.  But we
; do not allow that.  We basically insist that we have control over
; every one of these names.

       (chk-just-new-names names 'function nil ctx wrld state)
       (chk-just-new-names const-names 'const nil ctx wrld state))))
   (t

; An element of field-descriptors (i.e., of ftemps) is either a
; symbolic field name, field, or else of the form (field :type type
; :initially val), where either or both of the keyword fields can be
; omitted.  Val must be an evg, i.e., an unquoted constant like t,
; nil, 0 or undef (the latter meaning the symbol 'undef).  :Type
; defaults to the unrestricted type t and :initially defaults to nil.
; Type is either a primitive type, as recognized by
; translate-declaration-to-guard, or else is of the form (array ptype
; (n)) where ptype is a primitive type and n is an positive integer
; constant.

    (er-progn
     (chk-stobj-field-descriptor name (car ftemps) ctx wrld state)
     (let* ((field (if (atom (car ftemps))
                       (car ftemps)
                     (car (car ftemps))))
            (type (if (consp (car ftemps))
                      (or (cadr (assoc-keyword :type
                                               (cdr (car ftemps))))
                          t)
                    t))
            (key2 (if (and (consp type)
                           (eq (car type) 'array))
                      :array
                    :non-array))
            (fieldp-name (defstobj-fnname field :recognizer key2 renaming))
            (accessor-name (defstobj-fnname field :accessor key2 renaming))
            (accessor-const-name (defconst-name accessor-name))
            (updater-name (defstobj-fnname field :updater key2 renaming))
            (length-name (defstobj-fnname field :length key2 renaming))
            (resize-name (defstobj-fnname field :resize key2 renaming)))
       (er-progn
        (chk-all-but-new-name fieldp-name ctx 'function wrld state)
        (chk-all-but-new-name accessor-name ctx 'function wrld state)
        (chk-all-but-new-name updater-name ctx 'function wrld state)
        (chk-all-but-new-name accessor-const-name ctx 'const wrld state)
        (if (eq key2 :array)
            (er-progn (chk-all-but-new-name length-name ctx 'function wrld state)
                      (chk-all-but-new-name resize-name ctx 'function wrld state))
          (value nil))
        (chk-acceptable-defstobj1 name field-descriptors (cdr ftemps)
                                  renaming ctx wrld state
                                  (list* fieldp-name
                                         accessor-name
                                         updater-name
                                         (if (eq key2 :array)
                                             (list* length-name
                                                    resize-name
                                                    names)
                                           names))
                                  (cons accessor-const-name
                                        const-names))))))))

(defun the-live-var (name)

; If the user declares a single-threaded object named $S then we will
; use *the-live-$s* as the Lisp parameter holding the live object
; itself.  One might wonder why we don't choose to name this object
; $s?  Perhaps we could, since starting with Version  2.6 we no longer
; get the symbol-value of *the-live-$s* except at the top level,
; because of local stobjs.  Below we explain our earlier thinking.

; Historical Plaque for Why the Live Var for $S Is Not $S

; [Otherwise] Consider how hard it would then be to define the raw defs
; (below).  $S is the formal parameter, and naturally so since we want
; translate to enforce the rules on single-threadedness.  The raw code
; has to check whether the actual is the live object.  We could hardly
; write (eq $S $S).

  (packn-pos (list "*THE-LIVE-" name "*") name))

(defconst *defstobj-keywords*
  '(:renaming :doc :inline))

(defun defstobj-redundancy-bundle (name args storep)

; See redundant-defstobjp to see how this is used.

; The treatment of erp below is justified as follows.  When this function is
; called to create the redundancy bundle for an admitted defstobj, erp is
; guaranteed to be nil (storep = t).  If this function is used to compute a
; redundancy bundle for a new purported but ill-formed defstobj, the bundle
; will contain the symbol 'error in the field-descriptors slot, which will
; cause it not to match any correct redundancy bundle.  Thus, the purported
; defstobj will not be considered redundant and the error will be detected by
; the admissions process.

  (mv-let
   (erp field-descriptors key-alist)
   (partition-rest-and-keyword-args args *defstobj-keywords*)
   (cons (if erp
             (if storep
                 (er hard 'defstobj-redundancy-bundle
                     "Implementation error: ~x0 returned an error when ~
                      storing the redundancy-bundle for defstobj.  Please ~
                      contact the implementors."
                     `(defstobj-redundancy-bundle ,name ,args ,storep))
               'error)
           field-descriptors)
         (cdr (assoc-eq :renaming key-alist)))))

(defun redundant-defstobjp (name args wrld)

; Note: At one time we stored the defstobj template on the property
; list of a defstobj name and we computed the new template from args
; and compared the two templates to identify redundancy.  To make this
; possible without causing runtime errors we had to check, here, that
; the arguments -- which have not yet been checked for well-formedness
; -- were at least of the right basic shape, e.g., that the renaming
; is a doublet-style-symbol-to-symbol-alistp and that each
; field-descriptor is either a symbol or a true-list of length 1, 3,
; or 5 with :type and :initially fields.  But this idea suffered the
; unfortunate feature that an illegal defstobj event could be
; considered redundant.  For example, if the illegal event had a
; renaming that included an unnecessary function symbol in its domain,
; that error was not caught.  The bad renaming produced a good
; template and if a correct version of that defstobj had previously
; been executed, the bad one was recognized as redundant.
; Unfortunately, if one were to execute the bad one first, an error
; would result.
       
; So we have changed this function to be extremely simple.  

  (and (getprop name 'stobj nil 'current-acl2-world wrld)
       (equal (getprop name 'redundancy-bundle nil
                       'current-acl2-world wrld)
              (defstobj-redundancy-bundle name args nil))))

(defun chk-acceptable-defstobj (name args ctx wrld state)

; We check that (defstobj name . args) is well-formed and either
; signal an error or return nil.

  (mv-let
   (erp field-descriptors key-alist)
   (partition-rest-and-keyword-args args *defstobj-keywords*)
   (cond
    (erp
     (er soft ctx
         "The keyword arguments to the DEFSTOBJ event must appear ~
          after all field descriptors.  The allowed keyword ~
          arguments are ~&0, and these may not be duplicated, and ~
          must be followed by the corresponding value of the keyword ~
          argument.  Thus, ~x1 is ill-formed."
         *defstobj-keywords*
         (list* 'defstobj name args)))
    (t
     (let ((renaming (cdr (assoc-eq :renaming key-alist)))
           (doc (cdr (assoc-eq :doc key-alist)))
           (inline (cdr (assoc-eq :inline key-alist))))
       (cond
        ((redundant-defstobjp name args wrld)
         (value 'redundant))
        ((not (booleanp inline))
         (er soft ctx
             "DEFSTOBJ requires the :INLINE keyword argument to have a Boolean ~
              value.  See :DOC stobj."
             (list* 'defstobj name args)))
        (t
         (er-progn

; The defstobj name itself is not subject to renaming.  So we check it
; before we even bother to check the well-formedness of the renaming alist.

          (chk-all-but-new-name name ctx 'stobj wrld state)
          (cond ((or (eq name 'I)
                     (eq name 'V))
                 (er soft ctx
                     "DEFSTOBJ does not allow single-threaded objects with ~
                      the names I or V because those symbols are used as ~
                      formals, along with the new stobj name itself, in ~
                      ``primitive'' stobj functions that will be ~
                      defined."))
                (t (value nil)))
          (chk-legal-defstobj-name name state)
          (cond ((not (doublet-style-symbol-to-symbol-alistp renaming))
                 (er soft ctx
                     "The :RENAMING argument to DEFSTOBJ must be an ~
                      alist containing elements of the form (sym ~
                      sym), where each element of such a doublet is a ~
                      symbol. Your argument, ~x0, is thus illegal."
                     renaming))
                (t (value nil)))

; We use translate-doc here just to check the string.  We throw away
; the section-symbol and citations returned.  We'll repeat this later.

          (translate-doc name doc ctx state)
          (er-let*
            ((wrld1 (chk-just-new-name name 'stobj nil ctx wrld state))
             (wrld2 (chk-just-new-name (the-live-var name) 'stobj-live-var
                                       nil ctx wrld1 state)))
            (chk-acceptable-defstobj1 name field-descriptors
                                      field-descriptors renaming
                                      ctx wrld2 state nil nil))))))))))

; Essay on Defstobj Definitions

; Consider the following defstobj:

  #|
  (defstobj $st
    (flag :type t :initially run)
    (pc   :type (integer 0 255) :initially 128)
    (mem  :type (array (integer 0 255) (256)) :initially 0)
    :renaming ((pc pcn)))
  |#

; If you call (defstobj-template '$st '((flag ...) ...)) you will get
; back a ``template'' which is sort of a normalized version of the
; event with the renaming applied and all the optional slots filled
; appropriately.  (See the definition of defstobj-template for details.)
; Let template be that template.

; To see the logical definitions generated by this defstobj event, invoke
;   (defstobj-axiomatic-defs '$st template (w state))

; To see the raw lisp definitions generated, invoke
;   (defstobj-raw-defs '$st template (w state))

; The *1* functions for the functions are all generated by oneifying
; the axiomatic defs.

; To see the deconsts generated, invoke
;   (defstobj-defconsts (strip-accessor-names (caddr template)) 0)

; It is important the guard conjectures for these functions be
; provable!  They are assumed by the admission process!  To prove
; the guards for the defstobj above, it helped to insert the following
; lemma after the defun of memp but before the definition of memi.

  #|
  (defthm memp-implies-true-listp
    (implies (memp x)
             (true-listp x)))
  |#

; Even without this lemma, the proof succeeded, though it took much
; longer and involved quite a few generalizations and inductions.

; If you change any of the functions, I recommend generating the axiomatic
; defs for a particular defstobj such as that above and proving the guards.

; Up through v2-7 we also believed that we ensured that the guards in the
; axiomatic defs are sufficient for the raw defs.  However, starting with v2-8,
; this became moot because of the following claim: the raw Lisp functions are
; only called on live stobjs (this change, and others involving :inline, were
; contributed by Rob Sumners).  We believe this claim because of the following
; argument.
;
;   a) The *1* function now has an additional requirement that not only does
;      guard checking pass, but also, all of the stobjs arguments passed in
;      must be the live stobjs in order to execute raw Common Lisp.
;   b) Due to the syntactic restrictions that ACL2 enforces, we know that the
;      direct correspondence between live stobjs and stobj arguments in the
;      raw Common Lisp functions will persist throughout evaluation.
;      -- This can be proven by induction over the sequence of function calls
;         in any evaluation.
;      -- The base case is covered by the binding of stobj parameters to
;         the global live stobj in the acl2-loop, or by the restrictions
;         placed upon with-local-stobj.
;      -- The induction step is proven by the signature requirements of
;         functions that access and/or update stobjs.

; A reasonable question is: Should the guard for resize-name be
; strengthened so as to disallow sizes of at least (1- (expt 2 28))?
; Probably there is no need for this.  Logically, there is no such
; restriction; it is OK for the implementation to insist on such a
; bound when actually executing.

; Now we introduce the idea of the "template" of a defstobj, which
; includes a normalized version of the field descriptors under the
; renaming.

(defun defstobj-fields-template (field-descriptors renaming)
  (cond
   ((endp field-descriptors) nil)
   (t
    (let* ((field (if (atom (car field-descriptors))
                      (car field-descriptors)
                    (car (car field-descriptors))))
           (type (if (consp (car field-descriptors))
                     (or (cadr (assoc-keyword :type
                                              (cdr (car field-descriptors))))
                         t)
                   t))
           (init (if (consp (car field-descriptors))
                     (cadr (assoc-keyword :initially
                                          (cdr (car field-descriptors))))
                   nil))
           (resizable (if (consp (car field-descriptors))
                          (cadr (assoc-keyword :resizable
                                               (cdr (car field-descriptors))))
                        nil))
           (key2 (if (and (consp type)
                          (eq (car type) 'array))
                     :array
                   :non-array))
           (fieldp-name (defstobj-fnname field :recognizer key2 renaming))
           (accessor-name (defstobj-fnname field :accessor key2 renaming))
           (updater-name (defstobj-fnname field :updater key2 renaming))
           (resize-name (defstobj-fnname field :resize key2 renaming))
           (length-name (defstobj-fnname field :length key2 renaming)))
      (cons (list fieldp-name
                  type
                  init
                  accessor-name
                  updater-name
                  length-name
                  resize-name
                  resizable)
            (defstobj-fields-template (cdr field-descriptors) renaming))))))

(defun defstobj-doc (args)

; We retrieve the doc string, if any, from (defstobj name . args).

  (mv-let (erp field-descriptors key-alist)
          (partition-rest-and-keyword-args args *defstobj-keywords*)
          (declare (ignore field-descriptors))
          (assert$ (not erp)
                   (cdr (assoc-eq :doc key-alist)))))

(defun defstobj-template (name args)

; We unpack the args to get the renamed field descriptors.  We return
; a list of the form (namep create-name fields doc inline), where:
; namep is the name of the recognizer for the single-threaded object;
; create-name is the name of the constructor for the stobj; fields is
; a list corresponding to the field descriptors, but normalized with
; respect to the renaming, types, etc.; doc is the doc string, or
; nil if no doc string is supplied; and inline is t if :inline t was
; specified in the defstobj event, else nil.  A field in fields is of
; the form (recog-name type init accessor-name updater-name
; length-name resize-name resizable).  The last three fields are nil
; unless type has the form (ARRAY ptype (n)), in which case ptype is a
; primitive type and n is a positive integer.  Init is the evg of a
; constant term, i.e., should be quoted to be a treated as a term.
; Doc is the value of the :doc keyword arg in args.

  (mv-let
   (erp field-descriptors key-alist)
   (partition-rest-and-keyword-args args *defstobj-keywords*)
   (cond
    (erp

; If the defstobj has been admitted, this won't happen.

     (er hard 'defstobj
         "The keyword arguments to the DEFSTOBJ event must appear ~
          after all field descriptors.  The allowed keyword ~
          arguments are ~&0, and these may not be duplicated.  Thus, ~
          ~x1 is ill-formed."
         *defstobj-keywords*
         (list* 'defstobj name args)))
    (t
     (let ((renaming (cdr (assoc-eq :renaming key-alist)))
           (doc (cdr (assoc-eq :doc key-alist)))
           (inline (cdr (assoc-eq :inline key-alist))))
       (list (defstobj-fnname name :recognizer :top renaming)
             (defstobj-fnname name :creator :top renaming)
             (defstobj-fields-template field-descriptors renaming)
             doc
             inline))))))

(defun defstobj-component-recognizer-calls (ftemps n var ans)

; Warning:  See the guard remarks in the Essay on Defstobj Definitions.

; Given a list of field templates, e.g., ((regp ...) (pcp ...) ...),
; where n is one less than the number of fields and var is some
; symbol, v, we return ((regp (nth 0 v)) (pcp (nth 1 v)) ...).  Except,
; if field represents a non-resizable array then we also include a
; corresponding length statement in the list.

  (cond ((endp ftemps)
         (reverse ans))
        (t (defstobj-component-recognizer-calls
             (cdr ftemps)
             (+ n 1)
             var
             (let* ((type (cadr (car ftemps)))
                    (nonresizable-ar (and (consp type)
                                          (eq (car type) 'array)
                                          (not (nth 7 (car ftemps)))))
                    (pred-stmt `(,(car (car ftemps)) (nth ,n ,var))))
               (if nonresizable-ar
                   (list* `(equal (len (nth ,n ,var)) ,(car (caddr type)))
                          pred-stmt
                          ans)
                 (cons pred-stmt ans)))))))

(defun defstobj-component-recognizer-axiomatic-defs (name template ftemps wrld)

; Warning:  See the guard remarks in the Essay on Defstobj Definitions.

; It is permissible for wrld to be nil, as this merely defeats additional
; checking by translate-declaration-to-guard.

; We return a list of defs (see defstobj-axiomatic-defs) for all the
; recognizers for the single-threaded resource named name with the
; given template.  The answer contains the top-level recognizer and
; creator for the object, as well as the definitions of all component
; recognizers.  The answer contains defs for auxiliary functions used
; in array component recognizers.  The defs are listed in an order
; suitable for processing (components first, then top-level).

  (cond
   ((endp ftemps)
    (let* ((recog-name (car template))
           (field-templates (caddr template))
           (n (length field-templates)))

; Rockwell Addition: See comment below.

; Note: The recognizer for a stobj must be Boolean!  That is why we
; conclude the AND below with a final T.  The individual field
; recognizers need not be Boolean and sometimes are not!  For example,
; a field with :TYPE (MEMBER e1 ... ek) won't be Boolean, nor with
; certain :TYPE (OR ...) involving MEMBER.  The reason we want the
; stobj recognizer to be Boolean is so that we can replace it by T in
; guard conjectures for functions that have been translated with the
; stobj syntactic restrictions.  See optimize-stobj-recognizers.

      (list `(,recog-name (,name)
                          (declare (xargs :guard t
                                          :verify-guards t))
                          (and (true-listp ,name)
                               (= (length ,name) ,n)
                               ,@(defstobj-component-recognizer-calls
                                   field-templates 0 name nil)
                               t)))))
   (t
    (let ((recog-name (nth 0 (car ftemps)))
          (type (nth 1 (car ftemps))))

; Below we simply append the def or defs for this field to those for
; the rest.  We get two defs for each array field and one def for each
; of the others.

      (cons (cond
             ((and (consp type)
                   (eq (car type) 'array))
              (let ((etype (cadr type)))
                `(,recog-name (x)
                              (declare (xargs :guard t
                                              :verify-guards t))
                              (if (atom x)
                                  (equal x nil)
                                  (and ,(translate-declaration-to-guard
                                         etype '(car x) wrld)
                                       (,recog-name (cdr x)))))))
             (t (let ((type-term (translate-declaration-to-guard
                                  type 'x wrld)))
                  
; We may not use x in the type-term and so have to declare it ignored.

                  (cond
                   ((member-eq 'x (all-vars type-term))
                    `(,recog-name (x)
                                  (declare (xargs :guard t
                                                  :verify-guards t))
                                  ,type-term))
                   (t 
                    `(,recog-name (x)
                                  (declare (xargs :guard t
                                                  :verify-guards t)
                                           (ignore x))
                                  ,type-term))))))
            (defstobj-component-recognizer-axiomatic-defs 
              name template (cdr ftemps) wrld))))))

(defun defstobj-field-fns-axiomatic-defs (top-recog var n ftemps wrld)

; Warning:  See the guard remarks in the Essay on Defstobj Definitions.

; We return a list of defs (see defstobj-axiomatic-defs) for all the accessors,
; updaters, and optionally, array resizing and length, of a single-threaded
; resource.

  (cond
   ((endp ftemps)
    nil)
   (t (let* ((field-template (car ftemps))
             (type (nth 1 field-template))
             (init (nth 2 field-template))
             (arrayp (and (consp type) (eq (car type) 'array)))
             (type-term (and (not arrayp)
                             (translate-declaration-to-guard type 'v wrld)))
             (array-etype (and arrayp (cadr type)))
             (array-etype-term
              (and arrayp
                   (translate-declaration-to-guard array-etype 'v wrld)))
             (array-length (and arrayp (car (caddr type))))
             (accessor-name (nth 3 field-template))
             (updater-name (nth 4 field-template))
             (length-name (nth 5 field-template))
             (resize-name (nth 6 field-template))
             (resizable (nth 7 field-template)))
        (cond
         (arrayp
          (append 
           `((,length-name (,var)
                           (declare (xargs :guard (,top-recog ,var)
                                           :verify-guards t)
                                    ,@(and (not resizable)
                                           `((ignore ,var))))
                           ,(if resizable
                                `(len (nth ,n ,var))
                              `,array-length))
             (,resize-name
              (i ,var)
              (declare (xargs :guard (,top-recog ,var)
                              :verify-guards t)
                       ,@(and (not resizable)
                              '((ignore i))))
              ,(if resizable
                   `(update-nth ,n
                                (resize-list (nth ,n ,var) i ',init)
                                ,var)
                 `(prog2$ (hard-error
                           ',resize-name
                           "The array field corresponding to accessor ~x0 of ~
                             stobj ~x1 was not declared :resizable t.  ~
                             Therefore, it is illegal to resize this array."
                           (list (cons #\0 ',accessor-name)
                                 (cons #\1 ',var)))
                          ,var)))
              (,accessor-name (i ,var)
                              (declare (xargs :guard
                                              (and (,top-recog ,var)
                                                   (integerp i)
                                                   (<= 0 i)
                                                   (< i (,length-name ,var)))
                                              :verify-guards t))
                              (nth i (nth ,n ,var)))
              (,updater-name (i v ,var)
                             (declare (xargs :guard
                                             (and (,top-recog ,var)
                                                  (integerp i)
                                                  (<= 0 i)
                                                  (< i (,length-name ,var))
                                                  ,@(if (equal array-etype-term
                                                               t)
                                                        nil
                                                      (list array-etype-term)))
                                             :verify-guards t))
                             (update-nth-array ,n i v ,var)))
           (defstobj-field-fns-axiomatic-defs
             top-recog var (+ n 1) (cdr ftemps) wrld)))
         (t
          (append 
           `((,accessor-name (,var)
                             (declare (xargs :guard (,top-recog ,var)
                                             :verify-guards t))
                             (nth ,n ,var))
             (,updater-name (v ,var)
                            (declare (xargs :guard
                                            ,(if (equal type-term t)
                                                 `(,top-recog ,var)
                                               `(and ,type-term
                                                     (,top-recog ,var)))
                                            :verify-guards t))
                            (update-nth ,n v ,var)))
           (defstobj-field-fns-axiomatic-defs
             top-recog var (+ n 1) (cdr ftemps) wrld))))))))

(defun defstobj-axiomatic-init-fields (ftemps)

; Keep this in sync with defstobj-raw-init-fields.

  (cond
   ((endp ftemps) nil)
   (t (let* ((field-template (car ftemps))
             (type (nth 1 field-template))
             (arrayp (and (consp type) (eq (car type) 'array)))
             (array-size (and arrayp (car (caddr type))))
             (init (nth 2 field-template)))
        (cond
         (arrayp
          (cons `(make-list ,array-size :initial-element ',init)
                (defstobj-axiomatic-init-fields (cdr ftemps))))
         (t ; whether the type is given or not is irrelevant
          (cons (kwote init)
                (defstobj-axiomatic-init-fields (cdr ftemps)))))))))

(defun defstobj-creator-fn (creator-name field-templates)

; This function generates the logic initialization code for the given stobj
; name.

  `(,creator-name
    ()
    (declare (xargs :guard t :verify-guards t))
    (list ,@(defstobj-axiomatic-init-fields field-templates))))

(defun defstobj-axiomatic-defs (name template wrld)

; Warning:  See the guard remarks in the Essay on Defstobj Definitions.

; Template is the defstobj-template for name and args and thus
; corresponds to some (defstobj name . args) event.  We generate the
; #+acl2-loop-only defs for that event and return a list of defs.  For
; each def it is the case that (defun . def) is a legal defun, and
; they can executed in the order returned.

; These defs are processed to axiomatize the recognizer, accessor and
; updater functions for the single-threaded resource.  They are also
; oneified when we process the defstobj CLTL-COMMAND to define the *1*
; versions of the functions.  Finally, parts of them are re-used in
; raw lisp code when the code is applied to an object other than the
; live one.

; WARNING: If you change the formals of these generated axiomatic
; defs, be sure to change the formals of the corresponding raw defs.

; See the Essay on Defstobj Definitions

  (let ((field-templates (caddr template)))
    (append
     (defstobj-component-recognizer-axiomatic-defs name template
       field-templates wrld)
     (cons
      (defstobj-creator-fn (cadr template) field-templates)
      (defstobj-field-fns-axiomatic-defs (car template) name 0
        field-templates wrld)))))

(defun simple-array-type (array-etype dimensions)
  (declare (ignore dimensions))
  (cond
   ((member-eq array-etype '(* t))
    `(simple-vector *))
   (t `(simple-array ,array-etype (*)))))

#-acl2-loop-only
(defun-one-output stobj-copy-array-aref (a1 a2 i n)
  (declare (type (unsigned-byte 28) i n))

; Copy the first n elements of array a1 into array a2, starting with index i,
; and then return a2.  See also copy-array-svref and stobj-copy-array-fix-aref.

  (cond
   ((>= i n) a2)
   (t (setf (aref a2 i)
            (aref a1 i))
      (stobj-copy-array-aref a1 a2
                             (the (unsigned-byte 28) (1+ i))
                             (the (unsigned-byte 28) n)))))

#-acl2-loop-only
(defun-one-output stobj-copy-array-svref (a1 a2 i n)
  (declare (type (unsigned-byte 28) i n)
           (type simple-vector a1 a2))

; This is a variant of copy-array-aref for simple vectors a1 and a2.

  (cond
   ((>= i n) a2)
   (t (setf (svref a2 i)
            (svref a1 i))
      (stobj-copy-array-svref a1 a2
                              (the (unsigned-byte 28) (1+ i))
                              (the (unsigned-byte 28) n)))))

#-acl2-loop-only
(defun-one-output stobj-copy-array-fix-aref (a1 a2 i n)
  (declare (type (unsigned-byte 28) i n)
           (type (simple-array (signed-byte 29) (*)) a1 a2))

; This is a variant of copy-array-aref for arrays of fixnums a1 and a2.  We
; need this special version to avoid fixnum boxing in GCL during resizing.

  (cond
   ((>= i n) a2)
   (t (setf (aref a2 i)
            (aref a1 i))
      (stobj-copy-array-fix-aref a1 a2
                                 (the (unsigned-byte 28) (1+ i))
                                 (the (unsigned-byte 28) n)))))

(defmacro the-live-stobjp (name)

; With the introduction of local stobjs, we still rely on the
; symbol-value of (the-live-var name) to get the global value of a
; stobj, but we must also consider let-bound values.  Because of
; translate and oneify, we know that we are binding the live var as we
; go.  We could use `(arrayp ,name) below, but we stick to the eq test
; for now.

  `(eq ,name ,(the-live-var name)))

(defun array-etype-is-fixnum-type (array-etype)
  (declare (xargs :guard 
                  (implies (consp array-etype)
                           (true-listp array-etype))))
  (and (consp array-etype)
       (case (car array-etype)
             (integer
              (let* ((e1 (cadr array-etype))
                     (int1 (if (integerp e1)
                               e1
                             (and (consp e1)
                                  (integerp (car e1))
                                  (1- (car e1)))))
                     (e2 (caddr array-etype))
                     (int2 (if (integerp e2)
                               e2
                             (and (consp e2)
                                  (integerp (car e2))
                                  (1- (car e2))))))
                (and int1
                     int2
                     (>= int1 (- *expt2-28*))
                     (< int2 *expt2-28*))))
             (mod
              (and (integerp (cadr array-etype))
                   (< (cadr array-etype) 
                      *expt2-28*)))
             (unsigned-byte
              (and (integerp (cadr array-etype))
                   (<= (cadr array-etype) 
                       28)))
             (signed-byte
              (and (integerp (cadr array-etype))
                   (<= (cadr array-etype) 
                       29))))))

(defun defstobj-field-fns-raw-defs (var inline n ftemps)

; Warning:  See the guard remarks in the Essay on Defstobj Definitions.

  (cond
   ((endp ftemps) nil)
   (t
    (append
     (let* ((field-template (car ftemps))
            (type (nth 1 field-template))
            (init (nth 2 field-template))
            (arrayp (and (consp type) (eq (car type) 'array)))
            (array-etype (and arrayp (cadr type)))
            (simple-type (and arrayp
                              (simple-array-type array-etype (caddr type))))
            (array-length (and arrayp (car (caddr type))))
            (vref (and arrayp
                       (if (eq (car simple-type) 'simple-vector)
                           'svref
                         'aref)))
            (fix-vref (and arrayp
                           (if (array-etype-is-fixnum-type array-etype)
                               'fix-aref
                             vref)))
            (max-index (and arrayp (1- *expt2-28*)))
            (accessor-name (nth 3 field-template))
            (updater-name (nth 4 field-template))
            (length-name (nth 5 field-template))
            (resize-name (nth 6 field-template))
            (resizable (nth 7 field-template)))
       (cond
        (arrayp
         `((,length-name
            (,var)
            ,@(and inline (list *stobj-inline-declare*))
            ,@(if (not resizable)
                  `((declare (ignore ,var))
                    ,array-length)
                `((the (integer 0 ,max-index)
                       (length (svref ,var ,n))))))
           (,resize-name
            (k ,var)
            ,@(if (not resizable)
                  `((declare (ignore k))
                    (prog2$
                      (er hard ',resize-name
                          "The array field corresponding to accessor ~x0 of ~
                           stobj ~x1 was not declared :resizable t.  ~
                           Therefore, it is illegal to resize this array."
                          ',accessor-name
                          ',var)
                      ,var))
                `((if (not (and (integerp k)
                                (>= k 0)
                                (< k ,max-index)))
                      (hard-error
                       ',resize-name
                       "Attempted array resize failed because the requested ~
                        size ~x0 was not an integer between 0 and (1- (expt ~
                        2 28)).  These bounds on array sizes are fixed by ~
                        ACL2."
                       (list (cons #\0 k)))
                    (let* ((old (svref ,var ,n))
                           (min-index (if (< k (length old))
                                          k
                                        (length old)))
                           (new (make-array$ k

; The :initial-element below is probably not necessary in the case
; that we are downsizing the array.  At least, CLtL2 does not make any
; requirements about specifying an :initial-element, even when an
; :element-type is supplied.  However, it seems harmless enough to go
; ahead and specify :initial-element even for downsizing: resizing is
; not expected to be fast, we save a case split here (at the expense
; of this comment!), and besides, we are protecting against the
; possibility that some Common Lisp will fail to respect the spec and
; will cause an error by trying to initialize a fixnum array (say)
; with NILs.

                                             :initial-element
                                             ',init
                                             :element-type
                                             ',array-etype)))
                      (setf (svref ,var ,n)
                            (,(pack2 'stobj-copy-array- fix-vref)
                             old new 0 min-index))
                      ,var)))))
           (,accessor-name
            (i ,var)
            (declare (type (integer 0 ,max-index) i))
            ,@(and inline (list *stobj-inline-declare*))
            (the ,array-etype
              (,vref (the ,simple-type (svref ,var ,n))
                     (the (integer 0 ,max-index) i))))
           (,updater-name
            (i v ,var)
            (declare (type (integer 0 ,max-index) i)
                     (type ,array-etype v))
            ,@(and inline (list *stobj-inline-declare*))
            (progn 
              (setf (,vref (the ,simple-type (svref ,var ,n))
                           (the (integer 0 ,max-index) i))
                    (the ,array-etype v))
              ,var))))
        ((equal type t)
         `((,accessor-name (,var)
                           ,@(and inline (list *stobj-inline-declare*))
                           (svref ,var ,n))
           (,updater-name (v ,var)
                          ,@(and inline (list *stobj-inline-declare*))
                          (progn (setf (svref ,var ,n) v) ,var))))
        (t
         `((,accessor-name (,var)
                           ,@(and inline (list *stobj-inline-declare*))
                           (the ,type
                                (aref (the (simple-array ,type (1))
                                           (svref ,var ,n))
                                      0)))
           (,updater-name (v ,var)
                          (declare (type ,type v))
                          ,@(and inline (list *stobj-inline-declare*))
                          (progn
                            (setf (aref (the (simple-array ,type (1))
                                             (svref ,var ,n))
                                        0)
                                  (the ,type v))
                            ,var))))))
     (defstobj-field-fns-raw-defs var inline (1+ n) (cdr ftemps))))))

(defun defstobj-raw-init-fields (ftemps)

; Keep this in sync with defstobj-axiomatic-init-fields.

  (cond
   ((endp ftemps) nil)
   (t (let* ((field-template (car ftemps))
             (type (nth 1 field-template))
             (arrayp (and (consp type) (eq (car type) 'array)))
             (array-etype (and arrayp (cadr type)))
             (array-size (and arrayp (car (caddr type))))
             (init (nth 2 field-template)))
        (cond
         (arrayp
          (cons `(make-array$ ,array-size
                              :element-type ',array-etype
                              :initial-element ',init)
                (defstobj-raw-init-fields (cdr ftemps))))
         ((equal type t)
          (cons (kwote init) (defstobj-raw-init-fields (cdr ftemps))))
         (t (cons `(make-array$ 1
                                :element-type ',type
                                :initial-element ',init)
                  (defstobj-raw-init-fields (cdr ftemps)))))))))

(defun defstobj-raw-init (template)

; This function generates the initialization code for the live object
; representing the stobj name.

  (let ((field-templates (caddr template)))
    `(vector ,@(defstobj-raw-init-fields field-templates))))

(defun defstobj-raw-defs (name template wrld)

; Warning:  See the guard remarks in the Essay on Defstobj Definitions.

; This function generates a list of defs.  Each def is such that
; (defun . def) is a well-formed raw Lisp definition.  The defuns can
; be executed in raw lisp to define the versions of the recognizers,
; accessors, and updaters (and for array fields, length and resize
; functions) that are run when we know the guards are satisfied.  Many
; of these functions anticipate application to the live object itself.

; It is permissible for wrld to be nil, as this merely defeats additional
; checking by translate-declaration-to-guard.

; WARNING: If you change the formals of these generated raw defs be
; sure to change the formals of the corresponding axiomatic defs.

  (let* ((recog (first template))
         (creator (second template))
         (field-templates (third template))
         (inline (fifth template)))
    (append
     (all-but-last
      (defstobj-component-recognizer-axiomatic-defs name template
        field-templates wrld))
     (list* `(,recog (,name)
                     (cond
                      ((the-live-stobjp ,name)
                       t)
                      (t (and (true-listp ,name)
                              (= (length ,name) ,(length field-templates))
                              ,@(defstobj-component-recognizer-calls
                                  field-templates 0 name nil)))))
            `(,creator ()
                       ,(defstobj-raw-init template))
            (defstobj-field-fns-raw-defs name inline 0 field-templates)))))

(defun put-stobjs-in-and-outs1 (name ftemps wrld)

; See put-stobjs-in-and-outs for a table that explains what we're doing.

  (cond
   ((endp ftemps) wrld)
   (t (let ((type (nth 1 (car ftemps)))
            (acc-fn (nth 3 (car ftemps)))
            (upd-fn (nth 4 (car ftemps)))
            (length-fn (nth 5 (car ftemps)))
            (resize-fn (nth 6 (car ftemps))))
        (put-stobjs-in-and-outs1
         name
         (cdr ftemps)
         (cond
          ((and (consp type)
                (eq (car type) 'array))
           (putprop
            length-fn 'stobjs-in (list name) 
            (putprop
             resize-fn 'stobjs-in (list nil name)
             (putprop
              resize-fn 'stobjs-out (list name)
              (putprop
               acc-fn 'stobjs-in (list nil name)
               (putprop
                upd-fn 'stobjs-in (list nil nil name)
                (putprop
                 upd-fn 'stobjs-out (list name) wrld)))))))
          (t
           (putprop
            acc-fn 'stobjs-in (list name)
            (putprop
             upd-fn 'stobjs-in (list nil name)
             (putprop
              upd-fn 'stobjs-out (list name) wrld))))))))))
          
(defun put-stobjs-in-and-outs (name template wrld)

; We are processing a (defstobj name . args) event for which template
; is the template.  Wrld is a world containing the definitions of the
; accessors, updaters and recognizers of the stobj -- all of which
; were processed before we declared that name is a stobj.  Wrld now
; also contains the belated declaration that name is a stobj.  We now
; put the STOBJS-IN and STOBJS-OUT properties for the appropriate
; names.

; Relevant functions and their settings:

;      fn                  stobjs-in         stobjs-out
; topmost recognizer       (name)            (nil)
; creator                  ()                (name)
; field recogs             (nil ...)         (nil)  
; simple accessor          (name)            (nil)
; array accessor           (nil name)        (nil)
; simple updater           (nil name)        (name)
; array updater            (nil nil name)    (name)

; The entries above not involving name were correctly computed before
; we knew that name was a stobj and hence are correct in wrld now.

; It is important to realize, in the case of the topmost recognizer
; and the accessors -- which do not return stobjs, that the appearance
; of name in the stobjs-in setting can be interpreted to mean ``the
; stobj name MAY be supplied here'' as opposed to ``MUST be supplied
; here.''

  (let ((recog-name (car template))
        (creator-name (cadr template))
        (field-templates (caddr template)))

; Each element of field templates is of the form:
;       0      1                     2    3    4        5
; (field-recog field-recog-helper-fn type init accessor updater)
; or, for arrays,
; (field-recog field-recog-helper-fn type init accessor updater length-name
;  resize-name)
; and we know if the field is simple or an array according to whether
; (car type) is ARRAY.

    (put-stobjs-in-and-outs1 name
                             field-templates
                             (putprop creator-name
                                      'STOBJS-OUT
                                      (list name)
                                      (putprop recog-name
                                               'STOBJS-IN
                                               (list name)
                                               wrld)))))

(defun defconst-name-alist (lst n)
  (if (endp lst)
      nil
    (cons (cons n (defconst-name (car lst)))
          (defconst-name-alist (cdr lst) (1+ n)))))

(defun accessor-array (name field-names)
  (let ((len (length field-names)))
    (compress1 name
               (cons `(:HEADER :DIMENSIONS (,len)
                               :MAXIMUM-LENGTH ,(+ 1 len)
                               :DEFAULT nil ; should be ignored
                               :NAME ,name
                               :ORDER :none)
                     (defconst-name-alist field-names 0)))))

(defun strip-accessor-names (x)

; This could just as well be called strip-cadddrs.  X is the caddr of a
; defstobj template; see defstobj-template.

  (if (endp x)
      nil
    (cons (cadddr (car x))
          (strip-accessor-names (cdr x)))))

(defun defstobj-defconsts (names index)
  (if (endp names)
      nil
    (cons `(defconst ,(defconst-name (car names)) ,index)
          (defstobj-defconsts (cdr names) (1+ index)))))

(defun defstobj-fn (name args state event-form)
  (with-ctx-summarized
   (if (output-in-infixp state)
       event-form
     (msg "( DEFSTOBJ ~x0 ...)" name))
   (let ((event-form (or event-form (list* 'defstobj name args)))
         (wrld0 (w state)))
     (er-let*
       ((wrld1 (chk-acceptable-defstobj name args ctx wrld0 state)))
       (cond
        ((eq wrld1 'redundant)
         (stop-redundant-event state))
        (t
         (enforce-redundancy
          event-form ctx wrld0
          (let* ((template (defstobj-template name args))
                 (field-names (strip-accessor-names (caddr template)))
                 (defconsts (defstobj-defconsts field-names 0))
                 (field-const-names (strip-cadrs defconsts))
                 (ax-def-lst (defstobj-axiomatic-defs name template wrld1))
                 (raw-def-lst (defstobj-raw-defs name template wrld1))
                 (recog-name (car template))
                 (creator-name (cadr template))
                 (names (strip-cars ax-def-lst))
                 (the-live-var (the-live-var name))
                 (doc (defstobj-doc args)))
            (er-progn
             (cond ((set-equalp-equal names
                                      (strip-cars raw-def-lst))
                    (value nil))
                   (t (value
                       (er hard ctx
                           "Defstobj-axiomatic-defs and defstobj-raw-defs are ~
                            out of sync!  They should each define the same set ~
                            of names.  Here are the functions with axiomatic ~
                            defs that have no raw defs:  ~x0.  And here are ~
                            the with raw defs but no axiomatic ones:  ~x1."
                           (set-difference-equal
                            names
                            (strip-cars raw-def-lst))
                           (set-difference-equal
                            (strip-cars raw-def-lst)
                            names)))))
             (revert-world-on-error
              (pprogn
               (set-w 'extension wrld1 state)
               (er-progn
                (process-embedded-events 'defstobj
                                         (table-alist 'acl2-defaults-table wrld1)
                                         t                     ;;; skip-proofsp
                                         (current-package state)
                                         (list 'defstobj name names)
                                         (append

; We only need to lay down these set-*-ok calls in the case that we do not
; allow array resizing, for the resizing and length field functions.  But for
; simplicity, we always lay them down.  Note that since we are really modifying
; the acl2-defaults-table here, their effect is local to the defstobj.

                                          '((set-ignore-ok t)
                                            (set-irrelevant-formals-ok t))
                                          (pairlis-x1 'defun ax-def-lst)
                                          defconsts

; It is important to disable the executable counterpart of the creator
; function, so as not to expose the live stobj during proofs.  We ensure in
; function chk-theory-expr-value1 that the :executable-counterpart rune below
; will never be enabled.

                                          `((in-theory
                                             (disable
                                              (:executable-counterpart
                                               ,creator-name)))))
                                         0
                                         t ; might as well do make-event check
                                         ctx state)


; The processing above will define the functions in the logic, using
; defun, and that, in turn, will define their *1* counterparts in
; Lisp.  But because of code in defuns-fn, the processing above will
; not define the raw Lisp versions of the functions themselves
; (normally that would be derived from the axiomatic defs just
; processed).  Instead, we will store a CLTL-COMMAND below that
; handles the raw Lisp defs only.

; What follows is hard to follow and rather arcane.  Why do we include
; name in the ee-entry computed above, (defstobj name names)?  That
; entry will be added to the embedded-event-lst by
; process-embedded-events and be inspected by the individual defuns
; done.  Those defuns will recognize their fn name, fn, among names,
; to detect that they are being done as part of a defstobj.  The defun
; will pick up the stobj name, name, from the ee-entry and build it
; into the ignorep entry of the defun CLTL-COMMAND, to be processed by
; add-trip.  In add-trip, the stobj name, name, will find its way into
; the oneify-cltl-code that generates the *1* body for fn.  That body
; contains a throw upon detection of a guard error.  The object thrown
; contains the stobjs-in of the offensive expression, so we will know
; how to print it.  But the stobjs-in of fn is incorrectly set in the
; world right now -- more accurately, will be incorrectly set in the
; world in which the defun is done and the throw form is constructed
; -- because we have not yet declared name to be a stobj.  Indeed, we
; cannot declare it to be a stobj yet since we are defining functions
; that treat it as an ordinary list.  This is the stobj version of the
; super-defun-wart problem.

                (er-let*
                 ((doc-pair (translate-doc name doc ctx state)))
                 (let* ((wrld2 (w state))
                        (wrld3 (update-doc-data-base
                                name doc doc-pair

; Here I declare that name is Common Lisp compliant.  Below I
; similarly declare the-live-var.  All elements of the namex list of
; an event must have the same symbol-class.

                                (putprop
                                 name 'symbol-class :common-lisp-compliant
                                 (put-stobjs-in-and-outs
                                  name template

; Rockwell Addition: It is convenient for the recognizer to be in a
; fixed position in this list, so I can find out its name.

                                  (putprop
                                   name 'stobj
                                   (cons the-live-var
                                         (cons recog-name
                                               (append (remove1-eq recog-name
                                                                   names)
                                                       field-const-names)))
                                   (putprop
                                    name 'redundancy-bundle
                                    (defstobj-redundancy-bundle name args t)
                                    (putprop-x-lst1
                                     names 'stobj-function name
                                     (putprop-x-lst1
                                      field-const-names 'stobj-constant name
                                      (putprop
                                       the-live-var 'stobj-live-var name
                                       (putprop
                                        the-live-var 'symbol-class
                                        :common-lisp-compliant
                                        (putprop
                                         name
                                         'accessor-names
                                         (accessor-array name field-names)
                                         wrld2))))))))))))

; The property 'stobj marks a single-threaded object name.  Its value
; is a non-nil list containing all the names associated with this
; object.  The car of the list is always the live variable name for
; the object.  The cadr of the list (for all stobjs but our STATE) is
; the stobj recognizer for the stobj.  The remaining elements are all
; the functions used in the definition of the recognizer, the
; accessors and the updaters.  We don't list any of the STATE
; functions, just the live name, so the property is non-nil.

; Every supporting function is marked with the property
; 'stobj-function, whose value is the object name.  The live var name
; is marked with 'stobj-live-var, whose value is the object name.

; CHEAT:  I ought, at this point, 
;                 (pprogn
;                  (update-user-stobj-alist
;                   (cons (cons name (create-stobj name template))
;                         (user-stobj-alist state))
;                   state)

; That is, I should add to the user-stobj-alist in state an entry for
; this new stobj, binding its name to its initial value.  But I don't
; want to create the logical counterpart of its initial value -- the
; function create-stobj cannot be used this way (only uses
; resulting from with-local-stobj will pass translate), and we do
; not want to hack our way through the admission of this function
; which is apparently consing a stobj into an alist.  Instead, I rely
; on the live object representing the stobj.  This live object is
; created when the CLTL-COMMAND below is processed by add-trip.
; Add-trip evals the init form in raw lisp to create the live object
; and assign it to global variables.  It also creates array-based
; accessors and updaters.  It then stores this live object in the
; user-stobj-alist of the state just as suggested above, provided this
; is not a redefinition.  (For a redefinition of the stobj, it does a
; put-assoc-eq rather than a cons.)

; The down-side to this cheat is that this only works while
; defstobj-fn is a :program mode function called on the live state,
; where the raw code operates.  If I admitted this function to the
; logic and then called it on the live state, I would get an effect on
; the live state not explained by the code.  Furthermore, if I called
; it on a fake state, I would get a new fake state in which the new
; stobj was not on the user-stobj-alist.

; It will be a while before these discrepancies bother me enough to
; fix.  As long as this is a :program mode function, we won't be able
; to prove that its effect on state is contrary to its semantics as
; expressed here.

                   (install-event name
                                  event-form
                                  'defstobj

; Note: The namex generated below has consists of the single-threaded
; object name, the live variable name, and then the names of all the
; functions introduced.  Big-d-little-d-event knows it can cdr past
; the first two elements of the namex of a defstobj to find the list
; of functions involved.

                                  (list* name the-live-var names)
                                  nil
                                  `(defstobj ,name
                                     ,the-live-var
                                     ,(defstobj-raw-init template)
                                     ,raw-def-lst
                                     ,template
                                     ,ax-def-lst)
                                  t
                                  ctx
                                  wrld3
                                  state)))))))))))))))

(deflabel stobj
  :doc
  ":Doc-Section stobj

  single-threaded objects or ``von Neumann bottlenecks''~/

  In ACL2, a ``single-threaded object'' is a data structure whose use
  is so syntactically restricted that only one instance of the object
  need ever exist and its fields can be updated by destructive
  assignments.

  The documentation in this section is laid out in the form of a tour
  that visits the documented topics in a reasonable order.  We
  recommend that you follow the tour the first time you read about
  stobjs.  The list of all stobj topics is shown below.  The tour
  starts immediately afterwards.~/

  As noted, a ``single-threaded object'' is a data structure whose use
  is so syntactically restricted that only one instance of the object
  need ever exist.  Updates to the object must be sequentialized.
  This allows us to update its fields with destructive assignments
  without wrecking the axiomatic semantics of update-by-copy.  For
  this reason, single-threaded objects are sometimes called ``von
  Neumann bottlenecks.''

  From the logical perspective, a single-threaded object is an
  ordinary ACL2 object, e.g., composed of integers and conses.
  Logically speaking, ordinary ACL2 functions are defined to allow the
  user to ``access'' and ``update'' its fields.  Logically speaking,
  when fields in the object, obj, are ``updated'' with new values, a
  new object, obj', is constructed.

  But suppose that by syntactic means we could ensure that there were
  no more references to the ``old'' object, obj.  Then we could
  create obj' by destructively modifying the memory locations
  involved in the representation of obj.  The syntactic means is
  pretty simple but draconian: the only reference to obj is in
  the variable named ~c[OBJ].

  The consequences of this simple rule are far-reaching and require
  some getting used to.  For example, if ~c[OBJ] has been declared as a
  single-threaded object name, then:

  * ~c[OBJ] is a top-level global variable that contains the current
    object, obj.

  * If a function uses the formal parameter ~c[OBJ], the only 
    ``actual expression'' that can be passed into that slot is the variable
    ~c[OBJ], not merely a term that ``evaluates to an obj'';
    thus, such functions can only operate on the current object.  So for
    example, instead of ~c[(FOO (UPDATE-FIELD1 3 ST))] write
    ~c[(LET ((ST (UPDATE-FIELD1 3 ST))) (FOO ST))].

  * The accessors and updaters have a formal parameter named ~c[OBJ],
    thus, those functions can only be applied to the current object.

  * The ACL2 primitives, such as ~c[CONS], ~c[CAR] and ~c[CDR], may not
    be applied to the variable ~c[OBJ].  Thus, for example, obj may not
    be consed into a list (which would create another pointer to it) or
    accessed or copied via ``unapproved'' means.

  * The updaters return a ``new ~c[OBJ] object'', i.e., obj'; thus, when
    an updater is called, the only variable which can hold its result is
    ~c[OBJ].

  * If a function calls an ~c[OBJ] updater, it must return an ~c[OBJ] object
    (either as the sole value returned, or in ~c[(mv ... OBJ ...)]; ~pl[mv]).

  * When a top-level expression involving ~c[OBJ] returns an ~c[OBJ]
    object, that object becomes the new current value of ~c[OBJ].

  What makes ACL2 different from other functional languages supporting
  such operations (e.g., Haskell's ``monads'' and Clean's ``uniqueness
  type system'') is that ACL2 also gives single-threaded objects an
  explicit axiomatic semantics so that theorems can be proved about
  them.  In particular, the syntactic restrictions noted above are
  enforced only when single-threaded objects are used in function
  definitions (which might be executed outside of the ACL2
  read-eval-print loop in Common Lisp).  The accessor and update
  functions for single-threaded objects may be used without
  restriction in formulas to be proved.  Since function evaluation is
  sometimes necessary during proofs, ACL2 must be able to evaluate
  these functions on logical constants representing the object, even
  when the constant is not ``the current object.''  Thus, ACL2
  supports both the efficient von Neumann semantics and the clean
  applicative semantics, and uses the first in contexts where
  execution speed is paramount and the second during proofs.

  To start the stobj tour, ~pl[stobj-example-1].~/")

(deflabel stobj-example-1
  :doc
  ":Doc-Section stobj

  an example of the use of single-threaded objects~/

  Suppose we want to sweep a tree and (1) count the number of interior
  nodes, (2) count the number of tips and (3) keep a record of every
  tip we encounter that is an integer.  We could use a single-threaded
  object as our ``accumulator''.  Such an object would have three
  fields, one holding the number of nodes seen so far, one holding the
  number of tips, and one holding all the integer tips seen.~/

  The following event declares ~c[counters] to be a single-threaded object.
  ~bv[]
  (defstobj counters
    (NodeCnt     :type integer :initially 0)
    (TipCnt      :type integer :initially 0)
    (IntTipsSeen :type t       :initially nil))
  ~ev[]
  It has three fields, ~c[NodeCnt], ~c[TipCnt], and ~c[IntTipsSeen].
  (As always in ACL2, capitalization is irrelevant in simple symbol
  names, so the first name could be written ~c[nodecnt] or
  ~c[NODECNT], etc.) Those are the name of the accessor functions for
  the object.  The corresponding update functions are named
  ~c[update-NodeCnt], ~c[update-TipCnt] and ~c[update-IntTipsSeen].

  If you do not like the default function names chosen above, there is
  a feature in the ~ilc[defstobj] event that allows you to specify other
  names.

  If you want to see the ACL2 definitions of all the functions defined
  by this event, look at ~il[stobj-example-1-defuns].

  If, after this event, we evaluate the top-level ``global variable''
  ~c[counters] in the ACL2 read-eval-print loop we get:
  ~bv[]
  ACL2 !>counters
  <counters>
  ~ev[]
  Note that the value printed is ``~c[<counters>]''.  Actually, the
  value of ~c[counters] in the logic is ~c[(0 0 NIL)].  But ACL2 always prints
  single-threaded objects in this non-informative way because they are
  usually so big that to do otherwise would be unpleasant.

  Had you tried to evaluate the ``global variable'' ~c[counters] before
  declaring it a single-threaded object, ACL2 would have complained that
  it does not support global variables.  So a lesson here is that
  once you have declared a new single-threaded object your top-level
  forms can reference it.  In versions of ACL2 prior to Version  2.4
  the only variable enjoying this status was ~c[STATE].  single-threaded
  objects are a straightforward generalization of the long-implemented
  von Neumann ~ilc[state] feature of ACL2.

  We can access the fields of ~c[counters] as with:
  ~bv[]
  ACL2 !>(NodeCnt counters)
  0
  ACL2 !>(IntTipsSeen counters)  
  NIL
  ~ev[]
  and we can set the fields of ~c[counters] as with:
  ~bv[]
  ACL2 !>(update-NodeCnt 3 counters)
  <counters>
  ACL2 !>(NodeCnt counters)
  3  
  ~ev[]
  Observe that when we evaluate an expression that returns a
  counter object, that object becomes the ``current value'' of
  ~c[counters].  

  Here is a function that ``converts'' the ~c[counters] object to its
  ``ordinary'' representation:
  ~bv[]
  (defun show-counters (counters)
    (declare (xargs :stobjs (counters)))
    (list (NodeCnt counters)
          (TipCnt counters)
          (IntTipsSeen counters)))
  ~ev[]
  Observe that we ~em[must] declare, at the top of the ~c[defun], that
  we mean to use the formal parameter ~c[counters] as a single-threaded
  object!  If we did not make this declaration, the body of
  ~c[show-counters] would be processed as though ~c[counters] were an
  ordinary object.  An error would be caused because the accessors
  used above cannot be applied to anything but the single-threaded
  object ~c[counters].  If you want to know why we insist on this
  declaration, ~pl[declare-stobjs].

  When ~c[show-counters] is admitted, the following message is printed:
  ~bv[]
  Since SHOW-COUNTERS is non-recursive, its admission is trivial.  We
  observe that the type of SHOW-COUNTERS is described by the theorem
  (AND (CONSP (SHOW-COUNTERS COUNTERS))
       (TRUE-LISTP (SHOW-COUNTERS COUNTERS))).
  We used primitive type reasoning.

  (SHOW-COUNTERS COUNTERS) => *.

  The guard conjecture for SHOW-COUNTERS is trivial to prove.  
  SHOW-COUNTERS is compliant with Common Lisp.
  ~ev[]
  The line above containing the ``=>'' is called the ``signature'' of
  ~c[show-counters]; it conveys the information that the first argument
  is the single-threaded object ~c[counters] and the only result is an
  ordinary object.  Here is an example of another signature:
  ~bv[]
  (PROCESSOR * * COUNTERS) => (MV * COUNTERS)
  ~ev[]
  which indicates that the function ~c[PROCESSOR] (which we haven't
  shown you) takes three arguments, the third of which is the 
  ~c[COUNTERS] stobj, and returns two results, the second of which
  is the modified ~c[COUNTERS].

  Returning to the admission of ~c[show-counters] above, the last
  sentence printed indicates that the ~ilc[guard] conjectures for the
  function were proved.  When some argument of a function is declared
  to be a single-threaded object via the ~c[xargs] ~c[:stobj], we
  automatically add (conjoin) to the guard the condition that the
  argument satisfy the recognizer for that single-threaded object.  In
  the case of ~c[show-counters] the guard is ~c[(countersp counters)].

  Here is an example of ~c[show-counters] being called:
  ~bv[]
  ACL2 !>(show-counters counters)
  (3 0 NIL)
  ~ev[]
  This is what we would see had we set the ~c[NodeCnt] field of the
  initial value of ~c[counters] to ~c[3], as we did earlier in this
  example.

  We next wish to define a function to reset the ~c[counters] object.
  We could define it this way:
  ~bv[]
  (defun reset-counters (counters)
    (declare (xargs :stobjs (counters)))
    (let ((counters (update-NodeCnt 0 counters)))
      (let ((counters (update-TipCnt 0 counters)))
        (update-IntTipsSeen nil counters))))
  ~ev[]
  which ``successively'' sets the ~c[NodeCnt] field to ~c[0], then the
  ~c[TipCnt] field to ~c[0], and then the ~c[IntTipsSeen] field to ~c[nil] and
  returns the resulting object.

  However, the nest of ~c[let] expressions is tedious and we use this
  definition instead.  This definition exploits a macro, here named
  ``~c[seq]'' (for ``sequentially'') which evaluates each of the forms
  given, binding their results successively to the stobj name given.  
  ~bv[]
  (defun reset-counters (counters)
    (declare (xargs :stobjs (counters)))
    (seq counters
         (update-NodeCnt 0 counters)
         (update-TipCnt 0 counters)
         (update-IntTipsSeen nil counters)))
  ~ev[]
  This definition is syntactically identical to the one above, after macro
  expansion.  Our definition of ~c[seq] is shown below and is not part of
  native ACL2.
  ~bv[]
  (defmacro seq (stobj &rest rst)
    (cond ((endp rst) stobj)
          ((endp (cdr rst)) (car rst))
          (t `(let ((,stobj ,(car rst)))
               (seq ,stobj ,@(cdr rst))))))
  ~ev[]

  The signature printed for ~c[reset-counters] is
  ~bv[]
  (RESET-COUNTERS COUNTERS) => COUNTERS.
  ~ev[]

  Here is an example.
  ~bv[]
  ACL2 !>(show-counters counters)
  (3 0 NIL)
  ACL2 !>(reset-counters counters)
  <counters>
  ACL2 !>(show-counters counters)
  (0 0 NIL) 
  ~ev[]

  Here finally is a function that uses ~c[counters] as a single-threaded
  accumulator to collect the desired information about the tree ~c[x].
  ~bv[]
  (defun sweep-tree (x counters)
    (declare (xargs :stobjs (counters)))
    (cond ((atom x)
           (seq counters
                (update-TipCnt (+ 1 (TipCnt counters)) counters)
                (if (integerp x)
                    (update-IntTipsSeen (cons x (IntTipsSeen counters))
                                    counters)
                  counters)))
          (t (seq counters
                  (update-NodeCnt (+ 1 (NodeCnt counters)) counters)
                  (sweep-tree (car x) counters)
                  (sweep-tree (cdr x) counters)))))
  ~ev[]
  We can paraphrase this definition as follows.  If ~c[x] is an atom,
  then increment the ~c[TipCnt] field of ~c[counters] and ~em[then],
  if ~c[x] is an integer, add ~c[x] to the ~c[IntTipsSeen] field, and
  return ~c[counters].  On the other hand, if ~c[x] is not
  an atom, then increment the ~c[NodeCnt] field of ~c[counters], and
  ~em[then] sweep the ~c[car] of ~c[x] and ~em[then] sweep the ~c[cdr]
  of ~c[x] and return the result.

  Here is an example of its execution.  We have displayed the input tree
  in full dot notation so that the number of interior nodes is just the
  number of dots.
  ~bv[]
  ACL2 !>(sweep-tree '((((a . 1) . (2 . b)) . 3)
                       . (4 . (5 . d)))
                     counters)
  <counters>
  ACL2 !>(show-counters counters)
  (7 8 (5 4 3 2 1))
  ACL2 !>(reset-counters counters)
  <counters>
  ACL2 !>(show-counters counters)
  (0 0 NIL)
  ~ev[]

  The ~c[counters] object has two integer fields and a field whose
  type is unrestricted.  single-threaded objects support other types of
  fields, such as arrays.  We deal with that in the ~il[stobj-example-2].
  But we recommend that you first consider the implementation issues for
  the ~c[counters] example (in ~il[stobj-example-1-implementation]) and
  then consider the proof issues (in ~il[stobj-example-1-proofs]).

  To continue the stobj tour, ~pl[stobj-example-2].~/")

(deflabel declare-stobjs
  :doc
  ":Doc-Section stobj

  declaring a formal parameter name to be a single-threaded object~/

  When a ~ilc[defun] uses one of its formals as a single-threaded object
  (~il[stobj]), the ~c[defun] ~em[must] include a declaration that the
  formal is to be so used.  An exception is the formal ``~ilc[state],'' which
  if not declared as explained below, may still be used provided an
  appropriate global ``declaration'' is issued:
  ~pl[set-state-ok].~/

  If the formal in question is ~c[counters] then an appropriate declaration
  is
  ~bv[]
  (declare (xargs :stobjs counters))
  ~ev[]
  or, more generally,
  ~bv[]
  (declare (xargs :stobjs (... counters ...)))
  ~ev[]
  where all the single-threaded formals are listed.

  For such a declaration to be legal it must be the case that all the names
  have previously been defined as single-threaded objects with ~ilc[defstobj].

  When an argument is declared to be single-threaded the guard of the
  function is augmented by conjoining to it the condition that the
  argument satisfy the recognizer for the single-threaded object.
  Furthermore, the syntactic checks done to enforce the legal use of
  single-threaded objects are also sufficient to allow these guard
  conjuncts to be automatically proved.

  The obvious question arises:  Why does ACL2 insist that you declare
  stobj names before using them in ~c[defun]s if you can only declare names
  that have already been defined with ~c[defstobj]?  What would go wrong if
  a formal were treated as a single-threaded object if and only if it had
  already been so defined?

  Suppose that one user, say Jones, creates a book in which ~c[counters]
  is defined as a single-threaded object.  Suppose another user, Smith,
  creates a book in which ~c[counters] is used as an ordinary formal
  parameter.  Finally, suppose a third user, Brown, wishes to use both
  books.  If Brown includes Jones' book first and then Smith's, then
  Smith's function treats ~c[counters] as single-threaded.  But if Brown
  includes Smith's book first, the argument is treated as ordinary.

  ACL2 insists on the declaration to ensure that the definition is
  processed the same way no matter what the context.~/")

(deflabel stobj-example-1-defuns
  :doc
  ":Doc-Section stobj

  the defuns created by the ~c[counters] stobj~/
  
  Consider the event shown in ~il[stobj-example-1]:
  ~bv[]
  (defstobj counters
    (NodeCnt     :type integer :initially 0)
    (TipCnt      :type integer :initially 0)
    (IntTipsSeen :type t       :initially nil))
  ~ev[]

  Here is a complete list of the defuns added by the event.~/

  The careful reader will note that the ~c[counters] argument below is
  ~em[not] declared with the ~c[:stobjs] ~c[xarg] even though we
  insist that the argument be a stobj in calls of these functions.
  This ``mystery'' is explained below.

  ~bv[]
  (defun NodeCntp (x)                 ;;; Recognizer for 1st field
    (declare (xargs :guard t :verify-guards t))
    (integerp x))

  (defun TipCntp (x)                  ;;; Recognizer for 2nd field
    (declare (xargs :guard t :verify-guards t))
    (integerp x))

  (defun IntTipsSeenp (x)             ;;; Recognizer for 3rd field
    (declare (xargs :guard t :verify-guards t) (ignore x))
    t)

  (defun countersp (counters)         ;;; Recognizer for object
    (declare (xargs :guard t :verify-guards t))
    (and (true-listp counters)
         (= (length counters) 3)
         (NodeCntp (nth 0 counters))
         (TipCntp (nth 1 counters))
         (IntTipsSeenp (nth 2 counters))
         t))

  (defun create-counters ()           ;;; Creator for object
    (declare (xargs :guard t :verify-guards t))
    (list '0 '0 'nil))

  (defun NodeCnt (counters)           ;;; Accessor for 1st field
    (declare (xargs :guard (countersp counters) :verify-guards t))
    (nth 0 counters))

  (defun update-NodeCnt (v counters)  ;;; Updater for 1st field
    (declare (xargs :guard
                    (and (integerp v)
                         (countersp counters))
                    :verify-guards t))
    (update-nth 0 v counters))

  (defun TipCnt (counters)            ;;; Accessor for 2nd field
    (declare (xargs :guard (countersp counters) :verify-guards t))
    (nth 1 counters))

  (defun update-TipCnt (v counters)   ;;; Updater for 2nd field
    (declare (xargs :guard
                    (and (integerp v)
                         (countersp counters))
                    :verify-guards t))
    (update-nth 1 v counters))

  (defun IntTipsSeen (counters)       ;;; Accessor for 3rd field
    (declare (xargs :guard (countersp counters) :verify-guards t))
    (nth 2 counters))

  (defun update-IntTipsSeen (v counters) ;;; Updater for 3rd field
    (declare (xargs :guard (countersp counters) :verify-guards t))
    (update-nth 2 v counters))
  ~ev[]

  Observe that there is a recognizer for each of the three fields and
  then a recognizer for the ~c[counters] object itself.  Then, for each
  field, there is an accessor and an updater.

  Observe also that the functions are guarded so that they expect a
  ~c[countersp] for their ~c[counters] argument and an appropriate value
  for the new field values.

  You can see all of the ~c[defuns] added by a ~c[defstobj] event by
  executing the event and then using the ~c[:pcb!] command on the stobj
  name.  E.g.,
  ~bv[]
  ACL2 !>:pcb! counters
  ~ev[]
  will print the defuns above.

  We now clear up the ``mystery'' mentioned above.  Note, for example
  in ~c[TipCnt], that the formal ~c[counters] is used.  From the
  discussion in ~il[stobj-example-1] it has been made clear that
  ~c[TipCnt] can only be called on the ~c[counters] object.  And yet,
  in that same discussion it was said that an argument is so treated
  only if it it declared among the ~c[:stobjs] in the definition of
  the function.  So why doesn't ~c[TipCnt] include something like
  ~c[(declare (xargs :stobjs (counters)))]?

  The explanation of this mystery is as follows.  At the time
  ~c[TipCnt] was defined, during the introduction of the ~c[counters]
  stobj, the name ``~c[counters]'' was not yet a single-threaded
  object.  The introduction of a new single-threaded object occurs in
  three steps: (1) The new primitive recognizers, accessors, and
  updaters are introduced as ``ordinary functions,'' producing their
  logical axiomatizations.  (2) The executable counterparts are
  defined in raw Lisp to support destructive updating.  (3) The new
  name is declared a single-threaded object to ensure that all future
  use of these primitives respects the single-threadedness of the
  object.  The functions defined as part of the introduction of a new
  single-threaded object are the only functions in the system that
  have undeclared stobj formals other than ~c[state].

  You may return to ~il[stobj-example-1] here.~/")

(deflabel stobj-example-1-implementation
  :doc
  ":doc-section stobj

  the implementation of the ~c[counters] stobj~/

  the event  
  ~bv[]
  (defstobj counters
    (NodeCnt     :type integer :initially 0)
    (TipCnt      :type integer :initially 0)
    (IntTipsSeen :type t       :initially nil))
  ~ev[]
  discussed in ~il[stobj-example-1], creates a Common Lisp object to
  represent the current value of ~c[counters].  That object is created
  by evaluating either of the following ``raw'' (non-ACL2) Common Lisp
  forms:
  ~bv[]
  (create-counters)

  (vector (make-array 1 :element-type 'integer
                        :initial-element '0)
          (make-array 1 :element-type 'integer
                        :initial-element '0)
          'nil)
  ~ev[]
  and the value is stored in the Common Lisp global variable named
  ~c[*the-live-counters*].
  ~/

  Thus, the ~c[counters] object is an array of length three.  The first
  two elements are arrays of size 1 and are used to hold the
  ~c[NodeCnt] and ~c[TipCnt] fields.  The third element is the
  ~c[IntTipsSeen] field.  The first two fields are represented by
  arrays so that we can implement the ~c[integer] type specification
  efficiently.  Generally, integers are ``boxed'' in some Common Lisp
  implementations, for example, GCL.  Creating a new integer requires
  creating a new box to put it in.  But in some lisps, including GCL,
  the integers inside arrays of integers are not boxed.

  The function ~c[NodeCnt] is defined in raw Lisp as:
  ~bv[]
  (defun NodeCnt (counters)
    (the integer
         (aref (the (simple-array integer (1))
                    (svref counters 0))
               0)))
  ~ev[]
  Observe that the form ~c[(svref counters 0)] is evaluated to get
  an array of size 1, which is followed by a call of ~c[aref] to
  access the 0th element of that array.

  The function ~c[update-NodeCnt] is defined in raw Lisp as:
  ~bv[]
  (defun update-NodeCnt (v counters)
    (declare (type integer v))
    (progn
     (setf (aref (the (simple-array integer (1))
                      (svref counters 0))
                 0)
           (the integer v))
     counters))
  ~ev[]  
  Note that when this function is called, it does not create a new
  vector of length three, but ``smashes'' the existing one.

  One way to see all the raw Lisp functions defined by a given ~c[defstobj] is
  to evaluate the ~c[defstobj] event and then evaluate, in the ACL2 loop, the
  expression ~c[(nth 4 (global-val 'cltl-command (w state)))].  Those functions
  that contain ~c[(DECLARE (STOBJ-INLINE-FN T))] will generate ~ilc[defabbrev]
  forms because the ~c[:inline] keyword of ~ilc[defstobj] was supplied the
  value ~c[t].  The rest will generate ~ilc[defun]s.

  We now recommend that you look at ~il[stobj-example-1-proofs].~/")

(deflabel stobj-example-1-proofs
  :doc
  ":Doc-Section stobj

  some proofs involving the ~c[counters] stobj~/

  Consider again the event  
  ~bv[]
  (defstobj counters
    (NodeCnt     :type integer :initially 0)
    (TipCnt      :type integer :initially 0)
    (IntTipsSeen :type t       :initially nil))
  ~ev[]
  discussed in ~il[stobj-example-1], followed by the definition
  ~bv[]
  (defun reset-counters (counters)
    (declare (xargs :stobjs (counters)))
    (seq counters
         (update-NodeCnt 0 counters)
         (update-TipCnt 0 counters)
         (update-IntTipsSeen nil counters)))
  ~ev[]
  which, because of the ~c[seq] macro in ~il[stobj-example-1], is just
  syntactic sugar for
  ~bv[]
  (defun reset-counters (counters)
    (declare (xargs :stobjs (counters)))
    (let ((counters (update-NodeCnt 0 counters)))
      (let ((counters (update-TipCnt 0 counters)))
        (update-IntTipsSeen nil counters)))).
  ~ev[]

  Here is a simple theorem about ~c[reset-counters].

  ~bv[]
  (defthm reset-counters-is-constant
    (implies (countersp x)
             (equal (reset-counters x)
                    '(0 0 nil))))
  ~ev[]
  ~/
  Before we talk about how to prove this theorem, note that the theorem
  is unusual in two respects.

  First, it calls ~c[reset-counters] on an argument other than the
  variable ~c[counters]!  That is allowed in theorems; logically
  speaking, the stobj functions are indistinguishable from ordinary
  functions.  Their use is syntactically restricted only in
  ~c[defun]s, which might be compiled and run in raw Lisp.  Those
  restrictions allow us to implement stobj modification destructively.
  But logically speaking, ~c[reset-counters] and other stobj
  ``modifying'' functions just create new objects, constructively.

  Second, the theorem above explicitly provides the hypothesis that
  ~c[reset-counters] is being applied to an object satisfying
  ~c[countersp].  Such a hypothesis is not always required:
  ~c[reset-counters] is total and will do something no matter what
  ~c[x] is.  But in this particular case, the result is not ~c['(0 0 nil)]
  unless ~c[x] is, at least, a true-list of length three.

  To make a long story short, to prove theorems about stobj functions you
  behave in exactly the way you would to prove the same theorems about the
  same functions defined without the stobj features.

  How can we prove the above theorem?  Unfolding the definition of
  ~c[reset-counters] shows that ~c[(reset-counters x)] is equal to
  ~bv[]
  (update-IntTipsSeen nil
    (update-TipCnt 0 
      (update-NodeCnt 0 x)))
  ~ev[]
  which in turn is
  ~bv[]
  (update-nth 2 nil
   (update-nth 1 0
    (update-nth 0 0 x))).
  ~ev[]
  Opening up the definition of ~c[update-nth] reduces this to
  ~bv[]
  (list* 0 0 nil (cdddr x)).
  ~ev[]
  This is clearly equal to ~c['(0 0 nil)], provided we know that ~c[(cdddr x)]
  is ~c[nil].

  Unfortunately, that last fact requires a lemma.  The most specific lemma we
  could provide is
  ~bv[]
  (defthm special-lemma-for-counters
    (implies (countersp x)
             (equal (cdddr x) nil)))
  ~ev[]
  but if you try to prove that lemma you will find that it requires some
  reasoning about ~c[len] and ~c[true-listp].  Furthermore, the special
  lemma above is of interest only for ~c[counters].

  The following lemma about ~c[len] is the one we prefer.
  ~bv[]
  (defthm equal-len-n
    (implies (syntaxp (quotep n))
             (equal (equal (len x) n)
                    (if (integerp n)
                        (if (< n 0)
                            nil
                          (if (equal n 0)
                              (atom x)
                            (and (consp x)
                                 (equal (len (cdr x)) (- n 1)))))
                      nil))))
  ~ev[]
  This lemma will simplify any equality in which a ~c[len] expression
  is equated to any explicitly given constant ~em[n], e.g.,
  ~c[3], reducing the equation to a conjunction of ~c[consp] terms
  about the first ~em[n] ~c[cdr]s.

  If the above lemma is available then ACL2 immediately proves
  ~bv[]
  (defthm reset-counters-is-constant
    (implies (countersp x)
             (equal (reset-counters x)
                    '(0 0 nil))))
  ~ev[]

  The point is presumably well made: proving theorems about
  single-threaded object accessors and updaters is no different than
  proving theorems about other recursively defined functions on lists.

  As we have seen, operations on ~il[stobj]s turn into definitions
  involving ~ilc[nth] and ~ilc[update-nth] in the logic.  Here are two lemmas
  that are useful for simplifying terms involving ~c[nth] and ~c[update-nth],
  which are therefore useful in reasoning about single-threaded objects.
  ~bv[]
  (defthm update-nth-update-nth-same
    (implies (equal (nfix i1) (nfix i2))
             (equal (update-nth i1 v1 (update-nth i2 v2 l))
                    (update-nth i1 v1 l))))

  (defthm update-nth-update-nth-diff
    (implies (not (equal (nfix i1) (nfix i2)))
             (equal (update-nth i1 v1 (update-nth i2 v2 l))
                    (update-nth i2 v2 (update-nth i1 v1 l))))
    :rule-classes ((:rewrite :loop-stopper ((i1 i2)))))
  ~ev[]
  These lemmas are due to Matt Wilding.  ~l[nu-rewriter] for a
  discussion of the efficient simplification of terms of the form
  ~c[(nth n (update-nth key val lst))], which can be critical in
  settings involving sequential bindings that commonly arise in
  operations involving stobjs.

  We now recommend that you ~pl[stobj-example-2].~/")

(deflabel stobj-example-2
  :doc
  ":doc-section stobj

  an example of the use of arrays in single-threaded objects~/
  
  The following event
  ~bv[]
  (defstobj ms
    (pcn  :type integer                  :initially 0)
    (mem  :type (array integer (100000)) :initially -1)
    (code :type t                        :initially nil))
  ~ev[]
  introduces a single-threaded object named ~c[ms] (which stands for
  ``machine state'').  The object has three fields, a ~c[pcn] or program
  counter, a ~c[mem] or memory, and a ~c[code] field.

  The ~c[mem] field is occupied by an object initially of type 
  ~c[(array integer (100000))].  Logically speaking, this is a list of
  length ~c[100000], each element of which is an integer.  But in the
  underlying implementation of the ~c[ms] object, this field is occupied
  by a raw Lisp array, initially of size 100000.~/

  You might expect the above ~c[defstobj] to define the accessor function
  ~c[mem] and the updater ~c[update-mem].  ~em[That does not happen!].

  The above event defines the accessor function ~c[memi] and the updater
  ~c[update-memi].  These functions do not access/update the ~c[mem] field of
  the ~c[ms] object; they access/update the individual elements of the
  array in that field.

  In particular, the logical definitions of the two functions are:
  ~bv[]
  (defun memi (i ms)
    (declare (xargs :guard
                    (and (msp ms)
                         (integerp i)
                         (<= 0 i)
                         (< i (mem-length ms)))))
    (nth i (nth 1 ms)))

  (defun update-memi (i v ms)
    (declare (xargs :guard
                    (and (msp ms)
                         (integerp i)
                         (<= 0 i)
                         (< i (mem-length ms))
                         (integerp v))))
    (update-nth-array 1 i v ms))
  ~ev[]

  For example, to access the 511th (0-based) memory location of the
  current ~c[ms] you could evaluate:
  ~bv[]
  ACL2 !>(memi 511 ms)
  -1
  ~ev[]
  The answer is ~c[-1] initially, because that is the above-specified
  initial value of the elements of the ~c[mem] array.

  To set that element you could do
  ~bv[]
  ACL2 !>(update-memi 511 777 ms)
  <ms>
  ACL2 !>(memi 511 ms)
  777
  ~ev[]

  The raw Lisp implementing these two functions is shown below.
  ~bv[]
  (defun memi (i ms)
    (declare (type (integer 0 268435455) i))
    (the integer
         (aref (the (simple-array integer (*))
                    (svref ms 1))
               (the (integer 0 268435455) i))))

  (defun update-memi (i v ms)
    (declare (type (integer 0 268435455) i)
             (type integer v))
    (progn
     (setf (aref (the (simple-array integer (*))
                      (svref ms 1))
                 (the (integer 0 268435455) i))
           (the integer v))
     ms))
  ~ev[]

  If you want to see the raw Lisp supporting a ~c[defstobj], execute the
  ~c[defstobj] and then evaluate the ACL2 form
  ~c[(nth 4 (global-val 'cltl-command (w state)))].

  To continue the stobj tour, ~pl[stobj-example-3].~/")

(deflabel stobj-example-3
  :doc
  ":Doc-Section stobj

  another example of a single-threaded object~/

  The event
  ~bv[]
  (defstobj $s
    (x :type integer :initially 0)
    (a :type (array (integer 0 9) (3)) :initially 9 :resizable t))
  ~ev[]
  introduces a stobj named ~c[$S].  The stobj has two fields, ~c[X] and
  ~c[A].  The ~c[A] field is an array.  The ~c[X] field contains an
  integer and is initially 0.  The ~c[A] field contains a list of
  integers, each between 0 and 9, inclusively.  (Under the hood, this
  ``list'' is actually implemented as an array.)  Initially, the ~c[A]
  field has three elements, each of which is 9.~/

  This event introduces the following sequence of function definitions:
  ~bv[]
  (DEFUN XP (X) ...)               ; recognizer for X field
  (DEFUN AP (X) ...)               ; recognizer of A field
  (DEFUN $SP ($S) ...)             ; top-level recognizer for stobj $S
  (DEFUN CREATE-$S NIL ...)        ; creator for stobj $S
  (DEFUN X ($S) ...)               ; accessor for X field
  (DEFUN UPDATE-X (V $S) ...)      ; updater for X field
  (DEFUN A-LENGTH ($S) ...)        ; length of A field
  (DEFUN RESIZE-A (K $S) ...)      ; resizer for A field
  (DEFUN AI (I $S) ...)            ; accessor for A field at index I
  (DEFUN UPDATE-AI (I V $S) ...)   ; updater for A field at index I
  ~ev[]

  Here is the definition of ~c[$SP]:
  ~bv[]
  (DEFUN $SP ($S)
    (DECLARE (XARGS :GUARD T :VERIFY-GUARDS T))
    (AND (TRUE-LISTP $S)
         (= (LENGTH $S) 2)
         (XP (NTH 0 $S))
         (AP (NTH 1 $S))
         T))
  ~ev[]
  This reveals that in order to satisfy ~c[$SP] an object must be
  a true list of length 2 whose first element satisfies ~c[XP] and whose
  second satisfies ~c[AP].  By printing the definition of ~c[AP] one
  learns that it requires its argument to be a true list, each element
  of which is an integer between 0 and 9.

  The initial value of stobj ~c[$S] is given by zero-ary ``creator''
  function ~c[CREATE-$S].  Creator functions may only be used in limited
  contexts.  ~l[with-local-stobj].

  Here is the definition of ~c[UPDATE-AI], the updater for the ~c[A] field
  at index ~c[I]:
  ~bv[]
  (DEFUN UPDATE-AI (I V $S)
    (DECLARE (XARGS :GUARD
                    (AND ($SP $S)
                         (INTEGERP I)
                         (<= 0 I)
                         (< I (A-LENGTH $S))
                         (AND (INTEGERP V) (<= 0 V) (<= V 9)))
                    :VERIFY-GUARDS T))
    (UPDATE-NTH-ARRAY 1 I V $S))
  ~ev[]
  By definition ~c[(UPDATE-NTH-ARRAY 1 I V $S)] is
  ~c[(UPDATE-NTH 1 (UPDATE-NTH I V (NTH 1 $S)) $S)].
  This may be a little surprising but should be perfectly clear.

  First, ignore the guard, since it is irrelevant in the logic.
  Reading from the inside out, ~c[(UPDATE-AI I V $S)] extracts ~c[(NTH 1 $S)],
  which is array ~c[a] of ~c[$S].  (Recall that ~ilc[NTH] is
  0-based.)  The next higher expression in the definition above,
  ~c[(UPDATE-NTH I V a)], ``modifies'' ~c[a] by setting its ~c[I]th
  element to ~c[V].  Call this ~c[a'].  The next higher expression,
  ~c[(UPDATE-NTH 1 a' $S)], ``modifies'' ~c[$S] by setting its 1st
  component to ~c[a'].  Call this result ~c[$s'].  Then ~c[$s'] is
  the result returned by ~c[UPDATE-AI].

  So the first useful observation is that from the perspective of the
  logic, the type ``restrictions'' on stobjs are irrelevant.  They
  are ``enforced'' by ACL2's guard mechanism, not by the definitions
  of the updater functions.

  As one might also imagine, the accessor functions do not really
  ``care,'' logically, whether they are applied to well-formed stobjs
  or not.  For example, ~c[(AI I $S)] is defined to be ~c[(NTH I (NTH 1 $S))].

  Thus, you will not be able to prove that (AI 2 $S) is an
  integer.  That is,
  ~bv[]
  (integerp (AI 2 $S))
  ~ev[]
  is not a theorem, because ~c[$S] may not be well-formed.

  Now ~c[(integerp (AI 2 $S))] will always evaluate to ~c[T] in the
  top-level ACL2 command loop, because we insist that the current value of
  the stobj ~c[$S] always satisfies ~c[$SP] by enforcing the guards on
  the updaters, independent of whether guard checking is on or off;
  ~pl[set-guard-checking].  But in a theorem ~c[$S] is just
  another variable, implicitly universally quantified.

  So ~c[(integerp (AI 2 $S))] is not a theorem because it is not true when
  the variable ~c[$S] is instantiated with, say,
  ~bv[]
  '(1 (0 1 TWO))
  ~ev[]
  because, logically speaking, ~c[(AI 2 '(1 (0 1 TWO)))] evaluates to
  the symbol ~c[TWO].  That is,
  ~bv[]
  (equal (AI 2 '(1 (0 1 TWO))) 'TWO)
  ~ev[]
  is true.

  However,
  ~bv[]
  (implies (and ($SP $S) (< 2 (A-LENGTH $S))) (integerp (AI 2 $S)))
  ~ev[]
  is a theorem.  To prove it, you will have to prove a lemma about
  ~c[AP].  The following will do:
  ~bv[]
  (defthm ap-nth
    (implies (and (AP x)
                  (integerp i)
                  (<= 0 i)
                  (< i (len x)))
             (integerp (nth i x)))).
  ~ev[]

  Similarly, 
  ~bv[]
  (implies (and (integerp i)
                (<= 0 i)
                (< i (A-LENGTH $S))
                (integerp v)
                (<= 0 v)
                (<= v 9))
           ($SP (UPDATE-AI i v $S)))
  ~ev[]
  is not a theorem until you add the additional hypothesis ~c[($SP $S)].
  To prove the resulting theorem, you will need a lemma such as the
  following.
  ~bv[]
  (defthm ap-update-nth
    (implies (and (AP a)
                  (integerp v)
                  (<= 0 v)
                  (<= v 9)
                  (integerp i)
                  (<= 0 i)
                  (< i (len a)))
             (AP (update-nth i v a))))
  ~ev[]

  The moral here is that from the logical perspective, you must
  provide the hypotheses that, as a programmer, you think are
  implicit on the structure of your stobjs, and you must prove their
  invariance.  This is a good area for further support, perhaps in
  the form of a library of macros.

  ~em[Resizing Array Fields]

  Recall the specification of the array field, ~c[A] for the stobj ~c[$S]
  introduced above:
  ~bv[]
  (a :type (array (integer 0 9) (3)) :initially 9 :resizable t)
  ~ev[]
  Logically, this field is a list, initially of length 3.  Under the
  hood, this field is implemented using a Common Lisp array with 3
  elements.  In some applications, one may wish to lengthen an array
  field, or even (to reclaim space) to shrink an array field.  The
  ~ilc[defstobj] event provides functions to access the current length
  of an array field and to change the array field, with default names
  obtained by suffixing the field name with ``~c[LENGTH-]'' or prefixing
  it with ``~c[RESIZE-],'' respectively.  The following log shows the uses
  of these fields in the above example.
  ~bv[]
  ACL2 !>(A-LENGTH $S)
  3
  ACL2 !>(RESIZE-A 10 $S) ; change length of A to 10
  <$s>
  ACL2 !>(A-LENGTH $S)
  10
  ACL2 !>(AI 7 $S)        ; new elements get value from :initially
  9
  ACL2 !>(RESIZE-A 2 $S)  ; truncate A down to first 2 elements
  <$s>
  ACL2 !>(A-LENGTH $S)
  2
  ACL2 !>(AI 7 $S)        ; error:  access past array bound


  ACL2 Error in TOP-LEVEL:  The guard for the function symbol AI, which
  is (AND ($SP $S) (INTEGERP I) (<= 0 I) (< I (A-LENGTH $S))), is violated
  by the arguments in the call (AI 7 $S).

  ACL2 !>
  ~ev[]
  Here are the definitions of the relevant functions for the above
  example; also ~pl[resize-list].
  ~bv[]
  (DEFUN A-LENGTH ($S)
    (DECLARE (XARGS :GUARD ($SP $S) :VERIFY-GUARDS T))
    (LEN (NTH 1 $S)))

  (DEFUN RESIZE-A (K $S)
    (DECLARE (XARGS :GUARD ($SP $S) :VERIFY-GUARDS T))
    (UPDATE-NTH 1
                (RESIZE-LIST (NTH 1 $S) K 9)
                $S))
  ~ev[]

  It is important to note that the implementation of array resizing in
  ACL2 involves copying the entire array into a newly allocated space
  and thus can be quite costly if performed often.  This approach was
  chosen in order to make array access and update as efficient as
  possible, with the suspicion that for most applications, array
  access and update are considerably more frequent than resizing
  (especially if the programmer is aware of the relative costs
  beforehand).

  It should also be noted that computations of lengths of stobj array
  fields should be fast (constant-time) in all or most Common Lisp
  implementations.

  Finally, if ~c[:resizable t] is not supplied as shown above, then
  an attempt to resize the array will result in an error.  If you do
  not intend to resize the array, it is better to omit the ~c[:resizable]
  option (or to supply ~c[:resizable nil]), since then the length
  function will be defined to return a constant, namely the initial
  length, which can simplify guard proofs (compare with the definition
  of ~c[A-LENGTH] above).

  This completes the tour through the documentation of ~il[stobj]s.
  However, you may now wish to read the documentation for the event
  that introduces a new single-threaded object; ~pl[defstobj].~/")

(defdoc resize-list
  ":Doc-Section Stobj

  list resizer in support of stobjs~/

  ~c[(Resize-list lst n default-value)] takes a list, ~c[lst], and a desired
  length, ~c[n], for the result list, as well as a ~c[default-value] to use
  for the extra elements if ~c[n] is greater than the length of ~c[lst].~/

  ~c[Resize-list] has a guard of ~c[t].  This function is called in the body
  of function, ~c[resize-<a>] where ~c[<a>] is an array field of a ~il[stobj].
  ~l[stobj] and ~pl[defstobj].~/")

#-acl2-loop-only
(defun-one-output mv-let-for-with-local-stobj (mv-let-form st creator w)

; If w is not nil, then it is the current ACL2 world and we are to oneify the
; appropriate subforms.

; It was tempting to have an acl2-loop-only version of the body below as well,
; which would omit the binding of the live var.  But if someone were to
; verify-termination of this function, we could presumably prove nil using the
; discrepancy between the two versions.  So we take the attitude that
; with-local-stobj is a special form, like let, that is not defined.

  (let ((producer (caddr mv-let-form))
        (rest (cdddr mv-let-form)))
    `(mv-let ,(cadr mv-let-form)
             (let ((,st (,creator)))

; We bind the live var so that user-stobj-alist-safe can catch misguided
; attempts to use functions like trans-eval in inappropriate contexts.

               (let ((,(the-live-var st) ,st))
                 ,(if w (oneify producer w) producer)))
             (declare (ignore ,st))
             ,@(if w
                   (if (cdr rest) ; rest is ((declare (ignore ...)) body)
                       (list (car rest) (oneify (cadr rest) w))
                     (list (oneify (car rest) w)))
                 rest))))

#-acl2-loop-only ; see the comment in mv-let-for-with-local-stobj
(defmacro with-local-stobj (&rest args)

; Below are some tests of local stobjs.
#|
 (defstobj foo bar xxx)

 (thm (equal (create-foo) '(nil nil))) ; succeeds

 (defun up1 (x foo)
   (declare (xargs :stobjs foo))
   (update-bar x foo))

 (bar foo) ; nil

 (up1 3 foo) ; <foo>

 (bar foo) ; 3

 (defun test (x) ; should fail; must use with-local-stobj explicitly
   (mv-let (a b foo)
           (let ((foo (create-foo)))
             (let ((foo (up1 (1+ x) foo)))
               (mv (bar foo) (xxx foo) foo)))
           (declare (ignore foo))
           (mv a b x)))

 (defun test (x)
   (declare (xargs :guard (acl2-numberp x) :verify-guards nil))
   (with-local-stobj
    foo
    (mv-let (a b foo)
            (let ((foo (up1 (1+ x) foo)))
              (mv (bar foo) (xxx foo) foo))
            (mv a b x))))

 (test 17) ; (18 NIL 17)

 (bar foo) ; 3

 (thm (equal (test x) (list (1+ x) nil x))) ; succeeds

 (thm (equal (test x) (list (1+ x) nil x)) ; succeeds
      :hints (("Goal"
               :in-theory
               (enable
                (:executable-counterpart create-foo)))))

 (thm (equal (test x) (list (1+ x) nil x)) ; fails, creating (NOT (NTH 1 (HIDE (CREATE-FOO))))
      :hints (("Goal"
               :in-theory
               (set-difference-theories
                (enable
                 (:executable-counterpart create-foo))
                '(create-foo)))))

 (verify-guards test)

 (test 17) ; (18 nil 17)

 (bar foo) ; 3

 (defun test2 (x)
   (with-local-stobj
    foo
    (mv-let (a foo)
            (let ((foo (up1 (1+ x) foo))) (mv (bar foo) foo))
            (mv a x))))

 (test2 12) ; (13 12)

 (bar foo) ; 3

 (thm (equal (test x) (mv-let (x y) (test2 x) (mv x nil y)))) ; succeeds

 (create-foo) ; should get graceful error

 (defun test3 (x) ; Should be OK.
   (with-local-stobj
    foo
    (mv-let (a foo)
            (let ((foo (up1 (1+ x) foo))) (mv (bar foo) foo))
            a)))

 (test3 11) ; 12

 (bar foo) ; 3

 (defun test4 (x foo) ; Should be OK.
   (declare (xargs :stobjs foo
                   :verify-guards nil))
   (let* ((x+1
          (with-local-stobj
           foo
           (mv-let (a foo)
                   (let ((foo (up1 (1+ x) foo))) (mv (bar foo) foo))
                   a)))
          (foo (up1 92 foo)))
     (mv x+1 foo)))

 (test4 19 foo) ; (20 <foo>)

 (bar foo) ; 92

 (defun test5 (x foo) ; Should be OK.
   (declare (xargs :stobjs foo
                   :verify-guards nil))
   (let* ((foo (up1 23 foo))
          (x+1
           (with-local-stobj
            foo
            (mv-let (a foo)
                    (let ((foo (up1 (1+ x) foo))) (mv (bar foo) foo))
                    a))))
     (mv x+1 foo)))

 (test5 35 foo) ; (36 <foo>)

 (bar foo) ; 23

 (with-local-stobj ; should get macroexpansion error or the equivalent
  foo
  (mv foo 3))

 (defun trans-eval-test (x foo state) ; this part is ok
   (declare (xargs :stobjs (foo state)
                   :mode :program))
   (mv-let (erp val state)
           (trans-eval '(update-bar (cons 3 (bar foo)) foo) 'top state)
           (declare (ignore erp val))
           (mv x foo state)))

 (with-local-stobj ; should fail; cannot use with-local-stobj in top level loop
  foo
  (mv-let (x foo state)
          (trans-eval-test 3 foo state)
          (mv x state)))

 (pprogn
  (with-local-stobj ; should fail with create-foo error
   foo
   (mv-let (x foo state)
           (trans-eval-test 3 foo state)
           (declare (ignore x))
           state))
  (mv 3 state))

 (defun test6 (a state)
   (declare (xargs :mode :program :stobjs state))
   (with-local-stobj
    foo
    (mv-let (x foo state)
            (trans-eval-test a foo state)
            (mv x state))))

 (test6 100 state) ; should get trans-eval error:  user-stobj-alist mismatch

 (bar foo) ; 23, still -- trans-eval did not affect global state

|#

; Below are some more tests, contributed by Rob Sumners.

#|

 (defstobj foo foo-fld)
 (defstobj bar bar-fld)

 (defun test-wls1 (x)
   (with-local-stobj
    foo
    (mv-let (result foo)
            (let ((foo (update-foo-fld 2 foo)))
              (mv (with-local-stobj
                   bar
                   (mv-let (result bar)
                           (let ((bar (update-bar-fld 3 bar)))
                             (mv x bar))
                           result))
                  foo))
            result)))

 (test-wls1 129) ; 129

 :comp t

 (test-wls1 '(adjka 202)) ; '(ADJKA 202)

 (thm (equal (test-wls1 x) x))

 (defun test-wls2 (x)
   (with-local-stobj
    foo
    (mv-let (result foo)
            (let ((foo (update-foo-fld 2 foo)))
              (mv (with-local-stobj
                   foo
                   (mv-let (result foo)
                           (let ((foo (update-foo-fld 3 foo)))
                             (mv x foo))
                           result))
                  foo))
            result)))

 (test-wls2 129) ; 129

 :comp t

 (test-wls2 '(adjka 202)) ; (ADJKA 202)

 (thm (equal (test-wls2 x) x))

 (defun test-wls3 (x)
   (if (atom x) x
     (with-local-stobj
      foo
      (mv-let (result foo)
              (mv (cons (car x)
                        (test-wls3 (cdr x)))
                  foo)
              (let ((x result))
                (if (atom x) x (cons (car x) (cdr x))))))))

 (test-wls3 129) ; 129

 :comp t

 (test-wls3 '(adjka 202)) ; (ADJKA 202)

 (thm (equal (test-wls3 x) x))

|#

  (mv-let (erp st mv-let-form creator)
          (parse-with-local-stobj args)
          (if (or erp
                  (not (and (true-listp mv-let-form)
                            (<= 3 (length mv-let-form)))))
              (er hard 'with-local-stobj
                  "Macroexpansion of a with-local-stobj call caused an error. ~
                   See :DOC with-local-stobj.")
            (mv-let-for-with-local-stobj mv-let-form st creator nil))))

(defdoc with-local-stobj
  ":Doc-Section Stobj

  locally bind a single-threaded object~/

  ~l[stobj] for an introduction to single-threaded objects.
  ~bv[]
  Example Form:
  (with-local-stobj
   st
   (mv-let (result st)
           (compute-with-st x st)
           result))
  ~ev[]
  ~c[With-local-stobj] can be thought of as a macro, where the example
  form above expands as follows.
  ~bv[]
  (mv-let (result st)
          (let ((st (create-st)))
            (compute-with-st x st))
          (declare (ignore st))
          result)
  ~ev[]
  However, ACL2 expects you to use ~c[with-local-stobj], not its
  expansion.  More precisely, stobj creator functions are not allowed
  except (implicitly) via ~c[with-local-stobj] and in logic-only
  situations (like theorems and hints).  Moreover, neither
  ~c[with-local-stobj] nor its expansions are legal when typed directly at
  the top-level loop.~/
  ~bv[]
  General Forms:
  (with-local-stobj stobj-name mv-let-form)
  (with-local-stobj stobj-name mv-let-form creator-name)
  ~ev[]
  where ~c[stobj-name] is the name of a ~il[stobj] other than ~ilc[state],
  ~c[mv-let-form] is a call of ~ilc[mv-let], and if ~c[creator-name] is supplied
  then it should be the  name of the creator function for ~c[stobj-name];
  ~pl[defstobj].  For the example form above, its expansion would
  use ~c[creator-name], if supplied, in place of ~c[create-st].

  ~c[With-local-stobj] can be useful when a stobj is used to memoize
  intermediate results during a computation, yet it is desired not to
  make the ~c[stobj] a formal parameter for the function and its
  callers.

  ACL2 can reason about these ``local stobjs,'' and in particular
  about stobj creator functions.  For technical reasons, ACL2 will not
  allow you to enable the ~c[:EXECUTABLE-COUNTERPART] ~il[rune] of a stobj
  creator function.

  Finally, here is a small example concocted ino rder to illustrate that
  ~c[with-local-stobj] calls can be nested.
  ~bv[]
  (defstobj st fld1)

  (defun foo ()
    (with-local-stobj
     st ; Let us call this the ``outer binding of st''.
     (mv-let (val10 val20 st)
       (let ((st (update-fld1 10 st)))
         ;; At this point the outer binding of st has fld1 = 10.
         (let ((result (with-local-stobj
                        st ; Let us call this the ``inner binding of st''.
                        (mv-let (val st)
                          (let ((st (update-fld1 20 st)))
                            ;; Now fld1 = 20 for the inner binding of st.
                            (mv (fld1 st) st))
                          val))))
           ;; So result has been bound to 20 above, but here we are once again
           ;; looking at the outer binding of st, where fld1 is still 10.
           (mv (fld1 st) result st)))
       (mv val10 val20))))

  (thm (equal (foo) (mv 10 20))) ; succeeds
  ~ev[]~/")

(defun push-untouchable-fn (name fn-p state doc event-form)
  (with-ctx-summarized
   (if (output-in-infixp state)
       event-form
     (cond ((symbolp name)
            (cond ((null doc)
                   (msg "( PUSH-UNTOUCHABLE ~x0 ~x1)" name fn-p))
                  (t (msg "( PUSH-UNTOUCHABLE ~x0 ~x1 ...)" name fn-p))))
           ((null doc) "( PUSH-UNTOUCHABLE ...)")
           (t "( PUSH-UNTOUCHABLE ... ...)")))
   (let ((wrld (w state))
         (event-form (or event-form
                         (list* 'push-untouchable name fn-p
                                (if doc
                                    (list :doc doc)
                                  nil))))
         (names (if (symbolp name) (list name) name))
         (untouchable-prop (cond (fn-p 'untouchable-fns)
                                 (t 'untouchable-vars))))
     (er-let*
      ((doc-pair (translate-doc nil doc ctx state)))

; With no name to hang it on, we don't permit a formatted doc string.
; So the above causes an error if the string is formatted.  Otherwise,
; we ignore doc-pair.

      (cond
       ((not (symbol-listp names))
        (er soft ctx
            "The argument to push-untouchable must be either a non-nil
              symbol or a non-empty true list of symbols and ~x0 is ~
              neither."
            name))
       ((subsetp-eq names (global-val untouchable-prop wrld))
        (stop-redundant-event state))
       (t
        (install-event name
                       event-form
                       'push-untouchable
                       0
                       nil
                       nil
                       nil
                       nil
                       (global-set
                        untouchable-prop
                        (union-eq names (global-val untouchable-prop wrld))
                        wrld)
                       state)))))))

(defun remove-untouchable-fn (name fn-p state doc event-form)
  (with-ctx-summarized
   (if (output-in-infixp state)
       event-form
     (cond ((symbolp name)
            (cond ((null doc)
                   (msg "( REMOVE-UNTOUCHABLE ~x0 ~x1)" name fn-p))
                  (t (msg "( REMOVE-UNTOUCHABLE ~x0 ~x1 ...)" name fn-p))))
           ((null doc) "( REMOVE-UNTOUCHABLE ...)")
           (t "( REMOVE-UNTOUCHABLE ... ...)")))
   (let ((wrld (w state))
         (event-form (or event-form
                         (list* 'remove-untouchable name fn-p
                                (if doc
                                    (list :doc doc)
                                  nil))))
         (names (if (symbolp name) (list name) name))
         (untouchable-prop (cond (fn-p 'untouchable-fns)
                                 (t 'untouchable-vars))))
     (er-let*
      ((doc-pair (translate-doc nil doc ctx state)))

; With no name to hang it on, we don't permit a formatted doc string.
; So the above causes an error if the string is formatted.  Otherwise,
; we ignore doc-pair.

      (cond
       ((not (symbol-listp names))
        (er soft ctx
            "The argument to remove-untouchable must be either a non-nil
              symbol or a non-empty true list of symbols and ~x0 is neither."
            name))
       ((not (intersectp-eq names (global-val untouchable-prop wrld)))
        (stop-redundant-event state))
       (t
        (install-event name
                       event-form
                       'remove-untouchable
                       0
                       nil
                       nil
                       nil
                       nil
                       (global-set
                        untouchable-prop
                        (set-difference-eq (global-val untouchable-prop wrld)
                                           names)
                        wrld)
                       state)))))))

(defun def-body-runes (def-bodies lemmas)
  (cond ((endp def-bodies)
         nil)
        (t (cons (find-runed-lemma (access def-body (car def-bodies)
                                           :rune)
                                   lemmas)
                 (def-body-runes (cdr def-bodies) lemmas)))))

(defmacro show-bodies (fn)

  ":Doc-Section Miscellaneous

  show the potential definition bodies~/
  ~bv[]
  Examples:
  (show-bodies foo)
  :show-bodies foo
  ~ev[]
  A definition made using ~ilc[defun] installs a so-called ``body'' of a
  function symbol, as do certain ~c[:]~ilc[definition] rules.  Such bodies are
  used in a number of ways, including the application of ~c[:expand]
  ~il[hints]; ~pl[definition], in particular the discussion of ``body'' there,
  and ~pl[hints] for a discussion of the ~c[:expand] hint.  Also ~pl[set-body]
  for how to change which of the available definitions (among the original
  definition and appropriate ~c[:]~ilc[definition] rules) is the one that
  provides the body.  The ~c[show-bodies] command displays the available such
  bodies in an appropriate format, starting with the one that is currently used
  as the body.

  ~bv[]
  General Forms:
  (show-bodies function-symbol)
  :show-bodies function-symbol
  ~ev[]~/~/"

  (declare (xargs :guard (or (symbolp fn)
                             (and (true-listp fn)
                                  (eql (length fn) 2)
                                  (eq (car fn) 'quote)
                                  (symbolp (cadr fn))))))
  (let ((fn (if (symbolp fn) fn (cadr fn))))
    `(let* ((wrld (w state))
            (fn (deref-macro-name ',fn (macro-aliases wrld)))
            (runes (def-body-runes
                     (getprop fn 'def-bodies nil 'current-acl2-world wrld)
                     (getprop fn 'lemmas nil 'current-acl2-world wrld))))
       (cond (runes
              (pprogn (fms "Definitional bodies available for ~x0, current ~
                            one listed first:~|"
                           (list (cons #\0 fn))
                           (standard-co state) state nil)
                      (print-lemmas
                       runes
                       t
                       (ens state)
                       wrld
                       state)
                      (value :invisible)))
             (t (er soft 'show-bodies
                    "There are no definitional bodies for ~x0."
                    fn))))))

(defun set-body-fn1 (rune def-bodies acc)
  (cond ((null def-bodies) ; error
         nil)
        ((equal rune (access def-body (car def-bodies) :rune))
         (cons (car def-bodies)
               (revappend acc (cdr def-bodies))))
        (t (set-body-fn1 rune
                         (cdr def-bodies)
                         (cons (car def-bodies) acc)))))

(defun set-body-fn (fn name-or-rune state event-form)
  (with-ctx-summarized
   (if (output-in-infixp state)
       event-form
     (cond ((symbolp fn)
            (msg "( SET-BODY ~x0)" fn))
           (t "( SET-BODY ...)")))
   (let* ((wrld (w state))
          (rune (if (symbolp name-or-rune)

; We don't yet know that name-or-rune is a function symbol in the current
; world, so we do not call fn-rune-nume here.

                    (list :definition name-or-rune)
                  name-or-rune))
          (fn (and (symbolp fn)
                   (deref-macro-name fn (macro-aliases wrld))))
          (old-def-bodies
           (getprop fn 'def-bodies nil 'current-acl2-world wrld))
          (def-bodies
            (and fn
                 old-def-bodies
                 (cond ((equal rune
                               (access def-body (car old-def-bodies)
                                       :rune))
                        :redundant)
                       (t (set-body-fn1 rune old-def-bodies nil))))))
     (cond
      ((null def-bodies)
       (er soft ctx
           "No definitional body was found for function ~x0 with rune ~
            ~x1.  See :DOC set-body."
           fn rune))
      ((eq def-bodies :redundant)
       (stop-redundant-event state))
      (t (install-event rune event-form 'set-body 0 nil nil nil ctx
                        (putprop fn 'def-bodies def-bodies wrld)
                        state))))))

; Section:  trace/untrace, with-error-trace (wet).

(defdoc trace
  ":Doc-Section Trace

  tracing functions in ACL2~/

  ACL2 provides utilities that rely on the underlying Lisp image to trace
  functions.  There are two interfaces to the underlying lisp trace:~bq[]

  o Macros ~ilc[trace$] and ~ilc[untrace$] call the underlying Lisp's ~c[trace]
  and ~c[untrace], respectively.  ~l[trace$] and ~pl[untrace$].

  o Macro ~ilc[with-error-trace], or ~ilc[wet] for short, provides a backtrace
  showing function calls that lead to an error.  ~l[wet].

  ~eq[]NOTES:

  1. ~ilc[Wet] turns off all tracing (i.e., executes Lisp ~c[(untrace)]) other
  than temporarily doing some tracing under-the-hood in the evaluation of the
  form supplied to it.

  2. The underlying Lisp ~c[trace] and ~c[untrace] utilities have been modified
  for GCL and Allegro CL to trace the executable counterparts.  Other Lisps may
  give unsatisfying results.  For GCL and Allegro CL, you can invoke the
  original ~c[trace] and ~c[untrace] by exiting the ACL2 loop and invoking
  ~c[old-trace] and ~c[old-untrace], respectively..

  3. Trace output for ~ilc[trace$] and ~ilc[untrace$] can be redirected to a
  file.  ~l[open-trace-file] and ~pl[close-trace-file].  However, the backtrace
  printed by ~ilc[wet] always goes to ~ilc[standard-co].~/~/")

#-acl2-loop-only
(progn ; general tracing support

#-(or gcl clisp)
; Keep the above feature check in sync with the ones in trace-ppr and
; with-error-trace-fn.  Note that *trace-level* is already defined in gcl and
; clisp.
(defparameter *trace-level* 0)

(defparameter *trace-evisc-tuple*
  '(nil nil nil nil))  ;;; (evisc-tuple nil nil nil nil)

; Trace prints using the Lisp values of *print-level* and
; *print-length*.

(defun-one-output trace-evisc-tuple ()
  (if (and (eq (caaar *trace-evisc-tuple*)
               (w *the-live-state*))
           (eql *print-level* (cadr *trace-evisc-tuple*))
           (eql *print-length* (caddr *trace-evisc-tuple*)))
      *trace-evisc-tuple*
    (reset-trace-evisc-tuple)))

; Each time one traces a function, the *trace-evisc-tuple* stores the
; current *print-level* and *print-length* with the current world.

(defun-one-output reset-trace-evisc-tuple ()
  (setq *trace-evisc-tuple*
        (list (world-evisceration-alist *the-live-state* nil)
              *print-level*
              *print-length*
              (car (cddddr *trace-evisc-tuple*)))))

(defun-one-output trace-ppr (x)
  (let ((trace-evisc-tuple (trace-evisc-tuple)))
    (ppr (eviscerate x
                     (cadr trace-evisc-tuple)          ;;; print-level
                     (caddr trace-evisc-tuple)         ;;; print-length
                     (car trace-evisc-tuple)           ;;; alist
                     (car (cddddr trace-evisc-tuple))) ;;; hiding-cars
         (min (+ 3
                 (* #-(or gcl clisp) *trace-level*
                    #+(or gcl clisp) system::*trace-level*
                    2))
              20)
         (f-get-global 'trace-co *the-live-state*)
         *the-live-state*
         t)))

)

(defun open-trace-file-fn (filename state)

; Logically, this function opens a channel to the given file.  But there is no
; logical accounting for subsequent writes to that channel on behalf of
; tracing.  We view those subsequent writes as being to the file, but not the
; channel, in analogy to how cw prints to the screen but not not modify the
; contents of *standard-co*.

  (mv-let (chan state)
          (open-output-channel filename :character state)
          (cond
           (chan #-acl2-loop-only
                 (setq *trace-output*
                       (get-output-stream-from-channel chan))
                 (pprogn
                  (if (equal (f-get-global 'trace-co state) *standard-co*)
                      state
                    (close-output-channel (f-get-global 'trace-co state)
                                          state))
                  (f-put-global 'trace-co chan state)))
           (t (prog2$
               (er hard 'open-trace-file
                   "Unable to open file ~s0 for trace output."
                   filename)
               state)))))

(defmacro open-trace-file (filename)

  ":Doc-Section Trace

  redirect trace output to a file~/
  ~bv[]
  Example:
  (open-trace-file \"foo\") ; trace output will go to file foo~/

  General Form:
  (open-trace-file filename) ; trace output will go to file filename
  ~ev[]

  Output from ~ilc[trace$] normally goes to the screen, i.e.,
  ~ilc[standard-co].  But it can be redirected to a file as shown above.
  ~l[close-trace-file] for how to send trace output back to the screen.

  Note that the backtrace printed by ~ilc[wet] always goes to
  ~ilc[standard-co], even after the use of ~c[open-trace-file].~/"

  (declare (xargs :guard (stringp filename)))
  `(pprogn (close-trace-file-fn t state)
           (open-trace-file-fn ,filename state)))

(defun close-trace-file-fn (quiet-p state)
  #-acl2-loop-only
  (setq *trace-output* (get-output-stream-from-channel *standard-co*))
  (if (equal (f-get-global 'trace-co state) *standard-co*)
      (if quiet-p
          state
        (prog2$
         (er hard 'close-trace-file
             "No change: trace is already written to standard output.~%")
         state))
    (pprogn (close-output-channel (f-get-global 'trace-co state) state)
            (f-put-global 'trace-co *standard-co* state))))

(defmacro close-trace-file ()

  ":Doc-Section Trace

  stop redirecting trace output to a file~/
  ~bv[]
  General Form:
  (close-trace-file) ; trace output is no longer redirected to a file~/
  ~ev[]

  Output from ~ilc[trace$] normally goes to the screen, or more precisely,
  ~ilc[standard-co].  It can be redirected to a file; ~pl[open-trace-file].
  Use ~c[close-trace-file] to redirect trace output to ~ilc[standard-co].~/"

  '(close-trace-file-fn nil state))

; Here we develop the code for wet (with-error-trace).

#-acl2-loop-only
(progn

(defun-one-output push-error-trace-stack (name arglist)

; It seems like a good idea to create this function.  Its compilation,
; including macroexpansion, may speed up its evaluation during a trace.  It
; needs to return nil even though it modifies state, so we go ahead and put it
; outside the ACL2 loop.

  (f-put-global
   'error-trace-stack
   (cons (cons name arglist)
         (f-get-global 'error-trace-stack *the-live-state*))
   *the-live-state*)
  nil)

(defun-one-output pop-error-trace-stack ()
  (f-put-global
   'error-trace-stack
   (cdr (f-get-global 'error-trace-stack *the-live-state*))
   *the-live-state*))

#+gcl
(defun-one-output with-error-trace-fns-forms-rec (names)
  (if (endp names)
      nil
    (let* ((name (car names))
           (*1*name (*1*-symbol name)))
      `((,name
         :entrycond
         (push-error-trace-stack ',name si::arglist)
         :exitcond
         (pop-error-trace-stack))
        (,*1*name
         :entrycond
         (push-error-trace-stack ',*1*name si::arglist)
         :exitcond
         (pop-error-trace-stack))
        ,@(with-error-trace-fns-forms-rec (cdr names))))))

#+clisp
(defun-one-output with-error-trace-fns-forms-rec (names)
  (if (endp names)
      nil
    (let* ((name (car names))
           (*1*name (*1*-symbol name)))
      `((,name
         :suppress-if t
         :pre
         (push-error-trace-stack ',name ext:*trace-args*)
         :post
         (pop-error-trace-stack))
        (,*1*name
         :suppress-if t
         :pre
         (push-error-trace-stack ',*1*name ext:*trace-args*)
         :post
         (pop-error-trace-stack))
        ,@(with-error-trace-fns-forms-rec (cdr names))))))

#+allegro
(defun-one-output with-error-trace-fns-forms (names)
  (if (endp names)
      nil
    (let* ((name (car names))
           (*1*name (*1*-symbol name)))
      `((excl:unadvise ,name)
        (excl:unadvise ,*1*name)
        (excl:advise ,name :before nil nil
                     (push-error-trace-stack ',name si::arglist))
        (excl:advise ,*1*name :before nil nil
                     (push-error-trace-stack ',*1*name si::arglist))
        (excl:advise ,name :after nil nil
                     (pop-error-trace-stack))
        (excl:advise ,*1*name :after nil nil
                     (pop-error-trace-stack))
        ,@(with-error-trace-fns-forms (cdr names))))))

#+(or gcl clisp)
(defun-one-output with-error-trace-fns-forms (names)
  `((trace ,@(with-error-trace-fns-forms-rec names))))

#+openmcl
(progn

(defun with-error-trace-fns-before (fn &rest args)
  (push-error-trace-stack fn args))

(defun with-error-trace-fns-after (fn &rest results)
  (declare (ignore fn results))
  (pop-error-trace-stack))

(defun-one-output with-error-trace-fns-forms (names)
  (if (endp names)
      nil
    (let* ((name (car names))
           (*1*name (*1*-symbol name)))
      `((old-untrace ,name)
        (old-untrace ,*1*name)
        (old-trace (,name
                    :before #'with-error-trace-fns-before
                    :after #'with-error-trace-fns-after))
        (old-trace (,*1*name
                    :before #'with-error-trace-fns-before
                    :after #'with-error-trace-fns-after))
        ,@(with-error-trace-fns-forms (cdr names))))))

)

#-(or allegro gcl clisp openmcl)
(defun-one-output with-error-trace-fns-forms (names)
  (declare (ignore names))
  (er hard 'with-error-trace
      "It is illegal to use wet (with-error-trace) in this Common Lisp ~
       implementation.  If you want to see it supported for this Common Lisp, ~
       please contact the implementors."))

)

(defconst *non-exec-fns*
  '(must-be-equal
    time$
    with-prover-time-limit
    decrement-big-n
    zp-big-n
    prog2$))

(defun executable-ancestors (flg fn wrld acc)

; Here flg is nil if fn is a single function, else fn is a list of functions.

  (cond
   (flg (if (null fn)
            acc
          (executable-ancestors
           flg (cdr fn) wrld
           (executable-ancestors nil (car fn) wrld acc))))
   ((or (member-eq fn acc)
        (member-eq fn *non-exec-fns*))
    acc)
   ((equal (symbol-package-name fn) *main-lisp-package-name*)

; Should we exclude other functions that might get called a lot?

    acc)
   (t
    (mv-let (name x)
            (constraint-info fn wrld)
            (declare (ignore x))
            (cond
             (name acc)
             (t (let ((body (getprop fn 'unnormalized-body nil
                                     'current-acl2-world wrld)))
                  (cond
                   (body (executable-ancestors t (all-fnnames body) wrld
                                               (cons fn acc)))
                   (t acc)))))))))

#+(and (or openmcl allegro) (not acl2-loop-only))
(defun-one-output custom-trace-ppr (direction x)

; We need to provide all the output that one expects when using a trace
; facility.  Hence the cond clause and the first argument.

  (cond ((eq direction :in)

; Originally we incremented the trace level here.  But instead we wait until
; calling trace-ppr, in order to get the spacing to work out.

         (case *trace-level*
           (0 (princ "1> " *trace-output*))
           (1 (princ "  2> " *trace-output*))
           (2 (princ "    3> " *trace-output*))
           (3 (princ "      4> " *trace-output*))
           (4 (princ "        5> " *trace-output*))
           (5 (princ "          6> " *trace-output*))
           (6 (princ "            7> " *trace-output*))
           (7 (princ "              8> " *trace-output*))
           (8 (princ "                9> " *trace-output*))
           (t (princ (format nil "                  ~s>" (1+f *trace-level*))
                     *trace-output*))))
        (t
         (case *trace-level*
           (1 (princ "<1 " *trace-output*))
           (2 (princ "  <2 " *trace-output*))
           (3 (princ "    <3 " *trace-output*))
           (4 (princ "      <4 " *trace-output*))
           (5 (princ "        <5 " *trace-output*))
           (6 (princ "          <6 " *trace-output*))
           (7 (princ "            <7 " *trace-output*))
           (8 (princ "              <8 " *trace-output*))
           (9 (princ "                <9 " *trace-output*))
           (t (princ (format nil "                  <~s " *trace-level*)
                     *trace-output*)))
         (decf *trace-level*)))
  (trace-ppr x)
  (when (eq direction :in) (incf *trace-level*))
  (princ #\Newline *trace-output*)
  (finish-output *trace-output*))

(defun with-error-trace-fn (term evisc-tuple evisc-tuple-supplied
                                 fnlist omitfnlist state)
  (declare (xargs :stobjs state :mode :program)
           #+acl2-loop-only
           (ignore evisc-tuple evisc-tuple-supplied fnlist omitfnlist))
  #+(or acl2-loop-only no-hack)
  (er-let* ((val (trans-eval term 'with-error-trace state)))
    (value (cdr val)))
  #-(or acl2-loop-only no-hack)
  (let* ((fnlist0
          (or fnlist
              (mv-let (erp val bindings state)
                      (translate1 term :stobjs-out
                                  '((:stobjs-out . :stobjs-out)) t
                                  'top-level (w state) state)
                      (declare (ignore bindings))
                      (if erp
                          (er hard 'with-error-trace
                              "With-error-trace (WET) failed because the ~
                               indicated form could not be translated ~
                               (converted to internal form).")
                        (executable-ancestors t (all-fnnames val) (w state)
                                              nil)))))
         (fnlist (if omitfnlist
                     (set-difference-eq fnlist0 omitfnlist)
                   fnlist0)))
    (multiple-value-prog1
        (acl2-unwind-protect
         "with-error-trace"
         (progn (eval '(#+openmcl
                        old-untrace
                        #-openmcl
                        untrace)) ; use current (not compile-time) untrace
                (eval (cons 'progn (with-error-trace-fns-forms fnlist)))
                (f-put-global 'error-trace-stack nil state)
                (er-let* ((val (trans-eval term 'with-error-trace state)))
                  (value (cdr val))))
         (progn #-(or gcl clisp openmcl) (setq *trace-level* 0)
                (let* ((*trace-output* (get-output-stream-from-channel
                                       (standard-co state)))
                       (evisc-tuple (if evisc-tuple-supplied
                                        evisc-tuple
                                      (default-evisc-tuple state)))
                       (*print-level* (cadr evisc-tuple))
                       (*print-length* (caddr evisc-tuple))
                      #-(or allegro openmcl) (level 0))
                  (dolist (entry (reverse (f-get-global 'error-trace-stack state)))
                          (state-global-let*
                           ((trace-co (standard-co state)))
                           #+(or allegro openmcl)
                           (progn
                             (custom-trace-ppr
                              :in
                              (cons (car entry)
                                    (trace-hide-world-and-state
                                     (cdr entry))))

; Avoid compiler warning by Allegro:

                             state)
                           #-(or allegro openmcl)
                           (progn (spaces (min 20 (* 2 level))
                                          0
                                          (f-get-global 'trace-co state)
                                          state)
                                  (setq level (1+ level))
                                  (fmt1 "~Y01> "
                                        (list (cons #\0 level)
                                              (cons #\1 (trace-evisc-tuple)))
                                        0 (standard-co state) state
                                        nil)
                                  (trace-ppr (cons (car entry)
                                                   (trace-hide-world-and-state
                                                    (cdr entry))))
                                  (newline (standard-co state) state)))))
                (newline (f-get-global 'trace-co state) state))
         state)
      (progn
        (eval '(#+openmcl
                old-untrace
                #-openmcl
                untrace)) ; use current (not compile-time) untrace
        #-(or gcl clisp openmcl) (setq *trace-level* 0)
        (f-put-global 'error-trace-stack nil state)))))

(defmacro with-error-trace (term
                            &optional (evisc-tuple 'nil evisc-tuple-supplied)
                            &key fns omit)

  ":Doc-Section Trace

  evaluate a form and print subsequent error trace~/

  ~c[With-error-trace] is the same as ~c[wet]; ~pl[wet].~/~/"

  (declare (xargs :guard (and (symbol-listp fns)
                              (symbol-listp omit))))
  `(with-error-trace-fn ',term ,evisc-tuple ',evisc-tuple-supplied ',fns ',omit
                        state))

(defmacro wet (term &optional (evisc-tuple 'nil evisc-tuple-supplied)
                    &key fns omit)

  ":Doc-Section Trace

  evaluate a form and print subsequent error trace~/
  NOTE:  This feature is onlyh available if you are using GCL, Allegro CL, or
  CLISP.
  ~bv[]
  Examples:
  (wet (bar 3))            ; evaluate (bar 3) and print backtrace upon error
  (wet (bar 3) nil)        ; as above, but avoid hiding the structure of bar
  (wet (bar 3) (evisc-tuple 3 5 nil nil))
                           ; as above, but hiding the structure of bar up to
                           ; level 3 and length 5
  (wet (bar 3) :fns (f g)) ; as above but only include calls of f, g~/

  General Forms (all but the first argument may be omitted):
  (wet form
       evisc-tuple ; optional, and evaluated
       :fns  (f1 f2 ... fk)
       :omit (g1 g2 ... gk))
  ~ev[]
  where ~c[form] is an arbitrary ACL2 form and the ~c[fi] are function symbols
  whose calls are to appear in the backtrace if the evaluation of ~c[form]
  aborts.  Generally, ~c[wet] will hide parts of large structures that it
  prints out, but this can be avoided by supplying a value of ~c[nil] for
  ~c[evisc-tuple].

  More generally, the ~c[evisc-tuple] argument, which is evaluated, can be
  supplied to specify the print-level and print-length for the resulting
  backtrace; ~pl[ld-evisc-tuple].

  If the value of ~c[:fns] is ~c[nil] or not supplied, then calls of all
  functions appear in the backtrace, with the exception of built-in functions
  that are either in the main Lisp package or are in ~c[:]~ilc[program] mode.
  (In particular, all user-defined functions appear.)  The above description is
  modified if ~c[:omit] is supplied, in which case calls of the specified
  function symbols are removed from the backtrace.

  The following example illustrates the use of ~c[wet], which stands for
  ``~ilc[with-error-trace]''.  We omit uninteresting output from this example.
  ~bv[]
  ACL2 !>(defun foo (x) (car x))
   ...
   FOO
  ACL2 !>(defun bar (x) (foo x))
   ...
   BAR
  ACL2 !>(bar 3)


  ACL2 Error in TOP-LEVEL:  The guard for the function symbol CAR, which
  is (OR (CONSP X) (EQUAL X NIL)), is violated by the arguments in the
  call (CAR 3).  To see a trace of calls leading up to this violation,
  execute (wet <form>) where <form> is the form you submitted to the
  ACL2 loop.  See :DOC wet for how to get an error backtrace.

  ACL2 !>(wet (bar 3))


  ACL2 Error in WITH-ERROR-TRACE:  The guard for the function symbol
  CAR, which is (OR (CONSP X) (EQUAL X NIL)), is violated by the arguments
  in the call (CAR 3).  (Backtrace is below.)

  1> (ACL2_*1*_ACL2::BAR 3)
    2> (ACL2_*1*_ACL2::FOO 3)

  ACL2 !>(wet (bar 3) :fns (foo))



  ACL2 Error in WITH-ERROR-TRACE:  The guard for the function symbol
  CAR, which is (OR (CONSP X) (EQUAL X NIL)), is violated by the arguments
  in the call (CAR 3).  (Backtrace is below.)

  1> (ACL2_*1*_ACL2::FOO 3)

  ACL2 !>
  ~ev[]
  Notice that because guards were not verified, the so-called
  ~il[executable-counterpart] functions are evaluated for ~c[foo] and
  ~c[bar].  These can be identified with package names beginning with the
  string \"ACL2_*1*_\".

  ~l[trace$] for a general tracing utility.

  NOTES:

  1. Recursive calls of ~il[executable-counterpart] functions will not
  generally be traced.

  2. In the (probably rare) event of a hard Lisp error, you will have to exit
  the Lisp break before seeing the backtrace.

  3. ~c[Wet] always untraces all functions before it installs the traces it
  needs, and it leaves all functions untraced when it completes.  If existing
  functions were traced then you will need to re-execute ~ilc[trace$] in order
  to re-install tracing on those functions after ~c[wet] is called on any form.

  4. ~c[Wet] returns an error triple ~c[(mv error-p value state)], where
  ~c[value] is a print representation of the value returned by the form given
  to ~c[wet].  Presumably ~c[value] is not particularly important anyhow, as
  the intended use of ~c[wet] is for the case that an error occurred in
  evaluation of a form.

  5. As mentioned above, functions in the main Lisp package (i.e., those built
  into Common Lisp) will not be traced by ~c[wet].~/"

  `(with-error-trace-fn ',term ,evisc-tuple ',evisc-tuple-supplied ',fns ',omit
                        state))

(defun trace$-fn (fns)

; We declare :mode :program so that the Common Lisp function is called even if
; we ultimately put this in a file in :logic mode (in which case we could
; allow :mode :logic as long as we verify guards).

  (declare (xargs :mode :program)) ; so that Common Lisp function is called
  #+(or no-hack acl2-loop-only)
  (declare (ignore fns))
  #-(or no-hack acl2-loop-only)
  (eval (cons 'trace fns))
  nil)

(defmacro trace$ (&rest fns)

  ":Doc-Section Trace

  trace the indicated functions~/
  ~bv[]
  Example:
  (trace$ foo bar)~/

  General Form:
  (trace$ fn1 fn2 ... fnk)
  ~ev[]
  where the ~c[fni] are defined or even constrained functions.

  ~pl[untrace$] for how to undo the effect of ~ilc[trace$].

  Basically, ~c[trace$] calls on the underlying Lisp to trace the specified
  functions as well as their ~ilc[executable-counterpart]s.  However, for GCL,
  Allegro CL, and OpenMCL, the underlying Lisp trace routines are modified
  before an image is saved in order to hide the ACL2 world and other large data
  structures and provide slightly prettier output.

  Recursive calls of functions whose ~il[guard]s have not been verified will
  not generally be traced.  If you want to see traces for corresponding
  evaluations in raw Lisp, which will generally show recursive calls (except,
  in some Lisp implementations, for compiled tail recursive functions), then
  you can quit into raw Lisp (~c[:q]) and execute your form there.
  Alternatively, you can avoid ~il[executable-counterpart] functions by using
  ~c[:]~ilc[set-raw-mode] to enter a raw Lisp version of the ACL2 loop;
  ~pl[set-raw-mode] and ~pl[set-raw-mode-on!].

  Output from ~ilc[trace$] normally goes to the screen, i.e.,
  ~ilc[standard-co].  But it can be redirected to a file;
  ~pl[open-trace-file].

  Also ~pl[wet] (``~ilc[with-error-trace]'') for a different way that ACL2
  takes advantage of the underlying Lisp, namely to provide a backtrace when
  there is an error.

  Note that from a logical perspective all trace printing is a fiction.  For a
  related fiction, ~pl[cw].  ~c[Trace$] returns ~c[nil].

  The following example will give an idea of the options available other than
  just ~c[(trace$ fn1 fn2 ... fnk)].  It works about as shown in Allegro CL and
  GCL, but in OpenMCL the recursive calls are omitted unless you escape into
  raw Lisp and redefine ~c[fact] with ~c[(declare (notinline fact))].
  ~bv[]
  ACL2 !>(defun fact (n)
           (declare (xargs :guard (natp n) :mode :program))
           (if (zp n)
               1
             (* n (fact (1- n)))))

  Summary
  Form:  ( DEFUN FACT ...)
  Rules: NIL
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   FACT
  ACL2 !>(trace$ (fact :entry (car arglist) :exit values))
  NIL
  ACL2 !>(fact 5)
  1> (ACL2_*1*_ACL2::FACT . 5)
    2> (FACT . 5)
      3> (FACT . 4)
        4> (FACT . 3)
          5> (FACT . 2)
            6> (FACT . 1)
              7> (FACT . 0)
              <7 (FACT 1)
            <6 (FACT 1)
          <5 (FACT 2)
        <4 (FACT 6)
      <3 (FACT 24)
    <2 (FACT 120)
  <1 (ACL2_*1*_ACL2::FACT 120)
  120
  ACL2 !>
  ~ev[]
  Here is another example.
  ~bv[]
  ACL2 !>(defun fact2 (n acc)
           (declare (xargs :guard (and (natp n) (natp acc))))
           (if (zp n)
               (mv acc (* 2 acc))
             (fact2 (1- n) (* n acc))))

  The admission of FACT2 is trivial, using the relation O< (which is
  known to be well-founded on the domain recognized by O-P) and the measure
  (ACL2-COUNT N).  We observe that the type of FACT2 is described by
  the theorem (AND (CONSP (FACT2 N ACC)) (TRUE-LISTP (FACT2 N ACC))).
  We used primitive type reasoning.

  (FACT2 * *) => (MV * *).

  The guard conjecture for FACT2 is trivial to prove.  FACT2 is compliant
  with Common Lisp.

  Summary
  Form:  ( DEFUN FACT2 ...)
  Rules: ((:COMPOUND-RECOGNIZER NATP-COMPOUND-RECOGNIZER)
          (:COMPOUND-RECOGNIZER ZP-COMPOUND-RECOGNIZER)
          (:FAKE-RUNE-FOR-TYPE-SET NIL))
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   FACT2
  ACL2 !>(trace$ (fact2 :entry (list 'my-entry (car arglist))
                        :exit (list 'my-second-value (cadr values))))
  NIL
  ACL2 !>(fact2 6 1)

    1> (ACL2_*1*_ACL2::FACT2 MY-ENTRY 6)>
      2> (FACT2 MY-ENTRY 6)>
        3> (FACT2 MY-ENTRY 5)>
          4> (FACT2 MY-ENTRY 4)>
            5> (FACT2 MY-ENTRY 3)>
              6> (FACT2 MY-ENTRY 2)>
                7> (FACT2 MY-ENTRY 1)>
                  8> (FACT2 MY-ENTRY 0)>
                  <8 (FACT2 MY-SECOND-VALUE 1440)>
                <7 (FACT2 MY-SECOND-VALUE 1440)>
              <6 (FACT2 MY-SECOND-VALUE 1440)>
            <5 (FACT2 MY-SECOND-VALUE 1440)>
          <4 (FACT2 MY-SECOND-VALUE 1440)>
        <3 (FACT2 MY-SECOND-VALUE 1440)>
      <2 (FACT2 MY-SECOND-VALUE 1440)>
    <1 (ACL2_*1*_ACL2::FACT2 MY-SECOND-VALUE 1440)>
  (720 1440)
  ACL2 !>(trace$ (fact2 :entry (list 'my-args-reversed
                                     (list (cadr arglist) (car arglist)))
                        :exit (list 'values-reversed
                                    (cadr values) (car values))))
  NIL
  ACL2 !>(fact2 6 1)

    1> (ACL2_*1*_ACL2::FACT2 MY-ARGS-REVERSED (1 6))>
      2> (FACT2 MY-ARGS-REVERSED (1 6))>
        3> (FACT2 MY-ARGS-REVERSED (6 5))>
          4> (FACT2 MY-ARGS-REVERSED (30 4))>
            5> (FACT2 MY-ARGS-REVERSED (120 3))>
              6> (FACT2 MY-ARGS-REVERSED (360 2))>
                7> (FACT2 MY-ARGS-REVERSED (720 1))>
                  8> (FACT2 MY-ARGS-REVERSED (720 0))>
                  <8 (FACT2 VALUES-REVERSED 1440 720)>
                <7 (FACT2 VALUES-REVERSED 1440 720)>
              <6 (FACT2 VALUES-REVERSED 1440 720)>
            <5 (FACT2 VALUES-REVERSED 1440 720)>
          <4 (FACT2 VALUES-REVERSED 1440 720)>
        <3 (FACT2 VALUES-REVERSED 1440 720)>
      <2 (FACT2 VALUES-REVERSED 1440 720)>
    <1 (ACL2_*1*_ACL2::FACT2 VALUES-REVERSED 1440 720)>
  (720 1440)
  ACL2 !>
  ~ev[]
  ~/"

  `(trace$-fn ',fns))

(defun untrace$-fn (fns)

; We declare :mode :program so that the Common Lisp function is called even if
; we ultimately put this in a file in :logic mode (in which case we could
; allow :mode :logic as long as we verify guards).

  (declare (xargs :mode :program))
  #+(or no-hack acl2-loop-only)
  (declare (ignore fns))
  #-(or no-hack acl2-loop-only)
  (eval (cons 'untrace fns))
  nil)

(defmacro untrace$ (&rest fns)

  ":Doc-Section Trace

  untrace functions~/
  ~bv[]
  Examples:
  (untrace$)         ; untrace all traced functions
  (untrace$ foo bar) ; untrace foo and bar~/

  General Forms:
  (untrace$)                 ; untrace all traced functions
  (untrace$ fn1 fn2 ... fnk) ; untrace the indicated functions
  ~ev[]
  where the ~c[fni] are defined or even constrained functions.

  ~c[Untrace$] undoes the effect of ~ilc[trace$].  ~l[trace$].
  ~c[Untrace] returns ~c[nil]~/"

  `(untrace$-fn ',fns))

(defmacro break-on-error (&optional (on 't))

  ":Doc-Section Trace

  break when encountering a hard or soft error caused by ACL2.~/
  ~bv[]
  General forms:
  (break-on-error t)   ; installs a trace causing a continuable error (break)
                       ;   whenever a hard or soft error is invoked by ACL2.
  (break-on-error)     ; same as above
  (break-on-error nil) ; uninstall the above trace
  ~ev[]
  ~c[(Break-on-error)] is actually a macro that expands as follows.
  ~bv[]
  (trace$ (illegal :entry (break))
          (error1  :entry (break)))
  ~ev[]
  This trace should cause entry to the Lisp debugger (at least in most Lisps)
  whenever ACL2 calls its error routines.

  Also ~pl[trace$].~/~/"

  (if on
      '(trace$ (illegal :entry (break))
               (error1  :entry (break)))
    '(untrace$ illegal error1)))

(defun defexec-extract-key (x keyword result result-p)

; X is a keyword-value-listp from an xargs declaration, and result-p indicates
; whether we expect to see no further value of the indicated keyword (in which
; case we should return result and result-p unchanged if erp, below, is nil).
; We return (mv erp result result-p), where if erp is nil, result-p is nil
; coming in, and x contains (keyword result), then we return (mv nil result t).

  (declare (xargs :guard (and (keywordp keyword)
                              (keyword-value-listp x))))
  (cond ((endp x)
         (mv nil result result-p))
        (t (mv-let (erp result result-p)
             (defexec-extract-key (cddr x) keyword result result-p)
             (cond (erp (mv erp nil nil))
                   ((eq (car x) keyword)
                    (cond
                     (result-p (mv "more than one ~x0 has been specified"
                                  nil nil))
                     (t (mv nil (cadr x) t))))
                   (t (mv nil result result-p)))))))

(defun parse-defexec-dcls-1 (alist guard guard-p hints hints-p measure
                                   measure-p wfrel wfrel-p stobjs stobjs-p exec-xargs
                                   exec-test exec-default acc)

; We return (mv nil declare-form ...) as suggested in the first (endp) case
; below, where exec-xargs has been removed from alist in creating the declare
; form (the second returned value).

  (declare (xargs :guard (symbol-alistp alist)))
  (cond
   ((endp alist)
    (mv nil
        (cons 'declare (reverse acc))
        guard guard-p
        hints hints-p
        measure measure-p
        wfrel wfrel-p
        stobjs stobjs-p
        exec-xargs exec-test exec-default))
   (t (let* ((decl (car alist))
             (sym (car decl))
             (x (cdr decl)))
        (cond
         ((eq sym 'xargs)
          (cond
           ((keyword-value-listp x)
            (mv-let (erp guard guard-p)
              (defexec-extract-key x :GUARD guard guard-p)
              (cond
               (erp (mv erp nil nil nil nil nil nil nil nil nil nil nil nil nil
                        nil))
               (t (mv-let (erp hints hints-p)
                    (defexec-extract-key x :HINTS hints hints-p)
                    (cond
                     (erp (mv erp nil nil nil nil nil nil nil nil nil nil nil
                              nil nil nil))
                     (t (mv-let (erp measure measure-p)
                          (defexec-extract-key x :MEASURE measure measure-p)
                          (cond
                           (erp (mv erp nil nil nil nil nil nil nil nil nil nil
                                    nil nil nil nil))
                           (t (mv-let (erp wfrel wfrel-p)
                                (defexec-extract-key x :WELL-FOUNDED-RELATION
                                  wfrel wfrel-p)
                                (cond
                                 (erp (mv erp nil nil nil nil nil nil nil nil
                                          nil nil nil nil nil nil))
                                 (t (mv-let (erp stobjs stobjs-p)
                                      (defexec-extract-key x :STOBJS stobjs
                                        stobjs-p)
                                      (cond
                                       (erp (mv erp nil nil nil nil nil nil nil
                                                nil nil nil nil nil nil nil))
                                       (t (parse-defexec-dcls-1
                                           (cdr alist)
                                           guard guard-p
                                           hints hints-p
                                           measure measure-p
                                           wfrel wfrel-p
                                           stobjs stobjs-p
                                           exec-xargs exec-test exec-default
                                           (cons decl acc))))))))))))))))))
           (t (mv "we found (XARGS . x) where x is not a keyword-value-listp"
                  nil nil nil nil nil nil nil nil nil nil nil nil nil nil))))
         ((eq sym 'exec-xargs)
          (cond
           ((or exec-xargs exec-test exec-default)
            (mv "more than one EXEC-XARGS has been specified"
                nil nil nil nil nil nil nil nil nil nil nil nil nil nil))
           ((and (keyword-value-listp x) x)
            (let* ((exec-test (cadr (assoc-keyword :test x)))
                   (x (if exec-test (remove-keyword :test x) x))
                   (exec-default (cadr (assoc-keyword :default-value x)))
                   (x (if exec-default (remove-keyword :default-value x) x)))
              (parse-defexec-dcls-1 (cdr alist)
                                    guard guard-p
                                    hints hints-p
                                    measure measure-p
                                    wfrel wfrel-p
                                    stobjs stobjs-p
                                    x
                                    exec-test
                                    exec-default
                                    acc)))
           (t (mv "we found declaration (EXEC-XARGS . x) where x is not a ~
                   non-empty keyword-value-listp"
                  nil nil nil nil nil nil nil nil nil nil nil nil nil nil))))
         (t (parse-defexec-dcls-1 (cdr alist)
                                  guard guard-p
                                  hints hints-p
                                  measure measure-p
                                  wfrel wfrel-p
                                  stobjs stobjs-p
                                  x
                                  exec-test
                                  exec-default
                                  (cons (car alist) acc))))))))

(defun fix-exec-xargs (exec-xargs hints hints-p measure measure-p wfrel wfrel-p
                                  stobjs stobjs-p)
  (declare (xargs :guard (keyword-value-listp exec-xargs)))

; Update exec-xargs to incorporate the hints, measure, and stobjs extracted
; from the xargs (if any).

  (let* ((x (if (and hints-p (not (assoc-keyword :HINTS exec-xargs)))
                (list* :HINTS hints exec-xargs)
              exec-xargs))
         (x (if (and measure-p (not (assoc-keyword :MEASURE exec-xargs)))
                (list* :MEASURE measure x)
              x))
         (x (if (and wfrel-p (not (assoc-keyword :WELL-FOUNDED-RELATION
                                                 exec-xargs)))
                (list* :WELL-FOUNDED-RELATION wfrel x)
              x))
         (x (if (and stobjs-p (not (assoc-keyword :STOBJS exec-xargs)))
                (list* :STOBJS stobjs x)
              x)))
    x))

(defun parse-defexec-dcls (dcls-and-strings final guard guard-p hints hints-p
                                            measure measure-p wfrel wfrel-p
                                            stobjs stobjs-p exec-xargs
                                            exec-test exec-default)

; We return the following values.  Note that input guard-p is true if we have
; encountered a guard on an earlier call.

;  erp          - nil or a string that indicates an error
;  final        - what is left of dcls-and-strings after (exec-xargs ...) is
;                 removed
;  guard        - the guard from (xargs ... :guard ...)
;  exec-xargs   - the cdr of (exec-xargs ...) from input
;  exec-test    - from (exec-xargs ... :test ...) if present, else guard
;  exec-default - from (exec-xargs ... :default-value ...), else nil

  (cond
   ((endp dcls-and-strings)
    (cond
     ((null guard-p)
      (mv "no :GUARD has been specified in the XARGS.  The MBE proof ~
           obligation is actually a guard condition -- we have to prove that ~
           the guard ensures that the :LOGIC and :EXEC terms are equivalent ~
           and that the guards are satisfied for the :EXEC term.  Please ~
           specify a :GUARD.  Note also that you can delay the verification ~
           of the MBE conditions by delaying guard verification, as with ~
           :VERIFY-GUARDS NIL"
          nil nil nil nil nil))
     (t
      (mv nil
          (reverse final)
          guard
          (fix-exec-xargs exec-xargs hints hints-p measure measure-p wfrel
                          wfrel-p stobjs stobjs-p)
          (or exec-test guard)
          exec-default))))
   (t (let ((x (car dcls-and-strings)))
        (cond
         ((stringp x)
          (parse-defexec-dcls (cdr dcls-and-strings) (cons x final) guard
                              guard-p hints hints-p measure measure-p wfrel
                              wfrel-p stobjs stobjs-p exec-xargs exec-test
                              exec-default))
         ((and (consp x)
               (eq (car x) 'declare)
               (symbol-alistp (cdr x)))
          (mv-let (erp decl guard guard-p hints hints-p measure measure-p wfrel
                       wfrel-p stobjs stobjs-p exec-xargs exec-test
                       exec-default)
            (parse-defexec-dcls-1 (cdr x) guard guard-p hints hints-p measure
                                  measure-p wfrel wfrel-p stobjs stobjs-p
                                  exec-xargs exec-test exec-default nil)
            (cond
             (erp (mv erp nil nil nil nil nil))
             (t (parse-defexec-dcls (cdr dcls-and-strings) (cons decl final)
                                    guard guard-p hints hints-p measure
                                    measure-p wfrel wfrel-p stobjs stobjs-p
                                    exec-xargs exec-test exec-default)))))
         (t
          (mv (msg "the form ~x0 is neither a string nor a form (declare . x) ~
                    where x is a symbol-alistp"
                   x)
              nil nil nil nil nil)))))))

(defmacro defexec (&whole whole fn formals &rest rest)

  ":Doc-Section Events

  attach a terminating executable function to a definition~/
  
  Suppose you define a function ~c[(fn x)] with a ~il[guard] of
  ~c[(good-input-p x)], and you know that when the guard holds, the measure
  decreases on each recursive call.  Unfortunately, the definitional principle
  (~pl[defun]) ignores the guard.  For example, if the definition has the form
  ~bv[]
  (defun fn (x)
    (declare (xargs :guard (good-input-p x)))
    (if (not-done-yet x)
        (... (fn (destr x)) ...)
      ...))
  ~ev[]
  then in order to admit this definition, ACL2 must prove the appropriate
  formula asserting that ~c[(destr x)] is ``smaller than'' ~c[x] under the
  assumption ~c[(not-done-yet x)] but without the assumption
  ~c[(good-input-p x)], even if ~c[(not-done-yet x)] is true.  In essence, it
  may be necessary to submit instead the following definition. 
  ~bv[]
  (defun fn (x)
    (declare (xargs :guard (good-input-p x)))
    (if (good-input-p x)
        (if (not-done-yet x)
            (... (fn (destr x)) ...)
          ...)
      nil)
  ~ev[]
  But it is unfortunate that when calls of ~c[fn] are evaluated, for example
  when ~c[fn] is applied to an explicit constant during a proof, then a call of
  ~c[good-input-p] must now be evaluated on each recursive call.

  Fortunately, ~c[defexec] provides a way to keep the execution efficient.  For
  the example above we could use the following form.
  ~bv[]
  (defexec fn (x)
    (declare (xargs :guard (good-input-p x)))
    (mbe :logic (if (good-input-p x)
                    (if (not-done-yet x)
                        (... (fn (destr x)) ...)
                      ...)
                  nil)
         :exec  (if (not-done-yet x)
                    (... (fn (destr x)) ...)
                  ...)))
  ~ev[]
  Here ``~ilc[mbe]'' stands for ``must-be-equal'' and, roughly speaking, its
  call above is logically equal to the ~c[:logic] form but is evaluated using
  the ~c[:exec] form when the guard holds.  ~l[mbe].  The effect is thus to
  define ~c[fn] as shown in the ~ilc[defun] form above, but to cause execution
  of ~c[fn] using the ~c[:exec] body.  The use of ~c[defexec] instead of
  ~ilc[defun] in the example above causes a termination proof to be performed,
  in order to guarantee that evaluation always theoretically terminates, even
  when using the ~c[:exec] form for evaluation.
  ~bv[]
  Example:

  ; Some of the keyword arguments in the declarations below are irrelevant or
  ; unnecessary, but they serve to illustrate their use.

  (defexec f (x)
    (declare (xargs :measure (+ 15 (acl2-count x))
                    :hints ((\"Goal\" :in-theory (disable nth)))
                    :guard-hints ((\"Goal\" :in-theory (disable last)))
                    :guard (and (integerp x) (<= 0 x) (< x 25)))
             (exec-xargs
                    :test (and (integerp x) (<= 0 x))
                    :default-value 'undef ; defaults to nil
                    :measure (nfix x)
                    :well-founded-relation o<))
    (mbe :logic (if (zp x)
                    1
                  (* x (f (- x 1))))
         :exec  (if (= x 0)
                    1
                  (* x (f (- x 1))))))
  ~ev[]
  The above example macroexpands to the following.
  ~bv[]
  (ENCAPSULATE ()
   (LOCAL
    (ENCAPSULATE ()
     (SET-IGNORE-OK T)
     (SET-IRRELEVANT-FORMALS-OK T)
     (LOCAL (DEFUN F (X)
              (DECLARE
               (XARGS :VERIFY-GUARDS NIL
                      :HINTS ((\"Goal\" :IN-THEORY (DISABLE NTH)))
                      :MEASURE (NFIX X)
                      :WELL-FOUNDED-RELATION O<))
              (IF (AND (INTEGERP X) (<= 0 X))
                  (IF (= X 0) 1 (* X (F (- X 1))))
                  'UNDEF)))
     (LOCAL (DEFTHM F-GUARD-IMPLIES-TEST
              (IMPLIES (AND (INTEGERP X) (<= 0 X) (< X 25))
                       (AND (INTEGERP X) (<= 0 X)))
              :RULE-CLASSES NIL))))
   (DEFUN F (X)
     (DECLARE (XARGS :MEASURE (+ 15 (ACL2-COUNT X))
                     :HINTS ((\"Goal\" :IN-THEORY (DISABLE NTH)))
                     :GUARD-HINTS ((\"Goal\" :IN-THEORY (DISABLE LAST)))
                     :GUARD (AND (INTEGERP X) (<= 0 X) (< X 25))))
     (MBE :LOGIC
          (IF (ZP X) 1 (* X (F (- X 1))))
          :EXEC
          (IF (= X 0) 1 (* X (F (- X 1)))))))
  ~ev[]
  Notice that in the example above, the ~c[:]~ilc[hints] in the ~ilc[local]
  definition of ~c[F] are inherited from the ~c[:hints] in the ~ilc[xargs] of
  the ~c[defexec] form.  We discuss such inheritance below.
  ~bv[]
  General Form:
  (defexec fn (var1 ... varn) doc-string dcl ... dcl
    (mbe :LOGIC logic-body
         :EXEC  exec-body))
  ~ev[]
  where the syntax is identical to the syntax of ~ilc[defun] where the body is
  a call of ~c[mbe], with the exceptions described below.  Thus, ~c[fn] is the
  symbol you wish to define and is a new symbolic name and ~c[(var1 ... varn)]
  is its list of formal parameters (~pl[name]).  The first exception is that at
  least one ~c[dcl] (i.e., ~ilc[declare] form) must specify a ~c[:guard],
  ~c[guard].  The second exception is that one of the ~c[dcl]s is allowed to
  contain an element of the form ~c[(exec-xargs ...)].  The ~c[exec-xargs]
  form, if present, must specify a non-empty ~ilc[keyword-value-listp] each of
  whose keys is one of ~c[:test], ~c[:default-value], or one of the standard
  ~ilc[xargs] keys of ~c[:measure], ~c[:well-founded-relation], ~c[:hints], or
  ~c[:stobjs].  Any of these four standard ~c[xargs] keys that is present in an
  ~c[xargs] of some ~c[dcl] but is not specified in the (possibly nonexistent)
  ~c[exec-xargs] form is considered to be specified in the ~c[exec-xargs] form,
  as illustrated in the example above for ~c[:hints].  (So for example, if you
  want ~c[:hints] in the final, non-local definition but not in the local
  definition, then specify the ~c[:hints] in the ~c[xargs] but specify
  ~c[:hints nil] in the ~c[exec-xargs].)  If ~c[:test] is specified and not
  ~c[nil], let ~c[test] be its value; otherwise let ~c[test] default to
  ~c[guard].  If ~c[:default-value] is specified, let ~c[default-value] be its
  value; else ~c[default-value] is ~c[nil].  ~c[Default-value] should have the
  same ~il[signature] as ~c[exec-body]; otherwise the ~c[defexec] form will
  fail to be admitted.

  The above General Form's macroexpansion is of the form
  ~c[(PROGN encap final-def)], where ~c[encap] and ~c[final-def] are as
  follows.  ~c[Final-def] is simply the result of removing the ~c[exec-xargs]
  declaration (if any) from its ~ilc[declare] form, and is the result of
  evaluating the given ~c[defexec] form, since ~c[encap] is of the following
  form.
  ~bv[]
  ; encap
  (ENCAPSULATE ()
    (set-ignore-ok t)             ; harmless for proving termination
    (set-irrelevant-formals-ok t) ; harmless for proving termination
    (local local-def)
    (local local-thm))
  ~ev[]
  The purpose of ~c[encap] is to ensure the the executable version of ~c[name]
  terminates on all arguments.  Thus, ~c[local-def] and ~c[local-thm] are as
  follows, where the ~c[xargs] of the ~ilc[declare] form are the result of
  adding ~c[:VERIFY-GUARDS NIL] to the result of removing the ~c[:test] and
  (optional) ~c[:default-value] from the ~c[exec-xargs].
  ~bv[]
  ; local-def
  (DEFUN fn formals
    (DECLARE (XARGS :VERIFY-GUARDS NIL ...))
    (IF test
        exec-body
      default-value))

  ; local-thm
  (DEFTHM fn-EXEC-GUARD-HOLDS
    (IMPLIES guard test)
    :RULE-CLASSES NIL)
  ~ev[]
  We claim that if the above ~c[local-def] and ~c[local-thm] are admitted, then
  all evaluations of calls of ~c[fn] terminate.  The concern is that the use
  of ~ilc[mbe] in ~c[final-def] allows for the use of ~c[exec-body] for a call
  of ~c[fn], as well as for subsequent recursive calls, when ~c[guard] holds
  and assuming that the guards have been verified for ~c[final-def].  However,
  by ~c[local-thm] we can conclude in this case that ~c[test] holds, in which
  case the call of ~c[fn] may be viewed as a call of the version of ~c[fn]
  defined in ~c[local-def].  Moreover, since guards have been verified for
  ~c[final-def], then guards hold for subsequent evaluation of ~c[exec-body],
  and in particular for recursive calls of ~c[fn], which can thus continue to
  be viewed as calls using ~c[local=def].~/~/

  :cited-by mbe"
 
  (let ((dcls-and-strings (butlast rest 1))
        (body (car (last rest))))
    (mv-let (erp exec-body)
      (case-match body
        (('mbe ':logic & ':exec exec-body)
         (mv nil exec-body))
        (('mbe ':exec exec-body ':logic &)
         (mv nil exec-body))
        (('mbe . &)
         (mv 'mbe nil))
        (& (mv t nil)))
      (cond
       (erp `(er soft 'defexec
                 "A defexec form must have a body that is a valid call of mbe. ~
                  See :DOC ~s0."
                 ,(if (eq erp 'mbe) "mbe" "defexec")))
       ((not (symbolp fn))
        `(er soft 'defexec
             "The first argument of defexec must be a symbol, but ~x0 is not."
             ',fn))
       ((not (arglistp formals))
        `(er soft 'defexec
             "The second argument of defexec must be legal list of formals, ~
              but ~x0 is not."
             ',formals))
       (t (mv-let (erp final-dcls-and-strings guard exec-xargs exec-test
                       exec-default)
            (parse-defexec-dcls dcls-and-strings nil nil nil nil nil nil nil
                                nil nil nil nil nil nil nil)
            (cond
             (erp
              `(er soft 'defexec
                   "Macroexpansion of ~x0 has failed because ~@1."
                   ',whole
                   ',erp))
             (t `(encapsulate ()
                   (local
                    (encapsulate ()
                                 (set-ignore-ok t)
                                 (set-irrelevant-formals-ok t)
                                 (local (defun ,fn ,formals
                                          (declare (xargs :verify-guards nil
                                                          ,@exec-xargs))
                                          (if ,exec-test
                                              ,exec-body
                                            ,exec-default)))
                                 (local (defthm ,(packn
                                                  (list fn
                                                        '-GUARD-IMPLIES-TEST))
                                          (implies ,guard ,exec-test)
                                          :rule-classes nil))))
                   (defun ,fn ,formals
                     ,@final-dcls-and-strings
                     ,body))))))))))

; Start code for :pl and proof-checker show-rewrites command.

(defrec sar ; single-applicable-rewrite
  ((lemma . alist) (index . equiv))
  nil)

; Here's the idea.  Both showing and using of rewrites benefits from knowing
; which hypotheses are irrelevant.  But when rewriting in the proof-checker, we
; will try to do more, namely relieve all the hyps by instantiating free
; variables.  So we avoid doing any instantiation in forming the sar record.
; Actually, if we knew that rewriting were to be done with the empty
; substitution, then we'd go ahead and store the result of trying to relieve
; hypotheses at this point; but we don't.  Nevertheless, we should have a
; function that takes the fields of an sar record and returns an appropriate
; structure representing the result of trying to relieve the hyps (possibly
; starting with a unify-subst extending the one that was originally produced).

(defun applicable-rewrite-rules1 (term geneqv lemmas current-index
                                       target-name-or-rune target-index wrld)

; Call this initially with current-index equal to 1.

  (declare (xargs :guard (or (null target-index) (integerp target-index))))
  (if (consp lemmas)
      (let ((lemma (car lemmas)))
        ;; if the lemma needs to be considered, consder it
        (if (and (or (null target-name-or-rune)
                     (if (symbolp target-name-or-rune)
                         (equal target-name-or-rune
                                (cadr (access rewrite-rule lemma :rune)))
                       (equal target-name-or-rune
                              (access rewrite-rule lemma :rune))))
                 (member (access rewrite-rule lemma :subclass)
                         '(backchain abbreviation definition))
                 (or (eq geneqv :none)
                     (geneqv-refinementp (access rewrite-rule lemma :equiv)
                                         geneqv
                                         wrld)))
            (mv-let (flg alist)
                    (one-way-unify (access rewrite-rule lemma :lhs) term)
                    (if flg
                        (if target-index
                            (if (eql target-index current-index)
                                (list (make sar
                                            :index current-index
                                            :lemma lemma
                                            :alist alist
                                            :equiv (access rewrite-rule lemma
                                                           :equiv)))
                              (applicable-rewrite-rules1
                               term geneqv (cdr lemmas) (1+ current-index)
                               target-name-or-rune target-index wrld))
                          (cons (make sar
                                      :index (if target-name-or-rune
                                                 nil
                                               current-index)
                                      :lemma lemma
                                      :alist alist
                                      :equiv (access rewrite-rule lemma
                                                     :equiv))
                                (applicable-rewrite-rules1
                                 term geneqv (cdr lemmas) (1+ current-index)
                                 target-name-or-rune target-index wrld)))
                      (applicable-rewrite-rules1
                       term geneqv (cdr lemmas) current-index
                       target-name-or-rune target-index wrld)))
          (applicable-rewrite-rules1
           term geneqv (cdr lemmas) current-index
           target-name-or-rune target-index wrld)))
    nil))

(defun pc-relieve-hyp (rune hyp unify-subst type-alist wrld state ens ttree)

; This function is adapted from ACL2 function relieve-hyp, but it prevents
; backchaining, instead returning the new hypotheses.  Notice that there are no
; arguments for obj, equiv, fnstack, ancestors, or simplify-clause-pot-lst.
; Also notice that rcnst has been replaced by ens (an enable structure).

; We return t or nil indicating whether we won, an extended unify-subst
; and a new ttree.  This function is a No-Change Loser.

  (cond ((f-big-clock-negative-p state)
         (mv nil unify-subst ttree))
        ((and (nvariablep hyp)
              (not (fquotep hyp))
              (eq (ffn-symb hyp) 'synp))
         (mv-let
          (wonp failure-reason unify-subst ttree)
          (relieve-hyp-synp rune hyp unify-subst type-alist wrld
                            state
                            nil ; fnstack
                            nil ; ancestors
                            nil ; simplify-clause-pot-lst
                            *empty-rewrite-constant*
                            nil ; gstack
                            ttree)
          (declare (ignore failure-reason))
          (mv wonp unify-subst ttree)))
        (t (mv-let
            (forcep bind-flg)
            (binding-hyp-p hyp unify-subst wrld)
            (let ((hyp (if forcep (fargn hyp 1) hyp)))
              (cond
               (bind-flg
                (mv t
                    (cons (cons (fargn hyp 1) (fargn hyp 2))
                          unify-subst)
                    ttree))
               (t
                (mv-let
                 (lookup-hyp-ans unify-subst ttree)
                 (lookup-hyp hyp type-alist wrld unify-subst ttree)
                 (cond
                  (lookup-hyp-ans
                   (mv t unify-subst ttree))
                  ((free-varsp hyp unify-subst)
                   (search-ground-units` hyp unify-subst type-alist ens
                                        (ok-to-force-ens ens) wrld ttree))
                  (t
                   (let ((inst-hyp (sublis-var unify-subst hyp)))
                     (mv-let
                      (knownp nilp nilp-ttree)
                      (known-whether-nil inst-hyp type-alist ens
                                         (ok-to-force-ens ens) wrld ttree)
                      (cond
                       (knownp
                        (mv (not nilp) unify-subst nilp-ttree))
                       (t
                        (mv-let
                         (not-flg atm)
                         (strip-not hyp)

; Again, we avoid rewriting in this proof-checker code.

                         (cond
                          (not-flg
                           (if (equal atm *nil*)
                               (mv t unify-subst ttree)
                             (mv nil unify-subst ttree)))
                          (t
                           (if (if-tautologyp atm)
                               (mv t unify-subst ttree)
                             (mv nil unify-subst ttree)))))))))))))))))))

(defun pc-relieve-hyps1 (rune hyps unify-subst unify-subst0 ttree0 type-alist
                              keep-unify-subst wrld state ens ttree)

; This function is adapted from ACL2 function relieve-hyp.  Notice that there
; are no arguments for obj, equiv, fnstack, ancestors, or
; simplify-clause-pot-lst.  Also notice that rcnst has been replaced by ens (an
; enable structure).

; When keep-unify-subst is non-nil, we run through all of the hyps in order to
; find extensions of unify-subst that bind free variables in order to make hyps
; true.  Keep-unify-subst is true at the top level, but when we get a failure,
; we set it to :FAILED so that we can return nil at the end.

; This function a No-Change Loser when keep-unify-subst is nil.  In order to
; accomplish this without requiring it have to test the answer to its own
; recursive calls, we have to pass down the original unify-subst and ttree so
; that when it fails it can return them instead of the accumulated versions.

  (cond ((f-big-clock-negative-p state)
         (mv nil unify-subst ttree))
        ((null hyps)
         (mv (not (eq keep-unify-subst :FAILED)) unify-subst ttree))
        (t (mv-let (relieve-hyp-ans new-unify-subst ttree)

; We avoid rewriting in this proof-checker code, so new-ttree = ttree.

             (pc-relieve-hyp rune (car hyps) unify-subst type-alist wrld state
                             ens ttree)
             (cond
              ((or relieve-hyp-ans keep-unify-subst)
               (pc-relieve-hyps1 rune
                                 (cdr hyps)
                                 new-unify-subst
                                 unify-subst0 ttree0
                                 type-alist
                                 (if (and (eq keep-unify-subst t)
                                          (not relieve-hyp-ans))
                                     :FAILED
                                   keep-unify-subst)
                                 wrld state ens ttree))
              (t (mv nil unify-subst0 ttree0)))))))

(defun pc-relieve-hyps (rune hyps unify-subst type-alist keep-unify-subst wrld
                             state ens ttree)

; Adapted from ACL2 function relieve-hyp.  Notice that there are no arguments
; for obj, equiv, fnstack, ancestors, or simplify-clause-pot-lst.  Also notice
; that rcnst has been replaced by ens (an enable structure).

; We return t or nil indicating success, an extended unify-subst and
; a new ttree.  This function is a No-Change Loser.

  (pc-relieve-hyps1 rune hyps unify-subst unify-subst ttree type-alist
                    keep-unify-subst wrld state ens ttree))

(defun remove-trivial-lits (lst type-alist alist wrld ens ttree)

; Removes trivially true lits from lst.  However, we don't touch elements of
; lst that contain free variables.  We apply the substitution at this point
; because we need to know whether a lit contains a free variable (one not bound
; by alist) that might get bound later, thus changing its truth value.

  (if (consp lst)
      (mv-let (rest-list ttree)
        (remove-trivial-lits (cdr lst) type-alist alist wrld ens ttree)
        (let ((new-lit (sublis-var alist (car lst))))
          (if (free-varsp (car lst) alist)
              (mv (cons new-lit rest-list) ttree)
            (mv-let (knownp nilp nilp-ttree)
              (known-whether-nil new-lit type-alist
                                 ens (ok-to-force-ens ens) wrld ttree)
              (if (and knownp (not nilp))
                  (mv rest-list nilp-ttree)
                (mv (cons new-lit rest-list) ttree))))))
    (mv nil ttree)))

(defun unrelieved-hyps (rune hyps unify-subst type-alist keep-unify-subst wrld
                             state ens ttree)

; Returns unrelieved hyps (with the appropriate substitution applied), an
; extended substitution, and a new tag tree.  Note: the substitution really has
; been applied already to the returned hyps, even though we also return the
; extended substitution.

; If keep-unify-subst is true, then we allow unify-subst to extend even if we
; do not relieve all of the hypotheses.

  (mv-let (success-flg new-unify-subst new-ttree)
    (pc-relieve-hyps rune hyps unify-subst type-alist keep-unify-subst wrld
                     state ens ttree)
    (if success-flg
        (mv nil new-unify-subst new-ttree)
      (mv-let (unify-subst ttree)
        (if keep-unify-subst
            (mv new-unify-subst new-ttree)
          (mv unify-subst ttree))
        (mv-let (lits ttree)
          (remove-trivial-lits hyps type-alist unify-subst wrld ens ttree)
          (mv lits unify-subst ttree))))))

(defun untranslate-subst-abb (sub abbreviations state)
  (declare (xargs :guard (symbol-alistp sub)))
  (if (consp sub)
      (cons (list (caar sub) (untrans0 (cdar sub) nil abbreviations))
            (untranslate-subst-abb (cdr sub) abbreviations state))
    nil))

(defun show-rewrite (index col rune nume show-more subst-hyps subst-hyps-2
                           unify-subst unify-subst-2 free free-2 rhs
                           abbreviations term-id-iff ens enabled-only-flg
                           equiv pl-p state)

; Pl-p is true when we are calling this function on behalf of :pl, and is false
; when we are calling it on behalf of the proof-checker.

  (let ((enabledp (enabled-numep nume ens))
        (subst-rhs (sublis-var unify-subst rhs)))
    (if (and enabled-only-flg
             (not enabledp))
        state
      (pprogn
       (fms "~|~#a~[~c0. ~/  ~]~x1~#2~[~/ (disabled)~]"
            (list (cons #\a (if index 0 1))
                  (cons #\0 (cons index col))
                  (cons #\1
                        ;; Let's just print the name of the rune if it appears
                        ;; to be unique.
                        (if (cddr rune) rune (base-symbol rune)))
                  (cons #\2 (if enabledp 0 1)))
            (standard-co state) state nil)
       (let ((fmt-string
              "~ ~ New term: ~y3~|~
               ~ ~ Hypotheses: ~#b~[<none>~/~y4~]~|~
               ~ ~ Equiv: ~ye~|~
               ~#s~[~/~ ~ Substitution: ~ya~|~]~
               ~#5~[~/~
                    ~ ~ Remaining free variable: ~&6~/~
                    ~ ~ Remaining free variables: ~&6~sn~]~
               ~#7~[~/  WARNING:  One of the hypotheses is (equivalent to) NIL, ~
               and hence will apparently be impossible to relieve.~]~|"))
         (pprogn
          (cond
           ((and show-more pl-p) ; then just show more
            state)
           (t
            (fms fmt-string
                 (list (cons #\3 (untrans0 subst-rhs term-id-iff
                                           abbreviations))
                       (cons #\s (if pl-p 1 0))
                       (cons #\a (untranslate-subst-abb unify-subst
                                                        abbreviations
                                                        state))
                       (cons #\b (if subst-hyps 1 0))
                       (cons #\e equiv)
                       (cons #\4 (untrans0-lst subst-hyps t abbreviations))
                       (cons #\5 (zero-one-or-more (length free)))
                       (cons #\6 free)
                       (cons #\n "")
                       (cons #\7 (if (member-eq nil subst-hyps) 1 0)))
                 (standard-co state) state nil)))
          (cond (show-more
                 (pprogn
                  (cond (pl-p state)
                        (t (fms0 "  -- IF REWRITE is called with ~
                                  instantiate-free=t: --")))
                  (fms fmt-string
                       (list (cons #\3 (untrans0
                                        (sublis-var unify-subst-2 rhs)
                                        term-id-iff abbreviations))
                             (cons #\s (if pl-p 1 0))
                             (cons #\a (untranslate-subst-abb unify-subst-2
                                                              abbreviations
                                                              state))
                             (cons #\b (if subst-hyps-2 1 0))
                             (cons #\e equiv)
                             (cons #\4 (untrans0-lst subst-hyps-2 t
                                                     abbreviations))
                             (cons #\5 (if (eql (length free-2) 1)
                                           1
                                         2))
                             (cons #\6 free-2)
                             (cons #\n (if (null free-2)
                                           "[none]"
                                         ""))
                             (cons #\7 (if (member-eq nil subst-hyps-2)
                                           1
                                         0)))
                       (standard-co state) state nil)))
                (t state))))))))

(defun show-rewrites (app-rewrite-rules col abbreviations term-id-iff ens
                                        type-alist enabled-only-flg
                                        pl-p w state)

; Pl-p is true when we are calling this function on behalf of :pl, and is false
; when we are calling it on behalf of the proof-checker.

  (if (null app-rewrite-rules)
      state
    (pprogn (let ((sar (car app-rewrite-rules)))
              (let ((lemma (access sar sar :lemma))
                    (alist (access sar sar :alist))
                    (index (access sar sar :index)))
                (let ((hyps (access rewrite-rule lemma :hyps))
                      (rhs (access rewrite-rule lemma :rhs))
                      (rune (access rewrite-rule lemma :rune)))
                  (mv-let (subst-hyps unify-subst ttree)
                    (unrelieved-hyps rune hyps alist type-alist nil w state ens
                                     nil)
                    (declare (ignore ttree))
                    (let* ((rhs-and-hyps-vars
                            (union-eq (all-vars rhs)
                                      (all-vars1-lst hyps nil)))
                           (free (set-difference-assoc-eq
                                  rhs-and-hyps-vars
                                  unify-subst)))
                      (mv-let (show-more subst-hyps-2 unify-subst-2)
                        (cond ((and free subst-hyps)

; Then we try to find at least a partial extension of unify-subst that
; eliminates some hypotheses.

                               (mv-let (subst-hyps-2 unify-subst-2 ttree)
                                 (unrelieved-hyps rune hyps alist type-alist t
                                                  w state ens nil)
                                 (declare (ignore ttree))
                                 (cond ((equal unify-subst-2 unify-subst)
                                        (assert$
                                         (equal subst-hyps-2 subst-hyps)
                                         (mv nil subst-hyps unify-subst)))
                                       (t
                                        (mv t subst-hyps-2 unify-subst-2)))))
                              (t (mv  nil subst-hyps unify-subst)))
                        (show-rewrite index col
                                      rune
                                      (access rewrite-rule lemma :nume)
                                      show-more
                                      subst-hyps  subst-hyps-2
                                      unify-subst unify-subst-2
                                      free
                                      (set-difference-assoc-eq
                                       rhs-and-hyps-vars
                                       unify-subst-2)
                                      rhs abbreviations term-id-iff ens
                                      enabled-only-flg
                                      (access sar sar :equiv)
                                      pl-p
                                      state)))))))
            (show-rewrites (cdr app-rewrite-rules) col abbreviations
                           term-id-iff ens type-alist enabled-only-flg
                           pl-p w state))))

(defun expand-assumptions-1 (term) 
  (case-match term
    (('if a b ''nil)
     (append (expand-assumptions-1 a) (expand-assumptions-1 b)))
    ((equality-p a b)
     (if (or (and (eq equality-p 'eq)
                  (or (and (consp a) (eq (car a) 'quote) (symbolp (cadr a)))
                      (and (consp b) (eq (car b) 'quote) (symbolp (cadr b)))))
             (and (eq equality-p 'eql)
                  (or (and (consp a) (eq (car a) 'quote) (eqlablep (cadr a)))
                      (and (consp b) (eq (car b) 'quote) (eqlablep (cadr b))))))
         (list term (mcons-term* 'equal a b))
       (list term)))
    (& (list term))))

(defun expand-assumptions (x)

; If x is (and a b) then we get (list a b), etc.

  (declare (xargs :guard (true-listp x)))
  (if x
      (append (expand-assumptions-1 (car x))
              (expand-assumptions (cdr x)))
    nil))

(defun hyps-type-alist (assumptions ens wrld state)

; Note that the force-flg arg to type-alist-clause is nil here, so we shouldn't
; wind up with any assumptions in the returned tag tree. Also note that we
; return (mv contradictionp type-alist fc-pair-lst), where actually fc-pair-lst
; is a ttree if contradictionp holds; normally we ignore fc-pair-lst otherwise.

  (forward-chain (dumb-negate-lit-lst (expand-assumptions assumptions))
                 nil
                 (ok-to-force-ens ens)
                 nil ; do-not-reconsiderp
                 wrld ens (match-free-override wrld) state))

(defun show-rewrites-fn (rule-id enabled-only-flg ens current-term
                                 abbreviations term-id-iff all-hyps geneqv
                                 pl-p state)

; Pl-p is true when we are calling this function on behalf of :pl, and is false
; when we are calling it on behalf of the proof-checker.

  (let ((name (and (symbolp rule-id) rule-id))
        (index (and (integerp rule-id) (< 0 rule-id) rule-id))
        (rune (and (consp rule-id)
                   (equal (car rule-id) :rewrite)
                   rule-id))
        (w (w state)))
    (cond
     ((and rule-id (not (or name index rune)))
      (fms "The rule-id argument to SHOW-REWRITES must be a name, a positive ~
            integer, or a rewrite rule rune, but ~x0 is none of these.~|"
           (list (cons #\0 rule-id)) (standard-co state) state nil))
     ((or (variablep current-term)
          (fquotep current-term)
          (flambdap (ffn-symb current-term)))
      (fms "It is only possible to apply rewrite rules to terms that are not ~
            variables, (quoted) constants, or applications of lambda ~
            expressions.  However, the current term is:~%~ ~ ~y0.~|"
           (list (cons #\0 current-term)) (standard-co state) state nil))
     ((eq (ffn-symb current-term) 'if)
      (fms "It is only possible to apply rewrite rules to terms that are ~
            applications of function symbols other than IF.  However, the ~
            current term is~|~ ~ ~y0.~|"
           (list (cons #\0 current-term)) (standard-co state) state nil))
     (t
      (mv-let (flg hyps-type-alist ttree)
        (hyps-type-alist all-hyps ens w state)
        (declare (ignore ttree))
        (if flg
            (fms "*** Contradiction in the hypotheses! ***~%The S command ~
                  should complete this goal.~|"
                 nil (standard-co state) state nil)
          (let ((app-rewrite-rules
                 (applicable-rewrite-rules1
                  current-term
                  geneqv
                  (getprop (ffn-symb current-term) 'lemmas nil 'current-acl2-world w)
                  1 (or name rune) index w)))
            (if (null app-rewrite-rules)
                (if (and index (> index 1))
                    (fms "~|*** There are fewer than ~x0 applicable rewrite rules. ***~%"
                         (list (cons #\0 index)) (standard-co state) state nil)
                  (fms "~|*** There are no applicable rewrite rules. ***~%"
                       nil  (standard-co state) state nil))
              (show-rewrites app-rewrite-rules
                             (floor (length app-rewrite-rules) 10)
                             abbreviations term-id-iff
                             ens hyps-type-alist
                             enabled-only-flg pl-p w state)))))))))

(defun show-meta-lemmas1 (lemmas index term wrld ens state)
  (cond ((endp lemmas) state)
        (t
         (mv-let
          (new-index state)
          (let ((lemma (car lemmas)))
            (cond ((eq (access rewrite-rule lemma :subclass)
                       'meta)
                   (let* ((fn (access rewrite-rule lemma :lhs))
                          (extendedp (access rewrite-rule lemma :rhs))
                          (args (meta-fn-args term extendedp ens state)))
                     (mv-let
                      (erp new-term latches)
                      (ev-fncall-meta fn args state)
                      (declare (ignore latches))
                      (cond ((or erp
                                 (equal new-term term)
                                 (not (termp new-term wrld)))
                             (mv index state))
                            (t
                             (let ((hyp-fn (access rewrite-rule lemma :hyps)))
                               (mv-let
                                (erp hyp latches)
                                (if hyp-fn
                                    (ev-fncall-meta
                                     hyp-fn
                                     (meta-fn-args term extendedp ens state)
                                     state)
                                  (mv nil *t* nil))
                                (declare (ignore latches))
                                (cond
                                 ((or erp (not (termp hyp wrld)))
                                  (mv index state))
                                 (t
                                  (pprogn
                                   (fms
                                    "META ~x0. ~y1~|~
                                     ~ ~ New term: ~y2~|~
                                     ~ ~ Hypothesis: ~y3~|~
                                     ~ ~ Equiv: ~y4~|"
                                    (list (cons #\0 index)
                                          (cons #\1
                                                (let ((rune
                                                       (access rewrite-rule
                                                               lemma
                                                               :rune)))
                                                  (if (cddr rune)
                                                      rune
                                                    (base-symbol rune))))
                                          (cons #\2 new-term)
                                          (cons #\3 (untranslate hyp
                                                                 nil
                                                                 wrld))
                                          (cons #\4 (access rewrite-rule lemma
                                                            :equiv)))
                                    (standard-co state) state nil)
                                   (mv (1+ index) state)))))))))))
                  (t (mv index state))))
          (show-meta-lemmas1 (cdr lemmas) new-index term wrld ens state)))))

(defun show-meta-lemmas (term state)
  (cond ((and (nvariablep term)
              (not (fquotep term))
              (not (flambdap (ffn-symb term))))
         (let ((wrld (w state)))
           (show-meta-lemmas1 (getprop (ffn-symb term) 'lemmas nil
                                       'current-acl2-world wrld)
                              1 term wrld (ens state) state)))
        (t state)))

(defun pl-fn (name state)
  (if (symbolp name)
      (let* ((wrld (w state))
             (name (deref-macro-name name (macro-aliases wrld))))
        (if (function-symbolp name wrld)
            (pprogn (print-lemmas
                     (getprop name 'lemmas nil 'current-acl2-world wrld)
                     t
                     (ens state)
                     wrld
                     state)
                    (value :invisible))
          (if (getprop name 'macro-body nil 'current-acl2-world wrld)
              (er soft 'pl
                  "The argument to PL must be a function symbol in ~
                   the current world, but ~x0 is a macro."
                  name)
            (er soft 'pl
                "The argument to PL must be a function symbol in the current ~
                 world, or else a macro that is associated with a function ~
                 symbol (see :DOC add-macro-alias)."))))
    (er-let* ((term (translate name t t nil 'pl (w state) state)))
      (pprogn (show-rewrites-fn nil nil (ens state) term
                                nil nil nil :none t state)
              (show-meta-lemmas term state)
              (value :invisible)))))

(defmacro pl (name)

  ":Doc-Section History

  print the rules for the given name or term~/
  ~bv[]
  Examples:
  :pl foo     ; prints rules that rewrite some call of foo
  :pl (+ x y) ; prints rules that rewrite (+ x y)
  ~ev[]~/

  ~c[Pl] takes one argument, which should be a symbol or a term.  If the
  argument is a function symbol (or a macro corresponding to a function;
  ~pl[macro-aliases-table]), ~c[:pl] displays the ~c[:]~ilc[rewrite],
  ~c[:]~ilc[definition], and ~c[:]~ilc[meta] rules that rewrite some term whose
  top function symbol is the one specified.  Otherwise, ~c[:pl] displays the
  ~c[:]~ilc[rewrite] and ~c[:]~ilc[definition] rules that rewrite the specified
  term, followed by the applicable ~c[:]~ilc[meta] rules.  For
  ~c[:]~ilc[rewrite] and ~c[:]~ilc[definition] rules, ~c[:pl] also shows the
  substitution that, when applied to the left-hand side of the rule, yields the
  specified term.  For ~c[:]~ilc[meta] rules, only those are displayed that
  meet two conditions: the application of the metafunction returns a term
  different from the input term, and if there is a hypothesis metafunction then
  it also returns a term.  (A subtlety: In the case of extended metafunctions
  (~pl[extended-metafunctions]), a trivial metafunction context is used for the
  application of the metafunction.)

  The kinds of rules printed by ~c[:pl] are ~c[:]~ilc[rewrite] rules,
  ~c[:]~ilc[definition] rules, and ~il[meta] rules (not, for example,
  ~c[:]~ilc[forward-chaining] rules).~/"

  (list 'pl-fn name 'state))

; We moved add-include-book-dir-fn and delete-include-book-dir-fn to this file,
; after table-fn is defined, to avoid warnings from the following during the
; boot-strap that the *1* function for table-fn is undefined.

(defun add-include-book-dir-fn (keyword dir state)
  (declare (xargs :guard (state-p state)
                  :mode :program))
  (let ((ctx 'add-include-book-dir))
    (cond ((or (not (keywordp keyword))
               (eq keyword :SYSTEM))
           (er soft ctx
               "The first argument of add-include-book-dir must be a keyword ~
                (see :DOC keywordp) other than :SYSTEM, but ~x0 is not."
               keyword))
          ((not (and (stringp dir)
                     (absolute-pathname-string-p
                      dir t (os (w state)))))
           (er soft ctx
               "The second argument of add-include-book-dir must be an ~
                absolute pathname string, but ~x0 is not."
               dir))
          (t
           (state-global-let*
            ((inhibit-output-lst (cons 'summary (@ inhibit-output-lst))))
            (let* ((old (cdr (assoc-eq :include-book-dir-alist
                                       (table-alist 'acl2-defaults-table
                                                    (w state)))))
                   (new (put-assoc-eq keyword dir old)))
              (er-progn (table-fn 'acl2-defaults-table
                                  (list :include-book-dir-alist
                                        (list 'quote new))
                                  state
                                  (list 'table
                                        'acl2-defaults-table
                                        ':include-book-dir-alist
                                        (list 'quote new)))
                        (table-fn 'acl2-defaults-table
                                  '(:include-book-dir-alist)
                                  state
                                  '(table acl2-defaults-table
                                          :include-book-dir-alist)))))))))

(defun delete-include-book-dir-fn (keyword state)
  (declare (xargs :guard (state-p state)
                  :mode :program))
  (let ((ctx 'delete-include-book-dir))
    (cond ((or (not (keywordp keyword))
               (eq keyword :SYSTEM))
           (er soft ctx
               "The argument of delete-include-book-dir must be a keyword ~
                (see :DOC keywordp) other than :SYSTEM, but ~x0 is not."
               keyword))
          (t
           (state-global-let*
            ((inhibit-output-lst (cons 'summary (@ inhibit-output-lst))))
            (let* ((old (cdr (assoc-eq :include-book-dir-alist
                                       (table-alist 'acl2-defaults-table
                                                    (w state)))))
                   (new (delete-assoc-eq keyword old)))
              (er-progn (table-fn 'acl2-defaults-table
                                  (list :include-book-dir-alist
                                        (list 'quote new))
                                  state
                                  (list 'table
                                        'acl2-defaults-table
                                        ':include-book-dir-alist
                                        (list 'quote new)))
                        (table-fn 'acl2-defaults-table
                                  '(:include-book-dir-alist)
                                  state
                                  '(table acl2-defaults-table
                                          :include-book-dir-alist)))))))))

(defmacro set-tainted-okp (x)

  ":Doc-Section Other

  control output~/
  ~bv[]
  Forms:
  (set-tainted-okp nil) ; do not allow incremental version mismatches (default)
  (set-tainted-okp t)   ; allow incremental version mismatches
  ~ev[]

  ~c[Set-tainted-okp] is of use only in the presence of incremental releases.
  In short, evaluation of ~c[(set-tainted-okp t)] instructs ACL2 to consider an
  incremental release to have the same ACL2 ~il[version] as the most recently
  preceding normal release.  BUT THIS IS NOT THE CASE BY DEFAULT BECAUSE ACL2
  IS POTENTIALLY UNSOUND WHEN ~c[SET-TAINTED-OKP] IS EVALUATED.~/

  Incremental releases have an incremental (incrl) ~il[version] field, for
  example the number 1 in version  2.9.1.  (Also ~pl[version].)  Ordinary
  releases have an implicit incrl version field of 0 (for example, in version
  2.9).  By default, ~ilc[include-book] and ~ilc[certify-book] consider all
  fields of ACL2 version strings, including their incrl fields, in order to
  decide if there are version mismatches.  But it may be convenient to treat an
  incremental release as the same as the corresponding (immediately preceding)
  normal release, in order to avoid recertification of existing certified
  books.  SUCH RECERTIFICATION IS LOGICALLY REQUIRED, but we provide
  ~c[(set-tainted-okp t)] as a mechanism to allow users to experiment with
  incremental releases.

  Below we describe how books can be certified even though their certification
  has depended on ignoring mismatches of incrl version fields.  We call such
  certified books ``tainted''.

  If ~c[(set-tainted-okp t)] is evaluated, then any discrepancy is ignored
  between the incrl version field of an included book (representing the version
  of ACL2 in which that book was certified) and the current ACL2 version,
  namely the value of ~c[(@ acl2-version)].  Thus, with ~c[(set-tainted-okp t)]
  we allow certification of books that depend on included books that have such
  version mismatches with the current ACL2 version or are themselves tainted.
  Any book thus certified will have the string \"(tainted)\" included in its
  ~il[certificate]'s ~il[version] string.  Indeed, when ACL2 detects that a
  book may depend either on a book whose version's incrl field differs from
  that of the current ACL2 ~il[version], or on a tainted book, then such a book
  is marked as tainted.

  When ~c[(set-tainted-okp t)] has been executed, then even though ACL2
  ``ignores'' issues of tainting as discussed above, a ~c[\"Tainted\"] warning
  is printed whenever a tainted book is included or certified.~/"

  (if (member-eq x '(nil t))
      `(pprogn (f-put-global 'tainted-okp ,x state)
               (if ,x
                   (warning$ 'set-tainted-okp "Tainted"
                             "Inclusion of tainted books may render an ACL2 ~
                              session unsound.  See :DOC set-tainted-okp.")
                 state)
               (value ,x))
    `(er soft 'set-tainted-okp
         "The legal values of set-tainted-okp are ~x0 and ~x1.  Thus ~x2 is ~
          not a legal value.  See :DOC set-tainted-okp."
         t nil ,x)))

#-acl2-loop-only
(defmacro reset-prehistory (&rest args)
  (declare (ignore args))
  nil)

#+acl2-loop-only
(defmacro reset-prehistory (&whole event-form &optional permanent-p doc)

; Warning: See the Important Boot-Strapping Invariants before modifying!

  ":Doc-Section Events

  reset the prehistory~/
  ~bv[]
  Examples:
  (reset-prehistory)   ; restart command numbering at 0
  (reset-prehistory t) ; as above, and also disable ubt-prehistory~/

  General Forms:
  (reset-prehistory)
  (reset-prehistory permanent-p)
  (reset-prehistory permanent-p doc-string)
  ~ev[]
  where ~c[permanent-p] is ~c[t] or ~c[nil], and ~c[doc-string] is an optional
  ~il[documentation] string not beginning with ``~c[:doc-section] ...''.  After
  execution of this command, ACL2 will change the numbering provided by its
  ~il[history] utilities so that this ~c[reset-prehistory] command (or the
  top-level compound ~il[command] containing it, which for example might be an
  ~ilc[include-book]) is assigned the number 0.  The only way to undo this
  command is with command ~ilc[ubt-prehistory].  However, even that is
  disallowed if ~c[permanent-p] is ~c[t].

  Note that the second argument of ~ilc[certify-book], which specifies the
  number of commands in the certification world (i.e., since ground-zero), is
  not sensitive to ~c[reset-prehistory]; rather, it expects the number of
  commands since ground-zero.  To see such commands,
  ~c[:]~ilc[pbt]~c[ :start].

  ~l[ubt-prehistory] for how to undo a ~c[reset-prehistory] command that does
  not have a ~c[permanent-p] of t.~/"

; Warning: See the Important Boot-Strapping Invariants before modifying!

  (declare (xargs :guard (member-eq permanent-p '(t nil))))
  (list 'reset-prehistory-fn
        (list 'quote permanent-p)
        'state
        (list 'quote doc)
        (list 'quote event-form)))

(defun reset-prehistory-fn (permanent-p state doc event-form)
  (with-ctx-summarized
   (cond ((output-in-infixp state)
          event-form)
         ((null doc)
          (msg "( RESET-PREHISTORY ~x0)" permanent-p))
         (t
          (msg "( RESET-PREHISTORY ~x0 ...)" permanent-p)))
   (let* ((wrld (w state))
          (event-form (or event-form
                          (list* 'reset-prehistory
                                 permanent-p
                                 (if doc
                                     (list :doc doc)
                                   nil))))
          (next-absolute-command-number (next-absolute-command-number wrld)))
     (install-event :new-prehistory-set
                    event-form
                    'reset-prehistory
                    0
                    nil
                    nil
                    nil
                    ctx
                    (global-set 'command-number-baseline-info
                                (change command-number-baseline-info
                                        (global-val
                                         'command-number-baseline-info
                                         wrld)
                                        :permanent-p permanent-p
                                        :current next-absolute-command-number)
                                wrld)
                    state))))


