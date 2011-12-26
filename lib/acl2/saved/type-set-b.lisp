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

;; RAG - I changed this value from 6 to 9 to make room for the
;; positive-, negative-, and complex-irrationals.

(defconst *number-of-numeric-type-set-bits*
  #+:non-standard-analysis 9
  #-:non-standard-analysis 6)

(defconst *type-set-binary-+-table-list*
  (let ((len (expt 2 *number-of-numeric-type-set-bits*)))
    (cons (list :header
                :dimensions (list len len)
                :maximum-length (1+ (* len len))
                :default *ts-acl2-number*
                :name '*type-set-binary-+-table*)
          (type-set-binary-+-alist (1- len) (1- len) nil))))

(defconst *type-set-binary-+-table*
  (compress2 'type-set-binary-+-table
             *type-set-binary-+-table-list*))

(defconst *type-set-binary-*-table-list*
  (let ((len (expt 2 *number-of-numeric-type-set-bits*)))
    (cons (list :header
                :dimensions (list len len)
                :maximum-length (1+ (* len len))
                :default *ts-acl2-number*
                :name '*type-set-binary-*-table*)
          (type-set-binary-*-alist (1- len) (1- len) nil))))

(defconst *type-set-binary-*-table*
  (compress2 'type-set-binary-*-table
             *type-set-binary-*-table-list*))

;; RAG - As a consequence of the extra numeric arguments, I had to
;; change this table from 5 to 7, to make room for the positive
;; and negative irrationals.

(defconst *type-set-<-table-list*
  #+:non-standard-analysis
  (cons (list :header
              :dimensions '(128 128)
              :maximum-length (1+ (* 128 128))
              :name '*type-set-<-table*)
        (type-set-<-alist 127 127 nil))
  #-:non-standard-analysis
  (cons (list :header
              :dimensions '(32 32)
              :maximum-length 1025
              :name '*type-set-<-table*)
        (type-set-<-alist 31 31 nil))
  )

(defconst *type-set-<-table*
  (compress2 'type-set-<-table
             *type-set-<-table-list*))

; Essay on Enabling, Enabled Structures, and Theories

; The rules used by the system can be "enabled" and "disabled".  In a
; break with Nqthm, this is true even of :COMPOUND-RECOGNIZER rules.
; We develop that code now.  Some of the fundamental concepts here are
; that of "rule names" or "runes" and their associated numeric
; correspondents, numes.  We also explain "mapping pairs," "rule name
; designators", "theories," (both "common theories" and "runic
; theories") and "enabled structures".

(defun assoc-equal-cdr (x alist)

; Like assoc-equal but compares against the cdr of each pair in alist.

  (cond ((null alist) nil)
        ((equal x (cdar alist)) (car alist))
        (t (assoc-equal-cdr x (cdr alist)))))

(defun runep (x wrld)

; This function returns non-nil iff x is a rune, i.e., a "rule name,"
; in wrld.  When non-nil, the value of this function is the nume of
; the rune x, i.e., the index allocated to this rule name in the
; enabled array.  This function returns nil on fake-runes!  See the
; essay on fake-runes below.

; To clear up the confusion wrought by the proliferation of
; nomenclature surrounding rules and the ways by which one might refer
; to them, I have recently adopted more colorful nomenclature than the
; old "rule name," "rule index," "rule id," etc.  To wit,

; rune (rule name):
; an object that is syntactically of the form (token symb . x), where
; token is one of the rule-class tokens, e.g., :REWRITE, :META, etc.,
; symb is a symbolp with a 'runic-mapping-pairs property, and x is
; either nil or a positive integer that distinguishes this rune from
; others generated from different :rule-classes that share the same
; token and symb.  We say that (token symb . x) is "based" on the
; symbol symb.  Formally, rn is a rune iff it is of the form (& symb
; . &), symb is a symbolp with a non-nil runic-info and rn is in the
; range of the mapping-pairs of that runic-info.  This is just another
; way of saying that the range of the mapping pairs of a symbol is a
; complete list of all of the runes based on that symbol.  Each rule
; in the system has a unique rune that identifies it.  The user may
; thus refer to any rule by its rune.  An ordered list of runes is a
; theory (though we also permit some non-runes in theories presented
; by the user).  Each rune has a status, enabled or disabled, as given
; by an enabled structure.  I like the name "rune" because its
; etymology (from "rule name") is clear and it connotes an object that
; is at once obscure, atomic, and yet clearly understood by the
; scholar.

; nume (the numeric counterpart of a rune):
; a nonnegative integer uniquely associated with a rune.  The nume of
; a rune tells us where in the enabled array we find the status of the
; rune.

; runic mapping pair:
; a pair, (nume . rune), consisting of a rune and its numeric
; counterpart, nume.  The 'runic-mapping-pairs property of a symbol is
; the list of all runic mapping pairs whose runes are based on the
; given symbol.  The 'runic-mapping-pairs value is ordered by
; ascending numes, i.e., the first nume in the list is the least.  The
; primary role of runic mapping pairs is to make it more efficient to
; load a theory (ideally a list of runes) into an enabled structure.
; That process requires that we assemble an ACL2 array, i.e., an alist
; mapping array indices (numes) to their values (runes) in the array.
; We do that by collecting the runic mapping pairs of each rune in the
; theory.  We also use these pairs to sort theories: theories are kept
; in descending nume order to make intersection and union easier.  To
; sort a theory we replace each rune in it by its mapping pair, sort
; the result by descending cars, and then strip out the runes to obtain
; the answer.  More on this when we discuss sort-by-fnume.

; event name:
; The symbol, symb, in a rune (token symb . x), is an event name.
; Some event names are allowed in theories, namely, those that are the
; base symbols of runes.  In such usage, an event name stands for one
; or more runes, depending on what kind of event it names.  For
; example, if APP is the name of a defun'd function, then when APP
; appears in a theory it stands for the rune (:DEFINITION APP).  If
; ASSOC-OF-APP is a lemma name, then when it appears in a theory it
; stands for all the runes based on that name, e.g., if that event
; introduced two rewrite rules and an elim rule, then ASSOC-OF-APP
; would stand for (:REWRITE ASSOC-OF-APP . 1), (:REWRITE ASSOC-OF-APP
; . 2), and (:ELIM ASSOC-OF-APP).  This use of event names allows them
; to be confused with rule names.

; Historical Footnote: In nqthm, the executable counterpart of the
; function APP actually had a distinct name, *1*APP, and hence we
; established the expectation that one could prevent the use of that
; "rule" while allowing the use of the other.  We now use the runes
; (:DEFINITION APP) and (:EXECUTABLE-COUNTERPART APP) to identify
; those two rules added by a defun event.  In fact, the defun adds a
; third rule, named by the rune (:TYPE-PRESCRIPTION APP).  In fact, we
; were driven to the invention of runes as unique rule names when we
; added type-prescription lemmas.

  (cond ((and (consp x)
              (consp (cdr x))
              (symbolp (cadr x)))
         (car
          (assoc-equal-cdr x
                           (getprop (cadr x) 'runic-mapping-pairs nil
                                    'current-acl2-world wrld))))
        (t nil)))

; Essay on Fake-Runes

; The system has many built in rules that, for regularity, ought to
; have names and numes but don't because they can never be disabled.
; In addition, we sometimes wish to have a rune-like object we can use
; as a mark, without having to worry about the possibility that a
; genuine rune will come along with the same identity.  Therefore, we
; have invented the notion of "fake-runes."  Fake runes are constants.
; By convention, the constant name is always of the form
; *fake-rune-...* and the value of every fake rune is always
; (:FAKE-RUNE-... nil).  Since no rule class will ever start with the
; words "fake rune" this convention will survive the introduction of
; all conceivable new rule classes.  It is important that fake runes
; be based on the symbol nil.  This way they are assigned the nume
; nil by fnume (below) and will always be considered enabled.  The
; function runep does NOT recognize fake runes.  Fake runes cannot be
; used in theories, etc.  

; The fake runes are:

; *fake-rune-for-anonymous-enabled-rule*
; This fake rune is specially recognized by push-lemma and ignored.  Thus, if
; you wish to invent a rule that you don't wish to name or worry will be
; disabled, give the rule this :rune and the :nume nil.

; *fake-rune-for-linear*
; This fake rune is a signal that linear arithmetic was used.

; *fake-rune-for-type-set*
; This fake rune is used by type-set to record that built-in facts about
; primitive functions were used.

; *fake-rune-for-nu-rewriter*
; This fake rune is used by the nth-update-rewriter to record the fact
; that either nth-update-nth or nth-update-nth-array was used.  We do
; not want to use either of the actual runes for those two rules
; because one or both might be disabled and it would be disturbing to
; see a rune come into a proof if it is disabled.  To turn off the
; nu-rewriter, the user should use the nu-rewriter-mode.

; WARNING: If more fake runes are added, deal with them in tilde-*-simp-phrase1.

(defmacro base-symbol (rune)

; The "base symbol" of the rune (:token symbol . x) is symbol.

; Note: The existence of this function and the next one suggest that
; runes are implemented abstractly.  Ooooo... we don't know how runes
; are realy laid out.  But this just isn't true.  We use car to get
; the token of a rune and we use cddr to get x, above.  But for some
; reason we defined and began to use base-symbol to get the base
; symbol.  In any case, if the structure of runes is changed, all
; mention of runes will have to be inspected.

  `(cadr ,rune))

(defmacro strip-base-symbols (runes)
  `(strip-cadrs ,runes))

(deflabel executable-counterpart
  :doc
  ":Doc-Section Miscellaneous

  a rule for computing the value of a function~/
  ~bv[]
  Examples:
  (:executable-counterpart length)
  ~ev[]
  which may be abbreviated in ~il[theories] as
  ~bv[]
  (length)
  ~ev[]~/

  Every ~ilc[defun] introduces at least two rules used by the theorem
  prover.  Suppose ~c[fn] is the name of a ~ilc[defun]'d function.  Then
  ~c[(:definition fn)] is the rune (~pl[rune]) naming the rule that
  allows the simplifier to replace calls of ~c[fn] by its instantiated
  body.  ~c[(:executable-counterpart fn)] is the ~il[rune] for the rule for how
  to evaluate the function on known constants.

  When typing ~il[theories] it is convenient to know that ~c[(fn)] is a runic
  designator that denotes ~c[(:executable-counterpart fn)].
  ~l[theories].

  If ~c[(:executable-counterpart fn)] is ~il[enable]d, then when applications
  of ~c[fn] to known constants are seen by the simplifier they are
  computed out by executing the Common Lisp code for ~c[fn] (with the
  appropriate handling of ~il[guard]s).  Suppose ~c[fact] is defined as the
  factorial function.  If the executable counterpart ~il[rune] of ~c[fact],
  ~c[(:executable-counterpart fact)], is ~il[enable]d when the simplifier
  encounters ~c[(fact 12)], then that term will be ``immediately''
  expanded to ~c[479001600].  Note that even if subroutines of ~c[fn] have
  disabled executable counterparts, ~c[fn] will call their Lisp code
  nonetheless:  once an executable counterpart function is applied, no
  subsidiary enable checks are made.

  Such one-step expansions are sometimes counterproductive because
  they prevent the anticipated application of certain lemmas about the
  subroutines of the expanded function.  Such computed expansions can
  be prevented by disabling the executable counterpart ~il[rune] of the
  relevant function.  For example, if ~c[(:executable-counterpart fact)]
  is ~il[disable]d, ~c[(fact 12)] will not be expanded by computation.  In this
  situation, ~c[(fact 12)] may be rewritten to ~c[(* 12 (fact 11))], using the
  rule named ~c[(:definition fact)], provided the system's heuristics
  permit the introduction of the term ~c[(fact 11)].  Note that lemmas
  about multiplication may then be applicable (while such lemmas would
  be inapplicable to ~c[479001600]).  In many proofs it is desirable to
  ~il[disable] the executable counterpart ~il[rune]s of certain functions to
  prevent their expansion by computation.
  ~l[executable-counterpart-theory].

  Finally:  What do we do about functions that are ``constrained''
  rather than defined, such as the following?  (~l[encapsulate].)
  ~bv[]
  (encapsulate (((foo *) => *))
               (local (defun foo (x) x)))
  ~ev[]
  Does ~c[foo] have an executable counterpart?  Yes:  since the vast
  majority of functions have sensible executable counterparts, it was
  decided that ~st[all] functions, even such ``constrained'' ones, have
  executable counterparts.  We essentially ``trap'' when such calls
  are inappropriate.  Thus, consider for example:
  ~bv[]
  (defun bar (x)
    (if (rationalp x)
        (+ x 1)
      (foo x)))
  ~ev[]
  If the term ~c[(bar '3)] is encountered by the ACL2 rewriter during a
  proof, and if the ~c[:executable-counterpart] of ~c[bar] is ~il[enable]d, then it
  will be invoked to reduce this term to ~c['4].  However, if the term
  ~c[(bar 'a)] is encountered during a proof, then since ~c['a] is not a
  ~ilc[rationalp] and since the ~c[:executable-counterpart] of ~c[foo] is only a
  ``trap,'' then this call of the ~c[:executable-counterpart] of ~c[bar] will
  result in a ``trap.'' In that case, the rewriter will return the
  term ~c[(hide (bar 'a))] so that it never has to go through this process
  again.  ~l[hide].~/")

(deflabel world
  :doc
  ":Doc-Section Miscellaneous

  ACL2 property lists and the ACL2 logical data base~/

  A ``world'' is a list of triples, each of the form ~c[(sym prop . val)],
  implementing the ACL2 notion of property lists.  ACL2 permits the
  simultaneous existence of many property list worlds.  ``The world''
  is often used as a shorthand for ``the ACL2 logical world'' which is
  the particular property list world used within the ACL2 system to
  maintain the data base of rules.~/

  Common Lisp provides the notion of ``property lists'' by which one
  can attach ``properties'' and their corresponding ``values'' to
  symbols.  For example, one can arrange for the ~c['color] property of
  the symbol ~c['box-14] to be ~c['purple] and the ~c['color] property of the
  symbol ~c['triangle-7] to be ~c['yellow].  Access to property lists is given
  via the Common Lisp function ~c[get].  Thus, ~c[(get 'box-14 'color)] might
  return ~c['purple].  Property lists can be changed via the special form
  ~c[setf].  Thus, ~c[(setf (get 'box-14 'color) 'blue)] changes the Common
  Lisp property list configuration so that ~c[(get 'box-14 'color)]
  returns ~c['blue].  It should be obvious that ACL2 cannot provide this
  facility, because Common Lisp's ~c[get] ``function'' is not a function
  of its argument, but instead a function of some implicit state
  object representing the property list settings for all symbols.

  ACL2 provides the functions ~c[getprop] and ~c[putprop] which allow one to
  mimic the Common Lisp property list facility.  However, ACL2's
  ~c[getprop] takes as one of its arguments a list that is a direct
  encoding of what was above called the ``state object representing
  the property list settings for all symbols.''  Because ACL2 already
  has a notion of ``~il[state]'' that is quite distinct from that used
  here, we call this property list object a ``world.''  A world is
  just a true list of triples.  Each triple is of the form
  ~c[(sym prop . val)].  This world can be thought of as a slightly
  elaborated form of association list and ~c[getprop] is a slightly
  elaborated form of ~ilc[assoc] that takes two keys.  When ~c[getprop] is
  called on a symbol, ~c[s], property ~c[p], and world, ~c[w], it
  scans ~c[w] for the first triple whose ~c[sym] is ~c[s] and ~c[prop] is
  ~c[p] and returns the corresponding ~c[val]. ~c[Getprop] has two
  additional arguments, one of which that controls what it returns if
  no such ~c[sym] and ~c[prop] exist in ~c[w], and other other of which
  allows an extremely efficient implementation.  To set some
  property's value for some symbol, ACL2 provides ~c[putprop].
  ~c[(putprop sym prop val w)] merely returns a new world, ~c[w'], in
  which ~c[(sym prop . val)] has been ~ilc[cons]ed onto the front of ~c[w],
  thus ``overwriting'' the ~c[prop] value of ~c[sym] in ~c[w] to ~c[val]
  and leaving all other properties in ~c[w] unchanged.

  One aspect of ACL2's property list arrangment is that it is possible
  to have many different property list worlds.  For example, ~c['box-14]
  can have ~c['color] ~c['purple] in one world and can have ~c['color] ~c['yes] in
  another, and these two worlds can exist simultaneously because
  ~c[getprop] is explicitly provided the world from which the property
  value is to be extracted.

  The efficiency alluded to above stems from the fact that Common Lisp
  provides property lists.  Using Common Lisp's provisions behind the
  scenes, ACL2 can ``install'' the properties of a given world into
  the Common Lisp property list state so as to make retrieval via
  ~c[getprop] very fast in the special case that the world provided to
  ~c[getprop] has been installed.  To permit more than one installed world,
  each of which is permitted to be changed via ~c[putprop], ACL2 requires
  that worlds be named and these names are used to distinquish
  installed versions of the various worlds.  At the moment we do not
  further document ~c[getprop] and ~c[putprop].

  However, the ACL2 system uses a property list world, named
  ~c['current-acl2-world], in which to store the succession of user
  ~il[command]s and their effects on the logic.  This world is often
  referred to in our ~il[documentation] as ``the world'' though it should
  be stressed that the user is permitted to have worlds and ACL2's is
  in no way distinguished except that the user is not permitted to
  modify it except via event ~il[command]s.  The ACL2 world is part of the
  ACL2 ~il[state] and may be obtained via ~c[(w state)].

  ~st[Warning]: The ACL2 world is very large.  Its length as of this
  writing (Version  2.5) is over ~c[40,000] and it grows with each release.
  Furthermore, some of the values stored in it are pointers to old
  versions of itself.  Printing ~c[(w state)] is something you should
  avoid because you likely will not have the patience to await its
  completion.  For these practical reasons, the only thing you should
  do with ~c[(w state)] is provide it to ~c[getprop], as in the form
  ~bv[]
    (getprop sym prop default 'current-acl2-world (w state))
  ~ev[]
  to inspect properties within it, or to pass it to ACL2 primitives,
  such as theory functions, where it is expected.

  Some ACL2 ~il[command] forms, such as theory expressions
  (~pl[theories]) and the values to be stored in tables
  (~pl[table]), are permitted to use the variable symbol ~c[world]
  freely with the understanding that when these forms are evaluated
  that variable is bound to ~c[(w state)].  Theoretically, this gives
  those forms complete knowledge of the current logical configuration
  of ACL2.  However, at the moment, few world scanning functions have
  been documented for the ACL2 user.  Instead, supposedly convenient
  macro forms have been created and documented.  For example,
  ~c[(current-theory :here)], which is the theory expression which returns
  the currently ~il[enable]d theory, actually macroexpands to
  ~c[(current-theory-fn :here world)].  When evaluated with ~c[world] bound to
  ~c[(w state)], ~c[current-theory-fn] scans the current ACL2 world and
  computes the set of ~il[rune]s currently ~il[enable]d in it.")

(deflabel rune
  :doc
  ":Doc-Section Theories

  a rule name~/
  ~bv[]
  Examples:
  (:rewrite assoc-of-app)
  (:linear delta-aref . 2)
  (:definition length)
  (:executable-counterpart length)
  ~ev[]~/

  Background: The theorem prover is driven from a data base of rules.  The most
  common rules are ~c[:]~ilc[rewrite] rules, which cause the simplifier to
  replace one term with another.  ~il[Events] introduce rules into the data
  base.  For example, a ~ilc[defun] event may introduce runes for symbolically
  replacing a function call by its instantiated body, for evaluating the
  function on constants, for determining the type of a call of the function,
  and for the induction scheme introduced upon defining the function.
  ~ilc[Defthm] may introduce several rules, one for each of the
  ~c[:]~ilc[rule-classes] specified (where one rule class is specified if
  ~c[:]~ilc[rule-classes] is omitted, namely, ~c[:rewrite]).

  Every rule in the system has a name.  Each name is a structured
  object called a ``rune,'' which is short for ``rule name''.  Runes
  are always of the form ~c[(:token symbol . x)], where ~c[:token] is some
  keyword symbol indicating what kind of rule is named, ~c[symbol] is the
  event name that created the rule (and is called the ``base symbol''
  of the rune), and ~c[x] is either ~c[nil] or a natural number that makes the
  rule name distinct from that of rules generated by other ~il[events] or
  by other ~c[:]~ilc[rule-classes] within the same event.

  For example, an event of the form
  ~bv[]
  (defthm name thm
    :rule-classes ((:REWRITE :COROLLARY term1)
                   (:REWRITE :COROLLARY term2)
                   (:ELIM    :COROLLARY term3)))
  ~ev[]
  typically creates three rules, each with a unique rune.  The runes are
  ~bv[]
  (:REWRITE name . 1), (:REWRITE name . 2), and (:ELIM name).
  ~ev[]
  However, a given formula may create more than one rule, and all rules
  generated by the same ~c[:corollary] formula will share the same rune.
  Consider the following example.
  ~bv[]
  (defthm my-thm
    (and (equal (foo (bar x)) x)
         (equal (bar (foo x)) x)))
  ~ev[]
  This is treated identically to the following.
  ~bv[]
  (defthm my-thm
    (and (equal (foo (bar x)) x)
         (equal (bar (foo x)) x))
    :rule-classes ((:rewrite
                    :corollary
                    (and (equal (foo (bar x)) x)
                         (equal (bar (foo x)) x)))))
  ~ev[]
  In either case, two rules are created: one rewriting ~c[(foo (bar x))] to
  ~c[x], and one rewriting ~c[(bar (foo x))] to ~c[x].  However, only a single
  rune is created, ~c[(:REWRITE MY-THM)], because there is only one rule class.
  But now consider the following example.
  ~bv[]
  (defthm my-thm2
    (and (equal (foo (bar x)) x)
         (equal (bar (foo x)) x))
    :rule-classes ((:rewrite
                    :corollary
                    (and (equal (foo (bar x)) x)
                         (equal (bar (foo x)) x)))
                   (:rewrite
                    :corollary
                    (and (equal (foo (bar (foo x))) (foo x))
                         (equal (bar (foo (bar x))) (bar x))))))
  ~ev[]
  This time there are four rules created.  The first two rules are as before,
  and are assigned the rune ~c[(:REWRITE MY-THM . 1)].  The other two rules are
  similarly generated for the second ~c[:corollary], and are assigned the rune
  ~c[(:REWRITE MY-THM . 2)].

  The function ~ilc[corollary] will return the ~il[corollary] term associated
  with a given rune in a given ~il[world].  Example:
  ~bv[]
  (corollary '(:TYPE-PRESCRIPTION DIGIT-TO-CHAR) (w state))
  ~ev[]
  However, the preferred way to see the corollary term associated with
  a rune or a name is to use ~c[:pf]; ~pl[pf].

  The ~ilc[defun] event creates as many as four rules.  ~c[(:definition fn)] is
  the rune given to the equality axiom defining the function, ~c[fn].
  ~c[(:executable-counterpart fn)] is the rune given to the rule for computing
  ~c[fn] on known arguments.  A type prescription rule may be created under the
  name ~c[(:type-prescription fn)], and an ~il[induction] rule may be created under
  the name ~c[(:induction fn)].

  Runes may be individually ~il[enable]d and ~il[disable]d, according to whether
  they are included in the current theory.  ~l[theories].  Thus,
  it is permitted to ~il[disable] ~c[(:elim name)], say, while enabling the
  other rules derived from name.  Similarly, ~c[(:definition fn)] may be
  ~il[disable]d while ~c[(:executable-counterpart fn)] and the type
  prescriptions for ~c[fn] are ~il[enable]d.

  Associated with most runes is the formula justifying the rule named.  This is
  called the ``~il[corollary] formula'' of the rune and may be obtained via the
  function ~ilc[corollary], which takes as its argument a rune and a property
  list ~il[world].  Also ~pl[pf].  The ~il[corollary] formula for ~c[(:rewrite name . 1)]
  after the ~ilc[defthm] event above is ~c[term1].  The corollary formulas for
  ~c[(:definition fn)] and ~c[(:executable-counterpart fn)] are always
  identical: the defining axiom.  Some runes, e.g., ~c[(:definition car)], do
  not have corollary formulas.  ~ilc[Corollary] returns ~c[nil] on such runes.
  In any case, the corollary formula of a rune, when it is non-~c[nil], is a
  theorem and may be used in the ~c[:use] and ~c[:by] ~il[hints].

  Note: The system has many built in rules that, for regularity, ought
  to have names but don't because they can never be ~il[disable]d.  One
  such rule is that implemented by the linear arithmetic package.
  Because many of our subroutines are required by their calling
  conventions to return the justifying rune, we have invented the
  notion of ``fake runes.'' Fake runes always have the base symbol
  ~c[nil], use a keyword token that includes the phrase ``fake-rune'', and
  are always ~il[enable]d.  For example, ~c[(:fake-rune-for-linear nil)] is a
  fake rune.  Occasionally the system will print a fake rune where a
  rune is expected.  For example, when the linear arithmetic fake rune
  is reported among the rules used in a proof, it is an indication
  that the linear arithmetic package was used.  However, fake runes
  are not allowed in ~il[theories], they cannot be ~il[enable]d or ~il[disable]d, and
  they do not have associated ~il[corollary] formulas.  In short, despite
  the fact that the user may sometimes see fake runes printed, they
  should never be typed.~/")

(deflabel rule-names
  :doc
  ":Doc-Section Theories

  How rules are named.~/
  ~bv[]
  Examples:
  (:rewrite assoc-of-app)
  (:linear delta-aref . 2)
  (:definition length)
  (:executable-counterpart length)
  ~ev[]~/

  ~l[rune].~/")

(defun fnume (rune wrld)

; Rune has the shape of a rune.  We return its nume.  Actually, this function
; admits every fake-rune as a "rune" and returns nil on them (by virtue of the
; fact that the base-symbol of a fake rune is nil and hence there are no
; mapping pairs).  This fact may be exploited by functions which have obtained
; a fake rune as the name of some rule and wish to know its nume so they can
; determine if it is enabled.  More generally, this function returns nil if
; rune is not a rune in the given world, wrld.  Nil is treated as an enabled
; nume by enabled-runep but not by active-runep.

  (car
   (assoc-equal-cdr rune
                    (getprop (base-symbol rune) 'runic-mapping-pairs nil
                             'current-acl2-world wrld))))

(defun frunic-mapping-pair (rune wrld)

; Rune must be a rune in wrld.  We return its mapping pair.

  (assoc-equal-cdr rune
                   (getprop (base-symbol rune) 'runic-mapping-pairs nil
                            'current-acl2-world wrld)))

(defun fn-rune-nume (fn nflg xflg wrld)

; Fn must be a function symbol, not a lambda expression.  We return
; either the rune (nflg = nil) or nume (nflg = t) associated with
; either (:DEFINITION fn) (xflg = nil) or (:EXECUTABLE-COUNTERPART fn)
; (xflg = t).  This function knows the layout of the runic mapping
; pairs by DEFUNS -- indeed, it knows the layout for all function
; symbols whether DEFUNd or not!  See the Essay on the Assignment of
; Runes and Numes by DEFUNS.  If fn is a constrained function we
; return nil for all combinations of the flags.

  (let* ((runic-mapping-pairs
          (getprop fn 'runic-mapping-pairs nil 'current-acl2-world wrld))
         (pair (if xflg (cadr runic-mapping-pairs) (car runic-mapping-pairs))))
    (if nflg (car pair) (cdr pair))))

(defun definition-runes (fns xflg wrld)
  (cond ((null fns) nil)
        (t (cons (fn-rune-nume (car fns) nil xflg wrld)
                 (definition-runes (cdr fns) xflg wrld)))))

(defun get-next-nume (lst)

; We return the next available nume in lst, which is a cdr of the
; current world.  We scan down lst, looking for the most recently
; stored 'runic-mapping-pairs entry.  Suppose we find it, ((n1 .
; rune1) (n2 . rune2) ... (nk . runek)).  Then the next rune will get
; the nume nk+1, which is also just n1+k, where k is the length of the
; list of mapping pairs.  Note: If we see (name runic-mapping-pairs .
; atm) where atm is an atom, then atm is :acl2-property-unbound or
; nil, and we keep going.  Such tuples appear in part because in
; redefinition the 'runic-mapping-pairs property is "nil'd" out.

  (cond ((null lst)
         #+acl2-metering (meter-maid 'get-next-nume 100)
         0)
        ((and (eq (cadr (car lst)) 'runic-mapping-pairs)
              (consp (cddr (car lst))))
         #+acl2-metering (meter-maid 'get-next-nume 100)
         (+ (car (car (cddr (car lst))))
            (length (cddr (car lst)))))
        (t
         #+acl2-metering (setq meter-maid-cnt (1+ meter-maid-cnt))
         (get-next-nume (cdr lst)))))

; We now formalize the notion of "theory".  We actually use two
; different notions of theory here.  The first, which is formalized by
; the predicate theoryp, is what the user is accustomed to thinking of
; as a theory.  Formally, it is a truelist of rule name designators,
; each of which designates a set of runes.  The second is what we
; call a "runic theory" which is an ordered list of runes, where the
; ordering is by descending numes.  We sometimes refer to theories as
; "common theories" to distinguish them from runic theories.  To every
; common theory there corresponds a runic theory obtained by unioning
; together the runes designated by each element of the common theory.
; We call this the runic theory "corresponding" to the common one.

(defun deref-macro-name (macro-name macro-aliases)
  (let ((entry (assoc-eq macro-name macro-aliases)))
    (if entry
        (cdr entry)
      macro-name)))

(defun rule-name-designatorp (x macro-aliases wrld)

; A rule name designator is an object which denotes a set of runes.
; We call that set of runes the "runic interpretation" of the
; designator.  A rune, x, is a rule name designator, denoting {x}.  A
; symbol, x, with a 'runic-mapping-pairs property is a designator and
; denotes either {(:DEFINITION x)} or else the entire list of runes in
; the runic-mapping-pairs, depending on whether there is a :DEFINITION
; rune.  A symbol x that is a theory name is a designator and denotes
; the runic theory value.  Finally, a singleton list, (fn), is a
; designator if fn is a function symbol; it designates
; {(:EXECUTABLE-COUNTERPART fn)}.

; For example, if APP is a function symbol then its runic
; interpretation is {(:DEFINITION APP)}.  If ASSOC-OF-APP is a defthm
; event with, say, three rule classes then its runic interpretation is
; a set of three runes, one for each rule generated.  The idea here is
; to maintain some consistency with the Nqthm way of disabling names.
; If the user disables APP then only the symbolic definition is
; disabled, not the executable counterpart, while if ASSOC-OF-APP is
; disabled, all such rules are disabled.

; Note: We purposely do not define a function "runic-interpretation"
; which returns runic interpretation of a designator.  The reason is
; that we would have to cons that set up for every designator except
; theories.  The main reason we'd want such a function is to define
; the runic theory corresponding to a common one.  We do that below
; (in convert-theory-to-unordered-mapping-pairs1) and open-code "runic
; interpretation."

  (cond ((symbolp x)
         (cond
          ((getprop (deref-macro-name x macro-aliases) 'runic-mapping-pairs nil
                    'current-acl2-world wrld)
           t)
          (t (not (eq (getprop x 'theory t 'current-acl2-world wrld) t)))))
        ((and (consp x)
              (null (cdr x))
              (symbolp (car x)))
         (let ((fn (deref-macro-name (car x) macro-aliases)))
           (and (function-symbolp fn wrld)
                (runep (list :executable-counterpart fn) wrld))))
        (t (runep x wrld))))

(defun theoryp1 (lst macro-aliases wrld)
  (cond ((atom lst) (null lst))
        ((rule-name-designatorp (car lst) macro-aliases wrld)
         (theoryp1 (cdr lst) macro-aliases wrld))
        (t nil)))

(defun theoryp (lst wrld)

; A (common) theory is a truelist of rule name designators.  It is
; possible to turn a theory into a list of runes (which is, itself, a
; theory).  That conversion is done by coerce-to-runic-theory.

  (theoryp1 lst (macro-aliases wrld) wrld))

(defun theoryp!1 (lst fail-flg macro-aliases wrld)
  (cond ((atom lst) (and (not fail-flg) (null lst)))
        ((rule-name-designatorp (car lst) macro-aliases wrld)
         (theoryp!1 (cdr lst) fail-flg macro-aliases wrld))
        ((and (symbolp (car lst))

; Do not use the function macro-args below, as it can cause a hard error!

              (not (eq (getprop (car lst) 'macro-args
                                t
                                'current-acl2-world wrld)
                       t)))
         (prog2$ (cw "~|~%**NOTE**:  The name ~x0 is a macro.  See :DOC ~
                      add-macro-alias if you want it to be associated with a ~
                      function name."
                     (car lst))
                 (theoryp!1 (cdr lst) t macro-aliases wrld)))
        (t (prog2$ (cw "~|~%**NOTE**:~%The name ~x0 does not ~
                        designate a rule or non-empty list of rules."
                       (car lst))
                   (theoryp!1 (cdr lst) t macro-aliases wrld)))))

(defun theoryp! (lst wrld)
  (theoryp!1 lst nil (macro-aliases wrld) wrld))

; Now we define what a "runic theory" is.

(defun runic-theoryp1 (prev-nume lst wrld)

; We check that lst is an ordered true list of runes in wrld, where
; the ordering is by descending numes.  Prev-nume is the nume of the
; previously seen element of lst (or nil if we are at the top-level).

  (cond ((atom lst) (null lst))
        (t
         (let ((nume (runep (car lst) wrld)))
           (cond ((and nume
                       (or (null prev-nume)
                           (> prev-nume nume)))
                  (runic-theoryp1 nume (cdr lst) wrld))
                 (t nil))))))

(defun runic-theoryp (lst wrld)

; A "runic theory" (wrt wrld) is an ordered truelist of runes (wrt wrld), where
; the ordering is that imposed by the numes of the runes, greatest numes first.
; This function returns t or nil according to whether lst is a runic theory in
; wrld.  Common theories are converted into runic-theories in order to do such
; operations as union and intersection.  Our theory processing functions all
; yield runic-theories.  We can save some time in those functions by checking
; if an input theory is in fact a runic theory: if so, we need not sort it.

  (runic-theoryp1 nil lst wrld))

; When we start manipulating theories, e.g., unioning them together,
; we will actually first convert common theories into runic theories.
; We keep runic theories ordered so it is easier to intersect and
; union them.  However, this raises a slighly technical question,
; namely the inefficiency of repeatedly going to the property lists of
; the basic symbols of the runes to recover (by a search through the
; mapping pairs) the measures by which we compare runes (i.e., the
; numes).  We could order theories lexicographically -- there is no
; reason that theories have to be ordered by nume until it is time to
; load the enabled structure.  We could also obtain the measure of
; each rune and cons the two together into a mapping pair and sort
; that list on the cars.  This would at least save the repeated
; getprops at the cost of copying the list twice (once to pair the
; runes with their numes and once to strip out the final list of
; runes).

; We have compared these three schemes in a slightly simpler setting:
; sorting lists of symbols.  The sample list was the list of all event
; names in the initial world, i.e., every symbol in the initial world
; with an 'absolute-event-number property.  The lexicographic
; comparison was done with string<.  The measure (analogous to the
; nume) was the 'absolute-event-number.  We used exactly the same tail
; recursive merge routine used here, changing only the comparator
; expression.  The version that conses the nume to the rune before
; sorting paid the price of the initial and final copying.  The times
; to sort the 2585 symbols were:

; lexicographic:  1.29 seconds
; getprops:       1.18 seconds
; cars:           0.68 seconds

; We have decided to go the car route.  The argument that it does a
; lot of unnecessary consing is unpersuasive in light of the amount of
; consing done by sorting.  For example right off the bat in sort we
; divide the list into its evens and odds, thus effectively copying
; the entire list.  The point is that as it has always been coded,
; sort produces a lot of garbaged conses, so it is not as though
; copying the list twice is a gross insult to the garbage collector.

; We exhibit some performance measures of our actual theory manipulation
; functions later.  See Essay on Theory Manipulation Performance.

; Consider a runic theory.  We want to "augment" it by consing onto
; every rune its nume.  Common theories cannot be augmented until they
; are converted into runic ones.  Naively, then we want to consider two
; transformations: how to convert a common theory to a runic one, and
; how to augment a runic theory.  It turns out that the first
; transformation is messier than you'd think due to the possibility
; that distinct elements of the common theory designate duplicate
; runes.  More on this later.  But no matter what our final design, we
; need the second capability, since we expect that the theory
; manipulation functions will often be presented with runic theories.
; So we begin by augmentation of runic theories.

; Our goal is simply to replace each rune by its frunic-mapping-pair.
; But frunic-mapping-pair has to go to the property list of the basic
; symbol of the rune and then search through the 'runic-mapping-pairs
; for the pair needed.  But in a runic theory, it will often be the
; case that adjacent runes have the same symbol, e.g., (:REWRITE LEMMA
; . 1), (:REWRITE LEMMA . 2), ...  Furthermore, the second rune will
; occur downstream of the first in the 'runic-mapping-pairs of their
; basic symbol.  So by keeping track of where we found the last
; mapping pair we may be able to find the next one faster.

(defun find-mapping-pairs-tail1 (rune mapping-pairs)

; Rune is a rune and mapping-pairs is some tail of the
; 'runic-mapping-pairs property of its basic symbol.  Furthermore, we
; know that we have not yet passed the pair for rune in mapping-pairs.
; We return the tail of mapping-pairs whose car is the pair for rune.

  (cond ((null mapping-pairs)
         (er hard 'find-mapping-pairs-tail
             "We have exhausted the mapping-pairs of the basic symbol ~
              of ~x0 and failed to find that rune."
             rune))
        ((equal rune (cdr (car mapping-pairs))) mapping-pairs)
        (t (find-mapping-pairs-tail1 rune (cdr mapping-pairs)))))

(defun find-mapping-pairs-tail (rune mapping-pairs wrld)

; Rune is a rune and mapping-pairs is some tail of the
; 'runic-mapping-pairs property of some basic symbol -- but not
; necessarily rune's.  If it is rune's then rune has not yet been seen
; among those pairs.  If it is not rune's, then we get rune's from
; world.  In any case, we return a mapping-pairs list whose car is the
; mapping pair for rune.

  (cond ((and mapping-pairs
              (eq (base-symbol rune) (cadr (cdr (car mapping-pairs)))))
         (find-mapping-pairs-tail1 rune mapping-pairs))
        (t (find-mapping-pairs-tail1 rune
                                     (getprop (base-symbol rune)
                                              'runic-mapping-pairs nil
                                              'current-acl2-world wrld)))))

(defun augment-runic-theory1 (lst mapping-pairs wrld ans)

; Lst is a runic theory.  We iteratively accumulate onto ans the
; mapping pair corresponding to each element of lst.  Mapping-pairs is
; the tail of some 'runic-mapping-pairs property and is used to speed
; up the retrieval of the pair for the first rune in lst.  See
; find-mapping-pairs-tail for the requirements on mapping-pairs.  The
; basic idea is that as we cdr through lst we also sweep through
; mapping pairs (they are ordered the same way).  When the rune we get
; from lst is based on the same symbol as the last one, then we find
; its mapping pair in mapping-pairs.  When it is not, we switch our
; attention to the 'runic-mapping-pairs of the new basic symbol.

  (cond
   ((null lst) ans)
   (t (let ((mapping-pairs
             (find-mapping-pairs-tail (car lst) mapping-pairs wrld)))
        (augment-runic-theory1 (cdr lst)
                               (cdr mapping-pairs)
                               wrld
                               (cons (car mapping-pairs) ans))))))

(defun augment-runic-theory (lst wrld)

; We pair each rune in the runic theory lst with its nume, returning an
; augmented runic theory.

  (augment-runic-theory1 (reverse lst) nil wrld nil))

; Ok, so now we know how to augment a runic theory.  How about
; converting common theories to runic ones?  That is harder because of
; the duplication problem.  For example, '(APP APP) is a common
; theory, but the result of replacing each designator by its rune,
; '((:DEFINITION app) (:DEFINITION app)), is not a runic theory!  It
; gets worse.  Two distict designators might designate the same rune.
; For example, LEMMA might designate a collection of :REWRITE rules
; while (:REWRITE LEMMA . 3) designates one of those same rules.  To
; remove duplicates we actually convert the common theory first to a
; list of (possibly duplicated and probably unordered) mapping pairs
; and then use a bizarre sort routine which removes duplicates.  While
; converting a common theory to a unordered and duplicitous list of
; mapping pairs we simply use frunic-mapping-pair to map from a rune
; to its mapping pair; that is, we don't engage in the clever use of
; tails of the mapping pairs properties because we don't expect to see
; too many runes in a common theory, much less for two successive
; runes to be ordered properly.

(defconst *bad-runic-designator-string*
  "This symbol was expected to be suitable for theory expressions; see :DOC ~
   theories, in particular the discussion of runic designators.  One possible ~
   source of this problem is an attempt to include an uncertified book with a ~
   deftheory event that attempts to use the above symbol in a deftheory event.")

(defun convert-theory-to-unordered-mapping-pairs1 (lst macro-aliases wrld ans)

; This is the place we give meaning to the "runic interpretation" of a
; rule name designator.  Every element of lst is a rule name
; designator.

  (cond
   ((null lst) ans)
   ((symbolp (car lst))
    (let ((temp (getprop (deref-macro-name (car lst) macro-aliases)
                         'runic-mapping-pairs nil
                         'current-acl2-world wrld)))
      (cond
       ((and temp
             (eq (car (cdr (car temp))) :DEFINITION)
             (eq (car (cdr (cadr temp))) :EXECUTABLE-COUNTERPART))
        (convert-theory-to-unordered-mapping-pairs1
         (cdr lst) macro-aliases wrld
         (if (equal (length temp) 4)

; Then we have an :induction rune.  See the Essay on the Assignment of Runes
; and Numes by DEFUNS.

             (cons (car temp) (cons (cadddr temp) ans))
           (cons (car temp) ans))))
       (temp
        (convert-theory-to-unordered-mapping-pairs1
         (cdr lst) macro-aliases wrld (revappend temp ans)))
       (t

; In this case, we know that (car lst) is a theory name.  Its 'theory
; property is the value of the theory name and is a runic theory.  We
; must augment it.  The twisted use of ans below -- passing it into
; the accumulator of the augmenter -- is permitted since we don't care
; about order.

        (convert-theory-to-unordered-mapping-pairs1
         (cdr lst) macro-aliases wrld
         (augment-runic-theory1
          (reverse (getprop (car lst) 'theory
                            `(:error ,*bad-runic-designator-string*)
                            'current-acl2-world wrld))
          nil
          wrld
          ans))))))
   ((null (cdr (car lst)))
    (convert-theory-to-unordered-mapping-pairs1
     (cdr lst) macro-aliases wrld
     (cons (cadr (getprop (deref-macro-name (car (car lst)) macro-aliases)
                          'runic-mapping-pairs
                          `(:error ,*bad-runic-designator-string*)
                          'current-acl2-world wrld))
           ans)))
   (t (convert-theory-to-unordered-mapping-pairs1
       (cdr lst) macro-aliases wrld
       (cons (frunic-mapping-pair (car lst) wrld)
             ans)))))

(defun convert-theory-to-unordered-mapping-pairs (lst wrld)

; This function maps a common theory into a possibly unordered and/or
; duplicitous list of mapping pairs.

  (convert-theory-to-unordered-mapping-pairs1
   lst (macro-aliases wrld) wrld nil))

; Now we develop a merge sort routine that has four interesting
; properties.  First, it sorts arbitrary lists of pairs, comparing on
; their cars which are assumed to be rationals.  Second, it can be
; told whether to produce an ascending order or a descending order.
; Third, it deletes all but one occurrence of any element with the
; same car as another.  Fourth, its merge routine is tail recursive
; and so can handle very long lists.  (The sort routine is not tail
; recursive, but it cuts the list in half each time and so can handle
; long lists too.)

(defun duplicitous-cons-car (x y)

; This is like (cons x y) in that it adds the element x to the list y,
; except that it does not if the car of x is the car of the first element
; of y.

  (cond ((equal (car x) (caar y)) y)
        (t (cons x y))))

(defun duplicitous-revappend-car (lst ans)

; Like revappend but uses duplicitous-cons-car rather than cons.

  (cond ((null lst) ans)
        (t (duplicitous-revappend-car (cdr lst)
                                      (duplicitous-cons-car (car lst) ans)))))

(defun duplicitous-merge-car (parity lst1 lst2 ans)

; Basic Idea: Lst1 and lst2 must be appropriately ordered lists of
; pairs.  Comparing on the cars of respective pairs, we merge the two
; lists, deleting all but one occurrence of any element with the same
; car as another.

; Terminology: Suppose x is some list of pairs and that the car of
; each pair is a rational.  We say x is a "measured list" because the
; measure of each element is given by the car of the element.  We
; consider two orderings of x.  The "parity t" ordering is that in
; which the cars of x are ascending.  The "parity nil" ordering is
; that in which the cars of x are descending.  E.g., in the parity t
; ordering, the first element of x has the least car and the last
; element of x has the greatest.

; Let lst1 and lst2 be two measured lists.  This function merges lst1
; and lst2 to produce output in the specified parity.  However, it
; assumes that its two main inputs, lst1 and lst2, are ordered in the
; opposite parity.  That is, if we are supposed to produce output that
; is ascending (parity = t) then the input must be descending (parity
; = nil).  This odd requirement allows us to do the merge in a tail
; recursive way, accumulating the answers onto ans.  We do it tail
; recursively because we are often called upon to sort huge lists and
; the naive approach has blown the stack of AKCL.

  (cond ((null lst1) (duplicitous-revappend-car lst2 ans))
        ((null lst2) (duplicitous-revappend-car lst1 ans))
        ((if parity
             (> (car (car lst1)) (car (car lst2)))
             (< (car (car lst1)) (car (car lst2))))
         (duplicitous-merge-car parity (cdr lst1) lst2 (duplicitous-cons-car (car lst1) ans)))
        (t (duplicitous-merge-car parity lst1 (cdr lst2) (duplicitous-cons-car (car lst2) ans)))))

(defun duplicitous-sort-car (parity lst)

; Let lst be a list of runes.  If parity = t, we sort lst so that the
; numes of the resulting list are ascending; if parity = nil, the
; numes of the resulting list are descending.

; Note: This function is neat primarily because the merge function is
; tail recursive.  It is complicated by the entirely extraneous
; requirement that it delete duplicates.  The neat thing is that as it
; descends through lst, cutting it in half each time, it recursively
; orders the parts with the opposite sense of ordering.  That is, to
; sort into ascending order it recursively sorts the two parts into
; descending order, which it achieves by sorting their parts into
; ascending order, etc.

  (cond ((null (cdr lst)) lst)
        (t (duplicitous-merge-car parity
                                  (duplicitous-sort-car (not parity) (evens lst))
                                  (duplicitous-sort-car (not parity) (odds lst))
                                  nil))))

(defun augment-theory (lst wrld)

; Given a (common) theory we convert it into an augmented runic
; theory.  That is, we replace each designator in lst by the
; appropriate runes, pair each rune with its nume, sort the result and
; remove duplications.  In the special case that lst is in fact a
; runic theory -- i.e., is already a properly sorted list of runes --
; we just augment it directly.  We expect this case to occur often.
; The various theory manipulation functions take common theories as
; their inputs but produce runic theories as their outputs.
; Internally, they all operate by augmenting the input theory,
; computing with the augmented theory, and then dropping down to the
; corresponding runic theory at the end with strip-cdrs.  Thus if two
; such functions are nested in a user-typed theory expression, the
; inner one will generally have non-runic user-typed input but will
; produce runic output as input for the next one.  By recognizing
; runic theories as a special case we hope to improve the efficiency
; with which theory expressions are evaluated, by saving the sorting.

  (declare (xargs :guard (theoryp lst wrld)))
  (cond ((runic-theoryp lst wrld)
         (augment-runic-theory lst wrld))
        (t (duplicitous-sort-car
            nil
            (convert-theory-to-unordered-mapping-pairs lst wrld)))))

(defmacro assert$-runic-theoryp (runic-theory-expr wrld)

; Comment out one of the following two definitions.

;;; Faster, without checking:
  (declare (ignore wrld))
  runic-theory-expr

;;; Slower, with checking:
;  `(let ((thy ,runic-theory-expr))
;     (assert$ (runic-theoryp thy ,wrld)
;              thy))
  )

(defun runic-theory (lst wrld)

; Lst is a common theory.  We convert it to a runic theory.

  (cond ((runic-theoryp lst wrld) lst)
        (t (assert$-runic-theoryp
            (strip-cdrs
             (duplicitous-sort-car
              nil
              (convert-theory-to-unordered-mapping-pairs lst wrld)))
            wrld))))

; We now develop the foundations of the concept that a rune is
; "enabled" in the current theory.  In ACL2, the user can get "into" a
; theory with the in-theory event, which is similar in spirit to
; in-package but selects a theory as the "current" theory.  A rune is
; said to be "enabled" if it is a member of the runic theory
; corresponding to the current (common) theory and is said to be
; "disabled" otherwise.

; Historical Note about Nqthm

; Nqthm had no explicit notion of the current theory.  However,
; implicitly, nqthm contained a current theory and the events ENABLE
; and DISABLE allowed the user to add a name to it or delete a name
; from it.  The experimental xnqthm, mentioned elsewhere in this
; system, introduced the notion of theories and tied them to enabling
; and disabling, following suggestions and patches implemented by Bill
; Bevier during the Kit proofs (and implemented in Pc-Nqthm, and
; extended in Nqthm-1992).  The ACL2 notion of theory is much richer
; because it allows one to compute the value of theories using
; functions defined within the logic.  (end of note)

; Suppose we have a theory which has been selected as current.  This
; may be the globally current theory, as set by the in-theory event,
; or it may be a locally current theory, as set by the in-theory hint
; to defthm or defun.  We must somehow process the current theory so
; that we can quickly answer the question "is rune enabled?"  We now
; develop the code for doing that.

; The structure defined below is used as a fast way to represent a
; theory:

(defrec enabled-structure

; WARNING:  Keep this in sync with enabled-structurep.

  ((index-of-last-enabling . theory-array)
   (array-name . array-length)
   array-name-root . array-name-suffix)
  t)

; The following invariant is maintained in all instances of this
; structure.  Theory-array is an array1p whose array length is
; array-length.  Furthermore array-name is a symbol of the form rootj,
; root is the array-name-root (as a list of characters) and j is the
; array-name-suffix.  Thus, if i is a nonnegative integer less than
; array-length, then (acl2-aref1 array-name theory-array i) has a
; satisfied guard.  Furthermore, important to efficiency but
; irrelevant to correctness, it will always be the case that the von
; Neumann array associated with array-name is in fact theory-array.
; Thus the above expression executes quickly.  To get a new array
; name, should one ever be needed, it suffices to increment the
; array-name-suffix and built a name from that new value.

; The theory-array of an enabled-structure for a given common theory
; is (except for the header entry) just the augmented runic theory
; corresponding to the given common theory.  That is, the ACL2 array
; alist we need to construct for a theory maps each array index to a
; non-nil value.  The non-nil value we choose is in fact the
; corresponding rune.  It would suffice, for purposes of enabling, to
; store T in the array to signify enabledness.  By storing the rune
; itself we make it possible to determine what runic theory is in the
; array.  (There is no general purpose way to map from a nume to its
; rune (short of looking through the whole world).)

; The global variable 'global-enabled-structure contains an instance
; of the enabled-structure record in which the array-name is
; ENABLED-ARRAY-0, array-name-root is the list of characters in
; "ENABLED-ARRAY-" and the array-name-suffix is 0.  A rune with
; nume n is (globally) enabled in the current world iff either n is
; greater than the index-of-last-enabling or array[n] is non-nil.
; This is just the computation done by enabled-numep, below.

; The in-theory event loads the 'global-enabled-structure with
; the theory-value and sets the index-of-last-enabling to the maximum
; nume at that time.  This structure is passed into prove and
; thus into rewrite, etc.

; When an in-theory hint setting is provided we change the array name
; from ENABLED-ARRAY-j to ENABLED-ARRAY-j+1 (changing suffix
; appropriately) and load the local theory into that structure.  After
; we have done a few proofs with local in-theory hint settings, these
; auxiliary arrays will have been allocated and won't be deallocated.

; Historical Note about Nqthm

; In nqthm we solved this problem by having a list of temporarily
; disabled names which was bound when there were local enabling hint
; settings.  That implementation suffered because if hundreds of names
; were enabled locally the time spent searching the list was
; excessive.  In xnqthm we solved that problem by storing the enabled
; status on the property list of each name.  We could do that here.
; However, that implementation suffered from the fact that if a proof
; attempt was aborted then we had to carefully clean up the property
; list structures so that they once again reflected the current
; (global) theory.  The beauty of the ACL2 approach is that local hint
; settings have no affect on the global theory and yet involve no
; overhead.

; A delicacy of the current implementation however concerns the
; relation between the global enabled structure and undoing.  If the
; world is backed up to some previous point, the
; 'global-enabled-structure extant there is exposed and we are
; apparently ready to roll.  However, the von Neumann array named by
; that structure may be out-dated in the sense that it contains a now
; undone theory.  Technically there is nothing wrong with this, but if
; we let it persist things would be very slow because the attempt to
; access the applicative array would detect that the von Neumann array
; is out of date and would result in a linear search of the
; applicative array.  We must therefore compress the applicative array
; (and hence reload the von Neumann one) whenever we back up.

; Finally, there is one last problem.  Eventually the array-size of
; the array in one of these structures will be too small.  This
; manifests itself when the maximum rule index at the time we load the
; structure is equal to or greater than the array-length.  At that
; time we grow the array size by 500.

; Here is how we use an enabled structure, ens, to determine if a nume,
; rune, or function is enabled.

(defun enabled-numep (nume ens)

; This function takes a nume (or nil) and determines if it is enabled
; in the enabled structure ens.  We treat nil as though it were
; enabled.

  (cond ((null nume) t)
        ((> (the-fixnum nume)
            (the-fixnum
             (access enabled-structure ens :index-of-last-enabling)))
         t)
        (t (aref1 (access enabled-structure ens :array-name)
                  (access enabled-structure ens :theory-array)
                  nume))))

(defun enabled-arith-numep (nume ens)

; This function takes a nume (or nil) and determines if it is enabled
; in the enabled structure ens.  We treat nil as though it were
; enabled.  In current usage, ens is always the global arithmetic
; theory.  Any nume created since the most recent in-arithmetic-theory
; is considered disabled.  The normal enabled-numep would treat these
; more recent numes as enabled.

  (cond ((null nume) t)
        ((> nume (access enabled-structure ens :index-of-last-enabling))
         nil)
        (t (aref1 (access enabled-structure ens :array-name)
                  (access enabled-structure ens :theory-array)
                  nume))))

(defun enabled-runep (rune ens wrld)

; This takes a rune and determines if it is enabled in the enabled structure
; ens.  Since fnume returns nil on fake-runes, this function answers that a
; fake rune is enabled.  See also active-runep.

  (enabled-numep (fnume rune wrld) ens))

(defmacro active-runep (rune)

; This takes a rune and determines if it is enabled in the enabled structure
; ens.  Unlike enabled-runep, this returns nil if the rune is a fake-rune or is
; not a runep in the given wrld.

  ":Doc-Section Theories

  check that a ~il[rune] exists and is ~il[enable]d~/
  ~bv[]
  Example:
  (active-runep '(:rewrite left-to-right))~/

  General Form:
  (active-runep rune)
  ~ev[]
  where ~c[rune] has the shape of a ~il[rune].  This macro expands to an
  expression using the variables ~c[ens] and ~c[state], and returns non-~c[nil]
  when the given rune exists and is ~il[enable]d (according to the given
  ``enabled structure,'' ~c[ens], and the current logical ~il[world] of the
  given ~ilc[state]).  ~l[theory-invariant] for how this macro can be of
  use.~/"

  `(let* ((rune ,rune)
          (nume (and (consp rune)
                     (consp (cdr rune))
                     (symbolp (cadr rune))

; The tests above guard the call of fnume just below, the same way that runep
; guards the computation made in its body from the property list.

                     (fnume rune (w state)))))
     (and nume
          (enabled-numep nume ens))))

(defun enabled-xfnp (fn ens wrld)

; Fn must be either a function symbol or lambda expression, i.e., something you
; might get from ffn-symb of a term.  If fn is a lambda expression or a
; constrained function symbol, we return t.  Otherwise, we consider
; (:EXECUTABLE-COUNTERPART fn), and answer whether it is enabled.

; Note: This function exploits the fact that nil is considered enabled by
; enabled-numep.

; Note: Suppose you want to determine whether (:DEFINITION fn) is enabled.
; Perhaps you really want to know if the latest definition rule for fn that has
; non-nil :install-body field is enabled; and you may also want to ask other
; questions about the def-body of fn.  Then this function is not the one
; to use!

  (cond ((flambdap fn) t)
        (t (enabled-numep (fn-rune-nume fn t t wrld) ens))))

; Before we develop the code for loading a theory into an enabled
; structure, we put down code for warning when leaving a 0-ary
; function disabled while its executable counterpart is enabled.

(defun theory-warning-fns-aux (runes1 runes2 max-nume
                                      nume prev-rune1 prev-rune2 w acc)

; See the comment in theory-warning-fns for a general discussion, in particular
; of (1), (2), and (3) below.  We apply reverse to the returned accumulator in
; order to return function symbols in the order in which they were defined (a
; minor aesthetic preference).

  (declare (type (signed-byte 29) nume))
  (cond
   ((eql nume max-nume)
    (reverse acc))
   (t
    (let* ((found1 (eql (caar runes1) nume))
           (found2 (eql (caar runes2) nume))
           (curr-rune1 (and found1 (cdar runes1)))
           (curr-rune2 (and found2 (cdar runes2)))
           (rest-runes1 (if found1 (cdr runes1) runes1))
           (rest-runes2 (if found2 (cdr runes2) runes2)))
      (theory-warning-fns-aux
       rest-runes1 rest-runes2 max-nume (1+f nume) curr-rune1 curr-rune2 w
       (if (and (eq (car curr-rune2) :executable-counterpart)
                (null prev-rune2)                        ; (1)
                (not (and curr-rune1 (null prev-rune1))) ; (2)
                (null (formals (cadr curr-rune2) w)))    ; (3)
           (cons (cadr curr-rune2) acc)
         acc))))))

(defun theory-warning-fns (ens1 ens2 w)

; Here is our strategy for producing warnings when an in-theory event or hint
; leaves us with a 0-ary function whose :executable-counterpart is enabled but
; :definition is not.  We assume that we have our hands on two enabled
; structures: the pre-existing one, which we call ens1, and the one created by
; the in-theory event or hint, which we call ens2 and is returned by
; load-theory-into-enabled-structure.  Note that the length of ens2 is at least
; as great as the length of ens1.  We walk through all indices (numes) of ens1.
; When do we find something worth warning about?  We only have a problem when
; we find an enabled :executable-counterpart at the current nume in ens2.  By
; the Essay on the Assignment of Runes and Numes by DEFUNS, we know that the
; previous nume represents the corresponding :definition.  Three conditions
; must now hold: (1) The preceding (:definition) rune is disabled in ens2; (2)
; The same problem was not already present in ens1; and (3) The function in
; question is 0-ary.

; We deal with the arrays as lists ordered by car, rather than using aref1,
; because the two arrays may have the same name in which case the first one is
; probably out of date.  We apply cdr to remove the headers.

  (theory-warning-fns-aux (cdr (access enabled-structure ens1 :theory-array))
                          (cdr (access enabled-structure ens2 :theory-array))
                          (1+ (access enabled-structure ens2
                                      :index-of-last-enabling))
                          0 nil nil w nil))

(defun maybe-warn-about-theory (ens1 force-xnume-en1 imm-xnume-en1 ens2
                                     ctx wrld state)

; Ens1 is the enabled structure before an in-theory event or hint, and ens2 is
; the resulting enabled structure.  It is a bit unfortunate that warning-off-p
; is checked twice, but that is a trivial inefficiency, certainly overshadowed
; by the savings in calling theory-warning-fns needlessly.

; Force-xnume-en1 is the enabled status of forcing (*force-xnume*) in ens1.
; Imm-xnume-en1 is the status immediate force mode
; (*immediate-force-modep-xnume*) in ens1.

  (cond
   ((warning-disabled-p "Disable")
    state)
   (t (pprogn
       (let ((fns (theory-warning-fns ens1 ens2 wrld)))
         (if fns
             (warning$ ctx ("Disable")
                       "The following 0-ary function~#0~[~/s~] will now have ~
                        ~#0~[its :definition rune~/their :definition runes~] ~
                        disabled but ~#0~[its :executable-counterpart ~
                        rune~/their :executable-counterpart runes~] enabled, ~
                        which will allow ~#0~[its definition~/their ~
                        definitions~] to open up after all:  ~&0.~|See :DOC ~
                        theories."
                       fns)
           state))
       (cond
        ((and force-xnume-en1
              (not (enabled-numep *force-xnume* ens2)))
         (warning$ ctx ("Disable")
                   "Forcing has transitioned from enabled to disabled.~|~
                    See :DOC force."))
        ((and (not force-xnume-en1)
              (enabled-numep *force-xnume* ens2))
         (warning$ ctx ("Disable")
                   "Forcing has transitioned from disabled to enabled.~|~
                    See :DOC force."))
        (t state))
       (cond
        ((and imm-xnume-en1
              (not (enabled-numep *immediate-force-modep-xnume* ens2)))
         (warning$ ctx ("Disable")
                   "IMMEDIATE-FORCE-MODEP has transitioned from enabled to ~
                    disabled.~|See :DOC force."))
        ((and (not imm-xnume-en1)
              (enabled-numep *immediate-force-modep-xnume* ens2))
         (warning$ ctx ("Disable")
                   "IMMEDIATE-FORCE-MODEP has transitioned from disabled to ~
                    enabled.~|See :DOC immediate-force-modep."))
        (t state))))))

; And now we develop the code for loading a theory into an enabled
; structure.

(defrec theory-invariant-record
  (tterm error . untrans-term)
  t)

(defun chk-theory-invariant1 (theory-expr ens invariant-alist errp-acc ctx state)

; We check a theory represented in enabled structure ens against the theory
; invariants in invariant-alist.  If theory-expr is :from-hint then this theory
; comes from an :in-theory hint, and if it is :install then it is from
; installing a world; otherwise, theory-expr is a theory expression
; corresponding to this theory.

  (cond
   ((null invariant-alist)
    (mv errp-acc nil state))
   (t (let* ((table-entry (car invariant-alist))
             (inv-name (car table-entry))
             (inv-rec (cdr table-entry))
             (theory-inv (access theory-invariant-record inv-rec :tterm)))
        (mv-let
         (erp okp latches)
         (ev theory-inv
             (list (cons 'ens ens)
                   (cons 'state (coerce-state-to-object state)))
             state
             nil
             nil)
         (declare (ignore latches))
         (cond
          (erp (let ((msg (msg
                           "Theory invariant ~x0 could not be evaluated on ~
                            the theory produced by ~@1.  Theory invariant, ~
                            ~P32, produced the error message:~%~@4~@5"
                           inv-name
                           (cond ((eq theory-expr :from-hint)
                                  "an :in-theory hint")
                                 ((eq theory-expr :install)
                                  "the current event")
                                 (t (msg "~X01" theory-expr nil)))
                           nil
                           (access theory-invariant-record inv-rec :untrans-term)
                           okp
                           (if (access theory-invariant-record inv-rec :error)
                               "~|This theory invariant violation causes an ~
                                error."
                             ""))))
                 (mv-let
                  (errp-acc state)
                  (cond
                   ((access theory-invariant-record inv-rec :error)
                    (mv-let (erp val state)
                            (er soft ctx "~@0" msg)
                            (declare (ignore erp val))
                            (mv t state)))
                   (t (pprogn (warning$ ctx "Theory" "~@0" msg)
                              (mv errp-acc state))))
                  (chk-theory-invariant1 theory-expr ens (cdr invariant-alist)
                                         errp-acc ctx state))))
          (okp (chk-theory-invariant1 theory-expr ens (cdr invariant-alist)
                                      errp-acc ctx state))
          (t (let ((msg (msg
                         "Theory invariant ~x0 failed on the theory produced ~
                          by ~@1.  Theory invariant ~x0 is ~P32.~@4"
                         inv-name
                         (cond ((eq theory-expr :from-hint)
                                "an :in-theory hint")
                               ((eq theory-expr :install)
                                "the current event")
                               (t (msg "~X01" theory-expr nil)))
                         nil
                         (access theory-invariant-record inv-rec :untrans-term)
                         (if (access theory-invariant-record inv-rec :error)
                             "~|This theory invariant violation causes an ~
                               error."
                           ""))))
               (mv-let
                (errp-acc state)
                (cond
                 ((access theory-invariant-record inv-rec :error)
                  (mv-let (erp val state)
                          (er soft ctx "~@0" msg)
                          (declare (ignore erp val))
                          (mv t state)))
                 (t (pprogn (warning$ ctx "Theory" "~@0" msg)
                            (mv errp-acc state))))
                (chk-theory-invariant1 theory-expr ens (cdr invariant-alist)
                                       errp-acc ctx state))))))))))

(defun chk-theory-invariant (theory-expr ens ctx state)

; See the comment in chk-theory-invariant1.

  (chk-theory-invariant1 theory-expr
                         ens
                         (table-alist 'theory-invariant-table (w state))
                         nil
                         ctx
                         state))

(defun load-theory-into-enabled-structure
  (theory-expr theory augmented-p ens incrmt-array-name-flg
               index-of-last-enabling wrld ctx state)

; Note: Theory must be a runic theory if augmented-p is nil and otherwise an
; augmented runic theory, but never a common theory.

; We do exactly what the name of this function says, we load the given theory
; into the enabled structure ens.  If incrmt-array-name-flg is t we increment
; the array name suffix.  Otherwise, we use the same name.  Loading consists of
; augmenting the theory (if augmented-p is nil) to convert it into a list of
; pairs, (nume . rune), mapping numes to their runes, and then compressing it
; into the named array.  We set the index of last enabling to be the highest
; existing nume in wrld right now unless index-of-last-enabling is non-nil, in
; which case we use that (which should be a natp).  Thus, any name introduced
; after this is enabled relative to this ens.  If the array of the ens is too
; short, we extend it by 500.

; A Refresher Course on ACL2 One Dimensional Arrays:

; Suppose that a is an array with :dimension (d) and :maximum-length
; m.  Then you access it via (aref1 'name a i).  It must be the case
; that i<d.  If every slot of a were filled, the length of a would be
; d, but the maximum index would be d-1, since indexing is 0-based.
; You set elements with (aset1 'name a i val).  That increases the
; length of a by 1.  When (length a) > m, a compress is done.  If an
; array is never modified, then the mimimum acceptable m is in fact d.

; Note:  Every call of this function should be followed by a call of
; maybe-warn-about-theory on the enabled structure passed in and the one
; returned.

  (let* ((n (or index-of-last-enabling (1- (get-next-nume wrld))))
         (d (access enabled-structure ens :array-length))
         (new-d (cond ((< n d) d)
                      (t (+ d (* 500 (1+ (floor (- n d) 500)))))))
         (root (access enabled-structure ens :array-name-root))
         (suffix (cond (incrmt-array-name-flg
                        (1+ (access enabled-structure ens :array-name-suffix)))
                       (t (access enabled-structure ens :array-name-suffix))))
         (name (cond (incrmt-array-name-flg
                      (intern (coerce
                               (append root
                                       (explode-nonnegative-integer suffix
                                                                    10
                                                                    nil))
                               'string)
                              "ACL2"))
                     (t (access enabled-structure ens :array-name))))
         (alist (if augmented-p
                    theory
                  (augment-runic-theory theory wrld)))
         (ens (make enabled-structure
                    :index-of-last-enabling n
                    :theory-array
                    (compress1 name
                               (cons (list :header
                                           :dimensions (list new-d)
                                           :maximum-length (1+ new-d)
                                           :default nil
                                           :name name
                                           :order nil)
                                     alist))
                    :array-name name
                    :array-length new-d
                    :array-name-root root
                    :array-name-suffix suffix)))
    (er-progn (if (or (eq theory-expr :no-check)
                      (eq (ld-skip-proofsp state) 'include-book)
                      (eq (ld-skip-proofsp state) 'include-book-with-locals))
                  (value nil)
                (chk-theory-invariant theory-expr ens ctx state))
              (value ens))))

; Here is how we initialize the global enabled structure, from which
; all subsequent structures are built.

(defun initial-global-enabled-structure (root-string)

; We generate an initial enabled-structure in which everything is enabled.
; The array is empty but of length d (which is the constant 1000 here).
; The array name is formed by adding a "0" to the end of root-string.
; The array-name-root is the list of characters in root-string and the suffix
; is 0.

  (let* ((root (coerce root-string 'list))
         (name (intern (coerce (append root '(#\0)) 'string)
                       "ACL2"))
         (d 1000))
    (make enabled-structure
          :index-of-last-enabling -1
          :theory-array (compress1 name
                                   (cons (list :header
                                               :dimensions (list d)
                                               :maximum-length (1+ d)
                                               :default nil
                                               :name name)
                                         nil))
          :array-name name
          :array-length d
          :array-name-root root
          :array-name-suffix 0)))

; And here is the function that must be used when you undo back to a
; previous value of the global enabled structure:

(defun recompress-global-enabled-structure (varname wrld)

; Logically speaking this function is a no-op that returns t.  It is
; only called from #-acl2-loop-only code.  Practically speaking it
; side-effects the von Neumann array named in the global-enabled-
; structure so that it once again contains the current array.

; This function is called when you have reason to believe that the von
; Neumann array associated with the global enabled structure is out of
; date.  Suppose that wrld, above, was obtained from the then current
; ACL2 world by rolling back, as with UBT.  Then there is possibly a
; new 'global-enabled-structure in wrld.  But the array associated
; with it is still the one from the now-no-longer current ACL2 world.
; We just compress the new array again.  Because the array was already
; compressed, we know compress1 returns an eq object and so we don't
; actually have to store its value back into the global-enabled-structure.
; Indeed, we don't want to do that because it would require us to put a
; new binding of 'global-enabled-structure on wrld and we don't want to
; to do that.  (Once upon a time we did, and it was inconvenient because
; it covered up the most recent command-landmark; indeed, sometimes there
; would be two successive bindings of it.)

  (declare (xargs :guard (and (equal varname varname)
                              (equal wrld wrld))))

; Without the odd guard -- some term mentioning all the formals -- the formals
; are recognized as irrelevant!  This body below always returns t.

  (let* ((ges1 (getprop varname 'global-value nil
                        'current-acl2-world wrld))
         (theory-array (access enabled-structure ges1 :theory-array))
         (name (access enabled-structure ges1 :array-name)))

; We would rather not pay the price of making a new array if the proper
; association of array to alist is already set up.  Since this function is
; logically a no-op (it is just a function that returns t), it is certainly
; legitimate to punt if we like.  But it might be nice to abstract what we are
; doing here and make it available to the ACL2 user.

    #-acl2-loop-only
    (when (let ((old-ar (get name 'acl2-array)))
            (and old-ar
                 (eq (car old-ar) theory-array)))
      (return-from recompress-global-enabled-structure t))
    (let ((to-ignore
           (cond (ges1
                  (prog2$ (flush-compress name)
                          (compress1 name theory-array)))
                 (t nil))))
      (declare (ignore to-ignore))
      t)))

(defun recompress-stobj-accessor-arrays (stobj-names wrld)

; This function has nothing to do with theories, but we place it here because
; it has a similar function to recompress-global-enabled-structure, defined
; just above.  This function should be called when the 'accessor-names arrays
; (used for printing nth/update-nth accesses to stobjs) might be out of date.

  (if (endp stobj-names)
      t
    (let* ((st (car stobj-names))
           (ar (getprop st 'accessor-names nil 'current-acl2-world wrld)))
      (prog2$ (or (null ar)
                  (prog2$ (flush-compress st)
                          (compress1 st ar)))
              (recompress-stobj-accessor-arrays (cdr stobj-names) wrld)))))

; We have defined all the basic concepts having to do with theories
; except the interface to the user, i.e., the "theory manipulation
; functions" with which the user constructs theories, the polite and
; verbose theory checkers that ensure that a theory expression
; produced a theory, and the events for defining theories and setting
; the current one.  We do these things later and return now to the
; development of type-set.

; The Type-set-xxx Functions

; We are about to embark on a litany of definitions for determining
; the type-set of various function applications, given the type-sets
; of certain of their arguments.  There is no fixed style of
; definition here; because type-set is so often called, we thought it
; best to pass in only those arguments actually needed, rather than
; adhere to some standard style.  All of the functions return a type
; set and a ttree.  The ttree is always used as an accumulator in the
; sense that we may extend but may not ignore the incoming ttree.

; This raises a problem: Suppose that the ttree records our work in
; establishing the type set, ts, of an argument but that we ultimately
; ignore ts because it is not strong enough to help.  If we return the
; extended ttree, we might cause unnecessary case splits (justifying
; the irrelevant fact that the arg has type-set ts).  We adopt the
; convention of passing in two ttrees, named ttree and ttree0.  Ttree
; is always an extension of ttree0 and records the work done to get
; the argument type sets.  Therefore, we may return ttree (possibly
; extended) if our answer is based on the argument type sets, and may
; return ttree0 otherwise (which merely passes along unknown
; previously done work of any sort, not necessarily only work done on this
; particular term -- so do not return nil instead of ttree0!).

; The primitive type-set functions all push the following fake rune into the
; ttree they return, so that we know that primitive type-set knowledge was
; used.  Before we invented this fake rune, we attributed this type-set
; reasoning to propositional calculus, i.e., we would report that (implies
; (integerp x) (integerp (1- x))) was proved by trivial observations.  But we
; prefer to think of it (and report it) as type reasoning.  See the Essay on
; Fake-Runes for a discussion of fake runes.

(defconst *fake-rune-for-type-set*
  '(:FAKE-RUNE-FOR-TYPE-SET nil))

; To make it convenient to push this rune into a tree we provide:

(defun puffert (ttree)

; Once upon a time this function was called Push-Fake-Rune-for-Type-Set.  It
; got shortened to pfrts and pronounced "pufferts".  The name stuck except that
; the final s was dropped to make it more like a first-person verb form.  You
; know, ``I puffert'', ``you puffert'', ``he pufferts.''  Here, we frequently
; puffert ttrees.  Right.

  (push-lemma *fake-rune-for-type-set* ttree))

(defun immediate-forcep (fn ens)

; This function must return 'case-split, t or nil!

  (cond ((eq fn 'case-split) 'case-split)
        ((enabled-numep *immediate-force-modep-xnume* ens) t)
        (t nil)))

(defmacro numeric-type-set (ts)

; Warning:  This is a dangerous macro because it evaluates ts more than once!
; It is best if the argument is a variable symbol.

; This coerces ts into the type *ts-acl2-number*.  That is, if ts contains
; nonnumeric bits then those bits are shut off and *ts-zero* is turned on.
; Another way to look at it is that if term has type ts then (fix term) has
; type (numeric-type-set ts).

; Note:  We tried wrapping (the-type-set ...) around the form below, inside the
; backquote, but found that (disassemble 'TYPE-SET-BINARY-+) produced identical
; results either way.

  `(let ((numeric-ts-use-nowhere-else
          (ts-intersection (check-vars-not-free
                            (numeric-ts-use-nowhere-else)
                            ,ts)
                           *ts-acl2-number*)))
     (if (ts= numeric-ts-use-nowhere-else ,ts)
         ,ts
       (ts-union numeric-ts-use-nowhere-else *ts-zero*))))

(defmacro rational-type-set (ts)

; Warning:  This is a dangerous macro because it evaluates ts more than once!
; It is best if the argument is a variable symbol.

; This macro is like numeric-type-set, but coerces ts to the rationals.  Note
; that it reuses the special variable numeric-ts-use-nowhere-else even though
; this is a slight misnomer.

  `(let ((numeric-ts-use-nowhere-else
          (ts-intersection (check-vars-not-free
                            (numeric-ts-use-nowhere-else)
                            ,ts)
                           *ts-rational*)))
     (if (ts= numeric-ts-use-nowhere-else ,ts)
         ,ts
       (ts-union numeric-ts-use-nowhere-else *ts-zero*))))

;; RAG - I added this function analogously to rational-type-set.

#+:non-standard-analysis
(defmacro real-type-set (ts)

; Warning:  This is a dangerous macro because it evaluates ts more than once!
; It is best if the argument is a variable symbol.

; This macro is like numeric-type-set, but coerces ts to the reals.  Note
; that it reuses the special variable numeric-ts-use-nowhere-else even though
; this is a slight misnomer.

  `(let ((numeric-ts-use-nowhere-else
          (ts-intersection (check-vars-not-free
                            (numeric-ts-use-nowhere-else)
                            ,ts)
                           *ts-real*)))
     (if (ts= numeric-ts-use-nowhere-else ,ts)
         ,ts
       (ts-union numeric-ts-use-nowhere-else *ts-zero*))))

; We start with probably the most complicated primitive type set
; function, that for binary-+.

(defun type-set-binary-+ (term ts1 ts2 ttree ttree0)

; Because 1- (i.e., SUB1) is so common and often is applied to
; strictly positive integers, it is useful to know that, in such
; cases, the result is a non-negative integer.  We therefore test for
; (+ x -1) and its commuted version (+ -1 x).  To be predictable, we
; also look for (+ x +1), and its commuted version, when x is strictly
; negative.  We specially arrange for the answer type-set to be empty
; if either of the input type-sets is empty.  This occurs when we are
; guessing type sets.  The idea is that some other branch ought to
; give us a nonempty type-set before this one can meaningfully
; contribute to the answer.  Before we added the special processing of
; +1 and -1 we did not have to check for the empty case because the
; array referenced by aref2 has the property that if either type-set
; is empty the result is empty.

  (let ((arg1 (fargn term 1))
        (arg2 (fargn term 2)))
    (cond ((or (ts= ts1 *ts-empty*)
               (ts= ts2 *ts-empty*))
           (mv *ts-empty* ttree))
          ((and (equal arg2 ''-1)
                (ts-subsetp ts1 *ts-positive-integer*))
           (mv *ts-non-negative-integer* (puffert ttree)))
          ((and (equal arg1 ''-1)
                (ts-subsetp ts2 *ts-positive-integer*))
           (mv *ts-non-negative-integer* (puffert ttree)))
          ((and (equal arg2 ''+1)
                (ts-subsetp ts1 *ts-negative-integer*))
           (mv *ts-non-positive-integer* (puffert ttree)))
          ((and (equal arg1 ''+1)
                (ts-subsetp ts2 *ts-negative-integer*))
           (mv *ts-non-positive-integer* (puffert ttree)))
          (t (let ((ans (aref2 'type-set-binary-+-table
                               *type-set-binary-+-table*
                               (numeric-type-set ts1)
                               (numeric-type-set ts2))))
               (mv ans
                   (puffert (if (ts= ans *ts-acl2-number*)
                                ttree0
                              ttree))))))))

(defun type-set-binary-* (ts1 ts2 ttree ttree0)

; See type-set-binary-+ for a few comments.

  (cond ((or (ts= ts1 *ts-empty*)
             (ts= ts2 *ts-empty*))
         (mv *ts-empty* ttree))
        (t (let ((ans (aref2 'type-set-binary-*-table
                             *type-set-binary-*-table*
                             (numeric-type-set ts1)
                             (numeric-type-set ts2))))
             (mv ans
                 (puffert (if (ts= ans *ts-acl2-number*)
                              ttree0
                            ttree)))))))

(defun type-set-not (ts ttree ttree0)
  (cond
   ((ts= ts *ts-nil*)
    (mv *ts-t* (puffert ttree)))
   ((ts-subsetp *ts-nil* ts)
    (mv *ts-boolean* ttree0))
   (t (mv *ts-nil* (puffert ttree)))))

(defun type-set-<-1 (r arg2 commutedp type-alist)

; We are trying to determine the truth value of an inequality, (< arg1
; arg2) where arg1 is (quote r), a rational constant.  Except, in the
; case that commutedp is non-nil the inequality at issue is (< arg2
; (quote r)).

; We scan through the type-alist looking for inequalities that imply
; the truth or falsity of the inequality at issue, and return two
; values --- the determined type of the inequality, by default
; *ts-boolean*, and a governing ttree.

; Here is a trivial example of the problem this code is intended to
; solve.

#|
 (defstub bar (x) t)

 (defaxiom bar-thm
   (implies (and (integerp x)
                 (< 3 x))
            (bar x))
   :rule-classes :type-prescription)

 (thm
  (implies (and (integerp x)
                (< 4 x))
           (bar x)))
|#

; Robert Krug came up with the original version of this patch when
; playing with arithmetic functions that changed sign at some point
; other than zero.  Conceptually, this type of reasoning belongs to
; linear arithmetic rather than type-set, but it provides a modest
; improvement at a very small cost.  In a perfect world we might want
; to mix type-set reasoning with the linear arithmetic; the ability to
; call add-poly from within type-set or assume-true-false could be
; nice (although perhaps too expensive).

  (cond ((endp type-alist)
         (mv *ts-boolean* nil))
        (t
         (let ((type-alist-entry (car type-alist)))
           (case-match type-alist-entry
             ((typed-term type . ttree)
              (mv-let (c leftp x)

; We bind c to nil if we cannot use type-alist-entry.  Otherwise, c is a
; rational that is being compared with < to a term x, and leftp is true if and
; only if c occurs on the left side of the <.  We postpone the check that x is
; equal to arg2, which is potentially expensive, until we need to make that
; check (if at all).

                      (case-match typed-term
                        (('< ('quote c) x)
                         (if (rationalp c)
                             (mv c t x)
                           (mv nil nil x)))
                        (('< x ('quote c))
                         (if (rationalp c)
                             (mv c nil x)
                           (mv nil nil nil)))
                        (& (mv nil nil nil)))
                      (cond
                       ((null c)
                        (type-set-<-1 r arg2 commutedp (cdr type-alist)))
                       (leftp

; So type refers to (c < x).

                        (cond
                         ((and (<= r c)
                               (ts= type *ts-t*)
                               (equal x arg2))

;   (r <= c < arg2) implies (r < arg2), and hence also not (arg2 < r).

                          (mv (if commutedp *ts-nil* *ts-t*)
                              (puffert ttree)))
                         ((and (if commutedp (< c r) (<= c r))
                               (ts= type *ts-nil*)
                               (equal x arg2))

;   (arg2 <= c <= r) implies not (r < arg2);
;   (arg2 <= c <  r) implies (arg2 < r).

                          (mv (if commutedp *ts-t* *ts-nil*)
                              (puffert ttree)))
                         (t
                          (type-set-<-1 r arg2 commutedp (cdr type-alist)))))
                       (t ; (not leftp)

; So type refers to (arg2 < c).

                        (cond
                         ((and (if commutedp (<= r c) (< r c))
                               (ts= type *ts-nil*)
                               (equal x arg2))

;   (r <  c <= arg2) implies (r < arg2);
;   (r <= c <= arg2) implies not (arg2 < r).

                          (mv (if commutedp *ts-nil* *ts-t*)
                              (puffert ttree)))
                         ((and (<= c r)
                               (ts= type *ts-t*)
                               (equal x arg2))

;   (arg2 < c <= r) implies not (r < arg2) and also implies (arg2 < r).

                          (mv (if commutedp *ts-t* *ts-nil*)
                              (puffert ttree)))
                         (t
                          (type-set-<-1 r arg2 commutedp
                                        (cdr type-alist))))))))
             (& (type-set-<-1 r arg2 commutedp (cdr type-alist))))))))

;; RAG - I changed complex-rational to complex below.

(defun type-set-< (arg1 arg2 ts1 ts2 type-alist ttree ttree0 pot-lst pt)

; This function is not cut from the standard mold because instead of
; taking term it takes the two args.  This allows us easily to
; implement certain transformations on inequalities: When x is an
; integer,

; (<  x 1) is (not (< 0 x)) and
; (< -1 x) is (not (< x 0)).

; Warning: It is important to assume-true-false that type-set-< make
; these transformations.  See the comments about type-set-< in
; assume-true-false.

; As of Version_2.6, this function diverged even further from the standard
; mold.  We now use the type-alist to determine the truth or falsity
; of some simple inequalities which would be missed otherwise.
; See type-set-<-1 for details.

  (let* ((nts1 (numeric-type-set ts1))
         (nts2 (numeric-type-set ts2)))
    (cond ((and (equal arg2 *1*)

; Actually we don't have to add 0 back in, as done by numeric-type-set, before
; making the following test.  But let's keep things simple.

                (ts-subsetp nts1 *ts-integer*))
           (mv-let (ts ttree)
                   (type-set-< *0* arg1 *ts-zero* ts1
                               type-alist
                               (puffert ttree)

; Note: Once upon a time in v2-7, ttree0 was used immediately below
; instead of ttree.  Note however that we have depended upon ts1 to
; get here.  It might be unsound to do that and then report the
; dependencies of ttree0.  However, in v2-7 this was (probably) sound
; because the ttree0 exits were all reporting *ts-boolean* answers.
; But in the new code, below, we use add-polys0 in a way that could
; overwrite ttree with ttree0.  Put more intuitively: we think v2-7
; was sound even with a ttree0 here, but we think v2-8 would be
; unsound with a ttree0 here because of the add-polys0 below.

                               ttree
                               pot-lst pt)
                   (type-set-not ts ttree ttree0)))
          ((and (quotep arg1)
                (eql (cadr arg1) -1)
                (ts-subsetp nts2 *ts-integer*))
           (mv-let (ts ttree)
                   (type-set-< arg2 *0* ts2 *ts-zero*
                               type-alist
                               (puffert ttree)
; See note above about this ttree versus the old ttree0.
                               ttree 
                               pot-lst pt)
                   (type-set-not ts ttree ttree0)))

; If one of the args is a constant (a quotep) we look in the
; type-alist.  If we get a useful answer, we are done.  Note that if
; we do get a useful answer here, it is sufficient to use ttree0
; rather than ttree since our answer does not depend on the type of
; the args.  In particular, type-set-<-1 returns an accurate answer
; regardless of whether we make the tests above leading to here.  See
; the comments following ``The Type-set-xxx Functions'' above for an
; explanation of ttree0 vs. ttree.

          (t
           (mv-let (returned-ts returned-ttree)
             (cond
              ((and (quotep arg1) (rationalp (cadr arg1)))
               (type-set-<-1 (cadr arg1) arg2 nil type-alist))
              ((and (quotep arg2) (rationalp (cadr arg2)))
               (type-set-<-1 (cadr arg2) arg1   t type-alist))
              (t
               (mv *ts-boolean* nil)))
             (if (not (ts= returned-ts *ts-boolean*))
                 (mv returned-ts
                     (cons-tag-trees returned-ttree
                                     ttree0))

; We did not get a useful answer by looking in the type-alist.  We try
; 'type-set-<-table if we can.

               (let ((temp-ts 
                      (if (or (ts-intersectp ts1
                                             #+:non-standard-analysis
                                             *ts-complex*
                                             #-:non-standard-analysis
                                             *ts-complex-rational*)
                              (ts-intersectp ts2 
                                             #+:non-standard-analysis
                                             *ts-complex*
                                             #-:non-standard-analysis
                                             *ts-complex-rational*))
                          *ts-boolean*
                        (aref2 'type-set-<-table
                               *type-set-<-table* nts1 nts2))))
                 (cond ((or (ts= temp-ts *ts-t*)
                            (ts= temp-ts *ts-nil*))
                        (mv temp-ts (puffert ttree)))
                       ((null pot-lst)
                        (mv *ts-boolean* ttree0))

; We finally try using linear arithmetic by calling add-polys on, first,
; the negation of the original inequality.  If this returns a contradictionp,
; the original inequality must be true.  If this does not return a
; contradictionp, we try linear arithmetic with the original inequality.
; These final two tries are new to v2-8.

                       (t

; Note: Below there are two calls of base-poly, each of which has the
; ttree ttree0.  The argument that we're not dependent on ts1 and ts2
; here is as follows.  The setting of temp-ts, above, which appears to
; rely on ts1 and ts2, is irrelevant here because if we get here,
; temp-ts is *ts-boolean*, which is correct regardless of the ttrees.
; Reader further above, the only uses of ts1 and ts2 are heuristic:
; the methods by which we compute an answer is correct even if the
; preceding tests were not made.

                        (mv-let (contradictionp new-pot-lst)
                          (add-polys0
                           (list (normalize-poly
                                  (add-linear-terms
                                   :lhs arg2
                                   :rhs arg1
                                   (base-poly ttree0
                                              '<=
; The following nil is the rational-poly-p flag and we could supply a
; more accurate value by looking at ts1 and ts2.  But then it would
; appear that we were depending on them.  Actually, we're not: the
; rational-poly-p flag is irrelevant to add-polys0 and could only come
; into play if the poly here created eventually found its way into a
; non-linear setting.  But that won't happen because the poly is
; thrown away.  However, since the flag is indeed irrelevant we just
; supply nil to avoid the appearance of dependence.

                                              nil
                                              nil))))
                           pot-lst pt nil 2)
                          (declare (ignore new-pot-lst))
                          (if contradictionp
                              (mv *ts-t* (access poly contradictionp :ttree))
                            (mv-let (contradictionp new-pot-lst)
                              (add-polys0
                               (list (normalize-poly
                                      (add-linear-terms
                                       :lhs arg1
                                       :rhs arg2
                                       (base-poly ttree0
                                                  '<
                                                  nil
                                                  nil))))
                               pot-lst pt nil 2)
                              (declare (ignore new-pot-lst))
                              (if contradictionp
                                  (mv *ts-nil*
                                      (access poly contradictionp :ttree))
                                (mv *ts-boolean* ttree0))))))))))))))

;; RAG - I added entries for real and complex irrationals.

(defun type-set-unary-- (ts ttree ttree0)
  (let ((ts1 (numeric-type-set ts)))
    (cond
     ((ts= ts1 *ts-acl2-number*)
      (mv *ts-acl2-number* ttree0))
     (t
      (mv (ts-builder ts1
                      (*ts-zero* *ts-zero*)
                      (*ts-positive-integer* *ts-negative-integer*)
                      (*ts-positive-ratio* *ts-negative-ratio*)
                      #+:non-standard-analysis
                      (*ts-positive-non-ratio* *ts-negative-non-ratio*)
                      (*ts-negative-integer* *ts-positive-integer*)
                      (*ts-negative-ratio* *ts-positive-ratio*)
                      #+:non-standard-analysis
                      (*ts-negative-non-ratio* *ts-positive-non-ratio*)
                      (*ts-complex-rational* *ts-complex-rational*)
                      #+:non-standard-analysis
                      (*ts-complex-non-rational* *ts-complex-non-rational*))
          (puffert ttree))))))

;; RAG - I added entries for real and complex irrationals.

(defun type-set-unary-/ (ts ttree ttree0)
  (let* ((ts1 (numeric-type-set ts))
         (ans (ts-builder ts1
                          (*ts-zero* *ts-zero*)
                          (*ts-positive-rational* *ts-positive-rational*)
                          (*ts-negative-rational* *ts-negative-rational*)
                          #+:non-standard-analysis
                          (*ts-positive-non-ratio* *ts-positive-non-ratio*)
                          #+:non-standard-analysis
                          (*ts-negative-non-ratio* *ts-negative-non-ratio*)
                          (*ts-complex-rational* *ts-complex-rational*)
                          #+:non-standard-analysis
                          (*ts-complex-non-rational* *ts-complex-non-rational*))))
    (cond
     ((ts= ans *ts-acl2-number*)
      (mv *ts-acl2-number* (puffert ttree0)))
     (t
      (mv ans (puffert ttree))))))

(defun type-set-numerator (ts ttree ttree0)
  (let* ((ts1 (rational-type-set ts))
         (ans (ts-builder ts1
                          (*ts-zero* *ts-zero*)
                          (*ts-positive-rational* *ts-positive-integer*)
                          (*ts-negative-rational* *ts-negative-integer*))))
    (cond ((ts= ans *ts-integer*)
           (mv *ts-integer* (puffert ttree0)))
          (t (mv ans (puffert ttree))))))

;; RAG - I added an entry for *ts-complex-non-rational*.  Note that
;; since we don't know whether the type in non-rational because of an
;; irrational real or imaginary part, all we can say is that the
;; result is real, not necessarily irrational.

(defun type-set-realpart (ts ttree ttree0)
  (cond #+:non-standard-analysis
        ((ts-intersectp ts *ts-complex-non-rational*)
         (mv *ts-real* (puffert ttree0)))
        ((ts-intersectp ts *ts-complex-rational*)
         (mv *ts-rational* (puffert ttree0)))
        (t
         (mv (numeric-type-set ts) (puffert ttree)))))

;; RAG - I added an entry for *ts-complex-non-rational*.

(defun type-set-imagpart (ts ttree ttree0)
  (cond #+:non-standard-analysis
        ((ts-subsetp ts *ts-complex-non-rational*)
         (mv (ts-union *ts-positive-real*
                       *ts-negative-real*)
             (puffert ttree)))
        #+:non-standard-analysis
        ((ts-intersectp ts *ts-complex-non-rational*)
         (mv *ts-real* (puffert ttree0)))
        ((ts-subsetp ts *ts-complex-rational*)
         (mv (ts-union *ts-positive-rational*
                       *ts-negative-rational*)
             (puffert ttree)))
        ((ts-intersectp ts *ts-complex-rational*)
         (mv *ts-rational* (puffert ttree0)))
        (t
         (mv *ts-zero* (puffert ttree)))))

;; RAG - I allowed reals as well as rationals below for the type of
;; ts1 and ts2.

(defun type-set-complex (ts1 ts2 ttree ttree0)
  (let ((ts1 #+:non-standard-analysis
             (real-type-set ts1)
             #-:non-standard-analysis
             (rational-type-set ts1))
        (ts2 #+:non-standard-analysis
             (real-type-set ts2)
             #-:non-standard-analysis
             (rational-type-set ts2)))
    (cond ((ts= ts2 *ts-zero*)
           (mv ts1 (puffert ttree)))
          ((ts= (ts-intersection ts2 *ts-zero*)
                *ts-empty*)
           #+:non-standard-analysis
           (cond ((and (ts-subsetp ts1 *ts-rational*)
                       (ts-subsetp ts2 *ts-rational*))
                  (mv *ts-complex-rational* (puffert ttree)))
                 ((or (ts-subsetp ts1 *ts-non-ratio*)
                      (ts-subsetp ts2 *ts-non-ratio*))
                  (mv *ts-complex-non-rational* (puffert ttree)))
                 (t (mv *ts-complex* (puffert ttree))))
           #-:non-standard-analysis
           (mv *ts-complex-rational* (puffert ttree)))
          #+:non-standard-analysis
          ((ts= ts1 *ts-real*)
           (mv *ts-acl2-number* (puffert ttree0)))
          #-:non-standard-analysis
          ((ts= ts1 *ts-rational*)
           (mv *ts-acl2-number* (puffert ttree0)))
          (t
           (mv (ts-union ts1 
                         #+:non-standard-analysis
                         (cond ((and (ts-subsetp ts1 *ts-rational*)
                                     (ts-subsetp ts2 *ts-rational*))
                                *ts-complex-rational*)
                               (t *ts-complex*))
                         #-:non-standard-analysis
                         *ts-complex-rational*)
               (puffert ttree))))))

;; RAG - I added this function to account for the new built-in floor1.

#+:non-standard-analysis
(defun type-set-floor1 (ts ttree ttree0)
  (let* ((ts1 (real-type-set ts))
         (ans (ts-builder ts1
                          (*ts-zero* *ts-zero*)
                          (*ts-positive-integer* *ts-positive-integer*)
                          (*ts-positive-ratio* *ts-non-negative-integer*)
                          (*ts-positive-non-ratio* *ts-non-negative-integer*)
                          (*ts-negative-real* *ts-negative-integer*))))
    (cond ((ts= ans *ts-integer*)
           (mv *ts-integer* (puffert ttree0)))
          (t (mv ans (puffert ttree))))))

;; RAG - I added this function to account for the new built-in standard-part.

#+:non-standard-analysis
(defun type-set-standard-part (ts ttree ttree0)
  (let* ((ts1 (numeric-type-set ts))
         (ans (ts-builder ts1
                          (*ts-zero* *ts-zero*)
                          (*ts-positive-real* *ts-non-negative-real*)
                          (*ts-negative-real* *ts-non-positive-real*)
                          (*ts-complex* *ts-acl2-number*))))
    (cond ((ts= ans *ts-acl2-number*)
           (mv *ts-acl2-number* (puffert ttree0)))
          (t (mv ans (puffert ttree))))))

;; RAG - I added this function to account for the new built-in standard-numberp.

#+:non-standard-analysis
(defun type-set-standard-numberp (ts ttree ttree0)
  (cond ((ts= ts *ts-zero*)
         (mv *ts-t* (puffert ttree)))
        ((ts-subsetp ts (ts-complement *ts-acl2-number*))
         (mv *ts-nil* (puffert ttree)))
        (t (mv *ts-boolean* (puffert ttree0)))))

; Essay on the Recognizer-Alist and Recognizer-Tuples

; The "recognizer alist" of ACL2 is a combination of Nqthm's
; RECOGNIZER-ALIST and its two COMPOUND-RECOGNIZER-ALISTs.  The
; recognizer-alist is stored as a global variable in the world w and
; accessed via

; (global-val 'recognizer-alist w).

; The recognizer alist contains records of the following form:

(defrec recognizer-tuple
  (fn (nume . true-ts)
      (false-ts . strongp)
      . rune)
  t)

; The initial value of the recognizer alist is shown after we discuss the
; meaning of these records.

; In a recognizer-tuple, fn is the name of some Boolean-valued
; function of one argument.  True-ts and and false-ts are type sets.
; If such a record is on the recognizer-alist then it is the case that
; (fn x) implies that the type set of x is a subset of true-ts and
; (not (fn x)) implies that the type set of x is a subset of false-ts.
; Furthermore, if strongp is t, then true-ts is the complement of
; false-ts; i.e., (fn x) recognizes exactly the subset identified by
; true-ts.  Rune is either a rune or
; *fake-rune-for-anonymous-enabled-rule*.  Nume is the nume of rune
; (possibly nil).

; For example, if we prove that

; (BOOLEANP X) -> (OR (EQUAL X T) (EQUAL X NIL))

; then we can add the following tuple

; (make recognizer-tuple
;       :fn BOOLEANP
;       :true-ts *ts-boolean*
;       :false-ts *ts-unknown*
;       :strongp nil
;       :nume nil
;       :rune *fake-rune-for-anonymous-enabled-rule*)

; to the list.  Observe that the false-ts for this pair does not tell us
; much.  But if we proved the above AND

; (NOT (BOOLEANP X)) -> (NOT (OR (EQUAL X T) (EQUAL X NIL)))

; we could add the tuple:

; (make recognizer-tuple
;       :fn BOOLEANP
;       :true-ts *ts-boolean*
;       :false-ts (ts-complement *ts-boolean*)
;       :strongp t)

; And we would know as much about BOOLEANP as we know about integerp.

; Consider the function PRIMEP.  It implies its argument is a positive
; integer.  Its negation tells us nothing about the type of its argument.

; (make recognizer-tuple
;       :fn PRIMEP
;       :true-ts *ts-positive-integer*
;       :false-ts *ts-unknown*
;       :strongp nil)

; Suppose now x is a term whose type set we know.  What is the type
; set of (PRIMEP x)?  If the type set for x includes the positive
; integer bit, the type set for (PRIMEP x) may include *ts-t* so we
; will throw that in.  If the type set for x includes any of
; *ts-unknown*'s bits (of course it does) we will throw in *ts-nil*.
; The interesting thing about this is that if the type set of x does
; not include the positive integers, we'll know (PRIME x) is nil.

; If we assume (PRIME x) true, we will restrict the type of x to the
; positive integers.  If we assume (PRIME x) false, we won't restrict
; x at all.

; Consider the function RATTREEP that recognizes cons-trees of
; rational numbers.  We can prove that (RATTREEP x) implies the type
; set of x is in *ts-cons* union *ts-rational*.  We can prove that
; (NOT (RATTREEP x)) implies that the type set of x is not
; *ts-rational*.  That means the false-ts for RATTREEP is the
; complement of the rationals.  If we were asked to get the type set
; of (RATTREEP x) where x is rational, we'd throw in a *ts-t* because
; the type of x intersects the true-ts and we'd not throw in anythine
; else (because the type of x does not interesect the false ts).  If
; we were asked to assume (RATTREEP x) then on the true branch x's
; type would be interesected with the conses and the rationals.  On
; the false branch, the rationals would be deleted.

; Historical Note: In an earlier version of this code we did not allow
; compound recognizer rules to be enabled or disabled and hence did
; not store the runes and numes.  We were much cleverer then
; about allowing newly proved rules to strengthen existing recognizer
; tuples.  That is, you could prove a rule about the true-ts and then
; a second about the false-ts, and then perhaps a third tightening up
; the true-ts fact a little, etc.  This had the problem that it was
; not possible to identify a single rule name with the known facts
; about the type of fn.  Thus, when we decided to track use of all
; rules it was impossible to give a sensible meaning to disabled
; compound recognizer rules in some cases.  (E.g., the fact stored
; might be derived from both enabled and disabled rules.)  So an
; important aspect of the new design is that there is a 1:1
; correspondence between what we know and the rule that told us.  If
; you have proved a sequence of three rules about fn we will use the
; most recently proved, still-enabled one.  If you disable that one,
; we'll naturally fall back on the next most recently still-enabled
; one.

;; RAG - I added recognizers for realp and complexp.

(defconst *initial-recognizer-alist*
  (list (make recognizer-tuple
              :fn 'integerp
              :true-ts *ts-integer*
              :false-ts (ts-complement *ts-integer*)
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)
        (make recognizer-tuple
              :fn 'rationalp
              :true-ts *ts-rational*
              :false-ts (ts-complement *ts-rational*)
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)

        #+:non-standard-analysis
        (make recognizer-tuple
              :fn 'realp
              :true-ts *ts-real*
              :false-ts (ts-complement *ts-real*)
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)

        (make recognizer-tuple
              :fn 'complex-rationalp
              :true-ts *ts-complex-rational*
              :false-ts (ts-complement *ts-complex-rational*)
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)

        #+:non-standard-analysis
        (make recognizer-tuple
              :fn 'complexp
              :true-ts *ts-complex*
              :false-ts (ts-complement *ts-complex*)
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)

        (make recognizer-tuple
              :fn 'acl2-numberp
              :true-ts *ts-acl2-number*
              :false-ts (ts-complement *ts-acl2-number*)
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)
        (make recognizer-tuple
              :fn 'consp
              :true-ts *ts-cons*
              :false-ts (ts-complement *ts-cons*)
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)
        (make recognizer-tuple
              :fn 'atom
              :true-ts (ts-complement *ts-cons*)
              :false-ts *ts-cons*
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)
        (make recognizer-tuple
              :fn 'listp
              :true-ts (ts-union *ts-cons* *ts-nil*)
              :false-ts (ts-complement (ts-union *ts-cons* *ts-nil*))
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)
        (make recognizer-tuple
              :fn 'true-listp
              :true-ts *ts-true-list*
              :false-ts (ts-complement *ts-true-list*)
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)
        (make recognizer-tuple
              :fn 'characterp
              :true-ts *ts-character*
              :false-ts (ts-complement *ts-character*)
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)
        (make recognizer-tuple
              :fn 'stringp
              :true-ts *ts-string*
              :false-ts (ts-complement *ts-string*)
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)
        (make recognizer-tuple
              :fn 'null
              :true-ts *ts-nil*
              :false-ts (ts-complement *ts-nil*)
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)
        (make recognizer-tuple
              :fn 'symbolp
              :true-ts *ts-symbol*
              :false-ts (ts-complement *ts-symbol*)
              :strongp t
              :nume nil
              :rune *fake-rune-for-anonymous-enabled-rule*)))

(defun most-recent-enabled-recog-tuple (fn alist ens)

; This function finds the first recognizer-tuple on alist whose :fn is
; fn and whose :nume is enabled-numep.  Thus, primitive recognizer
; tuples, like that for rationalp, are always "enabled."

  (cond ((null alist) nil)
        ((and (eq fn (access recognizer-tuple (car alist) :fn))
              (enabled-numep (access recognizer-tuple (car alist) :nume) ens))
         (car alist))
        (t (most-recent-enabled-recog-tuple fn (cdr alist) ens))))

(defun type-set-recognizer (recog-tuple arg-ts ttree ttree0)

; Recog-tuple is a recognizer-tuple.  Then we know that (fn x) implies
; that the type set of x, arg-ts, is a subset of true-ts.
; Furthermore, we know that ~(fn x) implies that arg-ts is a subset of
; false-ts.  In addition, we know that fn is a Boolean valued fn.

; This function is supposed to determine the type set of (fn x) where
; arg-ts is the type set of x.  Observe that if arg-ts intersects with
; true-ts then (fn x) might be true, so we should throw in *ts-t*.
; Conversely, if arg-ts does not intersect with true-ts then (fn x)
; cannot possibly be true.  Exactly analogous statements can be made
; about false-ts.  

; We return two results, the type set of (fn x) and an extension of
; ttree (or ttree0) obtained by adding the named rule, tagged 'lemma.  We
; initially considered adding the rule name to the tree only if the
; type-set returned was stronger than just Boolean.  But it could be
; that this rule is the rule that established that fn was Boolean and
; so we can't be sure that even that weak response isn't an
; interesting use of this rule.

  (let ((ts (ts-builder
             arg-ts
             ((access recognizer-tuple recog-tuple :true-ts) *ts-t*)
             ((access recognizer-tuple recog-tuple :false-ts) *ts-nil*))))
    (cond
     ((ts= ts *ts-boolean*)
      (mv *ts-boolean*
          (push-lemma (access recognizer-tuple recog-tuple :rune) ttree0)))
     (t (mv ts
            (push-lemma (access recognizer-tuple recog-tuple :rune) ttree))))))

(defun type-set-car (ts ttree ttree0)
  (cond ((ts-intersectp ts *ts-cons*) (mv *ts-unknown* ttree0))
        (t (mv *ts-nil* ttree))))

(defun type-set-cdr (ts ttree ttree0)
  (let ((cdr-ts
         (ts-builder ts
                     (*ts-proper-cons* *ts-true-list*)
                     (*ts-improper-cons* (ts-complement *ts-true-list*))
                     (otherwise *ts-nil*))))
    (mv cdr-ts
        (if (ts= cdr-ts *ts-unknown*)
            ttree0
          (puffert ttree)))))

(defun type-set-coerce (term ts1 ts2 ttree1 ttree2 ttree0)

  (cond ((equal (fargn term 2) ''list)

; If the first argument is of type *ts-string* then the result could
; be either nil or a proper cons.  But if the first argument isn't
; possibly a string, the result is NIL.

         (cond ((ts-intersectp *ts-string* ts1)

; We are not really using ts1 here because (coerce x 'list) is always
; a true list.  So we report ttree0, not ttree1.

                (mv *ts-true-list* (puffert ttree0)))
               (t (mv *ts-nil* (puffert ttree1)))))
        ((quotep (fargn term 2))
         (mv *ts-string* (puffert ttree0)))
        ((not (ts-intersectp *ts-non-t-non-nil-symbol* ts2))
; Observe that the first argument (and its ttree1) don't matter here.
         (mv *ts-string* (puffert ttree2)))
        (t (mv (ts-union *ts-true-list* *ts-string*) (puffert ttree0)))))

(defun type-set-intern-in-package-of-symbol (ts1 ts2 ttree1 ttree2 ttree0)
  (cond ((not (ts-intersectp ts1 *ts-string*))
         (mv *ts-nil* (puffert ttree1)))
        ((not (ts-intersectp ts2 *ts-symbol*))
         (mv *ts-nil* (puffert ttree2)))
        (t (mv *ts-symbol* (puffert ttree0)))))

(defun type-set-length (ts ttree ttree0)
  (let ((ans (ts-builder ts
                         (*ts-string* *ts-non-negative-integer*)
                         (*ts-cons* *ts-positive-integer*)
                         (otherwise *ts-zero*))))
    (cond
     ((ts= ans *ts-integer*)
      (mv *ts-integer* (puffert ttree0)))
     (t
      (mv ans (puffert ttree))))))

(defun type-set-cons (ts2 ttree ttree0)

; Ts2 is the type set of the second argument of the cons.

  (let ((ts (ts-builder ts2
                        (*ts-true-list* *ts-proper-cons*)
                        (otherwise *ts-improper-cons*))))
    (cond ((ts= ts *ts-cons*)
           (mv ts (puffert ttree0)))
          (t (mv ts (puffert ttree))))))

(defconst *singleton-type-sets*
  (list *ts-t* *ts-nil* *ts-zero*))

(defun type-set-equal (ts1 ts2 ttree ttree0)
  (cond ((member ts1 *singleton-type-sets*)
         (cond ((ts= ts1 ts2) (mv *ts-t* (puffert ttree)))
               ((ts-intersectp ts1 ts2)
                (mv *ts-boolean* (puffert ttree0)))
               (t (mv *ts-nil* (puffert ttree)))))
        ((ts-intersectp ts1 ts2)
         (mv *ts-boolean* (puffert ttree0)))
        (t (mv *ts-nil* (puffert ttree)))))

;; RAG - I added entries here for realp evg.  This is probably not
;; needed, since we can't construct realp numbers!

(defun type-set-quote (evg)

; Most type-set-xxx functions return a pair consisting of a ts and a ttree.
; But the ttree is irrelevant here and so we don't waste the time passing
; it around.  We return the ts of the evg.

  (cond ((atom evg)
         (cond ((rationalp evg)
                (cond ((integerp evg)
                       (cond ((int= evg 0) *ts-zero*)
                             ((> evg 0) *ts-positive-integer*)
                             (t *ts-negative-integer*)))
                      ((> evg 0) *ts-positive-ratio*)
                      (t *ts-negative-ratio*)))

               #+:non-standard-analysis
               ((realp evg)
                (cond ((> evg 0) *ts-positive-non-ratio*)
                      (t *ts-negative-non-ratio*)))

               ((complex-rationalp evg)
                *ts-complex-rational*)

               #+:non-standard-analysis
               ((complexp evg)
                *ts-complex-non-rational*)

               ((symbolp evg)
                (cond ((eq evg t) *ts-t*)
                      ((eq evg nil) *ts-nil*)
                      (t *ts-non-t-non-nil-symbol*)))
               ((stringp evg) *ts-string*)
               (t *ts-character*)))
        ((true-listp evg)
         *ts-proper-cons*)
        (t *ts-improper-cons*)))

(defun type-set-char-code (ts ttree ttree0)

; (char-code x) is always a non-negative integer.  If x is not a
; characterp, then its code is 0.  If x is a character, its code
; might be 0 or positive.

  (cond ((not (ts-intersectp ts *ts-character*))
         (mv *ts-zero* (puffert ttree)))
        (t (mv *ts-non-negative-integer* (puffert ttree0)))))

(mutual-recursion

(defun var-fn-count (term)

; We return a triple --- the variable count, the function count,
; and the pseudo-function count --- derived from term.

; The fn count of a term is the number of function symbols in the
; unabbreviated term.  Thus, the fn count of (+ (f x) y) is 2.
; The primitives of ACL2, however, do not give very natural expression
; of the constants of the logic in pure first order form, i.e., as a
; variable-free nest of function applications.  What, for
; example, is 2?  It can be written (+ 1 (+ 1 0)), where 1 and 0 are
; considered primitive constants, i.e., 1 is (one) and 0 is (zero).
; That would make the fn count of 2 be 5.  However, one cannot even
; write a pure first order term for NIL or any other symbol or string
; unless one adopts NIL and 'STRING as primitives too.  It is probably
; fair to say that the primitives of CLTL were not designed for the
; inductive construction of the objects in an orderly way.  But we would
; like for our accounting for a constant to somehow reflect the structure
; of the constant rather than the structure of the rather arbitrary CLTL
; term for constructing it.  'A seems to have relatively little to do
; with (intern (coerce (cons #\A 'NIL) 'STRING)) and it is odd that
; the fn count of 'A should be larger than that of 'STRING, and odder
; still that the fn count of "STRING" be larger than that of 'STRING
; since the latter is built from the former with intern.

; We therefore adopt a story for how the constants of ACL2 are
; actually constructed inductively and the pseudo-fn count is the number
; of symbols in that construction.  The story is as follows.  (z), zero,
; is the only primitive integer.  Positive integers are constructed
; from it by the successor function s.  Negative integers are
; constructed from positive integers by unary minus.  Ratios are
; constructed by the dyadic function quo on an integer and a natural.
; For example, -2/3 is inductively built as (quo (- (s(s(z))))
; (s(s(s(z))))).  Complex rationals are similarly constructed from
; pairs of rationals.  All characters are primitive and are constructed by
; the function of the same name.  Strings are built from the empty
; string, (o), by "string-cons", cs, which adds a character to a
; string.  Thus "AB" is formally (cs (#\A) (cs (#\B) (o))).  Symbols
; are constructed by "packing" a string with p.  Conses are conses, as
; usual.  Again, this story is nowhere else relevant to ACL2; it just
; provides a consistent model for answering the question "how big is a
; constant?"

; Previously we had made no distinction between the fn-count and the
; pseudo-fn-count, but Jun Sawada ran into difficulty because
; (term-order (f) '2).  Note also that before we had
; (term-order (a (b (c (d (e (f (g (h (i x))))))))) (foo y '2/3))
; but
; (term-order (foo y '1/2) (a (b (c (d (e (f (g (h (i x)))))))))).

  (declare (xargs :guard (pseudo-termp term)))
  (cond ((variablep term)
         (mv 1 0 0))
        ((fquotep term)
         (mv 0 
             0
             (fn-count-evg (cadr term))))
        (t (mv-let (v f p-f)
                   (var-fn-count-lst (fargs term))
                   (mv v (+ 1 f) p-f)))))

(defun var-fn-count-lst (lst)
  (declare (xargs :guard (pseudo-term-listp lst)))
  (cond ((null lst)
         (mv 0 0 0))
        (t (mv-let (v1 f1 p-f1)
                   (var-fn-count (car lst))
                   (mv-let (v2 f2 p-f2)
                           (var-fn-count-lst (cdr lst))
                           (mv (+ v1 v2) (+ f1 f2) (+ p-f1 p-f2)))))))

)

(defun fn-count-1 (flg x fn-count-acc p-fn-count-acc)

; Keep this in sync with the var-fn-count/var-fn-count-lst nest.

; This definition is derived from the var-fn-count nest, except that it counts
; only fns and pseudo-fns, not vars, and it uses tail recursion.  It was
; introduced when a check of fn-counts was added to ancestors-check1, in order
; to improve efficiency a bit.  A 2.6% decrease in user time (using Allegro
; 5.0.1) was observed when that check was added, yet that test was found to be
; critical in certain cases (see the comment in ancestors-check1).  So, here we
; attempt to improve the efficiency of computing the fn-count.

; We discovered that the Allegro compiler does not do as good a job at tail
; recursion elimination for mutual recursion nests as for single recursion.  So
; we code this as a singly recursive function with a flag, flg:  When flg is
; nil then x is a term, and otherwise x is a list of terms.

  (declare (xargs :guard (and (if flg
                                  (pseudo-term-listp x)
                                (pseudo-termp x))
                              (integerp fn-count-acc)
                              (integerp p-fn-count-acc))))
  (cond (flg
         (cond ((null x)
                (mv fn-count-acc p-fn-count-acc))
               (t
                (mv-let (fn-cnt p-fn-cnt)
                        (fn-count-1 nil (car x) fn-count-acc p-fn-count-acc)
                        (fn-count-1   t (cdr x) fn-cnt p-fn-cnt)))))
        ((variablep x)
         (mv fn-count-acc p-fn-count-acc))
        ((fquotep x)
         (mv fn-count-acc
             (+ p-fn-count-acc (fn-count-evg (cadr x)))))
        (t (fn-count-1 t (fargs x) (1+ fn-count-acc) p-fn-count-acc))))

(defmacro fn-count (term)
  `(fn-count-1 nil ,term 0 0))

(defun term-order (term1 term2)

; A simple -- or complete or total -- ordering is a relation satisfying:
; "antisymmetric" XrY & YrX -> X=Y, "transitive" XrY & Y&Z -> XrZ, and
; "trichotomy" XrY v YrX.  A partial order weakens trichotomy to "reflexive"
; XrX.

; Term-order is a simple ordering on terms.  (term-order term1 term2) if and
; only if (a) the number of occurrences of variables in term1 is strictly
; less than the number in term2, or (b) the numbers of variable occurrences
; are equal and the fn-count of term1 is strictly less than that of term2,
; or (c) the numbers of variable occurrences are equal, the fn-counts are
; equal, and the pseudo-fn-count of term1 is strictly less than that of
; term2, or (d) the numbers of variable occurrences are equal, the fn-counts
; are equal, the pseudo-fn-counts are equal, and (lexorder term1 term2).

; Let (STRICT-TERM-ORDER X Y) be the LISP function defined as (AND
; (TERM-ORDER X Y) (NOT (EQUAL X Y))).  For a fixed, finite set of function
; symbols and variable symbols STRICT-TERM-ORDER is well founded, as can be
; proved with the following lemma.

; Lemma.  Suppose that M is a function whose range is well ordered
; by r and such that the inverse image of any member of the range is
; finite.  Suppose that L is a total order.  Define (LESSP x y) =
; (OR (r (M x) (M y)) (AND (EQUAL (M x) (M y)) (L x y) (NOT (EQUAL x
; y)))). < is a well-ordering.  Proof.  Suppose ... < t3 < t2 < t1
; is an infinite descending sequence. ..., (M t3), (M t2), (M t1) is
; weakly descending but not infinitely descending and so has a least
; element.  WLOG assume ... = (M t3) = (M t2) = (M t1).  By the
; finiteness of the inverse image of (M t1), { ..., t3, t2, t1} is a
; finite set, which has a least element under L, WLOG t27.  But t28
; L t27 and t28 /= t27 by t28 < t27, contradicting the minimality of
; t27.  QED

; If (TERM-ORDER x y) and t2 results from replacing one occurrence of
; y with x in t1, then (TERM-ORDER t2 t1).  Cases on why x is less
; than y.  1. If the number of occurrences of variables in x is
; strictly smaller than in y, then the number in t2 is strictly
; smaller than in t1.  2. If the number of occurrences of variables in
; x is equal to the number in y but the fn-count of x is smaller than
; the fn-count of y, then the number of variable occurrences in t1 is
; equal to the number in t2 but the fn-count of t1 is less than the
; fn-count of t2.  3. A similar argument to the above applies if the
; number of variable occurrences and fn-counts are the same but the
; pseudo-fn-count of x is smaller than that of y.  4. If the number of
; variable occurrences and parenthesis occurrences in x and y are the
; same, then (LEXORDER x y).  (TERM-ORDER t2 t1) reduces to (LEXORDER
; t2 t1) because the number of variable and parenthesis occurrences in
; t2 and t1 are the same.  The lexicographic scan of t1 and t2 will be
; all equals until x and y are hit.

  ":Doc-Section Miscellaneous

  the ordering relation on terms used by ACL2~/

  ACL2 must occasionally choose which of two terms is syntactically
  smaller.  The need for such a choice arises, for example, when using
  equality hypotheses in conjectures (the smaller term is substituted
  for the larger elsewhere in the formula), in stopping loops in
  permutative rewrite rules (~pl[loop-stopper]), and in choosing
  the order in which to try to cancel the addends in linear arithmetic
  inequalities.  When this notion of syntactic size is needed, ACL2
  uses ``term order.''  Popularly speaking, term order is just a
  lexicographic ordering on terms.  But the situation is actually more
  complicated.~/

  We define term order only with respect to terms in translated form.
  ~l[trans].  Constants are viewed as built up by ~em[pseudo-function]
  applications, as described at the end of this documentation.

  ~c[Term1] comes before ~c[term2] in the term order iff~bq[]

  (a) the number of variable occurrences in ~c[term1] is less than that in
  ~c[term2], or

  (b) the numbers of variable occurrences in the two terms are equal
  but the number of function applications in ~c[term1] is less than that
  in ~c[term2], or

  (c) the numbers of variable occurrences in the two terms are equal,
  the numbers of functions applications in the two terms are equal,
  but the number of pseudo-function applications in ~c[term1] is less
  than that in ~c[term2], or

  (d) the numbers of variable occurrences in the two terms are equal,
  the numbers of functions applications in the two terms are equal,
  the numbers of pseudo-function applications in the two terms are equal,
  and ~c[term1] comes before ~c[term2] in a lexicographic ordering, ~ilc[lexorder],
  based their structure as Lisp objects:  ~pl[lexorder].

  ~eq[]The function ~c[term-order], when applied to the translations of two
  ACL2 terms, returns ~c[t] iff the first is ``less than or equal'' to the
  second in the term order.

  By ``number of variable occurrences'' we do not mean ``number of
  distinct variables'' but ``number of times a variable symbol is
  mentioned.''  ~c[(cons x x)] has two variable occurrences, not one.
  Thus, perhaps counterintuitively, a large term that contains only
  one variable occurrence, e.g., ~c[(standard-char-p (car (reverse x)))]
  comes before ~c[(cons x x)] in the term order.

  Since constants contain no variable occurrences and non-constant
  expressions must contain at least one variable occurrence, constants
  come before non-constants in the term order, no matter how large the
  constants.  For example, the list constant
  ~bv[]
  '(monday tuesday wednesday thursday friday)
  ~ev[]
  comes before ~c[x] in the term order.  Because term order is involved
  in the control of permutative rewrite rules and used to shift
  smaller terms to the left, a set of permutative rules designed to
  allow the permutation of any two tips in a tree representing the
  nested application of some function will always move the constants
  into the left-most tips.  Thus,
  ~bv[]
  (+ x 3 (car (reverse klst)) (dx i j)) ,
  ~ev[]
  which in translated form is
  ~bv[]
  (binary-+ x
            (binary-+ '3
                      (binary-+ (dx i j)
                                (car (reverse klst))))),
  ~ev[]
  will be permuted under the built-in commutativity rules to
  ~bv[]
  (binary-+ '3
            (binary-+ x
                      (binary-+ (car (reverse klst))
                                (dx i j))))
  ~ev[]
  or
  ~bv[]
  (+ 3 x (car (reverse klst)) (dx i j)).
  ~ev[]
  Two terms with the same numbers of variable occurrences, function
  applications, and pseudo-function applications are ordered by
  lexicographic means, based on their structures.  ~l[lexorder].
  Thus, if two terms ~c[(member ...)] and ~c[(reverse ...)] contain the same
  numbers of variable occurrences and function applications, then the
  ~ilc[member] term is first in the term order because ~ilc[member] comes before
  ~ilc[reverse] in the term order (which is here reduced to alphabetic
  ordering).

  It remains to discuss the notion of ~em[pseudo-function] applications.

  Clearly, two constants are ordered using cases (c) and (d) of term
  order, since they each contain 0 variable occurrences and no
  function calls.  This raises the question ``How many function
  applications are in a constant?''  Because we regard the number of
  function applications as a more fundamental measure of the size of a
  constant than lexicographic considerations, we decided that for the
  purposes of term order, constants would be seen as being built by
  primitive constructor functions.  These constructor functions are
  not actually defined in ACL2 but merely imagined for the purposes of
  term order.  We here use suggestive names for these imagined
  functions, ignoring entirely the prior use of these names within
  ACL2.  The imagined applications of these functions are what we
  refer to as ~em[pseudo-function] applications.

  The constant function ~c[z] constructs ~c[0].  Positive integers are
  constructed from ~c[(z)] by the successor function, ~c[s].  Thus ~c[2] is
  ~c[(s (s (z)))] and contains three function applications.  ~c[100]
  contains one hundred and one applications.  Negative integers are
  constructed from their positive counterparts by ~ilc[-].  Thus, ~c[-2]
  is ~c[(- (s (s (z))))] and has four applications.  Ratios are
  constructed by the dyadic function ~ilc[/].  Thus, ~c[-1/2] is
  ~bv[]
  (/ (- (s (z))) (s (s (z))))
  ~ev[]
  and contains seven applications.  Complex rationals are similarly
  constructed from rationals.  All character objects are considered
  primitive and are constructed by constant functions of the same
  name.  Thus ~c[#\\a] and ~c[#\\b] both contain one application.
  Strings are built from the empty string, ~c[(o)] by the
  ``string-cons'' function written ~c[cs].  Thus ~c[\"AB\"] is
  ~c[(cs (#\\a) (cs (#\\b) (o)))] and contains five applications.
  Symbols are obtained from strings by ``packing'' the ~ilc[symbol-name]
  with the unary function ~c[p].  Thus ~c['ab] is
  ~bv[]
  (p (cs (#\\a) (cs (#\\b) (o))))
  ~ev[]
  and has six applications.  Note that packages are here ignored and
  thus ~c['acl2::ab] and ~c['my-package::ab] each contain just six
  applications.  Finally, ~il[cons]es are built with ~ilc[cons], as usual.
  So ~c['(1 . 2)] is ~c[(cons '1 '2)] and contains six applications,
  since ~c['1] contains two and ~c['2] contains three.  This, for better
  or worse, answers the question ``How many function applications are
  in a constant?''"

  (mv-let (var-count1 fn-count1 p-fn-count1)
    (var-fn-count term1)
    (mv-let (var-count2 fn-count2 p-fn-count2)
      (var-fn-count term2)
      (cond ((< var-count1 var-count2) t)
            ((> var-count1 var-count2) nil)
            ((< fn-count1 fn-count2) t)
            ((> fn-count1 fn-count2) nil)
            ((< p-fn-count1 p-fn-count2) t)
            ((> p-fn-count1 p-fn-count2) nil)
            (t (lexorder term1 term2))))))

; Type Prescriptions

; A type-prescription is a structure, below, that describes how to
; compute the type of a term.  They are stored on the property list of
; the top function symbol of the term, under the property
; 'type-prescriptions.  Unlike Nqthm's "type-prescription-lst" ANY
; enabled type-prescription in 'type-prescriptions may contribute to
; the type-set of the associated function symbol.

(defrec type-prescription
  (basic-ts (nume . term)
            hyps
            (vars . rune)
            . corollary)
  t)

; Term is a term, hyps is a list of terms, basic-ts is a type-set, and vars is
; a list of variables that occur in term.  Let term' be some instance of term
; under the substitution sigma.  Then, provided the sigma instance of hyps is
; true, the type-set of term' is the union of basic-ts with the type-sets of
; the sigma images of the vars.  Corollary is the theorem (translated term)
; from which this type-prescription was derived.  For system-generated
; type-prescriptions it is a term created by convert-type-prescription-to-term.

; (Note:  Why do we store the corollary when we could apparently
; recompute it with convert-type-prescription-to-term?  The reason is
; that the computation is sensitive to the ens in use and we do not
; wish the corollary for a type-prescription rule to change as the
; user changes the global enabled structure.)

; For example, for APP we might have the type-prescription:

; (make type-prescription :rune ... :nume ...
;       :term (app x y)
;       :hyps ((true-listp x))
;       :basic-ts *ts-cons*
;       :vars '(y)
;       :corollary (implies (true-listp x)
;                           (if (consp (app x y))
;                               't
;                               (equal (app x y) y))))

; The above example corresponds to what we'd get from the lemma:
; (implies (true-listp x)
;          (or (consp (app x y))
;              (equal (app x y) y)))

; When type-set uses :TYPE-PRESCRIPTION rules it will intersect all
; the known type-sets for term.

(defun find-runed-type-prescription (rune lst)

; Lst must be a list of type-prescription rules.  We find the first
; one with :rune rune.

  (cond ((null lst) nil)
        ((equal rune
                (access type-prescription (car lst) :rune))
         (car lst))
        (t (find-runed-type-prescription rune (cdr lst)))))

(defconst *expandable-boot-strap-non-rec-fns*
  '(not
    implies eq atom eql = /= null endp zerop

; If we ever make 1+ and 1- functions again, they should go back on this list.

    synp plusp minusp listp prog2$ must-be-equal time$ with-prover-time-limit
    force case-split double-rewrite))

; Warning: All functions listed above must be defun'd non-recursively
; in axioms.lisp!

; There has been some thought about whether we should put IFF on this
; list.  We have decided not, because type-set knows a lot about it by
; virtue of its being an equivalence relation.  But this position has
; never been seriously scrutinized.

; In a break with nqthm, we have decided to let type-set expand some
; function applications to get better type-sets for them.  The
; functions in question are those listed above.

; In an even more pervasive break, we have decided to make type-set
; keep track of the dependencies between literals of the goal clause
; and the type-sets computed.  The ttree argument to type-set below is
; a running accumulator that is returned as the second value of
; type-set.  Among the tags in the ttree are 'pt tags.  The value of
; the tag is a "parent tree" indicating the set of literals of the
; current-clause upon which the type deduction depends.  See the Essay
; on Parent Trees.  The type-alist in general contains entries of the
; form (term ts . ttree), where ttree is the tag tree encoding all of
; the 'PTs upon which depend the assertion that term has type-set ts.

; Note on Performance:

; An early time trial detected no measurable difference between the
; old type-set and the new when the ttree is t.  This was on a
; collection of simple defuns and thms (flatten, mc-flatten, their
; relation, and a guarded defun of (assoc-eq2 x y alist) where alist
; is a true list of triples of the form (sym1 sym2 . val)) that
; required a total of 15 seconds run-time in both versions.  However,
; because the only available "old" ACL2 is the first release, which
; does not have all of the proof techniques in place, and the new
; system does have them in place, it is difficult to make meaningful
; tests.  To make matters worse, we are about to go implement forward
; chaining.  The bottom line is whether ACL2 is fast enough.  We'll
; see...

; We now continue with the development of type-set.

(defun mv-atf (not-flg mbt mbf tta fta ttree1 ttree2)

; Every exit of assume-true-false is via this function.  See assume-
; true-false for details.  It is convenient, and only mildly wrong, to
; think of this function as equivalent to:

; (mv mbt mbf tta fta (cons-tag-trees ttree1 ttree2)).

; This is incorrect on two counts.  First, if not-flg is true, we swap
; the roles of mbt/mbf and tta/fta.  Second, since the ttree result of
; assume-true-false is irrelevant unless mbt or mbf is t, we sometimes
; produce a nil ttree.

; The reason this function takes two ttrees is that many (but not all)
; paths through assume-true-false have two ttrees in hand at the end.
; One is the ``xttree'' arg of assume-true-false, which was to be
; included in all the ttrees generated by the function.  The other is
; some local ttree that describes the derivation of facts during
; assume-true-false.  We could combine these two trees before calling
; mv-atf but that would, possibly, waste a cons since the ttrees are
; sometimes ignored.

; Finally, because we know that the ttrees are ignored when mbt and
; mbf are both nil, we sometimes pass in nil for the two ttrees in
; calls of mv-atf where we know they will be ignored.  Such a call
; should be taken (by the reader) as a clear signal that the ttrees
; are irrelevant.

  (if not-flg
      (mv mbf mbt fta tta
          (if (or mbt mbf)
              (cons-tag-trees ttree1 ttree2)
              nil))
      (mv mbt mbf tta fta
          (if (or mbt mbf)
              (cons-tag-trees ttree1 ttree2)
              nil))))

(defun assume-true-false-error (type-alist x temp-temp)
  (er
   hard 'assume-true-false-error
   "It was thought impossible for an equivalence relation, e.g., ~x0, ~
    to have anything besides a non-empty proper subset of ~
    *ts-boolean* on the type-alist!  But in the type-alist ~x1 the ~
    term ~x2 has type set ~x3."
   (ffn-symb x) type-alist x temp-temp))

(defun non-cons-cdr (term)
  (cond ((variablep term) term)
        ((fquotep term) term)
        ((eq (ffn-symb term) 'cons)
         (non-cons-cdr (fargn term 2)))
        (t term)))

; Because type-set now uses type-prescription rules with general
; patterns in them (rather than Nqthm-style rules for function
; symbols), we need one-way unification or pattern matching.

; One-way-unify1 can "see" (binary-+ 1 x) in 7, by letting x be 6.  Thus, we
; say that binary-+ is an "implicit" symbol to one-way-unify1.  Here is the
; current list of implicit symbols.  This list is used for heuristic reasons.
; Basically, a quick necessary condition for pat to one-way-unify with term is
; for the function symbols of pat (except for the implicit ones) to be a subset
; of the function smbols of term.

(defconst *one-way-unify1-implicit-fns*
  '(binary-+
    binary-*
    unary--
    unary-/
    intern-in-package-of-symbol
    coerce
    cons))

(mutual-recursion

(defun one-way-unify1 (pat term alist)

; This function is a "No-Change Loser" meaning that if it fails and returns nil
; as its first result, it returns the unmodified alist as its second.

  (declare (xargs :guard (and (pseudo-termp pat)
                              (pseudo-termp term)
                              (alistp alist))))
  (cond ((variablep pat)
         (let ((pair (assoc-eq pat alist)))
           (cond (pair (cond ((equal (cdr pair) term)
                              (mv t alist))
                             (t (mv nil alist))))
                 (t (mv t (cons (cons pat term) alist))))))
        ((fquotep pat)
         (cond ((equal pat term) (mv t alist))
               (t (mv nil alist))))
        ((variablep term) (mv nil alist))
        ((fquotep term)

; Caution:  If you change the code below, update *one-way-unify1-implicit-fns*.

; We have historically attempted to unify ``constructor'' terms with explicit
; values, and we try to simulate that here, treating the primitive arithmetic
; operators, intern-in-package-of-symbol, coerce (to a very limited extent),
; and, of course, cons, as constructors.

; In order to prevent loops, we insist that one-way-unification does not
; present the rewriter with ever-more-complex goals.  Robert Krug has sent the
; following examples, which motivated the controls in the code for binary-+ and
; binary-* below.

#|
 (defstub foo (x) t)
 (defaxiom foo-axiom
   (equal (foo (* 2 x))
          (foo x)))
 (thm
  (foo 4))
 :u
 (defaxiom foo-axiom
   (equal (foo (+ 1 x))
          (foo x)))
 (thm
   (foo 4))
|#

; Another interesting example is (thm (foo 4)) after replacing the second
; foo-axiom with (equal (foo (+ -1 x)) (foo x)).

         (cond ((acl2-numberp (cadr term))
                (let ((ffn-symb (ffn-symb pat)))
                  (case ffn-symb
                        (binary-+
                         (cond ((quotep (fargn pat 1))
                                (let ((new-evg
                                       (- (cadr term)
                                          (fix (cadr (fargn pat 1))))))
                                  (cond
                                   ((<= (acl2-count new-evg)
                                        (acl2-count (cadr term)))
                                    (one-way-unify1
                                     (fargn pat 2)
                                     (kwote new-evg)
                                     alist))
                                   (t (mv nil alist)))))
                               ((quotep (fargn pat 2))
                                (let ((new-evg
                                       (- (cadr term)
                                          (fix (cadr (fargn pat 2))))))
                                  (cond ((<= (acl2-count new-evg)
                                             (acl2-count (cadr term)))
                                         (one-way-unify1
                                          (fargn pat 1)
                                          (kwote new-evg)
                                          alist))
                                        (t (mv nil alist)))))
                               (t (mv nil alist))))
                        (binary-*
                         (cond ((or (not (integerp (cadr term)))
                                    (int= (cadr term) 0))
                                (mv nil alist))
                               ((and (quotep (fargn pat 1))
                                     (integerp (cadr (fargn pat 1)))
                                     (> (abs (cadr (fargn pat 1))) 1))
                                (let ((new-term-evg (/ (cadr term)
                                                       (cadr (fargn pat 1)))))
                                  (cond ((integerp new-term-evg)
                                         (one-way-unify1
                                          (fargn pat 2)
                                          (kwote new-term-evg)
                                          alist))
                                        (t (mv nil alist)))))
                               ((and (quotep (fargn pat 2))
                                     (integerp (cadr (fargn pat 2)))
                                     (> (abs (cadr (fargn pat 2))) 1))
                                (let ((new-term-evg (/ (cadr term)
                                                       (cadr (fargn pat 2)))))
                                  (cond ((integerp new-term-evg)
                                         (one-way-unify1
                                          (fargn pat 1)
                                          (kwote new-term-evg)
                                          alist))
                                        (t (mv nil alist)))))
                               (t (mv nil alist))))

; We once were willing to unify (- x) with 3 by binding x to -3.  John Cowles'
; experience with developing ACL2 arithmetic led him to suggest that we not
; unify (- x) with any constant other than negative ones.  Similarly, we do not
; unify (/ x) with any constant other than those between -1 and 1.  The code
; below reflects these suggestions.

                        (unary-- (cond ((>= (+ (realpart (cadr term))
                                               (imagpart (cadr term)))
                                            0)
                                        (mv nil alist))
                                       (t (one-way-unify1 (fargn pat 1)
                                                          (kwote (- (cadr term)))
                                                          alist))))
                        (unary-/ (cond ((or (>= (* (cadr term)
                                                   (conjugate (cadr term)))
                                                1)
                                            (eql 0 (cadr term)))
                                        (mv nil alist))
                                       (t (one-way-unify1 (fargn pat 1)
                                                          (kwote
                                                           (/ (cadr term)))
                                                          alist))))
                        (otherwise (mv nil alist)))))

               ((symbolp (cadr term))
                (cond
                 ((eq (ffn-symb pat) 'intern-in-package-of-symbol)
                  (let ((pkg (symbol-package-name (cadr term)))
                        (name (symbol-name (cadr term))))
                    (mv-let
                     (ans alist1)

; We are careful with alist to keep this a no change loser.

                     (one-way-unify1 (fargn pat 1) (kwote name) alist)
                     (cond
                      (ans

; We are unifying 'pkg::name with (intern-in-package-of-symbol x y) where x is
; now unified with "name".  So when is (intern-in-package-of-symbol "name" y)
; equal to pkg::name?  It would suffice to unify y with any symbol in pkg.  It
; might be that y is already such a quoted symbol.  Or perhaps we could unify y
; with pkg::name, which is one symbol we know is in pkg.  But note that it is
; not necessary that y unify with a symbol in pkg.  It would suffice, for
; example, if y could be unified with a symbol in some other package, say gkp,
; with the property that pkg::name was imported into gkp, for then gkp::name
; would be pkg::name.  Thus, as is to be expected by all failed unifications,
; failure does not mean there is no instance that is equal to the term.
; Suppose that y is not a quoted symbol and is not a variable (which could
; therefore be unified with pkg::name).  What else might unify with "any symbol
; in pkg?"  At first sight one might think that if y were
; (intern-in-package-of-symbol z 'pkg::name2) then the result is a symbol in
; pkg no matter what z is.  (The idea is that one might think that
; (intern-in-package-of-symbol z 'pkg::name2) is "the" generic expression of
; "any symbol in pkg.")  But that is not true because for certain z it is
; possible that the result isn't in pkg.  Consider, for example, the
; possibility that gkp::zzz is imported into pkg so that if z is "ZZZ" the
; result is a symbol in gkp not pkg.

                       (cond
                        ((and (nvariablep (fargn pat 2))
                              (fquotep (fargn pat 2)))
                         (cond
                          ((not (symbolp (cadr (fargn pat 2))))

; (intern-in-package-of-symbol x y) is NIL if y is not a symbol.  So we win if
; term is 'nil and lose otherwise.  If we win, note that x is unified
; (unnecessarily) with "NIL" in alist1 and so we report the win with alist!  If
; we lose, we have to report alist to be a no change loser.  So its alist
; either way.

                           (mv (if (equal term *nil*) ans nil)
                               alist))
                          (t (if (equal pkg
                                        (symbol-package-name
                                         (cadr (fargn pat 2))))
                                 (mv ans alist1)
                               (mv nil alist)))))
                        (t
                         (mv-let (ans alist2)
                                 (one-way-unify1 (fargn pat 2) term
                                                 alist1)
                                 (cond (ans (mv ans alist2))
                                       (t (mv nil alist)))))))
                      (t (mv nil alist))))))
                 (t (mv nil alist))))
               ((stringp (cadr term))
                (cond ((and (eq (ffn-symb pat) 'coerce)
                            (equal (fargn pat 2) ''string))
                       (one-way-unify1 (fargn pat 1)
                                       (kwote (coerce (cadr term) 'list))
                                       alist))
                      (t (mv nil alist))))
               ((consp (cadr term))
                (cond ((eq (ffn-symb pat) 'cons)

; We have to be careful with alist below so we are a no change loser.

                       (mv-let (ans alist1)
                         (one-way-unify1 (fargn pat 1)
                                         (kwote (car (cadr term)))
                                         alist)

                         (cond
                          (ans
                           (mv-let (ans alist2)
                                   (one-way-unify1 (fargn pat 2)
                                                   (kwote
                                                    (cdr (cadr term)))
                                                   alist1)
                                   (cond (ans (mv ans alist2))
                                         (t (mv nil alist)))))
                          (t (mv nil alist)))))
                      (t (mv nil alist))))
               (t (mv nil alist))))
        ((cond ((flambda-applicationp pat)
                (equal (ffn-symb pat) (ffn-symb term)))
               (t
                (eq (ffn-symb pat) (ffn-symb term))))
         (cond ((eq (ffn-symb pat) 'equal)
                (one-way-unify1-equal (fargn pat 1) (fargn pat 2)
                                      (fargn term 1) (fargn term 2)
                                      alist))
               (t (mv-let (ans alist1)
                          (one-way-unify1-lst (fargs pat) (fargs term) alist)
                          (cond (ans (mv ans alist1))
                                (t (mv nil alist)))))))
        (t (mv nil alist))))

(defun one-way-unify1-lst (pl tl alist)

; This function is NOT a No Change Loser.  That is, it may return nil
; as its first result, indicating that no substitution exists, but
; return as its second result an alist different from its input alist.

  (declare (xargs :guard (and (pseudo-term-listp pl)
                              (pseudo-term-listp tl)
                              (alistp alist))))
  (cond ((null pl) (mv t alist))
        (t (mv-let (ans alist)
             (one-way-unify1 (car pl) (car tl) alist)
             (cond
              (ans
               (one-way-unify1-lst (cdr pl) (cdr tl) alist))
              (t (mv nil alist)))))))

(defun one-way-unify1-equal1 (pat1 pat2 term1 term2 alist)

; At first glance, the following code looks more elaborate than
; necessary.  But this function is supposed to be a No Change Loser.
; The first time we coded this we failed to ensure that property.  The
; bug is the result of fuzzy thinking in the vicinity of conjunctive
; subgoals.  Suppose success requires success on x and success on y.
; The naive way to code it is (mv-let (ans nochanger) x (if ans y (mv
; nil nochanger))), i.e., to solve the x problem and if you win,
; return your solution to the y problem.  But if x wins it will have
; changed nochanger.  If y then loses, it returns the changed
; nochanger produced by x.  Clearly, if x might win and change things
; but ultimate success also depends on y, you must preserve the
; original inputs and explicitly revert to them if y loses.

  (mv-let (ans alist1)
    (one-way-unify1 pat1 term1 alist)
    (cond (ans
           (mv-let (ans alist2)
                   (one-way-unify1 pat2 term2 alist1)
                   (cond (ans (mv ans alist2))
                         (t (mv nil alist)))))
          (t (mv nil alist)))))

(defun one-way-unify1-equal (pat1 pat2 term1 term2 alist)
  (mv-let (ans alist)
    (one-way-unify1-equal1 pat1 pat2 term1 term2 alist)
    (cond
     (ans (mv ans alist))
     (t (one-way-unify1-equal1 pat2 pat1 term1 term2 alist)))))
)

(defun one-way-unify (pat term)
  (declare (xargs :guard (and (pseudo-termp pat)
                              (pseudo-termp term))))

; This function returns two values.  The first is T or NIL, according to
; whether unification succeeded.  The second value returned is a symbol alist
; that when substituted into pat will produce term, when the unification
; succeeded.

; The use of the phrase ``unify'' here is somewhat opaque but is
; historically justified by its usage in nqthm.  Really, all we are
; doing is matching because we do not treat the ``variable symbols''
; in term as instantiable.

; Note that the fact that this function returns nil should not be
; taken as a sign that no substition makes pat equal to term in the
; current theory.  For example, we fail to unify (+ x x) with '2 even
; though '((x . 1)) does the job.

  (one-way-unify1 pat term nil))

; Essay on the Invariants on Type-alists, and Canonicality

; There are four invariants on type-alists.

; First invariant on type-alists:  No quotep is bound in a type-alist.

; Second invariant on type-alists:  when (equiv x y) is bound in a type-alist,
; it is bound to a type of *ts-t* or *ts-nil*.

; Unlike the first two invariants, we will not depend on the third and fourth
; for soundness.  We'll present them in a moment.  We will maintain them both
; by insisting that the only operations allowed for extending type-alists are
; extend-type-alist-simple, extend-type-alist, extend-type-alist1, and
; extend-type-alist-with-bindings, and zip-variable-type-alist, called in
; accordance with their guards.

; Definition.  We say that a term u is "canonical" for an equivalence relation
; equiv of the current ACL2 world and a type-alist if no entry in type-alist is
; of the form ((equiv u z) *ts-t* . ttree).  When equiv and type-alist are
; understood, we may say simply that u is canonical.

; Third invariant on type-alists: For every element ((equiv x y) ts . ttree) of
; a type-alist for which equiv is an equivalence relation in the current ACL2
; world, y is canonical.  Moreover, if ts is *ts-nil*, then x is also
; canonical; and, if ts is *ts-t*, then (term-order y x) and x is not y.
; Finally, for each x there is at most one entry in type-alist of the form
; ((equiv x y) *ts-t* . ttree); in this case, or when x = y and there is no
; entry of the form ((equiv y y') *ts-t* . ttree), we say that y is the
; canonical form of x.

; Although we have decided to maintain the third invariant, if later we decide
; not to be insistent on that, we may obtain some speed-up by replacing some
; calls of extend-type-alist by extend-type-alist-simple.  Look for the string
; ";;*** -simple" to see some places where that might be especially
; appropriate.  Note that even extend-type-alist-simple is careful to preserve
; the first two invariants.

; The fourth invariant on type-alists:  No term is ever bound to *ts-unknown*.

(defun canonical-representative (equiv term type-alist)

; This function returns a tuple (mv occursp canonicalp term-canon ttree)
; satifying the following description.

; Occursp holds iff, for some x, (equiv term x) or (equiv x term) is bound in
; type-alist.

; Canonicalp is t or nil, and it is t iff term is canonical (see Essay above).

; Term-canon is the canonical form of term, i.e., is term if canonicalp is t
; and otherwise is the unique x such that ((equiv term x) *ts-t* . tt) belongs
; to type-alist for some tt.

; Ttree is a tag tree justifying the equality of term to term-canon.

; We will use the following easy-to-prove theorem:

; (occursp = nil)
;   implies
; (canonicalp = t)
;   which implies
; (term-canon = term)

; We will also use the fact that if canonicalp is t then ttree is nil.

  (declare (xargs :guard (symbolp equiv)))
  (cond
   ((null type-alist)
    (mv nil t term nil))
   (t (let ((first-term (caar type-alist))
            (ts (cadar type-alist)))
        (cond ((or (variablep first-term)

; Recall the first invariant on type-alists:  type-alists do not bind quoteps.

                   (not (eq (ffn-symb first-term) equiv)))
               (canonical-representative equiv term (cdr type-alist)))
              ((equal term (fargn first-term 1))
               (cond ((ts= ts *ts-t*)
                      (mv t nil (fargn first-term 2) (cddar type-alist)))
                     (t (mv t t term nil))))
              ((equal term (fargn first-term 2))
               (mv t t term nil))
              (t (canonical-representative equiv term (cdr type-alist))))))))

(defun subst-type-alist1-check (old equiv type-alist)
  (cond
   ((null type-alist)
    nil)
   (t (or (let ((term (caar type-alist)))
            (and (nvariablep term)
                 (eq (ffn-symb term) equiv)
                 (or (equal old (fargn term 1))
                     (equal old (fargn term 2)))))
          (subst-type-alist1-check old equiv (cdr type-alist))))))

(defun subst-type-alist1 (new old equiv ttree type-alist acc)

; This subsidiary function of subst-type-alist is coded so that we do not do
; any more consing than necessary.  Moreover, we expect it to be extremely rare
; that old and new are already related (and hence negatively so) by equiv in
; type-alist; someone is calling this function to create such a relationship.

; See also the comment in subst-type-alist.

  (cond
   ((null type-alist)
    (reverse acc))
   (t
    (subst-type-alist1
     new old equiv ttree (cdr type-alist)
     (cons (let ((term (caar type-alist)))
             (cond
              ((and (nvariablep term)
                    (eq (ffn-symb term) equiv)
                    (or (equal old (fargn term 1))
                        (equal old (fargn term 2))))

; Note that since subst-type-alist1 is only called by subst-type-alist, and
; subst-type-alist assumes that new and old are canonical in type-alist and
; distinct, we know that the third invariant on type-alists is being preserved:
; we are not creating an entry binding (equiv new new) to *ts-t*.

               (list* (if (equal old (fargn term 1))
                          (cons-term* equiv new (fargn term 2))
                        (cons-term* equiv (fargn term 1) new))
                      (cadar type-alist)

; Note on Tracking Equivalence Runes: If we ever give runic names to the
; theorems establishing equivalence- relation-hood and track those names
; through geneqvs, then we ought to push the appropriate rune here, rather than
; use puffert, which was intended for primitives and is thus here somewhat
; misused unless perhaps equiv is 'equal.  There are many other places where
; this comment applies.  You should inspect every use of puffert below and ask
; the question: is equivalential reasoning happening here or is it really
; primitive type reasoning?  We added a function equivalence-rune to record
; commutativity in bdd processing, and this function may be of use here.

                      (puffert
                       (cons-tag-trees (cddar type-alist) ttree))))
              (t (car type-alist))))
           acc)))))

(defun subst-type-alist (new old equiv ttree type-alist)

; This function creates a new type-alist by replacing each term of the form
; (equiv old x) bound in type-alist by (equiv new x), and each remaining term
; of the form (equiv x old) bound in type-alist by (equiv x new), respectively.
; Each time it makes such a replacement it records ttree as the reason why that
; step is valid.

; We assume that new and old are canonical in type-alist and distinct.

  (cond
   ((subst-type-alist1-check old equiv type-alist)
    (subst-type-alist1 new old equiv ttree type-alist nil))
   (t type-alist)))

(defun infect-type-alist-entry (entry ttree)

; Entry is of the form (term ts . ttree1) and we add ttree to ttree1.

  (cons (car entry)
        (cons (cadr entry)
              (cons-tag-trees (cddr entry) ttree))))

(defun infect-new-type-alist-entries2 (new-type-alist old-type-alist ttree)

; We infect the newly modified entries in new-type-alist.  See
; infect-new-type-alist-entries.

  (cond ((null new-type-alist)
         nil)
        ((equal (caar new-type-alist)
                (caar old-type-alist))
         (cons (car new-type-alist)
               (infect-new-type-alist-entries2 (cdr new-type-alist)
                                               (cdr old-type-alist)
                                               ttree)))
        (t
         (cons (infect-type-alist-entry (car new-type-alist) ttree)
               (infect-new-type-alist-entries2 (cdr new-type-alist)
                                               (cdr old-type-alist)
                                               ttree)))))

(defun infect-new-type-alist-entries1 (new-type-alist old-type-alist ttree n)

; We infect the newly created entries in new-type-alist.  See
; infect-new-type-alist-entries.

  (if (zp n)
      (infect-new-type-alist-entries2 new-type-alist old-type-alist
                                      ttree)
    (cons (infect-type-alist-entry (car new-type-alist) ttree)
          (infect-new-type-alist-entries1 (cdr new-type-alist)
                                          old-type-alist
                                          ttree (1- n)))))

(defun infect-new-type-alist-entries (new-type-alist old-type-alist ttree)

; New type-alist is an extension of old-type-alist, and ttree
; contains any assumptions made while deriving the extension.  We
; need to infect the new entries with these assumptions.  This is
; made slightly more complex by the fact that new-type-alist may
; actually be an extension of a modification of old-type-alist
; due to equality facts being added.  (See extend-type-alist1.)
; However, that modification is still in 1:1 correspondence with the
; original, i.e., there are no new entries, just modified entries.

  (if (null ttree)
      new-type-alist
    (infect-new-type-alist-entries1 new-type-alist
                                    old-type-alist
                                    ttree
                                    (- (length new-type-alist)
                                       (length old-type-alist)))))

(defun extend-type-alist-simple (term ts ttree type-alist)

; This function extends type-alist, essentially by adding the entry (term ts .
; ttree).  However, this function preserves the first two invariants on
; type-alists; see the "Essay on the Invariants on Type-alists, and
; Canonicality."  See also extend-type-alist, which is similar but also
; preserves the third invariant on type-alists.

; This function should never be called on a term that is a call of an
; equivalence relation.  When viewed that way, it trivially preserves the third
; invariant on type-alists as well.

  (cond
   ((ts= ts *ts-unknown*) type-alist)
   ((variablep term)
    (cons (list* term ts ttree) type-alist))
   ((fquotep term) type-alist)
   (t (cons (list* term ts ttree) type-alist))))

(defun extend-type-alist1 (equiv occursp1 occursp2 both-canonicalp arg1-canon
                                 arg2-canon swap-flg term ts ttree type-alist)

; This function creates a type-alist in which, intuitively, we bind the term
; (equiv arg1-canon arg2-canon) to ts unless the order is "wrong", in which
; case we use (equiv arg2-canon arg1-canon) instead.

; More precisely, it returns a type-alist that is implied by the current
; type-alist together with the assertion that (equiv arg1-canon arg2-canon) has
; type-set ts, under the following assumptions:

; equiv is an equivalence relation in the current ACL2 world;

; (equiv arg1-canon arg2-canon) is the same as term when (and (not swap-flg)
; both-canonicalp) is non-nil;

; swap-flg is non-nil iff (term-order arg1-canon arg2-canon);

; occurs1p and arg1-canon are returned by some single call of the function
; canonical-representative;

; occurs2p and arg2-canon are returned by some single call of the function
; canonical-representative;

; arg1-canon and arg2-canon are canonical in type-alist (by the two preceding
; assumptions) and distinct.  This is important for the correctness of the
; calls of subst-type-alist; and

; ts is either *ts-t* or *ts-nil*.

  (cons (cond ((and (not swap-flg) both-canonicalp)

; Then term is the term to push on type-alist; no need to cons up a new term.

               (list* term ts ttree))
              (swap-flg (list* (cons-term* equiv arg2-canon arg1-canon)
                               ts (puffert ttree)))
              (t (list* (cons-term* equiv arg1-canon arg2-canon)
                        ts (puffert ttree))))
        (cond ((ts= ts *ts-nil*) type-alist)
              (swap-flg (cond
                         (occursp2

; It's easy to see that occursp2 holds if arg2-canon is an argument of an equiv
; term bound in type-alist, even without assuming that type-alist satisfies the
; third invariant on type-alists.  Hence if occurs2p fails, there is no
; substituting to be done.

                          (subst-type-alist arg1-canon arg2-canon equiv ttree
                                            type-alist))
                         (t type-alist)))
              (t (cond
                  (occursp1

; See comment above for the entirely analogous situation when swap-flg = t.

                   (subst-type-alist arg2-canon arg1-canon equiv ttree
                                     type-alist))
                  (t type-alist))))))

; Regarding the maintenance of the second invariant on type alists:
; In the case that

;   (and (not (ts= ts *ts-t*))
;        (not (ts= ts *ts-nil*))
;        (equivalence-relationp (ffn-symb term) wrld))

; we used to return an unchanged type-alist when extending a type-alist.
; However, we already implicitly use (I think) the fact that equivalence
; relations are boolean-valued.  So, we will do just a bit better in the new
; code.

; Certain violations of the Second invariant on type-alists -- when (equiv x y)
; is bound in a type-alist, it is bound to a type of *ts-t* or *ts-nil* -- is
; reported in assume-true-false by the error function assume-true-false-error,
; which has caught an error in the past.  See the "Essay on the Invariants on
; Type-alists, and Canonicality."

(defun extend-type-alist (term ts ttree type-alist wrld)

; This function extends type-alist so that term gets type-set ts with the
; indicated ttree.  Unlike extend-type-alist-simple, it pays careful attention
; to equivalence relations in an attempt to maintain the third invariant on
; type-alists; see the "Essay on the Invariants on Type-alists, and
; Canonicality."

  (declare (xargs :guard (and (pseudo-termp term)
                              (not (quotep term)))))
  (cond
   ((and (nvariablep term)
         (not (fquotep term))
         (equivalence-relationp (ffn-symb term) wrld))
    (cond
     ((equal (fargn term 1) (fargn term 2))

; It's bizarre to imagine (ts= ts *ts-t*) being false here, so we'll ignore the
; information we could obtain if it were false.

      type-alist)
     ((not (or (ts= ts *ts-t*)
               (ts= ts *ts-nil*)))
      (cond ((ts-intersectp ts *ts-nil*)
             type-alist)
            (t (extend-type-alist
                term *ts-t* (puffert ttree) type-alist wrld))))
     (t (let ((equiv (ffn-symb term))
              (arg1 (fargn term 1))
              (arg2 (fargn term 2)))
          (mv-let (occursp1 canonicalp1 arg1-canon ttree1)
                  (canonical-representative equiv arg1 type-alist)
                  (mv-let (occursp2 canonicalp2 arg2-canon ttree2)
                          (canonical-representative equiv arg2 type-alist)
                          (cond
                           ((equal arg1-canon arg2-canon)
                            type-alist)
                           (t
                            (let ((swap-flg (term-order arg1-canon
                                                        arg2-canon)))
                              (extend-type-alist1
                               equiv occursp1 occursp2
                               (and canonicalp1 canonicalp2)
                               arg1-canon arg2-canon
                               swap-flg
                               term ts
                               (cons-tag-trees ttree1
                                               (cons-tag-trees ttree2 ttree))
                               type-alist))))))))))
   (t (extend-type-alist-simple term ts ttree type-alist))))

(defun zip-variable-type-alist (vars pairs)

; Vars must be a list of distinct variables.  Pairs must be a list of the
; same length as vars, pairing type-sets to ttrees.  This function is
; like (pairlis$ vars pairs) except that it deletes any binding to *ts-unknown*.
; Under the guards stated, we guarantee the result is a type-alist satisfying
; our invariants.

  (cond ((null vars) nil)
        ((ts= (caar pairs) *ts-unknown*)
         (zip-variable-type-alist (cdr vars) (cdr pairs)))
        (t (cons (cons (car vars) (car pairs))
                 (zip-variable-type-alist (cdr vars) (cdr pairs))))))

(defun assoc-equiv (fn arg1 arg2 alist)

; This function is equivalent to 
; (or (assoc-equal (list fn arg1 arg2) alist)
;     (assoc-equal (list fn arg2 arg1) alist))
; except that it looks for both at the same time and returns whichever
; one it finds first.  We assume that the car of each pair in
; alist is a non-quote term.

  (cond ((eq alist nil) nil)
        ((and (not (variablep (caar alist)))
              (eq (ffn-symb (caar alist)) fn)
              (if (equal (fargn (caar alist) 2) arg2)
                  (equal (fargn (caar alist) 1) arg1)
                (and (equal (fargn (caar alist) 1) arg2)
                     (equal (fargn (caar alist) 2) arg1))))
         (car alist))
        (t (assoc-equiv fn arg1 arg2 (cdr alist)))))

(defun assoc-equiv+ (equiv arg1 arg2 type-alist)

; This function body closely parallels code in the 'equal and
; equivalence-relationp cases of assume-true-false.

  (cond
   ((equal arg1 arg2)
    (mv *ts-t* (puffert nil)))
   ((and (eq equiv 'equal) (quotep arg1) (quotep arg2))
    (mv *ts-nil* (push-lemma '(:executable-counterpart equal) nil)))
   (t
    (mv-let
     (occursp1 canonicalp1 arg1-canon ttree1)
     (canonical-representative equiv arg1 type-alist)
     (declare (ignore canonicalp1))
     (cond
      ((and occursp1 (equal arg1-canon arg2))
       (mv *ts-t* (puffert ttree1)))
      ((and occursp1 (eq equiv 'equal) (quotep arg1-canon) (quotep arg2))
       (mv *ts-nil* (push-lemma '(:executable-counterpart equal) ttree1)))
      (t
       (mv-let
        (occursp2 canonicalp2 arg2-canon ttree2)
        (canonical-representative equiv arg2 type-alist)
        (declare (ignore canonicalp2))
        (cond
         ((and occursp2 (equal arg1-canon arg2-canon))
          (mv *ts-t* (puffert (cons-tag-trees ttree1 ttree2))))
         ((and (eq equiv 'equal) occursp2 (quotep arg1-canon) (quotep arg2-canon))
          (mv *ts-nil* (push-lemma '(:executable-counterpart equal)
                                   (cons-tag-trees ttree1 ttree2))))
         (t
          (let ((temp-temp
                 (assoc-equiv equiv arg1-canon arg2-canon type-alist)))
            (cond
             (temp-temp
              (cond ((ts= (cadr temp-temp) *ts-t*)

; See comment in corresponding place in the 'equal case of assume-true-false.

                     (mv (er hard 'assoc-equiv+
                             "Please send the authors of ACL2 a replayable ~
                              transcript of this problem if possible, so that ~
                              they can see what went wrong in the function ~
                              assoc-equiv+.  The offending call was ~x0.  The ~
                              surprising type-set arose from a call of ~x1."
                             (list 'assoc-equiv+
                                   (kwote equiv) (kwote arg1) (kwote arg2)
                                   type-alist)
                             (list 'assoc-equiv
                                   (kwote equiv)
                                   (kwote arg1-canon) (kwote arg2-canon)
                                   '<same_type-alist>))
                         nil))
                    ((ts= (cadr temp-temp) *ts-nil*)
                     (mv *ts-nil* (cons-tag-trees
                                   (cddr temp-temp)
                                   (cons-tag-trees ttree1 ttree2))))
                    (t
                     (let ((erp (assume-true-false-error
                                 type-alist
                                 (mcons-term* equiv arg1-canon arg2-canon)
                                 (cadr temp-temp))))
                       (mv erp nil)))))
             (t (mv nil nil)))))))))))))

(defun assoc-type-alist (term type-alist wrld)
  (cond ((variablep term)
         (let ((temp (assoc-eq term type-alist)))
           (if temp
               (mv (cadr temp) (cddr temp))
             (mv nil nil))))
        ((fquotep term) (mv nil nil))
        ((equivalence-relationp (ffn-symb term) wrld)
         (assoc-equiv+ (ffn-symb term)
                       (fargn term 1)
                       (fargn term 2)
                       type-alist))
        (t (let ((temp (assoc-equal term type-alist)))
             (if temp
                 (mv (cadr temp) (cddr temp))
               (mv nil nil))))))

(defun look-in-type-alist (term type-alist wrld)
  (mv-let (ts ttree)
    (assoc-type-alist term type-alist wrld)
    (mv (if ts ts *ts-unknown*) ttree)))

(defun member-char-stringp (chr str i)
  (cond ((< i 0) nil)
        (t (or (eql chr (char str i))
               (member-char-stringp chr str (1- i))))))

(defun terminal-substringp1 (str1 str2 max1 max2)
  (declare (xargs :guard (and (integerp max1)
                              (integerp max2)
                              (<= max1 max2))))
  (cond ((< max1 0) t)
        ((eql (char str1 max1) (char str2 max2))
         (terminal-substringp1 str1 str2 (1- max1) (1- max2)))
        (t nil)))

(defun terminal-substringp (str1 str2 max1 max2)
  (cond ((< max2 max1) nil)
        (t (terminal-substringp1 str1 str2 max1 max2))))

(defun evg-occur (x y)

; Consider the idealized inductive construction of the ACL2 objects x
; and y as described in the comment for var-fn-count.  Imagine that x
; and y are so represented.  Then this function answers the question:
; "Does x occur in y?"

;  Christ, I guess we have to look into symbol-package-names too???
;  Is this just heuristic?

  (cond ((atom y)
         (cond ((characterp y) (and (characterp x) (eql x y)))
               ((stringp y)
                (cond ((characterp x)
                       (member-char-stringp x y (1- (length y))))
                      ((stringp x)
                       (terminal-substringp x y
                                            (1- (length x))
                                            (1- (length y))))
                      (t nil)))
               ((symbolp y)
                (cond ((characterp x)
                       (let ((sny (symbol-name y)))
                         (member-char-stringp x sny (1- (length sny)))))
                      ((stringp x)
                       (let ((sny (symbol-name y)))
                         (terminal-substringp x sny
                                              (1- (length x))
                                              (1- (length sny)))))
                      ((symbolp x) (eq x y))
                      (t nil)))
               ((integerp y)
                (and (integerp x)
                     (or (int= x y)
                         (and (<= 0 x)
                              (<= x (if (< y 0) (- y) y))))))
               ((rationalp y)

; We know y is a non-integer rational.  X occurs in it either because
; x is the same non-integer rational or x is an integer that occurs in
; the numerator or denominator.

                (cond ((integerp x)
                       (or (evg-occur x (numerator y))
                           (evg-occur x (denominator y))))
                      ((rationalp x) (= x y))
                      (t nil)))
               (t

; We know y is a complex rational.  X occurs in it either because
; x is the same complex rational or x is a rational that occurs in
; the real or imaginary part.

                (cond ((rationalp x)
                       (or (evg-occur x (realpart y))
                           (evg-occur x (imagpart y))))
                      ((complex-rationalp x) (= x y))
                      (t nil)))))
        (t (or (evg-occur x (car y))
               (evg-occur x (cdr y))))))

(mutual-recursion

(defun occur (term1 term2)
  (cond ((variablep term2)
         (eq term1 term2))
        ((fquotep term2)
         (cond ((quotep term1)
                (evg-occur (cadr term1) (cadr term2)))
               (t nil)))
        ((equal term1 term2) t)
        (t (occur-lst term1 (fargs term2)))))

(defun occur-lst (term1 args2)
  (cond ((null args2) nil)
        (t (or (occur term1 (car args2))
               (occur-lst term1 (cdr args2))))))
)

; Rockwell Addition:  I found an exponential explosion in worse-than
; and it is fixed here.

; Up through Version  2.5 worse-than was defined as shown below:

; (defun worse-than (term1 term2)
;   (cond ((quick-worse-than term1 term2) t)
;         ((variablep term1) nil)
;         ((fquotep term1) nil)
;         (t (worse-than-lst (fargs term1) term2))))

; But we discovered via Rockwell examples that this performs terribly
; if term1 and term2 are variants of each other, i.e., the same up to
; the variables used.  So we have implemented a short circuit.

(mutual-recursion

(defun pseudo-variantp (term1 term2)

; We determine whether term1 and term2 are identical up to the
; variables used, down to the variables in term1.

; If (pseudo-variantp term1 term2) is true then we know that
; (worse-than term1 term2) is nil.

; Note: In the theorem proving literature, the word ``variant'' is
; used to mean that the two terms are identical up to a renaming of
; variables.  That is checked by our function variantp.  This function
; is different and of little logical use.  It does not insist that a
; consistent renaming of variable occur, just that the two terms are
; isomorphic down to the variable symbols.  It is here to avoid a very
; bad case in the worse-than check.

  (cond ((variablep term1)
         
; Suppose that term1 is a variable.  The only thing that it can be
; worse than is a quote.  That is, if we return t, then we must ensure
; that either term2 is term1 or (worse-than term1 term2) is nil.  The
; worse-than will be nil unless term2 is a quote.  See the exponential
; sequences below.

         (not (quotep term2)))

        ((fquotep term1) (equal term1 term2))
        ((or (variablep term2)
             (fquotep term2))
         nil)
        (t (and (equal (ffn-symb term1) (ffn-symb term2))
                (pseudo-variantp-list (fargs term1) (fargs term2))))))

(defun pseudo-variantp-list (args1 args2)
  (cond ((endp args1) t)
        (t (and (pseudo-variantp (car args1) (car args2))
                (pseudo-variantp-list (cdr args1) (cdr args2)))))))

; It turns out that without the use of pseudo-variantp in the
; definition of worse-than, below, worse-than's cost grows
; exponentially on pseudo-variant terms.  Consider the sequence of
; terms (f a a), (f a (f a a)), ..., and the corresponding sequence
; with variable symbol b used in place of a.  Call these terms a1, a2,
; ..., and b1, b2, ...  Then if pseudo-variantp were redefined to
; return nil, here are the real times taken to do (worse-than a1 b1),
; (worse-than a2 b2), ...  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
; 0.000, 0.020, 0.080, 0.300, 1.110, 4.230, 16.390.  This was measured
; on a 330 MHz Pentium II.

#|
(progn
  (time
   (new-worse-than
    '(f a a)
    '(f b b)))

  (time
   (new-worse-than
    '(f a (f a a))
    '(f b (f b b))))

  (time
   (new-worse-than
    '(f a (f a (f a a)))
    '(f b (f b (f b b)))))

  (time
   (new-worse-than
    '(f a (f a (f a (f a a))))
    '(f b (f b (f b (f b b))))))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a a)))))
    '(f b (f b (f b (f b (f b b)))))))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a a))))))
    '(f b (f b (f b (f b (f b (f b b))))))))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a a)))))))
    '(f b (f b (f b (f b (f b (f b (f b b)))))))))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a a))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b b))))))))))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b b)))))))))))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a (f a a))))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b))))))))))))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b)))))))))))))

  (time
   (new-worse-than
    '(f a
        (f a (f a (f a (f a (f a (f a (f a (f a (f a (f a (f a a))))))))))))
    '(f b
        (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b))))))))))))))

  (time
   (new-worse-than
    '(f a
        (f a
           (f a
              (f a (f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))))))
    '(f b
        (f b
           (f b
              (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b)))))))))))))
    ))
  )
|#

; If pseudo-variantp is defined so that instead of (not (quotep
; term2)) it insists of (variablep term2) when (variablep term1), then
; the following sequence goes exponential even though the preceding
; one does not.

#|
(progn
  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b b)))))))))))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b))))))))))))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b)))))))))))))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b))))))))))))
    ))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b)))))))))))))
    ))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b))))))))))))))
    ))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b)))))))))))))))
    ))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b))))))))))))))))
    ))

  (time
   (new-worse-than
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b)))))))))))))))))
    ))
  )
|#
; with times of 0.000, 0.120, 0.250, 0.430, etc.  But with the current
; definition of pseudo-variantp, the sequence above is flat.

; However, the sequence with the terms commuted grows exponentially,
; still.

#|
(progn
  (time
   (new-worse-than
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b b)))))))))
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))))

  (time
   (new-worse-than
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b))))))))))
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))))

  (time
   (new-worse-than
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b)))))))))))
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))))

  (time
   (new-worse-than
    '(f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b))))))))))))
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    ))

  (time
   (new-worse-than
    '(f b
        (f b
           (f b
              (f b (f b (f b (f b (f b (f b (f b (f b (f b (f b b)))))))))))))
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    ))

  (time
   (new-worse-than
    '(f b
        (f b
           (f b
              (f b
                 (f b
                    (f b
                       (f b (f b (f b (f b (f b (f b (f b (f b b))))))))))))))
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    ))

  (time
   (new-worse-than
    '(f b
        (f b
           (f b
              (f b
                 (f b
                    (f b
                       (f b
                          (f b
                             (f b
                                (f b (f b (f b (f b (f b (f b b)))))))))))))))
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    ))

  (time
   (new-worse-than
    '(f b
        (f b
           (f b
              (f b
                 (f b
                    (f b
                       (f b
                          (f b
                             (f b
                                (f b
                                   (f b
                                      (f b
                                         (f b (f b (f b (f b b))))))))))))))))
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    ))

  (time
   (new-worse-than
    '(f b
        (f b
           (f b
              (f b
                 (f b
                    (f b
                       (f b
                          (f b
                             (f b
                                (f b
                                   (f b
                                      (f b
                                         (f b
                                            (f b
                                               (f b
                                                  (f b
                                                     (f b b)))))))))))))))))
    '(f a (f a (f a (f a (f a (f a (f a (f a (f a a)))))))))
    ))
  )
|#

; Real times: 0.000, 0.000, 0.010, 0.000, 0.010, 0.020, 0.040, 0.100,
; 0.210, ...

(mutual-recursion

(defun worse-than (term1 term2)

; Term1 is worse-than term2 if it is basic-worse-than term2 or some
; proper subterm of it is worse-than or equal to term2.  However, we
; know that if two terms are pseudo-variants of eachother, then the
; worse-than relation does not hold.  

  (cond ((basic-worse-than term1 term2) t)
        ((pseudo-variantp term1 term2) nil)
        ((variablep term1) 

; If term1 is a variable and not basic-worse-than term2, what do we know
; about term2?  Term2 might be a variable.  Term2 cannot be quote.
; Term2 might be a function application.  So is X worse-than X or Y or
; (F X Y)?  No.

         nil)
        ((fquotep term1)

; If term1 is a quote and not basic-worse-than term2, what do we know
; about term2?  Term2 might be a variable.  Also, term2 might be a
; quote, but if it is, term2 is bigger than term1.  Term2 might be a
; function application.  So is term1 worse-than a bigger quote?  No.
; Is term1 worse-than a variable or function application?  No.

         nil)

        (t (worse-than-lst (fargs term1) term2))))



(defun worse-than-or-equal (term1 term2)

; This function is not really mutually recursive and could be removed
; from this nest.  It determines whether term1 is term2 or worse than
; term2.  This nest defines worse-than and does not use this function
; despite the use of similarly named functions.

; Note:  This function is supposed to be equivalent to
; (or (equal term1 term2) (worse-than term1 term2)).

; Clearly, that is equivalent to

; (if (pseudo-variantp term1 term2)
;     (or (equal term1 term2) (worse-than term1 term2))
;     (or (equal term1 term2) (worse-than term1 term2)))

; But if pseudo-variantp is true, then worse-than must return nil.
; And if pseudo-variantp is nil, then the equal returns nil.  So we
; can simplify the if above to:

  (if (pseudo-variantp term1 term2)
      (equal term1 term2)
    (worse-than term1 term2)))

(defun basic-worse-than-lst1 (args1 args2)

; Is some element of args2 ``uglier'' than the corresponding element
; of args1.  Technically, a2 is uglier than a1 if a1 is atomic (a
; variable or constant) and a2 is not or a2 is worse-than a1.

  (cond ((null args1) nil)
        ((or (and (or (variablep (car args1))
                      (fquotep (car args1)))
                  (not (or (variablep (car args2))
                           (fquotep (car args2)))))
             (worse-than (car args2) (car args1)))
         t)
        (t (basic-worse-than-lst1 (cdr args1) (cdr args2)))))

(defun basic-worse-than-lst2 (args1 args2)

; Is some element of arg1 worse-than the corresponding element of args2?

  (cond ((null args1) nil)
        ((worse-than (car args1) (car args2)) t)
        (t (basic-worse-than-lst2 (cdr args1) (cdr args2)))))

(defun basic-worse-than (term1 term2)

; We say that term1 is basic-worse-than term2 if 

; * term2 is a variable and term1 properly contains it, e.g., (F A B)
;   is basic-worse-than A;

; * term2 is a quote and term1 is either not a quote or is a bigger
;   quote, e.g., both X and '124 are basic-worse-than '17 and '(A B C D
;   E) is worse than 'X; or

; * term1 and term2 are applications of the same function and
;   no argument of term2 is uglier than the corresponding arg of term1, and
;   some argument of term1 is worse-than the corresponding arg of term2.

; The last case is illustrated by the fact that (F A B) is
; basic-worse-than (F A '17), because B is worse than '17, but (F '17
; B) is not basic-worse-than (F A '17) because A is worse than '17.
; Think of term2 as the old goal and term1 as the new goal.  Do we
; want to cut off backchaining?  Yes, if term1 is basic-worse-than
; term2.  So would we backchain from (F A '17) to (F '17 B)?  Yes,
; because even though one argument (the second) got worse (it went
; from 17 to B) another argument (the first) got better (it went from
; A to 17).

  (cond ((variablep term2)
         (cond ((eq term1 term2) nil)
               (t (occur term2 term1))))
        ((fquotep term2)
         (cond ((variablep term1) t)
               ((fquotep term1)
                (> (fn-count-evg (cadr term1))
                   (fn-count-evg (cadr term2))))
               (t t)))
        ((variablep term1) nil)
        ((fquotep term1) nil)
        ((cond ((flambda-applicationp term1)
                (equal (ffn-symb term1) (ffn-symb term2)))
               (t (eq (ffn-symb term1) (ffn-symb term2))))
         (cond ((pseudo-variantp term1 term2) nil)
               ((basic-worse-than-lst1 (fargs term1) (fargs term2)) nil)
               (t (basic-worse-than-lst2 (fargs term1) (fargs term2)))))
        (t nil)))

(defun some-subterm-worse-than-or-equal (term1 term2)

; Returns t if some subterm of term1 is worse-than or equal to term2.

  (cond ((variablep term1) (eq term1 term2))
        ((if (pseudo-variantp term1 term2)  ; see worse-than-or-equal
             (equal term1 term2)
           (basic-worse-than term1 term2))
         t)
        ((fquotep term1) nil)
        (t (some-subterm-worse-than-or-equal-lst (fargs term1) term2))))

(defun some-subterm-worse-than-or-equal-lst (args term2)
  (cond ((null args) nil)
        (t (or (some-subterm-worse-than-or-equal (car args) term2)
               (some-subterm-worse-than-or-equal-lst (cdr args) term2)))))

(defun worse-than-lst (args term2)

; We determine whether some element of args contains a subterm that is
; worse-than or equal to term2.  The subterm in question may be the
; element of args itself.  That is, we use ``subterm'' in the ``not
; necessarily proper subterm'' sense.

  (cond ((null args) nil)
        (t (or (some-subterm-worse-than-or-equal (car args) term2)
               (worse-than-lst (cdr args) term2)))))

)

; Here is how we add a frame to the ancestors stack.  

(defun push-ancestor (lit tokens ancestors)

; This function is used to push a new pair onto ancestors.  Lit is a
; term to be assumed true.  Tokens is a list of arbitrary objects.
; Generally, tokens is a singleton list containing the rune of a rule
; through which we are backchaining.  But when we rewrite forced
; assumptions we use the ``runes'' from the assumnotes (see defrec
; assumnote) as the tokens.  These ``runes'' are not always runes but
; may be symbols.
 
; Note:  It is important that the literal, lit, be in the car of the
; frame constructed below.

  (let* ((alit lit)
         (alit-atm (mv-let (not-flg atm)
                           (strip-not alit)
                           (declare (ignore not-flg))
                           atm)))
    (mv-let (fn-cnt-alit-atm p-fn-cnt-alit-atm)
            (fn-count alit-atm)
            (cons (list alit              ; the literal being assumed true
                                          ; (negation of hyp!)
                        alit-atm          ; the atom of that literal
                        fn-cnt-alit-atm   ; the fn-count of that atom
                        p-fn-cnt-alit-atm ; the pseudo-fn-count of that atom
                        tokens)          ; the runes involved in this backchain
                  ancestors))))

(defun earlier-ancestor-biggerp (fn-cnt p-fn-cnt tokens ancestors)

; We return t if some ancestor on ancestors has a bigger fn-count than
; fn-cnt and intersects with tokens.

  (cond ((null ancestors) nil)
        (t (let (; (alit            (car (car ancestors)))
                 ; (alit-atm        (cadr (car ancestors)))
                 (fn-cnt-alit-atm (caddr (car ancestors)))
                 (p-fn-cnt-alit-atm (cadddr (car ancestors)))
                 (atokens         (car (cddddr (car ancestors)))))
             (cond
              ((and (intersectp-equal tokens atokens)
                    (or (< fn-cnt fn-cnt-alit-atm)
                        (and (eql fn-cnt fn-cnt-alit-atm)
                             (< p-fn-cnt p-fn-cnt-alit-atm))))
               t)
              (t (earlier-ancestor-biggerp fn-cnt p-fn-cnt tokens
                                           (cdr ancestors))))))))

(defun ancestors-check1 (lit-atm lit fn-cnt p-fn-cnt ancestors tokens)
                                 
; Roughly speaking, ancestors is a list of all the things we can
; assume by virtue of our trying to prove their negations.  That is,
; when we backchain from B to A by applying (implies A B), we try to
; prove A and so we put (NOT A) on ancestors and can legitimately
; assume it (i.e., (NOT A)) true.  Roughly speaking, if lit is a
; member-equal of ancestors, we return (mv t t) and if the complement
; of lit is a member-equal we return (mv t nil).  If neither case
; obtains, we return (mv nil nil).

; We implement the complement check as follows.  lit-atm is the atom
; of the literal lit.  Consider a literal of ancestors, alit, and its
; atom, alit-atm.  If lit-atm is alit-atm and lit is not equal to alit,
; then lit and alit are complementary.  The following table supports
; this observation.  It shows all the combinations by considering that
; lit is either a positive or negative p, and alit is a p of either
; sign or some other literal of either sign.  The entries labeled =
; mark those when lit is alit.  The entries labeled comp mark those
; when lit and alit are complementary.

; lit \  alit:    p (not p) q (not q)

; p               =   comp  x    x
; (not p)         comp =    x    x

  (cond
   ((null ancestors)
    (mv nil nil))
   ((eq (caar ancestors) :binding-hyp)
    (ancestors-check1 lit-atm lit fn-cnt p-fn-cnt (cdr ancestors) tokens))
   (t
    (let ((alit              (car (car ancestors)))
          (alit-atm          (cadr (car ancestors)))
          (fn-cnt-alit-atm   (caddr (car ancestors)))
          (p-fn-cnt-alit-atm (cadddr (car ancestors)))
          (atokens           (car (cddddr (car ancestors)))))
      (cond
       ((equal alit lit)
        (mv t t))
       ((equal lit-atm alit-atm) (mv t nil))

; In Version_2.5, this function did not have the tokens argument.
; Instead we simply asked whether there was a frame on the ancestors
; stack such that fn-cnt was greater than or equal to the fn-count of
; the atom in the frame and lit-atm was worse-than-or-equal to the
; atom in the frame.  If so, we aborted with (mv t nil).  (The fn-cnt
; test is just an optimization because that inequality is implied by
; the worse-than-or-equal test.)  But Carlos Pacheco's TLA work
; exposed a situation in which the lit-atm was worse-than-or-equal to
; a completely unrelated atom on the ancestors stack.  So we added
; tokens and insisted that the ancestor so dominated by lit-atm was
; related to lit-atm by having a non-empty intersection with tokens.
; This was added in the final polishing of Version_2.6.  But we learned
; that it slowed us down about 10% because it allowed so much more
; backchaining.  We finally adopted a very conservative change
; targeted almost exactly to allow Carlos' example while preserving
; the rest of the old behavior. 

       ((intersectp-equal tokens atokens)

; We get here if the current lit-atm is related to that in the current
; frame.  We next ask whether the function symbols are the same and
; lit-atm is bigger.  If so, we abort.  Otherwise, we look for others.

        (cond ((and (nvariablep alit-atm)
                    (not (fquotep alit-atm))
                    (nvariablep alit-atm)
                    (not (fquotep lit-atm))
                    (equal (ffn-symb lit-atm) (ffn-symb alit-atm))
                    (or (> fn-cnt fn-cnt-alit-atm)
                        (and (eql fn-cnt fn-cnt-alit-atm)
                             (>= p-fn-cnt p-fn-cnt-alit-atm))))
               (mv t nil))
              (t (ancestors-check1 lit-atm lit fn-cnt p-fn-cnt
                                   (cdr ancestors) tokens))))
       ((and (or (> fn-cnt fn-cnt-alit-atm)
                 (and (eql fn-cnt fn-cnt-alit-atm)
                      (>= p-fn-cnt p-fn-cnt-alit-atm)))
             (worse-than-or-equal lit-atm alit-atm))

; The clause above is the old Version_2.5 test, but now it is tried
; only if the atms are unrelated by their tokens.  Most of the time we
; want to abort backchaining if we pass the check above.  But we want
; to allow continued backchaining in Carlos' example.  In that
; example:

; lit-atm          = (S::MEM (S::APPLY S::DISKSWRITTEN S::P)
;                            (S::POWERSET (S::DISK)))
; fn-cnt           = 4
; alit-atm         = (S::MEM S::D (S::DISK))
; fn-cnt-alit-atm  = 2

; with no token intersection.  Once upon a time we simply allowed all
; these, i.e., just coded a recursive call here.  But that really
; slowed us down by enabling a lot of backchaining.  So now we
; basically want to ask: "Is there a really good reason to allow this
; backchain?"  We've decided to allow the backchain if there is an
; earlier ancestor, related by tokens, to the ancestor that is trying
; to veto this one, that is bigger than this one.  In Carlos' example
; there is such a larger ancestor; but we suspect most of the time
; there isn't.  For example, at the very least it means that the
; vetoing ancestor must be the SECOND (or subsequent) time we've
; applied some rule on this backchaining path!  The first time we
; coded this heuristic we named the test ``random-coin-flip'' instead
; of earlier-ancestor-biggerp; the point: this is a pretty arbitrary
; decision heuristic mainly to make Carlos' example work.

        (cond ((earlier-ancestor-biggerp fn-cnt
                                         p-fn-cnt
                                         atokens
                                         (cdr ancestors))
               (ancestors-check1 lit-atm lit fn-cnt p-fn-cnt
                                 (cdr ancestors) tokens))
              (t (mv t nil))))
       (t (ancestors-check1 lit-atm lit fn-cnt p-fn-cnt
                            (cdr ancestors) tokens)))))))

; Note: In the type-set clique, and nowhere else, ancestors might be
; t.  The so-called t-ancestors hack is explained below.  But this
; function and push-ancestor above DO NOT implement the t-ancestors
; hack.  That is because they are used by the rewrite clique where
; there is no such hack, and we didn't want to slow that clique down.

(defun ancestors-check (lit ancestors tokens)

; We return two values.  The first is whether we should abort trying
; to establish lit on behalf of the given tokens.  The second is
; whether lit is (assumed) true in ancestors.

; We abort iff either lit is assumed true or else it is worse than or
; equal to some other literal we're trying to establish on behalf of
; some token in tokens.  (Actually, we compare the atoms of the two
; literals in the worse-than check.)

; A reminder about ancestors: Ancestors is a list of pairs.  Each pair
; is (term . tokens').  Term is something we can assume true (because
; we are currently trying to prove it false).  Tokens' is a list of
; tokens.  A token is a rune or a function symbol (or anything else,
; for all we care).  Generally speaking, tokens' is a singleton list
; containing a single rune, the one naming the lemma through which we
; are backchaining.  For example, if R is the lemma (IMPLIES p (EQUAL
; lhs rhs)) and we are trying to rewrite some target matching lhs, we
; backchain to establish p and we will add (NOT p) and the singleton
; list containing R to ancestors.

; Historical Note:  In nqthm, this function was named relieve-hyps-not-ok.

  (mv-let (not-flg lit-atm)
          (strip-not lit)
          (declare (ignore not-flg))
          (mv-let (fn-cnt p-fn-cnt)
                  (fn-count lit-atm)
                  (ancestors-check1 lit-atm lit fn-cnt p-fn-cnt
                                    ancestors tokens))))

(defun type-set-finish (ts0 ttree0 ts1 ttree1)

; We have obtained two type-set answers for some term.  Ts0 and ttree0
; were obtained by looking the term up in the type-alist; if ts0 is
; nil, then no binding was found in the type-alist.  Ts1 and ttree1
; were obtained by computing the type-set of the term "directly."
; Both are valid, provided ts0 is non-nil.  We intersect them.

; Note: Our answer must include ttree1 because that is the accumulated
; dependencies of the type-set computation to date!

  (cond ((null ts0)
         (mv ts1 ttree1))
        ((ts-subsetp ts1 ts0)

; This is an optimization.  We are about to intersect the type-sets and union
; the tag trees.  But if ts1 is a subset of ts0, the intersection is just ts1.
; We need not return ttree0 in this case; note that we must always return ttree1.

         (mv ts1 ttree1))
        (t (mv (ts-intersection ts0 ts1)
               (cons-tag-trees ttree0 ttree1)))))

(defun search-type-alist-rec (term alt-term typ type-alist unify-subst ttree)
  (cond ((null type-alist)
         (mv nil unify-subst ttree nil))
        ((ts-subsetp (cadr (car type-alist)) typ)
         (mv-let (ans unify-subst)
           (one-way-unify1 term (car (car type-alist)) unify-subst)
           (cond (ans
                  (mv t
                      unify-subst
                      (cons-tag-trees (cddr (car type-alist)) ttree)
                      (cdr type-alist)))
                 (alt-term
                  (mv-let (ans unify-subst)
                    (one-way-unify1 alt-term (car (car type-alist))
                                    unify-subst)
                    (cond (ans
                           (mv t
                               unify-subst
                               (cons-tag-trees (cddr (car type-alist)) ttree)
                               (cdr type-alist)))
                          (t (search-type-alist-rec term alt-term
                                                    typ
                                                    (cdr type-alist)
                                                    unify-subst
                                                    ttree)))))
                 (t (search-type-alist-rec term alt-term
                                           typ
                                           (cdr type-alist)
                                           unify-subst
                                           ttree)))))
        (t (search-type-alist-rec term alt-term
                                  typ
                                  (cdr type-alist)
                                  unify-subst
                                  ttree))))

(mutual-recursion

(defun free-varsp (term alist)
  (cond ((variablep term) (not (assoc-eq term alist)))
        ((fquotep term) nil)
        (t (free-varsp-lst (fargs term) alist))))

(defun free-varsp-lst (args alist)
  (cond ((null args) nil)
        (t (or (free-varsp (car args) alist)
               (free-varsp-lst (cdr args) alist)))))

)

(defun search-type-alist (term typ type-alist unify-subst ttree wrld)

; We search type-alist for an instance of term bound to a type-set
; that is a subset of typ.  Keep this in sync with search-type-alist+.

; For example, if typ is *ts-rational* then we seek an instance of
; term that is known to be a subset of the rationals.  Most commonly,
; typ is *ts-non-nil*.  In that case, we seek an instance of term
; that is non-nil.  Thus, this function can be thought of as trying to
; "make term true."  To use this function to "make term false," use
; the ts-complement of the desired type.  I.e., if you wish to find a
; false instance of term use *ts-nil*.

; By "instance" here we always mean an instance under an extension of
; unify-subst.  The extension is returned when we are successful.

; We return three values.  The first indicates whether we succeeded.
; The second is the final unify-subst.  The third is a modified ttree
; recording the literals used.  If we did not succeed, the second
; and third values are our input unify-subst and ttree.  I.e., we are
; a No-Change Loser.

; The No-Change Policy:  Many multi-valued functions here return a
; flag that indicates whether they "won" or "lost" and, in the case
; that they won, return "new values" for certain of their arguments.
; Here for example, we return a new value for unify-subst.  In early
; coding we adopted the policy that when they "lost" the additional
; values were irrelevant and were often nil.  This policy prevented
; the use of such forms as:
; (mv-let (wonp unify-subst)
;         (search-type-alist ... unify-subst ...)
;         (cond (wonp ...)
;               (t otherwise...)))
; because at otherwise... unify-subst was no longer what it had been before
; the search-type-alist.  Instead we had to think of a new name for
; it in the mv-let and use the appropriate one below.

; We then adopted what we now call the "No-Change Policy".  If a
; function returns a won/lost flag and some altered arguments, the
; No-Change Policy is that it returns its input arguments in case it
; loses.  We will note explicitly when a function is a No-Change
; Loser.

  (mv-let (term alt-term)
    (cond ((or (variablep term)
               (fquotep term)
               (not (equivalence-relationp (ffn-symb term) wrld)))
           (mv term nil))

; Otherwise, term is of the form (equiv term1 term2).  If term1 precedes term2
; in term-order, then term would be stored on a type-alist as (equiv term2
; term1); see the Essay on the Invariants on Type-alists, and Canonicality
; (specifically, the third invariant described there).  In such a case we may
; wish to search for the commuted version instead of term.  However, if there
; are free variables in term with respect to unify-subst then we need to search
; both for term and its commuted version, because the more specific term on
; type-alist can have its arguments in either term-order (unless we engage in a
; relatively expensive check; see e.g. maximal-terms).

          ((free-varsp term unify-subst)
           (mv term
               (fcons-term* (ffn-symb term) (fargn term 2) (fargn term 1))))
          (t (let ((arg1 (fargn term 1))
                   (arg2 (fargn term 2)))
               (cond ((term-order arg1 arg2)
                      (mv (fcons-term* (ffn-symb term)
                                       (fargn term 2)
                                       (fargn term 1))
                          nil))
                     (t (mv term nil))))))
    (mv-let (ans unify-subst ttree rest-type-alist)
      (search-type-alist-rec term alt-term typ type-alist unify-subst ttree)
      (declare (ignore rest-type-alist))
      (mv ans unify-subst ttree))))

(defun term-and-typ-to-lookup (hyp wrld)
  (mv-let
   (not-flg term)
   (strip-not hyp)
   (let* ((recog-tuple (and (nvariablep term)
                            (not (fquotep term))
                            (not (flambda-applicationp term))
                            (assoc-eq (ffn-symb term)
                                      (global-val 'recognizer-alist wrld))))
          (typ (if (and recog-tuple
                        (access recognizer-tuple recog-tuple :strongp))
                   (if not-flg
                       (access recognizer-tuple recog-tuple :false-ts)
                       (access recognizer-tuple recog-tuple :true-ts))
                   (if not-flg *ts-nil* *ts-non-nil*)))
          (term (if (and recog-tuple
                         (access recognizer-tuple recog-tuple :strongp))
                    (fargn term 1)
                    term)))
     (mv term typ))))

(defun lookup-hyp (hyp type-alist wrld unify-subst ttree)

; See if hyp is true by type-alist or simp-clause considerations --
; possibly extending the unify-subst.  If successful we return t, a
; new unify-subst and a new ttree.  No-Change Loser.

  (mv-let (term typ)
          (term-and-typ-to-lookup hyp wrld)
          (search-type-alist term typ type-alist unify-subst ttree wrld)))

(mutual-recursion

(defun sublis-var-and-mark-free (alist form)

; This function is rather odd: it is equivalent to (sublis-var alist'
; form) where alist' is derived from alist by adding a pair (var .
; ???-var) for each variable var in form that is not assigned a value
; by alist.  Thus it creates an instance of form.  However, the free
; vars of form are assigned essentially arbitrary variable values and
; while no two free vars are identified by this process, there is no
; guarantee that the variables introduced in their stead are "new."
; For example, ???-var may come into the instantiated term via alist.
; The only reason for this function is to highlight the free vars in a
; term upon which we will split, in the half-hearted hope that the
; user will spot it.

  (cond ((variablep form)
         (let ((a (assoc-eq form alist)))
           (cond (a (cdr a))
                 (t (packn (list "???-" form))))))
        ((fquotep form)
         form)
        (t (cons-term (ffn-symb form)
                      (sublis-var-and-mark-free-lst alist (fargs form))))))

(defun sublis-var-and-mark-free-lst (alist l)
  (if (null l)
      nil
    (cons (sublis-var-and-mark-free alist (car l))
          (sublis-var-and-mark-free-lst alist (cdr l)))))

)

; The Accumulated Persistence Essay

; We now develop the code needed to track accumulated-persistence.
; To activate accumulated-persistence, you must first call

; >(accumulated-persistence t)              ; activate and initialize
; > ...                                     ; do proofs
; >(show-accumulated-persistence :frames)   ; to display stats ordered by
;                                           ;  frame count
; >(accumulated-persistence nil)            ; deactivate

; A macro form is available for use in system code to support the tracking of
; accumulated persistence.  The special-form

; (with-accumulated-persistence rune (v1 ... vk) body)

; should be used in our source code whenever body is code that attempts to
; apply rune.  The list of variables, (v1 ... vk), tell us the multiplicity
; of body.  This form is logically equivalent to

; (mv-let (v1 ... vk) body (mv v1 ... vk))

; which is to say, it is logically equivalent to body itself.  (In the case
; where k is 1, we naturally use a let instead of an mv-let.)  However, we
; insert some additional code to accumulate the persistence of rune.

; The implementation of accumulated-persistence is as follows.  First, the
; necessary global data structures are maintained inside a wormhole called
; 'accumulated-persistence, so they can be accessed and changed anytime.  The
; data is maintained as the value of (f-global-val 'wormhole-output state) in
; that wormhole.  Recall that the 'wormhole-output of a wormhole named
; 'accumulated-persistence is restored upon entrance to the wormhole to the
; value it had upon the most recent exit of that wormhole.

; Our wormhole-output is a triple of the form (cnt stack . totals), where cnt
; is the total number of lemmas tried so far, stack is a list of old values of
; cnt, and totals is the accumulated totals.  Each element of stack corresponds
; to an attempt to apply a certain rune and the element is the value of cnt at
; the time the attempt started.  When the attempt is done, we subtract the then
; current value of cnt with the old value to find out how many lemmas were
; tried under that rune and we accumulate that increment into the totals.  The
; accumulated totals is an alist and each pair is of the form (rune n . ap),
; where n is the number of times rune was pushed onto the stack and ap is the
; accumulated persistence of that rune: the number of frames built while that
; rune was on the stack.

; Performance:
; A :mini-proveall that takes 66 seconds with accumulated-persistence
; disabled, takes 251 seconds (3.8 times longer) with accumulated-persistence
; on.

(defun add-accumulated-persistence (rune delta alist)

; Each element of alist is of the form (rune n . ap).  We
; increment n by 1 and ap by delta.

  (let ((pair (assoc-equal rune alist)))
    (cond ((null pair) (cons (cons rune (cons 1 delta)) alist))
          (t (cons (cons rune (cons (1+ (cadr pair)) (+ delta (cddr pair))))
                   (remove1-equal pair alist))))))

(defmacro accumulated-persistence (flg)

  ":Doc-Section Miscellaneous

  to get statistics on which ~il[rune]s are being tried~/
  ~bv[]
  Useful Forms:
  (accumulated-persistence t)             ; activate statistics gathering

  (show-accumulated-persistence :frames)  ; display statistics ordered by
  (show-accumulated-persistence :tries)   ; frames built, times tried,
  (show-accumulated-persistence :ratio)   ; or their ratio

  (accumulated-persistence nil)           ; deactivate
  ~ev[]~/

  Generally speaking, the more ACL2 knows, the slower it runs.  That
  is because the search space grows with the number of alternative
  rules.  Often, the system tries to apply rules that you have
  forgotten were even there, if you knew about them in the first
  place!  ``Accumulated-persistence'' is a statistic (originally
  developed for Nqthm) that helps you identify the rules that are
  causing ACL2's search space to explode.

  Accumulated persistence tracking can be turned on or off.  It is
  generally off.  When on, the system runs about two times slower than
  otherwise!  But some useful numbers are collected.  When it is
  turned on, by
  ~bv[]
  ACL2 !>(accumulated-persistence t)
  ~ev[]
  an accumulation site is initialized and henceforth data about
  which rules are being tried is accumulated into that site.  That
  accumulated data can be displayed with ~c[show-accumulated-persistence],
  as described in detail below.  When accumulated persistence is
  turned off, with ~c[(accumulated-persistence nil)], the accumulation
  site is wiped out and the data in it is lost.

  The ``accumulated persistence'' of a ~il[rune] is the number of ~il[rune]s the
  system has attempted to apply (since accumulated persistence was
  last activated) while the given ~il[rune] was being tried.

  Consider a ~c[:]~ilc[rewrite] rule named ~ilc[rune].  For simplicity, let us imagine
  that ~ilc[rune] is tried only once in the period during which accumulated
  persistence is being ~il[monitor]ed.  Recall that to apply a rewrite rule
  we must match the left-hand side of the conclusion to some term we
  are trying to rewrite, establish the hypotheses of ~ilc[rune] by
  rewriting, and, if successful, then rewrite the right-hand side of
  the conclusion.  We say ~ilc[rune] is ``being tried'' from the time we
  have matched its left-hand side to the time we have either abandoned
  the attempt or finished rewriting its right-hand side.  (By
  ``match'' we mean to include any loop-stopper requirement;
  ~pl[loop-stopper].)  During that period of time other rules
  might be tried, e.g., to establish the hypotheses.  The rules tried
  while ~ilc[rune] is being tried are ``billed'' to ~ilc[rune] in the sense
  that they are being considered here only because of the demands of
  ~ilc[rune].  Thus, if no other rules are tried during that period,
  the accumulated persistence of ~ilc[rune] is ~c[1] ~-[] we ``bill'' ~ilc[rune]
  once for its own application attempt.  If, on the other hand, we
  tried ~c[10] rules on behalf of that application of ~ilc[rune], then
  ~ilc[rune]'s accumulated persistence would be ~c[11].

  One way to envision accumulated persistence is to imagine that every
  time a ~il[rune] is tried it is pushed onto a stack.  The rules tried on
  behalf of a given application of a ~il[rune] are thus pushed and popped
  on the stack above that ~il[rune].  A lot of work might be done on its
  behalf ~-[] the stack above the ~il[rune] grows and shrinks repeatedly as
  the search continues for a way to use the ~il[rune].  All the while, the
  ~il[rune] itself ``persists'' in the stack, until we finish with the
  attempt to apply it, at which time we pop it off.  The accumulated
  persistence of a ~il[rune] is thus the number of stack frames built while
  the given ~il[rune] was on the stack.

  Note that accumulated persistence is not concerned with whether the
  attempt to apply a ~il[rune] is successful.  Each of the rules tried on
  its behalf might have failed and the attempt to apply the ~il[rune] might
  have also failed.  The ACL2 proof script would make no mention of
  the ~il[rune] or the rules tried on its behalf because they did not
  contribute to the proof.  But time was spent pursuing the possible
  application of the ~il[rune] and accumulated persistence is a measure of
  that time.

  A high accumulated persistence might come about in two extreme ways.
  One is that the rule causes a great deal of work every time it is
  tried.  The other is that the rule is ``cheap'' but is tried very
  often.  We therefore keep track of the number of times each rule is
  tried as well as its persistence.  The ratio between the two is the
  average amount of work done on behalf of the rule each time it is
  tried.

  When the accumulated persistence totals are displayed by the
  function ~c[show-accumulated-persistence] we sort them so that the most
  expensive ~il[rune]s are shown first.  We can sort according to one of
  three keys:
  ~bv[]
  :frames - the number of frames built on behalf of the rune
  :tries  - the number of times the rune was tried
  :ratio  - frames built per try
  ~ev[]
  The key simply determines the order in which the information is
  presented.  If no argument is supplied to
  ~c[show-accumulated-persistence], ~c[:frames] is used.

  Note that a ~il[rune] with high accumulated persistence may not actually
  be the ``culprit.''  For example, suppose ~c[rune1] is reported to have
  a ~c[:ratio] of ~c[101], meaning that on the average a hundred and one
  frames were built each time ~c[rune1] was tried.  Suppose ~c[rune2] has a
  ~c[:ratio] of ~c[100].  It could be that the attempt to apply ~c[rune1] resulted
  in the attempted application of ~c[rune2] and no other ~il[rune].  Thus, in
  some sense, ~c[rune1] is ``cheap'' and ~c[rune2] is the ``culprit'' even
  though it costs less than ~c[rune1].

  Users are encouraged to think about other meters we could install in
  ACL2 to help diagnose performance problems."

  `(wormhole t 'accumulated-persistence
             nil
             '(pprogn (f-put-global 'wormhole-output
                                    (if ,flg '(0 nil . nil) nil)
                                    state)
                      (value :q))
             :ld-prompt  nil
             :ld-pre-eval-filter :all
             :ld-pre-eval-print  nil
             :ld-post-eval-print :command-conventions
             :ld-evisc-tuple nil
             :ld-error-triples  t
             :ld-error-action :error
             :ld-query-control-alist nil
             :ld-verbose nil))

(defun merge-car-> (l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        ((> (car (car l1)) (car (car l2)))
         (cons (car l1) (merge-car-> (cdr l1) l2)))
        (t (cons (car l2) (merge-car-> l1 (cdr l2))))))

(defun merge-sort-car-> (l)
  (cond ((null (cdr l)) l)
        (t (merge-car-> (merge-sort-car-> (evens l))
                        (merge-sort-car-> (odds l))))))

(defun show-accumulated-persistence-phrase1 (alist)

; Alist has element of the form (x . (rune n . ap)), where x is
; the key upon which we sorted.

  (cond ((null alist) nil)
        (t (let* ((trip (cdr (car alist)))
                  (rune (car trip))
                  (n (cadr trip))
                  (ap (cddr trip)))
             (cons (msg "~c0 ~c1 (~c2.~f3~f4) ~y5"
                        (cons ap 10)
                        (cons n 8)
                        (cons (floor ap n) 5)
                        (mod (floor (* 10 ap) n) 10)
                        (mod (floor (* 100 ap) n) 10)
                        rune)
                   (show-accumulated-persistence-phrase1 (cdr alist)))))))

(defun show-accumulated-persistence-phrase2 (key alist)
  (cond ((null alist) nil)
        (t (cons (cons (case key
                             (:frames (cddr (car alist)))
                             (:tries  (cadr (car alist)))
                             (otherwise  (/ (cddr (car alist)) (cadr (car alist)))))
                       (car alist))
                 (show-accumulated-persistence-phrase2 key (cdr alist))))))

(defun show-accumulated-persistence-phrase (key alist)

; Alist is the accumulated totals alist from the wormhole-output of the
; 'accumulated-persistence wormhole.  Each element is of the form (rune n . ap)
; and we sort them into descending order on the specified key.  Key should be
; one of
; :frames - sort on the number of frames built on behalf of the rune
; :tries  - sort on the number of times the rune was tried
; :ratio  - sort on frames/calls

  (list "" "~@*" "~@*" "~@*" 
        (show-accumulated-persistence-phrase1
         (merge-sort-car->
          (show-accumulated-persistence-phrase2 key alist)))))

(defmacro show-accumulated-persistence (&optional (sortkey ':frames))
  `(wormhole t 'accumulated-persistence
             ,sortkey
    '(pprogn
      (io? temporary nil state
           nil
           (fms "Accumulated Persistence~%   :frames   :tries    :ratio  ~
                 rune~%~*0"
                (list (cons #\0
                            (show-accumulated-persistence-phrase
                             (f-get-global 'wormhole-input state)
                             (cddr (f-get-global 'wormhole-output state)))))
                *standard-co*
                state nil))
      (value :q))
    :ld-prompt  nil
    :ld-pre-eval-filter :all
    :ld-pre-eval-print  nil
    :ld-post-eval-print :command-conventions
    :ld-evisc-tuple nil
    :ld-error-triples  t
    :ld-error-action :error
    :ld-query-control-alist nil
    :ld-verbose nil))

(defun push-accp ()
  (assign-wormhole-output
   wormhole-output
   'accumulated-persistence
   nil
   '(let ((trip (f-get-global 'wormhole-output state)))
     (list* (1+ (car trip))   ;;; new cnt
            (cons (car trip)  ;;; new stack
                  (cadr trip))
            (cddr trip)))))   ;;; old totals

(defun pop-accp (rune)
  (assign-wormhole-output
   wormhole-output
   'accumulated-persistence
   rune
   '(let ((rune (f-get-global 'wormhole-input state))
         (trip (f-get-global 'wormhole-output state)))
     (list* (car trip)                   ;;; old cnt
            (cdr (cadr trip))            ;;; new stack
            (add-accumulated-persistence ;;; new totals
             rune
             (- (car trip)
                (car (cadr trip)))
             (cddr trip))))))

(defmacro with-accumulated-persistence (rune vars body)
  `(let ((with-accumulated-persistence-rune ,rune))
     (prog2$
      (push-accp)
      ,(cond ((and (true-listp vars)
                  (= (length vars) 1))
              `(let ((,(car vars)
                      (check-vars-not-free
                       (with-accumulated-persistence-rune)
                       ,body)))
                 (prog2$
                  (pop-accp with-accumulated-persistence-rune)
                  ,(car vars))))
             (t `(mv-let ,vars
                         (check-vars-not-free
                          (with-accumulated-persistence-rune)
                          ,body)
                         (prog2$
                          (pop-accp with-accumulated-persistence-rune)
                          (mv ,@vars))))))))

;; RAG - Changed the assumptions based on rational to realp.

(defun assume-true-false-<
  (not-flg arg1 arg2 ts1 ts2 type-alist ttree xttree w)

; This function returns an extended type-alist by assuming (< ts1 ts2) true if
; not-flg is nil, but assuming (< ts1 ts2) false if not-flg is not nil.  It
; assumes that type-set (and hence type-set-<) was not able to decide the truth
; or falsity of (< ts1 ts2).  We could put this code in-line in
; assume-true-false, but the `true-type-alist' and `false-type-alist' are dealt
; with symmetrically, so it's convenient to share code via this function.

; Here are the cases we handle.  In this sketch we are glib about the
; possibility that arg1 or arg2 is nonnumeric or complex, but our code handles
; the more general situation.

; When we assume (< arg1 arg2) true,
; * if arg1 is positive then arg2 is positive
; * if arg1 is in the nonnegatives then arg2 is strictly positive
; * if arg2 is in the nonpositives then arg1 is strictly negative
; When we say "arg1 is in the nonnegatives" we mean to include the
; case where arg1 is strictly positive.  Note also that if arg1 may be
; negative, then arg2 could be anything (given that we've made the
; normalization for integers above).  Thus, the above two cases are as
; strong as we can be.

; When we assume (< arg1 arg2) false we find it easier to think about
; assuming (<= arg2 arg1) true:
; * if arg1 is negative, then arg2 is negative
; * if arg1 is nonpositive, then arg2 is nonpositive
; * if arg2 is nonnegative, then arg1 is nonnegative
; Note that if arg1 may be positive then arg2 could be anything, so
; there are no other cases we can express.

  (cond
   ((and
     (not not-flg)
     (ts-subsetp ts1
                 (ts-union #+:non-standard-analysis
                           *ts-non-negative-real*
                           #-:non-standard-analysis
                           *ts-non-negative-rational*
                           (ts-complement *ts-acl2-number*)))
     (ts-intersectp
      ts2
      (ts-complement 
       #+:non-standard-analysis
       (ts-union *ts-positive-real* *ts-complex*)
       #-:non-standard-analysis
       (ts-union *ts-positive-rational* *ts-complex-rational*))))

; The test says: We are dealing with (< arg1 arg2) where arg1 is non-negative
; or a non-number.  We are thus allowed to deduce that arg2 is strictly
; positive or complex.  That is, we may delete the non-positive reals
; and non-numbers from its existing type-set.  If that doesn't change
; anything, we don't want to do it, so we have the third conjunct above that
; says arg2 contains some non-positive reals or some non-numbers.

; A worry is that the intersection below is empty.  Can that happen?  If it
; did, then we would have that arg1 is a non-negative real or a non-number,
; and arg2 is a non-positive real or a non-number.  Supposedly type-set-<
; would have then reported that (< arg1 arg2) must be false and mbf would be t.
; So the empty intersection cannot arise.

    (extend-type-alist
     ;;*** -simple
     arg2
     (ts-intersection ts2
                      #+:non-standard-analysis
                      (ts-union *ts-positive-real* *ts-complex*)
                      #-:non-standard-analysis
                      (ts-union *ts-positive-rational* *ts-complex-rational*))
     (cons-tag-trees ttree xttree)
     type-alist w))

; The remaining cases are analogous to that above.

   ((and (not not-flg)
         (ts-subsetp ts2
                     (ts-union #+:non-standard-analysis
                               *ts-non-positive-real*
                               #-:non-standard-analysis
                               *ts-non-positive-rational*
                               (ts-complement *ts-acl2-number*)))
         (ts-intersectp
          ts1
          (ts-complement 
           #+:non-standard-analysis
           (ts-union *ts-negative-real* *ts-complex*)
           #-:non-standard-analysis
           (ts-union *ts-negative-rational* *ts-complex-rational*))))
    (extend-type-alist
     ;;*** -simple
     arg1
     (ts-intersection ts1
                      #+:non-standard-analysis
                      (ts-union *ts-negative-real* *ts-complex*)
                      #-:non-standard-analysis
                      (ts-union *ts-negative-rational*
                                *ts-complex-rational*))
     (cons-tag-trees ttree xttree)
     type-alist w))
   ((and not-flg
         (ts-subsetp ts1 
                     #+:non-standard-analysis
                     *ts-negative-real*
                     #-:non-standard-analysis
                     *ts-negative-rational*)
         (ts-intersectp ts2
                        #+:non-standard-analysis
                        (ts-complement (ts-union *ts-complex*
                                                 *ts-negative-real*))
                        #-:non-standard-analysis
                        (ts-complement (ts-union *ts-complex-rational*
                                                 *ts-negative-rational*))))
; We are dealing with (not (< arg1 arg2)) which is (<= arg2 arg1) and we here
; know that arg1 is negative.  Thus, arg2 must be negative or complex.  See the
; case below for more details.

    (extend-type-alist
     ;;*** -simple
     arg2
     (ts-intersection ts2
                      #+:non-standard-analysis
                      (ts-union *ts-complex*
                                *ts-negative-real*)
                      #-:non-standard-analysis
                      (ts-union *ts-complex-rational*
                                *ts-negative-rational*))
     (cons-tag-trees ttree xttree)
     type-alist w))
   ((and not-flg
         (ts-subsetp ts1
                     (ts-union #+:non-standard-analysis
                               *ts-non-positive-real*
                               #-:non-standard-analysis
                               *ts-non-positive-rational*
                               (ts-complement *ts-acl2-number*)))
         (ts-intersectp ts2 
                        #+:non-standard-analysis
                        *ts-positive-real*
                        #-:non-standard-analysis
                        *ts-positive-rational*))

; Here we are dealing with (not (< arg1 arg2)) which is (<= arg2 arg1).  We
; know arg1 is <= 0.  We will thus deduce that arg2 is <= 0, and hence not a
; positive real, if we don't already know it.  But the worry again arises
; that the intersection of arg2's known type and the complement of the
; positive-reals is empty.  Suppose it were.  Then arg2 is a strictly
; positive real.  But if arg1 is a non-positive real or a non-number
; and arg2 is a positive real, then type-set-< knows that (< arg1 arg2) is
; true.  Thus, this worry is again baseless.

    (extend-type-alist
     ;;*** -simple
     arg2
     (ts-intersection
      ts2
      (ts-complement #+:non-standard-analysis *ts-positive-real*
                     #-:non-standard-analysis *ts-positive-rational*))
     (cons-tag-trees ttree xttree)
     type-alist w))
   ((and not-flg
         (ts-subsetp ts2 
                     #+:non-standard-analysis *ts-positive-real*
                     #-:non-standard-analysis *ts-positive-rational*)
         (ts-intersectp ts1
                        (ts-complement
                         #+:non-standard-analysis
                         (ts-union *ts-complex* *ts-positive-real*)
                         #-:non-standard-analysis
                         (ts-union *ts-complex-rational*
                                   *ts-positive-rational*))))
    (extend-type-alist
     ;;*** -simple
     arg1
     (ts-intersection
      ts1
      #+:non-standard-analysis
      (ts-union *ts-complex* *ts-positive-real*)
      #-:non-standard-analysis
      (ts-union *ts-complex-rational*
                *ts-positive-rational*))
     (cons-tag-trees ttree xttree)
     type-alist w))
   ((and not-flg
         (ts-subsetp
          ts2
          (ts-complement
           #+:non-standard-analysis
           (ts-union *ts-complex* *ts-negative-real*)
           #-:non-standard-analysis
           (ts-union *ts-complex-rational*
                     *ts-negative-rational*)))
         (ts-intersectp ts1 
                        #+:non-standard-analysis *ts-negative-real*
                        #-:non-standard-analysis *ts-negative-rational*))
    (extend-type-alist
     ;;*** -simple
     arg1
     (ts-intersection
      ts1
      (ts-complement #+:non-standard-analysis *ts-negative-real*
                     #-:non-standard-analysis *ts-negative-rational*))
     (cons-tag-trees ttree xttree)
     type-alist w))
   (t type-alist)))

(defun mv-atf-2 (not-flg true-type-alist false-type-alist
                         new-term xnot-flg x shared-ttree xttree ignore)

; This function is a variation of mv-atf in which mbt, mbf, ttree1,
; and ttree2 are all known to be nil.  The scenario is that there is
; an implicit term that we want to assume true or false, and we have
; generated two other terms x and new-term to assume true or false
; instead, each with its own parity (xnot-flg and not-flg,
; respectively).  We want to avoid putting redundant information on
; the type-alist, which would happen if we are not careful in the case
; that x and new-term are the same term modulo their respective
; parities.

; The tag tree shared-ttree justifies truth or falsity of new-term
; while xttree justifies truth or falsity of x.

; We assume that new-term is not a call of NOT.

; Ignore is :tta or :fta if we do not care about the value of true-type-alist
; or false-type-alist that is passed in (and may get passed out in the opposite
; position, due to not-flg).

  (let ((tta0 (and (not (eq ignore :tta))
                   (extend-type-alist-simple
                    new-term
                    *ts-t*
                    shared-ttree
                    true-type-alist)))
        (fta0 (and (not (eq ignore :fta))
                   (extend-type-alist-simple
                    new-term
                    *ts-nil*
                    shared-ttree
                    false-type-alist)))
        (same-parity (eq not-flg xnot-flg)))
    (cond
     ((equal new-term ; new-term is not a call of NOT, so we negate x
             (cond (same-parity x)
                   (t (dumb-negate-lit x))))
      (mv-atf not-flg nil nil tta0 fta0 nil nil))
     (t
      (let ((tta1 (extend-type-alist-simple
                   x
                   (if same-parity *ts-t* *ts-nil*)
                   xttree
                   tta0))
            (fta1 (extend-type-alist-simple
                   x
                   (if same-parity *ts-nil* *ts-t*)
                   xttree
                   fta0)))
        (mv-atf not-flg nil nil tta1 fta1 nil nil))))))

(defun binding-hyp-p (hyp alist wrld)

; Returns (mv forcep flg), where forcep is true if we have a call of force or
; case-split, in which case we consider the argument of that call for flg.  Flg
; indicates whether we have a call (equiv var term), where var is a free
; variable with respect to alist (typically a unifying substitution, but we
; only look at the cars) that we want to bind.  Starting with Version_2.9.4, we
; allow equiv to be an equivalence relation other than equal; however, to
; preserve existing proofs (in other words, to continue to allow kinds of
; equivalential reasoning done in the past), we only allow binding in the
; non-equal case when the right-hand side is a call of double-rewrite, which
; may well be what is desired anyhow.

; Starting with Version_2.7, we try all bindings of free variables.  Moreover,
; in the case that there are free variables, we formerly first looked in the
; type-alist before considering the special case of (equal v term) where v is
; free and term has no free variables.  Starting with Version_2.7 we avoid
; considerations of free variables when this special case arises, by handling
; it first.

  (let* ((forcep (and (nvariablep hyp)
                      (not (fquotep hyp))
                      (or (eq (ffn-symb hyp) 'force)
                          (eq (ffn-symb hyp) 'case-split))))
         (hyp (if forcep (fargn hyp 1) hyp))
         (eqp (equalityp hyp)))
    (mv forcep
        (cond (eqp (and (variablep (fargn hyp 1))
                        (not (assoc-eq (fargn hyp 1) alist))
                        (not (free-varsp (fargn hyp 2) alist))))
              (t

; We want to minimize the cost of the checks below.  In particular, we do the
; variablep check before the more expensive equivalence-relationp check (which
; can call getprop).

               (and (nvariablep hyp)
                    (not (fquotep hyp))
                    (fargs hyp)
                    (variablep (fargn hyp 1))
                    (equivalence-relationp (ffn-symb hyp) wrld)
                    (let ((arg2 (fargn hyp 2)))
                      (and (not (assoc-eq (fargn hyp 1) alist))
                           (nvariablep arg2)
                           (not (fquotep arg2))
                           (eq (ffn-symb arg2) 'double-rewrite)
                           (not (free-varsp arg2 alist))))))))))

(defmacro adjust-ignore-for-atf (not-flg ignore)

; Here, we rebind ignore to indicate which type-alist (tta or fta) is
; irrelevant for passing into a function that will swap them if and only if
; not-flg is true.

  `(cond ((and ,not-flg (eq ,ignore :fta)) :tta)
         ((and ,not-flg (eq ,ignore :tta)) :fta)
         (t ,ignore)))

(mutual-recursion

(defun type-set (x force-flg dwp type-alist ancestors ens w ttree
                 pot-lst pt)

  ":Doc-Section Miscellaneous

  how type information is encoded in ACL2~/

  To help you experiment with type-sets we briefly note the following
  utility functions.

  ~c[(type-set-quote x)] will return the type-set of the object ~c[x].  For
  example, ~c[(type-set-quote \"test\")] is ~c[2048] and
  ~c[(type-set-quote '(a b c))] is ~c[512].

  ~c[(type-set 'term nil nil nil nil (ens state) (w state) nil nil nil)] will
  return the type-set of ~c[term].  For example,
  ~bv[]
  (type-set '(integerp x) nil nil nil nil (ens state) (w state) nil nil nil)
  ~ev[]
  will return (mv 192 nil).  192, otherwise known as ~c[*ts-boolean*],
  is the type-set containing ~c[t] and ~c[nil].  The second result may
  be ignored in these experiments.  ~c[Term] must be in the
  ~c[translated], internal form shown by ~c[:]~ilc[trans].  ~l[trans]
  and ~pl[term].

  ~c[(type-set-implied-by-term 'x nil 'term (ens state)(w state) nil)]
  will return the type-set deduced for the variable symbol ~c[x] assuming
  the ~c[translated] term, ~c[term], true.  The second result may be ignored
  in these experiments.  For example,
  ~bv[]
  (type-set-implied-by-term 'v nil '(integerp v)
                            (ens state) (w state) nil)
  ~ev[]
  returns ~c[11].

  ~c[(convert-type-set-to-term 'x ts (ens state) (w state) nil)] will
  return a term whose truth is equivalent to the assertion that the
  term ~c[x] has type-set ~c[ts].  The second result may be ignored in these
  experiments.  For example
  ~bv[]
  (convert-type-set-to-term 'v 523 (ens state) (w state) nil)
  ~ev[]
  returns a term expressing the claim that ~c[v] is either an integer
  or a non-~c[nil] true-list.  ~c[523] is the ~c[logical-or] of ~c[11] (which
  denotes the integers) with ~c[512] (which denotes the non-~c[nil]
  true-lists).~/

  The ``actual primitive types'' of ACL2 are listed in
  ~c[*actual-primitive-types*], whose elements are shown below.  Each
  actual primitive type denotes a set ~-[] sometimes finite and
  sometimes not ~-[] of ACL2 objects and these sets are pairwise
  disjoint.  For example, ~c[*ts-zero*] denotes the set containing 0 while
  ~c[*ts-positive-integer*] denotes the set containing all of the positive
  integers.
  ~bv[]
  *TS-ZERO*                  ;;; {0}
  *TS-POSITIVE-INTEGER*      ;;; positive integers
  *TS-POSITIVE-RATIO*        ;;; positive non-integer rationals
  *TS-NEGATIVE-INTEGER*      ;;; negative integers
  *TS-NEGATIVE-RATIO*        ;;; negative non-integer rationals
  *TS-COMPLEX-RATIONAL*      ;;; complex rationals
  *TS-NIL*                   ;;; {nil}
  *TS-T*                     ;;; {t}
  *TS-NON-T-NON-NIL-SYMBOL*  ;;; symbols other than nil, t
  *TS-PROPER-CONS*           ;;; null-terminated non-empty lists
  *TS-IMPROPER-CONS*         ;;; conses that are not proper
  *TS-STRING*                ;;; strings
  *TS-CHARACTER*             ;;; characters
  ~ev[]

  The actual primitive types were chosen by us to make theorem proving
  convenient.  Thus, for example, the actual primitive type ~c[*ts-nil*]
  contains just ~c[nil] so that we can encode the hypothesis ``~c[x] is ~c[nil]''
  by saying ``~c[x] has type ~c[*ts-nil*]'' and the hypothesis ``~c[x] is
  non-~c[nil]'' by saying ``~c[x] has type complement of ~c[*ts-nil*].''  We
  similarly devote a primitive type to ~c[t], ~c[*ts-t*], and to a third type,
  ~c[*ts-non-t-non-nil-symbol*], to contain all the other ACL2 symbols.

  Let ~c[*ts-other*] denote the set of all Common Lisp objects other than
  those in the actual primitive types.  Thus, ~c[*ts-other*] includes such
  things as floating point numbers and CLTL array objects.  The actual
  primitive types together with ~c[*ts-other*] constitute what we call
  ~c[*universe*].  Note that ~c[*universe*] is a finite set containing one
  more object than there are actual primitive types; that is, here we
  are using ~c[*universe*] to mean the finite set of primitive types, not
  the infinite set of all objects in all of those primitive types.
  ~c[*Universe*] is a partitioning of the set of all Common Lisp objects:
  every object belongs to exactly one of the sets in ~c[*universe*].

  Abstractly, a ``type-set'' is a subset of ~c[*universe*].  To say that a
  term, ~c[x], ``has type-set ~c[ts]'' means that under all possible
  assignments to the variables in ~c[x], the value of ~c[x] is a member of
  some member of ~c[ts].  Thus, ~c[(cons x y)] has type-set
  ~c[{*ts-proper-cons* *ts-improper-cons*}].  A term can have more than
  one type-set.  For example, ~c[(cons x y)] also has the type-set
  ~c[{*ts-proper-cons* *ts-improper-cons* *ts-nil*}].  Extraneous types
  can be added to a type-set without invalidating the claim that a
  term ``has'' that type-set.  Generally we are interested in the
  smallest type-set a term has, but because the entire theorem-proving
  problem for ACL2 can be encoded as a type-set question, namely,
  ``Does ~c[p] have type-set complement of ~c[*ts-nil*]?,'' finding the
  smallest type-set for a term is an undecidable problem.  When we
  speak informally of ``the'' type-set we generally mean ``the
  type-set found by our heuristics'' or ``the type-set assumed in the
  current context.''

  Note that if a type-set, ~c[ts], does not contain ~c[*ts-other*] as an
  element then it is just a subset of the actual primitive types.  If
  it does contain ~c[*ts-other*] it can be obtained by subtracting from
  ~c[*universe*] the complement of ~c[ts].  Thus, every type-set can be
  written as a (possibly complemented) subset of the actual primitive
  types.

  By assigning a unique bit position to each actual primitive type we
  can encode every subset, ~c[s], of the actual primitive types by the
  nonnegative integer whose ith bit is on precisely if ~c[s] contains the
  ith actual primitive type.  The type-sets written as the complement
  of ~c[s] are encoded as the ~c[twos-complement] of the encoding of ~c[s].  Those
  type-sets are thus negative integers.  The bit positions assigned to
  the actual primitive types are enumerated from ~c[0] in the same order
  as the types are listed in ~c[*actual-primitive-types*].  At the
  concrete level, a type-set is an integer between ~c[*min-type-set*] and
  ~c[*max-type-set*], inclusive.

  For example, ~c[*ts-nil*] has bit position ~c[6].  The type-set containing
  just ~c[*ts-nil*] is thus represented by ~c[64].  If a term has type-set ~c[64]
  then the term is always equal to ~c[nil].  The type-set containing
  everything but ~c[*ts-nil*] is the twos-complement of ~c[64], which is ~c[-65].
  If a term has type-set ~c[-65], it is never equal to ~c[nil].  By ``always''
  and ``never'' we mean under all, or under no, assignments to the
  variables, respectively.

  Here is a more complicated example.  Let ~c[s] be the type-set
  containing all of the symbols and the natural numbers.  The relevant
  actual primitive types, their bit positions and their encodings are:
  ~bv[]
  actual primitive type       bit    value

  *ts-zero*                    0       1
  *ts-positive-integer*        1       2
  *ts-nil*                     6      64
  *ts-t*                       7     128
  *ts-non-t-non-nil-symbol*    8     256
  ~ev[]
  Thus, the type-set ~c[s] is represented by ~c[(+ 1 2 64 128 256)] = ~c[451].
  The complement of ~c[s], i.e., the set of all objects other than the
  natural numbers and the symbols, is ~c[-452]."

; X is a term and type-alist is a type alist mapping terms to their type-sets
; (and some ttrees) and thus encoding the current assumptions.  In a break with
; nqthm, the ACL2 type-set function tracks dependencies among the entries on
; the type-alist.  In particular, the type-alist here contains pairs of the
; form (term ts . ttree), where ttree is a tag tree generally containing 'PT
; tags.  (There may be other tags, e.g., 'LEMMA and maybe even 'FC-DERIVATION
; during forward- chaining.)  In nqthm, the type-alist contained pairs of the
; form (term . ts).  The ttree argument to type-set is an accumulator onto
; which we add all of the ttrees attached to the facts we use from the
; type-alist and the wrld.  We return two results, the final type set of term
; and a ttree.

; Note:  If ancestors is t it means:  don't backchain.  Act as though 
; the literal we're backchaining on is worse than everything in sight.
; This is called the ``t-ancestors hack'' and is commented upon below.

; Performance Notes:

; Type set was first made to track dependencies -- after a fashion --
; during the coding of forward chaining in April, 1990.  At that time,
; type-set had an option under which it would not track dependencies
; and that option was often used in rewrite; indeed, the only time
; dependencies were tracked was during setting up of the type-alist
; and forward chaining activations in simplify-clause.  In addition,
; compound recognizer lemmas were never tracked.  The paragraph below
; was written of that version of a ``dependency tracking'' type-set.

; Experiments show that (at the time of this writing) this type-set is
; roughly 30% slower than the type-set ACL2 had immediately before
; this addition.  That data was obtained by collecting roughly 70,000
; external calls of type-set that occurred during the proof that
; TAUTOLOGYP is sound and then timing their re-evaluation in both
; versions of ACL2 (with appropriate modifications of the type-alists
; being passed in).  That same experiment led to the surprising fact
; that type set may represent 50-75% of the time spent in the prover!

; We have since added full dependency tracking to type-set and have
; adopted the invariants on type-alist requiring canonical forms.  How
; much these changes slow things down is anyone's guess.  Stay tuned.

; The DWP flag 
; or
; Performance Notes on the "Type-Set Objective" Idea and Double Whammy

; "DWP" stands for "double whammy flag".  It does not affect the
; soundness of the result returned but, when t, makes type-set "try
; harder" to get a narrow type.  It was added Feb 7, 1995 and replaces
; a heuristic hack controlling the "double whammy" idea.  The
; historical comment below explains.

; I have tried an experiment in which type-set gets the type of x in
; two ways and intersects them.  The first way is to look x up on the
; type-alist.  The second way is to compute it from scratch.  Call
; this the "double whammy" approach.  Double whammy is the simplest
; case of a more sophisticated type-set that computes a type for x
; relative to some specified "expected type."  The idea of that design
; was to have the caller of type-set supply the type it wanted x to
; have and type-set then did a double whammy if the type-alist type
; wasn't a subset of the expected type.  But that "conditional double
; whammy" idea is hard to implement.  For example, suppose you want (-
; a) to have a type-set of non-negative rational.  Then when you get
; the type-set of a you should want it to have type non-positive
; rational.  Rather than implement that fine tuned use of the expected
; type-set, I decided simply to implement the double whammy method,
; i.e., always using both ways and intersecting the results.  To my
; surprise, the double whammy method is 3 times slower than the
; ordinary type-set.  That number was obtained by running nqthm.lisp,
; which ordinarily takes 3460 seconds but which takes 10300 seconds
; under double whammy.  I then backed off the full blown double whammy
; and limited its use to the special case where (a) the type-set
; computed from the type-alist is negative and (b) forcing is allowed.
; Under these two cases, type-set is about 6% slower than without any
; double whammy processing.

; Why these two cases?  Here is the rationale I had, for what it is
; worth.  Condition (a) often indicates the type was "non-nil" or
; "anything but a negative", as when you assume (< 0 a) without
; knowing a is rational.  Presumably, forcing was disallowed when this
; negative binding was created, since we probably have forced
; hypotheses around to ensure that the guards are met.  Therefore, if
; condition (b) is also met, the chances are good that a "from
; scratch" computation will force now and we'll get a better answer.

; The hit rate even with these restrictions is quite unimpressive:
; double whammy changes the looked up type in only 1% of the cases
; where we try it!  Of the roughly 4.4 million calls of type-set in
; the nqthm.lisp proofs, 39% of them (1.7 million) find a binding on
; the type-alist and therefore check conditions (a) and (b).  In
; roughly 90% of the those cases, either the found type-set is
; negative or forcing is disallowed and so double whammy is not used.
; In only 176000 calls is double whammy even tried.  That is only 4%
; of the total number of type-set calls.  But of those 176000 attempts
; to use double whammy, in only 2254 cases did it help.  That's a 1%
; hit rate on double whammy.  Not really impressive.  However, one of
; those hits is the case that Bishop reported we couldn't prove and
; now we can.  That example is given below just for future reference.

#| (progn (defstub foo (x) t)
         (defun bar-p (x)
           (consp x))
         (in-theory (disable bar-p))
         (defaxiom foo->=-0
           (implies
            (force (bar-p x))
            (and (integerp (foo x))
                 (>= (foo x) 0)))
           :rule-classes :type-prescription)
         (defaxiom foo-bound
           (implies
            (force (bar-p x))
            (< (foo x) 2))
           :rule-classes (:linear :rewrite)))
   (defthm test-foo-too
     (not (< 1 (foo '(1 . 1)))))|#

; The problem is that (foo '(1 . 1)) is given the type-set
; non-negative anything when we construct the type-alist for the
; linear pot, because we do not force the (bar-p '(1 . 1)).  But
; later, when we are in linear we ask whether (foo '(1 . 1)) is an
; integer and, because of double whammy, force the previously unforced
; bar-p.

; Thus ends the historical comment.  Below, where (not dwp) is tested,
; we used to test
; (or (null force-flg)            ;;;(b)
;     (>= (the-type-set ts0) 0))  ;;;(a)

; This heuristic control on when to double whammy is thwarted by the
; example below.  Consider the clause shown below.  It ought to be
; decidable by type-set alone because if i is an integerp then (binary-+ '1 i)
; is too.  But the "irrelevant" hypothesis that (binary-+ '1 i) is a rationalp
; messes things up.  Let 1+i stand for the binary-+ expression.

; (type-alist-clause '((not (rationalp (binary-+ '1 i)))  ; lit 1
;                      (not (integerp i))                 ; lit 2
;                      (integerp (binary-+ '1 i)))        ; lit 3
;                     nil nil nil (ens state) (w state))

; We process lit 3 in a type-alist generated by assuming lits 1 and 2
; false.  In that type-alist, 1+i is assumed rational and i is assumed
; integral.  When we process lit 3, we get the type of 1+i.  Because of
; lit 1, we find it on the type-alist with rational type.  Because the
; type is non-negative we do not double whammy.  Thus, we miss the
; chance to use lit 2 in computing the type of 1+i.  We thus return a
; type-alist in which 1+i is assumed to be a ratio (non-integer
; rational) and i is integral.

; However, by arranging for assume-true-false always to call type-set
; with dwp = t, we make it use double whammy when assuming things.
; That allows us to catch this.  Our hope is that it does not slow us
; down too much.  

  (mv-let
   (ts0 ttree0)
   (assoc-type-alist x type-alist w)
   (cond
    ((and ts0 (not dwp))
     (mv ts0 (cons-tag-trees ttree ttree0)))
    (t
     (let ((dwp nil))
       (cond
        ((variablep x)

; Warning: You may be tempted to change ttree below to nil on the
; grounds that we are not using any information about x to say its
; type is unknown.  But the specification for type-set is that ttree
; is an accumulator.  Our caller may have put a lot of work into
; deriving x and passed the associated ttree to type-set in good
; faith.  We are obliged to pass it on.

         (type-set-finish ts0 ttree0 *ts-unknown* ttree))
        ((fquotep x)
         (type-set-finish ts0 ttree0 (type-set-quote (cadr x)) ttree))
        ((flambda-applicationp x)

; Once upon a time, we tried to do this by using subcor-var to replace
; the actuals by the formals and then take the type-set of the body.
; The old code is shown, commented out, below.  But that was bad
; because it duplicated the actuals so often.  We now take the
; type-set of the body under a type-alist obtained from the actuals.
; We have to be careful to avoid forcing.

         (mv-let (ts1 ttree1)
                 (type-set (lambda-body (ffn-symb x))
                           nil
                           dwp
                           (zip-variable-type-alist
                            (lambda-formals (ffn-symb x))
                            (type-set-lst 
                             (fargs x)
                             force-flg dwp type-alist ancestors ens w
                             pot-lst pt))

; Here is the motivation of the t-ancestors hack.  We cannot compare subterms
; of the lambda body to terms on the current ancestors because the lambda
; body should be instantiated with the actuals and it is not.  For example
; we might know (p x) on ancestors and go into ((lambda (x) (p x)) a) and
; mistakenly think the body is true because it is on ancestors.  (Ancestors
; is not used that way, but the point is made.)  So when ancestors is
; set to t it means ignore ancestors and don't backchain.  The type-set
; clique is the only nest of functions that treats ancestors that way.

; Here is the one place we initiate the t-ancestors hack.

                           t  ;;; t-ancestors hack
                           ens w ttree pot-lst pt)
                 (type-set-finish ts0 ttree0 ts1 ttree1)))
        
 #|        ((flambda-applicationp x)

; Note: Once upon a time, assumptions only recorded the term forced and not the
; type-alist involved.  We could get away with that because rewrite-atm would
; recover all the assumptions and force a case split on the whole clause to
; handle them.  During those simple days, we treated lambda expressions
; efficiently here, by computing the type-set of the lambda-body under a
; type-alist binding the lambda-formals to the types of the actuals.
; Afterwards, we swept the ttree and appropriately instantiated the forced
; terms with the actuals.  But when we introduced assumption records, with the
; type-alists recorded in them, this design became problematic: we would have
; had to instantiate the :type-alists too.  Rather than do so, we abandoned
; efficiency here and did the obvious:  we expanded lambda applications
; by substituting actuals for formals in the body and computed the type-set
; of the result.  This survived until Version  2.6.

         #|(mv-let (ts1 ttree1)
                 (type-set (subcor-var (lambda-formals (ffn-symb x))
                                       (fargs x)
                                       (lambda-body (ffn-symb x)))
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens w ttree pot-lst pt)
                 (type-set-finish ts0 ttree0 ts1 ttree1))|#

; When we introduced the nu-rewriter, we made clausify no longer
; expand lambdas so aggressively.  This meant that type-set began to
; see lot more lambdas.  In that environment, the expansion of lambdas
; here was taking lot of time and generating a lot of conses.  So now
; we take the efficient AND braindead approach of saying we simply
; don't know anything about a lambda application.

         (type-set-finish ts0 ttree0 *ts-unknown* ttree))|#

        ((eq (ffn-symb x) 'not)
         (mv-let (ts1 ttree1)
                 (type-set (fargn x 1) force-flg dwp type-alist ancestors
                           ens w ttree pot-lst pt)
                 (mv-let (ts1 ttree1)
                         (type-set-not ts1 ttree1 ttree)
                         (type-set-finish ts0 ttree0 ts1 ttree1))))
        (t
         (let* ((fn (ffn-symb x))
                (recog-tuple
                 (most-recent-enabled-recog-tuple fn
                                                  (global-val 'recognizer-alist w)
                                                  ens)))
           (cond
            (recog-tuple
             (mv-let
              (ts1 ttree1)
              (type-set (fargn x 1) force-flg dwp type-alist ancestors
                        ens w ttree pot-lst pt)
              (mv-let
               (ts1 ttree1)
               (type-set-recognizer recog-tuple ts1 ttree1 ttree)
               (mv-let (ts ttree)
                       (type-set-finish ts0 ttree0 ts1 ttree1)

; At this point, ts is the intersection of the type-alist information and the
; recog information.  Unless ts is t or nil we also try any type-prescription
; rules we have about this function symbol.

                       (cond
                        ((or (ts= ts *ts-t*)
                             (ts= ts *ts-nil*))
                         (mv ts ttree))
                        (t

; WARNING: There is another call of type-set-with-rules below, in the normal
; case.  This call is for the recognizer function case.  If you change this
; one, change that one!

                         (mv-let
                          (ts2 ttree2)
                          (type-set-with-rules
                           (getprop fn 'type-prescriptions nil 'current-acl2-world w)
                           x force-flg dwp type-alist ancestors ens w
                           *ts-unknown* ttree pot-lst pt)
                          (mv (ts-intersection ts ts2) ttree2))))))))
            ((eq fn 'if)

; It is possible that the if-expression x is on the type-alist. It
; would get there if we had gone through an earlier assume-true-false
; on x, i.e., if we were processing b in (if (if x1 x2 x3) b c).  So
; we will compute the type-set of x directly based on its structure
; and then finish as usual by anding it with ts0, the type-set of x
; itself as recorded on the type-alist, as appropriate.

             (mv-let
              (ts1 ttree1)
              (mv-let
               (must-be-true
                must-be-false
                true-type-alist
                false-type-alist
                ttree1)
               (assume-true-false (fargn x 1)
                                  nil
                                  force-flg
                                  dwp
                                  type-alist
                                  ancestors
                                  ens
                                  w
                                  pot-lst pt nil)

; If must-be-true or must-be-false is set, then ttree1 explains the
; derivation of that result.  If neither is derived, then ttree1 is
; nil and we can ignore it.

               (cond (must-be-true
                      (type-set (fargn x 2)
                                force-flg
                                dwp
                                true-type-alist
                                ancestors
                                ens
                                w
                                (cons-tag-trees ttree1 ttree)
                                pot-lst pt))
                     (must-be-false
                      (type-set (fargn x 3)
                                force-flg
                                dwp
                                false-type-alist
                                ancestors
                                ens
                                w
                                (cons-tag-trees ttree1 ttree)
                                pot-lst pt))
                     (t (mv-let (ts1 ttree)
                                (type-set (fargn x 2)
                                          force-flg
                                          dwp
                                          true-type-alist
                                          ancestors
                                          ens
                                          w
                                          ttree
                                          pot-lst pt)
                                (mv-let (ts2 ttree)
                                        (type-set (fargn x 3)
                                                  force-flg
                                                  dwp
                                                  false-type-alist
                                                  ancestors
                                                  ens
                                                  w
                                                  ttree
                                                  pot-lst pt)
                                        (mv (ts-union ts1 ts2)
                                            ttree))))))
               (type-set-finish ts0 ttree0 ts1 ttree1)))
            ((member-eq fn *expandable-boot-strap-non-rec-fns*)

; For these particular functions we actually substitute the actuals
; for the formals into the guarded body and dive into the body to get
; the answer.  Typically we will not ever encounter these functions in
; proofs because they will have been expanded away.  However, we will
; encounter them during the early type-prescription work and
; pre-verify-guard work, and so think it is worthwhile to handle them.

             (mv-let (ts1 ttree1)
                     (type-set (subcor-var (formals fn w)
                                           (fargs x)
                                           (body fn t w))
                               force-flg
                               dwp
                               type-alist
                               ancestors
                               ens
                               w
                               ttree
                               pot-lst pt)
                     (type-set-finish ts0 ttree0 ts1 ttree1)))
            (t 

; Otherwise, we apply all known type-prescriptions and conclude with
; whatever is builtin about fn.

; Note: We do not know that 'type-prescriptions is non-nil.  Once upon
; a time we insisted that every fn have a type-prescription.  This
; complicated the defun principle because one might encounter the
; unadmitted function symbol in the termination proofs (e.g.,
; mc-flatten).  So now we are lenient.  This happens to be what Nqthm
; does too, and now we know why!

; WARNING:  There is another call of type-set-with-rules above, in the
; recog-tuple case.  If you change this one, change that one!

             (mv-let (ts1 ttree1)
                     (type-set-with-rules
                      (getprop fn 'type-prescriptions nil 'current-acl2-world w)
                      x force-flg dwp type-alist ancestors ens w
                      *ts-unknown* ttree
                      pot-lst pt)
                     (type-set-finish ts0 ttree0 ts1 ttree1))))))))))))

(defun type-set-lst (x force-flg dwp type-alist ancestors ens w
                     pot-lst pt)

; This function computes the type-set of each element of x, obtaining for each
; a ts and a ttree.  It conses the two together and makes a list of those pairs
; in 1:1 correspondence with x.  That is, if x is (x1 ... xn) then the answer,
; ans, is ((ts1 . ttree1) ... (tsn .  ttreen)).  (Strip-cars ans) will return a
; list of the type sets and (strip-cdrs ans) will return a list of the ttrees.
; Furthermore, if x is a list of variables, (zip-variable-type-alist x ans)
; will return a type-alist!

  (cond
   ((null x) nil)
   (t (mv-let (ts ttree)
              (type-set (car x) force-flg dwp type-alist ancestors ens w nil
                        pot-lst pt)
              (cons (cons ts ttree)
                    (type-set-lst (cdr x) 
                                  force-flg dwp type-alist ancestors ens w
                                  pot-lst pt))))))

(defun type-set-relieve-hyps
  (rune target hyps force-flg dwp alist type-alist ancestors ens wrld ttree ttree0
   pot-lst pt)

; Hyps is a list of terms, implicitly conjoined.  Alist is a substitution
; mapping variables in hyps to terms governed by type-alist.  Consider the
; result, hyps', of substituting alist into each hyp in hyps.  We wish to know
; whether, by type-set reasoning alone, we can get that hyps' are all true in
; the context of type-alist.  We do the substitution one hyp at a time, so we
; don't pay the price of consing up instances beyond the first hyp that fails.
; While we are at it, we record in an extension of type-alist the type computed
; for each hyp', so that if subsequent rules need that information, they can
; get it quickly.

; No Change Loser for ttree, but not for type-alist.

  (cond
   ((null hyps) (mv t type-alist (cons-tag-trees ttree ttree0)))
   (t (mv-let
       (forcep bind-flg)
       (binding-hyp-p (car hyps) alist wrld)
       (let ((hyp (if forcep
                      (fargn (car hyps) 1)
                    (car hyps))))
         (cond
          (bind-flg
           (type-set-relieve-hyps
            rune target (cdr hyps) force-flg dwp
            (cons (cons (fargn hyp 1) (fargn hyp 2)) alist)
            type-alist ancestors ens wrld ttree ttree0
            pot-lst pt))
          (t
           (mv-let
            (lookup-hyp-ans alist ttree)
            (lookup-hyp hyp type-alist wrld alist ttree)
            (cond
             (lookup-hyp-ans
              (type-set-relieve-hyps rune
                                     target
                                     (cdr hyps)
                                     force-flg dwp alist
                                     type-alist ancestors ens wrld
                                     ttree ttree0
                                     pot-lst pt))
             ((free-varsp hyp alist)
              (mv-let
               (force-flg ttree)
               (cond
                ((not (and forcep force-flg))
                 (mv nil ttree))
                (t (force-assumption
                    rune
                    target
                    (sublis-var-and-mark-free alist hyp)
                    type-alist
                    nil
                    (immediate-forcep (ffn-symb (car hyps)) ens)
                    force-flg
                    ttree)))
               (cond
                (force-flg
                 (type-set-relieve-hyps
                  rune target (cdr hyps) force-flg dwp
                  alist type-alist ancestors ens wrld
                  ttree ttree0
                  pot-lst pt))
                (t (mv nil type-alist ttree0)))))
             (t
              (mv-let
               (not-flg atm)
               (strip-not hyp)
               (let ((atm1 (sublis-var alist atm)))

; Note that we stripped off the force (or case-split) symbol if it was
; present.  We don't have to do that (given that the type-set of
; (force x) and of (case-split x) is known to be that of x) but it ought
; to speed things up slightly.  Note that we also stripped off the NOT
; if it was present and have not-flg and the instantiated atom, atm1,
; to work on.  We work on the atom so that we can record its type-set
; in the final type-alist, rather than recording the type-set of (NOT
; atm1).

                 (mv-let
                  (on-ancestorsp assumed-true)

; Here is the one place where we (potentially) use the t-ancestors 
; hack to abort early.

                  (if (eq ancestors t)
                      (mv t nil)
                    (ancestors-check (if not-flg
                                         (mcons-term* 'not atm1)
                                       atm1)
                                     ancestors
                                     (list rune)))
                  (cond
                   (on-ancestorsp
                    (cond
                     (assumed-true
                      (type-set-relieve-hyps rune
                                             target
                                             (cdr hyps)
                                             force-flg dwp alist
                                             type-alist ancestors ens wrld
                                             ttree ttree0
                                             pot-lst pt))
                     (t (mv nil type-alist ttree0))))
                   (t 
                    (mv-let
                     (ts1 ttree1)
                     (type-set atm1 force-flg dwp type-alist

; We know ancestors is not t here, by the tests above.

                               (push-ancestor
                                (if not-flg
                                    atm1
                                  (mcons-term* 'not atm1))
                                (list rune)
                                ancestors)
                               ens wrld ttree
                               pot-lst pt)
                     (mv-let (temp1 temp2)
                             (assoc-type-alist atm1 type-alist wrld)
                             (declare (ignore temp2))
                             (let ((type-alist
                                    (cond ((and temp1 (ts-subsetp temp1 ts1))
                                           type-alist)
                                          (t (extend-type-alist
                                              ;;*** -simple
                                              atm1 ts1 ttree1 type-alist
                                              wrld))))
                                   (ts (if not-flg
                                           (cond ((ts= ts1 *ts-nil*) *ts-t*)
                                                 ((ts-intersectp ts1 *ts-nil*)
                                                  *ts-boolean*)
                                                 (t *ts-nil*))
                                         ts1)))
                               (cond
                                ((ts= ts *ts-nil*) (mv nil type-alist ttree0))
                                ((ts-intersectp *ts-nil* ts)
                                 (mv-let
                                  (force-flg ttree)
                                  (cond
                                   ((not (and force-flg forcep))
                                    (mv nil ttree))
                                   (t (force-assumption
                                       rune
                                       target
                                       (if not-flg
                                           (mcons-term* 'not atm1)
                                         atm1)
                                       type-alist nil
                                       (immediate-forcep
                                        (ffn-symb (car hyps))
                                        ens)
                                       force-flg
                                       ttree1)))
                                  (cond
                                   (force-flg
                                    (type-set-relieve-hyps
                                     rune
                                     target
                                     (cdr hyps)
                                     force-flg dwp alist type-alist ancestors
                                     ens wrld ttree ttree0 pot-lst pt))
                                   (t (mv nil type-alist ttree0)))))
                                (t (type-set-relieve-hyps rune
                                                          target
                                                          (cdr hyps)
                                                          force-flg dwp alist
                                                          type-alist ancestors
                                                          ens wrld
                                                          ttree1 ttree0
                                                          pot-lst
                                                          pt)))))))))))))))))))))

(defun extend-type-alist-with-bindings
  (alist force-flg dwp type-alist ancestors ens w ttree
   pot-lst pt)

; Alist is an alist that pairs variables in some rule with terms.  We compute
; the type-set of each term in the range of alist and extend type-alist with
; new entries that pair each term to its type-set.

  (cond ((null alist) type-alist)
        (t (extend-type-alist-with-bindings
            (cdr alist)
            force-flg
            dwp
            (cond ((assoc-equal (cdr (car alist)) type-alist)
                   type-alist)
                  (t 
                   (mv-let (ts ttree1)
                           (type-set (cdr (car alist))
                                     force-flg dwp type-alist ancestors ens w ttree
                                     pot-lst pt)
                           (extend-type-alist
                            ;;*** -simple
                            (cdr (car alist)) ts ttree1 type-alist w))))
            ancestors
            ens w ttree
            pot-lst pt))))

(defun type-set-with-rule (tp term force-flg dwp type-alist ancestors ens w ttree
                           pot-lst pt)

; We apply the type-prescription, tp, to term, if possible, and return a
; type-set, an extended type-alist and a ttree.  If the rule is inapplicable,
; the type-set is *ts-unknown* and the ttree is unchanged.  Recall that
; type-set treats its ttree argument as an accumulator and we are obliged to
; return an extension of the input ttree.

; Note that the specification of this function is that if we were to take the
; resulting type-alist and cons the input ttree on to each ttree in that
; type-alist, the resulting type-alist would be "appropriate".  In particular,
; we don't put ttree into the type-alist, but instead we assume that our caller
; will compensate appropriately.

  (cond
   ((enabled-numep (access type-prescription tp :nume) ens)
    (mv-let
     (unify-ans unify-subst)
     (one-way-unify (access type-prescription tp :term)
                    term)
     (cond
      (unify-ans
       (with-accumulated-persistence
        (access type-prescription tp :rune)
        (ts type-alist ttree)
        (let* ((hyps (access type-prescription tp :hyps))
               (type-alist
                (cond
                 ((null hyps) type-alist)
                 (t (extend-type-alist-with-bindings unify-subst
                                                     force-flg
                                                     dwp
                                                     type-alist
                                                     ancestors
                                                     ens w

; We lie here by passing in the nil tag tree, so that we can avoid
; contaminating the resulting type-alist with a copy of ttree.  We'll make sure
; that ttree gets into the answer returned by type-alist-with-rules, which is
; the only function that calls type-set-with-rule.

                                                     nil
                                                     pot-lst pt)))))
          (mv-let
           (relieve-hyps-ans type-alist ttree)
           (type-set-relieve-hyps (access type-prescription tp :rune)
                                  term
                                  hyps
                                  force-flg dwp
                                  unify-subst
                                  type-alist
                                  ancestors
                                  ens w

; We pass in nil here to avoid contaminating the type-alist returned by this
; call of type-set-relieve-hyps.

                                  nil
                                  ttree
                                  pot-lst pt)
           (cond
            (relieve-hyps-ans
             (type-set-with-rule1 unify-subst
                                  (access type-prescription tp :vars)
                                  force-flg
                                  dwp
                                  type-alist ancestors ens w
                                  (access type-prescription tp :basic-ts)
                                  (push-lemma
                                   (access type-prescription tp :rune)
                                   ttree)
                                  pot-lst pt))
            (t (mv *ts-unknown* type-alist ttree)))))))
      (t (mv *ts-unknown* type-alist ttree)))))
   (t (mv *ts-unknown* type-alist ttree))))

(defun type-set-with-rule1
  (alist vars force-flg dwp type-alist ancestors ens w basic-ts ttree
   pot-lst pt)

; Alist is an alist that maps variables to terms.  The terms are in the context
; described by type-alist.  Vars is a list of variables.  We map over the pairs
; in alist unioning into basic-ts the type-sets of those terms whose
; corresponding vars are in vars.  We accumulate the ttrees into ttree and
; ultimately return the final basic-ts, type-alist and ttree.  The alist
; contains successive formals paired with actuals.

; We are about to return from type-set-with-rule with the type-set of
; a term, say (foo x), as indicated by a :type-prescription rule, say
; ts-foo, but we are not quite done yet.  On the initial entry to this
; function, basic-ts and vars are from the corresponding fields of
; ts-foo.  Vars is a (possibly empty) subset of the variables in
; ts-foo.  The meaning of ts-foo is that the type-set of (foo x) is
; the union of basic-ts and the types of (the terms bound to) vars.
; See the definition of a type-prescription rule and surrounding
; discussion.  (Search for ``defrec type-prescription'' in this file.)

  (cond ((null alist) (mv basic-ts type-alist ttree))
        ((member-eq (caar alist) vars)
         (mv-let (ts ttree)
                 (type-set
                  (cdar alist) force-flg dwp type-alist ancestors ens w ttree
                  pot-lst pt)
                 (type-set-with-rule1 (cdr alist) vars force-flg dwp
                                      type-alist ancestors ens w
                                      (ts-union ts basic-ts)
                                      ttree
                                      pot-lst pt)))
        (t (type-set-with-rule1 (cdr alist) vars force-flg dwp
                                type-alist ancestors ens w
                                basic-ts
                                ttree
                                pot-lst pt))))

(defun type-set-with-rules (tp-lst term force-flg dwp type-alist ancestors ens
                            w ts ttree pot-lst pt)

; We try to apply each type-prescription in tp-lst, intersecting
; together all the type sets we get and accumulating all the ttrees.
; However, if a rule fails to change the accumulating type-set, we
; ignore its ttree.

  (cond
   ((null tp-lst)
    (mv-let
     (ts1 ttree1)
     (type-set-primitive term force-flg dwp type-alist ancestors ens w ttree
                         pot-lst pt)
     (let ((ts2 (ts-intersection ts1 ts)))
       (mv ts2 (if (ts= ts2 ts) ttree ttree1)))))

   ((ts-subsetp ts
                (access type-prescription (car tp-lst) :basic-ts))

; Our goal is to make the final type-set, ts, as small as possible by
; intersecting it with the type-sets returned to the various rules.  If ts is
; already smaller than or equal to the :basic-ts of a rule, there is no point
; in trying that rule: the returned type-set will be at least as large as
; :basic-ts (it has the :vars types unioned into it) and then when we intersect
; ts with it we'll just get ts back.  The original motivation for this
; short-cut was to prevent the waste of time caused by the
; pre-guard-verification type-prescription if the post-guard-verification rule
; is present.

    (type-set-with-rules (cdr tp-lst)
                         term force-flg dwp type-alist ancestors ens w ts ttree
                         pot-lst pt))
   (t
     (mv-let
       (ts1 type-alist1 ttree1)
       (type-set-with-rule (car tp-lst)
                           term force-flg dwp type-alist ancestors ens w ttree
                           pot-lst pt)
       (let ((ts2 (ts-intersection ts1 ts)))
         (type-set-with-rules (cdr tp-lst)
                              term force-flg dwp type-alist1 ancestors ens w
                              ts2
                              (if (and (ts= ts2 ts)
                                       (equal type-alist type-alist1))
                                  ttree
                                  ttree1)
                              pot-lst pt))))))

;; RAG - I added an entry for floor1, which is the only primitive
;; non-recognizer function we added for the reals.  [Ruben added entries for
;; some other non-standard primitives too.]

(defun type-set-primitive (term force-flg dwp type-alist ancestors ens w ttree0
                           pot-lst pt)

; Note that we call our initial ttree ttree0 and we extend it below to ttree as
; we get the types of the necessary arguments.  This function should handle
; every non-recognizer function handled in *primitive-formals-and-guards*,
; ev-fncall, and cons-term1, though like cons-term1, we also handle NOT.
; Exception:  Since code-char is so simple type-theoretically, we handle its
; type set computation with rule code-char-type in axioms.lisp.  It is
; perfectly acceptable to handle function symbols here that are not handled by
; the functions above.  For example, we compute a type-set for length in a
; special manner below, but cons-term1 and the others do not know about
; length.

  (case (ffn-symb term)
        (cons
         (mv-let (ts2 ttree)
                 (type-set (fargn term 2)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-cons ts2 ttree ttree0)))
        (equal
         (cond ((equal (fargn term 1) (fargn term 2))
                (mv *ts-t* ttree0))
               (t (mv-let (ts1 ttree)
                          (type-set (fargn term 1)
                                    force-flg
                                    dwp
                                    type-alist
                                    ancestors
                                    ens
                                    w
                                    ttree0
                                    pot-lst pt)
                          (mv-let (ts2 ttree)
                                  (type-set (fargn term 2)
                                            force-flg
                                            dwp
                                            type-alist
                                            ancestors
                                            ens
                                            w
                                            ttree
                                            pot-lst pt)
                                  (type-set-equal ts1 ts2 ttree ttree0))))))
        (unary--
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-unary-- ts1 ttree ttree0)))
        (unary-/
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-unary-/ ts1 ttree ttree0)))
        #+:non-standard-analysis
        (floor1
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-floor1 ts1 ttree ttree0)))
        (denominator
         (mv *ts-positive-integer* (puffert ttree0)))
        (numerator
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-numerator ts1 ttree ttree0)))
        #+:non-standard-analysis
        (standard-numberp
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-standard-numberp ts1 ttree ttree0)))
        #+:non-standard-analysis
        (standard-part
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-standard-part ts1 ttree ttree0)))
        #+:non-standard-analysis
        (i-large-integer
         (mv *ts-positive-integer* (puffert ttree0)))
        (car
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-car ts1 ttree ttree0)))
        (cdr
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-cdr ts1 ttree ttree0)))
        (symbol-name
         (mv *ts-string* (puffert ttree0)))
        (symbol-package-name
         (mv *ts-string* (puffert ttree0)))
        (intern-in-package-of-symbol
         (mv-let (ts1 ttree1)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (mv-let (ts2 ttree2)
                         (type-set (fargn term 2)
                                   force-flg
                                   dwp
                                   type-alist
                                   ancestors
                                   ens
                                   w
                                   ttree0
                                   pot-lst pt)

; Note that ttree1 and ttree2 both have ttree0 in them, but ttree2 does not
; have ttree1 in it!
                         (type-set-intern-in-package-of-symbol ts1 ts2
                                                               ttree1 ttree2
                                                               ttree0))))
        (pkg-witness
         (mv *ts-symbol* (puffert ttree0)))
        (coerce
         (mv-let (ts1 ttree1)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (mv-let (ts2 ttree2)
                         (type-set (fargn term 2)
                                   force-flg
                                   dwp
                                   type-alist
                                   ancestors
                                   ens
                                   w
                                   ttree0
                                   pot-lst pt)

; Note that ttree1 and ttree2 both have ttree0 in them, but ttree2 does not
; have ttree1 in it!
                         (type-set-coerce term ts1 ts2 ttree1 ttree2 ttree0))))
        (length
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-length ts1 ttree ttree0)))
        (binary-+
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (mv-let (ts2 ttree)
                         (type-set (fargn term 2)
                                   force-flg
                                   dwp
                                   type-alist
                                   ancestors
                                   ens
                                   w
                                   ttree
                                   pot-lst pt)
                         (type-set-binary-+ term ts1 ts2 ttree ttree0))))
        (binary-*
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (mv-let (ts2 ttree)
                         (type-set (fargn term 2)
                                   force-flg
                                   dwp
                                   type-alist
                                   ancestors
                                   ens
                                   w
                                   ttree
                                   pot-lst pt)
                         (type-set-binary-* ts1 ts2 ttree ttree0))))
        (<
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (mv-let (ts2 ttree)
                         (type-set (fargn term 2)
                                   force-flg
                                   dwp
                                   type-alist
                                   ancestors
                                   ens
                                   w
                                   ttree
                                   pot-lst pt)
                         (type-set-< (fargn term 1)
                                     (fargn term 2)
                                     ts1 ts2
                                     type-alist ttree ttree0
                                     pot-lst pt))))
        (not
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-not ts1 ttree ttree0)))
        (realpart
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-realpart ts1 ttree ttree0)))
        (imagpart
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-imagpart ts1 ttree ttree0)))
        (complex
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (mv-let (ts2 ttree)
                         (type-set (fargn term 2)
                                   force-flg
                                   dwp
                                   type-alist
                                   ancestors
                                   ens
                                   w
                                   ttree
                                   pot-lst pt)
                         (type-set-complex ts1 ts2 ttree ttree0))))
        (char-code
         (mv-let (ts1 ttree)
                 (type-set (fargn term 1)
                           force-flg
                           dwp
                           type-alist
                           ancestors
                           ens
                           w
                           ttree0
                           pot-lst pt)
                 (type-set-char-code ts1 ttree ttree0)))
        (otherwise (mv *ts-unknown* ttree0))))

; Mini-Essay on Assume-true-false-if and Implies
; or
; How Strengthening One Part of a Theorem Prover Can Weaken the Whole.

; Normally, when ACL2 rewrites one of the branches of an IF expression,
; it ``knows'' the truth or falsity (as appropriate) of the test.  More
; precisely, if x is a term of the form (IF test true-branch false-branch),
; when ACL2 rewrites true-branch, it can determine that test is true by
; type reasoning alone, and when it rewrites false-branch it can determine
; that test is false by type reasoning alone.  This is certainly what one
; would expect.

; However, previously, if test was such that it would print as (AND
; test1 test2) -- so that x would print as (IF (AND test1 test2)
; true-branch false-branch) -- when ACL2 rewrote true-branch it only
; knew that (AND test1 test2) was true --- it did not know that test1
; was true nor that test2 was true, but only that their conjunction
; was true.  There were also other situations in which ACL2's
; reasoning was weak about the test of an IF expression.

; The function assume-true-false-if was written to correct this problem
; but it caused other problems of its own.  This mini-essay records
; one of the difficulties encountered and its solution.

; In initial tests with the new assume-true-false-if, more than three-
; fourths of the books distributed with ACL2 failed to certify.  Upon
; examination it turned out that ACL2 was throwing away many of the
; :use hints as well as some of the results from generalization
; rules.  Let us look at a particular example (from inequalities.lisp
; in the arithmetic library):
 
 #|(defthm <-*-right-cancel
     (implies (and (rationalp x)
                   (rationalp y)
                   (rationalp z))
              (iff (< (* x z) (* y z))
                   (cond
                    ((< 0 z)
                     (< x y))
                    ((equal z 0)
                     nil)
                    (t (< y x)))))
     :hints (("Goal" :use
              ((:instance (:theorem
                           (implies (and (rationalp a)
                                         (< 0 a)
                                         (rationalp b)
                                         (< 0 b))
                                    (< 0 (* a b))))
                          (a (abs (- y x)))
                          (b (abs z)))))))|#

; This yields the subgoal:

 #|(IMPLIES (IMPLIES (AND (RATIONALP (ABS (+ Y (- X))))
                          (< 0 (ABS (+ Y (- X))))
                          (RATIONALP (ABS Z))
                          (< 0 (ABS Z)))
                     (< 0 (* (ABS (+ Y (- X))) (ABS Z))))
            (IMPLIES (AND (RATIONALP X)
                          (RATIONALP Y)
                          (RATIONALP Z))
                     (IFF (< (* X Z) (* Y Z))
                          (COND ((< 0 Z) (< X Y))
                                ((EQUAL Z 0) NIL)
                                (T (< Y X))))))|#

; Previously, the preprocess-clause ledge of the waterfall would
; see this as

 #|((NOT (IMPLIES (IF (RATIONALP (ABS (BINARY-+ Y (UNARY-- X))))
                      (IF (< '0 (ABS (BINARY-+ Y (UNARY-- X))))
                          (IF (RATIONALP (ABS Z))
                              (< '0 (ABS Z))
                              'NIL)
                          'NIL)
                      'NIL)
                  (< '0
                     (BINARY-* (ABS (BINARY-+ Y (UNARY-- X)))
                               (ABS Z)))))
    (IMPLIES (IF (RATIONALP X)
                 (IF (RATIONALP Y) (RATIONALP Z) 'NIL)
                 'NIL)
             (IFF (< (BINARY-* X Z) (BINARY-* Y Z))
                  (IF (< '0 Z)
                      (< X Y)
                      (IF (EQUAL Z '0) 'NIL (< Y X))))))|#

; and return

 #|(((NOT (IF (IF (RATIONALP (ABS (BINARY-+ Y (UNARY-- X))))
                  (IF (< '0 (ABS (BINARY-+ Y (UNARY-- X))))
                      (IF (RATIONALP (ABS Z))
                          (< '0 (ABS Z))
                          'NIL)
                      'NIL)
                  'NIL)
              (IF (< '0
                     (BINARY-* (ABS (BINARY-+ Y (UNARY-- X)))
                               (ABS Z)))
                  'T
                  'NIL)
              'T))
     (NOT (RATIONALP X))
     (NOT (RATIONALP Y))
     (NOT (RATIONALP Z))
     (IF (< (BINARY-* X Z) (BINARY-* Y Z))
         (IF (IF (< '0 Z)
                 (< X Y)
                 (IF (EQUAL Z '0) 'NIL (< Y X)))
             'T
             'NIL)
         (IF (IF (< '0 Z)
                 (< X Y)
                 (IF (EQUAL Z '0) 'NIL (< Y X)))
             'NIL
             'T))))|#
; Now, under the old regime, when rewrite got hold of the conclusion
; of the :use hint, 

; (< '0
;    (BINARY-* (ABS (BINARY-+ Y (UNARY-- X)))
;              (ABS Z)))

; it would would know that X, Y, and Z were rational and that the IF
; expression

; (IF (RATIONALP (ABS (BINARY-+ Y (UNARY-- X))))
;     (IF (< '0 (ABS (BINARY-+ Y (UNARY-- X))))
;         (IF (RATIONALP (ABS Z))
;             (< '0 (ABS Z))
;             'NIL)
;         'NIL)
;     'NIL)

; was true.  But it would not know, for instance, that
; (< '0 (ABS (BINARY-+ Y (UNARY-- X)))) was true.

; With the introduction of assume-true-false-if, however, ACL2 would
; know that each element of the conjunction represented by the IF
; expression was true, and so would be able to determine that the
; conclusion of the :use hint was true by type-reasoning alone (since
; the original :theorem was so proved).  Thus, the whole hint rewrites
; to true and is removed.  Bummer.

; Previously, the conclusion of a :use hint or of a fact derived from
; a generalization rule would, in affect, be rewritten (the first
; time) under the type-alist of the overall goal to be proved.  We now
; handle IMPLIES differently in order to return to this original
; behavior.

; The basic idea is not to expand IMPLIES except within rewrite where
; we can easily maintain the proper type-alists.  Two simple exceptions
; to this rule are in tautologyp and distribute-first-if.  We expand
; IMPLIES within tautologyp for obvious reasons and within distribute-
; first-if because earlier tests (presumably still valid) showed that
; this was both faster and more fruitful.  Note that this second
; exception implies that an IMPLIES will never show up due to a user
; defined function.

; A slightly more subtle exception is in preprocess-clause where we
; expand an IMPLIES if it is derived from the original theorem to be
; proved.  This is necessary to provide a context for rewrite (See the
; comments in preprocess-clause beginning ``Note: Once upon a time (in
; Version 1.5)'' for more on this.

;; RAG - In this function, I relaxed the tests for rational to include
;; realp as well.

(defun assume-true-false-if (not-flg x xttree force-flg dwp
                                     type-alist ancestors ens w
                                     pot-lst pt)

; X is an IF-expression we have been asked to assume-true-false.  We
; return the standard tuple through the standard calls to mv-atf.

; The original motivation for this function lay in the fact that, within
; ACL2's logic, conjunctions and disjunctions are encoded as IF
; expressions.  In the simplest (conceptually) case, x is of the form
; (IF a b 'nil) which is the translation of (AND a b).  This function
; then ensures that the truth of both a and b are recorded in the
; true-type-alist returned.  It is, however, written to handle the
; general case. 

  (let ((test (fargn x 1))
        (true-branch (fargn x 2))
        (false-branch (fargn x 3)))

; We start by recurring on the test.

    (mv-let (test-mbt test-mbf test-tta test-fta test-ttree)
            (assume-true-false test xttree force-flg
                               dwp type-alist ancestors ens w
                               pot-lst pt nil)

; In the first two branches, we know that test must be true or that test
; must be false.  We recur on the true branch or the false branch 
; respectively.  Test-ttree is a (possibly trivial) extension of xttree
; containing any assumptions made when calling assume-true-false on test,
; so we use it on the recursive calls.

            (cond 
             (test-mbt
              (mv-let (mbt mbf tta fta ttree)
                      (assume-true-false true-branch test-ttree force-flg
                                         dwp test-tta ancestors ens w
                                         pot-lst pt nil)
                      (mv-atf not-flg mbt mbf tta fta ttree nil)))
             (test-mbf
              (mv-let (mbt mbf tta fta ttree)
                      (assume-true-false false-branch test-ttree force-flg
                                         dwp test-fta ancestors ens w
                                         pot-lst pt nil)
                      (mv-atf not-flg mbt mbf tta fta ttree nil)))

             (t

; We do not know whether test must be true or test must be false.  We
; recur on both true-branch and false-branch, using test-tta and test-fta
; respectively.  These two type-alists are proper extensions of type-alist
; which record the assumed type of test.  We use xttree since test-ttree
; is nil.

              (mv-let
               (tb-mbt tb-mbf tb-tta tb-fta tb-ttree)
               (assume-true-false true-branch xttree force-flg
                                  dwp test-tta ancestors ens w
                                  pot-lst pt nil)
               (mv-let
                (fb-mbt fb-mbf fb-tta fb-fta fb-ttree)
                (assume-true-false false-branch xttree force-flg
                                   dwp test-fta ancestors ens w
                                   pot-lst pt nil)
                (cond

                 ((and tb-mbf fb-mbf)

; Since both branches must be false, x must be false.  It is probably
; not very useful, but we record this fact in the returned type-alist.
; Tb-ttree and fb-ttree record any assumptions made, so we must record
; them in the type-alist also.

                  (mv-let (x-ts x-ts-ttree)
                          (look-in-type-alist x type-alist w)
                          (cond ((ts= x-ts *ts-nil*)
                                 (mv-atf not-flg nil t
                                         nil type-alist
                                         xttree x-ts-ttree))
                                ((not (ts-intersectp x-ts *ts-nil*))
                                 (mv (er hard 'assume-true-false-if
                                         "We did not believe that this could ~
                                          happen.  Please send the authors of ~
                                          ACL2 a replayable transcript of ~
                                          this problem if possible, so that ~
                                          we can see what went wrong.")
                                     nil nil nil nil))
                                (t
                                 (mv-atf not-flg nil t
                                         nil
                                         (extend-type-alist-simple
                                          x *ts-nil* 
                                          (cons-tag-trees tb-ttree fb-ttree)
                                          type-alist)

; Observe that we let mv-atf cons these two ttrees together.  We do this
; repeatedly in the calls of mv-atf below.

                                         tb-ttree fb-ttree)))))

                 ((and tb-mbt fb-mbt)

; Since both branches must be true, x must be true (non-nil).

                  (mv-let (x-ts x-ts-ttree)
                          (look-in-type-alist x type-alist w)
                          (cond ((not (ts-intersectp x-ts *ts-nil*))
                                 (mv-atf not-flg t nil
                                         type-alist nil
                                         xttree x-ts-ttree))
                                ((ts= x-ts *ts-nil*)
                                 (mv (er hard 'assume-true-false-if
                                         "We did not believe that this could ~
                                          happen.  Please send the authors of ~
                                          ACL2 a replayable transcript of ~
                                          this problem if possible, so that ~
                                          we can see what went wrong.")
                                     nil nil nil nil))
                                (t
                                 (mv-atf not-flg t nil 
                                         (extend-type-alist-simple
                                          x

; The ts= test above ensures that this intersection is not the empty type.
; We also know that x-ts is not equal to (ts-intersection x-ts *ts-non-nil*)
; because of the (not (ts-intersectp x-ts *ts-nil*)) test above.

                                          (ts-intersection x-ts *ts-non-nil*)
                                          (cons-tag-trees x-ts-ttree
                                                          (cons-tag-trees
                                                           tb-ttree
                                                           fb-ttree))
                                          type-alist)
                                         nil

; We do not need to use x-ts-ttree here, because x-ts is only being
; used to (possibly) tighten up the type of x stored in the type-alist.
; It is not being used in the determination that x must-be-true.

                                         tb-ttree fb-ttree)))))

                 ((and tb-mbt fb-mbf)

; Since the true branch must be true and the false branch must be false
; we know that
; a) The true type-alist we return should include that test and the
;    true branch are both true.
; b) The false type-alist we return should include that test and the
;    false branch are both false.
; Note that tb-tta and fb-fta contain exactly this information.
; However, we must infect any new entries with the ttrees which record
; any assumptions made in deriving these conclusions.

                  (mv-atf not-flg nil nil
                          (infect-new-type-alist-entries
                           tb-tta type-alist
                           (cons-tag-trees tb-ttree fb-ttree))
                          (infect-new-type-alist-entries
                           fb-fta type-alist
                           (cons-tag-trees tb-ttree fb-ttree))
                          nil nil))

                 ((and tb-mbf fb-mbt)
                  (mv-atf not-flg nil nil
                          (infect-new-type-alist-entries
                           fb-tta type-alist
                           (cons-tag-trees tb-ttree fb-ttree))
                          (infect-new-type-alist-entries
                           tb-fta type-alist
                           (cons-tag-trees tb-ttree fb-ttree))
                          nil nil))

                 (tb-mbt

; Since the true-branch must be true, the only way we can get a
; false-type-alist is if the test is false and the false-branch is false.
; The false-branch false-type-alist contains this information.
; We know little about a true-type-alist however.

                  (mv-let (x-ts x-ts-ttree)
                          (look-in-type-alist x type-alist w)
                          (cond ((ts= x-ts *ts-nil*)
                                 (mv-atf not-flg nil t
                                         nil 
                                         (infect-new-type-alist-entries
                                          fb-fta type-alist
                                          tb-ttree)
                                         xttree x-ts-ttree))
                                ((not (ts-intersectp x-ts *ts-nil*))
                                 (mv-atf not-flg t nil
                                         type-alist nil
                                         xttree x-ts-ttree))
                                (t
                                 (mv-atf not-flg nil nil
                                         (extend-type-alist-simple
                                          x
                                          (ts-intersection x-ts *ts-non-nil*)
                                          (cons-tag-trees x-ts-ttree xttree)
                                          type-alist)
                                         (infect-new-type-alist-entries
                                          fb-fta type-alist
                                          tb-ttree)
                                         nil nil)))))

                 (tb-mbf
                  (mv-let (x-ts x-ts-ttree)
                          (look-in-type-alist x type-alist w)
                          (cond ((ts= x-ts *ts-nil*)
                                 (mv-atf not-flg nil t
                                         nil type-alist
                                         xttree x-ts-ttree))
                                ((not (ts-intersectp x-ts *ts-nil*))
                                 (mv-atf not-flg t nil
                                         (infect-new-type-alist-entries
                                          fb-tta type-alist
                                          tb-ttree)
                                         nil
                                         xttree x-ts-ttree))
                                (t
                                 (mv-atf not-flg nil nil
                                         (infect-new-type-alist-entries
                                          fb-tta type-alist
                                          tb-ttree)
                                         (extend-type-alist-simple
                                          x *ts-nil* xttree type-alist)
                                         nil nil)))))

                 (fb-mbt
                  (mv-let (x-ts x-ts-ttree)
                          (look-in-type-alist x type-alist w)
                          (cond ((ts= x-ts *ts-nil*)
                                 (mv-atf not-flg nil t
                                         nil
                                         (infect-new-type-alist-entries
                                          tb-fta type-alist
                                          fb-ttree)
                                         xttree x-ts-ttree))
                                ((not (ts-intersectp x-ts *ts-nil*))
                                 (mv-atf not-flg t nil
                                         type-alist nil
                                         xttree x-ts-ttree))
                                (t
                                 (mv-atf not-flg nil nil
                                         (extend-type-alist-simple
                                          x
                                          (ts-intersection x-ts *ts-non-nil*)
                                          (cons-tag-trees x-ts-ttree xttree)
                                          type-alist)
                                         (infect-new-type-alist-entries
                                          tb-fta type-alist
                                          fb-ttree)
                                         nil nil)))))

                 (fb-mbf
                  (mv-let (x-ts x-ts-ttree)
                          (look-in-type-alist x type-alist w)
                          (cond ((ts= x-ts *ts-nil*)
                                 (mv-atf not-flg nil t
                                         nil type-alist
                                         xttree x-ts-ttree))
                                ((not (ts-intersectp x-ts *ts-nil*))
                                 (mv-atf not-flg t nil
                                         (infect-new-type-alist-entries
                                          tb-tta type-alist
                                          fb-ttree)
                                         nil
                                         xttree x-ts-ttree))
                                (t
                                 (mv-atf not-flg nil nil
                                         (infect-new-type-alist-entries
                                          tb-tta type-alist
                                          fb-ttree)
                                         (extend-type-alist-simple
                                          x *ts-nil* xttree type-alist)
                                         nil nil)))))
                 (t
                  (mv-let (x-ts x-ts-ttree)
                          (look-in-type-alist x type-alist w)
                          (cond ((ts= x-ts *ts-nil*)
                                 (mv-atf not-flg nil t
                                         nil type-alist
                                         xttree x-ts-ttree))
                                ((not (ts-intersectp x-ts *ts-nil*))
                                 (mv-atf not-flg t nil
                                         type-alist nil
                                         xttree x-ts-ttree))
                                (t
                                 (mv-atf not-flg nil nil
                                         (extend-type-alist-simple
                                          x
                                          (ts-intersection x-ts *ts-non-nil*)
                                          (cons-tag-trees x-ts-ttree xttree)
                                          type-alist)
                                         (extend-type-alist-simple
                                          x *ts-nil* xttree type-alist)
                                         nil nil)))))))))))))

(defun assume-true-false (x xttree force-flg dwp type-alist ancestors ens w
                            pot-lst pt ignore0)

; We assume x both true and false, extending type-alist as appropriate.
; Xttree is the ttree with which we are to tag all the entries added to
; type-alist when assuming x.  Generally speaking, xttree will contain
; a 'PT tag whose value is a parent tree for x.

; We return five values.
; must-be-true     - t iff x is definitely true under type-alist and w.
; must-be-false    - t iff x is definitely false under type-alist and w.
; true-type-alist  - an extension of type-alist encoding the assumption
;                    that x is true; valid only if not must-be-false.
; false-type-alist - an extension of type-alist encoding the assumption
;                    that x is false; valid only if not must-be-true.
; ttree            - a ttree recording all of the facts used to establish
;                    x definitely true or definitely false.  This result is
;                    nil if must-be-true and must-be-false are both nil.
;                    Ttree will always include xttree when ttree is non-nil.
;                    That is, if we decide the question of x's truth or
;                    falsity we report a dependency on xttree just as we
;                    would if we had assumed it and then asked about it.

; Input ignore0 is generally nil, but can be :tta or :fta if we will ignore the
; resulting true-type-alist or false-type-alist, respectively.  The following
; example, essentially from Dave Greve, shows a roughly 4X speedup using these
; flags, and saves nearly a billion bytes forcons cells (!), in an Allegro
; Common Lisp run.

#|
 (progn
   (defstub kstate-p (k) nil)
   (defstub aamp-st-p (st) nil)
   (defstub kstate-u (k) nil)
   (defstub kstate-k (k) nil)
   (defstub pred (k st) nil)

   (defaxiom rule
     (implies
      (equal (kstate-k k) 0)
      (equal (pred k st) 1))))

 (defun gen-forms (n acc)
   (declare (xargs :mode :program))
   (if (zp n)
       acc
     (gen-forms (1- n)
                (cons `(NOT (EQUAL (KSTATE-U K) ,n))
                      acc))))

 (defmacro mac ()
   `(AND (KSTATE-P K)
         (AAMP-ST-P ST)
         ,@(gen-forms 560 nil)
         (NOT (EQUAL (KSTATE-U K) -1))
         (EQUAL (KSTATE-K K) 0)))

 (time$ ; optional timing from Matt K.
  (thm (IMPLIES (mac)
                (EQUAL (pred k st)
                       1))
       ;; optional from Matt K.:
       :hints (("Goal" :do-not '(preprocess)))))
|# ; |

  (mv-let
   (xnot-flg x)

; Rockwell Addition:  This is a minor improvement.

; The following is just (strip-not term) except it also recognizes (IF
; x NIL T) as (NOT x).  The former comes up when we expand (ATOM x)
; using its body.

   (cond ((nvariablep x)

; Warning:  Actually x might be a quoted constant here.  But we ask
; if the ffn-symb is either NOT or IF and if it is QUOTE we will
; fail those tests anyway.  So this is a delicate but legitimate
; violation of our term abstract data type.

          (cond ((eq (ffn-symb x) 'NOT)
                 (mv t (fargn x 1)))
                ((and (eq (ffn-symb x) 'IF)
                      (equal (fargn x 2) *nil*)
                      (equal (fargn x 3) *t*))
                 (mv t (fargn x 1)))
                (t (mv nil x))))
         (t (mv nil x)))

   (cond
    ((variablep x)
     (assume-true-false1
      xnot-flg x xttree force-flg dwp type-alist ancestors ens w
      pot-lst pt))
    ((fquotep x)
     (if (equal x *nil*)
         (mv-atf xnot-flg nil t nil type-alist nil xttree)
         (mv-atf xnot-flg t nil type-alist nil nil xttree)))
    ((flambda-applicationp x)
     (assume-true-false1 xnot-flg x xttree
                         force-flg dwp type-alist ancestors ens w
                         pot-lst pt))
    (t
     (let ((recog-tuple
            (most-recent-enabled-recog-tuple (ffn-symb x)
                                             (global-val 'recognizer-alist w)
                                             ens))
           (ignore (adjust-ignore-for-atf xnot-flg ignore0)))
       (cond
        (recog-tuple

; Before v2-8, we did not check whether x is already explicitly true or false
; in the given type-alist.  Here is an example of what can go wrong,
; contributed (in essence) by Eric Smith.

#|
  (defund mod4 (n)
    (if (not (integerp n)) 0 (mod n 4)))

  (defun even-aux (x)
    (if (zp x)
        t
        (if (eql 1 x) nil (even-aux (+ -2 x)))))

  (defund even (x)
    (if (not (integerp x))
        nil
        (if (< x 0)
            (even-aux (- x))
            (even-aux x))))

  (skip-proofs
   (defthm mod4-is-0-fw-to-even
     (implies (and (equal 0 (mod4 n))
                   (integerp n))
              (even n))
     :rule-classes (:forward-chaining)))

  ; succeeds
  (thm (implies (and (equal 0 (mod4 n))
                     (integerp n))
                (even n)))

  (skip-proofs
   (defthm even-means-integerp
     (implies (even x) (integerp x))
     :rule-classes
     (:compound-recognizer)))

  ; fails (but succeeds after the change for v2-8, where we do the
  ;        assoc-type-alist below)
  (thm (implies (and (equal 0 (mod4 n))
                     (integerp n))
                (even n)))
|#

         (let ((strongp (access recognizer-tuple recog-tuple :strongp)))
           (mv-let
             (ts ttree)
             (cond (strongp

; We do not put expect to put calls of strong recognizers in the type-alist.
; See the discussion of strongp above the calls of
; extend-with-proper/improper-cons-ts-tuple below.

                    (mv nil nil))
                   (t (assoc-type-alist x type-alist w)))
             (cond
              ((and ts (ts= ts *ts-nil*))
               (mv-atf xnot-flg nil t nil type-alist ttree xttree))
              ((and ts (not (ts-intersectp ts *ts-nil*)))
               (mv-atf xnot-flg t nil type-alist nil ttree xttree))
              (t
               (mv-let
                 (ts ttree)
                 (type-set (fargn x 1) force-flg
                           dwp
                           type-alist ancestors ens w nil
                           pot-lst pt)
                 (let ((t-int (ts-intersection ts
                                               (access recognizer-tuple
                                                       recog-tuple :true-ts)))
                       (f-int (ts-intersection ts
                                               (access recognizer-tuple
                                                       recog-tuple :false-ts)))
                       (rune (access recognizer-tuple recog-tuple :rune)))
                   (cond
                    ((ts= t-int *ts-empty*)
                     (mv-atf xnot-flg nil t nil type-alist
                             (push-lemma rune ttree)
                             xttree))
                    ((ts= f-int *ts-empty*)
                     (mv-atf xnot-flg t nil type-alist nil
                             (push-lemma rune ttree)
                             xttree))
                    (t

; At this point we know that we can't determine whether (recog arg) is
; true or false.  We therefore will be returning two type-alists which
; restrict arg's type according to the two intersections computed
; above.  Both of these restrictions will depend upon rune (be-
; cause we used the rule to determine the types recognized by recog),
; ttree (because that was the previously known type of arg and was
; involved in the intersections), and xttree (because by contract we
; are obliged to make all new tuples depend on it).  So we construct
; shared-ttree below so we don't have to recreate this ttree twice (once
; for the tta and once for the fta).

                     (let ((shared-ttree
                            (push-lemma rune (cons-tag-trees ttree xttree))))

; The two calls of extend-with-proper/improper-cons-ts-tuple below can be
; thought of as simply extending a type-alist with (list* arg int
; shared-ttree).  They actually do some reasoning about true-lists but the
; effect of adding the tuples they produce to tta and fta is just to restrict
; the type of arg.  Now if recog is strongp, then the truth and falsity of
; (recog arg) is determined by the appropriate restriction on the type of arg
; and there is no need to add anything else to type-alist to form tta and fta.
; But if recog is not strongp, we need to record the additional assumptions
; that (recog arg) is true or false, appropriately.  That is why there are
; ``if'' expressions in the calls of extend-with-proper/improper-cons-ts-tuple
; below: it determines whether we add the arg restriction to type-alist itself
; or to an extension of type-alist that restricts (recog arg) appropriately.
; The assumption that (recog arg) is true depends both on xttree (by contract)
; and the rune, since we are exploiting the fact that recog is Boolean.  The
; assumption that (recog arg) is false only depends on xttree.

                       (mv-atf xnot-flg nil nil
                               (and (not (eq ignore :tta))
                                    (extend-with-proper/improper-cons-ts-tuple
                                     (fargn x 1) t-int shared-ttree force-flg dwp
                                     type-alist ancestors ens
                                     (if strongp
                                         type-alist
                                       (extend-type-alist-simple
                                        x *ts-t* (push-lemma rune xttree)
                                        type-alist))
                                     w
                                     pot-lst pt))
                               (and (not (eq ignore :fta))
                                    (extend-with-proper/improper-cons-ts-tuple
                                     (fargn x 1) f-int shared-ttree force-flg dwp
                                     type-alist ancestors ens
                                     (if strongp
                                         type-alist
                                       (extend-type-alist-simple
                                        x *ts-nil* xttree type-alist))
                                     w
                                     pot-lst pt))
                               nil nil)))))))))))
        ((member-eq (ffn-symb x)
                    *expandable-boot-strap-non-rec-fns*)

; Why do we expand these functions?  The original motivating example involved
; guards and stemmed from the pre-1.8 days.  However, here is a distillation of
; that example for Version 1.8 that illustrates that expansion may help.
; The bottom line in the example is that assuming (= x 0) false is less
; helpful than assuming (equal x 0) false.

; What is the type of the following expression?  One way to experiment is
; to define (foo n) to be the expression below.
; (if (and (integerp n) (>= n 0))
;     (if (equal n 0)
;         t
;         (if (> n 0) t nil))
;   t)
; The answer is that it always returns t.  That is because if we know n is
; in *ts-non-negative-integer* and then we learn that (equal n 0) is nil,
; then we know that n is in *ts-positive-integer* and so (> n 0) is true.
; Now what is the type of
; (if (and (integerp n) (>= n 0))
;     (if (= n 0)
;         t
;         (if (> n 0) t nil))
;   t)
; If the (= n 0) is not expanded into an equal, the answer reported is Boolean.
; We do not learn from the falsity of (= n 0) that n is non-0.

         (mv-let
          (mbt mbf tta fta ttree)
          (assume-true-false 
           (subcor-var (formals (ffn-symb x) w)
                       (fargs x)
                       (body (ffn-symb x) t w))
           xttree force-flg dwp type-alist ancestors ens w
           pot-lst pt ignore)
          (if xnot-flg
              (mv mbf mbt fta tta ttree)
              (mv mbt mbf tta fta ttree))))
        ((eq (ffn-symb x) 'equal)
         (let ((arg1 (fargn x 1))
               (arg2 (fargn x 2)))
           (cond
            ((equal arg1 arg2)
             (mv-atf xnot-flg t nil type-alist nil nil

; We could just as well use (push-lemma '(:executable-counterpart equal)
; xttree) here instead.  But in order to maintain analogy with the general case
; of an equivalence relation, we'll add the fake rune for type-set instead.  As
; noted elsewhere, if we ever give runic names to the theorems establishing
; equivalence-relation-hood and track those names through geneqvs, then we
; ought to push the appropriate rune here, rather than use puffert, which was
; intended for primitives and is thus here somewhat misused unless perhaps
; equiv is 'equal.

                     (puffert xttree)))
            ((and (quotep arg1) (quotep arg2))
             (mv-atf xnot-flg nil t nil type-alist nil
                     (push-lemma '(:executable-counterpart equal) xttree)))
            (t
             (mv-let
              (occursp1 canonicalp1 arg1-canon ttree1)
              (canonical-representative 'equal arg1 type-alist)

; Recall from the comment in the definition of canonical-representative that if
; occursp equals nil, then term-canon equals term.  It follows that in the
; present context, where arg1 and arg2 are distinct and are not both quoteps,
; the next two COND tests remain logically the same if we remove the occursp1
; conjuncts.  But we have put in the occursp1 conjuncts just in order to gain,
; we think, just a bit of efficiency.  They could of course be omitted.  We
; take a similar step a bit further below, with occursp2.

              (cond
               ((and occursp1 (equal arg1-canon arg2))
                (mv-atf xnot-flg t nil type-alist nil
                        nil

; Since we know that mv-atf will make the call of cons-tag-trees, we go ahead
; and do it now so that puffert can be called on the outside, thus perhaps
; eliminating an unnecessary cons.  We pull such a stunt a number of other
; times in calls of mv-atf.

                        (puffert (cons-tag-trees ttree1 xttree))))
               ((and occursp1 (quotep arg1-canon) (quotep arg2))
                (mv-atf xnot-flg nil t nil type-alist
                        nil
                        (push-lemma
                         '(:executable-counterpart equal)
                         (puffert (cons-tag-trees ttree1 xttree)))))
               (t
                (mv-let
                 (occursp2 canonicalp2 arg2-canon ttree2)
                 (canonical-representative 'equal arg2 type-alist)
                 (cond
                  ((and occursp2 (equal arg1-canon arg2-canon))
                   (mv-atf xnot-flg t nil type-alist nil
                           nil
                           (puffert
                            (cons-tag-trees
                             xttree
                             (cons-tag-trees ttree1 ttree2)))))
                  ((and occursp2 (quotep arg1-canon) (quotep arg2-canon))
                   (mv-atf xnot-flg nil t nil type-alist
                           nil
                           (push-lemma
                            '(:executable-counterpart equal)
                            (puffert (cons-tag-trees
                                      xttree
                                      (cons-tag-trees ttree1 ttree2))))))
                  (t
                   (let ((temp-temp
                          (assoc-equiv 'equal arg1-canon arg2-canon
                                       type-alist)))
                     (cond
                      (temp-temp
                       (cond ((ts= (cadr temp-temp) *ts-t*)

; Arg1-canon and arg2-canon are both supposed to be canonical, so 
; this case can't happen!!  It would be sound to return:

;                                 (mv-atf xnot-flg t nil type-alist nil
;                                         nil
;                                         (puffert
;                                          (cons-tag-trees
;                                           (cddr temp-temp)
;                                           (cons-tag-trees
;                                            ttree1
;                                            (cons-tag-trees ttree2 xttree)))))

; here, but let's see if we really understand what is going on!

                              (mv-atf (er hard 'assume-true-false
                                          "Please send the authors of ACL2 a ~
                                           replayable transcript of this ~
                                           problem if possible, so that they ~
                                           can see what went wrong in the ~
                                           function assume-true-false.  The ~
                                           offending call was ~x0.  The ~
                                           surprising type-set arose from a ~
                                           call of ~x1."
                                          (list 'assume-true-false
                                                (kwote x) '<xttree>
                                                force-flg (kwote type-alist)
                                                '<ens> '<w>)
                                          (list 'assoc-equiv
                                                ''equal
                                                (kwote arg1-canon)
                                                (kwote arg2-canon)
                                                '<same_type-alist>))
                                      nil nil nil nil nil nil))
                             ((ts= (cadr temp-temp) *ts-nil*)
                              (mv-atf xnot-flg nil t nil type-alist
                                      nil
                                      (if (and canonicalp1 canonicalp2)

; ... then ttree1 and ttree2 are nil (see comment in canonical-representative),
; and also there's no reason to puffert

                                          (cons-tag-trees (cddr temp-temp)
                                                          xttree)
                                          (puffert
                                           (cons-tag-trees
                                            (cddr temp-temp)
                                            (cons-tag-trees
                                             ttree1
                                             (cons-tag-trees ttree2 xttree)))))))
                             (t
                              (let ((erp (assume-true-false-error
                                          type-alist x (cadr temp-temp))))
                                (mv erp nil nil nil nil)))))
                      (t
                       (mv-let
                        (ts1 ttree)
                        (type-set arg1 force-flg
                                  dwp
                                  type-alist ancestors ens w
                                  nil
                                  pot-lst pt)
                        (mv-let
                         (ts2 ttree)
                         (type-set arg2 force-flg
                                   dwp
                                   type-alist ancestors ens w
                                   ttree
                                   pot-lst pt)

; Observe that ttree records the dependencies of both args.

                         (let ((int (ts-intersection ts1 ts2)))
                           (cond
                            ((ts= int *ts-empty*)
                             (mv-atf xnot-flg nil t nil type-alist
                                     nil
                                     (puffert
                                      (cons-tag-trees ttree xttree))))
                            ((and (ts= ts1 ts2)
                                  (member ts1 *singleton-type-sets*))
                             (mv-atf xnot-flg t nil type-alist nil
                                     nil
                                     (puffert
                                      (cons-tag-trees ttree xttree))))
                            (t
                             (let* ((swap-flg
                                     (term-order arg1-canon arg2-canon))
                                    (shared-ttree

; We could just use (cons-tag-trees ttree xttree) here, but let's save a cons
; if we don't need that tag tree.

                                     (cond
                                      ((or (not (ts= ts1 int))
                                           (not (ts= ts2 int))
                                           (member ts2 *singleton-type-sets*)
                                           (member ts1 *singleton-type-sets*))
                                       (cons-tag-trees ttree xttree))
                                      (t nil)))
                                    (xttree+
                                     (if (and canonicalp1 canonicalp2)

; ... then ttree1 and ttree2 are nil (see comment in canonical-representative),
; and also there's no reason to puffert

                                         xttree
                                       (puffert
                                        (cons-tag-trees
                                         ttree1
                                         (cons-tag-trees ttree2 xttree)))))
                                    (true-type-alist1
                                     (and (not (eq ignore :tta))
                                          (extend-type-alist1
                                           'equal occursp1 occursp2
                                           (and canonicalp1 canonicalp2)
                                           arg1-canon arg2-canon swap-flg x
                                           *ts-t* xttree+ type-alist)))
                                    (true-type-alist2
                                     (and (not (eq ignore :tta))
                                          (cond
                                           ((ts= ts1 int)
                                            true-type-alist1)
                                           (t (extend-with-proper/improper-cons-ts-tuple
                                               arg1 int shared-ttree
                                               force-flg dwp type-alist ancestors ens
                                               true-type-alist1 w
                                               pot-lst pt)))))
                                    (true-type-alist3
                                     (and (not (eq ignore :tta))
                                          (cond
                                           ((ts= ts2 int)
                                            true-type-alist2)
                                           (t (extend-with-proper/improper-cons-ts-tuple
                                               arg2 int shared-ttree
                                               force-flg dwp type-alist ancestors ens
                                               true-type-alist2 w
                                               pot-lst pt)))))
                                    (false-type-alist1
                                     (and (not (eq ignore :fta))
                                          (extend-type-alist1
                                           'equal occursp1 occursp2
                                           (and canonicalp1 canonicalp2)
                                           arg1-canon arg2-canon swap-flg x
                                           *ts-nil* xttree+ type-alist)))
                                    (false-type-alist2
                                     (and (not (eq ignore :fta))
                                          (cond
                                           ((member ts2 *singleton-type-sets*)
                                            (extend-with-proper/improper-cons-ts-tuple
                                             arg1
                                             (ts-intersection
                                              ts1
                                              (ts-complement ts2))
                                             shared-ttree
                                             force-flg dwp type-alist ancestors ens
                                             false-type-alist1 w
                                             pot-lst pt))
                                           (t false-type-alist1))))
                                    (false-type-alist3
                                     (and (not (eq ignore :fta))
                                          (cond
                                           ((member ts1 *singleton-type-sets*)
                                            (extend-with-proper/improper-cons-ts-tuple
                                             arg2
                                             (ts-intersection
                                              ts2
                                              (ts-complement ts1))
                                             shared-ttree
                                             force-flg dwp type-alist ancestors ens
                                             false-type-alist2 w
                                             pot-lst pt))
                                           (t false-type-alist2)))))
                               (mv-atf xnot-flg nil nil
                                       true-type-alist3 false-type-alist3
                                       nil nil))))))))))))))))))))
        ((eq (ffn-symb x) '<)
         (mv-let
          (ts0 ttree)
          (type-set x force-flg
                    dwp
                    type-alist ancestors ens w nil
                    pot-lst pt)
          (cond
           ((ts= ts0 *ts-nil*)
            (mv-atf xnot-flg nil t nil type-alist ttree xttree))
           ((not (ts-intersectp ts0 *ts-nil*))
            (mv-atf xnot-flg t nil type-alist nil ttree xttree))
           (t
            (mv-let
             (ts1 ttree)
             (type-set (fargn x 1) force-flg
                       dwp
                       type-alist ancestors ens w nil
                       pot-lst pt)
             (mv-let
              (ts2 ttree)
              (type-set (fargn x 2) force-flg
                        dwp
                        type-alist ancestors ens w ttree
                        pot-lst pt)

; In the mv-let below we effectively implement the facts that, when x
; is of type *ts-integer* (< x 1) is ~(< 0 x), and (< -1 x) is ~(< x
; 0).  By normalizing such inequalities around 0 we can more easily
; recognize the ones covered by our builtin types.

; WARNING: A bug once lurked here, so beware.  The term we are
; assuming is represented by xnot-flg and x.  We are about to
; re-represent it in terms of not-flg, arg1 and arg2.  Do not
; accidentally use not-flg with x or xnot-flg with (< arg1 arg2)!  In
; the old code, we had only one name for these two flgs.

              (mv-let
               (not-flg arg1 arg2 ts1 ts2)
               (cond ((and (equal (fargn x 2) *1*)
                           (ts-subsetp ts1
                                       (ts-union (ts-complement
                                                  *ts-acl2-number*)
                                                 *ts-integer*)))
                      (mv (not xnot-flg) *0* (fargn x 1) *ts-zero* ts1))
                     ((and (equal (fargn x 1) *-1*)
                           (ts-subsetp ts2
                                       (ts-union (ts-complement
                                                  *ts-acl2-number*)
                                                 *ts-integer*)))
                      (mv (not xnot-flg) (fargn x 2) *0* ts2 *ts-zero*))
                     (t (mv xnot-flg (fargn x 1) (fargn x 2) ts1 ts2)))

; Foreshadow 1:  Note that if neither of the newly bound arg1 nor arg2
; is *0* then not-flg is xnot-flg and arg1 and arg2 are the corresponding
; arguments of x.  That is because on the first two of the three branches
; of the cond above, one of the two args is set to *0*.  We use this curious
; fact below.

; In the mv-let below we effectively implement the fact that, when x is of type
; *ts-integer* (< 0 (+ 1 x)) is ~(< x 0).  The symmetric equivalence of (< (+
; -1 x) 0) to ~(< 0 x) is also handled.

; We will assume that the binary-+ has been commuted so that the constant arg,
; if any, is the first.

; Note that the output of this transformation is not subject to the first
; transformation, above, so we do not have to consider repeating that
; transformation.  However, it is conceivable that the output of this
; transformation is subject to its symmetric counterpart.  In particular, if we
; composed this transformation with itself we might reduce (< 0 (+ 1 (+ -1 x)))
; to (< 0 x).  We prefer instead to take the position that some arithmetic
; simplifier will reduce the +-expressions.

               (mv-let
                (not-flg arg1 arg2 ts1 ts2 ttree)
                (cond ((and (equal arg1 *0*)
                            (ts-subsetp ts2

; It is sound to use (ts-intersection ts2 *ts-acl2-number*) in place of ts2
; above, but since below we see that arg2 is a call of binary-+, we know that
; ts2 is already contained in *ts-acl2-number*.

                                        *ts-integer*)
                            (nvariablep arg2)
                            (not (fquotep arg2))
                            (eq (ffn-symb arg2) 'binary-+)
                            (equal (fargn arg2 1) *1*))

; So the term is of the form (< 0 (+ 1 x)) and we know x is some integer (or a
; non-number).  We transform it to ~(< x 0).  But we must determine the
; type-set of x.  It cannot be done merely by inverting the type-set of (+ 1
; x): the latter might be *ts-integer* and x could either be
; *ts-non-positive-integer* or *ts-integer*, or even a non-number.  Some cases
; we could invert:  if (+ 1 x) is non-positive, then we know x must be strictly
; negative.  But rather than invert, we just call type-set on x, accreting onto
; the existing ttree.

                       (mv-let (tsx ttree)
                               (type-set (fargn arg2 2) force-flg
                                         dwp
                                         type-alist
                                         ancestors ens w ttree
                                         pot-lst pt)
                               (mv (not not-flg) (fargn arg2 2) *0*
                                   tsx *ts-zero* ttree)))
                      ((and (equal arg2 *0*)
                            (ts-subsetp ts1 *ts-integer*)
                            (nvariablep arg1)
                            (not (fquotep arg1))
                            (eq (ffn-symb arg1) 'binary-+)
                            (equal (fargn arg1 1) *-1*))
                       (mv-let (tsx ttree)
                               (type-set (fargn arg1 2) force-flg
                                         dwp
                                         type-alist
                                         ancestors ens w ttree
                                         pot-lst pt)
                               (mv (not not-flg) *0* (fargn arg1 2) *ts-zero*
                                   tsx ttree)))
                      (t (mv not-flg arg1 arg2 ts1 ts2 ttree)))

; Foreshadow 2:  Observe that if, at this point, neither the newly bound arg1
; nor the newly bound arg2 is *0*, then the newly bound not-flg, arg1 and arg2
; are all equal to their old values (outside this mv-let).  That is because the
; cond above, which determines the new values of not-flg, arg1 and arg2 here,
; has the property that on the two branches that changes the not-flg, one of
; the two args is set to *0*.  If neither arg is *0* then we could have only
; come out on the last clause of the cond above and not-flg etc are thus
; unchanged.  We use this curious property below.

; The transformations just carried out have the possibly odd effect of
; assuming (< 0 x) false when asked to assume (< x 1) true, for integral x.
; This effectively sets x's type-set to the non-positives.  One might
; then ask, what happens if we later decide to get the type-set of (<
; x 1).  We would hope that, having assumed it, we would realize it
; was true!  Indeed we do, but only because type-set-< makes the same
; normalization of (< x 1).  This raises the question: Could we reduce
; the size of our code by doing the normalization in only one place,
; either here in assume-true-false or there in type-set-<?  The real
; reason we do it in both places has nothing to do with the subtleties
; discussed here; there is no guarantee that both of these functions
; will be called.  If (< x 1) appears in a test, we will call
; assume-true-false on it and we have to normalize it to produce the
; desired tta and fta.  If (< x 1) appears as the entire resultant
; term, we'll just call type-set on it and we have to normalize it to
; decide it.  

; Another question raised is: "What about the second transformation done
; above?"  We assume ~(< x 0) when asked to assume (< 0 (+ 1 x)), with the
; effect that x is given (roughly) the type-set non-negative integer.  Note
; that type-set-< does not make this second transformation.  Will we recognize
; (< 0 (+ 1 x)) as true later?  Yes.  If x is non-negative, then type-set
; determines that (+ 1 x) is positive and hence (< 0 (+ 1 x)) is true.

                (cond
                 ((equal arg1 *0*)
                  (cond
                   ((ts-subsetp ts2 
                                #+:non-standard-analysis *ts-positive-real*
                                #-:non-standard-analysis *ts-positive-rational*)
                    (mv-atf not-flg t nil type-alist nil
                            ttree
                            xttree))
                   ((ts-subsetp ts2
                                (ts-union (ts-complement *ts-acl2-number*)
                                          #+:non-standard-analysis
                                          *ts-non-positive-real*
                                          #-:non-standard-analysis
                                          *ts-non-positive-rational*))
                    (mv-atf not-flg nil t nil type-alist
                            ttree
                            xttree))
                   (t
                    (let* ((shared-ttree (cons-tag-trees ttree xttree))
                           (ignore (adjust-ignore-for-atf not-flg ignore0))
                           (true-type-alist
                            (and (not (eq ignore :tta))
                                 (extend-type-alist
                                  ;;*** -simple
                                  arg2
                                  (ts-intersection
                                   ts2 
                                   #+:non-standard-analysis
                                   (ts-union *ts-positive-real*
                                             *ts-complex*)
                                   #-:non-standard-analysis
                                   (ts-union *ts-positive-rational*
                                             *ts-complex-rational*))
                                  shared-ttree type-alist w)))
                           (false-type-alist
                            (and (not (eq ignore :fta))
                                 (extend-type-alist
                                  ;;*** -simple
                                  arg2
                                  (ts-intersection
                                   ts2 (ts-complement 
                                        #+:non-standard-analysis
                                        *ts-positive-real*
                                        #-:non-standard-analysis
                                        *ts-positive-rational*))
                                  shared-ttree type-alist w))))

; We formerly put the inequality explicitly on the type-alist only in
; the case that (ts-intersectp ts2 *ts-complex-rational*).  We leave
; in place the comment regarding that case, below.  However, we now
; put the inequality on the type-alist in all cases, in order to
; assist in relieving hypotheses involving free variables.  Robert
; Krug sent the following relevant example.

#|
 (defstub foo (x) t)

 (defaxiom test1
   (implies (and (<= 0 x) 
                 (rationalp x))
            (foo y)))

 (thm
  (implies (and (rationalp x)
                (<= 0 x))
           (foo y)))
|#

; The thm fails, because the hypothesis of test1 is not relieved.  The
; following trace excerpt shows why.

#|
  1> (SEARCH-TYPE-ALIST (< X '0)    ; Attempt to find that (< X 0)
                        64          ; is false.  Note:
                                    ; (decode-type-set 64) = '*TS-NIL*
                        ((X 7))     ; Type-alist:  X is a non-negative
                                    ; rational.  Note:
                                    ; (decode-type-set 7) =
                                    ; *TS-NON-NEGATIVE-RATIONAL*
                        ((Y . Y))   ; unify-subst
                        NIL)>       ; ttree
  <1 (SEARCH-TYPE-ALIST NIL ((Y . Y)) NIL)>   ; failed to relieve hyp
|#

; As seen below, assume-true-false had failed to put the inequality
; explicitly on the type-alist.

#|
  1> (ASSUME-TRUE-FALSE (< X '0) ; condition assumed true or false
                        NIL      ; a tag tree
                        NIL      ; force-flg
                        NIL      ; never mind this one...
                        ((X 31)) ; type-alist: X is rational
                        NIL      ; ancestors
                        |some-enabled-structure|
                        |current-acl2-world|)>
  <1 (ASSUME-TRUE-FALSE NIL             ; must-be-true
                        NIL             ; must-be-false
                        ((X 24) (X 31)) ; true-type-alist:
                                        ; X is negative rational
                        ((X 7) (X 31))  ; false-type-alist:
                                        ; X is non-negative rational
                        NIL)>           ; tag tree
|#

; But wait, there's more!  Robert subsequently sent an example showing
; that it is not enough to put the current inequality with 0, e.g.,
; (mcons-term* '< *0* arg2), on the type-alist.  The original equality
; may need to be there as well.  Here is his example, which ACL2 can
; now prove (see mv-atf-2).

#|
 (defstub foo (x) t)

 (defaxiom test
   (implies (and (<= 1 x)
                 (integerp x))
            (foo y)))

 (thm
   (implies (and (integerp x)
                 (<= 1 x))
            (foo y)))
|#

; Start old comment regarding the case that (ts-intersectp ts2
; *ts-complex-rational*).

; Long comment on why we extend the true-type-alist to accommodate complex
; numbers.

; For an example that illustrates why we need to put (mcons-term* '< *0* arg2)
; on the true-type-alist explicitly in this case, try the following.

#|
 (encapsulate
  (((foo *) => *))
  (local (defun foo (x) (<= 0 x)))
  (defthm foo-type (implies (<= 0 x) (equal (foo x) t))
    :rule-classes :type-prescription))

 (thm (implies (<= 0 x) (equal (foo x) t)))
|#

; If we simply use true-type-alist here, we'll lose the information that (< 0
; arg2).  That is, we desire that the true-type-alist is sufficient for
; deducing what we are assuming true; but if arg2 can be a complex number, we
; will not be able to make that determination.  So, we put this inequality on
; the type-alist, explicitly.  We do so in the order shown for two reasons,
; probably neither of them particularly important (but at least, we document
; what they are).  For one, we want type-set to find the explicit inequality
; first, in case it ever tries to decide it.  Although we do not expect
; type-set to have any trouble even if we bury the inequality after an entry
; for arg2, this coding seems more robust.  More importantly, however, we want
; to call extend-type-alist, which is a bit complicated, on as short a
; type-alist as possible.

; End old comment regarding the case that (ts-intersectp ts2
; *ts-complex-rational*).

                      (mv-atf-2 not-flg true-type-alist false-type-alist
                                (mcons-term* '< *0* arg2)
                                xnot-flg x shared-ttree xttree ignore)))))
                 ((equal arg2 *0*)
                  (cond
                   ((ts-subsetp ts1 
                                #+:non-standard-analysis
                                *ts-negative-real*
                                #-:non-standard-analysis
                                *ts-negative-rational*)
                    (mv-atf not-flg t nil type-alist nil
                            ttree
                            xttree))
                   ((ts-subsetp ts1
                                (ts-union (ts-complement *ts-acl2-number*)
                                          #+:non-standard-analysis
                                          *ts-non-negative-real*
                                          #-:non-standard-analysis
                                          *ts-non-negative-rational*))
                    (mv-atf not-flg nil t nil type-alist
                            ttree
                            xttree))
                   (t
                    (let* ((shared-ttree (cons-tag-trees ttree xttree))
                           (ignore (adjust-ignore-for-atf not-flg ignore0))
                           (true-type-alist
                            (and (not (eq ignore :tta))
                                 (extend-type-alist
                                  ;;*** -simple
                                  arg1
                                  (ts-intersection
                                   ts1 
                                   #+:non-standard-analysis
                                   (ts-union *ts-negative-real*
                                             *ts-complex*)
                                   #-:non-standard-analysis
                                   (ts-union *ts-negative-rational*
                                             *ts-complex-rational*))
                                  shared-ttree type-alist w)))
                           (false-type-alist
                            (and (not (eq ignore :fta))
                                 (extend-type-alist
                                  ;;*** -simple
                                  arg1
                                  (ts-intersection
                                   ts1 (ts-complement 
                                        #+:non-standard-analysis
                                        *ts-negative-real*
                                        #-:non-standard-analysis
                                        *ts-negative-rational*))
                                  shared-ttree type-alist w))))
                      (mv-atf-2 not-flg true-type-alist false-type-alist
                                (mcons-term* '< arg1 *0*)
                                xnot-flg x shared-ttree xttree ignore)))))
                 (t (mv-let
                     (mbt mbf tta fta dttree)
                     (assume-true-false1
                      xnot-flg ; = not-flg
                      x        ; = (mcons-term* '< arg1 arg2)

; Once upon a time we had (mcons-term* '< arg1 arg2), above, instead of x.
; But we claim that not-flg is xnot-flg and that arg1 and arg2 are the
; corresponding arguments of x so that x is equal to (mcons-term* '< arg1 arg2).
; The proof is as follows.  We are in the t clause of a cond.  The preceding
; tests establish that neither arg1 nor arg2 is *0* here.  Hence, by
; Foreshadow 2 above we conclude that not-flg, arg1 and arg2 are
; unchanged from their values at Foreshadow 1.  But at Foreshadow 1 we
; see that if neither arg is *0* not-flg is xnot-flg and arg1 and arg2 are
; the corresponding components of x.  Q.E.D.

                      xttree force-flg dwp type-alist ancestors ens w
                      pot-lst pt)

; Inefficiency: It is somewhat troubling that we are holding ts1 and
; ts2 in our hands while invoking assume-true-false1 on (< arg1 arg2),
; knowing full-well that it will recompute ts1 and ts2.  Sigh.  It
; would be nice to avoid this duplication of effort.

; We could now return (mv mbt mbf tta fta dttree) as the answer.  But, in the
; case that mbt and mbf are both nil we want to tighten up the returned
; type-alists a little if we can.  Suppose we are dealing with (< a1 a2) and a1
; is known to be positive.  Then a2 is (even more) positive.  We can add that
; to the tta, if it changes the type-set of a2.

                     (cond
                      ((or mbt mbf)

; Just return the already computed answers if we've settled the
; question.

                       (mv mbt mbf tta fta dttree))
                      (t (let ((tta
                                (and (not (eq ignore0 :tta))
                                     (assume-true-false-<
                                      not-flg
                                      arg1 arg2 ts1 ts2 tta ttree xttree w)))
                               (fta
                                (and (not (eq ignore0 :fta))
                                     (assume-true-false-<
                                      (not not-flg)
                                      arg1 arg2 ts1 ts2 fta ttree xttree w))))
                           (mv nil nil tta fta nil)))))))))))))))
        ((equivalence-relationp (ffn-symb x) w)
         (let ((arg1 (fargn x 1))
               (arg2 (fargn x 2)))
           (cond
            ((equal arg1 arg2)
             (mv-atf xnot-flg t nil type-alist nil nil (puffert xttree)))
            (t
             (let ((equiv (ffn-symb x)))
               (mv-let
                (occursp1 canonicalp1 arg1-canon ttree1)
                (canonical-representative equiv arg1 type-alist)
                (cond
                 ((and occursp1

; See comment in the 'equal case for an explanation of this use of occursp1
; and a similar use of occursp2 below.

                       (equal arg1-canon arg2))
                  (mv-atf xnot-flg t nil type-alist nil
                          nil
                          (puffert
                           (cons-tag-trees ttree1 xttree))))
                 (t (mv-let
                     (occursp2 canonicalp2 arg2-canon ttree2)
                     (canonical-representative equiv arg2 type-alist)
                     (cond
                      ((and occursp2 (equal arg1-canon arg2-canon))
                       (mv-atf xnot-flg t nil type-alist nil
                               nil
                               (puffert
                                (cons-tag-trees
                                 xttree
                                 (cons-tag-trees ttree1 ttree2)))))
                      (t
                       (let ((temp-temp
                              (assoc-equiv equiv arg1-canon arg2-canon
                                           type-alist)))
                         (cond
                          (temp-temp
                           (cond ((ts= (cadr temp-temp) *ts-t*)

; See comment in corresponding place in the 'equal case.

                                  (mv-atf (er hard 'assume-true-false
                                              "Please send the authors of ~
                                                  ACL2 a replayable transcript ~
                                                  of this problem if possible, ~
                                                  so that they can see what ~
                                                  went wrong in the function ~
                                                  assume-true-false.  The ~
                                                  offending call was ~x0.  The ~
                                                  surprising type-set arose ~
                                                  from a call of ~x1."
                                              (list 'assume-true-false
                                                    (kwote x) '<xttree>
                                                    force-flg
                                                    (kwote type-alist)
                                                    '<ens> '<w>)
                                              (list 'assoc-equiv
                                                    (kwote equiv)
                                                    (kwote arg1-canon)
                                                    (kwote arg2-canon)
                                                    '<same_type-alist>))
                                          nil nil nil nil nil nil))
                                 ((ts= (cadr temp-temp) *ts-nil*)
                                  (mv-atf xnot-flg nil t nil type-alist
                                          nil
                                          (if (and canonicalp1 canonicalp2)
                                              (cons-tag-trees (cddr temp-temp)
                                                              xttree)
                                              (puffert
                                               (cons-tag-trees
                                                (cddr temp-temp)
                                                (cons-tag-trees
                                                 ttree1
                                                 (cons-tag-trees ttree2 xttree)))))))
                                 (t
                                  (let ((erp (assume-true-false-error
                                              type-alist x (cadr temp-temp))))
                                    (mv erp nil nil nil nil)))))
                          (t
                           (let ((swap-flg
                                  (term-order arg1-canon arg2-canon))
                                 (xttree+
                                  (if (and canonicalp1 canonicalp2)
                                      xttree
                                    (puffert
                                     (cons-tag-trees
                                      ttree1
                                      (cons-tag-trees ttree2 xttree))))))
                             (mv-atf xnot-flg nil nil
                                     (and (not (eq ignore :tta))
                                          (extend-type-alist1
                                           equiv occursp1 occursp2
                                           (and canonicalp1 canonicalp2)
                                           arg1-canon arg2-canon swap-flg x
                                           *ts-t* xttree+ type-alist))
                                     (and (not (eq ignore :fta))
                                          (extend-type-alist1
                                           equiv occursp1 occursp2
                                           (and canonicalp1 canonicalp2)
                                           arg1-canon arg2-canon swap-flg x
                                           *ts-nil* xttree+ type-alist))
                                     nil nil))))))))))))))))
        ((or (eq (ffn-symb x) 'car)
             (eq (ffn-symb x) 'cdr))

; In this comment we assume (ffn-symb x) is car but everything we say is true
; for the cdr case as well.  Suppose xnot-flg is nil.  Then after the
; assume-true-false1 below, tta is the result of assuming (car arg) non-nil.
; But if (car arg) is non-nil, then arg is non-nil too.  That is, (implies (car
; arg) arg) is a theorem: Pf.  Consider the contrapositive, (implies (not arg)
; (not (car arg))). Q.E.D.  So we assume arg onto tta as well as (car arg).
; Fta, on the other hand, is the result of assuming (car arg) nil.  That tells
; us nothing about arg, e.g., arg could be nil, a cons (whose car is nil) or
; anything violating car's guard.  Summarizing this case: if xnot-flg is nil,
; then we assume both (car arg) and arg non-nil onto tta and assume only (car
; arg) nil onto fta.

; Now on the other hand, suppose xnot-flg is t.  Then tta contains
; the assumption that (car arg) is nil and fta contains the
; assumption that (car arg) is non-nil.  We can add to fta the
; assumption that arg is non-nil.  Observe that the two cases are
; symmetric if we simply swap the role of tta and fta before we start
; and after we are done.  The first swap is done by the let below.
; The second is done by mv-atf.

         (mv-let (mbt mbf tta fta ttree)
                 (assume-true-false1
                  xnot-flg x xttree force-flg dwp type-alist ancestors ens w
                  pot-lst pt)
                 (cond ((or mbt mbf)
                        (mv mbt mbf tta fta ttree))
                       (t (let ((tta (if xnot-flg fta tta))
                                (fta (if xnot-flg tta fta)))
                            (mv-let (mbt1 mbf tta1 fta1 ttree)
                                    (assume-true-false
                                     (fargn x 1)
                                     xttree force-flg dwp tta ancestors ens w
                                     pot-lst pt :fta)
                                    (declare (ignore mbt1 fta1))
                                    (mv-atf xnot-flg mbt mbf tta1 fta
                                            ttree nil)))))))
        ((eq (ffn-symb x) 'IF)
         (assume-true-false-if xnot-flg x xttree force-flg dwp
                               type-alist ancestors ens w
                               pot-lst pt))
        (t (assume-true-false1 xnot-flg x xttree
                               force-flg dwp type-alist ancestors ens
                               w
                               pot-lst pt))))))))

(defun assume-true-false1
  (not-flg x xttree force-flg dwp type-alist ancestors ens w
   pot-lst pt)

; Roughly speaking, this is the simple assume-true-false, which just
; computes the type-set of x and announces that x must be t, must be
; f, or else announces neither and creates two new type-alists with x
; bound to its type minus *ts-t* and to *ts-nil*.  It returns the
; standard 5 results of assume-true-false.  It puts xttree into the
; type-alist entries for x, if any.

; See assume-true-false.

; NOTE:  This function should not be called when x could be a call of an
; equivalence relation, since otherwise it may destroy the third invariant on
; type-alists.

  (mv-let (ts ttree)
          (type-set x force-flg
                    dwp
                    type-alist ancestors ens w nil
                    pot-lst pt)

; If we can decide x on the basis of ts, do so and report use of ttree.
; Xttree will be put in by mv-atf.

          (cond ((ts= ts *ts-nil*)
                 (mv-atf not-flg nil t nil type-alist ttree xttree))
                ((not (ts-intersectp ts *ts-nil*))
                 (mv-atf not-flg t nil type-alist nil ttree xttree))
                (t

; We produce two new type-alists.  In the one in which x is assumed
; true, we annotate the entry with a ttree that includes both
; xttree and ttree, the initial type-set of x.  In the one in
; which x is assumed false, we annotate the entry with the ttree that
; includes just xttree.  The true entry depends on the old type
; because we intersect the old and non-nil.  But the false entry is
; just the nil type and doesn't depend on the old one.

                 (mv-atf not-flg nil nil
                         (extend-with-proper/improper-cons-ts-tuple
                          x
                          (ts-intersection ts *ts-non-nil*) 
                          (cons-tag-trees ttree xttree)
                          force-flg dwp type-alist ancestors ens
                          type-alist w
                          pot-lst pt)

; It is legal to call extend-type-alist-simple below because we know that x is
; not the call of an equivalence relation.  However, the call of
; extend-with-proper/improper-cons-ts-tuple above cannot quite be safely
; simplified, because perhaps x is a final CDR that is the call of an
; equivalence relation.

                         (extend-type-alist-simple
                          x *ts-nil* xttree type-alist)
                         nil nil)))))

(defun proper/improper-cons-ts-tuple
  (term ts ttree force-flg dwp type-alist ancestors ens wrld
   pot-lst pt)

; We return a tuple of the form (mv term' ts' ttree') that asserts the
; assumption that term has type set ts (with the given ttree
; attached).  Most often, term', ts' and ttree' are just term, ts and
; ttree.  However, if term is of the form (cons a x) we do certain
; auxiliary processing related to the existence of *ts-true-list* and
; its subtypes.  We guarantee that ttree' includes ttree.

; We make various implicit assumptions about term and ts, all
; summarized by the restriction that this function can only be called
; by assume-true-false after checking the current type-set of term and
; failing to decide the question.

; We start with two examples.  Suppose term is (cons a1 x) and ts is
; *ts-true-list*.  Then the "various implicit assumptions" are
; violated because assume-true-false would never ask us to do this:
; the type-set of term is, at worst, the union of *ts-proper-cons* and
; *ts-improper-cons*, and certainly doesn't include *ts-nil*.  But
; assume-true-false always asks us to assume a type-set that is a
; subset of the current type-set.

; So suppose we are asked to assume that (cons a1 x) is of type
; *ts-proper-cons*.  Then we can deduce that x is of type
; *ts-true-list*.  Indeed, these two are equivalent because if we
; store the latter we can compute the former with type-set.  But
; because x is a subterm of (cons a1 x) we prefer to store the
; assumption about x because it will find greater use.  However, we
; may already know something about the type of x.  For example, x may
; be known to be non-nil.  So we are obliged to intersect the old type
; of x with the newly derived type if we want to keep maximizing what
; we know.  Because of the "implicit assumptions" this intersection
; will never produce the empty type set: if it is impossible for x to
; have the required type, then assume-true-false better not ask us to
; make the assumption.  For example, if x is known not to be a
; true-list, then assume-true-false would never ask us to assume that
; (cons a1 x) is proper.

; The example above is based on the theorem
;    (proper-consp (cons a1 x))   <-> (true-listp x).
; We similarly build in the theorem
;    (improper-consp (cons a1 x)) <-> (not (true-listp x)).

  (cond
   ((and (nvariablep term)
         (not (fquotep term))
         (eq (ffn-symb term) 'cons)
         (or (ts= ts *ts-proper-cons*)
             (ts= ts *ts-improper-cons*)))
    (let* ((x (non-cons-cdr term)))

; Can x be an explicit value?  If it can, then we'd be in trouble because
; we return a type-alist binding that sets the value of x.  But in fact
; x cannot be an explicit value.  If it were, then assume-true-false
; would have decided whether (cons a x) was proper or improper.

      (mv-let (tsx ttreex)
              (type-set x force-flg
                        dwp
                        type-alist ancestors ens wrld ttree
                        pot-lst pt)
              (cond
               ((ts= ts *ts-proper-cons*)
                (mv x
                    (ts-intersection tsx *ts-true-list*)
                    ttreex))
               (t
                (mv x
                    (ts-intersection tsx (ts-complement *ts-true-list*))
                    ttreex))))))
   (t (mv term ts ttree))))

(defun extend-with-proper/improper-cons-ts-tuple
  (term ts ttree force-flg dwp type-alist ancestors ens
        type-alist-to-be-extended wrld
        pot-lst pt)

; Programming Note:

; Our convention is to call this function to extend the type-alist unless we
; know that the supplied ts is neither *ts-proper-cons* nor *ts-improper-cons*.
; That is, we use this function to construct type-alist entries in only some of
; the cases.  See also extend-type-alist-simple and extend-type-alist.

  (mv-let (term ts ttree)
          (proper/improper-cons-ts-tuple term ts ttree force-flg dwp
                                         type-alist ancestors ens wrld
                                         pot-lst pt)
          (extend-type-alist term ts ttree type-alist-to-be-extended wrld)))

)

(defun ok-to-force-ens (ens)
  (and (enabled-numep *force-xnume* ens)
       t))

; Here is the function used to add an assumption to a ttree.  In principle,
; add-linear-assumption is called by type-set when it needs to force a
; hypothesis; but we don't want to make it mutually recursive with type-set and
; something like it is open coded in type-set.

(defun add-linear-assumption (target term type-alist ens
                                     immediatep force-flg wrld tag-tree)

; Adds the assumption term to the assumptions component of tag-tree, except
; that if the assumption is known to be true or known to be false under the
; type alist or is already in the tree, it is not added.

; This function returns (mv flg tag-tree'), where tag-tree' is the new
; tag-tree and flg is 
; a) :known-true if the assumption is known to be true (non-nil);
; b) :known-false if the assumption is known to be false;
; c) :added if the assumption is not known to be true or false and
;    the assumption was added; and
; d) :failed if the assumption is not known to be true or false, but
;    the assumption was not added because force-flg did not permit it.

  (mv-let
   (ts ttree)
   (type-set term force-flg nil type-alist nil ens wrld nil
             nil nil)

; Note that force-flg may be t above.  In that case, we force the
; guards of term.  For example, suppose term were (acl2-numberp (+ x y)),
; where x is known rational and y is unknown (and we use + for binary-+).
;  Then ts will come back *ts-t* and the ttree will contain a 
; (acl2-numberp y) 'assumption.  If we ultimately rely on ts then we
; must add ttree to the incoming tag-tree.  But if we ultimately just
; assume term, we can ignore ttree.

   (cond
    ((ts= ts *ts-nil*)
     (mv :known-false
         (cons-tag-trees ttree tag-tree)))
    ((not (ts-intersectp ts *ts-nil*))
     (mv :known-true
         (cons-tag-trees ttree tag-tree)))
    (t (mv-let
        (forced tag-tree)
        (cond
         ((not force-flg)
          (mv nil tag-tree))
         (t
          (force-assumption 'equal target term type-alist nil
                            immediatep
                            force-flg tag-tree)))
        (cond
         ((not forced)

; Since we cannot determine that the proposed assumption is non-nil,
; but we are not allowed to force, we signal failure instead.

          (mv :failed tag-tree))
         (t (mv :added tag-tree))))))))

; Historical note on the evolution of type-alist-clause.

; Early on, we had a simple version of type-alist-clause that simply looped
; through the literals in the clause, calling assume-true-false for each one.
; With this approach, before we eliminated guards from the logic, the result
; could be very sensitive to the order of the literals in the clause.  Consider
; the following theorem, which was not proved before we made this change:

#|
(implies (and (< 1 (+ 1 n))
              (< 0 n)
              (integerp n))
         (equal (< 1 (+ 1 n)) (< 0 n)))
|#

; The problem with this theorem was that the first two hypotheses
; weren't known to be Boolean unless we knew (for example) the third
; hypothesis.  A more low-level example could be obtained by applying
; type-alist-clause to ((< n '0) (not (integerp n)) (equal n '0)), and
; then checking the type-set of n in the resulting type-alist.

; One possible solution was to order the clause heuristically on the
; way in to type-alist-clause.  One nice thing about that idea is that
; the clause is only rearranged locally, so the change will not be
; pervasive.  However, the completeness of this process is
; questionable since the process is not iterative.

; So our next idea, through Version 1.7, was to iterate, calling
; type-alist-clause repeatedly until the answer stabilizes.  Actually we were
; slightly more clever than that.  The main trick was that when we applied
; assume-true-false to a literal, we did so with the force-flg set so that we
; were allowed to generate assumptions.  Then we checked if any assumptions
; were generated as a result.  If so, we delayed consideration of that literal
; and all other such literals until there was no further delay possible.  Note:
; if force-flg was actually t, then no change was necessary.  All this was
; simply to handle the case that force-flg is nil.

; Here are more details on that earlier approach.

; Stage 1:  Repeat the following process:  Loop through all the
; literals, allowing forcing to take place, but if any assumptions are
; generated then delay consideration of that literal.  As long as at
; least one literal seems to contribute to the evolving type-alist in
; a given pass through the loop, we'll pass through the loop again.

; Stage 2:  We may now assume that none of the remaining literals has
; been processed, because they all cause splitting.  We now process
; them all with the force-flg off so that we can be sure that no
; assumptions are generated.

; Stage 3 (perhaps overkill):  Repeat stage 1, but this time give up
; when we make it all the way through the loop without success.

; Starting with Version 1.8, with guards eliminated from the logic, we saw
; no reason to be so clever.  So, we reverted once again to a more
; straightforward algorithm.

(defun return-type-alist
  (hitp car-type-alist rest-type-alist original-type-alist wrld)

; Hitp should be t or nil, not 'contradiction.  We simply return (cons
; car-type-alist rest-type-alist), except that if hitp is nil then we
; assume that this is equal to original-type-alist and save ourselves a cons.

  (if hitp
      (mv-let (ts ttree)
              (assoc-type-alist (car car-type-alist) rest-type-alist wrld)
              (let* ((ts (or ts *ts-unknown*))
                     (int (ts-intersection ts (cadr car-type-alist))))
                (cond
                 ((ts= int ts) rest-type-alist)
                 ((ts= int (cadr car-type-alist))
                  (extend-type-alist
                   (car car-type-alist)
                   (cadr car-type-alist)
                   (cddr car-type-alist)
                   rest-type-alist
                   wrld))
                 (t
                  (extend-type-alist
                   (car car-type-alist)
                   int
                   (cons-tag-trees ttree (cddr car-type-alist))
                   rest-type-alist
                   wrld)))))
      original-type-alist))

(defun type-alist-equality-loop1 (type-alist top-level-type-alist ens w)

; Return (mv hitp type-alist ttree), where hitp is 'contradiction, t (meaning
; "hit"), or nil.  If hitp is 'contradiction, then ttree explains why;
; otherwise, ttree is irrelevant.  We map down type-alist (which is initially
; the top-level-type-alist) and milk each (EQUAL arg1 arg2) assumed true on it
; by setting the types of arg1 and arg2 each to the intersection of their
; top-level types.  This same intersection process was performed when the
; EQUALity was assumed true, but we might have since learned more about the
; types of the two arguments.

  (cond
   ((null type-alist)
    (mv nil nil nil))
   (t
    (mv-let (hitp rest-type-alist rest-ttree)
        (type-alist-equality-loop1 (cdr type-alist) top-level-type-alist ens w)
      (cond
       ((eq hitp 'contradiction)
        (mv hitp rest-type-alist rest-ttree))
       ((and (nvariablep (caar type-alist))
             (not (fquotep (caar type-alist)))
             (eq (ffn-symb (caar type-alist)) 'equal)
             (ts= (cadar type-alist) *ts-t*))
        (let ((arg1 (fargn (caar type-alist) 1))
              (arg2 (fargn (caar type-alist) 2))
              (ttree0 (cddar type-alist)))

; The following code is very similar to code in the 'equal case in
; assume-true-false

          (mv-let
              (ts1 ttree)
              (type-set arg1 nil nil top-level-type-alist nil ens w nil
                        nil nil)
            (mv-let
                (ts2 ttree)
                (type-set arg2 nil nil top-level-type-alist nil ens w ttree
                          nil nil)

; Observe that ttree records the dependencies of both args.

              (let ((int (ts-intersection ts1 ts2)))
                (cond
                 ((ts= int *ts-empty*)
                  (mv 'contradiction nil (puffert ttree)))
                 ((ts= ts1 ts2)
                  (mv hitp
                      (return-type-alist hitp (car type-alist)
                                         rest-type-alist type-alist w)
                      nil))
                 (t

; We return now with hitp = t.  But the returned type-alist could still equal
; the input type-alist, because when extend-with-proper/improper-cons-ts-tuple
; preserves the type-alist invariants, it can do so by refusing to extend
; type-alist.

                  (mv t
                      (let ((shared-ttree
                             (puffert (cons-tag-trees ttree0 ttree)))
                            (type-alist
                             (return-type-alist hitp (car type-alist)
                                                rest-type-alist type-alist w)))
                        (cond
                         ((ts= ts1 int)
                          (extend-with-proper/improper-cons-ts-tuple
                           arg2 int shared-ttree nil nil top-level-type-alist
                           nil ens type-alist w
                           nil nil))
                         ((ts= ts2 int)
                          (extend-with-proper/improper-cons-ts-tuple
                           arg1 int shared-ttree nil nil top-level-type-alist
                           nil ens type-alist w
                           nil nil))
                         (t
                          (extend-with-proper/improper-cons-ts-tuple
                           arg2 int shared-ttree nil nil top-level-type-alist
                           nil ens
                           (extend-with-proper/improper-cons-ts-tuple
                            arg1 int shared-ttree nil nil top-level-type-alist
                            nil ens type-alist w
                            nil nil)
                           w
                           nil nil))))
                      nil))))))))
       (t (mv hitp
              (return-type-alist
               hitp (car type-alist) rest-type-alist type-alist w)
              nil)))))))

(defun clean-up-alist (alist ans)

; Remove duplicate (mod equal) key entries from alist, accumulating the final
; answer onto ans (which is assumed to be nil initially).  We keep the first of
; each duplicate binding and thus we do not change the value of assoc-equal on
; the alist.  However, the order of the pairs in the returned alist is the
; reverse of that in the initial alist.

  (cond ((null alist) ans)
        ((assoc-equal (caar alist) ans)
         (clean-up-alist (cdr alist) ans))
        (t (clean-up-alist (cdr alist) (cons (car alist) ans)))))

(defun duplicate-keysp (alist)

; Determine whether there is a key bound twice (mod equal) in alist.

  (cond ((null alist) nil)
        ((assoc-equal (caar alist) (cdr alist)) t)
        (t (duplicate-keysp (cdr alist)))))

(defun clean-type-alist (type-alist)
  (if (duplicate-keysp type-alist)
      (reverse (clean-up-alist type-alist nil))
      type-alist))

(defun type-alist-equality-loop-exit (type-alist)
  (er hard 'type-alist-equality-loop-exit
      "We're apparently in an infinite type-alist-equality-loop!  The ~
       offending type-alist is:~%~x0"
      type-alist))

(defconst *type-alist-equality-loop-max-depth* 10)

(defun type-alist-equality-loop (type-alist0 ens w n)

; Returns (mv contradictionp type-alist ttree), where type-alist has no
; duplicate keys and all the (EQUAL arg1 arg2) assumed true in it have been
; milked so that arg1 and arg2 have equal type-sets.

  (let ((type-alist (clean-type-alist type-alist0)))
    (mv-let (hitp type-alist ttree)
            (type-alist-equality-loop1 type-alist type-alist ens w)
            (cond
             ((eq hitp 'contradiction)
              (mv t nil ttree))
             ((= n 0)
              (if (or (not hitp)

; It is possible, even with hitp, for (equal type-alist type-alist0) to be
; true.  There is a comment to this effect, regarding type-alist invariants, in
; type-alist-equality-loop1.  We discovered this in Version_2.7 during
; regression tests, specifically, with the last form in the book
; books/workshops/2000/manolios/pipeline/pipeline/deterministic-systems/128/top/ma128-isa128.
; This function was being called differently because of a change in in
; built-in-clausep to use forward-chaining.

                      (equal type-alist type-alist0))
                  (mv nil type-alist nil)
                (mv nil (type-alist-equality-loop-exit type-alist) nil)))
             (hitp
              (type-alist-equality-loop type-alist ens w (1- n)))
             (t
              (mv nil type-alist nil))))))

(defun reconsider-type-alist (type-alist xtype-alist force-flg ens w seen
                              pot-lst pt)

; We return (mv contradictionp xtype-alist' ttree) where either contradictionp
; is t and ttree explains, or else those two are nil and xtype-alist' is a
; strengthening of xtype-alist obtained by retyping every term in it, under
; type-alist, using double whammy.

  (cond ((null type-alist)
         (mv nil xtype-alist nil))
        ((or (member-equal (caar type-alist) seen)
             (and (nvariablep (caar type-alist))
                  (not (fquotep (caar type-alist)))
                  (eq (ffn-symb (caar type-alist)) 'IF)))

; Through Version_2.5 we retyped IF expressions.  But with the introduction
; of assume-true-false-if it became both prohibitively expensive and
; practically unnecessary.   So we don't do it anymore.

         (reconsider-type-alist (cdr type-alist)
                                xtype-alist force-flg ens w seen
                                pot-lst pt))
        (t
         (mv-let (ts ttree)
                 (type-set (caar type-alist)
                           force-flg t xtype-alist nil ens w nil
                           pot-lst pt)

; We are looking at a triple (term1 ts1 . ttree1).  So we obtain the type-set
; of term1, ts, using the double whammy.  That guarantees to intersect ts1 with
; the directly computed type-set of term1 and to cons together ttree1 and the
; directly computed ttree.  Proof: type-set will see this very triple (because
; seen ensures this is the first such one) and type-set-finish always conses in
; the old ttree.

; If ts is empty then a contradiction has been detected.  If ts is the same as
; ts1 we haven't learned anything and don't waste time and space adding the
; "new" binding of term1.

                 (cond
                  ((ts= ts *ts-empty*) (mv t nil ttree))
                  (t (reconsider-type-alist
                      (cdr type-alist)
                      (if (ts= ts (cadar type-alist))
                          xtype-alist
                          (extend-type-alist (caar type-alist) ts ttree
                                             xtype-alist w))
                      force-flg ens w
                      (cons (caar type-alist) seen)
                      pot-lst pt)))))))

(defun type-alist-clause-finish1 (lits ttree-lst force-flg type-alist ens wrld)

; Assume the falsity of every literal in lits, extending type-alist.  Return
; (mv contradictionp type-alist' ttree), where either contradictionp is t and
; ttree explains the contradiction or else those two results are nil and
; type-alist' is an extension of type-alist.

; This function is very sensitive to the order in which the literals are
; presented.  For example, if lits is ((rationalp (binary-+ '1 i)) (not
; (integerp i))) you will get back a type-alist in which (binary-+ '1 i) is
; assumed non-rational but i is assumed integral.  If the two literals are
; reversed you will get back the contradiction signal because if i is known
; integral then (binary-+ '1 i) is known integral.  It is the role of
; reconsider-type-alist to improve the first case.

  (cond ((null lits) (mv nil type-alist nil))
        (t (mv-let
            (mbt mbf tta fta ttree)
            (assume-true-false (car lits)
                               (car ttree-lst)
                               force-flg
                               nil
                               type-alist
                               nil
                               ens
                               wrld
                               nil nil :tta)
            (declare (ignore tta))
            (cond
             (mbt (mv t nil ttree))
             (mbf (type-alist-clause-finish1 (cdr lits) (cdr ttree-lst)
                                            force-flg 
                                            type-alist ens wrld))
             (t (type-alist-clause-finish1 (cdr lits) (cdr ttree-lst)
                                           force-flg
                                           fta ens wrld)))))))

(defun type-alist-clause-finish (lits ttree-lst force-flg type-alist ens wrld
                                 pot-lst pt)

; Assume the falsity of every literal in lits, extending type-alist.  Return
; (mv contradictionp type-alist' ttree), where either contradictionp is t and
; ttree explains the contradiction or else those two results are nil and
; type-alist' is an extension of type-alist.  This function is not as sensitive
; to order as the "single pass" version, type-alist-clause-finish1, because we
; reconsider the resulting type-alist and try to strengthen each type with a
; double whammy type-set.

  (mv-let (contradictionp type-alist ttree)
    (type-alist-clause-finish1 lits ttree-lst
                               force-flg type-alist ens wrld)
    (cond (contradictionp 
           (mv contradictionp type-alist ttree))
          (t
           (mv-let (contradictionp new-type-alist ttree)
             (reconsider-type-alist type-alist type-alist
                                    force-flg ens wrld nil
                                    pot-lst pt)
             (cond (contradictionp
                    (mv contradictionp new-type-alist ttree))
                   ((or (equal new-type-alist type-alist)
                        (null pot-lst))
                    (mv contradictionp new-type-alist ttree))

; As of v2-8, we reconsider-type-alist a second time if reconsidering
; once changed the type-alist and the pot-lst is not empty.  When we
; first constructed the type-alist, we did not use the pot-lst.  Thus,
; this second call to reconsider-type-alist performes much the same
; purpose relative to the pot-lst that the first (and originally, only)
; call plays with respect to the type-alist.  This type of heuristic
; is intimately tied up with the treatment of the DWP flag.

                   (t
                    (reconsider-type-alist new-type-alist new-type-alist
                                           force-flg ens wrld nil
                                           pot-lst pt))))))))

; Essay on Repetitive Typing

; Suppose you are asked to assume (not (integerp (1+ i))) and then are asked to
; assume (integerp i).  This might happen if you are building a type-alist for
; a sequence of literals and the literals appear in the order presented above.
; When it happens in that way, it is handled by type-alist-clause.  We have
; tried a variety of solutions to this problem, named and sketched below.

; naive: don't do anything.  The system will fail to note the contradiction
;  above.  This has actually burned Bishop in "real" theorems.

; force-based iteration in type-alist-clause: In Version 1.7 we had an
;  iteration scheme in type-alist-clause based on the fact that (in that
;  version of the system) (1+ i) forced the hyp that i is a number and, by
;  noting the attempted force and skipping the literal, we delayed the
;  processing of the first literal until we had processed the others.  But when
;  Version 1.8 came along and forcing became much less common, this approach
;  was removed and (without much thought) we reverted back to the naive
;  approach and burned Bishop.

; repetitious assume-true-false: The first fix to the situation just described
;  was to define a new version of assume-true-false, called
;  repetitious-assume-true-false, which noted when it added a pair on the
;  type-alist that bound a term which occurred as a proper subterm of some
;  previously bound term.  Thus, when (integerp i) was assumed in the context
;  of (not (integerp (1+ i))), the binding of i provoked
;  repetitious-assume-true-false to recompute the type of the previously bound
;  (1+ i).  To do this, repetitious-assume-true-false had to insist that the
;  recomputation use the "double whammy" idea (so as not to be fooled into just
;  looking up the previously assumed type) and we added the "dwp" flag to the
;  type-set clique.  Repetitious-assume-true-false was used in place of
;  assume-true-false everywhere in the code from rewrite.lisp onwards (i.e.,
;  type-set still used assume-true-false, as did normalize and
;  distribute-first-if).  Note that this is more general than an attack at the
;  type-alist-clause level because it affects any assumption, not just those
;  made at the clause level.  But we found this inefficient and have abandoned
;  it.

; reconsider-type-alist (1): As an alternative to the repetitious
;  assume-true-false, we reverted to the idea of working at the
;  type-alist-clause level, but basing the iteration on something besides
;  forcing.  The idea was to do a simple assume-true-false on each literal of
;  the clause (taking the false type-alist always) and then to reconsider the
;  final type-alist by computing the type of each bound term (with dwp) in the
;  context of the final type-alist.  Observe that we only make one
;  reconsideration pass.  As of this writing (Feb 10, 1995) Version 1.8 uses
;  this style of type-alist-clause.

; Here are some performance measures.

; The simplest example is one which fails unless we do some kind of iterated
; typing.  Here is Bishop's example:
#|
(thm (IMPLIES (AND (< 0 (+ (- I) N))
                   (NOT (INTEGERP (+ (- I) N)))
                   (INTEGERP N)
                   (INTEGERP I)
                   )
              nil))|#

; The naive method fails to prove this.  Force-based iteration catches it if (-
; I) forces something, but that is not the case in Version 1.8 and so that kind
; of iteration doesn't prove this.  The other two iterative schemes do catch
; it.  In experimenting though be sure to notice whether it is proved by
; type-alist-clause or something more expensive such as linear arithmetic.

; A good performance test is

#|(defthm ordered-symbol-alistp-remove-first-pair-test
  (implies (and (ordered-symbol-alistp l)
                (symbolp key)
                (assoc-eq key l))
           (ordered-symbol-alistp (remove-first-pair key l)))
  :hints (("Goal" :in-theory
           (disable ordered-symbol-alistp-remove-first-pair))))|#

; The naive approach does this in about 3.4 seconds (prove time, on Rana, a
; Sparc 2).  The repetitious approach takes 5.6 seconds.  The reconsidering
; approach takes 3.6.  Analysis of the repetitious data show that in this
; example repetitious-assume-true-false is called 5606 times but the repetition
; changes the type-alist only 92 times.  When it does change, the newly bound
; subterm was a variable symbol 80% of the 92 times.  and it was (CAR var) or
; (CDR var) in the remaining 20%.  The low hit rate of the repetitious
; assume-true-false encouraged us to reconsider the idea.

; But the following test convinced us that repetitious assume-true-false is
; doomed.  Consider processing the Nqthm package proofs.  The naive approach
; took about 1683 seconds.  The repetitious approach never completed.  See the
; comment in REWRITE-IF about repetitious-assume-true-false for an explanation.
; The reconsidering approach took 1654 seconds.  Only six events had times more
; than 1 second greater than in the naive approach (and overall, it is about 30
; seconds faster).

(defun type-alist-clause (cl ttree-lst force-flg type-alist ens wrld
                          pot-lst pt)

; We construct an extension of type-alist in which every literal of cl is
; assumed false.  Ttree-lst is a list of ttrees in (weak) 1:1 correspondence
; with cl.  The 'pt tags in the tree corresponding to a given literal indicates
; the parents of that literal.  (By "weak" we allow ttree-lst to be shorter
; than cl and for all "excess literals" of cl to have the nil ttree, i.e., we
; just cdr ttree-lst as we go through cl and use car of the car as a (possibly
; nil) ttree whenever we need a ttree.)  We return three values.  The first is
; t or nil and indicates whether we found a contradiction, i.e., that some
; literal of cl is true.  The second is the resulting type-alist (or nil if we
; got a contradiction).  The third is a ttree explaining the contradiction (or
; nil if we got no contradiction).  The type-alist entries generated for a
; given literal contain the corresponding ttree from ttree-lst.

; Warning: It is probably silly to call this function with force-flg =
; t except for heuristic use, e.g., to see what the probable types of
; some terms are.  The reason is that since we process the lits of cl
; one at a time, we may well add 'assumptions that are in fact denied
; by later literals, causing loops because the case split generates
; the original clause again.  If called with force-flg = t, then the
; type-alist we generate may have 'assumptions in it.  This type-alist
; must be handled with care so that if those assumptions are raised
; the necessary case splits are done.

; Note: Because force-flg can make provisionally false terms look like
; false terms, we sometimes appear simply to have dropped a literal of
; cl.  For example, consider the clause {...(not (acl2-numberp
; (binary-+ x y))) ...}.  Because of force-flg, that literal looks
; false, i.e., binary-+ "always" returns an acl2-numberp.  Assuming
; the literal raises the 'assumptions that x and y are both
; acl2-numberps but sets mbf below to t.  We therefore skip the
; literal, ignoring also its 'assumptions.  If we look at the clause
; as a formula: (implies (and ... (acl2-numberp (+ x y)) ...) ...)
; and imagine ourselves working on the conclusion, it is as though we
; have simply dropped the hypothesis.  This is sound.  More generally,
; we can throw away any pair from a type-alist.  But this is an
; acceptable thing to do here because, first, (acl2-numberp (+ x y))
; tells us nothing about x and y -- they may both be orange trees for
; all we know.  Second (pretending that we are in a pre-1.8 version of
; ACL2, in which the guard for + was part of its defining axiom), if (+ x y)
; occurs in the conjecture anywhere we will assume it is numeric anyway and
; deal with the 'assumptions that its args are acl2-numberps.  So in some
; sense, a hypothesis like (acl2-numberp (+ x y)) is irrelevant because it is
; always given and we pay for it only when it helps us.

  (if force-flg
      (type-alist-clause-finish cl ttree-lst force-flg type-alist ens wrld
                                pot-lst pt)
      (mv-let (contradictionp type-alist0 ttree0)
              (type-alist-clause-finish cl ttree-lst nil
                                        type-alist ens wrld
                                        pot-lst pt)
              (cond
               (contradictionp
                (mv t nil ttree0))
               (t
                (type-alist-equality-loop
                 type-alist0 ens wrld
                 *type-alist-equality-loop-max-depth*))))))

(defun known-whether-nil (x type-alist ens force-flg wrld ttree)

; This function determines whether we know, from type-set reasoning,
; whether x is nil or not.  It returns three values.  The first is the
; answer to the question "Do we know whether x is nil or not?"  If the
; answer to that question is yes, the second value is the answer to
; the question "Is x nil?" and the third value is a ttree that extends
; the input ttree and records the 'assumptions and dependencies of our
; derivation.  If the answer to the first question is no, the second
; and third values are nil.  Note that this function may generate
; 'assumptions and so splitting has to be considered.

; Note: This note ought to be plastered all over this code.  Beware
; the handling of ttree.  A bug was found in this function (11/9/92)
; because in the case that x was a quoted constant it ignored the
; incoming ttree and reported ``I know whether x is nil and I don't
; need any help!''  But we must always keep in mind that the ttree
; argument to many functions is an accumulator; the incoming ttree is
; responsible for the derivation of x itself and must be preserved.
; The original comment, above, is accurate: the outgoing ttree must be
; an extension of the incoming one when successful.

  (cond ((quotep x)
         (mv t (equal x *nil*) ttree))
        (t (mv-let (ts ttree)
                   (type-set x force-flg nil
                             type-alist nil ens wrld ttree
                             nil nil)
                   (cond ((ts= ts *ts-nil*)
                          (mv t t ttree))
                         ((ts-intersectp ts *ts-nil*)
                          (mv nil nil nil))
                         (t (mv t nil ttree)))))))

(defun ts-booleanp (term ens wrld)
  (mv-let (ts ttree)
          (type-set term nil nil nil nil ens wrld nil
                    nil nil)
          (cond ((tagged-object 'assumption ttree)
                 (er hard 'ts-booleanp
                     "It was thought impossible for a call of type-set with ~
                      force-flg = nil to produce an 'assumption, but ~
                      ts-booleanp did it on ~x0."
                     term))
                (t (ts-subsetp ts *ts-boolean*)))))

(defun weak-cons-occur (x y)

; Both x and y are terms.  In addition, x is known to be non-quoted
; and not a CONS expression.  Consider the binary tree obtained by
; viewing the term y as a CONS tree.  We return t iff x is a tip of
; that tree.

  (cond ((variablep y) (eq x y))
        ((fquotep y) nil)
        ((eq (ffn-symb y) 'cons)
         (or (weak-cons-occur x (fargn y 1))
             (weak-cons-occur x (fargn y 2))))
        (t (equal x y))))

(defun equal-x-cons-x-yp (lhs rhs)

; We answer the question ``Is (EQUAL lhs rhs) definitely nil?''  If
; our result is t, then the equality is definitely nil, without
; further qualification.  If we say we don't know, i.e., nil, nothing
; is claimed.

; However, we know some things about lhs and rhs that allow us to
; make this function answer ``I don't know'' more quickly and more
; often than it might otherwise.  We assume tht lhs and rhs are not
; identical terms and we know they are not both quoted constants
; (though either may be) and we know that their type sets have a
; non-empty intersection.

; We make our positive decision based on structural reasoning.  For
; example, (EQUAL x (CONS x &)), is NIL because x occurs properly
; within the CONS tree.  This observation does not depend on type-sets or
; anything else.

; However, we don't want to do too much work exploring the two terms.
; For example, if they are both large explicit values we don't want to
; look for them in eachother.  We know that we will eventually apply
; the CONS-EQUAL axiom, which will rewrite the equality of two conses
; (constants or otherwise) to the conjoined equalities of their
; components.  Thus, if both lhs and rhs are CONS expressions (i.e., a
; consityp) or quoted list constants, we just return nil and let the
; :REWRITE rules take care of it.

; One more minor optimization: if one of our args is a consityp and
; the other is a quoted constant then the constant must be a consp or
; else the type sets wouldn't intersect.

; Further Work:

; If this function is improved, also consider similar improvements to
; almost-quotep, which, like this function, currently only works on
; cons terms but could be generalized.

; This function has an arithmetic analog.  For example:
;    (EQUAL x (+ a (+ b (+ x c))))
; is NIL if x has type set *ts-acl2-number* and the type set of the
; sum of the other addends, (+ a (+ b c)), does not intersect *ts-zero*.
; We will eventually add :REWRITE rules to normalize + and do cancellation.
; That will make it easier to find x and may in fact subsume the kind of
; reasoning done here.  On the other hand, heuristically it is probably
; good to put every ounce of knowledge we have about these elementary
; things every place we can.

; Similarly, (EQUAL x (* a (* b (* x c)))), is false if x is a non-*ts-zero*
; *ts-acl2-number* and (* a (* b c)) is a non-*ts-integer*.

; Similarly, (EQUAL x (- x)) is nil if x is a non-*ts-zero* *ts-acl2-number*.

; Similarly, (EQUAL x (/ x)) is nil if x is a non-*ts-zero* *ts-ratio*
; or a *ts-complex-rational*.

  (cond ((variablep lhs)
         (cond ((consityp rhs)
                (or (weak-cons-occur lhs (fargn rhs 1))
                    (weak-cons-occur lhs (fargn rhs 2))))
               (t nil)))
        ((fquotep lhs) nil)
        ((eq (ffn-symb lhs) 'cons)
         (cond ((variablep rhs)
                (or (weak-cons-occur rhs (fargn lhs 1))
                    (weak-cons-occur rhs (fargn lhs 2))))
               ((fquotep rhs) nil)
               ((eq (ffn-symb rhs) 'cons) nil)
               (t (or (weak-cons-occur rhs (fargn lhs 1))
                      (weak-cons-occur rhs (fargn lhs 2))))))
        ((consityp rhs)
         (or (weak-cons-occur lhs (fargn rhs 1))
             (weak-cons-occur lhs (fargn rhs 2))))
        (t nil)))

(defun not-ident (term1 term2 type-alist ens wrld)

; We return two results.  The first is t iff (equal term1 term2) is
; false.  The second is a ttree that justifies the answer.  If the
; first result is nil, so is the second.  This function does not
; generate 'assumptions, so it is "weak."  I.e., it will not decide
; (equal (+ x y) (cons x y)) unless x and y are both acl2-numberps; one
; might have expected it to say the equality is false and to raise the
; assumptions that x and y are acl2-numberps.  The only place this function
; is used, presently, is in normalization, which does not raise
; assumptions.

  (cond ((and (quotep term1)
              (quotep term2))
         (mv (not (equal term1 term2)) nil))
        (t (mv-let (ts1 ttree)
                   (type-set term1 nil nil type-alist nil ens wrld nil
                             nil nil)
                   (mv-let (ts2 ttree)
                           (type-set term2 nil nil type-alist nil ens wrld ttree
                                     nil nil)
                           (cond
                            ((not (ts-intersectp ts1 ts2))
                             (mv t ttree))
                            ((equal-x-cons-x-yp term1 term2)

; Observe that we claim term1 is not term2 without any dependency on
; the type-set ttrees.  Even though the heuristic reasonableness of
; equal-x-cons-x-yp depends on the two having intersecting type-sets
; (otherwise, equal-x-cons-x-yp could do a little more work and decide
; the question), the correctness of its positive answer doesn't depend
; on anything.

                             (mv t nil))
                            (t (mv nil nil))))))))

(defun first-if (args i)

; This function searches the top level of the list args for an
; top-level IF expression.  If it does not find one, it returns
; 2 nils.  Otherwise, it returns the position of the first one
; it finds and the IF expression found.

  (cond ((null args) (mv nil nil))
        ((and (nvariablep (car args))
              (not (quotep (car args)))
              (eq (ffn-symb (car args)) 'if))
         (mv i (car args)))
        (t (first-if (cdr args) (1+ i)))))

(defun all-variablep (lst)
  (cond ((null lst) t)
        (t (and (variablep (car lst))
                (all-variablep (cdr lst))))))

(defun normalize-with-type-set (term iff-flg type-alist ens wrld ttree)

; The args to this function are as in normalize, below.  We return a
; term and a ttree.  The term is equivalent (mod iff-flg and
; type-alist) to term.  We base our answer on type-set reasoning
; alone.  No 'assumptions are generated.

  (mv-let
   (ts new-ttree)
   (type-set term nil nil type-alist nil ens wrld ttree
             nil nil)
   (let ((new-term
          (cond ((ts-intersectp ts *ts-nil*)
                 (cond
                  ((ts= ts *ts-nil*) *nil*)
                  (t term)))
                (iff-flg *t*)
                ((ts= ts *ts-t*) *t*)
                ((ts= ts *ts-zero*) *0*)
                (t term))))
     (mv new-term
         (if (equal term new-term) ttree new-ttree)))))

(mutual-recursion

; Note: The following function does not normalize IFs that occur in
; lambda arguments.  I once tried ``fixing'' that oversight and found
; that it cost a lot on big lambda examples in the Rockwell suite.

(defun normalize (term iff-flg type-alist ens wrld ttree)

; This function normalizes the if structure of term, simplifying with
; type-set reasoning as it goes.  We return two results, a term and a
; ttree.  The term is equivalent to term (propositionally equivalent,
; if the iff-flg is t) under the assumptions in type-alist.  No
; 'assumption tags are generated.  The ttree records the lemmas we
; used as recovered from the type-alist and elsewhere and is an
; extension of the input ttree.

; We expand some calls of members of
; *expandable-boot-strap-non-rec-fns* as we go.  The heuristic we use
; is that the expansion not introduce splits on the guards of the
; expanded fns.

; This function combines three more or less separate ideas: if
; normalization, expanding boot-strap non-rec fns, and simplifying
; with type-set information.  The reason we do all at once is to
; prevent explosions that would occur if we did them individually.

  (cond
   ((variablep term)
    (normalize-with-type-set term iff-flg type-alist ens wrld ttree))
   ((fquotep term)
    (mv (cond ((and iff-flg (not (equal term *nil*))) *t*)
              (t term))
        ttree))
   ((flambda-applicationp term)
    (mv-let (normal-args ttree)
            (normalize-lst (fargs term) nil
                           type-alist ens wrld ttree)

; We normalize the body of the lambda (under a type-alist determined
; from the normalized arguments).  But we leave a lambda application
; in place.

            (mv-let (normal-body ttree)
                    (normalize (lambda-body (ffn-symb term))
                               iff-flg
                               (zip-variable-type-alist
                                (lambda-formals (ffn-symb term))
                                (type-set-lst normal-args
                                              nil
                                              nil
                                              type-alist
                                              nil
                                              ens
                                              wrld
                                              nil nil))
                               ens wrld ttree)
                    (mv (mcons-term
                         (list 'lambda
                               (lambda-formals (ffn-symb term))
                               normal-body)
                         normal-args)
                        ttree))))
   ((eq (ffn-symb term) 'if)
    (mv-let
     (t1 ttree)
     (normalize (fargn term 1) t type-alist ens wrld ttree)
     (let ((t2 (fargn term 2))
           (t3 (fargn term 3)))
       (mv-let
        (mbt mbf tta fta ttree1)
        (assume-true-false t1 nil nil nil type-alist nil ens wrld
                           nil nil nil)
        (cond
         (mbt (normalize t2 iff-flg type-alist ens wrld
                         (cons-tag-trees ttree1 ttree)))
         (mbf (normalize t3 iff-flg type-alist ens wrld
                         (cons-tag-trees ttree1 ttree)))

; If mbt and mbf are both nil, then ttree1 is nil and we ignore it
; below.  (Actually, we use the same variable name to hold a different
; ttree.)

         ((and (nvariablep t1)
               (not (fquotep t1))
               (eq (ffn-symb t1) 'if))
          (let ((t11 (fargn t1 1))
                (t12 (fargn t1 2))
                (t13 (fargn t1 3)))
            (normalize (mcons-term* 'if t11
                                    (mcons-term* 'if t12 t2 t3)
                                    (mcons-term* 'if t13 t2 t3))
                       iff-flg type-alist ens wrld ttree)))
         (t (mv-let (t2 ttree)
                    (normalize t2 iff-flg tta ens wrld ttree)
                    (mv-let (t3 ttree)
                            (normalize t3 iff-flg fta ens wrld ttree)
                            (cond ((equal t2 t3)
                                   (mv t2 ttree))
                                  ((and (equal t1 t2)
                                        (equal t3 *nil*))
                                   (mv t1 ttree))
                                  ((and (equal t2 *t*)
                                        (equal t3 *nil*))

; If t1 is Boolean and t2 and t3 are t and nil respectively, we can normalize
; to t1.  Similarly, if t1 is not Boolean but we are working in iff-mode,
; we can normalize to t1.  At one time, we handled the iff-flg case separately
; and generalized the (equal t2 *t*) test to known-whether-nil.  That is
; unnecessary.  If iff-flg is set then t2 will have been normalized in iff
; mode.  Thus, if it is non-nilp t2 would be *t*.

                                   (cond
                                    (iff-flg (mv t1 ttree))
                                    (t
                                     (mv-let (ts1 ttree1)
                                             (type-set t1 nil nil type-alist
                                                       nil ens wrld nil
                                                       nil nil)
                                             (cond
                                              ((ts-subsetp ts1 *ts-boolean*)
                                               (mv t1 (cons-tag-trees ttree1
                                                                      ttree)))
                                              (t (mv (mcons-term* 'if t1 t2 t3)
                                                     ttree)))))))
                                  (t (mv (mcons-term* 'if t1 t2 t3)
                                         ttree)))))))))))
    (t
     (mv-let (normal-args ttree)
             (normalize-lst (fargs term) nil
                            type-alist ens wrld ttree)
             (let ((term (cons-term (ffn-symb term)
                                    normal-args)))
               (cond
                ((fquotep term) (mv term ttree))
                ((eq (ffn-symb term) 'equal)
                 (cond ((equal (fargn term 1) (fargn term 2))
                        (mv *t* ttree))
                       (t (mv-let (not-ident ttree1)
                                  (not-ident (fargn term 1)
                                             (fargn term 2)
                                             type-alist ens wrld)
                                  (cond (not-ident (mv *nil*
                                                       (cons-tag-trees ttree1
                                                                       ttree)))
                                        (t (distribute-first-if term iff-flg
                                                                type-alist ens
                                                                wrld
                                                                ttree)))))))
                (t (distribute-first-if term iff-flg type-alist ens wrld
                                        ttree))))))))

(defun normalize-lst (args iff-flg type-alist ens wrld ttree)
  (cond ((null args) (mv nil ttree))
        (t (mv-let (normal-arg ttree)
                   (normalize (car args) iff-flg type-alist ens wrld ttree)
                   (mv-let (normal-args ttree)
                           (normalize-lst (cdr args) iff-flg type-alist ens
                                          wrld ttree)
                           (mv (cons normal-arg normal-args) ttree))))))

(defun normalize-or-distribute-first-if (term iff-flg type-alist ens wrld
                                              ttree)
  (cond
   ((or (variablep term)
        (fquotep term))
    (normalize term iff-flg type-alist ens wrld ttree))
   ((eq (ffn-symb term) 'equal)
    (cond ((equal (fargn term 1) (fargn term 2))
           (mv *t* ttree))
          (t (mv-let (not-ident ttree1)
                     (not-ident (fargn term 1) (fargn term 2) type-alist ens
                                wrld)
                     (cond (not-ident (mv *nil* (cons-tag-trees ttree1 ttree)))
                           (t (distribute-first-if term iff-flg type-alist ens
                                                   wrld ttree)))))))
   (t
    (distribute-first-if term iff-flg type-alist ens wrld ttree))))

(defun distribute-first-if (term iff-flg type-alist ens wrld ttree)

; Term is known to be a non-variable non-quotep term in which all the
; args are in normal form.  We look for an if among its arguments and
; distribute the first one we find over the function symbol of term.
; In addition, this is the "bottoming out" code for normalize, at
; which we do anything else that is always done to non-IF terms.  In
; particular, we consider expanding certain non-rec fns.

; Rockwell Addition: We will get rid of certain functions like THE and
; HARD-ERROR in terms being processed by the theorem prover.  See
; remove-guard-holders and the Essay on the Removal of Guard Holders
; before it.  That code will eliminate prog2$ from terms before
; normalize ever sees it.  So I dropped the first case of the old
; code here.

  (mv-let (n if-expr)
          (first-if (fargs term) 0)
          (cond
           ((null n)

; There is no if at the top-level of term, and since all the args are
; normalized, we know there are no ifs at all.  We are thus at the
; bottom of the IF tree and type-alist has on it everything we know.
; We now expand the expandable boot-strap non-rec fns if we can do it
; without introducing their guards.

            (cond
             ((member-eq (ffn-symb term)
                         *expandable-boot-strap-non-rec-fns*)
              (normalize
               (subcor-var (formals (ffn-symb term) wrld)
                           (fargs term)
                           (body (ffn-symb term) t wrld))
               iff-flg type-alist ens wrld ttree))
             (t

; In this case the fn isn't expandable.  So we just take advantage of
; whatever type info we have and quit.

              (normalize-with-type-set term iff-flg
                                       type-alist ens wrld ttree))))

; And here is the code after which this function was named.  We have
; found an if-expr in the args of term at location n.  Since that if
; is in normal form, its test is not an if.  We split on that test and
; distribute the if.

; Note: In nqthm, instead of using subst-for-nth-arg as below, we used
; subst-expr and hence hit not only the top-level occurrence of the
; bad if but every occurrence of it in the term.  This seems better
; because it doesn't search the term for (unlikely) occurrences and
; doesn't cons up a copy of the term.  However, if proofs don't work,
; reconsider this decision.

           (t (let ((t1 (fargn if-expr 1)))
                (mv-let
                 (mbt mbf tta fta ttree1)
                 (assume-true-false t1 nil nil nil type-alist nil
                                    ens wrld
                                    nil nil nil)
                 (cond
                  (mbt
                   (normalize-or-distribute-first-if
                    (cons-term (ffn-symb term)
                               (subst-for-nth-arg (fargn if-expr 2)
                                                  n
                                                  (fargs term)))
                    iff-flg type-alist ens wrld
                    (cons-tag-trees ttree1 ttree)))
                  (mbf
                   (normalize-or-distribute-first-if
                    (cons-term (ffn-symb term)
                               (subst-for-nth-arg (fargn if-expr 3)
                                                  n
                                                  (fargs term)))
                    iff-flg type-alist ens wrld
                    (cons-tag-trees ttree1 ttree)))
                  (t
                   (mv-let
                    (t2 ttree)
                    (normalize-or-distribute-first-if
                     (cons-term (ffn-symb term)
                                (subst-for-nth-arg
                                 (fargn if-expr 2)
                                 n
                                 (fargs term)))
                     iff-flg tta ens wrld ttree)
                    (mv-let
                     (t3 ttree)
                     (normalize-or-distribute-first-if
                      (cons-term (ffn-symb term)
                                 (subst-for-nth-arg
                                  (fargn if-expr 3)
                                  n
                                  (fargs term)))
                      iff-flg fta ens wrld ttree)
                     (cond ((equal t2 t3) (mv t2 ttree))
                           ((and (equal t1 t2)
                                 (equal t3 *nil*))
                            (mv t1 ttree))
                           ((and (equal t2 *t*)
                                 (equal t3 *nil*))
                            (cond
                             (iff-flg (mv t1 ttree))
                             (t (mv-let
                                 (ts1 ttree1)
                                 (type-set t1 nil nil
                                           type-alist nil ens wrld nil
                                           nil nil)
                                 (cond
                                  ((ts-subsetp ts1 *ts-boolean*)
                                   (mv t1 (cons-tag-trees ttree1 ttree)))
                                  (t (mv (mcons-term* 'if t1 t2 t3) ttree)))))))
                           (t (mv (mcons-term* 'if t1 t2 t3)
                                  ttree)))))))))))))

)

; The following functions are used only for debugging purposes.

(defun decode-type-set1 (ts alist)
  (cond ((ts= ts *ts-empty*) nil)
        ((null alist) (list ts))
        ((ts-subsetp (cdar alist) ts)
         (cons (caar alist)
               (decode-type-set1 (ts-intersection ts
                                                  (ts-complement (cdar alist)))
                                 (cdr alist))))
        (t (decode-type-set1 ts (cdr alist)))))

(defun decode-type-set (ts)

; This function converts a type-set into an untranslated term in the ACL2
; coding world.  For example, 1536 is converted into *TS-CONS* (which is the
; (TS-UNION *TS-PROPER-CONS* *TS-IMPROPER-CONS*)).  We do this only so that we
; can look at computed type-sets symbolically.

  (cond ((ts= ts *ts-unknown*) '*ts-unknown*)
        ((ts= ts *ts-empty*) '*ts-empty*)
        ((ts-complementp ts)
         (list 'ts-complement
               (decode-type-set (ts-complement ts))))
        (t (let ((lst (decode-type-set1
                       ts
                       *code-type-set-alist*)))
             (cond ((null (cdr lst)) (car lst))
                   (t (cons 'ts-union lst)))))))

(defmacro dts (term type-alist)

; A typical interaction with this macro is:

; ACL2 |>(dts '(denominator x) (list (cons 'x *ts-rational*)))
; (mv *TS-POSITIVE-INTEGER* ttree)

  `(mv-let (ts ttree)
           (type-set ,term nil nil ,type-alist nil
                     (ens state)
                     (w state)
                     nil)
           (mv (decode-type-set ts) ttree)))

; It is convenient to be able to get your hands on the global enabled
; structure for testing fns that take an ens arg:

(defun ens (state)
  (f-get-global 'global-enabled-structure state))

(defmacro git (sym prop)
  `(getprop ,sym ,prop nil 'current-acl2-world (w state)))
