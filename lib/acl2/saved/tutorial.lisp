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

; This document currently has the following form:
;
; :doc ACL2-tutorial
;   introduction
;     OVERVIEW
;     ABOUT THIS TUTORIAL:
;     GETTING STARTED:
;     INTERACTING WITH ACL2:
;   :doc examples
;     EXAMPLE: TOWERS OF HANOI
;     EXAMPLE: EIGHTS PROBLEM
;     A LARGER EXAMPLE: A PHONEBOOK SPECIFICATION
;     DEFUN-SK-EXAMPLE:: example of quantified notions
;     :doc miscellaneous-examples
;       * FILE-READING-EXAMPLE:: example of reading files in ACL2
;       * MUTUAL-RECURSION-PROOF-EXAMPLE:: a small proof about mutually 
;            recursive functions
;       * GUARD-EXAMPLE    a brief transcript illustrating guards in ACL2
;   STARTUP
;   TIDBITS 
;   TIPS

(in-package "ACL2")

(deflabel ACL2-Tutorial
  :doc
  ":Doc-Section ACL2-Tutorial

  tutorial introduction to ACL2~/

  This section contains a tutorial ~il[introduction] to ACL2, some examples
  of the use of ACL2, and pointers to additional information.~/

  You might also find CLI Technical Report 101 helpful for a
  high-level view of the design goals of ACL2.

  If you are already familiar with Nqthm, ~pl[nqthm-to-acl2] for
  help in making the transition from Nqthm to ACL2.

  If you would like more familiarity with Nqthm, we suggest CLI
  Technical Report 100, which works through a non-trivial example.  A
  short version of that paper, which is entitled ``Interaction with
  the Boyer-Moore Theorem Prover:  A Tutorial Study Using the
  Arithmetic-Geometric Mean Theorem,'' is to appear in the Journal of
  Automated Reasoning's special issue on induction, probably in 1995
  or 1996.  Readers may well find that this paper indirectly imparts
  useful information about the effective use of ACL2.~/")

(deflabel Introduction
  :doc
  ":Doc-Section ACL2-Tutorial

  introduction to ACL2~/

  This section contains introductory material on ACL2 including what
  ACL2 is, how to get started using the system, how to read the
  output, and other introductory topics.  It was written almost
  entirely by Bill Young of Computational Logic, Inc.

  You might also find CLI Technical Report 101 helpful, especially if
  you are familiar with Nqthm.  If you would like more familiarity
  with Nqthm, we suggest CLI Technical Report 100.~/

  ~em[OVERVIEW]

  ACL2 is an automated reasoning system developed (for the first 9 years)
  at Computational Logic, Inc. and (from January, 1997) at the University
  of Texas at Austin.  It is the successor to the Nqthm (or Boyer-Moore)
  logic and proof system and its Pc-Nqthm interactive enhancement.  The
  acronym ACL2 actually stands for ``A Computational Logic for Applicative
  Common Lisp''.  This title suggests several distinct but related aspects
  of ACL2.

  We assume that readers of the ACL2 ~il[documentation] have at least a
  very slight familiarity with some Lisp-like language.  We will
  address the issue of prerequisites further, in ``ABOUT THIS
  TUTORIAL'' below.

  As a ~st[logic], ACL2 is a formal system with rigorously defined
  syntax and semantics.  In mathematical parlance, the ACL2 logic is a
  first-order logic of total recursive functions providing
  mathematical induction on the ordinals up to epsilon-0 and two
  extension principles: one for recursive definition and one for
  constrained introduction of new function symbols, here called
  encapsulation.  The syntax of ACL2 is that of Common Lisp; ACL2
  specifications are ``also'' Common Lisp programs in a way that we
  will make clear later.  In less formal language, the ACL2 logic is
  an integrated collection of rules for defining (or axiomatizing)
  recursive functions, stating properties of those functions, and
  rigorously establishing those properties.  Each of these activities
  is mechanically supported.

  As a ~st[specification language], ACL2 supports modeling of systems
  of various kinds.  An ACL2 function can equally be used to express
  purely formal relationships among mathematical entities, to describe
  algorithms, or to capture the intended behavior of digital systems.
  For digital systems, an ACL2 specification is a mathematical
  ~st[model] that is intended to formalize relevant aspects of system
  behavior.  Just as physics allows us to model the behavior of
  continuous physical systems, ACL2 allows us to model digital
  systems, including many with physical realizations such as computer
  hardware.  As early as the 1930's Church, Kleene, Turing and others
  established that recursive functions provide an expressive formalism
  for modeling digital computation.  Digital computation should be
  understood in a broad sense, covering a wide variety of activities
  including almost any systematic or algorithmic activity, or activity
  that can be reasonably approximated in that way.  This ranges from
  the behavior of a digital circuit to the behavior of a programming
  language compiler to the behavior of a controller for a physical
  system (as long as the system can be adequately modeled discretely).
  All of these have been modeled using ACL2 or its predecessor Nqthm.

  ACL2 is a ~st[computational] logic in at least three distinct
  senses.  First, the theory of recursive functions is often
  considered the mathematics of computation.  Church conjectured that
  any ``effective computation'' can be modeled as a recursive
  function.  Thus, ACL2 provides an expressive language for modeling
  digital systems.  Second, many ACL2 specifications are executable.
  In fact, recursive functions written in ACL2 ~st[are] Common Lisp
  functions that can be submitted to any compliant Common Lisp
  compiler and executed (in an environment where suitable
  ACL2-specific macros and functions are defined).  Third, ACL2 is
  computational in the sense that calculation is heavily integrated
  into the reasoning process.  Thus, an expression with explicit
  constant values but no free variables can be simplified by
  calculation rather than by complex logical manipulations.

  ACL2 is a powerful, automated ~st[theorem prover] or proof checker.
  This means that a competent user can utilize the ACL2 system to
  discover proofs of theorems stated in the ACL2 logic or to check
  previously discovered proofs.  The basic deductive steps in an
  ACL2-checked proof are often quite large, due to the sophisticated
  combination of decision procedures, conditional rewriting,
  mathematical and structural induction, propositional simplification,
  and complex heuristics to orchestrate the interactions of these
  capabilities.  Unlike some automated proof systems, ACL2 does not
  produce a formal proof.  However, we believe that if ACL2 certifies
  the ``theoremhood'' of a given conjecture, then such a formal proof
  exists and, therefore, the theorem is valid.  The ultimate result of
  an ACL2 proof session is a collection of ``~il[events],'' possibly
  grouped into ``~il[books],'' that can be replayed in ACL2.  Therefore, a
  proof can be independently validated by any ACL2 user.

  ACL2 may be used in purely automated mode in the shallow sense that
  conjectures are submitted to the prover and the user does not
  interact with the proof attempt (except possibly to stop it) until
  the proof succeeds or fails.  However, any non-trivial proof attempt
  is actually interactive, since successful proof ``~il[events]''
  influence the subsequent behavior of the prover.  For example,
  proving a lemma may introduce a rule that subsequently is used
  automatically by the prover.  Thus, any realistic proof attempt,
  even in ``automatic'' mode, is really an interactive dialogue with
  the prover to craft a sequence of ~il[events] building an
  appropriate theory and proof rules leading up to the proof of the
  desired result.  Also, ACL2 supports annotating a theorem with
  ``~il[hints]'' designed to guide the proof attempt.  By supplying
  appropriate ~il[hints], the user can suggest proof strategies that
  the prover would not discover automatically.  There is a
  ``~il[proof-tree]'' facility (~pl[proof-tree]) that allows the
  user to ~il[monitor] the progress and structure of a proof attempt
  in real-time.  Exploring failed proof attempts is actually where
  heavy-duty ACL2 users spend most of their time.

  ACL2 can also be used in a more explicitly interactive mode.  The
  ``~il[proof-checker]'' subsystem of ACL2 allows exploration of a proof on
  a fairly low level including expanding calls of selected function
  symbols, invoking specific ~il[rewrite] rules, and selectively navigating
  around the proof.  This facility can be used to gain sufficient
  insight into the proof to construct an automatic version, or to
  generate a detailed interactive-style proof that can be replayed in
  batch mode.

  Because ACL2 is all of these things ~-[] computational logic,
  specification language, ~il[programming] system, and theorem prover ~-[] it
  is more than the sum of its parts.  The careful integration of these
  diverse aspects has produced a versatile automated reasoning system
  suitable for building highly reliable digital systems.  In the
  remainder of this tutorial, we will illustrate some simple uses of
  this automated reasoning system.

  ~em[ABOUT THIS TUTORIAL]

  ACL2 is a complex system with a vast array of features, bells and
  whistles.  However, it is possible to perform productive work with
  the system using only a small portion of the available
  functionality.  The goals of this tutorial are to:
  ~bq[]

  familiarize the new user with the most basic features of and modes
  of interaction with ACL2;

  familiarize her with the form of output of the system; and

  work through a graduated series of examples.
  ~eq[]

  The more knowledge the user brings to this system, the easier it
  will be to become proficient.  On one extreme:  the ~st[ideal] user
  of ACL2 is an expert Common Lisp programmer, has deep understanding
  of automated reasoning, and is intimately familiar with the earlier
  Nqthm system.  Such ideal users are unlikely to need this tutorial.
  However, without some background knowledge, the beginning user is
  likely to become extremely confused and frustrated by this system.
  We suggest that a new user of ACL2 should:
  ~bq[]

  (a) have a little familiarity with Lisp, including basic Lisp
  programming and prefix notation (a Lisp reference manual such as Guy
  Steele's ``Common Lisp:  The Language'' is also helpful);

  (b) be convinced of the utility of formal modeling; and

  (c) be willing to gain familiarity with basic automated theorem
  proving topics such as rewriting and algebraic simplification.
  ~eq[]

  We will not assume any deep familiarity with Nqthm (the so-called
  ``Boyer-Moore Theorem Prover''), though the book ``A Computational
  Logic Handbook'' by Boyer and Moore (Academic Press, 1988) is an
  extremely useful reference for many of the topics required to become
  a competent ACL2 user.  We'll refer to it as ACLH below.

  As we said in the introduction, ACL2 has various facets.  For
  example, it can be used as a Common Lisp ~il[programming] system to
  construct application programs.  In fact, the ACL2 system itself is
  a large Common Lisp program constructed almost entirely within ACL2.
  Another use of ACL2 is as a specification and modeling tool.  That
  is the aspect we will concentrate on in the remainder of this
  tutorial.

  ~em[GETTING STARTED]

  This section is an abridged version of what's available elsewhere;
  feel free to ~pl[startup] for more details.

  How you start ACL2 will be system dependent, but you'll probably
  type something like ``acl2'' at your operating system prompt.
  Consult your system administrator for details.

  When you start up ACL2, you'll probably find yourself inside the
  ACL2 ~il[command] loop, as indicated by the following ~il[prompt].
  ~bv[]

    ACL2 !>

  ~ev[]
  If not, you should type ~c[(LP)].  ~l[lp], which has a lot more
  information about the ACL2 ~il[command] loop.

  There are two ``modes'' for using ACL2, ~c[:]~ilc[logic] and
  ~c[:]~ilc[program].  When you begin ACL2, you will ordinarily be in the
  ~c[:]~ilc[logic] mode.  This means that any new function defined is not
  only executable but also is axiomatically defined in the ACL2 logic.
  (~l[defun-mode] and ~pl[default-defun-mode].)  Roughly
  speaking, ~c[:]~ilc[program] mode is available for using ACL2 as a
  ~il[programming] language without some of the logical burdens
  necessary for formal reasoning.  In this tutorial we will assume
  that we always remain in ~c[:]~ilc[logic] mode and that our purpose is
  to write formal models of digital systems and to reason about them.

  Now, within the ACL2 ~il[command] loop you can carry out various
  kinds of activities, including the folllowing.  (We'll see examples
  later of many of these.)
  ~bq[]

  define new functions (~pl[defun]);

  execute functions on concrete data; 

  pose and attempt to prove conjectures about previously defined
  functions (~pl[defthm]);

  query the ACL2 ``~il[world]'' or database (e.g., ~pl[pe]); and

  numerous other things. 
  ~eq[]

  In addition, there is extensive on-line ~il[documentation], of which this
  tutorial introduction is a part.

  ~em[INTERACTING WITH ACL2]

  The standard means of interacting with ACL2 is to submit a sequence
  of forms for processing by the ACL2 system.  These forms are checked
  for syntactic and semantic acceptability and appropriately processed
  by the system.  These forms can be typed directly at the ACL2
  ~il[prompt].  However, most successful ACL2 users prefer to do their work
  using the Emacs text editor, maintaining an Emacs ``working'' buffer
  in which forms are edited.  Those forms are then copied to the ACL2
  interaction buffer, which is often the ~c[\"*shell*\"] buffer.

  In some cases, processing succeeds and makes some change to the ACL2
  ``logical ~il[world],'' which affects the processing of subsequent forms.
  How can this processing fail?  For example, a proposed theorem will
  be rejected unless all function symbols mentioned have been
  previously defined.  Also the ability of ACL2 to discover the proof
  of a theorem may depend on the user previously having proved other
  theorems.  Thus, the order in which forms are submitted to ACL2 is
  quite important.  Maintaining forms in an appropriate order in your
  working buffer will be helpful for re-playing the proof later.

  One of the most common ~il[events] in constructing a model is
  introducing new functions.  New functions are usually introduced
  using the ~ilc[defun] form; we'll encounter some exceptions later.
  Proposed function definitions are checked to make sure that they are
  syntactically and semantically acceptable (e.g., that all mentioned
  functions have been previously defined) and, for recursive
  functions, that their recursive calls ~st[terminate].  A recursive
  function definition is guaranteed to terminate if there is some some
  ``measure'' of the arguments and a ``well-founded'' ordering such
  that the arguments to the function get smaller in each recursive
  call.  ~l[well-founded-relation].

  For example, suppose that we need a function that will append two
  lists together.  (We already have one in the ACL2 ~ilc[append]
  function; but suppose perversely that we decide to define our own.)
  Suppose we submit the following definition (you should do so as well
  and study the system output):
  ~bv[]

    (defun my-app (x y)
      (if (atom x)
          y
        (cons (car x) (my-app x y))))

  ~ev[]
  The system responds with the following message:
  ~bv[]

    ACL2 Error in ( DEFUN MY-APP ...):  No :MEASURE was supplied with
    the definition of MY-APP.  Our heuristics for guessing one have not
    made any suggestions.  No argument of the function is tested along
    every branch and occurs as a proper subterm at the same argument
    position in every recursive call.  You must specify a :MEASURE.  See
    :DOC defun.

  ~ev[]
  This means that the system could not find an expression involving
  the formal parameters ~c[x] and ~c[y] that decreases under some
  well-founded order in every recursive call (there is only one such
  call).  It should be clear that there is no such measure in this
  case because the only recursive call doesn't change the arguments at
  all.  The definition is obviously flawed; if it were accepted and
  executed it would loop forever.  Notice that a definition that is
  rejected is not stored in the system database; there is no need to
  take any action to have it ``thrown away.''  Let's try again with
  the correct definition.  The interaction now looks like (we're also
  putting in the ACL2 ~il[prompt]; you don't type that):
  ~bv[]

    ACL2 !>(defun my-app (x y)
             (if (atom x)
                 y
               (cons (car x) (my-app (cdr x) y))))

    The admission of MY-APP is trivial, using the relation O<
    (which is known to be well-founded on the domain recognized by
    O-P) and the measure (ACL2-COUNT X).  We observe that the
    type of MY-APP is described by the theorem
    (OR (CONSP (MY-APP X Y)) (EQUAL (MY-APP X Y) Y)).
    We used primitive type reasoning.

    Summary
    Form:  ( DEFUN MY-APP ...)
    Rules: ((:FAKE-RUNE-FOR-TYPE-SET NIL))
    Warnings:  None
    Time:  0.07 seconds (prove: 0.00, print: 0.00, other: 0.07)
    MY-APP

  ~ev[]
  Notice that this time the function definition was accepted.  We
  didn't have to supply a measure explicitly; the system inferred one
  from the form of the definition.  On complex functions it may be
  necessary to supply a measure explicitly.  (~l[xargs].)

  The system output provides several pieces of information.
  ~bq[]

  The revised definition is acceptable.  The system realized that
  there is a particular measure (namely, ~c[(acl2-count x)]) and a
  well-founded relation (~c[o<]) under which the arguments of
  ~c[my-app] get smaller in recursion.  Actually, the theorem prover
  proved several theorems to admit ~c[my-app].  The main one was that
  when ~c[(atom x)] is false the ~c[acl2-count] of ~c[(cdr x)] is less
  than (in the ~c[o<] sense) the ~c[acl2-count] of ~c[x].
  ~ilc[Acl2-count] is the most commonly used measure of the ``size`` of
  an ACL2 object.  ~ilc[o<] is the ordering relation on ordinals
  less than epsilon-0.  On the natural numbers it is just ordinary
  ``<''.

  The observation printed about ``the type of MY-APP'' means that
  calls of the function ~c[my-app] will always return a value that is
  either a ~il[cons] pair or is equal to the second parameter.

  The summary provides information about which previously introduced
  definitions and lemmas were used in this proof, about some notable
  things to watch out for (the Warnings), and about how long this
  event took to process.
  ~eq[]

  Usually, it's not important to read this information.  However, it
  is a good habit to scan it briefly to see if the type information is
  surprising to you or if there are Warnings.  We'll see an example of
  them later.

  After a function is accepted, it is stored in the database and
  available for use in other function definitions or lemmas.  To see
  the definition of any function use the ~c[:]~ilc[pe] command
  (~pl[pe]).  For example,
  ~bv[]

    ACL2 !>:pe my-app
     L       73:x(DEFUN MY-APP (X Y)
                        (IF (ATOM X)
                            Y (CONS (CAR X) (MY-APP (CDR X) Y))))

  ~ev[]
  This displays the definition along with some other relevant
  information.  In this case, we know that this definition was
  processed in ~c[:]~ilc[logic] mode (the ``~c[L]'') and was the 73rd ~il[command]
  processed in the current session.

  We can also try out our newly defined function on some sample data.
  To do that, just submit a form to be evaluated to ACL2.  For
  example,
  ~bv[]

    ACL2 !>(my-app '(0 1 2) '(3 4 5))
    (0 1 2 3 4 5)
    ACL2 !>(my-app nil nil)
    NIL
    ACL2 !>

  ~ev[]

  Now suppose we want to prove something about the function just
  introduced.  We conjecture, for example, that the length of the
  ~il[append] of two lists is the sum of their lengths.  We can formulate
  this conjecture in the form of the following ACL2 ~ilc[defthm] form.
  ~bv[]

    (defthm my-app-length
      (equal (len (my-app x y))
             (+ (len x) (len y))))

  ~ev[]
  First of all, how did we know about the functions ~c[len] and ~ilc[+], etc.?
  The answer to that is somewhat unsatisfying ~-[] we know them from our
  past experience in using Common Lisp and ACL2.  It's hard to know
  that a function such as ~c[len] exists without first knowing some Common
  Lisp.  If we'd guessed that the appropriate function was called
  ~ilc[length] (say, from our knowledge of Lisp) and tried ~c[:pe length], we
  would have seen that ~ilc[length] is defined in terms of ~c[len], and we
  could have explored from there.  Luckily, you can write a lot of
  ACL2 functions without knowing too many of the primitive functions.

  Secondly, why don't we need some ``type'' hypotheses?  Does it make
  sense to append things that are not lists?  Well, yes.  ACL2 and
  Lisp are both quite weakly typed.  For example, inspection of the
  definition of ~c[my-app] shows that if ~c[x] is not a ~il[cons] pair, then
  ~c[(my-app x y)] always returns ~c[y], no matter what ~c[y] is.

  Thirdly, would it matter if we rewrote the lemma with the equality
  reversed, as follows?
  ~bv[]

    (defthm my-app-length2
      (equal (+ (len x) (len y))
             (len (my-app x y)))).

  ~ev[]
  The two are ~st[logically] equivalent, but...yes, it would make a
  big difference.  Recall our remark that a lemma is not only a
  ``fact'' to be proved; it also is used by the system to prove other
  later lemmas.  The current lemma would be stored as a ~il[rewrite] rule.
  (~l[rule-classes].)  For a ~il[rewrite] rule, a conclusion of the
  form ~c[(EQUAL LHS RHS)] means to replace instances of the ~c[LHS] by the
  appropriate instance of the ~c[RHS].  Presumably, it's better to ~il[rewrite]
  ~c[(len (my-app x y))] to ~c[(+ (len x) (len y))] than the other way around.
  The reason is that the system ``knows'' more about ~ilc[+] than it does
  about the new function symbol ~c[my-app].

  So let's see if we can prove this lemma.  Submitting our preferred
  ~ilc[defthm] to ACL2 (do it!), we get the following interaction:
  ~bv[]
            --------------------------------------------------
  ACL2 !>(defthm my-app-length
    (equal (len (my-app x y))
           (+ (len x) (len y))))

  Name the formula above *1.

  Perhaps we can prove *1 by induction.  Three induction schemes are
  suggested by this conjecture.  These merge into two derived
  induction schemes.  However, one of these is flawed and so we are
  left with one viable candidate.

  We will induct according to a scheme suggested by (LEN X), but
  modified to accommodate (MY-APP X Y).  If we let (:P X Y) denote *1
  above then the induction scheme we'll use is
  (AND (IMPLIES (NOT (CONSP X)) (:P X Y))
       (IMPLIES (AND (CONSP X) (:P (CDR X) Y))
                (:P X Y))).
  This induction is justified by the same argument used to admit LEN,
  namely, the measure (ACL2-COUNT X) is decreasing according to the
  relation O< (which is known to be well-founded on the domain
  recognized by O-P).  When applied to the goal at hand the
  above induction scheme produces the following two nontautological
  subgoals.

  Subgoal *1/2
  (IMPLIES (NOT (CONSP X))
           (EQUAL (LEN (MY-APP X Y))
                  (+ (LEN X) (LEN Y)))).

  But simplification reduces this to T, using the :definitions of FIX,
  LEN and MY-APP, the :type-prescription rule LEN, the :rewrite rule
  UNICITY-OF-0 and primitive type reasoning.

  Subgoal *1/1
  (IMPLIES (AND (CONSP X)
                (EQUAL (LEN (MY-APP (CDR X) Y))
                       (+ (LEN (CDR X)) (LEN Y))))
           (EQUAL (LEN (MY-APP X Y))
                  (+ (LEN X) (LEN Y)))).

  This simplifies, using the :definitions of LEN and MY-APP, primitive
  type reasoning and the :rewrite rules COMMUTATIVITY-OF-+ and
  CDR-CONS, to

  Subgoal *1/1'
  (IMPLIES (AND (CONSP X)
                (EQUAL (LEN (MY-APP (CDR X) Y))
                       (+ (LEN Y) (LEN (CDR X)))))
           (EQUAL (+ 1 (LEN (MY-APP (CDR X) Y)))
                  (+ (LEN Y) 1 (LEN (CDR X))))).

  But simplification reduces this to T, using linear arithmetic,
  primitive type reasoning and the :type-prescription rule LEN.

  That completes the proof of *1.

  Q.E.D.

  Summary
  Form:  ( DEFTHM MY-APP-LENGTH ...)
  Rules: ((:REWRITE UNICITY-OF-0)
          (:DEFINITION FIX)
          (:REWRITE COMMUTATIVITY-OF-+)
          (:DEFINITION LEN)
          (:REWRITE CDR-CONS)
          (:DEFINITION MY-APP)
          (:TYPE-PRESCRIPTION LEN)
          (:FAKE-RUNE-FOR-TYPE-SET NIL)
          (:FAKE-RUNE-FOR-LINEAR NIL))
  Warnings:  None
  Time:  0.30 seconds (prove: 0.13, print: 0.05, other: 0.12)
   MY-APP-LENGTH
            --------------------------------------------------
  ~ev[]

  Wow, it worked!  In brief, the system first tried to ~il[rewrite] and
  simplify as much as possible.  Nothing changed; we know that because
  it said ``Name the formula above *1.''  Whenever the system decides
  to name a formula in this way, we know that it has run out of
  techniques to use other than proof by induction.

  The induction performed by ACL2 is structural or ``Noetherian''
  induction.  You don't need to know much about that except that it is
  induction based on the structure of some object.  The heuristics
  infer the structure of the object from the way the object is
  recursively decomposed by the functions used in the conjecture.  The
  heuristics of ACL2 are reasonably good at selecting an induction
  scheme in simple cases.  It is possible to override the heuristic
  choice by providing an ~c[:induction] hint (~pl[hints]).  In the
  case of the theorem above, the system inducts on the structure of
  ~c[x] as suggested by the decomposition of ~c[x] in both ~c[(my-app x y)]
  and ~c[(len x)].  In the base case, we assume that ~c[x] is not a
  ~ilc[consp].  In the inductive case, we assume that it is a ~ilc[consp]
  and assume that the conjecture holds for ~c[(cdr x)].

  There is a close connection between the analysis that goes on when a
  function like ~c[my-app] is accepted and when we try to prove
  something inductively about it.  That connection is spelled out well
  in Boyer and Moore's book ``A Computational Logic,'' if you'd like to
  look it up.  But it's pretty intuitive.  We accepted ~c[my-app]
  because the ``size'' of the first argument ~c[x] decreases in the
  recursive call.  That tells us that when we need to prove something
  inductively about ~c[my-app], it's a good idea to try an induction on
  the size of the first argument.  Of course, when you have a theorem
  involving several functions, it may be necessary to concoct a more
  complicated ~il[induction] schema, taking several of them into account.
  That's what's meant by ``merging'' the induction schemas.

  The proof involves two cases: the base case, and the inductive case.
  You'll notice that the subgoal numbers go ~st[down] rather than up,
  so you always know how many subgoals are left to process.  The base
  case (~c[Subgoal *1/2]) is handled by opening up the function
  definitions, simplifying, doing a little rewriting, and performing
  some reasoning based on the types of the arguments.  You'll often
  encounter references to system defined lemmas (like
  ~c[unicity-of-0]).  You can always look at those with ~c[:]~ilc[pe]; but,
  in general, assume that there's a lot of simplification power under
  the hood that's not too important to understand fully.

  The inductive case (~c[Subgoal *1/1]) is also dispatched pretty
  easily.  Here we assume the conjecture true for the ~ilc[cdr] of the
  list and try to prove it for the entire list.  Notice that the
  prover does some simplification and then prints out an updated
  version of the goal (~c[Subgoal *1/1']).  Examining these gives you a
  pretty good idea of what's going on in the proof.

  Sometimes one goal is split into a number of subgoals, as happened
  with the induction above.  Sometimes after some initial processing
  the prover decides it needs to prove a subgoal by induction; this
  subgoal is given a name and pushed onto a stack of goals.  Some
  steps, like generalization (see ACLH), are not necessarily validity
  preserving; that is, the system may adopt a false subgoal while
  trying to prove a true one.  (Note that this is ok in the sense that
  it is not ``unsound.''  The system will fail in its attempt to
  establish the false subgoal and the main proof attempt will fail.)
  As you gain facility with using the prover, you'll get pretty good
  at recognizing what to look for when reading a proof script.  The
  prover's ~il[proof-tree] utility helps with monitoring an ongoing
  proof and jumping to designated locations in the proof
  (~pl[proof-tree]).  ~l[tips] for a number of useful
  pointers on using the theorem prover effectively.

  When the prover has successfully proved all subgoals, the proof is
  finished.  As with a ~ilc[defun], a summary of the proof is printed.
  This was an extremely simple proof, needing no additional guidance.
  More realistic examples typically require the user to look carefully
  at the failed proof log to find ways to influence the prover to do
  better on its next attempt.  This means either:  proving some rules
  that will then be available to the prover, changing the global state
  in ways that will affect the proof, or providing some ~il[hints]
  locally that will influence the prover's behavior.  Proving this
  lemma (~c[my-app-length]) is an example of the first.  Since this is
  a ~il[rewrite] rule, whenever in a later proof an instance of the
  form ~c[(LEN (MY-APP X Y))] is encountered, it will be rewritten to
  the corresponding instance of ~c[(+ (LEN X) (LEN Y))].  Disabling the
  rule by executing the ~il[command]
  ~bv[]

    (in-theory (disable my-app-length)),

  ~ev[] 
  is an example of a global change to the behavior of the prover
  since this ~il[rewrite] will not be performed subsequently (unless the rule
  is again ~il[enable]d).  Finally, we can add a (local) ~il[disable] ``hint''
  to a ~ilc[defthm], meaning to ~il[disable] the lemma only in the proof of one
  or more subgoals.  For example: 
  ~bv[]
  
    (defthm my-app-length-commutativity
      (equal (len (my-app x y))
             (len (my-app y x)))
      :hints ((\"Goal\" :in-theory (disable my-app-length))))
  
  ~ev[]
  In this case, the hint supplied is a bad idea since the proof is much
  harder with the hint than without it.  Try it both ways.

  By the way, to undo the previous event use ~c[:u] (~pl[u]).  To
  undo back to some earlier event use ~c[:ubt] (~pl[ubt]).  To view
  the current event use ~c[:pe :here].  To list several ~il[events] use
  ~c[:pbt] (~pl[pbt]).

  Notice the form of the hint in the previous example
  (~pl[hints]).  It specifies a goal to which the hint applies.
  ~c[\"Goal\"] refers to the top-level goal of the theorem.  Subgoals
  are given unique names as they are generated.  It may be useful to
  suggest that a function symbol be ~il[disable]d only for Subgoal
  1.3.9, say, and a different function ~il[enable]d only on Subgoal
  5.2.8.  Overuse of such ~il[hints] often suggests a poor global
  proof strategy.

  We now recommend that you visit ~il[documentation] on additional
  examples.  ~l[tutorial-examples].")

(deflabel Tutorial-Examples
  :doc
  ":Doc-Section ACL2-Tutorial

  examples of ACL2 usage~/

  Beginning users may find these examples at least as useful as the
  extensive ~il[documentation] on particular topics.  We suggest that you
  read these in the following order:
  ~bf[]
  ~il[Tutorial1-Towers-of-Hanoi]
  ~il[Tutorial2-Eights-Problem]
  ~il[Tutorial3-Phonebook-Example]
  ~il[Tutorial4-Defun-Sk-Example]
  ~il[Tutorial5-Miscellaneous-Examples]
  ~ef[]
  You may also wish to visit the other introductory sections,
  ~il[startup] and ~il[tidbits].  These contain further information
  related to the use of ACL2. ~/

  When you feel you have read enough examples, you might want to try
  the following very simple example on your own.  First define the
  notion of the ``fringe'' of a tree, where we identify trees simply
  as ~il[cons] structures, with ~il[atom]s at the leaves.  For
  example:
  ~bv[]

    ACL2 !>(fringe '((a . b) c . d))
    (A B C D)

  ~ev[]
  Next, define the notion of a ``leaf'' of a tree, i.e., a predicate
  ~c[leaf-p] that is true of an atom if and only if that atom appears
  at the tip of the tree.  Define this notion without referencing the
  function ~c[fringe].  Finally, prove the following theorem, whose
  proof may well be automatic (i.e., not require any lemmas).
  ~bv[]

    (defthm leaf-p-iff-member-fringe
      (iff (leaf-p atm x)
           (member-equal atm (fringe x))))

  ~ev[]
  For a solution, ~pl[solution-to-simple-example].")

(deflabel solution-to-simple-example
  :doc
  ":Doc-Section Tutorial-Examples

  solution to a simple example~/

  To see a statement of the problem solved below,
  ~pl[tutorial-examples].~/

  Here is a sequence of ACL2 ~il[events] that illustrates the use of ACL2
  to make definitions and prove theorems.  We will introduce the
  notion of the fringe of a tree, as well as the notion of a leaf of a
  tree, and then prove that the members of the fringe are exactly the
  leaves.

  We begin by defining the fringe of a tree, where we identify
  trees simply as ~il[cons] structures, with ~il[atom]s at the leaves.  The
  definition is recursive, breaking into two cases.  If ~c[x] is a ~il[cons],
  then the ~c[fringe] of ~c[x] is obtained by appending together the ~c[fringe]s
  of the ~ilc[car] and ~ilc[cdr] (left and right child) of ~c[x].  Otherwise, ~c[x] is an
  ~il[atom] and its ~c[fringe] is the one-element list containing only ~c[x].
  ~bv[]

    (defun fringe (x)
      (if (consp x)
          (append (fringe (car x))
                  (fringe (cdr x)))
        (list x)))

  ~ev[]
  Now that ~c[fringe] has been defined, let us proceed by defining the
  notion of an atom appearing as a ``leaf'', with the goal of proving
  that the leaves of a tree are exactly the members of its ~c[fringe].
  ~bv[]

    (defun leaf-p (atm x)
      (if (consp x)
          (or (leaf-p atm (car x))
              (leaf-p atm (cdr x)))
        (equal atm x)))

  ~ev[]
  The main theorem is now as follows.  Note that the rewrite rule
  below uses the equivalence relation ~ilc[iff] (~pl[equivalence])
  rather than ~ilc[equal], since ~ilc[member] returns the tail of the given
  list that begins with the indicated member, rather than returning a
  Boolean.  (Use ~c[:pe member] to see the definition of ~ilc[member].)
  ~bv[]

    (defthm leaf-p-iff-member-fringe
      (iff (leaf-p atm x)
           (member-equal atm (fringe x))))

  ~ev[]
  ")

(deflabel Tutorial1-Towers-of-Hanoi
  :doc
  ":Doc-Section Tutorial-Examples

  The Towers of Hanoi Example~/

  This example was written almost entirely by Bill Young of
  Computational Logic, Inc.~/

  We will tackle the famous ``Towers of Hanoi'' problem.  This problem
  is illustrated by the following picture.
  ~bv[]  
  
            |        |        |
            |        |        |
           ---       |        |
          -----      |        |
         -------     |        |
            
            A        B        C
  
  ~ev[] 
  We have three pegs ~-[] ~c[a], ~c[b], and ~c[c] ~-[] and ~c[n] disks of
  different sizes.  The disks are all initially on peg ~c[a].  The goal
  is to move all disks to peg ~c[c] while observing the following two
  rules.

  1. Only one disk may be moved at a time, and it must start and finish
  the move as the topmost disk on some peg;

  2. A disk can never be placed on top of a smaller disk. 

  Let's consider some simple instances of this problem.  If ~c[n] = 1,
  i.e., only one disk is to be moved, simply move it from ~c[a] to
  ~c[c].  If ~c[n] = 2, i.e., two disks are to be moved, the following
  sequence of moves suffices:  move from ~c[a] to ~c[b], move from ~c[a]
  to ~c[c], move from ~c[b] to ~c[c].

  In general, this problem has a straightforward recursive solution.
  Suppose that we desire to move ~c[n] disks from ~c[a] to ~c[c], using
  ~c[b] as the intermediate peg.  For the basis, we saw above that we
  can always move a single disk from ~c[a] to ~c[c].  Now if we have
  ~c[n] disks and assume that we can solve the problem for ~c[n-1]
  disks, we can move ~c[n] disks as follows.  First, move ~c[n-1] disks
  from ~c[a] to ~c[b] using ~c[c] as the intermediate peg; move the
  single disk from ~c[a] to ~c[c]; then move ~c[n-1] disks from ~c[b] to
  ~c[c] using ~c[a] as the intermediate peg.

  In ACL2, we can write a function that will return the sequence of
  moves.  One such function is as follows.  Notice that we have two
  base cases.  If ~c[(zp n)] then ~c[n] is not a positive integer; we
  treat that case as if ~c[n] were 0 and return an empty list of moves.
  If ~c[n] is 1, then we return a list containing the single
  appropriate move.  Otherwise, we return the list containing exactly
  the moves dictated by our recursive analysis above.
  ~bv[]

    (defun move (a b)
      (list 'move a 'to b))

    (defun hanoi (a b c n)
      (if (zp n)
          nil
        (if (equal n 1)
            (list (move a c))
          (append (hanoi a c b (1- n))
                  (cons (move a c)
                        (hanoi b a c (1- n)))))))

  ~ev[]
  Notice that we give ~c[hanoi] four arguments:  the three pegs, and
  the number of disks to move.  It is necessary to supply the pegs
  because, in recursive calls, the roles of the pegs differ.  In any
  execution of the algorithm, a given peg will sometimes be the source
  of a move, sometimes the destination, and sometimes the intermediate
  peg.

  After submitting these functions to ACL2, we can execute the ~c[hanoi]
  function on various specific arguments.  For example:
  ~bv[]

    ACL2 !>(hanoi 'a 'b 'c 1)
    ((MOVE A TO C))

    ACL2 !>(hanoi 'a 'b 'c 2)
    ((MOVE A TO B)
     (MOVE A TO C)
     (MOVE B TO C))

    ACL2 !>(hanoi 'a 'b 'c 3)
    ((MOVE A TO C)
     (MOVE A TO B)
     (MOVE C TO B)
     (MOVE A TO C)
     (MOVE B TO A)
     (MOVE B TO C)
     (MOVE A TO C))

  ~ev[]
  From the algorithm it is clear that if it takes ~c[m] moves to
  transfer ~c[n] disks, it will take ~c[(m + 1 + m) = 2m + 1] moves for
  ~c[n+1] disks.  From some simple calculations, we see that we need
  the following number of moves in specific cases:
  ~bv[]

     Disks   0   1   2   3   4   5   6   7  ...
     Moves   0   1   3   7  15  31  63  127 ...

  ~ev[]
  The pattern is fairly clear.  To move ~c[n] disks requires ~c[(2^n - 1)]
  moves.  Let's attempt to use ACL2 to prove that fact.

  First of all, how do we state our conjecture?  Recall that ~c[hanoi]
  returns a list of moves.  The length of the list (given by the
  function ~c[len]) is the number of moves required.  Thus, we can state
  the following conjecture.
  ~bv[]

    (defthm hanoi-moves-required-first-try
      (equal (len (hanoi a b c n))
             (1- (expt 2 n))))

  ~ev[]
  When we submit this to ACL2, the proof attempt fails.  Along the way
  we notice subgoals such as:
  ~bv[]

    Subgoal *1/1'
    (IMPLIES (NOT (< 0 N))
             (EQUAL 0 (+ -1 (EXPT 2 N)))).

  ~ev[]

  This tells us that the prover is considering cases that are
  uninteresting to us, namely, cases in which ~c[n] might be negative.
  The only cases that are really of interest are those in which ~c[n]
  is a non-negative natural number.  Therefore, we revise our theorem
  as follows:
  ~bv[]

    (defthm hanoi-moves-required
      (implies (and (integerp n) 
                    (<= 0 n))    ;; n is at least 0
               (equal (len (hanoi a b c n))
                      (1- (expt 2 n)))))

  ~ev[]
  and submit it to ACL2 again.  

  Again the proof fails.  Examining the proof script we encounter the
  following text.  (How did we decide to focus on this goal?  Some
  information is provided in ACLH, and the ACL2 documentation on
  ~il[tips] may be helpful.  But the simplest answer is:  this was the
  first goal suggested by the ``~il[proof-tree]'' tool below the start
  of the proof by induction.  ~l[proof-tree].)
  ~bv[]

    Subgoal *1/5''
    (IMPLIES (AND (INTEGERP N)
                  (< 0 N)
                  (NOT (EQUAL N 1))
                  (EQUAL (LEN (HANOI A C B (+ -1 N)))
                         (+ -1 (EXPT 2 (+ -1 N))))
                  (EQUAL (LEN (HANOI B A C (+ -1 N)))
                         (+ -1 (EXPT 2 (+ -1 N)))))
             (EQUAL (LEN (APPEND (HANOI A C B (+ -1 N))
                                 (CONS (LIST 'MOVE A 'TO C)
                                       (HANOI B A C (+ -1 N)))))
                    (+ -1 (* 2 (EXPT 2 (+ -1 N))))))

  ~ev[]
  It is difficult to make much sense of such a complicated goal.
  However, we do notice something interesting.  In the conclusion is
  a ~il[term] of the following shape.
  ~bv[]

     (LEN (APPEND ... ...))

  ~ev[]
  We conjecture that the length of the ~ilc[append] of two lists should
  be the sum of the lengths of the lists.  If the prover knew that, it
  could possibly have simplified this ~il[term] earlier and made more
  progress in the proof.  Therefore, we need a ~il[rewrite] rule that
  will suggest such a simplification to the prover.  The appropriate
  rule is:
  ~bv[]

    (defthm len-append
      (equal (len (append x y))
             (+ (len x) (len y))))

  ~ev[]
  We submit this to the prover, which proves it by a straightforward
  induction.  The prover stores this lemma as a ~il[rewrite] rule and
  will subsequently (unless we ~il[disable] the rule) replace
  ~il[term]s matching the left hand side of the rule with the
  appropriate instance of the ~il[term] on the right hand side.

  We now resubmit our lemma ~c[hanoi-moves-required] to ACL2.  On this
  attempt, the proof succeeds and we are done.   

  One bit of cleaning up is useful.  We needed the hypotheses that:
  ~bv[]

    (and (integerp n) (<= 0 n)).

  ~ev[]
  This is an awkward way of saying that ~c[n] is a natural number;
  natural is not a primitive data type in ACL2.  We could define a
  function ~c[naturalp], but it is somewhat more convenient to define a
  macro as follows:
  ~bv[]

    (defmacro naturalp (x)
      (list 'and (list 'integerp x)
                    (list '<= 0 x)))

  ~ev[]
  Subsequently, we can use ~c[(naturalp n)] wherever we need to note
  that a quantity is a natural number.  ~l[defmacro] for more
  information about ACL2 macros.  With this macro, we can reformulate
  our theorem as follows:
  ~bv[]

    (defthm hanoi-moves-required
      (implies (naturalp n)
               (equal (len (hanoi a b c n))
                      (1- (expt 2 n))))).

  ~ev[]
  Another interesting (but much harder) theorem asserts that the list
  of moves generated by our ~c[hanoi] function actually accomplishes
  the desired goal while following the rules.  When you can state and
  prove that theorem, you'll be a very competent ACL2 user.

  By the way, the name ``Towers of Hanoi'' derives from a legend that
  a group of Vietnamese monks works day and night to move a stack of
  64 gold disks from one diamond peg to another, following the rules
  set out above.  We're told that the world will end when they
  complete this task.  From the theorem above, we know that this
  requires 18,446,744,073,709,551,615 moves:
  ~bv[]

    ACL2 !>(1- (expt 2 64))
    18446744073709551615
    ACL2 !>

  ~ev[]
  We're guessing they won't finish any time soon.")

(deflabel Tutorial2-Eights-Problem
  :doc
  ":Doc-Section Tutorial-Examples

  The Eights Problem Example~/

  This example was written almost entirely by Bill Young of
  Computational Logic, Inc.~/

  This simple example was brought to our attention as one that Paul
  Jackson has solved using the NuPrl system.  The challenge is to
  prove the theorem:
  ~bv[]

    for all n > 7, there exist naturals i and j such that: n = 3i + 5j.

  ~ev[]
  In ACL2, we could phrase this theorem using quantification.  However
  we will start with a constructive approach, i.e., we will show that
  values of ~c[i] and ~c[j] exist by writing a function that will
  construct such values for given ~c[n].  Suppose we had a function
  ~c[(split n)] that returns an appropriate pair ~c[(i . j)].  Our
  theorem would be as follows:
  ~bv[]

    (defthm split-splits
      (let ((i (car (split n)))
            (j (cdr (split n))))
        (implies (and (integerp n)
                      (< 7 n))
                 (and (integerp i)
                      (<= 0 i)
                      (integerp j)
                      (<= 0 j)
                      (equal (+ (* 3 i) (* 5 j)) 
                             n)))))

  ~ev[]
  That is, assuming that ~c[n] is a natural number greater than 7,
  ~c[(split n)] returns values ~c[i] and ~c[j] that are in the
  appropriate relation to ~c[n].

  Let's look at a few cases:
  ~bv[]

    8 = 3x1 + 5x1;    11 = 3x2 + 5x1;     14 = 3x3 + 5x1;   ...
    9 = 3x3 + 5x0;    12 = 3x4 + 5x0;     15 = 3x5 + 5x0;   ...
   10 = 3x0 + 5x2;    13 = 3x1 + 5x2;     16 = 3x2 + 5x2;   ...

  ~ev[]
  Maybe you will have observed a pattern here; any natural number larger
  than 10 can be obtained by adding some multiple of 3 to 8, 9, or 10.
  This gives us the clue to constructing a proof.   It is clear that we
  can write split as follows:
  ~bv[]

    (defun bump-i (x)
      ;; Bump the i component of the pair
      ;; (i . j) by 1.
      (cons (1+ (car x)) (cdr x)))

    (defun split (n)
      ;; Find a pair (i . j) such that 
      ;; n = 3i + 5j.
      (if (or (zp n) (< n 8))
          nil ;; any value is really reasonable here
        (if (equal n 8)
            (cons 1 1)
          (if (equal n 9)
              (cons 3 0)
            (if (equal n 10)
                (cons 0 2)
              (bump-i (split (- n 3))))))))

  ~ev[]
  Notice that we explicitly compute the values of ~c[i] and ~c[j] for
  the cases of 8, 9, and 10, and for the degenerate case when ~c[n] is
  not a natural or is less than 8.  For all naturals greater than
  ~c[n], we decrement ~c[n] by 3 and bump the number of 3's (the value
  of i) by 1.  We know that the recursion must terminate because any
  integer value greater than 10 can eventually be reduced to 8, 9, or
  10 by successively subtracting 3.

  Let's try it on some examples:
  ~bv[]

    ACL2 !>(split 28)
    (6 . 2)

    ACL2 !>(split 45)
    (15 . 0)

    ACL2 !>(split 335)
    (110 . 1)

  ~ev[]
  Finally, we submit our theorem ~c[split-splits], and the proof
  succeeds.  In this case, the prover is ``smart'' enough to induct
  according to the pattern indicated by the function split.

  For completeness, we'll note that we can state and prove a quantified
  version of this theorem.  We introduce the notion ~c[split-able] to mean
  that appropriate ~c[i] and ~c[j] exist for ~c[n].
  ~bv[]

    (defun-sk split-able (n)
      (exists (i j)
              (equal n (+ (* 3 i) (* 5 j)))))

  ~ev[]
  Then our theorem is given below.  Notice that we prove it by
  observing that our previous function ~c[split] delivers just such an
  ~c[i] and ~c[j] (as we proved above).
  ~bv[]

    (defthm split-splits2 
      (implies (and (integerp n)
                    (< 7 n))
               (split-able n))
      :hints ((\"Goal\" :use (:instance split-able-suff 
                                      (i (car (split n)))
                                      (j (cdr (split n)))))))

  ~ev[]
  Unfortunately, understanding the mechanics of the proof requires
  knowing something about the way ~ilc[defun-sk] works.
  ~l[defun-sk] or ~pl[Tutorial4-Defun-Sk-Example] for more on
  that subject.")

(deflabel Tutorial3-Phonebook-Example

 #|
 Here is another solution to the exercise at the end of this topic.

 (defun good-phonebookp (bk)
   (setp (range bk)))

 (defthm member-equal-strip-cdrs-bind
   (implies (and (not (member-equal x (strip-cdrs bk)))
                 (not (equal x num)))
            (not (member-equal x (strip-cdrs (bind nm num bk))))))

 (defthm setp-range-bind
   (implies (and (setp (range bk))
                 (not (member num (range bk))))
            (setp (range (bind nm num bk))))
   :hints (("Goal" :in-theory (enable bind range))))

 (defthm ADD-PHONE-PRESERVES-NEW-INVARIANT
   (implies (and (good-phonebookp bk)
                 (not (member num (range bk))))
            (good-phonebookp (add-phone nm num bk))))

 (defthm CHANGE-PHONE-PRESERVES-NEW-INVARIANT
   (implies (and (good-phonebookp bk)
                 (not (member num (range bk))))
            (good-phonebookp (change-phone nm num bk))))

 (defthm member-equal-strip-cdrs-rembind
   (implies (not (member-equal x (strip-cdrs bk)))
            (not (member-equal x (strip-cdrs (rembind nm bk))))))

 (defthm setp-strip-cdrs-rembind
   (implies (setp (strip-cdrs bk))
            (setp (strip-cdrs (rembind nm bk))))
   :hints (("Goal" :in-theory (enable rembind))))

 (defthm DEL-PHONE-PRESERVES-NEW-INVARIANT
   (implies (good-phonebookp bk)
            (good-phonebookp (del-phone nm bk)))
   :hints (("Goal" :in-theory (enable range))))
 |#

  :doc
  ":Doc-Section Tutorial-Examples

  A Phonebook Specification~/

  The other tutorial examples are rather small and entirely self
  contained.  The present example is rather more elaborate, and makes
  use of a feature that really adds great power and versatility to
  ACL2, namely:  the use of previously defined collections of lemmas,
  in the form of ``~il[books].''

  This example was written almost entirely by Bill Young of
  Computational Logic, Inc.~/

  This example is based on one developed by Ricky Butler and Sally
  Johnson of NASA Langley for the PVS system, and subsequently revised
  by Judy Crow, ~i[et al], at SRI.  It is a simple phone book
  specification.  We will not bother to follow their versions closely,
  but will instead present a style of specification natural for ACL2.

  The idea is to model an electronic phone book with the following
  properties.
  ~bq[]

  Our phone book will store the phone numbers of a city.

  It must be possible to retrieve a phone number, given a name.

  It must be possible to add and delete entries. 

  ~eq[]

  Of course, there are numerous ways to construct such a model.  A
  natural approach within the Lisp/ACL2 context is to use
  ``association lists'' or ``alists.''  Briefly, an alist is a list of
  pairs ~c[(key .  value)] associating a value with a key.  A phone
  book could be an alist of pairs ~c[(name . pnum)].  To find the phone
  number associated with a given name, we merely search the alist
  until we find the appropriate pair.  For a large city, such a linear
  list would not be efficient, but at this point we are interested
  only in ~st[modeling] the problem, not in deriving an efficient
  implementation.  We could address that question later by proving our
  alist model equivalent, in some desired sense, to a more efficient
  data structure.

  We could build a theory of alists from scratch, or we can use a
  previously constructed theory (book) of alist definitions and facts.
  By using an existing book, we build upon the work of others, start
  our specification and proof effort from a much richer foundation,
  and hopefully devote more of our time to the problem at hand.
  Unfortunately, it is not completely simple for the new user to know
  what ~il[books] are available and what they contain.  We hope later
  to improve the documentation of the growing collection of ~il[books]
  available with the ACL2 distribution; for now, the reader is
  encouraged to look in the README file in the ~c[books] subdirectory.
  For present purposes, the beginning user can simply take our word
  that a book exists containing useful alist definitions and facts.
  On our local machine, these definitions and lemmas can be introduced
  into the current theory using the ~il[command]:
  ~bv[]

    (include-book \"/slocal/src/acl2/v1-9/books/public/alist-defthms\")

  ~ev[]
  This book has been ``certified,'' which means that the definitions
  and lemmas have been mechanically checked and stored in a safe
  manner.  (~l[books] and ~pl[include-book] for details.)

  Including this book makes available a collection of functions
  including the following:
  ~bv[]

  (ALISTP A)    ; is A an alist (actually a primitive ACL2 function)

  (BIND X V A)  ; associate the key X with value V in alist A

  (BINDING X A) ; return the value associated with key X in alist A

  (BOUND? X A)  ; is key X associated with any value in alist A

  (DOMAIN A)    ; return the list of keys bound in alist A

  (RANGE A)     ; return the list of values bound to keys in alist A

  (REMBIND X A) ; remove the binding of key X in alist A

  ~ev[]
  Along with these function definitions, the book also provides a
  number of proved lemmas that aid in simplifying expressions
  involving these functions.  (~l[rule-classes] for the way in
  which lemmas are used in simplification and rewriting.)  For
  example,
  ~bv[]

    (defthm bound?-bind 
      (equal (bound? x (bind y v a))
             (or (equal x y)
                 (bound? x a))))

  ~ev[]
  asserts that ~c[x] will be bound in ~c[(bind y v a)] if and only if:
  either ~c[x = y] or ~c[x] was already bound in ~c[a].  Also,
  ~bv[]

    (defthm binding-bind
      (equal (binding x (bind y v a))
             (if (equal x y)
                 v
               (binding x a))))

  ~ev[]
  asserts that the resulting binding will be ~c[v], if ~c[x = y], or the
  binding that ~c[x] had in ~c[a] already, if not.

  Thus, the inclusion of this book essentially extends our
  specification and reasoning capabilities by the addition of new
  operations and facts about these operations that allow us to build
  further specifications on a richer and possibly more intuitive
  foundation.

  However, it must be admitted that the use of a book such as this has
  two potential limitations:
  ~bq[]

  the definitions available in a book may not be ideal for your
  particular problem;

  it is (extremely) likely that some useful facts (especially, ~il[rewrite]
  rules) are not available in the book and will have to be proved.

  ~eq[]
  For example, what is the value of ~c[binding] when given a key that
  is not bound in the alist?  We can find out by examining the
  function definition.  Look at the definition of the ~c[binding]
  function (or any other defined function), using the ~c[:]~ilc[pe] command:
  ~bv[]

    ACL2 !>:pe binding
       d     33  (INCLUDE-BOOK
                      \"/slocal/src/acl2/v1-9/books/public/alist-defthms\")
                 \
    >V d          (DEFUN BINDING (X A)
                         \"The value bound to X in alist A.\"
                         (DECLARE (XARGS :GUARD (ALISTP A)))
                         (CDR (ASSOC-EQUAL X A)))

  ~ev[]

  This tells us that ~c[binding] was introduced by the given
  ~ilc[include-book] form, is currently ~il[disable]d in the current
  theory, and has the definition given by the displayed ~ilc[defun] form.
  We see that ~c[binding] is actually defined in terms of the primitive
  ~ilc[assoc-equal] function.  If we look at the definition of
  ~ilc[assoc-equal]:
  ~bv[]

    ACL2 !>:pe assoc-equal
     V     -489  (DEFUN ASSOC-EQUAL (X ALIST)
                        (DECLARE (XARGS :GUARD (ALISTP ALIST)))
                        (COND ((ENDP ALIST) NIL)
                              ((EQUAL X (CAR (CAR ALIST)))
                               (CAR ALIST))
                              (T (ASSOC-EQUAL X (CDR ALIST)))))

  ~ev[]

  we can see that ~ilc[assoc-equal] returns ~c[nil] upon reaching the end
  of an unsuccessful search down the alist.  So ~c[binding] returns
  ~c[(cdr nil)] in that case, which is ~c[nil].  Notice that we could also
  have investigated this question by trying some simple examples.
  ~bv[]

    ACL2 !>(binding 'a nil)
    NIL

    ACL2 !>(binding 'a (list (cons 'b 2)))
    NIL

  ~ev[]

  These definitions aren't ideal for all purposes. For one thing,
  there's nothing that keeps us from having ~c[nil] as a value bound to
  some key in the alist.  Thus, if ~c[binding] returns ~c[nil] we don't
  always know if that is the value associated with the key in the
  alist, or if that key is not bound.  We'll have to keep that
  ambiguity in mind whenever we use ~c[binding] in our specification.
  Suppose instead that we wanted ~c[binding] to return some error
  string on unbound keys.  Well, then we'd just have to write our own
  version of ~c[binding].  But then we'd lose much of the value of
  using a previously defined book.  As with any specification
  technique, certain tradeoffs are necessary.

  Why not take a look at the definitions of other alist functions and
  see how they work together to provide the ability to construct and
  search alists?  We'll be using them rather heavily in what follows
  so it will be good if you understand basically how they work.
  Simply start up ACL2 and execute the form shown earlier, but
  substituting our directory name for the top-level ACL2 directory
  with yours.  Alternatively, the following should work if you start
  up ACL2 in the directory of the ACL2 sources:
  ~bv[]

    (include-book \"books/public/alist-defthms\")

  ~ev[]
  Then, you can use ~c[:]~il[pe] to look at function definitions.
  You'll soon discover that almost all of the definitions are built on
  definitions of other, more primitive functions, as ~c[binding] is
  built on ~ilc[assoc-equal].  You can look at those as well, of course,
  or in many cases visit their documentation.

  The other problem with using a predefined book is that it will
  seldom be ``sufficiently complete,'' in the sense that the
  collection of ~il[rewrite] rules supplied won't be adequate to prove
  everything we'd like to know about the interactions of the various
  functions.  If it were, there'd be no real reason to know that
  ~c[binding] is built on top of ~ilc[assoc-equal], because everything
  we'd need to know about ~c[binding] would be nicely expressed in the
  collection of theorems supplied with the book.  However, that's very
  seldom the case.  Developing such a collection of rules is currently
  more art than science and requires considerable experience.  We'll
  encounter examples later of ``missing'' facts about ~c[binding] and
  our other alist functions.  So, let's get on with the example.

  Notice that alists are mappings of keys to values; but, there is no
  notion of a ``type'' associated with the keys or with the values.
  Our phone book example, however, does have such a notion of types;
  we map names to phone numbers.  We can introduce these ``types'' by
  explicitly defining them, e.g., names are strings and phone numbers
  are integers.  Alternatively, we can ~st[partially define] or
  axiomatize a recognizer for names without giving a full definition.
  A way to safely introduce such ``constrained'' function symbols in
  ACL2 is with the ~ilc[encapsulate] form.  For example, consider the
  following form.
  ~bv[]

    (encapsulate
      ;; Introduce a recognizer for names and give a ``type'' lemma.
      (((namep *) => *))
      ;;
      (local (defun namep (x)
               ;; This declare is needed to tell
               ;; ACL2 that we're aware that the 
               ;; argument x is not used in the body
               ;; of the function.
               (declare (ignore x))
               t))
      ;;
      (defthm namep-booleanp
        (booleanp (namep x))))

  ~ev[] 

  This ~ilc[encapsulate] form introduces the new function ~c[namep] of one
  argument and one result and constrains ~c[(namep x)] to be Boolean,
  for all inputs ~c[x].  More generally, an encapsulation establishes
  an environment in which functions can be defined and theorems and
  rules added without necessarily introducing those functions,
  theorems, and rules into the environment outside the encapsulation.
  To be admissible, all the events in the body of an encapsulate must be
  admissible.  But the effect of an encapsulate is to assume only the
  non-local events.

  The first ``argument'' to ~c[encapsulate], ~c[((namep (x) t))] above,
  declares the intended ~il[signature]s of new function symbols that
  will be ``exported'' from the encapsulation without definition.  The
  ~ilc[local] ~ilc[defun] of ~c[name] defines name within the encapsulation
  always to return ~c[t].  The ~c[defthm] event establishes that
  ~c[namep] is Boolean.  By making the ~c[defun] local but the ~c[defthm]
  non-~c[local] this encapsulate constrains the undefined function
  ~c[namep] to be Boolean; the admissibility of the encapsulation
  establishes that there exists a Boolean function (namely the
  constant function returning ~c[t]).

  We can subsequently use ~c[namep] as we use any other Boolean
  function, with the proviso that we know nothing about it except that
  it always returns either ~c[t] or ~c[nil].  We use ~c[namep] to
  ``recognize'' legal keys for our phonebook alist.

  We wish to do something similar to define what it means to be a legal
  phone number.  We submit the following form to ACL2:
  ~bv[]

    (encapsulate
      ;; Introduce a recognizer for phone numbers.
      (((pnump *) => *))
      ;;
      (local (defun pnump (x)
               (not (equal x nil))))
      ;;
      (defthm pnump-booleanp
        (booleanp (pnump x)))
      ;;
      (defthm nil-not-pnump
        (not (pnump nil)))).

  ~ev[]
  This introduces a Boolean-valued recognizer ~c[pnump], with the
  additional proviso that the constant ~c[nil] is not a ~c[pnump].  We
  impose this restriction to guarantee that we'll never bind a name to
  ~c[nil] in our phone book and thereby introduce the kind of ambiguity
  described above regarding the use of ~c[binding].

  Now a legal phone book is an alist mapping from ~c[namep]s to
  ~c[pnump]s.  We can define this as follows:
  ~bv[]

    (defun name-phonenum-pairp (x)
      ;; Recognizes a pair of (name . pnum).
      (and (consp x)
           (namep (car x))
           (pnump (cdr x))))

    (defun phonebookp (l)
      ;; Recognizes a list of such pairs.
      (if (not (consp l))
          (null l)
        (and (name-phonenum-pairp (car l))
             (phonebookp (cdr l)))))

  ~ev[]
  Thus, a phone book is really a list of pairs ~c[(name . pnum)].
  Notice that we have not assumed that the keys of the phone book are
  distinct.  We'll worry about that question later.  (It is not always
  desirable to insist that the keys of an alist be distinct.  But it
  may be a useful requirement for our specific example.)

  Now we are ready to define some of the functions necessary for our
  phonebook example.  The functions we need are:

  ~bv[]

  (IN-BOOK? NM BK)          ; does NM have a phone number in BK

  (FIND-PHONE NM BK)        ; find NM's phone number in phonebook BK

  (ADD-PHONE NM PNUM BK)    ; give NM the phone number PNUM in BK

  (CHANGE-PHONE NM PNUM BK) ; change NM's phone number to PNUM in BK

  (DEL-PHONE NM PNUM)       ; remove NM's phone number from BK

  ~ev[]

  Given our underlying theory of alists, it is easy to write these
  functions.  But we must take care to specify appropriate
  ``boundary'' behavior.  Thus, what behavior do we want when, say, we
  try to change the phone number of a client who is not currently in
  the book?  As usual, there are numerous possibilities; here we'll
  assume that we return the phone book unchanged if we try anything
  ``illegal.''

  Possible definitions of our phone book functions are as follows.
  (Remember, an ~c[include-book] form such as the ones shown earlier
  must be executed in order to provide definitions for functions such
  as ~c[bound?].)
  ~bv[]

    (defun in-book? (nm bk)
      (bound? nm bk))

    (defun find-phone (nm bk)
      (binding nm bk))

    (defun add-phone (nm pnum bk)
      ;; If nm already in-book?, make no change.
      (if (in-book? nm bk)
          bk
        (bind nm pnum bk)))

    (defun change-phone (nm pnum bk)
      ;; Make a change only if nm already has a phone number.
      (if (in-book? nm bk)
          (bind nm pnum bk)
        bk))

    (defun del-phone (nm bk)
      ;; Remove the binding from bk, if there is one.
      (rembind nm bk))

  ~ev[]
  Notice that we don't have to check whether a name is in the book
  before deleting, because ~c[rembind] is essentially a no-op if ~c[nm]
  is not bound in ~c[bk].

  In some sense, this completes our specification.  But we can't have
  any real confidence in its correctness without validating our
  specification in some way.  One way to do so is by proving some
  properties of our specification.  Some candidate properties are:
  ~bq[]

  1. A name will be in the book after we add it.

  2. We will find the most recently added phone number for a client.

  3. If we change a number, we'll find the change.

  4. Changing and then deleting a number is the same as just deleting.

  5. A name will not be in the book after we delete it.
  ~eq[]

  Let's formulate some of these properties.  The first one, for example, is:
  ~bv[]

    (defthm add-in-book 
      (in-book? nm (add-phone nm pnum bk))).

  ~ev[]
  You may wonder why we didn't need any hypotheses about the ``types''
  of the arguments.  In fact, ~c[add-in-book] is really expressing a
  property that is true of alists in general, not just of the
  particular variety of alists we are dealing with.  Of course, we
  could have added some extraneous hypotheses and proved:
  ~bv[]

    (defthm add-in-book 
      (implies (and (namep nm)
                    (pnump pnum)
                    (phonebookp bk))
               (in-book? nm (add-phone nm pnum bk)))),

  ~ev[]
  but that would have yielded a weaker and less useful lemma because it
  would apply to fewer situations.  In general, it is best to state
  lemmas in the most general form possible and to eliminate unnecessary
  hypotheses whenever possible.  The reason for that is simple: lemmas
  are usually stored as rules and used in later proofs.  For a lemma to
  be used, its hypotheses must be relieved (proved to hold in that
  instance); extra hypotheses require extra work.  So we avoid them
  whenever possible. 

  There is another, more important observation to make about our
  lemma.  Even in its simpler form (without the extraneous
  hypotheses), the lemma ~c[add-in-book] may be useless as a
  ~il[rewrite] rule.  Notice that it is stated in terms of the
  non-recursive functions ~c[in-book?] and ~c[add-phone].  If such
  functions appear in the left hand side of the conclusion of a lemma,
  the lemma may not ever be used.  Suppose in a later proof, the
  theorem prover encountered a ~il[term] of the form:
  ~bv[]

    (in-book? nm (add-phone nm pnum bk)).

  ~ev[]
  Since we've already proved ~c[add-in-book], you'd expect that this
  would be immediately reduced to true.  However, the theorem prover
  will often ``expand'' the non-recursive definitions of ~c[in-book?]
  and ~c[add-phone] using their definitions ~st[before] it attempts
  rewriting with lemmas.  After this expansion, lemma ~c[add-in-book]
  won't ``match'' the ~il[term] and so won't be applied.  Look back at
  the proof script for ~c[add-in-proof] and you'll notice that at the
  very end the prover warned you of this potential difficulty when it
  printed:
  ~bv[]

    Warnings:  Non-rec
    Time:  0.18 seconds (prove: 0.05, print: 0.00, other: 0.13)
    ADD-IN-BOOK

  ~ev[]
  The ``Warnings'' line notifies you that there are non-recursive
  function calls in the left hand side of the conclusion and that this
  problem might arise.  Of course, it may be that you don't ever plan
  to use the lemma for rewriting or that your intention is to
  ~il[disable] these functions.  ~il[Disable]d functions are not
  expanded and the lemma should apply.  However, you should always
  take note of such warnings and consider an appropriate response.  By
  the way, we noted above that ~c[binding] is ~il[disable]d.  If it
  were not, none of the lemmas about ~c[binding] in the book we
  included would likely be of much use for exactly the reason we just
  gave.

  For our current example, let's assume that we're just investigating
  the properties of our specifications and not concerned about using
  our lemmas for rewriting.  So let's go on.  If we really want to
  avoid the warnings, we can add ~c[:rule-classes nil] to each
  ~c[defthm] event; ~pl[rule-classes].

  Property 2 is:  we always find the most recently added phone number
  for a client.  Try the following formalization:
  ~bv[]

    (defthm find-add-first-cut
      (equal (find-phone nm (add-phone nm pnum bk))
             pnum))

  ~ev[]
  and you'll find that the proof attempt fails.  Examining the proof
  attempt and our function definitions, we see that the lemma is false
  if ~c[nm] is already in the book.  We can remedy this situation by
  reformulating our lemma in at least two different ways:
  ~bv[]

    (defthm find-add1
      (implies (not (in-book? nm bk))
               (equal (find-phone nm (add-phone nm pnum bk))
                      pnum)))

    (defthm find-add2
      (equal (find-phone nm (add-phone nm pnum bk))
             (if (in-book? nm bk)
                 (find-phone nm bk)
                 pnum)))

  ~ev[]
  For technical reasons, lemmas such as ~c[find-add2], i.e., which do
  not have hypotheses, are usually slightly preferable.  This lemma is
  stored as an ``unconditional'' ~il[rewrite] rule (i.e., has no
  hypotheses) and, therefore, will apply more often than ~c[find-add1].
  However, for our current purposes either version is all right.

  Property 3 says: If we change a number, we'll find the change.  This
  is very similar to the previous example.  The formalization is as
  follows.
  ~bv[]

    (defthm find-change
      (equal (find-phone nm (change-phone nm pnum bk))
             (if (in-book? nm bk)
                 pnum
               (find-phone nm bk))))

  ~ev[]
  Property 4 says: changing and then deleting a number is the same as
  just deleting.  We can model this as follows.
  ~bv[]

    (defthm del-change
      (equal (del-phone nm (change-phone nm pnum bk))
             (del-phone nm bk)))

  ~ev[]
  Unfortunately, when we try to prove this, we encounter subgoals that
  seem to be true, but for which the prover is stumped.  For example,
  consider the following goal.  (Note:  ~c[endp] holds of lists that
  are empty.)
  ~bv[]

    Subgoal *1/4
    (IMPLIES (AND (NOT (ENDP BK))
                  (NOT (EQUAL NM (CAAR BK)))
                  (NOT (BOUND? NM (CDR BK)))
                  (BOUND? NM BK))
             (EQUAL (REMBIND NM (BIND NM PNUM BK))
                    (REMBIND NM BK))).

  ~ev[]
  Our intuition about ~c[rembind] and ~c[bind] tells us that this goal
  should be true even without the hypotheses.  We attempt to prove the 
  following lemma.
  ~bv[]

    (defthm rembind-bind 
      (equal (rembind nm (bind nm pnum bk))
             (rembind nm bk)))

  ~ev[]
  The prover proves this by induction, and stores it as a rewrite
  rule.  After that, the prover has no difficulty in proving
  ~c[del-change].

  The need to prove lemma ~c[rembind-bind] illustrates a point we made
  early in this example:  the collection of ~il[rewrite] rules
  supplied by a previously certified book will almost never be
  everything you'll need.  It would be nice if we could operate purely
  in the realm of names, phone numbers, and phone books without ever
  having to prove any new facts about alists.  Unfortunately, we
  needed a fact about the relation between ~c[rembind] and ~c[bind] that
  wasn't supplied with the alists theory.  Hopefully, such omissions
  will be rare.

  Finally, let's consider our property 5 above:  a name will not be in
  the book after we delete it.  We formalize this as follows:
  ~bv[]

    (defthm in-book-del
      (not (in-book? nm (del-phone nm bk))))

  ~ev[]
  This proves easily.  But notice that it's only true because
  ~c[del-phone] (actually ~c[rembind]) removes ~st[all] occurrences of a
  name from the phone book.  If it only removed, say, the first one it
  encountered, we'd need a hypothesis that said that ~c[nm] occurs at
  most once in ~c[bk].  Ah, maybe that's a property you hadn't
  considered.  Maybe you want to ensure that ~st[any] name occurs at
  most once in any valid phonebook.

  To complete this example, let's consider adding an ~st[invariant] to
  our specification.  In particular, suppose we want to assure that no
  client has more than one associated phone number.  One way to ensure
  this is to require that the domain of the alist is a ``set'' (has no
  duplicates).
  ~bv[]

    (defun setp (l)
      (if (atom l)
          (null l)
        (and (not (member-equal (car l) (cdr l)))
             (setp (cdr l)))))

    (defun valid-phonebookp (bk)
      (and (phonebookp bk)
           (setp (domain bk))))

  ~ev[]
  Now, we want to show under what conditions our operations preserve
  the property of being a ~c[valid-phonebookp].  The operations
  ~c[in-book?]  and ~c[find-phone] don't return a phone book, so we
  don't really need to worry about them.  Since we're really
  interested in the ``types'' of values preserved by our phonebook
  functions, let's look at the types of those operations as well.
  ~bv[]

    (defthm in-book-booleanp
      (booleanp (in-book? nm bk)))

    (defthm in-book-namep
      (implies (and (phonebookp bk)
                    (in-book? nm bk))
               (namep nm))
      :hints ((\"Goal\" :in-theory (enable bound?))))

    (defthm find-phone-pnump
      (implies (and (phonebookp bk)
                    (in-book? nm bk))
               (pnump (find-phone nm bk)))
      :hints ((\"Goal\" :in-theory (enable bound? binding))))

  ~ev[]
  Note the ``~c[:]~ilc[hints]'' on the last two lemmas.  Neither of these
  would prove without these ~il[hints], because once again there are
  some facts about ~c[bound?] and ~c[binding] not available in our
  current context.  Now, we could figure out what those facts are and
  try to prove them.  Alternatively, we can ~il[enable] ~c[bound?] and
  ~c[binding] and hope that by opening up these functions, the
  conjectures will reduce to versions that the prover does know enough
  about or can prove by induction.  In this case, this strategy works.
  The hints tell the prover to ~il[enable] the functions in question
  when considering the designated goal.

  Below we develop the theorems showing that ~c[add-phone],
  ~c[change-phone], and ~c[del-phone] preserve our proposed invariant.
  Notice that along the way we have to prove some subsidiary facts,
  some of which are pretty ugly.  It would be a good idea for you to
  try, say, ~c[add-phone-preserves-invariant] without introducing the
  following four lemmas first.  See if you can develop the proof and
  only add these lemmas as you need assistance.  Then try
  ~c[change-phone-preserves-invariant] and ~c[del-phone-preserves-invariant].
  They will be easier.  It is illuminating to think about why
  ~c[del-phone-preserves-invariant] does not need any ``type''
  hypotheses.
  ~bv[]

    (defthm bind-preserves-phonebookp
      (implies (and (phonebookp bk)
                    (namep nm)
                    (pnump num))
               (phonebookp (bind nm num bk))))
    
    (defthm member-equal-strip-cars-bind 
      (implies (and (not (equal x y))
                    (not (member-equal x (strip-cars a))))
               (not (member-equal x (strip-cars (bind y z a))))))
    
    (defthm bind-preserves-domain-setp 
      (implies (and (alistp bk)
                    (setp (domain bk)))
               (setp (domain (bind nm num bk))))
      :hints ((\"Goal\" :in-theory (enable domain))))
    
    (defthm phonebookp-alistp
      (implies (phonebookp bk)
               (alistp bk)))
    
    (defthm ADD-PHONE-PRESERVES-INVARIANT
      (implies (and (valid-phonebookp bk)
                    (namep nm)
                    (pnump num))
               (valid-phonebookp (add-phone nm num bk)))
      :hints ((\"Goal\" :in-theory (disable domain-bind))))
    
    (defthm CHANGE-PHONE-PRESERVES-INVARIANT
      (implies (and (valid-phonebookp bk)
                    (namep nm)
                    (pnump num))
               (valid-phonebookp (change-phone nm num bk)))
      :hints ((\"Goal\" :in-theory (disable domain-bind))))
    
    (defthm remove-equal-preserves-setp
      (implies (setp l)
               (setp (remove-equal x l))))
    
    (defthm rembind-preserves-phonebookp 
      (implies (phonebookp bk)
               (phonebookp (rembind nm bk))))
    
    (defthm DEL-PHONE-PRESERVES-INVARIANT
      (implies (valid-phonebookp bk)
               (valid-phonebookp (del-phone nm bk))))
  ~ev[]

  As a final test of your understanding, try to formulate and prove an
  invariant that says that no phone number is assigned to more than
  one name.  The following hints may help.
  ~bq[]

  1. Define the appropriate invariant.  (Hint: remember the function
  ~c[range].)

  2. Do our current definitions of ~c[add-phone] and ~c[change-phone]
  necessarily preserve this property?  If not, consider what
  hypotheses are necessary in order to guarantee that they do preserve
  this property.

  3. Study the definition of the function ~c[range] and notice that it
  is defined in terms of the function ~ilc[strip-cdrs].  Understand how
  this defines the range of an alist.

  4. Formulate the correctness theorems and attempt to prove them.
  You'll probably benefit from studying the invariant proof above.  In
  particular, you may need some fact about the function ~ilc[strip-cdrs]
  analogous to the lemma ~c[member-equal-strip-cars-bind] above.

  ~eq[]

  Below is one solution to this exercise.  Don't look at the solution,
  however, until you've struggled a bit with it.  Notice that we
  didn't actually change the definitions of ~c[add-phone] and
  ~c[change-phone], but added a hypothesis saying that the number is
  ``new.''  We could have changed the definitions to check this and
  return the phonebook unchanged if the number was already in use.
  ~bv[]

    (defun pnums-in-use (bk)
      (range bk))
    
    (defun phonenums-unique (bk)
      (setp (pnums-in-use bk)))
    
    (defun new-pnump (pnum bk)
      (not (member-equal pnum (pnums-in-use bk))))
    
    (defthm member-equal-strip-cdrs-rembind
      (implies (not (member-equal x (strip-cdrs y)))
               (not (member-equal x (strip-cdrs (rembind z y))))))
    
    (defthm DEL-PHONE-PRESERVES-PHONENUMS-UNIQUE
      (implies (phonenums-unique bk)
               (phonenums-unique (del-phone nm bk)))
      :hints ((\"Goal\" :in-theory (enable range))))
    
    (defthm strip-cdrs-bind-non-member
      (implies (and (not (bound? x a))
                    (alistp a))
               (equal (strip-cdrs (bind x y a))
                      (append (strip-cdrs a) (list y))))
      :hints ((\"Goal\" :in-theory (enable bound?))))
    
    (defthm setp-append-list 
      (implies (setp l)
               (equal (setp (append l (list x)))
                      (not (member-equal x l)))))
    
    (defthm ADD-PHONE-PRESERVES-PHONENUMS-UNIQUE
      (implies (and (phonenums-unique bk)
                    (new-pnump pnum bk)
                    (alistp bk))
               (phonenums-unique (add-phone nm pnum bk)))
      :hints ((\"Goal\" :in-theory (enable range))))
    
    (defthm member-equal-strip-cdrs-bind
      (implies (and (not (member-equal z (strip-cdrs a)))
                    (not (equal z y)))
               (not (member-equal z (strip-cdrs (bind x y a))))))
    
    (defthm CHANGE-PHONE-PRESERVES-PHONENUMS-UNIQUE
      (implies (and (phonenums-unique bk)
                    (new-pnump pnum bk)
                    (alistp bk))
               (phonenums-unique (change-phone nm pnum bk)))
      :hints ((\"Goal\" :in-theory (enable range))))
  ~ev[]
  ")

(deflabel Tutorial4-Defun-Sk-Example
  :doc
  ":Doc-Section Tutorial-examples

  example of quantified notions~/

  This example illustrates the use of ~ilc[defun-sk] and ~ilc[defthm]
  ~il[events] to reason about quantifiers.  ~l[defun-sk].

  Many users prefer to avoid the use of quantifiers, since ACL2
  provides only very limited support for reasoning about
  quantifiers.~/

  Here is a list of ~il[events] that proves that if there are arbitrarily
  large numbers satisfying the disjunction ~c[(OR P R)], then either
  there are arbitrarily large numbers satisfying ~c[P] or there are
  arbitrarily large numbers satisfying ~c[R].
  ~bv[]

  ; Introduce undefined predicates p and r.
  (defstub p (x) t)
  (defstub r (x) t)

  ; Define the notion that something bigger than x satisfies p.
  (defun-sk some-bigger-p (x)
    (exists y (and (< x y) (p y))))

  ; Define the notion that something bigger than x satisfies r.
  (defun-sk some-bigger-r (x)
    (exists y (and (< x y) (r y))))

  ; Define the notion that arbitrarily large x satisfy p.
  (defun-sk arb-lg-p ()
    (forall x (some-bigger-p x)))

  ; Define the notion that arbitrarily large x satisfy r.
  (defun-sk arb-lg-r ()
    (forall x (some-bigger-r x)))

  ; Define the notion that something bigger than x satisfies p or r.
  (defun-sk some-bigger-p-or-r (x)
    (exists y (and (< x y) (or (p y) (r y)))))

  ; Define the notion that arbitrarily large x satisfy p or r.
  (defun-sk arb-lg-p-or-r ()
    (forall x (some-bigger-p-or-r x)))

  ; Prove the theorem promised above.  Notice that the functions open
  ; automatically, but that we have to provide help for some rewrite
  ; rules because they have free variables in the hypotheses.  The
  ; ``witness functions'' mentioned below were introduced by DEFUN-SK.

  (thm
   (implies (arb-lg-p-or-r)
            (or (arb-lg-p)
                (arb-lg-r)))
   :hints ((\"Goal\"
            :use
            ((:instance some-bigger-p-suff
                        (x (arb-lg-p-witness))
                        (y (some-bigger-p-or-r-witness 
                            (max (arb-lg-p-witness)
                                 (arb-lg-r-witness)))))
             (:instance some-bigger-r-suff
                        (x (arb-lg-r-witness))
                        (y (some-bigger-p-or-r-witness 
                            (max (arb-lg-p-witness)
                                 (arb-lg-r-witness)))))
             (:instance arb-lg-p-or-r-necc
                        (x (max (arb-lg-p-witness)
                                (arb-lg-r-witness))))))))

  ; And finally, here's a cute little example.  We have already
  ; defined above the notion (some-bigger-p x), which says that
  ; something bigger than x satisfies p.  Let us introduce a notion
  ; that asserts that there exists both y and z bigger than x which
  ; satisfy p.  On first glance this new notion may appear to be
  ; stronger than the old one, but careful inspection shows that y and
  ; z do not have to be distinct.  In fact ACL2 realizes this, and
  ; proves the theorem below automatically.

  (defun-sk two-bigger-p (x)
    (exists (y z) (and (< x y) (p y) (< x z) (p z))))

  (thm (implies (some-bigger-p x) (two-bigger-p x)))

  ; A technical point:  ACL2 fails to prove the theorem above
  ; automatically if we take its contrapositive, unless we disable
  ; two-bigger-p as shown below.  That is because ACL2 needs to expand
  ; some-bigger-p before applying the rewrite rule introduced for
  ; two-bigger-p, which contains free variables.  The moral of the
  ; story is:  Don't expect too much automatic support from ACL2 for
  ; reasoning about quantified notions.

  (thm (implies (not (two-bigger-p x)) (not (some-bigger-p x)))
       :hints ((\"Goal\" :in-theory (disable two-bigger-p))))
  ~ev[]
  ")

(deflabel Tutorial5-Miscellaneous-Examples
  :doc
  ":Doc-Section Tutorial-examples

  miscellaneous ACL2 examples~/

  The following examples are more advanced examples of usage of ACL2.
  They are included largely for reference, in case someone
  finds them useful.~/~/")

(deflabel file-reading-example
  :doc
  ":Doc-Section  Tutorial5-Miscellaneous-Examples

  example of reading files in ACL2~/

  This example illustrates the use of ACL2's ~il[IO] primitives to read the
  forms in a file.  ~l[io].~/

  This example provides a solution to the following problem.  Let's
  say that you have a file that contains s-expressions.  Suppose that
  you want to build a list by starting with ~c[nil], and updating it
  ``appropriately'' upon encountering each successive s-expression in
  the file.  That is, suppose that you have written a function
  ~c[update-list] such that ~c[(update-list obj current-list)] returns
  the list obtained by ``updating'' ~c[current-list] with the next
  object, ~c[obj], encountered in the file.  The top-level function for
  processing such a file, returning the final list, could be defined
  as follows.  Notice that because it opens a channel to the given
  file, this function modifies ~il[state] and hence must return ~il[state].
  Thus it actually returns two values:  the final list and the new
  ~il[state].
  ~bv[]

    (defun process-file (filename state)
      (mv-let
       (channel state)
       (open-input-channel filename :object state)
       (mv-let (result state)
               (process-file1 nil channel state) ;see below
               (let ((state (close-input-channel channel state)))
                 (mv result state)))))

  ~ev[]
  The function ~c[process-file1] referred to above takes the currently
  constructed list (initially, ~c[nil]), together with a channel to the
  file being read and the ~il[state], and returns the final updated
  list.  Notice that this function is tail recursive.  This is
  important because many Lisp compilers will remove tail recursion,
  thus avoiding the potential for stack overflows when the file
  contains a large number of forms.
  ~bv[]

    (defun process-file1 (current-list channel state)
      (mv-let (eofp obj state)
              (read-object channel state)
              (cond
               (eofp (mv current-list state))
               (t (process-file1 (update-list obj current-list)
                                 channel state)))))

  ~ev[]
  ")

(deflabel guard-example
  :doc
  ":Doc-Section Tutorial5-Miscellaneous-Examples

  a brief transcript illustrating ~il[guard]s in ACL2~/

  This note addresses the question:  what is the use of ~il[guard]s in
  ACL2?  Although we recommend that beginners try to avoid ~il[guard]s for
  a while, we hope that the summary here is reasonably self-contained
  and will provide a reasonable introduction to guards in ACL2.  For a
  more systematic discussion, ~pl[guard].  For a summary of that
  topic, ~pl[guard-quick-reference].

  Before we get into the issue of ~il[guard]s, let us note that there are
  two important ``modes'':

  ~il[defun-mode] ~-[] ``Does this ~il[defun] add an axiom (`:logic mode') or not
  (`:program mode')?''  (~l[defun-mode].)  Only ~c[:]~ilc[logic] mode
  functions can have their ``~il[guard]s verified'' via mechanized proof;
  ~pl[verify-guards].

  ~ilc[set-guard-checking] ~-[] ``Should runtime ~il[guard] violations signal an
  error (~c[:all], and usually with ~c[t] or ~c[:nowarn]) or go undetected
  (~c[nil], ~c[:none])?  Equivalently, are expressions evaluated in Common Lisp
  or in the logic?''  (~l[set-guard-checking].)~/

  ~em[Prompt examples]

  Here some examples of the relation between the ACL2 ~il[prompt] and the
  ``modes'' discussed above.  Also ~pl[default-print-prompt].  The
  first examples all have ~c[ld-skip-proofsp nil]; that is, proofs are
  ~em[not] skipped.
  ~bv[]

    ACL2 !>    ; logic mode with guard checking on
    ACL2 >     ; logic mode with guard checking off
    ACL2 p!>   ; program mode with guard checking on
    ACL2 p>    ; program mode with guard checking off

  ~ev[]
  Here are some examples with ~ilc[default-defun-mode] of ~c[:]~ilc[logic].
  ~bv[]

    ACL2 >     ; guard checking off, ld-skip-proofsp nil
    ACL2 s>    ; guard checking off, ld-skip-proofsp t
    ACL2 !>    ; guard checking on, ld-skip-proofsp nil
    ACL2 !s>   ; guard checking on, ld-skip-proofsp t

  ~ev[]

  ~em[Sample session]

  ~bv[]
  ACL2 !>(+ 'abc 3)

  ACL2 Error in TOP-LEVEL: The guard for the function symbol
  BINARY-+, which is (AND (ACL2-NUMBERP X) (ACL2-NUMBERP Y)),
  is violated by the arguments in the call (+ 'ABC 3).

  ACL2 !>:set-guard-checking nil
  ;;;; verbose output omitted here
  ACL2 >(+ 'abc 3)
  3
  ACL2 >(< 'abc 3)
  T
  ACL2 >(< 3 'abc)
  NIL
  ACL2 >(< -3 'abc)
  T
  ACL2 >:set-guard-checking t

  Turning guard checking on, value T.

  ACL2 !>(defun sum-list (x)
          (declare (xargs :guard (integer-listp x)
                          :verify-guards nil))
          (cond ((endp x) 0)
                (t (+ (car x) (sum-list (cdr x))))))

  The admission of SUM-LIST is trivial, using the relation
  O< (which is known to be well-founded on the domain
  recognized by O-P) and the measure (ACL2-COUNT X).
  We observe that the type of SUM-LIST is described by the
  theorem (ACL2-NUMBERP (SUM-LIST X)).  We used primitive type
  reasoning.

  Summary
  Form:  ( DEFUN SUM-LIST ...)
  Rules: ((:FAKE-RUNE-FOR-TYPE-SET NIL))
  Warnings:  None
  Time:  0.03 seconds
     (prove: 0.00, print: 0.00, proof tree: 0.00, other: 0.03)
   SUM-LIST
  ACL2 !>(sum-list '(1 2 3))

  ACL2 Warning [Guards] in TOP-LEVEL:  Guard-checking will be inhibited
  on recursive calls of the executable counterpart (i.e., in the ACL2
  logic) of SUM-LIST.  To check guards on all recursive calls:
    (set-guard-checking :all)
  To leave behavior unchanged except for inhibiting this message:
    (set-guard-checking :nowarn)

  6
  ACL2 !>(sum-list '(1 2 abc 3))

  ACL2 Error in TOP-LEVEL: The guard for the function symbol
  BINARY-+, which is (AND (ACL2-NUMBERP X) (ACL2-NUMBERP Y)),
  is violated by the arguments in the call (+ 'ABC 3).

  ACL2 !>:set-guard-checking nil
  ;;;; verbose output omitted here
  ACL2 >(sum-list '(1 2 abc 3))
  6
  ACL2 >(defthm sum-list-append
         (equal (sum-list (append a b))
                (+ (sum-list a) (sum-list b))))

  << Starting proof tree logging >>

  Name the formula above *1.

  Perhaps we can prove *1 by induction.  Three induction
  schemes are suggested by this conjecture.  Subsumption
  reduces that number to two.  However, one of these is flawed
  and so we are left with one viable candidate.

  ...

  That completes the proof of *1.

  Q.E.D.
  ~ev[]

  ~em[Guard verification vs. defun]

  ~bv[]

        Declare Form                        Guards Verified?

    (declare (xargs :mode :program ...))          no
    (declare (xargs :guard g))                    yes
    (declare (xargs :guard g :verify-guards nil)) no
    (declare (xargs ...<no :guard>...))           no

  ACL2 >:pe sum-list
   l        8  (DEFUN SUM-LIST (X)
                (DECLARE (XARGS :GUARD (INTEGER-LISTP X)
                                :VERIFY-GUARDS NIL))
                (COND ((ENDP X) 0)
                      (T (+ (CAR X) (SUM-LIST (CDR X))))))
  ACL2 >(verify-guards sum-list)
  The non-trivial part of the guard conjecture for SUM-LIST,
  given the :type-prescription rule SUM-LIST, is

  Goal
  (AND (IMPLIES (AND (INTEGER-LISTP X) (NOT (CONSP X)))
                (EQUAL X NIL))
       (IMPLIES (AND (INTEGER-LISTP X) (NOT (ENDP X)))
                (INTEGER-LISTP (CDR X)))
       (IMPLIES (AND (INTEGER-LISTP X) (NOT (ENDP X)))
                (ACL2-NUMBERP (CAR X)))).

  ...

  ACL2 >:pe sum-list
   lv       8  (DEFUN SUM-LIST (X)
                (DECLARE (XARGS :GUARD (INTEGER-LISTP X)
                                :VERIFY-GUARDS NIL))
  ACL2 >:set-guard-checking t
  
  Turning guard checking on, value T.
  
  ACL2 !>(sum-list '(1 2 abc 3))

  ACL2 Error in TOP-LEVEL: The guard for the function symbol
  SUM-LIST, which is (INTEGER-LISTP X), is violated by the
  arguments in the call (SUM-LIST '(1 2 ABC ...)).  See :DOC wet
  for how you might be able to get an error backtrace.

  ACL2 !>:set-guard-checking nil
  ;;;; verbose output omitted here
  ACL2 >(sum-list '(1 2 abc 3))
  6
  ACL2 >:comp sum-list
  Compiling gazonk0.lsp.
  End of Pass 1.
  End of Pass 2.
  Finished compiling gazonk0.lsp.
  Loading gazonk0.o
  start address -T 1bbf0b4 Finished loading gazonk0.o
  Compiling gazonk0.lsp.
  End of Pass 1.
  End of Pass 2.
  Finished compiling gazonk0.lsp.
  Loading gazonk0.o
  start address -T 1bc4408 Finished loading gazonk0.o
   SUM-LIST
  ACL2 >:q

  Exiting the ACL2 read-eval-print loop.
  ACL2>(trace sum-list)
  (SUM-LIST)

  ACL2>(lp)

  ACL2 Version 1.8.  Level 1.  Cbd \"/slocal/src/acl2/v1-9/\".
  Type :help for help.
  ACL2 >(sum-list '(1 2 abc 3))
  6
  ACL2 >(sum-list '(1 2 3))
    1> (SUM-LIST (1 2 3))>
      2> (SUM-LIST (2 3))>
        3> (SUM-LIST (3))>
          4> (SUM-LIST NIL)>
          <4 (SUM-LIST 0)>
        <3 (SUM-LIST 3)>
      <2 (SUM-LIST 5)>
    <1 (SUM-LIST 6)>
  6
  ACL2 >:pe sum-list-append
            9  (DEFTHM SUM-LIST-APPEND
                       (EQUAL (SUM-LIST (APPEND A B))
                              (+ (SUM-LIST A) (SUM-LIST B))))
  ACL2 >(verify-guards sum-list-append)

  The non-trivial part of the guard conjecture for
  SUM-LIST-APPEND, given the :type-prescription rule SUM-LIST,
  is

  Goal
  (AND (TRUE-LISTP A)
       (INTEGER-LISTP (APPEND A B))
       (INTEGER-LISTP A)
       (INTEGER-LISTP B)).

  ...

  ****** FAILED ******* See :DOC failure ****** FAILED ******
  ACL2 >(defthm common-lisp-sum-list-append
           (if (and (integer-listp a)
                    (integer-listp b))
               (equal (sum-list (append a b))
                      (+ (sum-list a) (sum-list b)))
               t)
           :rule-classes nil)

  << Starting proof tree logging >>

  By the simple :rewrite rule SUM-LIST-APPEND we reduce the
  conjecture to

  Goal'
  (IMPLIES (AND (INTEGER-LISTP A)
                (INTEGER-LISTP B))
           (EQUAL (+ (SUM-LIST A) (SUM-LIST B))
                  (+ (SUM-LIST A) (SUM-LIST B)))).

  But we reduce the conjecture to T, by primitive type
  reasoning.

  Q.E.D.
  ;;;; summary omitted here
  ACL2 >(verify-guards common-lisp-sum-list-append)

  The non-trivial part of the guard conjecture for
  COMMON-LISP-SUM-LIST-APPEND, given the :type-prescription
  rule SUM-LIST, is

  Goal
  (AND (IMPLIES (AND (INTEGER-LISTP A)
                     (INTEGER-LISTP B))
                (TRUE-LISTP A))
       (IMPLIES (AND (INTEGER-LISTP A)
                     (INTEGER-LISTP B))
                (INTEGER-LISTP (APPEND A B)))).

  ...

  Q.E.D.

  That completes the proof of the guard theorem for
  COMMON-LISP-SUM-LIST-APPEND.  COMMON-LISP-SUM-LIST-APPEND
  is compliant with Common Lisp.
  ;;;; Summary omitted here.
  ACL2 >(defthm foo (consp (mv x y)))

  ...

  Q.E.D.

  ~ev[]
  ~bv[]
  ACL2 >(verify-guards foo)

  ACL2 Error in (VERIFY-GUARDS FOO): The number of values we
  need to return is 1 but the number of values returned by the
  call (MV X Y) is 2.

  > (CONSP (MV X Y))


  ACL2 Error in (VERIFY-GUARDS FOO): The guards for FOO cannot
  be verified because the theorem has the wrong syntactic
  form.  See :DOC verify-guards.
  ~ev[]~/

  :cited-by guard")

(deflabel mutual-recursion-proof-example
  :doc
  ":Doc-Section Tutorial5-Miscellaneous-Examples

  a small proof about mutually recursive functions~/

  Sometimes one wants to reason about mutually recursive functions.
  Although this is possible in ACL2, it can be a bit awkward.  This
  example is intended to give some ideas about how one can go about
  such proofs.

  For an introduction to mutual recursion in ACL2,
  ~pl[mutual-recursion].~/

  We begin by defining two mutually recursive functions:  one that
  collects the variables from a ~il[term], the other that collects the
  variables from a list of ~il[term]s.  We actually imagine the ~il[term]
  argument to be a ~ilc[pseudo-termp]; ~pl[pseudo-termp].
  ~bv[]
  (mutual-recursion

  (defun free-vars1 (term ans)
    (cond ((atom term)
           (add-to-set-eq term ans))
          ((fquotep term) ans)
          (t (free-vars1-lst (fargs term) ans))))

  (defun free-vars1-lst (lst ans)
    (cond ((atom lst) ans)
          (t (free-vars1-lst (cdr lst)
                             (free-vars1 (car lst) ans)))))

  )
  ~ev[]
  The idea of the following function is that it suggests a proof by
  induction in two cases, according to the top-level ~ilc[if] structure of
  the body.  In one case, ~c[(atom x)] is true, and the theorem to be
  proved should be proved under no additional hypotheses except for
  ~c[(atom x)].  In the other case, ~c[(not (atom x))] is assumed together
  with three instances of the theorem to be proved, one for each
  recursive call in this case.  So, one instance substitutes ~c[(car x)]
  for ~c[x]; one substitutes ~c[(cdr x)] for ~c[x]; and the third substitutes
  ~c[(cdr x)] for ~c[x] and ~c[(free-vars1 (car x) ans)] for ~c[ans].  If you think
  about how you would go about a hand proof of the theorem to follow,
  you'll come up with a similar scheme.
  ~bv[]
  (defun symbol-listp-free-vars1-induction (x ans)
    (if (atom x)
  ; then we just make sure x and ans aren't considered irrelevant
        (list x ans)
      (list (symbol-listp-free-vars1-induction (car x) ans)
            (symbol-listp-free-vars1-induction (cdr x) ans)
            (symbol-listp-free-vars1-induction
             (cdr x)
             (free-vars1 (car x) ans)))))
  ~ev[]
  We now prove the two theorems together as a conjunction, because the
  inductive hypotheses for one are sometimes needed in the proof of
  the other (even when you do this proof on paper!).
  ~bv[]
  (defthm symbol-listp-free-vars1
    (and (implies (and (pseudo-termp x)
                       (symbol-listp ans))
                  (symbol-listp (free-vars1 x ans)))
         (implies (and (pseudo-term-listp x)
                       (symbol-listp ans))
                  (symbol-listp (free-vars1-lst x ans))))
    :hints
    ((\"Goal\" :induct (symbol-listp-free-vars1-induction x ans))))
  ~ev[]
  The above works, but let's try for a more efficient proof, which
  avoids cluttering the proof with irrelevant (false) inductive
  hypotheses.
  ~bv[]
  (ubt 'symbol-listp-free-vars1-induction)

  (defun symbol-listp-free-vars1-induction (flg x ans)

  ; Flg is non-nil (or t) if we are ``thinking'' of a single term.

    (if (atom x)
        (list x ans)
      (if flg
          (symbol-listp-free-vars1-induction nil (cdr x) ans)
        (list (symbol-listp-free-vars1-induction t (car x) ans)
              (symbol-listp-free-vars1-induction
               nil
               (cdr x)
               (free-vars1 (car x) ans))))))
  ~ev[]
  We now state the theorem as a conditional, so that it can be proved
  nicely using the ~il[induction] scheme that we have just coded.  The
  prover will not store an ~ilc[if] ~il[term] as a ~il[rewrite] rule, but that's OK
  (as long as we tell it not to try), because we're going to derive
  the corollaries of interest later and make ~st[them] into ~il[rewrite]
  rules.
  ~bv[]
  (defthm symbol-listp-free-vars1-flg
    (if flg
        (implies (and (pseudo-termp x)
                      (symbol-listp ans))
                 (symbol-listp (free-vars1 x ans)))
      (implies (and (pseudo-term-listp x)
                    (symbol-listp ans))
               (symbol-listp (free-vars1-lst x ans))))
    :hints
    ((\"Goal\" :induct (symbol-listp-free-vars1-induction flg x ans)))
    :rule-classes nil)
  ~ev[]
  And finally, we may derive the theorems we are interested in as
  immediate corollaries.
  ~bv[]
  (defthm symbol-listp-free-vars1
    (implies (and (pseudo-termp x)
                  (symbol-listp ans))
             (symbol-listp (free-vars1 x ans)))
    :hints ((\"Goal\" :by (:instance symbol-listp-free-vars1-flg
                                   (flg t)))))

  (defthm symbol-listp-free-vars1-lst
    (implies (and (pseudo-term-listp x)
                  (symbol-listp ans))
             (symbol-listp (free-vars1-lst x ans)))
    :hints ((\"Goal\" :by (:instance symbol-listp-free-vars1-flg
                                   (flg nil)))))
    ~ev[]
  ")

(deflabel functional-instantiation-example
  :doc
  ":Doc-Section Tutorial5-Miscellaneous-Examples

  a small proof demonstrating functional instantiation~/

  The example below demonstrates the use of functional instantiation,
  that is, the use of a generic result in proving a result about
  specific functions.  In this example we constrain a function to be
  associative and commutative, with an identity or ``root,'' on a
  given domain.  Next, we define a corresponding function that applies
  the constrained associative-commutative function to successive
  elements of a list.  We then prove that the latter function gives
  the same value when we first reverse the elements of the list.
  Finally, we use functional instantiation to derive the corresponding
  result for the function that multiplies successive elements of a
  list.

  Also ~pl[constraint] for more about ~c[:functional-instance] and
  ~pl[lemma-instance] for general information about the use of
  previously-proved lemmas.~/

  ~bv[]
  (in-package \"ACL2\")

  (encapsulate
   (((ac-fn * *) => *)
    ((ac-fn-domain *) => *)
    ((ac-fn-root) => *))
   (local (defun ac-fn (x y) (+ x y)))
   (local (defun ac-fn-root () 0))
   (local (defun ac-fn-domain (x) (acl2-numberp x)))
   (defthm ac-fn-comm
     (equal (ac-fn x y)
            (ac-fn y x)))
   (defthm ac-fn-assoc
     (equal (ac-fn (ac-fn x y) z)
            (ac-fn x (ac-fn y z))))
   (defthm ac-fn-id
     (implies (ac-fn-domain x)
              (equal (ac-fn (ac-fn-root) x)
                     x)))
   (defthm ac-fn-closed
     (and (ac-fn-domain (ac-fn x y))
          (ac-fn-domain (ac-fn-root)))))

  (defun ac-fn-list (x)
    (if (atom x)
        (ac-fn-root)
      (ac-fn (car x)
             (ac-fn-list (cdr x)))))

  (in-theory (disable (ac-fn-list)))

  (defun ac-fn-domain-list (x)
    (if (atom x)
        t
      (and (ac-fn-domain (car x))
           (ac-fn-domain-list (cdr x)))))

  (defun rev (x)
    (if (atom x)
        nil
      (append (rev (cdr x))
              (list (car x)))))

  (defthm ac-fn-list-closed
     (ac-fn-domain (ac-fn-list x)))

  (defthm ac-fn-list-append
    (implies (and (ac-fn-domain-list x)
                  (ac-fn-domain-list y))
             (equal (ac-fn-list (append x y))
                    (ac-fn (ac-fn-list x)
                           (ac-fn-list y)))))

  (defthm ac-fn-domain-list-rev
    (equal (ac-fn-domain-list (rev x))
           (ac-fn-domain-list x)))

  (defthm ac-fn-list-rev
    (implies (ac-fn-domain-list x)
             (equal (ac-fn-list (rev x))
                    (ac-fn-list x))))

  (defun times-list (x)
    (if (atom x)
        1
      (* (car x)
         (times-list (cdr x)))))

  (defun acl2-number-listp (x)
    (if (atom x)
        t
      (and (acl2-numberp (car x))
           (acl2-number-listp (cdr x)))))

  (defthm times-list-rev
    (implies (acl2-number-listp x)
             (equal (times-list (rev x))
                    (times-list x)))
    :hints ((\"Goal\"
             :use
             ((:functional-instance
               ac-fn-list-rev
               ;; Instantiate the generic functions:
               (ac-fn (lambda (x y) (* x y)))
               (ac-fn-root (lambda () 1))
               (ac-fn-domain acl2-numberp)
               ;; Instantiate the other relevant functions:
               (ac-fn-list times-list)
               (ac-fn-domain-list acl2-number-listp))))))
  ~ev[]~/")

(deflabel Startup
  :doc
  ":Doc-Section ACL2-Tutorial

  How to start using ACL2; the ACL2 ~il[command] loop~/~/

  When you start up ACL2, you'll probably find yourself inside the
  ACL2 ~il[command] loop, as indicated by the following ~il[prompt].
  ~bv[]

    ACL2 !>

  ~ev[]
  If not, you should type ~c[(LP)].  ~l[lp], which has a lot more
  information about the ACL2 ~il[command] loop.

  You should now be in ACL2.  The current ``~il[default-defun-mode]'' is
  ~c[:]~ilc[logic]; the other mode is ~c[:]~ilc[program], which would cause the letter ~c[p]
  to be printed in the ~il[prompt].  ~c[:]~ilc[Logic] means that any function we
  define is not only executable but also is axiomatically defined in
  the ACL2 logic.  ~l[defun-mode] and
  ~pl[default-defun-mode].  For example we can define a function
  ~c[my-cons] as follows.  (You may find it useful to start up ACL2 and
  submit this and other ~il[command]s below to the ACL2 ~il[command] loop, as we
  won't include output below.)
  ~bv[]

    ACL2 !>(defun my-cons (x y) (cons x y))

  ~ev[]
  An easy theorem may then be proved:  the ~ilc[car] of ~c[(my-cons a b)] is
  A.
  ~bv[]

    ACL2 !>(defthm car-my-cons (equal (car (my-cons a b)) a))

  ~ev[]

  You can place raw Lisp forms to evaluate at start-up into file
  ~c[acl2-init.lsp] in your home directory.  For example, if you put the
  following into ~c[acl2-init.lsp], then ACL2 will print \"HI\" when it starts
  up.
  ~bv[]
  (print \"HI\")
  ~ev[]
  But be careful; all bets are off when you submit forms to raw Lisp, so this
  capability should only be used when you are hacking or when you are setting
  some Lisp parameters (e.g., ~c[(setq si::*notify-gbc* nil)] to turn off
  garbage collection notices in GCL).

  Notice that unlike Nqthm, the theorem ~il[command] is ~ilc[defthm] rather than
  ~c[prove-lemma].  ~l[defthm], which explains (among other things)
  that the default is to turn theorems into ~il[rewrite] rules.

  Various keyword commands are available to query the ACL2 ``~il[world]'',
  or database.  For example, we may view the definition of ~c[my-cons] by
  invoking a command to print ~il[events], as follows.
  ~bv[]

    ACL2 !>:pe my-cons

  ~ev[]
  Also ~pl[pe].  We may also view all the lemmas that ~il[rewrite]
  ~il[term]s whose top function symbol is ~ilc[car] by using the following
  command, whose output will refer to the lemma ~c[car-my-cons] proved
  above.
  ~bv[]

    ACL2 !>:pl car

  ~ev[]
  Also ~pl[pl].  Finally, we may print all the ~il[command]s back
  through the initial ~il[world] as follows.
  ~bv[]

    ACL2 !>:pbt 0

  ~ev[]
  ~l[history] for a list of commands, including these, for
  viewing the current ACL2 ~il[world].

  Continue with the ~il[documentation] for ~il[tutorial-examples] to
  see a simple but illustrative example in the use of ACL2 for
  reasoning about functions.~/")

(deflabel Tidbits
  :doc
  ":Doc-Section ACL2-Tutorial

  some basic hints for using ACL2~/~/

  ~l[books] for a discussion of books.  Briefly, a book is a file
  whose name ends in ``.lisp'' that contains ACL2 ~il[events];
  ~pl[events].

  ~l[history] for a list of useful commands.  Some examples:
  ~bv[]

    :pbt :here      ; print the current event
    :pbt (:here -3) ; print the last four events
    :u              ; undo the last event
    :pe append      ; print the definition of append

  ~ev[]
  ~l[documentation] to learn how to print documentation to the
  terminal.  There are also versions of the ~il[documentation] for Mosaic,
  Emacs Info, and hardcopy.

  There are quite a few kinds of rules allowed in ACL2 besides
  ~c[:]~ilc[rewrite] rules, though we hope that beginners won't usually need
  to be aware of them.  ~l[rule-classes] for details.  In
  particular, there is support for ~il[congruence] rewriting.
  ~l[rune] (``RUle NamE'') for a description of the various kinds
  of rules in the system.  Also ~pl[theories] for a description of
  how to build ~il[theories] of ~il[rune]s, which are often used in hints;
  ~pl[hints].

  A ``~il[programming] mode'' is supported; ~pl[program],
  ~pl[defun-mode], and ~pl[default-defun-mode].  It can be
  useful to prototype functions after executing the command ~c[:]~ilc[program],
  which will cause definitions to be syntaxed-checked only.

  ACL2 supports mutual recursion, though this feature is not tied into
  the automatic discovery of ~il[induction] schemas and is often not the
  best way to proceed when you expect to be reasoning about the
  functions.  ~l[defuns]; also ~pl[mutual-recursion].

  ~l[ld] for discussion of how to load files of ~il[events].  There
  are many options to ~ilc[ld], including ones to suppress proofs and to
  control output.

  The ~c[:]~ilc[otf-flg] (Onward Thru the Fog FLaG) is a useful feature that
  Nqthm users have often wished for.  It prevents the prover from
  aborting a proof attempt and inducting on the original conjecture.
  ~l[otf-flg].

  ACL2 supports redefinition and redundancy in ~il[events];
  ~pl[ld-redefinition-action] and ~pl[redundant-events].

  A ~il[proof-tree] display feature is available for use with Emacs.  This
  feature provides a view of ACL2 proofs that can be much more useful
  than reading the stream of ~il[characters] output by the theorem prover
  as its ``proof.''  ~l[proof-tree].

  An interactive feature similar to Pc-Nqthm is supported in ACL2.
  ~l[verify] and ~pl[proof-checker].

  ACL2 allows you to ~il[monitor] the use of ~il[rewrite] rules.
  ~l[break-rewrite].

  ~l[arrays] to read about applicative, fast ~il[arrays] in ACL2.

  To quit the ACL2 ~il[command] loop, or (in akcl) to return to the ACL2
  ~il[command] loop after an interrupt, type ~c[:]~ilc[q].  To continue (resume)
  after an interrupt (in akcl), type ~c[:r].  To cause an interrupt (in
  akcl under Unix (trademark of AT&T)), hit control-C (twice, if
  inside Emacs).  To exit ACL2 altogether, first type ~c[:]~ilc[q] to exit
  the ACL2 ~il[command] loop, and then exit Lisp (by typing
  ~c[(user::bye)] in akcl).

  ~l[state] to read about the von Neumannesque ACL2 ~il[state] object that
  records the ``current state'' of the ACL2 session.
  Also ~pl[@], and ~pl[assign], to learn about reading and
  setting global ~il[state] variables.

  If you want your own von Neumannesque object, e.g., a structure that
  can be ``destructively modified'' but which must be used with some
  syntactic restrictions, ~pl[stobj].~/")

(deflabel Tips
  :doc
  ":Doc-Section ACL2-Tutorial

  some hints for using the ACL2 prover~/

  We present here some tips for using ACL2 effectively.  Though this
  collection is somewhat ~em[ad hoc], we try to provide some
  organization, albeit somewhat artificial:  for example, the sections
  overlap, and no particular order is intended.  This material has
  been adapted by Bill Young from a very similar list for Nqthm that
  appeared in the conclusion of:  ``Interaction with the Boyer-Moore
  Theorem Prover: A Tutorial Study Using the Arithmetic-Geometric Mean
  Theorem,'' by Matt Kaufmann and Paolo Pecchiari, CLI Technical
  Report 100, June, 1995.  We also draw from a similar list in Chapter
  13 of ``A Computational Logic Handbook'' by R.S. Boyer and J
  S. Moore (Academic Press, 1988).  We'll refer to this as ``ACLH''
  below.

  These tips are organized roughly as follows.

  ~bq[]
  A. ACL2 Basics

  B. Strategies for creating events

  C. Dealing with failed proofs 

  D. Performance tips 

  E. Miscellaneous tips and knowledge 

  F. Some things you DON'T need to know
  ~eq[]~/

  ~em[ACL2 BASICS]

  ~st[A1. The ACL2 logic.]~nl[]
  This is a logic of total functions.  For example, if ~c[A] and ~c[B]
  are less than or equal to each other, then we need to know something
  more in order to conclude that they are equal (e.g., that they are
  numbers).  This kind of twist is important in writing definitions;
  for example, if you expect a function to return a number, you may
  want to apply the function ~ilc[fix] or some variant (e.g., ~ilc[nfix] or
  ~ilc[ifix]) in case one of the formals is to be returned as the value.

  ACL2's notion of ordinals is important on occasion in supplying
  ``measure ~il[hints]'' for the acceptance of recursive definitions.  Be
  sure that your measure is really an ordinal.  Consider the following
  example, which ACL2 fails to admit (as explained below).
  ~bv[]

    (defun cnt (name a i x)
      (declare (xargs :measure (+ 1 i)))
      (cond ((zp (+ 1 i))
             0)
            ((equal x (aref1 name a i))
             (1+ (cnt name a (1- i) x)))
            (t (cnt name a (1- i) x))))

  ~ev[]
  One might think that ~c[(+ 1 i)] is a reasonable measure, since we
  know that ~c[(+ 1 i)] is a positive integer in any recursive call of
  ~c[cnt], and positive integers are ACL2 ordinals
  (~pl[o-p]).  However, the ACL2 logic requires that the
  measure be an ordinal unconditionally, not just under the governing
  assumptions that lead to recursive calls.  An appropriate fix is to
  apply ~ilc[nfix] to ~c[(+ 1 i)], i.e., to use
  ~bv[]

    (declare (xargs :measure (nfix (+ 1 i))))

  ~ev[]
  in order to guarantee that the measure will always be an ordinal (in
  fact, a positive integer).

  ~st[A2. Simplification.]~nl[]
  The ACL2 simplifier is basically a rewriter, with some ``~il[linear]
  arithmetic'' thrown in.  One needs to understand the notion of
  conditional rewriting.  ~l[rewrite].

  ~st[A3. Parsing of rewrite rules.]~nl[]

  ACL2 parses ~il[rewrite] rules roughly as explained in ACLH, ~em[except]
  that it never creates ``unusual'' rule classes.  In ACL2, if you
  want a ~c[:]~ilc[linear] rule, for example, you must specify ~c[:]~ilc[linear] in
  the ~c[:]~ilc[rule-classes].  ~l[rule-classes], and also
  ~pl[rewrite] and ~pl[linear].

  ~st[A4. Linear arithmetic.]~nl[]
  On this subject, it should suffice to know that the prover can
  handle truths about ~ilc[+] and ~ilc[-], and that ~il[linear] rules (see above)
  are somehow ``thrown in the pot'' when the prover is doing such
  reasoning.  Perhaps it's also useful to know that ~il[linear] rules can
  have hypotheses, and that conditional rewriting is used to relieve
  those hypotheses.

  ~st[A5. Events.]~nl[]
  Over time, the expert ACL2 user will know some subtleties of its
  ~il[events].  For example, ~ilc[in-theory] ~il[events] and ~il[hints] are
  important, and they distinguish between a function and its
  executable counterpart.

  ~em[B. STRATEGIES FOR CREATING EVENTS]

  In this section, we concentrate on the use of definitions and
  ~il[rewrite] rules.  There are quite a few kinds of rules allowed in ACL2
  besides ~il[rewrite] rules, though most beginning users probably won't
  usually need to be aware of them.  ~l[rule-classes] for
  details.  In particular, there is support for ~il[congruence] rewriting.
  Also ~pl[rune] (``RUle NamE'') for a description of the various
  kinds of rules in the system.

  ~st[B1. Use high-level strategy.]~nl[]
  Decompose theorems into ``manageable'' lemmas (admittedly,
  experience helps here) that yield the main result ``easily.''  It's
  important to be able to outline non-trivial proofs by hand (or in
  your head).  In particular, avoid submitting goals to the prover
  when there's no reason to believe that the goal will be proved and
  there's no ``sense'' of how an induction argument would apply.  It
  is often a good idea to avoid induction in complicated theorems
  unless you have a reason to believe that it is appropriate.

  ~st[B2. Write elegant definitions.]~nl[]
  Try to write definitions in a reasonably modular style, especially
  recursive ones.  Think of ACL2 as a ~il[programming] language whose
  procedures are definitions and lemmas, hence we are really
  suggesting that one follow good ~il[programming] style (in order to avoid
  duplication of ``code,'' for example).

  When possible, complex functions are best written as compositions of
  simpler functions.  The theorem prover generally performs better on
  primitive recursive functions than on more complicated recursions
  (such as those using accumulating parameters).

  Avoid large non-recursive definitions which tend to lead to large
  case explosions.  If such definitions are necessary, try to prove
  all relevant facts about the definitions and then ~il[disable] them.

  Whenever possible, avoid mutual recursion if you care to prove
  anything about your functions.  The induction heuristics provide
  essentially no help with reasoning about mutually defined functions.
  Mutually recursive functions can usually be combined into a single
  function with a ``flag'' argument.  (However,
  ~pl[mutual-recursion-proof-example] for a small example of proof
  involving mutually recursive functions.)

  ~st[B3. Look for analogies.]~nl[]
  Sometimes you can easily edit sequences of lemmas into sequences of
  lemmas about analogous functions.

  ~st[B4. Write useful rewrite rules.]~nl[]
  As explained in A3 above, every ~il[rewrite] rule is a directive to the
  theorem prover, usually to replace one ~il[term] by another.  The
  directive generated is determined by the syntax of the ~ilc[defthm]
  submitted.  Never submit a ~il[rewrite] rule unless you have considered
  its interpretation as a proof directive.

  ~st[B4a.  Rewrite rules should simplify.]~nl[]
  Try to write ~il[rewrite] rules whose right-hand sides are in some sense
  ``simpler than'' (or at worst, are variants of) the left-hand sides.
  This will help to avoid infinite loops in the rewriter.

  ~st[B4b.  Avoid needlessly expensive rules.]~nl[]
  Consider a rule whose conclusion's left-hand side (or, the entire
  conclusion) is a ~il[term] such as ~c[(consp x)] that matches many ~il[term]s
  encountered by the prover.  If in addition the rule has complicated
  hypotheses, this rule could slow down the prover greatly.  Consider
  switching the conclusion and a complicated hypothesis (negating
  each) in that case.

  ~st[B4c. The ``Knuth-Bendix problem''.]~nl[]
  Be aware that left sides of ~il[rewrite] rules should match the
  ``normalized forms'', where ``normalization'' (rewriting) is inside
  out.  Be sure to avoid the use of nonrecursive function symbols on
  left sides of ~il[rewrite] rules, except when those function symbols are
  ~il[disable]d, because they tend to be expanded away before the rewriter
  would encounter an instance of the left side of the rule.  Also
  assure that subexpressions on the left hand side of a rule are in
  simplified form.

  ~st[B4d. Avoid proving useless rules.]~nl[]
  Sometimes it's tempting to prove a ~il[rewrite] rule even before you see
  how it might find application.  If the rule seems clean and
  important, and not unduly expensive, that's probably fine,
  especially if it's not too hard to prove.  But unless it's either
  part of the high-level strategy or, on the other hand, intended to
  get the prover past a particular unproved goal, it may simply waste
  your time to prove the rule, and then clutter the database of rules
  if you are successful.

  ~st[B4e. State rules as strongly as possible, usually.]~nl[]
  It's usually a good idea to state a rule in the strongest way
  possible, both by eliminating unnecessary hypotheses and by
  generalizing subexpressions to variables.

  Advanced users may choose to violate this policy on occasion, for
  example in order to avoid slowing down the prover by excessive
  attempted application of the rule.  However, it's a good rule of
  thumb to make the strongest rule possible, not only because it will
  then apply more often, but also because the rule will often be
  easier to prove (see also B6 below).  New users are sometimes
  tempted to put in extra hypotheses that have a ``type restriction''
  appearance, without realizing that the way ACL2 handles (total)
  functions generally lets it handle trivial cases easily.

  ~st[B4f. Avoid circularity.]~nl[]
  A stack overflow in a proof attempt almost always results from
  circular rewriting.  Use ~ilc[brr] to investigate the stack;
  ~pl[break-lemma].  Because of the complex heuristics, it is not
  always easy to define just when a ~il[rewrite] will cause circularity.
  See the very good discussion of this topic in ACLH.

  ~l[break-lemma] for a trick involving use of the forms ~c[brr t]
  and ~c[(cw-gstack)] for inspecting loops in the rewriter.

  ~st[B4g. Remember restrictions on permutative rules.]~nl[]
  Any rule that permutes the variables in its left hand side could
  cause circularity.  For example, the following axiom is
  automatically supplied by the system:
  ~bv[]

    (defaxiom commutativity-of-+
              (equal (+ x y) (+ y x))).

  ~ev[] 
  This would obviously lead to dangerous circular rewriting if such
  ``permutative'' rules were not governed by a further restriction.
  The restriction is that such rules will not produce a ~il[term] that
  is ``lexicographically larger than'' the original ~il[term]
  (~pl[loop-stopper]).  However, this sometimes prevents intended
  rewrites.  See Chapter 13 of ACLH for a discussion of this problem.

  ~st[B5. Conditional vs. unconditional rewrite rules.]~nl[]
  It's generally preferable to form unconditional ~il[rewrite] rules unless
  there is a danger of case explosion.  That is, rather than pairs of
  rules such as
  ~bv[]

  (implies p
           (equal term1 term2))
  ~ev[]
  and
  ~bv[]

  (implies (not p)
           (equal term1 term3))

  ~ev[]
  consider:
  ~bv[]

  (equal term1
         (if p term2 term3))

  ~ev[]
  However, sometimes this strategy can lead to case explosions: ~ilc[IF]
  ~il[term]s introduce cases in ACL2.  Use your judgment.  (On the subject
  of ~ilc[IF]: ~ilc[COND], ~ilc[CASE], ~ilc[AND], and ~ilc[OR] are macros that
  abbreviate ~ilc[IF] forms, and propositional functions such as
  ~ilc[IMPLIES] quickly expand into ~ilc[IF] ~il[term]s.)

  ~st[B6. Create elegant theorems.]~nl[]
  Try to formulate lemmas that are as simple and general as possible.
  For example, sometimes properties about several functions can be
  ``factored'' into lemmas about one function at a time.  Sometimes
  the elimination of unnecessary hypotheses makes the theorem easier
  to prove, as does generalizing first by hand.

  ~st[B7. Use] ~ilc[defaxiom]s ~st[temporarily to explore possibilities.]~nl[]
  When there is a difficult goal that seems to follow immediately (by
  a ~c[:use] hint or by rewriting) from some other lemmas, you can
  create those lemmas as ~ilc[defaxiom] ~il[events] (or, the application of
  ~ilc[skip-proofs] to ~ilc[defthm] ~il[events]) and then double-check that the
  difficult goal really does follow from them.  Then you can go back
  and try to turn each ~ilc[defaxiom] into a ~ilc[defthm].  When you do
  that, it's often useful to ~il[disable] any additional ~il[rewrite] rules that
  you prove in the process, so that the ``difficult goal'' will still
  be proved from its lemmas when the process is complete.

  Better yet, rather than disabling ~il[rewrite] rules, use the ~ilc[local]
  mechanism offered by ~ilc[encapsulate] to make temporary rules
  completely ~ilc[local] to the problem at hand.  ~l[encapsulate] and
  ~pl[local].

  ~st[B9. Use books.]~nl[]
  Consider using previously certified ~il[books], especially for arithmetic
  reasoning.  This cuts down the duplication of effort and starts your
  specification and proof effort from a richer foundation.  See the
  file ~c[\"doc/README\"] in the ACL2 distribution for information on ~il[books]
  that come with the system.

  ~em[C. DEALING WITH FAILED PROOFS]

  ~st[C1. Look in proof output for goals that can't be further simplified.]~nl[]
  Use the ``~il[proof-tree]'' utility to explore the proof space.
  However, you don't need to use that tool to use the ``checkpoint''
  strategy.  The idea is to think of ACL2 as a ``simplifier'' that
  either proves the theorem or generates some goal to consider.  That
  goal is the first ``checkpoint,'' i.e., the first goal that does not
  further simplify.  Exception:  it's also important to look at the
  induction scheme in a proof by induction, and if induction seems
  appropriate, then look at the first checkpoint ~em[after] the
  induction has begun.

  Consider whether the goal on which you focus is even a theorem.
  Sometimes you can execute it for particular values to find a
  counterexample.

  When looking at checkpoints, remember that you are looking for any
  reason at all to believe the goal is a theorem.  So for example,
  sometimes there may be a contradiction in the hypotheses.

  Don't be afraid to skip the first checkpoint if it doesn't seem very
  helpful.  Also, be willing to look a few lines up or down from the
  checkpoint if you are stuck, bearing in mind however that this
  practice can be more distracting than helpful.

  ~st[C2. Use the ``break rewrite'' facility.]~nl[]
  ~ilc[Brr] and related utilities let you inspect the ``rewrite stack.''
  These can be valuable tools in large proof efforts.
  ~l[break-lemma] for an introduction to these tools, and
  ~pl[break-rewrite] for more complete information.

  The break facility is especially helpful in showing you why a
  particular rewrite rule is not being applied.

  ~st[C3. Use induction hints when necessary.]
  Of course, if you can define your functions so that they suggest the
  correct inductions to ACL2, so much the better!  But for complicated
  inductions, induction ~il[hints] are crucial.  ~l[hints] for a
  description of ~c[:induct] ~il[hints].

  ~st[C4. Use the ``Proof Checker'' to explore.]~nl[]
  The ~ilc[verify] command supplied by ACL2 allows one to explore problem
  areas ``by hand.''  However, even if you succeed in proving a
  conjecture with ~ilc[verify], it is useful to prove it without using
  it, an activity that will often require the discovery of ~il[rewrite]
  rules that will be useful in later proofs as well.

  ~st[C5. Don't have too much patience.]~nl[]
  Interrupt the prover fairly quickly when simplification isn't
  succeeding.

  ~st[C6. Simplify rewrite rules.]~nl[]
  When it looks difficult to relieve the hypotheses of an existing
  ~il[rewrite] rule that ``should'' apply in a given setting, ask yourself
  if you can eliminate a hypothesis from the existing ~il[rewrite] rule.
  If so, it may be easier to prove the new version from the old
  version (and some additional lemmas), rather than to start from
  scratch.

  ~st[C7. Deal with base cases first.]~nl[]
  Try getting past the base case(s) first in a difficult proof by
  induction.  Usually they're easier than the inductive step(s), and
  rules developed in proving them can be useful in the inductive
  step(s) too.  Moreover, it's pretty common that mistakes in the
  statement of a theorem show up in the base case(s) of its proof by
  induction.

  ~st[C8. Use] ~c[:expand] ~st[hints.]
  Consider giving ~c[:expand] ~il[hints].  These are especially useful when a
  proof by induction is failing.  It's almost always helpful to open
  up a recursively defined function that is supplying the induction
  scheme, but sometimes ACL2 is too timid to do so; or perhaps the
  function in question is ~il[disable]d.

  ~em[D. PERFORMANCE TIPS]

  ~st[D1. Disable rules.]~nl[]
  There are a number of instances when it is crucial to ~il[disable] rules,
  including (often) those named explicitly in ~c[:use] ~il[hints].  Also,
  ~il[disable] recursively defined functions for which you can prove what
  seem to be all the relevant properties.  The prover can spend
  significant time ``behind the scenes'' trying to open up recursively
  defined functions, where the only visible effect is slowness.

  ~st[D2. Turn off the ``break rewrite'' facility.]
  Remember to execute ~c[:brr nil] after you've finished with the
  ``break rewrite'' utility (~pl[break-rewrite]), in order to
  bring the prover back up to full speed.

  ~em[E. MISCELLANEOUS TIPS AND KNOWLEDGE]

  ~st[E1. Order of application of rewrite rules.]~nl[]
  Keep in mind that the most recent ~il[rewrite] rules in the ~il[history]
  are tried first.

  ~st[E2. Relieving hypotheses is not full-blown theorem proving.]~nl[]
  Relieving hypotheses on ~il[rewrite] rules is done by rewriting and ~il[linear]
  arithmetic alone, not by case splitting or by other prover processes
  ``below'' simplification.

  ~st[E3. ``Free variables'' in rewrite rules.]~nl[] The set of ``free
  variables'' of a ~il[rewrite] rule is defined to contain those
  variables occurring in the rule that do not occur in the left-hand
  side of the rule.  It's often a good idea to avoid rules containing
  free variables because they are ``weak,'' in the sense that
  hypotheses containing such variables can generally only be proved
  when they are ``obviously'' present in the current context.  This
  weakness suggests that it's important to put the most
  ``interesting'' (specific) hypotheses about free variables first, so
  that the right instances are considered.  For example, suppose you
  put a very general hypothesis such as ~c[(consp x)] first.  If the
  context has several ~il[term]s around that are known to be
  ~ilc[consp]s, then ~c[x] may be bound to the wrong one of them.  For much
  more information on free variables, ~pl[free-variables].

  ~st[E4. Obtaining information]
  Use ~c[:]~ilc[pl] ~c[foo] to inspect ~il[rewrite] rules whose left hand sides are
  applications of the function ~c[foo].  Another approach to seeing
  which ~il[rewrite] rules apply is to enter the ~il[proof-checker] with
  ~ilc[verify], and use the ~c[show-rewrites] or ~c[sr] command.

  ~st[E5. Consider esoteric rules with care.]~nl[]
  If you care to ~pl[rule-classes] and peruse the list of
  subtopics (which will be listed right there in most versions of this
  ~il[documentation]), you'll see that ACL2 supports a wide variety of
  rules in addition to ~c[:]~il[rewrite] rules.  Should you use them?
  This is a complex question that we are not ready to answer with any
  generality.  Our general advice is to avoid relying on such rules as
  long as you doubt their utility.  More specifically:  be careful not
  to use conditional type prescription rules, as these have been known
  to bring ACL2 to its knees, unless you are conscious that you are
  doing so and have reason to believe that they are working well.

  ~em[F. SOME THINGS YOU DON'T NEED TO KNOW]

  Most generally:  you shouldn't usually need to be able to predict
  too much about ACL2's behavior.  You should mainly just need to be
  able to react to it.

  ~st[F1. Induction heuristics.]~nl[]
  Although it is often important to read the part of the prover's
  output that gives the induction scheme chosen by the prover, it is
  not necessary to understand how the prover made that choice.
  (Granted, advanced users may occasionally gain minor insight from
  such knowledge.  But it's truly minor in many cases.)  What ~em[is]
  important is to be able to tell it an appropriate induction when it
  doesn't pick the right one (after noticing that it doesn't).  See C3
  above.

  ~st[F2. Heuristics for expanding calls of recursively defined functions.]~nl[]
  As with the previous topic, the important thing isn't to understand
  these heuristics but, rather, to deal with cases where they don't
  seem to be working.  That amounts to supplying ~c[:expand] ~il[hints] for
  those calls that you want opened up, which aren't.  See also C8
  above.

  ~st[F3. The ``waterfall''.]~nl[]
  As discussed many times already, a good strategy for using ACL2 is
  to look for checkpoints (goals stable under simplification) when a
  proof fails, perhaps using the ~il[proof-tree] facility.  Thus, it
  is reasonable to ignore almost all the prover output, and to avoid
  pondering the meaning of the other ``processes'' that ACL2 uses
  besides simplification (such as elimination, cross-fertilization,
  generalization, and elimination of irrelevance).  For example, you
  don't need to worry about prover output that mentions ``type
  reasoning'' or ``abbreviations,'' for example.")
