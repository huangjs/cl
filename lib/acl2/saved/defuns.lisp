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

; Rockwell Addition: A major change is the provision of non-executable
; functions.  These are typically functions that use stobjs but which
; are translated as though they were theorems rather than definitions.
; This is convenient (necessary?) for specifying some stobj
; properties.  These functions will have executable counterparts that
; just throw.  These functions will be marked with the property
; non-executablep.

(defconst *mutual-recursion-ctx-string*
  "( MUTUAL-RECURSION ( DEFUN ~x0 ...) ...)")

(defun translate-bodies1
  (non-executablep names bodies bindings known-stobjs-lst ctx wrld state)
  (cond ((null bodies) (trans-value nil))
        (t (trans-er-let*
            ((x (translate1 (car bodies)
                            (if non-executablep t (car names))
                            (if non-executablep nil bindings)
                            (car known-stobjs-lst)
                            (if (and (consp ctx)
                                     (equal (car ctx)
                                            *mutual-recursion-ctx-string*))
                                (msg "( MUTUAL-RECURSION ... ( DEFUN ~x0 ...) ~
                                      ...)"
                                     (car names))
                              ctx)
                            wrld state))
             (y (translate-bodies1 non-executablep
                                   (cdr names)
                                   (cdr bodies)
                                   bindings
                                   (cdr known-stobjs-lst)
                                   ctx wrld state)))
            (trans-value (cons x y))))))

(defun translate-bodies
  (non-executablep names bodies known-stobjs-lst ctx wrld state)

; Translate the bodies given and return a pair consisting of their
; translations and the final bindings from translate.

  (declare (xargs :guard (true-listp bodies)))
  (mv-let (erp lst bindings state)
          (translate-bodies1 non-executablep names bodies
                             (pairlis$ names names)
                             known-stobjs-lst
                             ctx wrld state)
          (cond (erp (mv t nil state))
                (non-executablep (value (cons lst (pairlis-x2 names '(nil)))))
                (t (value (cons lst bindings))))))

; The next section develops our check that mutual recursion is
; sensibly used.

(defun chk-mutual-recursion-bad-names (lst names bodies)
  (cond ((null lst) nil)
        ((ffnnamesp names (car bodies))
         (chk-mutual-recursion-bad-names (cdr lst) names (cdr bodies)))
        (t
         (cons (car lst)
               (chk-mutual-recursion-bad-names (cdr lst) names (cdr bodies))))))

(defconst *chk-mutual-recursion-string*
  "The definition~#0~[~/s~] of ~&1 ~#0~[does~/do~] not call any of ~
   the other functions being defined via ~
   mutual recursion.  The theorem prover ~
   will perform better if you define ~&1 ~
   without the appearance of mutual recursion.  See ~
  :DOC set-bogus-mutual-recursion-ok to get ~
   ACL2 to handle this situation differently.")

(defun chk-mutual-recursion1 (names bodies warnp ctx state)
  (cond
   ((and warnp
         (warning-disabled-p "mutual-recursion"))
    (value nil))
   (t
    (let ((bad (chk-mutual-recursion-bad-names names names bodies)))
      (cond ((null bad) (value nil))
            (warnp
             (pprogn
              (warning$ ctx ("mutual-recursion")
                        *chk-mutual-recursion-string*
                        (if (consp (cdr bad)) 1 0)
                        bad)
              (value nil)))
            (t (er soft ctx
                   *chk-mutual-recursion-string*
                   (if (consp (cdr bad)) 1 0)
                   bad)))))))

(defun chk-mutual-recursion (names bodies ctx state)

; We check that names has at least 1 element and that if it has
; more than one then every body calls at least one of the fns in
; names.  The idea is to ensure that mutual recursion is used only
; when "necessary."  This is not necessary for soundness but since
; mutually recursive fns are not handled as well as singly recursive
; ones, it is done as a service to the user.  In addition, several
; error messages and other user-interface features exploit the presence
; of this check.

  (cond ((null names)
         (er soft ctx
             "It is illegal to use MUTUAL-RECURSION to define no functions."))
        ((null (cdr names)) (value nil))
        (t
         (let ((bogus-mutual-recursion-ok
                (cdr (assoc-eq :bogus-mutual-recursion-ok
                               (table-alist 'acl2-defaults-table (w state))))))
           (if (eq bogus-mutual-recursion-ok t)
               (value nil)
             (chk-mutual-recursion1 names bodies
                                    (eq bogus-mutual-recursion-ok :warn)
                                    ctx state))))))

; We now develop put-induction-info.

(mutual-recursion

(defun ffnnamep-mod-mbe (fn term)

; We determine whether the function fn (possibly a lambda-expression) is used
; as a function in term', the result of expanding must-be-equal calls in term.
; Keep this in sync with the ffnnamep nest.  Unlike ffnnamep, we assume here
; that fn is a symbolp.

  (cond ((variablep term) nil)
        ((fquotep term) nil)
        ((flambda-applicationp term)
         (or (ffnnamep-mod-mbe fn (lambda-body (ffn-symb term)))
             (ffnnamep-mod-mbe-lst fn (fargs term))))
        ((eq (ffn-symb term) fn) t)
        ((eq (ffn-symb term) 'must-be-equal)
         (ffnnamep-mod-mbe fn (fargn term 1)))
        (t (ffnnamep-mod-mbe-lst fn (fargs term)))))

(defun ffnnamep-mod-mbe-lst (fn l)
  (declare (xargs :guard (and (symbolp fn)
                              (pseudo-term-listp l))))
  (if (null l)
      nil
    (or (ffnnamep-mod-mbe fn (car l))
        (ffnnamep-mod-mbe-lst fn (cdr l)))))

)

; Here is how we set the recursivep property.

; Rockwell Addition:  The recursivep property has changed.  Singly
; recursive fns now have the property (fn) instead of fn.

(defun putprop-recursivep-lst (names bodies wrld)

; On the property list of each function symbol is stored the 'recursivep
; property.  For nonrecursive functions, the value is implicitly nil but no
; value is stored (see comment below).  Otherwise, the value is a true-list of
; fn names in the ``clique.''  Thus, for singly recursive functions, the value
; is a singleton list containing the function name.  For mutually recursive
; functions the value is the list of every name in the clique.  This function
; stores the property for each name and body in names and bodies.

; WARNING: We rely on the fact that this function puts the same names into the
; 'recursivep property of each member of the clique, in our handling of
; being-openedp.

  (cond ((int= (length names) 1)
         (cond ((ffnnamep-mod-mbe (car names) (car bodies))
                (putprop (car names) 'recursivep names wrld))
               (t

; Until we started using the 'def-bodies property to answer most questions
; about recursivep (see macro recursivep), it was a good idea to put a
; 'recursivep property of nil in order to avoid having getprop walk through an
; entire association list looking for 'recursivep.  Now, this less-used
; property is just in the way.

                wrld)))
        (t (putprop-x-lst1 names 'recursivep names wrld))))

(defrec tests-and-call (tests call) nil)

; In nqthm this record was called TEST-AND-CASE and the second component was
; the arglist of a recursive call of the function being analyzed.  Because of
; the presence of mutual recursion, we have renamed it tests-and-call and the
; second component is a "recursive" call (possibly mutually recursive). 

(mutual-recursion

(defun all-calls (names term alist ans)

; Names is a list of defined function symbols.  Return a list with the
; property that if we append it to the set of all calls in the range
; of alist of any fn in names, then this list represents the set of
; all such calls in term/alist.  Ans is an accumulator onto which the
; calls are pushed.  (We really just explore term looking for calls,
; and instantiate them as we find them.)

; Our answer is in reverse print order (displaying lambda-applications
; as LETs).  For example, if a, b and c are all calls of fns in names,
; then if term is (foo a ((lambda (x) c) b)), which would be printed
; as (foo a (let ((x b)) c)), the answer is (c b a).

  (cond ((variablep term) ans)
        ((fquotep term) ans)
        ((flambda-applicationp term)
         (all-calls names
                    (lambda-body (ffn-symb term))
                    (pairlis$ (lambda-formals (ffn-symb term))
                              (sublis-var-lst alist (fargs term)))
                    (all-calls-lst names (fargs term) alist ans)))
        (t (all-calls-lst names
                          (fargs term)
                          alist
                          (cond ((member-eq (ffn-symb term) names)
                                 (add-to-set-equal
                                  (sublis-var alist term)
                                  ans))
                                (t ans))))))

(defun all-calls-lst (names lst alist ans)
  (cond ((null lst) ans)
        (t (all-calls-lst names
                          (cdr lst)
                          alist
                          (all-calls names (car lst) alist ans)))))

)

(defun all-calls-alist (names alist ans)

; This function processes an alist and computes all the calls of fns
; in names in the range of the alist and accumulates them onto ans.

  (cond ((null alist) ans)
        (t (all-calls-alist names (cdr alist)
                            (all-calls names (cdar alist) nil ans)))))

(defun termination-machine1 (tests calls ans)

; This function makes a tests-and-call with tests tests for every call
; in calls.  It accumulates them onto ans so that if called initially
; with ans=nil the result is a list of tests-and-call in the reverse
; order of the calls.

  (cond ((null calls) ans)
        (t (termination-machine1 tests
                                 (cdr calls)
                                 (cons (make tests-and-call
                                             :tests tests
                                             :call (car calls))
                                       ans)))))

(defun termination-machine (names body alist tests)

; This function builds a list of tests-and-call records for some
; fnname in names with body body/alist, with the following property:
; if we append to the result the set of all tests-and-call records
; (tests c), where c varies over all calls in the range of alist of
; any fn in names, then this list represents the set of all such
; records for term/alist.  Note that we don't need to know the
; function symbol to which the body belongs; all the functions in
; names are considered "recursive" calls.  Names is a list of all the
; mutually recursive fns in the clique.  Alist maps variables in body
; to actuals and is used in the exploration of lambda applications.

; For each recursive call in body a tests-and-call is returned
; whose tests are all the tests that "rule" the call and whose call is
; the call.  If a rules b then a governs b but not vice versa.  For
; example, in (if (g (if a b c)) d e) a governs b but does not rule b.
; The reason for taking this weaker notion of governance is that we
; can show easily that the tests-and-calls are together sufficient to
; imply the tests-and-calls generated by induction-machine.

  (cond
   ((or (variablep body)
        (fquotep body))
    nil)
   ((flambda-applicationp body)
    (termination-machine1
     (reverse tests)
     (all-calls-lst names
                    (fargs body)
                    alist
                    nil)
     (termination-machine names
                          (lambda-body (ffn-symb body))
                          (pairlis$ (lambda-formals (ffn-symb body))
                                    (sublis-var-lst alist (fargs body)))
                          tests)))
   ((eq (ffn-symb body) 'if)
    (let ((inst-test (sublis-var alist

; Since (remove-guard-holders x) is provably equal to x, the machine we
; generate using it below is equivalent to the machine generated without it.

                                 (remove-guard-holders (fargn body 1)))))
      (termination-machine1
       (reverse tests)
       (all-calls names (fargn body 1) alist nil)
       (append (termination-machine names
                                    (fargn body 2)
                                    alist
                                    (cons inst-test tests))
               (termination-machine names
                                    (fargn body 3)
                                    alist
                                    (cons (dumb-negate-lit inst-test)
                                          tests))))))
   ((eq (ffn-symb body) 'prog2$)

; Here we handle type declaration forms in mv-let; see translate1.
; Note that there is nothing special here about prog2$; we could
; extend the notion of "rules" further towards the notion of "governs"
; by allowing such an extension for other function symbols as well.

    (let ((all-calls-1 (all-calls names (fargn body 1) alist nil)))
      (cond (all-calls-1

; If there are recursive calls of the function in the first argument, then the
; user is presumably intending to continue the termination and induction
; analyses through that first argument as well.  Otherwise, we want to keep the
; induction scheme simple, so we do not extend that analysis into the first
; argument, either here or in induction-machine-for-fn1; so keep the handling
; of prog2$ for that function in sync with the handling of prog2$ here.

             (append (termination-machine names
                                          (fargn body 1)
                                          alist
                                          tests)
                     (termination-machine names
                                          (fargn body 2)
                                          alist
                                          tests)))
            (t (termination-machine1 (reverse tests)
                                     nil ; all-calls-1
                                     (termination-machine names
                                                          (fargn body 2)
                                                          alist
                                                          tests))))))
   ((eq (ffn-symb body) 'must-be-equal)

; It is sound to treat must-be-equal like a macro for logic purposes.  We do so
; both for induction and for termination.

    (termination-machine names
                         (fargn body 1)
                         alist
                         tests))
   (t (termination-machine1 (reverse tests)
                            (all-calls names body alist nil)
                            nil))))

(defun termination-machines (names bodies)

; This function builds the termination machine for each function defined
; in names with the corresponding body in bodies.  A list of machines
; is returned.

  (cond ((null bodies) nil)
        (t (cons (termination-machine names (car bodies) nil nil)
                 (termination-machines names (cdr bodies))))))

; We next develop the function that guesses measures when the user has
; not supplied them.

(defun proper-dumb-occur-as-output (x y)

; We determine whether the term x properly occurs within the term y, insisting
; in addition that if y is an IF expression then x occurs properly within each
; of the two output branches.

; For example, X does not properly occur in X or Z.  It does properly occur in
; (CDR X) and (APPEND X Y).  It does properly occur in (IF a (CDR X) (CAR X))
; but not in (IF a (CDR X) 0) or (IF a (CDR X) X).

; This function is used in always-tested-and-changedp to identify a formal to
; use as the measured formal in the justification of a recursive definition.
; We seek a formal that is tested on every branch and changed in every
; recursion.  But if (IF a (CDR X) X) is the new value of X in some recursion,
; then it is not really changed, since if we distributed the IF out of the
; recursive call we would see a call in which X did not change.

  (cond ((equal x y) nil)
        ((variablep y) nil)
        ((fquotep y) nil)
        ((eq (ffn-symb y) 'if)
         (and (proper-dumb-occur-as-output x (fargn y 2))
              (proper-dumb-occur-as-output x (fargn y 3))))
        (t (dumb-occur-lst x (fargs y)))))

(defun always-tested-and-changedp (var pos t-machine)

; Is var involved in every tests component of t-machine and changed
; and involved in every call, in the appropriate argument position?
; In some uses of this function, var may not be a variable symbol
; but an arbitrary term.

  (cond ((null t-machine) t)
        ((and (dumb-occur-lst var
                              (access tests-and-call
                                      (car t-machine)
                                      :tests))
              (let ((argn (nth pos
                               (fargs (access tests-and-call
                                              (car t-machine)
                                              :call)))))

; If argn is nil then it means there was no enough args to get the one at pos.
; This can happen in a mutually recursive clique not all clique members have the
; same arity.

                (and argn
                     (proper-dumb-occur-as-output var argn))))
         (always-tested-and-changedp var pos (cdr t-machine)))
        (t nil)))

(defun guess-measure (name defun-flg args pos t-machine ctx wrld state)

; T-machine is a termination machine, i.e., a lists of tests-and-call.
; Because of mutual recursion, we do not know that the call of a
; tests-and-call is a call of name; it may be a call of a sibling of
; name.  We look for the first formal that is (a) somehow tested in
; every test and (b) somehow changed in every call.  Upon finding such
; a var, v, we guess the measure (acl2-count v).  But what does it
; mean to say that v is "changed in a call" if we are defining (foo x
; y v) and see a call of bar?  We mean that v occurs in an argument to
; bar and is not equal to that argument.  Thus, v is not changed in
; (bar x v) and is changed in (bar x (mumble v)).  The difficulty here
; of course is that (mumble v) may not be being passed as the new
; value of v.  But since this is just a heuristic guess intended to
; save the user the burden of typing (acl2-count x) a lot, it doesn't
; matter.

; If we fail to find a measure we cause an error.  

; Pos is initially 0 and is the position in the formals list of the first
; variable listed in args.  Defun-flg is t if we are guessing a measure on
; behalf of a function definition and nil if we are guessing on behalf of a
; :definition rule.  It affects only the error message printed.

  (cond ((null args)
         (cond
          ((null t-machine)

; Presumably guess-measure was called here with args = NIL, for example if
; :set-bogus-mutual-recursion allowed it.  We pick a silly measure that will
; work.  If it doesn't work (hard to imagine), well then, we'll find out when
; we try to prove termination.

           (value (mcons-term* (default-measure-function wrld) *0*)))
          (t
           (er soft ctx
               "No ~#0~[:MEASURE~/:CONTROLLER-ALIST~] was supplied with the ~
                ~#0~[definition of~/:DEFINITION rule~] ~x1.  Our heuristics ~
                for guessing one have not made any suggestions.  No argument ~
                of the function is tested along every branch and occurs as a ~
                proper subterm at the same argument position in every ~
                recursive call.  You must specify a ~#0~[:MEASURE.  See :DOC ~
                defun.~/:CONTROLLER-ALIST.  See :DOC definition.~]"
               (if defun-flg 0 1)
               name))))
        ((always-tested-and-changedp (car args) pos t-machine)
         (value (mcons-term* (default-measure-function wrld) (car args))))
        (t (guess-measure name defun-flg (cdr args) (1+ pos)
                          t-machine ctx wrld state))))

(defun guess-measure-alist (names arglists measures t-machines ctx wrld state)

; We either cause an error or return an alist mapping the names in
; names to their measures (either user suggested or guessed).

  (cond ((null names) (value nil))
        ((equal (car measures) *0*)
         (er-let* ((m (guess-measure (car names)
                                     t
                                     (car arglists)
                                     0
                                     (car t-machines)
                                     ctx wrld state)))
                  (er-let* ((alist (guess-measure-alist (cdr names)
                                                        (cdr arglists)
                                                        (cdr measures)
                                                        (cdr t-machines)
                                                        ctx wrld state)))
                           (value (cons (cons (car names) m)
                                        alist)))))
        (t (er-let* ((alist (guess-measure-alist (cdr names)
                                                 (cdr arglists)
                                                 (cdr measures)
                                                 (cdr t-machines)
                                                 ctx wrld state)))
                    (value (cons (cons (car names) (car measures))
                                 alist))))))

; We now embark on the development of prove-termination, which must
; prove the justification theorems for each termination machine and
; the measures supplied/guessed.

(defun add-literal-to-clause-segments (lit segments at-end-flg)
  (cond ((null segments) nil)
        (t (conjoin-clause-to-clause-set
            (add-literal lit (car segments) at-end-flg)
            (add-literal-to-clause-segments lit (cdr segments) at-end-flg)))))

(defun remove-built-in-clauses (cl-set ens oncep-override wrld state ttree)

; We return two results.  The first is a subset of cl-set obtained by deleting
; all built-in-clauseps and the second is the accumulated ttrees for the
; clauses we deleted.

  (cond
   ((null cl-set) (mv nil ttree))
   (t (mv-let
       (built-in-clausep ttree1)
       (built-in-clausep (car cl-set) ens oncep-override wrld state)

; Ttree is known to be 'assumption free.

       (mv-let
        (new-set ttree)
        (remove-built-in-clauses (cdr cl-set) ens oncep-override wrld state
                                 (cons-tag-trees ttree1 ttree))
        (cond (built-in-clausep (mv new-set ttree))
              (t (mv (cons (car cl-set) new-set) ttree))))))))

(defun length-exceedsp (lst n)
  (cond ((null lst) nil)
        ((= n 0) t)
        (t (length-exceedsp (cdr lst) (1- n)))))

(defun clean-up-clause-set (cl-set ens wrld ttree state)

; Warning:  The set of clauses returned by this function only implies the input
; set.  They are thought to be equivalent only if the input set contains no
; tautologies.  See the caution in subsumption-replacement-loop.

; This function removes subsumed clauses from cl-set, does replacement
; (e.g., if the set includes the clauses {~q p} and {q p} replace them
; both with {p}), and removes built-in clauses.  It returns two
; results, the cleaned up clause set and a ttree justifying the
; deletions and extending ttree.  The returned ttree is 'assumption
; free (provided the incoming ttree is also) because all necessary
; splitting is done internally.

; Bishop Brock has pointed out that it is unclear what is the best order in
; which to do these two checks.  Subsumption-replacement first and then
; built-in clauses?  Or vice versa?  We do a very trivial analysis here to
; order the two.  Bishop is not to blame for this trivial analysis!

; Suppose there are n clauses in the initial cl-set.  Suppose there are b
; built-in clauses.  The cost of the subsumption-replacement loop is roughly
; n*n and that of the built-in check is n*b.  Contrary to all common sense let
; us suppose that the subsumption-replacement loop eliminates redundant clauses
; at the rate, r, so that if we do the subsumption- replacement loop first at a
; cost of n*n we are left with n*r clauses.  Note that the worst case for r is
; 1 and the smaller r is, the better; if r were 1/100 it would mean that we
; could expect subsumption-replacement to pare down a set of 1000 clauses to
; just 10.  More commonly perhaps, r is just below 1, e.g., 99 out of 100
; clauses are unaffected.  To make the analysis possible, let's assume that
; built-in clauses crop up at the same rate!  So,

; n^2 + bnr   = cost of doing subsumption-replacement first  = sub-first

; bn + (nr)^2 = cost of doing built-in clauses first         = bic-first

; Observe that when r=1 the two costs are the same, as they should be.  But
; generally, r can be expected to be slightly less than 1.  

; Here is an example.  Let n = 10, b = 100 and r = 99/100.  In this example we
; have only a few clauses to consider but lots of built in clauses, and we have
; a realistically low expectation of hits.  The cost of sub-first is 1090 but
; the cost of bic-first is 1098.  So we should do sub-first.

; On the other hand, if n=100, b=20, and r=99/100 we see sub-first costs 11980
; but bic-first costs 11801, so we should do built-in clauses first.  This is a
; more common case.

; In general, we should do built-in clauses first when sub-first exceeds
; bic-first.

; n^2 + bnr >= bn + (nr)^2  = when we should do built-in clauses first

; Solving we get:

; n > b/(1+r).  

; Indeed, if n=50 and b=100 and r=99/100 we see the costs of the two equal
; at 7450.

  (cond
   ((let ((sr-limit (sr-limit wrld)))
      (and sr-limit (> (length cl-set) sr-limit)))
    (pstk
     (remove-built-in-clauses
      cl-set ens (match-free-override wrld) wrld state
      (add-to-tag-tree 'sr-limit t ttree))))
   ((length-exceedsp cl-set (global-val 'half-length-built-in-clauses wrld))
    (mv-let (cl-set ttree)
            (pstk
             (remove-built-in-clauses cl-set ens
                                      (match-free-override wrld)
                                      wrld state ttree))
            (mv (pstk
                 (subsumption-replacement-loop
                  (merge-sort-length cl-set) nil nil))
                ttree)))
   (t (pstk
       (remove-built-in-clauses
        (pstk
         (subsumption-replacement-loop
          (merge-sort-length cl-set) nil nil))
        ens (match-free-override wrld) wrld state ttree)))))

(defun measure-clause-for-branch (name tc measure-alist rel wrld)

; Name is the name of some function, say f0, in a mutually recursive
; clique.  Tc is a tests-and-call in the termination machine of f0 and hence
; contains some tests and a call of some function in the clique, say,
; f1.  Measure-alist supplies the measures m0 and m1 for f0 and f1.
; Rel is the well-founded relation we are using.

; We assume that the 'formals for all the functions in the clique have
; already been stored in wrld.

; We create a set of clauses equivalent to

;    tests -> (rel m1' m0),

; where m1' is m1 instantiated as indicated by the call of f1.

  (let* ((f0 name)
         (m0 (cdr (assoc-eq f0 measure-alist)))
         (tests (access tests-and-call tc :tests))
         (call (access tests-and-call tc :call))
         (f1 (ffn-symb call))
         (m1-prime (subcor-var
                    (formals f1 wrld)
                    (fargs call)
                    (cdr (assoc-eq f1 measure-alist))))
         (concl (mcons-term* rel m1-prime m0)))
    (add-literal concl
                 (dumb-negate-lit-lst tests)
                 t)))

(defun measure-clauses-for-fn1 (name t-machine measure-alist rel wrld)
  (cond ((null t-machine) nil)
        (t (conjoin-clause-to-clause-set
            (measure-clause-for-branch name
                                       (car t-machine)
                                       measure-alist
                                       rel
                                       wrld)
            (measure-clauses-for-fn1 name
                                     (cdr t-machine)
                                     measure-alist
                                     rel
                                     wrld)))))

(defun measure-clauses-for-fn (name t-machine measure-alist mp rel wrld)

; We form all of the clauses that are required to be theorems for the admission
; of name with the given termination machine and measures.  Mp is the "domain
; predicate" for the well-founded relation rel, or else mp is t meaning rel is
; well-founded on the universe.  (For example, mp is o-p when rel is o<.)  For
; the sake of illustration, suppose the defun of name is simply

; (defun name (x)
;   (declare (xargs :guard (guard x)))
;   (if (test x) (name (d x)) x))

; Assume mp and rel are o-p and o<.  Then we will create clauses equivalent
; to:

;    (o-p (m x))
; and
;    (test x) -> (o< (m (d x)) (m x)).

; Observe that the guard of the function is irrelevant!

; We return a set of clauses which are implicitly conjoined.

  (cond
   ((eq mp t)
    (measure-clauses-for-fn1 name t-machine measure-alist rel wrld))
   (t (conjoin-clause-to-clause-set
       (add-literal (mcons-term* mp (cdr (assoc-eq name measure-alist)))
                    nil t)
       (measure-clauses-for-fn1 name t-machine measure-alist rel wrld)))))

(defun measure-clauses-for-clique (names t-machines measure-alist mp rel wrld)

; We assume we can obtain from wrld the 'formals for each fn in names.

  (cond ((null names) nil)
        (t (conjoin-clause-sets
            (measure-clauses-for-fn (car names)
                                    (car t-machines)
                                    measure-alist
                                    mp rel
                                    wrld)
            (measure-clauses-for-clique (cdr names)
                                        (cdr t-machines)
                                        measure-alist
                                        mp rel
                                        wrld)))))

(defun tilde-*-measure-phrase1 (alist wrld)
  (cond ((null alist) nil)
        (t (cons (msg (cond ((null (cdr alist)) "~p1 for ~x0.")
                            (t "~p1 for ~x0"))
                      (caar alist)
                      (untranslate (cdar alist) nil wrld))
                 (tilde-*-measure-phrase1 (cdr alist) wrld)))))

(defun tilde-*-measure-phrase (alist wrld)

; Let alist be an alist mapping function symbols, fni, to measure terms, mi.
; The fmt directive ~*0 will print the following, if #\0 is bound to
; the output of this fn:

; "m1 for fn1, m2 for fn2, ..., and mk for fnk."

; provided alist has two or more elements.  If alist contains
; only one element, it will print just "m1."

; Note the final period at the end of the phrase!  In an earlier version
; we did not add the period and saw a line-break between the ~x1 below
; and its final period.

; Thus, the following fmt directive will print a grammatically correct
; sentence ending with a period: "For the admission of ~&1 we will use
; the measure ~*0"

  (list* "" "~@*" "~@* and " "~@*, "
         (cond
          ((null (cdr alist))
           (list (cons "~p1."
                       (list (cons #\1
                                   (untranslate (cdar alist) nil wrld))))))
          (t (tilde-*-measure-phrase1 alist wrld)))
         nil))

(defun find-?-measure (measure-alist)
  (cond ((endp measure-alist) nil)
        ((let* ((entry (car measure-alist))
                (measure (cdr entry)))
           (and (consp measure)
                (eq (car measure) :?)
                entry)))
        (t (find-?-measure (cdr measure-alist)))))

(defun prove-termination (names t-machines measure-alist mp rel
                                hints otf-flg ctx ens wrld state ttree)

; Given a list of the functions introduced in a mutually recursive
; clique, their t-machines, the measure-alist for the clique, a domain
; predicate mp on which a certain relation, rel, is known to be
; well-founded, a list of hints in 1:1 correspondence with each of the
; above, and a world in which we can find the 'formals of
; each function in the clique, we prove the theorems required by the
; definitional principle.  In particular, we prove that each measure
; is an o-p and that in every tests-and-call in the t-machine
; of each function, the measure of the recursive calls is strictly
; less than that of the incoming arguments.  If we fail, we cause an
; error.

; This function produces output describing the proofs.  It should be
; the first output-producing function in the defun processing on every
; branch through defun.  It always prints something and leaves you in a
; clean state ready to begin a new sentence, but may leave you in the
; middle of a line (i.e., col > 0).

; If we succeed we return two values, consed together as "the" value
; in this error/value/state producing function.  The first value is
; the column produced by our output.  The second value is a ttree in
; which we have accumulated all of the ttrees associated with each
; proof done.

; This function is specially coded so that if t-machines is nil then
; it is a signal that there is only one element of names and it is a
; non-recursive function.  In that case, we short-circuit all of the
; proof machinery and simply do the associated output.  We coded it this
; way to preserve the invariant that prove-termination is THE place
; the defun output is initiated.

; This function increments timers.  Upon entry, any accumulated time
; is charged to 'other-time.  The printing done herein is charged
; to 'print-time and the proving is charged to 'prove-time.

  (mv-let
   (cl-set cl-set-ttree)
   (cond ((and (not (ld-skip-proofsp state))
               t-machines)
          (clean-up-clause-set
           (measure-clauses-for-clique names
                                       t-machines
                                       measure-alist
                                       mp rel
                                       wrld)
           ens
           wrld ttree state))
         (t (mv nil ttree)))
   (cond
    ((and (not (ld-skip-proofsp state))
          (find-?-measure measure-alist))
     (let* ((entry (find-?-measure measure-alist))
            (fn (car entry))
            (measure (cdr entry)))
       (er soft ctx
           "A :measure of the form (:? v1 ... vk) is only legal when the ~
            defun is redundant (see :DOC redundant-events) or when skipping ~
            proofs (see :DOC ld-skip-proofsp).  The :measure ~x0 supplied for ~
            function symbol ~x1 is thus illegal."
           measure fn)))
    (t
     (er-let*
      ((cl-set-ttree (accumulate-ttree-into-state cl-set-ttree state)))
      (pprogn
       (increment-timer 'other-time state)
       (let ((displayed-goal (prettyify-clause-set cl-set
                                                   (let*-abstractionp state)
                                                   wrld))
             (simp-phrase (tilde-*-simp-phrase cl-set-ttree)))
         (mv-let
          (col state)
          (cond
           ((ld-skip-proofsp state)
            (mv 0 state))
           ((null t-machines)
            (io? event nil (mv col state)
                 (names)
                 (fmt "Since ~&0 is non-recursive, its admission is trivial.  "
                      (list (cons #\0 names))
                      (proofs-co state)
                      state
                      nil)
                 :default-bindings ((col 0))))
           ((null cl-set)
            (io? event nil (mv col state)
                 (measure-alist wrld rel names)
                 (fmt "The admission of ~&0 ~#0~[is~/are~] trivial, using ~@1 ~
                       and the measure ~*2  "
                      (list (cons #\0 names)
                            (cons #\1 (tilde-@-well-founded-relation-phrase
                                       rel wrld))
                            (cons #\2 (tilde-*-measure-phrase
                                       measure-alist wrld)))
                      (proofs-co state)
                      state
                      nil)
                 :default-bindings ((col 0))))
           (t
            (io? event nil (mv col state)
                 (cl-set-ttree displayed-goal simp-phrase measure-alist wrld
                               rel names)
                 (fmt "For the admission of ~&0 we will use ~@1 and the ~
                       measure ~*2  The non-trivial part of the measure ~
                       conjecture~#3~[~/, given ~*4,~] is~@6~%~%Goal~%~q5."
                      (list (cons #\0 names)
                            (cons #\1 (tilde-@-well-founded-relation-phrase
                                       rel wrld))
                            (cons #\2 (tilde-*-measure-phrase
                                       measure-alist wrld))
                            (cons #\3 (if (nth 4 simp-phrase) 1 0))
                            (cons #\4 simp-phrase)
                            (cons #\5 displayed-goal)
                            (cons #\6 (if (tagged-object 'sr-limit cl-set-ttree)
                                          " as follows (where the ~
                                           subsumption/replacement limit ~
                                           affected this analysis; see :DOC ~
                                           case-split-limitations)."
                                        "")))
                      (proofs-co state)
                      state
                      nil)
                 :default-bindings ((col 0)))))
          (pprogn
           (increment-timer 'print-time state)
           (cond
            ((null cl-set)

; If the io? above did not print because 'event is inhibited, then col is nil.
; Just to keep ourselves sane, we will set it to 0.

             (value (cons (or col 0) cl-set-ttree)))
            (t
             (mv-let (erp ttree state)
                     (prove (termify-clause-set cl-set)
                            (make-pspv ens wrld
                                       :displayed-goal displayed-goal
                                       :otf-flg otf-flg)
                            hints ens wrld ctx state)
                     (cond (erp
                            (er soft ctx
                                "The proof of the measure conjecture for ~&0 ~
                                 has failed.~|"
                                names))
                           (t
                            (mv-let (col state)
                                    (io? event nil (mv col state)
                                         (names cl-set)
                                         (fmt "That completes the proof of ~
                                               the measure theorem for ~&1.  ~
                                               Thus, we admit ~#1~[this ~
                                               function~/these functions~] ~
                                               under the principle of ~
                                               definition."
                                              (list (cons #\0 cl-set)
                                                    (cons #\1 names))
                                              (proofs-co state)
                                              state
                                              nil)
                                         :default-bindings ((col 0)))
                                    (pprogn
                                     (increment-timer 'print-time state)
                                     (value
                                      (cons
                                       (or col 0)
                                       (cons-tag-trees
                                        cl-set-ttree ttree)))))))))))))))))))

; When we succeed in proving termination, we will store the
; justification properties.

(defun putprop-justification-lst (measure-alist mp rel wrld)

; Each function has a 'justification property.  The value of the property
; is a justification record.

  (cond ((null measure-alist) wrld)
        (t (putprop-justification-lst
            (cdr measure-alist) mp rel
            (putprop (caar measure-alist)
                     'justification
                     (make justification
                           :subset (all-vars (cdar measure-alist))
                           :mp mp
                           :rel rel
                           :measure (cdar measure-alist))
                     wrld)))))

(defun cross-tests-and-calls1 (top-tests tests calls tac-list)

; See the comment in cross-tests-and-calls.  Here, we cross a single
; tests-and-calls record, having the given tests and calls, with such records
; in tac-list.

  (cond ((endp tac-list)
         nil)
        (t (cons (make tests-and-calls
                       :tests (append
                               top-tests ; prettier if top-tests comes first
                               (set-difference-equal
                                (union-equal
                                 tests
                                 (access tests-and-calls (car tac-list)
                                         :tests))
                                top-tests))
                       :calls (union-equal
                               calls
                               (access tests-and-calls (car tac-list)
                                       :calls)))
                 (cross-tests-and-calls1 top-tests tests calls
                                         (cdr tac-list))))))

(defun cross-tests-and-calls (top-tests tacs1 tacs2)

; We are given two non-empty lists, tacs1 and tacs2, of tests-and-calls
; records.  Each such record represents a list of tests together with a
; corresponding list of calls.  For each <tests1, calls1> in tacs1 and <tests2,
; calls2> in tacs2, we can form a record by taking the union of top-tests,
; tests1, and tests2 for the tests, and the union of calls1 and calls2 for the
; calls.

; Here, we view a tests-and-calls record as the universal closure of an
; implication.  Its hypothesis is the conjunction of its top-tests and its
; tests.  Its conclusion is the conjunction formed from each call by creating
; the usual corresponding formula, stating that the measure decreases for the
; indicated substitution.  Moreover, the disjunction of all such hypotheses is
; the same for both tacs1 and tacs2.

; Thus, inputs tacs1 and tacs2 represent conjunctions C1 and C2 (respectively)
; of such formulas.  The returned list of tests-and-calls records then
; represents a formula equivalent to (and C1 C2), for which the disjunction of
; all hypotheses is equivalent to that for C1 and for C2, but with top-tests
; included explicitly.

; In the context of induction, we have presumably proved each measure
; conjecture mentioned above.  Moreover, the hypotheses completely cover the
; possibilities; that is, the disjunctions of the hypotheses for tacsi is
; equivalent to t (i=1,2), and thus the disjunction of the hypotheses for the
; returned tests-and-calls is equivalent to the conjunction of top-tests, which
; are the tests governing a top-level call of this function.

  (cond ((endp tacs1)
         nil)
        (t (append (cross-tests-and-calls1
                    top-tests
                    (access tests-and-calls (car tacs1) :tests)
                    (access tests-and-calls (car tacs1) :calls)
                    tacs2)
                   (cross-tests-and-calls top-tests (cdr tacs1) tacs2)))))

(defun induction-machine-for-fn1 (names body alist tests calls)

; This function builds a list of tests-and-calls for the fnname in
; names with the given body/alist, assuming that all calls in alist
; are among the given calls and that it is justified to
; add all the additional calls in calls.  Note that we don't need to
; know the function symbol to which the body belongs; all the
; functions in names are considered "recursive" calls.  Names is a
; list of all the mutually recursive fns in the clique.

  (cond
   ((or (variablep body)
        (fquotep body)
        (and (not (flambda-applicationp body))
             (not (eq (ffn-symb body) 'if))
             (not (eq (ffn-symb body) 'prog2$))
             (not (eq (ffn-symb body) 'must-be-equal))))
    (let ((reverse-tests (reverse tests)))
      (list
       (make tests-and-calls
             :tests reverse-tests
             :calls (reverse
                     (all-calls names body alist
                                (all-calls-lst names
                                               reverse-tests
                                               nil
                                               calls)))))))
   ((flambda-applicationp body)

; Observe that we just go straight into the body of the lambda (with the
; appropriate alist) but that we modify calls so that every tests-and-calls
; we build will contain all of the calls in the actuals to the lambda
; application.


    (induction-machine-for-fn1 names
                               (lambda-body (ffn-symb body))
                               (pairlis$
                                (lambda-formals (ffn-symb body))
                                (sublis-var-lst alist (fargs body)))
                               tests
                               (all-calls-lst names (fargs body) alist
                                              calls)))
   ((eq (ffn-symb body) 'prog2$)

    (cond ((all-calls names (fargn body 1) alist nil)

; If there are recursive calls of the function in the first argument, then the
; user is presumably intending to continue the termination and induction
; analyses through that first argument as well.  Otherwise, we want to keep the
; induction scheme simple, so we do not extend that analysis into the first
; argument, either here or in termination-machine; so keep the handling
; of prog2$ for that function in sync with the handling of prog2$ here.

           (cross-tests-and-calls
            tests

; Note that each of the following arguments to cross-tests-and-calls is
; non-nil, by an easy induction on induction-machine-for-fn1.

            (induction-machine-for-fn1 names
                                       (fargn body 1)
                                       alist
                                       nil ; fresh tests
                                       calls)
            (induction-machine-for-fn1 names
                                       (fargn body 2)
                                       alist
                                       nil ; fresh tests
                                       calls)))
          (t (induction-machine-for-fn1
              names
              (fargn body 2)
              alist
              tests
              calls ; equal to (all-calls names (fargn body 1) alist calls)
              ))))
   ((eq (ffn-symb body) 'must-be-equal)

; It is sound to treat must-be-equal like a macro for logic purposes.  We do so
; both for induction and for termination.

    (induction-machine-for-fn1 names
                               (fargn body 1)
                               alist
                               tests
                               calls))
   (t
    (let ((inst-test (sublis-var alist

; Since (remove-guard-holders x) is provably equal to x, the machine we
; generate using it below is equivalent to the machine generated without it.

                                 (remove-guard-holders (fargn body 1)))))
      (append
       (induction-machine-for-fn1 names
                                  (fargn body 2)
                                  alist
                                  (cons inst-test tests)
                                  calls)
       (induction-machine-for-fn1 names
                                  (fargn body 3)
                                  alist
                                  (cons (dumb-negate-lit inst-test)
                                        tests)
                                  calls))))))

(defun induction-machine-for-fn (names body)

; We build an induction machine for the function in names with the given body.
; We claim the soundness of the induction schema suggested by this machine is
; easily seen from the proof done by prove-termination.  See
; termination-machine.

; Note: The induction machine built for a clique of more than 1
; mutually recursive functions is probably unusable.  We do not know
; how to do inductions on such functions now.

  (induction-machine-for-fn1 names
                             body
                             nil
                             nil
                             nil))

(defun induction-machines (names bodies)

; This function builds the induction machine for each function defined
; in names with the corresponding body in bodies.  A list of machines
; is returned.  See termination-machine.

; Note: If names has more than one element we return nil because we do
; not know how to interpret the induction-machines that would be
; constructed from a non-trivial clique of mutually recursive
; functions.  As a matter of fact, as of this writing,
; induction-machine-for-fn constructs the "natural" machine for
; mutually recursive functions, but there's no point in consing them
; up since we can't use them.  So all that machinery is
; short-circuited here.

  (cond ((null (cdr names))
         (list (induction-machine-for-fn names (car bodies))))
        (t nil)))

(defun putprop-induction-machine-lst (names machines wrld)

; Note:  If names has more than one element we do nothing.  We only
; know how to interpret induction machines for singly recursive fns.

  (cond ((null (cdr names))
         (putprop (car names)
                  'induction-machine
                  (car machines)
                  wrld))
        (t wrld)))

(defun quick-block-initial-settings (formals)
  (cond ((null formals) nil)
        (t (cons 'un-initialized
                 (quick-block-initial-settings (cdr formals))))))

(defun quick-block-info1 (var term)
  (cond ((eq var term) 'unchanging)
        ((dumb-occur var term) 'self-reflexive)
        (t 'questionable)))

(defun quick-block-info2 (setting info1)
  (case setting
        (questionable 'questionable)
        (un-initialized info1)
        (otherwise
         (cond ((eq setting info1) setting)
               (t 'questionable)))))

(defun quick-block-settings (settings formals args)
  (cond ((null settings) nil)
        (t (cons (quick-block-info2 (car settings)
                                    (quick-block-info1 (car formals)
                                                       (car args)))
                 (quick-block-settings (cdr settings)
                                       (cdr formals)
                                       (cdr args))))))

(defun quick-block-down-t-machine (name settings formals t-machine)
  (cond ((null t-machine) settings)
        ((not (eq name
                  (ffn-symb (access tests-and-call (car t-machine) :call))))
         (er hard 'quick-block-down-t-machine
             "When you add induction on mutually recursive functions don't ~
              forget about QUICK-BLOCK-INFO!"))
        (t (quick-block-down-t-machine
            name
            (quick-block-settings
             settings
             formals
             (fargs (access tests-and-call (car t-machine) :call)))
            formals
            (cdr t-machine)))))

(defun quick-block-info (name formals t-machine)

; This function should be called a singly recursive function, name, and
; its termination machine.  It should not be called on a function
; in a non-trivial mutually recursive clique because the we don't know
; how to analyze a call to a function other than name in the t-machine.

; We return a list in 1:1 correspondence with the formals of name.
; Each element of the list is either 'unchanging, 'self-reflexive,
; or 'questionable.  The list is used to help quickly decide if a
; blocked formal can be tolerated in induction.

  (quick-block-down-t-machine name
                              (quick-block-initial-settings formals)
                              formals
                              t-machine))


(defun putprop-quick-block-info-lst (names t-machines wrld)

; We do not know how to compute quick-block-info for non-trivial
; mutually-recursive cliques.  We therefore don't do anything for
; those functions.  If names is a list of length 1, we do the
; computation.  We assume we can find the formals of the name in wrld.

  (cond ((null (cdr names))
         (putprop (car names)
                  'quick-block-info
                  (quick-block-info (car names)
                                    (formals (car names) wrld)
                                    (car t-machines))
                  wrld))
        (t wrld)))

(deflabel subversive-recursions
  :doc
  ":Doc-Section Miscellaneous

  why we restrict ~il[encapsulate]d recursive functions~/

  Subtleties arise when one of the ``constrained'' functions, ~c[f],
  introduced in the ~il[signature] of an ~ilc[encapsulate] event, is
  involved in the termination argument for a non-local recursively
  defined function, ~c[g], in that ~c[encapsulate].  During the
  processing of the encapsulated events, ~c[f] is locally defined to
  be some witness function, ~c[f'].  Properties of ~c[f'] are
  explicitly proved and exported from the encapsulate as the
  constraints on the undefined function ~c[f].  But if ~c[f] is used
  in a recursive ~c[g] defined within the encapsulate, then the
  termination proof for ~c[g] may use properties of ~c[f'] -- the
  witness -- that are not explicitly set forth in the constraints
  stated for ~c[f].

  Such recursive ~c[g] are said be ``subversive'' because if naively
  treated they give rise to unsound induction schemes or (via
  functional instantiation) recurrence equations that are impossible
  to satisfy.  We illustrate what could go wrong below.

  Subversive recursions are not banned outright.  Instead, they are
  treated as part of the constraint.  That is, in the case above, the
  definitional equation for ~c[g] becomes one of the constraints on
  ~c[f].  This is generally a severe restriction on future functional
  instantiations of ~c[f].  In addition, ACL2 removes from its knowledge
  of ~c[g] any suggestions about legal inductions to ``unwind'' its
  recursion.

  What should you do?  Often, the simplest response is to move the
  offending recursive definition, e.g., ~c[g], out of the encapsulate.
  That is, introduce ~c[f] by constraint and then define ~c[g] as an
  ``independent'' event.  You may need to constrain ``additional''
  properties of ~c[f] in order to admit ~c[g], e.g., constrain it to
  reduce some ordinal measure.  However, by separating the
  introduction of ~c[f] from the admission of ~c[g] you will clearly
  identify the necessary constraints on ~c[f], functional
  instantiations of ~c[f] will be simpler, and ~c[g] will be a useful
  function which suggests inductions to the theorem prover.

  Note that the functions introduced in the ~il[signature] should not
  even occur ancestrally in the termination proofs for non-local
  recursive functions in the encapsulate.  That is, the constrained
  functions of an encapsulate should not be reachable in the
  dependency graph of the functions used in the termination arguments
  of recursive functions in encapsulate.  If they are reachable, their
  definitions become part of the constraints.~/

  The following event illustrates the problem posed by subversive
  recursions.
  ~bv[]
  (encapsulate (((f *) => *))
    (local (defun f (x) (cdr x)))
    (defun g (x)
      (if (consp x) (not (g (f x))) t)))
  ~ev[]
  Suppose, contrary to how ACL2 works, that the encapsulate above
  were to introduce no constraints on ~c[f] on the bogus grounds that
  the only use of ~c[f] in the encapsulate is in an admissible function.
  We discuss the plausibility of this bogus argument in a moment.

  Then it would be possible to prove the theorem:

  ~bv[]
  (defthm f-not-identity
    (not (equal (f '(a . b)) '(a . b)))
    :rule-classes nil
    :hints ((\"Goal\" :use (:instance g (x '(a . b))))))
  ~ev[]  
  simply by observing that if ~c[(f '(a . b))] were ~c['(a . b)], then
  ~c[(g '(a . b))] would be ~c[(not (g '(a . b)))], which is impossible.

  But then we could functionally instantiate ~c[f-not-identity], replacing
  ~c[f] by the identity function, to prove ~c[nil]!  This is bad.
  ~bv[]
  (defthm bad
    nil
    :rule-classes nil
    :hints
    ((\"Goal\" :use (:functional-instance f-not-identity (f identity)))))
  ~ev[]
  
  This sequence of events was legal in versions of ACL2 prior to Version 1.5.
  When we realized the problem we took steps to make it illegal.  However,
  our steps were insufficient and it was possible to sneak in a subversive
  function (via mutual recursion) as late as Version  2.3.

  We now turn to the plausibility of the bogus argument above.  Why might
  one even be tempted to think that the definition of ~c[g] above poses
  no constraint on ~c[f]?  Here is a very similar encapsulate.
  ~bv[]
  (encapsulate (((f *) => *))
    (local (defun f (x) (cdr x)))
    (defun map (x)
      (if (consp x)
          (cons (f x) (map (cdr x)))
        nil)))
  ~ev[]
  Here ~c[map] plays the role of ~c[g] above.  Like ~c[g], ~c[map]
  calls the constrained function ~c[f].  But ~c[map] truly does not
  constrain ~c[f].  In particular, the definition of ~c[map] could be
  moved ``out'' of the encapsulate so that ~c[map] is introduced
  afterwards.  The difference between ~c[map] and ~c[g] is that the
  constrained function plays no role in the termination argument for
  the one but does for the other.

  As a ``user-friendly'' gesture, ACL2 implicitly moves ~c[map]-like
  functions out of encapsulations; logically speaking, they are
  introduced after the encapsulation.  This simplifies the constraint.
  This is done only for ``top-level'' encapsulations.  When an
  ~c[encapsulate] containing a non-empty ~il[signature] list is
  embedded in another ~c[encapsulate] with a non-empty signature list,
  no attempt is made to move ~c[map]-like functions out.  The user is
  advised, via the ``infected'' warning, to phrase the encapsulation
  in the simplest way possible.

  The lingering bug between Versions 1.5 and 2.3 mentioned above was
  due to our failure to detect the ~c[g]-like nature of some functions
  when they were defined in mutually recursively cliques with other
  functions.  The singly recursive case was recognized.  The bug arose
  because our detection ``algorithm'' was based on the ``suggested
  inductions'' left behind by successful definitions.  We failed to
  recall that mutually-recursive definitions do not, as of this
  writing, make any suggestions about inductions and so did not leave
  any traces of their subversive natures.~/")

(deflabel subversive-inductions
  :doc
  ":Doc-Section Miscellaneous

  why we restrict ~il[encapsulate]d recursive functions~/

  ~l[subversive-recursions]. ~/

  ")

(defmacro big-mutrec (names)

; All mutual recursion nests with more than the indicated number of defuns will
; be processed by installing intermediate worlds, for improved performance.  We
; have seen an improvement of roughly two orders of magnitude in such a case.
; The value below is merely heuristic, chosen with very little testing; we
; should feel free to change it.

  `(> (length ,names) 20))

(defmacro update-w (condition new-w &optional retract-p)

; WARNING: This function installs a world, so it may be necessary to call it
; only in the (dynamic) context of revert-world-on-error.  For example, its
; calls during definitional processing are all under the call of
; revert-world-on-error in defuns-fn.

  `(let ((wrld ,new-w))
     (cond
      (,condition
       (pprogn ,(if retract-p
                    '(set-w 'retraction wrld state)
                  '(set-w 'extension wrld state))
               (value wrld)))
      (t (value wrld)))))

(defun put-induction-info
  (names arglists measures bodies mp rel hints otf-flg big-mutrec ctx ens wrld
         state)

; WARNING: This function installs a world.  That is safe at the time of this
; writing because this function is only called by defuns-fn0, which is only
; called by defuns-fn, where that call is protected by a revert-world-on-error.

; We are processing a clique of mutually recursive functions with the
; names, arglists, measures, and bodies given.  All of the
; above lists are in 1:1 correspondence.  The hints is the result
; of appending together all of the hints provided.  Mp and rel are the
; domain predicate and well-founded relation to be used.  We attempt to
; prove the admissibility of the recursions.  We cause an error if any
; proof fails.  We put a lot of properties under the function symbols,
; namely:

;    recursivep                     all fns in names
;    justification                  all recursive fns in names
;    induction-machine              the singly recursive fn in name*
;    quick-block-info               the singly recursive fn in name*
;    symbol-class :ideal            all fns in names

; *If names consists of exactly one recursive fn, we store its
; induction-machine and its quick-block-info, otherwise we do not.

; If no error occurs, we return a triple consisting of the column the
; printer is in, the final value of wrld and a tag tree documenting
; the proofs we did.

; Note:  The function could be declared to return 5 values, but we
; would rather use the standard state and error primitives and so
; it returns 3 and lists together the three "real" answers.

  (let* ((wrld1 (putprop-recursivep-lst names bodies wrld)))

; The put above stores a note on each function symbol as to whether it is
; recursive or not.  An important question arises: have we inadventently
; assumed something axiomatically about inadmissible functions?  We say no.
; None of the functions in question have bodies yet, so the simplifier doesn't
; care about properties such as 'recursivep.  However, we make use of this
; property below to decide if we need to prove termination.

    (cond ((and (null (cdr names))
                (null (getprop (car names) 'recursivep nil
                               'current-acl2-world wrld1)))

; If only one function is being defined and it is non-recursive, we can quit.
; But we have to store the symbol-class and we have to print out the admission
; message with prove-termination so the rest of our processing is uniform.

           (er-progn
            (cond
             (hints
              (er soft ctx
                  "Since ~x0 is non-recursive it is odd that you have ~
                   supplied :hints (which are used only during ~
                   termination proofs).  We suspect something is ~
                   amiss, e.g., you meant to supply :guard-hints or ~
                   the body of the definition is incorrect."
                  (car names)))
             (t (value nil)))
            (er-let*
             ((wrld1 (update-w big-mutrec wrld1))
              (pair (prove-termination names nil nil mp rel nil
                                       otf-flg ctx ens wrld1 state nil)))

; We know that pair is of the form (col . ttree), where col is the column
; the output state is in.

             (value (list (car pair)
                          (putprop-x-lst1 names 'symbol-class :ideal wrld1)
                          (cdr pair))))))
          (t

; Otherwise we first construct the termination machines for all the
; functions in the clique.

           (let*
               ((t-machines (termination-machines names bodies)))

; Next we get the measures for each function.  That may cause an error
; if we couldn't guess one for some function.

             (er-let*
               ((wrld1 (update-w big-mutrec wrld1))
                (measure-alist
                 (guess-measure-alist names arglists
                                      measures
                                      t-machines
                                      ctx wrld1 state))
                (hints (if hints
                           (value hints)
                         (let ((default-hints (default-hints wrld1)))
                           (if default-hints ; then we haven't yet translated
                               (translate-hints
                                (cons "Measure Lemma for" (car names))
                                default-hints ctx wrld1 state)
                             (value hints)))))
                (pair (prove-termination names
                                         t-machines
                                         measure-alist
                                         mp
                                         rel
                                         hints
                                         otf-flg
                                         ctx
                                         ens
                                         wrld1
                                         state
                                         nil)))

; Ok, we have managed to prove termination!  Pair is a pair of the form (col .
; ttree), where col tells us what column the printer is in and ttree describes
; the proofs done.  We now store the 'justification of each function, the
; induction machine for each function, and the quick-block-info.

               (let* ((wrld2
                       (putprop-justification-lst measure-alist mp rel wrld1))
                      (wrld3 (putprop-induction-machine-lst
                              names
                              (induction-machines names bodies)
                              wrld2))
                      (wrld4 (putprop-quick-block-info-lst names
                                                           t-machines
                                                           wrld3))
                      (wrld5 (putprop-x-lst1 names
                                             'symbol-class :ideal wrld4)))

; We are done.  We will return the final wrld and the ttree describing
; the proofs we did.

                 (value
                  (list (car pair)
                        wrld5
                        (push-lemma
                         (cddr (assoc-eq rel
                                         (global-val
                                          'well-founded-relation-alist
                                          wrld5)))
                         (cdr pair)))))))))))

; We next worry about storing the normalized bodies.

(defconst *equality-aliases* '(eq eql =))

(defun destructure-definition (term install-body ens wrld ttree)

; Term is a translated term that is the :corollary of a :definition rule.  If
; install-body is non-nil then we intend to update the 'def-bodies
; property; and if moreover, install-body is :normalize, then we want to
; normalize the resulting new body.  Ens is an enabled structure if
; install-body is :normalize; otherwise ens is ignored.

; We return (mv hyps equiv fn args body new-body ttree) or else nils if we fail
; to recognize the form of term.  Hyps results flattening the hypothesis of
; term, when a call of implies, into a list of hypotheses.  Failure can be
; detected by checking for (null fn) since nil is not a legal fn symbol.

  (mv-let
   (hyps equiv fn-args body)
   (case-match term
     (('implies hyp (equiv fn-args body))
      (mv (flatten-ands-in-lit hyp)
          equiv
          fn-args
          body))
     ((equiv fn-args body)
      (mv nil
          equiv
          fn-args
          body))
     (& (mv nil nil nil nil)))
   (let ((equiv (if (member-eq equiv *equality-aliases*)
                    'equal
                  equiv))
         (fn (and (consp fn-args) (car fn-args))))
     (cond
      ((and fn
            (symbolp fn)
            (not (member-eq fn

; Hide is disallowed in chk-acceptable-definition-rule.

                            '(quote if)))
            (equivalence-relationp equiv wrld))
       (mv-let (body ttree)
               (cond ((eq install-body :NORMALIZE)
                      (normalize (remove-guard-holders body)
                                 nil ; iff-flg
                                 nil ; type-alist
                                 ens
                                 wrld
                                 ttree))
                     (t (mv body ttree)))
               (mv hyps
                   equiv
                   fn
                   (cdr fn-args)
                   body
                   ttree)))
      (t (mv nil nil nil nil nil nil))))))

(defun member-rewrite-rule-rune (rune lst)

; Lst is a list of :rewrite rules.  We determine whether there is a
; rule in lst with the :rune rune.

  (cond ((null lst) nil)
        ((equal rune (access rewrite-rule (car lst) :rune)) t)
        (t (member-rewrite-rule-rune rune (cdr lst)))))

(defun replace-rewrite-rule-rune (rune rule lst)

; Lst is a list of :rewrite rules and one with :rune rune is among them.
; We replace that rule with rule.

  (cond ((null lst) nil)
        ((equal rune (access rewrite-rule (car lst) :rune))
         (cons rule (cdr lst)))
        (t (cons (car lst) (replace-rewrite-rule-rune rune rule (cdr lst))))))

; We massage the hyps with this function to speed rewrite up.

(defun preprocess-hyp (hyp)

; In nqthm, this function also replaced (not (zerop x)) by
; ((numberp x) (not (equal x '0))).

  (case-match hyp
              (('atom x) (list (mcons-term* 'not (mcons-term* 'consp x))))
              (& (list hyp))))

(defun preprocess-hyps (hyps)
  (cond ((null hyps) nil)
        (t (append (preprocess-hyp (car hyps))
                   (preprocess-hyps (cdr hyps))))))

(defun add-definition-rule-with-ttree (rune nume clique controller-alist
                                            install-body term ens wrld ttree)

; We make a :rewrite rule of subtype 'definition (or 'abbreviation)
; and add it to the 'lemmas property of the appropriate fn.  This
; function is defined the way it is (namely, taking term as an arg and
; destructuring it rather than just taking term in pieces) because it
; is also used as the function for adding a user-supplied :REWRITE
; rule of subclass :DEFINITION.

  (mv-let
   (hyps equiv fn args body ttree)
   (destructure-definition term install-body ens wrld ttree)
   (let* ((vars-bag (all-vars-bag-lst args nil))
          (abbreviationp (and (null hyps)
                              (null clique)

; Rockwell Addition:  We have changed the notion of when a rule is an
; abbreviation.  Our new concern is with stobjs and lambdas.
 
; If fn returns a stobj, we don't consider it an abbreviation unless
; it contains no lambdas.  Thus, the updaters are abbreviations but
; lambda-nests built out of them are not.  We once tried the idea of
; letting a lambda in a function body disqualify the function as an
; abbreviation, but that made FLOOR no longer an abbreviation and some
; of the fp proofs failed.  So we made the question depend on stobjs
; for compatibility's sake.

                              (abbreviationp
                               (not (all-nils (stobjs-out fn wrld)))
                               vars-bag
                               body)))
          (rule
           (make rewrite-rule
                 :rune rune
                 :nume nume
                 :hyps (preprocess-hyps hyps)
                 :equiv equiv
                 :lhs (mcons-term fn args)
                 :free-varsp-lhs (not (null vars-bag))
                 :rhs body
                 :subclass (cond (abbreviationp 'abbreviation)
                                 (t 'definition))
                 :heuristic-info
                 (cond (abbreviationp nil)
                       (t (cons clique controller-alist)))

; Backchain-limit-lst does not make much sense for definitions.

                 :backchain-limit-lst nil)))
     (let ((wrld0 (if (eq fn 'hide)
                      wrld
                    (putprop fn 'lemmas
                             (cons rule (getprop fn 'lemmas nil
                                                 'current-acl2-world wrld))
                             wrld))))
       (cond (install-body
              (mv (putprop fn
                           'def-bodies
                           (cons (make def-body
                                       :nume nume
                                       :hyp (and hyps (conjoin hyps))
                                       :concl body
                                       :rune rune
                                       :formals args
                                       :recursivep clique
                                       :controller-alist controller-alist)
                                 (getprop fn 'def-bodies nil
                                          'current-acl2-world wrld))
                           wrld0)
                  ttree))
             (t (mv wrld0 ttree)))))))

(defun add-definition-rule (rune nume clique controller-alist install-body term
                                 ens wrld)
  (mv-let (wrld ttree)
          (add-definition-rule-with-ttree rune nume clique controller-alist
                                          install-body term ens wrld nil)
          (declare (ignore ttree))
          wrld))

#+:non-standard-analysis
(defun listof-standard-numberp-macro (lst)

; If the guard for standard-numberp is changed from t, consider changing
; the corresponding calls of mcons-term* to fcons-term*.

  (if (consp lst)
      (if (consp (cdr lst))
          (mcons-term*
           'if
           (mcons-term* 'standard-numberp (car lst))
           (listof-standard-numberp-macro (cdr lst))
           *nil*)
        (mcons-term* 'standard-numberp (car lst)))
    *t*))

(defun putprop-body-lst (names arglists bodies normalizeps
                               clique controller-alist
                               #+:non-standard-analysis std-p
                               ens wrld installed-wrld ttree)

#|

; Rockwell Addition:  A major change is the handling of PROG2$ and THE
; below.

; We store the body property for each name in names.  It is set to the
; normalized body.  Normalization expands some nonrecursive functions,
; namely those on *expandable-boot-strap-non-rec-fns*, which includes
; old favorites like EQ and ATOM.  In addition, we eliminate all the
; PROG2$s, MUST-BE-EQUALs and THEs from the body.  This can be seen as
; just an optimization of expanding nonrec fns.

; We add a definition rule equating the call of name with its
; normalized body.

; We store the unnormalized body under the property
; 'unnormalized-body.

; We return two results: the final wrld and a ttree justifying the
; normalization, which is an extension of the input ttree.

; Essay on the Normalization of Bodies

; We normalize the bodies of functions to speed up type-set
; and rewriting.  But there are some subtle issues here.  Let term be
; a term and let term' be its normalization.  We will ignore iff-flg
; and type-alist here.  First, we claim that term and term' are
; equivalent.  Thus, if we are allowed to add the axiom (fn x) = term
; then we may add (fn x) = term' too.  But while term and term' are
; equivalent they are not interchangeable from the perspective of
; defun processing.  For example, as nqthm taught us, the measure
; conjectures generated from term' may be inadequate to justify the
; admission of a function whose body is term.  A classic example is
; (fn x) = (if (fn x) t t), where the normalized body is just t.  The
; Hisorical Plaque below contains a proof that if (fn x) = term' is
; admissible then there exists one and only one function satisfying
; (fn x) = term.  Thus, while the latter definition may not actually
; be admissible it at least will not get us into trouble and in the
; end the issue vis-a-vis admissibility seems to be the technical one
; of exactly how we wish to define the Principle of Definition.

; Historical Plaque from Nqthm

; The following extensive comment used to guard the definition of
; DEFN0 in nqthm and is placed here partly as a nostalgic reminder of
; decades of work and partly because it has some good statistics in it
; that we might still want to look at.

;   This function is FUNCALLed and therefore may not be made a MACRO.

;   The list of comments on this function do not necessarily describe
;   the code below.  They have been left around in reverse chronology
;   order to remind us of the various combinations of preprocessing
;   we have tried.

;   If we ever get blown out of the water while normalizing IFs in a
;   large defn, read the following comment before abandoning
;   normalization.

;   18 August 1982.  Here we go again!  At the time of this writing
;   the preprocessing of defns is as follows, we compute the
;   induction and type info on the translated body and store under
;   sdefn the translated body.  This seems to slow down the system a
;   lot and we are going to change it so that we store under sdefn
;   the result of expanding boot strap nonrec fns and normalizing
;   IFs.  As nearly as we can tell from the comments below, we have
;   not previously tried this.  According to the record, we have
;   tried expanding all nonrec fns, and we have tried expanding boot
;   strap fns and doing a little normalization.  The data that
;   suggests this will speed things up is as follows.  Consider the
;   first call of SIMPLIFY-CLAUSE in the proof of PRIME-LIST-TIMES
;   -LIST.  The first three literals are trivial but the fourth call
;   of SIMPLIFY-CLAUSE1 is on (NOT (PRIME1 C (SUB1 C))).  With SDEFNs
;   not expanded and normalized -- i.e., under the processing as it
;   was immediately before the current change -- there are 2478 calls
;   of REWRITE and 273 calls of RELIEVE-HYPS for this literal.  With
;   all defns preprocessed as described here those counts drop to
;   1218 and 174.  On a sample of four theorems, PRIME-LIST-TIMES-
;   LIST, PRIME-LIST-PRIME-FACTORS, FALSIFY1-FALSIFIES, and ORDERED-
;   SORT, the use of normalized and expanded sdefns saves us 16
;   percent of the conses over the use of untouched sdefns, reducing
;   the cons counts for those theorems from 880K to 745K.  It seems
;   unlikely that this preprocessing will blow us out of the water on
;   large defns.  For the EV used in UNSOLV and for the 386L M with
;   subroutine call this new preprocessing only marginally increases
;   the size of the sdefn.  It would be interesting to see a function
;   that blows us out of the water.  When one is found perhaps the
;   right thing to do is to so preprocess small defns and leave big
;   ones alone.

;   17 December 1981.  Henceforth we will assume that the very body
;   the user supplies (modulo translation) is the body that the
;   theorem-prover uses to establish that there is one and only one
;   function satisfying the definition equation by determining that
;   the given body provides a method for computing just that
;   function.  This prohibits our "improving" the body of definitions
;   such as (f x) = (if (f x) a a) to (f x) = a.

;   18 November 1981.  We are sick of having to disable nonrec fns in
;   order to get large fns processed, e.g., the interpreter for our
;   386L class.  Thus, we have decided to adopt the policy of not
;   touching the user's typein except to TRANSLATE! it.  The
;   induction and type analysis as well as the final SDEFN are based
;   on the translated typein.

;   Before settling with the preprocessing used below we tried
;   several different combinations and did provealls.  The main issue
;   was whether we should normalize sdefns.  Unfortunately, the
;   incorporation of META0-LEMMAS was also being experimented with,
;   and so we do not have a precise breakdown of who is responsible
;   for what.  However, below we give the total stats for three
;   separate provealls.  The first, called 1PROVEALL, contained
;   exactly the code below -- except that the ADD-DCELL was given the
;   SDEFN with all the fn names replaced by *1*Fns instead of a fancy
;   TRANSLATE-TO-INTERLISP call.  Here are the 1PROVEALL stats.
;   Elapsed time = 9532.957, CPU time = 4513.88, GC time = 1423.261,
;   IO time = 499.894, CONSes consumed = 6331517.

;   We then incorporated META0-LEMMAS.  Simultaneously, we tried
;   running the RUN fns through DEFN and found that we exploded.  The
;   expansion of nonrec fns and the normalization of IFs before the
;   induction analysis transformed functions of CONS-COUNT 300 to
;   functions of CONS-COUNT exceeding 18K.  We therefore decided to
;   expand only BOOT-STRAP fns -- and not NORMALIZE-IFS for the
;   purposes of induction analysis.  After the induction and type
;   analyses were done, we put down an SDEFN with some trivial IF
;   simplification performed -- e.g., IF X Y Y => Y and IF bool T F
;   => bool -- but not a NORMALIZE-IFs version.  We then ran a
;   proveall with CANCEL around as a META0-LEMMA.  The result was
;   about 20 percent slower than the 1PROVEALL and used 15 percent
;   more CONSes.  At first this was attributed to CANCEL.  However,
;   we then ran two simultaneous provealls, one with META0-LEMMAS set
;   to NIL and one with it set to ((1CANCEL . CORRECTNESS-OF-CANCEL)).
;   The result was that the version with CANCEL available used
;   slightly fewer CONSes than the other one -- 7303311 to 7312505
;   That was surprising because the implementation of META0-LEMMAS
;   uses no CONSes if no META0-LEMMAS are available, so the entire 15
;   percent more CONSes had to be attributed to the difference in the
;   defn processing.  This simultaneous run was interesting for two
;   other reasons.  The times -- while still 20 percent worse than
;   1PROVEALL -- were one half of one percent different, with CANCEL
;   being the slower.  That means having CANCEL around does not cost
;   much at all -- and the figures are significant despite the slop
;   in the operating system's timing due to thrashing because the two
;   jobs really were running simultaneously.  The second interesting
;   fact is that CANCEL can be expected to save us a few CONSes
;   rather than cost us.

;   We therefore decided to return the DEFN0 processing to its
;   original state.  Only we did it in two steps.  First, we put
;   NORMALIZE-IFs into the pre-induction processing and into the
;   final SDEFN processing.  Here are the stats on the resulting
;   proveall, which was called PROVEALL-WITH-NORM-AND-CANCEL but not
;   saved.  Elapsed time = 14594.01, CPU time = 5024.387, GC time =
;   1519.932, IO time = 593.625, CONSes consumed = 6762620.

;   While an improvement, we were still 6 percent worse than
;   1PROVEALL on CONSes.  But the only difference between 1PROVEALL
;   and PROVEALL-WITH-NORM-AND-CANCEL -- if you discount CANCEL which
;   we rightly believed was paying for itself -- was that in the
;   former induction analyses and type prescriptions were being
;   computed from fully expanded bodies while in the latter they were
;   computed from only BOOT-STRAP-expanded bodies.  We did not
;   believe that would make a difference of over 400,000 CONSes, but
;   had nothing else to believe.  So we went to the current state,
;   where we do the induction and type analyses on the fully expanded
;   and normalized bodies -- bodies that blow us out of the water on
;   some of the RUN fns.  Here are the stats for
;   PROVEALL-PROOFS.79101, which was the proveall for that version.
;   Elapsed time = 21589.84, CPU time = 4870.231, GC time = 1512.813,
;   IO time = 554.292, CONSes consumed= 6356282.

;   Note that we are within 25K of the number of CONSes used by
;   1PROVEALL.  But to TRANSLATE-TO-INTERLISP all of the defns in
;   question costs 45K.  So -- as expected -- CANCEL actually saved
;   us a few CONSes by shortening proofs.  It takes only 18 seconds
;   to TRANSLATE-TO-INTERLISP the defns, so a similar argument does
;   not explain why the latter proveall is 360 seconds slower than
;   1PROVEALL.  But since the elapsed time is over twice as long, we
;   believe it is fair to chalk that time up to the usual slop
;   involved in measuring cpu time on a time sharing system.

;   We now explain the formal justification of the processing we do
;   on the body before testing it for admissibility.

;   We do not work with the body that is typed in by the user but
;   with an equivalent body' produced by normalization and the
;   expansion of nonrecursive function calls in body.  We now prove
;   that if (under no assumptions about NAME except that it is a
;   function symbol of the correct arity) (a) body is equivalent to
;   body' and (b) (name . args) = body' is accepted under our
;   principle of definition, then there exists exactly one function
;   satisfying the original equation (name . args) = body.

;   First observe that since the definition (name . args) = body' is
;   accepted by our principle of definition, there exists a function
;   satisfying that equation.  But the accepted equation is
;   equivalent to the equation (name .  args) = body by the
;   hypothesis that body is equivalent to body'.

;   We prove that there is only one such function by induction.
;   Assume that the definition (name . args) = body has been accepted
;   under the principle of definition.  Suppose that f is a new name
;   and that (f . args) = bodyf, where bodyf results from replacing
;   every use of name as a function symbol in body with f.  It
;   follows that (f . args) = bodyf', where bodyf' results from
;   replacing every use of name as a function symbol in body' with f.
;   We can now easily prove that (f . args) = (name . args) by
;   induction according to the definition of name. Q.E.D.

;   One might be tempted to think that if the defn with body' is
;   accepted under the principle of definition then so would be the
;   defn with body and that the use of body' was merely to make the
;   implementation of the defn principle more powerful.  This is not
;   the case.  For example

;        (R X) = (IF (R X) T T)

;   is not accepted by the definitional principle, but we would
;   accept the body'-version (R X) = T, and by our proof, that
;   function uniquely satisfies the equation the user typed in.

;   One might be further tempted to think that if we changed
;   normalize so that (IF X Y Y) = Y was not applied, then the two
;   versions were inter-acceptable under the defn principle.  This is
;   not the case either.  The function

;        (F X) = (IF (IF (X.ne.0) (F X-1) F) (F X-1) T)

;   is not accepted under the principle of defn.  Consider its
;   normalized body.

|#

  (cond ((null names) (mv wrld ttree))
        (t (let* ((fn (car names))
                  (args (car arglists))
                  (body (car bodies))
                  (normalizep (car normalizeps))
                  (rune (fn-rune-nume fn nil nil installed-wrld))
                  (nume (fn-rune-nume fn t nil installed-wrld)))
             (let* ((eqterm (fcons-term* 'equal
                                         (fcons-term fn args)
                                         body))
                    (term #+:non-standard-analysis
                          (if (and std-p (consp args))
                              (fcons-term*
                               'implies
                               (listof-standard-numberp-macro args)
                               eqterm)
                            eqterm)
                          #-:non-standard-analysis
                          eqterm)
                    #+:non-standard-analysis
                    (wrld (if std-p
                              (putprop fn 'constrainedp t wrld)
                            wrld)))
                (mv-let
                 (wrld ttree)
                 (add-definition-rule-with-ttree
                  rune nume clique controller-alist
                  (if normalizep :NORMALIZE t) ; install-body
                  term ens
                  (putprop fn
                           'unnormalized-body
                           body
                           wrld)
                  ttree)
                (putprop-body-lst (cdr names)
                                  (cdr arglists)
                                  (cdr bodies)
                                  (cdr normalizeps)
                                  clique controller-alist
                                  #+:non-standard-analysis std-p
                                  ens
                                  wrld installed-wrld ttree)))))))

; We now develop the facility for guessing the type-prescription of a defuned
; function.  When guards were part of the logic, the first step was to guess
; the types implied by the guard.  We no longer have to do that, but the
; utility written for it is used elsewhere and so we keep it here.

; Suppose you are trying to determine the type implied by term for some
; variable x.  The key trick is to normalize the term and replace every true
; output by x and every nil output by a term with an empty type-set.  Then take
; the type of that term.  For example, if term is (if (if p q) r nil) then it
; normalizes to (if p (if q (if r t nil) nil) nil) and so produces the
; intermediate term (if p (if q (if r x e ) e ) e ), where x is the formal in
; whose type we are interested and e is a new variable assumed to be of empty
; type.

(defun type-set-implied-by-term1 (term tvar fvar)

; Term is a normalized propositional term.  Tvar and fvar are two variable
; symbols.  We return a normalized term equivalent to (if term tvar fvar)
; except we drive tvar and fvar as deeply into term as possible.

  (cond ((variablep term)
         (fcons-term* 'if term tvar fvar))
        ((fquotep term)
         (if (equal term *nil*) fvar tvar))
        ((eq (ffn-symb term) 'if)
         (fcons-term* 'if
                      (fargn term 1)
                      (type-set-implied-by-term1 (fargn term 2) tvar fvar)
                      (type-set-implied-by-term1 (fargn term 3) tvar fvar)))
        (t 

; We handle all non-IF applications here, even lambda applications.
; Once upon a time we considered driving into the body of a lambda.
; But that introduces a free var in the body, namely fvar (or whatever
; the new variable symbol is) and there are no guarantees that type-set
; works on such a non-term.

           (fcons-term* 'if term tvar fvar))))

(defun type-set-implied-by-term (var not-flg term ens wrld ttree)

; Given a variable and a term, we determine a type set for the
; variable under the assumption that the term is non-nil.  If not-flg
; is t, we negate term before using it.  This function is not used in
; the guard processing but is needed in the compound-recognizer work.

; The ttree returned is 'assumption-free (provided the initial ttree
; is also).

  (let* ((new-var (genvar 'genvar "EMPTY" nil (all-vars term)))
         (type-alist (list (list* new-var *ts-empty* nil))))
    (mv-let (normal-term ttree)
            (normalize term t nil ens wrld ttree)
            (type-set
             (type-set-implied-by-term1 normal-term
                                        (if not-flg new-var var)
                                        (if not-flg var new-var))
             nil nil type-alist nil ens wrld ttree
             nil nil))))

(defun putprop-initial-type-prescriptions (names wrld)

; Suppose we have a clique of mutually recursive fns, names.  Suppose
; that we can recover from wrld both the formals and body of each
; name in names.

; This function adds to the front of each 'type-prescriptions property
; of the names in names an initial, empty guess at its
; type-prescription.  These initial rules are unsound and are only the
; starting point of our iterative guessing mechanism.  Oddly, the
; :rune and :nume of each rule is the same!  We use the
; *fake-rune-for-anonymous-enabled-rule* for the rune and the nume
; nil.  We could create the proper runes and numes (indeed, we did at
; one time) but those runes then find their way into the ttrees of the
; various guesses (and not just the rune of the function being typed
; but also the runes of its clique-mates).  By adopting this fake
; rune, we prevent that.

; The :term and :hyps we create for each rule are appropriate and survive into
; the final, accurate guess.  But the :basic-ts and :vars fields are initially
; empty here and are filled out by the iteration.

  (cond
   ((null names) wrld)
   (t (let ((fn (car names)))
        (putprop-initial-type-prescriptions
         (cdr names)
         (putprop fn
                  'type-prescriptions
                  (cons (make type-prescription
                              :rune *fake-rune-for-anonymous-enabled-rule*
                              :nume nil
                              :term (mcons-term fn (formals fn wrld))
                              :hyps nil
                              :basic-ts *ts-empty*
                              :vars nil
                              :corollary *t*)
                        (getprop fn
                                 'type-prescriptions
                                 nil
                                 'current-acl2-world
                                 wrld))
                  wrld))))))

; We now turn to the problem of iteratively guessing new
; type-prescriptions.  The root of this guessing process is the
; computation of the type-set and formals returned by a term.

(defun map-returned-formals-via-formals (formals pockets returned-formals)

; Formals is the formals list of a lambda expression, (lambda formals
; body).  Pockets is a list in 1:1 correspondence with formals.  Each
; pocket in pockets is a set of vars.  Finally, returned-formals is a
; subset of formals.  We return the set of vars obtained by unioning
; together the vars in those pockets corresponding to those in
; returned-formals.

; This odd little function is used to help determine the returned
; formals of a function defined in terms of a lambda-expression.
; Suppose foo is defined in terms of ((lambda formals body) arg1 ...
; argn) and we wish to determine the returned formals of that
; expression.  We first determine the returned formals in each of the
; argi.  That produces our pockets.  Then we determine the returned
; formals of body -- note however that the formals returned by body
; are not the formals of foo but the formals of the lambda.  The
; returned formals of body are our returned-formals.  This function
; can then be used to convert the returned formals of body into
; returned formals of foo.

  (cond ((null formals) nil)
        ((member-eq (car formals) returned-formals)
         (union-eq (car pockets)
                   (map-returned-formals-via-formals (cdr formals)
                                                     (cdr pockets)
                                                     returned-formals)))
        (t (map-returned-formals-via-formals (cdr formals)
                                             (cdr pockets)
                                             returned-formals))))

(defun map-type-sets-via-formals (formals ts-lst returned-formals)

; This is just like the function above except instead of dealing with
; a list of lists which are unioned together we deal with a list of
; type-sets which are ts-unioned.

  (cond ((null formals) *ts-empty*)
        ((member-eq (car formals) returned-formals)
         (ts-union (car ts-lst)
                   (map-type-sets-via-formals (cdr formals)
                                              (cdr ts-lst)
                                              returned-formals)))
        (t (map-type-sets-via-formals (cdr formals)
                                      (cdr ts-lst)
                                      returned-formals))))

(defun vector-ts-union (ts-lst1 ts-lst2)

; Given two lists of type-sets of equal lengths we ts-union
; corresponding elements and return the resulting list.

  (cond ((null ts-lst1) nil)
        (t (cons (ts-union (car ts-lst1) (car ts-lst2))
                 (vector-ts-union (cdr ts-lst1) (cdr ts-lst2))))))

(defun map-cons-tag-trees (lst ttree)

; Cons-tag-tree every element of lst into ttree.

  (cond ((null lst) ttree)
        (t (map-cons-tag-trees
            (cdr lst)
            (cons-tag-trees (car lst) ttree)))))

(defun type-set-and-returned-formals-with-rule1
  (alist rule-vars type-alist ens wrld basic-ts vars-ts vars ttree)

; See type-set-with-rule1 for a slightly simpler version of this.

; Note: This function is really just a loop that finishes off the
; computation done by type-set-and-returned-formals-with-rule, below.
; It would be best not to try to understand this function until you
; have read that function and type-set-and-returned-formals.

; Alist maps variables in a type-prescription to terms.  The context in which
; those terms occur is described by type-alist.  Rule-vars is the list of :vars
; of the rule.

; The last four arguments are accumulators that will become four of the
; answers delivered by type-set-and-returned-formals-with-rule, i.e.,
; a basic-ts, the type-set of a set of vars, the set of vars, and the
; justifying ttree.  We assemble these four answers by sweeping over
; alist, considering each var and its image term.  If the var is not
; in the rule-vars, we go on.  If the var is in the rule-vars, then
; its image is a possible value of the term for which we are computing
; a type-set.  If its image is a variable, we accumulate it and its
; type-set into vars and vars-ts.  If its image is not a variable, we
; accumulate its type-set into basic-ts.

; The ttree returned is 'assumption-free (provided the initial ttree
; is also).

  (cond
   ((null alist) (mv basic-ts vars-ts vars type-alist ttree))
   ((member-eq (caar alist) rule-vars)
    (mv-let (ts ttree)
            (type-set (cdar alist) nil nil type-alist nil ens wrld ttree
                      nil nil)
            (let ((variablep-image (variablep (cdar alist))))
              (type-set-and-returned-formals-with-rule1
               (cdr alist) rule-vars
               type-alist ens wrld
               (if variablep-image
                   basic-ts
                   (ts-union ts basic-ts))
               (if variablep-image
                   (ts-union ts vars-ts)
                   vars-ts)
               (if variablep-image
                   (add-to-set-eq (cdar alist) vars)
                   vars)
               ttree))))
   (t 
    (type-set-and-returned-formals-with-rule1
     (cdr alist) rule-vars
     type-alist ens wrld
     basic-ts
     vars-ts
     vars
     ttree))))

(defun type-set-and-returned-formals-with-rule
  (tp term type-alist ens wrld ttree)

; This function is patterned after type-set-with-rule, which the
; reader might understand first.

; The ttree returned is 'assumption-free (provided the initial ttree
; and type-alist are also).

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
        (basic-ts vars-ts vars type-alist ttree)
        (let ((type-alist (extend-type-alist-with-bindings unify-subst
                                                           nil
                                                           nil
                                                           type-alist
                                                           nil
                                                           ens wrld
                                                           nil
                                                           nil nil)))
          (mv-let
           (relieve-hyps-ans type-alist ttree)
           (type-set-relieve-hyps (access type-prescription tp :rune)
                                  term
                                  (access type-prescription tp :hyps)
                                  nil
                                  nil
                                  unify-subst
                                  type-alist
                                  nil ens wrld nil ttree
                                  nil nil)
           (cond
            (relieve-hyps-ans 

; Subject to the conditions in ttree, we now know that the type set of term is
; either in :basic-ts or else that term is equal to the image under unify-subst
; of some var in the :vars of the rule.  Our charter is to return five results:
; basic-ts, vars-ts, vars, type-alist and ttree.  We do that with the
; subroutine below.  It sweeps over the unify-subst, considering each vi and
; its image, ai.  If ai is a variable, then it accumulates ai into the returned
; vars (which is initially nil below) and the type-set of ai into vars-ts
; (which is initially *ts-empty* below).  If ai is not a variable, it
; accumulates the type-set of ai into basic-ts (which is initially :basic-ts
; below).

             (type-set-and-returned-formals-with-rule1
              unify-subst
              (access type-prescription tp :vars)
              type-alist ens wrld
              (access type-prescription tp :basic-ts)
              *ts-empty*
              nil
              (push-lemma
               (access type-prescription tp :rune)
               ttree)))
            (t

; We could not establish the hyps of the rule.  Thus, the rule tells us
; nothing about term.

             (mv *ts-unknown* *ts-empty* nil type-alist ttree)))))))
      (t 

; The :term of the rule does not unify with our term.

       (mv *ts-unknown* *ts-empty* nil type-alist ttree)))))
   (t 

; The rule is disabled.

      (mv *ts-unknown* *ts-empty* nil type-alist ttree))))

(defun type-set-and-returned-formals-with-rules
  (tp-lst term type-alist ens w ts vars-ts vars ttree)

; See type-set-with-rules for a simpler model of this function.  We
; try to apply each type-prescription in tp-lst, "conjoining" the
; results into an accumulating type-set, ts, and vars (and its
; associated type-set, vars-ts).  However, if a rule fails to change
; the accumulating answers, we ignore it.

; However, we cannot really conjoin two type-prescriptions and get a
; third.  We do, however, deduce a valid conclusion.  A rule
; essentially gives us a conclusion of the form (or basic-ts
; var-equations), where basic-ts is the proposition that the term is
; of one of several given types and var-equations is the proposition
; that the term is one of several given vars.  Two rules therefore
; tell us (or basic-ts1 var-equations1) and (or basic-ts2
; var-equations2).  Both of these propositions are true.  From them we
; deduce the truth
; (or (and basic-ts1 basic-ts2) 
;     (or var-equations1 var-equations2)).
; Note that we conjoin the basic type-sets but we disjoin the vars.  The
; validity of this conclusion follows from the tautology
; (implies (and (or basic-ts1 var-equations1)
;               (or basic-ts2 var-equations2))
;          (or (and basic-ts1 basic-ts2) 
;              (or var-equations1 var-equations2))).
; It would be nice if we could conjoin both sides, but that's not valid.

; Recall that we actually must also return the union of the type-sets
; of the returned vars.  Since the "conjunction" of two rules leads us
; to union the vars together we just union their types together too.

; The ttree returned is 'assumption free provided the initial ttree and
; type-alist are also.

  (cond
   ((null tp-lst)
    (mv-let
     (ts1 ttree1)
     (type-set term nil nil type-alist nil ens w ttree nil nil)
     (let ((ts2 (ts-intersection ts1 ts)))
       (mv ts2 vars-ts vars (if (ts= ts2 ts) ttree ttree1)))))
   (t (mv-let
       (ts1 vars-ts1 vars1 type-alist1 ttree1)
       (type-set-and-returned-formals-with-rule (car tp-lst) term
                                                type-alist ens w ttree)
       (let* ((ts2 (ts-intersection ts1 ts))
              (unchangedp (and (ts= ts2 ts)
                               (equal type-alist type-alist1))))

; If the type-set established by the new rule doesn't change (i.e.,
; narrow) what we already know, we simply choose to ignore the new
; rule.  If it does change, then ts2 is smaller and we have to union
; together what we know about the vars and report the bigger ttree.

         (type-set-and-returned-formals-with-rules
          (cdr tp-lst)
          term type-alist1 ens w
          ts2
          (if unchangedp
              vars-ts
              (ts-union vars-ts1 vars-ts))
          (if unchangedp
              vars
              (union-eq vars1 vars))
          (if unchangedp
              ttree
              ttree1)))))))

(mutual-recursion

(defun type-set-and-returned-formals (term type-alist ens wrld ttree)

; Term is the if-normalized body of a defined function.  The
; 'type-prescriptions property of that fn (and all of its peers in its mutually
; recursive clique) may or may not be nil.  If non-nil, it may contain many
; enabled rules.  (When guards were part of the logic, we computed the type-set
; of a newly defined function twice, once before and once after verifying its
; guards.  So during the second pass, a valid rule was present.)  Among the
; rules is one that is possibly unsound and represents our current guess at the
; type.  We compute, from that guess, a "basic type-set" for term and a list of
; formals that might be returned by term.  We also return the union of the
; type-sets of the returned formals and a ttree justifying all our work.  An
; odd aspect of this ttree is that it will probably include the rune of the
; very rule we are trying to create, since its use in this process is
; essentially as an induction hypothesis.

; Terminology: Consider a term and a type-alist, and the basic
; type-set and returned formals as computed here.  Let a "satisfying"
; instance of the term be an instance obtained by replacing each
; formal by an actual that has as its type-set a subtype of that of
; the corresponding formal under type-alist.  Let the "returned
; actuals" of such an instance be the actuals corresponding to
; returned formals.  We say the type set of such a satisfying instance
; of term is "described" by a basic type-set and some returned formals
; if the type-set of the instance is a subset of the union of the
; basic type-set and the type-sets of the returned actuals.  Claim:
; The type-set of a satisfying instance of term is given by our
; answer.

; This function returns four results.  The first is the basic type
; set computed.  The third is the set of returned formals.  The second
; one is the union of the type-sets of the returned formals.  Thus,
; the type-set of the term can in fact be obtained by unioning together
; the first and second answers.  However, top-level calls of this
; function are basically unconcerned with the second answer.  The fourth
; answer is a ttree justifying all the type-set reasoning done so far,
; accumulated onto the initial ttree.

; We claim that if our computation produces the type-set and formals
; that the type-prescription alleges, then the type-prescription is a
; correct one.

; The function works by walking through the if structure of the body,
; using the normal assume-true-false to construct the governing
; type-alist for each output branch.  Upon arriving at an output we
; compute the type set and returned formals for that branch.  If the
; output is a quote or a call to an ACL2 primitive, we just use
; type-set.  If the output is a call of a defun'd function, we
; interpret its type-prescription.

; The ttree returned is 'assumption-free provided the initial ttree
; and type-alist are also.

; Historical Plaque from Nqthm.

; In nqthm, the root of the guessing processing was DEFN-TYPE-SET,
; which was mutually recursive with DEFN-ASSUME-TRUE-FALSE.  The
; following comment could be found at the entrance to the guessing
; process:


;   ************************************************************* 
;   THIS FUNCTION WILL BE COMPLETELY UNSOUND IF TYPE-SET IS EVER
;   REACHABLE FROM WITHIN IT.  IN PARTICULAR, BOTH THE TYPE-ALIST AND
;   THE TYPE-PRESCRIPTION FOR THE FN BEING PROCESSED ARE SET TO ONLY
;   PARTIALLY ACCURATE VALUES AS THIS FN COMPUTES THE REAL TYPE-SET.
;   *************************************************************

; We now believe that this dreadful warning is an overstatement of the
; case.  It is true that in nqthm the type-alist used in DEFN-TYPE-SET
; would cause trouble if it found its way into TYPE-SET, because it
; bound vars to "defn type-sets" (pairs of type-sets and variables)
; instead of to type-sets.  But the fear of the inaccurate
; TYPE-PRESCRIPTIONs above is misplaced we think.  We believe that if
; one guesses a type-prescription and then confirms that it accurately
; describes the function body, then the type-prescription is correct.
; Therefore, in ACL2, far from fencing type-set away from
; "defun-type-set" we use it explicitly.  This has the wonderful
; advantage that we do not duplicate the type-set code (which is even
; worse in ACL2 than it was in nqthm).

  (cond
   ((variablep term)

; Term is a formal variable.  We compute its type-set under
; type-alist.  If it is completely unrestricted, then we will say that
; formal is sometimes returned.  Otherwise, we will say that it is not
; returned.  Once upon a time we always said it was returned.  But the
; term (if (integerp x) (if (< x 0) (- x) x) 0) as occurs in
; integer-abs, then got the type-set "nonnegative integer or x" which
; meant that it effectively had the type-set unknown.

; Observe that the code below satisfies our Claim.  If term' is a
; satisfying instance of this term, then we know that term' is in fact
; an actual being substituted for this formal.  Since term' is
; satisfying, the type-set of that actual (i.e., term') is a subtype
; of ts, below.  Thus, the type-set of term' is indeed described by
; our answer.

    (mv-let (ts ttree)
            (type-set term nil nil type-alist nil ens wrld ttree
                      nil nil)
            (cond ((ts= ts *ts-unknown*)
                   (mv *ts-empty* ts (list term) ttree))
                  (t (mv ts *ts-empty* nil ttree)))))

   ((fquotep term)

; Term is a constant.  We return a basic type-set consisting of the
; type-set of term.  Our Claim is true because the type-set of every
; instance of term is a subtype of the returned basic type-set is a
; subtype of the basic type-set.

    (mv-let (ts ttree)
            (type-set term nil nil type-alist nil ens wrld ttree
                      nil nil)
            (mv ts *ts-empty* nil ttree)))

   ((flambda-applicationp term)

; Without loss of generality we address ourselves to a special case.
; Let term be ((lambda (...u...) body) ...arg...).  Let the formals in
; term be x1, ..., xn.

; We compute a basic type-set, bts, some returned vars, vars, and the
; type-sets of the vars, vts, for a lambda application as follows.

; (1) For each argument, arg, obtain bts-arg, vts-arg, and vars-arg,
; which are the basic type-set, the variable type-set, and the
; returned variables with respect to the given type-alist.

; (2) Build a new type-alist, type-alist-body, by binding the formals
; of the lambda, (...u...), to the types of its arguments (...arg...).
; We know that the type of arg is the union of bts-arg and the types
; of those xi in vars-arg positions (which is to say, vts-arg).

; (3) Obtain bts-body, vts-body, and vars-body, by recursively
; processing body under type-alist-body.

; (4) Create the final bts by unioning bts-body and those of the
; bts-args in positions that are sometimes returned, as specified by
; vars-body.

; (5) Create the final vars by unioning together those of the
; vars-args in positions that are sometimes returned, as specified by
; vars-body.

; (6) Union together the types of the vars to create the final vts.

; We claim that the type-set of any instance of term that satisfies
; type-alist is described by the bts and vars computed above and that
; the vts computed above is the union of the the types of the vars
; computed.

; Now consider an instance, term', of term, in which the formals of
; term are mapped to some actuals and type-alist is satisfied.  Then
; the type-set of each actual is a subtype of the type assigned each
; xi.  Observe further that if term' is an instance of term satisfying
; type-alist then term' is ((lambda (...u...) body) ...arg'...), where
; arg' is an instance of arg satisfying type-alist.

; Thus, by induction, the type-set of arg' is a subtype of the union
; of bts-arg and the type-sets of those actuals in vars-arg positions.
; But the union of the type-sets of those actuals in vars-arg
; positions is a subtype of the union of the type-sets of the xi in
; vars-arg.  Also observe that term' is equal, by lambda expansion, to
; body', where body' is the instance of body in which each u is
; replaced by the corresponding arg'.  Note that body' is an instance
; of body satisfying type-alist-body: the type of arg' is a subtype of
; that assigned u in type-alist-body, because the type of arg' is a
; subtype of the union of bts-arg and the type-sets of the actuals in
; vars-arg positions, but the type assigned u in type-alist-body is
; the union of bts-arg and the type-sets of the xi in vars-arg.
; Therefore, by induction, we know that the type-set of body' is a
; subtype of bts-body and the type-sets of those arg' in vars-body
; positions.  But the type-set of each arg' is a subtype of bts-arg
; unioned with the type-sets of the actuals in vars-arg positions.
; Therefore, when we union over the selected arg' we get a subtype of
; the union of the union of the selected bts-args and the union of the
; type-sets of the actuals in vars positions.  By the associativity
; and commutativity of union, the bts and vars created in (4) and (5)
; are correct.

    (mv-let (bts-args vts-args vars-args ttree-args)
            (type-set-and-returned-formals-lst (fargs term)
                                               type-alist
                                               ens wrld)
            (mv-let (bts-body vts-body vars-body ttree)
                    (type-set-and-returned-formals
                     (lambda-body (ffn-symb term))
                     (zip-variable-type-alist
                      (lambda-formals (ffn-symb term))
                      (pairlis$ (vector-ts-union bts-args vts-args)
                                ttree-args))
                     ens wrld ttree)
                    (declare (ignore vts-body))
                    (let* ((bts (ts-union bts-body
                                          (map-type-sets-via-formals
                                           (lambda-formals (ffn-symb term))
                                           bts-args
                                           vars-body)))
                           (vars (map-returned-formals-via-formals
                                  (lambda-formals (ffn-symb term))
                                  vars-args
                                  vars-body))
                           (ts-and-ttree-lst
                            (type-set-lst vars nil nil type-alist nil ens wrld
                                          nil nil)))

; Below we make unconventional use of map-type-sets-via-formals.
; Its first and third arguments are equal and thus every element of
; its second argument will be ts-unioned into the answer.  This is
; just a hackish way to union together the type-sets of all the
; returned formals.

                      (mv bts
                          (map-type-sets-via-formals
                           vars
                           (strip-cars ts-and-ttree-lst)
                           vars)
                          vars
                          (map-cons-tag-trees (strip-cdrs ts-and-ttree-lst)
                                              ttree))))))
   ((eq (ffn-symb term) 'if)

; If by type-set reasoning we can see which way the test goes, we can
; clearly focus on that branch.  So now we consider (if t1 t2 t3) where
; we don't know which way t1 will go.  We compute the union of the
; respective components of the answers for t2 and t3.  In general, the
; type-set of any instance of this if will be at most the union of the
; type-sets of the instances of t2 and t3.  (In the instance, t1' might
; be decidable and a smaller type-set could be produced.)

    (mv-let
     (must-be-true
      must-be-false
      true-type-alist
      false-type-alist
      ts-ttree)
     (assume-true-false (fargn term 1)
                        nil nil nil type-alist nil ens wrld
                        nil nil nil)

; Observe that ts-ttree does not include ttree.  If must-be-true and
; must-be-false are both nil, ts-ttree is nil and can thus be ignored.

     (cond
      (must-be-true
       (type-set-and-returned-formals (fargn term 2)
                                      true-type-alist ens wrld
                                      (cons-tag-trees ts-ttree ttree)))
      (must-be-false
       (type-set-and-returned-formals (fargn term 3)
                                      false-type-alist ens wrld
                                      (cons-tag-trees ts-ttree ttree)))
      (t (mv-let
          (basic-ts2 formals-ts2 formals2 ttree)
          (type-set-and-returned-formals (fargn term 2)
                                         true-type-alist
                                         ens wrld ttree)
          (mv-let
           (basic-ts3 formals-ts3 formals3 ttree)
           (type-set-and-returned-formals (fargn term 3)
                                          false-type-alist
                                          ens wrld ttree)
           (mv (ts-union basic-ts2 basic-ts3)
               (ts-union formals-ts2 formals-ts3)
               (union-eq formals2 formals3)
               ttree)))))))
   (t
    (let* ((fn (ffn-symb term))
           (recog-tuple
            (most-recent-enabled-recog-tuple fn
                                             (global-val 'recognizer-alist wrld)
                                             ens)))
      (cond
       (recog-tuple
        (mv-let (ts ttree1)
                (type-set (fargn term 1) nil nil type-alist nil ens wrld ttree
                          nil nil)
                (mv-let (ts ttree)
                        (type-set-recognizer recog-tuple ts ttree1 ttree)
                        (mv ts *ts-empty* nil ttree))))
       (t
        (type-set-and-returned-formals-with-rules
         (getprop (ffn-symb term) 'type-prescriptions nil
                  'current-acl2-world wrld)
         term type-alist ens wrld
         *ts-unknown* *ts-empty* nil ttree)))))))

(defun type-set-and-returned-formals-lst
  (lst type-alist ens wrld)
  (cond
   ((null lst) (mv nil nil nil nil))
   (t (mv-let (basic-ts returned-formals-ts returned-formals ttree)
              (type-set-and-returned-formals (car lst)
                                             type-alist ens wrld nil)
              (mv-let (ans1 ans2 ans3 ans4)
                      (type-set-and-returned-formals-lst (cdr lst)
                                                         type-alist
                                                         ens wrld)
                      (mv (cons basic-ts ans1)
                          (cons returned-formals-ts ans2)
                          (cons returned-formals ans3)
                          (cons ttree ans4)))))))

)

(defun guess-type-prescription-for-fn-step (name body ens wrld ttree)

; This function takes one incremental step towards the type- prescription of
; name in wrld.  Body is the normalized body of name.  We assume that the
; current guess for a type-prescription for name is the car of the
; 'type-prescriptions property.  That is, initialization has occurred and every
; iteration keeps the current guess at the front of the list.

; We get the type-set of and formals returned by body.  We convert the two
; answers into a new type-prescription and replace the current car of the
; 'type-prescriptions property.

; We return the new world and an 'assumption-free ttree extending ttree.

  (let* ((ttree0 ttree)
         (old-type-prescriptions
          (getprop name 'type-prescriptions nil 'current-acl2-world wrld))
         (tp (car old-type-prescriptions)))
    (mv-let (new-basic-type-set returned-vars-type-set new-returned-vars ttree)
      (type-set-and-returned-formals body nil ens wrld ttree)
      (declare (ignore returned-vars-type-set))
      (cond ((ts= new-basic-type-set *ts-unknown*)

; Ultimately we will delete this rule.  But at the moment we wish merely to
; avoid contaminating the ttree of the ongoing process by whatever we've
; done to derive this.

             (mv (putprop name
                          'type-prescriptions
                          (cons (change type-prescription tp
                                        :basic-ts *ts-unknown*
                                        :vars nil)
                                (cdr old-type-prescriptions))
                          wrld)
                 ttree0))
            (t
             (mv (putprop name
                          'type-prescriptions
                          (cons (change type-prescription tp 
                                        :basic-ts new-basic-type-set
                                        :vars new-returned-vars)
                                (cdr old-type-prescriptions))
                          wrld)
                 ttree))))))

(defconst *clique-step-install-interval*

; This interval represents how many type prescriptions are computed before
; installing the resulting intermediate world.  The value below is merely
; heuristic, chosen with very little testing; we should feel free to change it.

  30)

(defun guess-and-putprop-type-prescription-lst-for-clique-step
  (names bodies ens wrld ttree interval state)

; Given a list of function names and their normalized bodies
; we take one incremental step toward the final type-prescription of
; each fn in the list.  We return a world containing the new
; type-prescription for each fn and a ttree extending ttree.

; Note: During the initial coding of ACL2 the iteration to guess
; type-prescriptions was slightly different from what it is now.  Back
; then we used wrld as the world in which we computed all the new
; type-prescriptions.  We returned those new type-prescriptions to our
; caller who determined whether the iteration had repeated.  If not,
; it installed the new type-prescriptions to generate a new wrld' and
; called us on that wrld'.

; It turns out that that iteration can loop indefinitely.  Consider the
; mutually recursive nest of foo and bar where
; (defun foo (x) (if (consp x) (not (bar (cdr x))) t))
; (defun bar (x) (if (consp x) (not (foo (cdr x))) nil))

; Below are the successive type-prescriptions under the old scheme:

; iteration    foo type      bar type
;   0             {}            {}
;   1             {T NIL}       {NIL}
;   2             {T}           {T NIL}
;   3             {T NIL}       {NIL}
;  ...            ...           ...

; Observe that the type of bar in round 1 is incomplete because it is
; based on the incomplete type of foo from round 0.  This kind of
; incompleteness is supposed to be closed off by the iteration.
; Indeed, in round 2 bar has got its complete type-set.  But the
; incompleteness has now been transferred to foo: the round 2
; type-prescription for foo is based on the incomplete round 1
; type-prescription of bar.  Isn't this an elegant example?

; The new iteration computes the type-prescriptions in a strict linear
; order.  So that the round 1 type-prescription of bar is based on the
; round 1 type-prescription of foo.

  (cond ((null names) (mv wrld ttree state))
        (t (mv-let
            (erp val state)
            (update-w (int= interval 0) wrld)
            (declare (ignore erp val))
            (mv-let
             (wrld ttree)
             (guess-type-prescription-for-fn-step
              (car names)
              (car bodies)
              ens wrld ttree)
             (guess-and-putprop-type-prescription-lst-for-clique-step
              (cdr names)
              (cdr bodies)
              ens
              wrld
              ttree
              (if (int= interval 0)
                  *clique-step-install-interval*
                (1- interval))
              state))))))

(defun cleanse-type-prescriptions
  (names type-prescriptions-lst def-nume rmp-cnt ens wrld installed-wrld ttree)

; Names is a clique of function symbols.  Type-prescriptions-lst is in
; 1:1 correspondence with names and gives the value in wrld of the
; 'type-prescriptions property for each name.  (We provide this just
; because our caller happens to be holding it.)  This function should
; be called when we have completed the guessing process for the
; type-prescriptions for names.  This function does two sanitary
; things: (a) it deletes the guessed rule if its :basic-ts is
; *ts-unknown*, and (b) in the case that the guessed
; rule is kept, it is given the rune and nume described by the Essay
; on the Assignment of Runes and Numes by DEFUNS.  It is assumed that
; def-nume is the nume of (:DEFINITION fn), where fn is the car of
; names.  We delete *ts-unknown* rules just to save type-set the
; trouble of relieving their hyps or skipping them.

; Rmp-cnt (which stands for "runic-mapping-pairs count") is the length of the
; 'runic-mapping-pairs entry for the functions in names (all of which have the
; same number of mapping pairs).  We increment our def-nume by rmp-cnt on each
; iteration.

; This function knows that the defun runes for each name are laid out
; as follows, where i is def-nume:

; i   (:definition name)                                   ^
; i+1 (:executable-counterpart name)
; i+2 (:type-prescription name)                       rmp-cnt=3 or 4
; i+4 (:induction name)                   ; optional       v

; Furthermore, we know that the nume of the :definition rune for the kth
; (0-based) name in names is def-nume+(k*rmp-cnt); that is, we assigned
; numes to the names in the same order as the names appear in names.

  (cond
   ((null names) (mv wrld ttree))
   (t (let* ((fn (car names))
             (lst (car type-prescriptions-lst))
             (new-tp (car lst)))
        (mv-let
         (wrld ttree1)
         (cond
          ((ts= *ts-unknown* (access type-prescription new-tp :basic-ts))
           (mv (putprop fn 'type-prescriptions (cdr lst) wrld) nil))
          (t (mv-let
              (corollary ttree1)
              (convert-type-prescription-to-term new-tp ens

; We use the installed world (the one before cleansing started) for efficient
; handling of large mutual recursion nests.

                                                 installed-wrld)
              (mv (putprop fn 'type-prescriptions
                           (cons (change type-prescription
                                         new-tp
                                         :rune (list :type-prescription
                                                     fn)
                                         :nume (+ 2 def-nume)
                                         :corollary corollary)
                                 (cdr lst))
                           wrld)
                  ttree1))))
         (cleanse-type-prescriptions (cdr names)
                                     (cdr type-prescriptions-lst)
                                     (+ rmp-cnt def-nume)
                                     rmp-cnt ens wrld installed-wrld
                                     (cons-tag-trees ttree1 ttree)))))))

(defun guess-and-putprop-type-prescription-lst-for-clique
  (names bodies def-nume ens wrld ttree big-mutrec state)

; We assume that in wrld we find 'type-prescriptions for every fn in
; names.  We compute new guesses at the type-prescriptions for each fn
; in names.  If they are all the same as the currently stored ones we
; quit.  Otherwise, we store the new guesses and iterate.  Actually,
; when we quit, we cleanse the 'type-prescriptions as described above.
; We return the final wrld and a ttree extending ttree.  Def-nume is
; the nume of (:DEFINITION fn), where fn is the first element of names
; and is used in the cleaning up to install the proper numes in the
; generated rules.

  (let ((old-type-prescriptions-lst
         (getprop-x-lst names 'type-prescriptions wrld)))
    (mv-let (wrld1 ttree state)
            (guess-and-putprop-type-prescription-lst-for-clique-step
             names bodies ens wrld ttree *clique-step-install-interval* state)
            (er-progn
             (update-w big-mutrec wrld1)
             (cond ((equal old-type-prescriptions-lst
                           (getprop-x-lst names 'type-prescriptions wrld1))
                    (mv-let
                     (wrld2 ttree)
                     (cleanse-type-prescriptions
                      names
                      old-type-prescriptions-lst
                      def-nume
                      (length (getprop (car names) 'runic-mapping-pairs nil
                                       'current-acl2-world wrld))
                      ens
                      wrld
                      wrld1
                      ttree)
                     (er-progn

; Warning:  Do not use set-w! here, because if we are in the middle of a
; top-level include-book, that will roll the world back to the start of that
; include-book.  We have found that re-installing the world omits inclusion of
; the compiled files for subsidiary include-books (see description of bug fix
; in :doc note-2-9 (bug fixes)).

                      (update-w big-mutrec wrld t)
                      (update-w big-mutrec wrld2)
                      (mv wrld2 ttree state))))
                   (t
                    (guess-and-putprop-type-prescription-lst-for-clique
                     names
                     bodies
                     def-nume ens wrld1 ttree big-mutrec state)))))))

(defun get-normalized-bodies (names wrld)

; Return the normalized bodies for names in wrld.

; WARNING: We ignore the runes and hyps for the normalized bodies returned.  So
; this function is probably only of interest when names are being introduced,
; where the 'def-bodies properties have been placed into wrld but no new
; :definition rules with non-nil :install-body fields have been proved for
; names.

  (cond ((endp names) nil)
        (t (cons (access def-body
                         (def-body (car names) wrld)
                         :concl)
                 (get-normalized-bodies (cdr names) wrld)))))

(defun putprop-type-prescription-lst (names def-nume ens wrld ttree state)

; Names is a list of mutually recursive fns being introduced.  We assume that
; for each fn in names we can obtain from wrld the 'formals and the normalized
; body (from 'def-bodies).  Def-nume must be the nume assigned (:DEFINITION
; fn), where fn is the first element of names.  See the Essay on the Assignment
; of Runes and Numes by DEFUNS.  We compute type-prescriptions for each fn in
; names and store them.  We return the new wrld and a ttree extending ttree
; justifying what we've done.

; This function knows that HIDE should not be given a
; 'type-prescriptions property.

; Historical Plaque for Versions Before 1.8

; In 1.8 we "eliminated guards from the ACL2 logic."  Prior to that guards were
; essentially hypotheses on the definitional equations.  This complicated many
; things, including the guessing of type-prescriptions.  After a function was
; known to be Common Lisp compliant we could recompute its type-prescription
; based on the fact that we knew that every subfunction in it would return its
; "expected" type.  Here is a comment from that era, preserved for posterity.

;   On Guards: In what way is the computed type-prescription influenced
;   by the changing of the 'guards-checked property from nil to t?

;   The key is illustrated by the following fact: type-set returns
;   *ts-unknown* if called on (+ x y) with gc-flg nil but returns a
;   subset of *ts-acl2-number* if called with gc-flg t.  To put this into
;   context, suppose that the guard for (fn x y) is (g x y) and that it
;   is not known by type-set that (g x y) implies that both x and y are
;   acl2-numberps.  Suppose the body of fn is (+ x y).  Then the initial
;   type-prescription for fn, computed when the 'guards-checked property
;   is nil, will have the basic-type-set *ts-unknown*.  After the guards
;   have been checked the basic type-set will be *ts-acl2-number*.

  (cond
   ((and (consp names)
         (eq (car names) 'hide)
         (null (cdr names)))
    (mv wrld ttree state))
   (t
    (let ((bodies (get-normalized-bodies names wrld))
          (big-mutrec (big-mutrec names)))
      (er-let*
       ((wrld1 (update-w big-mutrec
                         (putprop-initial-type-prescriptions names wrld))))
       (guess-and-putprop-type-prescription-lst-for-clique
        names
        bodies
        def-nume
        ens
        wrld1
        ttree
        big-mutrec
        state))))))

; So that finishes the type-prescription business.  Now to level-no...

(defun putprop-level-no-lst (names wrld)

; We compute the level-no properties for all the fns in names, assuming they
; have no such properties in wrld (i.e., we take advantage of the fact that
; when max-level-no sees a nil 'level-no it acts like it saw 0).  Note that
; induction and rewriting do not use heuristics for 'level-no, so it seems
; reasonable not to recompute the 'level-no property when adding a :definition
; rule with non-nil :install-body value.  We assume that we can get the
; 'recursivep and the 'def-bodies property of each fn in names from wrld.

  (cond ((null names) wrld)
        (t (let ((maximum (max-level-no (body (car names) t wrld) wrld)))
             (putprop-level-no-lst (cdr names)
                                   (putprop (car names)
                                            'level-no
                                            (if (getprop (car names)
                                                         'recursivep nil
                                                         'current-acl2-world
                                                         wrld)
                                                (1+ maximum)
                                              maximum)
                                            wrld))))))


; Next we put the primitive-recursive-defun property

(defun primitive-recursive-argp (var term wrld)

; Var is some formal of a recursively defined function.  Term is the actual in
; the var position in a recursive call in the definition of the function.
; I.e., we are recursively replacing var by term in the definition.  Is this
; recursion in the p.r. schema?  Well, that is impossible to tell by just
; looking at the recursion, because we need to know that the tests governing
; the recursion are also in the scheme.  In fact, we don't even check that; we
; just rely on the fact that the recursion was justified and so some governing
; test does the job.  So, ignoring tests, what is a p.r. function?  It is one
; in which every formal is replaced either by itself or by an application of a
; (nest of) primitive recursive destructors to itself.  The primitive recursive
; destructors recognized here are all unary function symbols with level-no 0
; (e.g., car, cdr, nqthm::sub1, etc) as well as terms of the form (+ & -n) and
; (+ -n &), where -n is negative.

; A consequence of this definition (before we turned 1+ into a macro) is that
; 1+ is a primitive recursive destructor!  Thus, the classic example of a
; terminating function not in the classic p.r. scheme,
; (fn x y) = (if (< x y) (fn (1+ x) y) 0)
; is now in the "p.r." scheme.  This is a crock!

; Where is this notion used?  The detection that a function is "p.r." is made
; after its admittance during the defun principle.  The appropriate flag is
; stored under the property 'primitive-recursive-defunp.  This property is only
; used (as of this writing) by induction-complexity1, where we favor induction
; candidates suggested by non-"p.r." functions.  Thus, the notion of "p.r." is
; entirely heuristic and only affects which inductions we choose.

; Why don't we define it correctly?  That is, why don't we only recognize
; functions that recurse via car, cdr, etc.?  The problem is the
; introduction of the "NQTHM" package, where we want NQTHM::SUB1 to be a p.r.
; destructor -- even in the defn of NQTHM::LESSP which must happen before we
; prove that NQTHM::SUB1 decreases according to NQTHM::LESSP.  The only way to
; fix this, it seems, would be to provide a world global variable -- perhaps a
; new field in the acl2-defaults-table -- to specify which function symbols are
; to be considered p.r. destructors.  We see nothing wrong with this solution,
; but it seems cumbersome at the moment.  Thus, we adopted this hackish notion
; of "p.r." and will revisit the problem if and when we see counterexamples to
; the induction choices caused by this notion.

  (cond ((variablep term) (eq var term))
        ((fquotep term) nil)
        (t (let ((fn (ffn-symb term)))
             (case
              fn
              (binary-+
               (or (and (nvariablep (fargn term 1))
                        (fquotep (fargn term 1))
                        (rationalp (cadr (fargn term 1)))
                        (< (cadr (fargn term 1)) 0)
                        (primitive-recursive-argp var (fargn term 2) wrld))
                   (and (nvariablep (fargn term 2))
                        (fquotep (fargn term 2))
                        (rationalp (cadr (fargn term 2)))
                        (< (cadr (fargn term 2)) 0)
                        (primitive-recursive-argp var (fargn term 1) wrld))))
              (otherwise
               (and (symbolp fn)
                    (fargs term)
                    (null (cdr (fargs term)))
                    (= (get-level-no fn wrld) 0)
                    (primitive-recursive-argp var (fargn term 1) wrld))))))))

(defun primitive-recursive-callp (formals args wrld)
  (cond ((null formals) t)
        (t (and (primitive-recursive-argp (car formals) (car args) wrld)
                (primitive-recursive-callp (cdr formals) (cdr args) wrld)))))

(defun primitive-recursive-callsp (formals calls wrld)
  (cond ((null calls) t)
        (t (and (primitive-recursive-callp formals (fargs (car calls)) wrld)
                (primitive-recursive-callsp formals (cdr calls) wrld)))))

(defun primitive-recursive-machinep (formals machine wrld)

; Machine is an induction machine for a singly recursive function with
; the given formals.  We return t iff every recursive call in the
; machine has the property that every argument is either equal to the
; corresponding formal or else is a primitive recursive destructor
; nest around that formal.

  (cond ((null machine) t)
        (t (and
            (primitive-recursive-callsp formals
                                        (access tests-and-calls
                                                (car machine)
                                                :calls)
                                        wrld)
            (primitive-recursive-machinep formals (cdr machine) wrld)))))

(defun putprop-primitive-recursive-defunp-lst (names wrld)

; The primitive-recursive-defun property of a function name indicates
; whether the function is defined in the primitive recursive schema --
; or, to be precise, in a manner suggestive of the p.r. schema.  We do
; not actually check for syntactic adherence to the rules and this
; property is of heuristic use only.  See the comment in
; primitive-recursive-argp.

; We say a defun'd function is p.r. iff it is not recursive, or else it
; is singly recursive and every argument position of every recursive call
; is occupied by the corresponding formal or else a nest of primitive
; recursive destructors around the corresponding formal.

; Observe that our notion doesn't include any inspection of the tests
; governing the recursions and it doesn't include any check of the
; subfunctions used.  E.g., the function that collects all the values of
; Ackerman's functions is p.r. if it recurses on cdr's.

  (cond ((null names) wrld)
        ((cdr names) wrld)
        ((primitive-recursive-machinep (formals (car names) wrld)
                                       (getprop (car names)
                                                'induction-machine nil
                                                'current-acl2-world wrld)
                                       wrld)
         (putprop (car names)
                  'primitive-recursive-defunp
                  t
                  wrld))
        (t wrld)))

; Onward toward defuns...  Now we generate the controller-alists.

(defun make-controller-pocket (formals vars)

; Given the formals of a fn and a measured subset, vars, of formals,
; we generate a controller-pocket for it.  A controller pocket is a
; list of t's and nil's in 1:1 correspondence with the formals, with
; t in the measured slots.

  (cond ((null formals) nil)
        (t (cons (if (member (car formals) vars)
                     t
                     nil)
                 (make-controller-pocket (cdr formals) vars)))))

(defun make-controller-alist1 (names wrld)

; Given a clique of recursive functions, we return the controller alist built
; for the 'justification.  A controller alist is an alist that maps fns in the
; clique to controller pockets.  The controller pockets describe the measured
; arguments in a justification.  We assume that all the fns in the clique have
; been justified (else none would be justified).

; This function should not be called on a clique consisting of a single,
; non-recursive fn (because it has no justification).

  (cond ((null names) nil)
        (t (cons (cons (car names)
                       (make-controller-pocket
                        (formals (car names) wrld)
                        (access justification
                                (getprop (car names)
                                         'justification
                                         '(:error
                                           "See MAKE-CONTROLLER-ALIST1.")
                                         'current-acl2-world wrld)
                                :subset)))
                 (make-controller-alist1 (cdr names) wrld)))))

(defun make-controller-alist (names wrld)

; We store a controller-alists property for every recursive fn in names.  We
; assume we can get the 'formals and the 'justification for each fn from wrld.
; If there is a fn with no 'justification, it means the clique consists of a
; single non-recursive fn and we store no controller-alists.  We generate one
; controller pocket for each fn in names.

; The controller-alist associates a fn in the clique to a controller pocket.  A
; controller pocket is a list in 1:1 correspondence with the formals of the fn
; with a t in slots that are controllers.  The controllers assigned for the fns
; in the clique by a given controller-alist were used jointly in the
; justification of the clique.

  (and (getprop (car names) 'justification nil 'current-acl2-world wrld)
       (make-controller-alist1 names wrld)))

(defun max-nume-exceeded-error (ctx)
  (er hard ctx
      "ACL2 assumes that no nume exceeds ~x0.  It is very surprising that ~
       this bound is about to be exceeded.  We are causing an error because ~
       for efficiency, ACL2 assumes this bound is never exceeded.  Please ~
       contact the ACL2 implementors with a request that this assumption be ~
       removed from enabled-numep."
      (fixnum-bound)))

(defun putprop-defun-runic-mapping-pairs1 (names def-nume tp-flg ind-flg wrld)

; Names is a list of function symbols.  For each fn in names we store some
; runic mapping pairs.  We always create (:DEFINITION fn) and (:EXECUTABLE-
; COUNTERPART fn).  If tp-flg is t, we create (:TYPE-PRESCRIPTION fn).  If
; ind-flg is t we create (:INDUCTION fn).  However, ind-flg is t only if tp-flg
; is t (that is, tp-flg = nil and ind-flg = t never arises).  Thus, we may
; store 2 (tp-flg = nil; ind-flg = nil), 3 (tp-flg = t; ind-flg = nil), or 4
; (tp-flg = t; ind-flg = t) runes.  As of this writing, we never call this
; function with tp-flg nil but ind-flg t and the function is not prepared for
; that possibility.

; WARNING: Don't change the layout of the runic-mapping-pairs without
; considering all the places that talk about the Essay on the Assignment of
; Runes and Numes by DEFUNS.

  (cond ((null names) wrld)
        (t (putprop-defun-runic-mapping-pairs1
            (cdr names)
            (+ 2 (if tp-flg 1 0) (if ind-flg 1 0) def-nume)
            tp-flg
            ind-flg
            (putprop
             (car names) 'runic-mapping-pairs
             (list* (cons def-nume (list :DEFINITION (car names)))
                    (cons (+ 1 def-nume)
                          (list :EXECUTABLE-COUNTERPART (car names)))
                    (if tp-flg
                        (list* (cons (+ 2 def-nume)
                                     (list :TYPE-PRESCRIPTION (car names)))
                               (if ind-flg
                                   (list (cons (+ 3 def-nume)
                                               (list :INDUCTION (car names))))
                                 nil))
                      nil))
             wrld)))))

(defun putprop-defun-runic-mapping-pairs (names tp-flg wrld)

; Essay on the Assignment of Runes and Numes by DEFUNS

; Names is a clique of mutually recursive function names.  For each
; name in names we store a 'runic-mapping-pairs property.  Each name
; gets either four (tp-flg = t) or two (tp-flg = nil) mapping pairs:

; ((n   . (:definition name))
;  (n+1 . (:executable-counterpart name))
;  (n+2 . (:type-prescription name))        ; only if tp-flg
;  (n+3 . (:induction name)))               ; only if tp-flg and name is
;                                           ;  recursively defined

; where n is the next available nume.  Important aspects to this
; include:
; * Fn-rune-nume knows where the :definition and :executable-counterpart
;   runes are positioned.
; * Several functions (e.g. augment-runic-theory) exploit the fact
;   that the mapping pairs are ordered ascending.
; * function-theory-fn1 knows that if the token of the first rune in
;   the 'runic-mapping-pairs is not :DEFINITION then the base symbol
;   is not a function symbol.
; * Get-next-nume implicitly exploits the fact that the numes are
;   consecutive integers -- it adds the length of the list to
;   the first nume to get the next available nume.
; * Cleanse-type-prescriptions knows that the same number of numes are 
;   consumed by each function in a DEFUNS.  We have consistently used
;   the formal parameter def-nume when we were enumerating numes for
;   definitions.
; * Convert-theory-to-unordered-mapping-pairs1 knows that if the first rune in
;   the list is a :definition rune, then the length of this list is 4 if and
;   only if the list contains an :induction rune, in which case that rune is
;   last in the list.

; In short, don't change the layout of this property unless you
; inspect every occurrence of 'runic-mapping-pairs in the system!
; (Even that won't find the def-nume uses.)  Of special note is the
; fact that all non-constrained function symbols are presumed to have
; the same layout of 'runic-mapping-pairs as shown here.  Constrained
; symbols have a nil 'runic-mapping-pairs property.

; We do not allocate the :type-prescription or :induction runes or their numes
; unless tp-flg is non-nil.  This way we can use this same function to
; initialize the 'runic-mapping-pairs for primitives like car and cdr, without
; wasting runes and numes.  We like reusing this function for that purpose
; because it isolates the place we create the 'runic-mapping-pairs for
; functions.

  (let ((next-nume (get-next-nume wrld)))
    (prog2$ (or (<= (the-fixnum next-nume)
                    (- (the-fixnum (fixnum-bound))
                       (the-fixnum (* (the-fixnum 4)
                                      (the-fixnum (length names))))))
                (max-nume-exceeded-error 'putprop-defun-runic-mapping-pairs))
            (putprop-defun-runic-mapping-pairs1
             names
             next-nume
             tp-flg
             (and tp-flg
                  (getprop (car names) 'recursivep nil 'current-acl2-world wrld))
             wrld))))

; Before completing the implementation of defun we turn to the implementation
; of the verify-guards event.  The idea is that one calls (verify-guards name)
; and we will generate the guard conditions for all the functions in the
; mutually recursive clique with name, prove them, and then exploit those
; proofs by resetting their symbol-classs.  This process is optionally available
; as part of the defun event and hence we must define it before defun.

; While reading this code it is best to think of ourselves as having completed
; defun.  Imagine a wrld in which a defun has just been done: the
; 'unnormalized-body is b, the unnormalized 'guard is g, the 'symbol-class is
; :ideal.  The user then calls (verify-guards name) and we want to prove that
; every guard encountered in the mutually recursive clique containing name is
; satisfied.

; We have to collect every subroutine mentioned by any member of the clique and
; check that its guards have been checked.  We cause an error if not.  Once we
; have checked that all the subroutines have had their guards checked, we
; generate the guard clauses for the new functions.

(defun guard-clauses-for-body (hyp-segments body stobj-optp wrld ttree)

; Hyp-segments is a list of clauses derived from the guard for body.  We
; generate the guard clauses for the unguarded body, body, under each of the
; different hyp segments.  We return a clause set and a ttree justifying all
; the simplification and extending ttree.

  (cond
   ((null hyp-segments) (mv nil ttree))
   (t (mv-let
       (cl-set1 ttree)
       (guard-clauses body stobj-optp (car hyp-segments) wrld ttree)
       (mv-let
        (cl-set2 ttree)
        (guard-clauses-for-body (cdr hyp-segments)
                                body
                                stobj-optp
                                wrld ttree)
        (mv (conjoin-clause-sets cl-set1 cl-set2) ttree))))))

(defun guard-clauses-for-fn (name ens wrld state ttree)

; Given a function name we generate the clauses that establish that
; all the guards in both the unnormalized guard and unnormalized body are
; satisfied.  While processing the guard we assume nothing.  But we
; generate the guards for the unnormalized body under each of the
; possible guard-hyp-segments derived from the assumption of the
; normalized 'guard.  We return the resulting clause set and an extension
; of ttree justifying it.  The resulting ttree is 'assumption-free,
; provided the initial ttree is also.

; Notice that in the two calls of guard below, used while computing
; the guard conjectures for the guard of name itself, we use stobj-opt
; = nil.

  (mv-let
   (cl-set1 ttree)
   (guard-clauses (guard name nil wrld)
                  nil nil wrld ttree)
   (mv-let
    (normal-guard ttree)
    (normalize (guard name nil wrld)
               t ; iff-flg
               nil ; type-alist
               ens wrld ttree)
    (mv-let
     (changedp body ttree)
     (eval-ground-subexpressions
      (getprop name 'unnormalized-body
               '(:error "See GUARD-CLAUSES-FOR-FN.")
               'current-acl2-world wrld)
      ens wrld state ttree)
     (declare (ignore changedp))
     (mv-let
      (cl-set2 ttree)

; Should we expand lambdas here?  I say ``yes,'' but only to be
; conservative with old code.  Perhaps we should change the t to nil?

      (guard-clauses-for-body (clausify (dumb-negate-lit normal-guard)
                                        nil t wrld)
                              body

; Observe that when we generate the guard clauses for the body we optimize
; the stobj recognizers away, provided the named function is executable.

                              (not (getprop name 'non-executablep nil
                                            'current-acl2-world wrld))
                              wrld ttree)
      (mv (conjoin-clause-sets cl-set1 cl-set2) ttree))))))

(defun guard-clauses-for-clique (names ens wrld state ttree)

; Given a mutually recursive clique of fns we generate all of the
; guard conditions for every function in the clique and return that
; set of clauses and a ttree extending ttree and justifying its
; construction.  The resulting ttree is 'assumption-free, provided the
; initial ttree is also.

  (cond ((null names) (mv nil ttree))
        (t (mv-let
            (cl-set1 ttree)
            (guard-clauses-for-fn (car names) ens wrld state ttree)
            (mv-let
             (cl-set2 ttree)
             (guard-clauses-for-clique (cdr names) ens wrld state ttree)
             (mv (conjoin-clause-sets cl-set1 cl-set2) ttree))))))

; That completes the generation of the guard clauses.  We will prove
; them with prove.

(defun print-verify-guards-msg (names col state)

; Note that names is either a singleton list containing a theorem name
; or is a mutually recursive clique of function names.

; This function increments timers.  Upon entry, the accumulated time
; is charged to 'other-time.  The time spent in this function is
; charged to 'print-time.

  (cond
   ((ld-skip-proofsp state) state)
   (t
    (pprogn
     (increment-timer 'other-time state)
     (mv-let (col state)
             (io? event nil (mv col state)
                  (col names)
                  (fmt1 "~&0 ~#0~[is~/are~] compliant with Common Lisp.~|"
                        (list (cons #\0 names))
                        col
                        (proofs-co state)
                        state nil)
                  :default-bindings ((col 0)))
             (declare (ignore col))
             (increment-timer 'print-time state))))))

(defun collect-ideals (names wrld acc)
  (cond ((null names) acc)
        ((eq (symbol-class (car names) wrld) :ideal)
         (collect-ideals (cdr names) wrld (cons (car names) acc)))
        (t (collect-ideals (cdr names) wrld acc))))

(defun collect-non-ideals (names wrld)
  (cond ((null names) nil)
        ((eq (symbol-class (car names) wrld) :ideal)
         (collect-non-ideals (cdr names) wrld))
        (t (cons (car names) (collect-non-ideals (cdr names) wrld)))))

(defun collect-non-common-lisp-compliants (names wrld)
  (cond ((null names) nil)
        ((eq (symbol-class (car names) wrld) :common-lisp-compliant)
         (collect-non-common-lisp-compliants (cdr names) wrld))
        (t (cons (car names)
                 (collect-non-common-lisp-compliants (cdr names) wrld)))))

(defun all-fnnames1-mbe-exec (flg x acc)

; Keep this in sync with all-fnnames1.

  (cond (flg ; x is a list of terms
         (cond ((null x) acc)
               (t (all-fnnames1-mbe-exec nil (car x)
                                (all-fnnames1-mbe-exec t (cdr x) acc)))))
        ((variablep x) acc)
        ((fquotep x) acc)
        ((flambda-applicationp x)
         (all-fnnames1-mbe-exec nil (lambda-body (ffn-symb x))
                       (all-fnnames1-mbe-exec t (fargs x) acc)))
        ((eq (ffn-symb x) 'must-be-equal)
         (all-fnnames1-mbe-exec nil (fargn x 2) acc))
        (t
         (all-fnnames1-mbe-exec t (fargs x)
                       (add-to-set-eq (ffn-symb x) acc)))))

(defmacro all-fnnames-mbe-exec (term)
  `(all-fnnames1-mbe-exec nil ,term nil))

(defun chk-common-lisp-compliant-subfunctions
  (names0 names terms wrld str ctx state)

; Assume we are defining (or have defined) names in terms of terms
; (1:1 correspondence).  We wish to make the definitions
; :common-lisp-compliant.  Then we insist that every function used in
; terms (other than names0) be :common-lisp-compliant.  Str is a
; string used in our error message and is "guard", "body" or
; "auxiliary function".  Note that this function is used by
; chk-acceptable-defuns and by chk-acceptable-verify-guards and
; chk-stobj-field-descriptor.  In the first usage, names have not been
; defined yet; in the other two they have.  So be careful about using
; wrld to get properties of names.

  (cond ((null names) (value nil))
        (t (let ((bad (collect-non-common-lisp-compliants
                       (set-difference-eq (all-fnnames-mbe-exec (car terms))
                                          names0)
                       wrld)))
             (cond
              (bad
               (er soft ctx
                   "The ~@0 for ~x1 calls the function~#2~[ ~&2~/s ~&2~], the ~
                    guards of which have not yet been verified.  See :DOC ~
                    verify-guards."
                   str (car names) bad))
              (t (chk-common-lisp-compliant-subfunctions
                  names0 (cdr names) (cdr terms)
                  wrld str ctx state)))))))

(defun chk-acceptable-verify-guards (name ctx wrld state)

; We check that name is acceptable input for verify-guards.  We return either
; the list of names in the clique of name (if name and every peer in the clique
; is :ideal and every subroutine of every peer is :common-lisp-compliant), the
; symbol 'redundant (if name and every peer is :common-lisp-compliant), or
; cause an error.

; One might wonder when two peers in a clique can have different symbol-classs,
; e.g., how is it possible (as implied above) for name to be :ideal but for one
; of its peers to be :common-lisp-compliant or :program?  Redefinition.  For
; example, the clique could have been admitted as :logic and then later one
; function in it redefined as :program.  Because redefinition invalidates the
; system, we could do anything in this case.  What we choose to do is to cause
; an error and say you can't verify the guards of any of the functions in the
; nest.

  (cond
   ((not (symbolp name))
    (er soft ctx
        "Verify-guards can only be applied to a name theorem or function ~
         symbol.  ~x0 is neither.  See :DOC verify-guards."
        name))
   ((getprop name 'theorem nil 'current-acl2-world wrld)

; Theorems are of either symbol-class :ideal or :common-lisp-compliant.

    (mv-let (erp term state)
            (translate (getprop name 'untranslated-theorem nil
                                'current-acl2-world wrld)
                       t t t ctx wrld state)
; known-stobjs = t (stobjs-out = t)
            (declare (ignore term))
            (cond
             (erp
              (er soft ctx
                  "The guards for ~x0 cannot be verified because the theorem ~
                   has the wrong syntactic form.  See :DOC verify-guards."
                  name))
             (t
              (value (if (eq (symbol-class name wrld) :ideal)
                         (list name)
                       'redundant))))))
   ((function-symbolp name wrld)
    (let ((symbol-class (symbol-class name wrld)))
      (case symbol-class
        (:program
         (er soft ctx
             "~x0 is :program.  Only :logic functions can have their guards ~
              verified.  See :DOC verify-guards."
             name))
        (:ideal
         (let* ((recp (getprop name 'recursivep nil
                               'current-acl2-world wrld))
                (names (cond
                        ((null recp)
                         (list name))
                        (t recp)))
                (non-ideal-names (collect-non-ideals names wrld)))
           (cond (non-ideal-names
                  (er soft ctx
                      "One or more of the mutually-recursive peers of ~
                       ~x0 either was not defined in :logic mode or ~
                       has already had its guards verified.  The ~
                       offending function~#1~[ is~/s are~] ~&1.  We ~
                       thus cannot verify the guards of ~x0.  This ~
                       situation can arise only through redefinition."
                      name
                      non-ideal-names))
                 (t
                  (er-progn
                   (chk-common-lisp-compliant-subfunctions
                    names names
                    (guard-lst names nil wrld)
                    wrld "guard" ctx state)
                   (chk-common-lisp-compliant-subfunctions
                    names names
                    (getprop-x-lst names 'unnormalized-body wrld)
                    wrld "body" ctx state)
                   (value names))))))
        (:common-lisp-compliant
         (let ((recp (getprop name 'recursivep nil
                              'current-acl2-world wrld)))
           (cond
            ((null recp) (value 'redundant))
            ((null (cdr recp)) (value 'redundant))
            (t (let ((non-common-lisp-compliant-names
                      (collect-non-common-lisp-compliants recp wrld)))
                 (cond ((null non-common-lisp-compliant-names)
                        (value 'redundant))
                       (t (er soft ctx
                              "While the guards for ~x0 have already been ~
                               verified, those for ~&1, which ~#1~[is~/are~] ~
                               mutually-recursive peers of ~x0, have not been ~
                               verified.  This can only happen through ~
                               redefinition."
                              name
                              non-common-lisp-compliant-names))))))))
        (otherwise
         (er soft ctx
             "The argument to VERIFY-GUARDS must be the name of a function ~
              defined in :logic mode.  ~x0 is thus illegal.  See :DOC ~
              verify-guards."
             name)))))
   (t (er soft ctx
          "~x0 is not a theorem name or a function symbol in the current ACL2 ~
           world.  See :DOC verify-guards."
          name))))

(defun prove-guard-clauses (names hints otf-flg ctx ens wrld state)

; Names is either a clique of mutually recursive functions or else a singleton
; list containing a theorem name.  We generate and attempt to prove the guard
; conjectures for the formulas in names.  We generate suitable output
; explaining what we are doing.  This is an error/value/state producing
; function that returns a pair of the form (col . ttree) when non-erroneous.
; Col is the column in which the printer is left.  We always output something
; and we always leave the printer reader to start a new sentence.  Ttree is a
; tag tree describing the proof.

; This function increments timers.  Upon entry, any accumulated time
; is charged to 'other-time.  The printing done herein is charged
; to 'print-time and the proving is charged to 'prove-time.

  (mv-let
   (cl-set cl-set-ttree state)
   (cond ((not (ld-skip-proofsp state))
          (pprogn
           (io? event nil state
                (names)
                (fms "Computing the guard conjecture for ~&0....~|"
                     (list (cons #\0 names))
                     (proofs-co state)
                     state
                     nil))
           (cond ((and (consp names)
                       (null (cdr names))
                       (getprop (car names) 'theorem nil
                                'current-acl2-world wrld))
                  (mv-let (cl-set cl-set-ttree)
                          (guard-clauses
                           (getprop (car names) 'theorem nil
                                    'current-acl2-world wrld)
                           nil ;stobj-optp = nil
                           nil wrld nil)
                          (mv cl-set cl-set-ttree state)))
                 (t (mv-let
                     (erp pair state)
                     (state-global-let*
                      ((guard-checking-on :all))
                      (mv-let (cl-set cl-set-ttree)
                              (guard-clauses-for-clique names ens wrld state
                                                        nil)
                              (value (cons cl-set cl-set-ttree))))
                     (declare (ignore erp))
                     (mv (car pair) (cdr pair) state))))))
         (t (mv nil nil state)))

; Cl-set-ttree is 'assumption-free.

   (mv-let
    (cl-set cl-set-ttree)
    (clean-up-clause-set cl-set ens wrld cl-set-ttree state)

; Cl-set-ttree is still 'assumption-free.

    (pprogn
     (increment-timer 'other-time state)
     (let ((displayed-goal (prettyify-clause-set cl-set
                                                 (let*-abstractionp state)
                                                 wrld))
           (simp-phrase (tilde-*-simp-phrase cl-set-ttree)))
       (cond
        ((ld-skip-proofsp state) (value '(0 . nil)))
        ((null cl-set)
         (mv-let (col state)
                 (io? event nil (mv col state)
                      (names simp-phrase)
                      (fmt "The guard conjecture for ~&0 is trivial to ~
                            prove~#1~[~/, given ~*2~].  "
                           (list (cons #\0 names)
                                 (cons #\1 (if (nth 4 simp-phrase) 1 0))
                                 (cons #\2 simp-phrase))
                           (proofs-co state)
                           state
                           nil)
                      :default-bindings ((col 0)))
                 (pprogn
                  (increment-timer 'print-time state)
                  (value (cons (or col 0) cl-set-ttree)))))
        (t
         (pprogn
          (io? event nil state
               (displayed-goal simp-phrase names)
               (fms "The non-trivial part of the guard conjecture for ~
                     ~&0~#1~[~/, given ~*2,~] is~%~%Goal~%~q3."
                    (list (cons #\0 names)
                          (cons #\1 (if (nth 4 simp-phrase) 1 0))
                          (cons #\2 simp-phrase)
                          (cons #\3 displayed-goal))
                    (proofs-co state)
                    state
                    nil))
          (increment-timer 'print-time state)
          (mv-let (erp ttree state)
                  (prove (termify-clause-set cl-set)
                         (make-pspv ens wrld
                                    :displayed-goal displayed-goal
                                    :otf-flg otf-flg)
                         hints
                         ens wrld ctx state)
                  (cond
                   (erp
                    (er soft ctx
                        "The proof of the guard conjecture for ~&0 has ~
                         failed.~|"
                        names))
                   (t
                    (mv-let (col state)
                            (io? event nil (mv col state)
                                 (names)
                                 (fmt "That completes the proof of the guard ~
                                       theorem for ~&0.  "
                                      (list (cons #\0 names))
                                      (proofs-co state)
                                      state
                                      nil)
                                 :default-bindings ((col 0)))
                            (pprogn
                             (increment-timer 'print-time state)
                             (value
                              (cons (or col 0)
                                    (cons-tag-trees cl-set-ttree
                                                    ttree))))))))))))))))

(defun verify-guards-fn1 (names hints otf-flg ctx state)

; This function is called on a clique of mutually recursively defined
; fns whose guards have not yet been verified.  Hints is a properly
; translated hints list.  This is an error/value/state producing
; function.  We cause an error if some subroutine of names has not yet
; had its guards checked or if we cannot prove the guards.  Otherwise,
; the "value" is a pair of the form (wrld .  ttree), where wrld results
; from storing symbol-class :common-lisp-compliant for each name and
; ttree is the ttree proving the guards.

; Note: In a series of conversations started around 13 Jun 94, with Bishop
; Brock, we came up with a new proposal for the form of guard conjectures.
; However, we have decided to delay the experiementation with this proposal
; until we evaluate the new logic of Version 1.8.  But, the basic idea is this.
; Consider two functions, f and g, with guards a and b, respectively.  Suppose
; (f (g x)) occurs in a context governed by q.  Then the current guard
; conjectures are
; (1) q -> (b x)      ; guard for g holds on x
; (2) q -> (a (g x))  ; guard for f holds on (g x)

; Note that (2) contains (g x) and we might need to know that x satisfies the
; guard for g here.  Another way of putting it is that if we have to prove both
; (1) and (2) we might imagine forward chaining through (1) and reformulate (2)
; as (2') q & (b x) -> (a (g x)).

; Now in the days when guards were part of the logic, this was a pretty
; compelling idea because we couldn't get at the definition of (g x) in (2)
; without establisthing (b x) and thus formulation (2) forced us to prove
; (1) all over again during the proof of (2).  But it is not clear whether
; we care now, because the smart user will define (g x) to "do the right thing"
; for any x and thus f will approve of (g x).  So it is our expectation that
; this whole issue will fall by the wayside.  It is our utter conviction of
; this that leads us to write this note.  Just in case...

#|
++++++++++++++++++++++++++++++

Date: Sun, 2 Oct 94 17:31:10 CDT
From: kaufmann (Matt Kaufmann)
To: moore
Subject: proposal for handling generalized booleans

Here's a pretty simple idea, I think, for handling generalized Booleans.  For
the rest of this message I'll assume that we are going to implement the
about-to-be-proposed handling of guards.  This proposal doesn't address
functions like member, which could be thought of as returning generalized
booleans but in fact are completely specified (when their guards are met).
Rather, the problem we need to solve is that certain functions, including EQUAL
and RATIONALP, only specify the propositional equivalence class of the value
returned, and no more.  I'll call these "problematic functions" for the rest of
this note.

The fundamental ideas of this proposal are  as follows.

====================

 (A) Problematic functions are completely a non-issue except for guard
verification.  The ACL2 logic specifies Boolean values for functions that are
specified in dpANS to return generalized Booleans.

 (B) Guard verification will generate not only the current proof obligations,
but also appropriate proof obligations to show that for all values returned by
relevant problematic functions, only their propositional equivalence class
matters.  More on this later.

 (C) If a function is problematic, it had better only be used in propositional
contexts when used in functions or theorems that are intended to be
:common-lisp-compliant.  For example, consider the following.

 (defun foo (x y z)
  (if x
      (equal y z)
    (cons y z)))

This is problematic, and we will never be able to use it in a
:common-lisp-compliant function or formula for other than its propositional
value (unfortunately).

====================

Elaborating on (B) above:

So for example, if we're verifying guards on

 (... (foo (rationalp x) ...) ...)

then there will be a proof obligation to show that under the appropriate
hypotheses (from governing IF tests),

 (implies (and a b)
         (equal (foo a ...) (foo b ...)))

Notice that I've assumed that a and b are non-NIL.  The other case, where a and
b are both NIL, is trivial since in that case a and b are equal.

Finally, most of the time no such proof obligation will be generated, because
the context will make it clear that only the propositional equivalence class
matters.  In fact, for each function we'll store information that gives
``propositional arguments'' of the function:  arguments for which we can be
sure that only their propositional value matters.  More on this below.

====================

Here are details.

====================

1. Every function will have a ``propositional signature,'' which is a list of
T's and NIL's.  The CAR of this list is T when the function is problematic.
The CDR of the list is in 1-1 correspondence with the function's formals (in
the same order, of course), and indicates whether the formal's value only
matters propositionally for the value of the function.

For example, the function

 (defun bar (x y z)
  (if x
      (equal y z)
    (equal y nil)))

has a propositional signature of (T T NIL NIL).  The first T represents the
fact that this function is problematic.  The second T represents the fact that
only the propositional equivalence class of X is used to compute the value of
this function.  The two NILs say that Y and Z may have their values used other
than propositionally.

An argument that corresponds to a value of T will be called a ``propositional
argument'' henceforth.  An OBSERVATION will be made any time a function is
given a propositional signature that isn't composed entirely of NILs.

 (2) Propositional signatures will be assigned as follows, presumably hung on
the 'propositional-signature property of the function.  We intend to ensure
that if a function is problematic, then the CAR of its propositional signature
is T.  The converse could fail, but it won't in practice.

a. The primitives will have their values set using a fixed alist kept in sync
with *primitive-formals-and-guards*, e.g.:

 (defconst *primitive-propositional-signatures*
  '((equal (t nil nil))
    (cons (nil nil nil))
    (rationalp (t nil))
    ...))

In particular, IF has propositional signature (NIL T NIL NIL):  although IF is
not problematic, it is interesting to note that its first argument is a
propositional argument.

b. Defined functions will have their propositional signatures computed as
follows.

b1. The CAR is T if and only if some leaf of the IF-tree of the body is the
call of a problematic function.  For recursive functions, the function itself
is considered not to be problematic for the purposes of this algorithm.

b2. An argument, arg, corresponds to T (i.e., is a propositional argument in
the sense defined above) if and only if for every subterm for which arg is an
argument of a function call, arg is a propositional argument of that function.

Actually, for recursive functions this algorithm is iterative, like the type
prescription algorithm, in the sense that we start by assuming that every
argument is propositional and iterate, continuing to cut down the set of
propositional arguments until it stabilizes.

Consider for example:

 (defun atom-listp (lst)
  (cond ((atom lst) (eq lst nil))
        (t (and (atom (car lst))
                (atom-listp (cdr lst))))))

Since EQ returns a generalized Boolean, ATOM-LISTP is problematic.  Since
the first argument of EQ is not propositional, ATOM-LISTP has propositional
signature (T NIL).

Note however that we may want to replace many such calls of EQ as follows,
since dpANS says that NULL really does return a Boolean [I guess because it's
sort of synonymous with NOT]:

 (defun atom-listp (lst)
  (cond ((atom lst) (null lst))
        (t (and (atom (car lst))
                (atom-listp (cdr lst))))))

Now this function is not problematic, even though one might be nervous because
ATOM is, in fact, problematic.  However, ATOM is in the test of an IF (because
of how AND is defined).  Nevertheless, the use of ATOM here is of issue, and
this leads us to the next item.

 (3) Certain functions are worse than merely problematic, in that their value
may not even be determined up to propositional equivalence class.  Consider for
example our old favorite:

 (defun bad (x)
  (equal (equal x x) (equal x x)))

In this case, we can't really say anything at all about the value of BAD, ever.

So, every function is checked that calls of problematic functions in its body
only occur either at the top-level of its IF structure or in propositional
argument positions.  This check is done after the computation described in (2)b
above.

So, the second version of the definition of ATOM-LISTP above,

 (defun atom-listp (lst)
  (cond ((atom lst) (null lst))
        (t (and (atom (car lst))
                (atom-listp (cdr lst))))))

is OK in this sense, because both calls of ATOM occur in the first argument of
an IF call, and the first argument of IF is propositional.

Functions that fail this check are perfectly OK as :ideal functions; they just
can't be :common-lisp-compliant.  So perhaps they should generate a warning
when submitted as :ideal, pointing out that they can never be
:common-lisp-compliant.

-- Matt

|#

  (let ((wrld (w state))
        (ens (ens state)))
    (er-let*
     ((pair (prove-guard-clauses names hints otf-flg ctx ens wrld state)))

; Pair is of the form (col . ttree)

     (let* ((col (car pair))
            (ttree1 (cdr pair))
            (wrld1 (putprop-x-lst1 names 'symbol-class
                                   :common-lisp-compliant wrld)))
       (pprogn 
        (print-verify-guards-msg names col state)
        (value (cons wrld1 ttree1)))))))

(defun append-lst (lst)
  (cond ((null lst) nil)
        (t (append (car lst) (append-lst (cdr lst))))))

(defun verify-guards-fn (name state hints otf-flg doc event-form)

; Important Note:  Don't change the formals of this function without
; reading the *initial-event-defmacros* discussion in axioms.lisp.

  (when-logic
   "VERIFY-GUARDS"
   (with-ctx-summarized
    (if (output-in-infixp state)
        event-form
        (cond ((and (null hints)
                    (null otf-flg)
                    (null doc))
               (msg "( VERIFY-GUARDS ~x0)"
                    name))
              (t (cons 'verify-guards name))))
    (let ((wrld (w state))
          (event-form (or event-form
                          (list* 'verify-guards
                                 name
                                 (append
                                  (if hints
                                      (list :hints hints)
                                    nil)
                                  (if otf-flg
                                      (list :otf-flg otf-flg)
                                    nil)
                                  (if doc
                                      (list :doc doc)
                                    nil)))))
          (assumep (or (eq (ld-skip-proofsp state) 'include-book)
                       (eq (ld-skip-proofsp state) 'include-book-with-locals)
                       (eq (ld-skip-proofsp state) 'initialize-acl2))))
      (er-let*
       ((names (chk-acceptable-verify-guards name ctx wrld state)))
       (cond
        ((eq names 'redundant)
         (stop-redundant-event state))
        (t (enforce-redundancy
            event-form ctx wrld
            (er-let*
             ((hints (if assumep
                         (value nil)
                       (translate-hints
                        (cons "Guard Lemma for" name)
                        (append hints (default-hints wrld))
                        ctx wrld state)))
              (doc-pair (translate-doc nil doc ctx state))

; Doc-pair is guaranteed to be nil because of the nil name supplied to
; translate-doc.

              (pair (verify-guards-fn1 names hints otf-flg ctx state)))

; pair is of the form (wrld1 . ttree)

             (er-progn
              (chk-assumption-free-ttree (cdr pair) ctx state)
              (install-event name
                             event-form
                             'verify-guards
                             0
                             (cdr pair)
                             nil
                             nil
                             nil
                             (car pair)
                             state)))))))))))

; That completes the implementation of verify-guards.  We now return
; to the development of defun itself.

; Here is the short-cut used when we are introducing :program functions.
; The super-defun-wart operations are not so much concerned with the
; :program defun-mode as with system functions that need special treatment.

; The wonderful super-defun-wart operations should not, in general, mess with
; the primitive state accessors and updaters.  They have to do with a
; boot-strapping problem that is described in more detail in STATE-STATE in
; axioms.lisp.

; The following table has gives the proper STOBJS-IN and STOBJS-OUT
; settings for the indicated functions.

; Warning: If you ever change this table so that it talks about stobjs other
; than STATE, then reconsider oneify-cltl-code.  These functions assume that if
; stobjs-in from this table is non-nil then special handling of STATE is
; required; or, at least, they did before Version_2.6.

(defconst *super-defun-wart-table*

;         fn                     stobjs-in       stobjs-out

  '((COERCE-STATE-TO-OBJECT      (STATE)         (NIL))
    (COERCE-OBJECT-TO-STATE      (NIL)           (STATE))
    (USER-STOBJ-ALIST            (STATE)         (NIL))
    (UPDATE-USER-STOBJ-ALIST     (NIL STATE)     (STATE))
    (BIG-CLOCK-NEGATIVE-P        (STATE)         (NIL)) 
    (DECREMENT-BIG-CLOCK         (STATE)         (STATE)) 
    (STATE-P                     (STATE)         (NIL)) 
    (OPEN-INPUT-CHANNEL-P        (NIL NIL STATE) (NIL)) 
    (OPEN-OUTPUT-CHANNEL-P       (NIL NIL STATE) (NIL)) 
    (OPEN-INPUT-CHANNEL-ANY-P    (NIL STATE)     (NIL)) 
    (OPEN-OUTPUT-CHANNEL-ANY-P   (NIL STATE)     (NIL)) 
    (READ-CHAR$                  (NIL STATE)     (NIL STATE)) 
    (PEEK-CHAR$                  (NIL STATE)     (NIL)) 
    (READ-BYTE$                  (NIL STATE)     (NIL STATE)) 
    (READ-OBJECT                 (NIL STATE)     (NIL NIL STATE)) 
    (READ-ACL2-ORACLE            (STATE)         (NIL NIL STATE)) 
    (READ-RUN-TIME               (STATE)         (NIL STATE)) 
    (READ-IDATE                  (STATE)         (NIL STATE)) 
    (LIST-ALL-PACKAGE-NAMES      (STATE)         (NIL STATE)) 
    (PRINC$                      (NIL NIL STATE) (STATE)) 
    (WRITE-BYTE$                 (NIL NIL STATE) (STATE)) 
    (PRINT-OBJECT$               (NIL NIL STATE) (STATE)) 
    (GET-GLOBAL                  (NIL STATE)     (NIL)) 
    (BOUNDP-GLOBAL               (NIL STATE)     (NIL)) 
    (MAKUNBOUND-GLOBAL           (NIL STATE)     (STATE)) 
    (PUT-GLOBAL                  (NIL NIL STATE) (STATE)) 
    (GLOBAL-TABLE-CARS           (STATE)         (NIL)) 
    (T-STACK-LENGTH              (STATE)         (NIL)) 
    (EXTEND-T-STACK              (NIL NIL STATE) (STATE)) 
    (SHRINK-T-STACK              (NIL STATE)     (STATE)) 
    (AREF-T-STACK                (NIL STATE)     (NIL)) 
    (ASET-T-STACK                (NIL NIL STATE) (STATE)) 
    (32-BIT-INTEGER-STACK-LENGTH (STATE)         (NIL)) 
    (EXTEND-32-BIT-INTEGER-STACK (NIL NIL STATE) (STATE)) 
    (SHRINK-32-BIT-INTEGER-STACK (NIL STATE)     (STATE)) 
    (AREF-32-BIT-INTEGER-STACK   (NIL STATE)     (NIL)) 
    (ASET-32-BIT-INTEGER-STACK   (NIL NIL STATE) (STATE)) 
    (OPEN-INPUT-CHANNEL          (NIL NIL STATE) (NIL STATE)) 
    (OPEN-OUTPUT-CHANNEL         (NIL NIL STATE) (NIL STATE)) 
    (CLOSE-INPUT-CHANNEL         (NIL STATE)     (STATE)) 
    (CLOSE-OUTPUT-CHANNEL        (NIL STATE)     (STATE))
    (SYS-CALL-STATUS             (STATE)         (NIL STATE))))

(defun project-out-columns-i-and-j (i j table)
  (cond
   ((null table) nil)
   (t (cons (cons (nth i (car table)) (nth j (car table)))
            (project-out-columns-i-and-j i j (cdr table))))))

(defconst *super-defun-wart-binding-alist*
  (project-out-columns-i-and-j 0 2 *super-defun-wart-table*))

(defconst *super-defun-wart-stobjs-in-alist*
  (project-out-columns-i-and-j 0 1 *super-defun-wart-table*))

(defun super-defun-wart-bindings (bindings)
  (cond ((null bindings) nil)
        (t (cons (or (assoc-eq (caar bindings)
                               *super-defun-wart-binding-alist*)
                     (car bindings))
                 (super-defun-wart-bindings (cdr bindings))))))

(defun store-stobjs-ins (names stobjs-ins w)
  (cond ((null names) w)
        (t (store-stobjs-ins (cdr names) (cdr stobjs-ins)
                             (putprop (car names) 'stobjs-in
                                      (car stobjs-ins) w)))))

(defun store-super-defun-warts-stobjs-in (names wrld)

; Store the built-in stobjs-in values of the super defuns among names, if any.

  (cond
   ((null names) wrld)
   ((assoc-eq (car names) *super-defun-wart-stobjs-in-alist*)
    (store-super-defun-warts-stobjs-in
     (cdr names)
     (putprop (car names) 'stobjs-in
              (cdr (assoc-eq (car names) *super-defun-wart-stobjs-in-alist*))
              wrld)))
   (t (store-super-defun-warts-stobjs-in (cdr names) wrld))))

(defun collect-old-nameps (names wrld)
  (cond ((null names) nil)
        ((new-namep (car names) wrld)
         (collect-old-nameps (cdr names) wrld))
        (t (cons (car names) (collect-old-nameps (cdr names) wrld)))))

(defun defuns-fn-short-cut (names docs pairs guards bodies
                                  wrld state)

; This function is called by defuns-fn when the functions to be defined are
; :program.  It short cuts the normal put-induction-info and other such
; analysis of defuns.  The function essentially makes the named functions look
; like primitives in the sense that they can be used in formulas and they can
; be evaluated on explicit constants but no axioms or rules are available about
; them.  In particular, we do not store 'def-bodies, type-prescriptions, or
; any of the recursion/induction properties normally associated with defuns and
; the prover will not execute them on explicit constants.

; We do take care of the documentation data base.

; Like defuns-fn0, this function returns a pair consisting of the new world and
; a tag tree recording the proofs that were done.

  (let ((wrld (update-doc-data-base-lst
               names docs pairs
               (putprop-x-lst2-unless
                names 'guard guards *t*
                (putprop-x-lst1
                 names 'symbol-class :program
                 (if (global-val 'boot-strap-flg wrld)
                     wrld
                   (putprop-x-lst2 names 'unnormalized-body bodies wrld)))))))
    (value (cons wrld nil))))

; Now we develop the output for the defun event.

(defun print-defun-msg/collect-type-prescriptions (names wrld)

; This function returns two lists, a list of names in names with
; trivial type-prescriptions (i.e., NIL 'type-prescriptions property)
; and an alist that pairs names in names with the term representing
; their (non-trivial) type prescriptions.

  (cond
   ((null names) (mv nil nil))
   (t (mv-let (fns alist)
              (print-defun-msg/collect-type-prescriptions (cdr names) wrld)
              (let ((lst (getprop (car names) 'type-prescriptions nil
                                  'current-acl2-world wrld)))
                (cond
                 ((null lst)
                  (mv (cons (car names) fns) alist))
                 (t (mv fns
                        (cons
                         (cons (car names)
                               (untranslate
                                (access type-prescription (car lst) :corollary)
                                t wrld))
                         alist)))))))))

(defun print-defun-msg/type-prescriptions1 (alist simp-phrase col state)

; See print-defun-msg/type-prescriptions.  We print out a string of
; phrases explaining the alist produced above.  We return the final
; col and state.  This function used to be a tilde-* phrase, but
; you cannot get the punctuation after the ~xt commands.

  (cond ((null alist) (mv col state))
        ((null (cdr alist))
         (fmt1 "the type of ~xn is described by the theorem ~pt.  ~#p~[~/We ~
                used ~*s.~]~|"
               (list (cons #\n (caar alist))
                     (cons #\t (cdar alist))
                     (cons #\p (if (nth 4 simp-phrase) 1 0))
                     (cons #\s simp-phrase))
               col
               (proofs-co state)
               state nil))
        ((null (cddr alist))
         (fmt1 "the type of ~xn is described by the theorem ~pt ~
                and the type of ~xm is described by the theorem ~ps.~|"
               (list (cons #\n (caar alist))
                     (cons #\t (cdar alist))
                     (cons #\m (caadr alist))
                     (cons #\s (cdadr alist)))
               col
               (proofs-co state)
               state nil))
        (t
         (mv-let (col state)
                 (fmt1 "the type of ~xn is described by the theorem ~pt, "
                       (list (cons #\n (caar alist))
                             (cons #\t (cdar alist)))
                       col
                       (proofs-co state)
                       state nil)
                 (print-defun-msg/type-prescriptions1 (cdr alist) simp-phrase
                                                      col state)))))

(defun print-defun-msg/type-prescriptions (names ttree wrld col state)

; This function prints a description of each non-trivial
; type-prescription for the functions names.  It assumes that at the
; time it is called, it is printing in col.  It returns the final col,
; and the final state.

  (let ((simp-phrase (tilde-*-simp-phrase ttree)))
    (mv-let
      (fns alist)
      (print-defun-msg/collect-type-prescriptions names wrld)
      (cond
       ((null alist)
        (fmt1
         "We could deduce no constraints on the type of ~#0~[~&0.~/any of the ~
          functions in the clique.~]~#1~[~/  However, in normalizing the ~
          definition~#0~[~/s~] we used ~*2.~]~%"
         (list (cons #\0 names)
               (cons #\1 (if (nth 4 simp-phrase) 1 0))
               (cons #\2 simp-phrase))
         col
         (proofs-co state)
         state nil))
       (fns
        (mv-let
          (col state)
          (fmt1
           "We could deduce no constraints on the type of ~#f~[~vf,~/any of ~
           ~vf,~] but we do observe that "
           (list (cons #\f fns))
           col
           (proofs-co state)
           state nil)
          (print-defun-msg/type-prescriptions1 alist simp-phrase col state)))
       (t
        (mv-let
          (col state)
          (fmt1
           "We observe that " nil col (proofs-co state)
           state nil)
          (print-defun-msg/type-prescriptions1 alist simp-phrase
                                               col state)))))))

(defun simple-signaturep (fn wrld)

; A simple signature is one in which no stobjs are involved and the
; output is a single value.

  (and (all-nils (stobjs-in fn wrld))
       (null (cdr (stobjs-out fn wrld)))))

(defun all-simple-signaturesp (names wrld)
  (cond ((endp names) t)
        (t (and (simple-signaturep (car names) wrld)
                (all-simple-signaturesp (cdr names) wrld)))))

(defun print-defun-msg/signatures1 (names wrld state)
  (cond
   ((endp names) state)
   ((not (simple-signaturep (car names) wrld))
    (pprogn
     (fms "~x0 => ~x1."
          (list
           (cons #\0
                 (cons (car names)
                       (prettyify-stobj-flags (stobjs-in (car names) wrld))))
           (cons #\1 (prettyify-stobjs-out (stobjs-out (car names) wrld))))
          (proofs-co state)
          state
          nil)
     (print-defun-msg/signatures1 (cdr names) wrld state)))
   (t (print-defun-msg/signatures1 (cdr names) wrld state))))

(defun print-defun-msg/signatures (names wrld state)
  (cond ((all-simple-signaturesp names wrld)
         state)
        ((cdr names)
         (pprogn
          (fms "The Non-simple Signatures" nil (proofs-co state) state nil)
          (print-defun-msg/signatures1 names wrld state)
          (newline (proofs-co state) state)))
        (t (pprogn
            (print-defun-msg/signatures1 names wrld state)
            (newline (proofs-co state) state)))))
               

(defun print-defun-msg (names ttree wrld col state)

; Once upon a time this function printed more than just the type
; prescription message.  We've left the function here to handle that
; possibility in the future.  This function returns the final state.

; This function increments timers.  Upon entry, the accumulated time
; is charged to 'other-time.  The time spent in this function is
; charged to 'print-time.

  (cond ((ld-skip-proofsp state)
         state)
        (t
         (io? event nil state
              (names ttree wrld col)
              (pprogn
               (increment-timer 'other-time state)
               (mv-let (erp ttree state)
                 (accumulate-ttree-into-state ttree state)
                 (declare (ignore erp))
                 (mv-let (col state)
                   (print-defun-msg/type-prescriptions names ttree
                                                       wrld col state)
                   (declare (ignore col))
                   (pprogn
                    (print-defun-msg/signatures names wrld state)
                    (increment-timer 'print-time state)))))))))

(defun get-ignores (lst)
  (cond ((null lst) nil)
        (t (cons (ignore-vars
                  (fourth (car lst)))
                 (get-ignores (cdr lst))))))

(defun get-ignorables (lst)
  (cond ((null lst) nil)
        (t (cons (ignorable-vars
                  (fourth (car lst)))
                 (get-ignorables (cdr lst))))))

(defun chk-all-stobj-names (lst msg ctx wrld state)

; Cause an error if any element of lst is not a legal stobj name in wrld.

  (cond ((endp lst) (value nil))
        ((not (stobjp (car lst) t wrld))
         (er soft ctx
             "Every name used as a stobj (whether declared explicitly ~
              via the :STOBJ keyword argument or implicitly via ~
              *-notation) must have been previously defined as a ~
              single-threaded object with defstobj.  ~x0 is used as ~
              stobj name ~#1~[~/in ~@1 ~]but has not been defined as ~
              a stobj."
             (car lst)
             msg))
        (t (chk-all-stobj-names (cdr lst) msg ctx wrld state))))

(defun get-declared-stobj-names (edcls ctx wrld state)

; Each element of edcls is the cdr of a DECLARE form.  We look for the
; ones of the form (XARGS ...) and find the first :stobjs keyword
; value in each such xargs.  We know there is at most one :stobjs
; occurrence in each xargs by chk-dcl-lst.  We union together all the
; values of that keyword, after checking that each value is legal.  We
; return the list of declared stobj names or cause an error.

; Keep this in sync with get-declared-stobjs (which does not do any checking
; and returns a single value).

  (cond ((endp edcls) (value nil))
        ((eq (caar edcls) 'xargs)
         (let* ((temp (assoc-keyword :stobjs (cdar edcls)))
                (lst (cond ((null temp) nil)
                           ((null (cadr temp)) nil)
                           ((atom (cadr temp))
                            (list (cadr temp)))
                           (t (cadr temp)))))
           (cond
            (lst
             (cond
              ((not (symbol-listp lst))
               (er soft ctx
                   "The value specified for the :STOBJS xarg ~
                          must be a true list of symbols and ~x0 is ~
                          not."
                   lst))
              (t (er-progn
                  (chk-all-stobj-names lst
                                       (msg "... :stobjs ~x0 ..."
                                            (cadr temp))
                                       ctx wrld state)
                  (er-let*
                    ((rst (get-declared-stobj-names (cdr edcls)
                                                    ctx wrld state)))
                    (value (union-eq lst rst)))))))
            (t (get-declared-stobj-names (cdr edcls) ctx wrld state)))))
        (t (get-declared-stobj-names (cdr edcls) ctx wrld state))))

(defun get-stobjs-in-lst (lst ctx wrld state)

; Lst is a list of ``fives'' as computed in chk-acceptable-defuns.
; Each element is of the form (fn args "doc" edcls body).  We know the
; args are legal arg lists, but nothing else.

; Unless we cause an error, we return a list in 1:1 correspondence
; with lst containing the STOBJS-IN flags for each fn.  This involves
; three steps.  First we recover from the edcls the declared :stobjs.
; We augment those with STATE, if STATE is in formals, which is always
; implicitly a stobj, if STATE is in the formals.  We confirm that all
; the declared stobjs are indeed stobjs in wrld.  Then we compute the
; stobj flags using the formals and the declared stobjs.

  (cond ((null lst) (value nil))
        (t (let ((fn (first (car lst)))
                 (formals (second (car lst))))
             (er-let* ((dcl-stobj-names
                        (get-declared-stobj-names (fourth (car lst))
                                                  ctx wrld state))
                       (dcl-stobj-namesx
                        (cond ((and (member-eq 'state formals)
                                    (not (member-eq 'state dcl-stobj-names)))
                               (er-progn
                                (chk-state-ok ctx wrld state)
                                (value (cons 'state dcl-stobj-names))))
                              (t (value dcl-stobj-names)))))
                             
                 (cond
                  ((not (subsetp-eq dcl-stobj-namesx formals))
                   (er soft ctx
                       "The stobj name~#0~[ ~&0 is~/s ~&0 are~] ~
                        declared but not among the formals of ~x1.  ~
                        This generally indicates some kind of ~
                        typographical error and is illegal.  Declare ~
                        only those stobj names listed in the formals. ~
                        The formals list of ~x1 is ~x2."
                       (set-difference-equal dcl-stobj-namesx formals)
                       fn
                       formals))
                  (t (er-let* ((others (get-stobjs-in-lst (cdr lst)
                                                          ctx wrld state)))

; Note: Wrld is irrelevant below because dcl-stobj-namesx is not T so
; we simply look for the formals that are in dcl-stobj-namesx.

                       (value
                        (cons (compute-stobj-flags formals
                                                   dcl-stobj-namesx
                                                   wrld)
                              others))))))))))

(defun chk-stobjs-out-bound (names bindings ctx state)
  (cond ((null names) (value nil))
        ((translate-unbound (car names) bindings)
         (er soft ctx
             "Translate failed to determine the output signature of ~
              ~x0." (car names)))
        (t (chk-stobjs-out-bound (cdr names) bindings ctx state))))

(defun store-stobjs-out (names bindings w)
  (cond ((null names) w)
        (t (store-stobjs-out
            (cdr names)
            bindings
            (putprop (car names) 'stobjs-out
                     (translate-deref (car names)
                                      bindings) w)))))

(defun unparse-signature (x)

; Suppose x is an internal form signature, i.e., (fn formals stobjs-in
; stobjs-out).  Then we return an external version of it, e.g., ((fn
; . stobjs-in) => (mv . stobjs-out)).  This is only used in error
; reporting.

  (let* ((fn (car x))
         (pretty-flags1 (prettyify-stobj-flags (caddr x)))
         (output (prettyify-stobjs-out (cadddr x))))
    `((,fn ,@pretty-flags1) => ,output)))

(defconst *built-in-program-mode-fns* '(sys-call gc$))

(defun chk-defun-mode (defun-mode names ctx state)
  (cond ((eq defun-mode :program)
         (value nil))
        ((eq defun-mode :logic)
         (cond ((intersectp-eq names *built-in-program-mode-fns*)
                (er soft ctx
                    "The built-in function~#0~[~/s~] ~&0 must ~
                     remain in :PROGRAM mode."
                    (intersection-eq names *built-in-program-mode-fns*)))
               (t (value nil))))
        (t (er soft ctx
               "The legal defun-modes are :program and :logic.  ~x0 is ~
                not a recognized defun-mode."
               defun-mode))))

(defun scan-to-cltl-command (wrld)

; Scan to the next binding of 'cltl-command or to the end of this event block.
; Return either nil or the global-value of cltl-command for this event.

  (cond ((null wrld) nil)
        ((and (eq (caar wrld) 'event-landmark)
              (eq (cadar wrld) 'global-value))
         nil)
        ((and (eq (caar wrld) 'cltl-command)
              (eq (cadar wrld) 'global-value))
         (cddar wrld))
        (t (scan-to-cltl-command (cdr wrld)))))

(defun plausible-dclsp1 (lst)

; We determine whether lst is a plausible cdr for a DECLARE form.  Ignoring the
; order of presentation and the number of occurrences of each element
; (including 0), we ensure that lst is of the form (... (TYPE ...) ... (IGNORE
; ...) ... (XARGS ... :key val ...) ...)  where the :keys are our xarg keys,
; :GUARD, :MEASURE, :WELL-FOUNDED-RELATION :HINTS, :GUARD-HINTS, :MODE,
; :VERIFY-GUARDS and :OTF-FLG.

  (cond ((atom lst) (null lst))
        ((and (consp (car lst))
              (true-listp (car lst))
              (or (member-eq (caar lst) '(type ignore))
                  (and (eq (caar lst) 'xargs)
                       (keyword-value-listp (cdar lst))
                       (subsetp-eq (evens (cdar lst))
                                   '(:guard :measure :well-founded-relation
                                            :hints :guard-hints :mode
                                            :verify-guards :otf-flg)))))
         (plausible-dclsp1 (cdr lst)))
        (t nil)))

(defun plausible-dclsp (lst)

; We determine whether lst is a plausible thing to include between the formals
; and the body in a defun, e.g., a list of doc strings and DECLARE forms.  We
; do not insist that the DECLARE forms are "perfectly legal" -- for example, we
; would approve (DECLARE (XARGS :measure m1 :measure m2)) -- but they are
; well-enough formed to permit us to walk through them with the fetch-from-dcls
; functions below.

; Note: This predicate is not actually used by defuns but is used by
; verify-termination in order to guard its exploration of the proposed dcls
; to merge them with the existing ones.  After we define the predicate we
; define the exploration functions, which implicitly assume this fn as their
; guard.  The exploration functions below are used in defuns, in particular,
; in the determination of whether a proposed defun is redundant.

  (cond ((atom lst) (null lst))
        ((stringp (car lst)) (plausible-dclsp (cdr lst)))
        ((and (consp (car lst))
              (eq (caar lst) 'declare)
              (plausible-dclsp1 (cdar lst)))
         (plausible-dclsp (cdr lst)))
        (t nil)))

; The above function, plausible-dclsp, is the guard and the role model for the
; following functions which explore plausible-dcls and either collect all the
; "fields" used or delete certain fields.

(defun dcl-fields1 (lst)
  (cond ((null lst) nil)
        ((or (eq (caar lst) 'type)
             (eq (caar lst) 'ignore))
         (add-to-set-eq (caar lst) (dcl-fields1 (cdr lst))))
        (t (union-eq (evens (cdar lst)) (dcl-fields1 (cdr lst))))))

(defun dcl-fields (lst)

; Lst satisfies plausible-dclsp, i.e., is the sort of thing you would find
; between the formals and the body of a DEFUN.  We return a list of all the
; "field names" used in lst.  Our answer is a subset of the following list of
; symbols: COMMENT, TYPE, IGNORE, :GUARD, :MEASURE, :WELL-FOUNDED-RELATION,
; :HINTS, :GUARD-HINTS, :MODE, :VERIFY-GUARDS, and :OTF-FLG.

  (cond ((null lst) nil)
        ((stringp (car lst))
         (add-to-set-eq 'comment (dcl-fields (cdr lst))))
        (t (union-eq (dcl-fields1 (cdar lst))
                     (dcl-fields (cdr lst))))))

(defun strip-keyword-list (fields lst)

; Lst is a keyword-value-listp, i.e., (:key1 val1 ...).  We remove any key/val
; pair whose key is in fields.

  (cond ((null lst) nil)
        ((member-eq (car lst) fields)
         (strip-keyword-list fields (cddr lst)))
        (t (cons (car lst)
                 (cons (cadr lst)
                       (strip-keyword-list fields (cddr lst)))))))

(defun strip-dcls1 (fields lst)
  (cond ((null lst) nil)
        ((or (eq (caar lst) 'type)
             (eq (caar lst) 'ignore))
         (cond ((member-eq (caar lst) fields) (strip-dcls1 fields (cdr lst)))
               (t (cons (car lst) (strip-dcls1 fields (cdr lst))))))
        (t
         (let ((temp (strip-keyword-list fields (cdar lst))))
           (cond ((null temp) (strip-dcls1 fields (cdr lst)))
                 (t (cons (cons 'xargs temp)
                          (strip-dcls1 fields (cdr lst)))))))))

(defun strip-dcls (fields lst)

; Lst satisfies plausible-dclsp.  Fields is a list as returned by dcl-fields,
; i.e., a subset of the symbols COMMENT, TYPE, IGNORE, :GUARD, :MEASURE,
; :WELL-FOUNDED-RELATION, :HINTS, :GUARD-HINTS, :MODE, :VERIFY-GUARDS, and
; :OTF-FLG.  We copy lst deleting any part of it that specifies a value for one
; of the fields named.  The result satisfies plausible-dclsp.

  (cond ((null lst) nil)
        ((stringp (car lst))
         (cond ((member-eq 'comment fields) (strip-dcls fields (cdr lst)))
               (t (cons (car lst) (strip-dcls fields (cdr lst))))))
        (t (let ((temp (strip-dcls1 fields (cdar lst))))
             (cond ((null temp) (strip-dcls fields (cdr lst)))
                   (t (cons (cons 'declare temp)
                            (strip-dcls fields (cdr lst)))))))))

(defun fetch-dcl-field1 (field-name lst)
  (cond ((null lst) nil)
        ((or (eq (caar lst) 'type)
             (eq (caar lst) 'ignore))
         (if (eq (caar lst) field-name)
             (cons (cdar lst) (fetch-dcl-field1 field-name (cdr lst)))
             (fetch-dcl-field1 field-name (cdr lst))))
        (t (let ((temp (assoc-keyword field-name (cdar lst))))
             (cond (temp (cons (cadr temp)
                               (fetch-dcl-field1 field-name (cdr lst))))
                   (t (fetch-dcl-field1 field-name (cdr lst))))))))

(defun fetch-dcl-field (field-name lst)

; Lst satisfies plausible-dclsp, i.e., is the sort of thing you would find
; between the formals and the body of a DEFUN.  Field-name is one of the
; symbols: COMMENT, TYPE, IGNORE, :GUARD, :MEASURE, :WELL-FOUNDED-RELATION,
; :HINTS, :GUARD-HINTS, :MODE, :VERIFY-GUARDS, and :OTF-FLG.  We return the
; list of the contents of all fields with that name.  We assume we will find at
; most one specification per XARGS entry for a given keyword.

; For example, if field-name is :GUARD and there are two XARGS among the
; DECLAREs in lst, one with :GUARD g1 and the other with :GUARD g2 we return
; (g1 g2).  Similarly, if field-name is TYPE and lst contains (DECLARE (TYPE
; INTEGER X Y)) then our output will be (... (INTEGER X Y) ...) where the ...
; are the other TYPE entries.

  (cond ((null lst) nil)
        ((stringp (car lst))
         (if (eq field-name 'comment)
             (cons (car lst) (fetch-dcl-field field-name (cdr lst)))
             (fetch-dcl-field field-name (cdr lst))))
        (t (append (fetch-dcl-field1 field-name (cdar lst))
                   (fetch-dcl-field field-name (cdr lst))))))

(defun set-equalp-eq (lst1 lst2)
  (declare (xargs :guard (and (symbol-listp lst1)
                              (symbol-listp lst2))))
  (and (subsetp-eq lst1 lst2)
       (subsetp-eq lst2 lst1)))

(defun non-identical-defp (def1 def2 chk-measure-p wrld)

; This predicate is used in recognizing redundant definitions.  In our intended
; application, def2 will have been successfully processed and def1 is merely
; proposed, where def1 and def2 are each of the form (fn args ...dcls... body)
; and everything is untranslated.  Two such tuples are "identical" if their
; fns, args, bodies, types, stobjs, measures, and guards are equal -- except
; that the new measure can be (:? v1 ... vk) if (v1 ... vk) is the measured
; subset for the old definition.  We return nil if def1 is thus redundant
; ("identical" to) with def2.  Otherwise we return a message suitable for
; printing using "  Note that ~@k.".

; Note that def1 might actually be syntactically illegal, e.g., it might
; specify two different :measures.  But it is possible that we will still
; recognize it as identical to def2 because the args and body are identical.
; Thus, the syntactic illegality of def1 might not be discovered if def1 is
; avoided because it is redundant.  This happens already in redundancy checking
; in defthm: a defthm event is redundant if it introduces an identical theorem
; with the same name -- even if the :hints in the new defthm are ill-formed.
; The idea behind redundancy checking is to allow books to be loaded even if
; they share some events.  The assumption is that def1 is in a book that got
; (or will get) processed by itself sometime and the ill-formedness will be
; detected there.  That will change the check sum on the book and cause
; certification to lapse in the book that considered def1 redundant.

  (cond
   ((equal def1 def2) ; optimization
    nil)
   ((not (eq (car def1) (car def2))) ; check same fn (can this fail?)
    (msg "the name of the new event, ~x0, differs from the name of the ~
          corresponding existing event, ~x1."
         (car def1) (car def2)))
   ((not (equal (cadr def1) (cadr def2))) ; check same args
    (msg "the proposed argument list for ~x0, ~x1, differs from the existing ~
          argument list, ~x2."
         (car def1) (cadr def1) (cadr def2)))
   ((not (equal (car (last def1)) (car (last def2)))) ; check same body
    (msg "the proposed body for ~x0,~|~%~p1,~|~%differs from the existing ~
          body,~|~%~p2.~|~%"
         (car def1) (car (last def1)) (car (last def2))))
   (t
    (let ((all-but-body1 (butlast (cddr def1) 1))
            (all-but-body2 (butlast (cddr def2) 1)))
      (cond
       ((not (equal (fetch-dcl-field :non-executable all-but-body1)
                    (fetch-dcl-field :non-executable all-but-body2)))
        (msg "the proposed and existing definitions for ~x0 differ on their ~
              :non-executable declarations."
             (car def1)))
       ((not (equal (fetch-dcl-field :stobjs all-but-body1)
                    (fetch-dcl-field :stobjs all-but-body2)))

; We insist that the :STOBJS of the two definitions be identical.  Vernon
; Austel pointed out the following bug.

#|
; Define a :program mode function with a non-stobj argument.
  (defun stobjless-fn (stobj-to-be)
    (declare (xargs :mode :program))
    stobj-to-be)
; Use it in the definition of another :program mode function.
  (defun my-callee-is-stobjless (x)
    (declare (xargs :mode :program))
    (stobjless-fn x))
; Then introduce a the argument name as a stobj:
  (defstobj stobj-to-be
    (a-field :type integer :initially 0))
; And reclassify the first function into :logic mode.
  (defun stobjless-fn (stobj-to-be)
    (declare (xargs :stobjs stobj-to-be))
    stobj-to-be)
; If you don't notice the different use of :stobjs then the :program
; mode function my-callee-is-stobjless [still] treats the original
; function as though its argument were NOT a stobj!  For example,
; (my-callee-is-stobjless 3) is a well-formed :program mode term
; that treats 3 as a stobj.
|#

        (msg "the proposed and existing definitions for ~x0 differ on their ~
              :stobj declarations."
             (car def1)))
       ((not (set-equalp-equal (fetch-dcl-field 'type all-but-body1)
                               (fetch-dcl-field 'type all-but-body2)))

; Once we removed the restriction that the type and :guard fields of the defs
; be set-equal.  But imagine that we have a strong guard on foo in our current
; ACL2 session, but that we then include a book with a much weaker guard.
; (Horrors!  What if the new guard is totally unrelated!?)  If we didn't make
; the tests below, then presumably the guard on foo would be unchanged by this
; include-book.  Suppose that in this book, we have verified guards for a
; function bar that calls foo.  Then after including the book, it will look as
; though correctly guarded calls of bar always generate only correctly guarded
; calls of foo, but now that foo has a stronger guard than it did when the book
; was certified, this might not always be the case.

        (msg "the proposed and existing definitions for ~x0 differ on their ~
              type declarations."
             (car def1)))
       ((let ((guards1 (fetch-dcl-field :guard all-but-body1))
              (guards2 (fetch-dcl-field :guard all-but-body2)))
          (not (or (set-equalp-equal guards1 guards2)

; See the comment above on type and :guard fields.  Here, we comprehend the
; fact that omission of a guard is equivalent to :guard t.  Of course, it is
; also equivalent to :guard 't and even to :guard (not nil), but we see no need
; to be that generous with our notion of redundancy.

                   (and (null guards1) (equal guards2 '(t)))
                   (and (null guards2) (equal guards1 '(t))))))
        (msg "the proposed and existing definitions for ~x0 differ on their ~
              :guard declarations."
             (car def1)))
       ((not chk-measure-p)
        nil)
       (t
        (let ((new-measures (fetch-dcl-field :measure all-but-body1))
              (old-measures (fetch-dcl-field :measure all-but-body2)))
          (cond
           ((equal new-measures old-measures)
            nil)
           (t
            (let ((old-measured-subset
                   (let ((justification
                          (getprop (car def2) 'justification nil
                                   'current-acl2-world wrld)))
                     (and

; Consider the case that the existing definition is non-recursive.  Then we
; treat the measured subset as nil.

                      justification
                      (access justification justification :subset)))))
              (cond
               ((and (consp new-measures)
                     (null (cdr new-measures))
                     (let ((new-measure (car new-measures)))
                       (or (equal new-measure (car old-measures))
                           (and (true-listp new-measure)
                                (eq (car new-measure) :?)
                                (arglistp (cdr new-measure))
                                (set-equalp-eq old-measured-subset (cdr new-measure))))))
                nil)
               (old-measures
                (msg "the proposed and existing definitions for ~x0 differ on ~
                      their measures.  The existing measure is ~x1.  The new ~
                      measure needs to be specified explicitly with :measure ~
                      (see :DOC xargs), either to be identical to the ~
                      existing measure or to be a call of :? on the measured ~
                      subset; for example, ~x2 will serve as the new :measure."
                     (car def1)
                     (car old-measures)
                     (cons :? old-measured-subset)))
               (t
                (msg "the existing definition for ~x0 does not have an ~
                      explicitly specified measure.  Either remove the ~
                      :measure declaration from your proposed definition, or ~
                      else specify a :measure that applies :? to the existing ~
                      measured subset, for example, ~x1."
                     (car def1)
                     (cons :? old-measured-subset))))))))))))))

(defun identical-defp (def1 def2 chk-measure-p wrld)

; This function is probably obsolete -- superseded by non-identical-defp -- but
; we leave it here for reference by comments.

  (not (non-identical-defp def1 def2 chk-measure-p wrld)))

(defun redundant-or-reclassifying-defunp (defun-mode symbol-class
                                           ld-skip-proofsp def wrld)

; Def is a defuns tuple such as (fn args ...dcls... body) that has been
; submitted to defuns with mode defun-mode.  We determine whether fn is already
; defined in wrld and has an "identical" definition (up to defun-mode).  We
; return either nil, a message (cons pair suitable for printing with ~@),
; 'redundant, or 'reclassifying, or 'verify-guards.  'Redundant is returned if
; there is an existing definition for fn that is identical-defp to def and has
; mode defun-mode, except that in this case 'verify-guards is returned if the
; symbol-class was :ideal but this definition indicates promotion to
; :common-lisp-compliant.  'Reclassifying is returned if there is an existing
; definition for fn that is identical-defp to def but in defun-mode :program
; while defun-mode is :logic.  Otherwise nil or an explanatory message,
; suitable for printing using "  Note that ~@0.", is returned.

; Functions further up the call tree will decide what to do with a result of
; 'verify-guards.  But a perfectly reasonable action would be to cause an error
; suggesting the use of verify-guards instead of defun.

; A successful redundancy check requires that the untranslated measure is
; identical to that of the earlier corresponding defun.  Without such a check
; we can store incorrect induction information, as exhibited by the "soundness
; bug in the redundancy criterion for defun events" mentioned in :doc
; note-3-0-2.  The following examples, which work with Version_3.0.1 but
; (fortunately) not afterwards, build on the aforementioned proof of nil given
; in :doc note-3-0-2, giving further weight to our insistence on the same
; measure if the mode isn't changing from :program to :logic.

; The following example involves redundancy only for :program mode functions.

#|
 (encapsulate
  ()

  (local (defun foo (x y)
           (declare (xargs :measure (acl2-count y) :mode :program))
           (if (and (consp x) (consp y))
               (foo (cons x x) (cdr y))
             y)))

  (defun foo (x y)
    (declare (xargs :mode :program))
    (if (and (consp x) (consp y))
        (foo (cons x x) (cdr y))
      y))

  (verify-termination foo))

 (defthm bad
   (atom x)
   :rule-classes nil
   :hints (("Goal" :induct (foo x '(3)))))

 (defthm contradiction
   nil
   :rule-classes nil
   :hints (("Goal" :use ((:instance bad (x '(7)))))))
|#

; Note that even though we do not store induction schemes for mutual-recursion,
; the following variant of the first example shows that we still need to check
; measures in that case:

#|
 (set-bogus-mutual-recursion-ok t) ; ease construction of example

 (encapsulate
  ()
  (local (encapsulate
          ()

          (local (mutual-recursion
                  (defun bar (x) x)
                  (defun foo (x y)
                    (declare (xargs :measure (acl2-count y)))
                    (if (and (consp x) (consp y))
                        (foo (cons x x) (cdr y))
                      y))))

          (mutual-recursion
           (defun bar (x) x)
           (defun foo (x y)
             (if (and (consp x) (consp y))
                 (foo (cons x x) (cdr y))
               y)))))
  (defun foo (x y)
    (if (and (consp x) (consp y))
        (foo (cons x x) (cdr y))
      y)))

 (defthm bad
   (atom x)
   :rule-classes nil
   :hints (("Goal" :induct (foo x '(3)))))

 (defthm contradiction
   nil
   :rule-classes nil
   :hints (("Goal" :use ((:instance bad (x '(7)))))))
|# ; |

  (cond ((function-symbolp (car def) wrld)
         (let* ((wrld1 (decode-logical-name (car def) wrld))
                (name (car def))
                (val (scan-to-cltl-command (cdr wrld1)))
                (chk-measure-p
                 (and

; If we are skipping proofs, then we do not need to check the measure.  Why
; not?  One case is that we are explicitly skipping proofs (with skip-proofs,
; rebuild, set-ld-skip-proofsp, etc.; or, inclusion of an uncertified book), in
; which case all bets are off.  Otherwise we are including a certified book,
; where the measured subset was proved correct.  This observation satisfies our
; concern, which is that the current redundant definition will ultimately
; become the actual definition because the earlier one is local.

                  (not ld-skip-proofsp)

; In later code, below, we disallow reclassifying :logic to :program.  Now
; consider the case of going from :program to :logic.  Then we ignore the
; measure of the :program mode definition when calculating the measure for the
; new :logic mode definition, so we do not need to check equality of the old
; and new measures.

                 (eq (cadr val) defun-mode))))

; The 'cltl-command val for a defun is (defuns :defun-mode ignorep . def-lst)
; where :defun-mode is a keyword (rather than nil which means this was an
; encapsulate or was :non-executable).

           (cond ((null val) nil)
                 ((and (consp val)
                       (eq (car val) 'defuns)
                       (keywordp (cadr val)))
                  (cond
                   ((non-identical-defp def
                                        (assoc-eq name (cdddr val))
                                        chk-measure-p
                                        wrld))

; Else, this cltl-command contains a member of def-lst that is identical to
; def.

                   ((eq (cadr val) defun-mode)
                    (cond ((and (eq symbol-class :common-lisp-compliant)
                                (eq (symbol-class name wrld) :ideal))

; The following produced a hard error in v2-7, because the second defun was
; declared redundant on the first pass and then installed as
; :common-lisp-compliant on the second pass:

; (encapsulate nil
;   (local
;     (defun foo (x) (declare (xargs :guard t :verify-guards nil)) (car x)))
;   (defun foo (x) (declare (xargs :guard t)) (car x)))
; (thm (equal (foo 3) xxx))

; The above example was derived from one sent by Jared Davis, who proved nil in
; an early version of v2-8 by exploiting this idea to trick ACL2 into
; considering guards verified for a function employing mbe.

; Here, we prevent such promotion of :ideal to :common-lisp-compliant.

                           'verify-guards)
                          (t 'redundant)))
                   ((and (eq (cadr val) :program)
                         (eq defun-mode :logic))
                    'reclassifying)
                   (t 

; We disallow redefinition from :logic to :program mode.  We once thought
; we should allow it and argued that it should just be redundant, i.e.,
; we should do nothing when processing the new :program mode defun.  But
; then we considered an example like this:

; (encapsulate nil
;   (local (defun foo (x) x))
;   (defun foo (x) (declare (xargs :mode :program)) x)  ; redundant?
;   (defthm foo-is-id (equal (foo x) x)))

; We clearly don't want to allow this encapsulation or analogous books.
; This is actually prevented by pass 2 of the encapsulate, when it 
; discovers that foo is now :program mode.  But we have little confidence
; that we avoid similar traps elsewhere and think it is a bad idea to
; allow :logic to :program.

                    "redefinition from :logic to :program mode is illegal")))
                 ((and (null (cadr val)) ; optimization
                       (fetch-dcl-field :non-executable
                                        (butlast (cddr def) 1)))
                  (cond
                   ((let ((event (cddddr (car wrld1))))
                      (non-identical-defp
                       def
                       (case (car event)
                         (mutual-recursion
                          (assoc-eq name (strip-cdrs (cdr event))))
                         (defuns
                           (assoc-eq name (cdr event)))
                         (otherwise
                          (cdr event)))
                       chk-measure-p
                       wrld)))
                   (t

; Note that :non-executable definitions always have mode :logic, we we do not
; have to think about the 'reclassifying case.

                    'redundant)))
                 (t nil))))
        (t nil)))

(defun redundant-or-reclassifying-defunsp1 (defun-mode symbol-class
                                             ld-skip-proofsp def-lst wrld ans)
  (cond ((null def-lst) ans)
        (t (let ((x (redundant-or-reclassifying-defunp
                     defun-mode symbol-class ld-skip-proofsp (car def-lst)
                     wrld)))
             (cond
              ((consp x) x) ; a message
              ((eq ans x)
               (redundant-or-reclassifying-defunsp1
                defun-mode symbol-class ld-skip-proofsp (cdr def-lst) wrld
                ans))
              (t nil))))))

(defun redundant-or-reclassifying-defunsp (defun-mode symbol-class
                                            ld-skip-proofsp def-lst wrld)

; We return 'redundant if the functions in def-lst are already identically
; defined with :mode defun-mode and class symbol-class.  We return
; 'reclassifying if they are all identically defined :programally and
; defun-mode is :logic.  We return nil otherwise.

; We answer this question by answering it independently for each def in
; def-lst.  Thus, every def must be 'redundant or 'reclassifying as
; appropriate.  This seems really weak because we do not insist that only one
; cltl-command tuple is involved.  But (defuns def1 ... defn) just adds one
; axiom for each defi and the axiom is entirely determined by the defi.  Thus,
; if we have executed a defuns that added the axiom for defi then it is the
; same axiom as would be added if we executed a different defuns that contained
; defi.  Furthermore, a cltl-command of the form (defuns :defun-mode ignorep
; def1 ... defn) means (defuns def1 ...  defn) was executed in this world with
; the indicated defun-mode.

; Note: Our redundancy check for definitions is based on the untranslated
; terms.  This is different from, say, theorems, where we compare translated
; terms.  The reason is that we do not store the translated versions of
; :program definitions and don't want to go to the cost of translating
; what we did store.  We could, I suppose.  We handle theorems the way we do
; because we store the translated theorem on the property list, so it is easy.
; Our main concern vis-a-vis redundancy is arranging for identical definitions
; not to blow us up when we are loading books that have copied definitions and
; I don't think translation will make an important difference to the utility of
; the feature.

; Note: There is a possible bug lurking here.  If the host Common Lisp expands
; macros before storing the symbol-function, then it is we could recognize as
; "redundant" an identical defun that, if actually passed to the underlying
; Common Lisp, would result in the storage of a different symbol-function
; because of the earlier redefinition of some macro used in the "redundant"
; definition.  This is not a soundness problem, since redefinition is involved.
; But it sure might annoy somebody who didn't notice that his redefinition
; wasn't processed.

  (cond
   ((null def-lst) 'redundant)
   (t (let ((ans (redundant-or-reclassifying-defunp
                  defun-mode symbol-class ld-skip-proofsp (car def-lst) wrld)))
        (cond ((consp ans) ans) ; a message
              (t (redundant-or-reclassifying-defunsp1
                  defun-mode symbol-class ld-skip-proofsp (cdr def-lst) wrld
                  ans)))))))

(defun collect-when-cadr-eq (sym lst)
  (cond ((null lst) nil)
        ((eq sym (cadr (car lst)))
         (cons (car lst) (collect-when-cadr-eq sym (cdr lst))))
        (t (collect-when-cadr-eq sym (cdr lst)))))

(defun all-programp (names wrld)

; Names is a list of function symbols.  Return t iff every element of
; names is :program.

  (cond ((null names) t)
        (t (and (programp (car names) wrld)
                (all-programp (cdr names) wrld)))))

; Essay on the Identification of Irrelevant Formals

; A formal is irrelevant if its value does not affect the value of the
; function.  Of course, ignored formals have this property, but we here
; address ourselves to the much more subtle problem of formals that are
; used only in irrelevant ways.  For example, y in

; (defun foo (x y) (if (zerop x) 0 (foo (1- x) (cons x y))))

; is irrelevant.  Clearly, any formal mentioned outside of a recursive call is
; relevant -- provided that no previously introduced function has irrelevant
; arguments and no definition tests constants as in (if t x y).  But a
; formal that is never used outside a recursive call may still be
; relevant, as illustrated by y in

; (defun foo (x y) (if (< x 2) x (foo y 0)))

; Observe that (foo 3 1) = 1 and (foo 3 0) = 0, thus, y is relevant.  (This
; function can be admitted with the measure (cond ((< x 2) 0) ((< y 2) 1) (t
; 2)).)

; Thus, we have to do a transitive closure computation based on which
; formals appear in which actuals of recursive calls.  In the first pass we
; see that x, above, is relevant because it is used outside the recursion.
; In the next pass we see that y is relevant because it is passed into the
; x argument position of a recursive call.

; The whole thing is made somewhat more hairy by mutual recursion, though no
; new intellectual problems are raised.  However, to cope with mutual recursion
; we stop talking about "formals" and start talking about "slots."  A slot here
; is a triple, (fn n . var), where fn is one of the functions in the mutually
; recursive clique, n is a nonnegative integer less than the arity of fn, and
; var is the nth formal of fn.  This is redundant (simply (fn . n) would
; suffice) but allows us to recover the formal conveniently.  We say a "slot
; is used" in a term if its formal is used in the term and the term occurs
; in the body of the fn of the slot.

; A "recursive call" here means a call of any function in the clique.  We
; generally use the variable clique-alist to mean an alist whose elements are
; each of the form (fn formals guard measure body).  Thus, if temp is (assoc-eq
; fn clique-alist) and is non-nil then
; formals = (nth 1 temp)
; guard   = (nth 2 temp)
; measure = (nth 3 temp)
; body    = (nth 4 temp).
; We make the convention that any variable occurring in either the guard
; or measure is relevant.  We do not discuss guards and measures further.

; A second problem is raised by the presence of lambda expressions.  We discuss
; them more below.

; Our algorithm iteratively computes the relevant slots of a clique by
; successively enlarging an initial guess.  The initial guess consists of all
; the slots used outside of a recursive call.  (In this computation we also
; sweep in any slot whose formal is used in an actual expression in a recursive
; call, provided the actual is in a slot already known to be used outside.)
; Clearly, every slot so collected is relevant.  We then iterate, sweeping into
; the set every slot used either outside recursion or in an actual used in a
; relevant slot.  When this computation ceases to add any new slots we consider
; the uncollected slots to be irrelevant.

; For example, in (defun foo (x y) (if (zerop x) 0 (foo (1- x) (cons x y)))) we
; intially guess that x is relevant and y is not.  The next iteration adds
; nothing, because y is not used in the x slot, so we are done.

; On the other hand, in (defun foo (x y) (if (< x 2) x (foo y 0))) we might
; once again guess that y is irrelevant.  (Actually, we don't, because we see
; in the initial scan that it is used in the x slot of a recursion AFTER we
; have spotted x occurring outside of a recursive call.  But this could be
; remedied by a defn like (defun bar (z x y) (if (not (< z 2)) (bar (1- z) y 0)
; x)) where we don't know x is relevant at the time we process the call and
; would thus guess that y was irrelevant.)  However, the second pass would note
; the occurrence of y in a relevant slot and would sweep it into the set.  We
; conclude that there are no irrelevant slots in this definition.

; So far we have not discussed lambda expressions; they are unusual in this
; setting because they may hide recursive calls that we should analyze.  We do
; not want to expand the lambdas away, for fear of combinatoric explosions.
; Instead, we expand the clique-alist, by adding, for each lambda-application a
; new entry that pairs that lambda expression with the appropriate terms.
; (That is, the "fn" of the new clique member is the lambda expression itself.)
; Thus, we actually use assoc-equal instead of assoc-eq when looking in
; clique-alist.

(defun formal-position (var formals i)
  (cond ((null formals) i)
        ((eq var (car formals)) i)
        (t (formal-position var (cdr formals) (1+ i)))))

(defun make-slot (fn formals var)
  (list* fn (formal-position var formals 0) var))

(defun make-slots (fn formals vars)
  (cond ((null vars) nil)
        (t (cons (make-slot fn formals (car vars))
                 (make-slots fn formals (cdr vars))))))

(defun slot-member (fn n lst)

; We ask whether (list* fn n var), for some var, is a member-equal of
; the list of slots, lst.

  (cond ((null lst) nil)
        ((and (equal fn (caar lst))
              (= n (cadar lst)))
         lst)
        (t (slot-member fn n (cdr lst)))))

; We now develop the code to expand the clique-alist for lambda expressions.

(mutual-recursion

(defun expand-clique-alist-term (term clique-alist)
  (cond ((variablep term) clique-alist)
        ((fquotep term) clique-alist)
        (t (let ((clique-alist
                  (expand-clique-alist-term-lst (fargs term)
                                                clique-alist))
                 (fn (ffn-symb term)))
             (cond
              ((flambdap fn)
               (cond ((assoc-equal fn clique-alist) clique-alist)
                     (t (expand-clique-alist-term
                         (lambda-body fn)
                         (cons
                          (list fn
                                (lambda-formals fn)
                                *t*
                                *0*
                                (lambda-body fn))
                          clique-alist)))))
              (t clique-alist))))))

(defun expand-clique-alist-term-lst (lst clique-alist)
  (cond ((null lst) clique-alist)
        (t (expand-clique-alist-term-lst
            (cdr lst)
            (expand-clique-alist-term (car lst) clique-alist)))))
)

(defun expand-clique-alist1 (alist clique-alist)
  (cond ((null alist) clique-alist)
        (t (expand-clique-alist1 (cdr alist)
                                 (expand-clique-alist-term
                                  (nth 4 (car alist))
                                  clique-alist)))))

(defun expand-clique-alist (clique-alist)
  (expand-clique-alist1 clique-alist clique-alist))

(defun make-clique-alist1 (fns formals guards measures bodies)
  (cond ((null fns) nil)
        (t (cons (list (car fns)
                       (car formals)
                       (car guards)
                       (car measures)
                       (car bodies))
                 (make-clique-alist1 (cdr fns)
                                     (cdr formals)
                                     (cdr guards)
                                     (cdr measures)
                                     (cdr bodies))))))

(defun make-clique-alist (fns formals guards measures bodies)

; This function converts from the data structures used in defuns-fn0 to our
; expanded clique-alist.  Each element of clique-alist is of the form (fn
; formals guard measure body), where fn is either a function symbol or lambda
; expression.

  (expand-clique-alist
   (make-clique-alist1 fns formals guards measures bodies)))

(mutual-recursion

(defun relevant-slots-term (fn formals term clique-alist ans)

; Term is a term occurring in the body of fn which has formals formals.  We
; collect a slot into ans if it is used outside a recursive call (or in an
; already known relevant actual to a recursive call).

  (cond
   ((variablep term)
    (add-to-set-equal (make-slot fn formals term) ans))
   ((fquotep term) ans)
   ((assoc-equal (ffn-symb term) clique-alist)
    (relevant-slots-call fn formals (ffn-symb term) (fargs term) 0
                         clique-alist ans))
   (t 
    (relevant-slots-term-lst fn formals (fargs term) clique-alist ans))))

(defun relevant-slots-term-lst (fn formals lst clique-alist ans)
  (cond ((null lst) ans)
        (t (relevant-slots-term-lst
            fn formals (cdr lst) clique-alist
            (relevant-slots-term fn formals (car lst)
                                 clique-alist ans)))))

(defun relevant-slots-call
  (fn formals called-fn actuals i clique-alist ans)

; Called-fn is the name of some function in the clique.  Actuals is (a tail of)
; the list of actuals in a call of called-fn occurring in the body of fn (which
; has formals formals).  Initially, i is 0 and in general is the position in
; the argument list of the first element of actuals.  Ans is a list of slots
; which are known to be relevant.  We extend ans by adding new, relevant slots
; to it, by seeing which slots are used in the actuals in the relevant slots of
; called-fn.

  (cond
   ((null actuals) ans)
   (t
    (relevant-slots-call
     fn formals called-fn (cdr actuals) (1+ i) clique-alist
     (if (slot-member called-fn i ans)
         (relevant-slots-term fn formals (car actuals)
                              clique-alist ans)
         ans)))))
)

(defun relevant-slots-def
  (fn formals guard measure body clique-alist ans)

; Returns all obviously relevant slots in a definition.  A slot is obviously
; relevant if it is mentioned in the guard or measure of the definition or is
; mentioned outside of all calls of functions in the clique or it is mentioned
; in some known relevant argument position of a function in the clique.

  (let ((vars (all-vars1 guard (all-vars measure))))
    (cond
     ((subsetp-eq formals vars)
      (union-equal (make-slots fn formals formals) ans))
     (t (relevant-slots-term
         fn formals body clique-alist
         (union-equal (make-slots fn formals vars) ans))))))

(defun relevant-slots-clique1 (alist clique-alist ans)
  (cond
   ((null alist) ans)
   (t (relevant-slots-clique1
       (cdr alist) clique-alist
       (relevant-slots-def (nth 0 (car alist))
                           (nth 1 (car alist))
                           (nth 2 (car alist))
                           (nth 3 (car alist))
                           (nth 4 (car alist))
                           clique-alist
                           ans)))))

(defun relevant-slots-clique (clique-alist ans)

; We compute the relevant slots in an expanded clique alist (one in which the
; lambda expressions have been elevated to clique membership).  The list of
; relevant slots includes the relevant lambda slots.  We do it by iteratively
; enlarging ans until it is closed.

  (let ((ans1 (relevant-slots-clique1 clique-alist clique-alist ans)))
    (cond ((equal ans1 ans) ans)
          (t (relevant-slots-clique clique-alist ans1)))))

(defun all-non-lambda-slots-clique (clique-alist)

; We return all the slots in a clique except those for lambda expressions.

  (cond ((null clique-alist) nil)
        ((symbolp (caar clique-alist))
         (append (make-slots (caar clique-alist) (cadar clique-alist) (cadar clique-alist))
                 (all-non-lambda-slots-clique (cdr clique-alist))))
        (t (all-non-lambda-slots-clique (cdr clique-alist)))))

(defun ignored/ignorable-slots (fns arglists ignores ignorables)

; Ignored formals are considered not to be irrelevant.  Should we do similarly
; for ignorable formals?

; - If yes (ignorables are not irrelevant), then we may miss some irrelevant
;   formals.  Of course, it is always OK to miss some irrelevant formals, but
;   we would prefer not to miss them needlessly.

; - If no (ignorables are irrelevant), then we may report an ignorable variable
;   as irrelevant, which might annoy the user even though it really is
;   irrelevant, if "ignorable" not only means "could be ignored" but also means
;   "could be irrelevant".

; We choose "yes".  If the user has gone through the trouble to label a
; variable as irrelevant, then the chance that irrelevance is due to a typo are
; dwarfed by the chance that irrelevance is due to being an ignorable var.

  (cond ((null fns) nil)
        (t (append (make-slots (car fns) (car arglists) (car ignores))
                   (make-slots (car fns) (car arglists) (car ignorables))
                   (ignored/ignorable-slots (cdr fns)
                                            (cdr arglists)
                                            (cdr ignores)
                                            (cdr ignorables))))))

(defun irrelevant-non-lambda-slots-clique (fns arglists guards measures ignores
                                               ignorables bodies)

; Let clique-alist be an expanded clique alist (one in which lambda expressions have
; been elevated to clique membership).  Return all the irrelevant slots for the non-lambda
; members of the clique.

; A "slot" is a triple of the form (fn n . var), where fn is a function symbol,
; n is some nonnegative integer less than the arity of fn, and var is the nth
; formal of fn.  If (fn n . var) is in the list returned by this function, then
; the nth formal of fn, namely var, is irrelevant to the value computed by fn.

  (let ((clique-alist (make-clique-alist fns arglists guards measures bodies)))
    (set-difference-equal (all-non-lambda-slots-clique clique-alist)
                          (append (relevant-slots-clique clique-alist nil)
                                  (ignored/ignorable-slots
                                   fns arglists ignores ignorables)))))

(defun tilde-*-irrelevant-formals-msg1 (slots)
  (cond ((null slots) nil)
        (t (cons (cons "~n0 formal of ~x1, ~x2,"
                       (list (cons #\0 (list (1+ (cadar slots))))
                             (cons #\1 (caar slots))
                             (cons #\2 (cddar slots))))
                 (tilde-*-irrelevant-formals-msg1 (cdr slots))))))

(defun tilde-*-irrelevant-formals-msg (slots)
  (list "" "~@*" "~@* and the " "~@* the " (tilde-*-irrelevant-formals-msg1 slots)))

(defun chk-irrelevant-formals
  (fns arglists guards measures ignores ignorables bodies ctx state)
  (let ((irrelevant-formals-ok
         (cdr (assoc-eq :irrelevant-formals-ok
                        (table-alist 'acl2-defaults-table (w state))))))
    (cond
     ((or (eq irrelevant-formals-ok t)
          (and (eq irrelevant-formals-ok :warn)
               (warning-disabled-p "Irrelevant-formals")))
      (value nil))
     (t
      (let ((irrelevant-slots
             (irrelevant-non-lambda-slots-clique
              fns arglists guards measures ignores ignorables bodies)))
        (cond
         ((null irrelevant-slots) (value nil))
         ((eq irrelevant-formals-ok :warn)
          (pprogn
           (warning$ ctx ("Irrelevant-formals")
                    "The ~*0 ~#1~[is~/are~] irrelevant.  See :DOC ~
                     irrelevant-formals."
                    (tilde-*-irrelevant-formals-msg irrelevant-slots)
                    (if (cdr irrelevant-slots) 1 0))
           (value nil)))
         (t (er soft ctx
                "The ~*0 ~#1~[is~/are~] irrelevant.  See :DOC ~
                 irrelevant-formals."
                (tilde-*-irrelevant-formals-msg irrelevant-slots)
                (if (cdr irrelevant-slots) 1 0)))))))))

(deflabel irrelevant-formals
  :doc
  ":Doc-Section ACL2::Programming

  formals that are used but only insignificantly~/

  Let ~c[fn] be a function of ~c[n] arguments.  Let ~c[x] be the ~c[i]th formal of ~c[fn].
  We say ~c[x] is ``irrelevant in ~c[fn]'' if ~c[x] is not involved in either the
  ~il[guard] or the measure for ~c[fn], ~c[x] is used in the body, but the value of
  ~c[(fn a1...ai...an)] is independent of ~c[ai].~/

  The easiest way to define a function with an irrelevant formal is
  simply not to use the formal in the body of the
  function.  Such formals are said to be ``ignored'' by Common Lisp
  and a special declaration is provided to allow ignored formals.
  ACL2 makes a distinction between ignored and irrelevant formals.  Note
  however that if a variable is ~ilc[declare]d ~c[ignore]d or ~c[ignorable],
  then it will not be reported as irrelevant.

  An example of an irrelevant formal is ~c[x] in the definition of ~c[fact]
  below.
  ~bv[]
  (defun fact (i x) 
    (declare (xargs :guard (and (integerp i) (<= 0 i))))
    (if (zerop i) 0 (* i (fact (1- i) (cons i x))))).
  ~ev[]
  Observe that ~c[x] is only used in recursive calls of ~c[fact]; it never
  ``gets out'' into the result.  ACL2 can detect some irrelevant
  formals by a closure analysis on how the formals are used.  For
  example, if the ~c[i]th formal is only used in the ~c[i]th argument position
  of recursive calls, then it is irrelevant.  This is how ~c[x] is used
  above.

  It is possible for a formal to appear only in recursive calls but
  still be relevant.  For example, ~c[x] is ~st[not] irrelevant below, even
  though it only appears in the recursive call.
  ~bv[]
  (defun fn (i x) (if (zerop i) 0 (fn x (1- i))))
  ~ev[]
  The key observation above is that while ~c[x] only appears in a
  recursive call, it appears in an argument position, namely ~c[i]'s, that
  is relevant.  (The function above can be admitted with a ~c[:]~ilc[guard]
  requiring both arguments to be nonnegative integers and the ~c[:measure]
  ~c[(+ i x)].)

  Establishing that a formal is irrelevant, in the sense defined
  above, can be an arbitrarily hard problem because it requires
  theorem proving.  For example, is ~c[x] irrelevant below?
  ~bv[]
  (defun test (i j k x) (if (p i j k) x 0))
  ~ev[]
  Note that the value of ~c[(test i j k x)] is independent of ~c[x] ~-[] thus
  making ~c[x] irrelevant ~-[] precisely if ~c[(p i j k)] is a theorem.
  ACL2's syntactic analysis of a definition does not guarantee to
  notice all irrelevant formals.

  We regard the presence of irrelevant formals as an indication that
  something is wrong with the definition.  We cause an error on such
  definitions and suggest that you recode the definition so as to
  eliminate the irrelevant formals.  If you must have an irrelevant
  formal, one way to ``trick'' ACL2 into accepting the definition,
  without slowing down the execution of your function, is to use the
  formal in an irrelevant way in the ~il[guard].  For example, to admit
  fact, above, with its irrelevant ~c[x] one might use
  ~bv[]
  (defun fact (i x) 
    (declare (xargs :guard (and (integerp i) (<= 0 i) (equal x x))))
    (if (zerop i) 0 (* i (fact (1- i) (cons i x)))))
  ~ev[]
  For those who really want to turn off this feature, we have
  provided a way to use the ~ilc[acl2-defaults-table] for this purpose;
  ~pl[set-irrelevant-formals-ok].

  If you need to introduce a function with an irrelevant formal,
  please explain to the authors why this should be allowed.")

(defun chk-logic-subfunctions (names0 names terms wrld str ctx state)

; Assume we are defining names in terms of terms (1:1 correspondence).  Assume
; also that the definitions are to be :logic.  Then we insist that every
; function used in terms be :logic.  Str is a string used in our error
; message and is either "guard" or "body".

  (cond ((null names) (value nil))
        (t (let ((bad (collect-programs
                       (set-difference-eq (all-fnnames (car terms))
                                          names0)
                       wrld)))
             (cond
              (bad
               (er soft ctx
                   "The ~@0 for ~x1 calls the :program function~#2~[ ~
                    ~&2~/s ~&2~].  We require that :logic definitions be ~
                    defined entirely in terms of :logically defined ~
                    functions.  See :DOC defun-mode."
                   str (car names) bad))
              (t (chk-logic-subfunctions names0 (cdr names) (cdr terms)
                                             wrld str ctx state)))))))

;; RAG - This function strips out the functions which are
;; non-classical in a chk-acceptable-defuns "fives" structure.

#+:non-standard-analysis
(defun get-non-classical-fns-from-list (names wrld fns-sofar)
  (cond ((null names) fns-sofar)
        (t (let ((fns (if (or (not (symbolp (car names)))
                              (classicalp (car names) wrld))
                          fns-sofar
                        (cons (car names) fns-sofar))))
             (get-non-classical-fns-from-list (cdr names) wrld fns)))))

;; RAG - This function takes in a list of terms and returns any
;; non-classical functions referenced in the terms.

#+:non-standard-analysis
(defmacro get-non-classical-fns (lst wrld)
  `(get-non-classical-fns-aux ,lst ,wrld nil))

#+:non-standard-analysis
(defun get-non-classical-fns-aux (lst wrld fns-sofar)
  (cond ((null lst) fns-sofar)
        (t (get-non-classical-fns-aux
            (cdr lst)
            wrld
            (get-non-classical-fns-from-list
             (all-fnnames (car lst)) wrld fns-sofar)))))

;; RAG - this function checks that the measures used to accept the definition
;; are classical.  Note, *0* is a signal that the default measure is being used
;; (see get-measures1) -- and in that case, we know it's classical, since it's
;; just the acl2-count of some tuple consisting of variables in the defun.

#+:non-standard-analysis
(defun strip-zero-measures (lst accum)
  (if (consp lst)
      (if (equal (car lst) *0*)
          (strip-zero-measures (cdr lst) accum)
        (strip-zero-measures (cdr lst) (cons (car lst) accum)))
    accum))

#+:non-standard-analysis
(defun chk-classical-measures (measures names ctx wrld state)
  (let ((non-classical-fns (get-non-classical-fns 
                            (strip-zero-measures measures nil)
                            wrld)))
    (cond ((null non-classical-fns)
           (value nil))
          (t
           (er soft ctx
               "It is illegal to use non-classical measures to justify a ~
                recursive definition.  However, there has been an ~
                attempt to recursively define ~*0 using the ~
                non-classical functions ~*1 in the measure."
               `("<MissingFunction>" "~x*," "~x* and " "~x*, " ,names)
               `("<MissingFunction>" "~x*," "~x* and " "~x*, " 
                 ,non-classical-fns))))))

;; RAG - This function checks that non-classical functions only appear
;; on non-recursive functions.

#+:non-standard-analysis
(defun chk-no-recursive-non-classical (non-classical-fns names mp rel
                                                         measures
                                                         bodies ctx
                                                         wrld state)
  (cond ((and (int= (length names) 1)
              (not (ffnnamep-mod-mbe (car names) (car bodies))))

; Then there is definitely no recursion (see analogous computation in
; putprop-recursivep-lst).  Note that with :bogus-mutual-recursion-ok, a clique
; of size greater than 1 might not actually have any recursion.  But then it
; will be up to the user in this case to eliminate the appearance of possible
; recursion.

         (value nil))
        ((not (null non-classical-fns))
         (er soft ctx
             "It is illegal to use non-classical functions in a ~
              recursive definition.  However, there has been an ~
              attempt to recursively define ~*0 using the ~
              non-classical function ~*1."
             `("<MissingFunction>" "~x*," "~x* and " "~x*, " ,names)
             `("<MissingFunction>" "~x*," "~x* and " "~x*, " 
               ,non-classical-fns)))
        ((not (and (classicalp mp wrld)
                   (classicalp rel wrld)))
         (er soft ctx
             "It is illegal to use a non-classical function as a ~
              well-ordering or well-ordered domain in a recursive ~
              definition.  However, there has been an ~
              attempt to recursively define ~*0 using the ~
              well-ordering function ~x* and domain ~x*."
             `("<MissingFunction>" "~x*," "~x* and " "~x*, " ,names)
             mp
             rel))
        (t
         (chk-classical-measures measures names ctx wrld state))))
    
(defun union-collect-non-x (x lst)
  (cond ((endp lst) nil)
        (t (union-equal (collect-non-x x (car lst))
                        (union-collect-non-x x (cdr lst))))))

(defun translate-measures (terms ctx wrld state)

; WARNING: Keep this in sync with translate-term-lst.  Here we allow (:? var1
; ... vark), where the vari are distinct variables.

  (cond ((null terms) (value nil))
        (t (er-let*
            ((term
              (cond ((and (consp (car terms))
                          (eq (car (car terms)) :?))
                     (cond ((arglistp (cdr (car terms)))
                            (value (car terms)))
                           (t (er soft ctx
                                  "A measure whose car is :? must be of the ~
                                   form (:? v1 ... vk), where (v1 ... vk) is ~
                                   a list of distinct variables.  The measure ~
                                   ~x0 is thus illegal."
                                  (car terms)))))
                    (t
                     (translate (car terms) t t t ctx wrld state))))
             (rst (translate-measures (cdr terms) ctx wrld state)))
            (value (cons term rst))))))

;; RAG - I modified the function below to check for recursive
;; definitions using non-classical predicates. 

(defun chk-acceptable-defuns (lst ctx wrld state #+:non-standard-analysis std-p)

; Rockwell Addition:  We now also return the non-executable flag.

; This function does all of the syntactic checking associated with
; defuns.  It causes an error if it doesn't like what it sees.  It
; returns the traditional 3 values of an error-producing,
; output-producing function.  However, the "real" value of the
; function is a list of 18 items extracted from lst during the
; checking.  These items are:

;    names     - the names of the fns in the clique
;    arglists  - their formals
;    docs      - their documentation strings
;    pairs     - the (section-symbol . citations) pairs parsed from docs
;    guards    - their translated guards
;    measures  - their translated measure terms
;    mp        - the domain predicate (e.g., o-p) for well-foundedness
;    rel       - the well-founded relation (e.g., o<)
;    hints     - their translated hints, to be used during the proofs of
;                the measure conjectures, all flattened into a single list
;                of hints of the form ((cl-id . settings) ...).
;    guard-hints
;              - like hints but to be used for the guard conjectures
;    std-hints (always returned, but only of interest when
;               #+:non-standard-analysis)
;              - like hints but to be used for the std-p conjectures
;    otf-flg   - t or nil, used as "Onward Thru the Fog" arg for prove
;    bodies    - their translated bodies
;    symbol-class
;              - :program, :ideal, or :common-lisp-compliant
;    normalizeps
;              - list of Booleans, used to determine for each fn in the clique
;                whether its body is to be normalized
;    reclassifyingp
;              - t or nil, t if this is a reclassifying from :program
;                with identical defs.
;    wrld      - a modified wrld in which the following properties
;                may have been stored for each fn in names:
;                  'formals, 'stobjs-in and 'stobjs-out
;    non-executablep - t or nil according to whether these defuns are to
;                  non-executable.  Non-executable defuns may violate the
;                  translate conventions on stobjs.

  (er-let*
   ((fives (chk-defuns-tuples lst ctx wrld state)))

; Fives is a list in 1:1 correspondence with lst.  Each element of
; fives is a 5-tuple of the form (name args doc edcls body).  Consider the
; element of fives that corresponds to

;   (name args (DECLARE ...) "Doc" (DECLARE ...) body)

; in lst.  Then that element of fives is (name args "Doc" (...) body),
; where the ... is the cdrs of the DECLARE forms appended together.
; No translation has yet been applied to them.  The newness of name
; has not been checked yet either, though we know it is all but new,
; i.e., is a symbol in the right package.  We do know that the args
; are all legal.

   (er-progn
    (chk-no-duplicate-defuns (strip-cars fives) ctx state)
    (chk-xargs-keywords fives
                        '(:non-executable
                          :guard :measure :stobjs :well-founded-relation
                          :hints
                          :guard-hints
                          #+:non-standard-analysis :std-hints
                          :mode :verify-guards
                          :otf-flg :normalize)
                        ctx state)
    (let* ((names (strip-cars fives))
           (arglists (strip-cadrs fives))
           (ignores (get-ignores fives))
           (ignorables (get-ignorables fives))
           (assumep (or (eq (ld-skip-proofsp state) 'include-book)
                        (eq (ld-skip-proofsp state) 'include-book-with-locals)))
           (do-not-translate-hints
            (or assumep
                (eq (ld-skip-proofsp state) 'initialize-acl2)))
           (docs (get-docs fives)))
      (er-let*
       ((stobjs-in-lst (get-stobjs-in-lst fives ctx wrld state))
        (guards (translate-term-lst (get-guards fives wrld)

; Stobjs-out:  Each guard returns one, non-stobj result.  This arg
; is used for each guard.
                                    '(nil)

; Logic-modep: Since guards have nothing to do with the logic, and
; since they may legitimately have mode :program, we set logic-modep
; to nil here.  This arg is used for each guard.

                                    nil

; Known-stobjs-lst:
; Here is a slight abuse.  Translate-term-lst is expecting, in this
; argument, a list in 1:1 correspondence with its first argument,
; specifying the known-stobjs for the translation of corresponding
; terms.  But we are supplying the stobjs-in for the term, not the
; known-stobjs.  The former is a list of stobj flags and the latter is
; a list of stobj names, i.e., the list we supply may contain a NIL
; element where it should have no element at all.  This is allowed by
; stobjsp.  Technically we ought to map over the stobjs-in-lst and
; change each element to its collect-non-x nil.

                                    stobjs-in-lst

                                    ctx wrld state))

; By using stobjs-out '(nil) we enable the thorough checking of the
; use of state.  Thus, the above call ensures that guards do not
; modify (or return) state.  We are taking the conservative position
; because intuitively there is a confusion over the question of
; whether, when, and how often guards are run.  By prohibiting them
; from modifying state we don't have to answer the questions about
; when they run.

        (non-executablep
         (get-unambiguous-xargs-flg :NON-EXECUTABLE
                                    fives
                                    nil ctx state))
        (defun-mode (get-unambiguous-xargs-flg :MODE
                                               fives
                                               (default-defun-mode wrld)
                                               ctx state))
        (verify-guards (get-unambiguous-xargs-flg :VERIFY-GUARDS
                                                  fives
                                                  '(unspecified)
                                                  ctx state)))
       (er-progn 
        (chk-defun-mode defun-mode names ctx state)
        (cond ((not (or (eq non-executablep t)
                        (eq non-executablep nil)))
               (er soft ctx
                   "The :NON-EXECUTABLE flag must be T or NIL, but ~
                      ~x0 is neither."
                   non-executablep))
              ((and non-executablep
                    (eq defun-mode :program))
               (er soft ctx
                   ":NON-EXECUTABLE functions must be defined in :LOGIC ~
                    mode."))
              ((and non-executablep
                    (union-collect-non-x nil stobjs-in-lst))
               (er soft ctx
                   ":NON-EXECUTABLE functions may not declare any ~
                    single-threaded objects, but you used ~&0."
                   (union-collect-non-x nil stobjs-in-lst)))
              (t (value nil)))
        (cond ((consp verify-guards)

; This means that the user did not specify a :verify-guards.  We will default
; it appropriately.

               (value nil))
              ((eq defun-mode :program)
               (if (eq verify-guards nil)
                   (value nil)
                 (er soft ctx
                     "When the :MODE is :program, the only legal ~
                      :VERIFY-GUARDS setting is NIL.  ~x0 is illegal."
                     verify-guards)))
              ((or (eq verify-guards nil)
                   (eq verify-guards t))
               (value nil))
              (t (er soft ctx
                     "The legal :VERIFY-GUARD settings are NIL and T.  ~x0 is ~
                      illegal."
                     verify-guards)))
        (let* ((symbol-class (cond ((eq defun-mode :program) :program)
                                   ((consp verify-guards)
                                    (cond
                                     ((= (default-verify-guards-eagerness wrld)
                                         0)
                                      :ideal)
                                     ((= (default-verify-guards-eagerness wrld)
                                         1)
                                      (if (get-guardsp fives wrld)
                                          :common-lisp-compliant
                                        :ideal))
                                     (t :common-lisp-compliant)))
                                   (verify-guards :common-lisp-compliant)
                                   (t :ideal)))
               (rc (redundant-or-reclassifying-defunsp
                    defun-mode symbol-class (ld-skip-proofsp state) lst wrld)))
          (cond
           ((eq rc 'redundant)
            (value 'redundant))
           ((eq rc 'verify-guards)

; We avoid needless complication by simply causing a polite error in this
; case.  If that proves to be too inconvenient for users, we could look into
; arranging for a call of verify-guards here.

            (let ((include-book-path
                   (global-val 'include-book-path wrld)))
              (mv-let
               (erp ev-wrld-and-cmd-wrld state)
               (state-global-let*
                ((inhibit-output-lst
                  (cons 'error (f-get-global 'inhibit-output-lst state))))

; Keep the following in sync with pe-fn.

                (let ((wrld (w state)))
                  (er-let*
                   ((ev-wrld (er-decode-logical-name (car names) wrld :pe state))
                    (cmd-wrld (superior-command-world ev-wrld wrld :pe
                                                      state)))
                   (value (cons ev-wrld cmd-wrld)))))
               (mv-let (erp1 val1 state)
                       (er soft ctx
                           "The definition of ~x0~#1~[~/ (along with the ~
                            others in its mutual-recursion clique)~]~@2 ~
                            demands guard verification, but there is already ~
                            a corresponding existing definition without its ~
                            guard verified.  ~@3Use verify-guards instead; ~
                            see :DOC verify-guards. ~#4~[Here is the existing ~
                            definition of ~x0:~/The existing definition of ~
                            ~x0 appears to precede this one in the same ~
                            top-level command.~]"
                           (car names)
                           names
                           (cond
                            (include-book-path
                             (cons " in the book ~xa"
                                   (list (cons #\a (car include-book-path)))))
                            (t ""))
                           (cond
                            ((cddr include-book-path)
                             (cons "Note: The above book is included under ~
                                    the following sequence of included books ~
                                    from outside to inside, i.e., top-level ~
                                    included book first:~|~&b.~|"
                                   (list (cons #\b (reverse
                                                    (cdr include-book-path))))))
                            ((cdr include-book-path)
                             (cons "Note: The above book is included inside ~
                                    the book ~xb.  "
                                   (list (cons #\b (cadr include-book-path)))))
                            (t ""))
                           (if erp 1 0))
                       (pprogn (if erp
                                   state
                                 (pe-fn1 wrld (standard-co state)
                                         (car ev-wrld-and-cmd-wrld)
                                         (cdr ev-wrld-and-cmd-wrld)
                                         state))
                               (mv erp1 val1 state))))))
           (t
            (er-let*
             ((wrld0 (chk-just-new-names names 'function rc ctx wrld state))
              (doc-pairs (translate-doc-lst names docs ctx state))
              (untranslated-measures

; If the defun-mode is :program, or equivalently, the symbol-class is :program,
; then we don't need the measures.  But we do need "measures" that pass the
; tests below, such as the call of chk-free-and-ignored-vars-lsts.  So, we
; simply pretend that no measures were supplied, which is clearly reasonable if
; we are defining the functions to have symbol-class :program.

               (get-measures symbol-class fives ctx state))
              (measures (translate-measures untranslated-measures ctx wrld0
                                            state))

; We originally used stobjs-out '(nil) above, again, because we felt
; uneasy about measures changing state.  But we know no logical
; justification for this feeling, nor do we ever expect to execute the
; measures in Common Lisp.  In fact we find it useful to be able to
; pass state into a measure even when its argument position isn't
; "state"; consider for example the function big-clock-entry.

              (rel (get-unambiguous-xargs-flg
                    :WELL-FOUNDED-RELATION
                    fives
                    (default-well-founded-relation wrld)
                    ctx state))
              (hints (if (or do-not-translate-hints
                             (eq defun-mode :program))
                         (value nil)
                       (let ((hints (get-hints fives)))
                         (if hints
                             (translate-hints
                              (cons "Measure Lemma for" (car names))
                              (append hints (default-hints wrld))
                              ctx wrld0 state)
                           (value nil)))))
              (guard-hints (if (or do-not-translate-hints
                                   (eq defun-mode :program))
                               (value nil)
                             (let ((guard-hints
                                    (get-guard-hints fives)))
                               (if guard-hints
                                   (translate-hints
                                    (cons "Guard for" (car names))
                                    (append guard-hints
                                            (default-hints wrld))
                                    ctx wrld0 state)
                                 (value nil)))))
              (std-hints #+:non-standard-analysis
                         (cond
                          ((and std-p (not assumep))
                           (translate-hints 
                            (cons "Std-p for" (car names))
                            (append (get-std-hints fives)
                                    (default-hints wrld))
                            ctx wrld0 state))
                          (t (value nil)))
                         #-:non-standard-analysis
                         (value nil))
              (otf-flg (if do-not-translate-hints
                           (value nil)
                         (get-unambiguous-xargs-flg :OTF-FLG
                                                    fives t ctx state)))
              (normalizeps (get-normalizeps fives nil ctx state)))
             (er-progn
              (cond
               ((not (and (symbolp rel)
                          (assoc-eq
                           rel
                           (global-val 'well-founded-relation-alist
                                       wrld))))
                (er soft ctx
                    "The :WELL-FOUNDED-RELATION specified by XARGS must be a ~
                     symbol which has previously been shown to be a ~
                     well-founded relation.  ~x0 has not been. See :DOC ~
                     well-founded-relation."
                    rel))
               (t (value nil)))
              (let ((mp (cadr (assoc-eq
                               rel
                               (global-val 'well-founded-relation-alist
                                           wrld))))
                    (big-mutrec (big-mutrec names)))
                (er-let*
                 ((wrld1 (update-w big-mutrec
                                   (putprop-x-lst2 names 'formals
                                                   arglists wrld0)))
                  (wrld2 (update-w big-mutrec
                                   (store-stobjs-ins names stobjs-in-lst
                                                     wrld1)))
                  (bodies-and-bindings
                   (translate-bodies non-executablep
                                     names
                                     (get-bodies fives)
; Slight abuse here, see guards translation above:
                                     stobjs-in-lst
                                     ctx wrld2 state)))
                 (let* ((bodies (car bodies-and-bindings))
                        (bindings
                         (super-defun-wart-bindings
                          (cdr bodies-and-bindings)))
                        #+:non-standard-analysis
                        (non-classical-fns
                         (get-non-classical-fns bodies wrld)))
                   (er-progn
                    (if assumep
                        (value nil)
                      (er-progn
                       (chk-stobjs-out-bound names bindings ctx state)
                       #+:non-standard-analysis
                       (chk-no-recursive-non-classical 
                        non-classical-fns
                        names mp rel measures bodies ctx wrld state)))
                    (let* ((wrld30 (store-super-defun-warts-stobjs-in
                                    names wrld2))
                           (wrld3 #+:non-standard-analysis
                                  (if (or std-p
                                          (null non-classical-fns))
                                      wrld30
                                    (putprop-x-lst1 names 'classicalp
                                                    nil wrld30))
                                  #-:non-standard-analysis 
                                  wrld30))
                      (er-progn
                       (if (eq defun-mode :logic)

; Although translate checks for inappropriate calls of :program functions,
; translate11 and translate1 do not.

                           (er-progn
                            (chk-logic-subfunctions names names
                                                    guards wrld3 "guard"
                                                    ctx state)
                            (chk-logic-subfunctions names names bodies
                                                    wrld3 "body"
                                                    ctx state))
                         (value nil))
                       (if (eq symbol-class :common-lisp-compliant)
                           (er-progn
                            (chk-common-lisp-compliant-subfunctions
                             names names guards wrld3 "guard" ctx state)
                            (chk-common-lisp-compliant-subfunctions
                             names names bodies wrld3 "body" ctx state))
                         (value nil))
                       (mv-let
                        (erp val state)
; This mv-let is just an aside that lets us conditionally check a bunch of
; conditions we needn't do in assumep mode.
                        (cond
                         (assumep (mv nil nil state))
                         (t
                          (er-progn
                           (chk-free-and-ignored-vars-lsts names
                                                           arglists
                                                           guards
                                                           measures
                                                           ignores
                                                           ignorables
                                                           bodies
                                                           ctx state)
                           (chk-irrelevant-formals names arglists
                                                   guards
                                                   measures
                                                   ignores
                                                   ignorables
                                                   bodies ctx state)
                           (chk-mutual-recursion names bodies ctx
                                                 state))))
                        (cond
                         (erp (mv erp val state))
                         (t (value (list 'chk-acceptable-defuns
                                         names
                                         arglists
                                         docs
                                         doc-pairs
                                         guards
                                         measures
                                         mp
                                         rel
                                         hints
                                         guard-hints
                                         std-hints ;nil for non-std
                                         otf-flg
                                         bodies
                                         symbol-class
                                         normalizeps
                                         (and (eq rc 'reclassifying)
                                              (all-programp names wrld))
                                         (store-stobjs-out
                                          names 
                                          bindings
                                          wrld3)
                                         non-executablep
                                         ))))))))))))))))))))))

(deflabel XARGS
  :doc
  ":Doc-Section Miscellaneous

  giving ~il[hints] to ~ilc[defun]~/

  Common Lisp's ~ilc[defun] function does not easily allow one to pass extra
  arguments such as ``~il[hints]''.  ACL2 therefore supports a peculiar new
  declaration (~pl[declare]) designed explicitly for passing
  additional arguments to ~ilc[defun] via a keyword-like syntax.

  The following declaration is nonsensical but does illustrate all of
  the ~ilc[xargs] keywords:
  ~bv[]
  (declare (xargs :guard (symbolp x)
                  :guard-hints ((\"Goal\" :in-theory (theory batch1)))
                  :hints ((\"Goal\" :in-theory (theory batch1)))
                  :measure (- i j)
                  :mode :logic
                  :non-executable t
                  :normalize nil
                  :otf-flg t
                  :stobjs ($s)
                  :verify-guards t
                  :well-founded-relation my-wfr))~/

  General Form:
  (xargs :key1 val1 ... :keyn valn)
  ~ev[]
  where the keywords and their respective values are as shown below.
  Note that once ``inside'' the xargs form, the ``extra arguments'' to
  ~ilc[defun] are passed exactly as though they were keyword arguments.

  ~c[:]~ilc[GUARD]~nl[]
  ~c[Value] is a term involving only the formals of the function being
  defined.  The actual ~il[guard] used for the definition is the
  conjunction of all the ~il[guard]s and types (~pl[declare]) ~il[declare]d.

  ~c[:GUARD-HINTS]~nl[]
  ~c[Value]:  hints (~pl[hints]), to be used during the ~il[guard]
  verification proofs as opposed to the termination proofs of the
  ~ilc[defun].

  ~c[:]~ilc[HINTS]~nl[]
  Value:  hints (~pl[hints]), to be used during the termination
  proofs as opposed to the ~il[guard] verification proofs of the ~ilc[defun].

  ~c[:MEASURE]~nl[]
  ~c[Value] is a term involving only the formals of the function being
  defined.  This term is indicates what is getting smaller in the
  recursion.  The well-founded relation with which successive measures
  are compared is ~ilc[o<].  Also allowed is a special case,
  ~c[(:? v1 ... vk)], where ~c[(v1 ... vk)] enumerates a subset of the
  formal parameters such that some valid measure involves only those
  formal parameters.  However, this special case is only allowed for
  definitions that are redundant (~pl[redundant-events]) or are
  executed when skipping proofs (~pl[skip-proofs]).

  ~c[:MODE]~nl[]
  ~c[Value] is ~c[:]~ilc[program] or ~c[:]~ilc[logic], indicating the ~ilc[defun] mode of the
  function introduced.  ~l[defun-mode].  If unspecified, the
  ~ilc[defun] mode defaults to the default ~ilc[defun] mode of the current ~il[world].
  To convert a function from ~c[:]~ilc[program] mode to ~c[:]~ilc[logic] mode,
  ~pl[verify-termination].

  ~c[:NON-EXECUTABLE]~nl[]
  ~c[Value] is ~c[t] or ~c[nil] (the default).  If ~c[t], the function has no
  executable counterpart and is permitted to use single-threaded object names
  and functions arbitrarily, as in theorems rather than as in executable
  definitions.  Such functions are not permitted to declare any names to be
  ~c[:]~ilc[stobj]s but accessors, etc., may be used, just as in theorems.
  Since the default is ~c[nil], the value supplied is only of interest when it
  is ~c[t].

  ~c[:NORMALIZE]~nl[]
  Value is a flag telling ~ilc[defun] whether to propagate ~ilc[if] tests
  upward.  Since the default is to do so, the value supplied is only of
  interest when it is ~c[nil].
  (~l[defun]).

  ~c[:]~ilc[OTF-FLG]~nl[]
  Value is a flag indicating ``onward through the fog''
  (~pl[otf-flg]).

  ~c[:STOBJS]~nl[]
  ~c[Value] is either a single ~ilc[stobj] name or a true list of stobj names.
  Every stobj name among the formals of the function must be listed, if the
  corresponding actual is to be treated as a stobj.  That is, if a function
  uses a stobj name as a formal parameter but the name is not declared among
  the ~c[:stobjs] then the corresponding argument is treated as ordinary.
  The only exception to this rule is ~ilc[state]:  whether you include it
  or not, ~c[state] is always treated as a single-threaded object.  This
  declaration has two effects.  One is to enforce the syntactic restrictions
  on single-threaded objects.  The other is to strengthen the ~ilc[guard] of
  the function being defined so that it includes conjuncts specifying that
  each declared single-threaded object argument satisfies the recognizer for
  the corresponding single-threaded object.

  ~c[:]~ilc[VERIFY-GUARDS]~nl[]
  ~c[Value] is ~c[t] or ~c[nil], indicating whether or not ~il[guard]s are to be
  verified upon completion of the termination proof.  This flag should
  only be ~c[t] if the ~c[:mode] is unspecified but the default ~ilc[defun] mode is
  ~c[:]~ilc[logic], or else the ~c[:mode] is ~c[:]~ilc[logic].

  ~c[:]~ilc[WELL-FOUNDED-RELATION]~nl[]
  ~c[Value] is a function symbol that is known to be a well-founded
  relation in the sense that a rule of class ~c[:]~ilc[well-founded-relation]
  has been proved about it.  ~l[well-founded-relation].~/")

(defmacro link-doc-to-keyword (name parent see)
  `(defdoc ,name
     ,(concatenate
       'string
       ":Doc-Section "
       (symbol-name parent)
       "

  "
       (string-downcase (symbol-name see))
       " keyword ~c[:" (symbol-name name) "]~/

  ~l["
       (string-downcase (symbol-name see))
       "].~/~/")))

(defmacro link-doc-to (name parent see)
  `(defdoc ,name
     ,(concatenate
       'string
       ":Doc-Section "
       (symbol-package-name parent)
       "::"
       (symbol-name parent)
       "

  ~l["
       (string-downcase (symbol-name see))
       "].~/~/~/")))

(link-doc-to-keyword guard-hints miscellaneous xargs)
(link-doc-to-keyword measure miscellaneous xargs)
(link-doc-to-keyword mode miscellaneous xargs)
(link-doc-to-keyword non-executable miscellaneous xargs)
(link-doc-to-keyword normalize miscellaneous xargs)
(link-doc-to-keyword stobjs miscellaneous xargs)

(link-doc-to-keyword do-not-induct miscellaneous hints)
(link-doc-to-keyword do-not miscellaneous hints)
(link-doc-to-keyword expand miscellaneous hints)
(link-doc-to-keyword restrict miscellaneous hints)
(link-doc-to-keyword hands-off miscellaneous hints)
(link-doc-to-keyword induct miscellaneous hints)
(link-doc-to-keyword use miscellaneous hints)
(link-doc-to-keyword cases miscellaneous hints)
(link-doc-to-keyword by miscellaneous hints)
(link-doc-to-keyword nonlinearp miscellaneous hints)

(link-doc-to read-byte$ programming io)
(link-doc-to open-input-channel programming io)
(link-doc-to open-input-channel-p programming io)
(link-doc-to close-input-channel programming io)
(link-doc-to read-char$ programming io)
(link-doc-to peek-char$ programming io)
(link-doc-to read-object programming io)
(link-doc-to open-output-channel programming io)
(link-doc-to open-output-channel-p programming io)
(link-doc-to close-output-channel programming io)
(link-doc-to write-byte$ programming io)
(link-doc-to print-object$ programming io)

(link-doc-to lambda miscellaneous term)
(link-doc-to untranslate miscellaneous user-defined-functions-table)

(link-doc-to set-ld-skip-proofsp other ld-skip-proofsp)
(link-doc-to set-ld-redefinition-action other ld-redefinition-action)

#+:non-standard-analysis
(defun build-valid-std-usage-clause (arglist body)
  (cond ((null arglist)
         (list (mcons-term* 'standard-numberp body)))
        (t (cons (mcons-term* 'not
                              (mcons-term* 'standard-numberp (car arglist)))
                 (build-valid-std-usage-clause (cdr arglist) body)))))

#+:non-standard-analysis
(defun verify-valid-std-usage (names arglists bodies hints otf-flg
                                     ttree0 ctx ens wrld state)
  (cond
   ((null (cdr names))
    (let* ((name (car names))
           (arglist (car arglists))
           (body (car bodies)))
      (mv-let
       (cl-set cl-set-ttree)
       (clean-up-clause-set
        (list (build-valid-std-usage-clause arglist body))
        ens
        wrld ttree0 state)
       (pprogn
        (increment-timer 'other-time state)
        (let ((displayed-goal (prettyify-clause-set
                               cl-set
                               (let*-abstractionp state)
                               wrld)))
          (pprogn
           (cond ((null cl-set)
                  (io? event nil state
                       (name)
                       (fms "~%The admission of ~x0 as a classical function ~
                               is trivial."
                            (list (cons #\0 name))
                            (proofs-co state)
                            state
                            nil)))
                 (t
                  (io? event nil state
                       (displayed-goal name)
                       (fms "~%The admission of ~x0 as a classical function ~
                             with non-classical body requires that it return ~
                             standard values for standard arguments.  That ~
                             is, we must prove~%~%Goal~%~q1."
                            (list (cons #\0 name)
                                  (cons #\1 displayed-goal))
                            (proofs-co state)
                            state
                            nil))))
           (increment-timer 'print-time state)
           (cond
            ((null cl-set)
             (value cl-set-ttree))
            (t
             (mv-let (erp ttree state)
                     (prove (termify-clause-set cl-set)
                            (make-pspv ens
                                       wrld
                                       :displayed-goal displayed-goal
                                       :otf-flg otf-flg)
                            hints ens wrld ctx state)
                     (cond (erp (mv t nil state))
                           (t
                            (pprogn
                             (io? event nil state
                                  (name)
                                  (fms "That completes the proof that ~x0 ~
                                        returns standard values for standard ~
                                        arguments."
                                       (list (cons #\0 name))
                                       (proofs-co state)
                                       state
                                       nil))
                             (increment-timer 'print-time state)
                             (value (cons-tag-trees 
                                     cl-set-ttree
                                     ttree))))))))))))))
   (t (er soft ctx
          "It is not permitted to use MUTUAL-RECURSION to define non-standard ~
           predicates.  Use MUTUAL-RECURSION to define standard versions of ~
           these predicates, then use DEFUN-STD to generalize them, if that's ~
           what you mean."))))

(defun defuns-fn0

; WARNING: This function installs a world.  That is safe at the time of this
; writing because this function is only called by defuns-fn, where that call is
; protected by a revert-world-on-error.

  (names arglists docs pairs guards measures mp rel hints guard-hints
         std-hints
         otf-flg bodies symbol-class normalizeps non-executablep
         #+:non-standard-analysis std-p
         ctx wrld state)
  #-:non-standard-analysis
  (declare (ignore std-hints))
  (cond
   ((eq symbol-class :program)
    (defuns-fn-short-cut names docs pairs guards bodies wrld state))
   (t
    (let ((ens (ens state))
          (big-mutrec (big-mutrec names)))
      (er-let*
       ((trip (put-induction-info names arglists
                                  measures
                                  bodies
                                  mp rel
                                  hints
                                  otf-flg
                                  big-mutrec
                                  ctx ens wrld state)))
       (let ((col (car trip)))
         (er-let*
          ((wrld1 (update-w big-mutrec (cadr trip)))
           (wrld2 (update-w big-mutrec
                            (putprop-defun-runic-mapping-pairs names t wrld1)))
           (wrld3 (update-w big-mutrec
                            (putprop-x-lst2-unless names 'guard guards *t*
                                                   wrld2)))

; There is no wrld4.

; Rockwell Addition:  To save time, the nu-rewriter doesn't look at
; functions unless they contain nu-rewrite targets, as defined in 
; rewrite.lisp.  Here is where I store the property that says whether a
; function is a target.

           (wrld5 (update-w
                   big-mutrec
                   (cond ((eq (car names) 'NTH)
                          (putprop 'nth 'nth-update-rewriter-targetp
                                   t wrld3))
                         ((getprop (car names) 'recursivep nil
                                   'current-acl2-world wrld3)

; Nth-update-rewriter does not go into recursive functions.  We could consider
; redoing this computation when installing a new definition rule, as well as
; the putprop below, but that's a heuristic decision that doesn't seem to be so
; important.

                          wrld3)
                         ((nth-update-rewriter-targetp (car bodies) wrld3)

; This precomputation of whether the body of the function is a
; potential target for nth-update-rewriter is insensitive to whether
; the functions being seen are disabled.  If the function being
; defined calls a non-recursive function that uses NTH, then this
; function is marked as being a target.  If later that subroutine is
; disabled, then nth-update-rewriter will not go into it and this
; function may no longer really be a potential target.  But if we do
; not ``memoize'' the computation this way then it may be
; exponentially slow, going repeatedly into large bodies called
; more than one time in a function.

                          (putprop (car names)
                                   'nth-update-rewriter-targetp
                                   t wrld3))
                         (t wrld3))))
           #+:non-standard-analysis
           (assumep
            (value (or (eq (ld-skip-proofsp state) 'include-book)
                       (eq (ld-skip-proofsp state)
                           'include-book-with-locals))))
           (ttree1 #+:non-standard-analysis
                   (if (and std-p (not assumep))
                       (verify-valid-std-usage names arglists bodies
                                               std-hints otf-flg
                                               (caddr trip)
                                               ctx ens wrld state)
                     (value (caddr trip)))
                   #-:non-standard-analysis
                   (value (caddr trip))))
          (mv-let
           (wrld6 ttree2)
           (putprop-body-lst names arglists bodies normalizeps
                             (getprop (car names) 'recursivep nil
                                      'current-acl2-world wrld5)
                             (make-controller-alist names wrld5)
                             #+:non-standard-analysis std-p
                             ens wrld5 wrld5 nil)
           (er-progn
            (update-w big-mutrec wrld6)
            (mv-let
             (wrld7 ttree2 state)
             (putprop-type-prescription-lst names
                                            (fn-rune-nume (car names)
                                                          t nil wrld6)
                                            ens wrld6 ttree2 state)
             (er-progn
              (update-w big-mutrec wrld7)
              (er-let*
               ((wrld8 (update-w big-mutrec
                                 (putprop-level-no-lst names wrld7)))
                (wrld9 (update-w big-mutrec
                                 (putprop-primitive-recursive-defunp-lst
                                  names wrld8)))
                (wrld10 (update-w big-mutrec
                                  (update-doc-data-base-lst names docs pairs
                                                            wrld9)))
                (wrld11 (update-w big-mutrec
                                  (putprop-x-lst1 names 'congruences nil wrld10)))
                (wrld11a (update-w big-mutrec
                                   (putprop-x-lst1 names 'coarsenings nil
                                                   wrld11)))
                (wrld11b (update-w big-mutrec
                                   (if non-executablep
                                       (putprop-x-lst1 names 'non-executablep
                                                       t
                                                       wrld11a)
                                     wrld11a))))
               (let ((wrld12
                      #+:non-standard-analysis
                      (if std-p
                          (putprop-x-lst1
                           names 'unnormalized-body nil
                           (putprop-x-lst1 names 'def-bodies nil wrld11b))
                        wrld11b)
                      #-:non-standard-analysis
                      wrld11b))
                 (pprogn
                  (print-defun-msg names ttree2 wrld12 col state)
                  (set-w 'extension wrld12 state)
                  (cond
                   ((eq symbol-class :common-lisp-compliant)
                    (er-let*
                     ((guard-hints
                       (if guard-hints
                           (value guard-hints)
                         (let ((default-hints (default-hints wrld12)))
                           (if default-hints ; then we haven't yet translated
                               (translate-hints
                                (cons "Guard for" (car names))
                                default-hints
                                ctx wrld12 state)
                             (value nil)))))
                      (pair (verify-guards-fn1 names guard-hints otf-flg
                                               ctx state)))

; Pair is of the form (wrld . ttree3) and we return a pair of the same
; form, but we must combine this ttree with the ones produced by the
; termination proofs and type-prescriptions.

                     (value
                      (cons (car pair)
                            (cons-tag-trees ttree1
                                            (cons-tag-trees
                                             ttree2
                                             (cdr pair)))))))
                   (t (value
                       (cons wrld12
                             (cons-tag-trees ttree1
                                             ttree2)))))))))))))))))))

(defun strip-non-hidden-package-names (known-package-alist)
  (if (endp known-package-alist)
      nil
    (let ((package-entry (car known-package-alist)))
      (cond ((package-entry-hidden-p package-entry)
             (strip-non-hidden-package-names (cdr known-package-alist)))
            (t (cons (package-entry-name package-entry)
                     (strip-non-hidden-package-names (cdr known-package-alist))))))))

(defun in-package-fn (str state)

; Important Note:  Don't change the formals of this function without
; reading the *initial-event-defmacros* discussion in axioms.lisp.

  (cond ((not (stringp str))
         (er soft 'in-package
             "The argument to IN-PACKAGE must be a string, but ~
              ~x0 is not."
             str))
        ((not (find-non-hidden-package-entry str (known-package-alist state)))
         (er soft 'in-package
             "The argument to IN-PACKAGE must be a known package ~
              name, but ~x0 is not.  The known packages are ~*1"
             str
             (tilde-*-&v-strings
              '&
              (strip-non-hidden-package-names (known-package-alist state))
              #\.)))
        (t (let ((state (f-put-global 'current-package str state)))
             (value str)))))

(defun defstobj-functionsp (names embedded-event-lst)

; This function determines whether all the names in names are being
; defined as part of a defstobj event.  If so, it returns the
; name of the stobj; otherwise, nil.

; Explanation of the context: Defstobj uses defun to define the recognizers,
; accessors and updaters.  But defstobj must install its own versions of the
; raw lisp code for these functions, to take advantage of the
; single-threadedness of their use.  So what happens when defstobj executes
; (defun name ...), where name is say an updater?  Defuns-fn is run on the
; singleton list '(name) and the axiomatic def of name.  At the end of the
; normal processing, defuns-fn computes a CLTL-COMMAND for name.  When this
; command is installed by add-trip, it sets the symbol-function of name to the
; given body.  Add-trip also installs a *1*name definition by oneifying the
; given body.  But in the case of a defstobj function we do not want the first
; thing to happen: defstobj will compute a special body for the name and
; install it with its own CLTL-COMMAND.  So to handle defstobj, defuns-fn tells
; add-trip not to set the symbol-function.  This is done by setting the ignorep
; flag in the defun CLTL-COMMAND.  So the question arises: how does defun know
; that the name it is defining is being introduced by defstobj?  This function
; answers that question.

; Note that *1*name should still be defined as the oneified axiomatic body, as
; with any defun.  Before v2-9 we introduced the *1* function at defun time.
; (We still do so if the function is being reclassified with an identical body,
; from :program mode to :logic mode, since there is no need to redefine its
; symbol-function -- -- indeed its installed symbol-function might be
; hand-coded as part of these sources -- but add-trip must generate a *1*
; body.)  Because stobj functions can be inlined as macros (via the :inline
; keyword of defstobj), we need to defer definition of the *1* function until
; after the raw Lisp def (which may be a macro) has been added.  We failed to
; do this in v2-8, which caused an error in openMCL as reported by John
; Matthews:

;   (defstobj tiny-state
;           (progc :type (unsigned-byte 10) :initially 0)
;         :inline t)
;
;   (update-progc 3 tiny-state)

; Note: At the moment, defstobj does not introduce any mutually
; recursive functions.  So every name is handled separately by
; defuns-fns.  Hence, names, here, is always a singleton, though we do
; not exploit that. Also, embedded-event-lst is always a list
; ee-entries, each being a cons with the name of some superevent like
; ENCAPSULATE, INCLUDE-BOOK, or DEFSTOBJ, in the car.  The ee-entry
; for the most immediate superevent is the first on the list.  At the
; moment, defstobj does not use encapsulate or other structuring
; mechanisms.  Thus, the defstobj ee-entry will be first on the list.
; But we look up the list, just in case.  The ee-entry for a defstobj
; is of the form (defstobj name names) where name is the name of the
; stobj and names is the list of recognizers, accessors and updaters
; and their helpers.

  (let ((temp (assoc-eq 'defstobj embedded-event-lst)))
    (cond ((and temp
                (subsetp-equal names (caddr temp)))
           (cadr temp))
          (t nil))))

; The following definition only supports non-standard analysis, but it seems
; reasonable to allow it in the standard version too.
; #+:non-standard-analysis
(defun index-of-non-number (lst)
  (cond
   ((endp lst) nil)
   ((acl2-numberp (car lst))
    (let ((temp (index-of-non-number (cdr lst))))
      (and temp (1+ temp))))
   (t 0)))

#+:non-standard-analysis
(defun non-std-error (fn index formals actuals)
  (er hard fn
   "Function ~x0 was called with the ~n1 formal parameter, ~x2, bound to ~
    actual parameter ~x3, which is not a (standard) number.  This is illegal, ~
    because the arguments of a function defined with defun-std must all be ~
    (standard) numbers."
   fn (list index) (nth index formals) (nth index actuals)))

#+:non-standard-analysis
(defun non-std-body (name formals body)

; The body below is a bit inefficient in the case that we get an error.
; However, we do not expect to get errors very often, and the alternative is to
; bind a variable that we have to check is not in formals.

  `(if (index-of-non-number (list ,@formals))
       (non-std-error ',name
                      (index-of-non-number ',formals)
                      ',formals
                      (list ,@formals))
     ,body))

#+:non-standard-analysis
(defun non-std-def-lst (def-lst)
  (if (and (consp def-lst) (null (cdr def-lst)))
      (let* ((def (car def-lst))
             (fn (car def))
             (formals (cadr def))
             (body (car (last def))))
        `((,@(butlast def 1)
             ,(non-std-body fn formals body))))
    (er hard 'non-std-def-lst
        "Unexpected call; please contact ACL2 implementors.")))

; Rockwell Addition:  To support non-executable fns we have to be able,
; at defun time, to introduce an undefined function.  So this stuff is
; moved up from other-events.lisp.

(defun make-udf-insigs (names wrld)
  (cond
   ((endp names) nil)
   (t (cons (list (car names)
                  (formals (car names) wrld)
                  (stobjs-in (car names) wrld)
                  (stobjs-out (car names) wrld))
            (make-udf-insigs (cdr names) wrld)))))

(defun intro-udf (insig wrld)
  (case-match
   insig
   ((fn formals stobjs-in stobjs-out)
    (putprop
     fn 'coarsenings nil
     (putprop
      fn 'congruences nil
      (putprop
       fn 'constrainedp t

; We could do a (putprop-unless fn 'guard *t* *t* &) here but it would
; be silly.

; We could do a (putprop-unless fn 'constraint-lst nil nil &) here but
; it would be silly.  The corresponding putprop isn't necessary
; either; we will put all the constraint-lst properties with
; putprop-constraints.

       (putprop
        fn 'symbol-class :common-lisp-compliant
        (putprop-unless
         fn 'stobjs-out stobjs-out nil
         (putprop-unless
          fn 'stobjs-in stobjs-in nil
          (putprop
           fn 'formals formals wrld))))))))
  (& (er hard 'store-signature "Unrecognized signature!" insig))))

(defun intro-udf-lst1 (insigs wrld)
  (cond ((null insigs) wrld)
        (t (intro-udf-lst1 (cdr insigs)
                           (intro-udf (car insigs) wrld)))))

(defun intro-udf-lst2 (insigs)

; Insigs is a list of internal form signatures, e.g., ((fn1 formals1
; stobjs-in1 stobjs-out1) ...), and we convert it to a "def-lst"
; suitable for giving the Common Lisp version of defuns, ((fn1
; formals1 body1) ...), where each bodyi is just a throw to
; 'raw-ev-fncall with the signal that says there is no body.  Note
; that the body we build (in this ACL2 code) is a Common Lisp body but
; not an ACL2 expression!

  (cond
   ((null insigs) nil)
   (t (cons `(,(caar insigs)
              ,(cadar insigs)
              (declare (ignore ,@(cadar insigs)))
              (throw-raw-ev-fncall '(ev-fncall-null-body-er ,(caar insigs))))
            (intro-udf-lst2 (cdr insigs))))))

(defun intro-udf-lst (insigs wrld)

; Insigs is a list of internal form signatures.  We know all the
; function symbols are new in wrld.  We declare each of them to have
; the given formals, stobjs-in, and stobjs-out, symbol-class
; :common-lisp-compliant, a guard of t and constrainedp of t.  We also
; arrange to execute a defun in the underlying Common Lisp so that
; each function is defined to throw to an error handler if called from
; ACL2.

  (if (null insigs)
      wrld
      (global-set 'cltl-command
                  `(defuns nil nil ,@(intro-udf-lst2 insigs))
                  (intro-udf-lst1 insigs wrld))))

(defun defuns-fn (def-lst state event-form #+:non-standard-analysis std-p)

; Important Note:  Don't change the formals of this function without
; reading the *initial-event-defmacros* discussion in axioms.lisp.

; On Guards

; When a function symbol fn is defund the user supplies a guard, g, and a
; body b.  Logically speaking, the axiom introduced for fn is

;    (fn x1...xn) = b.  

; After admitting fn, the guard-related properties are set as follows:

; prop                after defun

; body                   b*
; guard                  g
; unnormalized-body      b
; type-prescription      computed from b
; symbol-class           :ideal

; * We actually normalize the above.  During normalization we may expand some
; boot-strap non-rec fns.

; In addition, we magically set the symbol-function of fn

; symbol-function        b

; and the symbol-function of *1*fn as a program which computes the logical
; value of (fn x).  However, *1*fn is quite fancy because it uses the raw body
; in the symbol-function of fn if fn is :common-lisp-compliant, and may signal
; a guard error if 'guard-checking-on is set to other than nil or :none.  See
; oneify-cltl-code for the details.

; Observe that the symbol-function after defun may be a form that
; violates the guards on primitives.  Until the guards in fn are
; checked, we cannot let raw Common Lisp evaluate fn.

; Intuitively, we think of the Common Lisp programmer intending to defun (fn
; x1...xn) to be b, and is declaring that the raw fn can be called only on
; arguments satisfying g.  The need for guards stems from the fact that there
; are many Common Lisp primitives, such as car and cdr and + and *, whose
; behavior outside of their guarded domains is unspecified.  To use these
; functions in the body of fn one must "guard" fn so that it is never called in
; a way that would lead to the violation of the primitive guards.  Thus, we
; make a formal precondition on the use of the Common Lisp program fn that the
; guard g, along with the tests along the various paths through body b, imply
; each of the guards for every subroutine in b.  We also require that each of
; the guards in g be satisfied.  This is what we mean when we say fn is
; :common-lisp-compliant.

; It is, however, often impossible to check the guards at defun time.  For
; example, if fn calls itself recursively and then gives the result to +, we
; would have to prove that the guard on + is satisfied by fn's recursive
; result, before we admit fn.  In general, induction may be necessary to
; establish that the recursive calls satisfy the guards of their masters;
; hence, it is probably also necessary for the user to formulate general lemmas
; about fn to establish those conditions.  Furthermore, guard checking is no
; longer logically necessary and hence automatically doing it at defun time may
; be a waste of time.

  (with-ctx-summarized
   (if (output-in-infixp state)
       event-form
       (cond ((atom def-lst)
              (msg "( DEFUNS ~x0)"
                   def-lst))
             ((atom (car def-lst))
              (cons 'defuns (car def-lst)))
             ((null (cdr def-lst))
              #+:non-standard-analysis
              (if std-p
                  (cons 'defun-std (caar def-lst))
                (cons 'defun (caar def-lst)))
              #-:non-standard-analysis
              (cons 'defun (caar def-lst)))
             (t (msg *mutual-recursion-ctx-string*
                     (caar def-lst)))))
   (let ((wrld (w state)) 
         (def-lst0
           #+:non-standard-analysis
           (if std-p
               (non-std-def-lst def-lst)
             def-lst)
           #-:non-standard-analysis
           def-lst)
         (event-form (or event-form (list 'defuns def-lst))))
     (revert-world-on-error
      (er-let*
       ((tuple (chk-acceptable-defuns def-lst ctx wrld state
                                      #+:non-standard-analysis std-p)))

; chk-acceptable-defuns puts the 'formals, 'stobjs-in and 'stobjs-out
; properties (which are necessary for the translation of the bodies).
; All other properties are put by the defuns-fn0 call below.

       (cond
        ((eq tuple 'redundant)
         (stop-redundant-event state))
        (t
         (enforce-redundancy
          event-form ctx wrld
          (let ((names (nth 1 tuple))
                (arglists (nth 2 tuple))
                (docs (nth 3 tuple))
                (pairs (nth 4 tuple))
                (guards (nth 5 tuple))
                (measures (nth 6 tuple))
                (mp (nth 7 tuple))
                (rel (nth 8 tuple))
                (hints (nth 9 tuple))
                (guard-hints (nth 10 tuple))
                (std-hints (nth 11 tuple))
                (otf-flg (nth 12 tuple))
                (bodies (nth 13 tuple))
                (symbol-class (nth 14 tuple))
                (normalizeps (nth 15 tuple))
                (reclassifyingp (nth 16 tuple))
                (wrld (nth 17 tuple))
                (non-executablep (nth 18 tuple)))
            (er-let*
             ((pair (defuns-fn0
                      names
                      arglists
                      docs
                      pairs
                      guards
                      measures
                      mp
                      rel
                      hints
                      guard-hints
                      std-hints
                      otf-flg
                      bodies
                      symbol-class
                      normalizeps
                      non-executablep
                      #+:non-standard-analysis std-p
                      ctx
                      wrld
                      state)))

; Pair is of the form (wrld . ttree).

             (er-progn
              (chk-assumption-free-ttree (cdr pair) ctx state)
              (install-event (cond ((null (cdr names)) (car names))
                                   (t names))
                             event-form
                             (cond ((null (cdr names)) 'defun)
                                   (t 'defuns))
                             (cond ((null (cdr names)) (car names))
                                   (t names))
                             (cdr pair)
                             (cond
                              (non-executablep
                               `(defuns nil nil
                                  ,@(intro-udf-lst2
                                     (make-udf-insigs names wrld))))
                              (t `(defuns ,(if (eq symbol-class :program)
                                               :program
                                             :logic)
                                    ,(if reclassifyingp
                                         'reclassifying
                                       (if (defstobj-functionsp names
                                             (global-val 'embedded-event-lst
                                                         (car pair)))
                                           (cons 'defstobj

; The following expression computes the stobj name, e.g., $S, for
; which this defun is supportive.  The STOBJS-IN of this function is
; built into the expression created by oneify-cltl-code
; namely, in the throw-raw-ev-fncall expression (see
; oneify-fail-form).  We cannot compute the STOBJS-IN of the function
; accurately from the world because $S is not yet known to be a stobj!
; This problem is a version of the super-defun-wart problem.


                                                 (defstobj-functionsp names
                                                   (global-val
                                                    'embedded-event-lst 
                                                    (car pair))))
                                         nil))
                                    ,@def-lst0)))
                             t
                             ctx
                             (car pair)
                             state))))))))))))

(defun defun-fn (def state event-form #+:non-standard-analysis std-p)

; Important Note:  Don't change the formals of this function without
; reading the *initial-event-defmacros* discussion in axioms.lisp.

; The only reason this function exists is so that the defmacro for
; defun is in the form expected by primordial-event-defmacros.

  (defuns-fn (list def) state
    (or event-form (cons 'defun def))
    #+:non-standard-analysis std-p))

; Here we develop the :args keyword command that will print all that
; we know about a function.

(defun args-fn (name state)
  (io? temporary nil (mv erp val state)
       (name)
       (let ((wrld (w state))
             (channel (standard-co state)))
         (cond
          ((and (symbolp name)
                (function-symbolp name wrld))
           (let* ((formals (formals name wrld))
                  (stobjs-in (stobjs-in name wrld))
                  (stobjs-out (stobjs-out name wrld))
                  (docp (access-doc-string-data-base name state))
                  (guard (untranslate (guard name nil wrld) t wrld))
                  (tp (find-runed-type-prescription
                       (list :type-prescription name)
                       (getprop name 'type-prescriptions nil
                                'current-acl2-world wrld)))
                  (tpthm (cond (tp (untranslate
                                    (access type-prescription tp :corollary)
                                    t wrld))
                               (t nil)))
                  (constraint (mv-let (some-name constraint-lst)
                                      (constraint-info name wrld)
                                      (if some-name
                                          (untranslate (conjoin constraint-lst)
                                                       t wrld)
                                        t))))
             (pprogn
              (fms "Function         ~x0~|~
               Formals:         ~y1~|~
               Signature:       ~y2~|~
               ~                 => ~y3~|~
               Guard:           ~q4~|~
               Guards Verified: ~y5~|~
               Defun-Mode:      ~@6~|~
               Type:            ~#7~[built-in (or unrestricted)~/~q8~]~|~
               ~#9~[~/Constraint:  ~qa~|~]~
               ~#d~[~/Documentation available via :DOC~]~%"
                   (list (cons #\0 name)
                         (cons #\1 formals)
                         (cons #\2 (cons name
                                         (prettyify-stobj-flags stobjs-in)))
                         (cons #\3 (prettyify-stobjs-out stobjs-out))
                         (cons #\4 guard)
                         (cons #\5 (eq (symbol-class name wrld)
                                       :common-lisp-compliant))
                         (cons #\6 (defun-mode-string (fdefun-mode name wrld)))
                         (cons #\7 (if tpthm 1 0))
                         (cons #\8 tpthm)
                         (cons #\9 (if (eq constraint t) 0 1))
                         (cons #\a constraint)
                         (cons #\d (if docp 1 0)))
                   channel state nil)
              (value name))))
          ((and (symbolp name)
                (getprop name 'macro-body nil 'current-acl2-world wrld))
           (let ((args (macro-args name wrld))
                 (docp (access-doc-string-data-base name state))
                 (guard (untranslate (guard name nil wrld) t wrld)))
             (pprogn
              (fms "Macro ~x0~|~
               Macro Args:  ~y1~|~
               Guard:       ~q2~|~
               ~#3~[~/Documentation available via :DOC~]~%"
                   (list (cons #\0 name)
                         (cons #\1 args)
                         (cons #\2 guard)
                         (cons #\3 (if docp 1 0)))
                   channel state nil)
              (value name))))
          ((member-eq name '(let lambda declare quote))
           (pprogn (fms "Special form, basic to the Common Lisp language.  ~
                    See for example CLtL."
                        nil channel state nil)
                   (value name)))
          (t (er soft :args
                 "~x0 is neither a function symbol nor a macro name."
                 name))))))

(defmacro args (name)

  ":Doc-Section Documentation

  ~c[args], ~ilc[guard], ~c[type], ~ilc[constraint], etc., of a function symbol~/
  ~bv[]
  Example:
  :args assoc-eq
  ~ev[]~/

  ~c[Args] takes one argument, a symbol which must be the name of a
  function or macro, and prints out the formal parameters, the ~il[guard]
  expression, the output ~il[signature], the deduced type, the ~il[constraint]
  (if any), and whether ~il[documentation] about the symbol is available
  via ~c[:]~ilc[doc].~/"  

  (list 'args-fn name 'state))

; We now develop the code for verify-termination, a macro that is essentially
; a form of defun.

(defun make-verify-termination-def (old-def new-dcls state)

; Old-def is a def tuple that has previously been accepted by defuns.  For
; example, if is of the form (fn args ...dcls... body), where dcls is a list of
; at most one doc string and possibly many DECLARE forms.  New-dcls is a new
; list of dcls (known to satisfy plausible-dclsp).  We create a new def tuple
; that uses new-dcls instead of ...dcls... but which keeps any member of the
; old dcls not specified by the new-dcls except for the :mode (if any), which
; is replaced by :mode :logic.

  (let* ((fn (car old-def))
         (args (cadr old-def))
         (body (car (last (cddr old-def))))
         (dcls (butlast (cddr old-def) 1))
         (new-fields (dcl-fields new-dcls))
         (modified-old-dcls (strip-dcls
                             (add-to-set-eq :mode new-fields)
                             dcls)))
    `(,fn ,args
          ,@new-dcls
          ,@(if (and (not (member-eq :mode new-fields))
                     (eq (default-defun-mode (w state)) :program))
                '((declare (xargs :mode :logic)))
                nil)
          ,@modified-old-dcls
          ,body)))

(defun make-verify-termination-defs-lst (defs-lst lst state)

; Defs-lst is a list of def tuples as previously accepted by defuns.  Lst is
; a list of tuples supplied to verify-termination.  Each element of a list is
; of the form (fn . dcls) where dcls satisfies plausible-dclsp, i.e., is a list
; of doc strings and/or DECLARE forms.  We copy defs-lst, modifying each member
; by merging in the dcls specified for the fn in lst.  If some fn in defs-lst
; is not mentioned in lst, we don't modify its def tuple except to declare it
; of :mode :logic.

  (cond
   ((null defs-lst) nil)
   (t (let ((temp (assoc-eq (caar defs-lst) lst)))
        (cons (make-verify-termination-def (car defs-lst) (cdr temp) state)
              (make-verify-termination-defs-lst (cdr defs-lst) lst state))))))

(defun chk-acceptable-verify-termination1 (lst clique fn1 ctx wrld state)

; Lst is the input to verify-termination.  Clique is a list of function
; symbols, fn1 is a member of clique (and used for error reporting only).  Lst
; is putatively of the form ((fn . dcls) ...)  where each fn is a member of
; clique and each dcls is a plausible-dclsp, as above.  That means that each
; dcls is a list containing documentation strings and DECLARE forms mentioning
; only TYPE, IGNORE, and XARGS.  We do not check that the dcls are actually
; legal because what we will ultimately do with them in verify-termination-fn
; is just create a modified definition to submit to defuns.  Thus, defuns will
; ultimately approve the dcls.  By construction, the dcls submitted to
; verify-termination will find their way, whole, into the submitted defuns.  We
; return nil or cause an error according to whether lst satisfies the
; restrictions noted above.

  (cond ((null lst) (value nil))
        ((not (and (consp (car lst))
                   (symbolp (caar lst))
                   (function-symbolp (caar lst) wrld)
                   (plausible-dclsp (cdar lst))))
         (er soft ctx
             "Each argument to verify-termination must be of the form (name ~
              dcl ... dcl), where each dcl is either a DECLARE form or a ~
              documentation string.  The DECLARE forms may contain TYPE, ~
              IGNORE, and XARGS entries, where the legal XARGS keys are ~
              :GUARD, :MEASURE, :WELL-FOUNDED-RELATION, HINTS, :GUARD-HINTS, ~
              :MODE, :VERIFY-GUARDS and :OTF-FLG.  The argument ~x0 is ~
              illegal.  See :DOC verify-termination."
             (car lst)))
        ((not (member-eq (caar lst) clique))
         (er soft ctx
             "The functions symbols whose termination is to be verified must ~
              all be members of the same clique of mutually recursive ~
              functions.  ~x0 is not in the clique of ~x1.  The clique of ~x1 ~
              consists of ~&2.  See :DOC verify-termination."
             (caar lst) fn1 clique))
        (t (chk-acceptable-verify-termination1 (cdr lst) clique fn1 ctx wrld state))))

(defun uniform-defun-modes (defun-mode clique wrld)

; Defun-Mode should be a defun-mode.  Clique is a list of fns.  If defun-mode is
; :program then we return :program if every element of clique is
; :program; else nil.  If defun-mode is :logic we return :logic if
; every element of clique is :logic; else nil.

  (cond ((null clique) defun-mode)
        ((programp (car clique) wrld)
         (and (eq defun-mode :program)
              (uniform-defun-modes defun-mode (cdr clique) wrld)))
        (t (and (eq defun-mode :logic)
                (uniform-defun-modes defun-mode (cdr clique) wrld)))))

(defun recover-defs-lst (fn wrld state)

; Fn is a :program function symbol in wrld.  Thus, it was introduced by defun.
; (Constrained and :non-executable functions are :logic.)  We return the
; defs-lst that introduced it fn.  We recover this from the cltl-command for
; fn.

  (let ((val
         (scan-to-cltl-command
          (cdr (lookup-world-index 'event
                                   (getprop fn 'absolute-event-number
                                            '(:error "See ~
                                                      RECOVER-DEFS-LST.")
                                            'current-acl2-world wrld)
                                   wrld)))))
    (cond ((and (consp val)
                (eq (car val) 'defuns))

; Val is of the form (defuns defun-mode-flg ignorep def1 ... defn).  If
; defun-mode-flg is non-nil then the parent event was (defuns def1 ... defn)
; and the defun-mode was defun-mode-flg.  If defun-mode-flg is nil, the parent
; was an encapsulate or :non-executable.  In the former case we return (def1
; ... defn); in the latter we return nil.

           (cond ((cadr val) (value (cdddr val)))
                 (t (value nil))))
          (t (value
              (er hard 'recover-defs-lst
                  "We failed to find the expected CLTL-COMMAND for the ~
                   introduction of ~x0."
                  fn))))))

(defun get-clique (fn wrld state)

; Fn must be a function symbol.  We return the list of mutually recursive fns
; in the clique containing fn, according to their original definitions.  If fn
; is :program we have to look for the cltl-command and recover the clique from
; the defs-lst.  Otherwise, we can use the 'recursivep property.

  (cond ((programp fn wrld)
         (er-let* ((defs (recover-defs-lst fn wrld state)))
                  (value (strip-cars defs))))
        (t (let ((recp (getprop fn 'recursivep nil
                                'current-acl2-world wrld)))
             (value (cond ((null recp) (list fn))
                          (t recp)))))))

(defun chk-acceptable-verify-termination (lst ctx wrld state)

; We check that lst is acceptable input for verify-termination.  To be
; acceptable, lst must be of the form ((fn . dcls) ...) where each fn is the
; name of a function, all of which are in the same clique and have the same
; defun-mode and each dcls is a plausible-dclsp as above.  We cause an error or
; return nil

  (cond
   ((and (consp lst)
         (consp (car lst))
         (symbolp (caar lst)))
    (cond
     ((function-symbolp (caar lst) wrld)
      (er-let* ((clique (get-clique (caar lst) wrld state)))
        (cond
         ((not (uniform-defun-modes (fdefun-mode (caar lst) wrld)
                                clique
                                wrld))
          (er soft ctx
              "The function ~x0 is ~#1~[:program~/:logic~] but some ~
               member of its clique of mutually recursive peers is not.  The ~
               clique consists of ~&2.  Since verify-termination must deal ~
               with the entire clique, in the sense that it will either admit ~
               into the logic all of the functions in the clique (in which ~
               case the clique must be uniformly :program) or none of the ~
               functions in the clique (in which case the clique must be ~
               uniformly :logic, making verify-termination redundant).  ~
               The clique containing ~x0 is neither.  This can only happen ~
               because some members of the clique have been redefined."
              (caar lst)
              (if (programp (caar lst) wrld) 0 1)
              clique))
         (t (chk-acceptable-verify-termination1 lst clique (caar lst) ctx wrld state)))))
     (t (er soft ctx
            "The symbol ~x0 is not a function symbol in the current ACL2 world."
            (caar lst)))))
   ((atom lst)
    (er soft ctx
        "Verify-termination requires at least one argument."))
   (t (er soft ctx
          "The first argument supplied to verify-termination, ~x0, is not of ~
           the form (fn dcl ...)."
          (car lst)))))

(defun verify-termination-fn (lst state event-form #+:non-standard-analysis std-p)
  (when-logic

; It is convenient to use when-logic so that we skip verify-termination during
; pass1 of the boot-strap in axioms.lisp.

   "VERIFY-TERMINATION"
   (let* ((lst (cond ((and (consp lst)
                           (symbolp (car lst)))
                      (list lst))
                     (t lst)))
          (ctx
           (cond ((null lst) "(VERIFY-TERMINATION)")
                 ((and (consp lst)
                       (consp (car lst)))
                  (cond
                   ((null (cdr lst))
                    (cond
                     ((symbolp (caar lst))
                      (cond
                       ((null (cdr (car lst)))
                        (msg "( VERIFY-TERMINATION ~x0)" (caar lst)))
                       (t (msg "( VERIFY-TERMINATION ~x0 ...)" (caar lst)))))
                     ((null (cdr (car lst)))
                      (msg "( VERIFY-TERMINATION (~x0))" (caar lst)))
                     (t (msg "( VERIFY-TERMINATION (~x0 ...))" (caar lst)))))
                   ((null (cdr (car lst)))
                    (msg "( VERIFY-TERMINATION (~x0) ...)" (caar lst)))
                   (t (msg "( VERIFY-TERMINATION (~x0 ...) ...)" (caar lst)))))
                 (t (cons 'VERIFY-TERMINATION lst))))
          (wrld (w state))
          (event-form (or event-form
                          (cons 'VERIFY-TERMINATION lst))))
     (er-progn
      (chk-acceptable-verify-termination lst ctx wrld state)
      (er-let*
       ((defs-lst (recover-defs-lst (caar lst) wrld state)))
       (defuns-fn 
         (make-verify-termination-defs-lst defs-lst lst state)
         state
         event-form
         #+:non-standard-analysis std-p))))))

; When we defined instantiablep we included the comment that a certain
; invariant holds between it and the axioms.  The functions here are
; not used in the system but can be used to check that invariant.
; They were not defined earlier because they use event tuples.

(defun fns-used-in-axioms (lst wrld ans)

; Intended for use only by check-out-instantiablep.

  (cond ((null lst) ans)
        ((and (eq (caar lst) 'event-landmark)
              (eq (cadar lst) 'global-value)
              (eq (access-event-tuple-type (cddar lst)) 'defaxiom))

; In this case, (car lst) is a tuple of the form 

; (event-landmark global-value . tuple)

; where tuple is a defaxiom of some name, namex, and we are interested
; in all the function symbols occurring in the formula named namex.

         (fns-used-in-axioms (cdr lst)
                             wrld
                             (all-ffn-symbs (formula
                                             (access-event-tuple-namex
                                              (cddar lst))
                                             nil
                                             wrld)
                                            ans)))
        (t (fns-used-in-axioms (cdr lst) wrld ans))))

(defun check-out-instantiablep1 (fns wrld)

; Intended for use only by check-out-instantiablep.

  (cond ((null fns) nil)
        ((instantiablep (car fns) wrld)
         (cons (car fns) (check-out-instantiablep1 (cdr fns) wrld)))
        (t (check-out-instantiablep1 (cdr fns) wrld))))

(defun check-out-instantiablep (wrld)

; See the comment in instantiablep.

  (let ((bad (check-out-instantiablep1 (fns-used-in-axioms wrld wrld nil)
                                       wrld)))
    (cond
     ((null bad) "Everything checks")
     (t (er hard 'check-out-instantiablep
         "The following functions are instantiable and shouldn't be:~%~x0"
         bad)))))
