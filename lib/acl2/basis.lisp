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

; When we are ready to verify termination in this and later files, we should
; consider changing null to endp in a number of functions.

(in-package "ACL2")

(defun enforce-redundancy-er-args (event-form-var wrld-var)
  (list "Enforce-redundancy is active; see :DOC set-enforce-redundancy and ~
         see :DOC redundant-events.  However, the following event ~@0:~|~%~x1"
        `(if (and (symbolp (cadr ,event-form-var))
                  (decode-logical-name (cadr ,event-form-var) ,wrld-var))
             "conflicts with an existing event of the same name"
           "is not redundant")
        event-form-var))

(defmacro enforce-redundancy (event-form ctx wrld form)
  (let ((var 'redun-check-var))
    `(let ((,var (and (not (eq (ld-skip-proofsp state)
                               'include-book))
                      (cdr (assoc-eq :enforce-redundancy
                                     (table-alist 'acl2-defaults-table
                                                  ,wrld))))))
       (cond ((eq ,var t)
              (check-vars-not-free
               (,var)
               (er soft ,ctx
                   ,@(enforce-redundancy-er-args
                      event-form wrld))))
             (t (pprogn (cond (,var (check-vars-not-free
                                     (,var)
                                     (warning$ ,ctx "Enforce-redundancy"
                                               ,@(enforce-redundancy-er-args
                                                  event-form wrld))))
                              (t state))
                        (check-vars-not-free
                         (,var)
                         ,form)))))))

; System calls

#-acl2-loop-only
(defvar *last-sys-call-status* 0)

(defun sys-call (command-string args)

  ":Doc-Section ACL2::Programming

  make a system call to the host operating system~/
  ~bv[]
  Example Forms:
  (sys-call \"cp\" '(\"foo.lisp\" \"foo-copied.lisp\"))
  (prog2$ (sys-call \"cp\" '(\"foo.lisp\" \"foo-copied.lisp\"))
          (sys-call-status state))
  ~ev[]
  The first argument of ~c[sys-call] is a command for the host operating
  system, and the second argument is a list of strings that are the
  arguments for that command.  In GCL and perhaps other lisps, you can put the
  arguments with the command; but this is not the case, for example, in Allegro
  CL running on Linux.

  The use of ~ilc[prog2$] above is optional, but illustrates a typical sort
  of use when one wishes to get the return status.  ~l[sys-call-status].~/
  ~bv[]
  General Form:
  (sys-call cmd args)
  ~ev[]
  This function logically returns ~c[nil].  However, it makes the
  indicated call to the host operating system, as described above,
  using a function supplied ``under the hood'' by the underlying Lisp
  system.  On occasions where one wishes to obtain the numeric status
  returned by the host operating system (or more precisely, by the
  Lisp function under the hood that passes the system call to the host
  operating system), one may do so;  ~pl[sys-call-status].  The
  status value is the value returned by that Lisp function, which may
  well be the same numeric value returned by the host operating system
  for the underlying system call.

  Note that ~c[sys-call] does not touch the ACL2 ~ilc[state]; however,
  ~ilc[sys-call-status] updates the ~c[file-clock] field of the ~c[state].  One may
  view that update as modifying the ~c[file~clock] to be at least as
  recent as the time of the most recent ~c[sys-call].

  Be careful if you use ~c[sys-call]!  It can be used for example to overwrite
  files, or worse!  The following example from Bob Boyer shows how to use
  ~c[sys-call] to execute, in effect, arbitrary Lisp forms.  ACL2 provides a
  ``trust tag'' mechanism that requires execution of a ~ilc[defttag] form
  before you can use ~c[sys-call]; ~pl[defttag].  (Note: The setting of the raw
  Lisp variable ~c[*features*] below is just to illustrate that any such
  mischief is possible.  Normally ~c[*features*] is a list with more than a few
  elements.)
  ~bv[]
  % cat foo
  print *0x85d2064=0x838E920
  detach
  q
  % acl2
  ... boilerplate deleted
  ACL2 !>(sys-call \"gdb -p $PPID -w < foo >& /dev/null \" nil)
  NIL
  ACL2 !>:q

  Exiting the ACL2 read-eval-print loop.  To re-enter, execute (LP).
  ACL2>*features*

  (:AKCL-SET-MV)

  ACL2>
  ~ev[]

  Finally, we make a comment about output redirection, which also applies to
  other related features that one may expect of a shell.  ~c[Sys-call] does not
  directly support output redirection.  If you want to run a program, ~c[P],
  and redirect its output, we suggest that you create a wrapper script, ~c[W]
  to call instead.  Thus ~c[W] might be a shell script containing the line:
  ~bv[]
  P $* >& foo.out
  ~ev[]
  If this sort of solution proves inadequate, please contact the ACL2
  implementors and perhaps we can come up with a solution."

  #+acl2-loop-only
  (declare (ignore command-string args))
  #-acl2-loop-only 
  (let ((rslt (system-call command-string args)))
    (progn (setq *last-sys-call-status* rslt)
           nil))
  #+acl2-loop-only
  nil
  )

(encapsulate
 (((sys-call-status-sequence *) => *))
 (local (defun sys-call-status-sequence (n)
          (declare (ignore n)) 0)))

(defun sys-call-status (state-state)

; This function needs to stay in :program mode!  It isn't even a function!
; Well, except there seems to be no way for anything to go terribly wrong even
; if we verify-termination and verify-guards.

  ":Doc-Section ACL2::Programming

  exit status from the preceding system call~/

  This function returns two values, ~c[(mv status state)].  The first is
  the status returned by the most recent invocation of function
  ~c[sys-call]; ~pl[sys-call].  The second is the ACL2 ~ilc[state] object,
  which is also the input to this function.~/

  The function ~ilc[sys-call] makes a system call to the host operating
  system using a function supplied ``under the hood'' by the
  underlying Lisp system.  The status value is the value returned by
  that Lisp function, which may well be the same numeric value
  returned by the host operating system for the underlying system
  call.  For more information, ~pl[sys-call].~/"

; There is a signature problem here:
; (declare (xargs :guard (state-p state-state)))

  #-(or acl2-loop-only no-hack)
  (when (live-state-p state-state)
    (return-from sys-call-status
                 (progn (setq *file-clock* (1+ *file-clock*))
                        (mv *last-sys-call-status* state-state))))
  (let* ((new-clock (1+ (file-clock state-state)))
         (status
          (sys-call-status-sequence new-clock))
         (state-state (update-file-clock new-clock state-state)))
    (mv status state-state)))

; End of system calls

; For some reason, MCL didn't like it when there was a single definition of
; gc$-fn with acl2-loop-only directives in the body.  So we define the two
; versions separately.

#-acl2-loop-only
(defun-one-output gc$-fn (args)

; We will add some checks on the arguments as a courtesy, but really, it is up
; to the user to pass in the right arguments.

  #+allegro (apply `excl:gc args)
  #+gcl
  (if (eql (length args) 1)
      (apply 'si::gbc args)
    (er hard 'gc$
        "In GCL, gc$ requires exactly one argument, typically T."))
  #+clisp (apply 'ext:gc args)
  #+cmu (apply 'system::gc args)
  #+sbcl (apply 'sb-ext:gc args)
  #+(or mcl openmcl) (apply 'ccl::gc args)
  #+lispworks (apply 'cl-user::mark-and-sweep (or args (list 3)))
  #-(or allegro gcl clisp cmu sbcl mcl openmcl lispworks)
  (illegal 'gc$ "GC$ is not supported in this Common Lisp." nil)
  nil)

#+acl2-loop-only
(defun gc$-fn (args)
  (declare (ignore args))
  nil)

(defmacro gc$ (&rest args)
  ":Doc-Section Miscellaneous

  invoke the garbage collector~/

  This function is provided so that the user can call the garbage collector of
  the underlying Lisp from inside the ACL2 loop.  Specifically, a call of
  ~c[gc$] is translated into a call of a function below on the same arguments.
  ~bv[]
  Allegro CL:            excl:gc
  GCL                    si::gbc
  CLISP                  ext:gc
  CMU Common Lisp        system::gc
  SBCL                   sb-ext:gc
  Macintosh Common Lisp  ccl::gc
  ~ev[]
  The arguments, if any, are as documented in the underlying Common Lisp.  It
  is up to the user to pass in the right arguments, although we may do some
  rudimentary checks.

  This function always returns ~c[nil].~/~/"

  `(gc$-fn ',args))

(defdoc gcl
  ":Doc-Section miscellaneous

  tips on building and using ACL2 based on Gnu Common Lisp~/

  See the installation instructions for basic information about building ACL2
  on top of GCL, including information about where to fetch GCL.  Here, we
  provide some tips that may be useful.~/

  1. You can place forms to evaluate at start-up into file ~c[init.lsp] in the
  directory where you are starting ACL2 (GCL), or into file ~c[acl2-init.lsp]
  in your home directory.  For example, in order to evaluate both of the lisp
  forms mentioned in 2 below, you could put them both into ~c[init.lsp] in the
  current directory or in ~c[~~/acl2-init.lsp] (either way, without ~c[(lp)] or
  ~c[:q]):
  ~bv[]
  (setq si::*optimize-maximum-pages* nil)
  (si::allocate 'cons 75000 t)
  ~ev[]

  2. Suppose you run out of space, for example with an error like this:
  ~bv[]
     Error: The storage for CONS is exhausted.
       Currently, 59470 pages are allocated.
       Use ALLOCATE to expand the space.
  ~ev[]
  The following suggestion from Camm Maguire will minimize the heap size, at
  the cost of more garbage collection time.
  ~bv[]
  :q   ; exit the ACL2 loop
  (setq si::*optimize-maximum-pages* nil)
  (lp) ; re-enter the ACL2 loop
  ~ev[]
  A second thing to try, suggested by several people, is to preallocate more
  pages before the run, e.g.:
  ~bv[]
  :q   ; exit the ACL2 loop
  (si::allocate 'cons 75000 t)
  (lp) ; re-enter the ACL2 loop
  ~ev[]
  Also ~pl[reset-kill-ring] for a suggestion on how to free up space.

  3. Windows users have seen this error:
  ~bv[]
  cc1.exe: unrecognized option `-fno-zero-initialized-in-bss'
  ~ev[]
  Camm Maguire suggests that a solution may be to evaluate the following in GCL
  before building ACL2.
  ~bv[]
  (in-package 'compiler)
  (let* ((x `-fno-zero-initialized-in-bss')
         (i (search x *cc*))) 
          (setq *cc* (concatenate 'string 
                                  (subseq *cc* 0 i)
                                  (subseq *cc* (+ i (length x))))))
  ~ev[]

  4. It is possible to profile using ACL2 built on GCL.  See file
  ~c[save-gprof.lsp] in the ACL2 source directory.

  5. Some versions of GCL may have garbage-collector bugs that, on rare
  occasions, cause ACL2 (when built on GCL) to break.  If you run into this,
  a solution may be to execute the following:
  ~bv[]
  :q
  (si::sgc-on nil)
  (lp)
  ~ev[]
  Alternatively, put ~c[(si::sgc-on nil)] in your ~c[~~/acl2-init.lsp] file.

  A full regression test and found that this decreased performance by about
  10%.  But even with that, GCL may be the fastest Common Lisp for ACL2 on
  Linux (performance figures may often be found by following the ``Recent
  changes'' link on the ACL2 home page).

  ~/")

(defmacro wormhole (pseudo-flg name input form
                        &key
                        (current-package 'same current-packagep)
                        (ld-skip-proofsp 'same ld-skip-proofspp)
                        (ld-redefinition-action 'save ld-redefinition-actionp)
                        (ld-prompt ''wormhole-prompt)
                        (ld-keyword-aliases 'same ld-keyword-aliasesp)
                        (ld-pre-eval-filter 'same ld-pre-eval-filterp)
                        (ld-pre-eval-print 'same ld-pre-eval-printp)
                        (ld-post-eval-print 'same ld-post-eval-printp)
                        (ld-evisc-tuple 'same ld-evisc-tuplep)
                        (ld-error-triples 'same ld-error-triplesp)
                        (ld-error-action 'same ld-error-actionp)
                        (ld-query-control-alist 'same ld-query-control-alistp)
                        (ld-verbose 'same ld-verbosep))

  ":Doc-Section Miscellaneous

  ~ilc[ld] without ~ilc[state] ~-[] a short-cut to a parallel universe~/
  ~bv[]
  Example Form:
  (wormhole t 'interactive-break nil '(value 'hi!))
                               ; Enters a recursive read-eval-print loop
                               ; on a copy of the ``current ~ilc[state]'' and
                               ; returns nil!~/

  General Form:
  (wormhole pseudo-flg name input form
    :current-package    ...  ; known package name
    :ld-skip-proofsp    ...  ; t, nil or 'include-book
    :ld-redefinition-action  ; nil or '(:a . :b)
    :ld-prompt          ...  ; nil, t, or some prompt printer fn
    :ld-keyword-aliases ...  ; an alist pairing keywords to parse info
    :ld-pre-eval-filter ...  ; :all, :query, or some new name
    :ld-pre-eval-print  ...  ; nil, t, or :never
    :ld-post-eval-print ...  ; nil, t, or :command-conventions
    :ld-evisc-tuple     ...  ; nil or '(alist level length hiding-cars)
    :ld-error-triples   ...  ; nil or t
    :ld-error-action    ...  ; :continue, :return, or :error
    :ld-query-control-alist  ; alist supplying default responses
    :ld-verbose         ...) ; nil or t
  ~ev[]
  The keyword arguments above are exactly those of ~ilc[ld] (~pl[ld])
  except that three of ~ilc[ld]'s keyword arguments are missing: the three
  that specify the channels ~ilc[standard-oi], ~ilc[standard-co] and ~ilc[proofs-co].
  Essentially ~c[wormhole] is just a call of ~ilc[ld] on the current ~ilc[state] with
  the given keyword arguments.  ~c[Wormhole] always returns ~c[nil].  The
  ~st[amazing] thing about ~c[wormhole] is that it calls ~ilc[ld] and interacts with
  the user even though ~ilc[state] is not available as an argument!

  ~c[Wormhole] does this by manufacturing a ``wormhole ~il[state],'' a copy of
  the ``current ~il[state]'' (whatever that is) modified so as to contain
  some of the wormhole arguments.  ~ilc[Ld] is called on that wormhole ~il[state]
  with the three ~ilc[ld] channels directed to ACL2's ``comment window.'' At
  the moment, the comment window is overlaid on the terminal and you
  cannot tell when output is going to ~ilc[*standard-co*] and when it is
  going to the comment window.  But we imagine that eventually a
  different window will pop up on your screen.  In any case, the
  interaction provided by this call of ~ilc[ld] does not modify the ~ilc[state]
  ``from which'' wormhole was called, it modifies the copied ~ilc[state].
  When ~ilc[ld] exits (e.g., in response to ~c[:]~ilc[q] being typed in the comment
  window) the wormhole ~il[state] evaporates and ~c[wormhole] returns ~c[nil].
  Logically and actually (from the perspective of the ongoing
  computation) nothing has happened except that a ``no-op'' function was
  called and returned ~c[nil].

  The name ~c[wormhole] is meant to suggest the idea that the function
  provides easy access to ~ilc[state] in situations where it is apparently
  impossible to get ~ilc[state].  Thus, for example, if you define the
  ~c[factorial] function, say, except that you sprinkled into its body
  appropriate calls of ~c[wormhole], then the execution of ~c[(factorial 6)]
  would cause interactive breaks in the comment window.  During those
  breaks you would apparently be able to inspect the ``current ~il[state]''
  even though ~c[factorial] does not take ~ilc[state] as an argument.  The whole
  notion of there being a ``current ~il[state]'' during the evaluation of
  ~c[(factorial 6)] is logically ill-defined.  And yet, we know from
  practical experience with the sequential computing machines upon
  which ACL2 is implemented that there is a ``current ~il[state]'' (to
  which the ~c[factorial] function is entirely insensitive) and that is
  the ~il[state] to which ~c[wormhole] ``tunnels.'' A call of ~c[wormhole] from
  within ~c[factorial] can pass ~c[factorial]-specific information that is
  embedded in the wormhole ~il[state] and made available for inspection by
  the user in an interactive setting.  But no information ever flows
  out of a wormhole ~il[state]: ~c[wormhole] always returns ~c[nil].

  There are some restrictions about what can be done inside a wormhole.  As you
  may imagine, we really do not ``copy the current state'' but rather just keep
  track of how we modified it and undo those modifications upon exit.  An error is
  signalled if you try to modify ~c[state] in an unsupported way.  For this same
  reason, wormholes do not allow updating of any user-defined single-threaded
  objects.  ~l[stobj].

  There are four arguments to ~c[wormhole] that need further explanation:
  ~c[pseudo-flg], ~c[name], ~c[input], and ~c[form].  Roughly speaking, the value of
  ~c[pseudo-flg] should be ~c[t] or ~c[nil] and indicates whether we are actually
  to enter a wormhole or just return ~c[nil] immediately.  The actual
  handling of ~c[pseudo-flg] is more sophisticated and is explained in
  detail at the end of this ~il[documentation].

  ~c[Name] and ~c[input] are used as follows.  Recall that ~c[wormhole] copies the
  ``current ~il[state]'' and then modifies it slightly to obtain the ~ilc[state]
  upon which ~ilc[ld] is called.  We now describe the modifications.  First,
  the ~ilc[state] global variable ~c['wormhole-name] is set to ~c[name], which may
  be any non-~c[nil] ACL2 object but is usually a symbol.  Then,
  ~c['wormhole-input] is set to ~c[input], which may be any ACL2 object.
  Finally, and inexplicably, ~c['wormhole-output] is set to the value of
  ~c['wormhole-output] the last time a wormhole named ~c[name] was exited (or
  ~c[nil] if this is the first time a wormhole named ~c[name] was entered).
  This last aspect of wormholes, namely the preservation of
  ~c['wormhole-output], allows all the wormholes of a given name to
  communicate with each other.

  We can now explain how ~c[form] is used.  The modified ~ilc[state] described
  above is the ~ilc[state] on which ~ilc[ld] is called.  However, ~ilc[standard-oi] ~-[]
  the input channel from which ~ilc[ld] reads ~il[command]s ~-[] is set so that the
  first ~il[command] that ~ilc[ld] reads and evaluates is ~c[form].  If ~c[form] returns
  an error triple with value ~c[:]~ilc[q], i.e., ~c[form] returns via ~c[(value :q)],
  then no further ~il[command]s are read, ~ilc[ld] exits, and the wormhole exits
  and returns ~c[nil].  But if ~c[form] returns any other value (or is not an
  error triple), then subsequent ~il[command]s are read from the comment
  window.

  As usual, the ~ilc[ld]-specials affect whether a herald is printed upon
  entry, whether ~c[form] is printed before evaluation, whether a ~il[prompt]
  is printed, how errors are handled, etc.  The ~ilc[ld]-specials can be
  specified with the corresponding arguments to ~c[wormhole].  It is
  standard practice to call ~c[wormhole] so that the entry to ~ilc[ld] and the
  evaluation of ~c[form] are totally silent.  Then, tests in ~c[form] can
  inspect the ~ilc[state] and decide whether user interaction is desired.
  If so, ~c[form] can appropriately set ~ilc[ld-prompt], ~ilc[ld-error-action], etc.,
  print a herald, and then return ~c[(value :invisible)].  Recall
  (~pl[ld]) that ~c[(value :invisible)] causes ~ilc[ld] not to print a value
  for the just executed form.  The result of this arrangement is that
  whether interaction occurs can be based on tests that are performed
  on the wormhole ~il[state] after ~c[(@ wormhole-input)] and the last
  ~c[(@ wormhole-output)] are available for inspection.  This is
  important because outside the wormhole you can access
  ~c[wormhole-input] (you are passing it into the wormhole) but you may
  not be able to access the current ~ilc[state] (because you might be in
  ~c[factorial]) and you definitely cannot access the ~c[wormhole-output] of
  the last wormhole because it is not part of the ACL2 ~ilc[state].  Thus,
  if the condition under which you wish to interact depends upon the
  ~ilc[state] or that part of it preserved from the last wormhole
  interaction, that condition can only be tested from within the
  wormhole, via ~c[form].

  It is via this mechanism that ~ilc[break-rewrite] (~pl[break-rewrite])
  is implemented.  To be more precise, the list of ~il[monitor]ed ~il[rune]s is
  maintained as part of the preserved ~c[wormhole-output] of the
  ~ilc[break-rewrite] wormhole.  Because it is not part of the normal ~ilc[state],
  it may be changed by the user during proofs.  That is what allows
  you to install new ~il[monitor]s while debugging proofs.  But that means
  that the list of ~il[monitor]ed ~il[rune]s cannot be inspected from outside
  the wormhole.  Therefore, to decide whether a break is to occur when
  a given rule is applied, the rewriter must enter the ~ilc[break-rewrite]
  wormhole, supplying a form that causes interaction if the given
  rule's break condition is satisfied.  The user perceives this as
  though the wormhole was conditionally entered ~-[] a perception that
  is happily at odds with the informed user's knowledge that the list
  of ~il[monitor]ed ~il[rune]s is not part of the ~ilc[state].  In fact, the wormhole
  was unconditionally entered and the condition was checked from
  within the wormhole, that being the only ~il[state] in which the
  condition is known.

  Another illustrative example is available in the implemention of the
  ~ilc[monitor] command.  How can we add a new ~il[rune] to the list of ~il[monitor]ed
  ~il[rune]s while in the normal ACL2 ~ilc[state] (i.e., while not in a
  wormhole)?  The answer is: by getting into a wormhole.  In
  particular, when you type ~c[(monitor rune expr)] at the top-level of
  ACL2, ~ilc[monitor] enters the ~ilc[break-rewrite] wormhole with a cleverly
  designed first ~c[form].  That form adds ~il[rune] and ~c[expr] to the list of
  ~il[monitor]ed ~il[rune]s ~-[] said list only being available in ~ilc[break-rewrite]
  wormhole ~il[state]s.  Then the first form returns ~c[(value :q)], which
  causes us to exit the wormhole.  By using ~ilc[ld]-specials that
  completely suppress all output during the process, it does not
  appear to the user that a wormhole was entered.  The moral here is
  rather subtle: the first form supplied to ~c[wormhole] may be the entire
  computation you want to perform in the wormhole; it need not just be
  a predicate that decides if interaction is to occur.  Using
  wormholes of different names you can maintain a variety of
  ``hidden'' data structures that are always accessible (whether
  passed in or not).  This appears to violate completely the
  applicative semantics of ACL2, but it does not: because these data
  structures are only accessible via ~c[wormhole]s, it is impossible for
  them to affect any ACL2 computation (except in the comment window).

  As one might imagine, there is some overhead associated with
  entering a wormhole because of the need to copy the current ~ilc[state].
  This brings us back to ~c[pseudo-flg].  Ostensibly, ~c[wormhole] is a
  function and hence all of its argument expressions are evaluated
  outside the function (and hence, outside the wormhole it creates)
  and then their values are passed into the function where an
  appropriate wormhole is created.  In fact, ~c[wormhole] is a macro that
  permits the ~c[pseudo-flg] expression to peer dimly into the wormhole
  that will be created before it is created.  In particular,
  ~c[pseudo-flg] allows the user to access the ~c[wormhole-output] that will
  be used to create the wormhole ~il[state].

  This is done by allowing the user to mention the (apparently
  unbound) variable ~c[wormhole-output] in the first argument to ~c[wormhole].
  Logically, ~c[wormhole] is a macro that wraps
  ~bv[]
  (let ((wormhole-output nil)) ...)
  ~ev[]
  around the expression supplied as its first argument.  So logically,
  ~c[wormhole-output] is always ~c[nil] when the expression is
  evaluated.  However, actually, ~c[wormhole-output] is bound to the
  value of ~c[(@ wormhole-output)] on the last exit from a wormhole of
  the given name (or ~c[nil] if this is the first entrance).  Thus, the
  ~c[pseudo-flg] expression, while having to handle the possibility
  that ~c[wormhole-output] is ~c[nil], will sometimes see non-~c[nil]
  values.  The next question is, of course, ``But how can you get away
  with saying that logically ~c[wormhole-output] is always ~c[nil] but
  actually it is not?  That doesn't appear to be sound.'' But it is
  sound because whether ~c[pseudo-flg] evaluates to ~c[nil] or
  non-~c[nil] doesn't matter, since in either case ~c[wormhole] returns
  ~c[nil].  To make that point slightly more formal, imagine that
  ~c[wormhole] did not take ~c[pseudo-flg] as an argument.  Then it
  could be implemented by writing
  ~bv[]
  (if pseudo-flg (wormhole name input form ...) nil).
  ~ev[]
  Now since wormhole always returns ~c[nil], this expression is
  equivalent to ~c[(if pseudo-flg nil nil)] and we see that the value
  of ~c[pseudo-flg] is irrelevant.  So we could in fact allow the user
  to access arbitrary information to decide which branch of this if to
  take.  We allow access to ~c[wormhole-output] because it is often all
  that is needed.  We don't allow access to ~ilc[state] (unless ~ilc[state] is
  available at the level of the wormhole call) for technical reasons
  having to do with the difficulty of overcoming ~c[translate]'s
  prohibition of the sudden appearance of the variable ~ilc[state].

  We conclude with an example of the use of ~c[pseudo-flg].  This example
  is a simplification of our implementation of ~ilc[break-rewrite].  To
  enter ~ilc[break-rewrite] at the beginning of the attempted application of
  a rule, ~c[rule], we use
  ~bv[]
  (wormhole
   (and (f-get-global 'brr-mode state)
        (member-equal (access rewrite-rule rule :rune)
                      (cdr (assoc-eq 'monitored-runes wormhole-output))))
   'break-rewrite
   ...)
  ~ev[]
  The function in which this call of ~c[wormhole] occurs has ~ilc[state] as a
  formal.  The ~c[pseudo-flg] expression can therefore refer to ~ilc[state] to
  determine whether ~c['brr-mode] is set.  But the ~c[pseudo-flg] expression
  above mentions the variable ~c[wormhole-output]; this variable is not
  bound in the context of the call of ~c[wormhole]; if ~c[wormhole] were a
  simple function symbol, this expression would be illegal because it
  mentions a free variable.

  However, it is useful to think of ~c[wormhole] as a simple function that
  evaluates all of its arguments but to also imagine that somehow
  ~c[wormhole-output] is magically bound around the first argument so that
  ~c[wormhole-output] is the output of the last ~ilc[break-rewrite] wormhole.
  If we so imagine, then the ~c[pseudo-flg] expression above evaluates
  either to ~c[nil] or non-~c[nil] and we will enter the wormhole named
  ~ilc[break-rewrite] in the latter case.

  Now what does the ~c[pseudo-flg] expression above actually test?  We
  know the format of our own ~c[wormhole-output] because we are
  responsible for maintaining it.  In particular, we know that the
  list of ~il[monitor]ed ~il[rune]s can be obtained via
  ~bv[]
  (cdr (assoc-eq 'monitored-runes wormhole-output)).
  ~ev[]
  Using that knowledge we can design a ~c[pseudo-flg] expression which
  tests whether (a) we are in ~c[brr-mode] and (b) the ~il[rune] of the
  current rule is a member of the ~il[monitor]ed ~il[rune]s.  Question (a) is
  answered by looking into the current ~ilc[state].  Question (b) is
  answered by looking into that part of the about-to-be-created
  wormhole ~il[state] that will differ from the current ~ilc[state].  To
  reiterate the reason we can make ~c[wormhole-output] available here
  even though it is not in the current ~ilc[state]: logically speaking the
  value of ~c[wormhole-output] is irrelevant because it is only used to
  choose between two identical alternatives.  This example also makes
  it clear that ~c[pseudo-flg] provides no additional functionality.
  The test made in the ~c[pseudo-flg] expression could be moved into
  the first form evaluated by the wormhole ~-[] changing the free
  variable ~c[wormhole-output] to ~c[(@ wormhole-output)] and arranging
  for the first form to return ~c[(value :q)] when the ~c[pseudo-flg]
  expression returns ~c[nil].  The only reason we provide the
  ~c[pseudo-flg] feature is because it allows the test to be carried
  out without the overhead of entering the wormhole.

  Wormholes can be used not only in ~c[:]~ilc[program] mode definitions but also
  in ~c[:]~ilc[logic] mode definitions.  Thus, it is possible (though somewhat
  cumbersome without investing in macro support) to annotate logical
  functions with output facilities that do not require ~c[state].  These
  facilities do not complicate proof obligations.  Suppose then that
  one doctored a simple function, e.g., APP, so as to do some printing
  and then proved that APP is associative.  The proof may generate
  extraneous output due to the doctoring.  Furthermore, contrary to
  the theorem proved, execution of the function appears to affect
  *standard-co*.  To see what the function ``really'' does when
  evaluated, enter raw lisp and set the global variable
  *inhibit-wormhole-activityp* to t."

  `(let ((wormhole-name ,name))
     (cond
      ((let ((wormhole-output
              #+acl2-loop-only nil
              #-acl2-loop-only (cdr (assoc-equal wormhole-name
                                                  *wormhole-outputs*))))
         (prog2$ wormhole-output
                 (check-vars-not-free (wormhole-name) ,pseudo-flg)))
       (wormhole1
        wormhole-name
        (check-vars-not-free (wormhole-name) ,input)
        (check-vars-not-free (wormhole-name) ,form)
        (check-vars-not-free (wormhole-name)
          (list
           ,@(append
              (if current-packagep
                  (list `(cons 'current-package ,current-package))
                  nil)
              (if ld-skip-proofspp
                  (list `(cons 'ld-skip-proofsp ,ld-skip-proofsp))
                  nil)
              (if ld-redefinition-actionp
                  (list `(cons 'ld-redefinition-action
                               ,ld-redefinition-action))
                  nil)
              (list `(cons 'ld-prompt ,ld-prompt))
              (if ld-keyword-aliasesp
                  (list `(cons 'ld-keyword-aliases
                               ,ld-keyword-aliases))
                  nil)
              (if ld-pre-eval-filterp
                  (list `(cons 'ld-pre-eval-filter ,ld-pre-eval-filter))
                  nil)
              (if ld-pre-eval-printp
                  (list `(cons 'ld-pre-eval-print ,ld-pre-eval-print))
                  nil)
              (if ld-post-eval-printp
                  (list `(cons 'ld-post-eval-print ,ld-post-eval-print))
                  nil)
              (if ld-evisc-tuplep
                  (list `(cons 'ld-evisc-tuple ,ld-evisc-tuple))
                  nil)
              (if ld-error-triplesp
                  (list `(cons 'ld-error-triples ,ld-error-triples))
                  nil)
              (if ld-error-actionp
                  (list `(cons 'ld-error-action ,ld-error-action))
                  nil)
              (if ld-query-control-alistp
                  (list `(cons 'ld-query-control-alist ,ld-query-control-alist))
                  nil)
              (if ld-verbosep
                  (list `(cons 'ld-verbose ,ld-verbose))
                  nil))))))
           (t nil))))

(defmacro assign-wormhole-output (pseudo-flg name input expr)

; This function is supposed to be equivalent to the wormhole call below.  The
; reason we define it specially is because it is used by push-accp and pop-accp
; and hence its efficiency is paramount.  The trouble with the logical
; definition is that entering the wormhole requires saving a lot of state,
; entering ld and translating the form supplied, and then the state saving and
; undoing code.

; One side-effect of having this function it is is now pretty efficient to
; save things into wormholes, if that is all you want to do.  For example,
; you can put

; (assign-wormhole-output t 'my-place input (foo ...))

; any place you can evaluate (foo ...) to squirrel its value away into
; (@ wormhole-output) of the wormhole called 'my-place.  The expression
; (foo ...) may mention anything that could have been used in a wormhole
; command, including (@ wormhole-input) and (@ wormhole-output), which 
; have the obvious values.

  (declare (xargs :guard (and (consp expr)
                              (eq (car expr) 'quote)
                              (consp (cdr expr)))))
  #+acl2-loop-only
  `(wormhole ,pseudo-flg ,name ,input
             `(er-progn (assign wormhole-output ,,expr)
                        (value :q))
            :ld-prompt  nil
            :ld-pre-eval-filter :all
            :ld-pre-eval-print  nil
            :ld-post-eval-print :command-conventions
            :ld-evisc-tuple nil
            :ld-error-triples  t
            :ld-error-action :error
            :ld-query-control-alist nil
            :ld-verbose nil)
  #-acl2-loop-only
  `(let* ((state *the-live-state*)
          (wormhole-name ,name)
          (wormhole-output
           (cdr (assoc-equal wormhole-name *wormhole-outputs*))))
     (cond
      ((prog2$ wormhole-output
               (check-vars-not-free (wormhole-name) ,pseudo-flg))
       (let* ((wormhole-saved-input
               (check-vars-not-free (wormhole-name wormhole-output)
                                    ,input))

; Note that we evaluate input before we have f-put-globals wormhole-name,
; etc.  That is because the input is evaluated in the environment of the
; call, not in the wormhole.  Next we collect the information necessary to
; undo the assignments we must carry out before evaluating the expr.

              (wormhole-old-name-boundp (boundp-global1 'wormhole-name state))
              (wormhole-old-name (if wormhole-old-name-boundp
                                     (f-get-global 'wormhole-name state)
                                     nil))
              (wormhole-old-input-boundp (boundp-global1 'wormhole-input state))
              (wormhole-old-input
               (if wormhole-old-input-boundp
                   (f-get-global 'wormhole-input state)
                   nil))
              (wormhole-old-output-boundp
               (boundp-global1 'wormhole-output state))
              (wormhole-old-output (if wormhole-old-output-boundp
                                       (f-get-global 'wormhole-output state)
                                       nil)))
        (f-put-global 'wormhole-name wormhole-name state)
        (f-put-global 'wormhole-input wormhole-saved-input state)
        (f-put-global 'wormhole-output wormhole-output state)
        (setq *wormhole-outputs*
              (put-assoc-equal wormhole-name
                               (check-vars-not-free
                                (wormhole-name
                                 wormhole-output
                                 wormhole-old-name-boundp wormhole-old-name
                                 wormhole-old-input-boundp wormhole-old-input
                                 wormhole-old-output-boundp wormhole-old-output)
                                ,(cadr expr))
                               *wormhole-outputs*))
        (if wormhole-old-output-boundp
            (f-put-global 'wormhole-output wormhole-old-output state)
            (makunbound-global 'wormhole-output state))
        (if wormhole-old-input-boundp
            (f-put-global 'wormhole-input wormhole-old-input state)
            (makunbound-global 'wormhole-input state))
        (if wormhole-old-name-boundp
            (f-put-global 'wormhole-name wormhole-old-name state)
            (makunbound-global 'wormhole-name state))
        nil))
     (t nil))))

(defun global-set (var val wrld)
  (declare (xargs :guard (and (symbolp var)
                              (worldp wrld))))
  (putprop var 'global-value val wrld))

(defun defabbrev1 (lst)
  (declare (xargs :guard (true-listp lst)))
  (cond ((null lst) nil)
        (t (cons (list 'list (list 'quote (car lst)) (car lst))
                 (defabbrev1 (cdr lst))))))

(defmacro er (severity context str &rest str-args)
  (declare (xargs :guard (and (true-listp str-args)
                              (member-symbol-name (symbol-name severity)
                                                  '(hard hard? hard! soft))
                              (<= (length str-args) 10))))

; Note:  We used to require (stringp str) but then we
; started writing such forms as (er soft ctx msg x y z), where
; msg was bound to the error message str (because the same string
; was used many times).

; The special form (er hard "..." &...) expands into a call of
; illegal on "..." and an alist built from &....  Since
; illegal has a guard of nil, the attempt to prove the
; correctness of a fn producing a hard error will require proving
; that the error can never occur.  At runtime, illegal causes a
; CLTL error.

; The form (er soft ctx "..." &...) expands into a call of error1 on
; ctx, "..." and an alist built from &....  At runtime error1 builds an
; error object and returns it.  Thus, soft errors are not errors at
; all in the CLTL sense and any function calling one which might
; cause an error ought to handle it.

; Just to make it easier to debug our code, we have arranged for the
; er macro to actually produce a prog2 form in which the second arg
; is as described above but the preceding one is an fmt statement
; which will actually print the error str and alist.  Thus, we can
; see when soft errors occur, whether or not the calling program
; handles them appropriately.

; We do not advertise the hard! severity, at least not yet.  The implementation
; uses it to force a hard error even in contexts where we would normally return
; nil.

  ":Doc-Section ACL2::Programming

  print an error message and ``cause an error''~/
  ~bv[]
  Example Forms:
  (er hard  'top-level \"Illegal inputs, ~~x0 and ~~x1.\" a b)
  (er hard? 'top-level \"Illegal inputs, ~~x0 and ~~x1.\" a b)
  (er soft  'top-level \"Illegal inputs, ~~x0 and ~~x1.\" a b)
  ~ev[]
  The examples above all print an error message to standard output saying that
  ~c[a] and ~c[b] are illegal inputs.  However, the first two abort evaluation
  after printing an error message, while the third returns ~c[(mv t nil state)]
  after printing an error message.  The result in the third case can be
  interpreted as an ``error'' when programming with the ACL2 ~ilc[state],
  something most ACL2 users will probably not want to do;
  ~pl[ld-error-triples] and ~pl[er-progn].

  ~c[Er] is a macro, and the above three examples expand to calls of ACL2
  functions, as shown below.  ~l[illegal], ~pl[hard-error], and ~pl[error1],
  respectively.~/
  ~bv[]
  General forms:
  (er hard  ctx fmt-string arg1 arg2 ... argk)
    ==> {macroexpands, in essence, to:}
  (ILLEGAL    CTX FMT-STRING
              (LIST (CONS #\\0 ARG1) (CONS #\\1 ARG2) ... (CONS #\\k ARGk)))

  (er hard? ctx fmt-string arg1 arg2 ... argk)
    ==> {macroexpands, in essence, to:}
  (HARD-ERROR CTX FMT-STRING
              (LIST (CONS #\\0 ARG1) (CONS #\\1 ARG2) ... (CONS #\\k ARGk)))

  (er soft  ctx fmt-string arg1 arg2 ... argk)
    ==> {macroexpands, in essence, to:}
  (ERROR1     CTX FMT-STRING
              (LIST (CONS #\\0 ARG1) (CONS #\\1 ARG2) ... (CONS #\\k ARGk)))
  ~ev[]~/"

  (let ((alist (make-fmt-bindings '(#\0 #\1 #\2 #\3 #\4
                                    #\5 #\6 #\7 #\8 #\9)
                                  str-args))
        (severity-name (symbol-name severity)))
    (cond ((equal severity-name "SOFT")
           (list 'error1 context str alist 'state))
          ((equal severity-name "HARD?")
           (list 'hard-error context str alist))
          ((equal severity-name "HARD")
           (list 'illegal context str alist))
          ((equal severity-name "HARD!")
           #+acl2-loop-only (list 'illegal context str alist)
           #-acl2-loop-only `(let ((*hard-error-returns-nilp* nil))
                              (illegal ,context ,str ,alist)))
          (t

; The final case should never happen.

           (illegal 'top-level
                    "Illegal severity, ~x0; macroexpansion of ER failed!"
                    (list (cons #\0 severity)))))))

(defun legal-variable-or-constant-namep (name)

; This function checks the syntax of variable or constant name
; symbols.  In all cases, name must be a symbol that is not in the
; keyword package or among *common-lisp-specials-and-constants*
; (except t and nil), or in the main Lisp package but outside
; *common-lisp-symbols-from-main-lisp-package*, and that does not
; start with an ampersand.  The function returns 'constant, 'variable,
; or nil.

; WARNING: T and nil are legal-variable-or-constant-nameps
; because we want to allow their use as constants.

; We now allow some variables (but still no constants) from the main Lisp
; package.  See *common-lisp-specials-and-constants*.  The following two note
; explains why we have been cautious here.

; Historical Note

; This package restriction prohibits using some very common names as
; variables or constants, e.g., MAX and REST.  Why do we do this?  The
; reason is that there are a few such symbols, such as
; LAMBDA-LIST-KEYWORDS, which if bound or set could cause real
; trouble.  Rather than attempt to identify all of the specials of
; CLTL that are prohibited as ACL2 variables, we just prohibit them
; all.  One might be reminded of Alexander cutting the Gordian Knot.
; We could spend a lot of time unravelling complex questions about
; specials in CLTL or we can get on with it.  When ACL2 prevents you
; from using REST as an argument, you should see the severed end of a
; once tangled rope.

; For example, akcl and lucid (and others perhaps) allow you to define
; (defun foo (boole-c2) boole-c2) but then (foo 3) causes an error.
; Note that boole-c2 is recognized as special (by
; system::proclaimed-special-p) in lucid, but not in akcl (by
; si::specialp); in fact it's a constant in both.  Ugh.

; End of Historical Note.

  (and (symbolp name)
       (cond
        ((or (eq name t) (eq name nil))
         'constant)
        (t (let ((p (symbol-package-name name)))
             (and (not (equal p "KEYWORD"))
                  (let ((s (symbol-name name)))
                    (cond
                     ((and (not (= (length s) 0))
                           (eql (char s 0) #\*)
                           (eql (char s (1- (length s))) #\*))
                      (if (equal p *main-lisp-package-name*)
                          nil
                        'constant))
                     ((and (not (= (length s) 0))
                           (eql (char s 0) #\&))
                      nil)
                     ((equal p *main-lisp-package-name*)
                      (and (not (member-eq
                                 name
                                 *common-lisp-specials-and-constants*))
                           (member-eq
                            name
                            *common-lisp-symbols-from-main-lisp-package*)
                           'variable))
                     (t 'variable)))))))))

(defun legal-constantp1 (name)

; This function should correctly distinguish between variables and
; constants for symbols that are known to satisfy
; legal-variable-or-constant-namep.  Thus, if name satisfies this
; predicate then it cannot be a variable.

  (declare (xargs :guard (symbolp name)))
  (or (eq name t)
      (eq name nil)
      (let ((s (symbol-name name)))
        (and (not (= (length s) 0))
             (eql (char s 0) #\*)
             (eql (char s (1- (length s))) #\*)))))

(defun tilde-@-illegal-variable-or-constant-name-phrase (name)

; Assume that legal-variable-or-constant-namep has failed on name.
; We return a phrase that when printed with ~@0 will complete the
; sentence "Variable names must ...".  Observe that the sentence
; could be "Constant names must ...".

  (cond ((not (symbolp name)) "be symbols")
        ((keywordp name) "not be in the KEYWORD package")
        ((and (legal-constantp1 name)
              (equal (symbol-package-name name) *main-lisp-package-name*))
         (cons "not be in the main Lisp package, ~x0"
               (list (cons #\0 *main-lisp-package-name*))))
        ((and (> (length (symbol-name name)) 0)
              (eql (char (symbol-name name) 0) #\&))
         "not start with ampersands")
        ((and (not (legal-constantp1 name))
              (member-eq name *common-lisp-specials-and-constants*))
         "not be among certain symbols from the main Lisp package, namely, the ~
          value of the list *common-lisp-specials-and-constants*")
        ((and (not (legal-constantp1 name))
              (equal (symbol-package-name name) *main-lisp-package-name*)
              (not (member-eq name *common-lisp-symbols-from-main-lisp-package*)))
         "either not be in the main Lisp package, or else must be among the ~
          imports into ACL2 from that package, namely, the list ~
          *common-lisp-symbols-from-main-lisp-package*")
        (t "be approved by LEGAL-VARIABLE-OR-CONSTANT-NAMEP and this ~
            one wasn't, even though it passes all the checks known to ~
            the diagnostic function ~
            TILDE-@-ILLEGAL-VARIABLE-OR-CONSTANT-NAME-PHRASE")))

(defun legal-constantp (name)

; A name may be declared as a constant if it has the syntax of a
; variable or constant (see legal-variable-or-constant-namep) and
; starts and ends with a *.

; WARNING: Do not confuse this function with defined-constant.

  (eq (legal-variable-or-constant-namep name) 'constant))

(defun defined-constant (name w)

; Name is a defined-constant if it has been declared with defconst.
; If name is a defined-constant then we can show that it satisfies
; legal-constantp, because when a name is declared as a constant we
; insist that it satisfy the syntactic check.  But there are
; legal-constantps that aren't defined-constants, e.g., any symbol
; that could be (but hasn't yet been) declared as a constant.  We
; check, below, that name is a symbolp just to guard the getprop.

; This function returns the quoted term that is the value of name, if
; name is a constant.  That result is always non-nil (it may be (quote
; nil) of course).

  (and (symbolp name)
       (getprop name 'const nil 'current-acl2-world w)))

(defun legal-variablep (name)

; Name may be used as a variable if it has the syntax of a variable
; (see legal-variable-or-constant-namep) and does not have the syntax of
; a constant, i.e., does not start and end with a *.

  (eq (legal-variable-or-constant-namep name) 'variable))

(defun lambda-keywordp (x)
  (and (symbolp x)
       (eql 1 (string<= "&" (symbol-name x)))))

(defun no-lambda-keywordsp (lst)
  (declare (xargs :guard (true-listp lst)))
  (cond ((null lst) t)
        ((lambda-keywordp (car lst))
         nil)
        (t (no-lambda-keywordsp (cdr lst)))))

(defun arglistp1 (lst)

; Every element of lst is a legal-variablep.

  (cond ((atom lst) (null lst))
        (t (and (legal-variablep (car lst))
                (arglistp1 (cdr lst))))))

(defun arglistp (lst)
  (and (arglistp1 lst)
       (no-duplicatesp lst)))

(defun find-first-bad-arg (args)

; This function is only called when args is known to be a non-arglistp
; that is a true list.  It returns the first bad argument and a string
; that completes the phrase "... violates the rules because it ...".

  (declare (xargs :guard (and (true-listp args)
                              (not (arglistp args)))))
  (cond
   ;;((null args) (mv nil nil)) -- can't happen, given the guard!
   ((not (symbolp (car args))) (mv (car args) "is not a symbol"))
   ((legal-constantp1 (car args))
    (mv (car args) "has the syntax of a constant"))
   ((lambda-keywordp (car args))
    (mv (car args) "is a lambda keyword"))
   ((keywordp (car args))
    (mv (car args) "is in the KEYWORD package"))
   ((member-eq (car args) *common-lisp-specials-and-constants*)
    (mv (car args) "belongs to the list *common-lisp-specials-and-constants* ~
                    of symbols from the main Lisp package"))
   ((member-eq (car args) (cdr args))
    (mv (car args) "occurs more than once in the list"))
   ((and (equal (symbol-package-name (car args)) *main-lisp-package-name*)
         (not (member-eq (car args) *common-lisp-symbols-from-main-lisp-package*)))
    (mv (car args) "belongs to the main Lisp package but not to the list ~
                    *common-lisp-symbols-from-main-lisp-package*"))
   (t (find-first-bad-arg (cdr args)))))

(defun process-defabbrev-declares (decls)
  (cond ((endp decls) ())

; Here we do a cheap check that the declare form is illegal.  It is tempting to
; use collect-declarations, but it take state.  Anyhow, there is no soundness
; issue; the user will just be a bit surprised when the error shows up later as
; the macro defined by the defabbrev is applied.

        ((not (and (consp (car decls))
                   (eq (caar decls) 'DECLARE)
                   (true-list-listp (cdar decls))
                   (subsetp-eq (strip-cars (cdar decls))
                               '(IGNORE TYPE))))
         (er hard 'process-defabbrev-declares
             "In a DEFABBREV form, each expression after the argument list ~
              but before the body must be of the form (DECLARE decl1 .. ~
              declk), where each dcli is of the form (IGNORE ..) or (TYPE ~
              ..).  The form ~x0 is thus illegal."
             (car decls)))
        (t
         (cons (kwote (car decls))
               (process-defabbrev-declares (cdr decls))))))

(defmacro defabbrev (fn args &rest body)
  ":Doc-Section Events
  a convenient form of macro definition for simple expansions~/
  ~bv[]
  Examples:
  (defabbrev snoc (x y) (append y (list x)))
  (defabbrev sq (x) (declare (type (signed-byte 8) x)) (* x x))

  General Form:
  (defabbrev name (v1 ... vn) doc-string decl1 ... declk body)
  ~ev[]
  where ~c[name] is a new function symbol, the ~c[vi] are distinct
  variable symbols, and ~c[body] is a term.  The ~c[decli], if supplied,
  should be legal ~c[declare] forms; ~pl[declare].  ~c[Doc-string] is
  an optional ~il[documentation] string; ~pl[doc-string].

  Roughly speaking, the ~c[defabbrev] event is akin to defining
  ~c[f] so that ~c[(f v1 ... vn) = body].  But rather than do this
  by adding a new axiom, ~c[defabbrev] defines ~c[f] to be a macro
  so that ~c[(f a1 ... an)] expands to ~c[body], with the ``formals,''
  ~c[vi], replaced by the ``actuals,'' ~c[ai].~/

  For example, if ~c[snoc] is defined as shown in the first example
  above, then ~c[(snoc (+ i j) temp)] is just an abbreviation for
  ~bv[]
  (append temp (list (+ i j))).
  ~ev[]

  In order to generate efficiently executable Lisp code,
  the macro that ~c[defabbrev] introduces uses a ~ilc[let] to
  bind the ``formals'' to the ``actuals.''  Consider the second
  example above.  Logically speaking, ~c[(sq (ack i j))] is an
  abbreviation for ~c[(* (ack i j) (ack i j))].  But in fact
  the macro for ~c[sq] introduced by ~c[defabbrev] actually
  arranges for ~c[(sq (ack i j))] to expand to:
  ~bv[]
  (let ((x (ack i j)))
    (* x x))
  ~ev[]
  which executes more efficiently than ~c[(* (ack i j) (ack i j))].

  In the theorem prover, the ~c[let] above expands to
  ~bv[]
  ((lambda (x) (* x x)) (ack i j))
  ~ev[]
  and thence to ~c[(* (ack i j) (ack i j))].

  It is important to note that the term in ~c[body] should not contain a
  call of ~c[name] ~-[] i.e., ~c[defabbrev] should not be used in place of
  ~c[defun] when the function is recursive.  ACL2 will not complain when
  the ~c[defabbrev] form is processed, but instead ACL2 will more than
  likely go into an infinite loop during macroexpansion of any form that
  has a call of ~c[name].

  It is also important to note that the parameters of any call of a
  macro defined by defabbrev will, as is the case for the parameters
  of a function call, be evaluated before the body is evaluated, since
  this is the evaluation order of ~ilc[let].  This may lead to some
  errors or unexpected inefficiencies during evaluation if the body
  contains any conditionally evaluted forms like ~c[cond], ~c[case],
  or ~c[if].  Consider the following example.
  ~bv[]
  (defabbrev foo (x y)
    (if (test x) (bar y) nil))
  ~ev[]
  Notice a typical one-step expansion of a call of ~c[foo]
  (~pl[trans1]):
  ~bv[]
  ACL2 !>:trans1 (foo expr1 expr2)
   (LET ((X EXPR1) (Y EXPR2))
        (IF (TEST X) (BAR Y) NIL))
  ACL2 !>
  ~ev[]
  Now imagine that ~c[expr2] is a complicated expression whose
  evaluation is intended only when the predicate ~c[test] holds of
  ~c[expr1].  The expansion above suggests that ~c[expr2] will always
  be evaluated by the call ~c[(foo expr1 expr2)], which may be
  inefficient (since perhaps we only need that value when ~c[test] is
  true of ~c[expr1]).  The evaluation of ~c[expr2] may even cause an
  error, for example in ~c[:]~ilc[program] mode if the expression ~c[expr2] has
  been constructed in a manner that could cause a guard violation
  unless ~c[test] holds of ~c[expr1]."

  (cond ((null body)
         (er hard (cons 'defabbrev fn)
             "The body of this DEFABBREV form is missing."))
        ((not (true-listp args))
         (er hard (cons 'defabbrev fn)
             "The formal parameter list for a DEFABBREV must be a true list.  The ~
              argument list ~x0 is thus illegal."
             args))
        ((not (arglistp args))
         (mv-let (culprit explan)
                 (find-first-bad-arg args)
                 (er hard (cons 'defabbrev fn)
                     "The formal parameter list for a DEFABBREV must be a ~
                      list of distinct variables, but ~x0 does not meet these ~
                      conditions.  The element ~x1 ~@2."
                     args culprit explan)))
        (t 
         (mv-let (doc-string-list body)
                 (if (and (stringp (car body))
                          (cdr body))
                     (mv (list (car body)) (cdr body))
                   (mv nil body))
                 (cond ((null body)
                        (er hard (cons 'defabbrev fn)
                            "This DEFABBREV form has a doc string but no ~
                             body."))
                       ((and (consp (car (last body)))
                             (eq (caar (last body)) 'declare))
                        (er hard (cons 'defabbrev fn)
                            "The body of this DEFABBREV form is a DECLARE ~
                             form, namely ~x0.  This is illegal and probably ~
                             is not what was intended."
                            (car (last body))))
                       (t
                        `(defmacro ,fn ,args
                           ,@doc-string-list
                           (list 'let (list ,@(defabbrev1 args))
                                 ,@(process-defabbrev-declares (butlast body 1))
                                 ',(car (last body))))))))))

;; RAG - I changed the primitive guard for the < function, and the
;; complex function.  Added the functions complexp, realp, and floor1.

;; RAG - I subsequently changed this to add the non-standard functions
;; standard-numberp, standard-part and i-large-integer.  I had some
;; questions as to whether these functions should appear on this list
;; or not.  After considering carefully, I decided that was the right
;; course of action.  In addition to adding them to the list below, I
;; also add them to *non-standard-primitives* which is a special list
;; of non-standard primitives.  Functions in this list are considered
;; to be constrained.  Moreover, they are given the value t for the
;; property 'unsafe-induction so that recursion and induction are
;; turned off for terms built from these functions.

(defconst *primitive-formals-and-guards*

; Keep this in sync with ev-fncall, cons-term1, and type-set-primitive, and
; with the documentation and "-completion" axioms of the primitives.  Also be
; sure to define a *1* function for each function in this list that is not a
; member of *oneify-primitives*.

  '((acl2-numberp (x) 't)
    (bad-atom<= (x y) (if (bad-atom x) (bad-atom y) 'nil))
    (binary-* (x y) (if (acl2-numberp x) (acl2-numberp y) 'nil))
    (binary-+ (x y) (if (acl2-numberp x) (acl2-numberp y) 'nil))
    (unary-- (x) (acl2-numberp x))
    (unary-/ (x) (if (acl2-numberp x) (not (equal x '0)) 'nil))
    (< (x y) 

; We avoid the temptation to use real/rationalp below, since it is a macro.

       (if #+:non-standard-analysis (realp x) 
           #-:non-standard-analysis (rationalp x)
         #+:non-standard-analysis (realp y) 
         #-:non-standard-analysis (rationalp y)
         'nil))
    (car (x) (if (consp x) 't (equal x 'nil)))
    (cdr (x) (if (consp x) 't (equal x 'nil)))
    (char-code (x) (characterp x))
    (characterp (x) 't)
    (code-char (x) (if (integerp x) (if (< x '0) 'nil (< x '256)) 'nil))
    (complex (x y)
             (if #+:non-standard-analysis (realp x) 
                 #-:non-standard-analysis (rationalp x)
               #+:non-standard-analysis (realp y) 
               #-:non-standard-analysis (rationalp y)
               'nil))
    (complex-rationalp (x) 't)
    #+:non-standard-analysis
    (complexp (x) 't)
    (coerce (x y)
            (if (equal y 'list)
                (stringp x)
                (if (equal y 'string)
                    (character-listp x)
                    'nil)))
    (cons (x y) 't)
    (consp (x) 't)
    (denominator (x) (rationalp x))
    (equal (x y) 't)
    #+:non-standard-analysis
    (floor1 (x) (realp x))
    (if (x y z) 't)
    (imagpart (x) (acl2-numberp x))
    (integerp (x) 't)
    (intern-in-package-of-symbol (str sym) (if (stringp str) (symbolp sym) 'nil))
    (numerator (x) (rationalp x))
    (pkg-witness (pkg) (and (stringp pkg) (not (equal pkg ""))))
    (rationalp (x) 't)
    #+:non-standard-analysis
    (realp (x) 't)
    (realpart (x) (acl2-numberp x))
    (stringp (x) 't)
    (symbol-name (x) (symbolp x))
    (symbol-package-name (x) (symbolp x))
    (symbolp (x) 't)
    #+:non-standard-analysis
    (standard-numberp (x) 't)
    #+:non-standard-analysis
    (standard-part (x) (acl2-numberp x))
    #+:non-standard-analysis
    (i-large-integer () 't)))

(defconst *t* (quote (quote t)))

(defconst *true-clause* (list *t*))

(defconst *nil* (quote (quote nil)))

(defconst *0* (quote (quote 0)))

(defconst *1* (quote (quote 1)))

(defconst *-1* (quote (quote -1)))

;; RAG - To keep in sync with *primitive-formals-and-guards*, I
;; changed the primitive guard for the < function, and the complex
;; function.  Added the functions complexp, realp, and floor1.

(defconst *cons-term1-alist*

; Keep this in sync with *primitive-formals-and-guards*.  In particular, every
; fn mentioned in that list must be handled here identically if it is handled
; here at all.  We handle fn = 'NOT here as well, just so that substitution
; functions never create (NOT 'T).  But with the exception of NOT, every
; function dealt with here must also appear in *primitive-formals-and-guards*.
; It is legal to omit functions here.  For example, we omit bad-atom<= and,
; even with #+:non-standard-analysis, we omit i-large-integer, because there is
; no legal ACL2 evg to which these evaluate.

  '((acl2-numberp (kwote (acl2-numberp x)))
    (binary-* (kwote (* (fix x)
                        (fix y))))
    (binary-+ (kwote (+ (fix x)
                        (fix y))))
    (unary-- (kwote (- (fix x))))
    (unary-/ (cond ((and (acl2-numberp x) (not (equal x 0)))
                    (kwote (/ x)))
                   (t *0*)))
    (< (cond ((and (real/rationalp x) (real/rationalp y))
              (kwote (< x y)))
             (t (kwote (let ((x (fix x))
                             (y (fix y)))
                         (or (< (realpart x)
                                (realpart y))
                             (and (= (realpart x)
                                     (realpart y))
                                  (< (imagpart x)
                                     (imagpart y)))))))))
    (car (cond ((consp x)
                (kwote (car x)))
               (t *nil*)))
    (cdr (cond ((consp x)
                (kwote (cdr x)))
               (t *nil*)))
    (char-code (cond ((characterp x)
                      (kwote (char-code x)))
                     (t *0*)))
    (characterp (kwote (characterp x)))
    (code-char (cond ((and (integerp x) (<= 0 x) (< x 256))
                      (kwote (code-char x)))
                     (t (kwote (code-char 0)))))
    (complex (kwote (complex (if (real/rationalp x) x 0)
                             (if (real/rationalp y) y 0))))
    (complex-rationalp (kwote (complex-rationalp x)))
    #+:non-standard-analysis
    (complexp (kwote (complexp x)))
    (coerce (cond ((equal y 'list)
                   (if (stringp x)
                       (kwote (coerce x 'list))
                     *nil*))
                  ((character-listp x)
                   (kwote (coerce x 'string)))
                  (t (kwote (coerce (make-character-list x) 'string)))))
    (cons (kwote (cons x y)))
    (consp (kwote (consp x)))
    (denominator (cond ((rationalp x)
                        (kwote (denominator x)))
                       (t *1*)))
    (equal (kwote (equal x y)))
    #+:non-standard-analysis
    (floor1 (kwote (floor x 1)))
    (if (kwote (if x y (cadr (caddr args)))))
    (imagpart (cond ((complex-rationalp x) ; could be acl2-numberp
                     (kwote (imagpart x)))
                    (t *0*)))
    (integerp (kwote (integerp x)))
    (intern-in-package-of-symbol
     (cond ((and (stringp x)
                 (symbolp y))
            (kwote (intern-in-package-of-symbol x y)))
           (t *nil*)))
    (numerator (cond ((rationalp x)
                      (kwote (numerator x)))
                     (t *0*)))

; We need to obtain (known-package-alist state) in order to evaluate
; pkg-witness, so we do not give it any special treatment.

    (rationalp (kwote (rationalp x)))
    #+:non-standard-analysis
    (realp (kwote (realp x)))
    (realpart (cond ((acl2-numberp x)
                     (kwote (realpart x)))
                    (t *0*)))
    (stringp (kwote (stringp x)))
    (symbol-name (cond ((symbolp x)
                        (kwote (symbol-name x)))
                       (t (kwote ""))))
    (symbol-package-name
     (cond ((symbolp x)
            (kwote (symbol-package-name x)))
           (t (kwote ""))))
    (symbolp (kwote (symbolp x)))
    #+:non-standard-analysis
    (standard-numberp (kwote (acl2-numberp x)))
    #+:non-standard-analysis
    (standard-part (cond ((acl2-numberp x)
                          (kwote x))
                         (t *0*)))
    (not (kwote (not x)))))

(defmacro cons-term2-body ()
  `(let ((x (cadr (car args)))
         (y (cadr (cadr args))))
     (case fn
       ,@*cons-term1-alist*
       (otherwise (cons fn args)))))

(defun cons-term2 (fn args)
  (cons-term2-body))

(defmacro cons-term1 (fn args)
  (or (and (consp fn)
           (eq (car fn) 'quote)
           (symbolp (cadr fn))
           (let ((expr (cadr (assoc-eq (cadr fn) *cons-term1-alist*))))
             (if expr
                 `(let* ((args ,args)
                         (x (cadr (car args)))
                         ,@(and (cdr (cadr (assoc-eq
                                            (cadr fn)
                                            *primitive-formals-and-guards*)))
                                '((y (cadr (cadr args))))))
                    ,expr)
               `(cons ,fn ,args))))
      `(cons-term2 ,fn ,args)))

(defun quote-listp (l)
  (declare (xargs :guard (true-listp l)))
  (cond ((null l) t)
        (t (and (quotep (car l))
                (quote-listp (cdr l))))))

(defun cons-term (fn args)
  (cond ((quote-listp args)
         (cons-term1 fn args))
        (t (cons fn args))))

(defmacro cons-term* (fn &rest args)
  `(cons-term ,fn (list ,@args)))

(defmacro mcons-term (fn args)

; The "m" in "mcons-term" is for "maybe fast".  Some calls of this macro can
; probably be replaced with fcons-term.

  `(cons-term ,fn ,args))

(defmacro mcons-term* (fn &rest args)

; The "m" in "mcons-term*" is for "maybe fast".  Some of calls of this macro
; can probably be replaced with fcons-term*.

  `(cons-term* ,fn ,@args))

(defmacro fcons-term* (&rest x)

#|
; Start experimental code mod, to check that calls of fcons-term are legitimate
; shortcuts in place of the corresponding known-correct calls of cons-term.
  #-acl2-loop-only
  `(let* ((fn-used-only-in-fcons-term* ,(car x))
          (args-used-only-in-fcons-term* (list ,@(cdr x)))
          (result (cons fn-used-only-in-fcons-term*
                        args-used-only-in-fcons-term*)))
     (assert$ (equal result (cons-term fn-used-only-in-fcons-term*
                                       args-used-only-in-fcons-term*))
              result))
  #+acl2-loop-only
; End experimental code mod.
|#

  (cons 'list x))

(defmacro fcons-term (fn args)

#|
; Start experimental code mod, to check that calls of fcons-term are legitimate
; shortcuts in place of the corresponding known-correct calls of cons-term.
  #-acl2-loop-only
  `(let* ((fn-used-only-in-fcons-term ,fn)
          (args-used-only-in-fcons-term ,args)
          (result (cons fn-used-only-in-fcons-term
                        args-used-only-in-fcons-term)))
     (assert$ (equal result (cons-term fn-used-only-in-fcons-term
                                       args-used-only-in-fcons-term))
              result))
  #+acl2-loop-only
; End experimental code mod.
|#

  (list 'cons fn args))

(defun fargn1 (x n)
  (declare (xargs :guard (and (integerp n)
                              (> n 0))))
  (cond ((eql n 1) (list 'cdr x))
        (t (list 'cdr (fargn1 x (- n 1))))))

(defmacro fargn (x n)
  (list 'car (fargn1 x n)))

(defun cdr-nest (n v)
  (cond ((equal n 0) v)
        (t (fargn1 v n))))

(defun all-but-last (l)
  (cond ((endp l) nil)
        ((endp (cdr l)) nil)
        (t (cons (car l) (all-but-last (cdr l))))))

(defun equal-x-constant (x const)

; x is an arbitrary term, const is a quoted constant, e.g., a list of
; the form (QUOTE guts).  We return a term equivalent to (equal x
; const).

  (let ((guts (cadr const)))
    (cond ((symbolp guts)
           (list 'eq x const))
          ((or (acl2-numberp guts)
               (characterp guts))
           (list 'eql x guts))
          ((stringp guts)
           (list 'equal x guts))
          (t (list 'equal x const)))))

(defun match-tests-and-bindings (x pat tests bindings)

; We return two results.  The first is a list of tests, in reverse
; order, that determine whether x matches the structure pat.  We
; describe the language of pat below.  The tests are accumulated onto
; tests, which should be nil initially.  The second result is an alist
; containing entries of the form (sym expr), suitable for use as the
; bindings in the let we generate if the tests are satisfied.  The
; bindings required by pat are accumulated onto bindings and thus are
; reverse order, although their order is actually irrelevant.

; For example, the pattern
;   ('equal ('car ('cons u v)) u)
; matches only first order instances of (EQUAL (CAR (CONS u v)) u).

; The pattern
;   ('equal (ev (simp x) a) (ev x a))
; matches only second order instances of (EQUAL (ev (simp x) a) (ev x a)),
; i.e., ev, simp, x, and a are all bound in the match.

; In general, the match requires that the cons structure of x be isomorphic
; to that of pat, down to the atoms in pat.  Symbols in the pat denote
; variables that match anything and get bound to the structure matched.
; Occurrences of a symbol after the first match only structures equal to
; the binding.  Non-symbolp atoms match themselves.

; There are some exceptions to the general scheme described above.  A
; cons structure starting with QUOTE matches only itself.  The symbols
; nil and t, and all symbols whose symbol-name starts with #\* match
; only structures equal to their values.  (These symbols cannot be
; legally bound in ACL2 anyway, so this exceptional treatment does not
; restrict us further.)  Any symbol starting with #\! matches only the
; value of the symbol whose name is obtained by dropping the #\!.
; This is a way of referring to already bound variables in the
; pattern.  Finally, the symbol & matches anything and causes no
; binding.

  (cond
   ((symbolp pat)
    (cond
     ((or (eq pat t)
          (eq pat nil))
      (mv (cons (list 'eq x pat) tests) bindings))
     ((and (> (length (symbol-name pat)) 0)
           (eql #\* (char (symbol-name pat) 0)))
      (mv (cons (list 'equal x pat) tests) bindings))
     ((and (> (length (symbol-name pat)) 0)
           (eql #\! (char (symbol-name pat) 0)))
      (mv (cons (list 'equal x
                      (intern (coerce (cdr (coerce (symbol-name pat)
                                                   'list))
                                      'string)
                              "ACL2"))
                tests)
          bindings))
     ((eq pat '&) (mv tests bindings))
     (t (let ((binding (assoc-eq pat bindings)))
          (cond ((null binding)
                 (mv tests (cons (list pat x) bindings)))
                (t (mv (cons (list 'equal x (cadr binding)) tests)
                       bindings)))))))
   ((atom pat)
    (mv (cons (equal-x-constant x (list 'quote pat)) tests)
        bindings))
   ((eq (car pat) 'quote)
    (mv (cons (equal-x-constant x pat) tests)
        bindings))
   (t (mv-let (tests1 bindings1)
        (match-tests-and-bindings (list 'car x) (car pat)
                                  (cons (list 'consp x) tests)
                                  bindings)
        (match-tests-and-bindings (list 'cdr x) (cdr pat)
                                  tests1 bindings1)))))

(defun match-clause (x pat forms)
  (mv-let (tests bindings)
    (match-tests-and-bindings x pat nil nil)
    (list (if (null tests)
              t
            (cons 'and (reverse tests)))
          (cons 'let (cons (reverse bindings) forms)))))

(defun match-clause-list (x clauses)
  (cond ((consp clauses)
         (if (eq (caar clauses) '&)
             (list (match-clause x (caar clauses) (cdar clauses)))
           (cons (match-clause x (caar clauses) (cdar clauses))
                 (match-clause-list x (cdr clauses)))))
        (t '((t nil)))))

(defmacro case-match (&rest args)
  (declare (xargs :guard (and (consp args)
                              (symbolp (car args))
                              (alistp (cdr args))
                              (null (cdr (member-equal (assoc-eq '& (cdr args))
                                                       (cdr args)))))))
  ":Doc-Section ACL2::Programming

  pattern matching or destructuring~/
  ~bv[]
  General Form:
  (case-match x
    (pat1 dcl1 body1)
    ...
    (patk dclk bodyk))
  ~ev[]
  where ~c[x] is a variable symbol, the ~c[pati] are structural patterns
  as described below, the ~c[dcli] are optional ~ilc[declare] forms and
  the ~c[bodyi] are terms.  Return the value(s) of the ~c[bodyi]
  corresponding to the first ~c[pati] matching ~c[x], or ~c[nil] if none
  matches.

  Pattern Language:~nl[]
  With the few special exceptions described below, matching requires
  that the ~ilc[cons] structure of ~c[x] be isomorphic to that of the
  pattern, down to the ~il[atom]s in the pattern.  Non-symbol ~il[atom]s in the
  pattern match only themselves.  Symbols in the pattern denote
  variables which match anything and which are bound by a successful
  match to the corresponding substructure of ~c[x].  Variables that
  occur more than once must match the same (~ilc[EQUAL]) structure in
  every occurrence.
  ~bv[]
  Exceptions:
  &               Matches anything and is not bound.  Repeated
                    occurrences of & in a pattern may match different
                    structures.
  nil, t, *sym*   These symbols cannot be bound and match only their
                    global values.
  !sym            where sym is a symbol that is already bound in the
                    context of the case-match, matches only the
                    current binding of sym.
  'obj            Matches only itself.
  ~ev[]
  Some examples are shown below.~/

  Below we show some sample patterns and examples of things they match
  and do not match.
  ~bv[]  
  pattern       matches         non-matches
  (x y y)       (ABC 3 3)       (ABC 3 4)    ; 3 is not 4
  (fn x . rst)  (P (A I) B C)   (ABC)        ; NIL is not (x . rst)
                (J (A I))                    ; rst matches nil
  ('fn (g x) 3) (FN (H 4) 3)    (GN (G X) 3) ; 'fn matches only itself
  (& t & !x)    ((A) T (B) (C))              ; provided x is '(C)
  ~ev[]                                
  Consider the two binary trees that contain three leaves.  They might
  be described as ~c[(x . (y . z))] and ~c[((x . y) . z)], where ~c[x],
  ~c[y], and ~c[z] are atomic.  Suppose we wished to recognize those
  trees.  The following ~c[case-match] would do:
  ~bv[]
  (case-match tree
    ((x . (y . z))
     (and (atom x) (atom y) (atom z)))
    (((x . y) . z)
     (and (atom x) (atom y) (atom z))))
  ~ev[]
  Suppose we wished to recognize such trees where all three tips are
  identical.  Suppose further we wish to return the tip if the tree is
  one of those recognized ones and to return the number ~c[7] otherwise.
  ~bv[]
  (case-match tree
    ((x . (x . x))
     (if (atom x) x 7))
    (((x . x) . x)
     (if (atom x) x 7))
    (& 7))
  ~ev[]
  Note that ~c[case-match] returns ~c[nil] if no ~c[pati] matches.  Thus if we
  must return ~c[7] in that case, we have to add as the final pattern the
  ~c[&], which always matches anything."
  (cons 'cond (match-clause-list (car args) (cdr args))))

; Essay on Evisceration

; We have designed the pretty printer so that it can print an
; "eviscerated" object, that is, an object that has had certain
; substructures removed.  We discuss the prettyprinter in the Essay on
; the ACL2 Prettyprinter.  The pretty printer has a flag, eviscp,
; which indicates whether the object has been eviscerated or not.  If
; not, then the full object is printed as it stands.  If so, then
; certain substructures of it are given special interpretation by the
; printer.  In particular, when the printer encounters a cons of the
; form (:evisceration-mark . x) then x is a string and the cons is
; printed by printing the characters in x (without the double
; gritches).

;     object                            pretty printed output
; (:evisceration-mark . "#")                     #
; (:evisceration-mark . "...")                   ...
; (:evisceration-mark . "<state>")               <state>
; (:evisceration-mark . ":EVISCERATION-MARK")    :EVISCERATION-MARK

; So suppose you have some object and you want to print it, implementing
; the CLTL conventions for *print-level* and *print-length*.  Then you
; must first scan it, inserting :evisceration-mark forms where
; appropriate.  But what if it contains some occurrences of
; :evisceration-mark?  Then you must use evisceration mechanism to print
; them correctly!  Once you have properly eviscerated the object, you can
; call the prettyprinter on it, telling it that the object has been
; eviscerated.  If, on the other hand, you don't want to eviscerate it,
; then you needn't sweep it to protect the native :evisceration-marks:
; just call the prettyprinter with the eviscp flag off.

(defconst *evisceration-mark* :evisceration-mark)

; Note: It is important that the evisceration-mark be a keyword.
; One reason is that (:evisceration-mark . ":EVISCERATION-MARK")
; couldn't be used to print a non-keyword because the package might
; need to be printed.  Another is that we exploit the fact that no
; event name nor any formal is *evisceration-mark*.  See
; print-ldd-full-or-sketch.  Furthermore, if the particular keyword
; chosen is changed, alter *anti-evisceration-mark* below!

(defconst *evisceration-hash-mark* (cons *evisceration-mark* "#"))
(defconst *evisceration-ellipsis-mark* (cons *evisceration-mark* "..."))
(defconst *evisceration-world-mark*
  (cons *evisceration-mark* "<world>"))
(defconst *evisceration-state-mark*
  (cons *evisceration-mark* "<state>"))
(defconst *evisceration-error-triple-marks*
  (list nil nil *evisceration-state-mark*))
(defconst *evisceration-hiding-mark*
  (cons *evisceration-mark* "<hidden>"))

(defconst *anti-evisceration-mark*
  (cons *evisceration-mark* ":EVISCERATION-MARK"))

(defmacro evisceratedp (eviscp x)
; Warning:  The value of x should be a consp.
  `(and ,eviscp (eq (car ,x) *evisceration-mark*)))

; We now define the most elementary eviscerator, the one that implements
; *print-level* and *print-length*.  In this same pass we also arrange to
; hide any object in alist, where alist pairs objects with their
; evisceration strings -- or if not a string, with the appropriate
; evisceration pair.

(mutual-recursion

(defun eviscerate1 (x v max-v max-n alist hiding-cars)
  (let ((temp (assoc-equal x alist)))
    (cond (temp
           (cond ((stringp (cdr temp))
                  (cons *evisceration-mark* (cdr temp)))
                 (t (cdr temp))))
          ((atom x)
           (cond ((eq x *evisceration-mark*) *anti-evisceration-mark*)
                 (t x)))
          ((= v max-v) *evisceration-hash-mark*)
          ((member-eq (car x) hiding-cars) *evisceration-hiding-mark*)
          (t (eviscerate1-lst x (1+ v) 0 max-v max-n alist hiding-cars)))))

(defun eviscerate1-lst (lst v n max-v max-n alist hiding-cars)
  (let ((temp (assoc-equal lst alist)))
    (cond
     (temp
      (cond ((stringp (cdr temp))
             (cons *evisceration-mark* (cdr temp)))
            (t (cdr temp))))
     ((atom lst)
      (cond ((eq lst *evisceration-mark*) *anti-evisceration-mark*)
            (t lst)))
     ((= n max-n) (list *evisceration-ellipsis-mark*))
     (t (cons (eviscerate1 (car lst) v max-v max-n alist hiding-cars)
              (eviscerate1-lst (cdr lst) v (1+ n)
                               max-v max-n alist hiding-cars))))))
)

(mutual-recursion

(defun eviscerate1p (x alist hiding-cars)

; This function returns t iff (eviscerate1 x 0 -1 -1 alist hidep)
; returns something other than x.  That is, iff the evisceration of x
; either uses alist, hiding or the *anti-evisceration-mark* (assuming
; that print-level and print-length never max out).

  (let ((temp (assoc-equal x alist)))
    (cond (temp t)
          ((atom x)
           (cond ((eq x *evisceration-mark*) t)
                 (t nil)))
          ((member-eq (car x) hiding-cars) t)
          (t (eviscerate1p-lst x alist hiding-cars)))))

(defun eviscerate1p-lst (lst alist hiding-cars)
  (let ((temp (assoc-equal lst alist)))
    (cond (temp t)
          ((atom lst)
           (cond ((eq lst *evisceration-mark*) t)
                 (t nil)))
          (t (or (eviscerate1p (car lst) alist hiding-cars)
                 (eviscerate1p-lst (cdr lst) alist hiding-cars))))))
)

(defun eviscerate (x print-level print-length alist hiding-cars)

; Print-level and print-length should either be non-negative integers
; or nil.  Alist is an alist pairing arbitrary objects to strings or
; other objects.  Hiding-cars is a list of symbols.  Any x that starts
; with one of these symbols is printed as <hidden>.  If alist pairs an
; object with a string, the string is printed in place of the object.
; If alist pairs an object with anything else, x, then x is
; substituted for the the object and is treated as eviscerated.

; This function copies the structure x and replaces certain deep
; substructures with evisceration marks.  The determination of which
; substructures to so abbreviate is based on the same algorithm used
; to define *print-level* and *print-length* in CLTL, with the
; additional identification of all occurrences of any object in alist.

; For example, if x is '(if (member x y) (+ (car x) 3) '(foo . b)) and
; print-level is 2 and print-length is 3 then the output is:

; (IF (MEMBER X Y)
;     (+ (*evisceration-mark* . "#") 3)
;     (*evisceration-mark* . "..."))

; See pg 373 of CLTL.

; Of course we are supposed to print this as:

; (IF (MEMBER X Y) (+ # 3) ...)

; We consider a couple of special cases to reduce unnecessary consing
; of eviscerated values.

  (cond ((and (null print-level)
              (null print-length))

; Warning: Observe that even if alist is nil, x might contain the
; *evisceration-mark* or hiding expressions and hence have a
; non-trivial evisceration

         (cond ((eviscerate1p x alist hiding-cars)
                (eviscerate1 x 0 -1 -1 alist hiding-cars))
               (t x)))
        (t (eviscerate1 x 0
                        (or print-level -1)
                        (or print-length -1)
                        alist
                        hiding-cars))))

(defun world-evisceration-alist (state alist)
  (cons (cons (w state) *evisceration-world-mark*)
        alist))

(defun stobj-print-name (name)
  (coerce
   (cons #\<
         (append (string-downcase1 (coerce (symbol-name name) 'list))
                 '(#\>)))
   'string))

(defun evisceration-stobj-mark (name inputp)

; NAME is a stobj name.  We return an evisceration mark that prints as
; ``<name>''.  We make a special case out of STATE.

  (cond
   (inputp name)
   ((eq name 'STATE)
    *evisceration-state-mark*)
   (t
    (cons *evisceration-mark* (stobj-print-name name)))))

(defun evisceration-stobj-marks1 (stobjs-flags inputp)

; See the comment in eviscerate-stobjs, below.

  (cond ((null stobjs-flags) nil)
        ((car stobjs-flags)
         (cons (evisceration-stobj-mark (car stobjs-flags) inputp)
               (evisceration-stobj-marks1 (cdr stobjs-flags) inputp)))
        (t
         (cons nil
               (evisceration-stobj-marks1 (cdr stobjs-flags) inputp)))))

(defun evisceration-stobj-marks (stobjs-flags inputp)
  (cond ((equal stobjs-flags '(nil nil state))
         (if inputp
             '(nil nil state)
           *evisceration-error-triple-marks*))
        ((equal stobjs-flags '(nil)) '(nil))
        (t (evisceration-stobj-marks1 stobjs-flags inputp))))

(defun eviscerate-stobjs1 (estobjs-out lst print-level print-length
                                       alist hiding-cars)
  (cond
   ((null estobjs-out) nil)
   ((car estobjs-out)
    (cons (car estobjs-out)
          (eviscerate-stobjs1 (cdr estobjs-out) (cdr lst)
                              print-level print-length
                              alist hiding-cars)))
   (t
    (cons (eviscerate (car lst) print-level print-length
                      alist hiding-cars)
          (eviscerate-stobjs1 (cdr estobjs-out) (cdr lst)
                              print-level print-length
                              alist hiding-cars)))))
                                                    
(defun eviscerate-stobjs (estobjs-out lst print-level print-length
                                      alist hiding-cars)

; Warning: Right now, we abbreviate all stobjs with the <name>
; convention.  I have toyed with the idea of allowing the user to
; specify how a stobj is to be abbreviated on output.  This is
; awkward.  See the Essay on Abbreviating Live Stobjs below.

; We wish to eviscerate lst with the given print-level, etc., but
; respecting stobjs that we may find in lst.  Estobjs-out describes
; the shape of lst as a multiple value vector: if estobjs-out is of
; length 1, then lst is the single result; otherwise, lst is a list of
; as many elements as estobjs-out is long.  The non-nil elements of
; stobjs name the stobjs in lst -- EXCEPT that unlike an ordinary
; ``stobjs-out'', the elements of estobjs-out are evisceration marks
; we are to ``print!''  For example corresponding to the stobjs-out
; setting of '(NIL $MY-STOBJ NIL STATE) is the estobjs-out

; '(NIL
;   (:EVISCERATION-MARK . "<$my-stobj>")
;   NIL
;   (:EVISCERATION-MARK . "<state>"))

; Here, we assume *evisceration-mark* is :EVISCERATION-MARK.

  (cond
   ((null estobjs-out)

; Lst is either a single non-stobj output or a list of n non-stobj
; outputs.  We eviscerate it without regard for stobjs.

    (eviscerate lst print-level print-length alist hiding-cars))

   ((null (cdr estobjs-out))

; Lst is a single output, which is either a stobj or not depending on
; whether (car stobjs-out) is non-nil.

    (cond
     ((car estobjs-out) (car estobjs-out))
     (t (eviscerate lst print-level print-length alist hiding-cars))))
   

   (t (eviscerate-stobjs1 estobjs-out lst print-level print-length
                          alist hiding-cars))))

; Essay on Abbreviating Live Stobjs

; Right now the live state is abbreviated as <state> when it is
; printed, and the user's live stobj $s is abbreviated as <$s>.  It
; would be cool if the user could specify how he or she wants a stobj
; displayed, e.g., by selecting key components for printing or by
; providing a function which maps the stobj to some non-stobj
; ``stand-in'' or eviscerated object for printing.

; I have given this matter several hours' thought and abandoned it for
; the moment.  I am not convinced it is worth the trouble.  It IS a
; lot of trouble.

; We eviscerate stobjs in two main places, a very low-level place:
; ev-fncall-msg (and its more pervasive friend, ev-fncall-guard-er)
; and in a very high level place: ld-print-results.  The low-level
; place is used to print stobjs involved in calls of functions on args
; that violate a guard.  The high-level place is the ``print'' in the
; read-eval-print loop.

; Every stobj must have some ``stand-in transformer'' function, fn.
; We will typically be holding a stobj name, e.g., $S, and a live
; value, val, e.g., (#(777) #(1 2 3 ...)), and wish to obtain some
; ACL2 object to print in place of the value.  This value is obtained
; by applying fn to val.  The two main issues I see are

; (a) where do we find fn?  The candidate places are state, world, and
; val itself.  But we do not have state available in the low-level
; code.

; (b) how do we apply fn to val?  The obvious thing is to call
; trans-eval or do an ev-fncall.  Again, we need state.  Furthermore,
; depending on how we do it, we have to fight a syntactic battle of
; ``casting'' an arbitrary object, val, to a stobj of type name, to
; apply a function which has a STOBJS-IN of (name).  A more important
; problem is the one of order-of-definition.  Which is defined first:
; how to eviscerate a stobj or how to evaluate a form?  Stobj
; evisceration calls evaluation to apply fn, but evaluation calls
; stobj evisceration to report guard errors.

; Is user-specified stobj abbreviation really worth the trouble?

; One idea that presents itself is that val ``knows how to abbreviate
; itself.''  I think this is akin to the idea of having a :program
; mode function, say stobj-standin, which syntactically takes a
; non-stobj and returns a non-stobj.  Actually, stobj-standin would be
; called on val.  It is clear that I could define this function in raw
; lisp: look in *the-live-state* to determine how to abbreviate val
; and then just do it.  But what would be the logical definition of
; it?  We could leave it undefined, or defined to be an undefined
; function.  Until we admit the whole ACL2 system :logically, we could
; even define it in the logic to be t even though it really returned
; something else, since as a :program its logical definition is
; irrelevant.  But at the moment I don't think ACL2 has a precedent
; for such a function and I don't think user-specified stobj
; abbreviation is justification enough for doing it.


; Now we lay down some macros that help with the efficiency of the FMT
; functions, by making it easy to declare various formals and function
; values to be fixnums.  To the best of our knowledge, the values of
; most-positive-fixnum in various lisps are as follows, so we feel
; safe in using (signed-byte 29) to represent fixnums.  At worst, if a
; lisp is used for which (signed-byte 29) is not a subtype of fixnum,
; a compiler may simply fail to create efficient code.

; Values of most-positive-fixnum.
; AKCL, GCL: 2147483647
; Allegro:    536870911
; Lucid:      536870911
; cmulisp:    536870911
; SBCL:       536870911
; OpenMCL:    536870911
; MCL:        268435455
; CLISP:       16777215
; Lispworks:    8388607 [4.2.0; formerly 536870911]

(defmacro mv-letc (vars form body)
  `(mv-let ,vars ,form
           (declare (type (signed-byte 29) col))
           ,body))

(defmacro fixnum-bound ()
  (1- (expt 2 28)))

(defmacro er-hard-val (val &rest args)

; Use (er-hard-val val ctx str ...) instead of (er hard ctx str ...)
; when there is an expectation on the return type, which should be the
; type of val.  Compilation with the cmulisp compiler produces many
; warnings if we do not use some such device.

  `(prog2$ (er hard ,@args)
           ,val))

(defmacro the-fixnum! (n ctx)

; See also the-half-fixnum!.

  (let ((upper-bound (fixnum-bound)))
    (declare (type (signed-byte 29) upper-bound))
    (let ((lower-bound (- (1+ upper-bound))))
      (declare (type (signed-byte 29) lower-bound))
      `(the-fixnum
        (let ((n ,n))
          (if (and (<= n ,upper-bound)
                   (>= n ,lower-bound))
              n
            (er-hard-val 0 ,ctx
                         "The object ~x0 is not a fixnum ~
                          (precisely:  not a (signed-byte 29))."
                         n)))))))

(defmacro the-half-fixnum! (n ctx)

; Same as the-fixnum!, but leaves some room.

  (let ((upper-bound (floor (fixnum-bound) 2))) ; (1- (expt 2 27))
    (declare (type (signed-byte 28) upper-bound))
    (let ((lower-bound (- (1+ upper-bound))))
      (declare (type (signed-byte 28) lower-bound))
      `(the-fixnum
        (let ((n ,n))
          (if (and (<= n ,upper-bound)
                   (>= n ,lower-bound))
              n
            (er-hard-val 0 ,ctx
                         "The object ~x0 is not a `half-fixnum' ~
                          (precisely:  not a (signed-byte 28))."
                         n)))))))

(defmacro the-string! (s ctx)
  `(if (stringp ,s)
       (the string ,s)
     (er-hard-val "" ,ctx
                  "Not a string:  ~s0."
                  ,s)))

(defun xxxjoin-fixnum (fn args root)

; This is rather like xxxjoin, but we wrap the-fixnum around all
; arguments.

  (declare (xargs :guard (true-listp args)))
  (if (cdr args)
      (list 'the-fixnum
            (list fn
                  (list 'the-fixnum (car args))
                  (xxxjoin-fixnum fn (cdr args) root)))
    (if args ; one arg
        (list 'the-fixnum (car args))
      root)))

(defmacro +f (&rest args)
  (xxxjoin-fixnum '+ args 0))

(defmacro -f (arg1 &optional arg2)
  (if arg2
      `(the-fixnum (- (the-fixnum ,arg1)
                      (the-fixnum ,arg2)))
    `(the-fixnum (- (the-fixnum ,arg1)))))

(defmacro 1-f (x)
  (list 'the-fixnum
        (list '1- (list 'the-fixnum x))))

(defmacro 1+f (x)
  (list 'the-fixnum
        (list '1+ (list 'the-fixnum x))))

(defmacro charf (s i)
  (list 'the 'character
        (list 'char s i)))

; Essay on the ACL2 Prettyprinter

; The ACL2 prettyprinter is a two pass, linear time, exact
; prettyprinter.  By "exact" we mean that if it has a page of width w
; and a big enough form, it will guarantee to use all the columns,
; i.e., the widest line will end in column w.  The algorithm dates
; from about 1971 -- virtually the same code was in the earliest
; Edinburgh Pure Lisp Theorem Prover.  This approach to prettyprinting
; was invented by Bob Boyer.  Most prettyprinters are quadratic and
; inexact.

; The secret to this method is to make two linear passes, ppr1 and
; ppr2.  The first pass builds a data structure, called a ``ppr
; tuple,'' that tells the second pass how to print.

; Some additional general principles of our prettyprinter are
; (i)    Print flat whenever possible.  
; (ii)   However, don't print flat argument lists of length over 40,
;        they're too hard to parse.
; (iii)  Atoms and eviscerated things (which print like atoms,
;        e.g., `<world>') may be printed on a single line.
; (iv)   But parenthesized expressions should not be printed on
;        a line with any other argument (unless the whole form
;        fits on the line).  Thus we may produce:
;        `(foo (bar a) b c d)'
;        and
;        `(foo a b
;              c d)'
;        But we never produce
;        `(foo (bar a) b
;              c d)'
;        preferring instead
;        `(foo (bar a)
;              b c d)'
;        It is our belief that parenthesized expressions are
;        hard to parse and after doing so the eye tends to miss
;        little atoms (like b above) hiding in their shadows.

; To play with ppr we recommend executing this form:

; (ppr2 (ppr1 x (acl2-print-base) 30 0 state t) 0 *standard-co* state t)

; This will prettyprint x on a page of width 30, assuming that
; printing starts in column 0.  To see the ppr tuple that drives the
; printer, just evaluate the inner ppr1 form,
; (ppr1 x (acl2-print-base) 30 0 state nil).

; The following test macro is handy.  A typical call of the macro
; is

#|(test 15 (foo (bar x) (mum :key1 val1 :key2 :val2)))|#

; Note that x is not evaluated.  If you want to evaluate x and ppr
; the value, use 

#|(testfn 10
          (eviscerate `(foo (bar x)
                            (mum :key1 :val1 :key2 :val2)
                            ',(w state))
                      nil nil ; print-level and print-length
                      (world-evisceration-alist state nil)
                      nil)
          state)|#

; Note that x may be eviscerated, i.e., eviscerated objects in x are
; printed in their short form, not literally.

#|(defun testfn (d x state)
    (declare (xargs :mode :program :stobjs (state)))
    (let ((tuple (ppr1 x (acl2-print-base) d 0 state t)))
      (pprogn
       (fms "~%Tuple: ~x0~%Output:~%" (list (cons #\0 tuple))
            *standard-co* state nil)
       (ppr2 tuple 0 *standard-co* state t)
       (fms "~%" nil *standard-co* state nil))))


  (defmacro test (d x)
    `(testfn ,d ',x state))|#

; Ppr tuples record enough information about the widths of various
; forms so that it can be computed without having to recompute any
; part of it and so that the second pass can print without having to
; count characters.

; A ppr tuple has the form (token n . z).  In the display below, the
; variables ti represent ppr tuples and the variables xi represent
; objects to be printed directly.  Any xi could an eviscerated object,
; a list whose car is the evisceration mark.

; (FLAT n x1 ... xk) - Print the xi, separated by spaces, all on one
;                      line. The total width of output will be n.
;                      Note that k >= 1.  Note also that such a FLAT
;                      represents k objects.  A special case is (FLAT
;                      n x1), which represents one object.  We make
;                      this observation because sometimes (in
;                      cons-ppr1) we `just know' that k=1 and the
;                      reason is: we know the FLAT we're holding
;                      represents a single object.

; (FLAT n x1... . xk)- Print the xi, separated by spaces, with xk
;                      separated by `. ', all on one line.  Here xk
;                      is at atom or an eviscerated object.  

; (FLAT n . xk)      - Here, xk is an atom (or an eviscerated object).
;                      Print a dot, a space, and xk.  The width will
;                      be n.  Note that this FLAT does not actually
;                      represent an object.  That is, no Lisp object
;                      prints as `. xk'.

; Note: All three forms of FLAT are really just (FLAT n . x) where x
; is a possibly improper list and the elements of x (and its final
; cdr) are printed, separated appropriately by spaces or dot.

; (MATCHED-KEYWORD n x1)
;                    - Exactly like (FLAT n x1), i.e., prints x1,
;                      but by virtue of being different from FLAT
;                      no other xi's are ever added.  In this tuple,
;                      x1 is always a keyword and it will appear on
;                      a line by itself.  It's associated value will
;                      appear below it in the column because we tried
;                      to put them on the same line but we did not have
;                      room.

; (DOT 1)            - Print a dot.

; (QUOTE n . t1)     - Print a single-quote followed by pretty-
;                      printing the ppr tuple t1.

; (WIDE n t1 t2 ...) - Here, t1 is a FLAT tuple of width j.  We 
;                      print an open parent, the contents of t1, a
;                      space, and then we prettyprint each of the
;                      remaining ti in a column.  When we're done, we
;                      print a close paren.  The width of the longest
;                      line we will print is n.

; (i n t1 ...)       - We print an open paren, prettyprint t1, then
;                      do a newline.  Then we prettyprint the
;                      remaining ti in the column that is i to the
;                      right of the paren.  We conclude with a close
;                      paren.  The width of the longest line we will
;                      print is n.  We call this an `indent tuple'.

; (KEYPAIR n t1 . t2)- Here, t1 is a FLAT tuple of width j.  We print
;                      t1, a space, and then prettyprint t2.  The
;                      length of the longest line we will print is n.

; The sentences "The length of the longest line we will print is n."
; bears explanation.  Consider

; (FOO (BAR X)
;      (MUMBLE Y)
;      Z)
;|<- 15 chars  ->|
; 123456789012345

; The length of the longest line, n, is 15.  That is, the length of
; the longest line counts the spaces from the start of the printing.
; In the case of a KEYPAIR tuple:

; :KEY (FOO
;       (BAR X)
;       Y)
;|<- 13      ->|

; we count the spaces from the beginning of the keyword.  That is,
; we consider the whole block of text. 

; Below we print test-term in two different widths, and display
; the ppr tuple that drives each of the two printings.

#|
(assign test-term
        '(FFF (GGG (HHH (QUOTE (A . B))))
              (III YYY ZZZ)))
      

(ppr2 (ppr1 (@ test-term) (acl2-print-base) 30 0 state nil) 0 *standard-co*
      state nil)
; =>
(FFF (GGG (HHH '(A . B)))          (WIDE 25 (FLAT 3 FFF)                    
     (III YYY ZZZ))                         (FLAT 20 (GGG (HHH '(A . B))))
                                            (FLAT 14 (III YYY ZZZ)))      
<-          25         ->|

(ppr2 (ppr1 (@ test-term) (acl2-print-base) 20 0 state nil) 0 *standard-co*
      state nil)
; =>
(FFF                               (1 20 (FLAT 3 FFF)          
 (GGG                                    (4 19 (FLAT 3 GGG)            
     (HHH '(A . B)))                           (FLAT 15 (HHH '(A . B))))
 (III YYY ZZZ))                          (FLAT 14 (III YYY ZZZ)))    

<-       20       ->|                    
|#

; The function cons-ppr1, below, is the first interesting function in
; the nest.  We want to build a tuple to print a given list form, like
; a function call.  We basically get the tuple for the car and a list
; of tuples for the cdr and then use cons-ppr1 to combine them.  The
; resulting list of tuples will be embedded in either a WIDE or an
; indent tuple.  Thus, this list of tuples we will create describes a
; column of forms.  The number of items in that column is not necessarily
; the same as the number of arguments of the function call.  For
; example, the term (f a b c) might be prettyprinted as
; (f a
;    b c)
; where b and c are printed flat on a single line.  Thus, the
; three arguments of f end up being described by a list of two
; tuples, one for a and another for b and c.

; To form lists of tuples we just use cons-ppr1 to combine the tuples
; we get for each element.

; Let x and lst be, respectively, a ppr tuple for an element and a
; list of tuples for list of elements.  Think of lst as describing a
; column of forms.  Either x can become another item that column, or
; else x can be incorporated into the first item in that column.  For
; example, suppose x will print as X and lst will print as a column
; containing y1, y2, etc.  Then we have this choice for printing x and
; lst:

; lengthened column          lengthened first row
; x                          x y1
; y1                         y2
; ...                        ...

; We get the `lengthened column' behavior if we just cons x onto lst.
; We get the `lengthened row' behavior if we merge the tuples for x
; and y1.  But we only merge if they both print flat.

; Essay on the Printing of Dotted Pairs and

; It is instructive to realize that we print a dotted pair as though
; it were a list of length 3 and the dot was just a normal argument.

; In the little table below I show, for various values of d, two
; things: the characters output by

; (ppr2 (ppr1 `(xx . yy) (acl2-print-base) d 0 state nil) 0 *standard-co* state
;       nil)

; and the ppr tuple produced by the ppr1 call.
;        
; d         output                 ppr tuple

;        |<-  9  ->|

; 9       (XX . YY)              (FLAT 9 (XX . YY))

; 8       (XX                    (3 8 (FLAT 2 XX) (FLAT 5 . YY))
;            . YY)

; 7       (XX                    (2 7 (FLAT 2 XX) (FLAT 5 . YY))
;           . YY)

; 6       (XX                    (1 6 (FLAT 2 XX) (FLAT 5 . YY))
;          . YY)

; 5       (XX                    (2 5 (FLAT 2 XX) (DOT 1) (FLAT 3 YY))
;           .
;           YY) 

; 4       (XX                    (1 4 (FLAT 2 XX) (DOT 1) (FLAT 3 YY))
;          .
;          YY)

; The fact that the dot is not necessarily connected to (on the same
; line as) the atom following it is the reason we have the (DOT 1)
; tuple.  We have to represent the dot so that its placement is first
; class.  So when we're assembling the tuple for a list, we cdr down
; the list using cons-ppr1 to put together the tuple for the car with
; the tuple for the cdr.  If we reach a non-nil cdr, atm, we call
; cons-ppr1 on the dot tuple and the tuple representing the atm.
; Depending on the width we have, this may produce (FLAT n . atm)
; which attaches the dot to the atm, or ((DOT 1) (FLAT n atm)) which
; leaves the dot on a line by itself.

; We want keywords to appear on new lines.  That means if the first
; element of lst is a keyword, don't merge (unless x is one too).

#|BUG
ACL2 p!>(let ((x '(foo bigggggggggggggggg . :littlllllllllllllle)))
         (ppr2 (ppr1 x (acl2-print-base) 40 0 state nil)
               0 *standard-co* state nil))
(x   = (DOT 1)
lst = ((FLAT 21 :LITTLLLLLLLLLLLLLLE))
val = ((FLAT 23 . :LITTLLLLLLLLLLLLLLE)))

HARD ACL2 ERROR in CONS-PPR1:  I thought I could force it!
|#

; Note: In the function below, column is NOT a number!  Often in this
; code, ``col'' is used to represent the position of the character
; column into which we are printing.  But ``column'' is a list of ppr
; tuples.

(defun keyword-param-valuep (tuple eviscp)

; We return t iff tuple represents a single object that could
; plausibly be the value of a keyword parameter.  The (or i ii iii iv)
; below checks that tuple represents a single object, either by being
; (i) a FLAT tuple listing exactly one object (ii) a QUOTE tuple,
; (iii) a WIDE tuple, or (iv) an indent tuple.  The only other kinds
; of tuples are KEYPAIR tuples, FLAT tuples representing dotted
; objects `. atm', FLAT tuples representing several objects `a b c',
; and MATCHED-KEYWORD tuples representing keywords whose associated
; values are on the next line.  These wouldn't be provided as the
; value of a keyword argument.

  (or (and (eq (car tuple) 'flat)
           (not (or (atom (cddr tuple)) ; tuple is `. atm'
                    (evisceratedp eviscp (cddr tuple))))
           (null (cdr (cddr tuple))))
      (eq (car tuple) 'quote)
      (eq (car tuple) 'wide)
      (integerp (car tuple))))
  
(defun cons-ppr1 (x column width eviscp)

; Here, x is a ppr tuple representing either a dot or a single object
; and column is a list of tuples corresponding to a list of objects
; (possibly a list of length greater than that of column).
; Intuitively, column will print as a column of objects and we want to
; add x to that column, either by extending the top row or adding a
; new row.  In the most typical case, x might be (FLAT 3 ABC) and
; column is ((FLAT 7 DEF GHI) (...)).  Thus our choices would be to
; produce

; lengthened column          lengthened first row
; ABC                        ABC DEF GHI
; DEF GHI                    (...)
; (...)

; It is also here that we deal specially with keywords.  If x is
; (FLAT 3 :ABC) and column is ((...) (...)) then we have the choice:

; lengthened column          lengthened first row
; :ABC                       :ABC (...)
; (...)                      (...)
; (...)

; The default behavior is always to lengthen the column, which is just
; to cons x onto column.

  (cond
   ((and (eq (car x) 'flat)

; Note: Since x represents a dot or an object, we know that it is not
; of the form (FLAT n . atm).  Thus, (cddr x) is a list of length 1
; containing a single (possibly eviscerated) object, x1.  If that
; object is an atom (or prints like one) we'll consider merging it
; with whatever else is on the first row. 

         (or (atom (car (cddr x)))
             (evisceratedp eviscp (car (cddr x))))
         (consp column))

    (let ((x1 (car (cddr x)))
          (row1 (car column)))

; We know x represents the atom x1 (actually, x1 may be an eviscerated
; object, but if so it prints flat like an atom, e.g., `<world>').
; Furthermore, we know column is non-empty and so has a first element,
; e.g., row1.

      (cond
       ((keywordp x1)

; So x1 is a keyword.  Are we looking at a keypair?  We are if row1
; represents a single value.  By a ``single value'' we mean a single
; object that can be taken as the value of the keyword x1.  If row1
; represents a sequence of more than one object, e.g., (FLAT 5 a b c),
; then we are not in a keypair situation because keyword argument
; lists must be keyword/value pairs all the way down and we form these
; columns bottom up, so if b were a keyword in the proper context, we
; would have paired it with c as keypair, not merged it, or we would
; have put it in a MATCHED-KEYWORD, indicating that its associated
; value is below it in the column.  If row1 does not represent a
; single value we act just like x1 had not been a keyword, i.e., we
; try to merge it with row1.  This will shut down subsequent attempts
; to create keypairs above us.

        (cond
         ((and (keyword-param-valuep row1 eviscp)
               (or (null (cdr column))
                   (eq (car (cadr column)) 'keypair)
                   (eq (car (cadr column)) 'matched-keyword)))

; So x1 is a keyword, row1 represents a keyword parameter value, and
; the rest of the column represents keyword/value pairs.  The last
; test is made by just checking the item on the column below row1.  It
; would only be a keyword/value pair if the whole column consisted of
; those.  We consider making a keypair of width n = width of key, plus
; space, plus width of widest line in row1.  Note that we don't mind
; this running over the standard 40 character max line length because
; it is so iconic.

          (let ((n (+ (cadr x) (+ 1 (cadr row1)))))
            (cond ((<= n width)
                   (cons
                    (cons 'keypair (cons n (cons x row1)))
                    (cdr column)))

; Otherwise, we put x on a newline and leave the column as it was.
; Note that we convert x from a FLAT to a MATCHED-KEYWORD, so insure
; that it stays on a line by itself and to keyword/value pairs
; encountered above us in the bottom-up processing to be paired
; with KEYPAIR.

                  (t (cons (cons 'MATCHED-KEYWORD (cdr x))
                           column)))))

; In this case, we are not in the context of a keyword/value argument
; even though x is a keyword.  So we act just like x is not a keyword
; and see whether we can merge it with row1.  We merge only if row1 is
; FLAT already and the width of the merged row is acceptable.  Even if
; row1 prints as `. atm' we will merge, giving rise to such displays
; as

; (foo a b c
;      d e f . atm)

         ((eq (car row1) 'flat)
          (let ((n (+ (cadr x) (+ 1 (cadr row1)))))
            (cond ((and (<= n 40) (<= n width))
                   (cons
                    (cons 'flat (cons n (cons x1 (cddr row1))))
                    (cdr column)))
                  (t (cons x column)))))
         (t (cons x column))))

; In this case, x1 is not a keyword.  But it is known to print in
; atom-like way, e.g., `ABC' or `<world>'.  So we try a simple merge
; following the same scheme as above.

       ((eq (car row1) 'flat)
        (let ((n (+ (cadr x) (+ 1 (cadr row1)))))
          (cond ((and (<= n 40) (<= n width))
                 (cons
                  (cons 'flat (cons n (cons x1 (cddr row1))))
                  (cdr column)))
                (t (cons x column)))))
       (t (cons x column)))))
   ((and (eq (car x) 'dot)
         (consp column))
    (let ((row1 (car column)))
      (cond ((eq (car row1) 'flat)

; In this case we know (car (cddr row1)) is an atom (or an eviscerated
; object) and it becomes the cddr of the car of the answer, which puts
; the dot on the same line as the terminal cdr.

             (let ((n (+ (cadr x) (+ 1 (cadr row1)))))
               (cond ((and (<= n 40) (<= n width))
                      (cons
                       (cons 'flat
                             (cons n (car (cddr row1))))
                       (cdr column)))
                     (t (cons x column)))))
            (t (cons x column)))))

; In this case, x1 does not print flat.  So we add a new row.

   (t (cons x column))))

(defun flsz-integer (x print-base acc)
  (declare (type (unsigned-byte 5) print-base)
           (type (signed-byte 29) acc)
           (xargs :guard (print-base-p print-base)))
  (the-fixnum
   (cond ((< x 0)
          (flsz-integer (- x) print-base (1+f acc)))
         ((< x print-base) (1+f acc))
         (t (flsz-integer (truncate x print-base) print-base (1+f acc))))))

(defun flsz-atom (x print-base acc state)
  (declare (type (unsigned-byte 5) print-base)
           (type (signed-byte 29) acc))
  (the-fixnum
   (cond ((> acc (the (signed-byte 29) 100000))

; In order to make it very simple to guarantee that flsz and flsz-atom
; return fixnums, we ensure that acc is small enough below.  We could
; certainly provide a much more generous bound, but 100,000 seems safe
; at the moment!

          100000)
         ((integerp x)
          (flsz-integer x
                        print-base
                        (cond ((int= print-base 10)
                               acc)
                              (t ; #b, #o, or #x prefix
                               (+f 2 acc)))))
         ((symbolp x)

; For symbols we add together the length of the "package part" and the
; symbol name part.  We include the colons in the package part.

          (+f (cond
               ((keywordp x) (1+f acc))
               ((or (equal (symbol-package-name x)
                           (f-get-global 'current-package state))
                    (member-eq
                     x
                     (package-entry-imports
                      (find-package-entry
                       (f-get-global 'current-package state)
                       (known-package-alist state)))))
                acc)
               (t
                (let ((p (symbol-package-name x)))
                  (cond ((may-need-slashes p)
                         (+f 4 acc (the-half-fixnum! (length p)
                                                     'flsz-atom)))
                        (t (+f 2 acc (the-half-fixnum! (length p)
                                                       'flsz-atom)))))))
              (let ((s (symbol-name x)))
                 (cond ((may-need-slashes s)
                        (+f 2 (the-half-fixnum! (length s) 'flsz-atom)))
                       (t (+f (the-half-fixnum! (length s) 'flsz-atom)))))))
         ((rationalp x)
          (flsz-integer (numerator x)
                        print-base
                        (flsz-integer (denominator x)
                                      print-base
                                      (cond ((int= print-base 10)
                                             (1+f acc))
                                            (t ; #b, #o, or #x prefix
                                             (+f 3 acc))))))
         ((complex-rationalp x)
          (flsz-atom (realpart x)
                     print-base
                     (flsz-atom (imagpart x)
                                print-base
                                (cond ((int= print-base 10)
                                       (+f 5 acc))
                                      (t
; Compensate for adding twice for #b, #o, or #x prefix.
                                       (+f 3 acc)))
                                state)
                     state))
         ((stringp x)
          (+f 2 acc (the-half-fixnum! (length x) 'flsz-atom)))
         ((characterp x)
          (+f acc
              (cond ((eql x #\Newline) 9)
                    ((eql x #\Rubout) 8)
                    ((eql x #\Space) 7)
                    ((eql x #\Page) 6)
                    ((eql x #\Tab) 5)
                    (t 3))))
         (t 0))))

(defun flsz1 (x print-base j maximum state eviscp)

; Actually, maximum should be of type (signed-byte 28).

  (declare (type (unsigned-byte 5) print-base)
           (type (signed-byte 29) j maximum))
  (the-fixnum
   (cond ((> j maximum) j)
         ((atom x) (flsz-atom x print-base j state))
         ((evisceratedp eviscp x)
          (+f j (the-half-fixnum! (length (cdr x)) 'flsz)))
         ((atom (cdr x))
          (cond ((null (cdr x))
                 (flsz1 (car x) print-base (+f 2 j) maximum state eviscp))
                (t (flsz1 (cdr x)
                          print-base
                          (flsz1 (car x) print-base (+f 5 j) maximum state
                                 eviscp)
                          maximum state eviscp))))
         ((and (eq (car x) 'quote)
               (consp (cdr x))
               (null (cddr x)))
          (flsz1 (cadr x) print-base (+f 1 j) maximum state eviscp))
         (t (flsz1 (cdr x)
                   print-base
                   (flsz1 (car x) print-base (+f 1 j) maximum state eviscp)
                   maximum state eviscp)))))

(defun output-in-infixp (state)
  (let ((infixp (f-get-global 'infixp state)))
    (or (eq infixp t) (eq infixp :out))))

#-acl2-loop-only
(defun-one-output flatsize-infix (x print-base termp j max eviscp)

; Suppose that printing x flat in infix notation causes k characters
; to come out.  Then we return j+k.  All answers greater than max are
; equivalent.

; If you think of j as the column into which you start printing flat,
; then this returns the column you'll print into after printing x.  If
; that column exceeds max, which is the right margin, then it doesn't
; matter by how far it exceeds max.

; In our $ infix notation, flat output has two extra chars in it, the
; $ and space.  But note that we use infix output only if infixp is t
; or :out.

  (declare (ignore termp))
  (+ 2 (flsz1 x print-base j max *the-live-state* eviscp)))

(defun flsz (x termp j maximum state eviscp)
  #+acl2-loop-only
  (declare (ignore termp))
  #-acl2-loop-only
  (cond ((and (live-state-p state)
              (output-in-infixp state))
         (return-from flsz (flatsize-infix x (acl2-print-base) termp j maximum
                                           eviscp))))
  (flsz1 x (acl2-print-base) j maximum state eviscp))

(defun max-width (lst maximum)
  (cond ((null lst) maximum)
        ((> (cadr (car lst)) maximum)
         (max-width (cdr lst) (cadr (car lst))))
        (t (max-width (cdr lst) maximum))))

(mutual-recursion

(defun ppr1 (x print-base width rpc state eviscp)

; We create a ppr tuple for x, i.e., a list structure that tells us
; how to prettyprint x, in a column of the given width.  Rpc stands
; for `right paren count' and is the number of right parens that will
; follow the printed version of x.  For example, in printing the x in
; (f (g (h x)) u) there will always be 2 right parens after it.  So we
; cannot let x use the entire available width, only the width-2.  Rpc
; would be 2.  Eviscp indicates whether we are to think of evisc marks
; as printing as atom-like strings or whether they're just themselves
; as data.

  (declare (type (signed-byte 29) print-base width rpc))
  (let ((sz (flsz1 x print-base rpc width state eviscp)))
    (declare (type (signed-byte 29) sz))
    (cond ((or (atom x)
               (evisceratedp eviscp x)
               (and (<= sz width)
                    (<= sz 40)))
           (cons 'flat (cons sz (list x))))
          ((and (eq (car x) 'quote)
                (consp (cdr x))
                (null (cddr x)))
           (let* ((x1 (ppr1 (cadr x) print-base (+f width -1) rpc state
                            eviscp)))
             (cons 'quote (cons (+ 1 (cadr x1)) x1))))
          (t
           (let* ((x1 (ppr1 (car x) print-base (+f width -1)
                            (the-fixnum (if (null (cdr x)) (+ rpc 1) 0))
                            state eviscp))

; If the fn is a symbol (or eviscerated, which we treat as a symbol),
; then the hd-sz is the length of the symbol.  Else, hd-sz is nil.
; Think of (null hd-sz) as meaning "fn is a lambda expession".

                  (hd-sz (cond ((or (atom (car x))
                                    (evisceratedp eviscp (car x)))
                                (cadr x1))
                               (t nil)))

; When printing the cdr of x, give each argument the full width (minus
; 1 for the minimal amount of indenting).  Note that x2 contains the
; ppr tuples for the car and the cdr.

                  (x2 (cons x1
                            (ppr1-lst (cdr x) print-base (+f width -1)
                                      (+f rpc 1) state eviscp)))

; If the fn is a symbol, then we get the maximum width of any single
; argument.  Otherwise, we get the maximum width of the fn and its
; arguments.

                  (maximum (cond (hd-sz (max-width (cdr x2) -1))
                                 (t (max-width x2 -1)))))

             (cond ((null hd-sz)

; If the fn is lambda, we indent the args by 1 and report the width of
; the whole to be one more than the maximum computed above.

                    (cons 1 (cons (+ 1 maximum) x2)))
                   ((<= (+ hd-sz (+ 2 maximum)) width)

; We can print WIDE if we have room for an open paren, the fn, a space,
; and the widest argument.

                    (cons 'wide
                          (cons (+ hd-sz (+ 2 maximum)) x2)))
                   ((< maximum width)

; If the maximum is less than the width, we can do exact indenting of
; the arguments to make the widest argument come out on the right
; margin.  This exactness property is one of the things that makes
; this algorithm produce such beautiful output: we get the largest
; possible indentation, which makes it easy to identify peer
; arguments.  How much do we indent?  width-maximum will guarantee
; that the widest argument ends on the right margin.  However, we
; believe that it is more pleasing if argument columns occur at
; regular indents.  So we limit our indenting to 5 and just give up
; the white space over on the right margin.  Note that we compute the
; width of the whole term accordingly.

                    (cons (min 5 (+ width (- maximum)))
                          (cons (+ maximum (min 5 (+ width (- maximum))))
                                x2)))

; If maximum is not less than width, we indent by 1.

                   (t (cons 1 (cons (+ 1 maximum) x2)))))))))


; The next function computes a ppr tuple for each element of lst.
; Typically these are all arguments to a function.  But of course, we
; prettyprint arbitrary constants and so have to handle the case that
; the list is not a true-list.

; If you haven't read about cons-ppr1, above, do so now.

(defun ppr1-lst (lst print-base width rpc state eviscp)

  (declare (type (signed-byte 29) print-base width rpc))
  (cond ((atom lst)

; If the list is empty and null, then nothing is printed (besides the
; parens which are being accounted for otherwise).  If the list is
; terminated by some non-nil atom, we will print a dot and the atom.
; We do that by merging a dot tuple into the flat for the atom, if
; there's room on the line, using cons-ppr1.  Where this merged flat
; will go, i.e., will it be indented under the car as happens in the
; Essay on the Printing of Dotted Pairs, is the concern of ppr1-lst,
; not the cons-ppr1.  The cons-ppr1 below just produces a merged
; flat containing the dot, if the width permits.

         (cond ((null lst) nil)
               (t (cons-ppr1 '(dot 1)
                             (list (ppr1 lst print-base width rpc state
                                         eviscp))
                             width eviscp))))

; The case for an eviscerated terminal cdr is handled the same way.

        ((evisceratedp eviscp lst)
         (cons-ppr1 '(dot 1)
                    (list (ppr1 lst print-base width rpc state eviscp))
                    width eviscp))

; If the list is a true singleton, we just use ppr1 and we pass it the
; rpc that was passed in because this last item will be followed by
; that many parens on the same line.

        ((null (cdr lst))
         (list (ppr1 (car lst) print-base width rpc state eviscp)))

; Otherwise, we know that the car is followed by more elements.  So
; its rpc is 0.

        (t (cons-ppr1 (ppr1 (car lst) print-base width 0 state eviscp)
                      (ppr1-lst (cdr lst) print-base width rpc state eviscp)
                      width eviscp))))

)

(defun newline (channel state)
  (princ$ #\Newline channel state))

(defun fmt-hard-right-margin (state)
  (the-fixnum
   (f-get-global 'fmt-hard-right-margin state)))

(defun fmt-soft-right-margin (state)
  (the-fixnum
   (f-get-global 'fmt-soft-right-margin state)))

(defun set-fmt-hard-right-margin (n state)
  (cond
   ((and (integerp n)
         (< 0 n))
    (f-put-global 'fmt-hard-right-margin
                  (the-half-fixnum! n 'set-fmt-hard-right-margin)
                  state))
   (t (let ((err (er hard 'set-fmt-hard-right-margin
                     "The fmt-hard-right-margin must be a positive ~
                      integer, but ~x0 is not."
                     n)))
        (declare (ignore err))
        state))))

(defun set-fmt-soft-right-margin (n state)
  (cond
   ((and (integerp n)
         (< 0 n))
    (f-put-global 'fmt-soft-right-margin
                  (the-half-fixnum! n 'set-fmt-soft-right-margin)
                  state))
   (t (let ((err (er hard 'set-fmt-soft-right-margin
                     "The fmt-soft-right-margin must be a positive ~
                      integer, but ~x0 is not."
                     n)))
        (declare (ignore err))
        state))))

(defun write-for-read (state)
  (and (f-boundp-global 'write-for-read state)
       (f-get-global 'write-for-read state)))

(defun spaces1 (n col hard-right-margin channel state)
  (declare (type (signed-byte 29) n col hard-right-margin))
  (cond ((<= n 0) state)
        ((> col hard-right-margin)
         (pprogn (if (write-for-read state)
                     state
                   (princ$ #\\ channel state))
                 (newline channel state)
                 (spaces1 n 0 hard-right-margin channel state)))
        (t (pprogn (princ$ #\Space channel state)
                   (spaces1 (1-f n) (1+f col) hard-right-margin channel
                            state)))))

; The use of *acl2-built-in-spaces-array* to circumvent the call to
; spaces1 under spaces has saved about 25% in GCL and a little more
; than 50% in Allegro.

(defun make-spaces-array-rec (n acc)
  (if (zp n)
      (cons (cons 0 "") acc)
    (make-spaces-array-rec
     (1- n)
     (cons
      (cons n
            (coerce (make-list n :initial-element #\Space) 'string))
      acc))))

(defun make-spaces-array (n)
  (compress1
   'acl2-built-in-spaces-array
   (cons `(:HEADER :DIMENSIONS (,(1+ n))
                   :MAXIMUM-LENGTH ,(+ 2 n)
                   :DEFAULT nil ; should be ignored
                   :NAME acl2-built-in-spaces-array)
         (make-spaces-array-rec n nil))))

(defconst *acl2-built-in-spaces-array*

; Keep the 200 below in sync with the code in spaces.

  (make-spaces-array 200))

(defun spaces (n col channel state)
  (declare (type (signed-byte 29) n col))
  (let ((hard-right-margin (fmt-hard-right-margin state))
        (result-col (+f n col)))
    (declare (type (signed-byte 29) hard-right-margin result-col))
    (if (and (<= result-col hard-right-margin)

; Keep the 200 below in sync with the code in
; *acl2-built-in-spaces-array*.

             (<= n 200))
        ;; actually (1+ hard-right-margin) would do
        (princ$ (aref1 'acl2-built-in-spaces-array
                       *acl2-built-in-spaces-array*
                       n)
                channel state)
      (spaces1 (the-fixnum! n 'spaces)
               (the-fixnum col)
               hard-right-margin
               channel state))))

(mutual-recursion

(defun flpr1 (x channel state eviscp)
  (cond ((atom x)
         (prin1$ x channel state))
        ((evisceratedp eviscp x)
         (princ$ (cdr x) channel state))
        ((and (eq (car x) 'quote)
              (consp (cdr x))
              (null (cddr x)))
         (pprogn (princ$ #\' channel state)
                 (flpr1 (cadr x) channel state eviscp)))
        (t (pprogn (princ$ #\( channel state)
                   (flpr11 x channel state eviscp)))))

(defun flpr11 (x channel state eviscp)
  (pprogn
   (flpr1 (car x) channel state eviscp)
   (cond ((null (cdr x)) (princ$ #\) channel state))
         ((or (atom (cdr x))
              (evisceratedp eviscp (cdr x)))
          (pprogn
           (princ$ " . " channel state)
           (flpr1 (cdr x) channel state eviscp)
           (princ$ #\) channel state)))
         (t (pprogn
             (princ$ #\Space channel state)
             (flpr11 (cdr x) channel state eviscp))))))

)

#-acl2-loop-only
(defun-one-output print-flat-infix (x termp file eviscp)

; Print x flat (without terpri's) in infix notation to the open output
; stream file.  Give special treatment to :evisceration-mark iff
; eviscp.  We only call this function if flatsize-infix assures us
; that x will fit on the line.  See the Essay on Evisceration in this
; file to details on that subject.

  (declare (ignore termp eviscp))
  (let ((*print-case* :downcase)
        (*print-pretty* nil))
    (princ "$ " file)
    (prin1 x file)))

(defun flpr (x termp channel state eviscp)
  #+acl2-loop-only
  (declare (ignore termp))
  #-acl2-loop-only
  (cond ((and (live-state-p state)
              (output-in-infixp state))
         (print-flat-infix x termp
                           (get-output-stream-from-channel channel)
                           eviscp)
         (return-from flpr *the-live-state*)))
  (flpr1 x channel state eviscp))

(defun ppr2-flat (x channel state eviscp)

; We print the elements of x, separated by spaces.  If
; x is a non-nil atom, we print a dot and then x.

  (cond ((null x) state)
        ((or (atom x)
             (evisceratedp eviscp x))
         (pprogn (princ$ #\. channel state)
                 (princ$ #\Space channel state)
                 (flpr1 x channel state eviscp)))
        (t (pprogn
            (flpr1 (car x) channel state eviscp)
            (cond ((cdr x)
                   (pprogn (princ$ #\Space channel state)
                           (ppr2-flat (cdr x) channel state eviscp)))
                  (t state))))))

(mutual-recursion

(defun ppr2-column (lst loc col channel state eviscp)

; We print the elements of lst in a column.  The column number is col
; and we assume the print head is currently in column loc, loc <= col.
; Thus, to indent to col we print col-loc spaces.  After every element
; of lst but the last, we print a newline.

  (cond ((null lst) state)
        (t (pprogn
            (spaces (+ col (- loc)) loc channel state)
            (ppr2 (car lst) col channel state eviscp)
            (cond ((null (cdr lst)) state)
                  (t (pprogn
                      (newline channel state)
                      (ppr2-column (cdr lst) 0 col
                                   channel state eviscp))))))))

(defun ppr2 (x col channel state eviscp)

; We interpret the ppr tuple x.

  (case
    (car x)
    (flat (ppr2-flat (cddr x) channel state eviscp))
    (matched-keyword
     (ppr2-flat (cddr x) channel state eviscp)) ; just like flat!
    (dot (princ$ #\. channel state))
    (quote (pprogn (princ$ #\' channel state)
                   (ppr2 (cddr x) (+ 1 col) channel state eviscp)))
    (keypair (pprogn
              (ppr2-flat (cddr (car (cddr x))) channel state eviscp)
              (princ$ #\Space channel state)
              (ppr2 (cdr (cddr x))
                    (+ col (+ 1 (cadr (car (cddr x)))))
                    channel state eviscp)))
    (wide (pprogn
           (princ$ #\( channel state)
           (ppr2-flat (cddr (car (cddr x))) channel state eviscp)
           (ppr2-column (cdr (cddr x))
                        (+ col (+ 1 (cadr (car (cddr x)))))
                        (+ col (+ 2 (cadr (car (cddr x)))))
                        channel state eviscp)
           (princ$ #\) channel state)))
    (otherwise (pprogn
                (princ$ #\( channel state)
                (ppr2 (car (cddr x)) (+ col (car x)) channel
                      state eviscp)
                (cond ((cdr (cddr x))
                       (pprogn
                        (newline channel state)
                        (ppr2-column (cdr (cddr x))
                                     0
                                     (+ col (car x))
                                     channel state eviscp)
                        (princ$ #\) channel state)))
                      (t (princ$ #\) channel state)))))))
)

; We used to set *fmt-ppr-indentation* below to 5, but it the
; indentation was sometimes odd because when printing a list, some
; elements could be indented and others not.  At any rate, it should
; be less than the fmt-hard-right-margin in order to preserve the
; invariant that fmt0 is called on columns that do not exceed this
; value.

(defconst *fmt-ppr-indentation* 0)

(defun ppr (x col channel state eviscp)

; If eviscp is nil, then we pretty print x as given.  Otherwise,
; x has been eviscerated and we give special importance to the
; *evisceration-mark*.  NOTE WELL:  This function does not
; eviscerate -- it assumes the evisceration has been done if
; needed.

  (declare (type (signed-byte 29) col))
  (let ((fmt-hard-right-margin (fmt-hard-right-margin state)))
    (declare (type (signed-byte 29) fmt-hard-right-margin))
    (cond
     ((< col fmt-hard-right-margin)
      (ppr2 (ppr1 x (acl2-print-base) (+f fmt-hard-right-margin (-f col)) 0
                  state eviscp)
            col channel state eviscp))
     (t (let ((er
               (er hard 'ppr
                   "The `col' argument to ppr must be less than value ~
                    of the state global variable ~
                    fmt-hard-right-margin, but ~x0 is not less than ~
                    ~x1."
                   col fmt-hard-right-margin)))
          (declare (ignore er))
          state)))))

(defun scan-past-whitespace (s i maximum)
  (declare (type (signed-byte 29) i maximum)
           (type string s))
  (the-fixnum
   (cond ((< i maximum)
          (cond ((member (charf s i) '(#\Space #\Tab #\Newline))
                 (scan-past-whitespace s (+f i 1) maximum))
                (t i)))
         (t maximum))))

(defun zero-one-or-more (x)
  (let ((n (cond ((integerp x) x)
                 (t (length x)))))
    (case n
          (0 0)
          (1 1)
          (otherwise 2))))

(defun find-alternative-skip (s i maximum)

; This function finds the first character after a list of alternatives.
; i is the value of find-alternative-stop, i.e., it points to the ~ in
; the ~/ or ~] that closed the alternative used.

; Suppose s is "~#7~[ab~/cd~/ef~]acl2".
;               01234567890123456789
; If i is 11, the answer is 17.
;

  (declare (type (signed-byte 29) i maximum)
           (type string s))
  (the-fixnum
   (cond ((< i maximum)
          (let ((char-s-i (charf s i)))
            (declare (type character char-s-i))
            (case char-s-i
              (#\~
               (let ((char-s-1+i (charf s (1+f i))))
                 (declare (type character char-s-1+i))
                 (case char-s-1+i
                   (#\] (+f 2 i))
                   (#\[ (find-alternative-skip
                         s
                         (find-alternative-skip s (+f 2 i)
                                                maximum)
                         maximum))
                   (otherwise (find-alternative-skip
                               s (+f 2 i) maximum)))))
              (otherwise
               (find-alternative-skip s (+f 1 i) maximum)))))
         (t (er-hard-val 0 'find-alternative-skip
                "Illegal Fmt Syntax -  While looking for the terminating ~
                bracket of a tilde alternative directive in the string ~
                below we ran off the end of the string.~|~%~x0"
                s)))))

(defun find-alternative-start1 (x s i maximum)
  (declare (type (signed-byte 29) x i maximum)
           (type string s))
  (the-fixnum
   (cond ((= x 0) i)
         ((< i maximum)
          (let ((char-s-i (charf s i)))
            (declare (type character char-s-i))
            (case char-s-i
              (#\~
               (let ((char-s-1+-i (charf s (1+f i))))
                 (declare (type character char-s-1+-i))
                 (case char-s-1+-i
                   (#\/ (find-alternative-start1
                         (1-f x) s (+f 2 i)
                         maximum))
                   (#\] (er-hard-val 0 'find-alternative-start1
                            "Illegal Fmt Syntax -- The tilde directive ~
                             terminating at position ~x0 of the string below ~
                             does not have enough alternative clauses.  When ~
                             the terminal bracket was reached we still needed ~
                             ~#1~[~/1 more alternative~/~x2 more ~
                             alternatives~].~|~%~x3"
                            i
                            (zero-one-or-more x)
                            x
                            s))
                   (#\[ (find-alternative-start1
                         x s
                         (find-alternative-skip s (+f 2 i) maximum)
                         maximum))
                   (otherwise
                    (find-alternative-start1
                     x s (+f 2 i) maximum)))))
              (otherwise
               (find-alternative-start1 x s (+f 1 i)
                                        maximum)))))
         (t (er-hard-val 0 'find-alternative-start1
                "Illegal Fmt Syntax -- While searching for the appropriate ~
                alternative clause of a tilde alternative directive in the ~
                string below, we ran off the end of the string.~|~%~x0"
                s)))))

(defun fmt-char (s i j maximum err-flg)
  (declare (type (signed-byte 29) i maximum)

; We only increment i by a small amount, j.

           (type (integer 0 100) j)
           (type string s))
  (cond ((< (+f i j) maximum) (charf s (+f i j)))
        (err-flg
         (er hard 'fmt-char
             "Illegal Fmt Syntax.  The tilde directive at location ~x0 ~
                in the fmt string below requires that we look at the ~
                character ~x1 further down in the string.  But the ~
                string terminates at location ~x2.~|~%~x3"
             i j maximum s))
        (t nil)))

(defun find-alternative-start (x s i maximum)

; This function returns the index of the first character in the xth
; alternative, assuming i points to the ~ that begins the alternative
; directive.  If x is not an integer, we assume it is a non-empty
; list.  If its length is 1, pick the 0th alternative.  Otherwise,
; pick the 1st.  This means we can test on a list to get a "plural" test.

; Suppose s is "~#7~[ab~/cd~/ef~]acl2".  The indices into s are
;               01234567890123456789
; This function is supposed to be called with i=0.  Suppose register
; 7 contains a 1.  That's the value of x.  This function will return
; 9, the index of the beginning of alternative x.

  (declare (type (signed-byte 29) i maximum)
           (type string s))
  (the-fixnum
   (let ((x (cond ((integerp x) (the-fixnum! x 'find-alternative-start))
                  ((and (consp x)
                        (atom (cdr x)))
                   0)
                  (t 1))))
     (declare (type (signed-byte 29) x))
     (cond ((not (and (eql (the character (fmt-char s i 3 maximum t)) #\~)
                      (eql (the character (fmt-char s i 4 maximum t)) #\[)))
            (er-hard-val 0 'find-alternative-start
                "Illegal Fmt Syntax:  The tilde directive at ~x0 in the ~
                fmt string below must be followed immediately by ~~[. ~
                ~|~%~x1"
                i s))
           (t (find-alternative-start1 x s (+f i 5) maximum))))))

(defun find-alternative-stop (s i maximum)

; This function finds the end of the alternative into which i is
; pointing.  i is usually the first character of the current alternative.
; The answer points to the ~ in the ~/ or ~] closing the alternative.

; Suppose s is "~#7~[ab~/cd~/ef~]acl2".
;               01234567890123456789
; and i is 9.  Then the answer is 11.

  (declare (type (signed-byte 29) i maximum)
           (type string s))
  (the-fixnum
   (cond ((< i maximum)
          (let ((char-s-i (charf s i)))
            (declare (type character char-s-i))
            (case char-s-i
              (#\~ (let ((char-s-1+i (charf s (1+f i))))
                     (declare (type character char-s-1+i))
                     (case char-s-1+i
                       (#\/ i)
                       (#\[ (find-alternative-stop
                             s
                             (find-alternative-skip s (+f 2 i) maximum)
                             maximum))
                       (#\] i)
                       (otherwise (find-alternative-stop
                                   s (+f 2 i) maximum)))))
              (otherwise (find-alternative-stop s (+f 1 i) maximum)))))
         (t (er-hard-val 0 'find-alternative-stop
                "Illegal Fmt Syntax -- While looking for the terminating ~
                slash of a tilde alternative directive alternative clause ~
                in the string below we ran off the end of the string. ~
                ~|~%~x0"
                s)))))

(defun punctp (c)
  (if (member c '(#\. #\, #\: #\; #\? #\! #\) #\]))
      c
    nil))

(defun fmt-symbol-name1 (s i maximum col channel state)
  (declare (type (signed-byte 29) i maximum col)
           (type string s))
  (the2s
   (signed-byte 29)
   (cond ((not (< i maximum))
          (mv col state))
         ((and (> col (fmt-hard-right-margin state))
               (not (write-for-read state)))
          (pprogn
           (princ$ #\\ channel state)
           (newline channel state)
           (fmt-symbol-name1 s i maximum 0 channel state)))
         (t
          (let ((c (charf s i))
                (fmt-soft-right-margin (fmt-soft-right-margin state)))
            (declare (type character c)
                     (type (signed-byte 29) fmt-soft-right-margin))
            (cond ((and (> col fmt-soft-right-margin)
                        (not (write-for-read state))
                        (eql c #\Space))
                   (pprogn
                    (newline channel state)
                    (fmt-symbol-name1 s
                                      (scan-past-whitespace s (+f i 1) maximum)
                                      maximum 0 channel state)))
                  ((and (> col fmt-soft-right-margin)
                        (not (write-for-read state))
                        (or (eql c #\-)
                            (eql c #\_))
                        (not (int= (1+f i) maximum)))

; If we are beyond the soft right margin and we are about to print a
; hyphen or underscore and it is not the last character in the string,
; then print it and do a terpri.  If it is the last character, as it
; is in say, the function name "1-", then we don't do the terpri and
; hope there is a better place to break soon.  The motivating example
; for this was in seeing a list of function names get printed in a way
; that produced a comma as the first character of the newline, e.g.,
; "... EQL, 1+, 1-
; , ZEROP and PLUSP."

                   (pprogn
                    (princ$ c channel state)
                    (if (eql c #\-) state (princ$ #\- channel state))
                    (newline channel state)
                    (fmt-symbol-name1 s
                                      (scan-past-whitespace s (+f i 1) maximum)
                                      maximum 0 channel state)))
                  (t
                   (pprogn
                    (princ$ c channel state)
                    (fmt-symbol-name1 s (1+f i) maximum (1+f col)
                                      channel state)))))))))

(defun fmt-var (s alist i maximum)
  (declare (type (signed-byte 29) i maximum)
           (type string s))
  (let ((x (assoc (the character (fmt-char s i 2 maximum t)) alist)))
    (cond (x (cdr x))
          (t (er hard 'fmt-var
                 "Unbound Fmt Variable.  The tilde directive at location ~x0 ~
                  in the fmt string below uses the variable ~x1.  But ~
                  this variable is not bound in the association list, ~
                  ~x2, supplied with the fmt string.~|~%~x3"
                 i (char s (+f i 2)) alist s)))))

(defun splat-atom (x print-base indent col channel state)
  (let* ((sz (flsz-atom x print-base 0 state))
         (too-bigp (> (+ col sz) (fmt-hard-right-margin state))))
    (pprogn (if too-bigp
                (pprogn (newline channel state)
                        (spaces indent 0 channel state))
                state)
            (prin1$ x channel state)
            (mv (if too-bigp (+ indent sz) (+ col sz))
                state))))

; Splat, below, prints out an arbitrary ACL2 object flat, introducing
; the single-gritch notation for quote and breaking lines between lexemes
; to avoid going over the hard right margin.  It indents all but the first
; line by indent spaces.

(mutual-recursion

(defun splat (x print-base indent col channel state)
  (cond ((atom x)
         (splat-atom x print-base indent col channel state))
        ((and (eq (car x) 'quote)
              (consp (cdr x))
              (null (cddr x)))
         (pprogn (princ$ #\' channel state)
                 (splat (cadr x) print-base indent (1+ col) channel state)))
        (t (pprogn (princ$ #\( channel state)
                   (splat1 x print-base indent (1+ col) channel state)))))

(defun splat1 (x print-base indent col channel state)
  (mv-let (col state)
          (splat (car x) print-base indent col channel state)
          (cond ((null (cdr x))
                 (pprogn (princ$ #\) channel state)
                         (mv (1+ col) state)))
                ((atom (cdr x))
                 (cond ((> (+ 3 col) (fmt-hard-right-margin state))
                        (pprogn (newline channel state)
                                (spaces indent 0 channel state)
                                (princ$ ". " channel state)
                                (mv-let (col state)
                                        (splat (cdr x)
                                               print-base
                                               indent
                                               (+ indent 2)
                                               channel state)
                                        (pprogn (princ$ #\) channel state)
                                                (mv (1+ col) state)))))
                       (t (pprogn
                           (princ$ " . " channel state)
                           (mv-let (col state)
                                   (splat (cdr x)
                                          print-base
                                          indent (+ 3 col) channel
                                          state)
                                   (pprogn (princ$ #\) channel state)
                                           (mv (1+ col) state)))))))
                (t (pprogn
                    (princ$ #\Space channel state)
                    (splat1 (cdr x) print-base indent (1+ col) channel
                            state))))))

)

(defun number-of-digits (n)

; We compute the width of the field necessary to express the integer n
; in decimal notation.  We assume minus signs are printed but plus
; signs are not.  Thus, if n is -123 we return 4, if n is 123 we
; return 3.

  (cond ((< n 0) (1+ (number-of-digits (abs n))))
        ((< n 10) 1)
        (t (1+ (number-of-digits (floor n 10))))))

(defun left-pad-with-blanks (n width col channel state)

; Print the integer n right-justified in a field of width width.
; We return the final column (assuming we started in col) and state.

  (declare (type (signed-byte 29) col))
  (the2s
   (signed-byte 29)
   (let ((d (the-half-fixnum! (number-of-digits n) 'left-pad-with-blanks)))
     (declare (type (signed-byte 29) d))
     (cond ((>= d width)
            (pprogn (prin1$ n channel state)
                    (mv (+ col d) state)))
           (t (pprogn
               (spaces (- width d) col channel state)
               (prin1$ n channel state)
               (mv (the-fixnum! (+ col width) 'left-pad-with-blanks)
                   state)))))))

(defmacro maybe-newline (body)

; This macro is used in fmt0 to force a newline only when absolutely
; necessary.  It knows the locals of fmt0, in particular, col,
; channel, and state.  We wrap this macro around code that is about to
; print a character at col.  Once upon a time we just started fmt0
; with a newline if we were past the hard right margin, but that
; produced occasional lines that ended naturally at the hard right
; margin and then had a backslash inserted in anticipation of the 0
; characters to follow.  It was impossible to tell if more characters
; follow because there may be tilde commands between where you are and
; the end of the line, and they may or may not print things.

  `(mv-letc (col state)
            (cond
             ((and (> col (fmt-hard-right-margin state))
                   (not (write-for-read state)))
              (pprogn (princ$ #\\ channel state)
                      (newline channel state)
                      (mv 0 state)))
             (t (mv col state)))
            ,body))

; To support the convention that er, fmt, and even individual fmt
; commands such as ~X can control their own evisceration parameters,
; we now introduce the idea of an evisceration tuple, or evisc-tuple.

(defun evisc-tuple (print-level print-length alist hiding-cars)

; This is really just a record constructor, but we haven't got defrec
; yet so we do it by hand.  The following functions are all of the
; ones that construct or approve of evisc-tuples:
; * evisc-tuple
; * term-evisc-tuple
; * default-evisc-tuple
; * ld-evisc-tuple -- this function does not actually construct
;   one but returns one supplied by the user and approved by:
; * standard-evisc-tuplep
; * trace-evisc-tuple (see also the constant *trace-evisc-tuple*)
; * reset-trace-evisc-tuple

; In addition, we sometimes write out constant evisc tuples!  However
; they are commented nearby with (evisc-tuple ...).

; The primitive consumers of evisc tuples all call 
; * eviscerate, or
; * eviscerate-stobjs.

;         car   cadr        caddr        cadddr

  (list alist   print-level print-length hiding-cars))

(defun standard-evisc-tuplep (x)

; This function limits what ld-evisc-tuple may be assigned.

  (or (null x)
      (and (true-listp x)
           (= (length x) 4)
           (alistp (car x))
           (or (null (cadr x))
               (integerp (cadr x)))
           (or (null (caddr x))
               (integerp (caddr x)))
           (symbol-listp (cadddr x)))))

#-acl2-loop-only
(defparameter *approved-user-default-evisc-tuple* nil)

(defun user-default-evisc-tuple (state)
  (declare (xargs :guard (and (state-p state)
                              (f-boundp-global 'user-default-evisc-tuple state))))
  (let ((val (f-get-global 'user-default-evisc-tuple state)))
    (cond
     #-acl2-loop-only
     ((eq val *approved-user-default-evisc-tuple*) ; optimization
      val)
     ((standard-evisc-tuplep val)
      #-acl2-loop-only
      (setq *approved-user-default-evisc-tuple* val) ; optimization
      val)
     (t (er hard 'user-default-evisc-tuple
            "The user-supplied value for function default-evisc-tuple, which ~
             is (f-get-global 'user-default-evisc-tuple state), is not a legal ~
             evisc-tuple.  See :DOC ld-evisc-tuple.")))))

(defun default-evisc-tuple (state)
  (cond
   ((f-boundp-global 'user-default-evisc-tuple state)
    (user-default-evisc-tuple state))
   (t (cons (world-evisceration-alist state nil)
            '(5 7 nil)))))

#-acl2-loop-only
(defparameter *approved-user-term-evisc-tuple* nil)

(defun user-term-evisc-tuple (state)
  (declare (xargs :guard (and (state-p state)
                              (f-boundp-global 'user-term-evisc-tuple state))))
  (let ((val (f-get-global 'user-term-evisc-tuple state)))
    (cond
     #-acl2-loop-only
     ((eq val *approved-user-term-evisc-tuple*) ; optimization
      val)
     ((standard-evisc-tuplep val)
      #-acl2-loop-only
      (setq *approved-user-term-evisc-tuple* val) ; optimization
      val)
     (t (er hard 'user-term-evisc-tuple
            "The user-supplied value for function term-evisc-tuple, which is ~
             (f-get-global 'user-term-evisc-tuple state), is not a legal ~
             evisc-tuple.  See :DOC ld-evisc-tuple.")))))

(defun term-evisc-tuple (flg state)

; This evisceration tuple is used when we are printing terms or lists
; of terms.  We don't hide the world or state because they aren't
; (usually) found in terms.  This saves us a little time.  If the
; global value of 'eviscerate-hide-terms is t, we print (HIDE ...) as
; <hidden>.  Otherwise not.  Flg controls whether we actually
; eviscerate on the basis of structural depth and length.  If flg is t
; we do.  The choice of the print-length 4 is motivated by the idea
; of being able to print IF as (IF # # #) rather than (IF # # ...).
; Print-level 3 lets us print a clause as ((NOT (PRIMEP #)) ...)
; rather than ((NOT #) ...).

  (cond ((and flg (f-boundp-global 'user-term-evisc-tuple state))
         (user-term-evisc-tuple state))
        ((f-get-global 'eviscerate-hide-terms state)
         (cond (flg
                ;;; (evisc-tuple 3 4 nil '(hide))
                '(nil 3 4 (hide)))

               (t
                ;;; (evisc-tuple nil nil nil '(hide))
                '(nil nil nil (hide)))))
        (flg    ;;; (evisc-tuple 3 4 nil nil)
         '(nil 3 4 nil))
        (t nil)))

(deflabel eviscerate-hide-terms
  :doc
  ":Doc-Section Miscellaneous

  to print ~c[(hide ...)] as ~c[<hidden>]~/
  ~bv[]
  Example:
  (assign eviscerate-hide-terms t)
  (assign eviscerate-hide-terms nil)
  ~ev[]~/

  ~c[Eviscerate-hide-terms] is a ~ilc[state] global variable whose value is
  either ~c[t] or ~c[nil].  The variable affects how terms are displayed.  If
  ~c[t], terms of the form ~c[(hide ...)] are printed as ~c[<hidden>].  Otherwise,
  they are printed normally.")

#-acl2-loop-only
(defun-one-output print-infix (x termp width rpc col file eviscp)

; X is an s-expression denoting a term (if termp = t) or an evg (if
; termp = nil).  File is an open output file.  Prettyprint x in infix
; notation to file.  If eviscp is t then we are to give special treatment to
; the :evisceration-mark; otherwise not.

; This hook is modeled after the ACL2 pretty-printer, which has the following
; additional features.  These features need not be implemented in the infix
; prettyprinter.  The printer is assumed to be in column col, where col=0 means
; it is on the left margin.  We are supposed to print our first character in
; that column.  We are supposed to print in a field of width width.  That is,
; the largest column into which we might print is col+width-2.  Finally, assume
; that on the last line of the output somebody is going to write rpc additional
; characters and arrange for this not to overflow the col+width-2 limit.  Rpc
; is used when, for example, we plan to print some punctuation, like a comma,
; after a form and want to ensure that we can do it without overflowing the
; right margin.  (One might think that the desired effect could be obtained by
; setting width smaller, but that is wrong because it narrows the whole field
; and we only want to guarantee space on the last line.)  Here is an example.
; Use ctrl-x = in emacs to see what columns things are in.  The semi-colons are
; in column 0.  Pretend they are all spaces, as they would be if the printing
; had been done by fmt-ppr.

; (foobar
;   (here is a long arg)
;   a)                  

; Here, col = 2, width = 23, and rpc = 19!

; Infix Hack:
; We simply print out $ followed by the expression.  We print the
; expression in lower-case.

  (declare (ignore termp width rpc col eviscp))
  (let ((*print-case* :downcase)
        (*print-pretty* t))
    (princ "$ " file)
    (prin1 x file)))

(defun fmt-ppr (x termp width rpc col channel state eviscp)
  (declare (type (signed-byte 29) col))
  #+acl2-loop-only
  (declare (ignore termp))
  #-acl2-loop-only
  (cond
   ((and (live-state-p state)
         (output-in-infixp state))
    (print-infix x termp width rpc col
                 (get-output-stream-from-channel channel)
                 eviscp)
    (return-from fmt-ppr *the-live-state*)))
  (ppr2 (ppr1 x (acl2-print-base) width rpc state eviscp)
        col channel state eviscp))

(mutual-recursion

(defun fmt0* (str0 str1 str2 str3 lst alist col channel state evisc-tuple)

; This odd function prints out the members of lst.  If the list has no
; elements, str0 is used.  If the list has 1 element, str1 is used
; with #\* bound to the element.  If the list has two elements, str2
; is used with #\* bound to the first element and then str1 is used
; with #\* bound to the second.  If the list has more than two
; elements, str3 is used with #\* bound successively to each element
; until there are only two left.  The function is used in the
; implementation of ~&, ~v, and ~*.

  (declare (type (signed-byte 29) col)
           (type string str0 str1 str2 str3))
  (the2s
   (signed-byte 29)
   (cond ((null lst)
          (fmt0 str0 alist 0 (the-fixnum! (length str0) 'fmt0*) col channel
                state evisc-tuple))
         ((null (cdr lst))
          (fmt0 str1
                (cons (cons #\* (car lst)) alist)
                0 (the-fixnum! (length str1) 'fmt0*) col channel
                state evisc-tuple))
         ((null (cddr lst))
          (mv-letc (col state)
                   (fmt0 str2
                         (cons (cons #\* (car lst)) alist)
                         0 (the-fixnum! (length str2) 'fmt0*)
                         col channel state evisc-tuple)
                   (fmt0* str0 str1 str2 str3 (cdr lst) alist col channel
                          state evisc-tuple)))
         (t (mv-letc (col state)
                     (fmt0 str3
                           (cons (cons #\* (car lst)) alist)
                           0 (the-fixnum! (length str3) 'fmt0*)
                           col channel state evisc-tuple)
                     (fmt0* str0 str1 str2 str3 (cdr lst) alist col channel
                            state evisc-tuple))))))

(defun fmt0&v (flg lst punct col channel state evisc-tuple)
  (declare (type (signed-byte 29) col))
  (the2s
   (signed-byte 29)
   (case flg
     (&
      (case
          punct
        (#\. (fmt0* "" "~x*." "~x* and " "~x*, " lst nil col channel
                    state evisc-tuple))
        (#\, (fmt0* "" "~x*," "~x* and " "~x*, " lst nil col channel
                    state evisc-tuple))
        (#\: (fmt0* "" "~x*:" "~x* and " "~x*, " lst nil col channel
                    state evisc-tuple))
        (#\; (fmt0* "" "~x*;" "~x* and " "~x*, " lst nil col channel
                    state evisc-tuple))
        (#\! (fmt0* "" "~x*!" "~x* and " "~x*, " lst nil col channel
                    state evisc-tuple))
        (#\) (fmt0* "" "~x*)" "~x* and " "~x*, " lst nil col channel
                    state evisc-tuple))
        (#\? (fmt0* "" "~x*?" "~x* and " "~x*, " lst nil col channel
                    state evisc-tuple))
        (otherwise
         (fmt0* "" "~x*" "~x* and " "~x*, " lst nil col channel
                state evisc-tuple))))
     (otherwise
      (case
          punct
        (#\. (fmt0* "" "~x*." "~x* or " "~x*, " lst nil col channel
                    state evisc-tuple))
        (#\, (fmt0* "" "~x*," "~x* or " "~x*, " lst nil col channel
                    state evisc-tuple))
        (#\: (fmt0* "" "~x*:" "~x* or " "~x*, " lst nil col channel
                    state evisc-tuple))
        (#\; (fmt0* "" "~x*;" "~x* or " "~x*, " lst nil col channel
                    state evisc-tuple))
        (#\! (fmt0* "" "~x*!" "~x* or " "~x*, " lst nil col channel
                    state evisc-tuple))
        (#\) (fmt0* "" "~x*)" "~x* or " "~x*, " lst nil col channel
                    state evisc-tuple))
        (#\? (fmt0* "" "~x*?" "~x* or " "~x*, " lst nil col channel
                    state evisc-tuple))
        (otherwise
         (fmt0* "" "~x*" "~x* or " "~x*, " lst nil col channel
                state evisc-tuple)))))))

(defun spell-number (n cap col channel state evisc-tuple)

; If n is an integerp we spell out the name of the cardinal number n
; (for a few cases) or else we just print the decimal representation
; of n.  E.g., n=4 makes us spell "four".  If n is a consp then we
; assume its car is an integer and we spell the corresponding ordinal
; number, e.g., n= '(4 . th) makes us spell "fourth".  We capitalize
; the word if cap is t.

  (declare (type (signed-byte 29) col))
  (the2s
   (signed-byte 29)
   (let ((str
          (cond ((integerp n)
                 (cond ((int= n 0) (if cap "Zero" "zero"))
                       ((int= n 1) (if cap "One" "one"))
                       ((int= n 2) (if cap "Two" "two"))
                       ((int= n 3) (if cap "Three" "three"))
                       ((int= n 4) (if cap "Four" "four"))
                       ((int= n 5) (if cap "Five" "five"))
                       ((int= n 6) (if cap "Six" "six"))
                       ((int= n 7) (if cap "Seven" "seven"))
                       ((int= n 8) (if cap "Eight" "eight"))
                       ((int= n 9) (if cap "Nine" "nine"))
                       ((int= n 10) (if cap "Ten" "ten"))
                       ((int= n 11) (if cap "Eleven" "eleven"))
                       ((int= n 12) (if cap "Twelve" "twelve"))
                       ((int= n 13) (if cap "Thirteen" "thirteen"))
                       (t "~x0")))
                ((and (consp n)
                      (<= 0 (car n))
                      (<= (car n) 13))
                 (cond ((int= (car n) 0) (if cap "Zeroth" "zeroth"))
                       ((int= (car n) 1) (if cap "First" "first"))
                       ((int= (car n) 2) (if cap "Second" "second"))
                       ((int= (car n) 3) (if cap "Third" "third"))
                       ((int= (car n) 4) (if cap "Fourth" "fourth"))
                       ((int= (car n) 5) (if cap "Fifth" "fifth"))
                       ((int= (car n) 6) (if cap "Sixth" "sixth"))
                       ((int= (car n) 7) (if cap "Seventh" "seventh"))
                       ((int= (car n) 8) (if cap "Eighth" "eighth"))
                       ((int= (car n) 9) (if cap "Ninth" "ninth"))
                       ((int= (car n) 10) (if cap "Tenth" "tenth"))
                       ((int= (car n) 11) (if cap "Eleventh" "eleventh"))
                       ((int= (car n) 12) (if cap "Twelfth" "twelfth"))
                       (t (if cap "Thirteenth" "thirteenth"))))
                (t (let ((d (mod (abs (car n)) 10)))

; We print -11th, -12th, -13th, ... -20th, -21st, -22nd, etc., though
; what business anyone has using negative ordinals I can't imagine.

                     (cond ((or (int= d 0)
                                (> d 3)
                                (int= (car n) -11)
                                (int= (car n) -12)
                                (int= (car n) -13))
                            "~x0th")
                           ((int= d 1) "~x0st")
                           ((int= d 2) "~x0nd")
                           (t "~x0rd")))))))

     (fmt0 (the-string! str 'spell-number)
           (cond ((integerp n)
                  (cond ((and (<= 0 n) (<= n 13)) nil)
                        (t (list (cons #\0 n)))))
                 (t (cond ((and (<= 0 (car n)) (<= (car n) 13)) nil)
                          (t (list (cons #\0 (car n)))))))
           0 (the-fixnum! (length str) 'spell-number)
           col channel state evisc-tuple))))

(defun fmt-symbol-name (s col channel state)

; This function is misnamed.  S ought to be an acl2-numberp, a symbol or a
; string.  We print it out, breaking on hyphens but not being fooled by fmt
; directives inside it.  We return the new col and state.

  (declare (type (signed-byte 29) col))
  (the2s
   (signed-byte 29)
   (cond
    ((acl2-numberp s)
     (pprogn (prin1$ s channel state)
             (mv (flsz-atom s (acl2-print-base state) col state) state)))
    ((stringp s)
     (fmt-symbol-name1 s 0 (the-fixnum! (length s) 'fmt-symbol-name) col
                       channel state))
    (t
     (let ((str (symbol-name s)))
       (cond
        ((keywordp s)
         (cond
          ((may-need-slashes str)
           (fmt0 ":|~s0|" (list (cons #\0 str)) 0 6 col channel state nil))
          (t (fmt0 ":~s0" (list (cons #\0 str)) 0 4 col channel state nil))))
        ((or (equal (symbol-package-name s)
                    (f-get-global 'current-package state))
             (member-eq
              s
              (package-entry-imports
               (find-package-entry
                (f-get-global 'current-package state)
                (known-package-alist state)))))
         (cond
          ((may-need-slashes str)
           (fmt0 "|~s0|" (list (cons #\0 str)) 0 5 col channel state nil))
          (t (fmt-symbol-name1 str 0
                               (the-fixnum! (length str) 'fmt-symbol-name)
                               col channel state))))
        (t
         (let ((p (symbol-package-name s)))
           (cond
            ((may-need-slashes p)
             (cond
              ((may-need-slashes str)
               (fmt0 "|~s0|::~-|~s1|"
                     (list (cons #\0 p)
                           (cons #\1 str))
                     0 14 col channel state nil))
              (t (fmt0 "|~s0|::~-~s1"
                       (list (cons #\0 p)
                             (cons #\1 str))
                       0 12 col channel state nil))))
            ((may-need-slashes str)
             (fmt0 "~s0::~-|~s1|"
                   (list (cons #\0 p)
                         (cons #\1 str))
                   0 12 col channel state nil))
            (t (fmt0 "~s0::~-~s1"
                     (list (cons #\0 p)
                           (cons #\1 str))
                     0 10 col channel state nil)))))))))))

(defun fmt0 (s alist i maximum col channel state evisc-tuple)
  (declare (type (signed-byte 29) i maximum col)
           (type string s))

; WARNING:  If you add new tilde-directives update :DOC fmt and the
; copies in :DOC fmt1 and :DOC fms.

  (declare (type (signed-byte 29) col))
  (the2s
   (signed-byte 29)
   (cond
    ((>= i maximum)
     (mv col state))
    (t
     (let ((c (charf s i)))
       (declare (type character c))
       (cond
        ((eql c #\~)
         (let ((fmc (the character (fmt-char s i 1 maximum t))))
           (declare (type character fmc))
           (case
            fmc
             ((#\p #\q #\P #\Q)

; These directives print terms.  If 'infixp is t or :out, we use infix
; printing.  Otherwise we use s-expr printing.  We assume the term has already
; been untranslated.

; The difference between the lowercase directives and the uppercase ones
; is that the uppercase ones take two fmt-vars, e.g., ~P01, and use the
; contents of the second one as the evisceration value.  Otherwise the
; uppercase directives behave as their lowercase counterparts.

; On symbols, ~p and ~q are alike and just print starting in col.  On non-
; symbols they both prettyprint.  But ~q starts printing in col while ~p
; may do a terpri and indent first.  ~p concludes with a terpri if
; it put out a terpri before printing.  ~q always concludes with a terpri
; on non-symbols, so you know where you end up.

              (maybe-newline
               (let* ((local-evisc-tuple
                       (cond ((or (eql fmc #\P)
                                  (eql fmc #\Q))
                              (fmt-var s alist (1+f i) maximum))
                             (t evisc-tuple)))

; Here is the place we unpack (evisc-tuple ...) or (default-evisc-tuple ...).

                      (x (cond
                          (local-evisc-tuple
                           (eviscerate
                            (fmt-var s alist i maximum)
                            (cadr local-evisc-tuple)          ;;; print-level
                            (caddr local-evisc-tuple)         ;;; print-length
                            (car local-evisc-tuple)           ;;; alist
                            (cadddr local-evisc-tuple)))      ;;; hiding-cars
                          (t (fmt-var s alist i maximum)))))
                 (let ((fmt-hard-right-margin
                        (fmt-hard-right-margin state)))
                   (declare (type (signed-byte 29) fmt-hard-right-margin))
                   (let ((sz (flsz x t col fmt-hard-right-margin state
                                   local-evisc-tuple)))
                     (declare (type (signed-byte 29) sz))
                     (cond
                      ((and (or (eql fmc #\p)
                                (eql fmc #\P))
                            (> col (the-fixnum *fmt-ppr-indentation*))
                            (>= sz fmt-hard-right-margin)
                            (not (>= (flsz x
                                           t
                                           (the-fixnum
                                            *fmt-ppr-indentation*)
                                           fmt-hard-right-margin
                                           state local-evisc-tuple)
                                     fmt-hard-right-margin)))
                       (pprogn
                        (newline channel state)
                        (spaces1 (the-fixnum *fmt-ppr-indentation*) 0
                                 fmt-hard-right-margin
                                 channel state)
                        (fmt0 s alist i maximum
                              (the-fixnum *fmt-ppr-indentation*)
                              channel state evisc-tuple)))
                      ((or (eql fmc #\q)
                           (eql fmc #\Q)
                           (>= sz fmt-hard-right-margin))
                       (pprogn
                        (cond ((or (eql fmc #\q)
                                   (eql fmc #\Q))
                               state)
                              ((= col 0) state)
                              (t (newline channel state)))
                        (if (or (eql fmc #\q)
                                (eql fmc #\Q))
                            state
                          (spaces1 (the-fixnum *fmt-ppr-indentation*)
                                   0 fmt-hard-right-margin channel state))
                        (let ((c (fmt-char s i
                                           (the-fixnum
                                            (if (or (eql fmc #\P)
                                                    (eql fmc #\Q))
                                                4
                                              3))
                                           maximum nil)))
                          (cond ((punctp c)
                                 (pprogn
                                  (fmt-ppr
                                   x
                                   t
                                   (+f fmt-hard-right-margin
                                       (-f (if (or (eql fmc #\q)
                                                   (eql fmc #\Q))
                                               col
                                             *fmt-ppr-indentation*)))
                                   1
                                   (the-fixnum
                                    (if (or (eql fmc #\q)
                                            (eql fmc #\Q))
                                        col
                                      *fmt-ppr-indentation*))
                                   channel state local-evisc-tuple)
                                  (princ$ c channel state)
                                  (newline channel state)
                                  (fmt0 s alist
                                        (scan-past-whitespace
                                         s
                                         (+f i
                                             (if (or (eql fmc #\P)
                                                     (eql fmc #\Q))
                                                 5
                                               4))
                                         maximum)
                                        maximum 0 channel state
                                        evisc-tuple)))
                                (t
                                 (pprogn
                                  (fmt-ppr
                                   x
                                   t
                                   (+f fmt-hard-right-margin
                                       (-f (if (or (eql fmc #\q)
                                                   (eql fmc #\Q))
                                               col
                                             *fmt-ppr-indentation*)))
                                   0
                                   (the-fixnum
                                    (if (or (eql fmc #\q)
                                            (eql fmc #\Q))
                                        col
                                      *fmt-ppr-indentation*))
                                   channel state local-evisc-tuple)
                                  (newline channel state)
                                  (fmt0 s alist
                                        (scan-past-whitespace
                                         s
                                         (+f i (if (or (eql fmc #\P)
                                                       (eql fmc #\Q))
                                                   4
                                                 3))
                                         maximum)
                                        maximum 0 channel state
                                        evisc-tuple)))))))
                      (t (pprogn
                          (flpr x t channel state local-evisc-tuple)
                          (fmt0 s alist
                                (+f i (if (or (eql fmc #\P)
                                              (eql fmc #\Q))
                                          4
                                        3))
                                maximum sz
                                channel state evisc-tuple)))))))))
             ((#\x #\y #\X #\Y)

; The difference between the lowercase directives and the uppercase ones
; is that the uppercase ones take two fmt-vars, e.g., ~X01, and use the
; contents of the second one as the evisceration value.  Otherwise the
; uppercase directives behave as their lowercase counterparts.

; On symbols, ~x and ~y are alike and just print starting in col.  On non-
; symbols they both prettyprint.  But ~y starts printing in col while ~x
; may do a terpri and indent first.  ~x concludes with a terpri if
; it put out a terpri before printing.  ~y always concludes with a terpri
; on non-symbols, so you know where you end up.

              (maybe-newline
               (let* ((local-evisc-tuple
                       (cond ((or (eql fmc #\X)
                                  (eql fmc #\Y))
                              (fmt-var s alist (1+f i) maximum))
                             (t evisc-tuple)))

; Here is the place we unpack (evisc-tuple ...) or (default-evisc-tuple ...).

                      (x (cond
                          (local-evisc-tuple
                           (eviscerate
                            (fmt-var s alist i maximum)
                            (cadr local-evisc-tuple)          ;;; print-level
                            (caddr local-evisc-tuple)         ;;; print-length
                            (car local-evisc-tuple)           ;;; alist
                            (cadddr local-evisc-tuple)))      ;;; hiding-cars
                          (t (fmt-var s alist i maximum)))))
                 (cond
                  ((or (symbolp x)
                       (acl2-numberp x))
                   (mv-letc (col state)
                            (fmt-symbol-name x col channel state)
                            (fmt0 s alist
                                  (+f i (if (or (eql fmc #\X)
                                                (eql fmc #\Y))
                                            4
                                          3))
                                  maximum col channel state evisc-tuple)))
                  (t (let ((fmt-hard-right-margin
                            (fmt-hard-right-margin state)))
                       (declare (type (signed-byte 29) fmt-hard-right-margin))
                       (let ((sz (flsz x nil col fmt-hard-right-margin state
                                       local-evisc-tuple)))
                         (declare (type (signed-byte 29) sz))
                         (cond
                          ((and (or (eql fmc #\x)
                                    (eql fmc #\X))
                                (> col (the-fixnum *fmt-ppr-indentation*))
                                (>= sz fmt-hard-right-margin)
                                (not (>= (flsz x
                                               nil
                                               (the-fixnum
                                                *fmt-ppr-indentation*)
                                               fmt-hard-right-margin
                                               state local-evisc-tuple)
                                         fmt-hard-right-margin)))
                           (pprogn
                            (newline channel state)
                            (spaces1 (the-fixnum *fmt-ppr-indentation*) 0
                                     fmt-hard-right-margin
                                     channel state)
                            (fmt0 s alist i maximum
                                  (the-fixnum *fmt-ppr-indentation*)
                                  channel state evisc-tuple)))
                          ((or (eql fmc #\y)
                               (eql fmc #\Y)
                               (>= sz fmt-hard-right-margin))
                           (pprogn
                            (cond ((or (eql fmc #\y)
                                       (eql fmc #\Y))
                                   state)
                                  ((= col 0) state)
                                  (t (newline channel state)))
                            (if (or (eql fmc #\y)
                                    (eql fmc #\Y))
                                state
                              (spaces1 (the-fixnum *fmt-ppr-indentation*)
                                       0 fmt-hard-right-margin channel state))
                            (let ((c (fmt-char s i
                                               (the-fixnum
                                                (if (or (eql fmc #\X)
                                                        (eql fmc #\Y))
                                                    4
                                                  3))
                                               maximum nil)))
                              (cond ((punctp c)
                                     (pprogn
                                      (fmt-ppr
                                       x
                                       nil
                                       (+f fmt-hard-right-margin
                                           (-f (if (or (eql fmc #\y)
                                                       (eql fmc #\Y))
                                                   col
                                                 *fmt-ppr-indentation*)))
                                       1
                                       (the-fixnum
                                        (if (or (eql fmc #\y)
                                                (eql fmc #\Y))
                                            col
                                          *fmt-ppr-indentation*))
                                       channel state local-evisc-tuple)
                                      (princ$ c channel state)
                                      (newline channel state)
                                      (fmt0 s alist
                                            (scan-past-whitespace
                                             s
                                             (+f i (if (or (eql fmc #\X)
                                                           (eql fmc #\Y))
                                                       5
                                                     4))
                                             maximum)
                                            maximum 0 channel state
                                            evisc-tuple)))
                                    (t
                                     (pprogn
                                      (fmt-ppr
                                       x
                                       nil
                                       (+f fmt-hard-right-margin
                                           (-f (if (or (eql fmc #\y)
                                                       (eql fmc #\Y))
                                                   col
                                                 *fmt-ppr-indentation*)))
                                       0
                                       (the-fixnum
                                        (if (or (eql fmc #\y)
                                                (eql fmc #\Y))
                                            col
                                          *fmt-ppr-indentation*))
                                       channel state local-evisc-tuple)
                                      (newline channel state)
                                      (fmt0 s alist
                                            (scan-past-whitespace
                                             s
                                             (+f i (if (or (eql fmc #\X)
                                                           (eql fmc #\Y))
                                                       4
                                                     3))
                                             maximum)
                                            maximum 0 channel state
                                            evisc-tuple)))))))
                          (t (pprogn
                              (flpr x nil channel state local-evisc-tuple)
                              (fmt0 s alist
                                    (+f i (if (or (eql fmc #\X)
                                                  (eql fmc #\Y))
                                              4
                                            3))
                                    maximum sz
                                    channel state evisc-tuple)))))))))))
             (#\@ (let ((s1 (fmt-var s alist i maximum)))
                    (mv-letc (col state)
                             (cond ((stringp s1)
                                    (fmt0 s1 alist 0
                                          (the-fixnum! (length s1) 'fmt0)
                                          col channel state evisc-tuple))
                                   ((consp s1)
                                    (fmt0 (car s1)
                                          (append (cdr s1) alist)
                                          0
                                          (the-fixnum! (length (car s1)) 'fmt0)
                                          col channel state evisc-tuple))
                                   (t (mv (er-hard-val 0 'fmt0
                                              "Illegal Fmt Syntax.  The ~
                                               tilde-@ directive at position ~
                                               ~x0 of the string below is ~
                                               illegal because its variable ~
                                               evaluated to ~x1, which is ~
                                               neither a string nor a ~
                                               list.~|~%~x2"
                                              i s1 s)
                                          state)))
                             (fmt0 s alist (+f i 3) maximum col
                                   channel state evisc-tuple))))
             (#\# (let ((n (find-alternative-start
                            (fmt-var s alist i maximum) s i maximum)))
                    (declare (type (signed-byte 29) n))
                    (let ((m (find-alternative-stop s n maximum)))
                      (declare (type (signed-byte 29) m))
                      (let ((o (find-alternative-skip s m maximum)))
                        (declare (type (signed-byte 29) o))
                        (mv-letc (col state) (fmt0 s alist
                                                   (the-fixnum n)
                                                   (the-fixnum m)
                                                   col channel
                                                   state evisc-tuple)
                                 (fmt0 s alist (the-fixnum o) maximum
                                       col channel state evisc-tuple))))))
             (#\* (let ((x (fmt-var s alist i maximum)))
                    (mv-letc (col state)
                             (fmt0* (car x) (cadr x) (caddr x) (cadddr x)
                                    (car (cddddr x))
                                    (append (cdr (cddddr x)) alist)
                                    col channel state evisc-tuple)
                             (fmt0 s alist (+f i 3) maximum col
                                   channel state evisc-tuple))))
             (#\& (let ((i+3 (+f i 3)))
                    (declare (type (signed-byte 29) i+3))
                    (mv-letc (col state)
                             (fmt0&v '&
                                     (fmt-var s alist i maximum)
                                     (punctp (and (< i+3 maximum)
                                                  (char s i+3)))
                                     col channel state evisc-tuple)
                             (fmt0 s alist
                                   (the-fixnum
                                    (cond
                                     ((punctp (and (< i+3 maximum)
                                                   (char s i+3)))
                                      (+f i 4))
                                     (t i+3)))
                                   maximum
                                   col channel state evisc-tuple))))
             (#\v (let ((i+3 (+f i 3)))
                    (declare (type (signed-byte 29) i+3))
                    (mv-letc (col state)
                             (fmt0&v 'v
                                     (fmt-var s alist i maximum)
                                     (punctp (and (< i+3 maximum)
                                                  (char s i+3)))
                                     col channel state evisc-tuple)
                             (fmt0 s alist
                                   (the-fixnum
                                    (cond
                                     ((punctp (and (< i+3 maximum)
                                                   (char s i+3)))
                                      (+f i 4))
                                     (t i+3)))
                                   maximum
                                   col channel state evisc-tuple))))
             (#\n (maybe-newline
                   (mv-letc (col state)
                            (spell-number (fmt-var s alist i maximum)
                                          nil col channel state evisc-tuple)
                            (fmt0 s alist (+f i 3) maximum col channel
                                  state evisc-tuple))))
             (#\N (maybe-newline
                   (mv-letc (col state)
                            (spell-number (fmt-var s alist i maximum)
                                          t col channel state evisc-tuple)
                            (fmt0 s alist (+f i 3) maximum col channel
                                  state evisc-tuple))))
             (#\t (maybe-newline
                   (let ((goal-col (fmt-var s alist i maximum))
                         (fmt-hard-right-margin (fmt-hard-right-margin state)))
                     (declare (type (signed-byte 29)
                                    goal-col fmt-hard-right-margin))
                     (pprogn
                      (cond ((> goal-col fmt-hard-right-margin)
                             (let ((er (er hard 'fmt0
                                           "It is illegal to tab past the ~
                                            value of (@ ~
                                            fmt-hard-right-margin), ~x0, and ~
                                            hence the directive ~~t~s1 to tab ~
                                            to column ~x2 is illegal.  See ~
                                            :DOC set-fmt-hard-right-margin."
                                           fmt-hard-right-margin
                                           (string (fmt-char s i 2 maximum t))
                                           goal-col)))
                               (declare (ignore er))
                               state))
                            ((>= col goal-col)
                             (pprogn (newline channel state)
                                     (spaces1 (the-fixnum goal-col) 0
                                              fmt-hard-right-margin
                                              channel state)))
                            (t (spaces1 (-f goal-col col) col
                                        fmt-hard-right-margin
                                        channel state)))
                      (fmt0 s alist (+f i 3) maximum
                            (the-fixnum goal-col)
                            channel state evisc-tuple)))))
             (#\c (maybe-newline
                   (let ((pair (fmt-var s alist i maximum)))
                     (cond ((and (consp pair)
                                 (integerp (car pair))
                                 (integerp (cdr pair))
                                 (>= (cdr pair) 0))
                            (mv-letc (col state)
                                     (left-pad-with-blanks (car pair)
                                                           (cdr pair)
                                                           col channel state)
                                     (fmt0 s alist (+f i 3) maximum col channel
                                           state evisc-tuple)))
                           (t (mv (er-hard-val 0 'fmt0
                                      "Illegal Fmt Syntax.  The tilde-c ~
                                       directive at position ~x0 of the string ~
                                       below is illegal because its variable ~
                                       evaluated to ~x1, which is not of the ~
                                       form (n . width), where n and width are ~
                                       integers and width is ~
                                       nonnegative.~|~%~x2"
                                      i pair s)
                                  state))))))
             ((#\f #\F)
              (maybe-newline
               (mv-letc (col state)
                        (splat (fmt-var s alist i maximum)
                               (acl2-print-base state)
                               (if (eql fmc #\F) (1+f col) 0)
                               col channel state)
                        (fmt0 s alist (+f i 3) maximum col channel
                              state evisc-tuple))))
             (#\s (maybe-newline
                   (mv-letc (col state)
                            (fmt-symbol-name (fmt-var s alist i maximum)
                                             col channel state)
                            (fmt0 s alist (+f i 3) maximum col channel
                                  state evisc-tuple))))
             (#\Space (let ((fmt-hard-right-margin
                             (fmt-hard-right-margin state)))
                        (declare (type (signed-byte 29) fmt-hard-right-margin))
                        (pprogn
                         (cond ((> col fmt-hard-right-margin)
                                (newline channel state))
                               (t state))
                         (princ$ #\Space channel state)
                         (fmt0 s alist (+f i 2) maximum
                               (cond ((> col fmt-hard-right-margin)
                                      1)
                                     (t (1+f col)))
                               channel state evisc-tuple))))
             (#\_ (maybe-newline
                   (let ((fmt-hard-right-margin
                          (fmt-hard-right-margin state)))
                     (declare (type (signed-byte 29) fmt-hard-right-margin))
                     (let ((n (the-half-fixnum! (fmt-var s alist i maximum)
                                                'fmt0)))
                       (declare (type (signed-byte 29) n))
                       (let ((new-col (+f col n)))
                         (declare (type (signed-byte 29) new-col))
                         (pprogn
                          (spaces n col channel state)
                          (cond
                           ((> new-col fmt-hard-right-margin)
                            (newline channel state))
                           (t state))
                          (fmt0 s alist (+f i 3) maximum
                                (the-fixnum
                                 (cond
                                  ((> new-col fmt-hard-right-margin)
                                   0)
                                  (t new-col)))
                                channel state evisc-tuple)))))))
             (#\Newline
              (fmt0 s alist (scan-past-whitespace s (+f i 2) maximum)
                    maximum col channel state evisc-tuple))
             (#\| (pprogn
                   (if (int= col 0) state (newline channel state))
                   (fmt0 s alist (+f i 2)
                         maximum 0 channel state evisc-tuple)))
             (#\% (pprogn
                   (newline channel state)
                   (fmt0 s alist (+f i 2)
                         maximum 0 channel state evisc-tuple)))
             (#\~ (maybe-newline
                   (pprogn
                    (princ$ #\~ channel state)
                    (fmt0 s alist (+f i 2) maximum (1+f col) channel
                          state evisc-tuple))))
             (#\- (cond ((> col (fmt-soft-right-margin state))
                         (pprogn
                          (princ$ #\- channel state)
                          (newline channel state)
                          (fmt0 s alist
                                (scan-past-whitespace s (+f i 2) maximum)
                                maximum 0 channel state evisc-tuple)))
                        (t (fmt0 s alist (+f i 2) maximum col channel
                                 state evisc-tuple))))
             (otherwise (let ((x
                               (er hard 'fmt0
                                   "Illegal Fmt Syntax.  The tilde ~
                                     directive at position ~x0 of the ~
                                     string below is unrecognized.~|~%~x1"
                                   i s)))
                          (declare (ignore x))
                          (mv 0 state))))))
        ((and (> col (fmt-soft-right-margin state))
              (eql c #\Space))
         (pprogn (newline channel state)
                 (fmt0 s alist
                       (scan-past-whitespace s (+f i 1) maximum)
                       maximum
                       0 channel state evisc-tuple)))
        ((and (>= col (fmt-soft-right-margin state))
              (eql c #\-))
         (pprogn (princ$ c channel state)
                 (newline channel state)
                 (fmt0 s alist
                       (scan-past-whitespace s (+f i 1) maximum)
                       maximum
                       0 channel state evisc-tuple)))
;       ((and (eql c #\Space)
; I cut out this code in response to Kaufmann's complaint 38.  The idea is
; *not* to ignore spaces after ~% directives.  I've left the code here to
; remind me of what I used to do, in case I see output that is malformed.
;            (int= col 0))
;       (fmt0 s alist (+f i 1) maximum 0 channel state evisc-tuple))
        (t (maybe-newline
            (pprogn (princ$ c channel state)
                    (fmt0 s alist (+f i 1) maximum
                          (if (eql c #\Newline) 0 (+f col 1))
                          channel state evisc-tuple))))))))))

)

(defun tilde-*-&v-strings (flg lst punct)

; This function returns an object that when bound to #\0 will cause
; ~*0 to print a conjunction (flg='&) or disjunction (flg='v) of the
; strings in lst, followed by punctuation punct, which must be #\. or
; #\,.

; WARNING:  This displayed strings are not equal to the strings in lst
; because whitespace may be inserted!

; ~& doesn't print a list of short strings very well because the first
; group is printed flat across the line, then when the line gets too
; long, the next string is indented and followed by a newline, which
; allows another bunch to be printed flat.  This function prints them
; with ~s which actually breaks the strings up internally in a way
; that does not preserve their equality.  "history-management.lisp"
; might have a newline inserted after the hyphen.

  (case
   flg
   (&
    (case
     punct
     (#\. (list "" "\"~s*\"." "\"~s*\" and " "\"~s*\", " lst))
     (#\, (list "" "\"~s*\"," "\"~s*\" and " "\"~s*\", " lst))
     (#\: (list "" "\"~s*\":" "\"~s*\" and " "\"~s*\", " lst))
     (#\; (list "" "\"~s*\";" "\"~s*\" and " "\"~s*\", " lst))
     (#\! (list "" "\"~s*\"!" "\"~s*\" and " "\"~s*\", " lst))
     (#\) (list "" "\"~s*\")" "\"~s*\" and " "\"~s*\", " lst))
     (#\? (list "" "\"~s*\"?" "\"~s*\" and " "\"~s*\", " lst))
     (otherwise
      (list "" "\"~s*\"" "\"~s*\" and " "\"~s*\", " lst))))
   (otherwise
    (case
     punct
     (#\. (list "" "\"~s*\"." "\"~s*\" or " "\"~s*\", " lst))
     (#\, (list "" "\"~s*\"," "\"~s*\" or " "\"~s*\", " lst))
     (#\: (list "" "\"~s*\":" "\"~s*\" or " "\"~s*\", " lst))
     (#\; (list "" "\"~s*\";" "\"~s*\" or " "\"~s*\", " lst))
     (#\! (list "" "\"~s*\"!" "\"~s*\" or " "\"~s*\", " lst))
     (#\) (list "" "\"~s*\")" "\"~s*\" or " "\"~s*\", " lst))
     (#\? (list "" "\"~s*\"?" "\"~s*\" or " "\"~s*\", " lst))
     (otherwise
      (list "" "\"~s*\"" "\"~s*\" or " "\"~s*\", " lst))))))

(defun fmt1 (str alist col channel state evisc-tuple)

; WARNING:  The master copy of the tilde-directives list is in :DOC fmt.

  ":Doc-Section ACL2::Programming

  ~c[:(str alist col co-channel state evisc) => (mv col state)]~/

  ~l[fmt] for further explanation, including documentation of the
  tilde-directives.~/~/"

  (declare (type (signed-byte 29) col))
  (the2s
   (signed-byte 29)
   #-acl2-loop-only
   (mv-let (col state)
           (fmt0 (the-string! str 'fmt1) alist 0
                 (the-fixnum! (length str) 'fmt1)
                 (the-fixnum! col 'fmt1)
                 channel state evisc-tuple)
           (declare (type (signed-byte 29) col))

; Allegro 6.0 needs explicit calls of finish-output in order to flush to
; standard output when *print-pretty* is nil.

           (progn #+(and allegro allegro-version>= (version>= 6 0))
                  (if (eq channel *standard-co*)
                      (finish-output
                       (get-output-stream-from-channel *standard-co*)))
                  (mv col state)))
   #+acl2-loop-only
   (fmt0 (the-string! str 'fmt1) alist 0
         (the-fixnum! (length str) 'fmt1)
         (the-fixnum! col 'fmt1)
         channel state evisc-tuple)))

(defun fmt (str alist channel state evisc-tuple)

; WARNING: IF you change the list of tilde-directives, change the copy of it in
; the :DOC for fmt1 and fms.

  ":Doc-Section ACL2::Programming

  formatted printing~/

  ACL2 provides the functions ~c[fmt], ~ilc[fmt1], and ~ilc[fms] as substitutes for Common
  Lisp's ~c[format] function.  Also ~pl[fmt!], ~pl[fmt1!], and ~pl[fms!] for
  versions of these functions that write forms to files in a manner that allows
  them to be read, by avoiding using backslash (~c[\\]) to break long lines.

  All three print a given string under an alist pairing character
  objects with values, interpreting certain ``tilde-directives'' in
  the string.  ~c[Channel] must be a character output channel (e.g.,
  ~ilc[*standard-co*]).
  ~bv[]
  General Forms:                                            result
  (fms string alist channel state evisc-tuple)         ; state
  (fmt string alist channel state evisc-tuple)         ; (mv col state)
  (fmt1 string alist column channel state evisc-tuple) ; (mv col state)
  ~ev[]
  ~ilc[Fms] and ~c[fmt] print an initial newline to put ~c[channel] in column ~c[0];
  ~ilc[Fmt1] requires the current column as input.  Columns are numbered
  from ~c[0].  The current column is the column into which the next
  character will be printed.  (Thus, the current column number is also
  the number of ~il[characters] printed since the last newline.)  The ~c[col]
  returned by ~c[fmt] and ~ilc[fmt1] is the current column at the conclusion of
  the formatting.  ~c[Evisc-tuple] must be either ~c[nil] (meaning no
  abbreviations are used when objects are printed) or an
  ``evisceration tuple'' such as that returned by
  ~c[(default-evisc-tuple state)].

  We list the tilde-directives below.  The notation is explained after
  the chart.
  ~bv[]
  ~~xx  pretty print vx (maybe after printing a newline)
  ~~yx  pretty print vx starting in current column; end with newline
  ~~Xxy like ~~xx but use vy as the evisceration tuple
  ~~Yxy like ~~yx but use vy as the evisceration tuple
  ~~px  pretty print term (maybe with infix) vx
       (maybe after printing a newline)
  ~~qx  pretty print term (maybe with infix) vx
       starting in current column; end with newline
  ~~Pxy like ~~px but use vy as the evisceration tuple
  ~~Qxy like ~~qx but use vy as the evisceration tuple
  ~~@x  if vx is a string, \"str\",  recursively format \"str\"
       if vx is (\"str\" . a), recursively format \"str\" under a+
  ~~#x~~[...~~/...~~/ ... ~~/...~~] cases on vx
       ^    ^     ...   ^  if 0<=vx<=k, choose vxth alternative
       0    1     ...   k  if vx is a list of length 1, case 0; else 1
  ~~*x  iterator: vx must be of the form
       (\"str0\" \"str1\" \"str2\" \"str3\" lst . a);
       if lst is initially empty, format \"str0\" under a+; otherwise,
       bind #\\* successively to the elements of lst and then
       recursively format \"stri\" under a+, where i=1 if there is one
       element left to process, i=2 if there are two left, and i=3
       otherwise.
  ~~&x  print elements of vx with ~~x, separated by commas and a
       final ``and''
  ~~vx  print elements of vx with ~~x, separated by commas and a
       final ``or''
  ~~nx  if vx is a small positive integer, print it as a word, e.g.,
       seven;
       if vx is a singleton containing a small positive integer, print
         the corresponding ordinal as a word, e.g., seventh
  ~~Nx  like ~~nx but the word is capitalized, e.g., Seven or Seventh
  ~~tx  tab out to column vx; newline first if at or past column vx
  ~~cx  vx is (n . w), print integer n right justified in field of
       width w
  ~~fx  print object vx flat over as many lines as necessary
  ~~Fx  same as ~~f, except that subsequent lines are indented to
       start one character to the right of the first character printed
  ~~sx  if vx is a symbol, print vx, breaking on hyphens; if vx is a
       string, print the characters in it, breaking on hyphens
  ~~    tilde space: print a space
  ~~_x  print vx spaces
  ~~
       tilde newline: skip following whitespace
  ~~%   output a newline
  ~~|   output a newline unless already on left margin
  ~~~~   print a tilde
  ~~-   if close to rightmargin, output a hyphen and newline; else
       skip this char
  ~ev[]
  If ~c[x] is a character, then ~c[vx] is the value of ~c[#\\x] under the
  current alist.  When we say ``format ~c[str] under ~c[a+]'' we mean
  recursively process the given string under an alist obtained by
  appending ~c[a] to the current alist.~/

  ACL2's formatting functions print to the indicated channel, keeping
  track of which column they are in.  ~ilc[Fmt1] can be used if the caller
  knows which column the channel is in (i.e., how many ~il[characters] have
  been printed since the last newline).  Otherwise, ~c[fmt] or ~ilc[fms] must be
  used, both of which output a newline so as to establish the column
  position at ~c[0].  Unlike Common Lisp's ~c[format] routine, ~c[fmt] and its
  relatives break the output into lines so as to try to avoid printing
  past column ~c[77].  That number is built-into the definitions of ACL2's
  formatting functions.  Line breaks are automatically inserted as
  necessary in place of spaces and after hyphens in the text being
  printed.

  The formatting functions scan the string from left to right,
  printing each successive character unless it is a tilde ~c[(~~)].  Upon
  encountering tildes the formatters take action determined by the
  character or ~il[characters] immediately following the tilde.  The
  typical tilde-directive is a group of three successive ~il[characters]
  from the string being printed.  For example, ~c[~~x0] is a 3 character
  ~c[tilde-directive].  The first character in a tilde-directive is always
  the tilde character itself.  The next character is called the
  ``command'' character.  The character after that is usually taken as
  the name of a ``format variable'' that is bound in the alist under
  which the string is being printed.  Format variables are, by
  necessity, ~il[characters].  The objects actually printed by a
  tilde-directive are the objects obtained by looking up the command's
  format variables in the alist.  Typical format variable names are ~c[0],
  ~c[1], ~c[2], ..., ~c[9], ~c[a], ~c[b], ~c[c], etc., and if a tilde-directive uses the
  format variable ~c[0], as in ~c[~~x0], then the character ~c[#\\0] must be bound
  in the alist.  Some tilde commands take no arguments and others take
  more than one, so some directives are of length two and others are
  longer.

  It should be noted that this use of ~il[characters] in the string to
  denote arguments is another break from Common Lisp's ~c[format] routine.
  In Common Lisp, the directives refer implicitly to the ``next item
  to be printed.''  But in ACL2 the directives name each item
  explicitly with our format variables.

  The following text contains examples that can be evaluated.  To make
  this process easier, we use a macro which is defined as part of ACL2
  just for this ~il[documentation].  The macro is named ~c[fmx] and it takes up
  to eleven arguments, the first of which is a format string, ~c[str], and
  the others of which are taken as the values of format variables.
  The variables used are ~c[#\\0] through ~c[#\\9].  The macro constructs an
  appropriate alist, ~c[a], and then evaluates
  ~c[(fmt str a *standard-co* state nil)].

  Thus,
  ~bv[]
  (fmx \"Here is v0, ~~x0, and here is v1, ~~x1.\"
       (cons 'value 0)
       (cons 'value 1))
  ~ev[]
  is just an abbreviation for
  ~bv[]
  (fmt \"Here is v0, ~~x0, and here is v1, ~~x1.\"
       (list (cons #\\0 (cons 'value 0))
             (cons #\\1 (cons 'value 1)))
       *standard-co*
       state
       nil)
  ~ev[]
  which returns ~c[(mv 53 state)] after printing the line
  ~bv[]
     Here is v0, (VALUE . 0), and here is v1, (VALUE . 1).
  ~ev[]
  We now devote special attention to three of the tilde-directives
  whose use is non-obvious.

  ~em[The Case Statement]

  ~c[~~#x] is essentially a ``case statement'' in the language of ~c[fmt].
  The proper form of the statement is
  ~bv[]
  ~~#x~~[case-0~~/case-1~~/ ... ~~/case-k~~],
  ~ev[]
  where each of the ~c[case-i] is a format string.  In the most common
  use, the variable ~c[x] has an integer value, ~c[vx], between ~c[0] and ~c[k],
  inclusive.  The effect of formatting the directive is to format
  ~c[case-vx].

  For example
  ~bv[]
  (fmx \"Go ~~#0~~[North~~/East~~/South~~/West~~].~~%\" 1)
  ~ev[]
  will print ``Go East.'' followed by a newline and will return

  ~c[(mv 0 state)], while if you change the ~c[1] above to ~c[3] (the
  maximum legal value), it will print ``Go West.''

  In order to make it easier to print such phrases as ``there are
  seven cases'' requiring agreement between subject and verb based on
  the number of elements of a list, the case statement allows its
  variable to take a list as its value and selects ~c[case-0] if the list
  has length ~c[1] and ~c[case-1] otherwise.
  ~bv[]
  (let ((cases '(a b c)))
    (fmx \"There ~~#0~~[is ~~n1 case~~/are ~~n1 cases~~].\"
         cases
         (length cases)))
  ~ev[]
  will print ``There are three cases.'' but if you change the

  ~c['(a b c)] above simply to ~c['(a)] it will print ``There is one
  case.'' and if you change it to ~c[nil] it will print ``There are
  zero cases.''

  ~em[Indirection]

  Roughly speaking, ~c[~~@] will act as though the value of its argument
  is a format string and splice it into the current string at the
  current position.  It is often used when the phrase to be printed
  must be computed.  For example,
  ~bv[]
  (let ((ev 'DEFUN))
   (fmx \"~~x0 is an event~~@1.\"
        'foo
        (if (member-eq ev '(defun defstub encapsulate))
            \" that may introduce a function symbol\"
            \"\")))
  ~ev[]
  will print ``~c[foo] is an event that may introduce a function
  symbol,'' but if the value of ~c[ev] is changed from ~c[']~ilc[defun] to ~c[']~ilc[defthm],
  it prints ``~c[foo] is an event.''  The ~c[~~@] directive ``splices'' in the
  computed phrase (which might be empty).  Of course, this particular
  example could be done with the case statement
  ~bv[]
  ~~#1~~[~~/ that may introduce a function symbol~~]
  ~ev[]
  where the value of ~c[#\\1] is appropriately computed to be ~c[0] or ~c[1].

  If the argument to ~c[~~@] is a pair, it is taken to be a format string
  ~ilc[cons]ed onto an alist, i.e., ~c[(\"str\" . a)], and the alist, ~c[a], is used
  to extend the current one before ~c[\"str\"] is recursively processed.
  This feature of ~c[fmt] can be used to pass around ``phrases'' that
  contain computed contextual information in ~c[a].  The most typical use
  is as ``error messages.''  For example, suppose you are writing a
  function which does not have access to ~ilc[state] and so cannot print an
  error message.  It may nevertheless be necessary for it to signal an
  error to its caller, say by returning two results, the first of
  which is interpreted as an error message if non-~c[nil].  Our convention
  is to use a ~c[~~@] pair to represent such messages.  For example, the
  error value might be produced by the code:
  ~bv[]
  (cons
    \"Error:  The instruction ~~x0 is illegal when the stack is ~~x1.~~%\"
    (list (cons #\\0 (current-instruction st))
          (cons #\\1 (i-stack st))))
  ~ev[]
  If the ~c[current-instruction] and ~c[i-stack] (whatever they are) are
  ~c['(popi 3)] and ~c['(a b)] when the ~ilc[cons] above is evaluated, then it
  produces
  ~bv[]
  '(\"Error:  The instruction ~~x0 is illegal when the stack is ~~x1.~~%\"
    (#\\0 POPI 3)
    (#\\1 A B))
  ~ev[]
  and if this pair is made the value of the ~c[fmt] variable ~c[0], then
  ~c[~~@0] will print
  ~bv[]
     Error:  The instruction (POPI 3) is illegal when the stack is (A B).
  ~ev[]
  For example, evaluate
  ~bv[]
  (let
   ((pair
    '(\"Error:  The instruction ~~x0 is illegal when the stack is ~~x1.~~%\"
      (#\\0 POPI 3)
      (#\\1 A B))))
   (fmx \"~~@0\" pair)).
  ~ev[]
  Thus, even though the function that produced the ``error'' could
  not print it, it could specify exactly what error message and data
  are to be printed.

  This example raises another issue.  Sometimes it is desirable to
  break lines in your format strings so as to make your source code
  more attractive.  That is the purpose of the ~c[tilde-newline]
  directive.  The following code produces exactly the same output as
  described above.
  ~bv[]
  (let ((pair '(\"Error:  The instruction ~~x0 ~~
                is illegal when the stack is ~~
                ~~x1.~~%\"
                (#\\0 POPI 3)
                (#\\1 A B))))
   (fmx \"~~@0\" pair)).
  ~ev[] 
  Finally, observe that when ~c[~~@0] extends the current alist, ~c[alist],
  with the one, ~c[a], in its argument, the bindings from ~c[a] are added to
  the front of ~c[alist], overriding the current values of any shared
  variables.  This ensures that the variable values seen by the
  recursively processed string, ~c[\"str\"], are those from ~c[a], but if
  ~c[\"str\"] uses variables not bound in ~c[a], their values are as specified
  in the original alist.  Intuitively, variables bound in ~c[a] are local
  to the processing of ~c[(\"str\" . a)] but ~c[\"str\"] may use ``global
  variables.''  The example above illustrates this because when the
  ~c[~~@0] is processed, ~c[#\\0] is bound to the error message pair.  But
  when the ~c[~~x0] in the error string is processed, ~c[#\\0] is bound to the
  illegal instruction.

  ~em[Iteration]

  The ~c[~~*] directive is used to process each element of a list.  For
  example,
  ~bv[]
  (let ((lst '(a b c d e f g h))) ; a true-list whose elements we exhibit
   (fmx \"~~*0\"
        `(\"Whoa!\"          ; what to print if there's nothing to print
          \"~~x*!\"           ; how to print the last element
          \"~~x* and \"       ; how to print the 2nd to last element
          \"~~x*, \"          ; how to print all other elements
          ,lst)))          ; the list of elements to print
  ~ev[] 
  will print ``~c[A, B, C, D, E, F, G and H!]''.  Try this example with
  other true list values of ~c[lst], such as ~c['(a b)], ~c['(a)], and ~c[nil].  The
  tilde-directives ~c[~~&0] and ~c[~~v0], which take a true list argument and
  display its elements separated by commas and a final ``and'' or
  ``or,'' are implemented in terms of the more general ~c[~~*].

  The ~c[~~*] directive allows the 5-tuple to specify in its final ~ilc[cdr] an
  alist with which to extend the current one before processing the
  individual elements.

  We often use ~c[~~*] to print a series of phrases, separated by suitable
  punctuation, whitespace and noise words.  In such use, the ~c[~~*]
  handles the separation of the phrases and each phrase is generally
  printed by ~c[~~@].

  Here is a complex example.  In the ~ilc[let*], below, we bind phrases to a
  list of ~c[~~@] pairs and then we create a ~c[~~*] 5-tuple to print out the
  conjunction of the phrases with a parenthetical ``finally!'' if the
  series is longer than 3.
  ~bv[]
  (let* ((phrases
          (list (list \"simplifying with the replacement rules ~~&0\"
                      (cons #\\0 '(rewrite-rule1 
                                  rewrite-rule2
                                  rewrite-rule3)))
                (list \"destructor elimination using ~~x0\"
                      (cons #\\0 'elim-rule))
                (list \"generalizing the terms ~~&0\"
                      (cons #\\0 '((rev x) (app u v))))
                (list \"inducting on ~~x0\"
                      (cons #\\0 'I))))
         (5-tuple
          (list
           \"magic\"                            ; no phrases
           \"~~@*\"                              ; last phrase
           \"~~@*, and~~#f~~[~~/ (finally!)~~] \"    ; second to last phrase
           \"~~@*, \"                            ; other phrases
           phrases                            ; the phrases themselves
           (cons #\\f 
                 (if (>(length phrases) 3) 1 0))))) ;print ``finally''?
    (fmx \"We did it by ~~*0.\" 5-tuple))
  ~ev[]
  This ~ilc[let*] prints
  ~bv[]
     We did it by simplifying with the replacement rules REWRITE-RULE1,
     REWRITE-RULE2 and REWRITE-RULE3, destructor elimination using ELIM-
     RULE, generalizing the terms (REV X) and (APP U V), and (finally!)
     inducting on I.
  ~ev[]
  You might wish to try evaluating the ~ilc[let*] after removing elements
  of phrases.

  Most of the output produced by ACL2 is produced via ~c[fmt] statements.
  Thus, inspection of the source code will yield many examples.  A
  complicated example is the code that explains the simplifier's work.
  See ~c[:]~ilc[pc] ~c[simplify-clause-msg1].  An ad hoc example is provided by the
  function ~c[fmt-doc-example], which takes two arguments: an arbitrary
  true list and ~ilc[state].  To see how ~c[fmt-doc-example] works, ~c[:]~ilc[pe]
  ~c[fmt-doc-example].
  ~bv[]
  (fmt-doc-example '(a b c d e f g h i j k l m n o p) state)
  ~ev[]
  will produce the output
  ~bv[]
     Here is a true list:  (A B C D E F G H I J K L M N O P).  It has 16
     elements, the third of which is C.

     We could print each element in square brackets:
     ([A], [B], [C], [D], [E], [F], [G], [H], [I], [J], [K], [L], [M], [N],
     [almost there: O], [the end: P]).  And if we wished to itemize them
     into column 15 we could do it like this
     0123456789012345
         0 (zeroth) A
         1 (first)  B
         2 (second) C
         3 (third)  D
         4 (fourth) E
         5 (fifth)  F
         6 (sixth)  G
         7 (seventh)
                    H
         8 (eighth) I
         9 (ninth)  J
        10 (tenth)  K
        11 (eleventh)
                    L
        12 (twelfth)
                    M
        13 (thirteenth)
                    N
        14 (14th)   O
        15 (15th)   P
     End of example.
  ~ev[]
  and return ~c[(mv 15 state)].

  Finally, we should remind the reader that ~c[fmt] and its subfunctions,
  most importantly ~c[fmt0], are written entirely in ACL2.  We make this
  comment for two reasons.  First, it illustrates the fact that quite
  low level code can be efficiently written in the language.  Second,
  it means that as a last resort for documentation purposes you can
  read the source code without changing languages."

  (the2s
   (signed-byte 29)
   (pprogn
    (newline channel state)
    (fmt1 str alist 0 channel state evisc-tuple))))

(defun fms (str alist channel state evisc-tuple)

; WARNING: The master copy of the tilde-directives list is in :DOC fmt.

  ":Doc-Section ACL2::Programming

  ~c[:(str alist co-channel state evisc) => state]~/

  ~l[fmt] for further explanation, including documentation of the
  tilde-directives.~/~/"

  (pprogn
   (newline channel state)
   (mv-let (col state)
           (fmt1 str alist 0 channel state evisc-tuple)
           (declare (ignore col))
           state)))

(defun fmt1! (str alist col channel state evisc-tuple)

; WARNING: The master copy of the tilde-directives list is in :DOC fmt.

  ":Doc-Section ACL2::Programming

  ~c[:(str alist col channel state evisc) => (mv col state)]~/

  This function is nearly identical to ~c[fmt1]; ~pl[fmt1].  The only
  difference is that ~c[fmt1] may insert backslash (\\) characters when
  forced to print past the right margin in order to make the output a
  bit clearer in that case.  Use ~c[fmt1!] instead if you want to be able
  to read the forms back in.~/~/"

  (mv-let (erp col state)
          (state-global-let*
           ((write-for-read t))
           (mv-let (col state)
                   (fmt1 str alist col channel state evisc-tuple)
                   (mv nil col state)))
          (declare (ignore erp))
          (mv col state)))

(defun fmt! (str alist channel state evisc-tuple)

; WARNING: The master copy of the tilde-directives list is in :DOC fmt.

  ":Doc-Section ACL2::Programming

  ~c[:(str alist co-channel state evisc) => state]~/

  This function is nearly identical to ~c[fmt]; ~pl[fmt].  The only
  difference is that ~c[fmt] may insert backslash (\\) characters when
  forced to print past the right margin in order to make the output a
  bit clearer in that case.  Use ~c[fmt!] instead if you want to be able
  to read the forms back in.~/~/"

  (mv-let (erp col state)
          (state-global-let*
           ((write-for-read t))
           (mv-let (col state)
                   (fmt str alist channel state evisc-tuple)
                   (mv nil col state)))
          (declare (ignore erp))
          (mv col state)))

(defun fms! (str alist channel state evisc-tuple)

; WARNING: The master copy of the tilde-directives list is in :DOC fmt.

  ":Doc-Section ACL2::Programming

  ~c[:(str alist co-channel state evisc) => state]~/

  This function is nearly identical to ~c[fms]; ~pl[fms].  The only
  difference is that ~c[fms] may insert backslash (\\) characters when
  forced to print past the right margin in order to make the output a
  bit clearer in that case.  Use ~c[fms!] instead if you want to be able
  to read the forms back in.~/~/"

  (mv-let (erp val state)
          (state-global-let*
           ((write-for-read t))
           (pprogn (fms str alist channel state evisc-tuple)
                   (mv nil nil state)))
          (declare (ignore erp val))
          state))

(defmacro fmx (str &rest args)
  (declare (xargs :guard (<= (length args) 10)))
  `(fmt ,str ,(make-fmt-bindings '(#\0 #\1 #\2 #\3 #\4
                                   #\5 #\6 #\7 #\8 #\9)
                                 args)
        *standard-co* state nil))

(defun fmt-doc-example1 (lst i)
  (cond ((null lst) nil)
        (t (cons (cons "~c0 (~n1)~tc~y2~|"
                       (list (cons #\0 (cons i 5))
                             (cons #\1 (list i))
                             (cons #\2 (car lst))))
                 (fmt-doc-example1 (cdr lst) (1+ i))))))

(defun fmt-doc-example (x state)
  (fmt "Here is a true list:  ~x0.  It has ~#1~[no elements~/a single ~
        element~/~n2 elements~], ~@3~%~%We could print each element in square ~
        brackets:~%(~*4).  And if we wished to itemize them into column 15 we ~
        could do it like this~%0123456789012345~%~*5End of example."
       (list (cons #\0 x)
             (cons #\1 (cond ((null x) 0) ((null (cdr x)) 1)(t 2)))
             (cons #\2 (length x))
             (cons #\3 (cond ((< (length x) 3) "and so we can't print the third one!")
                             (t (cons "the third of which is ~x0."
                                      (list (cons #\0 (caddr x)))))))
             (cons #\4 (list "[empty]"
                             "[the end: ~y*]"
                             "[almost there: ~y*], "
                             "[~y*], "
                             x))
             (cons #\5 (list* "" "~@*" "~@*" "~@*"
                              (fmt-doc-example1 x 0)
                              (list (cons #\c 15)))))
         *standard-co* state nil))

(defconst *fmt-ctx-spacers*
  '(defun
     #+:non-standard-analysis defun-std
     mutual-recursion
     defuns
     defthm
     #+:non-standard-analysis defthm-std
     defaxiom
     defconst
     defstobj
     defpkg
     deflabel
     defdoc
     deftheory
     defchoose
     verify-guards
     verify-termination
     defmacro
     in-theory
     in-arithmetic-theory
     push-untouchable
     remove-untouchable
     reset-prehistory
     set-body
     table
     encapsulate
     include-book))

(defun fmt-ctx (ctx col channel state)

; We print the context in which an error has occurred.  If infix printing is
; being used (infixp = t or :out) then ctx is just the event form itself and we
; print it with evisceration.  Otherwise, we are more efficient in our choice
; of ctx and we interpret it according to its type, to make it convenient to
; construct the more common contexts.  If ctx is nil, we print nothing.  If ctx
; is a symbol, we print it from #\0 via "~x0".  If ctx is a pair whose car is a
; symbol, we print its car and cdr from #\0 and #\1 respectively with "(~x0 ~x1
; ...)".  Otherwise, we print it from #\0 with "~@0".

; We print no other words, spaces or punctuation.  We return the new
; col and state.

  (declare (type (signed-byte 29) col))
  (the2s
   (signed-byte 29)
   (cond ((output-in-infixp state)
          (fmt1 "~p0"
                (list (cons #\0 ctx))
                col channel state 
                (evisc-tuple 1 2 nil nil)))
         ((null ctx)
          (mv col state))
         ((symbolp ctx)
          (fmt1 "~x0" (list (cons #\0 ctx)) col channel state nil))
         ((and (consp ctx)
               (symbolp (car ctx)))
          (fmt1 "(~@0~x1 ~x2 ...)"
                (list (cons #\0
                            (if (member-eq (car ctx) *fmt-ctx-spacers*) " " ""))
                      (cons #\1 (car ctx))
                      (cons #\2 (cdr ctx)))
                col channel state nil))
         (t (fmt1 "~@0" (list (cons #\0 ctx)) col channel state nil)))))

(defun fmt-in-ctx (ctx col channel state)

; We print the phrase " in ctx:  ", if ctx is non-nil, and return
; the new col and state.

  (declare (type (signed-byte 29) col))
  (the2s
   (signed-byte 29)
   (cond ((null ctx)
          (fmt1 ":  " nil col channel state nil))
         (t (mv-let (col state)
                    (fmt1 " in " nil col channel state nil)
                    (mv-let (col state)
                            (fmt-ctx ctx col channel state)
                            (fmt1 ":  " nil col channel state nil)))))))

(defun error-fms (hardp ctx str alist state)

; This function prints the "ACL2 Error" banner and ctx, then the
; user's str and alist, and then two carriage returns.  It returns state.

; Historical Note about ACL2

; Once upon a time we accomplished all this with something like: "ACL2
; Error (in ~xc): ~@s~%~%" and it bound #\c and #\s to ctx and str in
; alist.  That suffers from the fact that it may overwrite the user's
; bindings of #\c and #\s -- unlikely if this error call was generated
; by our er macro.  We rewrote the function this way simply so we
; would not have to remember that some variables are special.

  (let ((channel (f-get-global 'standard-co state)))
    (mv-let (col state)
            (fmt1 (if hardp
                      "~%~%HARD ACL2 ERROR"
                    "~%~%ACL2 Error")
                  nil 0 channel state nil)
            (mv-let (col state)
                    (fmt-in-ctx ctx col channel state)
                    (mv-let (col state)
                            (fmt1 str alist col channel state
                                  (default-evisc-tuple state))
                            (mv-let (col state)
                                    (fmt1 "~%~%" nil col channel state nil)
                                    (declare (ignore col))
                                    state))))))

(defun push-warning-frame (state)
  (f-put-global 'accumulated-warnings
                (cons nil (f-get-global 'accumulated-warnings state))
                state))

(defun absorb-frame (lst stk)
  (if (consp stk)
      (cons (union-equal lst (car stk))
            (cdr stk))
    stk))

(defun pop-warning-frame (accum-p state)

; When a "compound" event has a "sub-event" that generates warnings, we want
; the warning strings from the sub-event's summary to appear in the parent
; event's summary.  Accum-p should be nil if and only if the sub-event whose
; warning frame we are popping had its warnings suppressed.

  (let ((stk (f-get-global 'accumulated-warnings state)))
    (cond ((consp stk)
           (pprogn
            (f-put-global 'accumulated-warnings
                          (if accum-p
                              (absorb-frame (car stk) (cdr stk))
                            (cdr stk))
                          state)
            (mv (car stk) state)))
          (t (mv (er hard 'pop-warning-frame
                     "The 'accumulated-warnings stack is empty.")
                 state)))))

(defun push-warning (summary state)
  (let ((stk (f-get-global 'accumulated-warnings state)))
    (cond ((consp stk)
           (f-put-global 'accumulated-warnings
                         (cons (add-to-set-equal summary (car stk))
                               (cdr stk))
                         state))
          (t 

; We used to cause an error, shown below, in this situation.  But WARNINGs are
; increasingly used by non-events, such as :trans and (thm ...) and rather than
; protect them all with push-warning-frame/pop-warning-frame we are just
; adopting the policy of not pushing warnings if the stack isn't set up for
; them.  Here is the old code.

;            (prog2$ (er hard 'push-warning
;                        "The 'accumulated-warnings stack is empty but we were ~
;                         asked to add ~x0 to the top frame."
;                        summary)
;                     state)

           state))))

(defun member-string-equal (str lst)
  (cond
   ((endp lst) nil)
   (t (or (string-equal str (car lst))
          (member-string-equal str (cdr lst))))))

(defabbrev flambda-applicationp (term)

; Term is assumed to be nvariablep.

  (consp (car term)))

(defabbrev lambda-applicationp (term)
  (and (consp term)
       (flambda-applicationp term)))

(defabbrev flambdap (fn)

; Fn is assumed to be the fn-symb of some term.

  (consp fn))

(defabbrev lambda-formals (x) (cadr x))

(defabbrev lambda-body (x) (caddr x))

(defabbrev make-lambda (args body)
  (list 'lambda args body))

(defabbrev make-let (bindings body)
  (list 'let bindings body))

(defmacro er-let* (alist body)

; This macro introduces the variable er-let-star-use-nowhere-else.
; The user who uses that variable in his forms is likely to be
; disappointed by the fact that we rebind it.

  (declare (xargs :guard (alistp alist)))
  (cond ((null alist)
         (list 'check-vars-not-free
               '(er-let-star-use-nowhere-else)
               body))
        (t (list 'mv-let
                 (list 'er-let-star-use-nowhere-else
                       (caar alist)
                       'state)
                 (cadar alist)
                 (list 'cond
                       (list 'er-let-star-use-nowhere-else
                             (list 'mv
                                   'er-let-star-use-nowhere-else
                                   (caar alist)
                                   'state))
                       (list t (list 'er-let* (cdr alist) body)))))))

(defmacro match (x pat)
  (list 'case-match x (list pat t)))

(defmacro match! (x pat)
  (list 'or (list 'case-match x
                  (list pat '(value nil)))
        (list 'er 'soft nil
              "The form ~x0 was supposed to match the pattern ~x1."
              x (kwote pat))))

#+:non-standard-analysis
(defconst *non-standard-primitives*
  '(standard-numberp
    standard-part
    i-large-integer))

(defun def-basic-type-sets1 (lst i)
  (declare (xargs :guard (and (integerp i)
                              (true-listp lst))))
  (cond ((null lst) nil)
        (t (cons (list 'defconst (car lst) (list 'the-type-set (expt 2 i)))
                 (def-basic-type-sets1 (cdr lst) (+ i 1))))))

(defmacro def-basic-type-sets (&rest lst)
  (let ((n (length lst)))
    `(progn
       (defconst *actual-primitive-types* ',lst)
       (defconst *min-type-set* (- (expt 2 ,n)))
       (defconst *max-type-set* (- (expt 2 ,n) 1))
       (defmacro the-type-set (x)

; Warning: Keep this definition in sync with the type declaration in
; ts-subsetp0 and ts-subsetp.

         `(the (integer ,*min-type-set* ,*max-type-set*) ,x))
       ,@(def-basic-type-sets1 lst 0))))

(defun list-of-the-type-set (x)
  (cond ((consp x)
         (cons (list 'the-type-set (car x))
               (list-of-the-type-set (cdr x))))
        (t nil)))

(defmacro ts= (a b)
  (list '= (list 'the-type-set a) (list 'the-type-set b)))

; We'll create fancier versions of ts-complement0, ts-union0, and
; ts-intersection0 once we have defined the basic type sets.

(defmacro ts-complement0 (x)
  (list 'the-type-set (list 'lognot (list 'the-type-set x))))

(defmacro ts-complementp (x)
  (list 'minusp x))

(defun ts-union0-fn (x)
  (list 'the-type-set
        (cond ((null x) '*ts-empty*)
              ((null (cdr x)) (car x))
              (t (xxxjoin 'logior
                          (list-of-the-type-set x))))))

(defmacro ts-union0 (&rest x)
  (declare (xargs :guard (true-listp x)))
  (ts-union0-fn x))

(defmacro ts-intersection0 (&rest x)
  (list 'the-type-set
        (cons 'logand (list-of-the-type-set x))))

(defmacro ts-intersectp (&rest x)
  (list 'not (list 'ts= (cons 'ts-intersection x) '*ts-empty*)))

; We do not define ts-subsetp0, both because we don't need it and because if we
; do define it, we will be tempted to add the declaration found in ts-subsetp,
; yet we have not yet defined *min-type-set* or *max-type-set*.

(defun ts-builder-case-listp (x)

; A legal ts-builder case list is a list of the form
;    ((key1 val1 ...) (key2 val2 ...) ... (keyk valk ...))
; where none of the keys is 'otherwise or 't except possibly keyk and
; every key is a symbolp if keyk is 'otherwise or 't.

; This function returns t, nil, or 'otherwise.  A non-nil value means
; that x is a legal ts-builder case list.  If it returns 'otherwise,
; it means keyk is an 'otherwise or a 't clause.  That aspect of the
; function is not used outside of its definition, but it is used in
; the definition below.

; If keyk is an 'otherwise or 't then each of the other keys will
; occur twice in the expanded form of the ts-builder expression and
; hence those keys must all be symbols.

  (cond ((atom x) (eq x nil))
        ((and (consp (car x))
              (true-listp (car x))
              (not (null (cdr (car x)))))
         (cond ((or (eq t (car (car x)))
                    (eq 'otherwise (car (car x))))
                (cond ((null (cdr x)) 'otherwise)
                      (t nil)))
               (t (let ((ans (ts-builder-case-listp (cdr x))))
                    (cond ((eq ans 'otherwise)
                           (cond ((symbolp (car (car x)))
                                  'otherwise)
                                 (t nil)))
                          (t ans))))))
        (t nil)))

(defun ts-builder-macro1 (x case-lst seen)
  (declare (xargs :guard (and (symbolp x)
                              (ts-builder-case-listp case-lst))))
  (cond ((null case-lst) nil)
        ((or (eq (caar case-lst) t)
             (eq (caar case-lst) 'otherwise))
         (sublis (list (cons 'x x)
                       (cons 'seen seen)
                       (cons 'ts2 (cadr (car case-lst))))
                 '((cond ((ts-intersectp x (ts-complement0 (ts-union0 . seen)))
                          ts2)
                         (t *ts-empty*)))))
        (t (cons (sublis (list (cons 'x x)
                               (cons 'ts1 (caar case-lst))
                               (cons 'ts2 (cadr (car case-lst))))
                         '(cond ((ts-intersectp x ts1) ts2)
                                (t *ts-empty*)))
                 (ts-builder-macro1 x (cdr case-lst) (cons (caar case-lst)
                                                           seen))))))

(defun ts-builder-macro (x case-lst)
  (declare (xargs :guard (and (symbolp x)
                              (ts-builder-case-listp case-lst))))
  (cons 'ts-union
        (ts-builder-macro1 x case-lst nil)))

(defmacro ts-builder (&rest args)
  #|
  (declare (xargs :guard (and (consp args)
                       (symbolp (car args))
                       (ts-builder-case-listp (cdr args)))))
  |#
  (ts-builder-macro (car args) (cdr args)))

(defun standard-guard (sym)
  (cond ((symbolp sym)
         (let* ((str (symbol-name sym))
                (c (cond ((int= (length str) 0) nil)
                         (t (char str 0)))))
           (case c
                 ((#\I #\J #\K #\M #\N) (list 'integerp sym))
                 (#\L (list 'true-listp sym))
                 (otherwise
                  (cond ((eql (string<= "TERM-L" str) 6)
                         (list 'pseudo-term-listp sym))
                        ((eql (string<= "TERM" str) 4)
                         (list 'pseudo-termp sym))
                        ((eql (string<= "SYM" str) 3)
                         (list 'symbolp sym))
                        ((eql (string<= "STR" str) 3)
                         (list 'stringp sym))
                        ((eql (string<= "ALIST" str) 5)
                         (list 'alistp sym))
                        (t nil))))))
        (t nil)))

(defun standard-guard-lst (lst)
  (cond ((atom lst)
         (cond ((eq lst nil) nil)
               (t (list nil))))
        (t (cons (standard-guard (car lst))
                 (standard-guard-lst (cdr lst))))))

(defmacro std (&rest x)
  (let ((hyps (standard-guard-lst x)))
    (cond ((member nil hyps)
           (er hard 'std
               "The standard guard macro std was given an argument ~
                list, ~x0, which it did not recognize.  Some element ~
                of the list does not have a standard type associated ~
                with its name."
               x))
          (t (cons 'and hyps)))))

(defabbrev strip-not (term)

; A typical use of this macro is:
; (mv-let (not-flg atm) (strip-not term)
;         ...body...)
; which has the effect of binding not-flg to T and atm to x if term
; is of the form (NOT x) and binding not-flg to NIL and atm to term
; otherwise.

  (cond ((and (nvariablep term)
;             (nquotep term)
              (eq (ffn-symb term) 'not))
         (mv t (fargn term 1)))
        (t (mv nil term))))

; The ACL2 Record Facilities

; Our record facility gives us the ability to declare "new" types of
; structures which are represented as lists.  If desired the lists
; are tagged with the name of the new record type.  Otherwise they are
; not tagged and are called "cheap" records.

; The expression (DEFREC SHIP (X . Y) NIL) declares SHIP to
; be a tagged (non-cheap) record of two components X and Y.  An
; example concrete SHIP is '(SHIP 2 . 4).  Note that cheapness refers
; only to whether the record is tagged and whether the tag is tested
; upon access and change, not whether the final cdr is used.

; To make a ship:  (MAKE SHIP :X x :Y y) or (MAKE SHIP :Y y :X x).
; To access the Xth component of the ship object obj: (ACCESS SHIP obj :X).
; To change the Xth component to val: (CHANGE SHIP obj :X val).
; Note the use of keywords in these forms.

; It is possible to change several fields at once, e.g.,
; (CHANGE SHIP obj :X val-x :Y val-y).  In general, to cons up a changed
; record one only does the conses necessary.

; The implementation of records is as follows.  DEFREC expands
; into a collection of macro definitions for certain generated function
; symbols.  In the example above we define the macros:

; |Make SHIP record|
; |Access SHIP record field X|
; |Access SHIP record field Y|
; |Change SHIP record fields|

; The macro expression (MAKE SHIP ...) expands to a call of the first
; function.  (ACCESS SHIP ... :X) expands to a call of the second.
; (CHANGE SHIP obj :X val-x :Y val-y) expands to
; (|Change SHIP record fields| obj :X val-x :Y val-y).

; The five new symbols above are defined as macros that further expand
; into raw CAR/CDR nests if the record is cheap and a similar nest
; that first checks the type of the record otherwise.

; In using the record facility I have sometimes pondered which fields I should
; allocate where to maximize access speed.  Other times I have just laid them
; out in an arbitrary fashion.  In any case, the following functions might be
; useful if you are wondering how to lay out a record.  That is, grab the
; following progn and execute it in the full ACL2 system.  (It cannot be
; executed at this point in basis.lisp because it uses functions defined
; elsewhere; it is here only to be easy to find when looking up the comments
; about records.)  Note that it changes the default-defun-mode to :program.  Then
; invoke :sbt n, where n is an integer.

; For example
; ACL2 g>:sbt 5

; The Binary Trees with Five Tips
; 2.400  ((2 . 2) 2 3 . 3)
; 2.600  (1 (3 . 3) 3 . 3)
; 2.800  (1 2 3 4 . 4)

; Sbt will print out all of the interesting binary trees with the
; given number of tips.  The integer appearing at a tip is the number
; of car/cdrs necessary to access that field of a cheap record laid
; out as shown.  That is also the number of conses required to change
; that single field.  The decimal number in the left column is the
; average number of car/cdrs required to access a field, assuming all
; fields are accessed equally often.  The number of trees generated
; grows exponentially with n.  Roughly 100 trees are printed for size
; 10.  Beware!

; The function (analyze-tree x state) is also helpful.  E.g.,

; ACL2 g>(analyze-tree '((type-alist . term) cl-ids rewrittenp
;                          force-flg . rune-or-non-rune)
;                        state)

; Shape:  ((2 . 2) 2 3 4 . 4)
; Field Depths:  
; ((TYPE-ALIST . 2)
;  (TERM . 2)
;  (CL-IDS . 2)
;  (REWRITTENP . 3)
;  (FORCE-FLG . 4)
;  (RUNE-OR-NON-RUNE . 4))
; Avg Depth:  2.833

#|
(progn
  (program)
  (defun bump-binary-tree (tree)
    (cond ((atom tree) (1+ tree))
          (t (cons (bump-binary-tree (car tree))
                   (bump-binary-tree (cdr tree))))))

  (defun cons-binary-trees (t1 t2)
    (cons (bump-binary-tree t1) (bump-binary-tree t2)))

  (defun combine-binary-trees1 (t1 lst2 ans)
    (cond ((null lst2) ans)
          (t (combine-binary-trees1 t1 (cdr lst2)
                                    (cons (cons-binary-trees t1 (car lst2))
                                          ans)))))

  (defun combine-binary-trees (lst1 lst2 ans)
    (cond
     ((null lst1) ans)
     (t (combine-binary-trees (cdr lst1)
                              lst2
                              (combine-binary-trees1 (car lst1) lst2 ans)))))

  (mutual-recursion

   (defun all-binary-trees1 (i n)
     (cond ((= i 0) nil)
           (t (revappend (combine-binary-trees (all-binary-trees i)
                                               (all-binary-trees (- n i))
                                               nil)
                         (all-binary-trees1 (1- i) n)))))

   (defun all-binary-trees (n)
     (cond ((= n 1) (list 0))
           (t (all-binary-trees1 (floor n 2) n))))
   )

  (defun total-access-time-binary-tree (x)
    (cond ((atom x) x)
          (t (+ (total-access-time-binary-tree (car x))
                (total-access-time-binary-tree (cdr x))))))

  (defun total-access-time-binary-tree-lst (lst)

; Pairs each tree in lst with its total-access-time.

    (cond ((null lst) nil)
          (t (cons (cons (total-access-time-binary-tree (car lst))
                         (car lst))
                   (total-access-time-binary-tree-lst (cdr lst))))))

  (defun show-binary-trees1 (n lst state)
    (cond ((null lst) state)
          (t (let* ((tat (floor (* (caar lst) 1000) n))
                    (d0 (floor tat 1000)) 
                    (d1 (- (floor tat 100) (* d0 10)))
                    (d2 (- (floor tat 10) (+ (* d0 100) (* d1 10))))
                    (d3 (- tat (+ (* d0 1000) (* d1 100) (* d2 10)))))

               (pprogn
                (mv-let (col state)
                        (fmt1 "~x0.~x1~x2~x3  ~x4~%"
                              (list (cons #\0 d0)
                                    (cons #\1 d1)
                                    (cons #\2 d2)
                                    (cons #\3 d3)
                                    (cons #\4 (cdar lst)))
                              0
                              *standard-co* state nil)
                        (declare (ignore col))
                        state)
                (show-binary-trees1 n (cdr lst) state))))))

  (defun show-binary-trees (n state)
    (let ((lst (reverse
                (merge-sort-car->
                 (total-access-time-binary-tree-lst
                  (all-binary-trees n))))))
      (pprogn
       (fms "The Binary Trees with ~N0 Tips~%"
            (list (cons #\0 n))
            *standard-co* state nil)
       (show-binary-trees1 n lst state))))

  (defun analyze-tree1 (x i)
    (cond ((atom x) i)
          (t (cons (analyze-tree1 (car x) (1+ i))
                   (analyze-tree1 (cdr x) (1+ i))))))

  (defun analyze-tree2 (x i)
    (cond ((atom x) (list (cons x i)))
          (t (append (analyze-tree2 (car x) (1+  i))
                     (analyze-tree2 (cdr x) (1+  i))))))

  (defun analyze-tree3 (x)
    (cond ((atom x) 1)
          (t (+ (analyze-tree3 (car x)) (analyze-tree3 (cdr x))))))

  (defun analyze-tree (x state)
    (let* ((binary-tree (analyze-tree1 x 0))
           (alist (analyze-tree2 x 0))
           (n (analyze-tree3 x))
           (k (total-access-time-binary-tree binary-tree)))
      (let* ((tat (floor (* k 1000) n))
             (d0 (floor tat 1000)) 
             (d1 (- (floor tat 100) (* d0 10)))
             (d2 (- (floor tat 10) (+ (* d0 100) (* d1 10))))
             (d3 (- tat (+ (* d0 1000) (* d1 100) (* d2 10)))))
        (pprogn
         (fms "Shape:  ~x0~%Field Depths:  ~x1~%Avg Depth:  ~x2.~x3~x4~x5~%"
              (list (cons #\0 binary-tree)
                    (cons #\1 alist)
                    (cons #\2 d0)
                    (cons #\3 d1)
                    (cons #\4 d2)
                    (cons #\5 d3))
              *standard-co* state nil)
         (value :invisible)))))

  (defmacro sbt (n) `(pprogn (show-binary-trees ,n state) (value :invisible))))

|#


(defun record-maker-function-name (name)
  (intern-in-package-of-symbol
   (coerce (append (coerce "Make " 'list)
                   (coerce (symbol-name name) 'list)
                   (coerce " record" 'list))
           'string)
   name))

(defun record-accessor-function-name (name field)
  (intern-in-package-of-symbol
   (coerce
    (append (coerce "Access " 'list)
            (coerce (symbol-name name) 'list)
            (coerce " record field " 'list)
            (coerce (symbol-name field) 'list))
    'string)
   name))

(defun record-changer-function-name (name)
  (intern-in-package-of-symbol
   (coerce
    (append (coerce "Change " 'list)
            (coerce (symbol-name name) 'list)
            (coerce " record fields" 'list))
    'string)
   name))

(defmacro make (&rest args)
  (cond ((keyword-value-listp (cdr args))
         (cons (record-maker-function-name (car args)) (cdr args)))
        (t (er hard 'record-error
               "Make was given a non-keyword as a field specifier.  ~
                The offending form is ~x0."
               (cons 'make args)))))

(defmacro access (name rec field)
  (cond ((keywordp field)
         (list (record-accessor-function-name name field)
               rec))
        (t (er hard 'record-error
               "Access was given a non-keyword as a field ~
                specifier.  The offending form was ~x0."
               (list 'access name rec field)))))

(defmacro change (&rest args)
  (cond ((keyword-value-listp (cddr args))
         (cons (record-changer-function-name (car args)) (cdr args)))
        (t (er hard 'record-error
               "Change was given a non-keyword as a field specifier.  ~
                The offending form is ~x0."
               (cons 'change args)))))

(defun record-error (name rec)
  (er hard 'record-error
      "An attempt was made to treat ~x0 as a record of type ~x1."
      rec name))

(defun make-record-car-cdrs1 (lst var)
  (cond ((null lst) var)
        (t (list (car lst) (make-record-car-cdrs1 (cdr lst) var)))))

(defun make-record-car-cdrs (field-layout car-cdr-lst)
  (cond ((atom field-layout)
         (cond ((null field-layout) nil)
               (t (list (make-record-car-cdrs1 car-cdr-lst field-layout)))))
        (t (append (make-record-car-cdrs (car field-layout)
                                         (cons 'car car-cdr-lst))
                   (make-record-car-cdrs (cdr field-layout)
                                         (cons 'cdr car-cdr-lst))))))

(defun make-record-accessors (name field-lst car-cdrs cheap)
  (cond ((null field-lst) nil)
        (t
         (cons (cond
                (cheap
                 (list 'defabbrev
                       (record-accessor-function-name name (car field-lst))
                       (list (car field-lst))
                       (car car-cdrs)))
                (t (list 'defabbrev
                         (record-accessor-function-name name (car field-lst))
                         (list (car field-lst))
                         (sublis (list (cons 'name name)
                                       (cons 'x (car field-lst))
                                       (cons 'z (car car-cdrs)))
                                 '(prog2$ (or (and (consp x)
                                                   (eq (car x) (quote name)))
                                              (record-error (quote name) x))
                                          z)))))
               (make-record-accessors name
                                      (cdr field-lst)
                                      (cdr car-cdrs)
                                      cheap)))))

(defun symbol-name-tree-occur (sym sym-tree)

; Sym is a symbol -- in fact, a keyword in proper usage -- and
; sym-tree is a tree of symbols.  We ask whether a symbol with
; the same symbol-name as key occurs in sym-tree.  If so, we return
; that symbol.  Otherwise we return nil.

  (cond ((symbolp sym-tree)
         (cond ((equal (symbol-name sym) (symbol-name sym-tree))
                sym-tree)
               (t nil)))
        ((atom sym-tree)
         nil)
        (t (or (symbol-name-tree-occur sym (car sym-tree))
               (symbol-name-tree-occur sym (cdr sym-tree))))))

(defun some-symbol-name-tree-occur (syms sym-tree)
  (cond ((null syms) nil)
        ((symbol-name-tree-occur (car syms) sym-tree) t)
        (t (some-symbol-name-tree-occur (cdr syms) sym-tree))))

(defun make-record-changer-cons (fields field-layout x)

; Fields is the list of keyword field specifiers that are being
; changed.  Field-layout is the user's layout of the record.  X is the
; name of the variable holding the instance of the record.

  (cond ((not (some-symbol-name-tree-occur fields field-layout))
         x)
        ((atom field-layout)
         field-layout)
        (t
         (list 'cons
               (make-record-changer-cons fields
                                         (car field-layout)
                                         (list 'car x))
               (make-record-changer-cons fields
                                         (cdr field-layout)
                                         (list 'cdr x))))))

(defun make-record-changer-let-bindings (field-layout lst)

; Field-layout is the symbol tree provided by the user describing the
; layout of the fields.  Lst is the keyword/value list in a change
; form.  We want to bind each field name to the corresponding value.
; The only reason we take field-layout as an argument is that we
; don't know from :key which package 'key is in.

  (cond ((null lst) nil)
        (t (let ((var (symbol-name-tree-occur (car lst) field-layout)))
             (cond ((null var)
                    (er hard 'record-error
                        "A make or change form has used ~x0 as though ~
                         it were a legal field specifier in a record ~
                         with the layout ~x1."
                        (car lst)
                        field-layout))
                   (t
                    (cons (list var (cadr lst))
                          (make-record-changer-let-bindings field-layout
                                                            (cddr lst)))))))))

(defun make-record-changer-let (name field-layout cheap rec lst)
  (cond
   (cheap
    (list 'let (cons (list 'record-changer-not-to-be-used-elsewhere rec)
                     (make-record-changer-let-bindings field-layout lst))
          (make-record-changer-cons
           (evens lst)
           field-layout
           'record-changer-not-to-be-used-elsewhere)))
   (t
    (list 'let (cons (list 'record-changer-not-to-be-used-elsewhere rec)
                     (make-record-changer-let-bindings field-layout lst))
          (sublis
           (list (cons 'name name)
                 (cons 'cons-nest
                       (make-record-changer-cons
                        (evens lst)
                        field-layout
                        '(cdr record-changer-not-to-be-used-elsewhere))))
           '(prog2$ (or (and (consp record-changer-not-to-be-used-elsewhere)
                             (eq (car record-changer-not-to-be-used-elsewhere)
                                 (quote name)))
                        (record-error (quote name)
                                      record-changer-not-to-be-used-elsewhere))
                    (cons (quote name) cons-nest)))))))

(defun make-record-changer (name field-layout cheap)
  (list 'defmacro
        (record-changer-function-name name)
        '(&rest args)
        (list 'make-record-changer-let
              (kwote name)
              (kwote field-layout)
              cheap
              '(car args)
              '(cdr args))))

(defun make-record-maker-cons (fields field-layout)

; Fields is the list of keyword field specifiers being initialized in
; a record.  Field-layout is the user's specification of the layout.
; We lay down a cons tree isomorphic to field-layout whose tips are
; either the corresponding tip of field-layout or nil according to
; whether the keyword corresponding to the field-layout tip is in fields.

  (cond ((atom field-layout)
         (cond ((some-symbol-name-tree-occur fields field-layout)

; The above call is a little strange isn't it?  Field-layout is an
; atom, a symbol really, and here we are asking whether any element of
; fields symbol-name-tree-occurs in it.  We're really just exploiting
; some-symbol-name-tree-occur to walk down fields for us taking the
; symbol-name of each element and seeing if it occurs in (i.e., in
; this case, is) the symbol name of field-layout.

                field-layout)
               (t nil)))
        (t
         (list 'cons
               (make-record-maker-cons fields
                                       (car field-layout))
               (make-record-maker-cons fields
                                       (cdr field-layout))))))

(defun make-record-maker-let (name field-layout cheap lst)
  (cond
   (cheap
    (list 'let (make-record-changer-let-bindings field-layout lst)
          (make-record-maker-cons (evens lst)
                                  field-layout)))
   (t
    (list 'let (make-record-changer-let-bindings field-layout lst)
          (list 'cons
                (kwote name)
                (make-record-maker-cons (evens lst)
                                        field-layout))))))

(defun make-record-maker (name field-layout cheap)
  (list 'defmacro
        (record-maker-function-name name)
        '(&rest args)
        (list 'make-record-maker-let
              (kwote name)
              (kwote field-layout)
              cheap
              'args)))

(defun make-record-field-lst (field-layout)
  (cond ((atom field-layout)
         (cond ((null field-layout) nil)
               (t (list field-layout))))
        (t (append (make-record-field-lst (car field-layout))
                   (make-record-field-lst (cdr field-layout))))))

(defun record-macros (name field-layout cheap)
  (cons 'progn
        (append
         (make-record-accessors name
                                (make-record-field-lst field-layout)
                                (make-record-car-cdrs field-layout
                                                      (if cheap nil '(cdr)))
                                cheap)
         (list (make-record-changer name field-layout cheap)
               (make-record-maker name field-layout cheap)))))

; WARNING: If you change the layout of records, you must change
; certain functions that build them in.  Generally, these functions
; are defined before defrec was defined, but need to access
; components.  See the warning associated with defrec rewrite-constant
; for a list of one group of such functions.  You might also search
; for occurrences of the word defrec prior to this definition of it.

(defmacro defrec (name field-lst cheap)
  (record-macros name field-lst cheap))

(defabbrev equalityp (term)

; Note that the fquotep below is commented out.  This function violates
; our standard rules on the use of ffn-symb but is ok since we are looking
; for 'equal and not for 'quote or any constructor that might be hidden
; inside a quoted term.

  (and (nvariablep term)
;      (not (fquotep term))
       (eq (ffn-symb term) 'equal)))

(defabbrev inequalityp (term)

; Note that the fquotep below is commented out.  This function violates
; our standard rules on the use of ffn-symb but is ok since we are looking
; for 'equal and not for 'quote or any constructor that might be hidden
; inside a quoted term.

  (and (nvariablep term)
;      (not (fquotep term))
       (eq (ffn-symb term) '<)))

(defabbrev consityp (term)

; Consityp is to cons what equalityp is equal:  it recognizes terms
; that are non-evg cons expressions.

  (and (nvariablep term)
       (not (fquotep term))
       (eq (ffn-symb term) 'cons)))

(defun power-rep (n b)
  (if (< n b)
      (list n)
    (cons (rem n b)
          (power-rep (floor n b) b))))

(defun decode-idate (n)
  (let ((tuple (power-rep n 100)))
    (cond
     ((< (len tuple) 6)
      (er hard 'decode-idate
          "Idates are supposed to decode to a list of at least length six ~
           but ~x0 decoded to ~x1."
          n tuple))
     ((equal (len tuple) 6) tuple)
     (t 
        
; In this case, tuple is (secs mins hrs day month yr1 yr2 ...) where 0
; <= yri < 100 and (yr1 yr2 ...) represents a big number, yr, in base
; 100.  Yr is the number of years since 1900.

        (let ((secs (nth 0 tuple))
              (mins (nth 1 tuple))
              (hrs  (nth 2 tuple))
              (day  (nth 3 tuple))
              (mo   (nth 4 tuple))
              (yr (power-eval (cdr (cddddr tuple)) 100)))
          (list secs mins hrs day mo yr))))))

(defun pcd2 (n channel state)
  (declare (xargs :guard (integerp n)))
  (cond ((< n 10)
         (pprogn (princ$ "0" channel state)
                 (princ$ n channel state)))
        (t (princ$ n channel state))))

(defun print-idate (n channel state)
  (let* ((x (decode-idate n))
         (sec (car x))
         (minimum (cadr x))
         (hrs (caddr x))
         (day (cadddr x))
         (mo (car (cddddr x)))
         (yr (cadr (cddddr x))))  ; yr = years since 1900.  It is possible
                                  ; that yr > 99!
    (pprogn
     (princ$ (nth (1- mo)
              '(|January| |February| |March| |April| |May|
                |June| |July| |August| |September|
                |October| |November| |December|))
             channel state)
     (princ$ #\Space channel state)
     (princ$ day channel state)
     (princ$ '|,| channel state)
     (princ$ #\Space channel state)
     (princ$ (+ 1900 yr) channel state)
     (princ$ "  " channel state)
     (pcd2 hrs channel state)
     (princ$ '|:| channel state)
     (pcd2 minimum channel state)
     (princ$ '|:| channel state)
     (pcd2 sec channel state)
     state)))

(defun print-current-idate (channel state)
  (mv-let (d state)
    (read-idate state)
    (print-idate d channel state)))


; Essay on Inhibited Output and the Illusion of Windows

; The "io" in io?, below, stands for "inhibit output".  Roughly speaking, it
; takes an unevaluated symbolic token denoting a "kind" of output, an output
; shape involving STATE, and a form with the indicated output signature.
; If the "kind" of output is currently inhibited, it returns all nils and the
; current state, e.g., (mv nil state nil) in the case where the output
; shape is something like (mv x state y).  If the kind of output is not
; inhibited, the form is evaluated and its value is returned.

; If form always returned an error triple, this could be said as:
; `(cond ((member-eq ',token (f-get-global 'inhibit-output-lst state))
;         (value nil))
;        (t ,form))
; This whole macro is just a simple way to do optionally inhibited output.

; The introduction of an emacs window-based interface, led us to put a little
; more functionality into this macro.  Each kind of output has a window
; associated with it.  If the kind of output is uninhibited, the io? macro
; sends to *standard-co* certain auxiliary output which causes the
; *standard-co* output by form to be shipped to the designated window.

; The association of windows is accomplished via the constant
; *window-descriptions* below which contains elements of the form (token str
; clear cursor-at-top pop-up), where token is a "kind" of output, str
; identifies the associated window, and the remaining components specify
; options for how output to the window is handled by default.  The io? macro
; provides keyword arguments for overriding these defaults.  If :clear t is
; specified, the window is cleared before the text is written into it,
; otherwise the text is appended to the end.  If :cursor-at-top t is specified,
; the cursor is left at the top of the inserted text, otherwise it is left at
; the bottom of the inserted text.  If :pop-up t is specified, the window is
; raised to the top of the desktop, otherwise the window remains where it was.

; We have purposely avoided trying to suggest that windows are objects in ACL2.
; We have no way to create them or manage them.  We merely ship a sequence of
; characters to *standard-co* and let the host do whatever it does with them.
; Extending ACL2 with some window abstraction is a desirable thing to do.  I
; would like to be able to manipulate windows as ACL2 objects.  But that is
; beyond the scope of the current work whose aim is merely to provide a more
; modern interface to ACL2 without doing too much violence to ACL2's
; applicative nature or to its claim to be Common Lisp.  Those two constraints
; make the introduction of true window objects truly interesting.

; Finally io? allows for the entire io process to be illusory.  This occurs if
; the commentp argument is t.  In this case, the io? form is logically
; equivalent to NIL.  The actual output is performed after opening a wormhole
; to state.  

(defconst *window-descriptions*
;                  str clr top pop
  '((proof-tree    "0" t   t   nil)
    (rewrite-state "1" t   nil nil)
    (frame         "2" t   t   t)
    (error         "3" t   t   t)
    (warning!      "3" t   t   t)
    (warning       "3" t   t   t)
    (observation   "3" t   t   t)
    (prove         "4" nil nil nil)
    (event         "4" nil nil nil)
    (expansion     "4" nil nil nil)
    (summary       "4" nil nil nil)
    (chronology    "5" t   nil nil)
    (proof-checker "6" nil nil nil)
    (temporary     "t" t   t   t)
    (query         "q" t   t   t)))

(defun io?-nil-output (lst default-bindings)
  (cond ((null lst) nil)
        (t (cons (cond ((eq (car lst) 'state) 'state)
                       ((cadr (assoc-eq (car lst) default-bindings)))
                       (t nil))
                 (io?-nil-output (cdr lst) default-bindings)))))

(defmacro check-exact-free-vars (ctx vars form)

; A typical use of this macro is (check-free-vars io? vars form) which just
; expands to the translation of form provided all vars occurring freely in form
; are among vars and vice-versa.  The first argument is the name of the calling
; routine, which is used in error reporting.

  (declare (xargs :guard (symbol-listp vars)))
  `(translate-and-test
    (lambda (term)
      (let ((vars ',vars)
            (all-vars (all-vars term)))
        (cond ((not (subsetp-eq all-vars vars))
               (msg "Free vars problem with ~x0:  Variable~#1~[~/s~] ~&1 ~
                     occur~#1~[s~/~] in ~x2 even though not declared."
                    ',ctx
                    (set-difference-eq all-vars vars)
                    term))
              ((not (subsetp-eq vars all-vars))
               (msg "Free vars problem with ~x0: Variable~#1~[~/s~] ~&1 ~
                     ~#1~[~does/do~] not occur in ~x2 even though declared."
                    ',ctx
                    (set-difference-eq vars all-vars)
                    term))
              (t t))))
    ,form))

(defun formal-bindings (vars)

; For example, if vars is (ab cd) then return the object
; ((list (quote ab) (list 'quote ab)) (list (quote cd) (list 'quote cd))).

  (if (endp vars)
      nil
    (cons (list 'list
                (list 'quote (car vars))
                (list 'list ''quote (car vars)))
          (formal-bindings (cdr vars)))))

(defrec io-record

; WARNING:  We rely on the shape of this record in io-record-forms.

  (io-marker . form)
  t)

(defmacro io-record-forms (io-records)

; WARNING:  If you change this macro, consider changing (defrec io-record ...)
; too.

  `(strip-cdrs ,io-records))

(defun push-io-record (io-marker form state)
  (f-put-global 'saved-output-reversed
                (cons (make io-record
                            :io-marker io-marker
                            :form form)
                      (f-get-global 'saved-output-reversed state))
                state))

(defun saved-output-token-p (token state)
  (and (f-get-global 'saved-output-p state)
       (or (eq (f-get-global 'saved-output-token-lst state) :all)
           (member-eq token (f-get-global 'saved-output-token-lst state)))))

(defmacro io? (token commentp shape vars body
                     &key
                     (clear 'nil clear-argp)
                     (cursor-at-top 'nil cursor-at-top-argp)
                     (pop-up 'nil pop-up-argp)
                     (io-marker 'nil)
                     (default-bindings 'nil))

; Typical use (io? error nil (mv col state) (x y) (fmt ...)), meaning execute
; the fmt statement unless 'error is on 'inhibit-output-lst.  The mv expression
; is the shape of the output produced by the fmt expression, and the list (x y)
; for vars indicates the variables other than state that occur free in that
; expression.  See the comment above, and see the Essay on Saved-output for a
; comment that gives a convenient macro for obtaining the free variables other
; than state that occur free in body.

; Default-bindings is a list of doublets (symbol value).  It is used in order
; to supply a non-nil return value for other than state when io is suppressed.
; For example, fmt returns col and state, as suggested by the third (shape)
; argument below.  Without the :default-bindings, this form would evaluate to
; (mv nil state) if event IO is inhibited.  But there are fixnum declarations
; that require the first return value of fmt to be an integer, and we can
; specify the result in the inhibited case to be (mv 0 state) with the
; following :default-bindings:

; (io? event nil (mv col state) nil (fmt ...) :default-bindings ((col 0)))

; The values in :default-bindings are evaulated, so it would be equivalent to
; replace 0 with (- 4 4), for example.

  (declare (xargs :guard (and (symbolp token)
                              (symbol-listp vars)
                              (not (member-eq 'state vars))
                              (assoc-eq token *window-descriptions*))))
  (let* ((associated-window (assoc-eq token *window-descriptions*))
         (expansion
          `(let* ((io?-output-inhibitedp
                   (member-eq ',token
                              (f-get-global 'inhibit-output-lst state)))
                  (io?-alist
                   (and (not io?-output-inhibitedp)
                        (list
                         (cons #\w ,(cadr associated-window))
                         (cons #\c ,(if clear-argp
                                        clear
                                      (caddr associated-window)))
                         (cons #\t ,(if cursor-at-top-argp
                                        cursor-at-top
                                      (cadddr associated-window)))
                         (cons #\p ,(if pop-up-argp
                                        pop-up
                                      (car (cddddr associated-window))))

; Peter Dillinger requested the following binding, so that he could specify a
; window prelude string that distinguishes between, for example, "prove",
; "event", and "summary" output, which with the default string would all just
; show up as window 4.

                         (cons #\k ,(symbol-name token))))))
             (pprogn
              (if (or io?-output-inhibitedp
                      (null (f-get-global 'window-interfacep state)))
                  state
                (mv-let (io?-col state)
                        (fmt1! (f-get-global 'window-interface-prelude state)
                               io?-alist 0 *standard-co* state nil)
                        (declare (ignore io?-col))
                        state))
              ,(let ((body
                      `(check-vars-not-free
                        (io?-output-inhibitedp io?-alist)
                        (check-exact-free-vars io? (state ,@vars) ,body)))
                     (nil-output (if (eq shape 'state)
                                     'state
                                   (cons 'mv (io?-nil-output (cdr shape)
                                                             default-bindings))))
                     (postlude
                      `(mv-let
                         (io?-col state)
                         (if (or io?-output-inhibitedp
                                 (null (f-get-global 'window-interfacep state)))
                             (mv 0 state)
                           (fmt1! (f-get-global 'window-interface-postlude state)
                                  io?-alist 0 *standard-co* state nil))
                         (declare (ignore io?-col))
                         (check-vars-not-free
                          (io?-output-inhibitedp io?-alist io?-col)
                          ,shape))))
                 (cond
                  ((eq shape 'state)
                   `(pprogn
                     (if io?-output-inhibitedp state ,body)
                     ,postlude))
                  (t `(mv-let ,(cdr shape)
                        (if io?-output-inhibitedp
                            ,nil-output
                          ,body)
                        ,postlude))))))))
    `(pprogn
      (cond ((saved-output-token-p ',token state)
             (push-io-record ,io-marker
                             (list 'let
                                   (list ,@(formal-bindings vars))
                                   ',expansion)
                             state))
            (t state))
      ,(cond
        (commentp
         `(wormhole t 'comment-window-io nil
                    ',(cond
                       ((eq shape 'state)
                        `(pprogn ,expansion (value :q)))
                       (t
                        `(mv-let ,(cdr shape)
                           ,expansion
                           (declare
                            (ignore ,@(remove1-eq 'state (cdr shape))))
                           (value :q))))
                    :ld-verbose nil
                    :ld-prompt nil))
        (t expansion)))))

(defun output-ignored-p (token state)
  (and (not (saved-output-token-p token state))
       (member-eq token
                  (f-get-global 'inhibit-output-lst state))))

(defun error1 (ctx str alist state)

  ":Doc-Section ACL2::Programming

  print an error message and cause a ``soft error''~/

  ~c[(Error1 ctx str alist)] returns ~c[(mv t nil state)].  An error message is
  first printed using the the ``context'' ~c[ctx], as well as the string
  ~c[str] and alist ~c[alist] that are of the same kind as expected by
  ~ilc[fmt].  ~l[fmt].

  ~c[Error1] can be interpreted as causing an ``error'' when programming with
  the ACL2 ~ilc[state], something most ACL2 users will probably not want to do;
  ~pl[ld-error-triples] and ~pl[er-progn].  In order to cause errors with
  ~c[:]~ilc[logic] mode functions, ~pl[hard-error] and ~pl[illegal].  Better
  yet, ~pl[er] for a macro that provides a unified way of signaling errors.~/

  As mentioned above, ~c[error1] always returns ~c[(mv t nil state)].  But if a
  call ~c[(error1 ctx str alist)] is encountered during evaluation, then the
  string ~c[str] is first printed using the association list ~c[alist] (as in
  ~ilc[fmt]).  Here is a trivial, contrived example.
  ~bv[]
  ACL2 !>(error1 'my-context
                 \"Printing 4: ~~n0\"
                 (list (cons #\\0 4))
                 state)


  ACL2 Error in MY-CONTEXT:  Printing 4: four

  ACL2 !>
  ~ev[]~/"

  (pprogn
   (io? error nil state (alist str ctx)
        (error-fms nil ctx str alist state))
   (mv t nil state)))


(defconst *uninhibited-warning-summaries*
  '("Uncertified"
    "Skip-proofs"
    "Defaxioms"
    "Tainted"
    "Compiled file"))

(defun warning-off-p (summary state)

; This function is used by warning$ to determine whether a given warning should
; be printed.  See also warning-disabled-p, which we can use to avoid needless
; computation on behalf of disabled warnings.

  (or (and summary
           (member-string-equal
            summary
            (cdr (assoc-eq :inhibit-warnings
                           (table-alist 'acl2-defaults-table
                                        (w state))))))
      (and (or (eq (ld-skip-proofsp state) 'include-book)
               (eq (ld-skip-proofsp state) 'include-book-with-locals)
               (eq (ld-skip-proofsp state) 'initialize-acl2))
           (not (and summary
                     (member-string-equal
                      summary
                      *uninhibited-warning-summaries*))))))

(defun warning1-body (ctx summary str alist evisc-tuple state)
  (let ((channel (f-get-global 'proofs-co state)))
    (pprogn
     (if summary
         (push-warning summary state)
       state)
     (mv-let
       (col state)
       (fmt "ACL2 Warning~#0~[~/ [~s1]~]"
            (list (cons #\0 (if summary 1 0))
                  (cons #\1 summary))
            channel state nil)
       (mv-let (col state)
         (fmt-in-ctx ctx col channel state)
         (mv-let (col state)
           (fmt1 str alist col channel state evisc-tuple)
           (mv-let (col state)
             (fmt1 "~%~%" nil col channel state nil)
             (declare (ignore col))
             state)))))))

(defun warning1 (ctx summary str alist evisc-tuple state)

; This function prints the "ACL2 Warning" banner and ctx, then the
; user's summary, str and alist, and then two carriage returns.

  (mv-let
   (check-warning-off summary)
   (cond ((consp summary)
          (mv nil (car summary)))
         (t (mv t summary)))
   (cond
    ((and check-warning-off
          (warning-off-p summary state))
     state)

; Note:  There are two io? expressions below.  They are just alike except
; that the first uses the token WARNING! and the other uses WARNING.  Keep
; them that way!

    ((and summary
          (member-string-equal summary *uninhibited-warning-summaries*))
     (io? WARNING! nil state
          (summary ctx evisc-tuple alist str)
          (warning1-body ctx summary str alist evisc-tuple state)))
    (t (io? WARNING nil state
            (summary ctx evisc-tuple alist str)
            (warning1-body ctx summary str alist evisc-tuple state))))))

(defmacro warning$ (&rest args)

; A typical use of this macro might be:
; (warning$ ctx "Loops" "The :REWRITE rule ~x0 loops forever." name) or
; (warning$ ctx nil "The :REWRITE rule ~x0 loops forever." name).
; If the second argument is wrapped in a one-element list, as in
; (warning$ ctx ("Loops") "The :REWRITE rule ~x0 loops forever." name),
; then no check will be made for whether the warning is disabled, presumably
; because we are in a context where we know the warning is enabled.

  (let ((warning-type-arg (if (and (consp (cadr args))
                                   (stringp (car (cadr args))))
                              (kwote (cadr args))
                            (cadr args))))
    (list 'warning1
          (car args)
          warning-type-arg
          (caddr args)
          (make-fmt-bindings '(#\0 #\1 #\2 #\3 #\4
                               #\5 #\6 #\7 #\8 #\9)
                             (cdddr args))
          '(default-evisc-tuple state)
          'state)))

(defmacro warning-disabled-p (summary)

; We can use this function to avoid needless computation on behalf of disabled
; warnings.

  (declare (xargs :guard (stringp summary)))
  (if (member-equal summary *uninhibited-warning-summaries*)
      (er hard 'warning-disabled-p
          "Implementation error!  warning-disabled-p should not be called for ~
           ~x0."
          summary)
    `(or (output-ignored-p 'warning state)
         (warning-off-p ,summary state))))

(defun observation1 (ctx str alist evisc-tuple state)

; This function prints the "ACL2 Observation" banner and ctx, then the
; user's str and alist, and then two carriage returns.

  (io? observation nil state
       (str alist evisc-tuple ctx)
       (let ((channel (f-get-global 'proofs-co state)))
         (mv-let
          (col state)
          (fmt "ACL2 Observation" nil channel state nil)
          (mv-let (col state)
                  (fmt-in-ctx ctx col channel state)
                  (mv-let (col state)
                          (fmt1 str alist col channel state evisc-tuple)
                          (mv-let (col state)
                                  (fmt1 "~|" nil col channel state nil)
                                  (declare (ignore col))
                                  state)))))))

(defmacro observation (&rest args)

; A typical use of this macro might be:
; (observation ctx "5 :REWRITE rules are being stored under name ~x0." name).

  `(cond
    ((or (eq (ld-skip-proofsp state) 'include-book)
         (eq (ld-skip-proofsp state) 'include-book-with-locals)
         (eq (ld-skip-proofsp state) 'initialize-acl2))
     state)
    (t
     (observation1
      ,(car args)
      ,(cadr args)
      ,(make-fmt-bindings '(#\0 #\1 #\2 #\3 #\4
                            #\5 #\6 #\7 #\8 #\9)
                          (cddr args))
      (default-evisc-tuple state)
      state))))

(defun skip-when-logic (str state)
  (pprogn
   (observation 'top-level
                "~s0 events are skipped when the default-defun-mode is ~x1."
                str
                (default-defun-mode-from-state state))
   (mv nil nil state)))

;                             CHECK SUMS

; We can choose any two nonnegative integers for the following two
; constants and still have a check-sum algorithm, provided, (a) that
; (< (* 127 *check-length-exclusive-maximum*) *check-sum-exclusive-maximum*)
; and provided (b) that (* 2 *check-sum-exclusive-maximum*) is of type
; (signed-byte 32).  The first condition assures that the intermediate
; sum we obtain by adding to a running check-sum the product of a
; character code with the current location can be reduced modulo
; *check-sum-exclusive-maximum* by subtracting *check-sum-exclusive-maximum*.
; Choosing primes, as we do, may help avoid some loss of information
; due to cancellation.  Choosing primes that are smaller may lead to
; check sums with less information.

(defconst *check-sum-exclusive-maximum* 268435399
  "268435399 is the first prime below 2^28.  We use integers
   modulo this number as check sums.")

(defconst *check-length-exclusive-maximum* 2097143
  "2097143 is the first prime below 2^21.  We use integers
   modulo this number as indices into the stream we are
   check summing.")

; We actually return check-sums which are in (mod
; *check-sum-exclusive-maximum*).

(defconst *-check-sum-exclusive-maximum* (- *check-sum-exclusive-maximum*))

(defconst *1-check-length-exclusive-maximum*
  (1- *check-length-exclusive-maximum*))

(defun ascii-code! (x)
  (let ((y (char-code x)))
    (cond
     ((or (= y 0) (= y 128))
      1)
     ((< 127 y)
      (- y 128))
     (t y))))

(defun check-sum1 (sum len channel state)
  (declare (type (signed-byte 32) sum len))
  (let ((len (cond ((= len 0) *1-check-length-exclusive-maximum*)
                   (t (the (signed-byte 32) (1- len))))))
    (declare (type (signed-byte 32) len))
    (mv-let (x state)
      (read-char$ channel state)
      (cond ((not (characterp x)) (mv sum state))
            (t (let ((inc (ascii-code! x)))
                 (declare (type (unsigned-byte 7) inc))
                 (cond ((and (= inc 0)
                             (not (eql x #\Tab)))
                        (mv x state))
                       (t (let ((inc (the (unsigned-byte 7)
                                          (cond ((= inc 0) 9) (t inc)))))
                            (declare (type (unsigned-byte 7) inc))
                            (let ((sum (+ sum (the (signed-byte 32)
                                                   (* inc len)))))
                              (declare (type (signed-byte 32) sum))
                              (check-sum1
                               (cond ((>= sum *check-sum-exclusive-maximum*)
                                      (the (signed-byte 32)
                                       (+ sum *-check-sum-exclusive-maximum*)))
                                     (t sum))
                               len channel state)))))))))))

(defun check-sum (channel state)

; This function returns a check-sum on the characters in a stream.
; This function also checks that every character read is either
; #\Newline, #\Tab, or #\Space, or a printing Ascii character.  If the
; first value returned is a character, that character was not legal.
; Otherwise, the first value returned is an integer, the check-sum.

  ":Doc-Section Miscellaneous

  assigning ``often unique'' integers to files and objects~/

  A ``check sum'' is an integer in some fixed range computed from the
  printed representation of an object, e.g., the sum, modulo ~c[2**32], of
  the ascii codes of all the ~il[characters] in the printed
  representation.~/

  Ideally, you would like the check sum of an object to be uniquely
  associated with that object, like a fingerprint.  It could then be
  used as a convenient way to recognize the object in the future: you
  could remember the check sum (which is relatively small) and when an
  object is presented to you and alleged to be the special one you
  could compute its check sum and see if indeed it was.  Alas, there
  are many more objects than check sums (after all, each check sum is
  an object, and then there's ~c[t]).  So you try to design a check sum
  algorithm that maps similar looking objects far apart, in the hopes
  that corruptions and counterfeits ~-[] which appear to be similar to
  the object ~-[] have different check sums.  Nevertheless, the best you
  can do is a many-to-one map.  If an object with a different check
  sum is presented, you can be positive it is not the special object.
  But if an object with the same check sum is presented, you have no
  grounds for positive identification.

  The basic check sum algorithm in ACL2 is called ~c[check-sum-obj], which
  computes the check sum of an ACL2 object.  Roughly speaking, we scan
  the print representation of the object and, for each character
  encountered, we multiply the ascii code of the character times its
  position in the stream (modulo a certain prime) and then add (modulo
  a certain prime) that into the running sum.  This is inaccurate in
  many senses (for example, we don't always use the ascii code and we
  see numbers as though they were printed in base 127) but indicates
  the basic idea.

  ACL2 uses check sums to increase security in the ~il[books]
  mechanism; ~pl[certificate]."

  (check-sum1 0 *1-check-length-exclusive-maximum* channel state))

(defun check-sum-inc (n state)
  (declare (type (unsigned-byte 7) n))
  (let ((top
         (32-bit-integer-stack-length state)))
    (declare (type (signed-byte 32) top))
    (let ((sum-loc (the (signed-byte 32) (+ top -1)))
          (len-loc (the (signed-byte 32) (+ top -2))))
      (declare (type (signed-byte 32) sum-loc len-loc))
      (let ((sum
             (aref-32-bit-integer-stack sum-loc state)))
        (declare (type (signed-byte 32) sum))
        (let ((len
               (aref-32-bit-integer-stack len-loc state)))
          (declare (type (signed-byte 32) len))
          (let ((len (cond ((= 0 len) *1-check-length-exclusive-maximum*)
                           (t (the (signed-byte 32) (+ len -1))))))
            (declare (type (signed-byte 32) len))
            (let ((state
                   (aset-32-bit-integer-stack len-loc len state)))
              (let ((new-sum
                     (the (signed-byte 32)
                      (+ sum (the (signed-byte 32) (* n len))))))
                (declare (type (signed-byte 32) new-sum))
                (let ((new-sum
                       (cond ((>= new-sum *check-sum-exclusive-maximum*)
                              (the (signed-byte 32)
                               (+ new-sum *-check-sum-exclusive-maximum*)))
                             (t new-sum))))
                  (declare (type (signed-byte 32) new-sum))
                  (aset-32-bit-integer-stack sum-loc new-sum state))))))))))

(defun check-sum-natural (n state)
  (declare (type unsigned-byte n))
  (cond ((<= n 127)
         (check-sum-inc (the (unsigned-byte 7) n) state))
        (t (pprogn (check-sum-inc (the (unsigned-byte 7) (rem n 127)) state)
                   (check-sum-natural (truncate n 127) state)))))

(defun check-sum-string1 (str i len state)
  (declare (type string str))
  (declare (type (signed-byte 32) i len))
  (cond ((= i len) state)
        (t (let ((chr (char str i)))
             (declare (type character chr))
             (let ((code (ascii-code! chr)))
               (declare (type (unsigned-byte 7) code))
               (cond ((> code 127)
                      (f-put-global
                       'check-sum-weirdness (cons str i) state))
                     (t (pprogn (check-sum-inc code state)
                                (check-sum-string1
                                 str
                                 (the (signed-byte 32) (1+ i))
                                 len
                                 state)))))))))

(defun check-sum-string2 (str i len state)

; This function serves the same purpose as check-sum-string1 except
; that no assumption is made that i or len fit into 32 bits.  It
; seems unlikely that this function will ever be called, since it
; seems unlikely that any Lisp will support strings of length 2 billion
; or more, but who knows.

  (declare (type string str))
  (cond ((= i len) state)
        (t (let ((chr (char str i)))
             (let ((code (ascii-code! chr)))
               (cond ((> code 127)
                      (f-put-global
                       'check-sum-weirdness (cons str i) state))
                     (t (pprogn (check-sum-inc code state)
                                (check-sum-string2
                                 str
                                 (1+ i)
                                 len
                                 state)))))))))

(defun check-sum-string (str state)
  (let ((len (the integer (length (the string str)))))
    (cond ((32-bit-integerp len)
           (check-sum-string1 str 0 (the (signed-byte 32) len) state))
          (t (check-sum-string2 str 0 len state)))))

(defun check-sum-obj1 (obj state)
  (cond ((symbolp obj)
         (pprogn (check-sum-inc 1 state)
                 (check-sum-string (symbol-name obj) state)))
        ((stringp obj)
         (pprogn (check-sum-inc 2 state)
                 (check-sum-string obj state)))
        ((rationalp obj)
         (cond ((integerp obj)
                (cond ((< obj 0)
                       (pprogn (check-sum-inc 3 state)
                               (check-sum-natural (- obj) state)))
                      (t (pprogn (check-sum-inc 4 state)
                                 (check-sum-natural obj state)))))
               (t (let ((n (numerator obj)))
                    (pprogn (check-sum-inc 5 state)
                            (check-sum-natural (if (< n 0) (1- (- n)) n) state)
                            (check-sum-natural (denominator obj) state))))))
        ((consp obj)
         (pprogn (check-sum-inc 6 state)
                 (check-sum-obj1 (car obj) state)
                 (cond ((atom (cdr obj))
                        (cond ((cdr obj)
                               (pprogn (check-sum-inc 7 state)
                                       (check-sum-obj1 (cdr obj) state)))
                              (t (check-sum-inc 8 state))))
                       (t (check-sum-obj1 (cdr obj) state)))))
        ((characterp obj)
         (pprogn (check-sum-inc 9 state)
                 (let ((n (ascii-code! obj)))
                   (cond ((< n 128)
                          (check-sum-inc (ascii-code! obj) state))
                         (t (f-put-global
                             'check-sum-weirdness obj state))))))
        ((complex-rationalp obj)
         (pprogn (check-sum-inc 14 state)
                 (check-sum-obj1 (realpart obj) state)
                 (check-sum-obj1 (imagpart obj) state)))
        (t (f-put-global
            'check-sum-weirdness obj state))))

(defun check-sum-obj (obj state)

; We return a check-sum on obj, using an algorithm similar to that of
; check-sum.  We return a non-integer as the first value if (and only if) the
; obj is not composed entirely of conses, symbols, strings, rationals, complex
; rationals, and characters. If the first value is not an integer, it is one of
; the offending objects encoutered.

; We typically use this function to compute check sums of cert-obj records and
; of objects of the form (cons expansion-alist ev-lst) where ev-lst is the list
; of forms in a book, including the initial in-package, and expansion-alist
; comes from make-event expansion.

  (pprogn
   (extend-32-bit-integer-stack 2 0 state)
   (let ((top
          (32-bit-integer-stack-length state)))
     (let ((sum-loc (+ top -1))
           (len-loc (+ top -2)))
       (pprogn
        (aset-32-bit-integer-stack sum-loc 0 state)
        (aset-32-bit-integer-stack len-loc *1-check-length-exclusive-maximum*
                                   state)
        (f-put-global 'check-sum-weirdness nil state)
        (check-sum-obj1 obj state)
        (let ((ans (aref-32-bit-integer-stack sum-loc state)))
          (pprogn (shrink-32-bit-integer-stack 2 state)
                  (let ((x (f-get-global 'check-sum-weirdness state)))
                    (cond (x (pprogn (f-put-global
                                      'check-sum-weirdness nil state)
                                     (mv x state)))
                          (t (mv ans state)))))))))))

(defun read-file-iterate (channel acc state)
  (mv-let (eof obj state)
    (read-object channel state)
    (cond (eof
           (mv (reverse acc) state))
          (t (read-file-iterate channel (cons obj acc) state)))))

(defun read-file (name state)
  (mv-let (channel state)
    (open-input-channel name :object state)
    (cond (channel
           (mv-let (ans state)
             (read-file-iterate channel nil state)
             (pprogn (close-input-channel channel state)
                     (mv nil ans state))))
          (t (er soft 'read-file "No file found ~x0." name)))))

(defun formals (fn w)
  (cond ((flambdap fn)
         (lambda-formals fn))
        (t (let ((temp (getprop fn 'formals t 'current-acl2-world w)))
             (cond ((eq temp t)
                    (er hard 'formals
                        "Every function symbol is supposed to have a ~
                         'FORMALS property but ~x0 does not!"
                        fn))
                   (t temp))))))

(defun arity (fn w)
  (cond ((flambdap fn) (length (lambda-formals fn)))
        (t (let ((temp (getprop fn 'formals t 'current-acl2-world w)))
             (cond ((eq temp t) nil)
                   (t (length temp)))))))

(defun stobjs-in (fn w)

; Fn must be a function symbol, not a lambda expression and not an
; undefined symbol.  See the Essay on STOBJS-IN and STOBJS-OUT.

  (if (eq fn 'cons)

; We call this function on cons so often we optimize it.

      '(nil nil)

    (getprop fn 'stobjs-in nil 'current-acl2-world w)))

(defun stobjs-out (fn w)

; See the Essay on STOBJS-IN and STOBJS-OUT.

  (if (eq fn 'cons)
; We call this function on cons so often we optimize it.
      '(nil)
    (getprop fn 'stobjs-out '(nil) 'current-acl2-world w)))

; With stobjs-out defined, we can define user-defined-functions-table.

(deflabel user-defined-functions-table
  :doc
  ":Doc-Section Events

  an advanced ~il[table] used to replace certain system functions~/
  ~bv[]
  Examples:
  (table user-defined-functions-table 'untranslate-preprocess 'my-preprocess)
  (table user-defined-functions-table 'untranslate 'my-untranslate)
  ~ev[]

  This feature should perhaps only be used by advanced users who have a
  thorough understanding of the system functions being replaced.  There are
  currently two ways a user can affect the way ACL2 prints terms.

  The first example associates the user-defined function symbol
  ~c[my-preprocess] with ~c[untranslate-preprocess].  As a result, when ACL2
  prints a term, say during a proof, using its so-called ``untranslate''
  process the first thing it does is to call ~c[my-preprocess] on two
  arguments: that term and the current ACL2 logical ~il[world].  If the call
  produces a non-~c[nil] result, then that result is passed to the untranslate
  process.

  The second example associates the user-defined function symbol
  ~c[my-untranslate] with the built-in function symbol ~c[untranslate].  As a
  result, the code for ~c[my-untranslate] will be run whenever the untranslate
  process is run.  The formals of the two functions must agree and must not
  contain any ~il[stobj] names.  Note that these overrides fail to occur upon
  guard violations and some other evaluation errors.~/

  The ~c[untranslate-preprocess] approach may suffice for most cases in which a
  user wants to modify the way output is produced by the theorem prover.  We
  present an example immediately below, but see
  ~c[books/misc/untranslate-patterns.lisp] for a more elaborate example.  If
  the ~c[untranslate-preprocess] approach does not seem sufficient for your
  purposes, you are invited to look at file ~c[books/misc/rtl-untranslate.lisp]
  for an example of user-defined ~c[untranslate] (i.e., following the second
  example displayed above).

  Suppose you have a large constant that you would prefer not to see in
  proofs.  For example, you may have submitted the following definition (but
  imagine a much larger constant, say, a list of length 1,000,000).
  ~bv[]
  (defconst *a* '(a b c d))
  ~ev[]
  If you submit the following (silly) theorem
  ~bv[]
  (thm (equal (cons x *a*) (car (cons yyy zzz))))
  ~ev[]
  then you will see the following output:
  ~bv[]
  (EQUAL (CONS X '(A B C D)) YYY).
  ~ev[]
  If ~c[*a*] had represented a much larger structure, we would wish we could
  see the following instead.
  ~bv[]
  (EQUAL (CONS X *A*) YYY)
  ~ev[]
  That can be accomplished as follows.  First we make the following definition.
  ~bv[]
  (defun my-preprocess (term wrld)
    (declare (ignore wrld))
    (if (equal term (list 'quote *a*))
        '*a*
      term))
  ~ev[]
  Now we submit the following ~ilc[table] event.
  ~bv[]
  (table user-defined-functions-table
         'untranslate-preprocess
         'my-preprocess)
  ~ev[]
  This will install ~c[my-preprocess] as a preprocessor before the normal
  untranslation routine is applied to printing a term.  When the untranslation
  routine encounters the constant ~c[(QUOTE (A B C D))], it will replace it
  with ~c[*a*], and the usual untranlation routine will print this as
  ~c[*A*].~/")

(defun all-nils (lst)
  (declare (xargs :guard (true-listp lst)))
  (cond ((endp lst) t)
        (t (and (eq (car lst) nil)
                (all-nils (cdr lst))))))

(defconst *user-defined-functions-table-keys*
  '(untranslate untranslate-lst untranslate-preprocess))

(table user-defined-functions-table nil nil
       :guard
       (and (member-eq key *user-defined-functions-table-keys*)
            (symbolp val)
            (not (eq (getprop val 'formals t 'current-acl2-world world)
                     t))
            (all-nils (stobjs-out val world))))

(defrec def-body

; Use the 'recursivep property, not this :recursivep field, when referring to
; the original definition, as is necessary for verify-guards,
; verify-termination, and handling of *1* functions.

  ((nume
    hyp ; nil if there are no hypotheses
    .
    concl)
   .
   (recursivep formals rune . controller-alist))
  t)

(defun latest-body (fncall hyp concl)
  (if hyp
      (fcons-term* 'if hyp concl
                   (fcons-term* 'hide fncall))
    concl))

(defun def-body (fn wrld)
  (car (getprop fn 'def-bodies nil 'current-acl2-world wrld)))

(defun body (fn normalp w)

; The safe way to call this function is with normalp = nil, which yields the
; actual original body of fn.  The normalized body is provably equal to the
; unnormalized body, but that is not a strong enough property in some cases,
; e.g., for collecting functions from the body whose guards have not yet been
; verified.  Functional instantiation may also be problematic if constraints
; are gathered using the normalized body, although we do not yet have an
; example showing that this is critical.

; WARNING: If normalp is non-nil, then we are getting the most recent body
; installed by a :definition rule with non-nil :install-body value.  Be careful
; that this is really what is desired; and if so, be aware that we are not
; returning the corresponding def-body rune.

  (cond ((flambdap fn)
         (lambda-body fn))
        (normalp (let ((def-body (def-body fn w)))
                   (latest-body (fcons-term fn
                                            (access def-body def-body
                                                    :formals))
                                (access def-body def-body :hyp)
                                (access def-body def-body :concl))))
        (t (getprop fn 'unnormalized-body nil 'current-acl2-world w))))

(defun symbol-class (sym wrld)

; The symbol-class of a symbol is one of three keywords:

; :program                  - not defined within the logic
; :ideal                 - defined in the logic but not known to be CL compliant
; :common-lisp-compliant - defined in the logic and known to be compliant with
;                          Common Lisp

; Convention: We never print the symbol-classes to the user.  We would prefer
; the user not to think about these classes per se.  It encourages a certain
; confusion, we think, because users want everything to be
; common-lisp-compliant and start thinking of it as a mode, sort of like "super
; :logic" or something.  So we are keeping these names to ourselves by not using
; them in error messages and documentation.  Typically used English phrases are
; such and such is "compliant with Common Lisp" or "is not known to be
; compliant with Common Lisp."

; Historical Note: :Program function symbols were once called "red", :ideal
; symbols were once called "blue", and :common-lisp-compliant symbols were once
; called "gold."

; Before we describe the storage scheme, let us make a few observations.
; First, most function symbols have the :program symbol-class, because until ACL2
; is admitted into the logic, the overwhelming majority of the function symbols
; will be system functions.  Second, all :logic function symbols have
; symbol-class :ideal or :common-lisp-compliant.  Third, this function,
; symbol-class, is most often applied to :logic function symbols, because most
; often we use it to sweep through the function symbols in a term before
; verify-guards.  Finally, theorem names are very rarely of interest here but
; they are always either :ideal or (very rarely) :common-lisp-compliant.

; Therefore, our storage scheme is that every :logic function will have a
; symbol-class property that is either :ideal or :common-lisp-compliant.  We
; will not store a symbol-class property for :program but just rely on the absence
; of the property (and the fact that the symbol is recognized as a function
; symbol) to default its symbol-class to :program.  Thus, system functions take no
; space but are slow to answer.  Finally, theorems will generally have no
; stored symbol-class (so it will default to :ideal for them) but when it is
; stored it will be :common-lisp-compliant.

; Note that the defun-mode of a symbol is actually determined by looking at its
; symbol-class.  We only store the symbol-class.  That is more often the
; property we need to look at.  But we believe it is simpler for the user to
; think in terms of :mode and :verify-guards.

  (or (getprop sym 'symbol-class nil 'current-acl2-world wrld)
      (if (getprop sym 'theorem nil 'current-acl2-world wrld)
          :ideal
          :program)))

(defmacro fdefun-mode (fn wrld)

; Fn must be a symbol and a function-symbol of wrld.

  `(if (eq (symbol-class ,fn ,wrld) :program)
       :program
       :logic))

(defmacro programp (fn wrld)
  `(eq (symbol-class ,fn ,wrld) :program))

(defmacro logicalp (fn wrld)
  `(not (eq (symbol-class ,fn ,wrld) :program)))

(mutual-recursion

(defun program-termp (term wrld)
  (cond ((variablep term) nil)
        ((fquotep term) nil)
        ((flambdap (ffn-symb term))
         (or (program-termp (lambda-body (ffn-symb term)) wrld)
             (program-term-listp (fargs term) wrld)))
        ((programp (ffn-symb term) wrld) t)
        (t (program-term-listp (fargs term) wrld))))

(defun program-term-listp (lst wrld)
  (cond ((null lst) nil)
        (t (or (program-termp (car lst) wrld)
               (program-term-listp (cdr lst) wrld)))))
)

(deflabel defun-mode-caveat
  :doc
  ":Doc-Section Miscellaneous

  functions with ~il[defun-mode] of ~c[:]~ilc[program] considered unsound~/

  Technically speaking, in the current implementation, the execution
  of functions having ~il[defun-mode] ~c[:]~ilc[program] may damage the ACL2 system
  in a way that renders it unsound.  ~l[defun-mode] for a
  discussion of ~il[defun-mode]s.  That discussion describes an imagined
  implementation that is slightly different from this one.  This note
  explains that the current implementation is open to unsoundness.

  For discussion of a different soundness issue that is also related
  to function execution, ~pl[generalized-booleans].~/

  The execution of a function having ~il[defun-mode] ~c[:]~ilc[program] may violate
  Common Lisp ~il[guard]s on the subroutines used.  (This may be true even
  for calls of a function on arguments that satisfy its ~il[guard], because
  ACL2 has not verified that its ~il[guard] is sufficient to protect its
  subroutines.)  When a ~il[guard] is violated at runtime all bets are off.
  That is, no guarantees are made either about the answer being
  ``right'' or about the continued rationality of the ACL2 system
  itself.

  For example, suppose you make the following ~ilc[defun]:
  ~bv[]

  (defun crash (i)
    (declare (xargs :mode :program :guard (integerp i)))
    (car i))
  ~ev[]

  Note that the declared guard does not in fact adequately protect the
  subroutines in the body of ~c[crash]; indeed, satisfying the guard to
  ~c[crash] will guarantee that the ~ilc[car] expression is in violation
  of its guard.  Because this function is admitted in
  ~c[:]~ilc[program]-mode, no checks are made concerning the suitability
  of the guard.  Furthermore, in the current ACL2 implementation,
  ~c[crash] is executed directly in Common Lisp.  Thus if you call
  ~c[crash] on an argument satisfying its guard you will cause an
  erroneous computation to take place.

  ~bv[]
  ACL2 !>(crash 7)
  
  Error: Caught fatal error [memory may be damaged]
  ...
  ~ev[]
  There is no telling how much damage is done by this errant
  computation.  In some lisps your ACL2 job may actually crash back to
  the operating system.  In other lisps you may be able to recover
  from the ``hard error'' and resume ACL2 in a damaged but apparently
  functional image.

  THUS, HAVING A FUNCTION WITH ~IL[DEFUN-MODE] ~c[:]~ilc[PROGRAM] IN YOUR SYSTEM
  ABSOLVES US, THE ACL2 IMPLEMENTORS, FROM RESPONSIBILITY FOR THE
  SOUNDNESS OF OUR SYSTEM.

  Furthermore

  ACL2 DOES NOT YET PROVIDE ANY MEANS OF REGAINING ASSURANCES OF
  SOUNDNESS AFTER THE INTRODUCTION OF A FUNCTION IN ~c[:]~ilc[PROGRAM] MODE,
  EVEN IF IT IS ULTIMATELY CONVERTED TO ~c[:]~ilc[LOGIC] MODE (since its
  execution could have damaged the system in a way that makes it
  possible to verify its termination and ~il[guard]s unsoundly).

  Finally,

  THE VAST MAJORITY OF ACL2 SYSTEM CODE IS IN ~c[:]~ilc[PROGRAM] MODE AND SO ALL
  BETS ARE OFF FROM BEFORE YOU START!

  This hopeless state of current affairs will change, we think.  We
  think we have defined our functions ``correctly'' in the sense that
  they can be converted, without ``essential'' modification, to
  ~c[:]~ilc[logic] mode.  We think it very unlikely that a mis-guarded
  function in ~c[:]~ilc[program] mode (whether ours or yours) will cause
  unsoundness without some sort of hard lisp error accompanying it.
  We think that ultimately we can make it possible to execute your
  functions (interpretively) without risk to the system, even when some have
  ~c[:]~ilc[program] mode.  In that imagined implementation, code using
  functions having ~c[:]~ilc[program] mode would run more slowly, but safely.
  These functions could be introduced into the logic ex post facto,
  whereupon the code's execution would speed up because Common Lisp
  would be allowed to execute it directly.  We therefore ask that you
  simply pretend that this is that imagined implementation, introduce
  functions in ~c[:]~ilc[program] mode, use them as convenient and perhaps
  ultimately introduce some of them in ~c[:]~ilc[logic] mode and prove their
  properties.  If you use the system this way we can develop (or
  dismiss) this style of formal system development.  BUT BE ON THE
  LOOKOUT FOR SCREWUPS DUE TO DAMAGE CAUSED BY THE EXECUTION OF YOUR
  FUNCTIONS HAVING ~c[:]~ilc[PROGRAM] MODE!")

(deflabel generalized-booleans
  :doc
  ":Doc-Section Miscellaneous

  potential soundness issues related to ACL2 predicates~/

  The discussion below outlines a potential source of unsoundness in
  ACL2.  Although to our knowledge there is no existing Lisp
  implementation in which this problem can arise, nevertheless we feel
  compelled to explain this situation.

  Consider for example the question:  What is the value of
  ~c[(equal 3 3)]?  According to the ACL2 axioms, it's ~c[t].  And as
  far as we know, based on considerable testing, the value is ~c[t] in
  every Common Lisp implementation.  However, according the Common
  Lisp draft proposed ANSI standard, any non-~c[nil] value could result.
  Thus for example, ~c[(equal 3 3)] is allowed by the standard to be ~c[17].~/

  The Common Lisp standard specifies (or soon will) that a number of
  Common Lisp functions that one might expect to return Boolean values
  may, in fact, return arbitrary values.  Examples of such functions
  are ~ilc[equal], ~ilc[rationalp], and ~ilc[<]; a much more complete list is
  given below.  Indeed, we dare to say that every Common Lisp function
  that one may believe returns only Booleans is, nevertheless, not
  specified by the standard to have that property, with the exceptions
  of the functions ~c[not] and ~c[null].  The standard refers to such
  arbitrary values as ``generalized Booleans,'' but in fact this
  terminology makes no restriction on those values.  Rather, it merely
  specifies that they are to be viewed, in an informal sense, as being
  either ~c[nil] or not ~c[nil].

  This situation is problematic for ACL2, which axiomatizes these
  functions to return Booleans.  The problem is that because (for
  efficiency and simplicity) ACL2 makes very direct use of compiled
  Common Lisp functions to support the execution of its functions,
  there is in principle a potential for unsoundness due to these
  ``generalized Booleans.''  For example, ACL2's ~ilc[equal] function is
  defined to be Common Lisp's ~c[equal].  Hence if in Common Lisp the
  form ~c[(equal 3 3)] were to evaluate to 17, then in ACL2 we could
  prove (using the ~c[:]~ilc[executable-counterpart] of ~ilc[equal])
  ~c[(equal (equal 3 3) 17)].  However, ACL2 can also prove
  ~c[(equal (equal x x) t)], and these two terms together are
  contradictory, since they imply (instantiating ~c[x] in the second
  term by ~c[3]) that ~c[(equal 3 3)] is both equal to ~c[17] and to
  ~c[t].

  To make matters worse, the standard allows ~c[(equal 3 3)] to evaluate
  to ~em[different] non-~c[nil] values every time.  That is:  ~c[equal]
  need not even be a function!

  Fortunately, no existing Lisp implementation takes advantage of the
  flexibility to have (most of) its predicates return generalized
  Booleans, as far as we know.  We may implement appropriate
  safeguards in future releases of ACL2, in analogy to (indeed,
  probably extending) the existing safeguards by way of guards
  (~pl[guard]).  For now, we'll sleep a bit better knowing that we
  have been up-front about this potential problem.

  The following list of functions contains all those that are defined
  in Common Lisp to return generalized Booleans but are assumed by
  ACL2 to return Booleans.  In addition, the functions ~ilc[acl2-numberp]
  and ~ilc[complex-rationalp] are directly defined in terms of
  respective Common Lisp functions ~c[numberp] and ~c[complexp].
  ~bv[]
  /=
  <
  =
  alpha-char-p
  atom
  char-equal
  char<
  char<=
  char>
  char>=
  characterp
  consp
  digit-char-p
  endp
  eq
  eql
  equal
  evenp
  integerp
  keywordp
  listp
  logbitp
  logtest
  lower-case-p
  minusp
  oddp
  plusp
  rationalp
  standard-char-p
  string-equal
  string<
  string<=
  string>
  string>=
  stringp
  subsetp
  symbolp
  upper-case-p
  zerop
  ~ev[]
  ")

(defun defun-mode (name wrld)

  ":Doc-Section Miscellaneous

  determines whether a function definition is a logical act~/

  Two ``~il[defun-mode]s'' are supported, ~c[:]~ilc[program] and ~c[:]~ilc[logic].  Roughly
  speaking, ~c[:]~ilc[program] mode allows you to prototype a function for
  execution without any proof burdens, while ~c[:]~ilc[logic] mode allows you to
  add a new definitional axiom to the logic.  The system comes up in
  ~c[:]~ilc[logic] mode.  Execution of functions whose ~il[defun-mode] is ~c[:]~ilc[program]
  may render ACL2 unsound!  ~l[defun-mode-caveat].~/

  When you define a function in the ACL2 logic, that function can be
  run on concrete data.  But it is also possible to reason deductively
  about the function because each definition extends the underlying
  logic with a definitional axiom.  To ensure that the logic is sound
  after the addition of this axiom, certain restrictions have to be
  met, namely that the recursion terminates.  This can be quite
  challenging.

  Because ACL2 is a ~il[programming] language, you often may wish simply to
  program in ACL2.  For example, you may wish to define your system
  and test it, without any logical burden.  Or, you may wish to define
  ``utility'' functions ~-[] functions that are executed to help manage
  the task of building your system but functions whose logical
  properties are of no immediate concern.  Such functions might be
  used to generate test data or help interpret the results of tests.
  They might create files or explore the ACL2 data base.  The
  termination arguments for such functions are an unnecessary burden
  provided no axioms about the functions are ever used in deductions.

  Thus, ACL2 introduces the idea of the ``~il[defun-mode]'' of a function.
  The ~c[:mode] keyword of ~ilc[defun]'s ~ilc[declare] ~c[xarg] allows you to
  specify the ~il[defun-mode] of a given definition.  If no ~c[:mode]
  keyword is supplied, the default ~il[defun-mode] is used;
  ~pl[default-defun-mode].

  There are two ~il[defun-mode]s, each of which is written as a keyword:

  ~c[:]~ilc[program] ~-[] logically undefined but executable outside deductive
  contexts.

  ~c[:]~ilc[logic] ~-[] axiomatically defined as per the ACL2 definitional
  principle.

  It is possible to change the ~il[defun-mode] of a function from ~c[:]~ilc[program]
  to ~c[:]~ilc[logic].  We discuss this below.

  We think of functions having ~c[:]~ilc[program] mode as ``dangerous''
  functions, while functions having ~c[:]~ilc[logic] mode are ``safe.''  The
  only requirement enforced on ~c[:]~ilc[program] mode functions is the
  syntactic one:  each definition must be well-formed ACL2.  Naively
  speaking, if a ~c[:]~ilc[program] mode function fails to terminate then no
  harm is done because no axiom is added (so inconsistency is avoided)
  and some invocations of the function may simply never return.  This
  simplistic justification of ~c[:]~ilc[program] mode execution is faulty
  because it ignores the damage that might be caused by
  ``mis-guarded'' functions.  ~l[defun-mode-caveat].

  We therefore implicitly describe an imagined implementation of
  ~il[defun-mode]s that is safe and, we think, effective.  But please
  ~pl[defun-mode-caveat].

  The default ~il[defun-mode] is ~c[:]~ilc[logic].  This means that when you ~ilc[defun] a
  function the system will try to prove termination.  If you wish to
  introduce a function of a different ~il[defun-mode] use the ~c[:mode] ~ilc[xargs]
  keyword.  Below we show ~c[fact] introduced as a function in ~c[:]~ilc[program]
  mode.
  ~bv[]
  (defun fact (n)
    (declare (xargs :mode :program))
    (if (or (not (integerp n)) (= n 0))
        1
      (* n (fact (1- n)))))
  ~ev[]
  No axiom is added to the logic as a result of this definition.  By
  introducing ~c[fact] in ~c[:]~ilc[program] mode we avoid the burden of a
  termination proof, while still having the option of executing the
  function.  For example, you can type
  ~bv[]
  ACL2 !>(fact 3)
  ~ev[]
  and get the answer ~c[6].  If you type ~c[(fact -1)] you will get a hard
  lisp error due to ``infinite recursion.''

  However, the ACL2 theorem prover knows no axioms about ~c[fact].  In
  particular, if the term ~c[(fact 3)] arises in a proof, the theorem
  prover is unable to deduce that it is ~c[6].  From the perspective of
  the theorem prover it is as though ~c[fact] were an undefined
  function symbol of arity ~c[1].  Thus, modulo certain important
  issues (~pl[defun-mode-caveat]), the introduction of this
  function in ~c[:]~ilc[program] mode does not imperil the soundness of the
  system ~-[] despite the fact that the termination argument for ~c[fact]
  was omitted ~-[] because nothing of interest can be proved about
  ~c[fact].  Indeed, we do not allow ~c[fact] to be used in logical
  contexts such as conjectures submitted for proof.

  It is possible to convert a function from ~c[:]~ilc[program] mode to
  ~c[:]~ilc[logic] mode at the cost of proving that it is admissible.  This can
  be done by invoking
  ~bv[]
  (verify-termination fact)
  ~ev[]
  which is equivalent to submitting the ~ilc[defun] of ~c[fact], again, but
  in ~c[:]~ilc[logic] mode.
  ~bv[]
  (defun fact (n)
    (declare (xargs :mode :logic))
    (if (or (not (integerp n)) (= n 0))
        1
      (* n (fact (1- n)))))
  ~ev[]
  This particular event will fail because the termination argument requires
  that ~c[n] be nonnegative.  A repaired ~ilc[defun], for example with ~ilc[=]
  replaced by ~ilc[<=], will succeed, and an axiom about ~c[fact] will
  henceforth be available.

  Technically, ~ilc[verify-termination] submits a redefinition of the
  ~c[:]~ilc[program] mode function.  This is permitted, even when
  ~ilc[ld-redefinition-action] is ~c[nil], because the new definition is
  identical to the old (except for its ~c[:mode] and, possibly, other
  non-logical properties).

  ~l[guard] for a discussion of how to restrict the execution of
  functions.  ~il[Guard]s may be ``verified'' for functions in ~c[:]~ilc[logic]
  mode; ~pl[verify-guards]."

; Only function symbols have defun-modes.  For all other kinds of names
; e.g., package names and macro names, the "defun-mode" is nil.

; Implementation Note:  We do not store the defun-mode of a symbol on the
; property list of the symbol.  We compute the defun-mode from the symbol-class.

  (cond ((and (symbolp name)
              (function-symbolp name wrld))
         (fdefun-mode name wrld))
        (t nil)))

; Rockwell Addition: Consider the guard conjectures for a stobj-using
; function.  Every accessor and updater application will generate the
; obligation to prove (stp st), where stp is the recognizer for the
; stobj st.  But this is guaranteed to be true for bodies that have
; been translated as defuns, because of the syntactic restrictions on
; stobjs.  So in this code we are concerned with optimizing these
; stobj recognizer expressions away, by replacing them with T.

(defun get-stobj-recognizer (stobj wrld)

; If stobj is a stobj name, return the name of its recognizer; else nil.
; The value of the 'stobj property is always (*the-live-var* recog ...),
; for all user defined stobj names.  The value is '(*the-live-state*) for
; STATE and is nil for all other names.

  (cond ((eq stobj 'state)
         'state-p)
        (t (cadr (getprop stobj 'stobj nil 'current-acl2-world wrld)))))

(defun stobj-recognizer-terms (known-stobjs wrld)

; Given a list of stobjs, return the list of recognizer applications.
; E.g., given (STATE MY-ST) we return ((STATE-P STATE) (MY-STP MY-ST)).

  (cond ((null known-stobjs) nil)
        (t (cons (fcons-term* (get-stobj-recognizer (car known-stobjs) wrld)
                              (car known-stobjs))
                 (stobj-recognizer-terms (cdr known-stobjs) wrld)))))

(defun mcons-term-smart (fn args)

; The following function is guaranteed to create a term provably equal to (cons
; fn args).  If we find other optimizations to make here, we should feel free
; to do so.

  (if (and (eq fn 'if)
           (equal (car args) *t*))
      (cadr args)
    (cons-term fn args)))

(mutual-recursion

(defun optimize-stobj-recognizers1 (known-stobjs recog-terms term)
  (cond
   ((variablep term) term)
   ((fquotep term) term)
   ((flambda-applicationp term)

; We optimize the stobj recognizers in the body of the lambda.  We do
; not have to watch out of variable name changes, since if a stobj
; name is passed into a lambda it is passed into a local of the same
; name.  We need not optimize the body if no stobj name is used as a
; formal.  But we have to optimize the args in either case.

    (let ((formals (lambda-formals (ffn-symb term)))
          (body (lambda-body (ffn-symb term))))
      (cond
       ((intersectp-eq known-stobjs formals)
        (fcons-term
         (make-lambda formals
                      (optimize-stobj-recognizers1
                       known-stobjs
                       recog-terms
                       body))
         (optimize-stobj-recognizers1-lst known-stobjs
                                          recog-terms
                                          (fargs term))))
       (t (fcons-term (ffn-symb term)
                      (optimize-stobj-recognizers1-lst known-stobjs
                                                       recog-terms
                                                       (fargs term)))))))
   ((and (null (cdr (fargs term)))
         (member-equal term recog-terms))
    
; If the term is a recognizer call, e.g., (MY-STP MY-ST), we replace
; it by T.  The first conjunct above is just a quick test: If the term
; has 2 or more args, then don't bother to do the member-equal.  If
; the term has 1 or 0 (!) args we do.  We won't find it if it has 0
; args.

    *t*)
   (t (mcons-term-smart (ffn-symb term)
                        (optimize-stobj-recognizers1-lst known-stobjs
                                                         recog-terms
                                                         (fargs term))))))

(defun optimize-stobj-recognizers1-lst (known-stobjs recog-terms lst)
  (cond
   ((endp lst) nil)
   (t (cons (optimize-stobj-recognizers1 known-stobjs recog-terms (car lst))
            (optimize-stobj-recognizers1-lst known-stobjs
                                             recog-terms
                                             (cdr lst)))))))

(defun optimize-stobj-recognizers (known-stobjs term wrld)

; Term is a term.  We scan it and find every call of the form (st-p
; st) where st is a member of known-stobjs and st-p is the stobj
; recognizer function for st.  We replace each such call by T.  The
; idea is that we have simplified term under the assumption that each
; (st-p st) is non-nil.  This simplification preserves equivalence
; with term PROVIDED all stobj recognizers are Boolean valued!

  (cond
   ((null known-stobjs) term)
   (t (optimize-stobj-recognizers1
       known-stobjs
       (stobj-recognizer-terms known-stobjs wrld)
       term))))

; Rockwell Addition: The next two functions are just moved from their
; old location in translate.lisp, so they can be used to see whether a
; function traffics in stobjs to be optimized.

(defun collect-non-x (x lst)

; This function preserves possible duplications of non-x elements in lst.
; We use this fact when we check the legality of signatures.

  (cond ((endp lst) nil)
        ((equal (car lst) x)
         (collect-non-x x (cdr lst)))
        (t (cons (car lst) (collect-non-x x (cdr lst))))))

; Rockwell Addition: The new flag, stobj-optp, determines whether the
; returned guard has had all the stobj recognizers optimized away.  Of
; course, whether you should call this with stobj-optp t or nil
; depends on the expression you're exploring: if it has been suitably
; translated, you can use t, else you must use nil.  Every call of
; guard (and all the functions that call those) has been changed to
; pass down this flag.  I won't mark every such place, but they'll
; show up in the compare-windows.

(defun guard (fn stobj-optp w)

; This function is just the standard way to obtain the guard of fn in
; world w.  
            
; If stobj-optp is t, we optimize the returned term, simplifying it
; under the assumption that every stobj recognizer in it is true.  If
; fn traffics in stobjs, then it was translated under the stobj
; syntactic restrictions.  Let st be a known stobj for fn (i.e.,
; mentioned in its stobjs-in) and let st-p be the corresponding
; recognizer.  This function should only be called with stobj-optp = t
; if you know (st-p st) to be true in the context of that call.
            
; The documentation string below addresses the general notion of
; guards in ACL2, rather than explaining this function.  After the doc
; string is a rather extensive essay in which we are collecting our
; thoughts on the topic of eliminating guards from the logic.

  ":Doc-Section Miscellaneous

  restricting the domain of a function~/

  The ACL2 system provides a mechanism for restricting a function
  definition to a particular domain.  Although such restrictions,
  which are called ``guards,'' are actually ignored by the ACL2
  ~em[logic], they can be useful as a specification device or as a
  means of causing the execution of definitions directly in Common
  Lisp.  To make such a restriction, use the ~c[:guard] ~c[xarg] to
  ~ilc[defun].  ~l[xargs] for a discussion of how to use ~c[:guard].
  The general issue of guards and guard verification is discussed
  in the topics listed below.
  ~/

  To begin further discussion of guards, ~pl[guard-introduction].
  That section directs the reader to further sections for more
  details.  To see just a summary of some ~il[command]s related to guards,
  ~pl[guard-quick-reference].~/

  :cite verify-guards
  :cite set-verify-guards-eagerness"

  (cond ((flambdap fn) *t*)
        ((or (not stobj-optp)
             (all-nils (stobjs-in fn w)) )
         (getprop fn 'guard *t* 'current-acl2-world w))
        (t

; If we have been told to optimize the stobj recognizers (stobj-optp =
; t) and there are stobjs among the arguments of fn, then fn was
; translated with the stobj syntactic restrictions enforced for those
; names.  That means we can optimize the guard of the function
; appropriately.

         (optimize-stobj-recognizers
          (collect-non-x 'nil (stobjs-in fn w))
          (or (getprop fn 'guard *t* 'current-acl2-world w)

; Once upon a time we found a guard of nil, and it took awhile to track down
; the source of the ensuing error.  We believe we have fixed the problem, and
; if there are no errors reported for awhile (it's now 9/93), we should remove
; this OR.

              (illegal 'guard "Found a nil guard for ~x0."
                       (list (cons #\0 fn))))
          w))))

(deflabel guard-quick-reference
  :doc
  ":Doc-Section Guard

  brief summary of guard checking and guard verification~/

  For a careful introduction to guards, ~pl[guard].~/

  ~b[I. GUARD CHECKING DURING EXECUTION]

  ~em[Effect]

  Guards on definitions are checked at execution time (except for
  guards on subsidiary calls of recursive or mutually recursive
  functions).

  ~em[When does it happen]

  By default, guards are checked for all forms submitted at the top
  level.

  ~em[To disable]~nl[]
  ~c[:set-guard-checking nil]   ; skip raw Lisp if there is a guard violation
  ~c[:set-guard-checking :none] ; skip guard checking entirely

  ~em[To (re-)enable]~nl[]
  ~c[:set-guard-checking t]

  ~l[set-guard-checking] for more options.

  ~b[II. GUARD VERIFICATION]

  ~em[Effect]

  A proof is attempted of the obligations arising from the guards of
  subsidiary functions in a ~ilc[defun], ~ilc[defthm], or ~ilc[defaxiom] event.
  In the case of a ~c[defun], the guard itself is also verified (under an
  implicit guard of ~c[t]).

  ~em[When does it happen]

  Only names of defined functions, ~ilc[defthm]s, and ~ilc[defaxiom]s are
  subject to guard verification.  Guard verification may occur when
  functions are defined (using ~ilc[defun]), but it requires an explicit
  call of ~ilc[verify-guards] in order to verify guards for ~ilc[defthm]s
  and ~ilc[defaxiom]s.  Constrained functions (~pl[encapsulate]) may
  not have their guards verified.

  ~c[(verify-guards foo ...)]~nl[]
  causes guard verification for the ~ilc[defun], ~ilc[defthm], or ~ilc[defaxiom] named
  by ~c[foo], if it has not already been successfully done.  The default
  ~il[defun-mode] (~pl[default-defun-mode]) must be ~c[:]~ilc[logic], or else
  this event is ignored.

  ~c[(defun foo ...)]~nl[]
  causes guard verification of ~c[foo] if and only if the following
  conditions are both met.  (However,
  ~pl[set-verify-guards-eagerness] for how to change this
  behavior.)
  ~bq[]
  1. Foo is processed in ~c[:]~ilc[logic] mode (either by setting mode ~c[:]~ilc[logic]
  globally, or by including ~c[:mode :logic] in the ~ilc[xargs] declaration).

  2. The ~ilc[xargs] declaration (~pl[xargs]) either specifies ~c[:]~ilc[guard] or
  specifies ~c[:]~ilc[verify-guards] ~c[t] (or both).
  ~eq[]

  ~c[(verify-termination foo ...)]~nl[]
  causes guard verification of ~c[foo] if ~c[foo] is a function currently
  defined in ~c[:]~ilc[program] mode and the appropriate ~ilc[xargs] are supplied, as
  discussed for the case of ~ilc[defun] above.  The default ~il[defun-mode]
  (~pl[default-defun-mode]) must be ~c[:]~ilc[logic], or else this event is
  ignored.")

(deflabel guard-introduction
  :doc
  ":Doc-Section Guard

  introduction to ~il[guard]s in ACL2~/

  Most users can probably profit by avoiding dealing with guards most
  of the time.  If they seem to get in the way, they can be ``turned
  off'' using the command ~c[:]~ilc[set-guard-checking] ~c[nil]; for more
  about this, ~pl[set-guard-checking].  For more about guards in
  general, ~pl[guard].~/

  The guard on a function symbol is a formula about the formals of the
  function.  To see the guard on a function, use the keyword command
  ~c[:]~ilc[args].  ~l[args].  To specify the guard on a function at
  ~c[defun-time], use the ~c[:]~ilc[guard] ~c[xarg].  ~l[xargs].

  Guards can be seen as having either of two roles: (a) they are a
  specification device allowing you to characterize the kinds of
  inputs a function ``should'' have, or (b) they are an efficiency
  device allowing logically defined functions to be executed directly
  in Common Lisp.  Briefly:  If the guards of a function definition
  are ``verified'' (~pl[verify-guards]), then the evaluation of a
  call of that function on arguments satisfying its guard will have
  the following property:
  ~bq[]
  All subsequent function calls during that evaluation will be on
  arguments satisfying the guard of the called function.
  ~eq[]
  The consequence of this fact for (a) is that your specification
  function is well-formed, in the sense that the values returned by
  this function on appropriate arguments only depend on the
  restrictions of the called functions to their intended domains.  The
  consequence of this fact for (b) is that in the ACL2 system, when a
  function whose guards have been verified is called on arguments that
  satisfy its guard, then the raw lisp function defined by this
  function's ~ilc[defun] event is used to evaluate the call.  Note
  however that even when the user-supplied ~ilc[defun] is not used, ACL2
  uses a corresponding ``executable counterpart'' that generally
  performs, we expect, nearly as well as the raw lisp function.
  ~l[comp] to see how ~il[compilation] can speed up both kinds of
  execution.

  Let us turn next to the issue of the relationship between guards and
  evaluation.  ~l[guards-and-evaluation].")

(deflabel guards-and-evaluation
  :doc
  ":Doc-Section Guard

  the relationship between guards and evaluation~/

  The guard has no effect on the logical axiom added by the definition
  of a function.  It does, however, have consequences for how calls of
  that function are evaluated in ACL2.  We begin by explaining those
  consequences, when ACL2 is in its default ``mode,'' i.e., as
  originally brought up.  In subsequent discussion we'll consider
  other ways that guards can interact with evaluation.

  For more about guards in general, ~pl[guard].  For in-depth discussion of the
  interaction between the ~il[defun-mode] and guard checking,
  ~pl[set-guard-checking], ~pl[guard-evaluation-table],
  ~pl[guard-evaluation-examples-script], and
  ~pl[guard-evaluation-examples-log].  Also ~pl[generalized-booleans] for
  discussion about a subtle issue in the evaluation of certain Common Lisp
  functions.~/

  ~em[Guards and evaluation I:  the default]

  Consider the following very simple definition.
  ~bv[]
  (defun foo (x) (cons 1 (cdr x)))
  ~ev[]
  First consider how raw Common Lisp behaves when evaluating calls of
  this function.  To evaluate ~c[(foo x)] for some expression ~c[x], first
  ~c[x] is evaluated to some value ~c[v], and then ~c[(cons 1 (cdr x))] is
  evaluated with ~c[x] bound to ~c[v].  For example, if ~c[v] is ~c[(cons 'a 3)], then
  Common Lisp computes ~c[(cons 1 3)].  But if (for example) ~c[v] is a
  number, e.g., ~c[7], then there is no way to predict what Common
  Lisp might do.  Some implementations would cause ``sensible''
  errors, others might return nonsense, still others might crash the
  host machine.  The results tend toward the catastrophic if the call
  of ~c[foo] in question is in compiled code.

  Now by default, ACL2 evaluates calls of ~c[foo] exactly as Common
  Lisp does, except that it uses guards to check the ``legality'' of
  each function call.  So for example, since ~c[(cdr x)] has a guard
  of ~c[(or (consp x) (equal x nil))], the call ~c[(foo 7)] would cause a
  ``guard violation,'' as illustrated below.
  ~bv[]
  ACL2 !>(foo 7)

  ACL2 Error in TOP-LEVEL:  The guard for the function symbol CDR, which
  is (OR (CONSP X) (EQUAL X NIL)), is violated by the arguments in the
  call (CDR 7).

  ACL2 !>
  ~ev[]
  Thus, the relation between evaluation in ACL2 and evaluation in
  Common Lisp is that the two produce the very same results, provided
  there is no guard violation.

  ~em[Guards and evaluation II:]  ~c[:]~ilc[set-guard-checking].

  The ACL2 logic is a logic of total functions.  That is, every
  application of a function defined has a completely specified result.
  See the ~il[documentation] for each individual primitive for the
  specification of what it returns when its guard is violated; for
  example, ~pl[cdr].

  The presence of guards thus introduces a choice in the sense of
  evaluation.  When you type a form for evaluation do you mean for
  guards to be checked or not?  Put another way, do you mean for the
  form to be evaluated in Common Lisp (if possible) or in the ACL2
  logic?  Note:  If Common Lisp delivers an answer, it will be the
  same as in the logic, but it might be erroneous to execute the form
  in Common Lisp.  For example, the ACL2 logic definition of ~ilc[cdr]
  implies that the ~ilc[cdr] of an ~il[atom] is ~c[nil]; ~pl[cdr].  So:
  should ~c[(cdr 7)] cause a guard violation error or return ~c[nil]?

  The top-level ACL2 loop has a variable which controls which sense of
  execution is provided.  By default, ``guard checking'' is on, by which we
  mean that guards are checked at runtime, in the sense already described.  To
  allow execution to proceed in the logic when there is a guard violation, do
  ~c[:]~ilc[set-guard-checking]~c[ nil]; or evaluate
  ~c[:]~ilc[set-guard-checking]~c[ :none] to skip guard checking entirely.  To
  turn ``guard checking'' back on, execute the top-level form
  ~c[:]~ilc[set-guard-checking]~c[ t].  The status of guard checking
  reflected in the ~il[prompt]; guard-checking is ``on'' when the ~il[prompt]
  contains an exclamation mark (also ~pl[default-print-prompt]).  For example,
  ~bv[]
  ACL2 !>
  ~ev[]
  means guard checking is on and  
  ~bv[]
  ACL2 > 
  ~ev[]
  means guard checking is off.  The exclamation mark can be thought of
  as ``barring'' certain computations.  The absence of the mark
  suggests the absence of error messages or unbarred access to the
  logical axioms.  Thus, for example
  ~bv[]
  ACL2 !>(car 'abc)
  ~ev[]
  will signal an error, while
  ~bv[]
  ACL2 >(car 'abc)
  ~ev[]
  will return ~c[nil].  To return to our previous example:  with guard
  checking off, ~c[(foo 7)] evaluates to ~c[(cons 1 nil)].  Also
  ~pl[set-guard-checking].

  ~em[Guards and evaluation III:  guard verification]

  Consider the defininition of ~c[foo] given above, but modified so
  that a reasonable guard of ~c[(consp x)] is specified, as shown below.
  ~bv[]
  (defun foo (x)
    (declare (xargs :guard (consp x)))
    (cons 1 (cdr x)))
  ~ev[]
  We say ``reasonable guard'' above because if ~c[x] is such that
  ~c[(consp x)] holds, then the call of ~ilc[cdr] in the evaluation of
  ~c[(foo x)] will not cause a guard violation.  Thus, it ``should'' be
  legal to evaluate ~c[(foo x)], for any such ~c[x], simply by
  evaluating this form in raw Common Lisp.

  The ~ilc[verify-guards] event has been provided for this purpose.
  Details may be found elsewhere; ~pl[verify-guards].  Briefly,
  for any defined function ~c[fn], the event ~c[(verify-guards fn)]
  attempts to check the condition discussed above, that whenever ~c[fn]
  is called on arguments that satisfy its guard, the evaluation of
  this call will proceed without any guard violation.  (Moreover, the
  guard itself should be evaluable without any guard violation.)  If
  this check is successful, then future calls of this sort will be
  evaluated in raw Common Lisp.

  Returning to our example above, the ~c[(verify-guards foo)] will
  succeed because the guard ~c[(consp x)] of ~c[foo] implies the guard
  generated from the call ~c[(cdr x)] in the body of the definition,
  namely, ~c[(or (consp x) (equal x nil))] (~pl[cdr]).  Then the
  evaluation of ~c[(foo (cons 'a 3))] will take place in raw Common
  Lisp, because ~c[(cons 'a 3)] satisfies the guard of ~c[foo].

  This ability to dive into raw Common Lisp hinges on the proof that
  the guards you attach to your own functions are sufficient to ensure
  that the guards encountered in the body are satisfied.  This is
  called ``guard verification.'' Once a function has had its guards
  verified, then ACL2 can evaluate the function somewhat faster (but
  see ``Guards and evaluation V:  efficiency issues'' below).  Perhaps
  more importantly, ACL2 can also guarantee that the function will be
  evaluated correctly by any implementation of Common Lisp (provided
  the guard of the function is satisfied on the input).  That is, if
  you have verified the guards of a system of functions and you have
  determined that they work as you wish in your host ACL2 (perhaps by
  proving it, perhaps by testing), then they will work identically in
  any Common Lisp.

  There is a subtlety to our treatment of evaluation of calls of
  functions whose guards have been verified.  If the function's guard
  is not satisfied by such a call, then no further attempt is made to
  evaluate any call of that function in raw lisp during the course of
  evaluation of that call.  This is obvious if guard checking is on,
  because an error is signalled the first time its guard is violated;
  but in fact it is also true when guard checking is off.
  ~l[guard-example] for an example.

  ~em[Guards and evaluation IV:  functions having :program mode]

  Strictly speaking, functions in ~c[:]~ilc[program] mode (~pl[defun-mode]) do
  not have definitions in the ACL2 logic.  So what does it mean to evaluate
  calls of such functions in ACL2?  In general we treat ~c[:]~ilc[program]
  functions much as we treat ~c[:]~ilc[logic] functions whose guards have been
  verified, except that when no error occurs then the corresponding raw Lisp
  function is always called.  (We say ``in general'' because there are
  exceptions, discussed in the ``Aside'' just below.)  Note that when the guard
  of a function in ~c[:]~ilc[logic] mode is violated, there is still a value
  that the ACL2 logic proves is equal to the given call.  But the same cannot
  be said for a function in ~c[:]~ilc[program] mode.  Nevertheless, for the
  sake of convenience we go ahead and evaluate the corresponding raw Lisp
  function except in the situation where the guard is violated and
  guard-checking is on, aside from the following:

  ~st[Aside].  There are exceptions to the use of raw Lisp, discussed just
  above, to evaluate calls of ~c[:]~ilc[program] mode functions.  The primary
  one is that after ~c[:]~ilc[set-guard-checking]~c[ :none], evaluation of
  user-defined ~c[:]~ilc[program] mode function calls is done in the ACL2
  logic, not in raw Lisp.  The more obscure exception is that during expansion
  of macros and ~ilc[make-event] forms, and during evaluation of ~ilc[defconst]
  forms, ACL2 enters a ``safe mode'' in which this escape to raw Lisp is
  prevented.  The following example illustrates how the user can experiment
  directly with safe mode, though it is preferred to use
  ~c[:]~ilc[set-guard-checking]~c[ :none] if you are happy to skip all guard
  checking and evaluate forms in the logic.
  ~bv[]
  ACL2 !>(defun foo (x)
           (declare (xargs :mode :program :guard t))
           (car x))

  Summary
  Form:  ( DEFUN FOO ...)
  Rules: NIL
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   FOO
  ACL2 !>(foo 3)
  Error: Attempt to take the car of 3 which is not listp.
    [condition type: SIMPLE-ERROR]

  Restart actions (select using :continue):
   0: Return to Top Level (an \"abort\" restart).
   1: Abort entirely from this process.
  [1] ACL2(2): :pop
  ACL2 !>(assign safe-mode t)
   T
  ACL2 !>(foo 3)


  ACL2 Error in TOP-LEVEL:  The guard for the function symbol CAR, which
  is (OR (CONSP X) (EQUAL X NIL)), is violated by the arguments in the
  call (CAR 3).  See :DOC wet for how you might be able to get an error
  backtrace.

  ACL2 !>(assign safe-mode nil)
   NIL
  ACL2 !>(foo 3)
  Error: Attempt to take the car of 3 which is not listp.
    [condition type: SIMPLE-ERROR]

  Restart actions (select using :continue):
   0: Return to Top Level (an \"abort\" restart).
   1: Abort entirely from this process.
  [1] ACL2(2):
  ~ev[]
  The other exception occurs after ~ilc[set-guard-checking] can be called with
  a value of ~c[:all]; ~pl[set-guard-checking].
  ~st[End of aside.]

  Thus, as with ~c[:]~ilc[logic] functions:  when a guard has been
  satisfied on a call of a function with ~c[:]~ilc[program] mode, no subsidiary
  guard checking will be done.

  Notice that by treating functions in ~c[:]~ilc[program] mode like functions
  whose guards have been verified, we are using raw lisp to compute
  their values when their guards are met.  We do not check guards any
  further once raw lisp is invoked.  This can lead to hard lisp errors
  if the guards are not appropriate, as illustrated below.
  ~bv[]
  ACL2 >:program
  ACL2 p>(defun foo (x)
          (declare (xargs :guard t))
          (cons 1 (cdr x)))

  Summary
  Form:  ( DEFUN FOO ...)
  Rules: NIL
  Warnings:  None
  Time:  0.02 seconds (prove: 0.00, print: 0.00, proof tree: 0.00, other: 0.02)
   FOO
  ACL2 p>(foo 3)

  Error: 3 is not of type LIST.
  Fast links are on: do (use-fast-links nil) for debugging
  Error signalled by CDR.
  Broken at COND.  Type :H for Help.
  ACL2>>
  ~ev[]
  ~l[defun-mode-caveat].

  However, here is a way to get ACL2 to do run-time guard checking for
  user-defined ~c[:]~ilc[program] mode functions.  With this method, ACL2 will
  evaluate calls of user-defined ~c[:program] mode functions in a manner
  that follows their ACL2 definitions.  Simply execute the following in the
  ACL2 loop to put ACL2 into a ``safe mode.''
  ~bv[]
  (f-put-global 'safe-mode t state)
  ~ev[]
  Let us revisit the example above, using safe mode.  Notice that the guard of
  ~ilc[cdr] is now being checked, because the executable counterpart of ~c[foo]
  is being called even though the ~il[guard] is ~c[t].
  ~bv[]
  ACL2 !>(f-put-global 'safe-mode t state)
  <state>
  ACL2 !>:program
  ACL2 p!>(defun foo (x)
            (declare (xargs :guard t))
            (cons 1 (cdr x)))

  Summary
  Form:  ( DEFUN FOO ...)
  Rules: NIL
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   FOO
  ACL2 p!>(foo 3)


  ACL2 Error in TOP-LEVEL:  The guard for the function symbol CDR, which
  is (OR (CONSP X) (EQUAL X NIL)), is violated by the arguments in the
  call (CDR 3).  You may be able to see a trace of calls leading up to
  this violation by executing (wet <form>), where <form> is the form
  you submitted to the ACL2 loop.  See :DOC wet for how to get an error
  backtrace.

  ACL2 p!>(wet (foo 3))



  ACL2 Error in WITH-ERROR-TRACE:  The guard for the function symbol
  CDR, which is (OR (CONSP X) (EQUAL X NIL)), is violated by the arguments
  in the call (CDR 3).  (Backtrace is below.)

  1> (ACL2_*1*_ACL2::FOO 3)

  ACL2 p!>
  ~ev[]
  If we go back into ``unsafe'' mode, then we once again see a raw Lisp error,
  as we now illustrate.
  ~bv[]
  ACL2 p!>(f-put-global 'safe-mode nil state)
  <state>
  ACL2 p!>(foo 3)

  Error: 3 is not of type LIST.
  Fast links are on: do (si::use-fast-links nil) for debugging
  Error signalled by CDR.
  Broken at COND.  Type :H for Help.
  ACL2>>
  ~ev[]

  ~em[Guards and evaluation V:  efficiency issues]

  We have seen that by verifying the guards for a ~c[:]~ilc[logic] function, we
  arrange that raw lisp is used for evaluation of calls of such
  functions when the arguments satisfy its guard.

  This has several apparent advantages over the checking of guards as
  we go.  First, the savings is magnified as your system of functions
  gets deeper: the guard is checked upon the top-level entry to your
  system and then raw Common Lisp does all the computing.  Second, if
  the raw Common Lisp is compiled, enormous speed-ups are possible.
  Third, if your Common Lisp or its compiler does such optimizations
  as ~c[tail-recursion] removal, raw Common Lisp may be able to
  compute your functions on input much ``bigger'' than ACL2 can.

  The first of these advantages is quite important if you have
  complicated guards.  However, the other two advantages are probably
  not very important, as we now explain.

  When a function is defined in ~c[:]~ilc[logic] mode, its ~ilc[defun] is
  executed in raw Common Lisp.  (We might call this the ``primary''
  raw lisp definition of the function.)  However, a corresponding
  ``logic definition'' is also executed.  The ``logic definition'' is
  a ~ilc[defun] in raw lisp that checks guards at runtime and escapes to
  the primary raw lisp definition if the guard holds of the arguments
  and the function has already had its guards verified.  Otherwise the
  logic definition executes the body of the function by calling the
  logic definitions of each subroutine.  Now it is true that
  ~il[compilation] generally speeds up execution enormously.  However, the
  ~c[:]~ilc[comp] command (~pl[comp]) compiles both of the raw lisp
  definitions associated with a ~c[:]~ilc[logic] function.  Also, we have
  attempted to arrange that for every tail recursion removal done on
  the actual ~ilc[defun], a corresponding tail recursion removal is done
  on the ``logic definition.''

  We believe that in most cases, the logic definition executes almost
  as fast as the primary raw lisp definition, at least if the
  evaluation of the guards is fast.  So, the main advantage of guard
  verification is probably that it lets you know that the function may
  be executed safely in raw lisp, returning the value predicted by the
  ACL2 logic, whenever its arguments satisfy its guard.  We envision
  the development of systems of applicative lisp functions that have
  been developed and reasoned about using ACL2 but which are intended
  for evaluation in raw Common Lisp (perhaps with only a small
  ``core'' of ACL2 loaded), so this advantage of guard verification is
  important.

  Nevertheless, guard verification might be important for optimal
  efficiency when the functions make use of type declarations.  For
  example, at this writing, the GCL implementation of Common Lisp can
  often take great advantage of ~ilc[declare] forms that assign small
  integer types to formal parameters.

  To continue the discussion of guards,
  ~pl[guards-for-specification] to read about the use of guards as
  a specification device.")

(deflabel guards-for-specification
  :doc
  ":Doc-Section Guard

  guards as a specification device~/

  A use of guard verification that has nothing to do with efficiency
  is as a way to gain confidence in specifications.  This use has the
  feel of ``types'' in many traditional ~il[programming] languages, though
  guards allow much greater expressiveness than most systems of types
  (and unfortunately, as a result they are not syntactically
  checkable).

  For more discussion of guards in general, ~pl[guard].~/

  Suppose you have written a collection of function definitions that
  are intended to specify the behavior of some system.  Perhaps
  certain functions are only intended to be called on certain sorts of
  inputs, so you attach guards to those functions in order to
  ``enforce'' that requirement.  And then, you verify the guards for
  all those functions.

  Then what have you gained, other than somewhat increased efficiency
  of execution (as explained above), which quite possibly isn't your
  main concern?  You have gained the confidence that when evaluating
  any call of a (specification) function whose arguments satisfy that
  function's guard, all subsequent function calls during the course of
  evaluation will have this same property, that the arguments satisfy
  the guard of the calling function.  In logical terms, we can say
  that the equality of the original call with the returned value is
  provable from weakened versions of the definitions, where each
  definitional axiom is replaced by an implication whose antecedent is
  the requirement that the arguments satisfy the guard and whose
  consequent is the original axiom.  For example,
  ~bv[]
  (defun foo (x)
    (declare (xargs :guard (consp x)))
    (cons 1 (cdr x)))
  ~ev[]
  originally generates the axiom
  ~bv[]
  (equal (foo x)
         (cons 1 (cdr x)))
  ~ev[]
  but in fact, when evaluation involves no guard violation then the
  following weaker axiom suffices in the justification of the
  evaluation.
  ~bv[]
  (implies (consp x)
           (equal (foo x)
                  (cons 1 (cdr x))))
  ~ev[]
  If you are following links to read this documentation as a hypertext
  style document, then please ~pl[guard-miscellany].  This
  concludes our discussion of guards with miscellaneous remarks, and
  also contains pointers to related topics.")

(deflabel guard-miscellany
  :doc
  ":Doc-Section Guard

  miscellaneous remarks about guards~/

  The discussion of guards concludes here with a few miscellaneous
  remarks.  (Presumably you found this documentation by following a
  link; ~pl[guards-for-specification].)  For further information
  related to guards other than what you find under ``~il[guard],'' see
  any of the following documentation topics:  ~il[guard-example],
  ~ilc[set-verify-guards-eagerness], ~ilc[set-guard-checking], and
  ~ilc[verify-guards].~/

  ~ilc[Defun] can be made to try to verify the guards on a function.
  This is controlled by the ``~il[defun-mode]'' of the ~ilc[defun];
  ~pl[defun-mode].  The ~il[defun-mode] is either as specified with the
  ~c[:mode] ~c[xarg] of the ~ilc[defun] or else defaults to the default
  ~il[defun-mode].  ~l[default-defun-mode].  If the ~il[defun-mode] of the
  ~ilc[defun] is ~c[:]~ilc[logic] and either a ~c[:]~ilc[guard] is specified or
  ~c[:]~ilc[verify-guards] ~c[t] is specified in the ~ilc[xargs], then we attempt to
  verify the guards of the function.  Otherwise we do not.

  It is sometimes impossible for the system to verify the guards of a
  recursive function at definition time.  For example, the guard
  conjectures might require the invention and proof of some
  inductively derived property of the function (as often happens when
  the value of a recursive call is fed to a guarded subroutine).  So
  sometimes it is necessary to define the function using
  ~c[:verify-guards nil] then to state and prove key theorems about the
  function, and only then have the system attempt guard verification.
  Post-~ilc[defun] guard verification is achieved via the event
  ~ilc[verify-guards].  ~l[verify-guards].

  It should be emphasized that guard verification affects only two
  things: how fast ACL2 can evaluate the function and whether the
  function is executed correctly by raw Common Lisp, without guard
  violations.  Since ACL2 does not use the raw Common Lisp definition
  of a function to evaluate its calls unless that function's guards
  have been verified, the latter effect is felt only if you run
  functions in raw Common Lisp rather than via ACL2's command loop.

  Guard verification does not otherwise affect the theorem prover or
  the semantics of a definition.  If you are not planning on running
  your function on ``big'' inputs and you don't care if your function
  runs correctly in raw Common Lisp (e.g., you have formalized some
  abstract mathematical property and just happened to use ACL2 as your
  language), there is no need to suffer through guard verification.
  Often users start by not doing guard verification and address that
  problem later.  Sometimes you are driven to it, even in mathematical
  projects, because you find that you want to run your functions
  particularly fast or in raw Common Lisp.

  If ~ilc[certify-book] is used to compile a file, and the file contains
  functions with unverified guard conjectures, then you will be warned
  that the compiled file cannot be loaded into raw Common Lisp with
  the expectation that the functions will run correctly.  This is just
  the same point we have been making:  ACL2 and Common Lisp agree only
  on the restricted domains specified by our guards.  When guards are
  violated, Common Lisp can do anything.  When you call a compiled
  function on arguments violating its guards, the chances are only
  increased that Common Lisp will go berserk, because compiled
  functions generally check fewer things at runtime and tend to be
  more fragile than interpreted ones.")

(deflabel guard-evaluation-examples-script
  :doc
  ":Doc-Section Guard

  a script to show combinations of ~il[defun-mode]s and ~il[guard]-checking~/

  Below is a script that illustrates the combination of ~il[defun-mode]s ~-[]
  ~c[:]~ilc[program] mode, ~c[:]~ilc[logic] mode without ~il[guard]s verified,
  and ~c[:]~ilc[logic] mode with ~il[guard]s verified ~-[] with values from
  ~ilc[set-guard-checking] ~-[] ~c[t] (the default), ~c[:all], ~c[:none], and
  ~c[nil].  (It does not illustrate the value ~c[:nowarn], which is the same as
  ~c[t] except for inhibiting a warning.)  The script also illustrates cases
  where the guard is not, or is, ~c[t].

  ~l[guard-evaluation-examples-log] for result of running this script.  Before
  presenting the script below, we give some instructions in case you want to
  run it yourself.

  ~l[set-guard-checking] for discussion of the interaction between
  ~il[defun-mode]s and ~il[guard]-checking that is illustrated by this script.
  Also ~pl[guard-evaluation-table] for a succinct table, with associated
  discussion, that covers in detail the interactions illustrated here.

  The script mentions the running of ``Tracing Code''.  The code is the
  following sequence of commands.

  ~bv[]
  (trace$ fact)
  :set-guard-checking t
  (fact 2)
  (fact t)
  :set-guard-checking :all
  (fact 2)
  (fact t)
  :set-guard-checking :none
  (fact 2)
  (fact t)
  :set-guard-checking nil
  (fact 2)
  (fact t)
  ~ev[]

  If you want to run the script yourself, you may find it handy to use the
  following Emacs keyboard macro for running the tracing code in 2-window mode,
  with the cursor in the window with the script and ACL2 running in the other
  window.

  ~bv[]
  (fset 'step-guard-script
     [?\C-a ?\C-  ?\C-e ?\M-w ?\C-a ?\C-n
      ?\C-x ?o ?\M-> ?\C-y return ?\C-x ?o])

  ; Put it on a key (if you have defined the indicated keymap by using
  ; emacs/emacs-acl2.el):
  (define-key ctl-t-keymap \"r\" 'step-guard-script)
  ~ev[]

  The script follows.~/

  ~bv[]
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Program mode
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun fact (x)
           (declare (xargs :guard (integerp x)
                           :mode :program))
           (if (posp x)
               (* x (fact (1- x)))
             1))

  ; Run the Tracing Code here.  It shows execution in raw Lisp in the t and nil
  ; cases of :set-guard-checking, but not in the :all or :none cases.  We get a
  ; guard violation for argument t in the case :set-guard-checking t.

  :u

  (defun fact (x)
           (declare (xargs :guard t
                           :mode :program))
           (if (posp x)
               (* x (fact (1- x)))
             1))

  ; Run the Tracing Code here.  It should give the same results as above,
  ; except that we no longer get a guard violation in the case
  ; :set-guard-checking t.

  :u

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Logic mode, guard other than t
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun fact (x)
           (declare (xargs :guard (integerp x)
                           :verify-guards nil
                           :mode :logic))
           (if (posp x)
               (* x (fact (1- x)))
             1))

  ; Run the Tracing Code here.  It should give guard violations for (fact t)
  ; with guard-checking set to t or :all.  It should never run in raw Lisp,
  ; because we have not verified guards.  In the t case, we should get a
  ; warning about avoiding the guard check on recursive calls.

  (verify-guards fact)

  ; Run the Tracing Code here.  The results should be as described just above,
  ; except that now we go into raw Lisp for (fact 2) with guard-checking other
  ; than :none.

  :u
  :u

  ; The following definition is the same as above, except that guards are
  ; verified.

  (defun fact (x)
           (declare (xargs :guard (integerp x)
                           :mode :logic))
           (if (posp x)
               (* x (fact (1- x)))
             1))

  ; Run the Tracing Code here.  We should get the same traces as in the
  ; immediately preceding case, since guards had been verified in both cases.

  :u

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Logic mode, guard t
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun fact (x)
           (declare (xargs :guard t
                           :verify-guards nil
                           :mode :logic))
           (if (posp x)
               (* x (fact (1- x)))
             1))

  ; Run the Tracing Code here.  We should never go in to raw Lisp, because
  ; guards have not been verified.  We will see the same traces for (fact 2) as
  ; with the (integerp x) guard above with :verify-guards nil specified, except
  ; that there is no warning for :set-guard-checking t about recursive calls.
  ; And, there are no guard violations for (fact t), of course, since posp
  ; (necessarily, if we are to verify guards) has a guard of t.

  (verify-guards fact)

  ; Run the Tracing Code here.  Now that guards have been verified, the :none
  ; trace for (fact 2) differs from the corresponding (guard-verified) trace
  ; for guard (integerp x) because now the guard is t, so we can (and do) go
  ; directly into raw Lisp without the need to check the guard.  The (fact 2)
  ; traces other than for :none are the same as in the corresponding trace for
  ; guard (integerp x).  And of course (fact t) now computes without guard
  ; violations, and using raw Lisp even in the :none case,

  ~ev[]~/")

(deflabel guard-evaluation-examples-log
  :doc
  ":Doc-Section Guard

  log showing combinations of ~il[defun-mode]s and ~il[guard]-checking~/

  ~l[guard-evaluation-examples-script] for a script that shows the interaction
  of ~il[defun-mode]s with the value set by ~ilc[set-guard-checking].  Here, we
  present a log resulting from running this script (using ACL2 Version  3.0,
  built on Allegro Common Lisp).~/

  ~l[set-guard-checking] for discussion of the interaction between
  ~il[defun-mode]s and ~il[guard]-checking that is illustrated by this script.
  Also ~pl[guard-evaluation-table] for a succinct table, with associated
  discussion, that covers in detail the interactions illustrated here.

  ~bv[]
  ACL2 !>(defun fact (x)
           (declare (xargs :guard (integerp x)
                           :mode :program))
           (if (posp x)
               (* x (fact (1- x)))
             1))

  Summary
  Form:  ( DEFUN FACT ...)
  Rules: NIL
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   FACT
  ACL2 !>(trace$ fact)
  NIL
  ACL2 !>:set-guard-checking t

  Guard-checking-on already has value T.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)


  ACL2 Error in TOP-LEVEL:  The guard for the :program function symbol
  FACT, which is (INTEGERP X), is violated by the arguments in the call
  (FACT T).  See :DOC wet for how you might be able to get an error backtrace.
  See :DOC set-guard-checking for information about suppressing this
  check with (set-guard-checking :none), as recommended for new users.

  ACL2 !>:set-guard-checking :all

  Leaving guard checking on, but changing value to :ALL.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (ACL2_*1*_ACL2::FACT 1)
      3> (ACL2_*1*_ACL2::FACT 0)
      <3 (ACL2_*1*_ACL2::FACT 1)
    <2 (ACL2_*1*_ACL2::FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)


  ACL2 Error in TOP-LEVEL:  The guard for the :program function symbol
  FACT, which is (INTEGERP X), is violated by the arguments in the call
  (FACT T).  See :DOC wet for how you might be able to get an error backtrace.
  See :DOC set-guard-checking for information about suppressing this
  check with (set-guard-checking :none), as recommended for new users.

  ACL2 !>:set-guard-checking :none

  Turning off guard checking entirely.  To allow execution in raw Lisp
  for functions with guards other than T, while continuing to mask guard
  violations, :SET-GUARD-CHECKING NIL.  See :DOC set-guard-checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (ACL2_*1*_ACL2::FACT 1)
      3> (ACL2_*1*_ACL2::FACT 0)
      <3 (ACL2_*1*_ACL2::FACT 1)
    <2 (ACL2_*1*_ACL2::FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >:set-guard-checking nil

  Masking guard violations but still checking guards.  To avoid guard
  checking entirely, :SET-GUARD-CHECKING :NONE.  See :DOC set-guard-
  checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
    2> (FACT T)
    <2 (FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >:u

            0:x(EXIT-BOOT-STRAP-MODE)
  ACL2 >(defun fact (x)
           (declare (xargs :guard t
                           :mode :program))
           (if (posp x)
               (* x (fact (1- x)))
             1))

  Summary
  Form:  ( DEFUN FACT ...)
  Rules: NIL
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   FACT
  ACL2 >(trace$ fact)
  NIL
  ACL2 >:set-guard-checking t

  Turning guard checking on, value T.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
    2> (FACT T)
    <2 (FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 !>:set-guard-checking :all

  Leaving guard checking on, but changing value to :ALL.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (ACL2_*1*_ACL2::FACT 1)
      3> (ACL2_*1*_ACL2::FACT 0)
      <3 (ACL2_*1*_ACL2::FACT 1)
    <2 (ACL2_*1*_ACL2::FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 !>:set-guard-checking :none

  Turning off guard checking entirely.  To allow execution in raw Lisp
  for functions with guards other than T, while continuing to mask guard
  violations, :SET-GUARD-CHECKING NIL.  See :DOC set-guard-checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (ACL2_*1*_ACL2::FACT 1)
      3> (ACL2_*1*_ACL2::FACT 0)
      <3 (ACL2_*1*_ACL2::FACT 1)
    <2 (ACL2_*1*_ACL2::FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >:set-guard-checking nil

  Masking guard violations but still checking guards.  To avoid guard
  checking entirely, :SET-GUARD-CHECKING :NONE.  See :DOC set-guard-
  checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
    2> (FACT T)
    <2 (FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >:u

            0:x(EXIT-BOOT-STRAP-MODE)
  ACL2 >(defun fact (x)
           (declare (xargs :guard (integerp x)
                           :verify-guards nil
                           :mode :logic))
           (if (posp x)
               (* x (fact (1- x)))
             1))

  For the admission of FACT we will use the relation O< (which is known
  to be well-founded on the domain recognized by O-P) and the measure
  (ACL2-COUNT X).  The non-trivial part of the measure conjecture is

  Goal
  (IMPLIES (POSP X)
           (O< (ACL2-COUNT (+ -1 X))
               (ACL2-COUNT X))).

  By the simple :definition POSP we reduce the conjecture to

  Goal'
  (IMPLIES (AND (INTEGERP X) (< 0 X))
           (O< (ACL2-COUNT (+ -1 X))
               (ACL2-COUNT X))).

  But we reduce the conjecture to T, by case analysis.

  Q.E.D.

  That completes the proof of the measure theorem for FACT.  Thus, we
  admit this function under the principle of definition. We observe that
  the type of FACT is described by the theorem 
  (AND (INTEGERP (FACT X)) (< 0 (FACT X))).  We used the :compound-recognizer
  rule POSP-COMPOUND-RECOGNIZER and primitive type reasoning.

  Summary
  Form:  ( DEFUN FACT ...)
  Rules: ((:COMPOUND-RECOGNIZER POSP-COMPOUND-RECOGNIZER)
          (:DEFINITION NOT)
          (:DEFINITION POSP)
          (:FAKE-RUNE-FOR-TYPE-SET NIL))
  Warnings:  None
  Time:  0.01 seconds (prove: 0.01, print: 0.00, other: 0.00)
   FACT
  ACL2 >(trace$ fact)
  NIL
  ACL2 >:set-guard-checking t

  Turning guard checking on, value T.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)

  ACL2 Warning [Guards] in TOP-LEVEL:  Guard-checking will be inhibited
  on recursive calls of the executable counterpart (i.e., in the ACL2
  logic) of FACT.  To check guards on all recursive calls:
    (set-guard-checking :all)
  To leave behavior unchanged except for inhibiting this message:
    (set-guard-checking :nowarn)

  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)


  ACL2 Error in TOP-LEVEL:  The guard for the function symbol FACT, which
  is (INTEGERP X), is violated by the arguments in the call (FACT T).
  See :DOC wet for how you might be able to get an error backtrace. 
  See :DOC set-guard-checking for information about suppressing this
  check with (set-guard-checking :none), as recommended for new users.

  ACL2 !>:set-guard-checking :all

  Leaving guard checking on, but changing value to :ALL.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (ACL2_*1*_ACL2::FACT 1)
      3> (ACL2_*1*_ACL2::FACT 0)
      <3 (ACL2_*1*_ACL2::FACT 1)
    <2 (ACL2_*1*_ACL2::FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)


  ACL2 Error in TOP-LEVEL:  The guard for the function symbol FACT, which
  is (INTEGERP X), is violated by the arguments in the call (FACT T).
  See :DOC wet for how you might be able to get an error backtrace. 
  See :DOC set-guard-checking for information about suppressing this
  check with (set-guard-checking :none), as recommended for new users.

  ACL2 !>:set-guard-checking :none

  Turning off guard checking entirely.  To allow execution in raw Lisp
  for functions with guards other than T, while continuing to mask guard
  violations, :SET-GUARD-CHECKING NIL.  See :DOC set-guard-checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >:set-guard-checking nil

  Masking guard violations but still checking guards.  To avoid guard
  checking entirely, :SET-GUARD-CHECKING :NONE.  See :DOC set-guard-
  checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >(verify-guards fact)

  Computing the guard conjecture for FACT....

  The guard conjecture for FACT is trivial to prove, given the :compound-
  recognizer rule POSP-COMPOUND-RECOGNIZER, primitive type reasoning
  and the :type-prescription rule FACT.  FACT is compliant with Common
  Lisp.

  Summary
  Form:  ( VERIFY-GUARDS FACT)
  Rules: ((:COMPOUND-RECOGNIZER POSP-COMPOUND-RECOGNIZER)
          (:FAKE-RUNE-FOR-TYPE-SET NIL)
          (:TYPE-PRESCRIPTION FACT))
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   FACT
  ACL2 >(trace$ fact)
  NIL
  ACL2 >:set-guard-checking t

  Turning guard checking on, value T.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)


  ACL2 Error in TOP-LEVEL:  The guard for the function symbol FACT, which
  is (INTEGERP X), is violated by the arguments in the call (FACT T).
  See :DOC wet for how you might be able to get an error backtrace. 
  See :DOC set-guard-checking for information about suppressing this
  check with (set-guard-checking :none), as recommended for new users.

  ACL2 !>:set-guard-checking :all

  Leaving guard checking on, but changing value to :ALL.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)


  ACL2 Error in TOP-LEVEL:  The guard for the function symbol FACT, which
  is (INTEGERP X), is violated by the arguments in the call (FACT T).
  See :DOC wet for how you might be able to get an error backtrace. 
  See :DOC set-guard-checking for information about suppressing this
  check with (set-guard-checking :none), as recommended for new users.

  ACL2 !>:set-guard-checking :none

  Turning off guard checking entirely.  To allow execution in raw Lisp
  for functions with guards other than T, while continuing to mask guard
  violations, :SET-GUARD-CHECKING NIL.  See :DOC set-guard-checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >:set-guard-checking nil

  Masking guard violations but still checking guards.  To avoid guard
  checking entirely, :SET-GUARD-CHECKING :NONE.  See :DOC set-guard-
  checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >:u
   L        1:x(DEFUN FACT (X) ...)
  ACL2 >:u
            0:x(EXIT-BOOT-STRAP-MODE)
  ACL2 >(defun fact (x)
           (declare (xargs :guard (integerp x)
                           :mode :logic))
           (if (posp x)
               (* x (fact (1- x)))
             1))

  For the admission of FACT we will use the relation O< (which is known
  to be well-founded on the domain recognized by O-P) and the measure
  (ACL2-COUNT X).  The non-trivial part of the measure conjecture is

  Goal
  (IMPLIES (POSP X)
           (O< (ACL2-COUNT (+ -1 X))
               (ACL2-COUNT X))).

  By the simple :definition POSP we reduce the conjecture to

  Goal'
  (IMPLIES (AND (INTEGERP X) (< 0 X))
           (O< (ACL2-COUNT (+ -1 X))
               (ACL2-COUNT X))).

  But we reduce the conjecture to T, by case analysis.

  Q.E.D.

  That completes the proof of the measure theorem for FACT.  Thus, we
  admit this function under the principle of definition. We observe that
  the type of FACT is described by the theorem 
  (AND (INTEGERP (FACT X)) (< 0 (FACT X))).  We used the :compound-recognizer
  rule POSP-COMPOUND-RECOGNIZER and primitive type reasoning.

  Computing the guard conjecture for FACT....

  The guard conjecture for FACT is trivial to prove, given the :compound-
  recognizer rule POSP-COMPOUND-RECOGNIZER, primitive type reasoning
  and the :type-prescription rule FACT.  FACT is compliant with Common
  Lisp.

  Summary
  Form:  ( DEFUN FACT ...)
  Rules: ((:COMPOUND-RECOGNIZER POSP-COMPOUND-RECOGNIZER)
          (:DEFINITION NOT)
          (:DEFINITION POSP)
          (:FAKE-RUNE-FOR-TYPE-SET NIL)
          (:TYPE-PRESCRIPTION FACT))
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   FACT
  ACL2 >(trace$ fact)
  NIL
  ACL2 >:set-guard-checking t

  Turning guard checking on, value T.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)


  ACL2 Error in TOP-LEVEL:  The guard for the function symbol FACT, which
  is (INTEGERP X), is violated by the arguments in the call (FACT T).
  See :DOC wet for how you might be able to get an error backtrace. 
  See :DOC set-guard-checking for information about suppressing this
  check with (set-guard-checking :none), as recommended for new users.

  ACL2 !>:set-guard-checking :all

  Leaving guard checking on, but changing value to :ALL.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)


  ACL2 Error in TOP-LEVEL:  The guard for the function symbol FACT, which
  is (INTEGERP X), is violated by the arguments in the call (FACT T).
  See :DOC wet for how you might be able to get an error backtrace. 
  See :DOC set-guard-checking for information about suppressing this
  check with (set-guard-checking :none), as recommended for new users.

  ACL2 !>:set-guard-checking :none

  Turning off guard checking entirely.  To allow execution in raw Lisp
  for functions with guards other than T, while continuing to mask guard
  violations, :SET-GUARD-CHECKING NIL.  See :DOC set-guard-checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >:set-guard-checking nil

  Masking guard violations but still checking guards.  To avoid guard
  checking entirely, :SET-GUARD-CHECKING :NONE.  See :DOC set-guard-
  checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >:u
            0:x(EXIT-BOOT-STRAP-MODE)
  ACL2 >(defun fact (x)
           (declare (xargs :guard t
                           :verify-guards nil
                           :mode :logic))
           (if (posp x)
               (* x (fact (1- x)))
             1))

  For the admission of FACT we will use the relation O< (which is known
  to be well-founded on the domain recognized by O-P) and the measure
  (ACL2-COUNT X).  The non-trivial part of the measure conjecture is

  Goal
  (IMPLIES (POSP X)
           (O< (ACL2-COUNT (+ -1 X))
               (ACL2-COUNT X))).

  By the simple :definition POSP we reduce the conjecture to

  Goal'
  (IMPLIES (AND (INTEGERP X) (< 0 X))
           (O< (ACL2-COUNT (+ -1 X))
               (ACL2-COUNT X))).

  But we reduce the conjecture to T, by case analysis.

  Q.E.D.

  That completes the proof of the measure theorem for FACT.  Thus, we
  admit this function under the principle of definition. We observe that
  the type of FACT is described by the theorem 
  (AND (INTEGERP (FACT X)) (< 0 (FACT X))).  We used the :compound-recognizer
  rule POSP-COMPOUND-RECOGNIZER and primitive type reasoning.

  Summary
  Form:  ( DEFUN FACT ...)
  Rules: ((:COMPOUND-RECOGNIZER POSP-COMPOUND-RECOGNIZER)
          (:DEFINITION NOT)
          (:DEFINITION POSP)
          (:FAKE-RUNE-FOR-TYPE-SET NIL))
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   FACT
  ACL2 >(trace$ fact)
  NIL
  ACL2 >:set-guard-checking t

  Turning guard checking on, value T.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 !>:set-guard-checking :all

  Leaving guard checking on, but changing value to :ALL.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (ACL2_*1*_ACL2::FACT 1)
      3> (ACL2_*1*_ACL2::FACT 0)
      <3 (ACL2_*1*_ACL2::FACT 1)
    <2 (ACL2_*1*_ACL2::FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 !>:set-guard-checking :none

  Turning off guard checking entirely.  To allow execution in raw Lisp
  for functions with guards other than T, while continuing to mask guard
  violations, :SET-GUARD-CHECKING NIL.  See :DOC set-guard-checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >:set-guard-checking nil

  Masking guard violations but still checking guards.  To avoid guard
  checking entirely, :SET-GUARD-CHECKING :NONE.  See :DOC set-guard-
  checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >(verify-guards fact)

  Computing the guard conjecture for FACT....

  The guard conjecture for FACT is trivial to prove, given the :compound-
  recognizer rule POSP-COMPOUND-RECOGNIZER and the :type-prescription
  rule FACT.  FACT is compliant with Common Lisp.

  Summary
  Form:  ( VERIFY-GUARDS FACT)
  Rules: ((:COMPOUND-RECOGNIZER POSP-COMPOUND-RECOGNIZER)
          (:TYPE-PRESCRIPTION FACT))
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   FACT
  ACL2 >(trace$ fact)
  NIL
  ACL2 >:set-guard-checking t

  Turning guard checking on, value T.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
    2> (FACT T)
    <2 (FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 !>:set-guard-checking :all

  Leaving guard checking on, but changing value to :ALL.

  ACL2 !>(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 !>(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
    2> (FACT T)
    <2 (FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 !>:set-guard-checking :none

  Turning off guard checking entirely.  To allow execution in raw Lisp
  for functions with guards other than T, while continuing to mask guard
  violations, :SET-GUARD-CHECKING NIL.  See :DOC set-guard-checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
    2> (FACT T)
    <2 (FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >:set-guard-checking nil

  Masking guard violations but still checking guards.  To avoid guard
  checking entirely, :SET-GUARD-CHECKING :NONE.  See :DOC set-guard-
  checking.

  ACL2 >(fact 2)
  1> (ACL2_*1*_ACL2::FACT 2)
    2> (FACT 2)
      3> (FACT 1)
        4> (FACT 0)
        <4 (FACT 1)
      <3 (FACT 1)
    <2 (FACT 2)
  <1 (ACL2_*1*_ACL2::FACT 2)
  2
  ACL2 >(fact t)
  1> (ACL2_*1*_ACL2::FACT T)
    2> (FACT T)
    <2 (FACT 1)
  <1 (ACL2_*1*_ACL2::FACT 1)
  1
  ACL2 >
  ~ev[]~/")

(deflabel guard-evaluation-table
  :doc
  ":Doc-Section Guard

  a table that shows combinations of ~il[defun-mode]s and ~il[guard]-checking~/

  ~l[set-guard-checking] for an introduction to the topic discussed here.  Also
  ~pl[guard] for a general discussion of guards, and
  ~pl[guard-evaluation-examples-script] for a script that illustrates
  combinations presented below.

  The table below illustrates the interaction of the ~il[defun-mode] with the
  value supplied to ~ilc[set-guard-checking].  The first row considers
  functions defined in ~c[:]~ilc[program] mode; the other two consider
  functions defined in ~c[:]~ilc[logic] mode.  The columns correspond to four
  values of state global ~c['guard-checking-on], as supplied to
  ~ilc[set-guard-checking].  (A fifth value, ~c[:nowarn], is similar to ~c[t]
  but suppresses warnings encountered with ~c[t] (as explained in those warning
  messages), and is not considered here.)  During proofs,
  ~c['guard-checking-on] is set to ~c[nil] regardless of how this variable has
  been set in the top-level loop.

  Below this table, we make some comments about its entries, ordered by row and
  then by column.  For example, when we refer to ``b2'' we are discussing the
  execution of a ~c[:]~ilc[logic] mode function whose guards have not been
  verified, after having executed ~c[:]~ilc[set-guard-checking]~c[ :all].

  ~bv[]
     guard-checking-on:  (1)t      (2):all   (3):none   (4)nil

   (a) :program             a1        a2        a3        a4
   (b) guards not verified  b1        b2        b3        b4
   (c) guards verified      c1        c2        c3        c4
  ~ev[]

  a1. Check the ~il[guard] upon entry, then use the raw Lisp code if the guard
  checks (else cause an error).  This is a common setting when one wants a
  little guard checking but also wants the efficiency of raw Lisp.  But note
  that you can get raw Lisp errors.  For example, if you make the definition
  ~c[(defun foo (x) (car x))] in ~c[:]~ilc[program] mode and execute
  ~c[:]~ilc[set-guard-checking]~c[ t], and then execute ~c[(foo 3)], you will
  likely get an error from the call ~c[(car 3)] made in raw Lisp.

  a2. For built-in (predefined) functions, see a1 instead.  Otherwise:~nl[]
  Check the ~il[guard], without exception.  Thus, we never run the raw Lisp
  code in this case.  This can be useful when testing ~c[:]~ilc[program] mode
  functions, but you may want to run ~c[:]~ilc[comp]~c[ t] or at least
  ~c[:]~ilc[comp]~c[ :exec] in this case, so that the execution is done using
  compiled code.

  a3. For built-in (predefined) functions, see a4 instead.  Otherwise:~nl[]
  Do not check the ~il[guard].  For ~c[:]~ilc[program] mode functions, we never
  run the raw Lisp code in this case; so if you care about efficiency, see the
  comment in a2 above about ~c[:]~ilc[comp].  This combination is useful if you
  are using ACL2 as a programming language and do not want to prove theorems
  about your functions or suffer ~il[guard] violations.  In this case, you can
  forget about any connection between ACL2 and Common Lisp.

  a4. Run the raw Lisp code without checking ~il[guard]s at all.  Thus, for
  ~c[:]~ilc[program] mode functions, the ~c[nil] setting is often preferable to
  the ~c[:none] setting because you get the efficiency of raw Lisp execution.
  However, with ~c[nil] you can therefore get hard Lisp errors as in a1 above.

  b1. Guards are checked at the top-level, though not at self-recursive calls.
  We never run the raw Lisp code in this case; guards would need to be verified
  first.

  b2. Unlike the ~c[t] setting, guards are checked even on self-recursive
  calls.  But like the ~c[t] setting, we do not run the raw Lisp code.  Use
  this setting if you want guards checked on each recursive call in spite of
  the cost of doing so.

  b3, b4. Execution avoids the raw Lisp code and never checks guards.  The
  ~c[nil] and ~c[:none] settings behave the same in this case (i.e., for
  ~c[:]~ilc[logic] mode functions whose guards have not been verified).

  c1, c2. Guards are checked.  If the checks pass, evaluation takes place using
  the raw Lisp code.  If the checks fail, we get a guard violation.  Either
  way, we do not execute ``in the logic''; we only execute using the raw Lisp
  code.  Note that ~c[t] and ~c[:all] behave the same in this case, (i.e. for
  ~c[:]~ilc[logic] mode functions whose ~il[guard]s have been verified).

  c3, c4. For the ~c[:none] and ~c[nil] settings, ~c[:]~ilc[logic] mode
  functions whose guards have been verified will never cause guard violations.
  However, with ~c[nil], guards are still checked: if the check succeeds, then
  evaluation is done using the raw Lisp code, and if not, it is done by the
  ``logic'' code, including self-recursive calls (though unlike the ~c[t] case,
  we will not see a warning about this).  But with ~c[:none], no guard checking
  is done, so the only time the raw Lisp code will be executed is when the
  guard is ~c[t] (so that no evaluation is necessary).  Thus, if you use
  ~c[:none] and you want a function ~c[(foo x)] with guard ~c[(g x)] to execute
  using raw Lisp code, you can write a ``wrapper'' function with a guard of
  ~c[t]:
  ~bv[]
  (defun foo-wrap (x)
    (if (g x)
        (foo x)
      'do-not-case))
  ~ev[]
  If you want the speed of executing raw Lisp code and you have non-trivial
  guards on functions that you want to call at the top-level, use ~c[nil]
  rather than ~c[:none].

  ~/~/")

(defun guard-lst (fns stobj-optp w)
  (cond ((null fns) nil)
        (t (cons (guard (car fns) stobj-optp w)
                 (guard-lst (cdr fns) stobj-optp w)))))

(defmacro equivalence-relationp (fn w)

; See the Essay on Equivalence, Refinements, and Congruence-based
; Rewriting.

; (Note: At the moment, the fact that fn is an equivalence relation is
; encoded merely by existence of a non-nil 'coarsenings property.  No
; :equivalence rune explaining why fn is an equivalence relation is to
; be found there -- though such a rune does exist and is indeed found
; among the 'congruences of fn itself.  We do not track the use of
; equivalence relations, we just use them anonymously.  It would be
; good to track them and report them.  When we do that, read the Note
; on Tracking Equivalence Runes in subst-type-alist1.)

  `(let ((fn ,fn))

; While both equal and iff have non-nil coarsenings properties, we make
; special cases of them here because they are common and we wish to avoid
; the getprop.

     (or (eq fn 'equal)
         (eq fn 'iff)
         (and (not (flambdap fn))
              (getprop fn 'coarsenings nil 'current-acl2-world ,w)))))

(defun >=-len (x n)
  (declare (xargs :guard (and (integerp n) (<= 0 n))))
  (if (= n 0)
      t
      (if (atom x)
          nil
          (>=-len (cdr x) (1- n)))))

(defun all->=-len (lst n)
  (declare (xargs :guard (and (integerp n) (<= 0 n))))
  (if (atom lst)
      (eq lst nil)
      (and (>=-len (car lst) n)
           (all->=-len (cdr lst) n))))

(defun strip-cadrs (x)
  (declare (xargs :guard (all->=-len x 2)))
  (cond ((null x) nil)
        (t (cons (cadar x) (strip-cadrs (cdr x))))))

; Rockwell Addition: Just moved from other-events.lisp

(defun strip-cddrs (x)
  (declare (xargs :guard (all->=-len x 2)))
  (cond ((null x) nil)
        (t (cons (cddar x) (strip-cddrs (cdr x))))))

(defun doubleton-list-p (x)
  (cond ((atom x) (equal x nil))
        (t (and (true-listp (car x))
                (eql (length (car x)) 2)
                (doubleton-list-p (cdr x))))))

(defun global-set-lst (alist wrld)
  (cond ((null alist) wrld)
        (t (global-set-lst (cdr alist)
                           (global-set (caar alist)
                                       (cadar alist)
                                       wrld)))))

(mutual-recursion

(defun sublis-var (alist form)
  (declare (xargs :guard (and (symbol-alistp alist)
                              (pseudo-termp form))))

; The two following comments come from the nqthm version of this
; function and do not necessarily pertain to ACL2 (but see below).

;     In REWRITE-WITH-LEMMAS we use this function with the nil alist
;     to put form into quote normal form.  Do not optimize this
;     function for the nil alist.

;     This is the only function in the theorem prover that we
;     sometimes call with a "term" that is not in quote normal form.
;     However, even this function requires that form be at least a
;     pseudo-termp.

; Note however the comment from rewrite-with-lemma that says:
;   The sublis-var below normalizes the explicit constant
;   constructors in evaled-hyp, e.g., (cons '1 '2) becomes '(1 . 2).

  (cond ((variablep form)
         (let ((a (assoc-eq form alist)))
           (cond (a (cdr a))
                 (t form))))
        ((fquotep form)
         form)
        (t (cons-term (ffn-symb form)
                      (sublis-var-lst alist (fargs form))))))

(defun sublis-var-lst (alist l)
  (declare (xargs :guard (and (symbol-alistp alist)
                              (pseudo-term-listp l))))
  (if (null l)
      nil
    (cons (sublis-var alist (car l))
          (sublis-var-lst alist (cdr l)))))

)

(defun subcor-var1 (vars terms var)
  (declare (xargs :guard (and (true-listp vars)
                              (true-listp terms)
                              (equal (length vars) (length terms))
                              (variablep var))))
  (cond ((null vars) var)
        ((eq var (car vars)) (car terms))
        (t (subcor-var1 (cdr vars) (cdr terms) var))))

(mutual-recursion

(defun subcor-var (vars terms form)

; "Subcor" stands for "substitute corresponding elements".  Vars and terms
; are in 1:1 correspondence and we substitute terms for corresponding vars
; into form.  This function was called sub-pair-var in nqthm.

  (cond ((variablep form)
         (subcor-var1 vars terms form))
        ((fquotep form) form)
        (t (cons-term (ffn-symb form)
                      (subcor-var-lst vars terms (fargs form))))))

(defun subcor-var-lst (vars terms forms)
  (cond ((null forms) nil)
        (t (cons (subcor-var vars terms (car forms))
                 (subcor-var-lst vars terms (cdr forms))))))

)

(defun conjoin2 (t1 t2)

; This function returns a term representing the logical conjunction of
; t1 and t2.  The term is IFF-equiv to (AND t1 t2).  But, the term is
; not EQUAL to (AND t1 t2) because if t2 is *t* we return t1's value,
; while (AND t1 t2) would return *t* if t1's value were non-NIL.

  (cond ((equal t1 *nil*) *nil*)
        ((equal t2 *nil*) *nil*)
        ((equal t1 *t*) t2)
        ((equal t2 *t*) t1)
        (t (fcons-term* 'if t1 t2 *nil*))))

(defun conjoin (l)
  (cond ((null l) *t*)
        ((null (cdr l)) (car l))
        (t (conjoin2 (car l) (conjoin (cdr l))))))

(defun cons-count-ac (x i)
  (cond ((atom x) i)
        (t (cons-count-ac (cdr x) (cons-count-ac (car x) (1+ i))))))

(defun cons-count (x)
  (cons-count-ac x 0))

; We now develop the code to take a translated term and "untranslate"
; it into something more pleasant to read.

(defun car-cdr-nest1 (term ad-lst n)
  (cond ((or (int= n 4)
             (variablep term)
             (fquotep term)
             (and (not (eq (ffn-symb term) 'car))
                  (not (eq (ffn-symb term) 'cdr))))
         (mv ad-lst term))
        (t (car-cdr-nest1 (fargn term 1)
                          (cons (if (eq (ffn-symb term) 'car)
                                    #\A
                                  #\D)
                                ad-lst)
                          (1+ n)))))

(defun car-cdr-nest (term)
  (cond ((variablep term) (mv nil term))
        ((fquotep term) (mv nil term))
        ((or (eq (ffn-symb term) 'car)
             (eq (ffn-symb term) 'cdr))
         (mv-let (ad-lst guts)
           (car-cdr-nest1 (fargn term 1) nil 1)
           (cond
            (ad-lst
             (mv
              (intern
               (coerce
                (cons #\C
                      (cons (if (eq (ffn-symb term) 'car)
                                #\A
                              #\D)
                            (revappend ad-lst '(#\R))))
                'string)
               "ACL2")
              guts))
            (t (mv nil term)))))
        (t (mv nil nil))))

(defun collect-non-trivial-bindings (vars vals)
  (cond ((null vars) nil)
        ((eq (car vars) (car vals))
         (collect-non-trivial-bindings (cdr vars) (cdr vals)))
        (t (cons (list (car vars) (car vals))
                 (collect-non-trivial-bindings (cdr vars) (cdr vals))))))

(defun untranslate-and (p q iff-flg)

; The following theorem illustrates the various cases:
; (thm (and (equal (and t q) q)
;           (iff (and p t) p)
;           (equal (and p (and q1 q2)) (and p q1 q2))))

  (cond ((eq p t) q)
        ((and iff-flg (eq q t)) p)
        ((and (consp q)
              (eq (car q) 'and))
         (cons 'and (cons p (cdr q))))
        (t (list 'and p q))))

(defun untranslate-or (p q)

; The following theorem illustrates the various cases:
; (thm (equal (or p (or q1 q2)) (or p q1 q2))))
      
  (cond ((and (consp q)
              (eq (car q) 'or))
         (cons 'or (cons p (cdr q))))
        (t (list 'or p q))))

(defun case-length (key term)

; Key is either nil or a variablep symbol.  Term is a term.  We are
; imagining printing term as a case on key.  How long is the case
; statement?  Note that every term can be printed as (case key
; (otherwise term)) -- a case of length 1.  If key is nil we choose it
; towards extending the case-length.

  (case-match term
              (('if ('equal key1 ('quote val)) & y)
               (cond ((and (if (null key)
                               (variablep key1)
                             (eq key key1))
                           (eqlablep val))
                      (1+ (case-length key1 y)))
                     (t 1)))
              (('if ('eql key1 ('quote val)) & y)
               (cond ((and (if (null key)
                               (variablep key1)
                             (eq key key1))
                           (eqlablep val))
                      (1+ (case-length key1 y)))
                     (t 1)))
              (('if ('member key1 ('quote val)) & y)
               (cond ((and (if (null key)
                               (variablep key1)
                             (eq key key1))
                           (eqlable-listp val))
                      (1+ (case-length key1 y)))
                     (t 1)))
              (& 1)))

; And we do a similar thing for cond...

(defun cond-length (term)
  (case-match term
              (('if & & z) (1+ (cond-length z)))
              (& 1)))

; In general the following list should be set to contain all the boot-strap
; functions that have boolean type set.

(defconst *untranslate-boolean-primitives*
  '(equal))

(defun right-associated-args (fn term)

; Fn is a function symbol of two arguments.  Term is a call of fn.
; E.g., fn might be 'BINARY-+ and term might be '(BINARY-+ A (BINARY-+
; B C)).  We return the list of arguments in the right-associated fn
; nest, e.g., '(A B C).

  (let ((arg2 (fargn term 2)))
    (cond ((and (nvariablep arg2)
                (not (fquotep arg2))
                (eq fn (ffn-symb arg2)))
           (cons (fargn term 1) (right-associated-args fn arg2)))
          (t (fargs term)))))

(defun dumb-negate-lit (term)
  (declare (xargs :guard (pseudo-termp term)))
  (cond ((variablep term)
         (fcons-term* 'not term))
        ((fquotep term)
         (cond ((equal term *nil*) *t*)
               (t *nil*)))
        ((eq (ffn-symb term) 'not)
         (fargn term 1))
        ((and (eq (ffn-symb term) 'equal)
              (or (equal (fargn term 2) *nil*)
                  (equal (fargn term 1) *nil*)))
         (if (equal (fargn term 2) *nil*)
             (fargn term 1)
             (fargn term 2)))
        (t (fcons-term* 'not term))))

(mutual-recursion

(defun term-stobjs-out-alist (vars args alist wrld)
  (if (endp vars)
      nil
    (let ((st (term-stobjs-out (car args) alist wrld))
          (rest (term-stobjs-out-alist (cdr vars) (cdr args) alist wrld)))
      (if (and st (symbolp st))
          (acons (car vars) st rest)
        rest))))

(defun term-stobjs-out (term alist wrld)
  (cond
   ((variablep term)
    (or (cdr (assoc term alist))
        (and (getprop term 'stobj nil 'current-acl2-world wrld)
             term)))
   ((fquotep term)
    nil)
   (t (let ((fn (ffn-symb term)))
        (cond
         ((member-eq fn '(nth mv-nth))
          (let* ((arg1 (fargn term 1))
                 (n (and (quotep arg1) (cadr arg1))))
            (and (integerp n)
                 (<= 0 n)
                 (let ((term-stobjs-out
                        (term-stobjs-out (fargn term 2) alist wrld)))
                   (and (consp term-stobjs-out)
                        (nth n term-stobjs-out))))))
         ((eq fn 'update-nth)
          (term-stobjs-out (fargn term 3) alist wrld))
         ((flambdap fn) ; (fn args) = ((lambda vars body) args)
          (let ((vars (lambda-formals fn))
                (body (lambda-body fn)))
            (term-stobjs-out body
                             (term-stobjs-out-alist vars (fargs term) alist wrld)
                             wrld)))
         ((eq fn 'if)
          (or (term-stobjs-out (fargn term 2) alist wrld)
              (term-stobjs-out (fargn term 3) alist wrld)))
         (t
          (let ((lst (stobjs-out fn wrld)))
            (cond ((and (consp lst) (null (cdr lst)))
                   (car lst))
                  (t lst)))))))))
)

(defun accessor-root (n term wrld)

; When term is a stobj name, say st, ac is the accessor function for st defined
; to return (nth n st), then untranslate maps (nth n st) to (nth *ac* st).
; The truth is that the 'accessor-names property of st is used to carry this
; out.  Update-nth gets similar consideration.

; But what about (nth 0 (run st n)), where run returns a stobj st?  Presumably
; we would like to print that as (nth *b* (run st n)) where b is the 0th field
; accessor function for st.  We would also like to handle terms such as (nth 1
; (mv-nth 3 (run st n))).  These more general cases are likely to be important
; to making stobj proofs palatable.  There is yet another consideration, which
; is that during proofs, the user may use variable names other than stobj names
; to refer to stobjs.  For example, there may be a theorem of the form
; (... st st0 ...), which could generate a term (nth n st0) during a proof that
; the user would prefer to see printed as (nth *b* st0).

; The present function returns the field name to be returned in place of n when
; untranslating (nth n term) or (update-nth n val term).  Wrld is, of course,
; an ACL2 world.

  (let ((st (term-stobjs-out term
                             (table-alist 'nth-aliases-table wrld)
                             wrld)))
    (and st
         (symbolp st)
         (let ((accessor-names
                (getprop st 'accessor-names nil 'current-acl2-world wrld)))
           (and accessor-names 
                (< n (car (dimensions st accessor-names)))
                (aref1 st accessor-names n))))))

; The LD Specials

; The function LD will "bind" some state globals in the sense that it will
; smash their global values and then restore the old values upon completion.
; These state globals are called "LD specials".  The LD read-eval-print loop
; will reference these globals.  The user is permitted to set these globals
; with commands executed in LD -- with the understanding that the values are
; lost when LD is exited and the pop occurs.

; To make it easy to reference them and to ensure that they are set to legal
; values, we will define access and update functions for them.  We define the
; functions here rather than in ld.lisp so that we may use them freely in our
; code.

(defun ld-redefinition-action (state)

  ":Doc-Section Miscellaneous
  to allow redefinition without undoing~/

  ~c[Ld-redefinition-action] is an ~ilc[ld] special (~pl[ld]).  The accessor is
  ~c[(ld-redefinition-action state)] and the updater is
  ~c[(set-ld-redefinition-action val state)].

  ~st[WARNING!]  If ~c[ld-redefinition-action] is non-~c[nil] then ACL2 is
  liable to be made unsafe or unsound by ill-considered definitions.  The
  keyword command ~c[:]~ilc[redef] will set ~c[ld-redefinition-action] to a
  convenient setting allowing unsound redefinition.  See below.~/

  When ~c[ld-redefinition-action] is ~c[nil], redefinition is prohibited.  In
  that case, an error message is printed upon any attempt to introduce a name
  that is already in use.  There is one exception to this rule.  It is
  permitted to redefine a function symbol in ~c[:]~ilc[program] mode to be a
  function symbol in ~c[:]~ilc[logic] mode provided the formals and body remain
  the same.  This is the standard way a function ``comes into'' logical
  existence.

  Throughout the rest of this discussion we exclude from our meaning of
  ``redefinition'' the case in which a function in ~c[:]~ilc[program] mode is
  identically redefined in ~c[:]~ilc[logic] mode.  At one time, ACL2 freely
  permitted the ~il[signature]-preserving redefinition of ~c[:]~ilc[program]
  mode functions but it no longer does.  ~l[redefining-programs].

  When ~c[ld-redefinition-action] is non-~c[nil], you are allowed to redefine a
  name that is already in use.  ~st[The system may be rendered unsound] by such
  an act.  It is important to understand how dangerous redefinition is.
  Suppose ~c[fn] is a function symbol that is called from within some other
  function, say ~c[g].  Suppose ~c[fn] is redefined so that its arity changes.
  Then the definition of ~c[g] is rendered syntactically ill-formed by the
  redefinition.  This can be devastating since the entire ACL2 system assumes
  that terms in its data base are well-formed.  For example, if ACL2 executes
  ~c[g] by running the corresponding function in raw Common Lisp the
  redefinition of ~c[fn] may cause raw lisp to break in irreparable ways.  As
  Lisp programmers we live with this all the time by following the simple rule:
  after changing the syntax of a function don't run any function that calls it
  via its old syntax.  This rule also works in the context of the evaluation of
  ACL2 functions, but it is harder to follow in the context of ACL2 deductions,
  since it is hard to know whether the data base contains a path leading the
  theorem prover from facts about one function to facts about another.
  Finally, of course, even if the data base is still syntactically well-formed
  there is no assurance that all the rules stored in it are valid.  For
  example, theorems proved about ~c[g] survive the redefinition of ~c[fn] but
  may have crucially depended on the properties of the old ~c[fn].  In summary,
  we repeat the warning: ~st[all bets are off if you set]
  ~c[ld-redefinition-action] to ~st[non]-~c[nil].

  ACL2 provides some enforcement of the concern above, by disabling
  ~ilc[certify-book] if any ~il[world]-changing ~il[events] exist in the
  certification ~il[world] that were executed with a non-~c[nil] value of
  ~c['ld-redefinition-action].  (This value is checked at the end of each
  top-level command, but the value does not change during evaluation of
  embedded event forms; ~pl[embedded-event-form].)

  If at any point in a session you wish to see the list of all names that have
  been redefined, ~pl[redefined-names].

  That said, we'll give you enough rope to hang yourself.  When
  ~c[ld-redefinition-action] is non-~c[nil], it must be a pair, ~c[(a .  b)].
  The value of ~c[a] determines how the system interacts with you when a
  redefinition is submitted.  The value of ~c[b] allows you to specify how the
  property list of the redefined name is to be ``renewed'' before the
  redefinition.

  There are several dimensions to the space of possibilities controlled by part
  a: Do you want to be queried each time you redefine a name, so you can
  confirm your intention?  (We sometimes make typing mistakes or simply forget
  we have used that name already.)  Do you want to see a warning stating that
  the name has been redefined?  Do you want ACL2 system functions given special
  protection from possible redefinition?  Below are the choices for part
  a:~bq[]

  ~c[:query] ~-[] every attempt to redefine a name will produce a query.  The
  query will allow you to abort the redefinition or proceed.  It will will also
  allow you to specify the part ~c[b] for this redefinition.  ~c[:Query] is the
  recommended setting for users who wish to dabble in redefinition.

  ~c[:warn] ~-[] any user-defined function may be redefined but a
  post-redefinition warning is printed.  The attempt to redefine a system name
  produces a query.  If you are prototyping and testing a big system in ACL2
  this is probably the desired setting for part ~c[a].

  ~c[:doit] ~-[] any user-defined function may be redefined silently
  (without query or warning) but when an attempt is made to redefine a system
  function, a query is made.  This setting is recommended when you start making
  massive changes to your prototyped system (and tire of even the warning
  messages issued by ~c[:warn]).

  ~eq[]In support of our own ACL2 systems ~il[programming] there are two other
  settings.  We suggest ordinary users not use them.~bq[]

  ~c[:warn!] ~-[] every attempt to redefine a name produces a warning but no
  query.  Since ACL2 system functions can be redefined this way, this setting
  should be used by the only-slightly-less-than supremely confident ACL2 system
  hacker.

  ~c[:doit!] ~-[] this setting allows any name to be redefined silently
  (without query or warnings).  ACL2 system functions are fair game.  This
  setting is reserved for the supremely confident ACL2 system hacker.
  (Actually, this setting is used when we are loading massively modified
  versions of the ACL2 source files.)

  ~eq[]Part ~c[b] of ~c[ld-redefinition-action] tells the system how to
  ``renew'' the property list of the name being redefined.  There are two
  choices:~bq[]

  ~c[:erase] ~-[] erase all properties stored under the name, or

  ~c[:overwrite] ~-[] preserve existing properties and let the redefining
  overwrite them.

  ~eq[]It should be stressed that neither of these ~c[b] settings is guaranteed
  to result in an entirely satisfactory state of affairs after the
  redefinition.  Roughly speaking, ~c[:erase] returns the property list of the
  name to the state it was in when the name was first introduced.  Lemmas, type
  information, etc., stored under that name are lost.  Is that what you wanted?
  Sometimes it is, as when the old definition is ``completely wrong.'' But
  other times the old definition was ``almost right'' in the sense that some of
  the work done with it is still (intended to be) valid.  In that case,
  ~c[:overwrite] might be the correct ~c[b] setting.  For example if ~c[fn] was
  a function and is being re-~ilc[defun]'d with the same ~il[signature], then
  the properties stored by the new ~ilc[defun] should overwrite those stored by
  the old ~ilc[defun] but the properties stored by ~ilc[defthm]s will be
  preserved.

  In addition, neither setting will cause ACL2 to erase properties stored under
  other symbols!  Thus, if ~c[FOO] names a rewrite rule which rewrites a term
  beginning with the function symbol ~c[BAR] and you then redefine ~c[FOO] to
  rewrite a term beginning with the function symbol ~c[BAZ], then the old
  version of ~c[FOO] is still available (because the rule itself was added to
  the rewrite rules for ~c[BAR], whose property list was not cleared by
  redefining ~c[FOO]).

  The ~c[b] setting is only used as the default action when no query is made.
  If you choose a setting for part a that produces a query then you will have
  the opportunity, for each redefinition, to specify whether the property list
  is to be erased or overwritten.

  The keyword command ~c[:]~ilc[redef] sets ~c[ld-redefinition-action] to the
  pair ~c[(:query . :overwrite)].  Since the resulting query will give you the
  chance to specify ~c[:erase] instead of ~c[:overwrite], this setting is quite
  convenient.  But when you are engaged in heavy-duty prototyping, you may wish
  to use a setting such as ~c[:warn] or even ~c[:doit].  For that you will have
  to invoke a form such as:
  ~bv[]
  (set-ld-redefinition-action '(:doit . :overwrite) state) .
  ~ev[]

  ~ilc[Encapsulate] causes somewhat odd interaction with the user if
  redefinition occurs within the encapsulation because the ~il[encapsulate]d
  event list is processed several times.  For example, if the redefinition
  action causes a query and a non-local definition is actually a redefinition,
  then the query will be posed twice, once during each pass.  C'est la vie.

  Finally, it should be stressed again that redefinition is dangerous because
  not all of the rules about a name are stored on the property list of the
  name.  Thus, redefinition can render ill-formed terms stored elsewhere in the
  data base or can preserve now-invalid rules.  ~l[redundant-events], in
  particular the section ``Note About Unfortunate Redundancies,'' for more
  discussion of potential pitfalls of redefinition."

  (f-get-global 'ld-redefinition-action state))

(deflabel redefining-programs
  :doc
  ":Doc-Section ACL2::Programming

  an explanation of why we restrict redefinitions~/

  ACL2 does not in general allow the redefinition of functions because
  logical inconsistency can result:  previously stored theorems can be
  rendered invalid if the axioms defining the functions involved are
  changed.  However, to permit prototyping of both ~c[:]~ilc[program] and
  ~c[:]~ilc[logic] mode systems, ACL2 permits redefinition if the user has
  accepted logical responsibility for the consequences by setting
  ~ilc[ld-redefinition-action] to an appropriate non-~c[nil] value.  The
  refusal of ACL2 to support the unrestricted redefinition of
  ~c[:]~ilc[program] mode functions may appear somewhat capricious.  After
  all, what are the logical consequences of changing a definition if
  no axioms are involved?~/

  Three important points should be made before we discuss redefinition
  further.

  The first is that ACL2 does support redefinition (of both
  ~c[:]~ilc[program] and ~c[:]~ilc[logic] functions) when
  ~ilc[ld-redefinition-action] is non-~c[nil].

  The second is that a ``redefinition'' that does not change the mode,
  formals, guards, type declarations, stobjs, or body of a function
  is considered redundant and is
  permitted even when ~ilc[ld-redefinition-action] is ~c[nil].  We
  recognize and permit redundant definitions because it is not
  uncommon for two distinct ~il[books] to share identical function
  definitions.  When determining whether the body of a function is
  changed by a proposed redefinition, we actually compare the
  untranslated versions of the two bodies.  ~l[term].  For
  example, redundancy is not recognized if the old body is ~c[(list a b)]
  and the new body is ~c[(cons a (cons b nil))].  We use the
  untranslated bodies because of the difficulty of translating the new
  body in the presence of the old syntactic information, given the
  possibility that the redefinition might attempt to change the
  ~il[signature] of the function, i.e., the number of formals, the
  number of results, or the position of single-threaded objects in either.

  The third important point is that a ``redefinition'' that preserves
  the formals, guard, types, stobjs, and body but changes the mode
  from ~c[:]~ilc[program] to
  ~c[:]~ilc[logic] is permitted even when ~ilc[ld-redefinition-action] is
  ~c[nil].  That is what ~ilc[verify-termination] does.

  This note addresses the temptation to allow redefinition of
  ~c[:]~ilc[program] functions in situations other than the three
  described above.  Therefore, suppose ~ilc[ld-redefinition-action] is
  ~c[nil] and consider the cases.

  Case 1.  Suppose the new definition attempts to change the formals
  or more generally the ~il[signature] of the function.  Accepting
  such a redefinition would render ill-formed other ~c[:]~ilc[program]
  functions which call the redefined function.  Subsequent attempts to
  evaluate those callers could arbitrarily damage the Common Lisp
  image.  Thus, redefinition of ~c[:]~ilc[program] functions under these
  circumstances requires the user's active approval, as would be
  sought with ~ilc[ld-redefinition-action] ~c['(:query . :overwrite)].

  Case 2.  Suppose the new definition attempts to change the body
  (even though it preserves the ~il[signature]).  At one time we
  believed this was acceptable and ACL2 supported the quiet
  redefinition of ~c[:]~ilc[program] mode functions in this circumstance.
  However, because such functions can be used in macros and redundancy
  checking is based on untranslated bodies, this turns out to be
  unsound!  (Aside: Perhaps this is not an issue if the function takes
  ~ilc[state] or a user-defined ~il[stobj] argument; but we do not further
  consider this distinction.)  Such redefinition is therefore now prohibited.
  We illustrate such an unsoundness below.  Let ~c[foo-thm1.lisp] be a book
  with the following contents.
  ~bv[]
  (in-package \"ACL2\")
  (defun p1 (x) (declare (xargs :mode :program)) (list 'if x 't 'nil))
  (defmacro p (x) (p1 x))
  (defun foo (x) (p x))
  (defthm foo-thm1 (iff (foo x) x) :rule-classes nil)
  ~ev[]
  Note that the macro form ~c[(p x)] translates to ~c[(if x t nil)].
  The ~c[:]~ilc[program] function ~c[p1] is used to generate this
  translation.  The function ~c[foo] is defined so that ~c[(foo x)] is
  ~c[(p x)] and a theorem about ~c[foo] is proved, namely, that ~c[(foo x)]
  is true iff ~c[x] is true.

  Now let ~c[foo-thm2.lisp] be a book with the following contents.
  ~bv[]
  (in-package \"ACL2\")
  (defun p1 (x) (declare (xargs :mode :program)) (list 'if x 'nil 't))
  (defmacro p (x) (p1 x))
  (defun foo (x) (p x))
  (defthm foo-thm2 (iff (foo x) (not x)) :rule-classes nil)
  ~ev[]
  In this book, the ~c[:]~ilc[program] function ~c[p1] is defined so that
  ~c[(p x)] means just the negation of what it meant in the first book,
  namely, ~c[(if x nil t)].  The function ~c[foo] is defined identically
  ~-[] more precisely, the ~i[untranslated] body of ~c[foo] is identical
  in the two ~il[books], but because of the difference between the two
  versions of the ~c[:]~ilc[program] function ~c[p1] the axioms
  defining the two ~c[foo]s are different.  In the second book we prove
  the theorem that ~c[(foo x)] is true iff ~c[x] is nil.

  Now consider what would happen if the ~il[signature]-preserving
  redefinition of ~c[:]~ilc[program] functions were permitted and these
  two ~il[books] were included.  When the second book is included the
  redefinition of ~c[p1] would be permitted since the ~il[signature] is
  preserved and ~c[p1] is just a ~c[:]~ilc[program].  But then when the
  redefinition of ~c[foo] is processed it would be considered redundant
  and thus be permitted.  The result would be a logic in which it was
  possible to prove that ~c[(foo x)] is equivalent to both ~c[x] and
  ~c[(not x)].  In particular, the following sequence leads to a proof
  of nil:
  ~bv[]
  (include-book \"foo-thm1\")
  (include-book \"foo-thm2\")
  (thm nil :hints ((\"Goal\" :use (foo-thm1 foo-thm2))))
  ~ev[]

  It might be possible to loosen the restrictions on the redefinition
  of ~c[:]~ilc[program] functions by allowing ~il[signature]-preserving
  redefinition of ~c[:]~ilc[program] functions not involved in macro
  definitions.  Alternatively, we could implement definition
  redundancy checking based on the translated bodies of functions
  (though that is quite problematic).  Barring those two changes, we
  believe it is necessary simply to impose the same restrictions on
  the redefinition of ~c[:]~ilc[program] mode functions as we do on
  ~c[:]~ilc[logic] mode functions.")

(defun chk-ld-redefinition-action (val ctx state)
  (cond ((or (null val)
             (and (consp val)
                  (member-eq (car val) '(:query :warn :doit :warn! :doit!))
                  (member-eq (cdr val) '(:erase :overwrite))))
         (value nil))
        (t (er soft ctx *ld-special-error* 'ld-redefinition-action val))))

(defun set-ld-redefinition-action (val state)
  (er-progn
   (chk-ld-redefinition-action val 'set-ld-redefinition-action state)
   (pprogn
    (f-put-global 'ld-redefinition-action val state)
    (value val))))

(defmacro redef nil

  ":Doc-Section Miscellaneous

  a common way to set ~il[ld-redefinition-action]~/
  ~bv[]
  Example and General Form:
  ACL2 !>:redef
  ~ev[]
  This command sets ~ilc[ld-redefinition-action] to ~c['(:query . :overwrite)].~/

  As explained elsewhere (~pl[ld-redefinition-action]), this
  allows redefinition of functions and other ~il[events] without undoing.
  A query will be made every time a redefinition is ~il[command]ed; the
  user must explicitly acknowledge that the redefinition is
  intentional.  It is possible to set ~ilc[ld-redefinition-action] so that
  the redefinition of non-system functions occurs quietly.
  ~l[ld-redefinition-action]."

 '(set-ld-redefinition-action '(:query . :overwrite) state))

(defmacro redef! nil

  ":Doc-Section Miscellaneous

  system hacker's redefinition ~il[command]~/
  ~bv[]
  Example and General Form:
  ACL2 !>:redef!
  ACL2 p!>
  ~ev[]
  This ~il[command] sets ~ilc[ld-redefinition-action] to ~c['(:warn! . :overwrite)]
  sets the default ~il[defun-mode] to ~c[:]~ilc[program], and invokes
  ~c[(set-state-ok t)].  It also introduces ~c[(defttag :redef!)], so that
  redefinition of system functions will be permitted; ~pl[defttag].~/

  This ~il[command] is intended for those who are modifying ACL2 source code
  definitions.  Thus, note that even system functions can be redefined with a
  mere warning.  Be careful!"

  `(state-global-let*
    ((inhibit-output-lst (list* 'summary 'event (@ inhibit-output-lst))))
    (er-progn
     (set-ld-redefinition-action '(:warn! . :overwrite) state)
     (defttag :redef!)
     (program)
     (set-state-ok t))))

(defun chk-current-package (val ctx state)
  (cond ((find-non-hidden-package-entry val (known-package-alist state))
         (value nil))
        (t (er soft ctx *ld-special-error* 'current-package val))))

(defun set-current-package (val state)

; This function is equivalent to in-package-fn except for the
; error message generated.

  (er-progn
   (chk-current-package val 'set-current-package state)
   (pprogn
    (f-put-global 'current-package val state)
    (value val))))

(defun standard-oi (state)

  ":Doc-Section ACL2::Programming

  the standard object input ``channel''~/

  ~c[Standard-oi] is an ~ilc[ld] special (~pl[ld]).  The accessor is
  ~c[(standard-oi state)] and the updater is ~c[(set-standard-oi val state)].
  ~c[Standard-oi] must be an open object input channel, a true list of objects,
  or a list of objects whose last ~ilc[cdr] is an open object input channel.
  It is from this source that ~ilc[ld] takes the input forms to process.  When
  ~ilc[ld] is called, if the value specified for ~c[standard-oi] is a string or
  a list of objects whose last ~ilc[cdr] is a string, then ~ilc[ld] treats the
  string as a file name and opens an object input channel from that file, where
  the connected book directory (~pl[cbd]) is used to resolve relative
  pathnames.  The channel opened by ~ilc[ld] is closed by ~ilc[ld] upon
  termination.~/

  ``Standard-oi'' stands for ``standard object input.'' The
  read-eval-print loop in ~ilc[ld] reads the objects in ~c[standard-oi] and
  treats them as forms to be evaluated.  The initial value of
  ~c[standard-oi] is the same as the value of ~ilc[*standard-oi*]
  (~pl[*standard-oi*])."

  (f-get-global 'standard-oi state))

(defun read-standard-oi (state)

; We let LD take a true-listp as the "input file" and so we here implement
; the generalized version of (read-object (standard-oi state) state).

  (let ((standard-oi (standard-oi state)))
    (cond ((consp standard-oi)
           (let ((state (f-put-global 'standard-oi (cdr standard-oi) state)))
             (mv nil (car standard-oi) state)))
          ((null standard-oi)
           (mv t nil state))
          (t (read-object standard-oi state)))))

(defun chk-standard-oi (val ctx state)
  (cond
   ((and (symbolp val)
         (open-input-channel-p val :object state))
    (value nil))
   ((true-listp val)
    (value nil))
   ((and (consp val)
         (symbolp (cdr (last val)))
         (open-input-channel-p (cdr (last val)) :object state))
    (value nil))
   (t (er soft ctx *ld-special-error* 'standard-oi val))))

(defun set-standard-oi (val state)
  (er-progn (chk-standard-oi val 'set-standard-oi state)
            (pprogn
             (f-put-global 'standard-oi val state)
             (value val))))

(defun standard-co (state)

  ":Doc-Section ACL2::Programming

  the character output channel to which ~ilc[ld] prints~/

  ~c[Standard-co] is an ~ilc[ld] special (~pl[ld]).  The accessor is
  ~c[(standard-co state)] and the updater is ~c[(set-standard-co val state)].
  ~c[Standard-co] must be an open character output channel.  It is to this
  channel that ~ilc[ld] prints the ~il[prompt], the form to be evaluated, and the
  results.  The event ~il[command]s such as ~ilc[defun], ~ilc[defthm], etc., which
  print extensive commentary do not print to ~c[standard-co] but rather to
  a different channel, ~ilc[proofs-co], so that you may redirect this
  commentary while still interacting via ~c[standard-co].
  ~l[proofs-co].~/

  ``Standard-co'' stands for ``standard character output.'' The
  initial value of ~c[standard-co] is the same as the value of
  ~ilc[*standard-co*] (~pl[*standard-co*])."

  (f-get-global 'standard-co state))

(defun chk-standard-co (val ctx state)
  (cond
   ((and (symbolp val)
         (open-output-channel-p val :character state))
    (value nil))
   (t (er soft ctx *ld-special-error* 'standard-co val))))

(defun set-standard-co (val state)
  (er-progn
   (chk-standard-co val 'set-standard-co state)
   (pprogn
    (f-put-global 'standard-co val state)
    (value val))))

(defun proofs-co (state)

  ":Doc-Section ACL2::Programming

  the proofs character output channel~/

  ~c[Proofs-co] is an ~ilc[ld] special (~pl[ld]).  The accessor is
  ~c[(proofs-co state)] and the updater is ~c[(set-proofs-co val state)].
  ~c[Proofs-co] must be an open character output channel.  It is to this
  channel that ~ilc[defun], ~ilc[defthm], and the other event ~il[command]s print their
  commentary.~/

  ``Proofs-co'' stands for ``proofs character output.'' The initial
  value of ~c[proofs-co] is the same as the value of ~ilc[*standard-co*]
  (~pl[*standard-co*])."

  (f-get-global 'proofs-co state))

(defun chk-proofs-co (val ctx state)
  (cond
   ((and (symbolp val)
         (open-output-channel-p val :character state))
    (value nil))
   (t (er soft ctx *ld-special-error* 'proofs-co val))))

(defun set-proofs-co (val state)
  (er-progn
   (chk-proofs-co val 'set-proofs-co state)
   (pprogn
    (f-put-global 'proofs-co val state)
    (value val))))

(deflabel prompt
  :doc
  ":Doc-Section Miscellaneous

  the prompt printed by ~ilc[ld]~/~/

  The prompt printed by ACL2 conveys information about various ``modes.''
  ~l[default-print-prompt] and ~pl[ld-prompt] for details.

  The prompt during raw Lisp breaks is, with most Common Lisp implementations,
  adjusted by ACL2 to include the string ~c[\"[RAW LISP]\"], in order to
  reminder users not to submit ACL2 forms there; ~pl[breaks].  For Lisps that
  seem to use the same code for printing prompts at the top-level as in
  ~il[breaks], the top-level prompt is similarly adjusted.  For Lisps with the
  above prompt adjustment, The following forms may be executed in raw Lisp
  (i.e., after typing ~c[:q]).
  ~bv[]
  (install-new-raw-prompt) ; install prompt with [RAW LISP] as described above
  (install-old-raw-prompt) ; revert to original prompt from host Common Lisp
  ~ev[]")

(defun ld-prompt (state)

  ":Doc-Section Miscellaneous

  determines the ~il[prompt] printed by ~ilc[ld]~/

  ~c[Ld-prompt] is an ~ilc[ld] special (~pl[ld]).  The accessor is
  ~c[(ld-prompt state)] and the updater is ~c[(set-ld-prompt val state)].
  ~c[Ld-prompt] must be either ~c[nil], ~c[t], or a function symbol that, when
  given an open output character channel and ~il[state], prints the desired
  ~il[prompt] to the channel and returns two values: the number of
  ~il[characters] printed and the ~il[state].  The initial value of ~c[ld-prompt] is
  ~c[t].~/

  The general-purpose ACL2 read-eval-print loop, ~ilc[ld], reads forms from
  ~ilc[standard-oi], evaluates them and prints the result to ~ilc[standard-co].
  However, there are various flags that control ~ilc[ld]'s behavior and
  ~c[ld-prompt] is one of them.  ~c[Ld-prompt] determines whether ~ilc[ld] prints a
  ~il[prompt] before reading the next form from ~ilc[standard-oi].  If ~c[ld-prompt]
  is ~c[nil], ~ilc[ld] prints no ~il[prompt].  If ~c[ld-prompt] is ~c[t], the default ~il[prompt]
  printer is used, which displays information that includes the
  current package, default ~il[defun-mode], ~il[guard] checking status (on or
  off), and ~ilc[ld-skip-proofsp]; ~pl[default-print-prompt].

  If ~c[ld-prompt] is neither ~c[nil] nor ~c[t], then it should be a function
  name, ~c[fn], such that ~c[(fn channel state)] will print the desired ~il[prompt]
  to ~c[channel] in ~ilc[state] and return ~c[(mv col state)], where ~c[col] is the
  number of ~il[characters] output (on the last line output).  You may
  define your own ~il[prompt] printing function.

  If you supply an inappropriate ~il[prompt] function, i.e., one that
  causes an error or does not return the correct number and type of
  results, the following odd ~il[prompt] will be printed instead:
  ~bv[]
  Bad Prompt
  See :DOC ld-prompt>
  ~ev[]
  which will lead you to this message.  You should either call ~ilc[ld]
  appropriately next time or assign an appropriate value to
  ~c[ld-prompt]."

  (f-get-global 'ld-prompt state))

(defun chk-ld-prompt (val ctx state)
  (cond ((or (null val)
             (eq val t)
             (let ((wrld (w state)))
               (and (symbolp val)
                    (equal (arity val wrld) 2)
                    (equal (stobjs-in val wrld) '(nil state))
                    (equal (stobjs-out val wrld) '(nil state)))))
         (value nil))
        (t (er soft ctx *ld-special-error* 'ld-prompt val))))

(defun set-ld-prompt (val state)
  (er-progn
   (chk-ld-prompt val 'set-ld-prompt state)
   (pprogn
    (f-put-global 'ld-prompt val state)
    (value val))))

(defun ld-keyword-aliases (state)

  ":Doc-Section Miscellaneous

  allows the abbreviation of some keyword commands~/
  ~bv[]
  Example:
  (set-ld-keyword-aliases '((:q 0 q-fn)
                            (:e 0 exit-acl2-macro))
                          state)
  ~ev[]
  ~c[Ld-keyword-aliases] is an ~ilc[ld] special (~pl[ld]).  The accessor
  is ~c[(ld-keyword-aliases state)] and the updater is
  ~c[(set-ld-keyword-aliases val state)].  ~c[Ld-keyword-aliases] must be an
  alist, each element of which is of the form ~c[(:keyword n fn)], where
  ~c[:keyword] is a keyword, ~c[n] is a nonnegative integer, and ~c[fn] is a
  function symbol of arity ~c[n], a macro symbol, or a ~c[lambda] expression
  of arity ~c[n].  When ~c[keyword] is typed as an ~ilc[ld] command, ~c[n] more forms
  are read, ~c[x1, ..., xn], and the form ~c[(fn 'x1 ... 'xn)] is then
  evaluated.  The initial value of ~c[ld-keyword-aliases] is ~c[nil].

  In the example above, ~c[:]~ilc[q] has been redefined to have the effect of
  executing ~c[(q-fn)], so for example if you define
  ~bv[]
  (defmacro q-fn ()
    '(er soft 'q \"You un-bound :q and now we have a soft error.\"))
  ~ev[]
  then ~c[:]~ilc[q] will cause an error, and if you define
  ~bv[]
  (defmacro exit-acl2-macro () '(exit-ld state))
  ~ev[]
  then ~c[:e] will cause the effect (it so happens) that ~c[:]~ilc[q] normally
  has.  If you prefer ~c[:e] to ~c[:]~ilc[q] for exiting the ACL2 loop, you might
  even want to put such definitions of ~c[q-fn] and ~c[exit-acl2-macro]
  together with the ~c[set-ld-keyword-aliases] form above in your
  ~c[\"acl2-customization.lisp\"] file; ~pl[acl2-customization].~/

  The general-purpose ACL2 read-eval-print loop, ~ilc[ld], reads forms from
  ~ilc[standard-oi], evaluates them and prints the result to ~ilc[standard-co].
  However, there are various flags that control ~ilc[ld]'s behavior and
  ~c[ld-keyword-aliases] is one of them.  ~c[Ld-keyword-aliases] affects how
  keyword commands are parsed.  Generally speaking, ~ilc[ld]'s command
  interpreter reads ``~c[:fn x1 ... xn]'' as ``~c[(fn 'x1 ... 'xn)]'' when ~c[:fn]
  is a keyword and ~c[fn] is the name of an ~c[n]-ary function.  But this
  parse is overridden, as described above, for the keywords bound in
  ~c[ld-keyword-aliases]."

  (f-get-global 'ld-keyword-aliases state))

(defun ld-keyword-aliasesp (x wrld)
  (cond ((atom x) (null x))
        ((and (true-listp (car x))
              (= (length (car x)) 3))
         (let ((key (car (car x)))
               (n (cadr (car x)))
               (fn (caddr (car x))))
           (and (keywordp key)
                (integerp n)
                (>= n 0)
                (cond
                 ((and (symbolp fn)
                       (function-symbolp fn wrld))
                  (equal (arity fn wrld) n))
                 ((and (symbolp fn)
                       (getprop fn 'macro-body nil
                                'current-acl2-world wrld))
                  t)
                 (t (and (true-listp fn)
                         (>= (length fn) 3)
                         (<= (length fn) 4)
                         (eq (car fn) 'lambda)
                         (arglistp (cadr fn))
                         (int= (length (cadr fn)) n))))
                (ld-keyword-aliasesp (cdr x) wrld))))
        (t nil)))

(defun chk-ld-keyword-aliases (val ctx state)
  (cond ((ld-keyword-aliasesp val (w state))
         (value nil))
        (t (er soft ctx *ld-special-error* 'ld-keyword-aliases val))))

(defun set-ld-keyword-aliases (val state)
  (er-progn
   (chk-ld-keyword-aliases val 'set-ld-keyword-aliases state)
   (pprogn
    (f-put-global 'ld-keyword-aliases val state)
    (value val))))

(defun ld-pre-eval-filter (state)

  ":Doc-Section Miscellaneous

  determines which forms ~ilc[ld] evaluates~/

  ~c[Ld-pre-eval-filter] is an ~ilc[ld] special (~pl[ld]).  The accessor is
  ~c[(ld-pre-eval-filter state)] and the updater is
  ~c[(set-ld-pre-eval-filter val state)].  ~c[Ld-pre-eval-filter] must be
  either ~c[:all], ~c[:query], or a new name that could be defined (e.g., by
  ~ilc[defun] or ~ilc[defconst]).  The initial value of ~c[ld-pre-eval-filter] is
  ~c[:all].~/

  The general-purpose ACL2 read-eval-print loop, ~ilc[ld], reads forms from
  ~ilc[standard-oi], evaluates them and prints the result to ~ilc[standard-co].
  However, there are various flags that control ~ilc[ld]'s behavior and
  ~c[ld-pre-eval-filter] is one of them.  If the filter is ~c[:all], then
  every form read is evaluated.  If the filter is ~c[:query], then after a
  form is read it is printed to ~ilc[standard-co] and the user is asked if
  the form is to be evaluated or skipped.  If the filter is a new
  name, then all forms are evaluated until that name is introduced, at
  which time ~ilc[ld] terminates normally.

  The ~c[:all] filter is, of course, the normal one.  ~c[:Query] is useful if
  you want to replay selected the ~il[command]s in some file.  The new name
  filter is used if you wish to replay all the ~il[command]s in a file up
  through the introduction of the given one."

  (f-get-global 'ld-pre-eval-filter state))

(defun new-namep (name wrld)

; We determine if name has properties on world wrld.  Once upon a time
; this was equivalent to just (not (assoc-eq name wrld)).  However, we
; have decided to ignore certain properties:
; * 'global-value - names with this property are just global variables
;                   in our code; we permit the user to define functions
;                   with those names.
; * 'table-alist - names with this property are being used as tables
; * 'table-guard - names with this property are being used as tables

; WARNING: If this list of properties is changed, change renew-name/erase.

; Additionally, if name has a non-nil 'redefined property name is treated as
; new if all of its other properties are as set by renew-name/erase or
; renew-name/overwrite, as appropriate.  The 'redefined property is set by
; renew-name to be (renewal-mode .  old-sig) where renewal-mode is :erase,
; :overwrite, or :reclassifying-overwrite.

  (let ((redefined (getprop name 'redefined nil 'current-acl2-world wrld)))
    (cond
     ((and (consp redefined)
           (eq (car redefined) :erase))

; If we erased the properties of name and they are still erased, then we
; will find no non-nil properties except for those left by
; renew-name/erase and renew-name.

      (not (has-propsp name
                       '(REDEFINED
                         GLOBAL-VALUE
                         TABLE-ALIST
                         TABLE-GUARD)
                       'current-acl2-world
                       wrld
                       nil)))
     ((and (consp redefined)
           (or (eq (car redefined) :overwrite)
               (eq (car redefined) :reclassifying-overwrite)))

; We make a check analogous to that for erasure, allowing arbitrary non-nil
; values on all the properties untouched by renew-name/overwrite and insisting
; that all the properties erased by that function are still gone.  Technically
; we should confirm that the lemmas property has been cleansed of all
; introductory rules, but in fact we allow it to have an arbitrary non-nil
; value.  This is correct because if 'formals is gone then we cleansed 'lemmas
; and nothing could have been put back there since name is not yet a function
; symbol again.

      (not (has-propsp name
                       '(REDEFINED

                         LEMMAS

                         GLOBAL-VALUE
                         LABEL
                         LINEAR-LEMMAS
                         FORWARD-CHAINING-RULES
                         ELIMINATE-DESTRUCTORS-RULE
                         COARSENINGS
                         CONGRUENCES
                         INDUCTION-RULES
                         THEOREM
                         UNTRANSLATED-THEOREM
                         CLASSES
                         CONST
                         THEORY
                         TABLE-GUARD
                         TABLE-ALIST
                         MACRO-BODY
                         MACRO-ARGS)
                       'current-acl2-world
                       wrld
                       nil)))
     (t (not (has-propsp name
                         '(GLOBAL-VALUE
                           TABLE-ALIST
                           TABLE-GUARD)
                         'current-acl2-world
                         wrld
                         nil))))))

(defun chk-ld-pre-eval-filter (val ctx state)
  (cond ((or (member-eq val '(:all :query))
             (and (symbolp val)
                  (not (keywordp val))
                  (not (equal (symbol-package-name val)
                              *main-lisp-package-name*))
                  (new-namep val (w state))))
         (value nil))
        (t (er soft ctx *ld-special-error* 'ld-pre-eval-filter val))))

(defun set-ld-pre-eval-filter (val state)
  (er-progn
   (chk-ld-pre-eval-filter val 'set-ld-pre-eval-filter state)
   (pprogn
    (f-put-global 'ld-pre-eval-filter val state)
    (value val))))

(defun ld-pre-eval-print (state)

  ":Doc-Section Miscellaneous

  determines whether ~ilc[ld] prints the forms to be ~c[eval]'d~/

  ~c[Ld-pre-eval-print] is an ~ilc[ld] special (~pl[ld]).  The accessor is
  ~c[(ld-pre-eval-print state)] and the updater is
  ~c[(set-ld-pre-eval-print val state)].  ~c[Ld-pre-eval-print] must be
  either ~c[t], ~c[nil], or ~c[:never].  The initial value of ~c[ld-pre-eval-print] is
  ~c[nil].~/

  The general-purpose ACL2 read-eval-print loop, ~ilc[ld], reads forms from
  ~ilc[standard-oi], evaluates them and prints the result to ~ilc[standard-co].
  However, there are various flags that control ~ilc[ld]'s behavior and
  ~c[ld-pre-eval-print] is one of them.  If this global variable is ~c[t],
  then before evaluating the form just read from ~ilc[standard-oi], ~ilc[ld]
  prints the form to ~ilc[standard-co].  If the variable is ~c[nil], no such
  printing occurs.  The ~c[t] option is useful if you are reading from a
  file of ~il[command]s and wish to assemble a complete script of the
  session in ~ilc[standard-co].

  The value ~c[:never] of ~c[ld-pre-eval-print] is rarely used.  During
  the evaluation of ~ilc[encapsulate] and of ~ilc[certify-book] forms,
  subsidiary events are normally printed, even if ~c[ld-pre-eval-print]
  is ~c[nil].  Thus for example, when the user submits an
  ~ilc[encapsulate] form, all subsidiary events are generally printed
  even in the default situation where ~c[ld-pre-eval-print] is ~c[nil].
  But occasionally one may want to suppress such printing.  In that
  case, ~c[ld-pre-eval-print] should be set to ~c[:never].  As
  described elsewhere (~pl[set-inhibit-output-lst]), another way to
  suppress such printing is to execute ~c[(set-inhibit-output-lst lst)]
  where ~c[lst] evaluates to a list including ~c['prove] and ~c['event]."

  (f-get-global 'ld-pre-eval-print state))

(defun chk-ld-pre-eval-print (val ctx state)
  (cond ((member-eq val '(nil t :never))
         (value nil))
        (t (er soft ctx *ld-special-error* 'ld-pre-eval-print val))))

(defun set-ld-pre-eval-print (val state)
  (er-progn
   (chk-ld-pre-eval-print val 'set-ld-pre-eval-print state)
   (pprogn
    (f-put-global 'ld-pre-eval-print val state)
    (value val))))

(defun ld-post-eval-print (state)

  ":Doc-Section Miscellaneous

  determines whether and how ~ilc[ld] prints the result of evaluation~/

  ~c[Ld-post-eval-print] is an ~ilc[ld] special (~pl[ld]).  The accessor is
  ~c[(ld-post-eval-print state)] and the updater is
  ~c[(set-ld-post-eval-print val state)].  ~c[Ld-post-eval-print] must be
  either ~c[t], ~c[nil], or ~c[:command-conventions].  The initial value of
  ~c[ld-post-eval-print] is ~c[:command-conventions].~/

  The general-purpose ACL2 read-eval-print loop, ~ilc[ld], reads forms from
  ~ilc[standard-oi], evaluates them and prints the result to ~ilc[standard-co].
  However, there are various flags that control ~ilc[ld]'s behavior and
  ~c[ld-post-eval-print] is one of them.  If this global variable is ~c[t], ~ilc[ld]
  prints the result.  In the case of a form that produces a multiple
  value, ~ilc[ld] prints the list containing all the values (which, logically
  speaking, is what the form returned).  If ~c[ld-post-eval-print] is ~c[nil],
  ~ilc[ld] does not print the values.  This is most useful when ~ilc[ld] is used
  to load a previously processed file.

  Finally, if ~c[ld-post-eval-print] is ~c[:command-conventions] then ~ilc[ld]
  prints the result but treats ``error triples'' specially.  An
  ``error triple'' is a result, ~c[(mv erp val state)], that consists of
  three values, the third of which is ~ilc[state].  Many ACL2 functions use
  such triples to signal errors.  The convention is that if ~c[erp] (the
  first value) is ~c[nil], then the function is returning ~c[val] (the second
  value) as its conventional single result and possibly side-effecting
  ~il[state] (as with some output).  If ~c[erp] is ~c[t], then an error has been
  caused, ~c[val] is irrelevant and the error message has been printed in
  the returned ~il[state].  Example ACL2 functions that follow this
  convention include ~ilc[defun] and ~ilc[in-package].  If such ``error
  producing'' functions are evaluated while ~c[ld-post-eval-print] is
  set to ~c[t], then you would see them producing lists of length 3.  This
  is disconcerting to users accustomed to Common Lisp (where these
  functions produce single results but sometimes cause errors or
  side-effect ~il[state]).

  When ~c[ld-post-eval-print] is ~c[:command-conventions] and a form produces
  an error triple ~c[(mv erp val state)] as its value, ~ilc[ld] prints nothing
  if ~c[erp] is non-~c[nil] and otherwise ~ilc[ld] prints just ~c[val].  Because it is a
  misrepresentation to suggest that just one result was returned, ~ilc[ld]
  prints the value of the global variable ~c['triple-print-prefix] before
  printing ~c[val].  ~c['triple-print-prefix] is initially ~c[\" \"], which means
  that when non-erroneous error triples are being abbreviated to ~c[val],
  ~c[val] appears one space off the left margin instead of on the margin.

  In addition, when ~c[ld-post-eval-print] is ~c[:command-conventions] and the
  value component of an error triple is the keyword ~c[:invisible] then ~ilc[ld]
  prints nothing.  This is the way certain commands (e.g., ~c[:]~ilc[pc]) appear
  to return no value.

  By printing nothing when an error has been signalled, ~ilc[ld] makes it
  appear that the error (whose message has already appeared in ~il[state])
  has ``thrown'' the computation back to load without returning a
  value.  By printing just ~c[val] otherwise, we suppress the fact that
  ~il[state] has possibly been changed."

  (f-get-global 'ld-post-eval-print state))

(defun chk-ld-post-eval-print (val ctx state)
  (cond ((member-eq val '(nil t :command-conventions))
         (value nil))
        (t (er soft ctx *ld-special-error* 'ld-post-eval-print val))))

(defun set-ld-post-eval-print (val state)
  (er-progn
   (chk-ld-post-eval-print val 'set-ld-post-eval-print state)
   (pprogn
    (f-put-global 'ld-post-eval-print val state)
    (value val))))

(defun ld-evisc-tuple (state)

  ":Doc-Section Miscellaneous

  determines whether ~ilc[ld] suppresses details when printing~/

  ~c[Ld-evisc-tuple] is an ~ilc[ld] special (~pl[ld]).  The accessor is
  ~c[(ld-evisc-tuple state)] and the updater is
  ~c[(set-ld-evisc-tuple val state)].  ~c[Ld-evisc-tuple] must be either
  ~c[nil] or a list of the form
  ~bv[]
  (alist print-level print-length hiding-cars)
  ~ev[]
  where ~c[alist] is an alist that pairs objects to strings, ~c[print-level]
  and ~c[print-length] are either ~c[nil] or non-negative integers, and
  ~c[hiding-cars] is a list of symbols.  The initial value of
  ~c[ld-evisc-tuple] is ~c[nil].~/

  The general-purpose ACL2 read-eval-print loop, ~ilc[ld], reads forms from
  ~ilc[standard-oi], evaluates them and prints the result to ~ilc[standard-co].
  However, there are various flags that control ~ilc[ld]'s behavior and
  ~c[ld-evisc-tuple] is one of them.  ~ilc[Ld] may print the forms it is
  evaluating and/or the results of evaluation.  If the value of
  ~c[ld-evisc-tuple] is a list as shown above, then ~ilc[ld] ``eviscerates'' the
  objects it prints before printing them.  To ``eviscerate'' an object
  we replace certain substructures within it by strings which are
  printed in their stead.  ~c[Print-level] and ~c[print-length], above, are
  used as described in CLTL (pp 372) to replace those substructures
  deeper than ~c[print-level] by ``~c[#]'' and those longer than ~c[print-length]
  by ``~c[...]''.  ~c[Alist] is used to replace any substructure occuring as a
  key in ~c[alist] by the corresponding string.  Finally, any ~ilc[consp] ~c[x]
  that starts with one of the symbols in ~c[hiding-cars] is printed as
  ~c[<hidden>].

  The printing of error messages and warnings, as well as certain other output,
  uses a different such evisc-tuple, ~c[(default-evisc-tuple state)].  The
  advanced user can override this evisc-tuple with the ~il[state] global
  ~c[user-default-evisc-tuple].  Similarly, some other printing uses yet
  another evisc-tuple, ~c[(term-evisc-tuple t state)], which can be overridden
  with ~il[state] global ~c[user-term-evisc-tuple].  We may document these
  mechanisms more fully if there is sufficient user interest."

  (f-get-global 'ld-evisc-tuple state))

(defun chk-ld-evisc-tuple (val ctx state)
  (cond ((or (null val)
             (standard-evisc-tuplep val))
         (value nil))
        (t (er soft ctx *ld-special-error* 'ld-evisc-tuple val))))

(defun set-ld-evisc-tuple (val state)
  (er-progn
   (chk-ld-evisc-tuple val 'set-ld-evisc-tuple state)
   (pprogn
    (f-put-global 'ld-evisc-tuple val state)
    (value val))))

(defun ld-error-triples (state)

  ":Doc-Section Miscellaneous

  determines whether a form caused an error during ~ilc[ld]~/

  ~c[Ld-error-triples] is an ~ilc[ld] special (~pl[ld]).  The accessor is
  ~c[(ld-error-triples state)] and the updater is
  ~c[(set-ld-error-triples val state)].  ~c[Ld-error-triples] must be
  either ~c[t] or ~c[nil].  The initial value of ~c[ld-error-triples] is
  ~c[t].~/

  The general-purpose ACL2 read-eval-print loop, ~ilc[ld], reads forms from
  ~ilc[standard-oi], evaluates them and prints the result to ~ilc[standard-co].
  However, there are various flags that control ~ilc[ld]'s behavior and
  ~c[ld-error-triples] is one of them.  If this variable has the value ~c[t]
  then when a form evaluates to 3 values, the first of which is
  non-~c[nil] and the third of which is ~ilc[state], an error is deemed to have
  occurred.  When an error occurs in evaluating a form, ~ilc[ld] rolls back
  the ACL2 ~il[world] to the configuration it had at the conclusion of the
  last error-free form.  Then ~ilc[ld] takes the action determined by
  ~ilc[ld-error-action]."

  (f-get-global 'ld-error-triples state))

(defun chk-ld-error-triples (val ctx state)
  (cond ((member-eq val '(nil t))
         (value nil))
        (t (er soft ctx *ld-special-error* 'ld-error-triples val))))

(defun set-ld-error-triples (val state)
  (er-progn
   (chk-ld-error-triples val 'set-ld-error-triples state)
   (pprogn
    (f-put-global 'ld-error-triples val state)
    (value val))))

(defun ld-error-action (state)

  ":Doc-Section Miscellaneous

  determines ~ilc[ld]'s response to an error~/

  ~c[Ld-error-action] is an ~ilc[ld] special (~pl[ld]).  The accessor is
  ~c[(ld-error-action state)] and the updater is
  ~c[(set-ld-error-action val state)].  ~c[Ld-error-action] must be
  ~c[:continue], ~c[:return], or ~c[:error].  The initial value of
  ~c[ld-error-action] is ~c[:continue], which means that the top-level
  ACL2 ~il[command] loop will not exit when an error is caused by
  ~c[user-typein].  But the default value for ~c[ld-error-action] on
  calls of ~ilc[ld] is ~c[:return].~/

  The general-purpose ACL2 read-eval-print loop, ~ilc[ld], reads forms from
  ~ilc[standard-oi], evaluates them and prints the result to ~ilc[standard-co].
  However, there are various flags that control ~ilc[ld]'s behavior and
  ~c[ld-error-action] is one of them.  If, while ~ilc[ld-error-triples] is ~c[t], a
  form evaluates to three results, the first of which is non-~c[nil] and
  the third of which is ~ilc[state], an error is said to have occurred.  If
  an error occurs, ~ilc[ld]'s action depends on ~c[ld-error-action].  If it is
  ~c[:continue], ~ilc[ld] just continues processing the forms in ~ilc[standard-oi].
  If it is ~c[:return], ~ilc[ld] stops and returns as though it had emptied the
  channel.  If it is ~c[:error], ~ilc[ld] stops and returns, signalling an error
  to its caller."

  (f-get-global 'ld-error-action state))

(defun chk-ld-error-action (val ctx state)
  (cond ((member-eq val '(:continue :return :error))
         (value nil))
        (t (er soft ctx *ld-special-error* 'ld-error-action val))))

(defun set-ld-error-action (val state)
  (er-progn
   (chk-ld-error-action val 'set-ld-error-action state)
   (pprogn
    (f-put-global 'ld-error-action val state)
    (value val))))

(defun ld-query-control-alist (state)

  ":Doc-Section Miscellaneous

  how to default answers to queries~/

  ~c[Ld-query-control-alist] is an ~ilc[ld] special (~pl[ld]).  The accessor
  is ~c[(ld-query-control-alist state)] and the updater is
  ~c[(set-ld-query-control-alist val state)].  Roughly speaking,
  ~c[ld-query-control-alist] is either ~c[nil] (meaning all queries should be
  interactive), ~c[t] (meaning all should default to the first accepted
  response), or an alist that pairs query ids to keyword responses.
  The alist may end in either ~c[t] or ~c[nil], indicating the default value
  for all ids not listed explicitly.  Formally, the
  ~c[ld-query-control-alist] must satisfy ~c[ld-query-control-alistp].  The
  initial ~c[ld-query-control-alist] is ~c[nil], which means all queries are
  handled interactively.~/

  When an ACL2 query is raised, a unique identifying symbol is printed
  in parentheses after the word ``Query''.  This symbol, called the
  ``query id,'' can be used in conjunction with ~c[ld-query-control-alist]
  to prevent the query from being handled interactively.  By ``handled
  interactively'' we mean that the query is printed to ~ilc[*standard-co*]
  and a response is read from ~ilc[*standard-oi*].  The alist can be used to
  obtain a ``default value'' for each query id.  If this value is ~c[nil],
  then the query is handled interactively.  In all other cases, the
  system handles the query without interaction (although text may be
  printed to ~ilc[standard-co] to make it appear that an interaction has
  occurred; see below).  If the default value is ~c[t], the system acts as
  though the user responded to the query by typing the first response
  listed among the acceptable responses.  If the default value is
  neither ~c[nil] nor ~c[t], then it must be a keyword and one of the
  acceptable responses.  In that case, the system acts as though the
  user responded with the given keyword.

  Next, we discuss how the ~c[ld-query-control-alist] assigns a default
  value to each query id.  It assigns each id the first value paired
  with the id in the alist, or, if no such pair appears in the alist,
  it assigns the final ~ilc[cdr] of the alist as the value.  Thus, ~c[nil]
  assigns all ids ~c[nil].  ~c[T] assigns all ids ~c[t].
  ~c['((:filter . nil) (:sysdef . :n) . t)] assigns ~c[nil] to the
  ~c[:filter] query, ~c[:n] to the ~c[:sysdef] query, and ~c[t] to all
  others.

  It remains only to discuss how the system prints text when the
  default value is not ~c[nil], i.e., when the query is handled without
  interaction.  In fact, it is allowed to pair a query id with a
  singleton list containing a keyword, rather than a keyword, and this
  indicates that no printing is to be done.  Thus for the example
  above, ~c[:sysdef] queries would be handled noninteractively, with
  printing done to simulate the interaction.  But if we change the
  example so that ~c[:sysdef] is paired with ~c[(:n)], i.e., if
  ~c[ld-query-control-alist] is ~c['((:filter . nil) (:sysdef :n) . t)], then
  no such printing would take place for ~c[sysdef] queries.  Instead, the
  default value of ~c[:n] would be assigned ``quietly''."

  (f-get-global 'ld-query-control-alist state))

(defun ld-query-control-alistp (val)
  (cond ((atom val) (or (eq val nil)
                        (eq val t)))
        ((and (consp (car val))
              (symbolp (caar val))
              (or (eq (cdar val) nil)
                  (eq (cdar val) t)
                  (keywordp (cdar val))
                  (and (consp (cdar val))
                       (keywordp (cadar val))
                       (null (cddar val)))))
         (ld-query-control-alistp (cdr val)))
        (t nil)))

(defun cdr-assoc-query-id (id alist)
  (cond ((atom alist) alist)
        ((eq id (caar alist)) (cdar alist))
        (t (cdr-assoc-query-id id (cdr alist)))))

(defun chk-ld-query-control-alist (val ctx state)
  (cond
   ((ld-query-control-alistp val)
    (value nil))
   (t (er soft ctx *ld-special-error* 'ld-query-control-alist val))))

(defun set-ld-query-control-alist (val state)
  (er-progn
   (chk-ld-query-control-alist val 'set-ld-query-control-alist state)
   (pprogn
    (f-put-global 'ld-query-control-alist val state)
    (value val))))

(defun ld-verbose (state)

  ":Doc-Section Miscellaneous

  determines whether ~ilc[ld] prints ``ACL2 Loading ...''~/

  ~c[Ld-verbose] is an ~ilc[ld] special (~pl[ld]).  The accessor is
  ~c[(ld-verbose state)] and the updater is ~c[(set-ld-verbose val state)].
  ~c[Ld-verbose] must be ~c[t], ~c[nil] or a string or ~ilc[consp] suitable for ~ilc[fmt]
  printing via the ~c[~~@] command.  The initial value of ~c[ld-verbose] is a
  ~ilc[fmt] message that prints the ACL2 version number, ~ilc[ld] level and
  connected book directory.~/

  Before processing the forms in ~ilc[standard-oi], ~ilc[ld] may print a header.
  The printing of this header is controlled by ~c[ld-verbose].  If
  ~c[ld-verbose] is ~c[nil], no header is printed.  If it is ~c[t], ~ilc[ld] prints the
  message
  ~bv[]
     ACL2 loading <file>
  ~ev[]
  where ~c[<file>] is the input channel supplied to ~ilc[ld].  A similar
  message is printed when ~ilc[ld] completes.  If ~c[ld-verbose] is neither ~c[t]
  nor ~c[nil] then it is presumably a header and is printed with the ~c[~~@]
  ~ilc[fmt] directive before ~ilc[ld] begins to read and process forms.  In this
  case the ~c[~~@] ~ilc[fmt] directive is interpreted in an environment in which
  ~c[#\\v] is the ACL2 version string, ~c[#\\l] is the level of the current
  recursion in ~ilc[ld] and/or ~ilc[wormhole], and ~c[#\\c] is the connected book
  directory ~c[(cbd)]."

  (f-get-global 'ld-verbose state))

(defun chk-ld-verbose (val ctx state)
  (cond ((or (stringp val)
             (and (consp val)
                  (stringp (car val))))
         (value nil))
        ((member-eq val '(nil t))
         (value nil))
        (t (er soft ctx *ld-special-error* 'ld-verbose val))))

(defun set-ld-verbose (val state)
  (er-progn
   (chk-ld-verbose val 'set-ld-verbose state)
   (pprogn
    (f-put-global 'ld-verbose val state)
    (value val))))

(defconst *nqthm-to-acl2-primitives*

; Keep this list in sync with documentation for nqthm-to-acl2.

  '((ADD1 1+)
    (ADD-TO-SET ADD-TO-SET-EQUAL ADD-TO-SET-EQ)
    (AND AND)
    (APPEND APPEND BINARY-APPEND)
    (APPLY-SUBR .   "Doesn't correspond to anything in ACL2, really.
                     See the documentation for DEFEVALUATOR and META.")
    (APPLY$ .       "See the documentation for DEFEVALUATOR and META.")
    (ASSOC ASSOC-EQUAL ASSOC ASSOC-EQ)
    (BODY .         "See the documentation for DEFEVALUATOR and META.")
    (CAR CAR)
    (CDR CDR)
    (CONS CONS)
    (COUNT ACL2-COUNT)
    (DIFFERENCE -)
    (EQUAL EQUAL EQ EQL =)
    (EVAL$ .        "See the documentation for DEFEVALUATOR and META.")
    (FALSE .        "Nqthm's F corresponds to the ACL2 symbol NIL.")
    (FALSEP NOT NULL)
    ;;(FIX)
    ;;(FIX-COST)
    ;;(FOR)
    (FORMALS .      "See the documentation for DEFEVALUATOR and META.")
    (GEQ >=)
    (GREATERP >)
    (IDENTITY IDENTITY)
    (IF IF)
    (IFF IFF)
    (IMPLIES IMPLIES)
    (LEQ <=)
    (LESSP <)
    (LISTP CONSP)
    (LITATOM SYMBOLP)
    (MAX MAX)
    (MEMBER MEMBER-EQUAL MEMBER MEMBER-EQ)
    (MINUS - UNARY--)
    (NEGATIVEP MINUSP)
    (NEGATIVE-GUTS ABS)
    (NLISTP ATOM)
    (NOT NOT)
    (NUMBERP ACL2-NUMBERP INTEGERP RATIONALP)
    (OR OR)
    (ORDINALP O-P)
    (ORD-LESSP O<)
    (PACK .         "See INTERN and COERCE.")
    (PAIRLIST PAIRLIS$)
    (PLUS + BINARY-+)
    ;;(QUANTIFIER-INITIAL-VALUE)
    ;;(QUANTIFIER-OPERATION)
    (QUOTIENT /)
    (REMAINDER REM MOD)
    (STRIP-CARS STRIP-CARS)
    (SUB1 1-)
    ;;(SUBRP)
    ;;(SUM-CDRS)
    (TIMES * BINARY-*)
    (TRUE . "The symbol T.")
    ;;(TRUEP)
    (UNION UNION-EQUAL UNION-EQ)
    (UNPACK .       "See SYMBOL-NAME and COERCE.")
    (V&C$ .         "See the documentation for DEFEVALUATOR and META.")
    (V&C-APPLY$ .   "See the documentation for DEFEVALUATOR and META.")
    (ZERO .         "The number 0.")
    (ZEROP ZEROP)))

(defconst *nqthm-to-acl2-commands*

; Keep this list in sync with documentation for nqthm-to-acl2.

  '((ACCUMULATED-PERSISTENCE ACCUMULATED-PERSISTENCE)
    (ADD-AXIOM DEFAXIOM)
    (ADD-SHELL .    "There is no shell principle in ACL2.")
    (AXIOM DEFAXIOM)
    (BACKQUOTE-SETTING .
                    "Backquote is supported in ACL2, but not
                     currently documented.")
    (BOOT-STRAP GROUND-ZERO)
    (BREAK-LEMMA MONITOR)
    (BREAK-REWRITE BREAK-REWRITE)
    (CH PBT .       "See also :DOC history.")
    (CHRONOLOGY PBT .
                    "See also :DOC history.")
    (COMMENT DEFLABEL)
    (COMPILE-UNCOMPILED-DEFNS COMP)
    (CONSTRAIN .    "See :DOC encapsulate and :DOC local.")
    (DATA-BASE .    "Perhaps the closest ACL2 analogue of DATA-BASE
                     is PROPS.  But see :DOC history for a collection
                     of commands for querying the ACL2 database
                     (``world'').  Note that the notions of
                     supporters and dependents are not supported in
                     ACL2.")
    (DCL DEFSTUB)
    (DEFN DEFUN DEFMACRO)
    (DEFTHEORY DEFTHEORY)
    (DISABLE DISABLE)
    (DISABLE-THEORY .
                    "See :DOC theories.  The Nqthm command
                     (DISABLE-THEORY FOO) corresponds roughly to the
                     ACL2 command
                     (in-theory (set-difference-theories
                                  (current-theory :here)
                                  (theory 'foo))).")
    (DO-EVENTS LD)
    (DO-FILE LD)
    (ELIM ELIM)
    (ENABLE ENABLE)
    (ENABLE-THEORY .
                    "See :DOC theories.  The Nqthm command
                     (ENABLE-THEORY FOO) corresponds roughly to the
                     ACL2 command
                     (in-theory (union-theories
                                  (theory 'foo)
                                  (current-theory :here))).")
    (EVENTS-SINCE PBT)
    (FUNCTIONALLY-INSTANTIATE .
                    "ACL2 provides a form of the :USE hint that
                     corresponds roughly to the
                     FUNCTIONALLY-INSTANTIATE event of Nqthm. See
                     :DOC lemma-instance.")
    (GENERALIZE GENERALIZE)
    (HINTS HINTS)
    (LEMMA DEFTHM)
    (MAINTAIN-REWRITE-PATH BRR)
    (MAKE-LIB .     "There is no direct analogue of Nqthm's notion of
                     ``library.''  See :DOC books for a description
                     of ACL2's mechanism for creating and saving
                     collections of events.")
    (META META)
    (NAMES NAME)
    (NOTE-LIB INCLUDE-BOOK)
    (PPE PE)
    (PROVE THM)
    (PROVEALL .     "See :DOC ld and :DOC certify-book.  The latter
                     corresponds to Nqthm's PROVE-FILE,which may be
                     what you're interested in, really.")
    (PROVE-FILE CERTIFY-BOOK)
    (PROVE-FILE-OUT CERTIFY-BOOK)
    (PROVE-LEMMA DEFTHM .
                    "See also :DOC hints.")
    (R-LOOP .       "The top-level ACL2 loop is an evaluation loop as
                     well, so no analogue of R-LOOP is necessary.")
    (REWRITE REWRITE)
    (RULE-CLASSES RULE-CLASSES)
    (SET-STATUS IN-THEORY)
    (SKIM-FILE LD-SKIP-PROOFSP)
    (TOGGLE IN-THEORY)
    (TOGGLE-DEFINED-FUNCTIONS EXECUTABLE-COUNTERPART-THEORY)
    (TRANSLATE TRANS TRANS1)
    (UBT UBT U)
    (UNBREAK-LEMMA UNMONITOR)
    (UNDO-BACK-THROUGH UBT)
    (UNDO-NAME .    "See :DOC ubt.  There is no way to undo names in
                     ACL2 without undoing back through such names.
                     However, see :DOC ld-skip-proofsp for
                     information about how to quickly recover the
                     state.")))

(defun nqthm-to-acl2-fn (name state)
  (declare (xargs :guard (symbolp name)))
  (io? temporary nil (mv erp val state)
       (name)
       (let ((prims (cdr (assoc-eq name *nqthm-to-acl2-primitives*)))
             (comms (cdr (assoc-eq name *nqthm-to-acl2-commands*))))
         (pprogn
          (cond
           (prims
            (let ((syms (fix-true-list prims))
                  (info (if (consp prims) (cdr (last prims)) prims)))
              (pprogn
               (if syms
                   (fms "Related ACL2 primitives (use :PE or see documentation ~
                         to learn more):  ~&0.~%"
                        (list (cons #\0 syms))
                        *standard-co*
                        state
                        nil)
                 state)
               (if info
                   (pprogn (fms info
                                (list (cons #\0 syms))
                                *standard-co*
                                state
                                nil)
                           (newline *standard-co* state))
                 state))))
           (t state))
          (cond
           (comms
            (let ((syms (fix-true-list comms))
                  (info (if (consp comms) (cdr (last comms)) comms)))
              (pprogn
               (if syms
                   (fms "Related ACL2 commands (use :PE or see documentation ~
                         to learn more):  ~&0.~%"
                        (list (cons #\0 syms))
                        *standard-co*
                        state
                        nil)
                 state)
               (if info
                   (pprogn (fms info
                                (list (cons #\0 syms))
                                *standard-co*
                                state
                                nil)
                           (newline *standard-co* state))
                 state))))
           (t state))
          (if (or prims comms)
              (value :invisible)
            (pprogn
             (fms "Sorry, but there seems to be no ACL2 notion corresponding ~
                   to the alleged Nqthm notion ~x0.~%"
                  (list (cons #\0 name))
                  *standard-co*
                  state
                  nil)
             (value :invisible)))))))

#|
; Here are functions that can be defined to print out the last part of the
; documentation string for nqthm-to-acl2, using (print-nqthm-to-acl2-doc
; state).

(defun print-nqthm-to-acl2-doc1 (alist state)
  (cond
   ((null alist) state)
   (t (let* ((x (fix-true-list (cdar alist)))
             (s (if (atom (cdar alist))
                    (cdar alist)
                  (cdr (last (cdar alist))))))
        (mv-let
         (col state)
         (fmt1 "  ~x0~t1--> "
               (list (cons #\0 (caar alist))
                     (cons #\1 16))
               0 *standard-co* state nil)
         (declare (ignore col))
         (mv-let
          (col state)
          (fmt1 " ~&0"
                (list (cons #\0 x))
                0 *standard-co* state nil)
          (declare (ignore col))
          (pprogn
           (if (or (null x) (null s))
               state
             (fms "~t0" (list (cons #\0 21)) *standard-co* state nil))
           (if s
               (mv-let
                (col state)
                (fmt1 "~@0~%" ; Here % was vertical bar, but emacs 19 has trouble...
                      (list (cons #\0 s)) 0 *standard-co* state nil)
                (declare (ignore col))
                state)
             (newline *standard-co* state))
           (print-nqthm-to-acl2-doc1 (cdr alist) state))))))))

(defun print-nqthm-to-acl2-doc (state)
  (pprogn
   (princ$ "  ~bv[]" *standard-co* state)
   (fms "  Nqthm functions  -->     ACL2"
        nil *standard-co* state nil)
   (fms "  ----------------------------------------~%"
        nil *standard-co* state nil)
   (print-nqthm-to-acl2-doc1 *nqthm-to-acl2-primitives* state)
   (fms "  ========================================~%"
        nil *standard-co* state nil)
   (fms "  Nqthm commands   -->     ACL2"
        nil *standard-co* state nil)
   (fms "  ----------------------------------------~%"
        nil *standard-co* state nil)
   (print-nqthm-to-acl2-doc1 *nqthm-to-acl2-commands* state)
   (princ$ "  ~ev[]" *standard-co* state)
   (newline *standard-co* state)
   (value :invisible)))
|#

(defmacro nqthm-to-acl2 (x)

; Keep documentation for this function in sync with *nqthm-to-acl2-primitives*
; and *nqthm-to-acl2-commands*.  See comment above for how some of this
; documentation was generated.

  (declare (xargs :guard (and (true-listp x)
                              (equal (length x) 2)
                              (eq (car x) 'quote)
                              (symbolp (cadr x)))))
  ":Doc-Section Documentation

  ACL2 analogues of Nqthm functions and commands~/
  ~bv[]
  Example Forms:
  :nqthm-to-acl2 prove-lemma   ; Display ACL2 topic(s) and/or print
                               ; information corresponding to Nqthm
                               ; PROVE-LEMMA command.
  (nqthm-to-acl2 'prove-lemma) ; Same as above.~/

  General Form:
  (nqthm-to-acl2 name)
  ~ev[]
  where ~c[name] is a notion documented for Nqthm:  either a function
  in the Nqthm logic, or a command.  If there is corresponding
  information available for ACL2, it will be printed in response to
  this command.  This information is not intended to be completely
  precise, but rather, is intended to help those familiar with Nqthm
  to make the transition to ACL2.

  We close with two tables that contain all the information used by
  this ~c[nqthm-to-acl2] command.  The first shows the correspondence
  between functions in the Nqthm logic and corresponding ACL2
  functions (when possible); the second is similar, but for commands
  rather than functions.

  ~bv[]
  Nqthm functions  -->     ACL2
  ----------------------------------------
  ADD1          -->  1+
  ADD-TO-SET    -->  ADD-TO-SET-EQUAL and ADD-TO-SET-EQ
  AND           -->  AND
  APPEND        -->  APPEND and BINARY-APPEND
  APPLY-SUBR    -->  No correspondent, but see the documentation for
                     DEFEVALUATOR and META.
  APPLY$        -->  No correspondent, but see the documentation for
                     DEFEVALUATOR and META.
  ASSOC         -->  ASSOC-EQUAL, ASSOC and ASSOC-EQ
  BODY          -->  No correspondent, but see the documentation for
                     DEFEVALUATOR and META.
  CAR           -->  CAR
  CDR           -->  CDR
  CONS          -->  CONS
  COUNT         -->  ACL2-COUNT
  DIFFERENCE    -->  -
  EQUAL         -->  EQUAL, EQ, EQL and =
  EVAL$         -->  No correspondent, but see the documentation for
                     DEFEVALUATOR and META.
  FALSE         -->  Nqthm's F corresponds to the ACL2 symbol NIL.
  FALSEP        -->  NOT and NULL
  FORMALS       -->  No correspondent, but see the documentation for
                     DEFEVALUATOR and META.
  GEQ           -->  >=
  GREATERP      -->  >
  IDENTITY      -->  IDENTITY
  IF            -->  IF
  IFF           -->  IFF
  IMPLIES       -->  IMPLIES
  LEQ           -->  <=
  LESSP         -->  <
  LISTP         -->  CONSP
  LITATOM       -->  SYMBOLP
  MAX           -->  MAX
  MEMBER        -->  MEMBER-EQUAL, MEMBER and MEMBER-EQ
  MINUS         -->  - and UNARY--
  NEGATIVEP     -->  MINUSP
  NEGATIVE-GUTS -->  ABS
  NLISTP        -->  ATOM
  NOT           -->  NOT
  NUMBERP       -->  ACL2-NUMBERP, INTEGERP and RATIONALP
  OR            -->  OR
  ORDINALP      -->  O-P
  ORD-LESSP     -->  O<
  PACK          -->  See intern and coerce.
  PAIRLIST      -->  PAIRLIS$
  PLUS          -->  + and BINARY-+
  QUOTIENT      -->  /
  REMAINDER     -->  REM and MOD
  STRIP-CARS    -->  STRIP-CARS
  SUB1          -->  1-
  TIMES         -->  * and BINARY-*
  TRUE          -->  The symbol T.
  UNION         -->  UNION-EQUAL and UNION-EQ
  UNPACK        -->  See symbol-name and coerce.
  V&C$          -->  No correspondent, but see the documentation for
                     DEFEVALUATOR and META.
  V&C-APPLY$    -->  No correspondent, but see the documentation for
                     DEFEVALUATOR and META.
  ZERO          -->  The number 0.
  ZEROP         -->  ZP

  ========================================

  Nqthm commands   -->     ACL2
  ----------------------------------------
  ACCUMULATED-PERSISTENCE
                -->  ACCUMULATED-PERSISTENCE
  ADD-AXIOM     -->  DEFAXIOM
  ADD-SHELL     -->  There is no shell principle in ACL2.
  AXIOM         -->  DEFAXIOM
  BACKQUOTE-SETTING
                -->  Backquote is supported in ACL2, but not
                     currently documented.
  BOOT-STRAP    -->  GROUND-ZERO
  BREAK-LEMMA   -->  MONITOR
  BREAK-REWRITE -->  BREAK-REWRITE
  CH            -->  PBT
                     See also :DOC history.
  CHRONOLOGY    -->  PBT
                     See also :DOC history.
  COMMENT       -->  DEFLABEL
  COMPILE-UNCOMPILED-DEFNS
                -->  COMP
  CONSTRAIN     -->  See :DOC encapsulate and :DOC local.
  DATA-BASE     -->  Perhaps the closest ACL2 analogue of DATA-BASE
                     is PROPS.  But see :DOC history for a collection
                     of commands for querying the ACL2 database
                     (``world'').  Note that the notions of
                     supporters and dependents are not supported in
                     ACL2.
  DCL           -->  DEFSTUB
  DEFN          -->  DEFUN and DEFMACRO
  DEFTHEORY     -->  DEFTHEORY
  DISABLE       -->  DISABLE
  DISABLE-THEORY
                -->  See :DOC theories.  The Nqthm command
                     (DISABLE-THEORY FOO) corresponds roughly to the
                     ACL2 command
                     (in-theory (set-difference-theories
                                  (current-theory :here)
                                  (theory 'foo))).
  DO-EVENTS     -->  LD
  DO-FILE       -->  LD
  ELIM          -->  ELIM
  ENABLE        -->  ENABLE
  ENABLE-THEORY -->  See :DOC theories.  The Nqthm command
                     (ENABLE-THEORY FOO) corresponds roughly to the
                     ACL2 command
                     (in-theory (union-theories
                                  (theory 'foo)
                                  (current-theory :here))).
  EVENTS-SINCE  -->  PBT
  FUNCTIONALLY-INSTANTIATE
                -->  ACL2 provides a form of the :USE hint that
                     corresponds roughly to the
                     FUNCTIONALLY-INSTANTIATE event of Nqthm. See
                     :DOC lemma-instance.
  GENERALIZE    -->  GENERALIZE
  HINTS         -->  HINTS
  LEMMA         -->  DEFTHM
  MAINTAIN-REWRITE-PATH
                -->  BRR
  MAKE-LIB      -->  There is no direct analogue of Nqthm's notion of
                     ``library.''  See :DOC books for a description
                     of ACL2's mechanism for creating and saving
                     collections of events.
  META          -->  META
  NAMES         -->  NAME
  NOTE-LIB      -->  INCLUDE-BOOK
  PPE           -->  PE
  PROVE         -->  THM
  PROVEALL      -->  See :DOC ld and :DOC certify-book.  The latter
                     corresponds to Nqthm's PROVE-FILE,which may be
                     what you're interested in,really.
  PROVE-FILE    -->  CERTIFY-BOOK
  PROVE-FILE-OUT
                -->  CERTIFY-BOOK
  PROVE-LEMMA   -->  DEFTHM
                     See also :DOC hints.
  R-LOOP        -->  The top-level ACL2 loop is an evaluation loop as
                     well, so no analogue of R-LOOP is necessary.
  REWRITE       -->  REWRITE
  RULE-CLASSES  -->  RULE-CLASSES
  SET-STATUS    -->  IN-THEORY
  SKIM-FILE     -->  LD-SKIP-PROOFSP
  TOGGLE        -->  IN-THEORY
  TOGGLE-DEFINED-FUNCTIONS
                -->  EXECUTABLE-COUNTERPART-THEORY
  TRANSLATE     -->  TRANS and TRANS1
  UBT           -->  UBT and U
  UNBREAK-LEMMA -->  UNMONITOR
  UNDO-BACK-THROUGH
                -->  UBT
  UNDO-NAME     -->  See :DOC ubt.  There is no way to undo names in
                     ACL2 without undoing back through such names.
                     However, see :DOC ld-skip-proofsp for
                     information about how to quickly recover the
                     state.
  ~ev[]
  "

  `(nqthm-to-acl2-fn ,x state))

#+(and gcl (not acl2-loop-only))
(progn
  (defvar *current-allocated-fixnum-lo* 0)
  (defvar *current-allocated-fixnum-hi* 0))

(defun allocate-fixnum-range (fixnum-lo fixnum-hi)

  ":Doc-Section Programming

  set aside fixnums in GCL~/

  ~c[(Allocate-fixnum-range fixnum-lo fixnum-hi)] causes Gnu Common Lisp
  (GCL) to create a persistent table for the integers between
  ~c[fixnum-lo] and ~c[fixnum-hi] (both bounds inclusive). This table is
  referenced first when any integer is boxed and the existing box in
  the table is used if the integer is in bounds.  This can speed up
  GCL considerably by avoiding wasteful fixnum boxing.  Here,
  ~c[fixnum-lo] and ~c[fixnum-hi] should be fixnums, more specifically of
  type ~c[(signed-byte 29)], with ~c[fixnum-lo <= fixnum-hi].~/

  When this function is executed in a Lisp implementation other than
  GCL, it has no side effect.  This function always returns ~c[nil]."

  (declare (xargs :guard (and (integerp fixnum-lo)
                              (integerp fixnum-hi)
                              (>= fixnum-hi fixnum-lo)))
           (type (signed-byte 29) fixnum-lo fixnum-hi))

; This function is simply NIL in the logic but allocates a range of fixnums
; (from fixnum-lo to fixnum-hi) in GCL as a side effect (a side effect which
; should only affect the speed with which ACL2 computes a value, but not the
; value itself up to EQUALity).  In GCL, there is a range of pre-allocated
; fixnums which are fixed to be -1024 to +1023.

  (let ((tmp (- fixnum-hi fixnum-lo)))
    (declare (ignore tmp))
    #+(and gcl (not acl2-loop-only))
    (cond ((or (> fixnum-hi *current-allocated-fixnum-hi*)
               (< fixnum-lo *current-allocated-fixnum-lo*))
           (fms "NOTE:  Allocating bigger fixnum table in GCL.~|"
                nil (standard-co *the-live-state*) *the-live-state*
                nil)
           (system::allocate-bigger-fixnum-range fixnum-lo (1+ fixnum-hi))
           (setq *current-allocated-fixnum-lo* fixnum-lo)
           (setq *current-allocated-fixnum-hi* fixnum-hi))
          (t
           (fms "No further fixnum allocation done:~|  Previous fixnum table ~
                 encompasses desired allocation.~|"
                nil (standard-co *the-live-state*) *the-live-state*
                nil)))
    #+(and (not gcl) (not acl2-loop-only))
    (fms "Fixnum allocation is only performed in GCL.~|"
         nil (standard-co *the-live-state*) *the-live-state*
         nil)
    nil))

; It has been found useful to allocate new space very gradually in Allegro CL
; 6.1 for at least one unusually large job on a version of RedHat Linux (over
; 600MB without this caused GC error; with this call, the corresponding image
; size was cut by very roughly one third and there was no GC error).  However,
; the problem seems to disappear in Allegro CL 6.2.  So we won't advertise
; (document) this utility.

#+allegro
(defmacro allegro-allocate-slowly (&key (free-bytes-new-other '1024)
                                        (free-bytes-new-pages '1024)
                                        (free-percent-new '3)
                                        (expansion-free-percent-old '3)
                                        (expansion-free-percent-new '3))
  `(allegro-allocate-slowly-fn ,free-bytes-new-other ,free-bytes-new-pages
                               ,free-percent-new ,expansion-free-percent-old
                               ,expansion-free-percent-new))

(defun allegro-allocate-slowly-fn (free-bytes-new-other
                                   free-bytes-new-pages
                                   free-percent-new
                                   expansion-free-percent-old
                                   expansion-free-percent-new)

  #-(and allegro (not acl2-loop-only))
  (declare (ignore free-bytes-new-other free-bytes-new-pages free-percent-new
                   expansion-free-percent-old expansion-free-percent-new))
  #+(and allegro (not acl2-loop-only))
  (progn
    (setf (sys:gsgc-parameter :free-bytes-new-other) free-bytes-new-other)
    (setf (sys:gsgc-parameter :free-bytes-new-pages) free-bytes-new-pages)
    (setf (sys:gsgc-parameter :free-percent-new) free-percent-new)
    (setf (sys:gsgc-parameter :expansion-free-percent-old)
          expansion-free-percent-old)
    (setf (sys:gsgc-parameter :expansion-free-percent-new)
          expansion-free-percent-new))
  nil)

; All code for the pstack feature occurs immediately below.  When a form is
; wrapped in (pstk form), form will be pushed onto *pstk-stack* during its
; evaluation.  The stack can be evaluated (during a break or after an
; interrupted proof) by evaluating the form (pstack), and it is
; initialized at the beginning of each new proof attempt (in prove-loop, since
; that is the prover's entry point under both prove and pc-prove).

#-acl2-loop-only
(progn
  (defparameter *pstk-stack* nil)
  (defvar *verbose-pstk* nil)

; The following are only of interest when *verbose-pstk* is true.

  (defparameter *pstk-level* 1)
  (defparameter *pstk-start-time-stack* nil))

(defmacro clear-pstk ()
  #+acl2-loop-only nil
  #-acl2-loop-only
  '(progn (setq *pstk-stack* nil)
          (setq *pstk-level* 1)
          (setq *pstk-start-time-stack* nil)))

(defconst *pstk-vars*
  '(pstk-var-0
    pstk-var-1
    pstk-var-2
    pstk-var-3
    pstk-var-4
    pstk-var-5
    pstk-var-6
    pstk-var-7
    pstk-var-8
    pstk-var-9))

(defun pstk-bindings-and-args (args vars)

; We return (mv bindings new-args fake-args).  Here new-args is a symbol-listp
; and of the same length as args, where each element of args is either a symbol
; or is the value of the corresponding element of new-args in bindings.
; Fake-args is the same as new-args except that (f-decrement-big-clock state)
; has been replaced by <state>.

  (cond
   ((endp args)
    (mv nil nil nil))
   ((endp vars)
    (mv (er hard 'pstk-bindings-and-args
            "The ACL2 sources need *pstk-vars* to be extended.")
        nil nil))
   (t
    (mv-let (bindings rest-args fake-args)
      (pstk-bindings-and-args (cdr args) (cdr vars))
      (cond
       ((member-equal (car args) '(state (f-decrement-big-clock state)))
        (mv bindings
            (cons (car args) rest-args)
            (cons ''<state> rest-args)))
       ((symbolp (car args))
        (mv bindings
            (cons (car args) rest-args)
            (cons (car args) fake-args)))
       (t
        (mv (cons (list (car vars) (car args)) bindings)
            (cons (car vars) rest-args)
            (cons (car vars) fake-args))))))))

(defmacro pstk (form)
  (declare (xargs :guard (consp form)))
  #+acl2-loop-only
  `(check-vars-not-free
    ,*pstk-vars*
    ,form)
  #-acl2-loop-only
  (mv-let (bindings args fake-args)
    (pstk-bindings-and-args (cdr form) *pstk-vars*)
    `(let ,bindings
       (setq *pstk-stack*
             (cons ,(list* 'list (kwote (car form)) fake-args)
                   *pstk-stack*))
       (when (and *verbose-pstk*
                  (or (eq *verbose-pstk* t)
                      (not (member-eq ',(car form) *verbose-pstk*))))
         (setq *pstk-start-time-stack*
               (cons (get-internal-run-time) *pstk-start-time-stack*))
         (format t "~V@TCP~D> ~S~%"
                 (* 2 *pstk-level*)
                 *pstk-level*
                 ',(car form))
         (setq *pstk-level* (1+ *pstk-level*)))
       (#+acl2-mv-as-values multiple-value-prog1
        #-acl2-mv-as-values prog1
        ,(cons (car form) args)

; Careful!  We must be careful not to smash any mv-ref value in the forms
; below, in case form returns a multiple value.  So, for example, we use format
; rather than fmt1.

        (when (and *verbose-pstk*
                   (or (eq *verbose-pstk* t)
                       (not (member-eq ',(car form) *verbose-pstk*))))
          (setq *pstk-level* (1- *pstk-level*))
          (format t "~V@TCP~D< ~S [~,2F seconds]~%"
                  (* 2 *pstk-level*)
                  *pstk-level*
                  ',(car form)
                  (/ (- (get-internal-run-time)
                        (pop *pstk-start-time-stack*))
                     (float internal-time-units-per-second))))
        (setq *pstk-stack* (cdr *pstk-stack*))))))

(defun pstack-fn (allp state)
  #+(or acl2-loop-only no-hack)
  (declare (ignore allp))
  #-(or acl2-loop-only no-hack)
  (fms
   "~p0~|"
   (list (cons #\0 (if allp *pstk-stack* (strip-cars *pstk-stack*))))
   (standard-co state)
   state
   (and allp
        (if (eq allp :all)
            (cons (world-evisceration-alist state nil)
                  '(nil nil nil))
          (default-evisc-tuple state))))
  #-(or acl2-loop-only no-hack)
  (if (assoc-eq 'preprocess-clause *pstk-stack*)
      (cw "NOTE:  You may find the hint :DO-NOT '(PREPROCESS) helpful.~|"))
  (value :invisible))

(defmacro pstack (&optional allp)

  ":Doc-Section Other

  seeing what is the prover up to~/
  ~bv[]
  General Forms:
  (pstack)      ; inspect break
  (pstack t)    ; inspect break, printing all calls in abbreviated form
  (pstack :all) ; as above, but only abbreviating the ACL2 world
  ~ev[]

  When the form ~c[(pstack)] is executed during a break from a proof, or at the
  end of a proof that the user has aborted, a ``process stack'' (or ``prover
  stack'') will be printed that gives some idea of what the theorem prover has
  been doing.  Moreover, by evaluating ~c[(verbose-pstack t)] before
  starting a proof (~pl[verbose-pstack]) one can get trace-like information
  about prover functions, including time summaries, printed to the screen
  during a proof.  This feature is currently quite raw and may be refined
  considerably as time goes on, based on user suggestions.  For example, the
  usual control of printing given by ~il[set-inhibit-output-lst] is irrelevant
  for printing the pstack.

  The use of ~c[(pstack t)] or ~c[(pstack :all)] should only be used
  by those who are comfortable looking at functions in the ACL2 source code.
  Otherwise, simply use ~c[(pstack)].~/

  Entries in the pstack include the following (listed here alphabetically,
  except for the first).

  ~c[preprocess-clause], ~c[simplify-clause], etc. (in general,~c[xxx-clause]):
  top-level processes in the prover ``waterfall''

  ~c[clausify]: splitting a goal into subgoals

  ~c[ev-fncall]: evaluating a function on explicit arguments

  ~c[ev-fncall-meta]:  evaluating a metafunction

  ~c[forward-chain]: building a context for the current goal using
  ~ilc[forward-chaining] rules

  ~c[induct]: finding an induction scheme

  ~c[pop-clause]:  getting the next goal to prove by induction

  ~c[process-assumptions]:  creating forcing rounds

  ~c[remove-built-in-clauses]: removing built-in clauses (~pl[built-in-clauses])

  ~c[process-equational-polys]:  deducing interesting equations

  ~c[remove-trivial-equivalences]:  removing trivial equalities (and
  equivalences) from the current goal

  ~c[rewrite-atm]:  rewriting a top-level term in the current goal

  ~c[setup-simplify-clause-pot-lst]:  building the linear arithmetic database
  for the current goal

  ~c[strip-branches],  ~c[subsumption-replacement-loop]:  subroutines of
  ~c[clausify]

  ~c[waterfall]: top-level proof control

  ~/"

  `(pstack-fn ,allp state))

(defun verbose-pstack (flg-or-list)

  ":Doc-Section Pstack

  seeing what is the prover up to (for advanced users)~/
  ~bv[]
  General Forms:
  (verbose-pstack t)   ; get trace-like information on prover during proofs
  (verbose-pstack '(fn1 fn2 ...))
                       ; as above, but omit calls of the indicated functions
  (verbose-pstack nil) ; turn off trace-like information on prover
  ~ev[]
  For example, (verbose-pstack '(ev-fncall)) will provide a trace of various
  prover functions during proofs, except for the function ~c[ev-fncall].

  By evaluating ~c[(verbose-pstack t)] one can get trace-like information
  during subsequent proofs about prover functions, including time summaries,
  printed to the screen during a proof.  To turn off this feature, evaluate
  ~c[(verbose-pstack nil)].  Also ~l[pstack].~/~/"

  (declare (xargs :guard (or (eq flg-or-list t)
                             (eq flg-or-list nil)
                             (symbol-listp flg-or-list))))
  #+acl2-loop-only
  flg-or-list
  #-acl2-loop-only
  (setq *verbose-pstk* flg-or-list))

; For those who started using the above feature in Version_2.7:

(defmacro checkpoints (&rest args)
  (declare (ignore args))
  '(er soft 'checkpoints
       "Checkpoints has been replaced by pstack (``process stack''or ``proof ~
        stack'')."))

(defun print-checkpoints (flg)
  (declare (ignore flg))
  (er hard 'print-checkpoints
      "Print-checkpoints has been replaced by verbose-pstack."))
