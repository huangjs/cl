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

; PC globals are those that can be changed from inside the proof-checker's
; interactive loop, and whose values we want saved.  Note that state-stack can
; also be changed outside the interactive loop (by use of :instruction), so we
; need to be careful.  We'll manage this by keeping state-stack as a PC global,
; updating pc-output upon entry to reflect the latest value of state-stack.

(defmacro pc-value (val)
  `(cdr (assoc-eq ',val
                  (f-get-global 'pc-output state))))

(defmacro pc-assign (key val)
  `(f-put-global
    'pc-output
    (put-assoc-eq ',key ,val
                  (f-get-global 'pc-output state))
    state))

(defun initialize-pc-acl2 (state)
  (er-progn
   (assign pc-output nil)
   (pprogn
    (pc-assign ss-alist nil)
    (pc-assign old-ss nil)
    (pc-assign state-stack nil)
    (pc-assign next-pc-enabled-array-suffix 0)
    (assign in-verify-flg nil))))

(defmacro state-stack ()
  '(pc-value state-stack))

(defmacro old-ss ()
  '(pc-value old-ss))

; The entries in ss-alist are of the form (name state-stack . old-ss).

(defmacro ss-alist ()
  '(pc-value ss-alist))

(defmacro define-global (var val)
  (let ((var-fn
         (intern-in-package-of-symbol
          (string-append (symbol-name var) "-FN")
          var)))
    `(progn (defun ,var-fn (state)
              (if (f-boundp-global ',var state)
                  (f-get-global ',var state)
                ,val))
            (defmacro ,var ()
              '(,var-fn state)))))

(define-global pc-prompt "->: ")
(define-global pc-prompt-depth-prefix "#")
(define-global print-macroexpansion-flg nil)
; Turn the following on for debugging macro commands.
(define-global print-prompt-and-instr-flg t)

; We will maintain an invariant that there are no unproved goals hanging around
; in the pc-state.  Moreover, for simplicity, we leave it up to each command to
; ensure that no newly-created goal has a conclusion with a non-NIL explicit
; value.  The function remove-proved-goal-from-pc-state will be applied to
; remove the current goal if it has been proved.

; The pc-ens component of the state is either an enabled structure or else is
; NIL, which indicates that we should use the global enabled structure.

(defrec pc-state
  (instruction
   (goals . abbreviations)
   local-tag-tree
   pc-ens
   .
   tag-tree)
  nil)

(defconst *pc-state-fields-for-primitives*
  '(instruction goals abbreviations tag-tree local-tag-tree pc-ens))

(defmacro instruction (&optional state-stack-supplied-p)
  `(access pc-state
           (car ,(if state-stack-supplied-p
                     'state-stack
                   '(state-stack)))
           :instruction))

(defmacro goals (&optional state-stack-supplied-p)
  `(access pc-state
           (car ,(if state-stack-supplied-p
                     'state-stack
                   '(state-stack)))
           :goals))

(defmacro abbreviations (&optional state-stack-supplied-p)
  `(access pc-state
           (car ,(if state-stack-supplied-p
                     'state-stack
                   '(state-stack)))
           :abbreviations))

(defmacro local-tag-tree (&optional state-stack-supplied-p)
  `(access pc-state
           (car ,(if state-stack-supplied-p
                     'state-stack
                   '(state-stack)))
           :local-tag-tree))

(defmacro pc-ens (&optional state-stack-supplied-p)
  `(access pc-state
           (car ,(if state-stack-supplied-p
                     'state-stack
                   '(state-stack)))
           :pc-ens))

(defmacro tag-tree (&optional state-stack-supplied-p)
  `(access pc-state
           (car ,(if state-stack-supplied-p
                     'state-stack
                   '(state-stack)))
           :tag-tree))

; A state-stack is a list of goal records.  The goal contains explicit hyps,
; and also (via current-addr) implicit if-term governors.  Depends-on is the
; first suffix available for subgoals of the current goal; so, (goal-name . n)
; has been used at some point for exactly those positive integers n for which n
; < depends-on.

(defrec goal
  (conc (hyps . current-addr)
        (goal-name . depends-on))
  nil)

(defconst *goal-fields*
  '(conc hyps current-addr goal-name depends-on))

(defmacro conc (&optional ss-supplied-p)
  `(access goal (car (goals ,ss-supplied-p)) :conc))

(defmacro hyps (&optional ss-supplied-p)
  `(access goal (car (goals ,ss-supplied-p)) :hyps))

(defmacro current-addr (&optional ss-supplied-p)
  `(access goal (car (goals ,ss-supplied-p)) :current-addr))

(defmacro goal-name (&optional ss-supplied-p)
  `(access goal (car (goals ,ss-supplied-p)) :goal-name))

(defmacro depends-on (&optional ss-supplied-p)
  `(access goal (car (goals ,ss-supplied-p)) :depends-on))

(defmacro make-official-pc-command (sym &optional pre-package)
  (if pre-package
      `(intern-in-package-of-symbol
        (symbol-name ,sym)
        (caar (table-alist 'pc-command-table (w state))))
    `(intern-in-package-of-symbol (symbol-name ,sym)
                                  'acl2-pc::any-symbol)))

(defun intern-in-keyword-package (sym)
  (declare (xargs :guard (symbolp sym)))
  (let ((ans
; See comment in intern-in-package-of-symbol for an explanation of this trick.
         (intern (symbol-name sym) "KEYWORD")))
    ans))

(defun make-pretty-pc-command (x)
  (declare (xargs :guard (symbolp x)))
  ;; Returns the user-and-stored version of the command x.
  (intern-in-keyword-package x))

(defun make-pretty-pc-instr (instr)
  (declare (xargs :guard (or (symbolp instr)
                             (and (consp instr)
                                  (symbolp (car instr))))))
  (if (atom instr)
      (make-pretty-pc-command instr)
    (if (null (cdr instr))
        (make-pretty-pc-command (car instr))
      (cons (make-pretty-pc-command (car instr))
            (cdr instr)))))

(defmacro change-pc-state (pc-s &rest args)
  (list* 'change 'pc-state pc-s args))

(defun make-official-pc-instr (instr state)

; This function always returns a syntactically legal instruction, i.e., a true
; list whose car is a symbol in the ACL2-PC package

  (if (consp instr)
      (if (and (symbolp (car instr))
               (true-listp (cdr instr)))
          (cons (make-official-pc-command (car instr) t) (cdr instr))
        (list (make-official-pc-command 'illegal t) instr))
    (if (symbolp instr)
        (list (make-official-pc-command instr t))
      (if (and (integerp instr)
               (> instr 0))
          (list (make-official-pc-command 'dv t) instr)
        (list (make-official-pc-command 'illegal t) instr)))))

(defun check-formals-length (formals args fn ctx state)
  (declare (xargs :guard (and (symbol-listp formals)
                              (true-listp args))))
  (let ((max-length (if (member-eq '&rest formals)
                        'infinity
                      (length (remove '&optional formals))))
        (min-length (let ((k (max (length (member-eq '&rest formals))
                                  (length (member-eq '&optional formals)))))
                      (- (length formals) k)))
        (n (length args)))
    (if (and (<= min-length n)
             (or (eq max-length 'infinity)
                 (<= n max-length)))
        (value t)
      (if (equal min-length max-length)
          (er soft ctx
              "Wrong number of arguments in argument list ~x0 to ~x1.  There should ~
               be ~n2 argument~#3~[s~/~/s~] to ~x1."
              args fn min-length (zero-one-or-more min-length))
        (if (equal max-length 'infinity)
            (er soft ctx
                "Wrong number of arguments in argument list ~x0 to ~x1.  There should ~
                 be at least ~n2 argument~#3~[s~/~/s~] to ~x1."
                args fn min-length (min min-length 2))
          (er soft ctx
              "Wrong number of arguments in argument list ~x0 to ~x1.  There should ~
               be between ~n2 and ~n3 arguments to ~x1."
               args fn min-length max-length))))))

(defun check-&optional-and-&rest (formals state)
  (cond
   ((not (true-listp formals))
    (er soft 'check-&optional-and-&rest
        "The formals are supposed to be a true list, but they are ~x0."
        formals))
   ;; &optional can only occur at most once
   ((member-eq '&optional (cdr (member-eq '&optional formals)))
    (er soft 'check-&optional-and-&rest
        "The &optional keywords occurs more than once in ~x0."
        formals))
   ;; &rest can only occur next to the end
   (t (let ((r-formals (reverse formals)))
        (if (or (eq (car r-formals) '&optional)
                (eq (car r-formals) '&rest))
            (er soft 'check-&optional-and-&rest
                "The &optional and &rest keywords may not occur as the last element of ~
                the formals list, ~x0."
                formals)
          (if (member-eq '&rest (cddr r-formals))
              (er soft 'check-&optional-and-&rest
                  "The &rest keyword may not occur except as the next-to-last ~
                   member of the formals list, which is not the case for ~x0."
                  formals)
            (value t)))))))

(defun make-let-pairs-from-formals (formals arg)
  ;; e.g. (make-let-pairs-from-formals '(a b c) 'x) = 
  ;; ((a (car x)) (b (car (cdr x))) (c (car (cdr (cdr x)))))
  (if (consp formals)
      (if (eq (car formals) '&optional)
          (make-let-pairs-from-formals (cdr formals) arg)
        (if (eq (car formals) '&rest)
            (list (list (cadr formals) arg))
          (cons (list (car formals) (list 'car arg))
                (make-let-pairs-from-formals (cdr formals) (list 'cdr arg)))))
    nil))

;; The following are like all-vars, but heuristic in that they deal with untranslated forms.

(mutual-recursion

(defun all-symbols (form)
  (cond
   ((symbolp form)
    (list form))
   ((atom form)
    nil)
   ((eq (car form) (quote quote))
    nil)
   (t
    ;; used to have just (all-symbols-list (cdr form)) below, but
    ;; then (cond (current-addr ...) ...) messed up
    (union-eq (all-symbols (car form))
              (all-symbols-list (cdr form))))))

(defun all-symbols-list (x)
  (if (consp x)
      (union-eq (all-symbols (car x))
                (all-symbols-list (cdr x)))
    nil))

)

(defun make-access-bindings (record-name record fields)
  (if (consp fields)
      (cons `(,(car fields) (access ,record-name ,record ,(intern-in-keyword-package (car fields))))
            (make-access-bindings record-name record (cdr fields)))
    nil))

(defun let-form-for-pc-state-vars (form)
  (let ((vars (all-symbols form)))
    (let* ((goal-vars
            (intersection-eq *goal-fields* vars))
           (pc-state-vars
            (if goal-vars
                (intersection-eq *pc-state-fields-for-primitives* (cons 'goals vars))
              (intersection-eq *pc-state-fields-for-primitives* vars))))
      `(let ,(make-access-bindings 'pc-state 'pc-state pc-state-vars)
         (let ,(make-access-bindings 'goal '(car goals) goal-vars)
           ,form)))))

(defun check-field-names (formals ctx state)
  (let ((bad-formals (intersection-eq formals
                                      (append *goal-fields* *pc-state-fields-for-primitives*))))
    (if bad-formals
        (er soft ctx
            "It is illegal to use names of pc-state or goal fields as formals to~
             define commands with ~x0, in this case ~&1."
            ctx bad-formals)
      (value t))))

(defmacro print-no-change (&optional str alist (col '0))
  `(print-no-change-fn ,str ,alist ,col state))

(defmacro print-no-change2 (&rest args)
  `(pprogn ,(cons 'print-no-change args)
           (mv nil state)))

(defun print-no-change-fn (str alist col state)
  (declare (xargs :guard (or (stringp str)
                             (null str))))
  (io? proof-checker nil state
       (col alist str)
       (mv-let (col state)
         (let ((channel (proofs-co state)))
           (mv-let (col state)
             (fmt1 "~|*** NO CHANGE ***" nil col channel state nil)
             (if str
                 (mv-let (col state)
                   (fmt1 " -- " nil col channel state nil)
                   (mv-let (col state)
                     (fmt1 str alist col channel state nil)
                     (fmt1 "~|" nil col channel state nil)))
               (fmt1 "~|" nil col channel state nil))))
         (declare (ignore col))
         state)))

(defmacro maybe-update-instruction (instr pc-state-and-state)
  `(mv-let (pc-state state)
           ,pc-state-and-state
           (mv (and pc-state ; in case the instruction failed!
                    (if (access pc-state pc-state :instruction)
                        pc-state
                      (change-pc-state pc-state :instruction (make-pretty-pc-instr ,instr))))
               state)))

(defun add-pc-doc-header (command-type str)
  (declare (xargs :guard (and (stringp command-type)
                              (stringp str))))
  (string-append
   ":Doc-Section ACL2::Proof-checker-commands

"
   (string-append (string-append command-type "
")
                  str)))

(defun remove-doc (command-type body)
  ;; puts in doc if there isn't any, and puts the appropriate header on
  (declare (xargs :guard (stringp command-type)))
  (if (and (consp body) (consp (cdr body)) (stringp (car body)))
      (mv (add-pc-doc-header command-type (car body)) (cdr body))
    (mv nil body)))

(defun pc-primitive-defun-form (raw-name name formals doc body)
  `(defun ,name (args state)
     ;; notice that args aren't ignored, since even if they're nil, they're
     ;; used for arity checking
     ,@(and doc (list doc))
     (mv-let
      ;; can't use er-progn because we return (mv nil state) for errors
      (erp v state)
      (check-formals-length ',formals args ',raw-name ',name state)
      (declare (ignore v))
      (if erp
          (mv nil state)
        (let ((pc-state
               (change pc-state
                       (car (state-stack))
                       :instruction nil))
              ,@(make-let-pairs-from-formals formals 'args))
          ;; in case we have (declare (ignore pc-state))
          ,@(butlast body 1)
          (maybe-update-instruction
           (cons ',raw-name args)
           ,(let-form-for-pc-state-vars (car (last body)))))))))

(defmacro add-pc-command (name command-type)
  `(table pc-command-table ',name ,command-type))

(defun table-get (name key wrld)
  (assoc-equal key
               (getprop name 'table-alist nil
                        'current-acl2-world wrld)))

(defmacro pc-command-type (name)
  `(cdr (table-get 'pc-command-table ,name (w state))))

(defmacro print-no-change3 (&optional str alist (col '0))
  `(pprogn (print-no-change-fn ,str ,alist ,col state)
           (value nil)))

(defun add-pc-command-1 (name command-type state)
  (table-fn
   'pc-command-table
   `(',name ',command-type)
   state
   (list 'table 'pc-command-table (list 'quote name) (list 'quote command-type))))

(defun toggle-pc-macro-fn (name new-tp state)
  (let ((tp (pc-command-type name)))
    (if (null tp)
        (print-no-change3 "The command ~x0 is not a proof-checker command."
                          (list (cons #\0 name)))
      (case tp
            (macro (if (or (null new-tp) (equal (symbol-name new-tp) "ATOMIC-MACRO"))
                       (add-pc-command-1 name 'atomic-macro state)
                     (if (equal (symbol-name new-tp) "MACRO")
                         (print-no-change3 "~x0 is already a non-atomic macro."
                                           (list (cons #\0 name)))
                       (print-no-change3 "You can't change a proof-checker macro ~
                                          to have type ~x0."
                                         (list (cons #\0 new-tp))))))
            (atomic-macro (if (or (null new-tp) (equal (symbol-name new-tp) "MACRO"))
                              (add-pc-command-1 name 'macro state)
                            (if (equal (symbol-name new-tp) "ATOMIC-MACRO")
                                (print-no-change3 "~x0 is already an atomic macro."
                                                  (list (cons #\0 name)))
                              (print-no-change3 "You can't change a proof-checker atomic macro ~
                                                 to have type ~x0."
                                                (list (cons #\0 new-tp))))))
            (otherwise (print-no-change3 "You can't change the type of a proof-checker ~x0 command."
                                         (list (cons #\0 tp))))))))

(defun pc-meta-or-macro-defun (raw-name name formals doc body)
  `(defun ,name (args state)
     ;; notice that args aren't ignored, since even if they're nil, they're
     ;; used for arity checking
     (declare (xargs :mode :program :stobjs state))
     ,@(and doc (list doc))
     (er-progn
      (check-formals-length ',formals args ',raw-name ',name state)
      (let ((state-stack (state-stack))
            ,@(make-let-pairs-from-formals formals 'args))
        ;; in case we have a doc-string and/or declare forms
        ,@(butlast body 1)
        (let ((very-silly-copy-of-state-stack state-stack))

; This silly trick ensures that we don't have to declare state-stack ignored.

          (declare (ignore very-silly-copy-of-state-stack))
          ,(car (last body)))))))

(defun my-trans-eval (form ctx state)

; This is like trans-eval except we flatten out the (stobjs-out
; . replaced-val) result and return 4 values.

  (mv-let (erp val state)
          (trans-eval form ctx state)
          (mv erp                       ;erp
              (car val)                 ;stobjs-out
              (cdr val)                 ;replaced-val
              state)))                  ;state

(defun goal-names (goals)
  (if (consp goals)
      (cons (access goal (car goals) :goal-name)
            (goal-names (cdr goals)))
    nil))

(defun instructions-of-state-stack (ss acc)
  (if (consp ss)
      (instructions-of-state-stack
       (cdr ss)
       (cons (access pc-state (car ss) :instruction)
             acc))
    ;; at the end we cdr the accumulator to get rid of the `start' instruction
    (cdr acc)))

(defmacro fms0 (str &optional alist col evisc-tuple)
  ;; This should only be called when the cursor is on the left margin, or when
  ;; a fresh line or new line indicator starts the string, unless col is
  ;; supplied.
  `(mv-let (new-col state)
           (fmt1 ,str ,alist
                 ,(or col
                      0)
                 (proofs-co state) state ,evisc-tuple)
           (declare (ignore new-col))
           state))

(defmacro with-output-forced (output-chan signature code)

; Use this to force output to output-chan after executing the give code.  See
; print-pc-prompt and print-prompt for examples that make the usage pretty
; obvious.

  (cond ((or (not (true-listp signature))
             (member-eq output-chan signature))
         (er hard 'with-output-forced
             "Ill-formed call: ~x0"
             `(with-output-forced ,output-chan ,signature ,code)))

        (t
         #+acl2-loop-only
         code
         #-acl2-loop-only
         `(mv-let ,signature
            ,code
            #-acl2-loop-only
            (progn (force-output (get-output-stream-from-channel ,output-chan))
                   (mv ,@signature))
            #+acl2-loop-only
            (mv ,@signature)))))

(defun print-pc-prompt (state)
  ;; Does NOT print a new line before or after, but assumes that we're in column 0.
  (let ((chan (proofs-co state)))
    (with-output-forced
     chan
     (col state)
     (io? proof-checker nil (mv col state)
          (chan)
          (fmt1 (pc-prompt) nil 0 chan state nil)
          :default-bindings ((col 0))))))

(defun pc-macroexpand (raw-instr state)
  ;; We assume that instr has already been "parsed", so that it's a
  ;; list whose car is in the ACL2-PC package.  This function
  ;; repeatedly expands instr until we have an answer.  At one time I
  ;; intended not to allow state to be returned by macroexpansion, but
  ;; now I want to take a more general view that all kinds of what I
  ;; used to call "help" commands are implemented by macro commands
  ;; (that probably return a meta command that says "skip" after doing
  ;; some printing).  Notice that unlike Lisp macros, the global Lisp
  ;; state is available for the expansion.  Hence we can query the
  ;; ACL2 database etc.
  ;;    Macro commands will always return an error triple.
  (let ((instr (make-official-pc-instr raw-instr state)))
    ;; Notice that instr is syntactically valid, i.e. is a true-listp headed
    ;; by a symbol in the acl2-pc package -- even if INSTR isn't of this form.
    (if (member-eq (pc-command-type (car instr)) '(macro atomic-macro))
        (mv-let (erp stobjs-out vals state)
                (my-trans-eval (list (car instr)
                                     (list 'quote (cdr instr))
                                     'state)
                               'pc-macroexpand
                               state)
                (cond
                 (erp
                  ;; this case shouldn't really happen
                  (pprogn (io? proof-checker nil state
                               (instr)
                               (fms0
                                "~|Macroexpansion of instruction ~x0 failed at ~
                                 an odd place!  Is it really defined?~%"
                                (list (cons #\0 instr))))
                          (mv erp nil state)))
                 ((equal stobjs-out '(nil nil state))
                  (if (car vals)
                      ;; presumably the failed macro already explained the
                      ;; "error" in this case
                      (mv (car vals) nil state)
                    (pc-macroexpand (cadr vals) state)))
                 (t (pprogn
                     (io? proof-checker nil state
                          (instr)
                          (fms0
                           "~|Macroexpansion of instruction ~x0 ~
                            failed unusually, since this macro ~
                            command does not have the output ~
                            signature of an ACL2 error triple.  Is it ~
                            really defined as a macro command?~%"
                           (list (cons #\0 instr))))
                     (mv t nil state)))))
      ;; so, now we have an instruction that SHOULD be primitive or meta
      (value instr))))

(defun find-goal (name goals)
  (if (consp goals)
      (if (equal name (access goal (car goals) :goal-name))
          (car goals)
        (find-goal name (cdr goals)))
    nil))

(defun print-all-goals-proved-message (state)
  (io? proof-checker nil state
       nil
       (pprogn
        (print-no-change "There are no unproved goals!")
        (if (f-get-global 'in-verify-flg state)
            (fms0 "You may wish to exit.~%")
          state))))

(defmacro when-goals (form)
  `(if (goals t)
       ,form
     (print-all-goals-proved-message state)))

(defmacro when-goals-trip (form)
  `(if (goals t)
       ,form
     (pprogn (print-all-goals-proved-message state)
             (value 'skip))))

(defun current-immediate-deps (goal-name goal-names)
  ;; Returns all names in goal-names that are immediate dependents of goal-name.
  (if (consp goal-names)
      (if (and (consp (car goal-names))
               (equal goal-name (caar goal-names)))
          (cons (car goal-names)
                (current-immediate-deps goal-name (cdr goal-names)))
        (current-immediate-deps goal-name (cdr goal-names)))
    nil))

(defun goal-dependent-p (parent name)
  ;; says whether parent is a proper ancestor of name
  (if (consp name)
      (if (equal parent (car name))
          t
        (goal-dependent-p parent (car name)))
    nil))

(defun current-all-deps (goal-name goal-names)
  ;; Returns all names in goal-names that are proper dependents (not necessarily
  ;; immediate) of goal-name.
  (if (consp goal-names)
      (if (goal-dependent-p goal-name (car goal-names))
          (cons (car goal-names)
                (current-immediate-deps goal-name (cdr goal-names)))
        (current-immediate-deps goal-name (cdr goal-names)))
    nil))

(defun maybe-print-proved-goal-message (goal old-goals goals state)

; Here goal is a goal in the existing pc-state and goals is the goals in the
; new pc-state.  old-goals is the goals in the existing pc-state.

; Warning: This function should be called under (io? proof-checker ...).

  (let* ((name (access goal goal :goal-name))
         (new-names (goal-names goals))
         (names (set-difference-equal new-names (goal-names old-goals))))
    (pprogn (if names
                (fms0 "~|~%Creating ~n0 new ~#1~[~/goal~/goals~]:  ~&2.~%"
                      (list (cons #\0 (length names))
                            (cons #\1 (zero-one-or-more (length names)))
                            (cons #\2 names)))
              state)
            (if (find-goal name goals)
                state
              (let ((unproved-deps (current-all-deps name new-names)))
                (if unproved-deps
                    (fms0 "~|~%The proof of the current goal, ~x0, has ~
                                been completed.  However, the following ~
                                subgoals remain to be proved:~%~ ~ ~&1.~%Now ~
                                proving ~x2.~%"
                          (list (cons #\0 name)
                                (cons #\1 unproved-deps)
                                (cons #\2 (access goal (car goals) :goal-name))))
                  (if goals 
                      (fms0 "~|~%The proof of the current goal, ~x0, has been ~
                             completed, as have all of its subgoals.~%Now proving ~x1.~%"
                            (list (cons #\0 name)
                                  (cons #\1 (access goal (car goals) :goal-name))))
                    (pprogn
                     (fms0 "~|*!*!*!*!*!*!* All goals have been proved! ~
                                 *!*!*!*!*!*!*~%")
                     (if (f-get-global 'in-verify-flg state)
                         (fms0 "You may wish to exit.~%")
                       state)))))))))

(defun accumulate-ttree-in-pc-state (pc-state state)
  (er-let* ((ttree (accumulate-ttree-into-state
                    (access pc-state pc-state :tag-tree)
                    state)))
           (value (change-pc-state pc-state :tag-tree ttree))))

(defun pc-process-assumptions (pc-ens ttree wrld state)

; Like process-assumptions, but returns (mv clauses known-assumptions ttree
; state).

  (let ((n (quickly-count-assumptions ttree 0 101)))
    (pprogn
     (cond
      ((< n 101)
       state)
      (t
       (io? prove nil state
            nil
            (fms "~%Note: processing over 100 forced hypotheses which we now ~
                  collect)~%"
                 nil
                 (proofs-co state) state nil))))
     (mv-let
      (n0 assns pairs ttree1)
      (extract-and-clausify-assumptions nil ttree nil pc-ens wrld)
      (cond
       ((= n0 0)
        (mv nil nil ttree state))
       (t
        (mv (strip-cdrs pairs) assns ttree1 state)))))))

(defun make-implication (assumptions concl)
  (cond
    (assumptions
     (fcons-term* (quote implies) (conjoin assumptions) concl))
    (t concl)))

(defun cl-set-to-implications (cl-set)
  (if (null cl-set)
      nil
      (cons (make-implication (butlast (car cl-set) 1)
                              (car (last (car cl-set))))
            (cl-set-to-implications (cdr cl-set)))))

(defun known-assumptions (type-alist assns)

; Here assns is a list of cleaned-up assumptions.  We want to collect those
; assumptions whose hypotheses are clearly true under the given type-alist.
; There seems to be no point in trying to add the ones that don't have this
; property, since they'd only introduce case splits.  In fact, though, probably
; most of the assumptions we encounter will have this property.

  (cond
   ((null assns)
    nil)
   ((dumb-type-alist-implicationp type-alist
                                  (access assumption (car assns) :type-alist))
    (cons (access assumption (car assns) :term)
          (known-assumptions type-alist (cdr assns))))
   (t (known-assumptions type-alist (cdr assns)))))

(defun add-assumptions-to-top-goal
  (goal-unproved-p known-assumptions forced-goals remaining-goals)
  (if forced-goals
      (if goal-unproved-p
          (cons (if known-assumptions
                    (change goal (car remaining-goals)
                            :hyps
                            (append (access goal (car remaining-goals) :hyps)
                                    known-assumptions))
                    (car remaining-goals))
                (append forced-goals (cdr remaining-goals)))
          (append forced-goals remaining-goals))

; Otherwise, we assume that since forced-goals is nil, assns is nil.
; This saves us the cons above.

      remaining-goals))

(defun unproved-goals (pc-state)
  (let ((goals (access pc-state pc-state :goals)))
    (if (and goals
             (equal (access goal (car goals) :conc)
                    *t*))
        (cdr goals)
        goals)))

(defun make-pc-ens (pc-ens state)
  (if (null pc-ens)
      (ens state)
    pc-ens))

(defun initial-rcnst-from-ens (ens wrld)
  (change rewrite-constant *empty-rewrite-constant*
          :current-enabled-structure ens
          :oncep-override (match-free-override wrld)
          :force-info t
          :nonlinearp (non-linearp wrld)))

(defun make-new-goals-fixed-hyps (termlist hyps goal-name start-index)
  ;; similar to make-new-goals
  (if (consp termlist)
      (cons (make goal
                  :conc (car termlist)
                  :hyps hyps
                  :current-addr nil
                  :goal-name (cons goal-name start-index)
                  :depends-on 1)
            (make-new-goals-fixed-hyps (cdr termlist) hyps goal-name
                                       (1+ start-index)))
    nil))

(defun pc-single-step-primitive (instr state)
 (state-global-let*
  ((guard-checking-on nil)) ; see the Essay on Guard Checking
  (let* ((goals (goals))
         (wrld (w state))
         (old-tag-tree (tag-tree)))
    (if (null goals)
        (pprogn (print-all-goals-proved-message state)
                (mv nil nil state))
        (mv-let
         (erp stobjs-out vals state)
         ;; vals is (x replaced-state), where
         ;; x is a pc-state or nil
         (my-trans-eval (list (car instr)
                              (list 'quote (cdr instr))
                              'state)
                        'pc-single-step
                        state)
         ;; I believe that stobjs-out has to be '(nil state), so why bother?
         (declare (ignore stobjs-out))
         (cond
          (erp
           (pprogn (print-no-change

; We used to say "Very odd" here, but it is perfectly natural to get such an
; error if there is an rdepth-error.

                    "An error occurred in executing ~x0."
                    (list (cons #\0 instr)))
                   (mv 'pc-single-step-error-primitive nil state)))
          ((car vals) ;so, there is a new state
           (let ((pc-ens (make-pc-ens (pc-ens) state)))
             (mv-let
              (bad-ass ttree)
              (resume-suspended-assumption-rewriting
               (access pc-state (car vals) :local-tag-tree)
               nil ;ancestors
               nil ;gstack
               nil ;simplify-clause-pot-lst
               (initial-rcnst-from-ens pc-ens wrld)
               wrld
               state)
              (cond
               (bad-ass
                (pprogn
                 (let ((assumnote

; Is the assumnotes field always non-empty?

                        (car (access assumption bad-ass :assumnotes))))
                   (print-no-change
                    "A false assumption was encountered from applying the rune ~
                   ~x0 to the target ~x1."
                    (list (cons #\0 (access assumnote assumnote :rune))
                          (cons #\1 (access assumnote assumnote :target)))))
                 (mv nil nil state)))
               (t
                (let* ((remaining-goals (unproved-goals (car vals)))

; Just what is the invariant on primitive instructions?  Can they leave a goal
; with conclusion *t* other than the top one being operated upon?

                       (goal-name (goal-name))
                       (goal-unproved-p
                        (and remaining-goals
                             (equal goal-name
                                    (access goal (car remaining-goals) :goal-name))))
                       (hyps (hyps))
                       (depends-on (depends-on)))
                  (mv-let
                   (cl-set assns ttree state)
                   (pc-process-assumptions pc-ens ttree wrld state)
                   (mv-let
                    (contradictionp hyps-type-alist ttree0)
                    (if (and assns goal-unproved-p)
                        (type-alist-clause (dumb-negate-lit-lst hyps)
                                           nil nil nil pc-ens wrld
                                           nil nil)
                        ;; else don't bother here
                        (mv nil nil nil))
                    (cond
                     (contradictionp
                      (er-let*
                       ((new-pc-state
                         (let ((local-ttree (cons-tag-trees ttree ttree0)))
                           (accumulate-ttree-in-pc-state
                            (change-pc-state
                             (car vals)
                             :goals
                             (cdr goals)
                             :tag-tree

; We are concerned about the comment in note-2-8-bug-fixes about the
; proof-checker running slowly because of tag-trees.  So we play it safe here
; by avoiding duplication.

                             (scons-tag-trees local-ttree old-tag-tree)
                             :local-tag-tree
                             local-ttree)
                            state))))
                       (pprogn (io? proof-checker nil state
                                    (instr goal-name)
                                    (fms0 "~|AHA!  A contradiction has been ~
                                           discovered in the hypotheses of ~
                                           goal ~x0 in the course of executing ~
                                           instruction ~x1, in the process of ~
                                           preparing to deal with forced ~
                                           assumptions.~|"
                                          (list (cons #\0 goal-name)
                                                (cons #\0 instr))))
                               (io? proof-checker nil state
                                    (goals)
                                    (maybe-print-proved-goal-message
                                     (car goals) goals (cdr goals) state))
                               (pc-assign
                                state-stack
                                (cons
                                 new-pc-state
                                 (state-stack)))
                               (value new-pc-state))))
                     (t
                      (let* ((termlist
                              (cl-set-to-implications cl-set))
                             (forced-goals
                              (make-new-goals-fixed-hyps
                               termlist hyps goal-name depends-on))
                             (new-goals
                              (add-assumptions-to-top-goal
                               goal-unproved-p
                               (known-assumptions hyps-type-alist assns)
                               forced-goals
                               remaining-goals))
                             (pc-state-1
                              (change-pc-state (car vals)
                                               :goals new-goals
                                               :tag-tree
                                               (scons-tag-trees
                                                ttree old-tag-tree)
                                               :local-tag-tree ttree)))
                        (er-let* ((new-pc-state
                                   (accumulate-ttree-in-pc-state
                                    pc-state-1
                                    state)))
                                 (pprogn
                                  (cond
                                   (forced-goals
                                    (io? proof-checker nil state
                                         (forced-goals)
                                         (fms0
                                          "~|NOTE (forcing):  Creating ~n0 new ~
                                           ~#1~[~/goal~/goals~] due to forcing ~
                                           assumptions.~%"
                                          (list
                                           (cons #\0 (length forced-goals))
                                           (cons #\1
                                                 (zero-one-or-more
                                                  (length forced-goals)))))))
                                   (t state))
                                  (io? proof-checker nil state
                                       (new-goals goals)
                                       (maybe-print-proved-goal-message
                                        (car goals) goals new-goals state))
                                  (pc-assign
                                   state-stack
                                   (cons
                                    new-pc-state
                                    (state-stack)))
                                  (value new-pc-state))))))))))))))
          (t
           (mv nil nil state))))))))

(defun maybe-print-macroexpansion (instr raw-instr state)
  (let ((print-macroexpansion-flg (print-macroexpansion-flg)))
    (if (and print-macroexpansion-flg
             (not (eq (car instr) (make-official-pc-command 'lisp t)))
             (not (equal instr (make-official-pc-instr raw-instr state))))
        (io? proof-checker nil state
             (print-macroexpansion-flg instr)
             (fms0 ">> ~x0~|" (list (cons #\0 instr)) 0
                   (if (and (consp print-macroexpansion-flg)
                            (integerp (car print-macroexpansion-flg))
                            (integerp (cdr print-macroexpansion-flg))
                            (> (car print-macroexpansion-flg) 0)
                            (> (cdr print-macroexpansion-flg) 0))
                       (evisc-tuple (car print-macroexpansion-flg)
                                    (cdr print-macroexpansion-flg)
                                    nil nil)
                     nil)))
      state)))

(defun pc-single-step-1 (raw-instr state)
  ;; Returns a triple (signal value new-state).  Among other things, new-state
  ;; contains the new value of the state-stack.  Value is thought of as
  ;; determining "success" or failure of raw-instr -- in particular, if raw-instr
  ;; is primitive (or expands to a primitive instruction) then value
  ;; is the new state (upon success) or nil (upon failure).  Except,
  ;; signal is handy for reporting errors.  Signals are to be used only
  ;; for simulating THROW and CATCH, unless one really wants to throw to
  ;; the top-level loop in case of a "really bad" error.

  ;; **** Obsolete comment follows:

  ;; A little IO thing:  I'll assume that this is called with the cursor already
  ;; on the left margin.  However, since printing may have been done by the prover,
  ;; I'll set last-fms0-col back to 0.

  (mv-let
   (erp instr state)
   (pc-macroexpand raw-instr state)
   (if erp
       (pprogn (io? proof-checker nil state
                    (raw-instr)
                    (fms0 "~%Macroexpansion of instruction ~x0 failed!~%"
                          (list (cons #\0 raw-instr))))
               (mv erp nil state))
     (case (pc-command-type (car instr))
           (primitive
            (pprogn (maybe-print-macroexpansion instr raw-instr state)
                    (pc-single-step-primitive instr state)))
           (meta
            (pprogn (maybe-print-macroexpansion instr raw-instr state)
                    (mv-let (erp stobjs-out vals state)
                            ;; vals is (er x replaced-state), where
                            ;; er is to be passed as the error flag in
                            ;; the triple returned by PC-SINGLE-STEP
                            (my-trans-eval (list (car instr)
                                                 (list 'quote (cdr instr))
                                                 'state)
                                           'pc-single-step
                                           state)
                            (declare (ignore stobjs-out)) ; = '(nil nil state)
                            (if erp
                                ;; actually, this case shouldn't
                                ;; happen, I think...
                                (pprogn (print-no-change
                                         "Very odd -- an error ~
                                          occurred in executing ~x0."
                                         (list (cons #\0 instr)))
                                        (mv 'pc-single-step-error-meta
                                            nil state))
                              (mv (car vals) (cadr vals) state)))))
           ((macro atomic-macro)
            (value (er hard 'pc-single-step
                       "Encountered instruction ~x0 whose pc-macroexpansion ~
                        produced ~x1, which is headed by a macro command!"
                       raw-instr instr)))
           (otherwise
            (pprogn (print-no-change "Undefined instruction, ~x0."
                                     (list (cons #\0
                                                 (make-pretty-pc-instr instr))))
                    ;; maybe I should cause an error below -- but then I should handle it too
                    (value nil)))))))

(defun union-lastn-pc-tag-trees (n ss acc)

; Union together the most recent n local-tag-tree fields of states in the
; state-stack ss.

  (if (zp n)
      acc
    (union-lastn-pc-tag-trees (1- n)
                              (cdr ss)
                              (scons-tag-trees
                               (access pc-state (car ss) :local-tag-tree)
                               acc))))

(defun pc-single-step (raw-instr state)
  ;;   We assume that raw-instr is an "official" instr.
  ;; same as pc-single-step-1, except that we deal with atomic macro commands
  (declare (xargs :guard (consp raw-instr)))
  (let ((tp (pc-command-type (car raw-instr))))
    (if (eq tp 'atomic-macro)
        (let* ((saved-ss (state-stack))
               (old-len (length saved-ss)))
          (mv-let (erp val state)
                  (pc-single-step-1 raw-instr state)
                  (let* ((new-ss (state-stack))
                         (new-len (length new-ss))
                         (diff (- new-len old-len)))
                    (if (and (< old-len new-len)
                             (equal saved-ss (nthcdr diff new-ss)))
                        (pprogn (pc-assign
                                 state-stack
                                 (cons (change pc-state
                                               (car new-ss)
                                               :instruction
                                               (make-pretty-pc-instr raw-instr)
                                               :local-tag-tree
                                               (union-lastn-pc-tag-trees
                                                diff new-ss nil))
                                       saved-ss))
                                ;; Notice that atomic macros can "return errors"
                                ;; even when they "fail".
                                (mv erp val state))
                      (mv erp val state)))))
      (pc-single-step-1 raw-instr state))))

(defconst *pc-complete-signal* 'acl2-pc-complete)

(defun pc-main-loop (instr-list quit-conditions last-value print-prompt-and-instr-flg state)
  ;; Returns an error triple whose state has
  ;; the new state-stack "installed".  Here instr-list is a (true) list of
  ;; instructions or else is a non-NIL atom, probably *standard-oi*,
  ;; from which the instructions are to be read.  Notice that by
  ;; taking (append instrs <stream>), one is able to get the system to read
  ;; from the instr-list input until there are no more instructions,
  ;; and then to read from the stream.
  ;;    quit-conditions indicates when we want to quit; it is a list
  ;; of atoms.  'signal means that we quit when
  ;; there's a signal, while 'value means that we quit when the
  ;; value is nil.  If quit-conditions is empty (nil) then we keep
  ;; going, no matter what.  However, a signal to quit (i.e. *pc-complete-signal*)
  ;; is always obeyed if 'exit is a quit-condition.
  ;;  This only returns non-nil if we exit successfully,
  ;; or if all instructions succeed (null erp, non-nil value) without error.

;;;;  (declare (xargs :guard instr-list));; instr-list should not be nil
;;;;  I can't see why instr-list needs to be non-nil any longer!

  (if (null instr-list)
      (mv nil last-value state)
    (mv-let (col state)
            (if print-prompt-and-instr-flg
                (print-pc-prompt state)
              (mv 0 state))
            (mv-let (erp instr state)
                    (if (consp instr-list)
                        (pprogn (if print-prompt-and-instr-flg
                                    (io? proof-checker nil state
                                         (col instr-list)
                                         (fms0 "~y0~|"
                                               (list (cons #\0
                                                           (car instr-list)))
                                               col))
                                  state)
                                (value (car instr-list)))
                      (state-global-let*
                       ((infixp nil))
                       (read-object instr-list state)))
                    (declare (ignore erp))
                    (mv-let (signal val state)
                            (pc-single-step (make-official-pc-instr instr state) state) 
                            (cond
                             ((and signal
                                   (or (member-eq 'signal quit-conditions)
                                       (and (equal signal *pc-complete-signal*)
                                            (member-eq 'exit quit-conditions))))
                              (mv signal val state))
                             ((and (null val) (member-eq 'value quit-conditions))
                              (mv signal val state))
                             (t (pc-main-loop
                                 (if (consp instr-list)
                                     (cdr instr-list)
                                   instr-list)
                                 quit-conditions
                                 ;; We ultimately "succeed" if and only if every instruction "succeeds".
                                 ;; If that decision is changed, then change documentation in CATCH
                                 ;; meta command.
                                 (and last-value (null signal) val)
                                 print-prompt-and-instr-flg
                                 state))))))))

(defun make-initial-goal (term)
  (make goal
        :conc term
        :hyps nil
        :current-addr nil
        :goal-name 'main
        :depends-on 1))

(defun initial-state-stack (term raw-term event-name rule-classes pc-ens)
  (list (make pc-state
              :instruction (list :start
                                 (list event-name rule-classes raw-term))
              :goals (list (make-initial-goal term))
              :local-tag-tree nil
              :tag-tree nil
              :abbreviations nil
              :pc-ens pc-ens)))

(defun event-name-and-types-and-raw-term (state-stack)
  (cadr (access pc-state (car (last state-stack)) :instruction)))

(defmacro install-initial-state-stack (term raw-term event-name rule-classes)
  `(pprogn
    (pc-assign
     state-stack
     (initial-state-stack ,term ,raw-term ,event-name ,rule-classes
                          ;; the initial enabled structure is nil, meaning
                          ;; that we should use the global enabled structure
                          nil))
    (pc-assign old-ss nil)))

(defun pc-top (raw-term event-name rule-classes instr-list quit-conditions state)
  ;; Here instr-list can have a non-nil last cdr, meaning "proceed
  ;; interactively".
  (declare (xargs :guard (symbolp event-name)))
  (mv-let (erp term state)
          (translate raw-term t t t 'pc-top (w state) state)

; known-stobjs = t (stobjs-out = t)

; Translate, above, does not enforce the mv-let or stobj signature rules.
; It does insist that the translation contain no :program mode functions.

          (if erp
              (mv t nil state)
            (pprogn (install-initial-state-stack term raw-term event-name rule-classes)
                    (pc-main-loop instr-list quit-conditions t t state)))))

(mutual-recursion

; Keep this in sync with termp.

(defun illegal-fnp (x w)
  (cond ((atom x) nil)
        ((eq (car x) 'quote)
         nil)
        ((symbolp (car x))
         (let ((arity (arity (car x) w)))
           (if (and arity
                    (eql (length (cdr x)) arity))
               (illegal-fnp-list (cdr x) w)
             (car x))))
        ((consp (car x))
         (illegal-fnp-list (cdr x) w))
        (t nil)))

(defun illegal-fnp-list (x w)
  (cond ((endp x) nil)
        (t (or (illegal-fnp (car x) w)
               (illegal-fnp-list (cdr x) w)))))
)

(defun verify-fn (raw-term raw-term-supplied-p event-name rule-classes instructions state)
  (if (f-get-global 'in-verify-flg state)
      (er soft 'verify
          "You are apparently already inside the VERIFY interactive loop.  It is ~
           illegal to enter such a loop recursively.")
    (mv-let
      (erp val state)
      (if raw-term-supplied-p
          (state-global-let*
           ((in-verify-flg t)
            (print-base 10))
           (pc-top raw-term event-name rule-classes
                   (append instructions *standard-oi*)
                   (list 'exit)
                   state))
        (if (null (state-stack))
            (er soft 'verify "There is no interactive verification to re-enter!")
          (let ((bad-fn
                 (illegal-fnp
                  (access goal
                          (car (access pc-state (car (last (state-stack)))
                                       :goals))
                          :conc)
                  (w state))))
            (if bad-fn
                (er soft 'verify
                    "The current proof-checker session was begun in an ACL2 ~
                     world with function symbol ~x0, but that function symbol ~
                     no longer exists."
                    bad-fn)
              (state-global-let*
               ((in-verify-flg t)
                (print-base 10))
               (pc-main-loop (append instructions *standard-oi*)
                             (list 'exit)
                             t t state))))))
      (if (equal erp *pc-complete-signal*)
          (value val)
        (mv erp val state)))))

(defun print-unproved-goals-message (goals state)
  (io? proof-checker nil state
       (goals)
       (fms0 "~%There ~#0~[is~/are~] ~x1 unproved goal~#0~[~/s~] from replay ~
              of instructions.  To enter the proof-checker state that exists ~
              at this point, type (VERIFY).~%"
             (list (cons #\0 goals)
                   (cons #\1 (length goals))))))

(defun state-stack-from-instructions
  (raw-term event-name rule-classes instructions replay-flg quit-conditions state)
  (if replay-flg
      (pprogn (io? proof-checker nil state
                   nil
                   (fms0 "~|~%Entering the proof-checker....~%~%"))
              (er-progn (pc-top raw-term event-name rule-classes
                                instructions quit-conditions state)
                        (value (state-stack))))
    (value (state-stack))))

(defun state-from-instructions
  (raw-term event-name rule-classes instructions quit-conditions state)
  (mv-let (erp val state)
          (pc-top raw-term event-name rule-classes
                  instructions quit-conditions state)
          (declare (ignore erp val))
          state))

(defun print-defthm (ev state)
  (let ((ldd (make-ldd 'event nil #\Space 0 t
                       ev)))
    (io? proof-checker nil state
         (ldd)
         (fms0 "~|~y0"
               (list (cons #\0
                           (print-ldd-full-or-sketch
                            (access-ldd-fullp ldd)
                            (access-ldd-form ldd))))))))

(defmacro print-goal (&optional goal)
  `(let ((goal ,(or goal '(car (access pc-state (car (state-stack)) :goals)))))
     (io? proof-checker nil state
          (goal)
          (if goal
              (fms0
               "~%-------  ~x3  -------~|~
           Conc:  ~q0~|~
           Hyps:  ~q1~|~
           Addr:  ~y2~|~
           Deps:  ~y4~|"
               (list 
                (cons #\0 (untranslate (access goal goal :conc) t (w state)))
                (cons #\1 (let ((hyps (access goal goal :hyps)))
                            (cond ((null hyps) t)
                                  ((null (cdr hyps))
                                   (untranslate (car hyps) t (w state)))
                                  (t (cons 'and (untranslate-lst
                                                 hyps t (w state)))))))
                (cons #\2 (access goal goal :current-addr))
                (cons #\3 (access goal goal :goal-name))
                (cons #\4 (access goal goal :depends-on))))
            (fms0 "~%No goal in CAR of state-stack.~|")))))

(defmacro print-pc-state (&optional pc-state)
  `(let ((pc-state ,(or pc-state '(car (state-stack)))))
     (io? proof-checker nil state
          (pc-state)
          (if pc-state
              (fms0
               "~%Instr:       ~y0~|~
                Goals:       ~y1~|~
                Abbrs:       ~y2~|~
                Local ttree: ~y3~|~
                Ttree:       ~y4~|"
               (list 
                (cons #\0 (access pc-state pc-state :instruction))
                (cons #\1 (access pc-state pc-state :goals))
                (cons #\2 (access pc-state pc-state :abbreviations))
                (cons #\3 (access pc-state pc-state :local-tag-tree))
                (cons #\4 (access pc-state pc-state :tag-tree))))
            (fms0 "~%No state in CAR of state-stack.~|")))))

(defun proof-checker
  (event-name raw-term term rule-classes instructions wrld state)
  ;; I'm only including wrld in the arglist because J has it there.
  ;; **** Be sure that in-verify-flg is untouchable, for soundness here (or
  ;; is that really an issue?).

  ":Doc-Section Proof-checker

  support for low-level interaction~/

  Call this up with ~c[(verify ...)].

  ~/

  This is an interactive system for checking theorems in ACL2.  One
  enters it using VERIFY; ~pl[verify].  The result of an
  interactive session is a ~ilc[defthm] event with an ~c[:]~ilc[instructions]
  parameter supplied; see the documentation for proof-checker command
  ~c[exit] (in package ~c[\"ACL2-PC\"]).  Such an event can be replayed later
  in a new ACL2 session with the ``proof-checker'' loaded.

  A tutorial is available on the world-wide web:
  ~bv[]
  http://www.cs.utexas.edu/users/kaufmann/tutorial/rev3.html
  ~ev[]
  The tutorial illustrates more than just the proof-checker.  The portion
  relevant to the proof-checker may be accessed directly:
  ~bv[]
  http://www.cs.utexas.edu/users/kaufmann/tutorial/rev3.html#slide29
  ~ev[]

  Individual proof-checker commands are documented in subsection
  ~il[proof-checker-commands]."

  (declare (ignore term wrld))
  (cond
   ((and (not (f-get-global 'in-verify-flg state))
         (ld-skip-proofsp state))

; Thus, we are not in an interactive loop, and we are to skip proofs.

    (value nil))
   (t
    (mv-let (erp state-stack state)
            (state-stack-from-instructions
             raw-term event-name rule-classes instructions
             (not (f-get-global 'in-verify-flg state))
             '(signal value)
             state)
            ;; could perhaps (declare (ignore erp)), but for now I'll abort upon error
            (if erp
                (pprogn
                 (io? proof-checker nil state
                      nil
                      (fms0 "~%~%Replay of proof-checker instructions ~
                             aborted.~%"))
                        (if (f-get-global 'in-verify-flg state)
                            (mv *pc-complete-signal* nil state)
                          (silent-error state)))
              (let ((goals (access pc-state (car state-stack) :goals)))
                (if (null goals)
                    (value (access pc-state (car state-stack) :tag-tree))
                  (pprogn
                   ;; could print the goals here instead of just the number of goals.
                   (print-unproved-goals-message goals state)
                   (if (f-get-global 'in-verify-flg state)
                       (mv *pc-complete-signal* nil state)
                     (silent-error state))))))))))

(deflabel proof-checker-commands
  :doc
  ":Doc-Section Proof-checker

  list of commands for the proof-checker~/

  This documentation section contains documentation for individual
  commands that can be given inside the interactive ~il[proof-checker] loop
  that is entered using ~ilc[verify].~/~/")

(deflabel macro-command
  :doc
  ":Doc-Section Proof-checker

  compound command for the proof-checker~/

  The proof-checker (~pl[proof-checker]) allows the user to supply
  interactive commands.  Compound commands, called macro commands, may
  be defined; these expand into zero or more other commands.  Some of
  these are ``atomic'' macro commands; these are viewed as a single
  command step when completed successfully.~/

  More ~il[documentation] will be written on the ~il[proof-checker].  For now,
  we simply point out that there are lots of examples of the use of
  ~c[define-pc-macro] and ~c[define-pc-atomic-macro] in the ACL2 source file
  ~c[\"proof-checker-b.lisp\"].  The former is used to create macro
  commands, which can be submitted to the interactive loop
  (~pl[verify]) and will ``expand'' into zero or more commands.
  The latter is similar, except that the undoing mechanism
  (~pl[acl2-pc::undo]) understands atomic macro commands to
  represent single interactive commands.  Also ~pl[acl2-pc::comm]
  and ~pl[acl2-pc::commands] for a discussion of the display of
  interactive commands.

  Also ~pl[toggle-pc-macro] for how to change a macro command to
  an atomic macro command, and vice versa.")

(defmacro verify (&optional (raw-term 'nil raw-term-supplied-p)
                            &key
                            event-name
                            (rule-classes '(:rewrite))
                            instructions)
  ":Doc-Section Proof-checker

  enter the interactive proof checker~/

  For proof-checker command summaries, ~pl[proof-checker].~/
  ~bv[]
  Examples:
  (VERIFY (implies (and (true-listp x) (true-listp y))
                        (equal (append (append x y) z)
                               (append x (append y z)))))
     -- Attempt to prove the given term interactively.

  (VERIFY (p x)
          :event-name p-always-holds
          :rule-classes (:rewrite :generalize)
          :instructions ((rewrite p-always-holds-lemma)
                         change-goal))
     -- Attempt to prove (p x), where the intention is to call the
        resulting DEFTHM event by the name p-always-holds, with
        rule-classes as indicated.  The two indicated instructions
        will be run immediately to start the proof.

  (VERIFY)
     -- Re-enter the proof-checker in the state at which is was last
        left.

  General Form:
  (VERIFY &OPTIONAL raw-term
          &KEY
          event-name
          rule-classes
          instructions)
  ~ev[]
  ~c[Verify] is the function used for entering the ~il[proof-checker]'s
  interactive loop."

  (if (and raw-term-supplied-p (eq raw-term nil))
      '(pprogn
        (io? proof-checker nil state
             nil
             (fms0 "It is not permitted to enter the interactive proof-checker ~
                    with a goal of NIL!  If you really MEANT to do such a ~
                    thing, (VERIFY 'NIL).~%"))
               (value :invisible))
    `(verify-fn ',raw-term ',raw-term-supplied-p ',event-name
                ',rule-classes ',instructions state)))

(deflabel instructions
  :doc
  ":Doc-Section Proof-checker

  instructions to the proof checker~/
  ~bv[]
  Example:
  :instructions (induct prove promote (dive 1) x
                        (dive 2) = top (drop 2) prove)
  ~ev[]~/

  ~l[defthm] for the role of ~c[:instructions] in place of
  ~c[:]~ilc[hints].  As illustrated by the example above, the value
  associated with ~c[:instructions] is a list of ~il[proof-checker]
  commands.  At the moment the best way to understand the idea of the
  interactive proof-checker (~pl[proof-checker] and
  ~pl[verify]) is probably to read the first 11 pages of CLI
  Technical Report 19, which describes the corresponding facility for
  Nqthm.

  When inside the interactive loop (i.e., after executing ~ilc[verify]),
  use ~ilc[help] to get a list of legal instructions and ~c[(help instr)]
  to get help for the instruction ~c[instr].")

; Finally, here is some stuff that is needed not only for the proof-checker but
; also for :pl.

(mutual-recursion

(defun sublis-expr-non-quoteps (alist term)

  ;; Same as ACL2's function sublis-expr, except that it doesn't take a
  ;; world argument.  However, for correctness it may be necessary that
  ;; every CDR in ALIST is non-quotep, so that we can guarantee that
  ;; non-quotep's are mapped to non-quotep's.

  (let ((temp (assoc-equal term alist)))
    (cond (temp (cdr temp))
          ((variablep term) term)
          ((fquotep term) term)
          (t (let ((new-args (sublis-expr-non-quoteps-lst alist (fargs term))))
               (if (quote-listp new-args)
                   ;; then no substitution was actually made
                   term
                 ;; otherwise, cons-term becomes simply cons
                 (cons (ffn-symb term) new-args)))))))

(defun sublis-expr-non-quoteps-lst (alist lst)
  (cond ((null lst) nil)
        (t (cons (sublis-expr-non-quoteps alist (car lst))
                 (sublis-expr-non-quoteps-lst alist (cdr lst))))))

)

(defun invert-abbreviations-alist (alist)
  (declare (xargs :guard (alistp alist)))
  (if (null alist)
      nil
    (cons (cons (cdr (car alist)) (list '? (car (car alist))))
          (invert-abbreviations-alist (cdr alist)))))

(defun abbreviate (term abbreviations)
  (if (null abbreviations)
      term
    (sublis-expr-non-quoteps (invert-abbreviations-alist abbreviations) term)))

(defmacro untrans0 (term &optional iff-flg abbreviations)

; Note that state should always be bound where this is called.

  `(untranslate (abbreviate ,term ,abbreviations) ,iff-flg (w state)))

(defun untrans0-lst-fn (termlist iff-flg abbreviations state)
  (if (consp termlist)
      (cons (untrans0 (car termlist) iff-flg abbreviations)
            (untrans0-lst-fn (cdr termlist) iff-flg abbreviations state))
    nil))

(defmacro untrans0-lst (termlist &optional iff-flg abbreviations)
  `(untrans0-lst-fn ,termlist ,iff-flg ,abbreviations state))
