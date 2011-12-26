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

; This file, ld.lisp, provides the definition of the ACL2 macro ld,
; which implements both the ACL2 read-eval-print loop and the ACL2
; file loader.

(defun default-print-prompt (channel state)

; This is the default function for printing the ACL2 ld loop prompt.  A typical
; prompt looks like: ACL2 !>, where the number of >'s indicates the ld-level.
; The prompt is printed by (fmt "~@0~sr ~@1~*2" a channel state nil), where a
; is an alist computed from current-package, ld-level, default-defun-mode,
; guard-checking-on, and ld-skip-proofsp, and #\r is bound to "" except for the
; #+:non-standard-analysis version, where it is bound to "(r)".  To keep from
; consing up this alist every time, we memoize it, storing in 'prompt-memo the
; tuple (pkg level skipp defun-mode+ gc-on a), where defun-mode+ is the
; default-defun-mode except in raw-mode, where defun-mode+ is nil.  Thus, if
; the current settings are as in the memo, we use the a in the memo.
; Otherwise, we compute and store a new memo.

; Warning:  If you change the default prompt format, be sure to change it
; in eval-event-lst, where we print it by hand.

  ":Doc-Section Miscellaneous

  the default ~il[prompt] printed by ~ilc[ld]~/
  ~bv[]
  Example prompt:
  ACL2 p!s>
  ~ev[]
  The ~il[prompt] printed by ACL2 displays the current package, followed by
  a space, followed by zero or more of the three ~il[characters] as
  specified below, followed by the character ~ilc[>] printed one or more
  times, reflecting the number of recursive calls of ~ilc[ld].  The three
  ~il[characters] in the middle are as follows:
  ~bv[]
  p     ; when (default-defun-mode (w state)) is :program
  !     ; when guard checking is on
  s     ; when (ld-skip-proofsp state) is t
  ~ev[]
  ~l[default-defun-mode], ~pl[set-guard-checking], and
  ~pl[ld-skip-proofsp].~/

  Also ~pl[ld-prompt] to see how to install your own ~il[prompt].

  Here are some examples with ~c[ld-skip-proofsp nil].
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
  Finally, here is the prompt in raw mode (~pl[set-raw-mode]),
  regardless of the settings above:
  ~bv[]
  ACL2 P>
  ~ev[]~/
  "

  (let ((prompt-memo (and (f-boundp-global 'prompt-memo state)
                          (f-get-global 'prompt-memo state))))
    (cond
     ((and prompt-memo
           (equal (car prompt-memo) (f-get-global 'current-package state))
           (equal (cadr prompt-memo) (f-get-global 'ld-level state))
           (eq (caddr prompt-memo) (f-get-global 'ld-skip-proofsp state))
           (eq (cadddr prompt-memo) (and (not (raw-mode-p state))
                                         (default-defun-mode (w state))))

; In the following, we could use iff instead of eq, because the dependence of
; defun-mode-prompt on (f-get-global 'guard-checking-on state) is restricted to
; whether or not the latter is nil/:none.  But it's cheap to update the
; prompt-memo so we keep the more restrictive eq test for robustness, in case
; the code for defun-mode-prompt changes.

           (eq (car (cddddr prompt-memo))
               (f-get-global 'guard-checking-on state)))
      (fmt1 "~@0~sr ~@1~*2" (cadr (cddddr prompt-memo)) 0 channel state nil))
     (t
      (let ((alist
             (list (cons #\0 (f-get-global 'current-package state))
                   (cons #\1 (defun-mode-prompt-string state))
                   (cons #\2 (list "" ">" ">" ">"
                                   (make-list-ac (f-get-global 'ld-level state)
                                                 nil nil)))
                   (cons #\r
                         #+:non-standard-analysis "(r)"
                         #-:non-standard-analysis ""))))
       (pprogn
        (f-put-global 'prompt-memo
                      (list (f-get-global 'current-package state)
                            (f-get-global 'ld-level state)
                            (f-get-global 'ld-skip-proofsp state)
                            (and (not (raw-mode-p state))
                                 (default-defun-mode (w state)))
                            (not (member-eq (f-get-global 'guard-checking-on
                                                          state)
                                            '(nil :none)))

; There is no need to memoize the binding of #\r for the purpose of checking if
; the prompt is current, since it never changes during a given session.  Of
; course, #\r is bound in the alist.

                            alist)
                      state)
        (fmt1 "~@0~sr ~@1~*2" alist 0 channel state nil)))))))

(defun print-prompt (prompt output-channel state)
  (with-output-forced
   output-channel
   (col state)
   (let ((prompt-fn (cond ((null prompt) nil)
                          ((eq prompt t)
                           (f-get-global 'prompt-function state))
                          (t prompt))))
     (cond
      ((null prompt-fn) (mv 0 state))
      ((eq prompt-fn 'default-print-prompt)
       (default-print-prompt output-channel state))
      (t (mv-let (erp trans-ans state)
           (trans-eval (list prompt-fn
                             (list 'quote output-channel)
                             'state)
                       'print-prompt state)

; If erp is non-nil, trans-ans is of the form (stobjs-out . valx).  We
; strongly expect that stobjs-out is (nil state).  (That is true if
; prompt is in fact ld-prompt.)  That being the case, we expect
; valx to be (col replaced-state).

           (cond
            ((or erp
                 (not (and (equal (car trans-ans) '(nil state))
                           (integerp (car (cdr trans-ans))))))
             (fmt1 "~%~%Bad Prompt~%See :DOC ld-prompt>"
                   nil 0 output-channel state nil))
            (t (mv (car (cdr trans-ans)) state)))))))))

(defun initialize-timers (state)
  (pprogn
   (set-timer 'prove-time '(0) state)
   (set-timer 'print-time '(0) state)
   (set-timer 'proof-tree-time '(0) state)
   (set-timer 'other-time '(0) state)))

(defun maybe-add-command-landmark (old-wrld old-default-defun-mode
                                            form last-make-event-expansion
                                            trans-ans state)

; Old-wrld is the world before the trans-evaluation of form.  That
; trans-evaluation returned trans-ans, which is thus of the form (stobjs-out
; . valx).  If valx contains a state (then it must in fact contain the state
; state), and the current world of that state is different from old-wrld and
; does not end with a command landmark, we add a command landmark for form.

; We set world global 'skip-proofs-seen or 'redef-seen if ld-skip-proofsp or
; ld-redefinition-action (respectively) is non-nil and the world global is not
; already true.  This way we can note when a form was executed with non-nil
; ld-skip-proofsp, in case this world becomes part of a certification world.
; Note-certification-world will cause an error if it encounters a
; set-ld-skip-proofsp or set-ld-redefition-action inside a command in the
; certification world, because these are not legal embedded event forms.  So if
; any proofs were skipped or redefinition was done for an event in a legal
; certification world, then 'ld-skip-proofsp (if the event is not explicitly
; marked with skip-proofs) or 'ld-redefinition-action, respectively, will be
; non-nil when that event was executed, so the corresponding world global will
; be set.

; We pass in old-default-defun-mode as the default-defun-mode of old-wrld.
; This way, we can compute that value at a time that old-wrld is still
; installed, so that the corresponding getprop will be fast.

  (let ((wrld (w state)))
    (cond ((and (member-eq 'state (car trans-ans))
                (not (and (eq (caar wrld) 'command-landmark)
                          (eq (cadar wrld) 'global-value)))
                (not (equal old-wrld wrld)))
           (pprogn
            (cond ((and (raw-mode-p state)
                        (not (eq wrld old-wrld)))

; If we are in raw mode, then it is scary to imagine that we have changed the
; logical world.

                   (warning$ 'top-level "Raw"
                             "The ACL2 world is being modified while in raw ~
                              mode.  See :DOC set-raw-mode.  Further ~
                              computation in this ACL2 may have some ~
                              surprising results.  Use :oops if this is not ~
                              what you intended (and ignore this warning if ~
                              you see it with :oops)."))
                  (t state))
            (set-w 'extension
                   (let* ((wrld1
                           (cond
                            ((and (ld-skip-proofsp state)
                                  (not (global-val 'include-book-path wrld))
                                  (not (global-val 'skip-proofs-seen wrld)))
                             (global-set 'skip-proofs-seen form wrld))
                            (t wrld)))
                          (wrld2
                           (cond
                            ((and (ld-redefinition-action state)
                                  (not (global-val 'redef-seen wrld)))
                             (global-set 'redef-seen form wrld1))
                            (t wrld1))))
                     (add-command-landmark old-default-defun-mode
                                           form last-make-event-expansion
                                           wrld2))
                   state)))
          (t state))))

(defun replace-last-cdr (x val)
  (cond ((atom x) val)
        ((atom (cdr x)) (cons (car x) val))
        (t (cons (car x) (replace-last-cdr (cdr x) val)))))

(defun chk-acceptable-ld-fn1-pair (pair ctx state co-string co-channel)

; We check that pair, which is of the form (var . val) where var is a
; symbolp, specifies a legitimate "binding" for the LD special var.
; This means that we check that var is one of the state globals that
; LD appears to bind (i.e., push and pop in an unwind-protected way)
; and that val is a reasonable value of that global.  For example,
; 'standard-oi is an LD special but must be bound to a true-list of
; objects or an open object input channel.

; Co-string and co-channel are here to provide a very subtle feature
; of LD.  If the same string is specified for both standard-co and
; proofs-co then we open one channel and use it in both places.  Our
; caller, chk-acceptable-ld-fn1, is responsible for maintaining these
; two accumulators as we map down the list of pairs.  It puts into
; co-string and co-channel the string and returned channel for the
; first of standard-co or proofs-co encountered.

  (let ((var (car pair))
        (val (cdr pair)))

; The first three LD specials, namely the three channels, are special
; because we may have to open a channel and create a new pair.  Once
; we get past those three, we can just use the standard checkers
; and return the existing pair.

    (case var
          (standard-oi
           (cond
            ((and (symbolp val)
                  (open-input-channel-p val :object state))
             (value pair))
            ((true-listp val)
             (value pair))
            ((stringp val)
             (let ((val (extend-pathname
                         (f-get-global 'connected-book-directory state)
                         val
                         (os (w state)))))
               (mv-let (ch state)
                       (open-input-channel val :object state)
                       (cond (ch (value (cons 'standard-oi ch)))
                             (t (er soft ctx
                                    "~@0  It is likely that the file you ~
                                     requested, ~x1, does not exist."
                                    (msg *ld-special-error*
                                         'standard-oi val)
                                    val))))))
            ((consp val)
             (let ((last-cons (last val)))
               (cond
                ((and (symbolp (cdr last-cons))
                      (open-input-channel-p (cdr last-cons) :object state))
                 (value pair))
                ((stringp (cdr last-cons))
                 (let ((file-name (extend-pathname
                                   (f-get-global 'connected-book-directory state)
                                   (cdr last-cons)
                                   (os (w state)))))
                   (mv-let (ch state)
                           (open-input-channel file-name :object state)
                           (cond
                            (ch (value (cons 'standard-oi
                                             (replace-last-cdr val ch))))
                            (t (er soft ctx
                                   "~@0  It is likely that the file you ~
                                    requested, ~x1, does not exist."
                                   (msg *ld-special-error*
                                        'standard-oi val)
                                   file-name))))))
                (t (er soft ctx *ld-special-error* 'standard-oi val)))))
            (t (er soft ctx *ld-special-error* 'standard-oi val))))
          (standard-co
           (cond
            ((and (symbolp val)
                  (open-output-channel-p val :character state))
             (value pair))
            ((equal val co-string)
             (value (cons 'standard-co co-channel)))
            ((stringp val)
             (mv-let (ch state)
                     (open-output-channel
                      (extend-pathname
                       (f-get-global 'connected-book-directory state)
                       val
                       (os (w state)))
                      :character
                      state)
                     (cond (ch (value (cons 'standard-co ch)))
                           (t (er soft ctx *ld-special-error* 'standard-co
                                  val)))))
            (t (er soft ctx *ld-special-error* 'standard-co val))))
          (proofs-co
           (cond
            ((and (symbolp val)
                  (open-output-channel-p val :character state))
             (value pair))
            ((equal val co-string)
             (value (cons 'proofs-co co-channel)))
            ((stringp val)
             (mv-let (ch state)
                     (open-output-channel
                      (extend-pathname
                       (f-get-global 'connected-book-directory state)
                       val
                       (os (w state)))
                      :character
                      state)
                     (cond (ch (value (cons 'proofs-co ch)))
                           (t (er soft ctx *ld-special-error* 'proofs-co val)))))
            (t (er soft ctx *ld-special-error* 'proofs-co val))))
          (current-package
           (er-progn (chk-current-package val ctx state)
                     (value pair)))
          (ld-skip-proofsp
           (er-progn (chk-ld-skip-proofsp val ctx state)
                     (value pair)))
          (ld-redefinition-action
           (er-progn (chk-ld-redefinition-action val ctx state)
                     (value pair)))
          (ld-prompt
           (er-progn (chk-ld-prompt val ctx state)
                     (value pair)))
          (ld-keyword-aliases
           (er-progn (chk-ld-keyword-aliases val ctx state)
                     (value pair)))
          (ld-pre-eval-filter
           (er-progn (chk-ld-pre-eval-filter val ctx state)
                     (value pair)))
          (ld-pre-eval-print
           (er-progn (chk-ld-pre-eval-print val ctx state)
                     (value pair)))
          (ld-post-eval-print
           (er-progn (chk-ld-post-eval-print val ctx state)
                     (value pair)))
          (ld-evisc-tuple
           (er-progn (chk-ld-evisc-tuple val ctx state)
                     (value pair)))
          (ld-error-triples
           (er-progn (chk-ld-error-triples val ctx state)
                     (value pair)))
          (ld-error-action
           (er-progn (chk-ld-error-action val ctx state)
                     (value pair)))
          (ld-query-control-alist
           (er-progn (chk-ld-query-control-alist val ctx state)
                     (value pair)))
          (ld-verbose
           (er-progn (chk-ld-verbose val ctx state)
                     (value pair)))
          (otherwise
           (er soft ctx
               "The variable ~x0 is not an authorized LD special and ~
                hence cannot be bound by LD."
               var)))))

(defun close-channels (channel-closing-alist state)

; It is necessary to close the channels that we open.  We must in fact
; record them somewhere in state so that if we abort LD with a hard error or
; user interrupt that throws us into the unwind-protect code of LP, they are
; still closed.  To enable such "remote closings" we invent the notion of a
; "channel closing alist" which is an alist that pairs opened channels to
; their "types", where a type is either 'oi (object input) or 'co (character
; output).  Given such an alist we close each channel in it, if the channel
; is in fact open.

  (cond
   ((null channel-closing-alist) state)
   (t (pprogn
       (cond
        ((eq (cdar channel-closing-alist) 'oi)
         (cond
          ((open-input-channel-p (caar channel-closing-alist) :object state)
           (close-input-channel (caar channel-closing-alist) state))
          (t state)))
        ((eq (cdar channel-closing-alist) 'co)
         (cond
          ((open-output-channel-p (caar channel-closing-alist)
                                  :character state)
           (close-output-channel (caar channel-closing-alist) state))
          (t state)))
        (t (let ((temp (er hard 'close-channels
                           "The channel ~x0 was tagged with an ~
                                 unimplemented channel type, ~x1."
                           (caar channel-closing-alist)
                           (cdar channel-closing-alist))))
             (declare (ignore temp))
             state)))
       (close-channels (cdr channel-closing-alist) state)))))

(defun chk-acceptable-ld-fn1 (alist ctx state co-string co-channel
                                    new-alist channel-closing-alist)

; We copy alist (reversing it) onto new-alist, checking that each pair in it
; binds an LD special to a legitimate value.  We open the requested files as we
; go and replace the file names with the open channels.  We also accumulate
; into channel-closing-alist the pairs necessary to close (with close-channels)
; the channels we have opened.  We return a pair consisting of the new-alist
; and the final channel-closing-alist.  See chk-acceptable-ld-fn1-pair for an
; explanation of co-string and co-channel.

; Implementation Note: This odd structure has the single redeeming feature that
; if any given pair of alist causes an error, we have in our hands enough
; information to close any channels we might have opened thus far.  If we get
; all the way down alist without causing an error, the channel-closing-alist
; will be used in the acl2-unwind-protect cleanup form and enable us to "close
; on pop" -- which was its original purpose.  But an earlier coding of this
; function suffered from the problem that we could open several channels and
; then, right here, cause an error (e.g., the proposed 'current-package setting
; is bad).  If that happened, those open channels would never be closed.  It is
; still possible to "lose" an opened channel: abort this function after some
; files have been opened.

; This flaw cannot be fixed, at least with the current set of primitives.  To
; close a channel we must have the channel.  We don't have the channel until
; after we have opened it, i.e., the way we get our hands on a channel in ACL2
; is to open a file, but the way we close a channel is to call
; close-output-channel on the channel object (rather than the file).  Thus,
; there is no way we can unwind protect code that opens a channel so as to
; guarantee to close the channel because we can't get the object we are to
; "cleanup" (the channel) until after we have "modified" (opened) it.  So there
; is a window of vulnerability between the time we open the channel and the
; time we stash it away in some location known to our cleanup form.  During
; that window an abort can cause us to lose a channel in the sense that we do
; not close it.  Now we can make that window much smaller than it is now.  As
; things stand now we are vulnerable to aborts from the time we start
; processing alist here until we finish and enter the acl2-unwind-protect in
; ld-fn that "binds" the ld specials.  But all this vulnerability means is that
; lisp fails to close some opened channels during an abort.  If such a thing
; happens, the user could detect it with some poking around.  For example, he
; could just type

; (open-output-channel-p 'ACL2-OUTPUT-CHANNEL::STANDARD-CHARACTER-OUTPUT-i
;                        :character state)

; for a bunch of i starting at 0 and see if there are some he doesn't know
; about.  This is not a catastrophic error.  It is as though the abort placed
; in the open-output-channels field of the state an additional channel or two.
; The only way, as far as we can see, that this can be a problem is in the
; sense of resource exhaustion: operating systems (and thus lisps) generally
; allow a finite number of open channels.

; If we someday endeavor to plug this hole some additional care must be taken
; because the act of opening an ACL2 channel (in raw lisp) is non-atomic -- we
; have to open the stream, generate a channel symbol, and store some stuff on
; the property list of the symbol.  So an abort there can cause an
; irretrievable loss of an open channel unless the problem is addressed down
; there as well.

; Finally we would just like to note that soft errors are handled perfectly
; here in the sense that if some channels are opened and then we get a soft
; error, we close the channels.  And aborts are handled perfectly once we get
; outside of the window of vulnerability discussed.

  (cond
   ((null alist) (value (cons new-alist channel-closing-alist)))
   (t (mv-let
       (erp pair state) 
       (chk-acceptable-ld-fn1-pair (car alist) ctx state co-string co-channel)
       (cond
        (erp (pprogn
              (close-channels channel-closing-alist state)
              (mv t nil state)))
        (t (chk-acceptable-ld-fn1
            (cdr alist) ctx state
            (cond ((and (null co-string)
                        (or (eq (car pair) 'standard-co)
                            (eq (car pair) 'proofs-co))
                        (stringp (cdr (car alist))))
                   (extend-pathname
                    (f-get-global 'connected-book-directory state)
                    (cdr (car alist))
                    (os (w state))))
                  (t co-string))
            (cond ((and (null co-channel)
                        (or (eq (car pair) 'standard-co)
                            (eq (car pair) 'proofs-co))
                        (stringp (cdr (car alist))))
                   (cdr pair))
                  (t co-channel))
            (cons pair new-alist)
            (cond
             ((eq (car pair) 'standard-oi)
              (cond ((stringp (cdr (car alist)))
                     (cons (cons (cdr pair) 'oi) channel-closing-alist))
                    ((and (consp (cdr (car alist)))
                          (stringp (cdr (last (cdr (car alist))))))
                     (cons (cons (cdr (last (cdr pair))) 'oi) channel-closing-alist))
                    (t channel-closing-alist)))
             ((and (or (eq (car pair) 'standard-co)
                       (eq (car pair) 'proofs-co))
                   (stringp (cdr (car alist))))
              (cons (cons (cdr pair) 'co) channel-closing-alist))
             (t channel-closing-alist)))))))))

(defun chk-acceptable-ld-fn (alist state)

; Alist is an alist that pairs LD specials with proposed values.  We check
; that those values are legitimate and that only authorized LD specials are
; bound.  If strings are supplied for the specials standard-oi, standard-co,
; and proofs-co, we open corresponding channels and put those channels in
; for the values in the alist.  We return a pair consisting of the modified
; alist and a channel closing alist that pairs opened channels with the
; type information it takes to close them.

  (let ((ctx 'ld))
    (er-progn
     (cond
      ((or (null (f-boundp-global 'current-acl2-world state))
           (null (w state)))
       (er soft ctx
           "The theorem prover's data base has not yet been initialized.  To ~
            initialize ACL2 to its full theory, which currently takes about 3 ~
            minutes on a Sparc 2 (Dec. 1992), invoke (initialize-acl2) from ~
            Common Lisp."))
      (t (value nil)))
     (cond ((symbol-alistp alist) (value nil))
           (t (er soft ctx
                  "The argument to ld-fn must be a symbol-alistp and ~x0 is ~
                   not."
                  alist)))
     (cond ((assoc-eq 'standard-oi alist) (value nil))
           (t (er soft ctx
                  "The alist argument to ld-fn must specify a value ~
                   for 'standard-oi and ~x0 does not."
                  alist)))
     (cond ((no-duplicatesp-equal (strip-cars alist)) (value nil))
           (t (er soft ctx
                  "The alist argument to ld-fn must contain no duplications ~
                   among the LD specials to be bound.  Your alist contains ~
                   duplicate values for ~&0."
                  (duplicates (strip-cars alist)))))
     (chk-acceptable-ld-fn1 alist ctx state nil nil nil nil))))

(defun f-put-ld-specials (alist state)

; Alist is an alist that pairs LD specials with their new values.  We
; f-put-global each special.  Because f-put-global requires an explicitly
; quoted variable, we case split on the authorized LD-specials.  This is
; easier and safer than making translate give us special treatment.  To add
; a new LD-special you must change this function, as well as
; f-get-ld-specials and the checker chk-acceptable-ld-fn1-pair.

; Warning: Somebody else better have checked that the values assigned are
; legitimate.  For example, we here set 'current-package to whatever we are
; told to set it.  This is not a function the user should call!

  (cond
   ((null alist) state)
   (t (pprogn
       (case
        (caar alist)
        (standard-oi
         (f-put-global 'standard-oi (cdar alist) state))
        (standard-co
         (f-put-global 'standard-co (cdar alist) state))
        (proofs-co
         (f-put-global 'proofs-co (cdar alist) state))
        (current-package
         (f-put-global 'current-package (cdar alist) state))
        (ld-skip-proofsp
         (f-put-global 'ld-skip-proofsp (cdar alist) state))
        (ld-redefinition-action
         (f-put-global 'ld-redefinition-action (cdar alist) state))
        (ld-prompt
         (f-put-global 'ld-prompt (cdar alist) state))
        (ld-keyword-aliases
         (f-put-global 'ld-keyword-aliases (cdar alist) state))
        (ld-pre-eval-filter
         (f-put-global 'ld-pre-eval-filter (cdar alist) state))
        (ld-pre-eval-print
         (f-put-global 'ld-pre-eval-print (cdar alist) state))
        (ld-post-eval-print
         (f-put-global 'ld-post-eval-print (cdar alist) state))
        (ld-evisc-tuple
         (f-put-global 'ld-evisc-tuple (cdar alist) state))
        (ld-error-triples
         (f-put-global 'ld-error-triples (cdar alist) state))
        (ld-error-action
         (f-put-global 'ld-error-action (cdar alist) state))
        (ld-query-control-alist
         (f-put-global 'ld-query-control-alist (cdar alist) state))
        (ld-verbose
         (f-put-global 'ld-verbose (cdar alist) state))
        (otherwise
         (let ((x (er hard 'f-put-ld-specials
                      "Someone is using ~x0 as an unauthorized LD-special."
                      (caar alist))))
           (declare (ignore x))
           state)))
       (f-put-ld-specials (cdr alist) state)))))

(defun f-get-ld-specials (state)

; Make an alist, suitable for giving to f-put-ld-specials, that records the
; current values of all LD-specials.  To add a new LD-special you must
; change this function, f-put-ld-specials, and the checker
; chk-acceptable-ld-fn1-pair.

  (list (cons 'standard-oi
              (f-get-global 'standard-oi state))
        (cons 'standard-co
              (f-get-global 'standard-co state))
        (cons 'proofs-co
              (f-get-global 'proofs-co state))
        (cons 'current-package
              (f-get-global 'current-package state))
        (cons 'ld-skip-proofsp
              (f-get-global 'ld-skip-proofsp state))
        (cons 'ld-redefinition-action
              (f-get-global 'ld-redefinition-action state))
        (cons 'ld-prompt
              (f-get-global 'ld-prompt state))
        (cons 'ld-keyword-aliases
              (f-get-global 'ld-keyword-aliases state))
        (cons 'ld-pre-eval-filter
              (f-get-global 'ld-pre-eval-filter state))
        (cons 'ld-pre-eval-print
              (f-get-global 'ld-pre-eval-print state))
        (cons 'ld-post-eval-print
              (f-get-global 'ld-post-eval-print state))
        (cons 'ld-evisc-tuple
              (f-get-global 'ld-evisc-tuple state))
        (cons 'ld-error-triples
              (f-get-global 'ld-error-triples state))
        (cons 'ld-error-action
              (f-get-global 'ld-error-action state))
        (cons 'ld-query-control-alist
              (f-get-global 'ld-query-control-alist state))
        (cons 'ld-verbose
              (f-get-global 'ld-verbose state))))

(defun ld-read-keyword-command1 (n state)
  (cond
   ((= n 0) (value nil))
   (t (mv-let (eofp obj state)
              (read-standard-oi state)
              (cond
               (eofp (er soft 'ld-read-keyword-command
                         "Unfinished keyword command at eof on (standard-oi ~
                          state)."))
               (t
                (er-let*
                 ((rst (ld-read-keyword-command1 (1- n) state)))

; Note: We take advantage of the fact that this function ALWAYS returns a list
; of quoted objects.  See the call of strip-cadrs in ld-read-keyword-command
; below.  So if you optmize away some of the quotes, beware!

                 (value (cons (list 'quote obj) rst)))))))))

(defun exit-ld (state)

; This is the function most commonly aliased to the keyword command :q.  Its
; evaluation causes LD to terminate immediately.  Any function that returns
; three results, the first of which is nil, the second of which is :q and the
; third of which is STATE will do the same.

  (value :q))

(defun ld-read-keyword-command (key state)

; ld supports the convention that when a keyword :key is typed
; as a command and the corresponding symbol in the "ACL2" package,
; ACL2::key is a function or macro of arity n, we read n more
; objects, quote them, and apply the ACL2 function or macro.
; Thus, 

; MY-PKG !>:ubt foo

; is the same thing as

; MY-PKG !>(ACL2::UBT 'foo)

; We require that the macro not have any lambda keyword arguments, since
; that makes it hard or impossible to determine how many things we should
; read.  

; We also support the convention that if :key is bound on 'ld-keyword-aliases
; in state, say in the entry (:key n fn), we manufacture (fn 'x1 ...  'xn)
; instead of requiring that key be a function and returning (key 'x1 ...  'xn).

; This function returns four results, (mv erp keyp form state).  If erp is t an
; error was caused and the message has been printed.  Otherwise, keyp is
; non-nil or nil according to whether the keyword hack was involved.  Form is
; the parsed form of the command read, e.g., (acl2::ubt 'foo).  If non-nil,
; keyp is the actual list of objects read, e.g., (:ubt foo).

  (let ((temp (assoc-eq key
                        (f-get-global 'ld-keyword-aliases state))))
    (cond
     (temp
      (mv-let (erp args state)
              (ld-read-keyword-command1 (cadr temp) state)
              (cond
               (erp (mv t nil nil state))
               (t (mv nil (cons key (strip-cadrs args)) (cons (caddr temp) args) state)))))
     ((eq key :q)

; Here is the only place we recognize :q as a special command.  Essentially :q is an
; alias for (exit-ld state) except it is overridden by any other aliases for :q.

      (mv nil '(:q) '(exit-ld state) state))
     (t
      (let ((sym (intern (symbol-name key) "ACL2"))
            (wrld (w state)))
        (cond
         ((function-symbolp sym wrld)
          (mv-let (erp args state)
                  (ld-read-keyword-command1 (length (formals sym wrld))
                                            state)
                  (cond (erp (mv t nil nil state))
                        (t (mv nil (cons key (strip-cadrs args)) (cons sym args) state)))))
         ((getprop sym 'macro-body nil 'current-acl2-world wrld)
          (cond ((no-lambda-keywordsp
                  (getprop sym 'macro-args
                           '(:error "See LD-READ-KEYWORD-COMMAND.")
                           'current-acl2-world wrld))
                 (mv-let (erp args state)
                         (ld-read-keyword-command1
                          (length
                           (getprop sym 'macro-args
                                    '(:error "See LD-READ-KEYWORD-COMMAND.")
                                    'current-acl2-world wrld))
                          state)
                         (cond (erp (mv t nil nil state))
                               (t (mv nil (cons key (strip-cadrs args)) (cons sym args) state)))))
                (t (mv-let (erp val state)
                           (er soft 'LD
                               "It is illegal to attempt to use the ACL2 ~
                                keyword command hack to invoke ~x0 because its ~
                                formals contain lambda keywords and we can't ~
                                easily determine how many objects to read and ~
                                feed the thing.  See :DOC keyword-commands."
                               sym)
                           (declare (ignore erp val))
                           (mv t nil nil state)))))
         (t (mv-let (erp val state)
                    (er soft 'LD
                        "Unrecognized keyword command ~x0."
                        key)
                    (declare (ignore erp val))
                    (mv t nil nil state)))))))))

(defun ld-read-command (state)

; This function reads an ld command from the standard-oi channel of state
; and returns it.  It implements the keyword command hack.  We return five
; results: (mv eofp erp keyp form state).  Eofp means we exhausted
; standard-oi.  Erp, when t, indicates that an error occurred, e.g., an
; ill-formed keyword command was read.  The error message has been printed.
; Keyp, when non-nil, indicates that form is the parsed form of a keyword command.
; The list of objects actually read is the non-nil value of keyp and that list, without
; the enclosing parentheses, should be printed instead of form.  Thus, if :kons
; is an alias for cons, then :kons x y will parse into (cons 'x 'y) and keyp will
; be (:kons x y).

  (mv-let (eofp val state)
          (read-standard-oi state)
          (cond (eofp (mv t nil nil nil state))
                ((keywordp val)
                 (mv-let (erp keyp form state)
                         (ld-read-keyword-command val state)
                         (mv nil erp keyp form state)))
                ((stringp val)
                 (let ((upval (string-upcase val)))
                   (cond ((find-non-hidden-package-entry
                           upval
                           (global-val 'known-package-alist (w state)))
                          (mv nil nil nil `(in-package ,upval) state))
                         (t (mv nil nil nil val state)))))
                (t (mv nil nil nil val state)))))

(deflabel acl2-customization
  :doc
  ":Doc-Section Miscellaneous

  file of initial commands for ACL2 to run at ~il[startup]~/

  The file ~c[\"acl2-customization.lisp\"] is automatically loaded, via
  ~ilc[ld], the first time ~ilc[lp] is called in an ACL2 session, provided
  such a file exists on the current directory.  Except for the fact
  that this ~ilc[ld] command is not typed explicitly by you, it is a
  standard ~ilc[ld] command, with one exception:  any settings of ~ilc[ld]
  specials are remembered once this call of ~ilc[ld] has completed.  For
  example, suppose that you start your customization file with
  ~c[(set-ld-skip-proofsp t state)], so that proofs are skipped as it is
  loaded with ~ilc[ld].  Then the ~ilc[ld] special ~ilc[ld-skip-proofsp] will remain ~c[t]
  after the ~ilc[ld] has completed, causing proofs to be skipped in your
  ACL2 session, unless your customization file sets this variable back
  to ~c[nil], say with ~c[(set-ld-skip-proofsp nil state)].~/

  The customization file ~c[\"acl2-customization.lisp\"] actually
  resides on the connected book directory; ~pl[cbd].  Except, if
  that file does not exist, then ACL2 looks for
  ~c[\"acl2-customization.lisp\"] on your home directory.  If ACL2 does
  not find that file either, then no customization occurs and ~ilc[lp]
  enters the standard ACL2 read-eval-print loop.

  If the customization file exists, it is loaded with ~ilc[ld] using the
  usual default values for the ~ilc[ld] specials (~pl[ld]).  Thus, if an
  error is encountered, no subsequent forms in the file will be
  evaluated.

  To create a customization file it is recommended that you first give
  it a name other than ~c[\"acl2-customization.lisp\"] so that ACL2 does
  not try to include it prematurely when you next enter ~ilc[lp].  Then,
  while in the uncustomized ~ilc[lp], explicitly invoke ~ilc[ld] on your evolving
  (but renamed) customization file until all forms are successfully
  evaluated.  The same procedure is recommended if for some reason
  ACL2 cannot successfully evaluate all forms in your customization
  file:  rename your customization file so that ACL2 does not try to
  ~ilc[ld] it automatically and then debug the new file by explicit calls to
  ~ilc[ld].

  When you have created a file that can be loaded with ~ilc[ld] without
  error and that you wish to be your customization file, name it
  ~c[\"acl2-customization.lisp\"] and put it on the current directory or
  in your home directory.  The first time after starting up ACL2 that
  you invoke ~c[(lp)], ACL2 will automatically load the
  ~c[\"acl2-customization.lisp\"] file from the cbd (~pl[cbd]) if
  there is one, and otherwise will load it from your home directory.

  Note that if you certify a book after the (automatic) loading of an
  ~c[acl2-customization] file, the forms in that file ~st[will be part of the]
  ~st[portcullis] of the ~il[books] you certify!  That is, the forms in your
  customization file at certification time will be loaded whenever
  anybody uses the ~il[books] you are certifying.  Since customization
  files generally contain idiosyncratic ~il[command]s, you may not want
  yours to be part of the ~il[books] you create for others.  Thus, if you
  have a customization file then you may want to invoke ~c[:ubt 1] before
  certifying any ~il[books].

  The conventions concerning ACL2 customization are liable to change
  as we get more experience with the interaction between
  customization, certification of ~il[books] for others, and routine
  undoing.  For example, at the moment it is regarded as a ~st[feature] of
  customization that it can be undone but it might be regarded as a
  bug if you accidentally undo your customization.~/

  :cited-by Programming")

(deflabel keyword-commands
  :doc
  ":Doc-Section Miscellaneous

  how keyword commands are processed~/
  ~bv[]
  Examples:
  user type-in                 form evaluated
  :pc 5                        (ACL2::PC '5)
  :pcs app rev                 (ACL2::PCS 'app 'rev)
  :length (1 2 3)              (ACL2::LENGTH '(1 2 3))
  ~ev[]~/

  When a keyword, ~c[:key], is read as a command, ACL2 determines whether
  the symbol with the same name in the ~c[\"ACL2\"] package, ~c[acl2::key], is
  a function or simple macro of n arguments.  If so, ACL2 reads ~c[n] more
  objects, ~c[obj1], ..., ~c[objn], and then acts as though it had read the
  following form (for a given ~c[key]):
  ~bv[]
  (ACL2::key 'obj1 ... 'objn)
  ~ev[]
  Thus, by using the keyword command hack you avoid typing the
  parentheses, the ~c[\"ACL2\"] package name, and the quotation marks.

  Note the generality of this hack.  Almost any function or macro in
  the ~c[\"ACL2\"] package can be so invoked, not just ``commands.''
  Indeed, there is no such thing as a distinguished class of commands.
  The one caveat is that the keyword hack can be used to invoke a
  macro only if that macro has a simple argument list ~-[] one
  containing no lambda keywords (such as ~c[&rest]), since they complicate
  or render impossible the task of deciding how many objects to read.
  Users may take advantage of the keyword command hack by defining
  functions and macros in the ~c[\"ACL2\"] package.~/")

(defun ld-print-command (keyp form col state)
  (with-base-10
   (mv-let (col state)
     (cond
      ((not (eq (ld-pre-eval-print state) t)) (mv col state))
      (keyp
       (fmt1 "~*0~|"
             (list (cons #\0 (list "" "~x*" "~x* " "~x* " keyp)))
             col
             (standard-co state)
             state
             (ld-evisc-tuple state)))
      (t
       (fmt1 "~q0~|"
             (list (cons #\0 form))
             col
             (standard-co state)
             state
             (ld-evisc-tuple state))))
     (declare (ignore col))
     state)))

(defun ld-filter-command (form state)
  (let ((filter (ld-pre-eval-filter state)))
    (cond ((eq filter :all) (value t))
          ((eq filter :query)
           (acl2-query :filter
                       '("~#0~[~Y12?~/Eval?~]"
                         :y t :n nil :r :return :q :error
                         :? ("We are in the LD read-eval-print loop, ~
                              processing the forms in standard-oi.  The ~
                              form printed above is one of those forms.  Do ~
                              you want to evaluate it (Y) or not (N)?   You ~
                              may also answer R, meaning ``return ~
                              immediately from LD (without reading or ~
                              evaluating any more forms)'' or Q meaning ~
                              ``return immediately from LD, signalling an ~
                              error.''"
                             :y t :n nil :r :return :q :error))
                       (list (cons #\0 (if (eq (ld-pre-eval-print state) t) 1 0))
                             (cons #\1 form)
                             (cons #\2 (ld-evisc-tuple state)))
                       state))
          (t (value t)))))

#-acl2-loop-only
(defun-one-output ppr? (x raw-x col channel state)
  (cond
   ((and (raw-mode-p state)
         (bad-lisp-objectp x nil))
    (if (not (eq channel *standard-co*))
        (error "Attempted to print LD results to other than *standard-co*!"))
    (format t "[Note:  Printing non-ACL2 result.]")
    (terpri)
    (prin1 raw-x)
    state)
   (t
    (ppr x col channel state t))))

(defun ld-print-results (trans-ans state)

; This is the function used by ld to print the results of the
; trans-evaluation of the form read.  Trans-ans is of the form
; (stobjs-out . valx).

; If ld-post-eval-print is nil we print nothing.  If it is t, we
; print with the standard evisceration (ld-evisc-tuple).  If it is
; :command-conventions, we hide error/value/state pairs by just printing
; value and we don't print anyting when the value is :invisible.

  (let* ((output-channel (standard-co state))
         (flg (ld-post-eval-print state))
         (stobjs-out (car trans-ans))
         (valx (cdr trans-ans))
         (evisc-tuple (ld-evisc-tuple state))
         (evisc-alist (world-evisceration-alist state (car evisc-tuple)))
         (print-level (cadr evisc-tuple))
         (print-length (caddr evisc-tuple))
         (eviscerated-valx
          (eviscerate-stobjs (evisceration-stobj-marks stobjs-out nil)
                             valx
                             print-level print-length evisc-alist
                             nil)))

; In raw mode in Allegro Common Lisp (and not GCL, but perhaps other lisps),
; evaluation of (time ...) causes the result value to be printed at the end of
; a comment line printed by time, which is unfortunate.  This sort of printing
; problem does not seem to have come up in other than raw mode, and besides, we
; do not want to try to model this sort of maybe-newline printing in the
; logic.  So we restrict this solution to raw mode.  Furthermore, the lisps
; listed below do not need this fix, and they all print a newline even with
; "~&" when apparently not necessary, so we exclude them from this fix.

    #-(or acl2-loop-only gcl cmu sbcl lispworks)
    (when (raw-mode-p state)
      (format (get-output-stream-from-channel output-channel) "~&"))
    (cond
     ((null flg) state)
     ((and (eq flg :command-conventions)
           (equal (length stobjs-out) 3)
           (eq (car stobjs-out) nil)
           (eq (caddr stobjs-out) 'state))

; We get here if we are following command-conventions and the form
; returned triple (mv erp val state).  Note that erp must be a
; non-stobj (typically a Boolean) but that val may be a stobj or not.

      (cond
       ((eq (cadr valx) :invisible)
        state)
       (t
        (pprogn
         (princ$ (if (stringp (f-get-global 'triple-print-prefix state))
                     (f-get-global 'triple-print-prefix state)
                     "")
                 output-channel state)

; The following raw code is identical to the logic code below except that the
; raw code handles infix printing, which is, at the moment, entirely
; extra-logical.

         #-acl2-loop-only
         (let ((col (if (stringp (f-get-global 'triple-print-prefix state))
                        (length (f-get-global 'triple-print-prefix state))
                      0))
               (evg (cadr eviscerated-valx)))
           (cond
            ((and (live-state-p state)
                  (output-in-infixp state))
             (print-infix
              evg
              nil
              (- (fmt-hard-right-margin state) col)
              0 col
              (get-output-stream-from-channel output-channel)
              t)
             *the-live-state*)
            (t (ppr? evg (cadr valx) col output-channel state))))
         #+acl2-loop-only
         (ppr (cadr eviscerated-valx)
              (if (stringp (f-get-global 'triple-print-prefix state))
                  (length (f-get-global 'triple-print-prefix state))
                0)
              output-channel state t)
         (newline output-channel state)))))   
     (t (pprogn
         #-acl2-loop-only
         (cond
          ((and (live-state-p state)
                (output-in-infixp state))
           (print-infix
            eviscerated-valx
            nil
            (fmt-hard-right-margin state)
            0 0
            (get-output-stream-from-channel output-channel)
            t)
           *the-live-state*)
          (t (ppr? eviscerated-valx valx 0 output-channel state)))
         #+acl2-loop-only
         (ppr eviscerated-valx 0 output-channel state t)
         (newline output-channel state))))))

(defun ld-print-prompt (state)

; Like print-prompt except may print the prompt both to *standard-co*
; and (standard-co state).

  (mv-let (col state)
          (print-prompt (ld-prompt state) (standard-co state) state)
          (cond
           ((and (eq (standard-oi state) *standard-oi*)
                 (not (eq (standard-co state) *standard-co*)))
            (mv-let (irrel-col state)
                    (print-prompt (ld-prompt state) *standard-co* state)
                    (declare (ignore irrel-col))
                    (mv col state)))
           (t (mv col state)))))

(defun print-newline-for-time$ ()
  #+(and (not acl2-loop-only) (or allegro clisp))
  (when *time-call-seen*

; The time utilities in Allegro and CLISP leave us without a newline where we
; need one, when printing a top-level result that has been timed.

    (terpri *trace-output*))
  nil)

(defun good-bye-fn (cmd)
  (declare (xargs :mode :program))
  #+acl2-loop-only
  (declare (ignore cmd))
  #-acl2-loop-only
  (progn #-(or clisp openmcl lispworks sbcl)

; We rely on exit-lisp to print the appropriate message on how to exit lisp if
; we are in CLISP or OpenMCL.

         (format t
                 "Attempting to exit Lisp.  If this fails to work, type :q ~
                  and then try again.~%")
         (exit-lisp cmd))
  nil)

(defmacro good-bye ()

  ":Doc-Section Other

  quit entirely out of Lisp~/
  ~bv[]
  Example:
  ACL2 !>:good-bye
  ~ev[]
  ~st[Note:  Your entire session will disappear forever when you type]
  ~c[:good-bye].~/

  The command ~c[:good-bye] quits not only out of the ACL2 ~il[command] loop,
  but in fact quits entirely out of the underlying Lisp.  Thus, there
  is no going back!  You will ~st[not] be able to re-enter the ~il[command] loop
  after typing ~c[:good-bye]!  All your work will be lost!!!

  This command may not work in some underlying Common Lisp
  implementations.  But we don't expect there to be any harm in
  trying.  It ~st[does] work in GCL and Allegro CL, at least as of
  this writing.

  In some systems, typing ~c[control-d] at the top-level ACL2 prompt
  (~c[control-c control-d] if inside emacs) will call this function.

  If you merely want to exit the ACL2 ~il[command] loop, use ~c[:]~ilc[q] instead
  (~pl[q])."

  '(good-bye-fn "(GOOD-BYE)"))

(defun ld-read-eval-print (state)

; This is LD's read-eval-print step.  We read a form from standard-oi, eval it,
; and print the result to standard-co, will lots of bells and whistles
; controlled by the various LD specials.  The result of this function is a
; triple (mv signal val state), where signal is one of :CONTINUE, :RETURN, or
; :ERROR.  When the signal is :continue or :error, val is irrelevant.  When the
; signal is :return, val is the "reason" we are terminating and is one of
; :exit, :eof, :error, or :filter.

  (mv-let
   (col state)
   (ld-print-prompt state)
   (mv-let
    (eofp erp keyp form state)
    (ld-read-command state)
    (cond
     (eofp (cond ((ld-prompt state)
                  (pprogn (princ$ "Bye." (standard-co state) state)
                          (newline (standard-co state) state)

; In versions before v2-8, typing ctrl-d (ctrl-c ctrl-d in Emacs) did not immediately
; kill the Lisp if the resulting eof condition was detected by BRR processing.  The
; code below fixes that; let's hope it doesn't "fix" anything else!

                          (prog2$ (and (equal (standard-oi state) *standard-oi*)
                                       (good-bye))
                                  state)
                          (mv :return :eof state)))
                 (t (mv :return :eof state))))
     (erp (mv (ld-error-action state) :error state))
     (t (pprogn
         (ld-print-command keyp form col state)
         (mv-let
          (erp ans state)
          (ld-filter-command form state)
          (cond
           (erp (mv (ld-error-action state) :error state))
           ((null ans) (mv :continue nil state))
           ((eq ans :error) (mv :error nil state))
           ((eq ans :return) (mv :return :filter state))
           (t (pprogn
               (initialize-timers state)
               (f-put-global 'accumulated-warnings nil state)
               (f-put-global 'last-make-event-expansion nil state)
               (let* ((old-wrld (w state))
                      (old-default-defun-mode
                       (default-defun-mode old-wrld)))
                 (mv-let
                  (error-flg trans-ans state)
                  (revert-world-on-error
                   (mv-let (error-flg trans-ans state)
                           (if (raw-mode-p state)
                               (acl2-raw-eval form state)
                             (trans-eval form 'top-level state))

; If error-flg is non-nil, trans-ans is (stobjs-out . valx).

                           (cond
                            (error-flg (mv t nil state))
                            ((and (ld-error-triples state)
                                  (equal (car trans-ans) '(nil nil state))
                                  (car (cdr trans-ans)))
                             (mv t nil state))
                            (t (er-progn
                                (get-and-chk-last-make-event-expansion
                                 form (w state) 'top-level state
                                 *primitive-event-macros*)
                                (mv nil trans-ans state))))))

; If error-flg is non-nil, trans-ans is (stobjs-out . valx) and we know
; that valx is not an erroneous error triple if we're paying attention to
; error triples.

; The code inside the revert-world-on-error arranges to revert if either
; trans-eval returns an error, or the value is to be thought of as an
; error triple and it signals an error.  Error-flg, now, is set to t
; iff we reverted.

                  (prog2$
                   (print-newline-for-time$)
                   (cond
                    (error-flg
                     (mv (ld-error-action state) :error state))
                    ((and (equal (car trans-ans) '(nil nil state))
                          (eq (cadr (cdr trans-ans)) :q))
                     (mv :return :exit state))
                    (t (pprogn
                        (maybe-add-command-landmark
                         old-wrld
                         old-default-defun-mode
                         form
                         (f-get-global 'last-make-event-expansion state)
                         trans-ans state)
                        (ld-print-results trans-ans state)

; We make the convention of checking the new-namep filter immediately after
; we have successfully eval'd a form (rather than waiting for the next form)
; so that if the user has set the filter up he gets a satisfyingly
; immediate response when he introduces the name.

                        (let ((filter (ld-pre-eval-filter state)))
                          (cond
                           ((and (not (eq filter :all))
                                 (not (eq filter :query))
                                 (not (new-namep filter
                                                 (w state))))
                            (er-progn

; We reset the filter to :all even though we are about to exit this LD
; with :return.  This just makes things work if "this LD" is the top-level
; one and LP immediately reenters.

                             (set-ld-pre-eval-filter :all state)
                             (mv :return :filter state)))
                           (t (mv :continue nil state))))))))))))))))))))

(defun ld-loop (state)

; Note: We use a bit of raw lisp to ensure that the ACL2 unwind protect stack
; is properly configured before we execute the prompt for the next command.
; This acl2-unwind can be exercised, we think, by evaluating LD recursively
; and aborting the inferior LD so that it fails to cleanup after itself.

  (mv-let
   (signal val state)
   #+acl2-loop-only (ld-read-eval-print state)
   #-acl2-loop-only (progn (acl2-unwind *ld-level* t)
                           #+(or allegro openmcl)
                           (when (<= *ld-level* 1)
                             (setq *trace-level* 0))
                           #+(or allegro clisp) (setq *time-call-seen* nil)
                           (ld-read-eval-print state))
   (cond ((eq signal :continue)
          (ld-loop state))
         ((eq signal :return)
          (value val))
         (t (mv t nil state)))))

; The following raw lisp special variable controls whether the raw lisp version
; of ld-fn-body, below, prints the header as per ld-verbose or does not.  The
; handling of aborts in ld-fn forces us to call ld-fn-body again after each
; abort and we wish to suppress the header message after all entrances other
; than the first.  This only happens after an abort (all bets are off) and the
; idea is to fool the user into thinking a normal error was signalled.

#-acl2-loop-only
(defvar *first-entry-to-ld-fn-body-flg*)

(defun update-cbd (standard-oi0 state)
  (let ((old-cbd (f-get-global 'connected-book-directory state)))
    (if (and old-cbd (stringp standard-oi0))
        (let ((dir (maybe-add-separator
                    (remove-after-last-directory-separator
                     (extend-pathname old-cbd standard-oi0 (os (w state)))))))
          (f-put-global 'connected-book-directory dir state))
      state)))

(defun ld-fn-body (standard-oi0 new-ld-specials-alist state)

; This function is defined only to make it convenient for ld-fn to execute its
; "body" either inside or outside an acl2-unwind-protect.

; WARNING: Because of the hidden acl2-unwind in the raw code for ld-loop above
; do not try to use acl2-unwind-protect in this function.  The cleanup form for
; it will be executed before the first form is read because ld-loop rolls back
; to the initialized version of the frame.  Furthermore, do not execute
; non-idempotent state changing forms here, i.e., incrementing or decrementing
; some counter in state, because the abort handling may cause this body to be
; reentered after an abort while the logical semantics suggests that it is
; entered only once.  (Of course, aborts mean all bets are off, but the idea is
; to make it seem like they are errors.)  We once incremented and decremented
; ld-level here and found the load level going down every time an abort
; occurred (because its increment was undo by the hidden acl2-unwind in
; ld-loop, mentioned above, and it was decremented every abort).

  (pprogn
    (f-put-ld-specials new-ld-specials-alist state)
    (update-cbd standard-oi0 state)
    (cond (#+acl2-loop-only (ld-verbose state)
           #-acl2-loop-only (and *first-entry-to-ld-fn-body-flg*
                                  (ld-verbose state))

; We print the file name rather than the channel.

           (cond
            ((eq (ld-verbose state) t)
             (fms (if (eq standard-oi0 *standard-oi*)
                      "ACL2 loading *standard-oi*.~%"
                      "ACL2 loading ~x0.~%")
                  (list (cons #\0 (cond ((consp standard-oi0) (kwote standard-oi0))
                                        (t standard-oi0))))
                  (standard-co state)
                  state
                  (ld-evisc-tuple state)))
            (t (with-base-10
                (fms
                 "~@0"
                 (list (cons #\0 (ld-verbose state))
                       (cons #\v (f-get-global 'acl2-version state))
                       (cons #\l (f-get-global 'ld-level state))
                       (cons #\c (f-get-global 'connected-book-directory state)))
                 (standard-co state)
                 state
                 (ld-evisc-tuple state))))))
          (t state))
    (mv-let
     (erp val state)
     (ld-loop state)
     (pprogn
      (cond ((eq (ld-verbose state) t)
             (fms (if (eq standard-oi0 *standard-oi*)
                      "Finished loading *standard-oi*.~%"
                      "Finished loading ~x0.~%")
                  (list (cons #\0 (cond ((consp standard-oi0) (kwote standard-oi0))
                                        (t standard-oi0))))
                  (standard-co state)
                  state
                  (ld-evisc-tuple state)))
            (t state))
      (mv erp val state)))))

#-acl2-loop-only
(defvar *acl2-panic-exit-flg*)

(defun ld-fn1 (standard-oi0 alist state bind-flg)

; If this function weren't defined we would have to duplicate its body twice in
; ld-fn, once in the #+acl2-loop-only section and again in the
; #-acl2-loop-only section in the case where the state is not the live state.
; The reason we grab the old ld-level and use it in the cleanup form rather
; than just decrementing the then current value is that we do not know how many
; times the cleanup form will be tried before it is not interrupted.

  (let* ((old-ld-level (f-get-global 'ld-level state))
         (new-ld-level (1+ old-ld-level))
         (old-cbd (f-get-global 'connected-book-directory state)))
    (er-let*
     ((pair (chk-acceptable-ld-fn alist state)))
     (let ((old-ld-specials-alist (f-get-ld-specials state))
           (new-ld-specials-alist (car pair))
           (channel-closing-alist (cdr pair)))
       (if bind-flg
           (acl2-unwind-protect
            "ld-fn"
            (pprogn
             (f-put-global 'ld-level new-ld-level state)
             (ld-fn-body standard-oi0 new-ld-specials-alist state))
            (pprogn
             (f-put-global 'ld-level old-ld-level state)
             (f-put-global 'connected-book-directory old-cbd state)
             (f-put-ld-specials old-ld-specials-alist state)
             (close-channels channel-closing-alist state))
            (pprogn
             (f-put-global 'ld-level old-ld-level state)
             (f-put-global 'connected-book-directory old-cbd state)
             (f-put-ld-specials old-ld-specials-alist state)
             (close-channels channel-closing-alist state)))
         (acl2-unwind-protect
          "ld-fn"
          (pprogn (f-put-global 'ld-level new-ld-level state)
                  (ld-fn-body standard-oi0 new-ld-specials-alist state))
          (pprogn (f-put-global 'ld-level old-ld-level state)
                  (f-put-global 'connected-book-directory old-cbd state))
          (pprogn (f-put-global 'ld-level old-ld-level state)
                  (f-put-global 'connected-book-directory old-cbd state))))))))

(defun ld-fn-alist (alist state)
  (let ((standard-oi (cdr (assoc 'standard-oi alist)))
        (dir         (cdr (assoc 'dir alist)))
        (ctx         'ld))
    (cond ((and (stringp standard-oi)
                dir)
           (cond ((absolute-pathname-string-p standard-oi nil (os (w state)))
                  (er hard ctx
                      "It is illegal to supply a :DIR argument to LD here ~
                       because the supplied filename,~|~%  ~s0,~|~%is an ~
                       absolute pathname (see :DOC pathname), and hence there ~
                       is no reasonable way to merge it with a :DIR value."
                      standard-oi))
                 (t
                  (let ((resolve-dir
                         (include-book-dir-with-chk hard 'ld dir)))
                    (cond (resolve-dir
                           (put-assoc-eq 'standard-oi
                                         (our-merge-pathnames resolve-dir
                                                              standard-oi)
                                         (delete-assoc-eq 'dir alist)))
                          (t alist))))))
          (t alist))))

(defun ld-fn (alist state bind-flg)

; We set the ld specials to the values specified in alist and then enter the
; standard ACL2 read-eval-print loop.  If bind-flg is t then the ld specials
; are restored to their pre-call values upon exit or abort.  Otherwise they are
; not.  Another interpretation of the flag is: if bind-flg is t then the load
; specials are merely "bound" locally to the values in alist, otherwise, they
; are globally smashed to values in alist.  If this call is considered the
; "top-level" call of ld-fn, bind-flg ought to be nil: the final values of the
; load specials established during the interaction survive exiting to raw lisp
; and are present when ld-fn is reentered later.  If this call is not
; "top-level" then the values established during interaction are lost on exit.

; Advice: It is best to read this function as though ld-fn1's body were
; substituted below.  Ld-fn1 is just a way to avoid duplication of code and has
; nothing to do with the unwind protection we are really implementing.

  (let ((alist (ld-fn-alist alist state)))

    #+acl2-loop-only
    (ld-fn1 (cdr (assoc-eq 'standard-oi alist)) alist state bind-flg)

; The part in UPPERCASE below is raw lisp that manages the unwind stack and
; *ld-level*.  The part in lowercase is identical to the pure ACL2 in ld-fn1
; above.  It is helpful to split the buffer, put the pure ACL2 in the top
; window and read what follows in the bottom one.  Observe that if the state is
; not live, we just use the pure ACL2.  So start with the PROGN below.

    #-acl2-loop-only
    (COND
     ((LIVE-STATE-P STATE)
      (PROGN
       (ACL2-UNWIND *LD-LEVEL* NIL)
       (PUSH NIL *ACL2-UNWIND-PROTECT-STACK*)
       (LET* ((*LD-LEVEL* (1+ *LD-LEVEL*))
              (*READTABLE* *ACL2-READTABLE*)
              #+(OR ALLEGRO CLISP) (*TIME-CALL-SEEN* NIL)
              (*ACL2-PANIC-EXIT-FLG* NIL)
              (*FIRST-ENTRY-TO-LD-FN-BODY-FLG* T)
              (ABORT-OBJ (CONS 'ABORT NIL))
              (THROWN-VAL NIL)
              (LD-ERP ABORT-OBJ)
              (LD-VAL NIL)) ; below implies an abort happened
             (let* ((old-ld-level (f-get-global 'ld-level state))
                    (new-ld-level (1+ old-ld-level))
                    (old-cbd (f-get-global 'connected-book-directory state)))
               (MV-LET
                (ERP pair STATE)
                (chk-acceptable-ld-fn alist state)
                (COND
                 (ERP (ACL2-UNWIND (1- *LD-LEVEL*) NIL) (MV ERP PAIR STATE))
                 (T
                  (let ((old-ld-specials-alist (f-get-ld-specials state))
                        (new-ld-specials-alist (car pair))
                        (channel-closing-alist (cdr pair)))
                    (PUSH-CAR
                     (CONS "ld-fn"
                           (IF bind-flg
                               (FUNCTION
                                (LAMBDA
                                 NIL
                                 (pprogn
                                  (f-put-global 'ld-level old-ld-level state)
                                  (f-put-global 'connected-book-directory
                                                old-cbd state)
                                  (f-put-ld-specials old-ld-specials-alist
                                                     state)
                                  (close-channels channel-closing-alist
                                                  state))))
                               (FUNCTION
                                (LAMBDA
                                 NIL
                                 (pprogn
                                  (f-put-global 'ld-level old-ld-level state)
                                  (f-put-global 'connected-book-directory
                                                old-cbd state))))))
                     *ACL2-UNWIND-PROTECT-STACK*
                     'LD-FN)
                    (TAGBODY
                     LOOP
                     (UNWIND-PROTECT
                      (pprogn (f-put-global 'ld-level new-ld-level state)
                              (SETQ THROWN-VAL
                                    (CATCH
                                     'LOCAL-TOP-LEVEL
                                     (MV-LET
                                      (ERP VAL STATE)
                                      (ld-fn-body (cdr (assoc-eq 'standard-oi
                                                                 alist))
                                                  new-ld-specials-alist state)
                                      (f-put-global 'connected-book-directory
                                                    old-cbd
                                                    state)
                                      (SETQ LD-ERP ERP)
                                      (SETQ LD-VAL VAL)
                                      NIL))))
                      (COND
                       (*ACL2-PANIC-EXIT-FLG*
                        (RETURN-FROM LD-FN (MV T NIL STATE)))
                       ((EQ LD-ERP ABORT-OBJ)

; We get here if the ld-fn-body failed to terminate normally.  This can happen
; either because lisp caused some error or because we threw to the tag above.
; If we threw to the tag then LD-ERP is ABORT-OBJ (because we didn't get to
; the SETQ above) and THROW-VAL is whatever we threw.  If we did not throw,
; then THROWN-VAL is NIL (because the lisp error prevented us from doing the
; SETQ THROWN-VAL).  We make the convention that we always throw non-nil
; values to the tag so as to distinguish these two cases.

                        #+akcl (si::RESET-STACK-LIMITS)
                        (WHEN (AND (OR (= *LD-LEVEL* 1)
                                       (NULL THROWN-VAL))
                                   (OR (F-GET-GLOBAL 'IN-PROVE-FLG STATE)
                                       (F-GET-GLOBAL 'IN-VERIFY-FLG STATE)))
                              (CW
                               "Here is the current pstack [see :DOC pstack]:")
                              (PSTACK))
                        (COND ((EQ THROWN-VAL :ABORT)

; THROWN-VAL is always either NIL (meaning no throw occurred) or else the
; "reason" we threw.  Currently the possibilities are :ABORT (thrown when the
; user types #.) or :WORMHOLE-ER (thrown when we tried to make a non-undoable
; change to state while in a wormhole).  We only care about :ABORT.
; :WORMHOLE-ER is treated as a "normal" lisp error, i.e., we just unwind back
; to here and continue at this level.  :ABORT means we are to exit all the way
; back to *LD-LEVEL* 1.

                               (COND ((= *LD-LEVEL* 1)

; At *LD-LEVEL* = 1 we know *standard-co* is *STANDARD-OUTPUT*.

                                      (PRINC "Abort to ACL2 top-level"
                                             *STANDARD-OUTPUT*)
                                      (TERPRI *STANDARD-OUTPUT*))
                                     (T 
                                      (THROW 'LOCAL-TOP-LEVEL :ABORT)))))
                        (ACL2-UNWIND *LD-LEVEL* T)
; We first unwind back to the current level so STANDARD-OI and LD-ERROR-ACTION
; are correctly set.
                        (COND ((EQ (LD-ERROR-ACTION STATE) :CONTINUE)
                               (SETQ *FIRST-ENTRY-TO-LD-FN-BODY-FLG*
                                     (COND ((EQ THROWN-VAL :ABORT) T)
                                           (T NIL)))
                               (SETQ NEW-LD-SPECIALS-ALIST NIL)
                               (SETQ THROWN-VAL NIL)
                               (GO LOOP))
                              ((EQ (LD-ERROR-ACTION STATE) :RETURN)
                               (ACL2-UNWIND (1- *LD-LEVEL*) NIL)
                               (RETURN-FROM LD-FN (VALUE :ERROR)))
                              (T (ACL2-UNWIND (1- *LD-LEVEL*) NIL)
                                 (RETURN-FROM LD-FN (MV T NIL STATE)))))
                       (T
                        (ACL2-UNWIND (1- *LD-LEVEL*) NIL)
                        (RETURN-FROM LD-FN
                                     (MV LD-ERP LD-VAL STATE))))))))))))))
     (T (ld-fn1 (cdr (assoc-eq 'standard-oi alist)) alist state bind-flg)))))

(defmacro ld (standard-oi
              &key
              dir
              (standard-co 'same standard-cop)
              (proofs-co 'same proofs-cop)
              (current-package 'same current-packagep)
              (ld-skip-proofsp 'same ld-skip-proofspp)
              (ld-redefinition-action 'same ld-redefinition-actionp)
              (ld-prompt 'same ld-promptp)
              (ld-keyword-aliases 'same ld-keyword-aliasesp)
              (ld-pre-eval-filter 'same ld-pre-eval-filterp)
              (ld-pre-eval-print 'same ld-pre-eval-printp)
              (ld-post-eval-print 'same ld-post-eval-printp)
              (ld-evisc-tuple 'same ld-evisc-tuplep)
              (ld-error-triples 'same ld-error-triplesp)
              (ld-error-action ':RETURN)
              (ld-query-control-alist 'same ld-query-control-alistp)
              (ld-verbose 'same ld-verbosep))

  ":Doc-Section Other

  the ACL2 read-eval-print loop, file loader, and ~il[command] processor~/
  ~bv[]
  Examples:
  (LD \"foo.lisp\")              ; read and evaluate each form in file
                               ; \"foo.lisp\", in order
  (LD \"foo.lisp\" :ld-pre-eval-print t)
                               ; as above, but print each form to standard
                               ; character output just before it is evaluated

  General Form:
  (LD standard-oi                  ; open obj in channel, stringp file name
                                   ; to open and close, or list of forms~/
                                   ; Optional keyword arguments:
      :dir                ...      ; use this add-include-book-dir directory
      :standard-co        ...      ; open char out or file to open and close
      :proofs-co          ...      ; open char out or file to open and close
      :current-package    ...      ; known package name
      :ld-skip-proofsp    ...      ; nil, 'include-book, or t
                                   ;   (~pl[ld-skip-proofsp])
      :ld-redefinition-action ...  ; nil or '(:a . :b)
      :ld-prompt          ...      ; nil, t, or some prompt printer fn
      :ld-keyword-aliases ...      ; an alist pairing keywords to parse info
      :ld-pre-eval-filter ...      ; :all, :query, or some new name
      :ld-pre-eval-print  ...      ; nil, t, or :never
      :ld-post-eval-print ...      ; nil, t, or :command-conventions
      :ld-evisc-tuple     ...      ; nil or '(alist nil nil level length)
      :ld-error-triples   ...      ; nil or t
      :ld-error-action    ...      ; :return (default), :continue or :error
      :ld-query-control-alist ...  ; alist supplying default responses
      :ld-verbose         ...)     ; nil or t
  ~ev[]

  ~c[Ld] is the top-level ACL2 read-eval-print loop.  (When you call ~ilc[lp], a
  little initialization is done in raw Common Lisp and then ~c[ld] is
  called.)  ~c[ld] is also a general-purpose ACL2 file loader and a
  ~il[command] interpreter.  ~c[Ld] is actually a macro that expands to a
  function call involving ~ilc[state].  ~c[Ld] returns an ``error/value/state''
  triple as explained below.

  The arguments to ~c[ld], except for ~c[:dir], all happen to be global
  variables in ~ilc[state].  For example, ~c[']~ilc[current-package] and
  ~c[']~ilc[ld-verbose] are global variables, which may be accessed via
  ~c[(@ current-package)] and ~c[(@ ld-verbose)].  When ~c[ld] is called, it
  ``binds'' these variables.  By ``binds'' we actually mean the variables are
  globally set but restored to their old values on exit.  Because ~c[ld]
  provides the illusion of ~il[state] global variables being bound, they are
  called ``~c[ld] specials'' (after the Lisp convention of calling a variable
  ``special'' if it is referenced freely after having been bound).

  Note that all arguments but the first are passed via keyword.  Any
  variable not explicitly given a value in a call retains its pre-call
  value, with the exception of ~c[:]~ilc[ld-error-action], which defaults to
  ~c[:return] if not explicitly specified.

  Just as an example to drive the point home: If ~ilc[current-package] is
  ~c[\"ACL2\"] and you typed
  ~bv[]
  (ld *standard-oi* :current-package \"MY-PKG\")
  ~ev[]
  you would find yourself in (an inner) read-eval-print loop in
  which the ~il[current-package] was ~c[\"MY-PKG\"].  You could operate
  there as long as you wished, changing the current package at will.
  But when you typed ~c[:]~ilc[q] you would return to the outer
  read-eval-print loop where the current package would still be
  ~c[\"ACL2\"].

  Roughly speaking, ~c[ld] repeatedly reads a form from ~ilc[standard-oi],
  evaluates it, and prints its result to ~ilc[standard-co].  It does this until
  the form evaluates to an error triple whose value component is ~c[:]~ilc[q]
  or until the input channel or list is emptied.  However, ~c[ld] has many
  bells and whistles controlled by the ~c[ld] specials.  Each such special is
  documented individually.  For example, see the documentation for
  ~ilc[standard-oi], ~ilc[current-package], ~ilc[ld-pre-eval-print], etc.

  A more precise description of ~c[ld] is as follows.  In the description below
  we use the ~c[ld] specials as variables, e.g., we say ``a form is read from
  ~ilc[standard-oi].''  By this usage we refer to the current value of the
  named ~il[state] global variable, e.g., we mean ``a form is read from the
  current value of ~c[']~ilc[standard-oi].'' This technicality has an important
  implication: If while interacting with ~c[ld] you change the value of one of
  the ~c[ld] specials, e.g., ~c[']~ilc[standard-oi], you will change the
  behavior of ~c[ld], e.g., subsequent input will be taken from the new value.

  Three ~c[ld] specials are treated as channels: ~ilc[standard-oi] is treated
  as an object input channel and is the source of forms evaluated by ~c[ld];
  ~ilc[standard-co] and ~ilc[proofs-co] are treated as character output
  channels and various flavors of output are printed to them.  However, the
  supplied values of these specials need not actually be channels; several
  special cases are recognized.

  If the supplied value of one of these is in fact an open channel of the
  appropriate type, that channel is used and is not closed by ~c[ld].  If the
  supplied value of one of these specials is a string, the string is treated as
  a file name in (essentially) Unix syntax (~pl[pathname]) and a channel of the
  appropriate type is opened to/from that file.  Any channel opened by ~c[ld]
  during the binding of the ~c[ld] specials is automatically closed by ~c[ld]
  upon termination.  If ~ilc[standard-co] and ~ilc[proofs-co] are equal
  strings, only one channel to that file is opened and is used for both.

  As a special convenience, when ~ilc[standard-oi] is a string and the ~c[:dir]
  argument is also provided, we look up ~c[:dir] in the table of directories
  maintained by ~ilc[add-include-book-dir], and prepend this directory to
  ~ilc[standard-oi] to create the filename.  (In this case, however, we require
  that ~c[standard-oi] is a relative pathname, not an absolute pathname.)  For
  example, one can write
  ~c[(ld \"arithmetic/top-with-meta.lisp\" :dir :system)] to ~c[ld] that
  particular system library.  (Of course, you should almost always load books
  like ~c[arithmetic/top-with-meta] using ~ilc[include-book] instead of
  ~c[ld].)  If ~c[:dir] is not specified, then a relative pathname is resolved
  using the connected book directory; ~pl[cbd].

  Several other alternatives are allowed for ~ilc[standard-oi].  If
  ~ilc[standard-oi] is a true list then it is taken as the list of forms to
  be processed.  If ~ilc[standard-oi] is a list ending in an open channel,
  then ~c[ld] processes the forms in the list and then reads and processes
  the forms from the channel.  Analogously, if ~ilc[standard-oi] is a list
  ending a string, an object channel from the named file is opened and
  ~c[ld] processes the forms in the list followed by the forms in the
  file.  That channel is closed upon termination of ~c[ld].

  The remaining ~c[ld] specials are handled more simply and generally have
  to be bound to one of a finite number of tokens described in the
  ~c[:]~ilc[doc] entries for each ~c[ld] special.  Should any ~c[ld] special be supplied
  an inappropriate value, an error message is printed.

  Next, if ~ilc[ld-verbose] is ~c[t], ~c[ld] prints the message ``ACL2 loading
  name'' where ~c[name] names the file or channel from which forms are
  being read.  At the conclusion of ~c[ld], it will print ``Finished
  loading name'' if ~ilc[ld-verbose] is ~c[t].

  Finally, ~c[ld] repeatedly executes the ACL2 read-eval-print step, which
  may be described as follows.  A ~il[prompt] is printed to ~ilc[standard-co] if
  ~ilc[ld-prompt] is non-~c[nil].  The format of the ~il[prompt] is determined by
  ~ilc[ld-prompt].  If it is ~c[t], the default ACL2 ~il[prompt] is used.  If it is
  any other non-~c[nil] value then it is treated as an ACL2 function that
  will print the desired ~il[prompt].  ~l[ld-prompt].  In the
  exceptional case where ~c[ld]'s input is coming from the terminal
  ~c[(*standard-oi*)] but its output is going to a different sink (i.e.,
  ~ilc[standard-co] is not ~ilc[*standard-co*]), we also print the ~il[prompt] to the
  terminal.

  ~c[Ld] then reads a form from ~ilc[standard-oi].  If the object read is a
  keyword, ~c[ld] constructs a ``keyword command form'' by possibly
  reading several more objects.  ~l[keyword-commands].  This
  construction process is sensitive to the value of
  ~ilc[ld-keyword-aliases].  ~l[ld-keyword-aliases].  Otherwise, the
  object read is treated as the command form.

  ~c[Ld] next decides whether to evaluate or skip this form, depending on
  ~ilc[ld-pre-eval-filter].  Initially, the filter must be either ~c[:all],
  ~c[:query], or a new name.  If it is ~c[:all], it means all forms are
  evaluated.  If it is ~c[:query], it means each form that is read is
  displayed and the user is queried.  Otherwise, the filter is a name
  and each form that is read is evaluated as long as the name remains
  new, but if the name is ever introduced then no more forms are read
  and ~c[ld] terminates.  ~l[ld-pre-eval-filter].

  If the form is to be evaluated, first prints the form to
  ~ilc[standard-co], if ~ilc[ld-pre-eval-print] is ~c[t].  With this feature, ~c[ld] can
  process an input file or form list and construct a script of the
  session that appears as though each form was typed in.
  ~l[ld-pre-eval-print].

  ~c[Ld] then evaluates the form, with ~ilc[state] bound to the current ~il[state].
  The result is some list of (multiple) values.  If a ~il[state] is among the
  values, then ~c[ld] uses that ~il[state] as the subsequent current ~il[state].

  Depending on ~ilc[ld-error-triples], ~c[ld] may interpret the result as an
  ``error.'' ~l[ld-error-triples].  We first discuss ~c[ld]'s
  behavior if no error signal is detected (either because none was
  sent or because ~c[ld] is ignoring them as per ~ilc[ld-error-triples]).

  In the case of a non-erroneous result, ~c[ld] does two things: First, if
  the logical ~il[world] in the now current ~il[state] is different than the
  ~il[world] before execution of the form, ~c[ld] adds to the ~il[world] a ``~il[command]
  landmark'' containing the form evaluated.
  ~l[command-descriptor].  Second, ~c[ld] prints the result to
  ~ilc[standard-co], according to ~ilc[ld-post-eval-print].  If ~ilc[ld-post-eval-print]
  is ~c[nil], no result is printed.  If it is ~c[t], all of the results are
  printed as a list of (multiple) values.  Otherwise, it is
  ~c[:command-conventions] and only the non-erroneous ``value'' component
  of the result is printed.  ~l[ld-post-eval-print].

  Whenever ~c[ld] prints anything (whether the input form, a query, or
  some results) it ``eviscerates'' it if ~ilc[ld-evisc-tuple] is non-~c[nil].
  Essentially, evisceration is a generalization of Common Lisp's use
  of ~c[*print-level*] and ~c[*print-length*] to hide large substructures.
  ~l[ld-evisc-tuple].

  We now return to the case of a form whose evaluation signals an
  error.  In this case, ~c[ld] first restores the ACL2 logical ~il[world] to
  what it was just before the erroneous form was evaluated.  Thus, a
  form that partially changes the ~il[world] (i.e., begins to store
  properties) and then signals an error, has no effect on the ~il[world].
  You may see this happen on ~il[command]s that execute several ~il[events]
  (e.g., an ~ilc[encapsulate] or a ~ilc[progn] of several ~ilc[defuns]): even though the
  output makes it appear that the initial ~il[events] were executed, if an
  error is signalled by a later event the entire block of ~il[events] is
  discarded.

  After rolling back, ~c[ld] takes an action determined by
  ~ilc[ld-error-action].  If the action is ~c[:continue], ~c[ld] merely iterates the
  read-eval-print step.  Note that nothing suggestive of the value of
  the ``erroneous'' form is printed.  If the action is ~c[:return], ~c[ld]
  terminates normally.  If the action is ~c[:error], ~c[ld] terminates
  signalling an error to its caller.  If its caller is in fact another
  instance of ~c[ld] and that instance is watching out for error signals,
  the entire ~il[world] created by the erroneous inner ~c[ld] will be discarded
  by the outer ~c[ld].

  ~c[Ld] returns an error triple, ~c[(mv erp val state)].  ~c[Erp] is ~c[t] or ~c[nil]
  indicating whether an error is being signalled.  If no error is
  signalled, ~c[val] is the ``reason'' ~c[ld] terminated and is one of ~c[:exit]
  (meaning ~c[:]~ilc[q] was read), ~c[:eof] (meaning the input source was
  exhausted), ~c[:error] (meaning an error occurred but has been
  supressed) or ~c[:filter] (meaning the ~ilc[ld-pre-eval-filter] terminated
  ~c[ld])."

  `(ld-fn
    (list ,@(append
             (list `(cons 'standard-oi ,standard-oi))
             (if dir
                 (list `(cons 'dir ,dir))
                 nil)
             (if standard-cop
                 (list `(cons 'standard-co ,standard-co))
                 nil)
             (if proofs-cop
                 (list `(cons 'proofs-co ,proofs-co))
                 nil)
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
             (if ld-promptp
                 (list `(cons 'ld-prompt ,ld-prompt))
                 nil)
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
             (list `(cons 'ld-error-action ,ld-error-action))
             (if ld-query-control-alistp
                 (list `(cons 'ld-query-control-alist ,ld-query-control-alist))
                 nil)
             (if ld-verbosep
                 (list `(cons 'ld-verbose ,ld-verbose))
                 nil)))
    state
    t))

(defmacro quick-test nil

; We might want to add other events to the list below to test a wide variety of
; features.

  '(ld '((defun app (x y)
           (declare (xargs :guard (true-listp x)))
           (if (eq x nil) y (cons (car x) (app (cdr x) y))))
         (defthm true-listp-app
           (implies (true-listp x) (equal (true-listp (app x y)) (true-listp y))))
         :program
         (defun rev (x)
           (declare (xargs :guard (true-listp x)))
           (if (eq x nil) nil (app (rev (cdr x)) (list (car x)))))
         :logic
         (verify-termination rev)
         (verify-guards rev)
         (defthm true-listp-rev
           (implies (true-listp x) (true-listp (rev x)))
           :rule-classes :type-prescription)
         (defthm rev-rev (implies (true-listp x) (equal (rev (rev x)) x))))
       :ld-pre-eval-print t
       :ld-error-action :return))

(defun wormhole-prompt (channel state)
  (fmt1 "Wormhole ~s0~sr ~@1>"
        (list (cons #\0 (f-get-global 'current-package state))
              (cons #\1 (defun-mode-prompt-string state))
              (cons #\r
                    #+:non-standard-analysis "(r)"
                    #-:non-standard-analysis ""))
        0 channel state nil))

(defun reset-ld-specials-fn (reset-channels-flg state)

; We restore all of the ld specials to their initial, top-level
; values, except for the three channels, standard-oi, standard-co, and
; proofs-co, which are not reset unless the reset-channels-flg is t.
; Of course, if this function is called while under a recursive ld,
; then when we pop out of that ld, the reset values will be lost.

  (f-put-ld-specials
   (cond (reset-channels-flg *initial-ld-special-bindings*)
         (t (cdddr *initial-ld-special-bindings*)))
   state))

(defmacro reset-ld-specials (reset-channels-flg)

  ":Doc-Section Other
  restores initial settings of the ~ilc[ld] specials~/
  ~bv[]
  Examples:
  (reset-ld-specials t)
  (reset-ld-specials nil)
  ~ev[]~/

  Roughly speaking, the ~ilc[ld] specials are certain ~il[state] global
  variables, such as ~ilc[current-package], ~ilc[ld-prompt], and
  ~ilc[ld-pre-eval-filter], which are managed by ~ilc[ld] as though they were
  local variables.  These variables determine the channels on which ~ilc[ld]
  reads and prints and control many options of ~ilc[ld].  ~l[ld] for
  the details on what the ~ilc[ld] specials are.

  This function, ~c[reset-ld-specials], takes one Boolean argument, ~c[flg].
  The function resets all of the ~ilc[ld] specials to their initial,
  top-level values, except for the three channel variables,
  ~ilc[standard-oi], ~ilc[standard-co], and ~ilc[proofs-co], which are reset to their
  initial values only if ~c[flg] is non-~c[nil].  Of course, if you are in a
  recursive call of ~ilc[ld], then when you exit that call, the ~ilc[ld] specials
  will be restored to the values they had at the time ~ilc[ld] was called
  recursively.  To see what the initial values are, inspect the value
  of the constant ~c[*initial-ld-special-bindings*]."

  `(reset-ld-specials-fn ,reset-channels-flg state))

(defun maybe-reset-defaults-table1
  (key pre-defaults-tbl post-defaults-tbl state)
  (let* ((pre-val (cdr (assoc-eq key pre-defaults-tbl)))
         (post-val (cdr (assoc-eq key post-defaults-tbl)))
         (cmd `(table acl2-defaults-table ,key ',pre-val)))
    (if (equal pre-val post-val)
        (value nil)
      (er-let*
       ((ans
         (acl2-query
          :ubt-defaults
          '("The default ~s0 was ~x1 before undoing, but will be ~x2 after ~
             undoing unless the command ~X34 is executed.  Do you wish to ~
             re-execute this command after the :ubt?"
            :y t :n nil
            :? ("If you answer in the affirmative, then the command ~X34 will ~
                 be executed on your behalf.  This will make the default ~s0 ~
                 equal to ~x1, which is what it was just before your :ubt ~
                 command was executed.  Otherwise, the default ~s0 will be ~
                 what it is in the world after the undoing, namely ~x2.  See ~
                 also :DOC acl2-defaults-table."
                :y t :n nil))
          (list (cons #\0 (string-downcase (symbol-name key)))
                (cons #\1 pre-val)
                (cons #\2 post-val)
                (cons #\3 cmd)
                (cons #\4 nil))
          state)))
       (if ans
           (ld (list cmd)
               :ld-keyword-aliases nil
               :ld-pre-eval-filter :all
               :ld-pre-eval-print t
               :ld-post-eval-print :command-conventions
               :ld-evisc-tuple (default-evisc-tuple state)
               :ld-error-triples t
               :ld-error-action :return)
         (value nil))))))

(defun maybe-reset-defaults-table2
  (keys pre-defaults-tbl post-defaults-tbl state)
  (if keys
      (er-progn (maybe-reset-defaults-table1
                 (car keys) pre-defaults-tbl post-defaults-tbl state)
                (maybe-reset-defaults-table2
                 (cdr keys) pre-defaults-tbl post-defaults-tbl state))
    (value nil)))

(defun maybe-reset-defaults-table (pre-defaults-tbl post-defaults-tbl state)
  (maybe-reset-defaults-table2 (union-equal (strip-cars pre-defaults-tbl)
                                            (strip-cars post-defaults-tbl))
                               pre-defaults-tbl post-defaults-tbl state))

(defun delete-something (lst)

; Lst must be non-nil.  We return a list that is one shorter than lst by either
; dropping the first nil we find in lst or, if there are no nils, the last
; element.

  (cond ((null (cdr lst)) nil)
        ((null (car lst)) (cdr lst))
        (t (cons (car lst) (delete-something (cdr lst))))))

(defun store-in-kill-ring (x0 ring)

; A "kill ring" is a fancy queue that stores a fixed number, say n, of non-nil
; items in the order in which they were stored.  Only the most recent n non-nil
; items stored are saved.  When a non-nil item is stored and the ring is full,
; the oldest item is dropped out and lost.  So we have described a queue so
; far.  The only other operation on kill rings is "rotate" which selects an
; item from the kill ring but does not remove it.  Given a ring containing n
; items, n+1 rotations will return the each of the items in turn and in the
; reverse order in which they were stored.  More on rotation later.

; Kill rings are just lists of the n items, in order.  The length of the list
; is n but there may be nils in the list.  The initial kill ring of length n
; is just n nils.

  (cond ((or (null x0)          ; item is nil or the size of the
             (null ring))       ; ring is 0.  We store nothing.
         ring)
        (t (cons x0 (delete-something ring)))))

(defun rotate-kill-ring1 (ring xn)
  (cond ((null ring) xn)
        ((car ring) (append ring xn))
        (t (rotate-kill-ring1 (cdr ring) (append xn (list nil))))))

(defun rotate-kill-ring (ring xn)

; See store-in-kill-ring for background on rings.  Xn is an element to add to
; the ring.  We step the ring once, returning (mv item ring'), where item is
; the most recently added item in ring and ring' is the result of removing that
; item and adding xn as the oldest item in the ring.  Thus, a series of
; rotate-kill-ring n+1 long will return us to the original configuration.

  (cond ((null (car ring)) (mv nil ring))
        (t (mv (car ring)
               (rotate-kill-ring1 (cdr ring) (list xn))))))

(defun ubt-ubu-fn1 (kwd wrld pred-wrld state)
  (let ((pre-defaults-table (table-alist 'acl2-defaults-table wrld)))
    (er-let*
     ((redo-cmds (ubt-ubu-query kwd wrld pred-wrld nil
                                nil wrld state nil)))
     (pprogn
      (f-put-global
       'undone-worlds-kill-ring
       (store-in-kill-ring wrld
                           (f-get-global
                            'undone-worlds-kill-ring
                            state))
       state)
      (set-w 'retraction pred-wrld state)
      (let ((redo-cmds (if (eq (car redo-cmds)
                               (default-defun-mode pred-wrld))
                           (cdr redo-cmds)
                         redo-cmds)))
        (er-progn
         (if redo-cmds
             (mv-let (col state)
                     (fmt1 "Undoing complete.  Redoing ~
                                              started...~%"
                           nil 0 (standard-co state) state nil)
                     (declare (ignore col))
                     (value nil))
           (value nil))
         (if redo-cmds
             (ld redo-cmds
                 :ld-redefinition-action '(:doit! . :overwrite)
                 :ld-keyword-aliases nil
                 :ld-pre-eval-filter :all
                 :ld-pre-eval-print t
                 :ld-post-eval-print :command-conventions
                 :ld-evisc-tuple (default-evisc-tuple state)
                 :ld-error-triples t
                 :ld-error-action :return
                 :ld-query-control-alist
                 (cons '(:redef :y)
                       (ld-query-control-alist state)))
           (value nil))
         (if redo-cmds
             (mv-let (col state)
                     (fmt1 "Redoing complete.~%~%"
                           nil 0 (standard-co state) state nil)
                     (declare (ignore col))
                     (value nil))
           (value nil))
         (maybe-reset-defaults-table
          pre-defaults-table
          (table-alist 'acl2-defaults-table (w state))
          state)
         (pcs-fn :x :x nil state)
         (value :invisible)))))))

(defun ubt-ubu-fn (kwd cd state)

; Kwd is :ubt or :ubu.

  (let* ((wrld (w state))
         (command-number-baseline
          (access command-number-baseline-info
                  (global-val 'command-number-baseline-info wrld)
                  :current)))
    (er-let* ((cmd-wrld (er-decode-cd cd wrld kwd state)))
             (cond ((if (eq kwd :ubt)
                        (<= (access-command-tuple-number (cddar cmd-wrld))
                            command-number-baseline)
                      (< (access-command-tuple-number (cddar cmd-wrld))
                         command-number-baseline))

; We prevent ubt and ubu from going into prehistory, thus burning users due to
; typos.  But sometimes developers need to do it.  Here is how from within the
; ACL2 loop:

; (set-state-ok t)
; (defun my-ubt-ubu-fn (inclp x state) (declare (xargs :guard t)) (value x))
; :q
; Grab this defun, rename it to my-ubt-ubu-fn, edit out the cond clause
; containing this comment and define my-ubt-ubu-fn in raw lisp.
; (lp)
; (my-ubt-ubu-fn t 'sys-fn state), where sys-fn is the desired target of the
; ubt.

                    (cond
                     ((let ((command-number-baseline-original
                             (access command-number-baseline-info
                                     (global-val 'command-number-baseline-info wrld)
                                     :original)))
                        (if (eq kwd :ubt)
                            (<= (access-command-tuple-number (cddar cmd-wrld))
                                command-number-baseline-original)
                          (< (access-command-tuple-number (cddar cmd-wrld))
                             command-number-baseline-original)))
                      (er soft kwd "Can't undo into system initialization."))
                     (t (er soft kwd
                            "Can't undo into prehistory.  See :DOC ~
                             reset-prehistory."))))
                   ((and (eq kwd :ubu) (equal wrld cmd-wrld))
                    (er soft kwd
                        "Can't undo back to where we already are!"))
                   (t
                    (let ((pred-wrld (if (eq kwd :ubt)
                                         (scan-to-command (cdr cmd-wrld))
                                       cmd-wrld)))
                      (ubt-ubu-fn1 kwd wrld pred-wrld state)))))))

(defun ubt!-ubu!-fn (kwd cd state)

; Kwd is :ubt or :ubu.

  (state-global-let*
   ((ld-query-control-alist
     (list* `(,kwd :n!)
            '(:ubt-defaults :n)
            (@ ld-query-control-alist)))
    (inhibit-output-lst
     (union-equal '(observation warning error)
                  (@ inhibit-output-lst))))
   (mv-let (erp val state)
           (ubt-ubu-fn kwd cd state)
           (declare (ignore erp val))
           (value :invisible))))

(defmacro ubt-prehistory ()

  ":Doc-Section History

  undo the ~il[command]s back through the last ~ilc[reset-prehistory] event~/

  This command is only used to eliminate a ~ilc[reset-prehistory] event.  If
  your most recent ~c[reset-prehistory] event does not have a flag argument
  of ~c[t], then ~c[:ubt-prehistory] undoes all command back through, and
  including, that ~c[reset-prehistory] event.~/~/"

  (list 'ubt-prehistory-fn 'state))

(defun ubt-prehistory-fn (state)
  (let* ((ctx 'ubt-prehistory)
         (wrld (w state))
         (command-number-baseline-info
          (global-val 'command-number-baseline-info wrld))
         (command-number-baseline
          (access command-number-baseline-info
                  command-number-baseline-info
                  :current)))
    (cond ((eql command-number-baseline
                (access command-number-baseline-info
                        command-number-baseline-info
                        :original))
           (er soft ctx
               "There is no reset-prehistory event to undo."))
          ((access command-number-baseline-info
                   command-number-baseline-info
                   :permanent-p)
           (er soft ctx
               "It is illegal to undo a reset-prehistory event that had its ~
                permanent-p flag set to t.  See :DOC reset-prehistory."))
          (t (ubt-ubu-fn1 :ubt-prehistory
                          wrld
                          (scan-to-command
                           (cdr (lookup-world-index
                                 'command command-number-baseline wrld)))
                          state)))))

(defun oops-fn (state)
  (mv-let (new-wrld new-kill-ring)
          (rotate-kill-ring (f-get-global 'undone-worlds-kill-ring state)
                            (w state))
          (cond ((null new-wrld)
                 (cond ((null (f-get-global 'undone-worlds-kill-ring state))
                        (er soft :oops
                            "Oops has been disabled in this ACL2 session.  ~
                             See :DOC reset-kill-ring"))
                       (t
                        (er soft :oops
                            "ACL2 cannot execute :oops at this time, ~
                             presumably because you have never executed :ubt ~
                             or :u during this ACL2 session (at least not ~
                             since the last invocation of reset-kill-ring)."))))
                (t (er-progn
                    (revert-world-on-error
                     (pprogn
                      (fms "Installing the requested world.  Note that ~
                            functions being re-defined during this procedure ~
                            will not have compiled definitions, even if they ~
                            had compiled definitions before the last :ubt or ~
                            :u.~|~%"
                           nil (f-get-global 'standard-co state) state nil)
                      (set-w! new-wrld state)
                      (er-progn (pcs-fn :x :x nil state)
                                (value nil))))
                    (pprogn
                     (f-put-global 'undone-worlds-kill-ring
                                   new-kill-ring state)
                     (value :invisible)))))))

(defmacro oops nil

  ":Doc-Section History

  undo a ~c[:u] or ~c[:]~ilc[ubt]~/

  The keyword ~il[command] ~c[:oops] will undo the most recent ~c[:]~ilc[ubt] (or ~c[:u],
  which we here consider just another ~c[:]~ilc[ubt]).  A second ~c[:oops] will undo
  the next most recent ~c[:]~ilc[ubt], a third will undo the ~c[:]~ilc[ubt] before that
  one, and a fourth ~c[:oops] will return the logical ~il[world] to its
  configuration before the first ~c[:oops].~/

  Consider the logical world (~pl[world]) that represents the
  current extension of the logic and ACL2's rules for dealing with it.
  The ~c[:]~ilc[ubt] and ~c[:u] ~il[command]s ``roll back'' to some previous ~il[world]
  (~pl[ubt]).  Sometimes these ~il[command]s are used to inadvertently
  undo useful work and user's wish they could ``undo the last undo.''
  That is the function provided by ~c[:oops].

  ~c[:Oops] is best described in terms of an implementation.  Imagine a
  ring of four ~il[world]s and a marker (~c[*]) indicating the current ACL2
  ~il[world]:
  ~bv[]
               *
             w0 
           /    \\
         w3      w1
           \\    /
             w2
  ~ev[]
  This is called the ``kill ring'' and it is maintained as follows.
  When you execute an event the current ~il[world] is extended and the kill
  ring is not otherwise affected.  When you execute ~c[:]~ilc[ubt] or ~c[:u], the
  current ~il[world] marker is moved one step counterclockwise and that
  ~il[world] in the ring is replaced by the result, say ~c[w0'], of the ~c[:]~ilc[ubt] or
  ~c[:u].
  ~bv[]
             w0 
           /    \\
        *w0'     w1
           \\    /
             w2
  ~ev[]
  If you were to execute ~il[events] at this point, ~c[w0'] would be extended
  and no other changes would occur in the kill ring.

  When you execute ~c[:oops], the marker is moved one step clockwise.
  Thus the kill ring becomes
  ~bv[]
               *
             w0 
           /    \\
         w0'     w1
           \\    /
             w2
  ~ev[]
  and the current ACL2 ~il[world] is ~c[w0] once again.  That is, ~c[:oops]
  ``undoes'' the ~c[:]~ilc[ubt] that produced ~c[w0'] from ~c[w0].  Similarly,
  a second ~c[:oops] will move the marker to ~c[w1], undoing the undo that
  produced ~c[w0] from ~c[w1].  A third ~c[:oops] makes ~c[w2] the current
  ~il[world].  Note however that a fourth ~c[:oops] restores us to the
  configuration previously displayed above in which ~c[w0'] has the marker.

  In general, the kill ring contains the current ~il[world] and the three
  most recent ~il[world]s in which a ~c[:]~ilc[ubt] or ~c[:u] were done.

  While ~c[:]~ilc[ubt] may appear to discard the information in the ~il[events]
  undone, we can see that the ~il[world] in which the ~c[:]~ilc[ubt] occurred is
  still available.  No information has been lost about that ~il[world].
  But ~c[:]~ilc[ubt] does discard information!  ~c[:]~ilc[Ubt] discards the information
  necessary to recover from the third most recent ~ilc[ubt]!  An ~c[:oops], on
  the other hand, discards no information, it just selects the next
  available ~il[world] on the kill ring and doing enough ~c[:oops]es will
  return you to your starting point.

  We can put this another way.  You can freely type ~c[:oops] and inspect
  the ~il[world] that you thus obtain with ~c[:]~ilc[pe], ~c[:]~ilc[pc], and other ~il[history]
  ~il[command]s.  You can repeat this as often as you wish without risking
  the permanent loss of any information.  But you must be more careful
  typing ~c[:]~ilc[ubt] or ~c[:u].  While ~c[:oops] makes ~c[:]~ilc[ubt] seem ``safe'' because the
  most recent ~c[:]~ilc[ubt] can always be undone, information is lost when you
  execute ~c[:]~ilc[ubt].

  We note that ~c[:ubt] and ~c[:u] may remove compiled definitions (except in
  Lisps such as OpenMCL, in which functions are always compiled).  When the
  original world is restored using ~c[:oops], restored functions will not
  generally be compiled, though the user can remedy this situation; ~pl[comp].

  Finally, we note that our implementation of ~c[oops] can use a significant
  amount of memory, because of the saving of old logical ~il[world]s.  Most
  users are unlikely to experience a memory problem, but if you do, then you
  may want to disable ~c[oops] by evaluting ~c[(reset-kill-ring 0 state)];
  ~pl[reset-kill-ring]."

  '(oops-fn state))

(defun reset-kill-ring (n state)

  ":Doc-Section History

  save memory by resetting and perhaps resizing the kill ring used by ~ilc[oops]~/

  By default, ACL2 holds on to old logical ~il[world]s when you undo
  ~il[command]s (~pl[ubt]), as documented elswhere; ~pl[oops].  You can free up
  memory by deleting those old worlds using ~c[reset-kill-ring].
  ~bv[]
  Examples:
  (reset-kill-ring t state)   ; replace each element of the kill ring by nil
  (reset-kill-ring 2 state)   ; create a new kill ring of '(nil nil)
  (reset-kill-ring 0 state)   ; create a new kill ring that is empty
  (reset-kill-ring nil state) ; just return the length of the kill ring

  General form:
  (reset-kill-ring n state)
  ~ev[]
  where ~c[n] evaluates either to ~c[t], to ~c[nil], or to a nonnegative
  integer (a ~ilc[natp]).  If ~c[n] evaluates to ~c[t], it is treated as the
  length of the current kill ring.  If ~c[n] is ~c[nil], then the length ~c[k]
  of the current kill ring is returned as a value triple ~c[(mv nil k state)].
  If ~c[n] is a ~ilc[natp], then the kill ring is replaced with a list of ~c[n]
  ~c[nil]s.

  In particular, use ~c[(reset-kill-ring 0 state)] to avoid saving any old
  logical ~il[world]s, at the cost of disabling the effect of the ~ilc[oops]
  ~il[command].~/~/"

  (declare (xargs :guard (or (eq n t) (natp n))))
  (let ((n (if (eq n t)
               (length (f-get-global 'undone-worlds-kill-ring state))
             n)))
    (if n
        (pprogn (f-put-global 'undone-worlds-kill-ring (make-list n) state)
                (value :invisible))
      (value (f-get-global 'undone-worlds-kill-ring state)))))

(defmacro i-am-here ()

  ":Doc-Section Miscellaneous

  a convenient marker for use with ~ilc[rebuild]~/
  ~bv[]
  Example Input File for Rebuild:
  (defun fn1 (x y) ...)
  (defthm lemma1 ...)
  (defthm lemma2 ...)
  (i-am-here)
  The following lemma won't go through.  I started
  typing the hint but realized I need to prove a
  lemma first.  See the failed proof attempt in foo.bar.
  I'm going to quit for the night now and resume tomorrow
  from home.

  (defthm lemma3 ...
    :hints ((\"Goal\" :use (:instance ???
  ...
  ~ev[]~/

  By putting an ~c[(i-am-here)] form at the ``frontier'' of an evolving
  file of ~il[command]s, you can use ~ilc[rebuild] to load the file up to the
  ~c[(i-am-here)].  ~c[I-am-here] simply returns an ~ilc[ld] error triple and any
  form that ``causes an error'' will do the same job.  Note that the
  text of the file after the ~c[(i-am-here)] need not be machine
  readable."

  '(mv-let (col state)
           (fmt1 "~ I-AM-HERE" nil 0 (standard-co state) state nil) 
           (declare (ignore col))
           (mv t nil state)))

(defun rebuild-fn-read-filter (file state)
  (state-global-let*
   ((standard-oi *standard-oi*)
    (standard-co *standard-co*))
   (er-let*
     ((ans
       (acl2-query
        :rebuild
        '("How much of ~x0 do you want to process?"
          :t :all :all :all :query :query :until :until
          :? ("If you answer T or ALL, then the entire file will be ~
               loaded.  If you answer QUERY, then you will be asked ~
               about each command in the file.  If you answer UNTIL, ~
               then you should also type some name after the UNTIL ~
               and we will then proceed to process all of the events ~
               in file until that name has been introduced.  Rebuild ~
               automatically stops if any command causes an error.  ~
               When it stops, it leaves the logical world in the ~
               state it was in immediately before the erroneous ~
               command.  Thus, another way to use rebuild is to get ~
               into the habit of planting (i-am-here) -- or any other ~
               form that causes an error when executed -- and then ~
               using the filter T or ALL when you rebuild."
              :t :all :all :all :query :query :until :until))
        (list (cons #\0 file))
        state)))
     (cond ((eq ans :until)
            (state-global-let*
             ((infixp nil))
             (read-object *standard-oi* state)))
           (t (value ans))))))

(defun rebuild-fn (file filter filterp state)
  (er-let*
      ((filter
        (if filterp
            (value (if (eq filter t) :all filter))
          (rebuild-fn-read-filter file state))))
    (mv-let (erp val state)
      (ld file
          :standard-co *standard-co*
          :proofs-co *standard-co*
          :ld-skip-proofsp t
          :ld-prompt nil
          :ld-keyword-aliases nil
          :ld-pre-eval-filter filter
          :ld-pre-eval-print nil
          :ld-post-eval-print :command-conventions
          :ld-evisc-tuple (default-evisc-tuple state)
          :ld-error-triples t
          :ld-error-action :return
          :ld-query-control-alist '((:filter . nil) . t)
          :ld-verbose t)
      (declare (ignore erp val))
      (value t))))

(defmacro rebuild (file &optional (filter 'nil filterp))

  ":Doc-Section Other

  a convenient way to reconstruct your old ~il[state]~/
  ~bv[]
  Examples:
  ACL2 !>(rebuild \"project.lisp\")
  ACL2 !>(rebuild \"project.lisp\" t)
  ACL2 !>(rebuild \"project.lisp\" :all)
  ACL2 !>(rebuild \"project.lisp\" :query)
  ACL2 !>(rebuild \"project.lisp\" 'lemma-22)
  ~ev[]~/

  ~c[Rebuild] allows you to assume all the ~il[command]s in a given file or
  list, supplied in the first argument.  Because ~c[rebuild] processes an
  arbitrary sequence of ~il[command]s with ~ilc[ld-skip-proofsp] ~c[t], it is
  unsound!  However, if each of these ~il[command]s is in fact admissible,
  then processing them with ~c[rebuild] will construct the same logical
  ~il[state] that you would be in if you typed each ~il[command] and waited
  through the proofs again.  Thus, ~c[rebuild] is a way to reconstruct a
  ~il[state] previously obtained by proving your way through the ~il[command]s.

  The second, optional argument to ~c[rebuild] is a ``filter''
  (~pl[ld-pre-eval-filter]) that lets you specify which ~il[command]s
  to process.  You may specify ~c[t], ~c[:all], ~c[:query], or a new logical name.
  ~c[t] and ~c[:all] both mean that you expect the entire file or list to be
  processed.  ~c[:query] means that you will be asked about each ~il[command]
  in turn.  A new name means that all ~il[command]s will be processed as
  long as the name is new, i.e., ~c[rebuild] will stop processing ~il[command]s
  immediately after executing a ~il[command] that introduces name.  ~C[Rebuild]
  will also stop if any ~il[command] causes an error.  You may therefore
  wish to plant an erroneous form in the file, e.g., ~c[(mv t nil state)],
  (~pl[ld-error-triples]), to cause ~c[rebuild] to stop there.  The
  form ~c[(i-am-here)] is such a pre-defined form.  If you do not specify
  a filter, ~c[rebuild] will query you for one.

  Inspection of the definition of ~c[rebuild], e.g., via ~c[:]~ilc[pc] ~c[rebuild-fn],
  will reveal that it is just a glorified call to the function ~ilc[ld].
  ~l[ld] if you find yourself wishing that ~c[rebuild] had additional
  functionality."

  `(rebuild-fn ,file ,filter ,filterp state))

;           The Tall Texas Tale about  BIG-CLOCK

; Like any Lisp system, it may be said, loosely speaking, that ACL2
; typically reads a form, evaluates it in the current state, and
; prints the result.  This read-eval-print activity in ACL2 is done by
; the function ld-fn.  When the user enters ACL2 by invoking (LP),
; ld-fn is called to do the work.

; The read phase of the read-eval-print activity is done with the
; read-object function, which calls the Common Lisp read function.
; This read is influenced by *package*, *readtable*, and *features*,
; as described in acl2.lisp.

; The semantics of an ACL2 read-eval-print cycles is best desribed
; from the logical point of view via the logic programming pradigm, to
; which we degress momentarity.  In the Lisp paradigm, one thinks
; of an interaction as always being something like

; >  (fact 3) = ?

; wherein a variable free term is evaluated to obtain a suitable
; value, say 6.  In logic programming, as in Baroque or Prolog, one
; can ask a question like:

; ? (fact x) = 6

; i.e. does there exist an x whose factorial is 6?  The system then
; attempts to answer the question and may find one or several values for
; x that does the job, e.g. 3.  In fact, one can even imagine asking

; ? (fact x) = y

; to obtain a variety of values of x and y that satisfy the relation.
; Or might might merely be informed that that, yes, there do exist
; values of x and y satisfying the relation, without being given x and
; y explicitly.

; The point of this digression is merely to mention the well-known
; (but non-Lispish) idea that the input to a computation need not
; always be given entirely in advance of the commencement of a
; computation.  In truth, even in regular Common Lisp, the input is not
; really always given entirely in advance because the charcters that
; may appear in *standard-input* or the file system need not be known
; before evaluation commences.  ACL2 employs this ``incompletely
; specified at evaluation commencement'' idea.

; From the logical point of view, an ACL2 ``state'' is any object in
; the logic satifying the state-p predicate, q.v. in axioms.lisp.
; There is a long comment in axioms.lisp under the heading STATE which
; describes the many fields that a state has.

; At the beginning of any interaction with the top-level ACL2 ld-fn,
; there is a ``partial current state'', which may be partially
; perceived, without side-effect, in Common Lisp, but outside of ACL2,
; by invoking (what-is-the-global-state).  This partial current-state
; includes (a) the names, types, and times of the open input and
; output channels (but not the characters read or written to those
; channels), (b) the symbols in the global table, (c) the t-stack, (d)
; the 32-bit stack, and (e) the file clock.  We say that an object o
; satisfying state-p is ``consistent with the current paritial state''
; provided that every fact revealed by (what-is-the-global-state) and
; by examination of the bound globals is true about o.

; In Lisp (as opposed to Prolog) the input form has no explicit free
; variable.  In ACL2, however, one free variable is permitted, and
; this variable, always named STATE, refers, loosely speaking to the
; ``value of the state at the time of input''.  In ACL2, the variable
; STATE includes the input via files and channels.


;                   Common LISP IO

; If we have a Common Lisp system that is connected to an IO system,
; then at each tick of time, the system may (a) print a character,
; byte, or object to any of the open streams, (b) read a character,
; byte, or object from any of the open streams, (c) open a file for
; reading or writing and (c) close an open stream.

; Suppose that old and new are two objects satisfying state-p and that
; we have an implementation of ACL2 in a Common Lisp which is
; connected to an IO system.  We say that old and new are ``IO
; consistent with the Common Lisp IO system's behavior in the time
; period between old and new'' provided that the relationships between
; the various io fields of old and new are just what happened.  For
; example, suppose that old and new are different only in that in new
; on one input character channel one character has been consumed.
; Then that is consistent with a Common Lisp IO system in which the
; stream corresponding to the channel was read to get just one
; character.  As another example, suppose that old and new are
; different only because a file is now on read-files that was not
; there before and file-clock has been ticked twice and the two most
; recent values of the file clock are the open and close time of the
; read file.  Then that is consistent with a Common Lisp IO system in
; which a stream for a file of the read file's name was opened and
; consumed and the characters read were exactly the characters
; associated with the file name in readable-files at the file-clock
; upon open.  This concept needs to be completely and fully spelled
; out, but we believe it is all boring and obvious:  the file clock is
; to keep track of the opening and closing times.  The read-files and
; written-files entries record closing times and contents.  The
; readable-files and input channels entries record characters actually
; consumed.

; In the extremely important degenerate case, old and new are
; consistent with the Common Lisp IO system's behavior over a time
; interval if all the fields of old and new are identical, excepting
; only the global-table, stacks, and big-clock entries, and no IO
; occurred in the time interval.


;                        The ACL2 ld theorem

; Let us suppose, without loss of generality, that run is a function
; of one argument, state, that has been defined by the user, and
; accepted by ACL2.  Let us further suppose that run returns a single
; state value.  (There is no loss of generality here because any
; particular arguments or output value that the user wishes to provide
; or see can be placed in state globals.  For example, one could add
; two to three by defining run as (defun run (state) (f-set-global
; 'foo (+ 2 3)))).  Let us suppose that an ACL2 interaction of the
; form

; ACL2 !> (run state)

; completes.  What is the theorem that describes the relationship
; between the old current partial state and the new current partial
; state?  The theorem is that (a) there exists an object, old, which
; satisfies the predicate statep and an object, new, which also
; satisfies the predicate statep such that old is consistent with the
; partial current state at the time of the input and new is consistent
; with the partial current state at the time of the output (b) new and
; old are IO consistent with the Common Lisp IO system's behavior in
; the time period between the beginning and ending of the evaluation
; (c) new = (trans-eval '(run state) nil old), and (d) (run old) =
; (trans-eval '(run state) nil old) except in the big-clock field.

; In the important degenerate case in which no io occurs, this means
; essentially that there exists (in the constructive sense) a
; big-clock entry in old which is ``large enough'' to perform the
; trans-eval of the input form without ``running out of time''.  ACL2
; does not reveal to the user how much ``time'' was required, but
; merely guarantees that there exists a sufficiently large amount of
; time.  In fact, because we ``jump into compiled code'' in
; raw-ev-fncall, we have no way of efficiently keeping track of how
; much time has been used.

; Note that there is no commitment to a uniform value for big-clock
; across all ACL2 interactions.  In particular, there obviously exists
; an infinite sequence of forms, say (fact 1), (fact 2), (fact 3),
; ....  which would require an infinitely increasing series of
; big-clocks.  An ACL2 evaluation effort may fail for a variety of
; reasons, including resource errors and certain design decisions,
; e.g. the detection that a function should not be clobbered because
; there is already a function by that name with a symbol-function
; property.  If evaluation fails, some characters may nevertheless
; have been printed or read and state may have been changed.

(defconst *basic-sweep-error-str*
  "The state back to which we have been asked to roll would contain an ~
   object that is inconsistent with the requested resetting of the ~
   ACL2 known-package-alist.  Logical consistency would be imperiled ~
   if the rollback were undertaken.  Please get rid of pointers to ~
   such objects before attempting such a rollback.~|~%")

(defun sweep-symbol-binding-for-bad-symbol (sym obj deceased-packages state)
  (cond ((symbolp obj)
         (cond ((member-equal (symbol-package-name obj) deceased-packages)
                (er soft "undo consistency check"
                    "~@0In particular, the value of the global ~
                     variable ~x1 contains the symbol ~x2 in package ~
                     ~x3, which we have been asked to remove.   ~
                     Please reset ~x1, as with (assign ~x1 nil)."
                    *basic-sweep-error-str*
                    sym
                    obj
                    (symbol-package-name obj)))
               (t (value nil))))
        ((atom obj) (value nil))
        ((equal obj (w state))
         (value nil))
        (t (er-progn (sweep-symbol-binding-for-bad-symbol
                      sym (car obj)
                      deceased-packages state)
                     (sweep-symbol-binding-for-bad-symbol
                      sym (cdr obj) deceased-packages state)))))

(defun sweep-global-lst (l deceased-packages state)
  (cond ((null l) (value nil))
        (t (er-progn
            (sweep-symbol-binding-for-bad-symbol
             (car l)
             (get-global (car l) state)
             deceased-packages state)
            (sweep-global-lst (cdr l) deceased-packages state)))))

(defun sweep-stack-entry-for-bad-symbol (name i obj deceased-packages state)
  (cond ((symbolp obj)
         (cond ((member-equal (symbol-package-name obj) deceased-packages)
                (er soft "undo consistency check"
                    "~@0In particular, the entry in the ~@1 at ~
                     location ~x2 contains the symbol ~x3 in package ~
                     ~x4, which we have been asked to undo.  Please ~
                     change the ~@1 entry at location ~x2 or ~
                     shrink the ~@1."
                    *basic-sweep-error-str*
                    name
                    i
                    obj
                    (symbol-package-name obj)))
               (t (value nil))))
        ((atom obj) (value nil))
        ((equal obj (w state))
         (value nil))
        (t (er-progn (sweep-stack-entry-for-bad-symbol
                      name i (car obj) deceased-packages state)
                     (sweep-stack-entry-for-bad-symbol
                      name i (cdr obj) deceased-packages state)))))

(defun sweep-t-stack (i deceased-packages state)
  (cond ((> i (t-stack-length state))
         (value nil))
        (t (er-progn
            (sweep-stack-entry-for-bad-symbol
             "t-stack" i (aref-t-stack i state) deceased-packages state)
            (sweep-t-stack (+ 1 i) deceased-packages state)))))

(defun sweep-acl2-oracle (i deceased-packages state)

; A valid measure is (- (len (acl2-oracle state)) if we want to admit this
; function in logic mode, since read-acl2-oracle replaces the acl2-oracle of
; the state with its cdr.

  (mv-let
   (nullp car-oracle state)
   (read-acl2-oracle state)
   (cond (nullp (value nil))
         (t (er-progn
             (sweep-stack-entry-for-bad-symbol
              "acl2-oracle" i car-oracle deceased-packages state)
             (sweep-acl2-oracle (+ 1 i) deceased-packages state))))))

(defun sweep-global-state-for-lisp-objects (deceased-packages state)

; This function sweeps every component of the state represented by
; *the-live-state* to verify that no symbol is contained in a package that we
; are about to delete.  This is sensible before we undo a defpkg, for example,
; which may ``orphan'' some objects held in, say, global variables in the
; state.  We look in the global variables, the t-stack, and acl2-oracle.  If a
; global variable, t-stack entry, or acl2-oracle entry contains such an object,
; we cause an error.  This function is structurally similar to
; what-is-the-global-state in axioms.lisp.

; The components of the state and their disposition are:

; open-input-channels  -  there are no objects in the dynamic channels.
;                         Objects obtained from those channels will be
;                         read into an otherwise ok state.

; open-output-channels -  there are no objects in the dynamic channels

; global-table - the global table is the most likely place we will find
;                bad objects.  However, we know that the value of
;                'current-acl2-world is not bad, because after an undo
;                it is set to a previously approved value.

  (er-progn
   (sweep-global-lst (global-table-cars state) deceased-packages state)


; t-stack - this stack may contain bad objects.

   (sweep-t-stack 0 deceased-packages state)
   (sweep-acl2-oracle 0 deceased-packages state))

; The remaining fields contain no ``static'' objects.  The fields are:
; 32-bit-integer-stack
; big-clock
; idates
; file-clock
; readable-files
; written-files
; read-files
; writeable-files
; list-all-package-names-lst

  )

(deflabel compilation
  :doc
  ":Doc-Section ACL2::Programming

  compiling ACL2 functions~/
  ~bv[]
  Example:
  ACL2 !>:comp app
  ACL2 !>:set-compile-fns t
  ~ev[]~/

  ~l[comp] and ~pl[set-compile-fns].")

(deflabel escape-to-common-lisp
  :doc
  ":Doc-Section Miscellaneous

  escaping to Common Lisp~/
  ~bv[]
  Example:
  ACL2 !>:Q
  ~ev[]~/

  There is no Common Lisp escape feature in the ~ilc[lp].  This is part of
  the price of purity.  To execute a form in Common Lisp as opposed to
  ACL2, exit ~ilc[lp] with ~c[:]~ilc[q], submit the desired forms to the Common Lisp
  read-eval-print loop, and reenter ACL2 with ~c[(lp)].~/")

(deflabel copyright
  :doc
  ":Doc-Section Miscellaneous

  ACL2 copyright, license, sponsorship~/~/

  ACL2 Version 3.1 -- A Computational Logic for Applicative Common Lisp
  Copyright (C) 2006  University of Texas at Austin

  This version of ACL2 is a descendent of ACL2 Version 1.9, Copyright
  (C) 1997 Computational Logic, Inc.  See the documentation topic NOTE-2-0.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  See the file ~c[LICENSE].  If not, write
  to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
  02139, USA.

  Written by:  Matt Kaufmann               and J Strother Moore
  email:       Kaufmann@cs.utexas.edu      and Moore@cs.utexas.edu
  Department of Computer Sciences
  University of Texas at Austin
  Austin, TX 78712-1188 U.S.A.

  Please also ~pl[acknowledgments].~/")

(deflabel acknowledgments
  :doc
  ":Doc-Section Miscellaneous

  some contributors to the well-being of ACL2~/~/

  The development of ACL2 was initially made possible by funding from the
  U. S. Department of Defense, including ARPA and ONR.  We thank all the
  organizations that have contributed support, including the following (in
  alphabetical order).~bq[]

  o AMD, for providing significant time over several years for Matt Kaufmann
  to carry out ACL2 research, support, and development~nl[]
  o Computational Logic, Inc. and its president, Don Good, where the first
  eight years of ACL2 development occurred~nl[]
  o DARPA~nl[]
  o Digital Equipment Corporation~nl[]
  o EDS, which provided some time for Matt Kaufmann's ACL2 work 1998-1999~nl[]
  o IBM~nl[]
  o NSF~nl[]
  o ONR~nl[]
  o Rockwell Collins~nl[]
  o SRC~nl[]
  o Sun Microsystems~nl[]
  o University of Texas at Austin (in particular support to J Moore through
  the Admiral B. R.  Inman Chair of Computing Theory)

  ~eq[]ACL2 was started in August, 1989 by Boyer and Moore working
  together.  They co-authored the first versions of axioms.lisp and
  basis.lisp, with Boyer taking the lead in the formalization of
  ``~il[state]'' and the most primitive ~il[io] functions.  Boyer also
  had a significant hand in the development of the early versions of
  the files interface-raw.lisp and translate.lisp.  For several years,
  Moore alone was responsible for developing the ACL2 system code,
  though he consulted often with both Boyer and Kaufmann.  In August,
  1993, Kaufmann became jointly responsible with Moore for developing
  the system.  Boyer has continued to provide valuable consulting on
  an informal basis.

  Bishop Brock was the heaviest early user of ACL2, and provided many
  suggestions for improvements.  In particular, the ~c[:cases] and
  ~c[:restrict] ~il[hints] were his idea; he developed an early
  version of congruence-based reasoning for Nqthm; and he helped in
  the development of some early ~il[books] about arithmetic.  In a
  demonstration of his courage and faith in us, he pushed for
  Computational Logic, Inc., to agree to the Motorola CAP contract --
  which required formalizing a commercial DSP in the untested ACL2 --
  and moved to Scottsdale, AZ, to do the work with the Motorola design
  team.  His demonstration of ACL2's utility was an inspiration, even
  to those of us designing ACL2.

  John Cowles also helped in the development of some early ~il[books] about
  arithmetic, and also provided valuable feedback and bug reports.

  Other early users of ACL2 at Computational Logic, Inc. helped
  influence its development.  In particular, Warren Hunt helped with
  the port to Macintosh Common Lisp, and Art Flatau and Mike Smith
  provided useful general feedback.

  Mike Smith helped develop the Emacs portion of the implementation of
  proof trees.

  Bill Schelter made some enhancements to akcl (now gcl) that helped
  to enhance ACL2 performance in that Common Lisp implementation, and
  more generally, responded helpfully to our bug reports.  Camm Maguire
  has since provided wonderful gcl support, and has created a Debian package
  for ACL2 built on GCL.  We are also grateful to developers of other Common
  Lisp implementations.

  Kent Pitman helped in our interaction with the ANSI Common Lisp
  standardization committee, X3J13.

  John Cowles helped with the port to Windows (98) by answering
  questions and running tests.

  Ruben Gamboa created a modification of ACL2 to allow reasoning about
  the real numbers using non-standard analysis.  His work has been
  incorporated into the ACL2 distribution; ~pl[real].

  Rob Sumners has made numerous useful suggestions.  In particular, he
  has designed and implemented improvements for ~il[stobj]s and been
  key in our development of locally-bound stobjs; ~pl[note-2-6].

  Robert Krug has designed and implemented many changes in the
  vicinity of the linear arithmetic package and its connection to
  type-set and rewrite.  He was also instrumental in the development of
  ~il[extended-metafunctions].

  Pete Manolios has made numerous useful suggestions.  In particular, Pete
  helped us to organize the first workshop and was a wonderful equal partner
  with the two of us (Kaufmann and Moore) in producing the books that arose
  from that workshop.  Pete and his student, Daron Vroon, provided the current
  implementation of ~il[ordinals].

  We also thank the contributors to the ACL2 workshops for some
  suggested improvements and for the extensive collection of publicly
  distributed benchmark problems.  And we thank participants at the ACL2
  seminar at the University of Texas for useful feedback.

  ~em[Regarding the documentation:]

  ~bq[]
  Bill Young wrote significant portions of the ~il[ACL2-TUTORIAL]
  section of the ACL2 documentation, an important task for which we
  are grateful.  He, Bishop Brock, Rich Cohen, and Noah Friedman read
  over considerable amounts of the documentation, and made many useful
  comments.  Others, particularly Bill Bevier and John Cowles, have
  also made useful comments on the ~il[documentation].

  Art Flatau helped develop the ACL2 ~il[markup] language and translators
  from that language to Texinfo and HTML.  Michael ``Bogo'' Bogomolny
  created a search engine, beginning with Version  2.6, and for that
  purpose modified the HTML translator to create one file per topic (a
  good idea in any case).

  Laura Lawless provided many hours of help in marking up appropriate
  parts of the ~il[documentation] in typewriter font.

  Noah Friedman developed an Emacs tool that helped us insert
  ``invisible links'' into the ~il[documentation], which improve the
  usability of that documentation under HTML readers such as Mosaic.

  Richard Stallman contributed a texinfo patch, to be found in the
  file ~c[doc/texinfo.tex].~eq[]

  ")

(deflabel breaks
  :doc
  ":Doc-Section Miscellaneous

  Common Lisp breaks~/
  ~bv[]
  Example:
  Broken at PROVE.  Type :H for Help.
  >>:Q

  ACL2 !>
  ~ev[]~/

  You may interrupt the system by typing various control character sequences.
  The precise sequences are determined by the host Lisp and operating system
  environment.  For example, in GCL and Allegro Common Lisp, a console
  interrupt is caused by typing ``~c[ctrl-c]''.  If, however, the GCL or
  Allegro is running in an Emacs shell buffer, one must type ``ctrl-c ctrl-c''.

  If a break occurs, for example because of a bug in ACL2 or a user interrupt,
  the break will run a Common Lisp read-eval-print loop, not an ACL2
  read-eval-print loop.  This may not be obvious if the ~il[prompt]s in the two
  loops are similar.  Because you are typing to a Common Lisp evaluator, you
  must be careful.  It is possible to damage your ACL2 state in irreparable
  ways by executing non-ACL2 Common Lisp.  It is even possible to disrupt and
  render inaccurate the interrupted evaluation of a simple ACL2 expression.

  For ACL2 built on most host Common Lisps, you will see the string
  ~c[[RAW LISP~]] in the ~il[prompt] at a break, to emphasize that one is
  inside a break and hence should quit from the break.  For some host Common
  Lisps, the top-level prompt also contains the string ~c[[RAW LISP~]].
  ~l[prompt] for how to control printing of that string.

  The most reliable way to return to the ACL2 top level is by executing the
  following command: ~c[(]~ilc[abort!]~c[)].  Appropriate cleanup will then be
  done, which should leave you in an appropriate state.

  However, you may be able to quit from the break in the normal Lisp manner (as
  with ~c[:q] in GCL, ~c[:reset] in Allegro CL, and ~c[q] in CMU CL).  If this
  attempt to quit is successful, it will return you to the innermost ACL2
  read-eval-print loop, with appropriate cleanup performed first.  Note that if
  you are within a ~ilc[brr] environment when the break occurs, quitting from
  the break will only return you to that environment, not to the top of ACL2's
  read-eval-print loop.~/")

(deflabel saving-and-restoring
  :doc
  ":Doc-Section Miscellaneous

  saving and restoring your logical state~/

  One normally works on an ACL2-based project by developing ~il[books], which
  can then be included when continuing on that project in later ACL2 sessions;
  ~pl[include-book].  However, this approach can be time-consuming when there
  are very large collections of ~il[books] to be included.  See ~pl[save-exec]
  for the description of a utility that saves your ACL2 state so that you can
  immediately re-start later in that same state.~/~/")

(deflabel ordinals
  :doc
  ":Doc-Section Miscellaneous

  ordinals in ACL2~/

  Ordinals are used in ACL2 for proving termination in the admission of
  recursive function definitions.  For a proof that the ACL2 ordinals are
  well-founded, ~pl[proof-of-well-foundedness].

  The representation of ordinals changed in ACL2 Version_2.8, and is due to
  Pete Manolios and Daron Vroon.  They have also defined algorithms for ordinal
  arithmetic, created a library of theorems to reason about ordinal arithmetic,
  and written the rest of this documentation in order to explain this change.
  We thank them for their efforts.  Although they have provided the
  implementation and even modified the distributed books and workshop books as
  needed, we have looked over their work and are maintaining it (and this
  documentation); if there are any bugs, they should be considered ours (Matt
  Kaufmann and J Moore).

  A book is included for compatibility with the representation before
  Version_2.8.  For books that contain events relying on the previous ordinal
  implementation, insert the following lines before the first such event:
  ~bv[]
  (include-book \"ordinals/e0-ordinal\" :dir :system)
  (set-well-founded-relation e0-ord-<)
  ~ev[]

  The new ordinal representation is based on a slightly different version of
  Cantor Normal Form than that used by the old ordinals.  An advantage of the
  new representation is that it is exponentially more succinct than the old
  representation.

  While pre-Version_2.8 ACL2 versions provided built-in functions for checking
  if an object is an ordinal and for comparing two ordinals, they did not
  provide support for reasoning about and constructing ordinals.  The books in
  the directory ~c[books/ordinals] provide such support.  First, they provide
  efficient algorithms for ordinal arithmetic (including addition, subtraction,
  multiplication, and exponentiation).  The algorithms and their complexity are
  described in the following paper.
  ~bf[]
  Manolios, Panagiotis & Vroon, Daron. 
  Algorithms for ordinal arithmetic.
  Baader, Franz (ed),
  19th International Conference on Automated Deduction--CADE-19.
  Pages 243-257 of LNAI, vol. 2741.  Springer-Verlag.
  ~ef[]
  Second, the algorithms are mechanically verified and libraries of theorems
  which can be used to automate reasoning involving the ordinals are provided.
  For details, see the following paper.
  ~bf[]
  Manolios, Panagiotis & Vroon, Daron.
  Ordinal arithmetic in ACL2.
  Kaufmann, Matt, & Moore, J Strother (eds). 
  Fourth International Workshop on the ACL2 Theorem
  Prover and Its Applications (ACL2-2003),
  July, 2003.
  See URL ~c[http://www.cs.utexas.edu/users/moore/acl2/workshop-2003/].
  ~ef[]
  We now describe aspects of the above mentioned books in more detail.

  The new ordering function is ~ilc[o<] and the new ordinal recognizer is
  ~ilc[o-p].  See also ~ilc[natp], ~ilc[posp], ~ilc[o<=], ~ilc[o>], ~ilc[o>=],
  ~ilc[o-first-expt], ~ilc[o-first-coeff], ~ilc[o-rst], ~ilc[make-ord],
  ~ilc[o-finp], and ~ilc[o-infp]. ~/

  The old ordinals were based on the following formulation of Cantor Normal
  Form:

  For any ordinal, ~c[a < epsilon-0], there exist natural numbers ~c[p] and
  ~c[n], and ordinals ~c[a1 >= a2 >= ... >= an > 0] such that ~c[a > a1]
  and ~c[a = w^(a1) + w^(a2) + ... + w^(an) + p].

  Thus, a predicate recognizing ACL2's old ordinals is given by the following
  definition.
  ~bv[]
  (defun e0-ordinalp (x)
    (if (consp x)
        (and (e0-ordinalp (car x))
             (not (equal (car x) 0))
             (e0-ordinalp (cdr x))
             (or (atom (cdr x))
                 (not (e0-ord-< (car x) (cadr x)))))
      (and (integerp x)
           (>= x 0))))
  ~ev[]
  The new representation is based on a corollary to the above theorem, which we
  get by the left distributive property of ordinal multiplication over ordinal
  addition. Thus, ~c[w^a + w^a = (w^a)2], ~c[w^a + w^a + w^a = (w^a)3] and so
  forth. The corollary is as follows:

  For any ordinal, ~c[a < epsilon-0], there exist natural numbers ~c[p]
  and ~c[n], positive integers ~c[x1, x2, ..., xn] and ordinals
  ~c[a1 > a2 > ... > an > 0] such that ~c[a > a1] and 
  ~c[a = w^(a1)x1 + w^(a2)x2 + ... + w^(an)xn + p].

  Instead of representing an ordinal as a list of non-increasing ordinals, we
  represent it as a list of exponent-coefficient pairs, such that the exponents
  are strictly decreasing (~pl[o-p]).  Note that this representation is
  exponentially more efficient than the old representation.

  The ordinal arithmetic functions: ~c[o+], ~c[o-], ~c[o*], and ~c[o^] are
  defined in the ordinals library (in the subdirectory ~c[books/ordinals]). To
  use them, include the book ~c[ordinals-without-arithmetic] or ~c[ordinals],
  depending on whether you want the arithmetic books included or not
  (~c[ordinals] includes ~c[books/arithmetic/top-with-meta]). To use the old
  ordinals, include the book ~c[e0-ordinal] and run the command
  ~c[(set-well-founded-relation e0-ord-<)]

  The file ~c[books/arithmetic/natp-posp] is a book for reasoning
  about ~c[posp] and ~c[natp].  We recommend using this book if you
  have to reason about ~c[posp] and ~c[natp].  It is included in
  ~c[books/arithmetic/top], which is included in
  ~c[books/arithmetic/top-with-meta], which is included in
  ~c[books/ordinals/ordinals].

  If you have a good reason to use the old definitions of the ordinals (e.g.,
  because of legacy code and theorems), then we provide a convenient way to do
  this.  In the book ~c[ordinal-isomorphism] we prove that the new ordinals are
  order-isomorphic to the old ordinals and thus theorems proved in one context
  can be directly transferred to the other.  For an example of how to do this,
  look at the book ~c[defmul] in the directory
  ~c[books/workshops/2000/ruiz/multiset].

  The ordinals books have been used to prove non-trivial theorems.  For a good
  example, see the books in the directory
  ~c[books/workshops/2003/sustik/support], where Matyas Sustik proves Dickson's
  lemma.

  Finally, many termination proofs can be carried out with weaker orderings
  than the ordinals up to ~c[epsilon-0].  For example, many inductive theorem
  provers only know that the lexicographic ordering on natural numbers is
  well-founded.  The book ~c[lexicographic-ordering] contains a definition of
  such an ordering ~c[l<] whose arguments are either a list of natural numbers,
  or a natural number.  In the book we prove that ~c[l<] is well-founded (that
  is, we prove a ~c[:well-founded-relation] ~ilc[defthm] and provide a macro
  ~c[llist] to simplify the generation of measure functions.  We also show how
  to use ~c[l<] to prove that the famous Ackermann function terminates.
  Finally, since ~c[l<] does something reasonable with natural numbers, it gets
  along with ~ilc[acl2-count], the default measure chosen by ACL2.")

(deflabel release-notes
  :doc
  ":Doc-Section release-notes

  pointers to what has changed~/~/

  This section of the online ~il[documentation] contains notes on the
  changes that distinguish successive released versions of ACL2.

  The current version of ACL2 is the value of the constant
  ~c[(@ acl2-version)].")

(deflabel note1
  :doc
  ":Doc-Section release-notes

  Acl2 Version 1.1 Notes~/

  The new features are extensively documented.  The relevant topics
  are:~/

  It is especially important to read all of of the ~il[documentation] for
  ~il[books] before trying to use books.  However, the new ~c[:]~ilc[more]
  keyword command is so handy for reading long ~il[documentation] strings
  that we recommend you start with ~c[:]~ilc[doc] more if reading at the
  terminal.  Some documentation has been written for ~il[guard]s which
  you might find interesting.~/

  :cite books
  :cite more
  :cite guard
  :cite redundant-events")

(deflabel note2
  :doc
  ":Doc-Section release-notes

  Acl2 Version 1.2 Notes~/

  Hacker mode has been eliminated and ~il[programming] mode has been added.
  ~il[Programming] mode is unsound but does syntax checking and permits
  redefinitions of names.  See ~c[:]~ilc[doc] ~c[load-mode] and ~c[:]~ilc[doc] ~c[g-mode].

  The arguments to ~ilc[ld] have changed.  ~ilc[Ld] is now much more
  sophisticated.  ~l[ld].

  For those occasions on which you wish to look at a large list
  structure that you are afraid to print, try ~c[(walkabout x state)],
  where ~c[x] is an Acl2 expression that evaluates to the structure in
  question.  I am afraid there is no ~il[documentation] yet, but it is
  similar in spirit to the Interlisp structure editor.  You are
  standing on an object and commands move you around in it.  E.g., 1
  moves you to its first element, 2 to its second, etc.; 0 moves you
  up to its parent; ~c[nx] and ~c[bk] move you to its next sibling and
  previous sibling; ~c[pp] prettyprints it; ~ilc[q] exits returning ~c[nil]; ~ilc[=] exits
  returning the thing you're standing on; ~c[(= symb)] assigns the thing
  you're standing on to the ~il[state] global variable ~c[symb].

  Several new ~il[hints] have been implemented, including ~c[:by] and ~c[:do-not].
  The old ~c[:do-not-generalize] has been scrapped in favor of such new
  ~il[hints] as ~c[:do-not] ~c[(generalize elim)].  ~c[:By] lets you say ``this goal is
  subsumed by'' a given lemma instance.  The ~c[:by] hint also lets you
  say ``this goal can't be proved yet but skip it and see how the rest
  of the proof goes.'' ~l[hints].~/

  ")

(deflabel note3
  :doc
  ":Doc-Section release-notes

  Acl2 Version 1.3 Notes~/

  ~il[Programming] mode has been eliminated.  Instead, all functions have a
  ``color'' which indicates what can be done with the function.  For
  example, ~c[:red] functions can be executed but have no axioms
  describing them.  Thus, ~c[:red] functions can be introduced after
  passing a simple syntactic check and they can be redefined without
  undoing.  But nothing of consequence can be proved about them.  At
  the other extreme are ~c[:gold] functions which can be executed and
  which also have passed both the termination and the ~il[guard]
  verification proofs.  The color of a function can be specified with
  the new ~ilc[xargs] keyword, ~c[:color], which, if omitted defaults to the
  global setting of ~c[ld-color].  ~c[Ld-color] replaces ~c[load-mode].  Setting
  ~c[ld-color] to ~c[:red] causes behavior similar to the old ~c[:g-mode].
  Setting ~c[ld-color] to ~c[:gold] causes behavior similar to the old
  ~c[:v-mode].  It is possible to prototype your system in ~c[:red] and then
  convert ~c[:red] functions to :~c[blue] individually by calling
  ~ilc[verify-termination] on them.  They can then be converted to ~c[:gold]
  with ~ilc[verify-guards].  This allows us to undertake to verify the
  termination and ~il[guard]s of system functions.  See ~c[:]~ilc[doc] color for an
  introduction to the use of colors.

  Type prescription rules have been added.  Recall that in Nqthm, some
  ~ilc[rewrite] rules were actually stored as ``~il[type-prescription]s.''  Such
  rules allow the user to inform Nqthm's primitive type mechanism as
  to the kinds of shells returned by a function.  Earlier versions of
  Acl2 did not have an analogous kind of rule because Acl2's type
  mechanism is complicated by ~il[guard]s.  Version 1.3 supports
  ~ilc[type-prescription] rules.  ~l[type-prescription].

  Three more new ~il[rule-classes] implement congruence-based rewriting.
  It is possible to identify a binary relation as an equivalence
  relation (~pl[equivalence]), to show that one equivalence
  relation refines another (~pl[refinement]) and to show that a
  given equivalence relation is maintained when rewriting a given
  function call, e.g., ~c[(fn ...xk...)], by maintaining another
  equivalence relation while rewriting the ~c[k]th argument
  (~pl[congruence]).  If ~c[r] has been shown to be an ~il[equivalence]
  relation and then ~c[(implies hyps (r (foo x) (bar x)))] is proved as a
  ~c[:]~ilc[rewrite] rule, then instances of ~c[(foo x)] will be replaced by
  corresponding instances of ~c[(bar x)] provided the instance occurs in a
  slot where the maintainence of ~c[r-equivalence] is known to be
  sufficient and ~c[hyps] can be established as usual.

  In Version 1.2, ~il[rule-classes] were simple keywords, e.g., ~c[:]~ilc[rewrite] or
  ~c[:]~ilc[elim].  In Version 1.3, ~il[rule-classes] have been elaborated to allow
  you to specify how the theorem ought to be used as a rule.  That is,
  the new ~il[rule-classes] allows you to separate the mathematical
  statement of the formula from its interpretation as a rule.
  ~l[rule-classes].

  Rules used to be named by symbols, e.g., ~ilc[car] and ~c[car-cons] were the
  names of rules.  Unfortunately, this was ambiguous because there are
  three rules associated with function symbols: the symbolic
  definition, the executable counterpart, and the ~il[type-prescription];
  many different rules might be associated with theorems, depending on
  the rule classes.  In Version 1.3 rules are named by ``~il[rune]s''
  (which is just short hand for ``rule names'').  Example ~il[rune]s are
  ~c[(:definition car)], ~c[(:executable-counterpart car)], and
  ~c[(:type-prescription car . 1)].  Every rule added by an event has a
  different name and you can ~il[enable] and ~il[disable] them independently.
  ~l[rune] and ~pl[theories].

  The identity function ~ilc[force], of one argument, has been added and
  given a special interpretation by the functions responsible for
  establishing hypotheses in backchaining: When the system fails to
  establish some hypothesis of the form ~c[(force term)], it simply
  assumes it is true and goes on, delaying until later the
  establishment of term.  In particular, pushes a new subgoal to prove
  term in the current context.  When that subgoal is attacked, all of
  the resources of the theorem prover, not just rewriting, are brought
  to bear.  Thus, for example, if you wish to prove the rule
  ~c[(implies (good-statep s) (equal (exec s n) s'))] and it is your
  expectation that every time ~c[exec] appears its first argument is a
  ~c[good-statep] then you might write the rule as
  ~c[(implies (force (good-statep s)) (equal (exec s n) s'))].  This
  rule is essentially an unconditional rewrite of ~c[(exec s n)] to
  ~c[s'] that spawns the new goal ~c[(good-statep s)].  ~l[force].
  Because you can now specify independently how a theorem is used as a
  rule, you need not write the ~ilc[force] in the actual theorem proved.
  ~l[rule-classes].

  Version 1.3 supports a facility similar to Nqthm's ~ilc[break-lemma].
  ~l[break-rewrite].  You can install ``~il[monitor]s'' on ~il[rune]s that
  will cause interactive breaks under certain conditions.

  Acl2 also provides ``~il[wormhole]s'' which allow you to write functions
  that cause interaction with the user but which do not require that
  you have access to ~ilc[state].  ~l[wormhole].

  The rewriter now automatically backchains to stronger recognizers.
  There is no user hook to this feature but it may simplify some
  proofs with which older versions of Acl2 had trouble.  For example,
  if the rewriter is trying to prove ~c[(rationalp (foo a b c))] it is now
  smart enough to try lemmas that match with ~c[(integerp (foo a b c))].~/

  ")

(deflabel note4
  :doc
  ":Doc-Section release-notes

  Acl2 Version 1.4 Notes~/

  Once again ~ilc[ld] only takes one required argument, as the ~c[bind-flg] has
  been deleted.

  Three commands have been added in the spirit of ~c[:]~ilc[pe].  ~c[:]~ilc[Pe!] is
  similar to ~c[:]~ilc[pe] but it prints all ~il[events] with the given name, rather
  than just the most recent.  The command ~c[:]~ilc[pf] prints the corollary
  formula corresponding to a name or ~il[rune].  The command ~c[:]~ilc[pl] (print
  lemmas) prints rules whose top function symbol is the given name.
  ~l[pe!], ~pl[pf], and ~pl[pl].

  Book naming conventions have been changed somewhat.  The
  once-required ~c[.lisp] extension is now prohibited!  Directories are
  supported, including a notion of ``connected book directory''.
  ~l[book-name].  Also, the second argument of ~ilc[certify-book] is
  now optional, defaulting to ~c[0].

  ~il[Compilation] is now supported inside the Acl2 loop.  ~l[comp]
  and ~pl[set-compile-fns].

  The default color is now part of the Acl2 ~il[world];
  see ~c[:]~ilc[doc] ~c[default-color].  ~c[Ld-color] is no longer an ~ilc[ld] special.
  Instead, colors are ~il[events]; see the documentation for ~c[red],
  ~c[pink], ~c[blue], and ~c[gold].

  A ~il[table] exists for controlling whether Acl2 prints comments when it
  ~il[force]s hypotheses of rules; see ~c[:]~ilc[doc] ~c[force-table].  Also, it is now
  possible to turn off the forcing of assumptions by disabling the
  definition of ~il[force]; ~pl[force].

  The event ~c[defconstant] is no longer supported, but a very similar
  event, ~ilc[defconst], has been provided in its place.  ~l[defconst].

  The event for defining ~il[congruence] relations is now ~ilc[defcong]
  (formerly, ~c[defcon]).

  Patterns are now allowed in ~c[:expand] ~il[hints].  See the documentation
  for ~c[:expand] inside the documentation for ~il[hints].

  We have improved the way we report rules used by the simplifier.
  All ~il[rune]s of the same type are reported together in the running
  commentary associated with each goal, so that for example,
  executable counterparts are listed separately from definitions, and
  rewrite rules are listed separately from ~il[linear] rules.  The
  preprocessor now mentions ``simple'' rules; ~pl[simple].

  The mechanism for printing warning messages for new rewrite rules,
  related to subsumption, now avoids worrying about nonrecursive
  function symbols when those symbols are ~il[disable]d.  These messages
  have also been eliminated for the case where the old rule is a
  ~c[:]~ilc[definition] rule.

  Backquote has been modified so that it can usually provide
  predictable results when used on the left side of a rewrite rule.

  Time statistics are now printed even when an event fails.

  The Acl2 trace package has been modified so that it prints using the
  values of the Lisp globals ~c[*print-level*] and ~c[*print-length*]
  (respectively).

  ~il[Table] has been modified so that the ~c[:clear] option lets you replace
  the entire ~il[table] with one that satisfies the ~c[val] and key guards (if
  any); ~pl[table].

  We have relaxed the translation rules for ~c[:measure] ~il[hints] to ~ilc[defun],
  so that the the same rules apply to these terms that apply to terms
  in ~ilc[defthm] ~il[events].  In particular, in ~c[:measure] ~il[hints] ~ilc[mv] is treated
  just like ~ilc[list], and ~ilc[state] receives no special handling.

  The ~il[loop-stopper] test has been relaxed.  The old test required that
  every new argument be strictly less than the corresponding old
  argument in a certain ~il[term-order].  The new test uses a lexicographic
  order on term lists instead.  For example, consider the following
  rewrite rule.
  ~bv[]
    (equal
     (variable-update var1
                      val1 (variable-update var2 val2 vs))
     (variable-update var2
                      val2 (variable-update var1 val1 vs)))
  ~ev[]
  This rule is permutative.  Now imagine that we want to apply this
  rule to the term
  ~bv[]
    (variable-update u y (variable-update u x vs)).
  ~ev[]
  Since the actual corresponding to both ~c[var1] and ~c[var2] is ~c[u], which
  is not strictly less than itself in the ~il[term-order], this rule would
  fail to be applied in this situation when using the old test.
  However, since the pair ~c[(u x)] is lexicographically less than the
  pair ~c[(u y)] with respect to our ~il[term-order], the rule is in fact
  applied using our new test.

  Messages about ~il[events] now contain a space after certain left
  parentheses, in order to assist emacs users.  For example, the event
  ~bv[]
    (defthm abc (equal (+ (len x) 0) (len x)))
  ~ev[]
  leads to a summary containing the line
  ~bv[]
    Form:  ( DEFTHM ABC ...)
  ~ev[]
  and hence, if you search backwards for ``~c[(defthm abc]'', you won't
  stop at this message.

  More tautology checking is done during a proof; in fact, no goal
  printed to the screen, except for the results of applying ~c[:use] and
  ~c[:by] ~il[hints] or the top-level goals from an induction proof, are known
  to Acl2 to be tautologies.

  The ~ilc[ld-query-control-alist] may now be used to suppress printing of
  queries; ~pl[ld-query-control-alist].

  Warning messages are printed with short summary strings, for example
  the string ``~c[Use]'' in the following message.
  ~bv[]
    Acl2 Warning [Use] in DEFTHM:  It is unusual to :USE an enabled
    :REWRITE or :DEFINITION rule, so you may want to consider
    disabling FOO.
  ~ev[]
  At the end of the event, just before the time is printed, all such
  summary strings are printed out.

  The keyword command ~c[:u] has been introduced as an abbreviation for
  ~c[:]~ilc[ubt] ~c[:]~ilc[max].  Printing of query messages is suppressed by ~c[:u].

  The keyword ~c[:cheat] is no longer supported by any event form.

  Some irrelevant formals are detected; ~pl[irrelevant-formals].

  A bug in the application of metafunctions was fixed: now if the
  output of a metafunction is equal to its input, the application of
  the metafunction is deemed unsuccessful and the next metafunction is
  tried.

  An example has been added to the documentation for ~il[equivalence]
  to suggest how to make use of ~il[equivalence] relations in rewriting.

  The following Common Lisp functions have been added to Acl2:
  ~ilc[alpha-char-p], ~ilc[upper-case-p], ~ilc[lower-case-p], ~ilc[char-upcase],
  ~ilc[char-downcase], ~ilc[string-downcase], ~ilc[string-upcase], and ~c[digit-charp-p].

  A documentation section called ~ilc[proof-checker] has been added for the
  interactive facility, whose documentation has been slightly
  improved.  See in particular the documentation for
  ~il[proof-checker], ~ilc[verify], and ~il[macro-command].

  A number of ~il[events] that had been inadvertently disallowed in ~il[books]
  are now permitted in ~il[books].  These are:  ~ilc[defcong], ~c[defcor], ~ilc[defequiv],
  ~ilc[defrefinement], ~ilc[defstub], and ~ilc[verify-termination].

  ~/

  ")

(deflabel note5
  :doc
  ":Doc-Section release-notes

  Acl2 Version 1.5 Notes~/

  Acl2 now allows ``complex rationals,'' which are complex numbers
  whose real parts are rationals and whose imaginary parts are
  non-zero rationals.  ~l[complex].

  A new way of handling ~ilc[force]d hypotheses has been implemented.
  Rather than cause a case split at the time the ~ilc[force] occurs, we
  complete the main proof and then embark on one or more ``forcing
  rounds'' in which we try to prove the ~il[force]d hypotheses.
  ~l[forcing-round].  To allow us to compare the new handling of
  ~ilc[force] with the old, Version 1.5 implements both and uses a flag in
  ~ilc[state] to determine which method should be used.  Do
  ~c[(assign old-style-forcing t)] if you want ~ilc[force] to be handled
  as it was in Version 1.4.  However, we expect to eliminate the
  old-style forcing eventually because we think the new style is more
  effective.  To see the difference between the two approaches to
  forcing, try proving the associativity of ~il[append] under both settings
  of ~c[old-style-forcing].  To get the new behavior invoke:
  ~bv[]
  (thm (implies (and (true-listp a) (true-listp b))
                (equal (append (append a b) c)
                       (append a (append b c)))))
  ~ev[]
  Then ~c[(assign old-style-forcing t)] and invoke the ~c[thm] ~il[command] above
  again.

  A new ~c[:cases] ~il[hints] allows proof by cases.  ~l[hints].

  ~ilc[Include-book] and ~ilc[encapsulate] now restore the ~ilc[acl2-defaults-table]
  when they complete.  ~l[include-book] and ~pl[encapsulate].

  The ~il[guard]s on many Acl2 primitives defined in ~c[axioms.lisp] have been
  weakened to permit them to be used in accordance with lisp custom
  and tradition.

  It is possible to attach heuristic filters to ~c[:]~ilc[rewrite] rules to
  limit their applicability.  ~l[syntaxp].

  A tutorial has been added; ~pl[acl2-tutorial].

  ~il[Events] now print the Summary paragraph listing ~il[rune]s used, time,
  etc., whether they succeed or fail.  The format of the ``~il[failure]
  banner'' has been changed but still has multiple asterisks in it.
  ~c[Thm] also prints a Summary, whether it succeeds or fails; but ~c[thm] is
  not an event.

  A new event form ~ilc[skip-proofs] has been added; ~pl[skip-proofs].

  A user-specific customization facility has been added in the form of
  a book that is automatically included, if it exists on the current
  directory.  ~l[acl2-customization].

  A facility for conditional metalemmas has been implemented;
  ~pl[meta].

  The acceptable values for ~ilc[ld-skip-proofsp] have changed.  In the old
  version (Version 1.4), a value of ~c[t] meant that proofs and ~ilc[local]
  ~il[events] are to be skipped.  In Version 1.5, a value of ~c[t] means proofs
  (but not ~ilc[local] ~il[events]) are to be skipped.  A value of ~c[']~ilc[include-book]
  means proofs and ~ilc[local] ~il[events] are to be skipped.  There are two
  other, more obscure, acceptable values.  ~l[ld-skip-proofsp].

  In order to turn off the forcing of assumptions, one should now
  ~il[disable] the ~c[:]~ilc[executable-counterpart] of ~ilc[force] (rather than the
  ~c[:]~ilc[definition] of ~ilc[force], as in the previous release); ~pl[force].

  The macros ~ilc[enable-forcing] and ~ilc[disable-forcing] make it convenient to
  ~il[enable] or ~il[disable] forcing.  ~l[enable-forcing] and
  ~pl[disable-forcing].

  The new commands ~c[:]~ilc[pr] and ~c[:]~ilc[pr!] print the rules created by an event or
  command.  ~l[pr] and ~pl[pr!].

  The new ~il[history] ~il[command]s ~c[:]~ilc[puff] and ~c[:]~ilc[puff*] will replace a compound
  ~il[command] such as an ~ilc[encapsulate] or ~ilc[include-book] by the sequence of
  ~il[events] in it.  That is, they ``~il[puff] up'' or ``lift'' the subevents
  of a ~il[command] to the ~il[command] level, eliminating the formerly superior
  ~il[command] and lengthening the ~il[history].  This is useful if you want to
  ``partially undo'' an ~ilc[encapsulate] or book or other compound ~il[command]
  so you can experiment.  ~l[puff] and ~pl[puff*].

  Theory expressions now are allowed to use the free variable ~ilc[world]
  and prohibited from using the free variable ~ilc[state].
  ~l[theories], although it is essentially the same as before
  except it mentions ~ilc[world] instead of ~ilc[state].  ~l[world] for a
  discussion of the Acl2 logical ~il[world].  Allowing ~ilc[in-theory] ~il[events] to
  be state-sensitive violated an important invariant about how ~il[books]
  behaved.

  ~ilc[Table] keys and values now are allowed to use the free variable ~ilc[world]
  and prohibited from using the free variable ~ilc[state].  See the note
  above about theory expressions for some explanation.

  The macro for minus, ~ilc[-], used to expand ~c[(- x 3)] to ~c[(+ x -3)] and now
  expands it to ~c[(+ -3 x)] instead.  The old macro, if used in the
  left-hand sides of rewrite rules, produced inapplicable rules
  because the constant occurs in the second argument of the ~ilc[+], but
  potential target terms generally had the constant in the first
  argument position because of the effect of ~c[commutativity-of-+].

  A new class of rule, ~c[:linear-alias] rules, allows one to implement
  the nqthm package and similar hacks in which a ~il[disable]d function is
  to be known equivalent to an arithmetic function.

  A new class of rule, ~c[:built-in-clause] rules, allows one to extend
  the set of clauses proved silently by ~ilc[defun] during measure and ~il[guard]
  processing.  ~l[built-in-clauses].

  The new command ~ilc[pcb!] is like ~ilc[pcb] but sketches the ~il[command] and then
  prints its subsidiary ~il[events] in full.  ~l[pcb!].

  ~c[:]~ilc[Rewrite] class rules may now specify the ~c[:]~ilc[loop-stopper] field.
  ~l[rule-classes] and ~pl[loop-stopper].

  The rules for how ~il[loop-stopper]s control permutative rewrite rules
  have been changed.  One effect of this change is that now when the
  built-in commutativity rules for ~ilc[+] are used, the terms ~c[a] and ~c[(- a)]
  are permuted into adjacency.  For example, ~c[(+ a b (- a))] is now
  normalized by the commutativity rules to ~c[(+ a (- a) b)]; in Version
  1.4, ~c[b] was considered syntactically smaller than ~c[(- a)] and so
  ~c[(+ a b (- a))] is considered to be in normal form.  Now it is
  possible to arrange for unary functions be be considered
  ``invisible'' when they are used in certain contexts.  By default,
  ~ilc[unary--] is considered invisible when its application appears in
  the argument list of ~ilc[binary-+].  ~l[loop-stopper] and
  see :DOC set-invisible-fns-table.

  Extensive documentation has been provided on the topic of Acl2's
  ``term ordering.''  ~l[term-order].

  Calls of ~ilc[ld] now default ~ilc[ld-error-action] to ~c[:return] rather than to
  the current setting.

  The ~il[command] descriptor ~c[:x] has been introduced and is synonymous with
  ~c[:]~ilc[max], the most recently executed ~il[command].  ~il[History] ~il[command]s such as
  ~c[:]~ilc[pbt] print a ~c[:x] beside the most recent ~il[command], simply to indicate
  that it ~st[is] the most recent one.

  The ~il[command] descriptor ~c[:x-23] is synonymous with ~c[(:x -23)].  More
  generally, every symbol in the keyword package whose first character
  is ~c[#\\x] and whose remaining ~il[characters] parse as a negative integer
  is appropriately understood.  This allows ~c[:]~ilc[pbt] ~c[:x-10] where ~c[:]~ilc[pbt]
  ~c[(:max -10)] or ~c[:]~ilc[pbt] ~c[(:here -10)] were previously used.  The old forms
  are still legal.

  The order of the arguments to ~ilc[defcong] has been changed.

  The simplifier now reports the use of unspecified built-in type
  information about the primitives with the phrase ``primitive type
  reasoning.''  This phrase may sometimes occur in situations where
  ``propositional calculus'' was formerly credited with the proof.

  The function ~ilc[pairlis] has been replaced in the code by a new function
  ~ilc[pairlis$], because Common Lisp does not adequately specify its
  ~ilc[pairlis] function.

  Some new Common Lisp functions have been added, including ~ilc[logtest],
  ~ilc[logcount], ~ilc[integer-length], ~ilc[make-list], ~ilc[remove-duplicates], ~ilc[string], and
  ~ilc[concatenate].  The source file ~c[/slocal/src/acl2/axioms.lisp] is the
  ultimate reference regarding Common Lisp functions in Acl2.

  The functions ~ilc[defuns] and ~ilc[theory-invariant] have been documented.
  ~l[defuns] and ~pl[theory-invariant].

  A few symbols have been added to the list ~c[*acl2-exports*].

  A new key has been implemented for the ~ilc[acl2-defaults-table],
  ~c[:irrelevant-formals-ok].  ~l[set-irrelevant-formals-ok].

  The connected book directory, ~ilc[cbd], must be nonempty and begin and
  end with a slash.  It is set (and displayed) automatically upon your
  first entry to ~ilc[lp].  You may change the setting with ~ilc[set-cbd].
  ~l[cbd].

  ~c[:]~ilc[oops] will undo the last ~c[:]~ilc[ubt].  ~l[oops].

  Documentation has been written about the ordinals.  See :DOC ~c[e0-ordinalp]
  and see :DOC ~c[e0-ord-<].  [Note added later: Starting with Version_2.8,
  instead ~pl[o-p] and ~pl[o<].~/

  The color ~il[events] ~-[] (red), (pink), (blue), and (gold) ~-[] may no
  longer be enclosed inside calls of ~ilc[local], for soundness reasons.  In
  fact, neither may any event that sets the ~ilc[acl2-defaults-table].
  ~l[embedded-event-form].

  ~l[ld-keyword-aliases] for an example of how to change the exit
  keyword from ~c[:]~ilc[q] to something else.

  The attempt to install a ~il[monitor] on ~c[:]~ilc[rewrite] rules stored as simple
  abbreviations now causes an error because the application of
  abbreviations is not tracked.

  A new message is sometimes printed by the theorem prover, indicating
  that a given simplification is ``specious'' because the subgoals it
  produces include the input goal.  In Version 1.4 this was detected
  but not reported, causing behavior some users found bizarre.
  ~l[specious-simplification].

  ~c[:]~ilc[Definition] rules are no longer always required to specify the
  ~c[:clique] and ~c[:controller-alist] fields; those fields can be defaulted
  to system-determined values in many common instances.
  ~l[definition].

  A warning is printed if a macro form with keyword arguments is given
  duplicate keyword values.  Execute ~c[(thm t :doc nil :doc \"ignored\")]
  and read the warning printed.

  A new restriction has been placed on ~ilc[encapsulate].  Non-~ilc[local]
  recursive definitions inside the ~ilc[encapsulate] may not use, in their
  tests and recursive calls, the constrained functions introduced by
  the ~ilc[encapsulate].  ~l[subversive-recursions].  (Note added in
  Version  2.3:  Subversive recursions were first recognized by us here
  in Version 1.5, but our code for recognizing them was faulty and the
  bug was not fixed until Version  2.3.) 

  The ~il[events] ~ilc[defequiv], ~ilc[defcong], ~ilc[defrefinement], and ~ilc[defevaluator] have
  been reimplemented so that they are just macros that expand into
  appropriate ~ilc[defthm] or ~ilc[encapsulate] ~il[events]; they are no longer
  primitive ~il[events].  See the ~il[documentation] of each affected event.

  The ~c[defcor] event, which was a shorthand for a ~ilc[defthm] that
  established a ~il[corollary] of a named, previously proved event, has
  been eliminated because its implementation relied on a technique we
  have decided to ban from our code.  If you want the effect of a
  ~c[defcor] in Version 1.5 you must submit the corresponding ~ilc[defthm] with
  a ~c[:by] hint naming the previously proved event.

  Error reporting has been improved for inappropriate ~ilc[in-theory] ~il[hints]
  and ~il[events], and for syntax errors in rule classes, and for
  non-existent filename arguments to ~ilc[ld].

  Technical Note:  We now maintain the Third Invariant on ~c[type-alists],
  as described in the Essay on the Invariants on Type-alists, and
  Canonicality.  This change will affect some proofs, for example, by
  causing a to rewrite more quickly to ~c[c] when ~c[(equiv a b)] and
  ~c[(equiv b c)] are both known and ~c[c] is the canonical
  representative of the three.

  ~/

  ")

(deflabel note6
  :doc
  ":Doc-Section release-notes

  Acl2 Version 1.6 Notes~/

  A new key has been implemented for the ~ilc[acl2-defaults-table],
  ~c[:ignore-ok].  ~l[set-ignore-ok].

  It is now legal to have color ~il[events], such as ~c[(red)], in the
  ~il[portcullis] of a book.  More generally, it is legal to set the
  ~ilc[acl2-defaults-table] in the ~il[portcullis] of a book.  For example, if
  you execute ~c[:red] and then certify a book, the event ~c[(red)] will show
  up in the ~il[portcullis] of that book, and hence the definitions in that
  book will all be red (except when overridden by appropriate
  declarations or ~il[events]).  When that book is included, then as
  always, its ~il[portcullis] must first be ``raised,'' and that will cause
  the default color to become red before the ~il[events] in the book are
  executed.  As always, the value of ~ilc[acl2-defaults-table] immediately
  after execution of an ~ilc[include-book], ~ilc[certify-book], or ~ilc[encapsulate]
  form will be the same as it was immediately before execution (and
  hence, so will the default color).  ~l[portcullis] and, for
  more about books, ~pl[books].

  A theory ~ilc[ground-zero] has been defined to contain exactly those rules
  that are ~il[enable]d when Acl2 starts up.  ~l[ground-zero].

  The function ~ilc[nth] is now ~il[enable]d, correcting an oversight from
  Version 1.5.

  Customization files no longer need to meet the syntactic
  restrictions put on ~il[books]; rather, they can contain arbitrary Acl2
  forms.  ~l[acl2-customization].

  Structured directory names and structured file names are supported;
  see especially the documentation for ~il[pathname], ~il[book-name],
  and ~ilc[cbd].

  Acl2 now works with some Common Lisp implementations other than
  akcl, including Lucid, Allegro, and MCL.

  A facility has been added for displaying proof trees, especially
  using emacs; ~pl[proof-tree].

  There is a considerable amount of new ~il[documentation], in particular
  for the printing functions ~ilc[fmt], ~ilc[fmt1], and ~ilc[fms], and for the notion of
  Acl2 term (~pl[term]).

  It is possible to introduce new well-founded relations, to specify
  which relation should be used by ~ilc[defun], and to set a default
  relation.  ~l[well-founded-relation].

  It is possible to make functions suggest new inductions.
  ~l[induction].

  It is possible to change how Acl2 expresses ~il[type-set] information; in
  particular, this affects what clauses are proved when ~il[force]d
  assumptions are generated.  ~l[type-set-inverter].

  A new restriction has been added to ~ilc[defpkg], having to do with
  undoing.  If you undo a ~ilc[defpkg] and define the same package name
  again, the imports list must be identical to the previous imports or
  else an explanatory error will occur.
  ~l[package-reincarnation-import-restrictions].

  ~ilc[Theory-invariant] and ~ilc[set-irrelevant-formals-ok] are now embedded
  event forms.

  The command ~c[:]~ilc[good-bye] may now be used to quit entirely out of Lisp,
  thus losing your work forever.  This command works in akcl but may
  not work in every Common Lisp.

  A theory ~ilc[ground-zero] has been added that contains exactly the
  ~il[enable]d rules in the ~il[startup] theory.  ~l[ground-zero].

  ~c[Define-pc-macro] and ~c[define-pc-atomic-macro] now automatically define
  ~c[:red] functions.  (It used to be necessary, in general, to change
  color to ~c[:red] before invoking these.)

  ~/

  For a proof of the well-foundedness of ~c[e0-ord-<] on the ~c[e0-ordinalp]s,
  ~pl[proof-of-well-foundedness].  [Note added later: Starting with
  Version_2.8, ~ilc[o<] and ~ilc[o-p] replace ~c[e0-ord-<] and ~c[e0-ordinalp],
  respectively.]

  Free variables are now handled properly for hypotheses of
  ~c[:]~ilc[type-prescription] rules.

  When the system is loaded or saved, ~ilc[state] is now bound to
  ~c[*the-live-state*].

  ~ilc[Certify-book] has been modified so that when it compiles a file, it
  loads that object file.

  ~ilc[Defstub] has been modified so that it works when the color is hot
  (~c[:red] or ~c[:pink]).

  Several basic, but not particularly commonly used, ~il[events] have been
  added or changed.  The obscure axiom ~c[symbol-name-intern] has been
  modified.  The definition of ~c[firstn] has been changed.  ~ilc[Butlast] is
  now defined.  The definition of ~ilc[integer-length] has been modified.
  The left-hand side of the rewrite rule ~c[rational-implies2] has been
  changed from ~c[(* (numerator x) (/ (denominator x)))] to
  ~c[(* (/ (denominator x)) (numerator x))], in order to respect the
  fact that ~ilc[unary-/] is invisible with respect to ~ilc[binary-*].
  ~l[loop-stopper].

  The `preprocess' process in the waterfall (~pl[hints] for a
  discussion of the ~c[:do-not] hint) has been changed so that it works to
  avoid case-splitting.  The `simplify' process refuses to force
  (~pl[force]) when there are ~ilc[if] terms, including ~ilc[and] and ~ilc[or]
  terms, in the goal being simplified.

  The function ~c[apply] is no longer introduced automatically by
  translation of user input to internal form when functions are called
  on inappropriate explicit values, e.g., ~c[(car 3)].

  The choice of which variable to use as the measured variable in a
  recursive definition has been very slightly changed.

  ~/

  ")

(deflabel note7
  :doc
  ":Doc-Section release-notes

  ACL2 Version 1.7 (released October 1994) Notes~/

  ~ilc[Include-book] now takes (optionally) an additional keyword
  argument, indicating whether a compiled file is to be loaded.  The
  default behavior is unchanged, except that a warning is printed when
  a compiled file is not loaded.  ~l[include-book].

  A ~il[markup] language for ~il[documentation] strings has been implemented,
  and many of the source files have been marked up using this language
  (thanks largely to the efforts of Laura Lawless).  ~l[markup].
  Moreover, there are translators that we have used to provide
  versions of the ACL2 ~il[documentation] in info (for use in emacs), html
  (for Mosaic), and tex (for hardcopy) formats.

  A new event ~ilc[defdoc] has been implemented.  It is like ~ilc[deflabel],
  but allows redefinition of ~il[doc] strings and has other advantages.
  ~l[defdoc].

  We used to ignore corollaries when collecting up the axioms
  introduced about constrained functions.  That bug has been fixed.
  We thank John Cowles for bringing this bug to our attention.

  The macro ~ilc[defstub] now allows a ~c[:]~ilc[doc] keyword argument, so that
  ~il[documentation] may be attached to the name being introduced.

  A new command ~ilc[nqthm-to-acl2] has been added to help Nqthm users to
  make the transition to ACL2.  ~l[nqthm-to-acl2], which also
  includes a complete listing of the relevant tables.

  Many function names, especially of the form ``foo~c[-lst]'', have been
  changed in order to support the following convention, for any
  ``foo'':
  ~bf[]
  ~c[(foo-listp lst)] represents the notion ~c[(for x in lst always foop x)].
  ~ef[]
  A complete list of these changes may be found at the end of this
  note.  All of them except ~c[symbolp-listp] and
  ~c[list-of-symbolp-listp] have the string ``~c[-lst]'' in their names.
  Note also that ~c[keyword-listp] has been renamed ~ilc[keyword-value-listp].

  Accumulated persistence has been implemented.  It is not connected
  to ~c[:]~ilc[brr] or rule monitoring.  ~l[accumulated-persistence].

  ~c[:Trigger-terms] has been added for ~c[:]~ilc[linear] rule classes, so you
  can hang a ~il[linear] rule under any addend you want.  ~l[linear],
  which has been improved and expanded.

  ACL2 now accepts ~c[256] ~il[characters] and includes the Common Lisp
  functions ~ilc[code-char] and ~ilc[char-code].  However, ACL2 controls the lisp
  reader so that ~c[#\\c] may only be used when ~c[c] is a single standard
  character or one of ~c[Newline], ~c[Space], ~c[Page], ~c[Rubout], ~c[Tab].  If you want
  to enter other ~il[characters] use ~ilc[code-char], e.g.,
  ~c[(coerce (list (code-char 7) (code-char 240) #\a) 'string)].
  ~l[characters].  Note:  our current handling of ~il[characters]
  makes the set of theorems different under Macintosh Common Lisp
  (MCL) than under other Common Lisps.  We hope to rectify this
  situation before the final release of ACL2.

  A new ~il[table], ~ilc[macro-aliases-table], has been implemented, that
  associates macro names with function names.  So for example, since
  ~ilc[append] is associated with ~ilc[binary-append], the form ~c[(disable append)]
  it is interpreted as though it were ~c[(disable binary-append)].
  ~l[macro-aliases-table], ~pl[add-macro-alias] and
  ~pl[remove-macro-alias].

  The implementation of conditional metalemmas has been modified so
  that the metafunction is applied before the hypothesis metafunction
  is applied.  ~l[meta].

  The Common Lisp functions ~ilc[acons] and ~ilc[endp] have been defined in
  the ACL2 logic.

  We have added the symbol ~ilc[declare] to the list ~c[*acl2-exports*],
  and hence to the package ~c[\"ACL2-USER\"].

  A new hint, ~c[:restrict], has been implemented.  ~l[hints].

  It used to be that if ~c[:]~ilc[ubt] were given a number that is greater
  than the largest current ~il[command] number, it treated that number the
  same as ~c[:]~ilc[max].  Now, an error is caused.

  The ~il[table] ~c[:force-table] has been eliminated.

  A command ~c[:]~ilc[disabledp] (and macro ~ilc[disabledp]) has been added;
  ~pl[disabledp].

  ~il[Compilation] via ~c[:]~ilc[set-compile-fns] is now suppressed during
  ~ilc[include-book].  In fact, whenever the ~il[state] global variable
  ~ilc[ld-skip-proofsp] has value ~c[']~ilc[include-book].

  ~/

  Here are some less important changes, additions, and so on.

  Unlike previous releases, we have not proved all the theorems in
  ~c[axioms.lisp]; instead we have simply assumed them.  We have deferred
  such proofs because we anticipate a fairly major changed in Version
  1.8 in how we deal with ~il[guard]s.

  We used to (accidentally) prohibit the ``redefinition'' of a ~il[table]
  as a function.  That is no longer the case.

  The check for whether a ~il[corollary] follows tautologically has been
  sped up, at the cost of making the check less ``smart'' in the
  following sense:  no longer do we expand primitive functions such as
  ~ilc[implies] before checking this propositional implication.

  The ~il[command] ~ilc[ubt!] has been modified so that it never causes or
  reports an error.  ~l[ubt!].

  ACL2 now works in Harlequin Lispworks.

  The user can now specify the ~c[:trigger-terms] for ~c[:]~ilc[linear] rules.
  ~l[linear].

  The name of the system is now ``ACL2''; no longer is it ``Acl2''.

  The raw lisp counterpart of ~ilc[theory-invariant] is now defined to be a
  no-op as is consistent with the idea that it is just a call of
  ~ilc[table].

  A bug was fixed that caused ~il[proof-checker] ~il[instructions] to be
  executed when ~ilc[ld-skip-proofsp] was ~c[t].

  The function ~ilc[rassoc] has been added, along with a corresponding
  function used in its ~il[guard], ~c[r-eqlable-alistp].

  The ~ilc[in-theory] event and hint now print a warning not only when
  certain ``primitive'' ~c[:]~ilc[definition] rules are ~il[disable]d, but also when
  certain ``primitive'' ~c[:]~ilc[executable-counterpart] rules are ~il[disable]d.

  The modified version of ~c[trace] provided by ACL2, for use in raw
  Lisp, has been modified so that the lisp special variable
  ~c[*trace-alist*] is consulted.  This alist associates, using ~ilc[eq],
  values with their print representations.  For example, initially
  ~c[*trace-alist*] is a one-element list containing the pair
  ~c[(cons state '|*the-live-state*|)].

  The system now prints an observation when a form is skipped because
  the default color is ~c[:red] or ~c[:pink].  (Technically:  ~c[when-cool] has
  been modified.)

  Additional protection exists when you submit a form to raw Common
  Lisp that should only be submitted inside the ACL2 read-eval-print
  loop.

  Here is a complete list of the changes in function names described
  near the top of this note, roughly of the form
  ~bv[]
  foo-lst --> foo-listp
  ~ev[]
  meaning:  the name ``~c[foo-lst]'' has been changed to ``~c[foo-listp].''
  ~bv[]
  symbolp-listp    --> symbol-listp
  list-of-symbolp-listp  --> symbol-list-listp
                         {for consistency with change to symbol-listp}
  rational-lst     --> rational-listp
                       {which in fact was already defined as well}
  integer-lst      --> integer-listp
  character-lst    --> character-listp
  stringp-lst      --> string-listp
  32-bit-integer-lst   --> 32-bit-integer-listp
  typed-io-lst     --> typed-io-listp
  open-channel-lst --> open-channel-listp
  readable-files-lst   --> readable-files-listp
  written-file-lst --> written-file-listp
  read-file-lst    --> read-file-listp
  writeable-file-lst   --> writable-file-listp
                       {note change in spelling of ``writable''}
  writeable-file-lst1  --> writable-file-listp1
  pseudo-termp-lst     --> pseudo-term-listp
  hot-termp-lst --> hot-term-listp {by analogy with pseudo-term-listp}
  weak-termp-lst   --> weak-term-listp
  weak-termp-lst-lst   --> weak-termp-list-listp
  ts-builder-case-lstp -> ts-builder-case-listp
  quotep-lst       --> quote-listp
  termp-lst        --> term-listp
  instr-lst        --> instr-listp
  spliced-instr-lst    --> spliced-instr-listp
  rewrite-fncallp-lst  --> rewrite-fncallp-listp
  every-occurrence-equiv-hittablep1-lst -->
              every-occurrence-equiv-hittablep1-listp
  some-occurrence-equiv-hittablep1-lst  -->
              some-occurrence-equiv-hittablep1-listp
              {by analogy with the preceding, even though it's a
               ``some'' instead of ``all'' predicate]
  almost-quotep1-lst   --> almost-quotep1-listp
  ffnnames-subsetp-lst --> ffnnames-subsetp-listp
  boolean-lstp     --> boolean-listp
  subst-expr1-lst-okp  --> subst-expr1-ok-listp
  ~ev[]
  ~/

  ")

(deflabel note8
  :doc
  ":Doc-Section release-notes

  ACL2 Version 1.8 (May, 1995) Notes~/

  ~l[note8-update] for yet more recent changes.

  ~il[Guard]s have been eliminated from the ACL2 logic.  A summary is
  contained in this brief note.  Also ~pl[defun-mode] and
  ~pl[set-guard-checking].

  ~il[Guard]s may be included in ~il[defuns] as usual but are ignored from the
  perspective of admission to the logic: functions must terminate on
  all arguments.

  As in Nqthm, primitive functions, e.g., ~ilc[+] and ~ilc[car], logically
  default unexpected arguments to convenient values.  Thus, ~c[(+ 'abc 3)]
  is ~c[3] and ~c[(car 'abc)] is ~c[nil].  ~l[programming], and see
  the ~il[documentation] for the individual primitive functions.

  In contrast to earlier versions of ACL2, Version 1.8 logical
  functions are executed at Nqthm speeds even when ~il[guard]s have not
  been verified.  In versions before 1.8, such functions were
  interpreted by ACL2.

  Colors have been eliminated.  Two ``~il[defun-mode]s'' are supported,
  ~c[:]~ilc[program] and ~c[:]~ilc[logic].  Roughly speaking, ~c[:]~ilc[program] does what ~c[:red] used
  to do, namely, allow you to prototype functions for execution
  without any proof burdens.  ~c[:]~ilc[Logic] mode does what ~c[:blue] used to do,
  namely, allow you to add a new definitional axiom to the logic.  A
  global ~il[default-defun-mode] is comparable to the old default color.
  The system comes up in ~c[:]~ilc[logic] mode.  To change the global
  ~il[defun-mode], type ~c[:]~ilc[program] or ~c[:]~ilc[logic] at the top-level.  To specify
  the ~il[defun-mode] of a ~ilc[defun] locally use
  ~bv[]
  ~c[(declare (xargs :mode mode))].
  ~ev[]

  The ~il[prompt] has changed.  The initial ~il[prompt], indicating ~c[:]~ilc[logic] mode,
  is
  ~bv[]
  ACL2 !>
  ~ev[]
  If you change to ~c[:]~ilc[program] mode the ~il[prompt] becomes
  ~bv[]
  ACL2 p!>
  ~ev[]

  ~il[Guard]s can be seen as having either of two roles: (a) they are a
  specification device allowing you to characterize the kinds of
  inputs a function ``should'' have, or (b) they are an efficiency
  device allowing logically defined functions to be executed directly
  in Common Lisp.  If a ~il[guard] is specified, as with ~ilc[xargs] ~c[:]~ilc[guard], then
  it is ``verified'' at defun-time (unless you also specify ~ilc[xargs]
  ~c[:verify-guards nil]).  ~il[Guard] verification means what it always has:
  the input ~il[guard] is shown to imply the ~il[guard]s on all subroutines in
  the body.  If the ~il[guard]s of a function are verified, then a call of
  the function on inputs satisfying the ~il[guard] can be computed directly
  by Common Lisp.  Thus, verifying the ~il[guard]s on your functions will
  allow them to execute more efficiently.  But it does not affect
  their logical behavior and since you will automatically get Nqthm
  speeds on unverified logical definitions, most users will probably
  use ~il[guard]s either as a specification device or only use them when
  execution efficiency is extremely important.

  Given the presence of ~il[guard]s in the system, two issues are unavoidable.
  Are ~il[guard]s verified as part of the ~ilc[defun] process?  And are ~il[guard]s checked
  when terms are evaluated?  We answer both of those questions below.

  Roughly speaking, in its initial ~il[state] the system will try to verify
  the ~il[guard]s of a ~ilc[defun] if a ~c[:]~ilc[guard] is supplied in the ~ilc[xargs]
  and will not try otherwise.  However, ~il[guard] verification in ~ilc[defun]
  can be inhibited ``locally'' by supplying the ~ilc[xargs]
  ~c[:]~ilc[verify-guards] ~c[nil].  ``Global'' inhibition can be obtained via
  the ~c[:]~ilc[set-verify-guards-eagerness].  If you do not use the
  ~c[:]~ilc[guard] ~ilc[xargs], you will not need to think about ~il[guard]
  verification.

  We now turn to the evaluation of expressions.  Even if your functions contain
  no ~il[guard]s, the primitive functions do and hence you have the choice: when you
  submit an expression for evaluation do you mean for ~il[guard]s to be checked at
  runtime or not?  Put another way, do you mean for the expression to be
  evaluated in Common Lisp (if possible) or in the logic?  Note: If Common Lisp
  delivers an answer, it will be the same as in the logic, but it might be
  erroneous to execute the form in Common Lisp.  For example, should
  ~c[(car 'abc)] cause a ~il[guard] violation error or return ~c[nil]?

  The top-level ACL2 loop has a variable which controls which sense of
  execution is provided.  To turn ``~il[guard] checking on,'' by which we
  mean that ~il[guard]s are checked at runtime, execute the top-level form
  ~c[:set-guard-checking t].  To turn it off, do ~c[:set-guard-checking nil].
  The status of this variable is reflected in the ~il[prompt].
  ~bv[]
  ACL2 !>
  ~ev[]
  means ~il[guard] checking is on and
  ~bv[]
  ACL2 >
  ~ev[]
  means ~il[guard] checking is off.  The exclamation mark can be thought of
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
  will return ~c[nil].

  Note that whether or not ~il[guard]s are checked at runtime is
  independent of whether you are operating in ~c[:]~ilc[program] mode or
  ~c[:]~ilc[logic] mode and whether theorems are being proved or not.
  (Although it must be added that functions defined in ~c[:]~ilc[program]
  mode cannot help but check their ~il[guard]s because no logical
  definition exists.)

  Version 1.8 permits the verification of the ~il[guard]s of theorems, thus
  insuring that all instances of the theorem will evaluate without
  error in Common Lisp.  To verify the ~il[guard]s of a theorem named
  ~c[name] execute the event
  ~bv[]
  (verify-guards name).
  ~ev[]
  If a theorem's ~il[guard]s have been verified, the theorem is guaranteed
  to evaluate without error to non-~c[nil] in Common Lisp (provided
  resource errors do not arise).

  Caveat about ~ilc[verify-guards]: ~ilc[implies] is a function symbol, so in the
  term ~c[(implies p q)], ~c[p] cannot be assumed true when ~c[q] is evaluated;
  they are both evaluated ``outside.''  Hence, you cannot generally
  verify the ~il[guard]s on a theorem if ~ilc[implies] is used to state the
  hypotheses.  Use ~ilc[if] instead.  In a future version of ACL2, ~ilc[implies]
  will likely be a macro.

  See sum-list-example.lisp for a nice example of the use of Version
  1.8.  This is roughly the same as the documentation for
  ~il[guard-example].

  We have removed the capability to do ``old-style-forcing'' as
  existed before Version 1.5.  ~l[note5].

  NOTE:  Some low level details have, of course, changed.  One such
  change is that there are no longer two distinct type prescriptions
  stored when a function is admitted with its ~il[guard]s verified.  So for
  example, the type prescription ~il[rune] for ~ilc[binary-append] is now
  ~bv[]
  (:type-prescription binary-append)
  ~ev[]
  while in Versions 1.7 and earlier, there were two such ~il[rune]s:
  ~bv[]
  (:type-prescription binary-append . 1)
  (:type-prescription binary-append . 2)
  ~ev[]

  Nqthm-style forcing on ~il[linear] arithmetic assumptions is no longer
  executed when forcing is ~il[disable]d.

  Functional instantiation now benefits from a trick also used in
  Nqthm:  once a ~il[constraint] generated by a ~c[:functional-instance]
  lemma instance (~pl[lemma-instance]) has been proved on behalf
  of a successful event, it will not have to be re-proved on behalf of
  a later event.

  ~ilc[1+] and ~ilc[1-] are now macros in the logic, not functions.  Hence, for
  example, it is ``safe'' to use them on left-hand sides of rewrite
  rules, without invoking the common warning about the presence of
  nonrecursive function symbols.

  A new ~il[documentation] section ~il[file-reading-example] illustrates how to
  process forms in a file.

  A new ~il[proof-checker] command ~c[forwardchain] has been added;
  ~pl[acl2-pc::forwardchain].

  It is now possible to use quantifiers.  ~l[defun-sk] and
  ~pl[defchoose].

  There is a new event ~ilc[set-inhibit-warnings], which allows the user
  to turn off warnings of various types.
  ~pl[set-inhibit-warnings].

  An unsoundness relating ~ilc[encapsulate] and ~c[:functional-instance]
  ~il[hints] has been remedied, with a few small effects visible at the
  user level.  The main observable effect is that ~ilc[defaxiom] and
  non-local ~ilc[include-book] ~il[events] are no longer allowed in the scope
  of any ~ilc[encapsulate] event that has a non-empty ~il[signature].

  When ~ilc[certify-book] is called, we now require that the default
  ~il[defun-mode] (~pl[default-defun-mode]) be ~c[:]~ilc[logic].  On a related
  note, the default ~il[defun-mode] is irrelevant to ~ilc[include-book]; the
  mode is always set to ~c[:]~ilc[logic] initially, though it may be changed
  within the book and reverts to its original value at the conclusion
  of the ~ilc[include-book].  A bug in ~ilc[include-book] prevented it from
  acting this way even though the ~il[documentation] said otherwise.

  The ~il[documentation] has been substantially improved.  A new
  section ``Programming'' contains ~il[documentation] of many useful
  functions provided by ACL2; ~pl[programming].  Also, the
  ~il[documentation] has been ``marked up'' extensively.  Thus in
  particular, users of Mosaic will find many links in the
  ~il[documentation].

  The symbols ~ilc[force], ~ilc[mv-nth], and ~c[acl2-count] have been added
  to the list ~c[*acl2-exports*].

  We now permit most names from the main Lisp package to be used as
  names, except for names that define functions, macros, or constants.
  ~l[name].

  We have changed the list of imports from the Common Lisp package to
  ACL2, i.e., the list ~c[*common-lisp-symbols-from-main-lisp-package*],
  to be exactly those external symbols of the Common Lisp package as
  specified by the draft Common Lisp standard.  In order to
  accommodate this change, we have renamed some ACL2 functions as
  shown below, but these and other ramifications of this change should
  be transparent to most ACL2 users.
  ~bv[]
  warning      --> warning$
  print-object --> print-object$
  ~ev[]

  Proof trees are no longer enabled by default.  To start them up,
  ~c[:]~ilc[start-proof-tree].

  We have added the capability of building smaller images.  The
  easiest way to do this on a Unix (trademark of AT&T) system is:
  ~c[make small].

  ~/

  Here we will put some less important changes, additions, and so on.

  We have added definitions for the Common Lisp function ~ilc[position]
  (for the test ~ilc[eql]), as well as corresponding versions
  ~ilc[position-equal] and ~ilc[position-eq] that use tests ~ilc[equal] and
  ~ilc[eq], respectively.  ~l[position], ~pl[position-equal],
  and ~pl[position-eq].

  The ~ilc[defthm] event ~c[rational-listp-implies-rationalp-car] no
  longer exists.

  We fixed a bug in the hint mechanism that applied ~c[:by], ~c[:cases], and
  ~c[:use] ~il[hints] to the first induction goal when the prover reverted to
  proving the original goal by induction.

  We fixed a bug in the handling of ~c[(set-irrelevant-formals-ok :warn)].

  In support of removing the old-style forcing capability, we deleted
  the initialization of ~il[state] global ~c[old-style-forcing] and deleted the
  definitions of ~c[recover-assumptions], ~c[recover-assumptions-from-goal],
  ~c[remove-assumptions1], ~c[remove-assumptions], and ~c[split-on-assumptions],
  and we renamed ~c[split-on-assumptions1] to ~c[split-on-assumptions].

  The special value ~c['none] in the ~il[proof-checker] commands ~c[claim] and ~ilc[=]
  has been replaced by ~c[:none].

  A bug in the handling of ~il[hints] by subgoals has been fixed.  For
  example, formerly a ~c[:do-not] hint could be ``erased'' by a ~c[:use] hint
  on a subgoal.  Thanks go to Art Flatau for noticing the bug.

  The functions ~c[weak-termp] and ~c[weak-term-listp] have been
  deleted, and their calls have been replaced by corresponding calls
  of ~ilc[pseudo-termp] and ~c[pseudo-term-listp].  The notion of
  ~ilc[pseudo-termp] has been slightly strenthened by requiring that
  terms of the form ~c[(quote ...)] have length 2.

  Performance has been improved in various ways.  At the prover level,
  backchaining through the recognizer alist has been eliminated in
  order to significantly speed up ACL2's rewriter.  Among the other
  prover changes (of which there are several, all technical):  we no
  longer clausify the input term when a proof is interrupted in favor
  of inducting on the input term.  At the ~il[IO] level, we have improved
  performance somewhat by suitable declarations and proclamations.
  These include technical modifications to the macros ~ilc[mv] and
  ~ilc[mv-let], and introduction of a macro ~c[the-mv] analogous to the
  macro ~ilc[the] but for forms returning multiple values.

  The function ~c[spaces] now takes an extra argument, the current column.

  A bug in the ~il[proof-checker] ~c[equiv] command was fixed.

  The function ~c[intersectp] has been deleted, because it was
  essentially duplicated by the function ~ilc[intersectp-equal].

  We now proclaim functions in AKCL and GCL before compiling ~il[books].
  This should result in somewhat increased speed.

  The function ~c[repeat] has been eliminated; use ~ilc[make-list]
  instead.

  The ~il[proof-checker] command ~c[expand] has been fixed so that it
  eliminates ~ilc[let] (lambda) expressions when one would expect it to.

  A new primitive function, ~ilc[mv-nth], has been introduced.  ~ilc[Mv-nth]
  is equivalent to ~ilc[nth] and is used in place of ~ilc[nth] in the
  translation of ~ilc[mv-let] expressions.  This allows the user to
  control the simplification of ~ilc[mv-let] expressions without
  affecting how ~ilc[nth] is treated.  In that spirit, the rewriter has
  been modified so that certain ~ilc[mv-nth] expressions, namely those
  produced in the translation of ~c[(mv-let (a b c)(mv x y z) p)], are
  given special treatment.

  A minor bug in ~c[untranslate] has been fixed, which for example will
  fix the printing of conjunctions.

  ~c[Translate] now takes a ~c[logicp] argument, which indicates whether it
  enforces the restriction that ~c[:]~ilc[program] mode functions do not occur
  in the result.

  The modified version of ~c[trace] provided by ACL2, for use in raw Lisp,
  has been modified so that the lisp special variable ~c[*trace-alist*]
  has a slightly different functionality.  This alist associates,
  using ~ilc[eq], symbols with the print representations of their values.
  For example, initially ~c[*trace-alist*] is a one-element list
  containing the pair ~c[(cons 'state '|*the-live-state*|)].  Thus, one
  may cons the pair ~c[(cons '*foo* \"It's a FOO!\")] on to ~c[*trace-alist*];
  then until ~c[*foo*] is defined, this change will have no effect, but
  after for example
  ~bv[]
  (defconst *foo* 17)
  ~ev[]
  then ~c[trace] will print ~c[17] as ~c[\"It's a FOO!\"].

  ~c[Trace] also traces the corresponding logic function.

  ~il[Proof-tree] display has been improved slightly in the case of
  successful proofs and certain event failures.

  The function ~c[positive-integer-log2] has been deleted.

  The macro ~ilc[skip-proofs] now prints a warning message when it is
  encountered in the context of an ~ilc[encapsulate] event or a book.
  ~l[skip-proofs].

  Some functions related to ~c[the-fn] and ~c[wormhole1] now have
  ~il[defun-mode] ~c[:]~ilc[program], but this change is almost certain to
  be inconsequential to all users.

  ~/

  ")

(deflabel note8-update
  :doc
  ":Doc-Section release-notes

  ACL2 Version 1.8 (Summer, 1995) Notes~/

  ACL2 can now use Ordered Binary Decision Diagram technology.
  ~l[bdd].  There is also a ~il[proof-checker] ~c[bdd] command.

  ACL2 is now more respectful of the intention of the function
  ~ilc[hide].  In particular, it is more careful not to dive inside any
  call of ~ilc[hide] during equality substitution and case splitting.

  The ~ilc[ld] special (~pl[ld]) ~ilc[ld-pre-eval-print] may now be used
  to turn off printing of input forms during processing of
  ~ilc[encapsulate] and ~ilc[certify-book] forms, by setting it to the value
  ~c[:never], i.e., ~c[(set-ld-pre-eval-print :never state)].
  ~l[ld-pre-eval-print].

  The TUTORIAL documentation section has, with much help from Bill
  Young, been substantially improved to a bona fide introduction, and
  has been renamed ~il[acl2-tutorial].

  The term pretty-printer has been modified to introduce ~c[(<= X Y)]
  as an abbreviation for ~c[(not (< Y X))].

  Forward chaining and linear arithmetic now both benefit from the
  evaluation of ground subterms.

  A new macro ~ilc[set-inhibit-output-lst] has been defined.  This should
  be used when setting the ~il[state] global ~c[inhibit-output-lst];
  ~pl[set-inhibit-output-lst] and ~pl[proof-tree].

  The test for redundancy in definitions includes the ~il[guard] and type
  declarations.  ~l[redundant-events].

  ~l[generalized-booleans] for a discussion of a potential
  soundness problem for ACL2 related to the question:  Which Common
  Lisp functions are known to return Boolean values?

  ~/

  Here we will put some less important changes, additions, and so on.

  A bug has been fixed so that now, execution of ~c[:comp t]
  (~pl[comp]) correctly handles non-standard characters.

  A bug in ~ilc[digit-char-p] has been fixed, so that the ``default'' is
  ~c[nil] rather than ~c[0].

  ~ilc[True-listp] now tests the final ~ilc[cdr] against ~c[nil] using ~ilc[eq]
  instead of ~ilc[equal], for improved efficiency.  The logical meaning
  is, however, unchanged.

  ~ilc[Put-assoc-equal] has been added to the logic (it used to have
  ~c[:]~ilc[defun-mode] ~c[:]~ilc[program], and has been documented.

  ~/

  ")

(deflabel note9
  :doc
  ":Doc-Section release-notes

  ACL2 Version 1.9 (Fall, 1996) Notes~/

  By default, when the system is started it is illegal to use the
  variable ~ilc[STATE] as a formal parameter of a function definition.
  The aim is to prevent novice users from stumbling into the Byzantine
  syntactic restrictions on that variable symbol.  Use
  ~bv[]
  :set-state-ok t
  ~ev[]
  or, equivalently,
  ~bv[]
  (set-state-ok t)
  ~ev[]
  to switch back to the old default mode.  ~l[set-state-ok]

  ~c[Set-state-ok] is an event that affects the ACL2 defaults table
  (~pl[acl2-defaults-table]).  Recall that when books are
  included, the defaults table is restored to its pre-inclusion state.
  Thus, while a ~c[set-state-ok] form will permit the book to define a
  ~c[state]-using function, it will not permit the user of the book to
  make such a definition.  We recommend putting ~c[(set-state-ok t)] in
  any book that defines a ~c[state] using function.

  Books certified under Version 1.8 must be recertified under Version
  1.9.  See :DOC version.

  The simplifier has been made to look out for built-in clauses,
  whereas in past versions such clauses were only noticed by the
  ``preprocessor'' at the top of the waterfall.  THIS CHANGE MAY
  PREVENT OLD SCRIPTS FROM REPLAYING!  The undesirable side-effect is
  caused by the fact that ~c[:HINTS] require you to refer to clauses by
  their exact name (~pl[goal-spec]) and because the new simplifier
  proves more clauses than before, the goals produced have different
  names.  Thus, if a script uses ~c[:HINTS] that refer to clauses other
  than \"Goal\", e.g., \"Subgoal 1.3\" then the hint may be applied to
  a different subgoal than originally intended.

  The use of built-in-clauses has been made more efficient.  If a set
  of clauses arise often in a piece of work, it might be advantageous
  to build them in even if that results in a large set (hundreds?) of
  built-in clauses.  ~l[built-in-clauses]

  Wormholes can now be used in :logic mode functions. ~l[wormhole]

  It is now possible to provide ``computed hints.''  For example, have
  you ever wished to say ``in all goals with a name like this, :use
  that'' or ``if this term is in the subgoal, then :use that''?  Well,
  ~pl[computed-hints] and the extraordinarily long example in
  ~pl[using-computed-hints].

  ~c[Hide] terms may be rewritten with :rewrite rules about ~c[hide].
  ~l[hide], where we also now explain why ~c[hide] terms are sometimes
  introduced into your proof attempts.~/

  A bug that sometimes caused the ``non-lazy IF'' hard error message
  was fixed.

  A bug that sometimes caused a hard error in forward chaining was
  fixed.

  A bug in print-rules (:pr) was fixed.

  We report the use of :executable-counterparts in the evaluation of
  SYNTAXP forms.

  Some documentation errors were fixed.

  A bug in parent-tree tracking in add-literal-and-pt was fixed.

  A bug in ok$, go$ and eval$ was fixed.

  Clausify now optimizes (mv-nth 'k (list x0 ... xk ... xn)) to xk.

  ~/

  ")

(deflabel note-2-0
  :doc
  ":Doc-Section release-notes

  ACL2 Version 2.0 (July, 1997) Notes~/

  This is the first version of ACL2 released under the copyright of
  the University of Texas (UT).  Future releases of ACL2 will be made
  from UT rather than Computational Logic, Inc. (CLI).  Version 2.0 is just
  Version 1.9 as released by CLI, with a few bugs fixed.

  A bug causing an infinite loop was fixed in functional instantiation.
  The bug manifested itself when two conditions occurred simultaneously:
  First, the functional substitution replaces a function symbol, e.g., ~c[FOO],
  with a ~c[LAMBDA] expression containing a free variable (a variable not among
  in the ~c[LAMBDA] formals).  And, second, in one of the constraints being
  instantiated there is a call of the function symbol ~c[FOO] within the scope
  of another ~c[LAMBDA] expression.  Unless you used such a functional
  substitution, this bug fix will not affect you.

  ~/
  Less important notes:

  The implementation of ~c[PRINC$] was changed so that it was no longer 
  sensitive to the external setting of ~c[*print-base*] and other Common Lisp
  special variables.

  Typographical errors were fixed in the documentation.

  ~/

  ")

(deflabel note-2-1
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.1 (December, 1997) Notes~/

  The identity function ~ilc[case-split] has been added.  It is similar
  to ~ilc[force] but causes an immediate split of the top-level goal on
  whether the indicated hypothesis is true.
  
  ~/
  Less important notes:

  Minor bugs in the documentation were fixed.
  ~/

  ")

(deflabel note-2-2
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.2 (August, 1998) Notes~/

  Important changes:  

  A bug was fixed in the compile command, ~c[:comp].  The compiled code
  produced by ~c[:comp] in previous versions could be wildly incorrect
  because of a confusion between the printer and the reader regarding
  what was the current Lisp ~c[*package*].  This bug could manifest itself
  only if you used the ~c[:comp] command to compile previously uncompiled
  functions while the current package was different from ~c[\"ACL2\"].
  What happened in that situation depended upon what symbols were
  imported into your current package.  The most likely behavior is
  that the compiler would break or complain or the resulting compiled
  code would call functions that did not exist.

  There have been no other important changes to the code.
  
  However, this release contains some useful new books, notably those on
  the ~c[books] subdirectories ~c[cli-misc] and ~c[ihs].  Both have
  ~c[README] files.  The ~c[ihs] books provide support for integer
  hardware specifications.  These books were crucial to Bishop Brock's
  successful modeling of the Motorola CAP.  We thank Bishop for producing
  them and we thank all those who worked so hard to get these books released.
  We highly recommend the ~c[ihs] books to those modeling ALUs and other 
  arithmetic components of microprocessors or programming languages.

  In previous versions of ACL2, the arithmetic books, found on
  ~c[books/arithmetic/], included the addition of several unproved axioms
  stating properties of the rationals that we believed could be derived from
  our ``official'' axioms but which we had not mechanically proved.  The axioms
  were found in the book ~c[rationals-with-axioms.lisp],
  which was then used in the uppermost arithmetic books ~c[top.lisp] and
  ~c[top-with-meta.lisp].  John Cowles has now provided us with ACL2 proofs
  of those ``axioms'' and so in this release you will find both
  ~c[rationals-with-axioms.lisp] and ~c[rationals-with-axioms-proved.lisp].
  The former is provided for compatibility's sake.  The latter is identical
  but contains ~c[defthm]s where the former contains ~c[defaxiom]s.
  The top-most books have been rebuilt using ``~c[-axioms-proved]'' book.
  Thanks John.
  
  ~/
  Less important notes:

  Bishop Brock found a bug in ~c[translated-acl2-unwind-protectp4].
  Jun Sawada reported a bug in linear arithmetic that caused us not to
  prove certain trivial theorems concluding with ~c[(not (equal i j))].
  We have fixed both.

  We now prohibit definitions that call certain event commands
  such as ~c[DEFTHM] and ~c[TABLE] because our Common Lisp implementations
  of them differ from their ACL2 meanings (so that compiled books
  can be loaded correctly and efficiently).

  Minor bugs in the documentation were fixed.
  ~/

  ")

(deflabel note-2-3
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.3 (October, 1998) Notes~/

  Important changes:  

  Versions of ACL2 preceding this one contain a subtle soundness bug!
  We found a flaw in our detection of ~il[subversive-recursions].  The
  bug allowed some subversive recursions to slip through undetected.

  We believe it would have been difficult to have exploited this flaw
  inadvertently.  In particular, the following five conditions are
  necessary.
  ~nl[]~nl[]
  (1) Introduce a constrained function, say ~c[f], via an ~c[encapsulate].
  ~nl[]~nl[]
  (2) In the same encapsulation, define a clique of mutually
  recursive functions.  This clique must be non-~c[local] and in
  ~c[:logic] mode.
  ~nl[]~nl[]
  (3) In that mutually recursive clique, use the constrained function
  ~c[f] (perhaps indirectly) so that the termination argument for the
  clique depends on properties of the ~i[witness] for ~c[f].  Thus,
  ~c[f] or some other function dependent upon ~c[f], must be used in
  an argument in a recursive call or in a term governing a recursive
  call.  Furthermore, the use of ~c[f] must be such that the
  termination proof cannot be done without exploiting properties of
  the witness for ~c[f].  Other uses of the constrained functions in
  the clique are ok.
  ~nl[]~nl[]
  (4) Fail to include the exploited properties of ~c[f] among the
  constraints of the encapsulation.
  ~nl[]~nl[]
  (5) Later, outside the encapsulation, explicitly use a functional
  instantiation in which ~c[f] is replaced by a function not enjoying
  the crucial properties.
  ~nl[]~nl[]
  See ~il[subversive-recursions] for details.
  
  ~/

  Less important notes:

  We have begun to write some introductory tutorial material for those
  who wish to learn to program in ACL2.  Most of this material is
  HTML-based.  See the Hyper-Card on the ACL2 home page.
  
  The documentation of ~ilc[verify-guards] was improved to explain why
  one might wish to verify the ``guards'' of a ~c[defthm] event.  The
  missing documentation was noticed by John Cowles.

  A bug was fixed in cross fertilization.  The bug caused the system to report
  that it had substituted one term for another when in fact no substitution
  occurred.  The bug was noticed by Bill McCune.

  ~/
  ")

(deflabel note-2-4
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.4 (August, 1999) Notes~/

  Important changes:  

  We corrected a soundness bug in Version 2.3 related to the handling of
  ~ilc[immediate-force-modep].  The bad behavior was noticed by Robert
  Krug.  Thanks!

  We corrected a bug that permitted ~ilc[verify-guards] to accept a function
  even though a subfunction had not yet had its guards verified.  Thanks to
  John Cowles for noticing this.

  User defined single-threaded objects are now supported.  See ~il[stobj].
  ~/
  Less important notes:

  We corrected a bug that prevented the intended expansion of some recursive
  function calls.

  We changed the handling of the primitive function ~ilc[ILLEGAL], which
  is logically defined to be ~c[nil] but which is programmed to signal an
  error, so that when it is evaluated as part of a proof, it does not signal
  an error.  The old handling of the function prevented some guard proofs
  involving ~ilc[THE] or ~ilc[LET]s with internal declarations.

  We corrected a bug that permitted some ~c[LOCAL] ~c[DEFAXIOM] events to slip 
  into certified books.

  We corrected a bug that prevented the correct undoing of certain ~c[DEFPKG]
  forms.

  Changes were made to support CMU Lisp.  Pete Manolios helped with these
  changes.

  Changes were made to make the make files more compatible with Allegro
  Common Lisp.  Jun Sawada, who has been a great help with keeping ACL2
  up and running at UT on various platforms, was especially helpful.
  Thanks Jun.

  ~/
  ")

(deflabel note-2-5
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.5 (June, 2000) Notes~/

  Important Changes:  

  Concurrent with the release of ACL2 Version  2.5 is the publication
  of two books about ACL2.  See the ``Books and Papers about ACL2 and Its
  Applications'' on the ACL2 Home Page.  

  The ~c[books] subdirectory now contains many new certifiable books,
  including solutions to the exercises in the two published books and
  full scripts for the case studies.  See ~c[books/README.html].

  Improved Unix ~c[Makefile] support for book certification has also been
  written.  See ~c[books/README.html].

  The list of symbols in ~c[*acl2-exports*] has been considerably expanded.
  If you have packages built by importing ~c[*acl2-exports*] you might want
  to look carefully at the new value of that constant.  The new value includes
  all ~c[:logic] mode functions as of Version  2.5, as well as all documented
  macros and all built-in theorem names.

  ~ilc[Include-book] and ~ilc[certify-book] were modified to
  have some additional keyword arguments.  It is possible to
  certify a book containing ~ilc[defaxiom] and/or ~ilc[skip-proofs]
  events and get warning messages or errors signaled, according to
  the settings of these new flags.  In addition, it is possible to
  specify in ~c[include-book] whether the book must be certified
  (under penalty of error if not).  The default values of these new
  arguments cause warnings to be printed rather than errors signaled.

  The above change involved altering the form of certificate files.
  When books certified under previous versions are included, more
  warnings will be generated because these books are considered
  possibly to contain ~c[defaxiom] and/or ~c[skip-proofs] events.

  We anticipate further changes to this aspect of books and consider
  the current mechanisms (for controlling whether warnings or errors
  are signaled) just a prototype.  See also the discussion below of
  ``soundness related'' warnings.  Your suggestions are welcome.

  A discrepancy between ACL2 and Common Lisp was fixed, having to do
  with ~c[declare ignore].  In past versions of ACL2, a formal
  parameter of a ~c[defun] was considered ignored if it was not used
  in the body, the guard or the measure of the ~c[defun].  That meant
  that a variable used only in the guard could not be declared ignored
  in ACL2; but some Common Lisp compilers would complain because the
  variable was not used in the body.  Now, ACL2 considers a variable
  ignored if it is not used in the body.

  ACL2 can now be built in releases 5.0 and later of Allegro Common
  Lisp.  (Other releases of Allegro Common Lisp and of other lisps
  continue to be supported as well.)  This includes Allegro Common
  Lisp running on Windows 98 platforms.  John Cowles helped us do
  some testing and answered questions for us.  Thanks John!

  We incorporated Ruben Gamboa's changes to allow the building of a
  variant, ACL2(r), of ACL2, in which the user can reason about the real
  numbers using non-standard analysis.  ~l[real].  Note that ACL2(r)
  and ACL2 have different underlying theories, and books certified in
  one system may not be included in the other.  For backward
  compatibility and to ensure a smooth transition, ACL2 is built by
  default, not ACL2(r).  This is a compile-time switch; see the 
  makefile for instructions.  There should be no changes to ACL2
  resulting from the capability of building ACL2(r) from the same
  sources.  Also ~pl[acknowledgments] for more on the history of
  ACL2(r).

  A large number of bugs (some affecting soundness) were fixed, and
  many small new features were added.  See below.

  ~/
  Less Important Changes:

  Some warnings are now considered ``soundness related,'' namely,
  those that advise you that an uncertified book has been included
  or that a book containing ~c[DEFAXIOM] or ~c[SKIP-PROOFS] events.
  (Technically, ~c[DEFAXIOM]s do not imperil soundness in the proof-
  theoretic sense, though they may imperil the validity of theorems.
  But you sould know when a book has added an axiom to your logic!)  In
  previous versions of ACL2, all warnings were inhibited if the token
  ~c[warning] was included in the argument to
  ~ilc[set-inhibit-output-lst].  Now, soundness related warnings are
  printed even if ~c[warning]s have been inhibited.  To inhibit all
  warnings, supply the token ~c[warning!] to ~c[set-inhibit-output-lst].

  Several bugs in ~ilc[defstobj] were fixed, relating to the
  possibility that some of the subfunctions introduced by the
  ~c[defstobj] were already defined.

  ~c[:]~ilc[Puff] no longer tries to expand ~ilc[defstobj] events.
  Previously, the attempt would cause a hard error.
  
  A soundness bug was fixed.  The bug might have been exercised if you
  had an alternative definition (implies hyps (equiv (fn ...) body)) in
  which equiv is an equivalence relation other than EQUAL.  In this case,
  calls of fn might have been expanded to body in places that were not
  equiv-hittable.

  An obscure soundness bug was fixed.  The bug was exercised only if
  you had a metafunction with a computed hypothesis (i.e., a ``meta
  hypothesis function''), the hypothesis contained a free variable,
  i.e., a variable not involved in the term being rewritten, and the
  free variable occurred in the output of the metafunction.  The
  possibility of this bug was brought to our attention by Robert Krug.

  We fixed a bug in the handling of ~c[hide] related to the question
  of whether a variable symbol occurs in a term.  The old code did not
  find the variable and could cause the system to throw away a
  hypothesis about it on the grounds that it was never mentioned.  Rob
  Sumners helped discover this problem.

  The handling of ~c[:]~ilc[elim] rules was generalized, permitting arbitrary
  known equivalence relations instead of merely ~c[equal] in the
  concluding equality.

  The printing of runes (rule names; ~pl[rune]) used has been made
  \"deterministic,\" both in proof output and in proof attempt
  summaries, by sorting the runes before printing.

  The handling of free variables has been improved for hypotheses such
  as ~c[(< 0 X)], and more generally, any hypotheses involving a
  comparison with ~c[0] (even for example ~c[(< X 1)] where ~c[X] is known to be
  an integer, which is handled as ~c[(<= X 0)]).  Thanks to Robert Krug
  for bringing relevant examples to our attention.

  A new value, ~c[:comp], has been implemented for the
  ~c[:load-compiled-file] keyword of ~ilc[include-book].  If this
  value is supplied, then a compiled file will always be loaded, even
  if that requires creating the compiled file first.

  The event ~c[include-book] now generates a warning when a compiled
  file is expected but not found (~pl[include-book]).  Formerly,
  it only did so when executed at the top level; it failed to generate
  the warning when executed on behalf of a surrounding ~c[include-book]
  command.

  Certain redefinition warnings generated by Allegro Common Lisp have
  been eliminated.

  A new key has been implemented for the ~ilc[acl2-defaults-table],
  ~c[:bogus-mutual-recursion-ok], set with ~c[:]~ilc[set-bogus-mutual-recursion-ok].
  Thanks to David Russinoff for pointing out the utility of such a key.

  A bug was fixed in ~ilc[defun-sk] that prevented its generated events from
  being accepted when guard verification is being performed.  Thanks
  to Bill Young for bringing this problem to our attention.  A second
  bug was brought to our attention by Pete Manolios, which was causing
  certain ~ilc[defun-sk] events to be rejected.  That problem has been
  fixed, and an \"Infected\" warning has also been eliminated.

  The command ~ilc[good-bye] now works with Allegro Common Lisp.

  A low-level bug was fixed that could, for example, cause an error
  such as \"Error: Expected 5 args but received 4 args\" when
  interrupting a ~c[local] event.

  A bug has been fixed in the ~il[proof-checker] related to definition
  expansion.  Thanks to Pete Manolios for bringing this to our attention with a
  simple example.

  A bug has been fixed related to the ~c[:]~il[bdd] hint in the presence of
  ~il[equivalence] relations.  Thanks to Pete Manolios for bringing this to our
  attention with a simple example.

  The functions ~ilc[position] and ~ilc[position-equal] formerly
  required the second argument to be a true list.  In accordance with
  Common Lisp, we now also allow the second argument to be a string.
  This could cause earlier proofs about these functions to fail unless
  ~ilc[true-listp] is known to hold where necessary.

  Robert Krug wrote a patch, which has been incorporated, to prevent
  certain infinite loops that can arise in linear arithmetic.  Thanks,
  Robert!

  The macro ~ilc[let*] no longer requires the bound variables to be
  distinct.

  An obscure bug was fixed related to congruence rules.  The bug would
  sometimes cause ACL2 to behave as though no rules (other than equality)
  were available for some argument positions.  Thanks to Pete Manolios for
  bringing this bug to our attention.

  Documentation topics have been added for ~ilc[hard-error] and ~ilc[prog2$],
  and the documentation for ~ilc[illegal] has been improved.  Thanks to Rob
  Sumners for a useful suggestion in the examples in documentation for
  ~c[prog2$] and a fix in documentation for ~ilc[sublis].

  The event form ~ilc[certify-book] was made more secure, in that it can now
  catch attempts to write a book to disk during its certification.
  Thanks to Rob Sumners for pointing out the insecurity of the
  existing mechanism.

  A Y2K problem was fixed with our applicative handling of dates.

  Accessors and updaters for ~ilc[stobj]s have been made more efficient when
  the underlying lisp is Allegro Common Lisp, by the use of
  appropriate simple array declarations.

  A raw Lisp break had been possible when a certified book that had no
  guard verification was included in a session after
  ~c[(]~ilc[set-verify-guards-eagerness]~c[ 2)].  This has been fixed.

  The keyword command ~c[:]~ilc[comp] can now be used to compile only raw
  Lisp functions, excluding executable counterparts, by supplying the
  argument ~c[:raw].

  Rewrite rule ~c[nth-of-character-listp] was removed from source file
  axioms.lisp since it is essentially subsumed by ~c[characterp-nth].

  Printing has been sped up.  In one example the improvement was over
  50% in both Allegro and GCL.

  We now allow printing in a \"downcase\" mode, where symbols are
  printed in lower case.  All printing functions except ~c[print-object$]
  now print characters in lower case for a symbol when the ACL2 state
  global variable ~c[print-case] has value ~c[:downcase] and vertical bars are
  not necessary for printing that symbol.  ~l[IO] for a discussion of
  the macros ~c[acl2-print-case] and ~c[set-acl2-print-case].  The default
  printing remains unchanged, i.e., symbols are printed in upper case
  when vertical bars are not required.

  A low-level printing function (~c[prin1$]) was modified so that it is
  not sensitive to various Common Lisp globals related to printing.  So
  for example, the function ~ilc[fmt] is no longer sensitive to the value
  of Common Lisp global ~c[*print-case*].  (The preceding paragraph
  explains how to control the case for printing in ACL2.)

  The definition of ~ilc[array1p] was fixed so that the ~c[:maximum-length] of
  an array must be strictly greater than the number specified in the
  ~c[:dimensions] field; they may no longer be equal.  This was always the
  intention; the documentation (~pl[arrays]) has remained unchanged.
  The corresponding change was also made to ~ilc[array2p].  Allegro Common
  Lisp formerly caused an error when ~ilc[compress1] was called on an array
  where the numbers above were equal; now, we get a guard violation
  instead, which is appropriate.

  In the context of theories, a name now represents not just the
  corresponding ~c[:definition] ~il[rune], as it has done in earlier versions
  of ACL2, but also the corresponding ~c[:]~ilc[induction] rune.
  ~l[theories] for a discussion of runic designators.  Most users
  will rarely, if ever, notice this change.  One situation where this
  change will make a difference is after executing
  ~c[(in-theory (current-theory 'foo))] followed by
  ~c[(in-theory (enable bar))], where function ~c[bar] is introduced after
  event ~c[foo], and ~c[bar] is recursively defined.  The latter ~ilc[in-theory]
  form now enables the rune ~c[(:induction bar)], which implies that the
  prover can use the induction scheme stored at definition time for
  ~c[bar].  Formerly, the rune ~c[(:induction bar)] was not enabled by
  ~c[(in-theory (enable bar))], and hence the induction scheme for ~c[bar] was
  ignored even when explicit ~c[:induct] hints were supplied.

  You may now supply ~ilc[xargs] keyword pair ~c[:normalize nil] in order to
  prevent certain definitions from ``hanging'' when there are many
  ~c[if]-subexpressions.  ~pl[defun].

  We now translate type declarations of ~c[real] into guards, as we have
  already done for other types such as ~c[rational].  For example,
  ~c[(declare (type real x))] generates the ~il[guard] ~c[(rationalp x)].
  ~l[type-spec].

  The theorem prover now behaves reasonably under the combination of
  specifying a value of ~c[t] both for ~c[:]~ilc[otf-flg] and for a hint
  ~c[:do-not-induct].  Previously, it aborted the first time it would have
  otherwise pushed a goal for induction, but now, it will continue and
  wait until all induction subgoals have been pushed before it aborts.

  We changed slightly the definition of ~ilc[round].  However, we believe
  that the new definition is equivalent to the old.

  The definition of Common Lisp function ~ilc[substitute] has been added.

  The following changes have been made in the use of file names within
  ACL2.  We thank Warren Hunt and John Cowles for running some tests
  of these changes on Macintosh and Windows 98 platforms
  (respectively).~bq[]

  (1) Names of directories and files now use a syntax like that used
  for Unix (trademark of AT&T), where directories are separated using
  the ``~c[/]'' character even when the operating system is not Unix or
  Linux.  ~l[pathname].  ACL2 also continues to support its notion
  of ~em[structured pathnames] from Version  2.4 and before, but might not
  do so in future releases and hence no longer documents such syntax.

  (2) The command ~c[:]~ilc[set-cbd] may now take a relative pathname
  as an argument.

  (3) When the macro ~ilc[ld] is given a file name as a value for
  ~ilc[standard-oi], then if that file name is a relative pathname
  it refers to the result of prepending the connected book directory
  (~pl[pathname], ~pl[cbd], and ~pl[set-cbd]) in order to
  obtain an absolute pathname.  Simiarly for the ~c[ld] specials
  ~ilc[standard-co] and ~ilc[proofs-co].

  ~eq[]It is no longer necessary to issue ~c[:]~ilc[set-state-ok]~c[ t] if you
  include a ~il[stobj] declaration for ~ilc[state], for example:
  ~bv[]
  (declare (xargs :stobjs state))
  ~ev[]
  ~l[declare-stobjs].

  The ~il[proof-checker] has been cleaned up a bit, including the
  documentation and the capability (once again) to define pc-macro
  commands (~pl[define-pc-macro]) and proof-checker meta commands
  (~pl[define-pc-meta]).

  Recall that events generate summaries that include a line beginning
  with ``~c[Warnings:]'', which is followed (on the same line) by zero or
  more brief strings that summarize the warnings generated by that
  event.  Formerly, this warnings summary for an ~ilc[encapsulate] or
  ~ilc[include-book] event did not include the summary strings for
  warnings generated by subsidiary events.  This has been fixed.

  Macro ~ilc[cw] has been documented and now expands to a call of
  a ~c[;]~ilc[logic] mode function.  ~l[cw] for a way to print to the screen
  without having to involve the ACL2 ~ilc[state].  Thanks to Rob Sumners
  for suggesting that we document this useful utility.

  Functions ~c[duplicates], ~c[add-to-set-equal], ~c[intersection-eq], ~c[evens], and
  ~c[odds] are now ~c[:]~ilc[logic] mode functions.

  ~/
  ")

; Do not make note-2-5(r) below conditional on #+:non-standard-analysis,
; because we want to make just one version of the documentation.

(deflabel |NOTE-2-5(R)|
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.5(r) (June, 2000) Notes~/

  Important changes to non-standard version:

  ~/

  Please ~pl[note-2-5] for changes to Version  2.5 of ACL2.  We
  hope to write more documentation for ACL2(r) in the future.

  ~/
  ")

(deflabel note-2-6
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.6 (November, 2001) Notes~/

  Because of the large number of modifications, we have divided up 
  the Version  2.6 notes into the following subtopics.~bq[]

  o New functionality (~pl[note-2-6-new-functionality]):~nl[]
  o Changes in proof engine (~pl[note-2-6-proofs]):~nl[]
  o Changes in rules and definitions (~pl[note-2-6-rules]):~nl[]
  o Guard-related changes (~pl[note-2-6-guards]):~nl[]
  o Proof-checker changes (~pl[note-2-6-proof-checker]):~nl[]
  o System-level changes (~pl[note-2-6-system]):~nl[]
  o Other (minor) changes (~pl[note-2-6-other]):~nl[]

  ~eq[]~/~/")

(deflabel note-2-6-new-functionality
  :doc
  ":Doc-Section note-2-6

  ACL2 Version  2.6 Notes on New Functionality~/

  A fundamental change is the provision of the ``nu-rewriter'' for
  simplifying expressions composed of ~c[NTH], ~c[UPDATE-NTH], and
  ~c[UPDATE-NTH-ARRAY] applications and ~c[LET] expressions and other
  calls of non-recursive functions or ~c[LAMBDA] expressions involving
  those symbols.  The nu-rewriter applies the obvious rewrite rule for
  ~c[(NTH i (UPDATE-NTH j v s))] and the analogous rule for
  ~c[UPDATE-NTH-ARRAY].  ~l[nu-rewriter]  The nu-rewriter can be
  enabled with ~ilc[set-nu-rewriter-mode].

  A new flag has been added to the ~c[xargs] of ~ilc[defun] permitting
  the declaration that the function is ~c[non-executable].  The
  usage is ~c[(declare (xargs :non-executable t))] and the effect is that
  the function has no executable counterpart.  On the positive side:  the
  function is permitted to use single-threaded object names and functions
  arbitrarily, as in theorems rather than as in executable definitions.
  Such functions are not permitted to declare any names ~c[:]~ilc[stobj]~c[s] but
  accessors, etc., may be used, just as in theorems.

  A new flag has been added to permit the system to abbreviate output
  by introducing ~c[LET*] notation identifying common subterms.  The
  formula being proved is not affected; this flag changes its
  displayed form only.  See ~il[set-let*-abstractionp].

  A ``raw mode'' has been added, primarily for faster loading of
  applications.  ~pl[set-raw-mode].

  Functions ~ilc[alphorder] and ~ilc[lexorder] have been put in ~c[:]~ilc[logic] mode.
  ~c[Lexorder] is now a total order ordering of the ACL2 universe, and
  theorems are included to that effect.  Thanks to Pete Manolios for
  suggesting the idea and providing events to use, and to Rob Sumners
  for assistance with some modifications.  See also the new book
  ~c[books/misc/total-order] for an irreflexive total order.

  The ACL2 user can now make system calls to the host operating system.
  ~l[sys-call] and ~pl[sys-call-status].  Thanks to Rob Sumners
  for working out this idea with Pete Manolios and Robert Krug, who we
  also thank, and for working out the implementation with us.

  It is no longer required to use absolute ~il[pathname]s in ~ilc[include-book]
  forms that have been executed before a ~ilc[certify-book].  Any relative
  pathname strings in such contexts will be expanded into absolute
  pathnames before they are saved in the ~ilc[portcullis] of the ~ilc[certificate]
  of the book being certified.

  ACL2 can now be built on top of Allegro Common Lisp 6.0, and also on
  Windows platforms on top of Allegro Common Lisp and GCL.  Thanks to Pete
  Manolios and Vinay K. Siddhavanahalli for their help with Windows.

  Rob Sumners has designed and provided an initial implementation for two
  improvements to ~ilc[defstobj] (also ~pl[stobj]).  First, array fields can
  now be resized.  Resize and length functions are provided for array fields,
  which can be used to resize stobj array fields dynamically.  The recognizers
  for array fields have been simplified to accommodate this change, so that
  they only check that each element of the array field has the specified type.
  Second, performance has been improved for stobjs with a large number of
  fields, by changing their Common Lisp implementation to store the fields in a
  simple vector instead of a list.

  Now ~il[stobj]s may be bound locally; ~pl[with-local-stobj].
  Thanks to Rob Sumners, who encouraged us to implement this
  capability, was an early user of it, and participated usefully in
  discussions on its design.

  New functions ~ilc[fms!], ~ilc[fmt!], and ~ilc[fmt1!] are the same as their respective
  functions without the ``~c[!],'' except that the ``~c[!]'' functions are
  guaranteed to print forms that can be read back in (at a slight
  readability cost).

  We added ~ilc[extended-metafunctions], metafunctions which
  allow ~ilc[state] and context sensitive rewriting to some
  extent.  We thank Robert Krug for pushing for and on this idea.

  The documentation has been improved.  In particular, a new
  documentation topic provides a gentle introduction to ACL2
  ~ilc[arrays] ~-[] ~pl[arrays-example] ~-[] and additional
  documentation has been provided for getting started with proof trees
  in emacs ~-[] ~pl[proof-tree-emacs].

  New Makefile targets ~c[fasl] and ~c[o] have been added to the ~c[books/]
  directory of the distribution.  For example, you might first certify
  books using an ACL2 built on top of GCL (which creates compiled
  files with suffix ~c[o]).  Then, when standing in the ~c[books/]
  directory, you might execute the command~bq[]

  make fasl ACL2=my-allegro-acl2

  ~eq[]which will create compiled (~c[.fasl]) files for Allegro Common
  Lisp, assuming that ~c[my-allegro-acl2] starts up an ACL2 built on
  that Common Lisp.

  The macro ~ilc[let*] now allows variables to be declared ignored.
  ~l[let*] and ~pl[let].

  The user may now control backchaining.  This feature was designed and
  primarily implemented by Robert Krug (though the authors
  of ACL2 are resposible for any errors); thanks, Robert!
  ~l[backchain-limit].

  It is now possible to ``slow down'' the rate at which case splits are
  generated by the simplifier.  ~l[set-case-split-limitations].

  Accesses to ~il[stobj]s using ~ilc[nth] or ~ilc[update-nth] are now
  displayed using symbolic constants instead of numeric indices.  For
  example, given the event
  ~bv[]
  (defstobj foo a b :renaming ((b c)))
  ~ev[]
  then the term ~c[(nth 0 foo)] will be displayed (for example, during
  proofs) as ~c[(nth *a* foo)] while ~c[(nth 1 foo)] will be displayed
  as ~c[(nth *c* foo)].  The ~ilc[defstobj] event now correspondingly
  introduces a ~ilc[defconst] event for each field accessor function,
  introducing a constant whose name is obtained from the accessor's
  name by prefixing and suffixin a ``~c[*],'' as in the example above:
  accessor ~c[a] generates ~c[(defconst *a* 0)] and accessor ~c[c]
  generates ~c[(defconst *c* 1)]. ~l[nth-aliases-table] for how to
  extend this feature for alternate names of ~il[stobj]s.

  Computed hints have been improved.  It is now possible to detect
  within a computed hint whether the goal clause is stable under
  simplification; it is also possible for a computed hint to change
  the list of available hints.  ~l[computed-hints].

  It is now possible to provide ``default hints'' that are appended
  to the hints explicitly provided.  ~l[set-default-hints].

  Using computed hints (~pl[computed-hints]) and default hints
  (~pl[set-default-hints]) it is possible to implement a book that
  supports ``priority phased simplification.''  Using this book
  you can assign priorities to your rules and cause the theorem
  prover to simplify each goal maximally under all the rules of
  one priority before enabling rules of the next priority.
  See ~c[books/misc/priorities.lisp].

  The macro ~ilc[defabbrev] has been improved to allow ~ilc[declare] forms and
  documentation strings and to do more error-checking.  Thanks to Rob Sumners
  for designing this enhancement and providing the first implementation.
  ~l[defabbrev].

  Further changes were made to support CMU Lisp.  Wolfhard Buss helped
  with these changes.

  A new table was added that is used when printing proof output, so
  that nests of right-associated calls of a binary function are
  replaced by corresponding macro calls, as has been the case for
  ~ilc[binary-+] and ~ilc[+], ~ilc[binary-append] and ~ilc[append], and so on.
  ~l[add-binop].

  Operators ~ilc[logand], ~ilc[logior], ~ilc[logxor], and ~ilc[logeqv] are now
  macros (formerly, they were functions) that call corresponding
  binary functions (e.g., ~c[binary-logand]) defined in source file
  ~c[\"axioms.lisp\"].  Thanks to Rob Sumners for this enhancement.  Proof
  output will however continue to show calls of ~ilc[logand], ~ilc[logior],
  ~ilc[logxor], and ~ilc[logeqv].

  Function ~c[(]~ilc[allocate-fixnum-range]~c[ fixnum-lo fixnum-hi)] sets aside more
  \"permanent\" fixnums in GCL.

  ACL2 now runs under ~c[CLISP].  Thanks to Wolfhard Buss and Sam
  Steingold for their assistance with the port.

  Michael ``Bogo'' Bogomolny has created a search engine, accessible
  from the ACL2 home page.  For that purpose he modified the HTML
  translator to create one file per topic (a good idea in any case).
  Thanks, Bogo!

  An emacs file of potential (but optional) use for ACL2 users may be
  found in ~c[emacs/emacs-acl2.el].  In particular, this file supports
  the use of proof trees (~pl[proof-tree]).

  Some ~il[books] have been added or modified.  In particular, Robert Krug has
  contributed ~c[books/arithmetic-2/], which provides an alternative to the
  existing collection of books about arithmetic, ~c[books/arithmetic/].  For a
  discussion of the distributed books see the link to ~c[README.html] in the
  installation instructions.

  ~/~/")

(deflabel note-2-6-proofs
  :doc
  ":Doc-Section note-2-6

  ACL2 Version  2.6 Notes on Changes in Proof Engine~/

  Certain optimizations are performed when converting terms to clausal
  form.  For example, ~c[(< 0 1)] is known to be ~c[t], 
  ~c[(HARD-ERROR ctx str alist)] is known to be ~c[nil], and
  ~c[(INTEGERP n)] is known to imply ~c[(RATIONALP n)].
  
  In earlier versions of ACL2, the conversion of a term to clausal
  form expanded ~c[LAMBDA] applications.  That may no longer occur.
  Some proofs may slow down (or fail) because your
  ~c[LAMBDA]-expressions are not expanded away when you ``expected''
  them to be.

  Robert Krug found a soundness bug in our linear arithmetic package.
  The bug was caused by the derivation of an equation from two
  inequalities without taking adequate precautions to ensure that both
  sides of the inequalities were numeric.  Robert also kindly provided
  a fix which we adopted.  Thanks Robert!

  We fixed a bug that could prevent the application of a metatheorem.

  A bug has been fixed that had caused bogus forcing rounds
  (~pl[forcing-round]).  The bug could occur when the hypothesis of
  a rule was forced (~pl[force]) before the prover decided to start
  over and prove the original goal by induction.  Thanks to Rob
  Sumners for drawing our attention to this problem.

  Some low-level fixes have been made that prevent certain infinite
  loops, based on reports by users.  We thank Yunja Choi, Matt
  Wilding, and Pete Manolios for reporting such problems.

  An obscure potential soundness hole has been fixed by redoing the
  way evaluation takes place in the ACL2 loop and during theorem
  proving.  We expect that users will see no difference based on this
  change.  (Those interested in the details can see the long comment
  ``Essay on Evaluation in ACL2'' in source file interface-raw.lisp.)

  A small change was made in computation for a heuristic that controls
  backchaining.  This will speed up proofs dramatically in a very few
  cases but should have a very small impact in general.

  The simplifier has been modified to avoid eliminating hypotheses of
  goals that can be established by contextual (specifically, type-set)
  reasoning alone.  We believe that this change will generally
  strengthen ACL2's reasoning engine, although on rare occasions a
  lemma that formerly was provable may require user assistance.
  Thanks to Robert Krug for suggesting this change and providing its
  implementation.

  Case splits are now limited, by default.  This may allow some proof
  attempts to provide output where previously the prover would appear
  to ``go out to lunch.''  For a more complete discussion, including
  instructions for how users can control case splitting,
  ~pl[set-case-split-limitations].

  A bug has been fixed in the handling of ~c[:]~ilc[type-prescription] rules by
  the ~il[bdd] package.  Thanks to Rob Sumners for discovering this bug
  and supplying a helpful example.

  ACL2 may now use the built-in induction scheme for a function symbol
  even if that function symbol is disabled.  Formerly, if a function
  symbol was disabled then its induction scheme was only considered if
  an explicit induction hint was supplied, other than ~c[:induct t].

  We eliminated the rule-class ~c[linear-alias].  This rule class was seldom
  used and complicated the linear arithmetic decision procedure in ways that
  made it difficult to extend to handle some non-linear special cases.  
  The only use of the rule-class that we know of was in our own ~c[nqthm]
  books, which were an attempt to provide an embedding of the Nqthm logic
  and theorem prover into ACL2.  But that facility was also practically
  never used, as far as we know.  So both ~c[linear-alias] rules and the
  ~c[nqthm] books have been eliminated.

  In earlier versions of ACL2, when the ~c[IF]-form of ~c[(AND p q)] was
  assumed true -- as when rewriting the ~c[alpha] expression in
  ~c[(IF (AND p q) alpha beta)] -- the assumption mechanism did not deduce
  that ~c[p] and ~c[q] are true, only that their conjunction, in its
  ~c[IF]-form, is true.  This has long been known as a deficiency in
  both ACL2 and the earlier Nqthm but it was tedious to do better when
  one considered the full range of ~c[IF]-forms one might encounter in the
  test of another ~c[IF].  Rather than code all the cases, we just waited
  until clausification got rid of them.  Robert Krug developed a pretty
  nice treatment of the general case and we added it in this version.
  This also involved a surprising number of changes elsewhere in the system
  because the improved handling of assumptions caused the theorem prover
  often to ``erase'' hypotheses provided by ~c[:use] hints because it could
  simplify them to ~c[t].  Thank you Robert!

  In response to a suggestion from Robert Krug, we added ~c[mfc-ap] so
  that extended metafunctions can take advantage of linear arithmetic.
  ~l[extended-metafunctions].

  There is less delay in printing goals.  In previous versions, a
  goal was not printed until its subgoals were created (or the goal
  was proved).  Now, the goal is printed essentially as soon as it is
  created.

  A small technical change has been made in the function ~ilc[term-order],
  to give priority on the function symbol count over the weighting of
  constants.  So for example, while previously the term ~c[(f)] preceded
  the constant 2, that is no longer the case.  If this change is noticed
  at all, it will probably be noticed in how so-called ~em[permutative]
  rewrite rules are applied; ~pl[loop-stopper].  Thanks to Robert Krug
  for suggesting this improvement and providing part of the
  implemtation.

  ~/~/")

(deflabel note-2-6-rules
  :doc
  ":Doc-Section note-2-6

  ACL2 Version  2.6 Notes on Changes in Rules and Constants~/

  The following symbols have been added to the list constant
  ~c[*common-lisp-specials-and-constants*]: ~c[REPLACE], ~c[FILL], ~c[CHARACTER],
  ~c[=], ~c[BREAK], and ~c[PRIN1].  This was done in support of ports to
  Allegro 6.0 and Windows platforms (~pl[note-2-6-new-functionality]).

  The list of symbols in ~c[*acl2-exports*] has been modified, for
  example to include ~c[show-accumulated-persistence] and the legal
  arguments to ~ilc[set-inhibit-output-lst].

  Functions ~ilc[zp] and ~ilc[zip] are now handled slightly differently.  They are
  are now disabled, but each comes with a ~c[:]~ilc[rewrite] rule that allows
  their expansion on non-variable terms, and also with a
  ~c[:]~ilc[compound-recognizer] rule that avoids the need for opening up these
  functions when applied to variables.  The resulting behavior should
  be very similar to the behavior of previous versions, except that
  case splits will be avoided when these functions are applied to
  variables.

  Function ~ilc[standard-string-alistp] replaces function
  ~c[string-alistp].  For further discussion, ~pl[note-2-6-guards].

  Rules of class ~c[:]~ilc[rewrite] whose conclusion is a term of the form
  ~c[(equal lhs rhs)] have always been stored in the expected way:  ~c[lhs]
  rewrites to ~c[rhs].  This way of storing ~c[:rewrite] rules has been
  extended to allow ~ilc[=], ~ilc[eq], or ~ilc[eql] in place of ~ilc[equal].

  Rewrite rule ~c[nth-update-nth], in source file ~c[axioms.lisp], has been
  strengthened.

  A new rewrite rule ~c[equal-constant-+] has been added to the book
  ~c[arithmetic/equalities].  This should generally be a beneficial
  change, but existing proofs involving the arithmetic books could
  conceivably be affected.

  Function ~ilc[symbol-package-name] and constant ~c[*main-lisp-package-name*]
  have undergone small changes.  This change should rarely be noticed
  by users and is discussed elsewhere; ~pl[note-2-6-system].

  We mention here that proofs involving ~il[stobj]s may need to be modified
  because of changes in auxiliary functions generated by ~ilc[defstobj].
  (These changes were made in support of a new resizing capability,
  mentioned elsewhere in these release notes;
  ~pl[note-2-6-new-functionality].

  In the distributed book directory ~c[books/arithmetic/], the book
  ~c[rationals-with-axioms-proved.lisp] has been renamed ~c[rationals.lisp].

  (ACL2(r) only) Rewrite rules ~c[realp-+], ~c[realp-*], ~c[realp-unary--], and
  ~c[realp-unary-/] have been added in analogy to existing rules
  ~c[rationalp-+], ~c[rationalp-*], ~c[rationalp-unary--], and ~c[rationalp-unary-/].
  Thanks to Jun Sawada for suggesting this change.

  The definition of ~ilc[aref1] has been modified slightly.  Previously, if
  ~c[*my-a*] were an array then ~c[(aref1 'some-name *my-a* :header)]  would
  evaluate to the ~c[cdr] of the ~ilc[header] of ~c[*my-a*] rather than to its
  ~ilc[default].  ~l[arrays].

  Changes have been made in the ~c[ihs] books, based on suggestions from
  Jun Sawada, that support its use with ACL2(r) (~pl[real]).  The
  primary change is to replace calls of ~ilc[rationalp] with calls of
  ~ilc[real/rationalp], which should have no effect on users of standard
  ACL2.

  ~/~/")

(deflabel note-2-6-guards
  :doc
  ":Doc-Section note-2-6

  ACL2 Version  2.6 Notes on Guard-related Changes~/

  When you ~ilc[declare] that a function treats certain formals
  as ~c[:]~ilc[stobj]~c[s], the ~il[guard] of the function is automatically
  extended to include the corresponding stobj-recognizer calls.  For example,
  if a definition includes ~c[(declare (xargs :stobjs (ST)))] then the
  guard of the function is changed by the addition of the conjunct
  ~c[(ST-P ST)].

  One impact of this is that if you use the built-in ACL2 ~ilc[state]
  as a formal parameter of a function, ~c[(STATE-P STATE)] is added to
  the guard.  This may introduce a guard where there was none in
  previous versions of the system.  In older versions, therefore, no
  attempt would be made to ~ilc[verify-guards], while in the new
  version, we would attempt guard verification.  You may wish to add
  ~c[(declare (xargs :verify-guards nil))] to such definitions.

  A related change affects users who do not use stobjs or ~c[state].
  In previous versions of the system ~-[] as now ~-[] a ~c[type]
  declaration extended the guard you provided explicitly.  Thus, if
  you wrote ~c[(declare (type integer n))] then ~c[(INTEGERP n)] was
  added to your guard.  This is still the case and ~c[:stobjs]
  recognizers are similarly added.  But in older versions of the system
  we ``added'' the conjuncts without checking whether they were already
  present in the guard you provided.  This sometimes produced such
  guards as ~c[(and (integerp n) (integerp n))] where the first was
  produced by your ~c[type] declaration and the second was your
  ~c[:guard].  We now eliminate redundant conjuncts; this may rearrange
  the order of the conjuncts.

  The guard conjectures for functions using ~c[stobj]s have been simplified
  somewhat by taking advantage of the syntactic restrictions checked for
  single-threaded objects.  

  The following functions have been modified so that character and
  string arguments are restricted to standard characters.
  (~l[standard-char-p] and ~pl[standard-char-listp].)~bq[]

  ~ilc[upper-case-p]
  ~ilc[lower-case-p]
  ~ilc[char-upcase]
  ~ilc[char-downcase]
  ~c[string-downcase1]
  ~ilc[string-downcase]
  ~c[string-upcase1]
  ~ilc[string-upcase]
  ~ilc[char-equal]
  ~c[string-equal1]
  ~ilc[string-equal]

  ~eq[]Also, function ~ilc[standard-string-alistp] replaces function
  ~c[string-alistp], with concomitant changes in the guard to
  ~ilc[assoc-string-equal], and in variable ~c[*acl2-exports*].
  Also, lemma ~c[standard-string-alistp-forward-to-alistp] replaces
  lemma ~c[string-alistp-forward-to-alistp].  There is a new lemma
  ~c[standard-char-p-nth], which has also been added to ~c[*acl2-exports*].

  The guard had been inadvertently omitted from the definition of the
  function ~ilc[substitute] (and its subroutine ~c[substitute-ac]).  This
  omission has been corrected; also, the guard is slightly stronger
  than the documentation had claimed (and that has been corrected).

  ~/~/")

(deflabel note-2-6-proof-checker
  :doc
  ":Doc-Section note-2-6

  ACL2 Version  2.6 Notes on Proof-checker Changes~/

  The proof-checker command ~c[=], when used with no arguments, now
  reports which hypothesis is being used.

  The output from ~il[proof-checker] command ~c[type-alist] has been
  improved.

  A slight change has been made to the ~il[proof-checker] for commands
  ~c[promote], ~c[casesplit], ~c[equiv], and ~c[=], so that terms of the form
  ~c[(if x nil y)] are recognized as conjunctions, ~c[(and (not x) y)].
  Thanks to Pete Manolios for suggesting that we consider such a change.

  There is a new ~il[proof-checker] command ~c[print-all-concs] that prints
  all the conclusions of the unproved goals.

  A new ~ilc[proof-checker] command, ~c[runes], has been added.  It reports
  the ~il[rune]s that have participated in the interactive proof up to the
  current point.

  ~/~/")

(deflabel note-2-6-system
  :doc
  ":Doc-Section note-2-6

  ACL2 Version  2.6 Notes on System-level Changes~/

  We modified the tracking of ~ilc[skip-proofs] events and the use of
  ~ilc[state] global ~c[ld-skip-proofsp] in order to avoid some soundness
  issues.  For example, ~ilc[skip-proofs] events buried in locally-included
  books are now tracked.  The ``Essay on Skip-proofs'' in source file
  ~c[axioms.lisp] gives several examples of dicey behavior that is no
  longer supported.

  We fixed a problem with some of the makefiles, so that recursive invocations
  of ~c[make] now use the version of ~c[make] specified on the command line.

  Files were fixed to help non-Unix/Linux users with book
  certification.  Thanks to John Cowles for finding some problems
  and suggesting fixes to ~c[books/certify-numbers.lisp],
  ~c[books/arithmetic/certify.lsp], and ~c[books/cowles/certify.lsp].
  We thank Scott Burson for noticing and fixing some other such
  problems.  Moreover, a bdd test was being ignored entirely in
  Version  2.5; this problem has been fixed as well.

  A minor change in system function save-acl2-in-allegro will allow
  this function to continue to work in Allegro CL versions starting
  (someday) with 10.0.  Thanks to Art Flatau for suggesting such a
  fix.

  The ~c[books/case-studies/] directory has been removed.  These books are
  in support of the first (1998) ACL2 workshop, and are accessible via the
  ACL2 home page on the Web at
  ~c[http://www.cs.utexas.edu/users/moore/acl2/].  Also, the
  ~c[books/cli-misc] directory has been renamed ~c[books/misc], and the
  ~c[books/nqthm] directory has been removed.

  The notion of ACL2 version has been slightly modified to catch
  unsoundness due to implementation dependencies.  ~l[version].
  Another change to eliminate such unsoundness is that built-in
  symbols now have a ~ilc[symbol-package-name] of ~c[\"COMMON-LISP\"]; formerly,
  this string was ~c[\"LISP\"] for ACL2 images built on GCL.
  ~l[symbol-package-name].  At a low level, the (undocumented) constant 
  ~c[*main-lisp-package-name*] is now ~c[\"COMMON-LISP\"]; before, it was
  ~c[\"LISP\"] for GCL.

  ~/~/")

(deflabel note-2-6-other
  :doc
  ":Doc-Section note-2-6

  ACL2 Version  2.6 Notes on Other (Minor) Changes~/

  Warning strings are now case-insensitive.
  ~l[set-inhibit-warnings].

  ACL2 causes a warning when an ~il[in-theory] hint or event causes a 0-ary
  function's definition to be disabled but its ~c[:]~ilc[executable-counterpart]
  to be enabled.

  A minor modification has been made to ~ilc[defstobj] that can have a
  positive impact on performance in Allegro Common Lisp.  (For Lisp
  hackers:  the stobj name was formerly declared special, and that was
  disabling Allegro's tail-merging routing for compilation of some
  recursive functions using stobjs.)  The downside is that stobj names
  can no longer be evaluated in raw Lisp.  However, raw Lisp is not
  the right place to be evaluating ACL2 forms anyhow;
  ~pl[set-raw-mode].  We thank Rob Sumners for bringing this issue
  to our attention.

  Before Version  2.6, there has been the following problem with
  ~ilc[defstub] and ~ilc[encapsulate] in the case that the current package is not
  the ACL2 package.  If a ~il[signature] was specified using the symbol ~c[=>],
  then that symbol had have been imported into the current package
  from the ACL2 package when the current package was defined.  There
  are no longer any package restrictions on the use of ~c[=>].  Thanks to
  John Cowles for bringing this problem to our attention.

  Bugs in ~ilc[defun-sk] have been fixed.  ~c[Defun-sk] forms introducing
  functions of no arguments were failing to be admitted, for example:
  ~c[(defun-sk always-p1 () (forall (x) (p1 x)))].
  Thanks to John Cowles for bringing this problem to our attention.
  Also, ~c[defun-sk] failed on an example in the documentation
  (~pl[tutorial4-defun-sk-example]), as pointed out by Matyas
  Sustik; this bug has been fixed as well.

  The trace mechanism has been fixed to handle ~il[stobj]s, and to
  avoid the printing of so-called ~em[enabled structures].

  The ~ilc[brr] command ~c[:type-alist] now produces more readable output.

  An ~ilc[include-book] of an uncertified book no longer loads an associated
  compiled file.

  We added a few checks to make sure that the underlying lisp is
  suitable, for example checking that the reader is case-insensitive
  and reads in symbols with upper-case names where appropriate.

  We now warn when forcing (~pl[force]) or immediate force mode
  (~pl[immediate-force-modep]) change state between enabled and
  disabled.  Also ~pl[enable-immediate-force-modep] and
  ~pl[disable-immediate-force-modep] for information about these
  new macros, which may be used to control immediate force mode.

  We have eliminated the use of a low-level raw Lisp constant,
  ~c[*most-recent-multiplicity*].  Our test suite saw a speed-up
  of approximately 2% as a result for an ACL2 image built on GCL
  (but no significant speed-up for an ACL2 image built on Allegro
  Common Lisp).  We thank Rob Sumners for suggesting this improvement.

  Fixnum declarations are now realized as ~c[(signed-byte 29)] instead of
  ~c[(signed-byte 27)].  We check that the underlying Common Lisp
  recognizes objects of type ~c[(signed-byte 29)] as fixnums, with
  the exception of CLISP, which is said to have an efficient bignum
  implementation.

  A new documentation topic ~il[functional-instantiation-example]
  illustrates functional instantiation.

  A bug has been fixed in the monitoring of runes (~pl[monitor]).
  Thanks to Dave Greve for sending an example that clearly showed
  the problem.

  A warning is now issued when it is detected that a
  ~c[:]~ilc[type-prescription] rule may not be as strong as it appears because
  it is not sufficient to prove itself by type reasoning.

  An error is caused for rules of class ~c[:]~ilc[meta] when the function symbol
  ~c[IF] is among the ~c[:trigger-fns].  (~c[IF] was ignored anyhow; the point of
  this change is to avoid misleading the user.)

  A minor bug has been fixed in ~c[:]~ilc[pr], evident for example if this
  command was applied to ~c[IF].

  A minor hole in ~c[:]~ilc[set-bogus-mutual-recursion-ok] did not permit the
  acceptance of ~ilc[mutual-recursion] forms that include constant function
  definitions.  This has been fixed.  Thanks to Eric Smith for coming
  up with a simple example illustrating the problem.

  The temporary files \"TMP.lisp\" and \"TMP1.lisp\" written out by ~c[:]~ilc[comp]
  are now written to the connected book directory (~pl[cbd]).

  Previously, the Allegro compiler was not eliminating tail recursion
  for executable counterparts of functions, because of the way one of
  its flags had been set.  As a result, calls of functions whose
  guards had not been verified could run out of stack space when this
  was not necessary.  This situation has been fixed.

  Executable counterparts could have slow array accesses.  This has
  been fixed (specifically, constants are no longer replaced with
  their values in the definitions of executable counterparts).

  Various improvements have been made to the documentation.  Thanks in
  particular to Eric Smith for pointing out a numbers of places where
  fixes were in order.

  File \"mcl-acl2-startup.lisp\" has been updated, thanks to feedback
  from Philippe Georgelin.

  Inefficiencies in GCL fixnum computations were remedied for macros ~c[+f] and
  ~c[*f].  Thanks to Rob Sumners for pointing out this issue.

  ~/~/")

; Do not make note-2-6(r) below conditional on #+:non-standard-analysis,
; because we want to make just one version of the documentation.

(deflabel |NOTE-2-6(R)|
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.6(r) (November, 2001) Notes~/

  Important changes to non-standard version:  None since Version  2.5.

  ~/

  Please ~pl[note-2-6] for changes to Version  2.6 of ACL2.  We
  hope to write more documentation for ACL2(r) in the future.

  ~/
  ")

(deflabel note-2-7
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.7 (November, 2002) Notes~/

  The Version_2.7 notes are divided into the subtopics below.  Here we give
  only a brief summary of a few of the changes that seem most likely to impact
  existing proofs.  Not included in this brief summary, but included in the
  subtopics, are descriptions of improvements (including bug fixes and new
  functionality) that should not get in the way of existing proof efforts.

  In particular, please ~pl[note-2-7-new-functionality] for discussion of a
  number of new features that you may find useful.

  Acknowledgements and elaboration, as well as other changes, can be found in
  the subtopics listed below.

  o Bug fixes (~pl[note-2-7-bug-fixes]):~bq[]

  + Three soundness bugs were fixed.  These bugs were probably rarely hit, so
  users may well not notice these changes.

  + ~ilc[Certify-book] now requires ~c[:skip-proofs-ok t] (respectively,
  ~c[:defaxioms-okp t]) if there are ~ilc[skip-proofs] (respectively,
  ~ilc[defaxiom]) events in the book or any included sub-books.

  + When ~c[:by] hints refer to a definition, they now use the original body of
  that definition rather than the simplfied (``normalized'') body.

  + When ~ilc[ld] is applied to a stringp file name, it now temporarily sets the
  connected book directory (~pl[cbd]) to the directory of that file while
  evaluating forms in that file.~eq[]

  o New functionality (~pl[note-2-7-new-functionality]):~bq[]

  + ACL2 now works harder to apply ~c[:]~ilc[rewrite] and ~c[:]~ilc[linear]
  rules with free variables in the hypotheses.  ~l[note-2-7-new-functionality],
  in particular its first two paragraphs, for details.  ~il[Forward-chaining]
  also does more with free variables.~eq[]

  o Changes in proof engine (~pl[note-2-7-proofs]):~bq[]

  + Some prover heuristics have changed slightly.  Among other consequences,
  this can cause subgoal ~il[hints] to change.  For example, suppose that the
  Version_2.6 proof of a particular theorem generated \"Subgoal 2\" and
  \"Subgoal 1\" while Version_2.7 only generates the second of these.  Then a
  subgoal hint attached to \"Subgoal 1\" in Version_2.6 would have to be
  attached to \"Goal'\" in Version_2.7.  (~l[goal-spec].)  The full topic has
  details (~pl[note-2-7-proofs]).~eq[]

  o Changes in rules and definitions (~pl[note-2-7-rules]):~bq[]

  + The package name of a generated variable has changed for ~ilc[defcong].~eq[]

  o Guard-related changes (~pl[note-2-7-guards]):~bq[]

  + ~ilc[Guard] verification formerly succeeded in a few cases where it should
  have failed.

  + Guards generated from type declarations now use functions
  ~c[signed-byte-p] and ~c[unsigned-byte-p], now defined in source file
  ~c[axioms.lisp] and formerly defined rather similarly under ~c[books/ihs/].~eq[]

  o Proof-checker changes (~pl[note-2-7-proof-checker]):~bq[]

  + See the above doc topic.~eq[]

  o System-level changes (~pl[note-2-7-system]):~bq[]

  + See the above doc topic.~eq[]

  o Other changes (~pl[note-2-7-other]):~bq[]

  + A new ~ilc[table], ~ilc[invisible-fns-table], takes the place of the
  handling of invisible functions in the ~ilc[acl2-defaults-table],

  + The ~ilc[theory-invariant] event has been modified so that the default action
  is an error rather than a warning.

  +  Proof output that reports destructor elimination no longer uses the word
  ``generalizing''.~eq[]

  Again, please proceed to the subtopics for more thorough release notes.

  ~/~/")

(deflabel note-2-7-bug-fixes
  :doc
  ":Doc-Section note-2-7

  ACL2 Version  2.7 Notes on Bug Fixes~/

  Francisco J. Martin-Mateos emailed us a soundness bug (!) in our handling of
  functional instantiation (for example ~pl[functional-instantiation-example]).
  We are grateful for that email, which clearly illustrated the problem.
  It is included just below the definition of ~c[push-clause] in ACL2 source file
  ~c[prove.lisp], where we have fixed the bug.  This bug was fixed in a
  re-release of Version  2.6 in February, 2002.

  Rob Sumners emailed us a soundness bug (!) in function ~c[commutative-p1],
  which is used by the ACL2 ~il[bdd] package.  We are grateful for his help;
  his email gave a proof of nil and also pointed to the problem function.
  This bug was fixed in a re-release of Version  2.6 in February, 2002.

  We discovered and fixed a soundness bug illustrated by the book below, which
  was certifiable in Version  2.6 and ends in a proof of ~c[nil].  The event
  ~c[(verify-guards foo)] should have been rejected, because ~c[foo] calls a
  function whose guards have not been verified, namely, ~c[bar].  However, ACL2
  did not notice the call of function ~c[bar] in the body of ~c[foo] because it
  was looking in the simplified (normalized) body of ~c[foo] rather than in the
  original body of ~c[foo].  During processing of the book below, the logical
  definition of ~c[zp] is used before ~c[(verify-guards foo)], and ~c[(zp -3)]
  reduces to ~c[t] in the logic.  After ~c[(verify-guards foo)], ACL2
  simplifies ~c[(foo -3)] by going into raw Lisp, where ~c[(zp -3)] is
  evaluated and reduces to ~c[nil].
  ~bv[]
    (in-package \"ACL2\")
    (defun bar (x)
      (zp x))
    (defthm zp-false-on-negatives
      (implies (< x 0)
               (bar x))
      :rule-classes :type-prescription)
    (defun foo (x)
      (declare (xargs :guard (rationalp x)
                      :verify-guards nil))
      (if (< x 0)
          (if (bar x) 0 1) ; simplified body reduces this line to 0
        17))
    (defthm foo-of-minus-3-is-0
      (equal (foo -3) 0)
      :rule-classes nil)
    (verify-guards foo)
    (defthm foo-of-minus-3-is-1
      (equal (foo -3) 1)
      :rule-classes nil)
    (defthm bug
      nil
      :rule-classes nil
      :hints ((\"Goal\" :use (foo-of-minus-3-is-0 foo-of-minus-3-is-1))))
  ~ev[]
  The above bug exploited the fact that ~ilc[zp] has a different definition in
  raw Lisp than in the logic for arguments that violate its guard).  The
  following example caused a hard error in raw Lisp, though not a soundness
  error.
  ~bv[]
    (in-package \"ACL2\")
    (defun bar (x)
      (cons (car x) (car x)))
    (defun foo (x)
      (declare (xargs :guard t
                      :verify-guards nil))
      (if (bar x) x nil))
    (verify-guards foo)
    (defthm bug
      (equal (foo 3) t)
      :rule-classes nil)
  ~ev[]
  We have made a minor change to the notion of the ~em[formula] of a function
  symbol, related to the change above, which however is unlikely to be
  noticeable.

  In order to make it harder to hit problems like the guard problem above, we
  have slighly modified the raw Lisp definition of ~ilc[zp].

  A ~ilc[break-rewrite] command, ~c[:ancestors], was broken, but has been
  fixed.  Thanks to Eric Smith for bringing the problem to our attention, and
  to Robert Krug for supplying the final part of the fix.

  Some ~il[proof-checker] commands caused errors when all goals have already
  been proved.  This has been fixed.  Thanks to Matt Wilding for reporting this
  bug.

  Fixed a bug in ~c[:]~ilc[comp].  When compiling uncompiled functions with
  very large definitions, ACL2 was inserted a backslash (~c[\\]) character into
  generated files.

  Fixed the ~c[:type-alist] ~c[:]~ilc[brr] command (~pl[brr-commands]), whose
  output was difficult to read when typed after an ~c[:eval]..

  Fixed some clumsy handling of errors when including an uncertified book, for
  example, with the error message when including an uncertified book with a bad
  ~ilc[deftheory] event.  Thanks to Eric Smith for pointing out this problem.

  Two modifications to ~ilc[certify-book] now cause it to reflect natural
  expectations with respect to soundness.  First, it now has default values of
  ~c[nil] instead of ~c[t] for keyword arguments ~c[:skip-proofs-okp] and
  ~c[:defaxioms-okp].  Thanks to Robert Krug for suggesting this change and the
  ACL2 seminar at the University of Texas for discussing it.  Second, when
  ~c[:skip-proofs-okp] (respectively, ~c[:defaxioms-okp]) is ~c[nil], either
  explicitly or by default, then ~ilc[skip-proofs] commands (respectively,
  ~ilc[defaxiom] events) are disallowed inside any included books, regardless
  of the keyword parameters passed to ~ilc[include-book].  This had not been
  the case for previous versions of ACL2, regardless of the values of
  ~c[:skip-proofs-okp] or ~c[:defaxioms-okp] passed to ~ilc[include-book].

  Improved warnings and errors for ~ilc[certify-book] and ~ilc[include-book] to
  mention the ~il[portcullis] as a possible source of ~ilc[skip-proofs] and
  ~ilc[defaxiom]s.

  ACL2 formerly caused an error when ~il[hints] in a ~c[:]~ilc[corollary] were
  not well-formed.  This situation could arise as follows when certifying a
  book.  A lemma FOO is proved ~ilc[LOCAL]ly to the book (or, is present in a
  sub-book that is included locally).  The ~c[:corollary] of a subsequent
  theorem, BAR, disables that rule in a hint.  When BAR is proved, this is not
  a problem.  But ~ilc[certify-book] makes a second pass after processing the
  events in a book: it essentially does an ~ilc[include-book].  During the
  ~c[include-book] pass, FOO is not known (because it was ~ilc[local]), and
  therefore ACL2 fails to process the ~ilc[disable] of FOO in an
  ~ilc[in-theory] hint.  The fix is that during ~ilc[include-book], ~il[hints]
  are ignored in corollaries just as they have been for the main theorem (or
  definition).

  It was possible for guard verification to succeed where it should have
  failed.  We have fixed the bug (which was in source function (ironically
  named!) ~c[fcons-term-smart]).  Thanks to Robert Krug for sending us an
  example of bungled guard verification.  It turns out that this bug was also
  present in Version_2.6.

  The ~il[proof-checker] command ~c[=] has been improved.  Formerly, it could
  fail to apply when certain ~ilc[implies] terms were in the context.  Thanks
  to Pete Manolios for bringing this problem to our attention.

  The command ~ilc[add-binop] failed to work.  This has been fixed.  Thanks to
  Rob Sumners for pointing out this problem.  Also ~pl[note-2-7-other] for a
  discussion of how this and another ~il[table] are no longer part of the
  ~ilc[acl2-defaults-table]. 

  Book certification could cause a segmentation fault in cases where the
  certification world (~pl[certify-book]) has a very large number of events.
  This has been fixed.

  We now allow empty ~c[:use] ~il[hints] and empty hints, as requested by Eric
  Smith.  Examples:
  ~bv[]
  (\"Goal\" :use ())
  (\"Goal\")
  ~ev[]

  A large ~ilc[mutual-recursion] nest could cause a stack overflow when
  executing either ~c[:pr FN], ~c[:pr! FN], or ~c[:monitor (:definition FN) t],
  where ~c[FN] is in that large mutual recursion nest.  This has been fixed
  (implementation detail:  function ~c[actual-props] has been made
  tail-recursive).  NOTE:  If you just want the definition of ~c[FN],
  ~c[:]~ilc[pf]~c[ FN] can be much faster than ~c[:]~ilc[pr]~c[ FN] if ~c[FN]
  is in a large ~ilc[mutual-recursion].

  Hard Lisp errors could occur when including uncertified books.  This has been
  fixed; ACL2 now does syntax-checking formerly omitted when including
  uncertified books.

  Previously, the evaluation of ~ilc[defstobj] and ~ilc[mutual-recursion] forms
  could cause ``undefined'' warnings when the form was compiled.  This has been
  fixed.  Thanks to Eric Smith for bring a ~c[mutual-recursion] example to our
  attention.

  A bug has been fixed in the syntactic check for valid ~c[:]~ilc[loop-stopper]
  values.  Formerly, valid ~c[:loop-stopper] values were erroneously restricted
  to lists of length at most 2 (a minor problem, since these lists typically
  have length 1), and the function symbol(s) need not have been defined in the
  current ACL2 ~il[world].  Thanks to Eric Smith for sending an example to
  demonstrate the latter problem.

  Functions definitions that are ~c[:non-executable] (~pl[xargs]) had never
  been recognized as redundant, but this has been fixed.  Thanks to Vernon
  Austel for pointing out this problem.

  Compilation using ~c[:]~ilc[comp] now compiles user-defined
  ~c[:]~ilc[program] mode functions.  Formerly only ~c[:]~ilc[logic] mode
  functions could be compiled using ~c[:comp].

  Handling of ~c[:by] hints has been improved in essentially three ways.  The
  primary change is that now, when the current goal exactly matches the
  supplied lemma instance, the subsumption test will always succeeds
  (~pl[hints], in particular the discussion of ~c[:by]).  Second, certain proof
  failures involving ~c[:by] ~il[hints] were failing silently, with duplicate
  messages ``As indicated by the hint, this goal is subsumed by....''  This
  could happen when the original goal was among the goals generated by applying
  the hint.  This problem has been fixed by no longer considering this proof
  step to be specious (~pl[specious-simplification]).  Third and finally, when
  the ~il[lemma-instance] refers to a definition, the original body of that
  definition is used rather than the simplfied (``normalized'') body.

  In addition to the obove, we now recognize more cases of specious
  simplification (~pl[specious-simplification]).  Thanks to Eric Smith for
  bringing this issue to our attention.

  Fixed building of ACL2 under CLISP so that (1) the appropriate ACL2 startup
  message is printed out when ACL2 starts up, and (2) the lisp process supplied
  to make, e.g., LISP=/usr/bin/clisp, is the one written out to the saved ACL2
  file.  Thanks to Dave Greve and Noah Friedman for suggesting (2).  Also, ACL2
  now works with CLISP 2.30.  We have accommodated a change in CLISP's handling
  of streams and its package-locking mechanism, as well as certain non-standard
  characters that formerly could cause CLISP 2.30 to break, even when those
  characters are in comments.

  Eliminated compiler warnings for CMU Lisp.

  Fixed an incorrect error supplied when book certification proceeded so
  quickly that the file write dates of the book (~c[.lisp] file) and the
  corresponding compiled file are equal.  Now that error only occurs if the
  compiled file has a strictly earlier write date, which probably should never
  happen.

  Fixed an infinite loop when executing ~c[make clean-books] (and hence
  ~c[make] with targets that call ~c[clean-books], namely,
  ~c[certify-books-fresh], ~c[regression-fresh], and
  ~c[regression-nonstd-fresh]), which could occur when any subdirectories of
  ~c[books/] are missing ~-[] even ~c[workshops/], which is intended to be
  optional.  Thanks to Pete Manolios for pointing out this bug.

  The ~ilc[include-book] command now works properly even when filenames, or
  their directories or parent directories (etc.) are links.  Thanks to Matt
  Wilding for pointing out this problem.

  The commands ~c[:]~ilc[puff] ~c[:]~ilc[puff*] have been fixed.  Formerly,
  there was a bug when ~c[:puff] or ~c[:puff*] caused the execution of an
  ~ilc[include-book] for an absolute ~il[pathname], ~c[P], that was other than
  the current connected book directory (~pl[cbd]).  When including ~c[P], any
  subsidiary ~ilc[include-book] with a relative pathname would be erroneously
  considered relative to the current ~ilc[cbd] rather than relative to the
  directory of ~c[P].  Thanks to Pete Manolios and Matt Wilding for pointing
  out this problem.

  It had been possible in a ``large'' ACL2 image to call
  ~ilc[verify-termination] successfully on built-in function ~ilc[sys-call],
  with undesirable results.  This hole has been plugged.  Thanks to Rob Sumners
  for pointing out this problem.  The new function ~ilc[gc$] must also stay in
  ~c[:]~ilc[program] mode.

  ACL2 no longer warns when certifying a book based on ~ilc[local] functions
  whose ~il[guard]s have not yet been verified.  Thanks to Pete Manolios for
  pointing out this issue.

  An occasional ``slow array warning'' had been possible during proofs.  The
  following sequence shows how to evoke that warning in previous versions.
  ~bv[]
  (in-theory (disable binary-append))
  (in-theory (enable binary-append))
  (in-theory (disable binary-append))
  (ubt 2)
  (thm (equal (car (cons x y)) x))
  ~ev[]
  (~l[note-2-7-other] for a discussion of a change to ~ilc[compress1] in
  support of this fix; however, users should not need to read that discussion.)

  The raw Lisp code for ~ilc[defchoose] had a small bug, which was only
  evidenced in CLISP implementations as far as we know.  It has been fixed.

  When ~ilc[ld] is applied to a stringp file name, it now temporarily sets the
  connected book directory (~pl[cbd]) to the directory of that file while
  evaluating forms in that file.  To see the effect of this change, imagine a
  subdirectory ~c[\"sub\"] of the current directory, and imagine executing
  ~c[(ld \"sub/foo.lisp\")], where file ~c[foo.lisp] contains the form
  ~c[(include-book \"bar\")].  Presumably the intention was to consider the
  file ~c[bar.lisp] in the same directory, ~c[sub/], as ~c[foo.lisp].  ~c[Ld]
  now honors that intention, but in previous versions ~c[\"bar.lisp\"] would
  have been a reference to a file in the current directory, not in ~c[sub/].

  For users of ~c[run-acl2] [perhaps there are none!]: A fix has been provided
  by a Debian user via Camm Maguire so that acl2-mode anyone using that?] will
  work in Xemacs, which apparently uses variable ~c[lisp-mode-shared-map] rather
  than ~c[shared-lisp-mode-map].

  ACL2 has, for a long time (always?), had a mechanism for avoiding re-proving
  ~il[constraint]s generated by ~c[:functional-instance] ~il[lemma-instance]s
  in ~c[:use] and ~c[:by] hints.  But this mechanism had not applied to defined
  (as opposed to constrained) functions.  This has been fixed.  Thanks to
  Francisco J. Martin-Mateos (ChesKo) for pointing out this problem by sending
  a clear example.

  ~/~/")

(deflabel note-2-7-new-functionality
  :doc
  ":Doc-Section note-2-7

  ACL2 Version  2.7 Notes on New Functionality~/

  ACL2 now has a more powerful technique for relieving a ~c[:]~ilc[rewrite] or
  ~c[:]~ilc[linear] rule's hypothesis that contains free variables.  A new
  ~il[documentation] section has been written describing the handling free
  variables in rules; ~pl[free-variables].  In brief, the primary change is
  that when a free-variable match for the current hypothesis fails to allow
  subsequent hypotheses to be relieved, then additional matches may be
  attempted until they have all been tried.  Also ~pl[rule-classes] (discussion
  of ~c[:match-free]).  Also ~pl[set-match-free-error],
  ~pl[set-match-free-default], and ~pl[add-match-free-override] for interfaces
  provided to the user for controlling the way ACL2 deals with free variables
  in hypotheses.  We thank Rob Sumners for several helpful discussions about
  the designs of those interfaces, as well as Eric Smith and Robert Krug for
  helpful related discussions.  Robert Krug also found a performance bug in a
  preliminary version, for which we are grateful.

  WARNING: Book certification attempts may take much longer now that, by
  default, ACL2 looks for more free variable matches (see paragraph just
  above).  You can get the old behavior by inserting the form
  ~bv[]
  (set-match-free-default :once)
  ~ev[]
  just after the initial ~ilc[in-package] form.  However, rules from included
  books that have free variables can still slow down certification.  This can
  be fixed by inserting
  ~bv[]
  (add-match-free-override :once t)
  ~ev[]
  before the first event in the file that generates a proof.

  ~il[Forward-chaining] has been made more powerful in the presence of free
  variables (~pl[free-variables]), thanks to a contribution by Erik Reeber.
  Both before and now, when an attempt is made to relieve (prove) a hypothesis
  of a ~c[:forward-chaining] rule in the case that at least one variable in
  that hypothesis is not yet bound, ACL2 looks in the current context for an
  instance of that hypothesis.  If it finds one, then it binds the unbound
  variables and continues to the next hyopothesis.  What is new is that ACL2
  can now looks for multiple instances of that hypothesis.  Consider the
  following example; an explanation is below.
  ~bv[]
  (encapsulate (((op * *) => *))
               (local (defun op (x y) (< x y)))
               (defthm transitivity-of-op
                 (implies (and (op x y) (op y z)) (op x z))
                 :rule-classes :forward-chaining))

  ; fails in Version_2.6; succeeds in in Version_2.7
  (thm (implies (and (op a b) (op b c) (op b e)) (op a c)))
  ~ev[]
  Before Version_2.7, the proof of the ~c[thm] above fails.  When the
  ~c[:forward-chaining] rule ~c[transitivity-of-op] binds ~c[x] to ~c[a] and
  ~c[y] to ~c[b], it then looks for an instance of ~c[(op y z)] in the current
  context, with ~c[y] bound to ~c[b] but ~c[z] unbound.  It happens to find
  ~c[(op b e)] before ~c[(op b c)], and it then adds ~c[(op a e)] to the
  context.  But starting with Version_2.7, it continues to look for additional
  instances and finds ~c[(op b c)] in the context as well, chaining forward to
  ~c[(op a c)] and thus proving the theorem.

  A new macro, ~ilc[bind-free], provides a simple way to get much or most of
  the power of ~il[meta]functions.  Thanks to Eric Smith for coming up with the
  idea and to Robert Krug for providing an implementation (which we modified
  only very slightly) and documentation. ~l[bind-free] and
  ~pl[bind-free-examples].

  With the addition of ~ilc[bind-free] (mentioned above), ~ilc[syntaxp] has
  become a macro, although that change should be transparent to the user.  More
  importantly, the argument of ~c[syntaxp] may now refer to variables ~c[mfc]
  and ~c[state], giving ~c[syntaxp] some of the power of extended metafunctions;
  ~pl[syntaxp] and ~pl[extended-metafunctions].  Thanks to Robert Krug for
  implementing that extension.  Also, the argument of ~ilc[syntaxp] may now
  include calls of ~c[:]~ilc[program] mode functions.  ~l[syntaxp] and
  ~pl[syntaxp-examples] (thanks to Robert Krug for updating the former and
  creating the latter documentation).

  The linear-arithmetic decision procedure (~pl[linear-arithmetic]) has now
  been extended so that ACL2 can reason about non-linear arithmetic as well
  (~pl[non-linear-arithmetic] for how to turn on this feature).  We thank
  Robert Krug for the initial implementation of this, and Eric Smith for finding
  a couple of bugs in it.

  Some ~ilc[trace] utilities have been made available in the ACL2 loop.~bq[]

  o Function ~ilc[trace$] (and also ~ilc[untrace$]) calls the corresponding
  underlying Lisp routine ~c[trace] (and ~c[untrace]), which however continues
  (as it has for some time) to be enhanced for GCL and Allegro CL.

  o Macro ~ilc[open-trace-file] causes trace output to go to a specified
  file.  Macro ~ilc[close-trace-file] causes trace output to go to the
  screen (which is the default).

  o Macro ~ilc[with-error-trace] (or, ~ilc[wet] for short) causes a backtrace
  to be written out for many failures, including guard violations.  ~l[trace],
  ~pl[trace$], and ~pl[wet].

  ~eq[]

  A new ~ilc[theory], ~ilc[minimal-theory] has been provided (~pl[theories]).
  It can be particularly useful for speeding up proofs involving ~c[:use]
  ~il[hints].

  New ~ilc[events] ~ilc[defund] and ~ilc[defthmd] behave exactly like
  ~ilc[defun] and ~ilc[defthm], respectively, except that these new events
  disable the new name.

  The new macro ~ilc[with-output] can be used to suppress output that would
  normally result from evaluation of a form.

  The form ~c[(]~ilc[pstack]~c[)] can give the user an idea of what the
  prover has been up to during a proof, or after a user-aborted proof.
  Moreover, by evaluating ~c[(verbose-pstack t)] (~pl[verbose-pstack])
  one can get ~il[trace]-like information about prover functions, including
  time summaries, printed to the screen during a proof.  Thanks to Bill Legato
  and Robert Krug for initiating this work and to Robert for providing some
  initial implementation.

  The new command ~c[:]~ilc[comp-gcl] is identical in functionality, except
  that it always leaves ~c[.c] and ~c[.h] files when compiling in GCL.  Thanks
  to Rob Sumners and Vernon Austel for suggesting such a capability.

  The macro ~ilc[e/d] provides a convenient way to ~ilc[enable] some rules and
  ~ilc[disable] others.  It was formerly in a book supplied with the
  distribution, ~c[books/ihs/ihs-init.lisp], written by Bishop Brock (who we
  thank for providing this useful macro).

  New distributed books include those in ~c[books/ordinals/],
  ~c[books/rtl/rel3/], and ~c[books/misc/simplify-defuns.lisp] (which is
  documented in ~c[books/misc/simplify-defuns.txt]).

  The ~c[:expand] hint now accepts a special value, ~c[:LAMBDAS], that tells
  the ACL2 rewriter to expand all lambda applications (~ilc[let] expressions).
  ~l[hints].

  A new function ~ilc[zpf] has been added as fast test against 0 for
  nonnegative fixnums.

  A new macro ~ilc[gc$] allows the user to call the garbage collector of the
  underlying Common Lisp.  Thanks to Rob Sumners for suggesting this feature.

  It is now possible to ~ilc[monitor] ~il[simple] (abbreviation) rules.
  However, as a warning explains, they are still not considered monitored
  during preprocessing; ~pl[monitor].  Thanks to Robert Krug for providing this
  improvement.

  The second argument of ~ilc[certify-book], if supplied, formerly had to be
  either ~c[t] or a non-negative integer.  Now it can be the symbol ~c[?], in
  the ~c[ACL2] package, indicating that the usual check should be suppressed on
  the number of commands that have been executed to create the world in which
  ~ilc[certify-book] was called.

  ~/~/")

(deflabel note-2-7-proofs
  :doc
  ":Doc-Section note-2-7

  ACL2 Version  2.7 Notes on Changes in Proof Engine~/

  An improvement in the linear arithmetic heuristics has been provided
  by Robert Krug.  For information about this change, search for the
  comment in ~c[add-linear-lemma] (file ~c[rewrite.lisp]) that begins
  as follows.
  ~bv[]
  ; Previous to Version_2.7, we just went ahead and used the result of
  ~ev[]
  Thanks, Robert!  Also thanks to Eric Smith for providing a
  motivating example.

  The non-linear-arithmetic addition (~pl[non-linear-arithmetic]) led to
  several small changes in the linear-arithmetic decision procedure
  (~pl[linear-arithmetic]).  Two of these changes could affect existing
  proofs.~bq[]

  First, when we are setting up the initial arithmetic data-base (which we call
  the ``pot-lst''), we have always scanned it to see if there were any pairs of
  inequalities from which we could derive a previously unknown equality.  In
  some cases we added this equality to the clause and in others we used it to
  rewrite the clause, substituting one side of the equality for the other
  throughout the clause.  Previously, the heuristics that we used to determine
  whether we performed the substitution differed from those used in several
  other places in the code.  This has now been regularized, and similar
  heuristics are now used throughout the code.

  The second change to the linear-arithmetic decision procedure is
  that we now explicitly add inequalities derived from type reasoning
  to the pot-lst.  Previously, we performed cancellations against these
  inequalities without adding them to the pot-lst.  This change results
  in there being more inequalities in the pot-lst than before, and
  so more chances for there to be a pair of inequalities from which an
  equality can be derived.  In effect, certain simple consequences of
  the current goal (~pl[type-set]) may now be added as hypotheses of
  the goal or used to peform equality substitutions.

  ~eq[]

  A slight improvement has been made to the way certain rewrite rules are
  stored.  It was already the case that a rewrite rule rule whose conclusion
  ~c[C] is not a call of a known equivalence relation (or ~ilc[eq], ~ilc[eql],
  or ~ilc[=]) is stored as ~c[(iff C t)], except that if ACL2 can determine
  (using its ~ilc[type-set] mechanism) that ~c[C] is Boolean, then the rule is
  stored as ~c[(equal C t)].  The iprovement is that if ~c[C] and ~c[C'] are
  Boolean, then a rule stated as ~c[(iff C C')] is stored as ~c[(equal C C')].
  Thanks to Pete Manolios for providing an example that led us to consider this
  improvement.

  The heuristic use of equalities (fertilization) has been modified.
  Previously, ACL2 would sometimes substitute using an equality but keep the
  equality, and then undo the substitution by using the equality again.  Now,
  when ACL2 keeps an equality after using it, it puts the equality inside a
  call of ~ilc[hide].  Descendents of that goal that are unchanged by
  simplification will have this call of ~ilc[hide] removed so that the equality
  can once again contribute to the proof.  This change can cause some proofs to
  succeed that otherwise would fail.  In the unlikely event that a proof fails
  that formerly succeeded, the following hint on \"Goal\" may fix the problem
  (~pl[hints]):
  ~bv[]
  :expand ((:free (x) (hide x)))
  ~ev[]

  We have refined the heuristics employed when an ~ilc[IF] form is assumed true
  or false.  Our previous attempt (see ~il[note-2-6-proofs] for the original
  announcement) was not as general as we had believed.  We have also improved
  some low-level code responsible for rewriting ~c[IF] expressions.  In
  earlier versions of ACL2, it was possible to have the truth or falsity
  of an ~c[IF] expression explicitly recorded in the type-alist, and yet
  not use this information during rewriting.  This problem has been corrected.
  Thanks to Robert Krug for noticing this problem and implementing the fix.

  We have sped up the rewriter in some cases where there are large collections
  of mutually-recursive functions (~pl[mutual-recursion]).  (Implementation
  notes: technically, we have modified the way function ~c[being-openedp]
  operates on the ~c[fnstack], and we have modified
  ~c[*current-acl2-world-key-ordering*] as described in the essay above its
  definition.)

  ~il[Forward-chaining] is now done in the preprocessing phase of proof
  attempts (see the discussion of ~c[:DO-NOT] ~-[] ~pl[hints]).  This is part
  of a technical change, made in support of translation of type declarations to
  ~il[guard]s (~pl[note-2-7-guards]).  Previously, whenever ACL2 checked for
  ~il[built-in-clauses], it then looked for a contradiction using
  ~ilc[type-set] reasoning if it did not find a suitable built-in clause.  The
  change is to perform forward-chaining in such cases (i.e., when a built-in
  clause is not found).

  A couple of changes have been made in the generation of goals for
  ~il[forcing-round]s.  Thanks to Eric Smith for bringing issues to our
  attention that led to these changes.  For one, ~il[guard]s are no longer
  relevant in such goal generation.  Formerly, the addition of a guard could
  make a proof fail that otherwise succeeded.  Secondly, contextual information
  is now always kept when it involves a constrained constant, i.e., a zero-ary
  function introduced in the signature of an ~ilc[encapsulate].

  ~/~/")

(deflabel note-2-7-rules
  :doc
  ":Doc-Section note-2-7

  ACL2 Version  2.7 Notes on Changes in Rules and Constants~/

  The ~ilc[defcong] macro has been slightly changed.  The difference is that
  the variable generated with suffix ~c[-EQUIV] will now be in the same package
  as the name of the variable from which it is generated, rather than always
  belonging to the ACL2 package.  Thanks to Hanbing Liu for suggesting this
  change.  (Note that a couple of books have been modified to accommodate this
  change, e.g., ~c[books/finite-set-theory/set-theory].)

  In Version_2.6, a change was made for rules of class ~c[:]~ilc[rewrite] whose
  conclusion is a term of the form ~c[(EQV lhs rhs)], where ~c[EQV] is ~ilc[=],
  ~ilc[eq], or ~ilc[eql]: the rule was stored as though ~c[EQV] were
  ~ilc[equal].  (~l[note-2-6-rules].)  This change has been extended to rules
  of class ~c[:]~ilc[definition].

  ~/~/")

(deflabel note-2-7-guards
  :doc
  ":Doc-Section note-2-7

  ACL2 Version  2.7 Notes on Guard-related Changes~/

  It was possible for guard verification to succeed where it should have
  failed.  See the discussion under ~il[note-2-7-bug-fixes].

  There have been changes in the guards generated from type declarations for
  the following cases.  Thanks to Dave Greve and Matt Wilding for suggesting
  such changes.
  ~bv[]
  (type (signed-byte n) val)
  (type (unsigned-byte n) val)
  (type (integer m n) val)
  ~ev[]
  The following examples illustrate the changes.
  ~bv[]
  (type (signed-byte 4) x)
  ==> [old] (AND (INTEGERP X) (<= -8 X) (<= X 7))
  ==> [new] (SIGNED-BYTE-P 4 X)

  (type (unsigned-byte 4) x)
  ==> [old] (AND (INTEGERP X) (<= 0 X) (<= X 15))
  ==> [new] (UNSIGNED-BYTE-P 4 X)
  ~ev[]

  ~/~/")

(deflabel note-2-7-proof-checker
  :doc
  ":Doc-Section note-2-7

  ACL2 Version  2.7 Notes on Proof-checker Changes~/

  Output from the ~il[proof-checker] can now be inhibited by supplying the
  symbol ~c[proof-checker] in the list given to ~il[set-inhibit-output-lst].

  ~/~/")

(deflabel note-2-7-system
  :doc
  ":Doc-Section note-2-7

  ACL2 Version  2.7 Notes on System-level Changes~/

  ACL2 now runs (once again) under Lispworks, specifically, Lispworks 4.2.0.
  However, we needed a patch, which presumably will be unnecessary after 4.2.7.
  From Lispworks support:
  ~bq[]
  Users with LispWorks4.2.7 should ask us at lisp-support@xanalys.com
  for the transform-if-node patch. It will be helpful if they quote
  (Lisp Support Call #11372) when doing so. Also, they must send a bug
  form generated from their LispWorks image: instructions at
  http://www.lispworks.com/support/bug-report.html.
  ~eq[]

  File ~c[books/Makefile-generic] has been improved so that failed attempts to
  certify a book will cause the ~c[make] to fail.  Previously, an existing
  ~c[.cert] file was left in place, and that sufficed for the ~c[make] to be
  considered a success.  Now, the old ~c[.cert] file is first removed when
  recertification is found to be necessary.

  A change has been made to source file ~c[acl2.lisp] to accommodate GCL 2.4.3.
  (ACL2 Version  2.6 does not work with some versions of GCL 2.4.3.)

  The error message has been improved when certain forms are typed to raw Lisp
  and the ACL2 loop has never been entered (with ~c[(]~ilc[LP]~c[)]).

  The following symbols in the ACL2 package have been made untouchable, meaning
  that they are not available to the user: ~c[ev-fncall], ~c[ev], ~c[ev-lst],
  ~c[ev-acl2-unwind-protect], ~c[ev-fncall!], and ~c[user-stobj-alist-safe].
  The reason is that these functions can not be called safely except under
  certain restrictions.  If you want to call the ACL2 evaluator, consider using
  the built-in system functions ~c[trans-eval] or simple-translate-and-eval.

  CLISP Version_2.30 implements a notion of ``locking'' the \"LISP\" package
  that is incompatible with building ACL2.  (CLISP Version_2.27 does not appear
  to have had this feature.)  We have gotten around this problem by unlocking
  the \"LISP\" package in ACL2 images built on such CLISPs.

  Automatic proclaiming for GCL, which has (for a long time) been done for
  functions in compiled books, has been improved.  Formerly, the only time a
  non-trivial output type (i.e., other than ~c[t]) was inferred was when
  macroexpansion produced an explicit call of ~ilc[the].  Now, ~ilc[if]
  expressions can also generate non-~c[t] output types.  Consider the following
  example.
  ~bv[]
  (defmacro the-fixnum (n)
    (list 'the '(signed-byte 29) n))
  (defmacro 1+f (x)
    (list 'the-fixnum
          (list '1+ (list 'the-fixnum x))))
  (defun foo (x)
    (declare (type (unsigned-byte 27) x))
    (if (zp x)
        0
      (1+f (foo (1-f x)))))
  ~ev[]
  Formerly, the ~c[proclaim] forms for ~c[foo], before and after this
  improvement, are as shown below.
  ~bv[]
  (PROCLAIM '(FTYPE (FUNCTION ((UNSIGNED-BYTE 27)) T) FOO))                ;old
  (PROCLAIM '(FTYPE (FUNCTION ((UNSIGNED-BYTE 27)) (SIGNED-BYTE 29)) FOO)) ;new
  ~ev[]

  Compiler info messages sent to error stream were eliminated for CMUCL.

  ~/~/")

(deflabel note-2-7-other
  :doc
  ":Doc-Section note-2-7

  ACL2 Version  2.7 Notes on Miscellaneous Changes~/

  Made several minor ~il[documentation] improvements.  We are grateful to Eric
  Smith for suggesting (most of) these.

  Improved ~c[(show-bdd)] (~pl[bdd]) to give more useful feedback when there
  are ``leaf'' terms not known to be Boolean.

  Sped up processing of large mutual-recursion nests.  In one large example the
  speedup was roughly two orders of magnitude.

  Modified event printing so that if both ~c['prove] and ~c['event]
  are inhibited, then events are no longer printed on behalf of
  ~ilc[certify-book], ~ilc[encapsulate], or ~ilc[defstobj].  Thanks
  to Eric Smith for prompting consideration of such a change.

  The following technical change was made to support ~ilc[with-error-trace] and
  ~ilc[wet] (~pl[note-2-7-new-functionality]), but may be of interest to those
  who do low-level programming using the ACL2 logical ~ilc[world].  The
  ~c['unnormalized-body] property is now stored not only for functions defined
  in ~c[:]~ilc[logic] mode, but also for functions defined by the user in
  ~c[:]~ilc[program] mode.  (~c[:Program] mode Functions built into ACL2 still
  have their ~c['unnormalized-body] property omitted, in order to save space.)

  The handling of ``invisible'' functions for purposes of controlling rewriting
  (~pl[loop-stopper]) has been moved to a new table; ~pl[invisible-fns-table].
  Macros that access and modify this table are called
  ``~c[...-invisible-fns-table]'' in place of their former names,
  ``~c[...-invisible-fns-alist].''  This feature was formerly implemented in
  the ~ilc[acl2-defaults-table], which prevented a book from exporting lists of
  invisible functions intended to work with the ~il[rewrite] rules developed in
  the book.  Thanks to Eric Smith and Rob Sumners for suggesting this change.
  ~l[set-invisible-fns-table] (formerly ~c[set-invisible-fns-alist]), and also
  ~pl[add-invisible-fns] and ~pl[remove-invisible-fns], which provides ways to
  incrementally add to and remove from this table, respectively.  The handling
  of printing binary function call nests using macros
  (~l[add-binop]) has also been moved out of the ~ilc[acl2-defaults-table] as
  suggested by Eric and Rob, but this feature didn't work anyhow
  (~pl[note-2-7-bug-fixes]).  Incidentally, the symbols ~ilc[binop-table],
  ~ilc[add-binop], and ~ilc[remove-binop] have all been added to the list
  ~c[*acl2-exports*] (~pl[acl2-user]), ~ilc[add-invisible-fns] and
  ~ilc[remove-invisible-fns] have been added to that list, and
  ~c[set-invisible-fns-alist] has been replaced in that list by
  ~ilc[set-invisible-fns-table].  Function ~c[invisible-fns-alistp] is no
  longer defined and has been removed from ~c[*acl2-exports*].

  We now enforce the stated restriction on the pairings in
  ~c[macro-aliases-table] (~pl[macro-aliases-table]), namely, that it
  associates names of macros with names of funcions (with respect to the
  current ACL2 logical ~il[world]).  We make a similar requirement on
  ~ilc[invisible-fns-table].

  The ~ilc[theory-invariant] event has been modified so that the default action
  is an error rather than a warning.  Thanks to Eric Smith for suggesting this
  change.  Also, the value returned upon successful execution of a
  ~ilc[theory-invariant] event is now the key.

  Proof output that reports destructor elimination no longer uses the word
  ``generalizing''.  This small change may help in browsing proof output, since
  now ``generaliz'' takes you to true uses of generalization.  Thanks to Matyas
  Sustik for suggesting such a change.

  The command ~c[:]~ilc[pl] now prints an abbreviated controller-alist for
  ~c[;]~ilc[definition] rules.  Formerly the output from ~c[:pl] could be
  overwhelming when the supplied function was part of a large
  ~ilc[mutual-recursion] nest.

  The defaults for keyword parameters of ~ilc[certify-book] have changed.
  ~l[note-2-7-bug-fixes], in particular, the discussion there of two
  modifications to ~c[certify-book].

  Technical changes have been made to ~ilc[compress1] and ~ilc[compress2] that
  should usually be invisible to users.  The next paragraph describes them in
  detail, only for competeness (i.e., that description can be ignored by most
  users).  But first, here is an example showing an effect on users.  The slow
  array warning was not there previously.  Notice that the warning only arises
  if the event form is changed.  The solution is to be sure that redundant
  ~ilc[defconst] forms are syntactically identical.
  ~bv[]
  ACL2 !>(defconst *a* (compress1 'demo 
                                  '((:header :dimensions (5)
                                             :maximum-length 15
                                             :default uninitialized
                                             :name demo)
                                    (1 . one)
                                    (0 . zero))))

  Summary
  Form:  ( DEFCONST *A* ...)
  Rules: NIL
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   *A*
  ACL2 !>(aref1 'demo *a* 0)
  ZERO
  ACL2 !>(defconst *a* (compress1 'demo 
                                  '((:header :dimensions (5)
                                             :maximum-length 15
                                             :default uninitialized
                                             :name demo)
                                    (1 . one)
                                    (0 . zero))))

  This event is redundant.  See :DOC redundant-events.

  Summary
  Form:  ( DEFCONST *A* ...)
  Rules: NIL
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   :REDUNDANT
  ACL2 !>(aref1 'demo *a* 0)
  ZERO
  ACL2 !>(defconst *a* (compress1 'demo 
                                  '((:header :dimensions (5)
                                             :maximum-length 15
                                             :default uninitialized
                                             :name demo)
                                    (0 . zero)
                                    (1 . one))))

  This event is redundant.  See :DOC redundant-events.

  Summary
  Form:  ( DEFCONST *A* ...)
  Rules: NIL
  Warnings:  None
  Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
   :REDUNDANT
  ACL2 !>(aref1 'demo *a* 0)


  **********************************************************
  Slow Array Access!  A call of AREF1 on an array named
  DEMO is being executed slowly.  See :DOC slow-array-warning
  **********************************************************

  ZERO
  ACL2 !>
  ~ev[]

  As before, the von Neumann structure stored in the ~c['acl2-array] property
  of the array name contains the array list object in its ~ilc[car].  However,
  previously it was the case that ~c[compress1] and ~c[compress2] did not
  update that ~c[car] when its new value would be equal to its old value.  This
  was done largely in support of some type-set tables defined using
  ~ilc[defconst] in ~c[type-set-b.lisp].  The new versions of ~ilc[compress1]
  and ~ilc[compress2] are simpler in that no such exception is made in the case
  of equal lists, although instead the entire compression process is
  short-circuited when the input array list object is ~ilc[eq] to the ~c[car]
  of the ~c['acl2-array] property.  This change was made because the equality
  test was causing a ``slow array access'' warning to be printed in rare cases
  during proofs, as described elswhere (~pl[note-2-7-bug-fixes]).

  We no longer distribute documentation specific to Lucid Emacs.  The Info
  documentation in directory ~c[doc/EMACS/] works well both for Gnu Emacs and
  XEmacs.

  A little-advertised macro, ~c[value], has long been allowed for top-level
  forms in ~il[books]; ~pl[embedded-event-form].  This has been replaced by a
  new macro, ~c[value-triple].  The two have the same semantics at the
  top-level of books, where ~ilc[state] is ``live''.  However, ~c[value-triple]
  should be used at the top-level of a book, while ~c[value] should be used in
  function definitions (as before).  This change eliminates a warning put out
  by the Allegro Common Lisp compiler for top-level ~c[value] forms in
  ~il[books].

  ~/~/")

(deflabel |NOTE-2-7(R)|
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.7(r) (November, 2002) Notes~/

  In source file ~c[axioms.lisp], in order for proofs to succeed,
  (~c[make proofs]), the definitions of ~ilc[acl2-count] and ~c[explode-atom]
  have been modified slightly, and lemma ~c[standard-numberp-one] has been
  given ~c[:rule-classes nil].

  All ~ilc[skip-proofs] forms have been eliminated from the nonstd books, thanks
  to Ruben Gamboa.

  The directory ~c[books/sqrt/], which was intended for ACL2(r), has been moved
  to ~c[books/nonstd/sqrt/] and added as appropriate to
  ~c[books/nonstd/Makefile].

  ~/

  Please ~pl[note-2-7] for changes to Version_2.7 of ACL2.
  ~/
  ")

(deflabel note-2-8
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.8 (March, 2004) Notes~/

  BRIEF SUMMARY.

  The Version_2.8 notes are divided into the indicated subtopics.  Here we give
  only a brief summary of just a few of the major new features and changes that
  seem most likely to impact existing proofs.  Not included in this brief
  summary, but included in the subtopics, are descriptions of many improvements
  (including bug fixes and new functionality) that should not get in the way of
  existing proof efforts.  In the description below we also omit discussion of
  changes that will become clear by way of error messages if they affect you.

  In particular, please ~pl[note-2-8-new-functionality] for discussion of a
  number of new features that you may find useful.

  Acknowledgements and elaboration, as well as other changes, can be found in
  the subtopics listed below.

  o Some of the bug fixes (~pl[note-2-8-bug-fixes]):~bq[]

  + Some soundness bugs were fixed.

  + The handling of free variables in hypotheses (~pl[free-variables]) of
  rewrite and linear rules had a bug that prevented some proofs from going
  through.  Now that this bug has been fixed, you may find some proofs running
  much more slowly than before.  You can use ~ilc[accumulated-persistence] and
  ~ilc[add-match-free-override] to remedy this situation;
  ~pl[note-2-8-bug-fixes] for details.

  + The ~il[default-hints] in the current logical ~il[world] are no longer
  ignored by ~ilc[verify-guards].

  + Forms violating guard-checking such as ~c[(defconst *silly* (car 3))] are
  now allowed in ~il[books].~eq[]

  o Some of the new functionality (~pl[note-2-8-new-functionality]):~bq[]

  + WARNING: You may find that ~c[control-d] (in emacs,
  ~c[control-c control-d]) can throw you completely out of Lisp where it had
  not formerly done so.

  + ACL2 now starts up inside the ACL2 loop ~-[] that is, ~c[(]~ilc[LP]~c[)] is
  executed automatically ~-[] when built on CLISP or Allegro CL.  This was
  already the case for GCL and CMUCL, and it still is not true for Lispworks.

  + ~l[note-2-8-ordinals] for a discussion of a significant change in ordinal
  represtation, and in particular, for how to preserve existing proofs that
  depend on the previous ordinal representation.

  + Macros ~ilc[mbe] (``must be equal''), ~ilc[mbt] (``must be true''), and
  ~ilc[defexec] have been introduced, which allow the user to attach
  alternate executable definitions to functions.

  + The user can now control multiple matching for free variables in hypotheses
  for ~c[:]~ilc[forward-chaining] rules, as has already been supported for
  ~c[:]~ilc[rewrite] and ~c[:]~ilc[linear] rules.

  + It is no longer necessary to specify ~c[(set-match-free-error nil)] in
  order to avoid errors when a rule with free variables in its hypotheses is
  missing the ~c[:match-free] field.

  + The form ~c[(break-on-error)] causes, at least for most Lisps, entry into
  the Lisp debugger whenever ACL2 causes an error.

  + A new ~ilc[table] has been provided so that advanced users can override the
  built-in ~c[untranslate] functionality.  ~l[user-defined-functions-table].

  + The ~ilc[pstack] (`process [prover] stack'') mechanism, formerly denoted
  ~c[checkpoints], has been improved.  One of these improvements is to show
  actual parameters with ~c[(pstack t)] rather than formals.

  + The ~ilc[defstobj] event is now allowed to take an ~c[:inline] argument,
  which can speed up execution.

  + Macro ~ilc[cw-gstack] no longer takes arguments for the ~c[gstack] or
  ~ilc[state].  To print terms in full rather than abbreviated:
  ~c[(cw-gstack :evisc-tuple nil)].

  + The ~ilc[include-book] event now has an additional (optional) keyword,
  ~c[:dir].  In particular, ~c[(include-book \"foo/bar\" :dir :system)] will
  include the indicated book after prepending the path of the built-in
  ~c[books/] directory.  You will probably not find ~c[:dir :system] to be
  useful if you move the executable image or distributed books;
  ~pl[include-book], in particular its ``soundness warning''.

  + The printing of results in raw mode (~pl[set-raw-mode]) may now be
  partially controlled by the user:  ~pl[add-raw-arity].

  + For those using Unix/Linux ~c[make]:  A ~c[cert.acl2] file can contain
  forms to be evaluated before an appropriate ~ilc[certify-book] command is
  invoked automatically (not included in ~c[cert.acl2]).~eq[]

  o Some of the changes in the proof engine (~pl[note-2-8-proofs]):~bq[]

  + ACL2 now prevents certain rewriting loops; ~pl[rewrite-stack-limit].

  + Small changes have been made to heuristics for controlling rewriting during
  proofs by induction and in handling certain ``weak'' ~il[compound-recognizer]
  rules.

  + The handling of free variables in a hypothesis of a ~il[rewrite] rule
  (~pl[free-variables]) has been improved in the case that the hypothesis is of
  the form ~c[(equiv x y)], where ~c[equiv] is a known equivalence relation
  (~pl[equivalence]).

  + We have modified how the ACL2 simplifier handles the application of a
  defined function symbol to constant arguments, by avoiding the introduction
  of ~il[hide] when evaluation fails if the term can be rewritten.

  + The generation of \"Goal\" for recursive (and mutually-recursive) definitions
  now uses the subsumption/replacement limitation (default 500).
  ~l[case-split-limitations].

  + Default hints now apply to hints given in definitions, not just theorems.
  ~l[default-hints].

  + Linear arithmetic now uses the conclusions of ~ilc[forward-chaining] rules,
  and ~ilc[type-set] now uses a small amount of linear reasoning when deciding
  inequalities.~eq[]

  o Some of the changes in rules, definitions, and constants
  (~pl[note-2-8-rules]):~bq[]

  + See the above doc topic.~eq[]

  o Guard-related changes are described in ~pl[note-2-8-bug-fixes].

  o Some of the proof-checker changes (~pl[note-2-8-proof-checker]):~bq[]

  + Added new ~il[proof-checker] commands ~c[wrap1], ~c[wrap], and
  ~c[wrap-induct], to combine multiple conjuncts or goals.

  + The ~c[type-alist] command now takes optional arguments that control
  whether or not the governors and/or conclusion are used in computing the
  context.~eq[]

  o Some of the system-level changes (~pl[note-2-8-system]):~bq[]

  + ACL2 now runs on OpenMCL and on MCL 5.0.~eq[]

  o Some of the other changes (~pl[note-2-8-other]):~bq[]

  + Emacs file ~c[emacs/emacs-acl2.el] has been updated (~pl[note-2-8-other]
  for details).

  + When ~c[:pl] is given a term other than a symbol, it will print all rewrite
  rules that match that term.

  + A new function, ~ilc[pkg-witness], returns a symbol in the given package.

  + The list constant ~c[*acl2-exports*] has been extended.

  + A new release of the rtl library has been included: ~c[books/rtl/rel4/].
  See the ~c[README] file in that directory.~eq[]

  Again, please proceed to the subtopics for more thorough release notes.

  ~/~/")

(deflabel note-2-8-bug-fixes
  :doc

; Fixes not included below, and other notes:

; The tautology checker bug mentioned in the :doc below was in call-stack.

; A bug was fixed in assign-wormhole-output (er-progn replaces pprogn).

; It is no longer legal for user code to call include-book (this is disallowed
; in translate11).  See the comment in *inside-include-book-fn*.

; The missing argument in the first (er hard ...) in rewrite-fncall has been
; supplied.

; Subterm-one-way-unify has been modified in order to avoid any possibility of
; calling fargs on a quotep.

; Here is a way to exhibit the proof-checker expand bug described in the first
; paragraph of the documentation below:
#|
 (in-package "ACL2")

 (encapsulate
  (((foo *) => *)
   ((bar *) => *))

  (local (defun foo (x) x))
  (local (defun bar (x) (not x)))

  (defthm foo-open
    (equal (foo x) x)
    :rule-classes :definition)

  (defthm bar-not-foo
    (equal (bar x) (not (foo x)))
    :rule-classes :definition))

 (defthm bad (equal (foo x) (bar x))
   :rule-classes nil
   :instructions
   ((:dv 1) :expand :nx :expand :top :s))

 (defthm contradiction
   nil
   :rule-classes nil
   :hints (("Goal" :use bad)))
|#

; The second proof-checker bug mentioned below can be exhibited as follows:
#|
 (encapsulate
  ()
  (local
   (defthm bug-lemma (if x (if x t nil) nil)
     :rule-classes nil
     :instructions ((dive 2 3) :s)))
  (defthm bug nil
    :rule-classes nil
    :hints (("Goal" :use ((:instance bug-lemma (x nil)))))))
|#

; The function ev-acl2-unwind-protect was fixed to incorporate a change made
; long ago, by J, to acl2-unwind-protect.  This function was subsequently
; replaced by ev-w-acl2-unwind-protect.

; Without the new fix based on Matyas's suggestion (see paragraph on tautology
; checker below and Qiang's example), we had to change subgoal numbers in hints
; in the following books:
; direct-incorporation-sound-iff in
;   workshops/2003/matlin-mccune/support/simp.lisp
; graph-equivp1-load-graph1 in
;   workshops/2003/greve-wilding_mbe/support/run-fpst.lisp

; Regarding the soundness bug about type-prescription rules, whose description
; below mentions local-incompatibility and refers to an example in that :doc
; topic: The full example is below.  It actually proved in every GCL image of
; ACL2 from v2-7 back to at least v2-4.  The problem goes back at least through
; v2-3 as well; evaluate :PROPS FOO to see a type-prescription record with a
; :basic-ts of nil.  Our fix is to modify add-type-prescription-rule to cause a
; hard error when destructure-type-prescription fails; previously we had
; ignored the erp return value from destructure-type-prescription.

#|
  (in-package "ACL2")

  (defun my-natp (x)
    (declare (xargs :guard t))
    (and (integerp x)
         (<= 0 x)))

  (defun foo (x)
    (nfix x))

  (in-theory (disable foo (:type-prescription foo)))

  (encapsulate
   ()
   (local (defthm my-natp-cr
            (equal (my-natp x)
                   (and (integerp x)
                        (<= 0 x)))
            :rule-classes :compound-recognizer))
   (defthm foo-type-prescription
     (my-natp (foo x))
     :hints (("Goal" :in-theory (enable foo)))
     :rule-classes ((:type-prescription :typed-term (foo x)))))

  (defthm rationalp-foo
    (rationalp (foo x))
    :hints (("Goal" :in-theory (enable foo)))
    :rule-classes :type-prescription)

  (defthm bad-lemma
    (equal (foo x) 1)
    :rule-classes nil)

  (defthm bad
    nil
    :rule-classes nil
    :hints (("Goal" :use ((:instance bad-lemma (x 1))))))
|#

  ":Doc-Section note-2-8

  ACL2 Version  2.8 Notes on Bug Fixes~/

  We have fixed a soundness bug in the tautology checker's handling of
  expressions of the form ~c[(not (not x))].  This bug has gone back at least
  as far as Version_2.4.  All of the regression tests passed after the fix,
  without modification.  So we hope that this bug has rarely bitten anyone.
  Thanks to Qiang Zhang for sending us a proof of ~c[nil] that led us to this
  fix: ~c[(thm (equal (and p q) (not (or (not p) (not q)))))].  And thanks to
  Matyas Sustik for an observation that led to an improvement of our initial
  fix.

  The preceding version (2.7) introduced a soundness bug in handling of ACL2
  ~il[arrays], in which functions ~ilc[compress1] and ~ilc[compress2] were
  returning the input alist rather than compressing it appropriately.  Here is
  a proof of ~c[nil] that no longer succeeds, based on a bug report from Warren
  Hunt, who we thank for bringing this problem to our atttention.
  ~bv[]
  (defthm bad
    (not (let* ((ar2 (aset1 'my-array ar1 3 10))
                (ar3 (compress1 'my-array ar2))
                (ar4 (reverse (reverse ar2)))
                (ar5 (compress1 'my-array ar4)))
           (and (equal ar2 ar4)
                (not (equal ar3 ar5)))))
    :rule-classes nil)
  (defthm contradiction
    nil
    :rule-classes nil
    :hints ((\"Goal\" :use
             ((:instance bad
                         (ar1 (compress1 'my-array
                                         '((3 . 5)
                                           (:HEADER :DIMENSIONS (5)
                                                    :MAXIMUM-LENGTH 6
                                                    :DEFAULT 0
                                                    :NAME MY-ARRAY)))))))))
  ~ev[]
  On a related note, a new function ~ilc[flush-compress] can be used for subtle
  control of under-the-hood raw Lisp support for fast array access, although we
  expect it to be very rare that users need this extra support.

  Previous versions have had two soundness bugs that can occur when using the
  ~il[proof-checker]:
  ~bq[]
  o The first bug pertains to the ~c[expand] command, and hence ~c[x] and
  ~c[x-dumb] commands (which call ~c[expand]); ~pl[proof-checker-commands].
  The bug can occur when applying the above commands when the current term is a
  call of a constrained function symbol for which there is a
  ~c[:]~ilc[definition] rule.  Now, the ~c[expand] command will succeed only
  when the function symbol of the current term is a defined function symbol, in
  which case the original definition is always used, in analogy to how the
  ~c[:expand] hint works in the prover; ~pl[hints].  Thanks to John Erickson
  for sending an example that led us to wonder if there might be a soundness
  problem.

  o The second bug pertains to the ~c[s] command (and commands that call it,
  e.g., ~c[s-prop]).  The proof-checker forms a context out of the top-level
  hypotheses and the ~c[if]-terms governing the current term.  If there is a
  contradiction in the top-level hypotheses, the proof-checker can
  appropriately consider the goal to be proved, and it does so.  But formerly,
  the criterion was weaker:  the contradiction could involve the combination of
  the top-level hypotheses and ~c[if]-term governors.  Thanks to Rob Sumners
  for noticing this bug.~eq[]

  A soundness bug could be provoked in some Lisps by applying ~ilc[defpkg] to
  the empty string.  This has been disallowed.

  We fixed a soundness bug related to packages caused by a failure to track
  axioms introduced ~ilc[local]ly on behalf of ~ilc[defpkg] events.
  ~l[hidden-death-package].

  We fixed a soundness bug caused by a failure to check that a
  ~c[:]~ilc[type-prescription] rule can be processed when proofs are skipped or
  under a ~ilc[defequiv] event.  The former case can occur when processing an
  ~ilc[encapsulate] or ~ilc[include-book] event, where the rule could depend
  on a ~ilc[local] ~c[:]~ilc[compound-recognizer] rule preceding the proposed
  ~c[:]~ilc[type-prescription] rule under the same ~ilc[encapsulate] or
  ~ilc[include-book] event.  ~l[local-incompatibility] for such an example.

  We fixed a potential soundness bug relating to reclassifying a
  ~c[:program] mode function to ~c[:logic] mode (as done by
  ~ilc[verify-termination] or the submission of an appropriate ``redundant''
  definition) without adequate checking that ~ilc[stobj] usage was identical.
  Allegedly redundant definitions must now preserve the ~c[stobjs] declaration
  as well as the formals, body, guard and type declarations.  We thank
  Vernon Austel for pointing out this problem.

  It was possible to get a raw Lisp error by introducing a ~ilc[local]ly defined
  function with ~il[guard] verification inhibited and then subsequently
  introducing the same definition non-locally without that inhibition.  The
  following example will clarify.
  ~bv[]
  (encapsulate nil
    (local
      (defun foo (x) (declare (xargs :guard t :verify-guards nil)) (car x)))
    (defun foo (x) (declare (xargs :guard t)) (car x)))
  ; The following causes a raw lisp error because ACL2 runs the Common Lisp
  ; definition of foo, because it thinks that foo's guard of t was verified.
  (thm (equal (foo 3) xxx))
  ~ev[]
  Thanks to Jared Davis for bringing this problem to our attention.  We are
  particularly grateful to Jared because his example exploited this bug by
  applying it to a function defined using ~ilc[mbe] (introduced in this same
  version, 2.8), in order to prove ~c[nil]!

  The sort of error message shown below can legitimately occur when certifying
  a book in a certification world where there was an ~ilc[include-book] command
  with a relative pathname (~pl[pathname]).  However, it was occurring more
  often than necessary.  This has been fixed.
  ~bq[]
  ACL2 Error in (CERTIFY-BOOK \"foo\" ...): The certification world has
  include-book commands for book \"bar\" that correspond to different full
  pathnames, namely \"/u/dir1/bar\" and \"/u/dir2/bar\".  ACL2 cannot currently
  certify a book in such a world.  To work around this problem, use an absolute
  pathname for at least one of these books (see :DOC pathname).~eq[]

  Bugs were fixed in ~ilc[with-output], in particular related to the use of
  values ~c[:all].  Also, documentation for ~c[with-output] has been improved.
  Thanks to Vernon Austel for pointing out the bugs.

  Fixed a lisp error occurring when ~c[bash] proof-checker command was given
  illegal syntax, e.g., ~c[(bash ((\"Goal\" :in-theory (enable binary-append))))]
  instead of ~c[(bash (\"Goal\" :in-theory (enable binary-append)))].

  We added an appropriate guard to ~ilc[find-rules-of-rune], which will avoid
  hard lisp errors when this function is called on non-~il[rune] arguments.
  Thanks to Eric Smith for pointing out this issue.

  It was possible for a redundant ~ilc[include-book] form
  (~pl[redundant-events]) to leave a ~il[command] in the ACL2 logical
  ~il[world] and to cause (re-)loading of a compiled file.  These behaviors
  have been fixed.  In particular, if ~c[book1] has already been included in
  the current ACL2 ~il[world] and ~c[(include-book \"book1\")] occurs in
  ~c[book2], then the compiled file for ~c[book1] will not be loaded again when
  ~c[book2] is included.  Thanks to Dave Greve for bringing our attention to
  these problems, and to Eric Smith for bringing up a special case earlier
  (where \"//\" occurred in the book name).

  The summary printed at the end of a proof had not listed ~c[:]~ilc[induction]
  rules used in a proof.  This has been corrected.

  The use of proof trees in emacs redefined `~c[control-c control-c]' in such a
  way that in telnet mode, the telnet session was interrupted and perhaps could
  not be continued.  This has been fixed.

  Source function ~c[load-theory-into-enabled-structure] contained a
  guard-violating call of ~ilc[compress1].  Thanks to Vernon Austel for
  bringing this problem to our attention; even though this bug was benign
  (as he pointed out), we like keeping the source code free of guard
  violations.

  A number of proof-checker atomic macros caused a hard error when all goals
  have already been proved.  This has been fixed.  Thanks to John Erickson for
  sending an example of the issue.

  A bug has been fixed in ~ilc[add-match-free-override].  Formerly, a
  ~ilc[table] ~il[guard] violation occurred when calling
  ~ilc[add-match-free-override] more than once with first argument other than
  ~c[:clear].

  Defininitions of functions involving large constants could cause stack
  overflows.  This has been fixed, at least in some of the most egregious
  cases (by making a source function ~c[fn-count-evg] tail-recursive).  Thanks
  to Jared Davis for bringing this problem to our attention.

  Evaluation of computed hints could cause stack overflows.  This has been
  fixed.  Thanks to Eric Smith for bringing this problem to our attention.

  Evaluation of ~c[:]~ilc[monitor] on ~c[:]~ilc[definition] ~il[rune]s is now
  fast even if the specified function is part of a very large
  ~ilc[mutual-recursion] nest.  Thanks to Eric Smith for sending an example
  showing that this wasn't always the case.

  Fixed a bug in ~c[books/bdd/cbf.lisp] that was causing certification of
  distributed bdd books to fail when the connected book directory (~pl[cbd])
  differs from the current working directory.  Thanks to Scott Guthery for
  bringing this bug to our attention and supplying a helpful log.

  Duplicate rule names have been eliminated from warnings generated upon the
  use of enabled ~c[:]~ilc[rewrite] or ~c[:]~ilc[definition] rules.  Thanks to
  Eric Smith for pointing out this problem.

  The trace utilities (~pl[trace]), as modified for GCL and Allegro Common
  Lisp, had failed to show more than the first return value for so-called
  ``~c[*1*]'' functions (essentially, ~il[executable-counterpart] functions)
  when they were returning multiple values (via ~il[mv]).  This has been fixed.
  Thanks to Erik Reeber for pointing out this problem.
  Also, it is now possible to refer to ~c[arglist] in ~il[trace$] forms when
  ACL2 is built on GCL, not just when ACL2 is built on Allegro Common Lisp.

  Uses of ~ilc[hide] introduced during proofs by failed attempts to evaluate
  constrained functions (~pl[hide]) are now tracked, so that the ~il[rune]
  ~c[(:DEFINITION HIDE)] will show up in the summary.

  The following bug, introduced back in Version  2.7, has been fixed.  The bug
  applied only to GCL and may well not have affected anyone.  But the function
  proclamation computed by ACL2 for compilation usually had an output type of
  ~c[nil] where it should have been ~c[t].

  The macro ~ilc[gc$] had a bug exhibited when it was supplied one or more
  arguments.  This has been fixed.

  The macro ~ilc[defabbrev] broke when supplied a string and no documentation,
  e.g., ~c[(defabbrev foo () \"\")].  Thanks to Rob Sumners for noticing this
  problem and providing a fix, which we have incorporated.

  For ACL2 executables built on Allegro Common Lisp, a Lisp error occurred when
  ~ilc[trace$] was called on other than a defined function symbol.  Now ACL2
  prints a more useful error message.

  The proof-checker no longer accepts a ~c[(]~ilc[verify]~c[)] command when
  some function symbol in the original goal no longer exists in the current
  ACL2 logical ~il[world].  Thanks to John Erickson for bringing this issue
  to our attention.

  The function ~c[ld-redefinition-action] may now be called by the user.
  Thanks to Vernon Austel for suggesting that we remove this symbol from
  the list of so-called untouchables.

  The handling of free variables in hypotheses (~pl[free-variables]) of rewrite
  and linear rules had a bug that prevented some proofs from going through.
  Here is a simple example, essentially provided by Diana Moisuc, who we thank
  for bringing this issue to our attention.  The proof of the ~ilc[thm] below
  had failed, but now will succeed.  This particular bug prevented, for
  example, the ~c[:all] behavior from occurring when the first hypothesis of
  the rule does not have free variables.  NOTE:  Now that this bug has been
  fixed, you may find some proofs running much more slowly than before.  You
  can use ~ilc[accumulated-persistence] to locate rules that are slowing down
  your proofs because of excessive attention to free variables, and then
  execute ~ilc[add-match-free-override] for those rules (or, just change the
  rules themselves to specify ~c[:once] in the ~c[:]~ilc[rule-classes]).
  ~bv[]
  (defstub foo1 (* ) => *)
  (skip-proofs
   (defthm aux-foo1
     (implies (and (integerp a)
                   (integerp i)
                   (equal (foo1 0)  (list 0 i)))
              (equal (foo1 a) (list 0 (+ a i))))
     :rule-classes ((:rewrite :match-free :all))))
  (thm
   (implies (and (integerp i) 
                 (integerp a) 
                 (equal (foo1 0) (list 0 i)))
            (equal (foo1 a) (list 0 (+ a i)))))
  ~ev[]

  Formerly, creation of large arrays could cause an error in the underlying
  Common Lisp implementation without helpful messages for the user.  Now, we
  check Common Lisp restrictions on arrays and print a helpful error message if
  they are violated, namely: each dimension must be less than the value of
  Common Lisp constant ~c[array-dimension-limit], and the product of the
  dimensions must be less than the value of Common Lisp constant
  ~c[array-total-size-limit].  Thanks to Warren Hunt for bringing this issue to
  our attention.  Note:  this change also removes a former restriction of
  ~ilc[stobj] array fields to size smaller than 2^28-1, provided the underlying
  Lisp can support larger arrays.

  The ~il[default-hints] in the current logical ~il[world] were ignored by
  ~ilc[verify-guards].  This has been fixed.  Thanks to Jared Davis for
  pointing out this bug and sending a helpful example.

  The ~ilc[brr] mechanism has been cleaned up in order to avoid hard errors and
  infinite loops that can arrive when typing interrupts (~c[control-c]) or
  end-of-files (~c[control-d]) inside the ~ilc[brr] loop.  Thanks to Dave
  Greve, Olga Matlin, Eric Smith, and Serita Van Groningen for bringing this
  issue to our attention.  As a byproduct, if you type ~c[control-d] (or if
  inside emacs, ~c[control-c control-d]), you may now quit entirely out of ACL2
  and lisp (~pl[good-bye]) in some cases where you formerly would not have, for
  example when sitting at the ACL2 prompt (which formerly, in Allegro Common
  Lisp for example, would merely take you into raw Lisp rather than quitting
  everything).

  We have eliminated structural flaws in the HTML documentation pages that
  could make them unreadable in some browsers.  Thanks to Bill Young for
  bringing this issue to our attention and to Joe Hendrix for diagnosing the
  problem.

  The ~il[proof-checker] could run very slowly after many instructions in a
  given session.  This has been fixed; thanks to Art Flatau for bringing this
  problem to our attention.  (Implementation detail: We now keep tag-trees
  duplicate-free when we accumulate them into state.  This change could have
  minor speed advantages for some top-level proofs too, not just in the
  proof-checker.)

  The printing of accesses to stobjs using nth or update-nth has been done
  using symbolic constants since ACL2 Version_2.6.  However, there was a bug
  that prevented this feature from working for ~ilc[update-nth] except at a
  top-level call.  This has been fixed.  Thanks to Julien Schmaltz for bringing
  this problem to our attention.  For example, consider these events:
  ~bv[]
  (defstobj st field0 field1)
  (thm (equal (nth 1 (update-nth 0 17 st)) (car (cons xxx yyy)))
       :hints ((\"Goal\" :in-theory (disable nth update-nth))))
  ~ev[]
  Before the fix, the proof attempt of the above silly thm printed the
  following.
  ~bv[]
  (NTH 1 (UPDATE-NTH *FIELD0* 17 ST))
  ~ev[]
  After the fix, we instead see the following.
  ~bv[]
  (NTH *FIELD1* (UPDATE-NTH *FIELD0* 17 ST))
  ~ev[]

  It is now possible to certify and subsequently include ~il[books] that
  require guard-checking to be off.  For example, the book can contain the form
  ~c[(defconst *silly* (car 3))] even though ~c[3] fails to satisfy the guard
  of ~ilc[car].  Formerly, it was necessary to execute
  ~c[:]~ilc[set-guard-checking]~c[ nil] before a ~ilc[certify-book] or
  ~ilc[include-book] in order for such a form to be handled without error.
  Thanks to Hanbing Liu for bringing this problem to our attention.

  Fixed a ~il[proof-checker] bug that could cause probably cause strange error,
``Attempt to access the plist field''.  Thanks to Bill Young for bringing this
  problem to our attention.

  Fixed a ~il[proof-checker] bug that was failing to record applications of
  rewrite rules using the proof-checker's ~c[:rewrite] command, causing the
  proof summary to omit mention of that rule (for example, when using the
  proof-checker's ~c[:exit] command to generate an ~c[:instructions] hint).
  Thanks to Bill Young for pointing out this bug.

  Modernized some of the proof-tree emacs and infix printing stuff, thanks to
  suggestions made by Camm Maguire.

  ~/~/")

(deflabel note-2-8-new-functionality

; Not mentioned in detail below:

; Rob's stobj :inline mod moves the live stobj tests from the raw Lisp
; definitions to the *1* code.  The idea is not even to call the raw Lisp code
; with non-live stobjs.

  :doc
  ":Doc-Section note-2-8

  ACL2 Version  2.8 Notes on New Functionality~/

  WARNING: You may find that ~c[control-d] (in emacs, ~c[control-c control-d])
  can throw you completely out of Lisp where it had not formerly done so.

  (CLISP and Allegro CL only) ACL2 now starts up inside the ACL2 loop ~-[] that
  is, ~c[(]~ilc[LP]~c[)] is executed automatically ~-[] when built on CLISP or
  Allegro CL.  This was already the case for GCL and CMUCL, and it still is not
  true for Lispworks.  Thanks to Joe Corneli for bringing the CLISP
  command-line option ~c[\"-i\"] to our attention, which led to this CLISP
  change and inspired reconsideration of how to do this for Allegro CL.

  Pete Manolios and Daron Vroon have changed the representation of ordinals in
  ACL2, defined algorithms for ordinal arithmetic, and created a library of
  theorems to reason about ordinal arithmetic.  We thank them for these nice
  contributions.  ~l[note-2-8-ordinals] for details, in particular, for how to
  preserve existing proofs that depend on the previous ordinal representation.

  Sometimes users create rules of class ~c[:]~ilc[rewrite] that cause an
  infinite loop in the ACL2 rewriter.  This has lead to Lisp stack overflows
  and even segmentation faults.  Now, the depth of calls of functions in the
  ACL2 rewriter is limited, and under user control.  ~l[rewrite-stack-limit].

  Macros ~ilc[mbe] (``must be equal'') and ~ilc[mbt] (``must be true'') have
  been introduced, which allow the user to attach fast executable definitions
  to (presumably slower) ~c[:]~ilc[logic] mode functions.  Thanks to Vernon
  Austel for a key idea.  Also provided is a macro ~ilc[defexec], which employs
  ~ilc[mbe] but enforces the requirement that the executable definition also
  terminates.  Thanks to Jose Luis Ruiz Reina for collaborating in the design
  and development of ~ilc[defexec], and for useful comments from a number of
  others as well in the development of ~c[mbe] including Joe Hendrix and Rob
  Sumners.

  Definitions have been added for functions ~ilc[rassoc-eq] and
  ~ilc[rassoc-equal], which are like ~ilc[rassoc] but use different tests
  and have different guards.  (Compare ~ilc[assoc-eq] and ~ilc[assoc-equal],
  which are in similar relation to ~ilc[assoc].)

  The user can now control multiple matching for free variables in hypotheses
  for ~c[:]~ilc[forward-chaining] rules, as has already been supported for
  ~c[:]~ilc[rewrite] and ~c[:]~ilc[linear] rules.  For ~c[:forward-chaining]
  rules, ``free variables'' are those in the hypotheses not bound by a given
  trigger term.  As for ~c[:rewrite] and ~c[:linear] rules, free-variable
  matching may be limited to the first successful attempt by specifying
  ~c[:match-free :once] with ~c[:forward-chaining] in the
  ~c[:]~ilc[rule-classes], and ~ilc[add-match-free-override] may be used to
  modify the behavior of an existing rule.  Thanks to Erik Reeber for most of
  the implementation of these new capabilities, as well as significant
  assistance with a corresponding new documentation topic
  (~pl[free-variables-examples-forward-chaining]).

  It is no longer necessary to specify ~c[(set-match-free-error nil)] in order
  to avoid errors when a rule with free variables in its hypotheses is missing
  the ~c[:match-free] field.  (This was already true during book certification,
  but now it is the case in interactive sessions as well.)

  The form ~c[(break-on-error)] causes, at least for most Lisps, entry into
  the Lisp debugger whenever ACL2 causes an error.  ~l[break-on-error].  Thanks
  to John Erickson for providing encouragement to provide this feature.

  A new ~ilc[table] has been provided so that advanced users can override the
  built-in ~c[untranslate] functionality.  ~l[user-defined-functions-table].

  The ~ilc[pstack] mechanism (formerly denoted ~c[checkpoints]) has been
  improved.  The ``process [prover] stack,'' or pstack, is automatically
  printed when proofs abort.  Evaluation of function calls on explicit
  arguments during proofs is now tracked.  Actual parameters are shown with
  ~c[(pstack t)] rather than formals.  Thanks to Bill Legato for
  suggesting the first two of these improvements and, in general, encouraging
  changes that make ACL2 easier to use.

  The ~ilc[defstobj] event is now allowed to take an ~c[:inline] argument,
  which can speed up execution.  Thanks to Rob Sumners for suggesting and
  implementing this new feature.

  Macro ~ilc[assert$] has been added in order to make it easy to write
  assertions in one's code.  Semantically, ~c[(assert$ test form)] is the same
  as ~c[form], but it causes a hard error (using ~ilc[illegal]) if ~c[test]
  evaluates to ~c[nil].

  Macro ~ilc[cw-gstack] no longer takes arguments for the gstack or ~ilc[state].
  However, it now takes a keyword argument (which is optional),
  ~c[:evisc-tuple], that can be used to control how it prints terms.  In
  particular, ~c[cw-gstack] abbreviates large terms by default, but
  ~c[(cw-gstack :evisc-tuple nil)] causes terms to be printed in full.
  Thanks to Robert Krug and Eric Smith for requesting this improvement.

  The advanced user now has more control over the evisceration of terms.
  ~l[ld-evisc-tuple], in particular the new paragraph on ``The printing of
  error messages and warnings.''

  The ~ilc[include-book] event now has an additional (optional) keyword,
  ~c[:dir].  The value of ~c[:dir] should be a keyword that is associated with
  an absolute directory pathname to be used in place of the current book
  directory (~pl[cbd]) for resolving the first argument of ~c[include-book] to
  an absolute pathname.  At start-up, the only such keyword is ~c[:system], so
  that for example ~c[(include-book \"arithmetic/top\" :dir :system)] will
  include the book ~c[\"arithmetic/top\"] under the ~c[\"books/\"] directory of
  your ACL2 installation.  But you can associate ``projects'' with keywords
  using ~ilc[add-include-book-dir], e.g.,
  ~c[(add-include-book-dir :my-project \"/u/smith/project0/\")].
  ~l[add-include-book-dir] and also ~pl[delete-include-book-dir] and
  ~pl[include-book].  Note: You will probably not find ~c[:dir :system] to be
  useful if the distributed books are not placed in the path of their original
  location, pointed to by ~c[:dir :system], which will often happen if the
  executable image is obtained from another site.  Also ~pl[include-book], in
  particular its ``soundness warning''.

  The printing of results in raw mode (~pl[set-raw-mode]) may now be partially
  controlled by the user:  ~pl[add-raw-arity].  Also, newlines are printed when
  necessary before the value is printed.

  For those using Unix/Linux ~c[make]: A ~c[cert.acl2] file can contain forms
  to be evaluated before an appropriate ~ilc[certify-book] command is invoked
  automatically (not included in ~c[cert.acl2]).

  Jared Davis has contributed a new set of books for ordered finite set theory
  to the standard distribution, ~c[books/finite-set-theory/osets-0.81/].  See
  the ~c[README] file in that directory.  Thanks, Jared.

  Robert Krug has contributed two related changes (thanks, Robert!) in support
  of stronger arithmetic reasoning.  First, one can now enable and disable
  nonlinear arithmetic with a ~c[:nonlinearp] hint, which will override the
  default provided by ~ilc[set-non-linearp] (initially, ~c[nil]).  ~l[hints].
  Second, ~il[computed-hints] can now have access to the ~c[HISTORY], ~c[PSPV],
  and ~c[CTX] variables of the waterfall, which (for example) allows the
  writing of a hint which will enable nonlinear arithmetic on precisely those
  goals that are ~c[stable-under-simplificationp].  ~l[computed-hints].

  Robert Krug has contributed a new set of arithmetic books to the standard
  distribution, ~c[books/arithmetic-3/].  See the ~c[README] file in that
  directory.  Thanks, Robert.

  ~/~/")

(deflabel note-2-8-proofs
  :doc
  ":Doc-Section note-2-8

  ACL2 Version  2.8 Notes on Changes in Proof Engine~/

  ACL2 now prevents certain rewriting loops; ~pl[rewrite-stack-limit].

  During the computation of ~ilc[constraint]s for functional instantiation,
  ~c[(prog2$ term1 term2)] and ~c[(the type term2)] are now treated as
  ~c[term2].

  A change has been made in heuristics for controlling rewriting during proofs
  by induction.  Formerly, during induction proofs, ACL2 suppressed rewriting
  of certain ``induction hypothesis'' terms, and forced expansion of certain
  ``induction conclusion'' terms, until rewriting had stabilized.  This
  meddling with the rewriter is still turned off when rewriting has stabilized,
  but it is now turned off earlier once an ancestor has been through the
  rewriter and the current goal is free of ``induction conclusion'' terms.
  Thanks to Dave Greve and Matt Wilding for providing an example and associated
  analysis that led us to look for a heuristic modification.

  A change has been made in the heuristics for handling certain ``weak''
  ~il[compound-recognizer] rules when building contexts.  Those who want to dig
  deeply into this change are welcome to look at the code following the call of
  ~c[most-recent-enabled-recog-tuple] in the code for function
  ~c[assume-true-false] in the ACL2 sources.

  The handling of free variables in a hypothesis of a ~il[rewrite] rule
  (~pl[free-variables]) has been improved in the case that the hypothesis is of
  the form ~c[(equiv x y)], where ~c[equiv] is a known equivalence relation
  (~pl[equivalence]).  Previously, if the rewriter was attempting to rewrite
  the hypothesis ~c[(equiv x y)] of a rewrite rule, in a context where ~c[x']
  is an instance of ~c[x], then the rewriter could fail to notice a term
  ~c[(equiv x' y')] true in the current context where ~c[y'] is an instance of
  ~c[y], in the case that ~c[x'] precedes ~c[y'] in the ~ilc[term-order].  This
  has been remedied.  This improvement applies regardless of whether ~c[x],
  ~c[y], or (we believe) both are already fully instantiated in the present
  context.  Thanks to Joe Hendrix for bringing up an example and to Vernon
  Austel for providing another, simple example.

  A very minor change has been made to the rewriter in the case that an
  equality appears on the left-hand side of a ~c[:]~ilc[rewrite] rule.
  Formerly, when such an equality ~c[(equal x y)] was commuted to
  ~c[(equal y x)] in order for the rule to match the current term, then all
  equalities on the instantiated right-hand side of the rule were commuted,
  except for those occurring inside another equality.  The instantiated
  right-hand side is no longer modified.  It seems very unlikely that this
  change will cause proofs to fail, though we cannot completely rule out that
  possibility.

  We have modified how the ACL2 simplifier handles the application of a defined
  function symbol to constant arguments in certain cases, which we now
  describe.  As before, ACL2 attempts to simplify such a function application
  by evaluation, provided the ~c[:]~ilc[executable-counterpart] of the function
  is enabled.  And as before, if that evaluation fails due to a subroutine call
  of a constrained function (introduced by ~ilc[encapsulate]), ACL2 may wrap a
  call of ~c[hide] around this function application.  (~l[hide].)  But now,
  ACL2 attempts to apply definitions and rewrite rules in the case that this
  evaluation fails, and only if the resulting term is unchanged does ACL2 wrap
  ~ilc[hide] around this function application.  Thanks to Matt Wilding for
  bringing up the idea of this modification.

  The generation of \"Goal\" for recursive (and mutually-recursive) definitions
  now uses the subsumption/replacement limitation (default 500).
  ~l[case-split-limitations].

  Default hints now apply to hints given in definitions, not just theorems.
  ~l[default-hints].

  Thanks to Robert Krug for implementing the following two improvements
  involving linear arithmetic reasoning: linear arithmetic now uses the
  conclusions of ~ilc[forward-chaining] rules, and ~ilc[type-set] now uses a
  small amount of linear reasoning when deciding inequalities.

  ~/~/")

(deflabel note-2-8-rules
  :doc
  ":Doc-Section note-2-8

  ACL2 Version  2.8 Notes on Changes in Rules, Definitions, and Constants~/

  The ~il[theory] ~ilc[minimal-theory] has been changed by adding the
  ~il[definition] ~il[rune] for ~ilc[mv-nth] to the theory.  A corresponding
  change has been made to the theory warning mechanism, which was failing to
  warn if the definition of ~c[mv-nth] is disabled, even though calls of
  ~c[mv-nth] can be expanded by special-purpose code in the rewriter.  Thanks
  to Serita Van Groningen for pointing out this problem with the theory warning
  mechanism. 

  The ~ilc[defevaluator] event has been modified so that in the body of the
  evaluator function, to add a new case ~c[(ATOM X)] (returning ~c[nil]) has
  been inserted immediately after the case ~c[(EQ (CAR X) 'QUOTE)].  This is a
  no-op semantically but may speed up proofs.  Thanks to Warren Hunt for
  suggesting this change.

  A new form of ~c[:]~ilc[compound-recognizer] rule is now allowed:
  ~bv[]
  (if (fn x) concl1 concl2)
  ~ev[]
  This is equivalent to an existing form:
  ~bv[]
  (and (implies (fn x) concl1)
       (implies (not (fn x)) concl2))
  ~ev[]
  Thanks to Josh Purinton for bringing this to our attention.

  Rewrite rules ~c[realpart-+] and ~c[imagpart-+] have been added in order
  to simplify the ~ilc[realpart] and ~ilc[imagpart] (respectively) of a sum.
  They follow from a theorem ~c[add-def-complex] that equates a sum with
  the complex number formed by adding real and imaginary parts.  All three
  of these theorems may be found in source file ~c[axioms.lisp].  Thanks to
  Eric Smith for raising a question leading to these additions, as well as
  to Joe Hendrix and Vernon Austel for helpful suggestions.

  ~/~/")

(deflabel note-2-8-guards
  :doc
  ":Doc-Section note-2-8

  ACL2 Version  2.8 Notes on Guard-related Changes~/

  All the guard-related changes may be found elsewhere; in particular,
  ~pl[note-2-8-bug-fixes].

  ~/~/")

(deflabel note-2-8-proof-checker
  :doc
  ":Doc-Section note-2-8

  ACL2 Version  2.8 Notes on Proof-checker Changes~/

  Added new ~il[proof-checker] commands ~c[wrap1], ~c[wrap], and
  ~c[wrap-induct].  ~c[Wrap] replaces multiple goals by their conjunction:
  ~c[(wrap instr1 instr2 ...)] employs ~c[wrap1] so that the indicated
  instructions create only at most one new goal.  ~c[Wrap-induct] is a simple
  example of the use of ~c[wrap], so that induction creates only one goal (the
  conjunction of the base and induction steps).  ~c[Wrap1] can be used
  immediately after a prover call (~c[bash], ~c[prove], ~c[reduce], ~c[bdd], or
  ~c[induct]) to collapse the new goals into one.  ~l[proof-checker-commands].

  The ~il[proof-checker] command ~c[=] failed to work as expected when a
  governing ~c[IF]-test of the current term is T.  This has been fixed (by
  fixing source function ~c[conjuncts-of]).  Thanks to Yoann Padioleau for
  bringing this problem to our attention.

  The ~c[type-alist] command now takes optional arguments that control whether
  or not the governors and/or conclusion are used in computing the context
  that is printed (~pl[proof-checker-commands], specifically subtopic
  ~c[type-alist]).  Thanks to Rob Sumners for suggesting this improvement.

  The macro ~ilc[toggle-pc-macro] has always taken an optional second argument
  of ~c[atomic-macro] or ~c[macro].  However, this was not clearly documented,
  and those two symbols had to be in the ~c[ACL2] package.  Both of these
  problems have been remedied.  Thanks to John Erickson for bringing the lack
  of documentation of the second argument to our attention.

  ~/~/")

(deflabel note-2-8-system
  :doc

; Modified compile-uncompiled-defuns/compile-uncompiled-*1*-defuns to write
; comment to the top of TMP.lisp/TMP1.lisp saying that file can be deleted.
; (Matyas Sustik had asked about these files.)

  ":Doc-Section note-2-8

  ACL2 Version  2.8 Notes on System-level Changes~/

  ACL2 now runs on OpenMCL, ``an opensourced Common Lisp implementation,
  derived from Digitool's Macintosh Common Lisp product.''  Thanks to Greg
  Wright and Robert Krug for doing most of the work for this port.

  When ~c[(]~ilc[LP]~c[)] is first executed, the underlying raw Lisp package
  will change to ~c[\"ACL2\"] (if that is not already the current package in
  raw  Lisp).  This is a minor change that will probably not be noticed, since
  up to now it has probably been the case that the ACL2 executable starts up
  with ~c[\"ACL2\"] as the underlying raw Lisp package.  But this change was
  made because we have been informed that ACL2 executables based on OpenMCL
  need not start up with ~c[\"ACL2\"] as the underlying raw Lisp package.

  ACL2 now runs on MCL 5.0.  Thanks to Pascal Costanza for updates to the
  instructions in file ~c[mcl-acl2-startup.lisp] and for an update to the
  ACL2 sources (parameter ~c[*compiled-file-extension*]).

  ~/~/")

(deflabel note-2-8-ordinals
  :doc
  ":Doc-Section note-2-8

  ACL2 Version  2.8 Notes on Changes to the Ordinals~/

  Please ~pl[ordinals].~/~/")

(deflabel note-2-8-other

; Changed ev-fncall, ev, etc. so that we could have versions that take the
; world (see note below about user-defined-functions-table).

; Used memoization to speed up global-symbol and *1*-symbol by avoiding string
; concatentation.

; Eliminated all-new-flg/all-new-flag from the following, as it always had
; value nil (Robert Krug agrees with this change).
;   new-vars-in-pot-lst (formerly arg 3)
;   expanded-new-vars-in-pot-lst (formerly arg 3)
;   add-polys-and-lemmas1-nl (formerly arg 2)
;   add-polys-and-lemmas (formerly arg 2)
;   add-disjunct-polys-and-lemmas (formerly arg 3)
;   add-disjuncts-polys-and-lemmas (formerly arg 2)
;   add-terms-and-lemmas (formerly arg 4)

  :doc
  ":Doc-Section note-2-8

  ACL2 Version  2.8 Notes on Miscellaneous Changes~/

  Execution of ~ilc[table] events has been sped up in many cases by avoiding
  excessive consing.

  ACL2 now warns if ~c[:]~ilc[rewrite] (or ~c[:]~ilc[definition]) rules contain
  free variables on the right-hand side.  Thanks to Dave Greve for raising this
  issue.

  Emacs file ~c[emacs/emacs-acl2.el] has been updated to better comprehend the
  notion of the ``ACL2 shell'', which is the buffer to which ACL2 forms are
  written by commands defined in the above file.  Thus, command ~c[control-t e]
  has been modified always to write to the ACL2 shell (which is ~c[\"*shell*\"]
  by default), and the following new commands have been defined.
  ~bq[]
  o control-t c~nl[]
    Set the ACL2 shell to the current buffer.
  o control-t b~nl[]
    Change to the ACL2 shell.
  ~eq[]

  The commands ~c[:]~ilc[pl] and ~c[:]~ilc[pr] may now be given a macro name
  that corresponds via the ~c[macro-aliases-table] to a function name, so that
  for example ~c[:pl append] is treated the same as ~c[:pl binary-append].  A
  more interesting improvement, for ~c[:pl] only, is that ~c[:pl] may now take
  any term.  When ~c[:pl] is given a term other than a symbol, it will print
  all rewrite rules that match that term.  Thanks to David Russinoff, Robert
  Krug, and Bill Legato for getting this going.

  A new function, ~ilc[pkg-witness], returns a symbol in the given package.

  The installation instructions have been updated, for example to give more
  guidance on obtaining Lisp implementations and to mention the acl2-help
  mailing list.

  Jared Davis has suggested some symbols to be added to ~c[*acl2-exports*],
  and we have done so.  Thanks, Jared.
  ~bq[]
  o ~c[MFC] (used in ~ilc[syntaxp] and ~ilc[extended-metafunctions]; thanks
  also to Robert Krug for this one)
  o ~c[ID], ~c[CLAUSE], ~c[WORLD], and ~c[STABLE-UNDER-SIMPLIFICATIONP] (used
    in ~ilc[computed-hints])
  o ~ilc[SET-DEFAULT-HINTS]
  ~eq[]

  The command ~c[:]~ilc[pe] has been improved so that when the event is inside
  an included book, the path of included books (from the top-level book down to
  the one containing the event) is shown.  Thanks to Eric Smith (perhaps among
  others) for pointing out the utility of this improvement.

  A new release of the rtl library has been included: ~c[books/rtl/rel4/].
  See the ~c[README] file in that directory.

  ~/~/")

(deflabel |NOTE-2-8(R)|
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.8(r) (March, 2003) Notes~/

  The ~c[Makefile] has been modified by adding a new target, ~c[clean-links].
  This can be used in order to remove all soft links, which is useful if the
  directory is copied or moved to a new location or if there are file system
  changes that cause problems with link pathnames.

  ~/

  Please also ~pl[note-2-8] for changes to Version_2.8 of ACL2.
  ~/
  ")

(deflabel note-2-9

; BUG FIXES:
; Example for the soundness bug involving *1* package names:

; ============================== sub.lisp ==============================
; 
; #|
; (defpkg "ACL2_*1*_MYPKG" ())
; (certify-book "sub" 1)
; |#
; 
; (in-package "ACL2_*1*_MYPKG")
; 
; (acl2::defun foo (x)
;              (acl2::declare (acl2::xargs :verify-guards acl2::t))
;              x)
; 
; ============================== top.lisp ==============================
; 
; #|
; (include-book "sub")
; (defpkg "MYPKG" ())
; (certify-book "top" 2)
; |#
; 
; (in-package "ACL2")
; 
; (defthm lemma1
;   (equal (acl2_*1*_mypkg::foo 3) 3)
;   :rule-classes nil)
; 
; (defun mypkg::foo (x)
;   (cons x x))
; 
; (defthm lemma2
;   (equal (acl2_*1*_mypkg::foo 3) '(3 . 3))
;   :rule-classes nil)
; 
; (defthm ouch
;   nil
;   :rule-classes nil
;   :hints (("Goal" :use (lemma1 lemma2))))

; End of example related to *1* package names.

; Example related to soundness bug on local and redundancy checking:

; (encapsulate
;  ()
;  (defun foo (x)
;    (declare (xargs :mode :program))
;    (zp x))
;  (local (verify-termination foo))
;  (defun bar (x)
;    (foo x))
;  (defthm thm-1
;    (bar -1)
;    :rule-classes nil))
; 
; (defthm thm-2
;   (not (bar -1))
;   :rule-classes nil)
; 
; (defthm bad
;   nil
;   :rule-classes nil
;   :hints (("Goal" :use (thm-1 thm-2))))

; Here's a related example, showing how to get a world where a
; :common-lisp-compliant function, bar, calls an :ideal function, foo.

; ============================== abc.lisp ==============================
; 
; (in-package "ACL2")
; 
; (defun foo (x)
;   (declare (xargs :guard (consp x)))
;   (car x))
; 
; ============================== abc-top.lisp ==============================
; 
; (in-package "ACL2")
; 
; (local (include-book "abc"))
; 
; (defun foo (x)
;   (declare (xargs :guard (consp x) :verify-guards nil))
;   (car x))
; 
; (defun bar (x)
;   (declare (xargs :guard (consp x)))
;   (foo x))

; End of example related to soundness bug on local and redundancy checking.

; Example related to soundness bug pertaining to safe-mode.

; ============================== bad1.lisp ==============================
; 
; (in-package "ACL2")
; 
; (defconst *c* '(((a b))))
; 
; (defconst *d* *c*)
; 
; (defmacro bad-macro ()
;   (list 'quote (union-eq-cars (list *c* *d*))))
; 
; (defthm thm1
;   (equal (bad-macro)
;          '((a b)))
;   :rule-classes nil)
; 
; ============================== bad2.lisp ==============================
; 
; (in-package "ACL2")
; 
; (defconst *c* '(((a b))))
; 
; (defconst *d* '(((a b))))
; 
; (defmacro bad-macro ()
;   (list 'quote (union-eq-cars (list *c* *d*))))
; 
; (defthm thm2
;   (equal (bad-macro)
;          '((a b) (a b)))
;   :rule-classes nil)
; 
; ============================== bad.lisp ==============================
; 
; (in-package "ACL2")
; 
; (include-book "bad1" :load-compiled-file nil)
; 
; (include-book "bad2" :load-compiled-file nil)
; 
; (defthm ouch
;   nil
;   :hints (("Goal" :use (thm1 thm2)))
;   :rule-classes nil)
; 
; ======================================================================

; End of example related to soundness bug pertaining to safe-mode.

  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.9 (October, 2004) Notes~/

  ~bf[]
  ~st[TABLE OF CONTENTS.]~nl[]
  ==============================
  BUG FIXES.
  NEW FUNCTIONALITY.
  CHANGES IN PROOF ENGINE.
  GUARD-RELATED CHANGES.
  PROOF-CHECKER CHANGES.
  SYSTEM-LEVEL CHANGES.
  BOOK CHANGES.
  MISCELLANEOUS CHANGES.
  ==============================
  ~ef[]

  ~st[BUG FIXES.]

  We fixed a soundness bug due to a conflict between user-supplied package
  names and internal package names (obtained by prepending a Lisp constant,
  ~c[*1*-package-prefix*]) and user-supplied package names.  For example, the
  form ~c[(defpkg \"ACL2_*1*_MYPKG\" ())] is no longer legal.  Thanks to Robert
  Krug for asking a question that led directly to the discovery of this bug.

  We fixed a soundness bug that allows ~c[:]~ilc[logic] mode functions to call
  ~c[:]~ilc[program] mode functions.  The fix furthermore prevents functions
  with ~il[guard]s verified from calling functions with guards not verified.
  We had thought we already prevented all this, but there was a problem with
  the interaction of ~ilc[local] definitions and redundancy checking
  (~pl[redundant-events]).

  We fixed a soundness bug that could occur when built-in functions were called
  during macroexpansion (a hole in so-called ``safe-mode'').

  Fixed a minor bug in system functions ~c[genvar1] and ~c[genvar], where
  ~ilc[eq] had been used in place of ~ilc[eql].  This bug was discovered during
  the plugging of a hole in safe-mode, mentioned just above.

  We fixed handling of the ~c[:inline] keyword for ~ilc[defstobj], which
  previously could cause raw Lisp errors in OpenMCL and CMU Common Lisp.
  Thanks to John Matthews for bringing this issue to our attention.

  Calls of ~ilc[include-book] could result in a state for which some function
  definitions were not compiled that should have been.  The result could be
  performance degradation or even stack overflows.  This situation could arise
  in the following two ways.~bq[]

  o Inclusion of a book with an absolute pathname that differs from the
  absolute pathname at certification time, presumably because of the use of
  soft links.  Thanks to Bob Boyer and Warren Hunt for bringing a stack
  overflow to our attention that led us to this fix.

  o Large ~ilc[mutual-recursion] nests (more than 20 functions) are executed
  in a superior book.

  ~eq[]
  We fixed some performance bugs that can increase the speed of
  ~ilc[include-book] calls by a factor of close to 2.  Thanks to Eric Smith for
  asking if we could speed up ~ilc[include-book] processing; we have done so in
  the past, but primarily focusing on large ~ilc[mutual-recursion] nests (which
  have nothing in particular to do with the current improvements).  Also,
  thanks to Rob Sumners for a very useful remark early in the process that kept
  us from drawing an incorrect conclusion at that point.

  We fixed ~c[:]~ilc[pl] so that it can be run on a form that returns multiple
  values, which it could not do previouslly.  Thanks to Eric Smith for pointing
  out this problem.

  Fixed a bug in the Allegro ACL2 trace utility (~pl[trace$]) that was causing
  ``~c[10>]'' to be printed as ``~c[9>]'', ``~c[11>]'' to be printed as
  ``~c[10 >]'', ``~c[12>]'' to be printed as ``~c[11 >]'', and so on.

  Fixed a ~il[proof-checker] bug that was preventing the use of the ~c[DV]
  command (or a numerical command) on ~ilc[let] expressions.  Thanks to Bill
  Young for pointing out this problem.

  Fixed a bug in a comment on how to set ~c[ACL2_BOOKS_DIR] in the makefile.
  Thanks to Dave Greve for pointing out this problem.

  Fixed a potential soundness bug in the linear arithmetic routines.  Thanks to
  Jared Davis for noticing this problem and to Robert Krug for implementing the
  fix.  (Technical details: We had been assuming that polynomials were being
  normalized -- see the definition of good-polyp in linear-a.lisp -- but this
  assumption was false.)

  When the macro ~ilc[open-trace-file] is opened twice in succession, it now
  automatically closes the first trace output channel before opening another.

  It is now possible to use ~c[make] to build ACL2 on Windows systems that
  support ~c[make].  Thanks to Pete Manolios and Mike Thomas for pointing out
  the problem, to Jared Davis and Mike for helping us to analyze the problem,
  and to Jared for testing the fix.

  Fixed a bug in the ~il[guard] of ~ilc[with-output] that was causing a
  needless guard violation.

  ~st[NEW FUNCTIONALITY.]

  The new events ~ilc[add-default-hints] and ~ilc[remove-default-hints] allow
  one to append to or subtract from the current list of default hints.  The
  event ~ilc[set-default-hints] continues to set the list of default hints,
  discarding the previous value of the ~ilc[default-hints].  Note that
  ~ilc[set-default-hints] is still ~ilc[local] to the ~ilc[encapsulate] or book
  in which it occurs, and ~ilc[add-default-hints] has the same property,
  although neither is implemented any longer using the
  ~ilc[acl2-defaults-table].  New events ~ilc[add-default-hints!],
  ~ilc[remove-default-hints!], and ~ilc[set-default-hints!] are the same as
  ~ilc[add-default-hints], ~ilc[remove-default-hints], and
  ~ilc[set-default-hints], respectively, except that the former three events
  are not ~ilc[local] to their enclosing ~ilc[encapsulate] or book.  Thanks to
  Jared Davis for suggesting these enhancements.

  OpenMCL's tracing routines have been modified in a similar manner as those
  of Allegro CL.  Thanks to Robert Krug for providing this enhancement.

  Guard-checking can now be caused to happen on recursive calls.
  See ``GUARD-RELATED CHANGES'' below for details.

  Advanced users can now inhibit compilation of so-called ``*1* functions''
  with the ~c[:comp] command; ~pl[comp].  Thanks to Rob Sumners for suggesting
  this enhancement.

  Added new legal argument ~c[hard?] for the ~ilc[er] macro, which is now
  documented.  ~l[er].  Thanks to Rob Sumners for a discussion leading to this
  change.  Also, the three legal first arguments to ~ilc[er] ~-[] ~c[hard],
  ~c[hard?], and ~c[soft] ~-[] may now be in any package (thanks to Jared Davis
  for bringing this issue to our attention).

  We have removed the requirement that for a rule's hypothesis
  ~c[(bind-free term var-list)], at least one variable must occur free in
  ~c[term].  For example, the expression ~c[(bind-free (bind-divisor a b) (x))]
  was legal because ~c[a] and ~c[b] occur free in ~c[(bind-divisor a b)]; but
  ~c[(bind-free (foo (bar)) (x))] was not legal.  The latter is no longer
  disallowed. (Technical note: this allows ~ilc[bind-free] to be used to create
  explicit substitutions in metafunction hypotheses.)

  The following two enhancements have been implemented for rules of class
  ~c[:]~ilc[meta].  Thanks to Eric Smith for requesting more control of
  reasoning with ~c[:]~ilc[meta] rules, which led to these enhancements, and to
  him and Robert Krug for helpful discussions.~bq[]

  o It is now possible to control backchaining in rules of class
  ~c[:]~ilc[meta] by providing a ~c[:backchain-limit-lst] argument, as was
  already allowed for rules of class ~c[:]~ilc[rewrite] and ~c[:]~ilc[linear].
  ~l[rule-classes].  However, unlike those other two rule classes, the value
  for ~c[:backchain-limit-lst] is prohibited from being a non-empty list; it
  must be either ~c[nil] or a non-negative integer.

  o (For advanced users.) It is now legal for hypothesis metafunctions to
  generate, in essense, calls of ~ilc[syntaxp] and ~ilc[bind-free], handled
  essentially as they are handled in hypotheses of ~c[:]~ilc[rewrite] and
  ~c[:]~ilc[linear] rules.  We say ``essentially'' primarily because both
  ~ilc[syntaxp] and ~ilc[bind-free] are actually macros, but hypothesis
  metafunctions must generate translated terms (~pl[term]).  The enterprising
  advanced user can call ~c[:]~ilc[trans] to see examples of translated terms
  corresponding to calls of ~ilc[syntaxp] and ~ilc[bind-free].

  ~eq[]A new command ~c[:]~ilc[trans!] has been added, which is like
  ~c[:]~ilc[trans] except that ~c[:]~ilc[trans!] ignored issues of
  single-threadedness.  ~l[trans!].  Thanks to Eric Smith for suggesting this
  addition.

  The ~c[:]~ilc[pf] command now works when the argument is the name of a macro
  associated with a function by ~il[macro-aliases-table].

  ~st[CHANGES IN PROOF ENGINE.]

  The simplifier has been changed slightly in order to avoid using
  ~il[forward-chaining] facts derived from a literal (essentially, a top-level
  hypothesis or conclusion) that has been rewritten.  As a practical matter,
  this may mean that the user should not expect forward-chaining to take place
  on a term that can be rewritten for any reason (generally function expansion
  or application of rewrite rules).  Formerly, the restriction was less severe:
  forward-chaining facts from a hypothesis could be used as long as the
  hypothesis was not rewritten to ~c[t].  Thanks to Art Flatau for providing an
  example that led us to make this change; see the comments in source function
  ~c[rewrite-clause] for details.

  The rewriter has been modified to work slightly harder in relieving
  hypotheses.  Thanks to Eric Smith for providing an example that inspired the
  following, which illustrates the issue.  Suppose we introduce functions
  ~c[foo] and ~c[bar] with the (non-~ilc[local]) properties shown below.
  ~bv[]
   (encapsulate
    (((foo *) => *)
     ((bar *) => *))

    (local (defun foo (x) (declare (ignore x)) t))
    (local (defun bar (x) (declare (ignore x)) t))

    (defthm foo-holds
      (implies x
               (equal (foo x) t)))
    (defthm bar-holds-propositionally
      (iff (bar x) t)))
  ~ev[]
  Consider what happens when ACL2's rewriter is used to prove the following
  theorem.
  ~bv[]
  (thm (foo (bar y)))
  ~ev[]
  With ACL2's inside-out rewriting, ~c[(bar y)] is first considered, but
  rewrite rule ~c[bar-holds-propositionally] does not apply because the context
  requires preserving equality, not mere Boolean (~c[iff]) equivalence.  Then
  the rewriter moves its attention outward and sees the term ~c[(foo (bar y))].
  It attempts to apply the rule ~c[foo-holds], in a context created by binding
  its variable ~c[x] to the term ~c[(bar y)].  It then attempts to relieve the
  hypothesis ~c[x] of rule ~c[foo-holds] in that context.  Before this change,
  ACL2 basically assumed that since rewriting was inside out, then ~c[(bar y)]
  had already been rewritten as much as possible, so the rewrite of ~c[x] in
  the aforementioned context (binding ~c[x] to ~c[(bar y)]) simply returned
  ~c[(bar y)], and the attempt to relieve the hypothesis of ~c[foo-holds]
  failed.  The change is essentially for ACL2's rewriter to make a second pass
  through the rewriter when the attempt fails to rewrite a variable to ~c[t],
  this time using the fact that we are in a Boolean context.  (We mention that
  source function ~c[rewrite-solidify-plus] implements this idea, for those who
  want to dig deeply into this issue.)  In our example, that means that the
  rewriter considers ~c[(bar y)] in a Boolean context, where it may apply the
  rule ~c[bar-holds-propositionally] to relieve the hypothesis successfully.

  When ~c[(]~ilc[set-non-linearp]~c[ t)] has been executed,
  ~il[non-linear-arithmetic] can now be applied in some cases for which it
  previously was not.  Thanks to Robert Krug for supplying this modification
  and to Julien Schmaltz for providing a motivating example.

  We modified the rewriter to avoid certain infinite loops caused by an
  interaction of the opening of recursive functions with equality reasoning.
  (This change is documented in detail in the source code, in particular
  functions ~c[rewrite-fncall] and ~c[fnstack-term-member].)  Thanks to Fares
  Fraij for sending us an example that led us to make this change.

  The ~c[:]~ilc[executable-counterpart] of function ~ilc[hide] is now disabled
  when ACL2 starts up.  This removes an anomoly, for example that
  ~bv[]
  (thm (not (equal 1 (* (hide 0) a))))
  ~ev[]
  succeeded while
  ~bv[]
  (thm (equal (foo (equal 1 (* (hide 0) a))) (foo nil)))
  ~ev[]
  failed.  Now both fail.

  The theory ~c[*s-prop-theory*] is no longer used by the ~i[proof-checker];
  it has been replaced by ~c[(theory ']~ilc[minimal-theory].  We have left
  the constant ~c[*s-prop-theory*] defined in the source code in support of
  existing books, however.  This change eliminates annoying theory warnings
  printed upon invocation of ~il[proof-checker] commands ~c[s-prop], ~c[sl],
  and ~c[split].

  Terms are now kept in an internal form that avoids calls of primitive
  functions (built-ins without explicit definitions; see code for ~c[cons-term]
  for details), in favor of the constants that result from evlaluating those
  calls.  So for example, the internal form for ~c[(cons 1 2)] is
  ~c[(quote (1 . 2))].  This change was made at around the same time as changes
  in support of ~ilc[bind-free]; see above.  One consequence is that the
  splitting of goals into cases (technically, source function ~c[clausify] and
  even more technically, source function ~c[call-stack]) has been modified,
  which can cause subgoal numbers to change.

  ~st[GUARD-RELATED CHANGES.]

  Guard-checking can now be caused to happen on recursive calls, where this was
  formerly not the case for ~c[:]~ilc[program] mode functions and, perhaps more
  important, for ~c[:]~ilc[logic] mode functions whose ~il[guard]s have not
  been verified.  Moreover, a warning is printed when ACL2 does not rule out
  the exclusion of guard-checking on recursive calls.  ~l[set-guard-checking].
  Thanks to David Rager for bringing this issue to our attention, and to Rob
  Sumners and the Univ. of Texas ACL2 seminar in general for their feedback.

  Guard violations are reported with less of the offending term hidden.  Thanks
  to Jared Davis for suggesting that we look at this issue.

  ~st[PROOF-CHECKER CHANGES.]

  We fixed the ~il[proof-checker] so that diving works as you might expect for
  a macro call ~c[(op a b c)] representing ~c[(binary-op a (binary-op b c))].
  In the past, if the current term was of the form ~c[(append t1 t2 t3)], then
  ~c[(DV 3)] (and ~c[3]) would dive to ~c[t3] even though the corresponding
  function call is ~c[(binary-append t1 (binary-append t2 t3))].  This is still
  the case, but now this behavior holds for any macro associated with a
  function in ~ilc[binop-table] (~pl[add-binop]).  Moreover, users can now
  write customized diving functions; ~pl[dive-into-macros-table], and also see
  ~c[books/misc/rtl-untranslate.lisp] for example calls to
  ~ilc[add-dive-into-macro].  Of course, the old behavior can still be obtained
  using the ~il[proof-checker]'s ~c[DIVE] command; ~pl[proof-checker-commands].

  The ~c[runes] command in the ~il[proof-checker] now shows only the ~il[rune]s
  used by the most recent primitive or macro command (as shown by ~c[:comm]),
  unless it is given a non-~c[nil] argument.  Also, ~il[proof-checker] command
  ~c[lemmas-used] has been added as, in essence, an alias for ~c[runes].

  (The following two items are also mentioned above under ``BUG FIXES.'')

  Fixed a ~il[proof-checker] bug that was preventing the use of the ~c[DV]
  command (or a numerical command) on ~ilc[let] expressions.  Thanks to Bill
  Young for pointing out this problem.

  The theory ~c[*s-prop-theory*] is no longer used by the ~i[proof-checker];
  it has been replaced by ~c[(theory ']~ilc[minimal-theory].  We have left
  the constant ~c[*s-prop-theory*] defined in the source code in support of
  existing books, however.  This change eliminates annoying theory warnings
  printed upon invocation of ~il[proof-checker] commands ~c[s-prop], ~c[sl],
  and ~c[split].

  ~st[SYSTEM-LEVEL CHANGES.]

  Fixed a problem with building ACL2 on CMUCL in some systems (source function
  ~c[save-acl2-in-cmulisp]).  Thanks to Bill Pase for bringing this to our
  attention.

  The installation instructions have been extended to include instructions for
  building on GCL in Mac OS X.  Thanks to Jun Sawada and Camm Maguire.

  Initial pre-allocation of space has been updated for GCL to reflect more
  current GCL executables (we considered GCL 2.6.1-38).  This can help avoid
  running out of memory for large ACL2 sessions.

  The main ~c[Makefile] has been replaced by ~c[GNUmakefile], in order to
  enforce the use of GNU ~c[make].  If you use another ~c[make] program, you'll
  get an error message that may help you proceed.

  (GCL only) SGC is no longer turned on for GCL 2.6 sub-versions through 2.6.3
  if ~c[si::*optimize-maximum-pages*] is bound to ~c[T], due to an apparent
  issue with their interaction in those sub-versions.  Also, we have eliminated
  preallocation for all versions after 2.6.1 because GCL doesn't need it
  (comments are in source function ~c[save-acl2-in-akcl]).  Thanks to Camm
  Maguire for excellent GCL help and guidance, and to Camm and Bob Boyer for
  useful discussions.

  We have removed support for so-called ``small'' images.  Thus,
  ~c[:]~ilc[doc], ~c[:]~ilc[pe] and ~c[:]~ilc[pc], ~ilc[verify-termination],
  and other commands are fully supported in ACL2 saved images.  Because of this
  and other changes in the generation of the so-called ``*1*'' logical
  functions, related to guards (as described above in -GUARD-RELATED CHANGES'',
  and related to the discussion of safe-mode in ``BUG FIXES'' above), image
  sizes have increased substantially.

  We no longer ~c[time] or run ``~c[nice]'' the certification of individual
  books.  The file ~c[books/Makefile-generic] had done these by default, and
  some individual distributed and workshop book directories had ~c[Makefile]s
  that did so as well.  Thanks to Mike Thomas, who pointed out the lack of
  ~c[nice] on some Windows systems (and we decided on this simple solution).
  Overall targets in ~c[books/Makefile] still ~c[time] their runs by default,
  and the partiular ~c[time] program is now controlled by a ~c[Makefile]
  variable.

  Failures during ~c[make certify-books] or ~c[make regression] now show up
  in the log as ``~c[**CERTIFICATION FAILED**]'', regardless of the operating
  system (as long as it supports ~c[make]).  Formerly, one searched for
  ``~c[**]'' but this did not appear in openMCL runs.

  We have eliminated ``Undefined function'' warnings that could occur in
  OpenMCL.

  ~st[BOOK CHANGES.]

  Reconciled the definitions of ~c[firstn] in ~c[book/misc/csort.lisp],
  ~c[books/bdd/bdd-primitives.lisp],
  ~c[books/ordinals/ordinal-definitions.lisp], and
  ~c[books/data-structures/list-defuns.lisp].  Thanks to Ray Richards for
  bringing this issue to our attention.

  Distributed book ~c[books/misc/defpun] now can handle ~il[stobj]s where it
  did not previously.  Thanks to John Matthews for bringing this issue to our
  attention.

  The \"make\" variable ~c[COMPILE_FLG] in file ~c[books/Makefile-generic]
  formerly only had an effect if there was a ~c[cert.acl2] file present.  That
  oversight has been remedied.

  File ~c[\"books/arithmetic/certify.lsp\"] was missing a ~ilc[certify-book]
  command for ~c[\"natp-posp\"].  Thanks to John Cowles for noticing this
  deficiency and supplying a fix.  (This file is of use to those who want to
  certify the ~c[\"books/arithmetic/\"] books without using ~c[\"make\"].)

  A few small changes have been made to ~c[\"books/rtl/rel4\"].

  Small changes were made to books ~c[misc/symbol-btree] and
  ~c[misc/rtl-untranslate].  In particular, the definition of ~c[symbol-btreep]
  was strengthened.

  We made a minor fix to ~c[books/ordinals/e0-ordinal.lisp], adding
  ~c[(verify-guards ob+)] and hence ~c[(verify-guards ocmp)] as well.  This was
  necessitated by the fix prohibiting functions with guards verified from
  calling functions with guards not verified (see also the related discussion
  under ``BUG FIXES'' above).

  ~st[MISCELLANEOUS CHANGES.]

  Further sped up processing of large ~ilc[mutual-recursion] nests (extending
  what was done for Version_2.7), perhaps by a factor of two in some cases.

  As promised in Version_2.5 (~pl[note-2-5]), structured pathnames are no
  longer supported.  So for example, the argument to ~ilc[include-book] must
  now be a string constant.

  Some documentation has been improved, for ~il[stobj]s thanks to suggestions
  by John Matthews and much of the rest thanks to feedback from Eric Smith.

  The function ~ilc[current-package] is now available to users (it has been
  taken off the list of so-called ``untouchables'').  Thanks to Jared Davis for
  bringing this issue to our attention.

  The documentation for topic ~il[using-computed-hints-7] has been improved.
  Thanks to Doug Harper and Eric Smith for inspiring this improvement.

  We added several symbols to ~c[*acl2-exports*]: ~ilc[cw], ~ilc[er],
  ~ilc[intern$], ~ilc[set-case-split-limitations], and ~ilc[set-difference-eq].
  Thanks to Jared Davis for suggesting most of these.

  Now, a ~ilc[table] event that sets the value for a key,
  ~c[(table tbl key val :put)], is redundant (~pl[redundant-events]) when it
  does not change the value associated with an existing key of the table.  In
  particular, ~ilc[define-pc-macro] is now fully redundant when it does not
  change an existing ~il[proof-checker] macro-command definition.  Thanks to
  Bill Young for bringing the latter issue to our attention.

  The definitions of unused system functions ~c[ev-w] and ~c[ev-w-lst] have
  been deleted.

  ACL2 now prints a warning if a ~ilc[defpkg] event introduces a package name
  with lower-case letters, since there is opportunity for later confusion in
  that case.  Thanks to Frederic Peschanski for bringing this problem to our
  attention and Sandip Ray for encouragement.

  ACL2 now works in Version 19 of CMU Common Lisp.

  The function ~ilc[sys-call] has been modified so that for ACL2 built on
  Allegro Common Lisp in Unix or Linux, the existing environment is used.
  Thanks to Erik Reeber for bringing this issue to our attention.

  The function ~ilc[disabledp] can now be given a macro name that has a
  corresponding function; ~pl[macro-aliases-table].  Also, ~ilc[disabledp] now
  has a ~il[guard] of ~c[t] but causes a hard error on an inappropriate
  argument.

  ~/~/")

(deflabel |NOTE-2-9(R)|
  :doc
  ":Doc-Section release-notes

  ACL2 Version  2.9(r) (October, 2004) Notes~/

  No changes have been made for support of non-standard analysis, other than
  a minor modification or two in ~c[books/nonstd/] books.

  ~/

  Please also ~pl[note-2-9] for changes to Version_2.9 of ACL2.
  ~/
  ")

(deflabel note-2-9-1

; Changes that are too low-level for the user documentation:

; The name *soundness-related-warning-summaries* has been changed to
; *uninhibited-warning-summaries*, and "Compiled file" has been added to this
; list, along with "Tainted".  When we added "Tainted", we found that a
; "Compiled file" warning could be made without the "Compiled file" showing up
; in the event summary.  That appears to be because we circumvented the basic
; design of warnings in function unfound-compiled-file by locally binding state
; global 'ld-skip-proofsp to nil.

; Here is an example of how to exploit the soundness bug, mentioned below, in
; symbol-package-name.
;
; ++++++++++++++++++++++++++++++ bad-1.lisp ++++++++++++++++++++++++++++++
; 
; (in-package "ACL2")
; 
; (defthm lemma-1
;   (equal (symbol-package-name (intern (car (cons "X-EQUIV" xxx)) "COMMON-LISP"))
;          "LISP")
;   :rule-classes nil)
; 
; ++++++++++++++++++++++++++++++ bad-2.lisp ++++++++++++++++++++++++++++++
; 
; (in-package "ACL2")
; 
; (defthm lemma-2
;   (equal (symbol-package-name 'COMMON-LISP::X-EQUIV)
;          "COMMON-LISP")
;   :rule-classes nil)
; 
; ++++++++++++++++++++++++++++++ bad.lisp ++++++++++++++++++++++++++++++
; 
; (in-package "ACL2")
; 
; (include-book "bad-1")
; 
; (include-book "bad-2")
; 
; (defthm bad
;   nil
;   :hints (("Goal" :use lemma-1))
;   :rule-classes nil)
; 
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  :doc
  ":Doc-Section release-notes


  ACL2 Version  2.9.1 (December, 2004) Notes~/

  (GCL only) A bug in ~ilc[symbol-package-name] has been fixed that could be
  exploited to prove ~c[nil], and hence is a soundness bug.  Thanks to Dave
  Greve for sending us an example of a problem with ~ilc[defcong] (see below)
  that led us to this discovery.

  ACL2 now warns when ~ilc[defcong] specifies ~ilc[equal] as the first
  equivalence relation, e.g., ~c[(defcong equal iff (member x y) 2)].  The
  warning says that the rule has no effect because ~ilc[equal] already refines
  all other equivalence relations.  Formerly, this caused an error unless
  ~c[:event-name] was supplied (~pl[defcong]), and in fact the error was a
  nasty raw Lisp error on GCL platforms due to some mishandling of packages by
  ACL2 that has been fixed (see the paragraph about ~ilc[symbol-package-name]
  above).  Thanks to Dave Greve for sending a helpful example in his report of
  this problem.

  (GCL only) The build process was broken for GCL 2.6.0 (and perhaps some
  earlier versions), and has been fixed.  Thanks to Jose Luis Ruiz-Reyna for
  bringing this problem to our attention.

  (GCL only) We have increased the hole size to at least 20% of max-pages,
  which may eliminate some garbage collection at the expense of larger virtual
  memory (not larger resident memory or larger image).  Thanks to Camm Maguire
  for helpful explanations on this topic.

  We have clarified the ~il[guard] warning message that is printed during
  evaluation of recursively-defined functions whose ~il[guard]s have not been
  verified, for example:
  ~bv[]
    ACL2 Warning [Guards] in TOP-LEVEL:  Guard-checking may be inhibited
    on some recursive calls of executable counterparts (i.e., in the ACL2
    logic), including perhaps EVENLP.  To check guards on all recursive
    calls:
      (set-guard-checking :all)
    To leave behavior unchanged except for inhibiting this message:
      (set-guard-checking :nowarn)
  ~ev[]
  And, ACL2 no longer prints that message when the ~il[guard] was
  unspecified for the function or was specified as ~c[T].  Thanks to Serita
  Nelesen for bringing the latter issue to our attention.  Finally, ACL2 now
  prints such a warning at most once during the evaluation of any top-level
  form; thanks to Bill Young for pointing out this issue.

  The function ~ilc[verbose-pstack] has been enhanced to allow specified prover
  functions ~st[not] to be traced.  ~l[verbose-pstack].

  Added ~ilc[lp], ~ilc[wet], and ~ilc[set-non-linearp] to ~c[*acl2-exports*],
  and hence to the ~c[\"]~ilc[ACL2-USER]~c[\"] package.

  The distributed book
  ~c[books/arithmetic-3/bind-free/integerp.lisp] has been modified in order to prevent
  potential looping; specifically, the definition of function
  ~c[reduce-integerp-+-fn-1].  Thanks to Robert Krug for providing this change.

  A small improvement was made in the ~ilc[wet] failure message when the error
  occurs during translation to internal form.  Thanks to Jared Davis for
  pointing out the obscurity of some ~ilc[wet] error messages.

  We have improved ACL2's evaluation mechanism for the function ~c[bad-atom<=],
  which now is specified to return ~c[nil] if neither argument is a so-called
  ``bad atom'' (as recognized by function ~c[bad-atom]).  The following events
  had caused a hard error, for example.  (We're sorry that ~c[bad-atom] and
  ~c[bad-atom<=] are not documented, but we also consider it unlikely that
  anyone needs such documentation; otherwise, please contact the implementors.)
  ~bv[]
  (defun foo (x y) (declare (xargs :guard t)) (bad-atom<= x y))
  (defun bar (x y) (declare (xargs :guard t)) (foo x y))
  (thm (equal (bar 3 4) 7))
  ~ev[]
  We have also changed the guard on ~ilc[alphorder] to require both arguments
  to be atoms.

  For forms ~c[(local x)] that are skipped during ~ilc[include-book], or during
  the second pass of ~ilc[certify-book] or ~ilc[encapsulate], ACL2 had
  nevertheless checked that ~c[x] is a legal event form.  This is no longer the
  case.

  The ~il[proof-checker] now does non-linear arithmetic when appropriate.  It
  had formerly ignored ~ilc[set-non-linearp] executed in the ACL2 command loop.

  Incremental releases are now supported.  ~l[version] and
  ~pl[set-tainted-okp].  Thanks to Hanbing Liu for discovering a flaw in our
  original design.

  The pattern-matching algorithm for ~c[:]~ilc[rewrite] rules has been made
  slightly more restrictive, thanks to a suggestion and examples from Robert
  Krug.  For example, previously one could get an infinite loop as follows.
  ~bv[]
  (defstub foo (x) t)
  (defaxiom foo-axiom
    (equal (foo (+ 1 x))
           (foo x)))
  (thm (foo 0)) ; or replace 0 by any integer!
  ~ev[]
  That is because the term ~c[(foo 0)] was considered to match against the
  pattern ~c[(foo (+ 1 x))], with ~c[x] bound to ~c[-1].  While such matching
  is sound, it leads to an infinite loop since it allows ~c[foo-axiom] to
  rewrite ~c[(foo 0)] to ~c[(foo -1)], and then ~c[(foo -1)] to ~c[(foo -2)],
  and so on.  The fix is to insist that the new value, in this case ~c[-1], is
  no larger in size according to ~ilc[acl2-count] than the old value, in this
  case ~c[0].  Since that test fails, the match is considered to fail and the
  loop no longer occurs.  An analogous fix has been made for multiplication,
  where now we only match when the new term is still a non-zero integer.  That
  change avoids a loop here.
  ~bv[]
  (defstub foo (x) t)
  (defaxiom foo-axiom
    (equal (foo (* 2 x))
           (foo x)))
  (thm (foo 0)) ; or try (thm (foo 4))
  ~ev[]

  Added macro ~c[find-lemmas] in ~c[books/misc/find-lemmas.lisp] (see
  brief documentation there) for finding all lemmas that mention all function
  symbols in a given list.

  ~c[:Restrict] ~il[hints] now work for ~c[:]~ilc[definition] rules, though
  they continue to be ignored by the preprocessor and hence you may want to use
  ~c[:do-not '(preprocess)] with any restrict hints.  Thanks to John Matthews
  for pointing out the lack of support for ~c[:definition] rules in
  ~c[:restrict] hints.

  Some books have been updated.  In particular, there is a new directory
  ~c[books/workshops/2004/] in workshops distribution, for the 2004 ACL2
  workshop.  There is also a new version of Jared Davis's ordered sets library,
  formerly in ~c[books/finite-set-theory/osets-0.81/] but now in
  ~c[books/finite-set-theory/osets/].

  Fixed a bug in the (under-the-hood) raw Lisp definition of ~ilc[defchoose],
  which had been causing a warning in CMU Common Lisp.

  [Technical improvements related to the use of ``~c[make dependencies]'' for
  certifying distributed books:]~nl[]File ~c[books/Makefile-generic] now does a
  better job with ``~c[make dependencies],'' specifically with respect to
  handling ~c[*.acl2] files and handling ~ilc[include-book] commands with
  ~c[:dir :system].  Regarding the latter, suppose for example that book
  ~c[basic.lisp] contains the line:
  ~bv[]
  (include-book \"arithmetic/top-with-meta\" :dir :system)
  ~ev[]
  Then ~c[make dependencies] would generate the following line:
  ~bv[]
  basic.cert: $(ACL2_SRC_BOOKS)/arithmetic/top-with-meta.cert
  ~ev[]
  Thus, if ~c[:dir :system] is used with ~ilc[include-book], the corresponding
  ~c[Makefile] should define the variable ~c[ACL2_SRC_BOOKS].  A standard
  ~c[Makefile] header for a books directory could thus be as follows.
  ~bv[]
  # The following variable should represent the ACL2 source directory.  It is the
  # only variable in this Makefile that may need to be edited.
  ACL2_SRC = ../../../../../..

  ACL2_SRC_BOOKS = $(ACL2_SRC)/books
  include $(ACL2_SRC_BOOKS)/Makefile-generic
  ACL2 = $(ACL2_SRC)/saved_acl2
  ~ev[]
  Finally, the ``~c[-s]'' flag may now be omitted when running
  ``~c[make dependencies].''

  ~/~/")

(deflabel note-2-9-2

; Example for item below,
; "Guard-related warnings could be printed during proofs.  These warnings have
;  been eliminated."
;
;(defun my-test1 (expr)
;  (declare (xargs :guard (true-listp expr)
;                  :verify-guards nil))
;  (if (atom expr)
;      expr
;    (cons (my-test1 (car expr))
;          (my-test1 (cdr expr)))))
;
;(defun foo (n)
;  (declare (xargs :measure (acl2-count n)))
;  (if (zp n) n (foo (equal (list (my-test1 '(a b c)) (my-test1 '(a b c))) 17)) ))
;
;(thm (equal (list (my-test1 '(a b c)) (my-test1 '(a b c))) 17))

  :doc
  ":Doc-Section release-notes


  ACL2 Version  2.9.2 (April, 2005) Notes~/

  Also ~pl[note-2-9-1] for other changes since the last non-incremental release
  (Version_2.9).

  There was a bug in non-linear arithmetic (~pl[non-linear-arithmetic]) that
  caused the following error:
  ~bv[]
  ACL2 !>(include-book \"rtl/rel4/lib/top\" :dir :system)
  ....
  ACL2 !>(set-non-linearp t)
   T
  ACL2 !>(thm
   (implies (and (bvecp a 77)
                 (bvecp b 50))
            (bvecp (fl (/ (* a b) (expt 2 23)))
                   104))
   :hints ((\"Goal\" :in-theory (enable bvecp))))

  [Note:  A hint was supplied for our processing of the goal above. 
  Thanks!]

  By the simple :definition BVECP, the :executable-counterparts of EXPT
  and UNARY-/ and the simple :rewrite rule ASSOCIATIVITY-OF-* we reduce
  the conjecture to

  Goal'
  (IMPLIES (AND (INTEGERP A)
                (<= 0 A)
                (< A 151115727451828646838272)
                (INTEGERP B)
                (<= 0 B)
                (< B 1125899906842624))
           (BVECP (FL (* A B 1/8388608)) 104)).


  HARD ACL2 ERROR in VARIFY:  This should not have happened.  The supposed
  variable, '1/8388608, is instead a constant.



  ACL2 Error in TOP-LEVEL:  Evaluation aborted.  See :DOC wet for how
  you might be able to get an error backtrace.

  ACL2 !>
  ~ev[]
  Thanks to Robert Krug for providing a fix for the above error.

  Guard-checking was being inhibited (since v2-9) for calls of built-in
  primitives on explicit values, e.g., ~c[(car 3)].  This has been fixed.

  Guard-related warnings could be printed during proofs (this bug was
  introduced in Version_2.9.1).  These warnings have been eliminated.

  Compound-recognizer rules ~c[natp-compound-recognizer] and
  ~c[posp-compound-recognizer] are now built into ACL2 for predicates
  ~ilc[natp] and ~ilc[posp], and hence have been deleted from book
  ~c[natp-posp.lisp] (where they were called ~c[natp-cr] and ~c[posp-cr],
  respectively).

  The function ~c[file-clock-p], which recognizes a component of the ACL2
  ~ilc[state], is now defined using ~ilc[natp] instead of ~ilc[integerp].
  Thanks to Jared Davis for suggesting this change.  (Technical explanation
  about functions in ACL2 source file ~c[axioms.lisp]: With a ~c[file-clock] of
  -1, the call of ~c[make-input-channel] in ~c[open-input-channel] will create
  a channel that can't be closed; see the guard of ~c[close-input-channel].)

  (Allegro CL users only) Support is now provided for building an Allegro CL
  application, provided you have an Allegro CL dynamic runtime license.  (Our
  belief is that with such a license, many users can use the same application,
  rather than each user needing a separate license.)  See new GNUmakefile
  target ~c[allegro-app] and file ~c[build-allegro-exe.cl] for more
  information.

  The new home page now contains a link to a new page ~c[other-releases.html],
  which contains information about other ACL2 releases.  (This is in one's
  local home page, but may not show up on the central ACL2 home page until the
  next non-incremental release.)  Thanks to Warren Hunt for suggesting this
  addition.

  We thank Erik Reeber for suggesting a solution to output redirection using
  ~ilc[sys-call], which we have described at the end of its documentation.

  A new documentation topic fixes the flawed argument for conservativity of the
  ~ilc[defchoose] event that appears in Appendix B of Kaufmann and Moore's
  paper, ``Structured Theory Development for a Mechanized Logic'' (Journal of
  Automated Reasoning 26, no. 2 (2001), pp. 161-203).
  ~l[conservativity-of-defchoose].  Thanks to John Cowles and Ruben Gamboa for
  helpful feedback on drafts of this note.

  The solution to exercise 6.15 in ~c[books/textbook/chap6/solutions.txt] has
  been fixed.  Thanks to Aaron Smith for pointing out the problem.

  A new documentation topic ~il[defun-sk-example] gives a little more help in
  using ~ilc[defun-sk] effectively.  Thanks to Julien Schmaltz for presenting
  this example as a challenge.

  (GCL only) There is now a way to speed up GCL builds of ACL2, at the cost of
  perhaps a percent or so in performance of the resulting image.  Using
  ~c[make] one supplies the following.
  ~bv[]
  LISP='gcl -eval \"(defparameter user::*fast-acl2-gcl-build* t)\"
  ~ev[]

  Various makefiles have been improved in several ways.
  ~bq[]
  (1) Parallel book certification, using GNU make's ~c[-j] option, can be used.

  (2) Book certifications now stops at the first failure if ~c[books/Makefile]
  or ~c[books/Makefile-generic] is used, and returns non-zero exit status.
  However, the various make targets in the ACL2 source directory
  (~c[regression], ~c[certify-books], etc.) still continue past failures unless
  you provide ~c[ACL2_IGNORE=' '] on the ~c[make] command line.

  (3) The build process has been modified (file ~c[GNUmakefile]) so that it
  stops upon a failed compile or a failed initialization.

  (4) The automatic dependency generation (from ``~c[make dependencies]'' has
  been improved so that commands of the form ~c[(ld \"my-book.lisp\")] in
  ~c[.acl2] files cause the appropriate depedencies to be generated.~eq[]
  Thanks to comments from several users that led to the above Makefile
  improvements: Ray Richards, Doug Harper, and the Rockwell ACL2 users for (1)
  and (2) (and inspiring (4)), and David Rager for (2) and (3).  In particular,
  Doug Harper sent a replacement for the ~c[.date] mechanism, which was
  interfering with ~c[make -n]; so, these files are no longer written.

  A mechanism has been added for saving output.  In particular, you can now
  call ~ilc[ld] on a file with output turned off, for efficiency, and yet when
  a proof fails you can then display the proof attempt for the failed (last)
  event.  ~l[set-saved-output].  Another new command ~-[]
  ~pl[set-print-clause-ids] ~-[] causes subgoal numbers to be printed during
  proof attempts when output is inhibited.

  Documentation has been added for using ACL2's makefile support to automate
  the certification of collections of books.  ~l[book-makefiles].

  Fixed a bug in ~ilc[sys-call-status] that was causing hard Lisp errors.

  Improved ~ilc[cw-gstack] to allow a ~c[:frames] argument to specify a range
  of one or more frames to be printed.  ~pl[cw-gstack].

  Fixed a bug in ~il[proof-checker] command ~c[forwardchain].  Thanks to
  Ming-Hsiu Wang for bringing this bug to our attention.

  We have provided a mechanism for saving an executable image.
  ~l[saving-and-restoring] and ~pl[save-exec].  We have eliminated obsolete
  functions ~c[note-lib] and ~c[make-lib].

  Modified the ~ilc[ground-zero] ~il[theory] so that it contains all of the
  built-in rules (in ACL2 source file ~c[axioms.lisp]).  It had formerly failed
  to include rules from some definitions and theorems near the end of
  ~c[axioms.lisp].

  A new event, ~ilc[set-enforce-redundancy], allows the enforcement of
  ~ilc[defthm], ~ilc[defun], and most other events during book development.
  ~l[set-enforce-redundancy].

  A bug has been fixed that had allowed ~ilc[deftheory] ~il[events] to cause a
  hard Lisp error when calling ~ilc[union-theories] on ill-formed theories
  after, for example:
  ~bv[]
  :set-guard-checking nil
  (in-theory (union-theories '((:rewrite no-such-rule))
                             (current-theory 'ground-zero)))
  ~ev[]
  The handling of ~il[guard] checking has been modified somewhat in a way that
  should only very rarely affect users.  (An ``Essay on Guard Checking'' in the
  ACL2 source code explains this point to anyone interested in implementation
  details.)

  (GCL ONLY) Removed the -dir setting in the ACL2 wrapper script for GCL.  This
  should generally have no effect for most users, but it eliminates a potential
  source of error down the road.

  Several interesting new definitions and lemmas have been added to the rtl
  library developed at AMD, and incorporated into ~c[books/rtl/rel4/lib/].
  Other book changes include a change to lemma ~c[truncate-rem-elim] in
  ~c[books/ihs/quotient-remainder-lemmas.lisp], as suggested by Jared Davis.

  The macro ~ilc[real/rationalp] may now be referred to in ~ilc[in-theory]
  ~il[events] and ~il[hints], thanks to a new ~ilc[add-macro-alias] event.
  Thanks to Jared Davis for this suggestion.

  ACL2 terms of the form ~c[(if p 'nil 't)] are now printed as ~c[(not p)],
  where in some setting they had been printed as ~c[(and (not p) t)].  Thanks
  to Robert Krug for this improvement.

  (GCL ONLY) Added profiling support, based heavily on code supplied by Camm
  Maguire.  See file ~c[save-gprof.lsp] for instructions.  Thanks to Camm, and
  also to David Hardin for inspiring this addition.

  Added support for preprocessing before printing (untranslating) a term.
  ~l[user-defined-functions-table], in particular the discussion of
  ~c[untranslate-preprocess].  Thanks to Jared Davis for inspiring this
  addition, and for providing a book that takes advantage of it
  (~c[books/misc/untranslate-patterns.lisp]).

  The documentation has been improved for explaining how ~il[rune]s are
  assigned; ~pl[rune].  Thanks to Robert Krug for pointing out inaccuracies in
  the existing documentation.

  ~/~/")

(deflabel note-2-9-3
  :doc

; Things that seem too minor to mention:

; (Intern (coerce (list #\Page) 'string) "ACL2") was printing as control-L
; without surrounding |..|, which cannot be read back in (at least in GCL).  A
; similar problem occurs with (Intern (coerce (list #\A #\Page #\B) 'string)
; "ACL2") So, added #\Page to *slashable-chars*.

; Eliminated a warning in CMUCL 19b due to missing arguments in error cases for
; source functions find-alternative-start1 and find-alternative-stop.

  ":Doc-Section release-notes

  ACL2 Version  2.9.3 (August, 2005) Notes~/

  Also ~pl[note-2-9-1] and ~pl[note-2-9-2] for other changes since the last
  non-incremental release (Version_2.9).

  We fixed a soundness bug that exploited the ability to define
  ~c[:]~ilc[program] mode functions that are improperly guarded, and then to
  use those functions in ~ilc[defconst] forms.  The fix is to evaluate
  ~ilc[defconst] forms using the same ``safe-mode'' that is used in
  macroexpansion (~pl[guards-and-evaluation]).  Here is a proof of ~c[nil] that
  succeeded in Allegro Common Lisp (but not, for example, GCL).  See also a
  long comment in source function ~c[defconst-fn] for an example that does not
  require the use of ~c[:set-guard-checking].
  ~bv[]
  :set-guard-checking nil ; execute before certifying the book below

  (in-package \"ACL2\")

  (encapsulate
   ()
   (local (defun f1 ()
            (declare (xargs :mode :program))
            (char-upcase (code-char 224))))
   (local (defconst *b* (f1)))
   (defun f1 ()
     (char-upcase (code-char 224)))
   (defconst *b* (f1))
   (defthm bad
     (not (equal *b* (code-char 224)))
     :rule-classes nil))

  (defthm ouch
    nil
    :hints ((\"Goal\" :use bad))
    :rule-classes nil)
  ~ev[]

  We fixed a soundness hole due to the fact that the \"LISP\" package does not
  exist in OpenMCL.  We now explicitly disallow this package name as an
  argument to ~ilc[defpkg].  Thanks to Bob Boyer and Warren Hunt for bringing
  an issue to our attention that led to this fix.

  ACL2 now requires all package names to consist of standard characters
  (~pl[standard-char-p], none of which is lower case.  The reason is that we
  have seen at least one lisp implementation that does not handle lower case
  package names correctly.  Consider for example the following raw lisp log
  (some newlines omitted).
  ~bv[]
  >(make-package \"foo\")
  #<\"foo\" package>
  >(package-name (symbol-package 'FOO::A))
  \"foo\"
  >(package-name (symbol-package '|FOO|::A))
  \"foo\"
  >
  ~ev[]
  Distributed book ~c[books/textbook/chap10/compiler], as well as workshop
  books in directory ~c[books/workshops/2004/cowles-gamboa/support/], were
  modified to accommodate the above change.

  Added ~c[newline], ~ilc[add-to-set-eql], ~c[the-fixnum], and ~c[the-fixnum!]
  to ~c[*acl2-exports*].  Thanks to Jared Davis for bringing these to our
  attention.

  Added a line to ~c[acl2.lisp] to support CMUCL running on Mac OSX, thanks to
  a suggestion from Fabricio Chalub Barbosa do Rosario.

  The executable scripts for saved ACL2 images now include ~c[$*], so that
  command-line arguments will be passed along.

  (For GCL profiling only) Fixed a colon (~c[:]) that should have been a
  semicolon (~c[;]) in file ~c[save-gprof.lsp].  Thanks to David Hardin for
  pointing out this bug.

  The documentation for ~c[:]~ilc[elim] rules has been expanded and improved,
  thanks to useful feedback from Hanbing Liu.

  Fixed a bug in the guard for function ~c[include-book-dir].

  For those who want to experiment with an alternate implementation of ~ilc[mv]
  and ~ilc[mv-let], there is now support for under-the-hood implementation of
  these in terms of raw Lisp functions ~c[values] and ~c[multiple-value-bind],
  respectively.  The regression suite has seen about a 10% speed-up in Allegro
  CL and about an 8% slowdown in GCL for builds with this change.  See the
  makefile (~c[GNUmakefile]) for examples of how to build ACL2 by including the
  feature, ~c[:acl2-mv-as-values].  Source file ~c[init.lsp] has been renamed
  to ~c[init.lisp] in support of this change (technical detail: otherwise GCL
  loads the init file too soon, before its ~c[-eval] argument is evaluated).
  Thanks to David Rager for inspiring this change, by pointing out the
  problematic use of globals by the existing ~ilc[mv] implementation from the
  standpoint of supporting parallel evaluation.  This capability is
  experimental: there is likely to be some remaining work to be done on it.

  A change related to the one just above is that we now limit the maximum
  number of arguments to any call of ~ilc[mv] to 32.  Thanks to Bob Boyer for
  raising a question that lead to this change.

  Eliminated some compiler warnings in OpenMCL.

  In the rtl library (~c[books/rtl/rel4/]), functions ~c[bits] and ~c[setbits]
  have had their ~il[guard]s improved (as they had been too restrictive,
  especially for ~c[setbits]).

  A new function ~ilc[time$] permits timing of forms, by using (under the hood)
  the host Common Lisp's ~c[time] utility.

  We fixed an infinite loop that could occur during destructor elimination
  (~pl[elim]).  Thanks to Sol Swords to bringing this to our attention and
  sending a nice example, and to Doug Harper for sending a second example that
  we also found useful.

  The method of speeding up GCL-based builds (~pl[note-2-9-2]) has changed
  slightly from Version_2.9.2.  Now, in the ~c[make] command:
  ~bv[]
    LISP='gcl -eval \"(defparameter user::*fast-acl2-gcl-build* t)\"
  ~ev[]

  We improved the pretty-printer's handling of keywords.  For example, before
  this change one might see the following printed by ACL2.
  ~bv[]
  (MODIFY TH S :KEY1 VAL1 :KEY2
          (IF (IF X Y Z) AAAAAAAAAA BBBBBBB))
  ~ev[]
  Now, the above might print as follows.  Notice that we have avoided breaking
  after a keyword (~pl[keywordp]) that is preceded by other forms on the same
  line.
  ~bv[]
  (MODIFY TH S
          :KEY1 VAL1
          :KEY2 (IF (IF X Y Z) AAAAAAAAAA BBBBBBB))
  ~ev[]
  ~l[note-2-9-3-ppr-change] for a detailed discussion of this change.

  (GCL ONLY) Evaluation in a break is no longer inhibited by ACL2 when built on
  top of GCL, so GCL now matches other Common Lisps in this respect.

  For ACL2 built on most host Common Lisps, you will see the string
  ~c[[RAW LISP~]] in the prompt, at least at a break, to emphasize
  that one is inside a break and hence should probably quit from the
  break.  ~l[breaks].

  Jared Davis suggested improvements to lemmas ~c[len-update-nth] (in source
  file ~c[axioms.lisp]) and ~c[append-true-listp-type-prescription] (in
  ~c[books/meta/term-defuns.lisp]), which have been incorporated.  The former
  required a change in ~c[books/workshops] book
  ~c[2004/ruiz-et-al/support/q-dag-unification.cert], which has been made.

  The ~il[proof-checker] command ~c[rewrite] allows further binding of free
  variables in hypotheses, with new optional argument ~c[instantiate-free].
  Proof-checker command ~c[show-rewrites] (~c[sr]) gives corresponding
  additional information.  Documentation for these commands has been improved;
  ~pl[proof-checker-commands].  Thanks to John Matthews and Bill Young for
  suggestions and feedback leading to these improvements.

  Fixed downcase printing so that the package name of a symbol is also
  downcased.  For example, after execution of ~c[(defpkg \"FOO\" nil)] and
  ~c[(set-acl2-print-case :downcase)], ~c['foo::ab] will print back as the
  same, rather than as ~c['FOO::ab].

  It is now possible to control the output so that numbers are printed in
  binary, octal, or hex, though the default is still radix 10.
  ~l[set-acl2-print-base].  Note that in support of this change, built-in
  functions ~ilc[explode-nonnegative-integer] and ~c[explode-atom] now take an
  extra ~c[print-base] argument.  Different support for radix conversion may be
  found in a book newly contributed by Jun Sawada, ~c[books/misc/radix.lisp].

  Built-in axiom ~c[car-cdr-elim] is now only an ~c[:]~ilc[elim] rule.  It was
  formerly both an ~c[:elim] rule and a ~c[:]~ilc[rewrite] rule.  A new rule,
  ~c[cons-car-cdr], takes the place of the old ~c[:rewrite] rule, but is
  instead a hypothesis-free rule that can cause a case split (see source file
  ~c[axioms.lisp]).  Thanks to Jared Davis for suggesting this change.

  Lemmas about ~ilc[alphorder] (~c[alphorder-reflexive], ~c[alphorder-transitive],
  ~c[alphorder-anti-symmetric], and ~c[alphorder-total]) are now available.
  (They had been ~ilc[local] in source file ~c[axioms.lisp].)  Thanks to Serita
  Nelesen for bringing this issue to our attention.

  ACL2 has, for some time, printed a space in the event summary after the open
  parenthesis for a ~ilc[defthm] event, in order to ease backward searching for
  the original form, for example ~c[(defthm bar ...)]:
  ~bv[]
  Form:  ( DEFTHM BAR ...)
  ~ev[]
  The intention was that this extra space should be printed for every event
  form; but it was missing in some cases, for example, for ~ilc[verify-guards].
  This has been fixed.

  In analogy to ~ilc[include-book], now ~ilc[ld] takes the (optional) keyword
  argument ~c[:dir].  Thanks to Jared Davis for providing an implementation of
  this feature and to Eric Smith and Jeff Marshall for requesting this feature.

  We fixed a bug in ~ilc[include-book] that could cause an error when
  redefinition is on, for example:
  ~bv[]
  (set-ld-redefinition-action '(:warn! . :overwrite) state)
  (include-book \"/u/acl2/books/arithmetic/top\")
  ~ev[]

  The behavior of ~ilc[include-book] now matches the documentation: handling of
  compiled files for uncertified ~il[books] will follow the same rules as for
  certified books.  In particular, if you create an object file in raw Lisp for
  some book, then including that book will load that object file.  Thanks to
  Jared Davis for bringing this issue to our attention.

  New documentation explains the interaction of redefinition and redundancy.
  ~l[redundant-events] ~-[] the ``Note About Unfortunate Redundancies'' is new.
  Thanks to Grant Passmore for providing examples that led us to write this
  additional documentation.

  Solutions to exercises in ``How To Prove Theorems Formally''
  (~c[http://www.cs.utexas.edu/users/moore/publications/how-to-prove-thms]) are
  now available in distributed book ~c[books/misc/how-to-prove-thms.lisp].
  Also in that directory may be found a new book ~c[hanoi.lisp] that contains a
  solution to the Towers of Hanoi problem.

  ~/~/")

(deflabel note-2-9-3-ppr-change
  :doc
  ":Doc-Section note-2-9-3

  change in pretty-printing for ACL2 Version_2.9.3~/

  We have improved pretty-printing in ACL2 Version_2.9.3 to handle keywords a
  little differently.  To see a discussion of the basics of this change,
  ~pl[note-2-9-3].  In this note we describe it in considerable detail.~/

  Those who wish to understand the ACL2 pretty-printer's implementation can now
  find considerably more comments on it in the source code.  In this note, we
  do not focus on the implementation.  Rather, we motivate the change and show
  how the improved prettyprinter performs.

  Why do we want better keyword handling?  Imagine a macro that builds a new
  state from an old state by changing the values in the affected fields,
  leaving everything else unchanged.  One could write
  ~bv[]
  (modify th s :key1 val1 :key2 val2 :key3 val3)
  ~ev[]
  where the three keys identify fields in the state.

  To make it easier to read new concrete states, we may have a function that
  prints them ``relative'' to a given base state, expressing the new state as a
  modification of the given base state.  So we may find ourselves
  prettyprinting modify forms like that above.

  The previous prettyprinter will sometimes print the form above as follows.
  ~bv[]
  (modify th s :key1
          val1
          :key2 val2 :key3 val3)
  ~ev[]
  This can be unpleasant to read, because of the way ~c[:key1] and ~c[val1] are
  separated.  Here is an example of the old prettyprinter and the new one, both
  printing an expression from the ACL2 source code in a width of 40:
  ~bv[]
  Old:
  (ADD-TO-TAG-TREE
   'ASSUMPTION
   (MAKE
    ASSUMPTION :TYPE-ALIST TYPE-ALIST
    :TERM TERM :REWRITTENP REWRITTENP
    :IMMEDIATEP IMMEDIATEP :ASSUMNOTES
    (LIST
     (MAKE
          ASSUMNOTE :CL-ID
          NIL :RUNE RUNE :TARGET TARGET)))
   TTREE)

  New:
  (ADD-TO-TAG-TREE
       'ASSUMPTION
       (MAKE ASSUMPTION
             :TYPE-ALIST TYPE-ALIST
             :TERM TERM
             :REWRITTENP REWRITTENP
             :IMMEDIATEP IMMEDIATEP
             :ASSUMNOTES
             (LIST (MAKE ASSUMNOTE
                         :CL-ID NIL
                         :RUNE RUNE
                         :TARGET TARGET)))
       TTREE)
  ~ev[]
  Basically the change we made forces the prettyprinter to print each ~c[:key]
  on a new line unless they all fit on a single line.  So we would now get
  either
  ~bv[]
  (modify th s :key1 val1 :key2 :val2 :key3 val3)
  ~ev[]
  or
  ~bv[]
  (modify th s
          :key1 val1
          :key2 val2
          :key3 val3)
  ~ev[]
  Furthermore, we fixed it so that if ~c[val1] (say) is a big s-expression we
  may still print it on the same line as its key.  The old prettyprinter
  enforced the rule that if you wanted to print ~c[(foo a b)] and ~c[b] gets
  broken up into several lines, then it has to start on a new line.  Thus,
  we'd never print
  ~bv[]
  (foo a (bbb
          (mum x)))
  ~ev[]
  but would print instead
  ~bv[]
  (foo a
       (bbb
        (mum x)))
  ~ev[]
  Now, if a is a keyword, we can print the first way.

  So here are some nice examples of prettyprinted keyword forms.  All of these
  are printed for a page of width 40.
  ~bv[]
  <--            40 chars               ->
  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

  (MODIFY TH S :KEY1 V1 :KEY2 V2)

  (MODIFY TH S :KEY1 V1 :KEY2 V2 :KEY3 V3)

  (MODIFY TH S1                               ; Because of the extra char
          :KEY1 V1                            ; in S1 the flat size exceeds
          :KEY2 V2                            ; 40 and we break it.
          :KEY3 V3)
  ~ev[]
  The old ppr would have printed this as:
  ~bv[]
  (MODIFY
       TH S1 :KEY1 V1 :KEY2 V2 :KEY3 V3)
  ~ev[]
  Returning to new examples:
  ~bv[]
  <--            40 chars               ->
  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

  (MODIFY TH S
          :KEY1 (IF (IF X Y Z) AAAA BBBB)
          :KEY2 VAL2
          :KEY3 VAL3)
  ~ev[]
  Now we extend ~c[AAAA] and ~c[BBBB] by one char each, so it would overflow
  the right margin if printed as above, and we get:
  ~bv[]
  <--            40 chars               ->
  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

  (MODIFY
       TH S
       :KEY1 (IF (IF X Y Z) AAAAX BBBBX)
       :KEY2 VAL2
       :KEY3 VAL3)
  ~ev[]
  If we make these names even longer we force the value off the line containing
  ~c[:key1]:
  ~bv[]
  (MODIFY
       TH S
       :KEY1
       (IF (IF X Y Z) AAAAXXXXX BBBBXXXXX)
       :KEY2 VAL2
       :KEY3 VAL3)
  ~ev[]
  Here are some examples from the ACL2 source code, printed in 40 characters:
  ~bv[]
  (DEFTHM
   ALPHORDER-ANTI-SYMMETRIC
   (IMPLIES (AND (NOT (CONSP X))
                 (NOT (CONSP Y))
                 (ALPHORDER X Y)
                 (ALPHORDER Y X))
            (EQUAL X Y))
   :HINTS
   ((\"Goal\"
     :IN-THEORY
     (UNION-THEORIES
      '(STRING< SYMBOL-<)
      (DISABLE
         CODE-CHAR-CHAR-CODE-IS-IDENTITY))
     :USE
     ((:INSTANCE SYMBOL-EQUALITY (S1 X)
                 (S2 Y))
      (:INSTANCE BAD-ATOM<=-ANTISYMMETRIC)
      (:INSTANCE
           CODE-CHAR-CHAR-CODE-IS-IDENTITY
           (C Y))
      (:INSTANCE
           CODE-CHAR-CHAR-CODE-IS-IDENTITY
           (C X)))))
   :RULE-CLASSES
   ((:FORWARD-CHAINING
     :COROLLARY
     (IMPLIES
        (AND (ALPHORDER X Y)
             (NOT (CONSP X))
             (NOT (CONSP Y)))
        (IFF (ALPHORDER Y X) (EQUAL X Y)))
     :HINTS
     ((\"Goal\"
       :IN-THEORY (DISABLE ALPHORDER))))))
  ~ev[]
  Here is that same one, printed in a width of 60.
  ~bv[]
  (DEFTHM
   ALPHORDER-ANTI-SYMMETRIC
   (IMPLIES (AND (NOT (CONSP X))
                 (NOT (CONSP Y))
                 (ALPHORDER X Y)
                 (ALPHORDER Y X))
            (EQUAL X Y))
   :HINTS
   ((\"Goal\"
       :IN-THEORY
       (UNION-THEORIES
            '(STRING< SYMBOL-<)
            (DISABLE CODE-CHAR-CHAR-CODE-IS-IDENTITY))
       :USE ((:INSTANCE SYMBOL-EQUALITY (S1 X)
                        (S2 Y))
             (:INSTANCE BAD-ATOM<=-ANTISYMMETRIC)
             (:INSTANCE CODE-CHAR-CHAR-CODE-IS-IDENTITY (C Y))
             (:INSTANCE CODE-CHAR-CHAR-CODE-IS-IDENTITY
                        (C X)))))
   :RULE-CLASSES
   ((:FORWARD-CHAINING
        :COROLLARY (IMPLIES (AND (ALPHORDER X Y)
                                 (NOT (CONSP X))
                                 (NOT (CONSP Y)))
                            (IFF (ALPHORDER Y X) (EQUAL X Y)))
        :HINTS ((\"Goal\" :IN-THEORY (DISABLE ALPHORDER))))))
  ~ev[]
  Just for comparison, here is the above printed in 60 columns by the old
  prettyprinter.
  ~bv[]
  (DEFTHM
   ALPHORDER-ANTI-SYMMETRIC
   (IMPLIES (AND (NOT (CONSP X))
                 (NOT (CONSP Y))
                 (ALPHORDER X Y)
                 (ALPHORDER Y X))
            (EQUAL X Y))
   :HINTS
   ((\"Goal\" :IN-THEORY
            (UNION-THEORIES
                 '(STRING< SYMBOL-<)
                 (DISABLE CODE-CHAR-CHAR-CODE-IS-IDENTITY))
            :USE
            ((:INSTANCE SYMBOL-EQUALITY (S1 X)
                        (S2 Y))
             (:INSTANCE BAD-ATOM<=-ANTISYMMETRIC)
             (:INSTANCE CODE-CHAR-CHAR-CODE-IS-IDENTITY (C Y))
             (:INSTANCE CODE-CHAR-CHAR-CODE-IS-IDENTITY
                        (C X)))))
   :RULE-CLASSES
   ((:FORWARD-CHAINING
         :COROLLARY
         (IMPLIES (AND (ALPHORDER X Y)
                       (NOT (CONSP X))
                       (NOT (CONSP Y)))
                  (IFF (ALPHORDER Y X) (EQUAL X Y)))
         :HINTS
         ((\"Goal\" :IN-THEORY (DISABLE ALPHORDER))))))
  ~ev[]

  Of course, given that you cannot tell for sure whether the keywords you're
  seeing are part of a keyword/value parameter list or part of some constant
  containing random keywords, the prettyprinter can't solve the problem
  perfectly.  We just tried to make it work nicely on well-formed keyword/value
  parameter lists.

  For example, here is a form printed by the each prettyprinter.
  ~bv[]
  Old:
  (MEMBER
   X
   '(:MONDAY
        :MON :TUESDAY :TUES :WEDNESDAY
        :WED :THURSDAY :THURS :FRIDAY
        :FRI :SATURDAY :SAT :SUNDAY :SUN))

  New:
  (MEMBER X
          '(:MONDAY :MON
                    :TUESDAY :TUES
                    :WEDNESDAY :WED
                    :THURSDAY :THURS
                    :FRIDAY :FRI
                    :SATURDAY :SAT
                    :SUNDAY :SUN))
  ~ev[]

  The new way is not how one would print it by hand!  But then, neither is the
  old way.~/")

(deflabel note-2-9-4

; Things that seem too minor to mention:

; Added a force-output call to print-prompt by way of new macro
; with-output-forced.  This seemed necessary for SBCL.

; In load-acl2, replaced (eval '(setq state *the-live-state*)) with what we
; used to do only for cmulisp, namely (set 'state *the-live-state*).

; Made small fix to translate11's handling of synp, to avoid requiring
; quote-normal form but to insist that the QUOTE calls have one argument.

; The error message for guard violations was cleaned up: added a sentence for
; new users, added ev-fncall-guard-er-msg for code sharing, and added initial
; part for new users to :doc set-guard-checking.

; To see the change mentioned below for set-let*-abstractionp, try this:
; (defpkg "FOO" (union-eq *acl2-exports*
;                         *common-lisp-symbols-from-main-lisp-package*))
; (in-package "FOO")
; (defstub foo (x) t)
; (defstub bar (x) t)
; :set-let*-abstractionp t
; (thm (equal (foo (append x y)) (bar (append x y))))

; Added hard! severity value for er (not advertised).

; Replaced conjuncts-of by flatten-ands-in-lit.

; Modified books/workshops/2004/sumners-ray/support/Makefile to avoid having to
; remake success.txt when that is unnecessary.

; Regarding this item below:
;
; We fixed an inefficiency that could cause an ~ilc[ld] command to seem to hang
; at its conclusion.  Thanks to Sandip Ray for pointing out this problem.
;
; Here is a way to see the problem.  The change was to maybe-add-command-landmark.
; (include-book "arithmetic/top-with-meta" :dir :system)
; (ld '((u) (include-book "arithmetic/top-with-meta" :dir :system)))

; Added (type (signed-byte 29) col) declarations for parameter col in fmt0 and
; fmt1.

; Modified many warnings to say something like "A rule generated by FOO"
; instead of "The rule generated by FOO", in response to an email from John
; Cowles.  This may not really deal with the objection that it appears that
; more than one copy of the same warning can appear for a single event (for the
; case of more than one rule), but at least the new wording is more accurate,
; and much more of a change could require considerable effort.

  :doc

  ":Doc-Section release-notes

  ACL2 Version  2.9.4 (February, 2006) Notes~/

  Also ~pl[note-2-9-1], ~pl[note-2-9-2], and ~pl[note-2-9-3] for other changes
  since the last non-incremental release (Version_2.9).

  A soundness bug has been fixed that was due to inadequate checking of
  ~c[:]~ilc[meta] rules in the presence of ~ilc[local] ~il[events].
  Specifically, a ~c[local] ~ilc[defevaluator] event is insufficient for
  supporting a ~c[:meta] rule (an example is shown in source function
  ~c[chk-acceptable-rules]).  Thanks to Dave Greve and Jared Davis for bringing
  this bug to our attention, by sending a proof of ~c[nil] that exploited this
  bug.  The fix is to check legality of ~c[:meta] rules even when skipping
  proofs during an ~ilc[include-book] event or the second pass of an
  ~ilc[encapsulate] event.

  Fixed problem with parallel make for workshop books by adding a dependency
  line to ~c[books/workshops/2003/Makefile].

  Default hints (~pl[set-default-hints]) no longer prevent the use of
  ~c[:INSTRUCTIONS] (~pl[proof-checker]).  Thanks to Jared Davis for pointing
  out this problem.

  New functions ~ilc[remove-eq] and ~ilc[remove-equal] have been defined, in
  analogy to ~ilc[remove].  These two symbols have also been added to
  ~c[*acl2-exports*].  Thanks to David Rager for pointing out that
  ~c[remove-equal] was missing.  Moreover, the definitions of ~c[delete1-eq]
  and ~c[delete1-equal] have been eliminated.  Function ~c[remove1-eq], now in
  ~c[:]~ilc[logic] mode in source file ~c[axioms.lisp], serves in place of
  ~c[delete1-eq], with corresponding new function definitions for ~ilc[remove1]
  and ~ilc[remove1-equal].

  The symbol ~ilc[assert$] has been added to ~c[*acl2-exports*].  Thanks to
  Jared Davis for the suggestion.

  Added SBCL support.  Thanks to Juho Snellman for significant assistance with
  the port.  Thanks to Bob Boyer for suggesting the use of feature
  ~c[:acl2-mv-as-values] with SBCL, which can allow thread-level parallelism
  in the underlying lisp; we have done so when feature ~c[:sb-thread] is
  present.

  We have continued to incorporate suggestions for wording improvements in
  documentation and error messages.  Thanks to all who send these suggestions,
  especially to Eric Smith, who has suggested the vast majority of them.

  Made a small improvement to errors and warnings caused on behalf of
  ~il[set-enforce-redundancy], to indicate when an event of the same name
  already exists.

  Fixed a bug in ~c[books/misc/rtl-untranslate.lisp] that was causing a guard
  violation when adding a new entry for an existing key.

  Fixed a bug in translation to internal form that caused ~ilc[defun-sk] and
  ~ilc[defchoose] to have difficulties handling ignored variables in ~ilc[let]
  forms.  Thanks to Sandip Ray to bringing this issue to our attention with a
  helpful example.

  The form ~c[(push :acl2-mv-as-values *features*)] has been added in source
  file ~c[acl2-init.lisp] for SBCL and OpenMCL only, in order to support
  parallel execution (looking to the future...).

  Default-hints (~pl[set-default-hints]) were being ignored inside the
  ~il[proof-checker], but no longer.  Thanks to John Erickson for bringing this
  problem to our attention and providing a simple example of it.

  Modified the ~c[TAGS] ~c[\"make\"] target (specifically, function
  ~c[make-tags]) so that it is gracefully skipped if the ~c[etags] program is
  not found.  Thanks to David Rager for pointing out this issue.

  Sandip Ray has re-worked the supporting materials for his ACL2 Workshop 2003
  talk (originally with John Matthews and Mark Tuttle), to run in a few
  minutes.  The result is in ~c[workshops/2003/ray-matthews-tuttle/support/]
  and is included in the full ACL2 regression suite.  Thanks, Sandip.

  Debian releases of ACL2 had created superfluous ~c[.cert.final] files when
  certifying books.  This has been fixed; thanks to Jared Davis for noticing
  this problem.

  Jared Davis has pointed out that ``If you add a ~c[:backchain-limit-lst 0] to
  a rewrite rule whose hypotheses are all forced, then ACL2 `really assumes them'
  without trying to relieve them right there through rewriting.''  Relevant
  documentation has been added for ~c[:backchain-limit-lst]; ~pl[rule-classes].

  A new version of the rtl library has been included in ~c[books/rtl/rel5/].
  Thanks to David Russinoff for contributing hand proofs for the new lemmas,
  and to Matt Kaufmann for carrying out their mechanization.

  Fixed a bug in ~ilc[save-exec] that was failing to set the initial ~c[cbd]
  according to the current directory when starting up ACL2.  Thanks to Camm
  Maguire for bringing our attention to this problem.

  Variables introduced during ~c[let*] abstraction are now in the current
  package.  Thanks to Jared Davis for suggesting such a change.
  ~l[set-let*-abstractionp].

  It is now allowed for two definitions to be considered the same from the
  standpoint of redundancy (~pl[redundant-events]) when one specifies a
  ~c[:]~ilc[guard] of ~c[t] and the other has no explicit ~c[:guard] (hence,
  the guard is implicitly ~c[t]).  Thanks to Jared Davis for bringing this
  issue to our attention.

  (For users of ~c[emacs/emacs-acl2.el]) There have been a few enhancements to
  distributed file ~c[emacs/emacs-acl2. el] (skip this paragraph if you don't
  use that file):
  ~bq[]
  o ~c[Control-t q] continues to compare windows ignoring whitespace, but now,
     a prefix argument can be given to control case is also ignored (ignore case if
     positive, else use case). 

  o ~c[Control-t Control-l] has been defined to be similar to ~c[Control-t l],
     except that proofs are skipped and output is suppressed.

  o ~c[Control-t u] has been defined to print, to the shell buffer, a
    ~c[:]~ilc[ubt!] form for the command containing the cursor.

  o Control-t Control-f buries the current buffer.

  o ~c[Meta-x new-shell] now puts the new shell buffer in ~c[shell-mode]
    (thanks to David Rager for noticing this issue).~eq[]

  Linear arithmetic has been modified so that we do not generate the equality
  ~c[(equal term1 term2)] from the pair of inequalities ~c[(<= term1 term2)]
  and ~c[(<= term2 term1)] in the case that we would have to ~ilc[force] both
  ~c[term1] and ~c[term2] to be ~ilc[acl2-numberp]s.  Thanks to Dave Greve for
  providing a motivating example and to Robert Krug for providing a fix.

  The event ~ilc[delete-include-book-dir] had not been allowed inside
  ~il[books] and ~ilc[encapsulate] forms.  This was an oversight, and has been
  fixed.

  Sandip Ray has contributed a new library of books to support proofs of
  partial and total correctness of sequential programs based on assertional
  reasoning, in ~c[books/symbolic/].  This work is based on the paper
  J. Matthews, J S. Moore, S. Ray, and D. Vroon, ``A Symbolic Simulation
  Approach to Assertional Program Verification,'' currently in draft form.
  In particular, the books include the macro ~c[defsimulate], which
  automatically transforms inductive assertion proofs of correctness of
  sequential programs to the corresponding interpreter proofs.  See the
  ~c[README] in that directory.

  We have changed the implementation of ~c[:dir :system] for ~ilc[ld] and
  ~ilc[include-book].  This change will not affect you if you build an ACL2
  executable in the normal manner, leaving in place the ~c[books/] subdirectory
  of the source directory; nor will it affect you if you download a GCL Debian
  binary distribution.  The change is that if environment variable
  ~c[ACL2_SYSTEM_BOOKS] is set, then it specifies the distributed books
  directory, i.e., the directory determined by ~c[:dir :system].  You may find
  it convenient to set this variable in your ACL2 script file (typically,
  ~c[saved_acl2]).  If it is set when you build ACL2, the generated script for
  running ACL2 will begin by setting ~c[ACL2_SYSTEM_BOOKS] to that value.
  Thanks to various people who have discussed this issue, in particular Jared
  Davis who sent an email suggesting consideration of the use of an environment
  variable, and to Eric Smith who helped construct this paragraph.  (Note that
  this use of ~c[ACL2_SYSTEM_BOOKS] replaces the use of ~c[ACL2_SRC_BOOKS]
  described previously; ~pl[note-2-9-1].)

  ACL2 now automatically deletes files ~c[TMP*.lisp] created during the build
  process and created by ~c[:]~ilc[comp].  If you want these to be saved,
  evaluate ~c[(assign keep-tmp-files t)] in the ACL2 loop or in raw Lisp.  The
  ~c[clean] target for the standard ~c[make] process for certifying books
  (~pl[book-makefiles]) will however delete all files ~c[TMP*.*].

  The ~c[TMP] files discussed just above now generally include the current process
  ID in their names, e.g., ~c[TMP@16388@1.lisp] instead of ~c[TMP1.lisp].
  Thanks to Bob Boyer for suggesting this measure, which will reduce the
  possibility that two different processes will attempt to access the same
  temporary file.

  Now, ~c[:]~ilc[pe] will print the information formerly printed by ~c[:pe!],
  slightly enhanced to work for logical names that are strings, not just
  symbols.  Thanks to Warren Hunt for leading us to this change by suggesting
  that ~c[:pe nth] print the definition of ~ilc[nth].

  We eliminated spurious warnings that could occur in raw mode in OpenMCL or
  CMUCL when ~il[stobj]s are present.  We thank Juho Snellman for pointing out
  the relevant bug and appropriate fix.

  ~c[Mfc-rw] now takes a third argument that can specify an arbitrary known
  equivalence relation; ~pl[extended-metafunctions].  Thanks to Dave Greve for
  discussions suggesting this improvement.

  A small modification to a symbol-reading function allows documentation string
  processing on Windows systems that use CR/LF for line breaks.  Thanks to
  William Cook for bringing this issue to our attention.

  The documentation has been improved on how to control the printing of ACL2
  terms.  ~l[user-defined-functions-table].  Thanks to Sandip Ray for asking a
  question that led to the example presented there.

  We fixed an inefficiency that could cause an ~ilc[ld] command to seem to hang
  at its conclusion.  Thanks to Sandip Ray for pointing out this problem.

  We checked that ACL2 runs under Lispworks 4.4.5 (last checked before 4.3),
  and have inhibited redefinition warnings.

  Two changes have been made on behalf of congruence-based reasoning.  Thanks
  to Dave Greve for examples and discussions that have led to these changes,
  and to Eric Smith and Vernon Austel, who also sent relevant examples.
  ~bq[]
  o When a call of the new unary function ~ilc[double-rewrite] is encountered
  by the rewriter, its argument will be rewritten twice.  This solves certain
  problems encountered in congruence-based rewriting.  Warnings for
  ~c[:]~ilc[rewrite] and ~c[:]~ilc[linear] rules will suggest when calls of
  ~ilc[double-rewrite] on variables in hypotheses are likely to be a good idea.
  ~l[double-rewrite].

  o Hypotheses of the form ~c[(equiv var (double-rewrite term))], where
  ~c[equiv] is a known ~il[equivalence] relation and ~c[var] is a free variable
  (~pl[free-variables]), will bind ~c[var] to the result of rewriting ~c[term]
  twice.  Previously, hypotheses of the form ~c[(equal var term)] would bind a
  free variable ~c[var], but the call had to be of ~c[equal] rather than of an
  arbitrary known equivalence relation.~eq[]

  The following improvements were made in support of ACL2 on top of OpenMCL.
  ~bq[]

  o New versions of OpenMCL that do not have ~c[:mcl] in Lisp variable
  ~c[*features*] will now work with ACL2.  Thanks to David Rager for bringing
  this issue to our attention.

  o Added support for OpenMCL 1.0 for 64-bit DarwinPPC/MacOS X, thanks to
  Robert Krug.

  o Fixed tracing in OpenMCL so that the level is reset to 1 even if there has
  been an abort.

  o Added support in OpenMCL for ~c[WET] (~pl[wet]).

  o Incorporated suggestions from Gary Byers for printing the ``Welcome to
  OpenMCL'' prompt before initially entering the ACL2 loop and, and for setting
  useful environment variable ~c[CCL_DEFAULT_DIRECTORY] in the ACL2
  script.~eq[]

  Fixed a long-standing bug in forward-chaining, where variable-free hypotheses
  were being evaluated even if the ~il[executable-counterpart]s of their
  function symbols had been disabled.  Thanks to Eric Smith for bringing this
  bug to our attention by sending a simple example that exhibited the problem.

  Improved reporting by the ~il[break-rewrite] utility upon failure to relieve
  hypotheses in the presence of free variables, so that information is shown
  about the attempting bindings.  ~l[free-variables-examples-rewrite].  Thanks
  to Eric Smith for requesting this improvement.  Also improved the
  ~il[break-rewrite] loop so that terms, in particular from unifying
  substitutions, are printed without hiding subterms by default.  The user can
  control such hiding (``evisceration''); ~pl[set-brr-term-evisc-tuple].

  A new directory ~c[books/defexec/] contains books that illustrate the use of
  ~ilc[mbe] and ~ilc[defexec].  Thanks to the contributors of those books (see
  the ~c[README] file in that directory).

  The directories ~c[books/rtl/rel2] and ~c[books/rtl/rel3] are no longer
  distributed.  They are still available by email request.  (Subdirectory
  ~c[rel1/] supports some of the optional ~c[workshop/] books, so it is still
  distributed.)

  Added book ~c[books/misc/sticky-disable.lisp] to manage ~il[theories] that
  might otherwise be modified adversely by ~ilc[include-book].  Thanks to Ray
  Richards for a query that led to our development of this tool.

  The commands ~c[(exit)] and ~c[(quit)] may now be used to quit ACL2 and Lisp
  completely; in fact they macroexpand to calls of the same function as does
  ~ilc[good-bye] (which is now a macro).  Thanks to Jared Davis for suggesting
  the new aliases.  (OpenMCL-only comment:) These all work for OpenMCL even
  inside the ACL2 loop.

  The macro ~ilc[wet] now hides structure by default on large expressions.
  However, a new optional argument controls this behavior, for example avoiding
  such hiding if that argument is ~c[nil].  ~l[wet].  Thanks to Hanbing Liu for
  pointing out that ~c[wet] was not helpful for very large terms.

  We have fixed a bug in the forward-chaining mechanism that, very rarely,
  could cause a proof to be aborted needlessly with an obscure error message.
  Thanks to Jared Davis for sending us an example that evoked this bug.

  Fixed a bug that was causing proof output on behalf of
  ~c[:functional-instance] to be confusing, because it failed to mention that
  the number of constraints may be different from the number of subgoals
  generated.  Thanks to Robert Krug for pointing out this confusing output.
  The fix also causes the reporting of rules used when silently simplifying the
  constraints to create the subgoals.

  Fixed a bug in handling of leading ~c[./] in pathnames, as in:
  ~c[(include-book \"./foo\")].  Thanks to Jared Davis for bringing this bug to
  our attention.

  Made a small fix for handling of free variables of ~c[:]~il[forward-chaining]
  rules, which had erroneously acted as though a hypothesis
  ~c[(equal var term)] can bind the variable ~c[var].

  A small change has been made for ~c[:]~ilc[type-prescription] rules for
  hypotheses of the form ~c[(equal var term)], where c[var] is a free variable
  and no variable of ~c[term] is free (~pl[free-variables]).  As with
  ~c[:]~ilc[rewrite] and ~c[:]~ilc[linear] rules, we now bind ~c[var] to
  ~c[term] even if ~c[(equal u term)] happens to be known in the current
  context for some term ~c[u].  Also as with ~c[:rewrite] and ~c[:linear]
  rules, similar handling is given to hypotheses
  ~c[(equiv var (double-rewrite term))] where ~c[equiv] is a known
  ~il[equivalence] relation.

  We changed the handling of free variables in hypotheses of ~c[:]~ilc[rewrite]
  rules being handled by the ~il[proof-checker]'s ~c[rewrite] (~c[r]) command,
  in complete analogy to the change described just above for
  ~c[:]~ilc[type-prescription] rules.

  The installation instructions have been updated for obtaining GCL on a
  Macintosh.  Thanks to Robert Krug for supplying this information and to Camm
  Maguire for simplifying the process by eliminating the ~c[gettext]
  dependency.

  The macro ~ilc[comp] is now an event, so it may be placed in ~il[books].

  Previously, a ~ilc[save-exec] call could fail because of file permission
  issues, yet ACL2 (and the underlying Lisp) would quit anyhow.  This has been
  fixed.  Thanks to Peter Dillinger for bringing this problem to our attention.

  Jared Davis, with assistance from David Rager, has updated his ordered sets
  library, ~c[books/finite-set-theory/osets/].  See file ~c[CHANGES.html] in
  that directory.

  A new function, ~ilc[reset-kill-ring], has been provided for the rare user
  who encounters memory limitations.  ~l[reset-kill-ring].

  ~/~/")

(deflabel note-2-9-5

; Things that seem too minor to mention:

; Modified translate11 to complain about LOCAL in code.  Before this fix, the
; definition
; (defun foo (x state) (local (value x)))
; caused (foo 3 state) to cause a hard error in getprop.

; Removed "time nice" from books/defexec/dag-unification/Makefile (doesn't work
; in some environments, as I recall).

; Added targets d64fsl and all-d64fsl to books/Makefile*, to support
; compilation of already-certified books on 64-bit OpenMCL.

; Added initial bindings of acl2-raw-mode-p and raw-mode-restore-lst to nil (in
; *initial-global-table*), as suggested by Peter Dillinger.

; Fixed warning stack issue with print-summary, which caused new
; theory-invariant implementation to avoid popping warning stacks because of
; with-output calls.

; Inlined call of equivalence-relation-to-geneqv with something more
; appropriate, and elminated equivalence-relation-to-geneqv.

; Changed rewrite-with-lemmas so that :expand hints (from user or from
; induction) are always followed, without the being-openedp call that used to
; be there.

; The change for bodies (new 'def-bodies property) causes the following
; to use the latest definition for the body (e.g., affecting :expand
; hints).
; - non-rec-defun, called by deref for nu-rewriter
; - recursivep, called in rewrite-with-lemma and bdd-rules-alist1
; - expand-and-or, used by preprocess
; - non-recursive-fnnames, used only in warnings
; - proof-checker's expand command (hence also x and x-dumb commands)
; - induction heuristics that use controller-alist

; Modified extend-type-alist to add check for (not (fquotep term)), which had
; been assumed but was not necessarily true because of the call of
; extend-type-alist from extend-type-alist-with-bindings.

; Updated *current-acl2-world-key-ordering*, and comments about it.

; Replaced the use of proclaim by the use of declaim (thanks to Bob Boyer for
; the suggestion to consider this).

; Fixed typo in uncovered-equivs-alist, 'f instead of 'if, that could cause
; incorrect double-rewrite warnings.  But went beyond this and gave more
; appropriate special handling for 'if.

; Fixed obscure bug in maybe-push-undo-stack, which could happen with:
; 
; :redef!
; (defun exit-boot-strap-mode () t)
; 
; The problem was that we assumed that a *1* function is defined when its raw
; Lisp counterpart is defined.

; Added missing dependencies in books/ihs/Makefile.

; Changed chk-embedded-event-form so that it returns the expansion rather than
; the original form, in support of the implementation of make-event.

; Eliminated get-check-sums-lst and check-sum-file (dead code).

; Acl2-compile-file now loads the compiled file.

; Simplified *compiled-file-extension* according to a suggestion from Gary
; Byers.

; With Robert Krug's help, including arithmetic-3/bind-free/top will now print
; a message about turning on non-linear.

; Made slight improvement in ev-fncall-guard-er-msg so that we don't suggest
; set-guard-checking nil when the problem is safe-mode or stobjs.

; Documentation and comments have been changed, for the most part, so that the
; word ``multiple'' is used correctly.  Thanks to David Russinoff for pointing
; out this issue.

; Fixed getpid$ for clisp to use 'system::process-id rather than 'process-id.
; This puts the process id into the name of TMP* files produced by :comp t,
; which hadn't been done for clisp because of the getpid$ bug.

; Changed definition of macro state-global-let* (specifically, changed
; definition of supporting function state-global-let*-cleanup) to produce much
; more compact code for large numbers of bindings as in
; protect-state-globals-for-make-event.  Without that change, lispworks
; reported:
; **++++ Error in ACL2::PROTECTED-EVAL-WITH-PROOFS: 
;   Function size 87365 is too large.

; Eliminated compiler note "Ignoring free ignore declaration" for mbe, for
; CMUCL and SBCL (had already done so for OpenMCL).

; Fixed a bug in make-include-books-absolute that caused an "Implementation
; error" when with-output occurs in a progn with an include-book in the
; certification world, as in:
; (progn (include-book "cowles/acl2-asg" :dir :system)
;        (with-output :off summary (defun abc (x) x)))

; Added a call to the garbage collector before saving in Allegro CL, CMUCL,
; SBCL, CLISP, and OpenMCL.  (There was already such a call in GCL and
; Lispworks.)  We saw a little performance increase and significant shrinkage
; of the saved image when we did this for Allegro CL.

  :doc
  ":Doc-Section release-notes

  Changes in Version  3.0 since Version  2.9.4~/

  Fixed a bug in ~ilc[cw-gstack] that was causing a hard error when attempting
  to report on a forced assumption.  Thanks to Jared Davis for pointing this
  out and sending an example that helped us to determine a fix.

  Added ~ilc[set-backchain-limit] to the set of legal ~il[events] that can be
  placed in ~ilc[encapsulate] forms and ~il[books].  Thanks to John Cowles for
  bringing this issue to our attention.

  Fixed a bug that broke ~ilc[wet].  Thanks to David Rager for bringing this
  bug to our attention.

  Guard verification now evaluates ground subexpressions (those with no free
  variables) when computing the guard conjecture for the body of a function.
  Thanks to Jared Davis for useful conversations leading to this change.
  ~l[verify-guards], in particular its ``Note on computation of guard
  conjectures and evaluation'' near the end of that topic, for more details.

  Added a warning when a ~ilc[theory-invariant] is redefined.  Thanks to Jared
  Davis for suggesting a warning in this case and providing an informative
  example.  Also, ~ilc[theory-invariant]s are now maintained more completely,
  as they are checked at the end of every event except for events executed on
  behalf of an ~ilc[include-book] or the second pass of an
  ~ilc[encapsulate].

  Fixed the handling of runic designators to match their specification
  (~pl[theories]), so that disabling the name of a ~ilc[defthm] event
  ~ilc[disable]s all rules generated for that event.

  (For those who do numerous builds using feature ~c[:acl2-mv-as-values],
  currently only OpenMCL and multi-threaded SBCL by default:) You can speed up
  builds by adding the following parameter to ~c[make], under conditions
  described in ~c[GNUmakefile]: ~c[USE_ACL2_PROCLAIMS=:REUSE].

  Arranged that traced functions (~pl[trace$]) are automatically untraced when
  events are undone (for example ~pl[ubt]), at least for most underlying Common
  Lisp implementations.

  The macro ~ilc[defun-sk] now creates non-executable functions, which allows
  ~ilc[stobj]s to be used where they had previously been prohibited.  More
  generally, the user now has control over ~ilc[declare] forms to be used by
  the underlying ~ilc[defun]'d function; ~pl[defun-sk].  Thanks to Sandip for
  pointing out the need for such a modification.

  ~c[:]~ilc[Definition] rules are now treated, at least by default, as truly
  first-class definitions.  In particular, ~c[:expand] ~il[hints] use the
  latest ~c[:]~ilc[definition] rule by default.  You may specify
  ~c[:install-body nil] to get the previous behavior of ~c[:definition] rules;
  ~l[definition], and you may choose a previously-installed ~c[:definition]
  rule to provide the current body; ~pl[set-body].  Also ~pl[rule-classes] for
  details of the ~c[:install-body] field, and ~pl[hints] to see a new ~c[:with]
  directive for controlling expansion.  The ~c[:with] directive for ~c[:expand]
  hints can even direct the use of a ~c[:]~ilc[rewrite] rule for expansion!
  Thanks to various people, including Sandip Ray and Rob Sumners, for
  discussions on the issue of the applicability of ~c[:definition] rules for
  ~c[:expand] ~il[hints].

  ~il[Constraint]s for functional instantiation now use the original definition
  rather than a simplified (``normalized'') version of it.

  Fixed a bug that caused the prompt to stay the same when guard-checking is
  off (~pl[set-guard-checking]) and raw-mode is changed (~pl[set-raw-mode]).

  Lemma names in directory ~c[books/ordinals] have been changed by replacing
  ~c[/\\] with ~c[&] and replacing ~c[\\/] with ~c[V].  We made this change
  because backslash is an escape character and hence disappears unless it is
  itself escaped.

  Fixed ~il[proof-tree] output so that failed non-proof events do not cause the
  proof-tree to be re-printed.  Thus for example, if you have already advanced
  the checkpoint marker, it will not be reset by subequent failed non-proof
  events.  Thanks to Pete Manolios and Peter Dillinger for bringing this bug to
  our attention.

  Fixed a bug that was preventing the printing of ~il[stobj] fields as
  constants instead of numbers in certain cases.  (Note that this bug only
  affected printing, not soundness.) Thanks to Eric Smith for bringing this
  problem to our attention and providing the following example (which now works
  fine).
  ~bv[]
  (defstobj st fld1 fld2)
  (in-theory (disable update-nth))
  (defund run (st)
    (declare (xargs :stobjs (st))) ;adding this didn't seem to help..
    st)

  ;works great; *fld1* prints as  *fld1*
  (thm (equal (update-nth *fld1* 'abc st)
              (car (cons x y))))

  ;*fld1* gets printed as 0, presumably because the call to run intervenes.
  (thm (equal (update-nth *fld1* 'abc (run st))
              (car (cons x y))))
  ~ev[]

  The macro ~ilc[progn] now allows the use of macros defined within its bodies
  even when at the event level, as illustrated by the following example.
  ~bv[]
  (progn (defmacro my-defun (&rest args)
           `(defun ,@args))
         (my-defun g (x) x))
  ~ev[]
  Thanks to Anna Slobodova for bringing this issue to our attention.  A related
  change is that all arguments of ~ilc[progn] must now be embedded event forms
  (~pl[embedded-event-form]), so use ~ilc[er-progn] instead if this is not the
  case.

  The change to ~ilc[progn] mentioned above also fixes a bug in handling
  ~il[local] events inside a ~ilc[progn] that is inside an ~ilc[encapsulate] or
  in a book.  For example, the following form formerly caused an error.
  ~bv[]
  (encapsulate
   ()
   (defun foo (x) x)
   (progn (local (defun bar (x) x))
          (defun abc (x) x)))
  ~ev[]

  We fixed two bugs in ~c[:]~ilc[puff] and ~c[:]~ilc[puff*].  The first,
  brought to our attention by Eric Smith (who we thank), caused a cryptic error
  message when puffing a command with no subsidiary stored events; try, for
  example, ~c[(encapsulate () (value-triple 3))].  The second was due to a
  failure to restore the ~ilc[acl2-defaults-table].  Suppose for example that
  we have certified the book ~c[foo.lisp], which contains
  ~c[(]~ilc[program]~c[)] followed by some definitions and/or theorems.  Now
  suppose we start ACL2 and execute the following.
  ~bv[]
  (include-book \"foo\")
  (defthm test-thm
    (equal x x)
    :rule-classes nil)
  ~ev[]
  If we now execute ~c[:]~ilc[puff]~c[ 1], ACL2 will roll back the world to
  before the ~ilc[include-book]; then ``puff'' the include-book, which will
  leave us in ~c[:]~ilc[program] mode; and finally skip re-execution of the
  ~ilc[defthm] because such ~il[events] are skipped in ~c[:]~ilc[program] mode.
  The fix is to re-install the ~ilc[acl2-defaults-table] immediately after the
  ~ilc[include-book] to its pre-~ilc[include-book] value.

  A new event, ~ilc[make-event], provides something like macros that take
  state.  For example, one can use it to put tests into certified books, do
  proof search, and generate new function names.  Many examples appear in
  directory ~c[books/make-event/].  ~l[make-event].  Thanks to Bob Boyer and
  Jared Davis for useful feedback and to Warren Hunt, David Rager, and Sandip
  Ray for helpful discussions leading to some of the examples in directory
  ~c[books/make-event/].

  In support of ~ilc[make-event], which is described in the preceding
  paragraph, ~c[certify-book] has a new keyword argument, ~c[:save-expansion],
  that controls whether the result of expanding ~ilc[make-event] forms is
  written out to a file.  ~l[certify-book]; and for a discussion of book
  expansion files, ~pl[make-event].

  We fixed a soundness bug that did not correctly detect ~ilc[local] events.
  For example, the following event was admitted.
  ~bv[]
  (encapsulate
   ()
   (local
    (encapsulate
     ()
     (local (progn (program))) ; or, (local (with-output :off summary (program)))
     (set-irrelevant-formals-ok t)
     (defun foo (x)
       (declare (xargs :measure (acl2-count x)))
       (1+ (foo x)))))
   (defthm inconsistent
     nil
     :hints ((\"Goal\" :use foo))
     :rule-classes nil))
  ~ev[]

  A new value for ~il[guard] checking, ~c[:none], is now allowed.  If you
  execute ~c[:]~ilc[set-guard-checking]~c[ :none], then no guard checking will
  take place (but raw Lisp code will not be executed in this case).  As a
  result, you should never see a guard violation, even for calls of
  ~c[:]~c[program] mode functions.  We thank Pete Manolios, who has long wanted
  this feature, and also Peter Dillinger, for asking for it.  New documentation
  explains the interaction between the ~il[defun-mode] and the value supplied
  to ~c[:]~ilc[set-guard-checking].  ~l[guard-evaluation-table],
  ~pl[guard-evaluation-examples-script], and
  ~pl[guard-evaluation-examples-log].

  In the course of adding the ~il[guard]-checking value ~c[:none] described in
  the paragraph above, we eliminated an optimization that eliminated guard
  checking for some recursive calls of ~c[:]~il[logic] mode mutually-recursive
  functions that have not had their guards verified.  But we doubt that this
  change will be noticed by any users!)

  The ACL2 hyper-card has been enhanced, thanks to David Rager, with a listing
  of ``Useful EMACS Commands'' to match comments in ~c[emacs/emacs-acl2.el].

  Users contributed books following the ~c[Readme.lsp] methodology:
  ~c[data-structures/memories] and ~c[unicode] (Jared Davis), ~c[proofstyles]
  (Sandip Ray and J Moore).

  Made some improvements to ~c[books/Makefile-generic] (a file discussed
  elsewhere; ~pl[book-makefiles]).  In particular, improved handling of
  ~c[.acl2] files for ~c[dependencies] target.

  (Only OpenMCL and, with feature ~c[:acl2-mv-as-values], GCL) Fixed a bug that
  was causing proclaiming to fail when definitions are submitted interactively.

  The default stack size has been increased for several lisps.

  (Very technical) A restriction has been weakened on the use of ~ilc[local]
  ~il[stobj]s under a call of an ACL2 evaluator (~c[trans-eval] or
  ~c[simple-translate-and-eval]).  Now, the error can only take place for
  ~ilc[stobj] names that occur in the term being evaluated.  Thanks to Erik
  Reeber for bringing this issue to our attention.

  The notion of ``ancestor'' has been changed slightly.  This notion is used by
  extended metafunctions and ~il[break-rewrite] (~pl[extended-metafunctions]
  and ~pl[brr@]), and also with backchain limits (~pl[backchain-limit] and
  ~pl[set-backchain-limit]).  Basically, each time a hypothesis is encountered
  during application of a ~il[rewrite] rule, that hypothesis is pushed (after
  instantiating and negating) onto the current list of ancestors before it is
  rewritten.  However, hypotheses of the form ~c[(equal var term)], where
  ~c[var] is free (~pl[free-variables]), had not been included in the ancestors
  (similarly for ~c[(equiv var (double-rewrite term))] where ~c[equiv] is a
  known ~il[equivalence] relation).  Now such ``binding hypotheses'' are
  included in a special way in ancestors data structures.  In particular,
  ~c[(null (mfc-ancestors mfc))] will now be true if and only if the term being
  rewritten is part of the current goal as opposed to a hypothesis from a rule
  encountered during backchaining, even if that hypothesis is a binding
  hypothesis.  Thanks to Dave Greve for bringing this issue to our attention.

  Termination and induction analysis now continue through both arguments of
  ~ilc[prog2$], not just the second.  (Normally, the gathering up of ~ilc[if]
  tests stops at function calls; but it continued through the second argument
  of ~ilc[prog2$], and now it will continue through both arguments.)  Thanks to
  Sol Swords for discussion leading to this change.

  The ACL2 distribution is now kept on the http server rather than the ftp
  server (but the home page has not been moved).  Thanks to Robert Krug for
  letting us know that some ACL2 users have found it inconvenient to fetch ACL2
  using ftp.

  The file ~c[books/README.html] has been renamed to ~c[books/Readme.html],
  since some browsers don't show the former in the directory listing.

  ~/~/")

(deflabel note-3-0

; See note-2-9-5 for some comments enumerating changes not in the :doc since
; v2-9-4.

  :doc
  ":Doc-Section release-notes

  ACL2 Version  3.0 (June, 2006) Notes~/

  Please ~pl[note-2-9-5] for a description of changes since Version  2.9.4.
  These include the new ~ilc[make-event] feature, a soundness bug fix, an
  improvement for ~c[:expand] ~il[hints], evaluation in the logic by way of
  ~c[:]~ilc[set-guard-checking]~c[ :none], and many other improvements.

  More generally, there have been several incremental releases since
  Version  2.9: ~pl[note-2-9-1], ~pl[note-2-9-2], ~pl[note-2-9-3],
  ~pl[note-2-9-4], and ~pl[note-2-9-5].

  A very few users have contributed books following the instructions on the
  web.  We expect that when more contributions come in, we will give more
  attention to the question of how to organize the distributed and workshop
  books.  For now, we have simply added the new contributions according to the
  old-style distribution methodology.

  ~/~/")

(deflabel |NOTE-3-0(R)|
  :doc
  ":Doc-Section release-notes

  ACL2 Version  3.0(r) (June, 2006) Notes~/

  No significant changes have been made since Version  2.9 for support of
  non-standard analysis in particular.

  ~/

  Please also ~pl[note-3-0] for changes to Version  3.0 of ACL2.
  ~/
  ")

(deflabel note-3-0-1

; Things that seem too minor to mention:

; Modified proclaiming to understand (declare (type fixnum ...)) for function
; arguments.

; Regarding the subsumption item below: We still do the old subsumption check
; for chk-evaluator and processing of :by hints, because the user has some idea
; in these cases of what is going on (plus, the former will probably always be
; fast).  But we do the restricted check in the clause-set-subsumes call of
; chk-acceptable-equivalence-rule and (for induction) in
; some-pool-member-subsumes because those are kind of subtle and it's OK if
; they fail.

; The new requirement on the argument of satisfies resulted in an extra wrld
; argument for translate-declaration-to-guard (and other source functions),
; which is used in several books.

; Took suggestion from Bob Boyer to arrange to throw out extra values returned
; by intern, for efficiency (see intern-in-package-of-symbol and comments "See
; comment in intern-in-package-of-symbol").

; Fixed *initial-global-table* to satisfy ordered-symbol-alistp, and added a
; check for this just after the definition of *initial-global-table*.

; The improvement in handling of theories included an extra argument in
; set-difference-theories-fn and in union-theories-fn, lst1-known-to-be-runic.
; Users who are sophisticated and brave enough to use these unadvertised
; internal functions should be able to figure out this change, so we choose not
; to confuse other users by mentioning that in this :doc topic.

; Regarding the item below on hard lisp error and safe mode: We added a number
; of function/macro symbols to the list in *oneify-primitives* and to avoid
; oneifying, in order to support the use of safe-mode with make-event (which
; however we have abandoned for now because it seems difficult to ensure that
; we are dealing properly with *1* functions with corresponding hand-coded raw
; Lisp definitions).  We also fixed bugs discovered as we attempted to certify
; the books/make-event books:
; - state-global-let*-get-globals to use f-boundp-global in place of
;   boundp-global;
; - the call of fmt1 in print-warnings-summary;
; - the call (eq new-type-alist type-alist) in type-alist-clause-finish; and
; - the calls of eq in changed-pot-vars.

; Oneify-cltl-code has been slightly optimized for the case that
; 'guard-checking-on is :none, by eliminating some code that has no effect.

; The event comp now compiles properly for lists of functions even when some
; are in the boot-strap world.  In general, the code for
; compile-uncompiled-defuns and compile-uncompiled-*1*-defuns was cleaned up
; considerably, including the removal of proclaiming since add-trip is
; responsible for that.

; Deleted now-dead code collect-ideal-user-defuns and
; collect-ideal-user-defuns1.

; Among the "miscellaneous efficiency improvements not listed above" is an
; improvement to assume-true-false, which now takes an ignore argument that
; avoids some computation of the true-type-alist or false-type-alist when these
; are to be thrown away anyhow.

; Improved the "Guards" warning from certify-book to avoid rather odd mention
; of loading into raw Lisp.  Thanks to Sandip Ray and Robert Krug for helpful
; discussions.

; Modified documentation for topics ``~ilc[brr]'' and ``~ilc[breaks]'' to
; clarify that if you are at a raw Lisp break, then ~c[(]~ilc[abort!]~c[)] will
; get you back to the ACL2 top level.  Thanks to Dave Greve and Eric Smith for
; bringing this issue to our attention.

; Made minor mods, e.g. to avoid an eq test of n against header in the raw Lisp
; code for aref1.

; Improved support for window interfaces, in particular with new state globals
; window-interface-prelude and window-interface-postlude, thanks to
; contributions from Peter Dillinger.

  :doc
  ":Doc-Section release-notes

  ACL2 Version  3.0.1 (August, 2006) Notes~/

  NOTE!  New users can ignore these release notes, because the documentation
  has been updated to reflect all changes that are recorded here.

  Fixed a soundness bug, introduced in the previous release, due to a failure
  to disallow ~ilc[table] ~il[events] that set the ~ilc[acl2-defaults-table] in
  a ~ilc[local] context.  Here is a proof of ~c[nil] that exploits the bug.
  ~bv[]
  (encapsulate ()
   (local (program))
   (defun foo ()
     (declare (xargs :measure 17))
     (+ 1 (foo))))
  (thm
   nil
   :hints ((\"Goal\" :in-theory (disable foo (foo)) :use foo)))
  ~ev[]

  Fixed a bug in the alternatives to ~ilc[good-bye], which are the ~ilc[exit]
  and ~ilc[quit] commands.  Thanks to Jared Davis and Peter Dillinger for
  pointing this out right away.

  The definition of ~ilc[len] has been highly optimized in raw Lisp.  Thanks to
  Bob Boyer and Warren Hunt for suggesting such an improvement and providing
  a lot of help in coming up with the current implementation.

  The clause subsumption algorithms have been improved, both to improve
  efficiency during warnings for ~c[:]~ilc[rewrite] rules and to punt when the
  subsumption computation for induction appears to be blowing up.  Thanks to
  Robert Krug for bringing this issue to our attention and supplying a useful
  example.

  A bug has been fixed that prevented ~ilc[time$] from working properly in
  OpenMCL and multi-threaded SBCL (actually, in any ACL2 image where feature
  ~c[:acl2-mv-as-values] is present).  Thanks to Sol Swords for bringing this
  problem to our attention.

  A ~il[type-spec] of the form ~c[(satisfies pred)] carries the requirement
  that ~c[pred] be a unary function symbol in the current ACL2 ~il[world];
  otherwise, it is illegal.  Thanks to Dave Greve for pointing out that Common
  Lisp has this requirement.

  Installed a fix provided by Gary Byers (for ACL2 source function
  ~c[install-new-raw-prompt]), for OpenMCL, that fixes an issue exposed in some
  versions of OpenMCL when compiler optimization is off.

  Fixed a bug in contributed book ~c[misc/untranslate-patterns.lisp] that was
  causing calls of ~c[add-untranslate-pattern] to be rejected in ~il[books].
  Thanks to Ray Richards for pointing out this bug and to Jared Davis for
  assisting in the fix.

  Fixed a bug in ~ilc[defstobj] when keywords ~c[:initially] and ~c[:resizable]
  are both supplied.  In this case, the definition of the resizing function
  mistakenly failed to quote the ~c[:initially] value, even though this value
  is not to be evaluated.  One could even get an error in this case, as in the
  following example supplied by Erik Reeber, whom we thank for bringing this
  bug to our attention:
  ~bv[]
    (defstobj $test 
      (test-x :type (array t (5)) :initially (0) :resizable t))
  ~ev[]

  A new feature, ~ilc[with-prover-time-limit], allows the setting of time
  limits during proofs.  This is ~st[not] a general-purpose time-limit utility,
  nor is it guaranteed to implement a strict bound; it only attempts to limit
  time approximately during proofs.  Thanks to Pete Manolios and Daron Vroon,
  who made the most recent request for such a feature, and to Robert Krug for a
  helpful discussion.

  (GCL only) Fixed a bug in the procedure for building a profiling image.
  Thanks to Sol Swords for bringing this bug to our attention and to Eric Smith
  for bringing a subsequent problem to our attention.

  Handling of ~il[theories] can now use significantly less time and space.  A
  regression suite run took about 25% longer before this change than it did
  after making this change (and also the ones in the next two paragraphs).
  Thanks to Vernon Austel for bringing this issue to our attention and for
  supplying code, quite some time ago, that provided detailed, useful
  implementation suggestions.  Also thanks to the folks at Rockwell Collins,
  Inc. for pushing the limits of the existing implementation, thus encouraging
  this improvement.

  Fixed a performance bug in obtaining executable counterpart symbols.

  We now avoid certain computations made on behalf of warnings, when such
  warnings are disabled.

  We have relaxed the checks made when including an uncertified book, to
  match the checks made when including a certified book.  Thanks to Eric Smith
  for suggesting this change.

  Fixed a bug in ~c[:]~ilc[pso] (~pl[set-saved-output]) that caused an error
  when printing the time summary.

  Made fixes to avoid potential hard Lisp errors caused by the use of
  ~c[:]~ilc[program] mode functions.  The fix was to use a ``safe mode,''
  already in use to prevent such errors during macroexpansion;
  ~pl[guards-and-evaluation].  However, such errors were possible during
  evaluation of macro ~il[guard]s, for example as follows:
  ~bv[]
  (defun foo (x)
    (declare (xargs :mode :program))
    (car x))
  (defmacro mac (x)
    (declare (xargs :guard (foo 3)))
    x)
  (defun g (x)
    (mac x))
  ~ev[]
  A similar issue existed for calls of ~ilc[defpkg], ~ilc[in-theory],
  ~ilc[table], ~ilc[make-event], and ~c[value-triple], but has been fixed for
  all but ~c[in-theory] and ~c[make-event], where technical issues have caused
  us to defer this change.

  Fixed a bug in ~ilc[wet] that caused problems in OpenMCL, and perhaps other
  Lisp implementations, when the argument to ~c[wet] calls, or depends on,
  certain built-ins including ~ilc[prog2$], ~ilc[time$], ~ilc[mbe], and
  ~ilc[must-be-equal].  Thanks to David Rager for bringing this problem to our
  attention.

  The file ~c[books/Makefile-generic] has been improved so that when book
  certification fails with ~c[make], the failure message contains the book
  filename.

  Documentation has been written to explain how to avoid an expensive immediate
  rewrite of the result of applying a ~c[:]~ilc[rewrite] or ~c[:]~ilc[meta]
  rule.  ~l[meta].  Thanks to Robert Krug for supplying this trick, and to Eric
  Smith and Dave Greve for useful discussions.

  (OpenMCL only) OpenMCL-based ACL2 image names formerly had extension
  ~c[\".dppccl\"], which was correct only for some platforms (including 32-bit
  Darwin PPC).  That has been fixed, thanks to a suggestion from Gary Byers.

  It is now legal to attach both a ~c[:use] and a ~c[:cases] hint at the same
  goal.  Thanks to Eric Smith for (most recently) requesting this feature.

  It is now permissible to include the same symbol more than once in the
  imports list of a ~ilc[defpkg] form (i.e., its second argument).  Also, the
  evaluation of ~ilc[defpkg] forms with long import lists now uses a reasonably
  efficient sorting routine to check for two different symbols with the same
  name (see also ~c[books/sort-symbols.lisp]).  If you currently call a
  function like ~c[remove-duplicates-eql] for your imports list, as had been
  suggested by a ~ilc[defpkg] error message, then you may experience some
  speed-up by removing that call.  Thanks to Eric Smith for helping to discover
  this issue through profiling.

  Made miscellaneous efficiency improvements not listed above (for example,
  following a suggestion of Eric Smith to avoid checking for so-called ``bad
  Lisp objects'' during ~ilc[include-book], which saved almost 3% in time on
  one large example).

  Modified the notion of ``untouchable'' to separate the notion of untouchable
  functions and macros from the notion of untouchable state global variables.
  ~l[push-untouchable].  Thanks to Bob Boyer for sending an example,
  ~c[(put-global 'ld-evisc-tuple t state)], that suggested to us the need for
  more restrictive handling of untouchables.  In particular, many ~c[ld]
  specials (~pl[ld]) are now untouchable.  You may be able to work around this
  restriction by calling ~ilc[ld]; see for example the change to
  ~c[books/misc/expander.lisp].  But please contact the ACL2 implementors if
  this sort of workaround does not appear to be sufficient for your purposes.

  Fixed a bug in function ~c[set-standard-oi] (~pl[standard-oi]).

  Fixed a bug in the use of ~ilc[ld-evisc-tuple].  The bad behavior was an
  improper use of the print-level and print-length components of the tuple
  (specifically, taking its ~ilc[caddr] and ~ilc[cadddr] instead of taking its
  ~ilc[cadr] and ~ilc[caddr]).  Thanks to Bob Boyer for bringing this bug to
  our attention.

  A new argument to the ~c[compile-flg] argument of ~ilc[certify-book],
  ~c[:all], causes creation of a file to be compiled in place of the given
  book, where that file contains not only a copy of the book (with
  ~ilc[make-event] forms expanded) but also contains definitions of the
  so-called ``executable counterparts'' of the functions defined in the book.
  Then, functions defined in the book will be run compiled when including the
  book, even for functions whose ~il[guard]s have not been verified, or are in
  ~c[:program] mode and running in so-called ``safe mode''
  (for example, during expansion of macros).  The default behavior, value ~c[t]
  of ~c[compile-flg], is unchanged.  Moreover, a new ~c[:comp!] argument of
  ~ilc[include-book] now compiles the executable counterparts when creating the
  book's compiled file, and unlike ~c[:comp], always deletes the old compiled
  file first so that one always gets a fresh compile.

  Now, ~ilc[certify-book] gives a \"Guards\" warning only for ~c[:]~ilc[logic]
  mode functions that are defined in the given book but have not had their
  guards verified.  Previously, it also warned about such functions that were
  defined in the certification world or in sub-books.

  A new command, ~ilc[redo-flat], facilitates the debugging of failed
  ~ilc[encapsulate] and ~ilc[progn] forms by evaluating preliminary forms in
  order to leave one at the point of failure.  ~l[redo-flat].  Thanks to
  Ray Richards and others for asking for such a utility, and to Sandip Ray
  for useful discussions.

  We have changed the automatic declaration of of function types (still done in
  GCL and OpenMCL only, for now).  Our motivation was to avoid the assumption
  that Common Lisp functions return one value when ACL2 says that they do;
  thanks to Bob Boyer for bringing this issue to our attention with the example
  of defining ~c[(foo x y)] to be ~c[(floor x y)].  ACL2 was saying that
  ~c[foo] returns a single value, but because ~c[floor] returns two values in
  raw Lisp, so does ~c[foo].  Other changes to automatic declaration include
  comprehending ~ilc[defund], not just ~ilc[defun].

  A new function, ~ilc[mod-expt], computes ~c[(mod (expt base exp) m)], and
  does so efficiently in some implementations (currently only in GCL 2.7.0,
  which is not yet released).  Thanks to Warren Hunt for suggesting such an
  addition.

  New functions ~ilc[getenv$] and ~ilc[setenv$] have been made available for
  reading and writing environment variables.  Thanks to Jun Sawada for
  requesting these utilities.

  The query utility ~c[:]~ilc[pl] has been improved in several ways.  As
  before, ~c[:]~ilc[meta] rules are only printed if the argument is a symbol;
  but the information printed for them is now more appropriate.  The following
  are changes for the case that the argument is not a symbol, but rather, a
  term.  (1) Rules are displayed that have ~il[equivalence] relations other
  than ~ilc[equal].  (2) All matching ~c[:]~ilc[definition] rules are
  displayed, where previously ~c[:definition] rules were only shown if they
  were ``simple'' rules (sometimes known as ``abbreviations''); ~pl[simple].
  (3) The ``Equiv'' field is printed for terms, not just symbols.  (4) The
  substitution is shown that, when applied to the left-hand side of the rule,
  will yield the specified term.  Thanks to Eric Smith for suggesting these
  changes.

  The ~il[proof-checker] command ~c[;show-rewrites] has been improved to match
  the changes described above for ~c[:]~ilc[pl].  In particular,
  ~c[:]~ilc[definition] rules that are not ``~il[simple]'' are now displayed by
  the ~il[proof-checker]'s ~c[show-rewrites] (and ~c[sr]) command, and the
  ~il[proof-checker]'s ~c[rewrite] command has been correspondingly modified to
  accept these ~c[:definition] rules.

  Fixed ~c[make] targets ~c[copy-distribution], ~c[copy-workshops], and
  ~c[copy-nonstd] so that they should also work for non-developers.

  Fixed a bug that was causing ~c[:]~ilc[pr] to display ~ilc[syntaxp]
  hypotheses oddly in some cases, in particular ~c[(syntaxp (let ...))].
  (The problem was in the ``untranslate'' display of the internal form of
  ~c[syntaxp] calls.)  Thanks to Robert Krug for bringing this problem to our
  attention.  We also removed the restriction on ~ilc[bind-free] that its
  argument could not be a variable, a constant, or (more interestingly) a
  ~ilc[lambda] application (i.e., a ~ilc[let] or ~ilc[mv-let] expression).

  ~/~/")

(deflabel |NOTE-3-0-1(R)|
  :doc
  ":Doc-Section release-notes

  ACL2 Version  3.0.1(r) (August, 2006) Notes~/

  No significant changes have been made since Version  3.0 for support of
  non-standard analysis in particular.

  ~/

  Please also ~pl[note-3-0-1] for changes to Version  3.0.1 of ACL2.
  ~/
  ")

(deflabel note-3-0-2

; Things that seem too minor to mention to users:

; Added ev-w and ev-w-lst.

; The "soundness bug in linear arithmetic" mentioned below was confined to
; linearize1.  Robert provided the fix and we checked it.  Below is an example
; from Robert that proves nil in ACL2 Version_3.0.1 but fails after the patch.
#|
 (defun id (x) x)

 (defthm id-rationalp
   (implies (force (rationalp x))
            (rationalp (id x)))
   :rule-classes :type-prescription)

 (in-theory (disable id))

 (defun id2 (x y)
   (if (zp x)
       y
     (id2 (+ -1 x) y)))

 (in-theory (disable (:type-prescription id2)))

 (defthm bad
   (implies (and (not (equal (id x) (id2 y z)))
                 (acl2-numberp y)
                 (integerp z)
                 (<= 0 z))
            (or (< (id x) (id2 y z))
                (< (id2 y z) (id x))))
   :hints (("[1]Goal" :in-theory (enable (:type-prescription id2))))
   :rule-classes nil)

 (set-guard-checking :none)

 (let ((x "foo")
       (y 0)
       (z 0))
   (implies (and (not (equal (id x) (id2 y z)))
                 (acl2-numberp y)
                 (integerp z)
                 (<= 0 z))
            (or (< (id x) (id2 y z))
                (< (id2 y z) (id x)))))

 (thm
  nil
  :hints (("Goal" :use (:instance bad (x "foo") (y 0) (z 0)))))
|# ; |

; Added type declaration in ts-subsetp (but seemed not to make a measurable
; difference in time, at least for fast GCL build).

; Here is evidence for the bug in symbol-package-name-pkg-witness-name:
#|
 (defthm contradiction
   nil
   :hints (("Goal"
            :use ((:instance symbol-package-name-pkg-witness-name
                             (pkg-name ""))
                  (:instance intern-in-package-of-symbol-symbol-name
                             (x (pkg-witness ""))
                             (y 3)))
            :in-theory (disable (pkg-witness)
                                intern-in-package-of-symbol-symbol-name)))
   :rule-classes nil)
|# ; |

; Implementation note: for reset-prehistory, the key idea is to manipulate
; world global 'command-number-baseline-info.

; Set ld-skip-proofsp to 'include-book during loading of compiled file by
; include-book.

; The "what could be considered a soundness hole" could be exploited as
; follows.

;     (in-package "ACL2")
;     
;     ; Portcullis commands:
;     #|
;     (set-ld-redefinition-action '(:warn! . :overwrite) state)
;     
;     (encapsulate
;      ()
;      (defun foo () t)
;      (local (defun foo () nil))
;      (defthm foo-prop
;        (equal (foo) nil)
;        :rule-classes nil))
;     |# ; |
;     
;     ; NOTE: After the above commands:
;     ; ACL2 !>(redefined-names state)
;     ; NIL
;     ; ACL2 !>
;     
;     ; Now execute:
;     
;     ; (certify-book "bad" 1)
;     
;     (defthm contradiction
;       nil
;       :hints (("Goal" :use foo-prop))
;       :rule-classes nil)

; After certifying the book we can do this:

; (include-book "bad")
; (thm nil :hints (("Goal" :use contradiction)))

; The documentation can now be built on Linux and probably Mac OS (seems that
; Linux texinfo issues have been solved).

; Made a correction in :doc guard-evaluation-table for built-in :program mode
; functions.

; Improved error message when compound event (including certify-book and
; include-book) has sub-event that is not an embedded event form.  If the
; sub-event is a macro call, then then "Note" at the end will give the original
; form as the macro call, not as the compound event.

; Improved max-output-type-for-declare-form-lst to add missing arguments in
; error message, as suggested by Robert.  Also improved
; max-output-type-for-declare-form to check for true lists before calling
; max-output-type-for-declare-form-lst.

; Improved translate error message from mutual-recursion so that the ctx
; identifies the problem function.  Thanks to Robert Krug for pointing out the
; value of making such an identification.

; Expanded *initial-global-table* to include all built-in state globals, so as
; do allow a more inclusive value of *protected-state-globals-for-make-event*.
; Thanks to Peter Dillinger for pointing out that
; *protected-state-globals-for-make-event* was incomplete.

; In the course of implementing ttags for include-book-fn, we noticed that we
; were accessing a world with global-val (in particular on property
; 'boot-strap-flg) after calling chk-certificate-file, which calls
; chk-raise-portcullis, which can extend the world.  We fixed this to call
; global-val on an installed world instead.  It seems possiblee that for large
; proof efforts, this change might provide performance improvements.

; Moved Boyer's scary sys-call example from :doc make-event to :doc sys-call,
; since make-event isn't the issue.

; Here are the books changed to accommodate the fix for the soundness bug in
; the redundancy criterion for defun events.  It's possible that a few of these
; changes are no longer necessary now that we ignore the measure for redundancy
; when including books.
;
; workshops/2000/moore-manolios/partial-functions/report.lisp
; rtl/rel1/lib1/float.lisp
; rtl/rel1/lib1/round.lisp
; rtl/rel1/lib3/float.lisp
; rtl/rel1/lib3/round.lisp
; rtl/rel2/lib/bits.lisp
; rtl/rel2/lib/float.lisp
; rtl/rel3/lib/bits.lisp
; rtl/rel3/lib/float.lisp
;
; Problem with rtl/rel3/lib/top.lisp: can't have :? before real measure..
; Solution: Modify the following to use :? in expo:
; rtl/rel3/support/bvecp-lemmas.lisp
; rtl/rel4/support/bvecp-lemmas.lisp
; (Might not be necessary with the skip-proofs change, but it's a nice change
; anyhow.)
; Then also needed (even after skip-proofs mod) to make such a change in:
; rtl/rel4/lib/bvecp-raw-helpers.lisp
;
; rtl/rel4/lib/bits.lisp
; rtl/rel4/lib/float.lisp
; rtl/rel4/lib/fadd.lisp
; rtl/rel5/lib/log.lisp
; rtl/rel5/lib/float.lisp
; rtl/rel5/lib/add.lisp
; finite-set-theory/osets-0.9/sets.lisp
; finite-set-theory/osets/sets.lisp
; workshops/2004/davis/support/sets.lisp
; workshops/2000/russinoff-short/summary.lisp

; Improved the error message for a bad bind-free alist result, based on an
; example sent by Serita Nelesen to the acl2-help mailing list.

; Directory books/defexec/chapter3 has been renamed books/defexec/other-apps/
; because the records stuff is part of Section 4 of the corresponding paper,
; not Section 3.

  :doc
  ":Doc-Section release-notes

  ACL2 Version  3.0.2 (December, 2006) Notes~/

  NOTE!  New users can ignore these release notes, because the documentation
  has been updated to reflect all changes that are recorded here.

  Fixed soundness bugs in the handling of primitive function ~ilc[pkg-witness],
  and improved its documentation.  (The executable counterpart returned an
  incorrect default value, and the axiom
  ~c[symbol-package-name-pkg-witness-name] needed ~c[pkg-name] to be other than
  ~c[\"\"] in order to avoid the default value of \"ACL2\".)  As fallout, a new
  built-in ~c[:]~ilc[forward-chaining] rule,
  ~c[symbol-package-name-of-symbol-is-not-empty-string], now asserts that the
  ~ilc[symbol-package-name] of a symbol is never ~c[\"\"].  Thanks to Mike
  Gordon for bringing these soundness bugs to our attention by attempting to
  prove translations of ACL2 axioms in HOL4.

  Fixed a soundness bug in linear arithmetic, due to incomplete tracking of
  forced assumptions while deriving inequalities.  Thanks to Robert Krug for
  providing a fix and a proof of ~c[nil] before the fix.

  Fixed a soundness bug in the redundancy criterion for ~ilc[defun] events,
  which has been modified; ~pl[redundant-events].  This bug is illustrated
  below.  Thanks to Peter Dillinger and Jared Davis for contributions to an
  email thread that led us to discover this bug.  The solution is that for a
  definition to be redundant with an earlier definition, ACL2 no longer ignores
  ~c[:]~ilc[measure] ~ilc[xargs] except when skipping proofs (e.g., during
  ~ilc[include-book]).  However, a new ``measure'', ~c[(:? v1 ... vk)], is
  supported, for specifying a measured subset of the set of formals, i.e., a
  set of formals that serves as the set of parameters for some valid measure.
  ~bv[]
  (encapsulate
   ()

   (local (defun foo (x y)
            (declare (xargs :measure (acl2-count y)))
            (if (and (consp x) (consp y))
                (foo (cons x x) (cdr y))
              y)))

  ; So the following is redundant -- but it guesses a measure
  ; of (acl2-count x), which isn't right!
   (defun foo (x y)
     (if (and (consp x) (consp y))
         (foo (cons x x) (cdr y))
       y)))

  ; end of encapsulate

  ; Now we prove a non-theorem by exploiting the bug above,
  ; erroneously replacing formal y by a constant in the induction
  ; scheme hinted below.  (This should not be allowed, as y should be
  ; labeled as a measured formal.)

  (defthm bad
    (atom x)
    :rule-classes nil
    :hints ((\"Goal\" :induct (foo x '(3)))))

  ; It's easy to get a contradiction by instantiating the
  ; non-theorem just above.
  (defthm contradiction
    nil
    :rule-classes nil
    :hints ((\"Goal\" :use ((:instance bad (x '(7)))))))
  ~ev[]

  Fixed a bug in ~c[:]~ilc[pl] and the ~il[proof-checker]'s ~c[show-rewrites]
  (~c[sr]) command that was causing a Lisp break.  For ~c[:]~ilc[pl], also
  improved the display of unifying substitutions, modified output to take
  binding hypotheses ~c[(equal var term)] into account properly, and arranged
  for inclusion of ~il[meta] rules that modify the given term.  Thanks to Eric
  Smith for bringing these issues to our attention.

  Introduced new utilities for undoing ~il[command]s, ~c[:]~ilc[ubu] and
  ~c[:]~ilc[ubu!], which are analogous to ~c[:]~ilc[ubt] and ~c[:]~ilc[ubt!]
  (respectively) except that they only undo back up to, but not including, the
  indicated command.

  Fixed a performance bug, pointed out by Eric Smith, that was negating efforts
  made for the preceding release to avoid computation for disabled warnings.

  Added ~ilc[time$] and ~c[value-triple] to ~c[*acl2-exports*].  Thanks to Bob
  Boyer and Erik Reeber (respectively) for bringing these issues to our
  attention.

  Improved the automatic proclaiming of function types for GCL and OpenMCL,
  specifically to use an output format consistent with the Common Lisp spec.
  Thanks to Bob Boyer for bringing this issue to our attention.

  Added ~c[books/misc/transfinite.lisp], which deals with transfinite induction
  in ACL2.  Thanks to Eric Smith for contributing this book.

  Added ~c[books/misc/process-book-readme.lisp] to the distribution.  Thanks to
  Sandip Ray for pointing out its omission.

  Added contributions ~c[books/concurrent-programs/bakery/] and
  ~c[books/concurrent-programs/german-protocol/].  These contributions can be
  used as tutorials, especially by new ACL2 users, for learning how to model
  concurrent protocols in ACL2 and the steps involved in reasoning about their
  correctness.  Thanks to Sandip Ray for these contributions.  See the
  ~c[Readme.lsp] files in these directories.

  Theory invariants may now involve the variable ~c[ENS] instead of the
  variable ~c[THEORY].  The practical effect of this change is that any
  expression of the form ~c[(MEMBER-EQUAL rune THEORY)] occurring in a
  ~ilc[theory-invariant] expression should be replaced by
  ~c[(ACTIVE-RUNEP rune)].  ~l[theory-invariant].  Thanks to Eric Smith and
  Dave Greve for pointing out an inefficiency in the handling of theory
  invariants that led to this change, which can speed up their handling by
  orders of magnitude on large examples, and to Eric for testing this change
  and pointing out problems with an early implementation of it.

  Theory invariants (~pl[theory-invariant]) are no longer checked on theories
  defined by ~ilc[deftheory] ~il[events].  After all, one can define a theory
  with ~c[deftheory] that is not intended to be used as the current theory, but
  rather is intended to be combined with other ~il[theories]
  (~pl[theory-functions]).  Thanks to Eric Smith for bringing this issue to our
  attention.

  ~ilc[Theory-invariant] errors had been reported with very little detail when
  warnings were inhibited.  This problem has been fixed; thanks to Eric Smith
  for bringing it to our attention and providing an example.  We have also
  improved the handling of redundancy for ~ilc[theory-invariant] ~il[events].

  The macro ~ilc[defun-sk] now has a new optional keyword, ~c[rewrite], that
  can be used to change the form of the ~c[:]~ilc[rewrite] rule generated when
  the quantifier is ~ilc[forall].  Thanks to Eric Smith and Sandip Ray for
  useful discussions on this topic.  We have also slightly modified the
  ~il[hints] for the ~ilc[defthm] event underneath a ~c[defun-sk] in order to
  make the proof more reliably efficient.

  A new event, ~ilc[reset-prehistory], allows setting of a barrier before which
  undoing is illegal.  An argument to this macro allows the barrier to be made
  permanent; otherwise, it can be removed with ~c[:]~ilc[ubt-prehistory].
  Thanks to Peter Dillinger for useful conversations leading to the addition of
  ~ilc[reset-prehistory].

  A new query, ~c[(]~ilc[wormhole-p]~c[ ]~ilc[state]~c[)], allows users to
  determine whether or not they are in a ~ilc[wormhole].  Thanks to Peter
  Dillinger for providing this utility.

  ~c[Value-triple] no longer evaluates its form during ~ilc[include-book], and
  in raw Lisp its calls trivially macroexpand to ~c[nil], without any
  consideration of its argument.  This change avoids errors and warnings when
  ~il[stobj] names occur in the argument.

  We fixed what could be considered a soundness hole that could occur by
  exploiting redefinition in a particular way.  Thanks to Peter Dillinger for
  raising a question that led to discovery of this hole.

  A bug has been fixed in handling of illegal ~il[theory] expressions.  Thanks
  to Eric Smith, who reported this problem and provided the example
  ~c[(in-theory '((:definition natp) (:rewrite doesntexist)))]
  to show how a hard error could occur.

  Improved error reporting by ~ilc[certify-book] when the certification
  ~il[world] contains inadmissible forms.

  Modified ~ilc[defchoose] to add two new keyword arguments.  There is now a
  ~c[:doc] keyword argument; previously, an optional documentation string
  (~pl[doc-string]) was to be placed just before the body, without a keyword.
  There is also a ~c[:strengthen] argument that strengthens the axiom added,
  which allows for the definition of ``fixing'' functions for equivalence
  relations that choose canonical representatives of equivalence classes.
  ~l[defchoose].  Thanks for Dave Greve for useful discussions that led us to
  this ~c[:strengthen] enhancement.

  Added ~c[books/misc/bash.lisp], which provides utilities for simplifying a
  goal into a list of subgoals (as documented at the top of that file).  Thanks
  to Dave Greve for requesting this utility and suggesting refinements to its
  functionality, which have been incorporated.

  (For Emacs users only) The command ~c[meta-x new-shell] provided by file
  ~c[emacs/emacs-acl2.el] now puts you in shell-mode, which for example
  supports directory tracking.  Thanks to Jared Davis for suggesting this
  change.

  Fixed some mishandling of ~il[stobj]s by ~ilc[make-event] expansion.

  Introduced a new event, ~ilc[defttag], that introduces a ``trust tag''
  (``ttag'') allowing for extensions of ACL2 and for the use of generally
  unsafe ACL2 constructs.  Thanks to Peter Dillinger, Sandip Ray, and Erik
  Reeber for useful discussions on ~c[defttag] and the following related
  items.
  ~bq[]

  A new event, ~ilc[remove-untouchable], can be used to give users access to
  system functions and data structures.  We also fixed a bug in
  ~ilc[push-untouchable]; and, it no longer is a no-op in ~c[:]~ilc[program]
  mode.  Thanks to Peter Dillinger for proposing ~ilc[remove-untouchable] and
  suggesting that it and ~ilc[push-untouchable] be functional in
  ~c[:]~ilc[program] mode.

  Raw-mode (~pl[set-raw-mode]) no longer disables ~ilc[certify-book].  However,
  ~ilc[set-raw-mode] is now disallowed unless there is an active ttag
  (~pl[defttag]).  If you want to execute ~c[(]~ilc[set-raw-mode]~c[ t)] and
  there is no active ttag, consider executing ~c[(]~ilc[set-raw-mode-on!]~c[)]
  instead.

  Redefinition of system functions is disallowed unless there is an active
  ttag.  However, ~ilc[redef!] now introduces ~c[(defttag :redef!)] in order to
  allow redefinition of system functions.

  A new event, ~ilc[progn!], is a legal embedded event form that can go in
  ~il[books] and both ~ilc[encapsulate] and ~ilc[progn] forms
  (~pl[embedded-event-form]), and is similar to ~ilc[progn] except that it
  allows arbitrary forms.  Thus, a ~ilc[progn!] form is potentially dangerous
  and can only be evaluated if there is an active ttag.

  ~l[ttags-seen] for information about how to find the ttags known in the
  current ACL2 ~il[world], and for related caveats.

  A new book created with Peter Dillinger, ~c[books/misc/hacker.lisp], uses
  ~ilc[progn!] to define utiliities ~c[with-raw-mode] and
  ~c[with-redef-allowed], which respectively allow raw Lisp evaluation and
  redefinition to take place within a certifiable book (!).~eq[]

  Macro ~ilc[with-output] is no longer allowed in function bodies because it
  does not have (and has never had) any effect in raw Lisp.  ~l[with-output]
  for a workaround.

  Fixed a bug in redundancy of ~ilc[defstobj] in raw Lisp, which caused an
  error when certifying a book with a redundant ~ilc[defstobj] event whose
  ~ilc[stobj] had already been modified.  Here is an example:
  ~bv[]
  (defstobj st fld)
  (update-fld 3 st)
  (certify-book \"foo\" 1) ; where foo.lisp contains (defstobj st fld)
  ~ev[]

  New books illustrating ~ilc[make-event] have been contributed in directory
  ~c[books/make-event/]: ~c[dotimes.lisp] (David Rager), ~c[stobj-test.lisp],
  and ~c[logical-tangent.lisp] (Peter Dillinger).

  Modified ~c[print-object$] (~pl[io]) so that it no longer prints an extra
  space at the end.

  Replaced the ``draconian restriction to avoid capture'' that had prevented
  some ~c[:functional-instance] ~il[hints] from being legal.  The corresponding
  check now only requires that no variable free in the functional substitution
  is captured by a ~ilc[let] or ~ilc[mv-let] (or ~ilc[lambda]) binding.
  ~l[lemma-instance].

  Added new extended metafunction, ~c[mfc-rw+], which is equivalent to
  ~c[mfc-rw] except that it takes an alist argument, which may be useful for
  efficiency.  ~l[extended-metafunctions].  Thanks to Robert Krug for
  suggesting this more efficient variant of ~c[mfc-rw].

  Added support for the ~c[ignorable] ~ilc[declare] form.

  We now cause an error on a call of ~c[open-input-channel] (~pl[io]) with an
  argument string whose first character is the ~c[|] character.  Thanks to Bob
  Boyer for providing an example (several months ago) showing the danger of
  such calls, namely that the following command would log you out and kill all
  of your processes when running on top of GCL in Linux:~nl[]
  ~c[(open-input-channel \"|kill -9 -1\" :object state)]

  Restricted the use of ~ilc[make-event] to contexts in which it can be tracked
  properly, under legal ~il[events] (~pl[embedded-event-form]).  Thanks to
  Peter Dillinger for bringing an example to our attention that led to this
  fix.

  Fixed a bug that was avoiding ~il[guard]-checking for the functions
  ~ilc[compress1] and ~ilc[compress2].  Thanks to David Rager for bringing this
  bug to our attention.

  Added an error message when a ~ilc[defun] or ~ilc[mutual-recursion] event
  fails, to clarify whether failure is for the ~il[measure] conjecture or for
  the ~il[guard] conjecture.  Thanks to David Rager for requesting
  clarification for such failures.

  Fixed a bug in reporting of ~il[guard] violations (hard Lisp error) when certain
  macros (for example, ~ilc[cond]) are used in the ~il[guard].  Thanks to Jared
  Davis for bringing this problem to our attention and providing assistance
  with the solution, in particular by providing a helpful example.

  Grant Passmore has contributed a resolution/paramodulation prover written in
  ACL2, in directory ~c[books/deduction/passmore/].  Thanks, Grant.

  Improved the error message when illegal theories are encountered.

  Improved the suppression of output for ~c[inhibit-output] arguments of
  routines in the book ~c[books/misc/expander.lisp].  Thanks to Qiang Zhang for
  pointing out the possibility for improvement here.

  Added a new directory ~c[books/arithmetic-3/extra/] that extends
  ~c[books/arithmetic-3] with additional rules, contributed by Alex
  Spiridonov with guidance from Robert Krug.  WARNING: This directory is under
  development.  It may undergo large changes in future releases, so please
  consider it experimental and subject to change.  Feedback is welcomed.

  As part of the work mentioned just above, Robert Krug and Alex Spiridonov
  contributed improvements to ~c[books/arithmetic-3/]:
  ~bq[]

  o A new rule ~c[|(* (/ x) (/ (expt x n)))|] in ~c[bind-free/collect.lisp],
  which is important for reducing ~c[collect-*] expressions though it slowed
  down one proof (see comment above this rule in ~c[bind-free/collect.lisp]).

  o Slight improvements of rules ~c[integerp-mod] and ~c[rationalp-mod] in
  ~c[floor-mod/floor-mod.lisp].

  o To avoid conflict with ~c[books/rtl/rel6/arithmetic/], renamed rule
  ~c[mod-minus] to ~c[mod-neg] in ~c[floor-mod/floor-mod.lisp], and renamed
  ~c[integerp-+-reduce-leading-constant] to
  ~c[integerp-+-reduce-leading-rational-constant] in
  ~c[bind-free/integerp.lisp].~eq[]

  (GCL on Windows only) Made a low-level change to avoid multiplying stacks for
  GCL on Windows, since GCL 2.6.6 broke while doing this.

  Fixed bugs in linear arithmetic (rarely evidenced, it seems) involving using
  ~c[<] to compare complex rational constants.  Thanks to Robert Krug for
  helping with the fixes.

  Added a new event, ~ilc[assert-event], for checking that forms evaluate to
  non-~c[nil] values.  Thanks to Peter Dillinger for suggesting and
  collaborating on this addition.

  ~/~/")

(deflabel note-3-1

  :doc
  ":Doc-Section release-notes

  ACL2 Version  3.1 (December, 2006) Notes~/

  NOTE!  New users can ignore these release notes, because the documentation
  has been updated to reflect all changes that are recorded here.

  Please ~pl[note-3-0-2] for a description of changes since Version  3.0.1, and
  also ~pl[note-3-0-1] for additional changes since Version  3.0.

  ~/~/")

(deflabel |NOTE-3-1(R)|
  :doc
  ":Doc-Section release-notes

  ACL2 Version  3.1(r) (December, 2006) Notes~/

  No significant changes have been made since Version  3.0 for support of
  non-standard analysis in particular.

  ~/

  Please also ~pl[note-3-1] for changes to Version  3.1 of ACL2.

  ~/
  ")

(deflabel the-method
  :doc
  ":Doc-Section Miscellaneous

  how to find proofs~/

  Many users develop proof scripts in an Emacs buffer and submit one
  event at a time to the theorem prover running in a ~c[*shell*] buffer.
  The script buffer is logically divided into two regions: the events
  that have been accepted by the theorem prover and those that have
  not yet been accepted.  An imaginary ``barrier'' divides these two
  regions.  The region above the barrier describes the state of the
  ~c[*shell*] buffer (and ACL2's logical world).  The region below the
  barrier is the ``to do'' list.

  We usually start a proof project by typing the key lemmas, and main
  goal into the to do list.  Definitions are here just regarded as
  theorems to prove (i.e., the measure conjectures).  Then we follow
  ``The Method.''

  (1) Think about the proof of the first theorem in the to do list.
  Structure the proof either as an induction followed by
  simplification or just simplification.  Have the necessary lemmas
  been proved? That is, are the necessary lemmas in the done list
  already?  If so, proceed to Step 2.  Otherwise, add the necessary
  lemmas at the front of the to do list and repeat Step 1.

  (2) Call the theorem prover on the first theorem in the to do list
  and let the output stream into the *shell* buffer.  Abort the proof
  if it runs more than a few seconds.

  (3) If the theorem prover succeeded, advance the barrier past the
  successful command and go to Step 1.

  (4) Otherwise, inspect the failed proof attempt, starting from the
  beginning, not the end.  Basically you should look for the first
  place the proof attempt deviates from your imagined proof.  If your
  imagined proof was inductive, inspect the induction scheme used by
  ACL2.  If that is ok, then find the first subsequent subgoal that is
  stable under simplification and think about why it was not proved by
  the simplifier.  If your imagined proof was not inductive, then
  think about the first subgoal stable under simplification, as above.
  Modify the script appropriately.  It usually means adding lemmas to
  the to do list, just in front of the theorem just tried.  It could
  mean adding hints to the current theorem.  In any case, after the
  modifications go to Step 1.~/

  We do not seriously suggest that this or any rotely applied
  algorithm will let you drive ACL2 to difficult proofs.  Indeed, to
  remind you of this we call this ``The Method'' rather than ``the
  method.''  That is, we are aware of the somewhat pretentious nature
  of any such advice.  But these remarks have helped many users
  approach ACL2 in a constructive and disciplined way.

  We say much more about The Method in the ACL2 book.  See the
  home page.

  Learning to read failed proofs is a useful skill.  There are several
  kinds of ``checkpoints'' in a proof: (1) a formula to which induction
  is being (or would be) applied, (2) the first formula stable under
  simplification, (3) a formula that is possibly generalized, either
  by cross-fertilizing with and throwing away an equivalence hypothesis
  or by explicit generalization of a term with a new variable.  

  At the induction checkpoint, confirm that you believe the formula
  being proved is a theorem and that it is appropriately strong for an
  inductive proof.  Read the selected induction scheme and make sure it
  agrees with your idea of how the proof should go.

  At the post-simplification checkpoint, which is probably the most
  commonly seen, consider whether there are additional rewrite rules
  you could prove to make the formula simplify still further.  Look
  for compositions of function symbols you could rewrite.  Look for
  contradictions among hypotheses and prove the appropriate
  implications: for example, the checkpoint might contain the two
  hypotheses ~c[(P (F A))] and ~c[(NOT (Q (G (F A))))] and you might
  realize that ~c[(implies (p x) (q (g x)))] is a theorem.  Look for
  signs that your existing rules did not apply, e.g., for terms that
  should have been rewritten, and figure out why they were not.
  Possible causes include that they do not exactly match your old
  rules, that your old rules have hypotheses that cannot be relieved
  here -- perhaps because some other rules are missing, or perhaps
  your old rules are disabled.  If you cannot find any further
  simplifications to make in the formula, ask yourself whether it is
  valid.  If so, sketch a proof.  Perhaps the proof is by appeal to a
  combination of lemmas you should now prove?  

  At the two generalization checkpoints --- where hypotheses are
  discarded or terms are replaced by variables --- ask yourself whether
  the result is a theorem.  It often is not.  Think about rewrite rules
  that would prove the formula.  These are often restricted versions of the
  overly-general formulas created by the system's heuristics.

  ~l[proof-tree] for a discussion of a tool to help you navigate through
  ACL2 proofs.")

(deflabel lp
  :doc
  ":Doc-Section Miscellaneous

  the Common Lisp entry to ACL2~/

  To enter the ACL2 ~il[command] loop from Common Lisp, call the Common
  Lisp program ~c[lp] (which stands for ``loop,'' as in ``read-eval-print
  loop'' or ``~il[command] loop.'')  The ACL2 ~il[command] loop is actually
  coded in ACL2 as the function ~ilc[ld] (which stands for ``load'').  The
  ~il[command] loop is just what you get by loading from the standard
  object input channel, ~ilc[*standard-oi*].  Calling ~ilc[ld] directly from
  Common Lisp is possible but fragile because hard lisp errors or
  aborts throw you out of ~ilc[ld] and back to the top-level of Common Lisp.
  ~c[Lp] calls ~ilc[ld] in such a way as to prevent this and is thus the
  standard way to get into the ACL2 ~il[command] loop.  Also
  ~pl[acl2-customization] for information on the loading of an
  initialization file.~/

  All of the visible functionality of ~c[lp] is in fact provided by ~ilc[ld],
  which is written in ACL2 itself.  Therefore, you should ~pl[ld]
  for detailed ~il[documentation] of the ACL2 ~il[command] loop.  We sketch it
  below, for novice users.

  Every expression typed to the ACL2 top-level must be an ACL2
  expression.

  Any ACL2 expression may be submitted for evaluation.  Well-formedness is checked.
  Some well-formed expressions cannot be evaluated because they involve (at some level)
  undefined constrained functions (~pl[encapsulate]).  In addition, ACL2 does not
  allow ``global variables'' in expressions to be evaluated.  Thus, ~c[(car '(a b c))]
  is legal and evaluates to ~c[A], but ~c[(car x)] is not, because there is no
  ``global context'' or binding environment that gives meaning to the variable symbol
  ~c[x].

  There is an exception to the global variable rule outlined above:
  single-threaded objects (~pl[stobj]) may be used as global variables
  in top-level expressions.  The most commonly used such object is the
  ACL2 ``current state,'' which is the value of the variable symbol
  ~ilc[state].  This variable may occur in the top-level form to be
  evaluated, but must be passed only to ACL2 functions ``expecting'' ~c[state]
  as described in the documentation for ~ilc[state] and for ~ilc[stobj]s in general.
  If the form returns a new ~il[state] object as one of its
  values, then that is considered the new ``current'' ~il[state] for
  the evaluation of the subsequent form.  ~l[state].  

  ACL2 provides some support for the functionality usually provided by
  global variables in a read-eval-print loop, namely the saving of the
  result of a computation for subsequent re-use in another expression.
  ~l[assign] and ~pl[@].

  If the form read is a single keyword, e.g., ~c[:]~ilc[pe] or ~c[:]~ilc[ubt], then
  special procedures are followed.  ~l[keyword-commands].

  The ~il[command] loop keeps track of the ~il[command]s you have typed and
  allows you to review them, display them, and roll the logical ~il[state]
  back to that created by any ~il[command].  ~l[history].

  ACL2 makes the convention that if a top-level form returns three
  values, the last of which is an ACL2 ~il[state], then the first is
  treated as a flag that means ``an error occurred,'' the second is
  the value to be printed if no error occurred, and the third is (of
  course) the new ~il[state].  When ``an error occurs'' no value is
  printed.  Thus, if you execute a top-level form that happens to
  return three such values, only the second will be printed (and that
  will only happen if the first is ~c[nil]!).  ~l[ld] for details.")

(defun cltl-def-from-name (fn stobj-function wrld)

; If fn does not have a 'stobj-function property in wrld, then this function
; returns the raw Lisp definition of fn, which is also the definition that is
; oneified to create the corresponding *1* function.

; But suppose that fn does have a 'stobj-function property in wrld.  If
; stobj-function is non-nil, it should be that property, and we return the raw
; Lisp definition of fn.  If stobj-function is, then we return the definition
; of fn that is to be oneified.

  (let* ((event-number
          (getprop (or stobj-function fn) 'absolute-event-number nil
                   'current-acl2-world wrld))
         (cltl-command-value
          (and event-number
               (getprop 'cltl-command
                        'global-value
                        nil
                        'any-old-world
                        (lookup-world-index
                         'event event-number
                         wrld))))
         (def (and cltl-command-value
                   (assoc-eq fn
                             (if stobj-function
                                 (nth 4 cltl-command-value)
                               (cdddr cltl-command-value))))))
    (and def
         (or (null stobj-function)
             (not (member-equal *stobj-inline-declare* def)))
         (cons 'defun def))))

#-acl2-loop-only
(defun-one-output compiled-function-p! (fn)

; In CMU Lisp, compiled-function-p is braindead.  It seems that the
; symbol-function of every defun'd function is a ``compiled'' object.
; Some are #<Interpreted Function ...> and others are #<Function ...>.
; I think the following test works.  Fn is assumed to be a symbol.

  #+cmu
  (not (eq (type-of (symbol-function fn)) 'eval:interpreted-function))
  #-cmu
  (compiled-function-p (symbol-function fn)))

(defun compile-function (ctx fn0 state)

; Fn0 can either be a symbol, (:raw sym), or (:exec sym).

  (declare (xargs :guard
                  (and (or (symbolp fn0)
                           (and (consp fn0)
                                (member-eq (car fn0) '(:raw :exec))
                                (consp (cdr fn0))
                                (null (cddr fn0))))
                       (state-p state))))
  (let ((wrld (w state))
        (fn (if (consp fn0)
                (cadr fn0)
              fn0)))
    (cond
     ((eq (getprop fn 'formals t 'current-acl2-world wrld)
          t)
      (er soft ctx
          "~x0 is not a defined function in the current ACL2 world."
          fn))
     (t
      #-acl2-loop-only
      (let* ((stobj-function (getprop fn 'stobj-function nil
                                      'current-acl2-world wrld))
             (form (cltl-def-from-name fn stobj-function wrld))
             (*1*fn (*1*-symbol fn))
             (raw-only-p  (and (consp fn0) (eq (car fn0) :raw)))
             (exec-only-p (and (consp fn0) (eq (car fn0) :exec))))
        (cond
         ((not (or exec-only-p
                   (compiled-function-p! fn)))
          (cond (form
                 (eval (make-defun-declare-form fn form))))
          (compile fn)))
        (cond
         ((and (not raw-only-p)
               (fboundp *1*fn)
               (not (compiled-function-p! *1*fn)))
          #-acl2-mv-as-values ; may delete this restriction in the future
          (eval
           (make-defun-declare-form
            fn
            (cons 'defun (oneify-cltl-code
                          (if (eq (symbol-class fn wrld) :program)
                              :program

; What happens here if the function is non-executable?  We believe that
; functions defined by encapsulate will always have mode :logic, since LOCAL
; forms are skipped in :program mode.  In fact, we believe that this function
; would be :common-lisp-compliant, and hence its oneified code will simply be a
; call of the raw lisp function, which will do a throw.  But the bottom line is
; that the function body contains a throw-raw-ev-fncall, so regardless of how
; we oneify the code, we will get a function that ultimately does a throw.

                            :logic)
                          (if stobj-function
                              (cltl-def-from-name fn nil wrld)
                            (cdr form))
                          stobj-function
                          wrld))))
          (compile *1*fn))))
      (value fn)))))

#-acl2-loop-only
(defun getpid$ ()

; This function is intended to return the process id.  But it may return nil
; instead, depending on the underlying lisp platform.

  (let ((fn
         #+allegro 'excl::getpid
         #+gcl 'si::getpid
         #+sbcl 'sb-unix::unix-getpid
         #+cmu 'unix::unix-getpid
         #+clisp (or (let ((fn0 (find-symbol "PROCESS-ID" "SYSTEM")))
                       (and (fboundp fn0) ; CLISP 2.34
                            fn0))
                     (let ((fn0 (find-symbol "PROGRAM-ID" "SYSTEM")))
                       (and (fboundp fn0) ; before CLISP 2.34
                            fn0)))
         #+openmcl 'ccl::getpid
         #+lispworks 'system::getpid
         #-(or allegro gcl sbcl cmu clisp openmcl lispworks) nil))
    (and fn
         (fboundp fn)
         (funcall fn))))

#-acl2-loop-only
(defun-one-output tmp-filename (dir suffix)

; Warning: If this function is changed, look at its call in save-gprof.lsp.

  (let ((pid (and (not (eq (f-get-global 'keep-tmp-files *the-live-state*)
                           :no-pid))
                  (getpid$))))
    (coerce (packn1 (list* dir
                           "TMP"
                           (if pid
                               (if suffix
                                   (list "@" pid "@" suffix)
                                 (list "@" pid "@"))
                             (if suffix
                                 (list suffix)
                               nil))))
            'string)))

(defun keep-tmp-files (state)
  (f-get-global 'keep-tmp-files state))

(defun comp-fn (fns gcl-flg tmp-suffix state)

; Gcl-flg should only be used with GCL, and causes .c and .h files to be left
; around after compilation.

  (declare (xargs :guard (and (state-p state)
                              (or (and (true-listp fns) fns)
                                  (symbolp fns))
                              (stringp tmp-suffix)
                              (not (equal tmp-suffix ""))))
           #+acl2-loop-only
           (ignore tmp-suffix))
  (cond
   #-gcl
   (gcl-flg
    (er soft 'comp
        "Comp-gcl may only be used in GCL implementations."))
   (t
    (let ((fns (cond
                ((or (and (symbolp fns)
                          (not (eq fns t))
                          (not (eq fns :raw))
                          (not (eq fns :exec))
                          (not (eq fns nil)))
                     (and (consp fns)
                          (member-eq (car fns) '(:raw :exec))
                          (consp (cdr fns))
                          (null (cddr fns))))
                 (list fns))
                (t fns))))
      (cond
       ((and (consp fns)
             (null (cdr fns))
             (not gcl-flg))
        (compile-function 'comp (car fns) state))
       ((null fns)
        (er soft 'comp
            "We do not allow the notion of compiling the empty list of ~
             functions.  Perhaps you meant to do something else."))
       (t
        #-acl2-loop-only
        (let ((*package* *package*)
              (dir (or (f-get-global 'connected-book-directory state)
                       ""))
              (raw-fns nil)
              (exec-fns nil))
          (cond
           ((consp fns)
            (dolist (fn fns)
              (cond
               ((and (consp fn)
                     (member-eq (car fn) '(:raw :exec)))
                (cond ((and (consp (cdr fn))
                            (null (cddr fn))
                            (symbolp (cadr fn)))
                       (cond ((eq (car fn) :raw)
                              (setq raw-fns (cons (cadr fn) raw-fns)))
                             (t ; :exec
                              (setq exec-fns (cons (cadr fn) exec-fns)))))
                      (t
                       (er hard 'comp
                           "Unexpected function specifier, ~x0."
                           fn))))
               ((symbolp fn)
                (setq raw-fns (cons fn raw-fns))
                (setq exec-fns (cons fn exec-fns)))
               (t (er hard 'comp
                      "Unexpected function specifier, ~x0."
                      fn)))
              (setq raw-fns (nreverse raw-fns))
              (setq exec-fns (nreverse exec-fns))))
           (t (setq raw-fns fns)
              (setq exec-fns fns)))
          (when (not (eq fns :exec))
            (let ((tmpfile (tmp-filename dir nil)))
              (compile-uncompiled-defuns
               tmpfile
               (if (or (eq fns t)
                       (eq fns :raw))
                   :some
                 raw-fns)
               gcl-flg)))
          (or (eq fns :raw)
              (let ((tmpfile (tmp-filename dir tmp-suffix)))
                (compile-uncompiled-*1*-defuns
                 tmpfile
                 (if (member-eq fns '(t :exec))
                     :some
                   exec-fns)
                 gcl-flg))))
        (value t)))))))

#-acl2-loop-only
(defmacro comp (fns)
  (declare (ignore fns))
  nil)

#+acl2-loop-only
(defmacro comp (fns)

  ":Doc-Section Events

  compile some ACL2 functions~/
  ~bv[]
  Examples:
  :comp t          ; compile all uncompiled ACL2 functions
  (comp t)         ; same as above, but can be put into a book
  (comp :exec)     ; compile all uncompiled logic (``*1*'') definitions
  :comp foo        ; compile the defined function foo
  :comp (:raw foo) ; compile the raw Lisp version of the defined function foo
                     but not the corresponding logic definition
  :comp (foo bar)  ; compile the defined functions foo and bar
  :comp (foo (:raw bar))  ; compile the defined functions foo and bar, but for
                          ; bar do not compile the corresponding logic definition

  General Form:
  :comp specifier
  where specifier is one of the following:

    t                     compile all user-defined ACL2 functions that are
                            currently uncompiled (redefined built-in functions
                            are not recompiled)
    :exec                 same as t, except that only logic versions are
                            compiled (see below), not raw Lisp definitions
    :raw                  same as t, except that only raw Lisp definitions are
                            compiled, not logic version (see below)
    (name-1 ... name-k)   a non-empty list of names of functions defined by
                            DEFUN in ACL2, except that each name-i can be of
                            the form (:raw sym) or (:exec sym), where sym is
                          the name of such a function
    name                  same as (name)
  ~ev[]

  When you define a function in ACL2, you are really causing two definitions to
  be made ``under the hood'' in Common Lisp: the definition is submitted
  explicitly to raw Lisp, but so is a corresponding ``logic definition''.  If
  guards have not been verified, then only the logic definition will be
  evaluated; ~pl[guards-and-evaluation], in particular the section titled
  ``Guards and evaluation V: efficiency issues''.

  Thus, if you are not verifying ~il[guard]s and you want the benefit of Lisp
  compilation for speed and space efficiency, then you may want to place the
  form ~c[(comp :exec)] in your ~il[books].

  Generally it is not necessary to place the form ~c[(comp t)], or the form
  ~c[(comp :raw)], in a book, because ~ilc[certify-book] compiles the raw Lisp
  definitions anyhow, by default.  But you may wish to put ~c[(comp t)] or
  ~c[(comp fn1 fn2 ... fnk)] in a book when such a form precedes expensive
  calls of functions, for example for proofs involving calls of functions on
  large constants, or to support computationally expensive macroexpansion.

  As suggested by the examples above, if a function specifier is of the form
  ~c[(:raw fn)], then ~c[fn] will be compiled in raw Common Lisp but its
  corresponding logic definition will not be compiled; and for ~c[(:exec fn)],
  it's the other way around.

  The use of ~c[:comp] creates various files whose names start with
  ``~c[TMP*]'', but then deletes them.  If you want to keep these files around
  for some reason, evaluate ~c[(assign keep-tmp-files t)].~/

  Also ~pl[set-compile-fns] for a way to compile each function as it is
  defined.  But note that ~c[set-compile-fns] is ignored during
  ~ilc[include-book].

  :cited-by Programming"

  `(comp-fn ,fns nil "1" state))

(defmacro comp-gcl (fns)

  ":Doc-Section Comp

  compile some ACL2 functions leaving .c and .h files~/~/

  ~c[Comp-gcl] is for use by experts who want to examine the results of GCL
  compilation, and it may only be used with ACL2 implementations built on top
  of GCL.  It takes exactly the same arguments as ~ilc[comp], and has the same
  basic functionality (~pl[comp]), but has two additional effects.  First,
  files ~c[\"TMP.lisp\"] and ~c[\"TMP1.lisp\"] are always created, even when a
  single function is specified.  Second, ~c[comp-gcl] always leaves files
  ~c[\"TMP.c\"], ~c[\"TMP.h\"], ~c[\"TMP1.c\"], and ~c[\"TMP1.h\"] when
  compilation is complete.~/

  :cited-by Programming"

  `(comp-fn ,fns t "1" state))

(defun scan-past-deeper-event-landmarks (depth wrld)

; We scan down wrld until either it is exhausted or we find a command-landmark
; or we find an event-landmark whose access-event-tuple-depth is depth or less.
; Thus, the world we return is either nil or begins with a command-landmark or
; event-landmark.

  (cond
   ((or (null wrld)
        (and (eq (car (car wrld)) 'command-landmark)
             (eq (cadr (car wrld)) 'global-value)))
    wrld)
   ((and (eq (car (car wrld)) 'event-landmark)
         (eq (cadr (car wrld)) 'global-value))
    (cond
     ((> (access-event-tuple-depth (cddr (car wrld))) depth)
      (scan-past-deeper-event-landmarks depth (cdr wrld)))
     (t wrld)))
   (t (scan-past-deeper-event-landmarks depth (cdr wrld)))))

(defun puffable-command-blockp (wrld cmd-form)

; Initially, wrld should be the cdr of a world starting at some
; command-landmark.  Cmd-form should be the command-tuple form of that
; landmark (note that it will be nil for the very first
; command-landmark ever laid down, the one with
; access-command-tuple-number -1).

; This function returns t if the command block starting at wrld satisfies
; either of the following:

; (a) the first (last executed) event-tuple in the command block has a
;     different form than cmd-form, or
; (b) the first event form is either an encapsulate or an include-book.

; Suppose neither obtains.  Then we return nil.  What does this mean?
; It means the command and event tuples are the same (so no macros
; were invovled in hiding the event) and the event wasn't an an
; encapsulate or include-book.

  (cond
   ((or (null wrld)
        (and (eq (car (car wrld)) 'command-landmark)
             (eq (cadr (car wrld)) 'global-value)))
    nil)
   ((and (eq (car (car wrld)) 'event-landmark)
         (eq (cadr (car wrld)) 'global-value))
    (or (and cmd-form
             (not (equal cmd-form (access-event-tuple-form (cddr (car wrld))))))
        (eq (access-event-tuple-type (cddr (car wrld))) 'encapsulate)
        (eq (access-event-tuple-type (cddr (car wrld))) 'include-book)))
   (t (puffable-command-blockp (cdr wrld) cmd-form))))

(defun puffable-command-numberp (i state)

; Let i be a legal relative command number for (w state).  We determine whether
; the command at i is puffable.

  (mv-let (flg n)
          (normalize-absolute-command-number
           (relative-to-absolute-command-number i (w state))
           (w state))
          (and (null flg)
               (let ((wrld (lookup-world-index 'command n (w state))))
                 (puffable-command-blockp
                  (cdr wrld)
                  (access-command-tuple-form (cddr (car wrld))))))))

(defun puff-command-block (wrld ans restore-cbd ctx state)

; Wrld is a world that starts just after a command landmark.  We scan down to
; the next command landmark and return the list of events in this command
; block.  We replace every encapsulate and include-book by the events in its
; body or file, which exposes the LOCAL events that are not actually part of
; wrld now.  However, we do not recursively flatten the encapsulates and
; include-books that are exposed by this flattening.

  (cond
   ((or (null wrld)
        (and (eq (car (car wrld)) 'command-landmark)
             (eq (cadr (car wrld)) 'global-value)))
    (value (if restore-cbd
               (append ans (list `(set-cbd ,(cbd))))
             ans)))
   ((and (eq (car (car wrld)) 'event-landmark)
         (eq (cadr (car wrld)) 'global-value))
    (cond
     ((eq (access-event-tuple-type (cddr (car wrld))) 'encapsulate)

; In the case of an encapsulate event, flattening means do the body of the
; encapsulate ~-[] including the LOCAL events.  Note that this destroys the sense
; of those encapsulates that introduce constrained functions!  After flattening
; the constrained functions are defined as their witnesses!  We cannot recover
; the LOCAL events by a scan through wrld since they are not in wrld.  We must
; instead re-execute the body of the encapsulate.  Therefore, we just append
; the body of the encapsulate to our evolving ans.

; Now there is a problem here.  The body of the encapsulate might contain a
; macro form such as (defstub fn (x y) t) which when executed will expand to an
; encapsulate and which, intuitively, we ought to flatten.  Because it is a
; macro form, we cannot here recognize it as an encapsulate nor could we figure
; out its body.

; The way out of this problem, if one wants to recursively flatten, is to
; re-execute the events in our returned ans, thereby exposing the next layer of
; flattenable events, and then flatten the area again.

      (puff-command-block
       (scan-past-deeper-event-landmarks
        (access-event-tuple-depth (cddr (car wrld)))
        (cdr wrld))
       (append (cddr (access-event-tuple-form (cddr (car wrld)))) ans)
       restore-cbd ctx state))
     ((eq (access-event-tuple-type (cddr (car wrld))) 'include-book)

; Comments similar to those about encapsulate apply to include-book.  We simply
; go to the file named by the include-book and read the events in it, appending
; them to our ans.  Recursive include-books are not flattened here.

      (let ((full-book-name (access-event-tuple-namex (cddr (car wrld)))))
        (er-progn
         (chk-input-object-file full-book-name ctx state)
         (chk-book-name full-book-name full-book-name ctx state)
         (er-let*
          ((ev-lst (read-object-file full-book-name ctx state))
           (cert-obj (chk-certificate-file
                      full-book-name
                      t
                      ctx
                      state
                      '((:uncertified-okp . t)
                        (:defaxioms-okp t)
                        (:skip-proofs-okp t))
                      nil))
           (expansion-alist
            (value (and cert-obj
                        (access cert-obj cert-obj :expansion-alist)))))
          (mv-let
           (ev-lst-chk-sum state)
           (check-sum-obj
            (append expansion-alist ev-lst)
            state)
           (cond
            ((not (integerp ev-lst-chk-sum))

; This error should never arise because check-sum-obj is only called on
; something produced by read-object, which checks that the object is ACL2
; compatible.  And if it somehow did happen, it is presumably not because of
; the expansion-alist, which must be well-formed since it is in the book's
; certificate.

             (er soft ctx
                 "The file ~x0 is not a legal list of embedded event forms ~
                  because it contains an object, ~x1, which check sum was ~
                  unable to handle."
                 full-book-name ev-lst-chk-sum))
            (t (let ((temp (assoc-equal full-book-name
                                        (global-val 'include-book-alist
                                                    (w state)))))

; Temp is of the form (full-book-name user-book-name familiar-name
; cert-annotations . ev-lst-chk-sum).

                 (cond
                  ((and (cddddr temp)
                        (not (equal ev-lst-chk-sum (cddddr temp))))
                   (er soft ctx
                       "When the certified book ~x0 was included, its check ~
                        sum was ~x1.  The check sum for ~x0 is now ~x2.  The ~
                        file has thus been modified since it was last ~
                        included and we cannot now recover the events that ~
                        created the current logical world."
                       full-book-name
                       (cdddr temp)
                       ev-lst-chk-sum))
                  (t (puff-command-block
                      (scan-past-deeper-event-landmarks
                       (access-event-tuple-depth (cddr (car wrld)))
                       (cdr wrld))
                      (append (cons `(set-cbd
                                      ,(remove-after-last-directory-separator
                                        full-book-name))
                                    (cons (assert$
                                           (and (consp (car ev-lst))
                                                (eq (caar ev-lst) 'in-package))
                                           (car ev-lst))
                                          (subst-by-position expansion-alist
                                                             (cdr ev-lst)
                                                             1)))
                              `((maybe-install-acl2-defaults-table
                                 ',(table-alist 'acl2-defaults-table wrld)
                                 ',ctx
                                 state))
                              ans)
                      t ctx state)))))))))))
     (t (puff-command-block (cdr wrld)
                            (cons (access-event-tuple-form (cddr (car wrld)))
                                  ans)
                            restore-cbd ctx state))))
   (t (puff-command-block (cdr wrld) ans restore-cbd ctx state))))

(defun commands-back-to (wrld1 wrld2 ans)

; Wrld2 is a tail of wrld1.  Each starts with a command-landmark initially.  We
; collect all the non-eviscerated commands back to (but not including) the one
; at wrld2.

  (cond
   ((equal wrld1 wrld2) ans)
   ((and (eq (car (car wrld1)) 'command-landmark)
         (eq (cadr (car wrld1)) 'global-value))
    (commands-back-to (cdr wrld1) wrld2
                      (cons (access-command-tuple-form (cddr (car wrld1)))
                            ans)))
   (t (commands-back-to (cdr wrld1) wrld2 ans))))

(defun puffed-command-sequence (cd ctx wrld state)

; Cd is a command descriptor.  We puff up the command at cd, into the list of
; immediate subevents, and then append to that list the commands in wrld that
; chronologically followed cd.

  (er-let*
   ((cmd-wrld (er-decode-cd cd wrld ctx state)))
   (cond
    ((puffable-command-blockp (cdr cmd-wrld)
                              (access-command-tuple-form (cddr (car cmd-wrld))))
     (er-let*
      ((ans (puff-command-block (cdr cmd-wrld)
                                (commands-back-to wrld cmd-wrld nil)
                                nil ctx state)))
      (value ans)))
    (t (er soft ctx
           "The command at ~x0, namely ~X12, cannot be puffed.  See :DOC puff."
           cd
           (access-command-tuple-form (cddr (car cmd-wrld)))
           ;;; (evisc-tuple 2 3 nil nil)
           '(nil 2 3 nil))))))

(defun puff-fn1 (cd state)

; This function is essentially :puff except that it does no printing.
; It returns a pair, (i . j), where i and j are the relative command numbers
; delineating the region inserted by the puff.  In particular, cd points to
; the command with relative command number i, that command got puffed up,
; and the new commands have the numbers i through j, inclusive.

  (let ((wrld (w state))
        (ctx 'puff))
    (er-let* ((cmd-wrld (er-decode-cd cd wrld :puff state))) 
             (cond ((<= (access-command-tuple-number (cddar cmd-wrld))
                        (access command-number-baseline-info
                                (global-val 'command-number-baseline-info wrld)
                                :current))

; See the similar comment in ubt-ubu-fn.

                    (cond
                     ((<= (access-command-tuple-number (cddar cmd-wrld))
                          (access command-number-baseline-info
                                  (global-val 'command-number-baseline-info wrld)
                                  :original))
                      (er soft :puff
                          "Can't puff a command within the system ~
                           initialization."))
                     (t
                      (er soft :puff
                          "Can't puff a command within prehistory.  See :DOC ~
                           reset-prehistory."))))
                   (t
                    (er-let*
                     ((cmds (puffed-command-sequence cd :puff wrld state)))
                     (let* ((pred-wrld (scan-to-command (cdr cmd-wrld)))
                            (i (absolute-to-relative-command-number
                                (max-absolute-command-number cmd-wrld)
                                (w state)))
                            (k (- (absolute-to-relative-command-number
                                   (max-absolute-command-number (w state))
                                   (w state))
                                  i)))
                       (pprogn
                        (set-w 'retraction pred-wrld state)
                        (er-let*
                         ((defpkg-items
                            (defpkg-items
                              (global-val 'known-package-alist cmd-wrld)
                              ctx pred-wrld state)))
                         (er-progn
                          (state-global-let*
                           ((guard-checking-on nil)) ; agree with include-book
                           (ld (append (new-defpkg-list defpkg-items
                                                        (global-val
                                                         'known-package-alist
                                                         pred-wrld))
                                       cmds)
                               :ld-skip-proofsp 'include-book-with-locals
                               :ld-keyword-aliases nil
                               :ld-verbose nil
                               :ld-prompt nil
                               :ld-pre-eval-filter :all
                               :ld-pre-eval-print :never
                               :ld-post-eval-print nil
                               :ld-error-triples t
                               :ld-error-action :error
                               :ld-query-control-alist
                               (cons '(:redef :y)
                                     (ld-query-control-alist state))))
                          (value (cons i
                                       (- (absolute-to-relative-command-number
                                           (max-absolute-command-number (w state))
                                           (w state))
                                          k)))))))))))))

(defun puff-report (caller new-cd1 new-cd2 cd state)
  (cond ((eql new-cd1 (1+ new-cd2))
         (pprogn (io? temporary nil state
                      (caller cd)
                      (fms "Note: ~x0 is complete, but no events were ~
                            executed under the given command descriptor, ~
                            ~x1.~|"
                           (list (cons #\0 caller)
                                 (cons #\1 cd))
                           (standard-co state) state nil))
                 (value :invisible)))
        (t (pcs-fn new-cd1 new-cd2 t state))))

(defun puff-fn (cd state)
  (er-let* ((pair (puff-fn1 cd state)))
           (puff-report :puff (car pair) (cdr pair) cd state)))

(defun puff*-fn11 (ptr k i j state)

; If there is a command whose relative command number, n, is i<=n<=j, then we
; puff the command with the smallest such n.  Then, we iterate, over the
; interval [ptr, max-k], where max is the maximum relative command number in
; the puffed world.  This function must be protected with
; revert-world-on-error.

  (cond
   ((> i j) (value (cons ptr j)))
   ((puffable-command-numberp i state)
    (er-progn
     (puff-fn1 i state)
     (puff*-fn11 ptr k
                 ptr (- (absolute-to-relative-command-number
                         (max-absolute-command-number (w state))
                         (w state))
                        k)
                 state)))
   (t (puff*-fn11 ptr k (1+ i) j state))))

(defun puff*-fn1 (ptr k state)

; Ptr is a relative command number.  K is an integer.  Let max be the maximum
; relative command number in (w state).  We are to recursively puff all the
; commands whose relative command numbers lie between ptr and max-k,
; inclusively.  Thus, for example, if ptr is 12, max is 21 and k is 2, we are
; to puff all the commands that lie in the interval [12, 19].  Observe that
; this means we leave the last k commands of (w state) unpuffed.  Observe that
; every time we puff a command in the interval, max grows (or stays fixed) and
; the width of the region to be puffed grows (weakly).  See the comment in
; puff*-fn for an example.

; We therefore find the first command (the command with the smallest number) in
; the region that is puffable, we puff it, and we iterate.  We stop when no
; command in the region is puffable.  This function uses
; revert-world-on-error because it is possible that the attempt to puff some
; command will cause an error (e.g., because some book's check sum no longer
; agrees with include-book-alist).

  (revert-world-on-error
   (puff*-fn11 ptr k
               ptr
               (- (absolute-to-relative-command-number
                   (max-absolute-command-number (w state))
                   (w state))
                  k)
               state)))

(defun puff*-fn (cd state)
  (let ((wrld (w state)))
    (er-let* ((cmd-wrld (er-decode-cd cd wrld :puff* state)))
             (cond ((<= (access-command-tuple-number (cddar cmd-wrld))
                        (access command-number-baseline-info
                                (global-val 'command-number-baseline-info wrld)
                                :current))

; See the similar comment in ubt-ubu-fn.

                    (cond
                     ((<= (access-command-tuple-number (cddar cmd-wrld))
                          (access command-number-baseline-info
                                  (global-val 'command-number-baseline-info wrld)
                                  :original))
                      (er soft :puff*
                          "Can't puff* a command within the system ~
                           initialization."))
                     (t
                      (er soft :puff*
                          "Can't puff* a command within prehistory.  See :DOC ~
                           reset-prehistory."))))
                   (t
                    (let* ((mx (absolute-to-relative-command-number
                                (max-absolute-command-number wrld)
                                wrld))
                           (ptr (absolute-to-relative-command-number
                                 (max-absolute-command-number cmd-wrld)
                                 wrld))
                           (k (- mx ptr)))
                      (er-let*
                       ((pair (puff*-fn1 ptr k state)))

; The difference between puff and puff* is that puff* iterates puff across the
; region generated by the first puff until there are no more commands that are
; puffable.  Before continuing, we illustrate how we determine the bounds of
; the region in question.  We bound the region with relative command numbers.
; Suppose we are asked to puff* cd, where cd points to relative command number
; 12 below.

; 12   cmd1   ; ptr = 12 = the relative command number indicated by cd
; 13   cmd2
; 14   cmd3   ; mx = latest command

; Then mx, above, will be 14 and ptr will be 12.  Observe that there are two
; commands then that are not part of the region to be puffed, namely commands
; 13 and 14.  Now after puffing once, we will have something like:

; 12   cmd1a
; 13   cmd1b
; ...  
; 19   cmd1h
; 20   cmd2
; 21   cmd3

; Observe that the new max command number is 21.  The region to be recursively
; puffed now lies between 12 and 19, inclusive.  The last two commands, now
; numbered 20 and 21, are outside the region.

; Let k be (- mx ptr), i.e., 2 in this example and, in general, the number of
; commands not in the region.  Then in general we should recursively puff
; commands whose numbers are between ptr and (- max k), where max is the
; current maximum relative command number, inclusive.  Initially this region
; contains just one command, the one we are to puff first.  ;

                      (puff-report :puff* (car pair) (cdr pair) cd
                                   state))))))))

(defmacro puff (cd)

  ":Doc-Section History

  replace a compound ~il[command] by its immediate subevents~/
  ~bv[]
  Example Forms:
  ACL2 !>:puff :max
  ACL2 !>:puff :x
  ACL2 !>:puff 15
  ACL2 !>:puff \"book\"~/

  General Form:
  :puff cd
  ~ev[]
  where ~c[cd] is a ~il[command] descriptor (~pl[command-descriptor]) for
  a ``puffable'' ~il[command] (see below).  ~c[Puff] replaces the ~il[command] at ~c[cd]
  by the immediate subevents of the ~il[command], executed as ~il[command]s.
  ~c[Puff] then prints, using ~ilc[pcs], the ~c[puff]ed region.

  A ~il[command] is ``puffable'' if it is an ~ilc[encapsulate] ~il[command], an
  ~ilc[include-book] ~il[command], or any ~il[command] other than those consisting of
  a single primitive event.  For example, since ~ilc[defun] is a primitive
  event, a ~ilc[defun] ~il[command] is not puffable.  But a macro form that
  expands into several ~ilc[defun] ~il[events] is puffable.  The only primitive
  event ~il[command]s that are puffable are ~ilc[encapsulate] and ~ilc[include-book]
  ~il[command]s.  A puffable ~il[command] contains (interesting) subevents,
  namely, the ~il[events] in the body of the ~ilc[encapsulate], in the file of
  the book included, or in the ~il[command] block.

  The puff ~il[command] ``lifts'' the immediate subevents of the indicated
  ~il[command] so that they become ~il[command]s themselves.  The ~il[command] ~ilc[puff*]
  recursively puffs the newly introduced ~il[command]s.  ~l[puff*],
  which also gives an example illustrating both ~c[puff] and ~ilc[puff*].  ~c[Puff]
  undoes the ~il[command] at ~c[cd] and replaces it by its immediate subevents.
  Thus, in general the length of the ~il[history] grows when a puff ~il[command]
  is executed.  If ~c[puff] causes an error (see below), the logical ~il[world]
  remains unchanged from its initial configuration.

  The intended use of ~c[puff] is to allow the user access to the ~il[events]
  ``hidden'' inside compound ~il[command]s.  For example, while trying to
  prove some theorem, ~c[p], about a constrained function, ~c[fn], one might
  find that the ~ilc[encapsulate], ~c[cd], that introduced ~c[fn] failed to include
  an important ~il[constraint], ~c[q].  Without ~c[puff], the only way to proceed
  is to undo back through ~c[cd], create a suitable ~ilc[encapsulate] that
  proves and exports ~c[q] as well as the old ~il[constraint]s, re-execute the
  new ~ilc[encapsulate], re-execute the ~il[events] since ~c[cd], and then try ~c[p]
  again.  Unfortunately, it may be hard to prove ~c[q] and additional
  ~il[events] may have to be inserted into the ~ilc[encapsulate] to prove it.  It
  may also be hard to formulate the ``right'' ~c[q], i.e., one that is
  provable in the ~ilc[encapsulate] and provides the appropriate facts for
  use in the proof of ~c[p].

  Using ~c[puff], the user can erase the ~ilc[encapsulate] at ~c[cd], replacing it
  by the ~il[events] in its body.  Now the formerly constrained function,
  ~c[fn], is defined as its witness.  The user can experiment with
  formulations and proofs of ~c[q] suitable for ~c[p].  Of course, to get into
  the ultimately desired ~il[state] ~-[] where ~c[fn] is constrained rather than
  defined and ~c[q] is exported by an ~ilc[encapsulate] at ~c[cd] ~-[] the user must
  ultimately undo back to ~c[cd] and carry out the more tedious program
  described above.  But by using ~c[puff] it is easier to experiment.

  Similar applications of ~c[puff] allow the user of a book to expose the
  innards of the book as though they had all be typed as ~il[command]s.
  The user might then ``partially undo'' the book, keeping only some
  of the ~il[events] in it.

  ~c[Puff] operates as follows.  First, it determines the list of
  immediate subevents of the ~il[command] indicated by ~c[cd].  It causes an
  error if there is only one subevent and that subevent is identical
  to the ~il[command] ~-[] i.e., if the ~il[command] at ~c[cd] is a primitive.  Next,
  ~c[puff] undoes back through the indicated ~il[command].  This not only
  erases the ~il[command] at ~c[cd] but all the ~il[command]s executed after it.
  Finally, ~c[puff] re-executes the subevents of (the now erased) ~c[cd]
  followed by all the ~il[command]s that were executed afterwards.

  Observe that the ~il[command]s executed after ~c[cd] will generally have
  higher ~il[command] numbers than they did before the puff.  For example,
  suppose 100 ~il[command]s have been executed and that ~c[:puff 80] is then
  executed.  Suppose ~il[command] 80 contains 5 immediate subevents (i.e.,
  is an encapsulation of five ~il[events]).  Then, after puffing, ~il[command]
  80 is the first event of the puffed ~il[command], ~il[command] 81 is the
  second, and so on; 104 ~il[command]s appear to have been executed.

  When puffing an ~ilc[encapsulate] or ~ilc[include-book], the ~ilc[local] ~il[command]s are
  executed.  Note that this will replace constrained functions by
  their witnesses.

  Finally, it is impossible to ~c[puff] in the presence of ~ilc[include-book]
  ~il[command]s involving certified files that have been altered since they
  were included.  To be specific, suppose ~c[\"arith\"] is a certified
  book that has been included in a session.  Suppose that after
  ~c[\"arith\"] was included, the source file is modified.  (This might
  happen if the user of ~c[\"arith\"] is not its author and the author
  happens to be working on a new version of ~c[\"arith\"] during the same
  time period.)  Now suppose the user tries to ~c[puff] the ~il[command] that
  included ~c[\"arith\"].  The attempt to obtain the subevents in
  ~c[\"arith\"] will discover that the check sum of ~c[\"arith\"] has changed
  and an error will be caused.  No change is made in the logical
  ~il[world].  A similar error is caused if, in this same situation, the
  user tries to puff ~st[any command that occurred before the inclusion] of
  ~c[\"arith\"]!  That is, ~c[puff] may cause an error and leave the ~il[world]
  unchanged even if the ~il[command] puffed is not one involving the
  modified book.  This happens because in order to reconstruct the
  ~il[world] after the puffed ~il[command], ~c[puff] must obtain the ~il[events] in the
  book and if the book's source file has changed there is no assurance
  that the reconstructed ~il[world] is the one the user intends.

  Warning: We do not detect changes to uncertified ~il[books] that have
  been included and are then puffed or re-included!  The act of
  including an uncertified book leaves no trace of the check sum of
  the book.  Furthermore, the act prints a warning message disclaiming
  soundness.  In light of this, ~c[:puff] quietly ``re-''executes the
  current contents of the book."

  `(puff-fn ,cd state))

(defmacro puff* (cd)

  ":Doc-Section History

  replace a compound ~il[command] by its subevents~/
  ~bv[]
  Example Forms:
  ACL2 !>:puff* :max
  ACL2 !>:puff* :x
  ACL2 !>:puff* 15
  ACL2 !>:puff* \"book\"~/

  General Form:
  :puff* cd
  ~ev[]
  where ~c[cd] is a ~il[command] descriptor (~pl[command-descriptor]) for
  a ``puffable'' ~il[command].  ~l[puff] for the definition of
  ``puffable'' and for a description of the basic act of ``puffing'' a
  ~il[command].  ~c[Puff*] is just the recursive application of ~il[puff].  ~c[Puff*]
  prints the region ~il[puff]ed, using ~ilc[pcs].

  To ~il[puff] a ~il[command] is to replace it by its immediate subevents, each
  of which is executed as a ~il[command].  To ~c[puff*] a ~il[command] is to replace
  the ~il[command] by each of its immediate subevents and then to ~c[puff*]
  each of the puffable ~il[command]s among the newly introduced ones.

  For example, suppose ~c[\"ab\"] is a book containing the following
  ~bv[]
  (in-package \"ACL2\")
  (include-book \"a\")
  (include-book \"b\")
  ~ev[]
  Suppose that book ~c[\"a\"] only contained ~ilc[defun]s for the functions ~c[a1]
  and ~c[a2] and that ~c[\"b\"] only contained ~ilc[defun]s for ~c[b1] and ~c[b2].

  Now consider an ACL2 ~il[state] in which only two ~il[command]s have been
  executed, the first being ~c[(include-book \"ab\")] and the second being
  ~c[(include-book \"c\")].  Thus, the relevant part of the display
  produced by ~c[:]~ilc[pbt] 1 would be:
  ~bv[]
  1 (INCLUDE-BOOK \"ab\")
  2 (INCLUDE-BOOK \"c\")
  ~ev[]
  Call this ~il[state] the ``starting ~il[state]'' in this example, because we
  will refer to it several times.

  Suppose ~c[:puff 1] is executed in the starting ~il[state].  Then the first
  ~il[command] is replaced by its immediate subevents and ~c[:pbt 1] would
  show:
  ~bv[]
  1 (INCLUDE-BOOK \"a\")
  2 (INCLUDE-BOOK \"b\")
  3 (INCLUDE-BOOK \"c\")
  ~ev[]
  Contrast this with the execution of ~c[:puff* 1] in the starting
  ~il[state].  ~c[Puff*] would first ~il[puff] ~c[(include-book \"ab\")] to get the
  ~il[state] shown above.  But then it would recursively ~c[puff*] the puffable
  ~il[command]s introduced by the first ~il[puff].  This continues recursively
  as long as any ~il[puff] introduced a puffable ~il[command].  The end result
  of ~c[:puff* 1] in the starting ~il[state] is
  ~bv[]
  1 (DEFUN A1 ...)
  2 (DEFUN A2 ...)
  3 (DEFUN B1 ...)
  4 (DEFUN B2 ...)
  5 (INCLUDE-BOOK \"c\")
  ~ev[]
  Observe that when ~c[puff*] is done, the originally indicated ~il[command],
  ~c[(include-book \"ab\")], has been replaced by the corresponding
  sequence of primitive ~il[events].  Observe also that puffable ~il[command]s
  elsewhere in the ~il[history], for example, ~il[command] 2 in the starting
  ~il[state], are not affected (except that their ~il[command] numbers grow as a
  result of the splicing in of earlier ~il[command]s)."

 `(puff*-fn ,cd state))

(defmacro mini-proveall nil

; ACL2 (a)>:mini-proveall

; will change the default-defun-mode to :logic and do a short proveall.  The
; final defun-mode will be :logic.

  '(ld
    '(:logic

; We start with a nice example of forcing, involving primitive fns.

      (thm (implies (and (true-listp x)
                         (true-listp y))
                    (equal (revappend (append x y) z)
                           (revappend y (revappend x z)))))
      (defun app (x y)
        (if (consp x)
            (cons (car x) (app (cdr x) y))
            y))
      (defthm assoc-of-app
        (equal (app (app a b) c) (app a (app b c))))
      (defun rev (x)
        (if (consp x)
            (app (rev (cdr x)) (cons (car x) nil))
            nil))
      (defthm true-listp-rev
        (true-listp (rev x))
        :rule-classes (:REWRITE :GENERALIZE))

; Here we test the proof-checker using the same theorem as the one that
; follows (but not storing it as a :rewrite rule).

      (defthm rev-app-proof-checker
        (equal (rev (app a b)) (app (rev b) (rev a)))
        :rule-classes nil
        :instructions
        (:induct :bash :induct :bash :split (:dv 1)
                 :x :nx (:dv 1)
                 :x :top :s :bash (:dive 1 1)
                 := (:drop 2)
                 :top :bash))
      (defthm rev-app
        (equal (rev (app a b)) (app (rev b) (rev a))))
      (defthm rev-rev
        (implies (true-listp x) (equal (rev (rev x)) x)))

;    The following events are the big example in deflabel equivalence.

      (encapsulate (((lt * *) => *))
                   (local (defun lt (x y) (declare (ignore x y)) nil))
                   (defthm lt-non-symmetric (implies (lt x y) (not (lt y x)))))

      (defun insert (x lst)
        (cond ((atom lst) (list x))
              ((lt x (car lst)) (cons x lst))
              (t (cons (car lst) (insert x (cdr lst))))))

      (defun insert-sort (lst)
        (cond ((atom lst) nil)
              (t (insert (car lst) (insert-sort (cdr lst))))))

      (defun del (x lst)
        (cond ((atom lst) nil)
              ((equal x (car lst)) (cdr lst))
              (t (cons (car lst) (del x (cdr lst))))))

      (defun mem (x lst)
        (cond ((atom lst) nil)
              ((equal x (car lst)) t)
              (t (mem x (cdr lst)))))

      (defun perm (lst1 lst2)
        (cond ((atom lst1) (atom lst2))
              ((mem (car lst1) lst2)
               (perm (cdr lst1) (del (car lst1) lst2)))
              (t nil)))

      (defthm perm-reflexive
        (perm x x))

      (defthm perm-cons
        (implies (mem a x)
                 (equal (perm x (cons a y))
                        (perm (del a x) y)))
        :hints (("Goal" :induct (perm x y))))

      (defthm perm-symmetric
        (implies (perm x y) (perm y x)))

      (defthm mem-del
        (implies (mem a (del b x)) (mem a x))
        :rule-classes ((:rewrite :match-free :once)))

      (defthm perm-mem
        (implies (and (perm x y)
                      (mem a x))
                 (mem a y))
        :rule-classes ((:rewrite :match-free :once)))

      (defthm mem-del2
        (implies (and (mem a x)
                      (not (equal a b)))
                 (mem a (del b x))))

      (defthm comm-del
        (equal (del a (del b x)) (del b (del a x))))

      (defthm perm-del
        (implies (perm x y)
                 (perm (del a x) (del a y))))

      (defthm perm-transitive
        (implies (and (perm x y) (perm y z)) (perm x z))
        :rule-classes ((:rewrite :match-free :once)))

      (defequiv perm)

      (in-theory (disable perm perm-reflexive perm-symmetric perm-transitive))

      (defcong perm perm (cons x y) 2)

      (defcong perm iff (mem x y) 2)

      (defthm atom-perm
        (implies (not (consp x)) (perm x nil))
        :rule-classes :forward-chaining
        :hints (("Goal" :in-theory (enable perm))))

      (defthm insert-is-cons
        (perm (insert a x) (cons a x)))

      (defthm insert-sort-is-id 
        (perm (insert-sort x) x))

      (defcong perm perm (app x y) 2)

      (defthm app-cons
        (perm (app a (cons b c)) (cons b (app a c))))

      (defthm app-commutes
        (perm (app a b) (app b a)))

      (defcong perm perm (app x y) 1 :hints (("Goal" :induct (app y x))))

      (defthm rev-is-id (perm (rev x) x))

      (defun == (x y)
        (if (consp x)
            (if (consp y)
                (and (equal (car x) (car y))
                     (== (cdr x) (cdr y)))
                nil)
            (not (consp y))))

      (defthm ==-symmetric (== x x))

      (defthm ==-reflexive (implies (== x y) (== y x)))

      (defequiv ==)

      (in-theory (disable ==-symmetric ==-reflexive))

      (defcong == == (cons x y) 2)

      (defcong == iff (consp x) 1)

      (defcong == == (app x y) 2)

      (defcong == == (app x y) 1)

      (defthm rev-rev-again (== (rev (rev x)) x))

; This next block tests forcing.

      (defun ends-in-a-0 (x)
        (declare (xargs :guard t))
        (if (consp x) (ends-in-a-0 (cdr x)) (equal x 0)))

      (defun app0 (x y)
        (declare (xargs :guard (ends-in-a-0 x)))
        (if (ends-in-a-0 x)
            (if (equal x 0) y (cons (car x) (app0 (cdr x) y)))
            'default))

      (defun rev0 (x)
        (declare (xargs :guard (ends-in-a-0 x)))
        (if (ends-in-a-0 x)
            (if (equal x 0) 0 (app0 (rev0 (cdr x)) (cons (car x) 0)))
            'default))

      (defthm app0-right-id
        (implies (force (ends-in-a-0 x)) (equal (app0 x 0) x)))

      (defun ends-in-a-zero (x) (ends-in-a-0 x))

      (defthm ends-in-a-zero-app0
        (implies (force (ends-in-a-zero x)) (ends-in-a-0 (app0 x (cons y 0)))))

      (in-theory (disable ends-in-a-zero))

; The following theorem causes two forcing rounds.  In the first, there
; are three goals, all variants of one another.  An inductive proof of one
; of them is done and generates the second forcing round.

      (defthm force-test
        (and (implies (ends-in-a-0 x) (equal (app0 (rev0 x) 0) (rev0 x)))
             (implies (ends-in-a-0 y) (equal (app0 (rev0 y) 0) (rev0 y)))
             (implies (ends-in-a-0 z) (equal (app0 (rev0 z) 0) (rev0 z))))
        :hints (("[2]Goal" :in-theory (enable ends-in-a-zero))))

; This defun does a lot of proving for both termination and guard verification.

      (defun proper-cons-nest-p (x)
        (declare (xargs :guard (pseudo-termp x)))
        (cond ((symbolp x) nil)
              ((fquotep x) (true-listp (cadr x)))
              ((eq (ffn-symb x) 'cons)
               (proper-cons-nest-p (fargn x 2)))
              (t nil)))

; This defthm has two forcing rounds and is very realistic.

      (defthm ordered-symbol-alistp-remove-first-pair-test
        (implies (and (ordered-symbol-alistp l)
                      (symbolp key)
                      (assoc-eq key l))
                 (ordered-symbol-alistp (remove-first-pair key l)))
        :hints (("Goal" :in-theory (disable ordered-symbol-alistp-remove-first-pair))))

      )
    :ld-skip-proofsp nil
    :ld-redefinition-action nil
    :ld-pre-eval-print t
    :ld-error-action :return))

(defmacro exit ()

  ":Doc-Section Other

  quit entirely out of Lisp~/

  Same as ~ilc[good-bye].~/~/"

  '(good-bye-fn "(EXIT)"))

(defmacro quit ()

  ":Doc-Section Other

  quit entirely out of Lisp~/

  Same as ~ilc[good-bye].~/~/"

  '(good-bye-fn "(QUIT)"))

(defmacro set-guard-checking (flg)
  (declare (xargs :guard
                  (let ((flg (if (and (consp flg)
                                      (eq (car flg) 'quote)
                                      (consp (cdr flg)))
                                 (cadr flg)
                               flg)))
                    (member-eq flg '(t nil :nowarn :all :none)))))

  ":Doc-Section Other

  control checking ~il[guard]s during execution of top-level forms~/

  Detailed comments about the arguments of this function may be found
  elsewhere: ~pl[guard-evaluation-table].  Here we provide an introduction to
  the use of ~c[set-guard-checking].

  New users are encouraged to execute one of the following forms in order to
  avoid evaluation errors due to ~il[guard]s:
  ~bv[]
  (set-guard-checking :none)
  (set-guard-checking nil)
  ~ev[]
  The former avoids all guard-checking on user-defined functions and should
  generally work fine for new users, the only drawback being efficiency loss on
  compute-intensive problems.  All settings other than ~c[:none] check guards,
  but a value of ~c[nil] allows evaluation to continue in the logic when guards
  fail (avoiding the raw Lisp definition in that case).

  You may put one of the above forms in the ~c[\"acl2-customization.lisp\"]
  file in your current directory (~pl[cbd]) or your home directory;
  ~pl[acl2-customization].

  Note that ~il[guard]s are not part of the ACL2 logic, and hence new users can
  completely ignore the notion of ~il[guard] (and the rest of this
  documentation section after this paragraph!).  For example, ~c[(car 3)] and
  ~c[nil] can be proved equal in the ACL2 logic, as follows, even though the
  ~il[guard] on ~ilc[car] requires its first argument to be a ~ilc[cons] pair
  or ~c[nil].
  ~bv[]
  (thm (equal (car 3) nil))
  ~ev[]
  Moreover, unless your functions or top-level forms call built-in ACL2
  functions that are defined in ~c[:]~ilc[program] mode, the following property
  will hold.
  ~bq[]
  Evaluation of ~c[(set-guard-checking :none)] will allow evaluation of forms
  such as ~c[(car 3)] to take place without error in the top level loop, not
  only when proving theorems.
  ~eq[]

  If you feel bold, then you may wish to read the rest of this
  documentation topic; also ~pl[guard].

  ~l[guard-evaluation-table] for a succinct table, with associated discussion,
  that covers in detail the material presented in the rest of the present
  topic.~/

  The top-level ACL2 loop has a variable which controls which sense of
  execution is provided.  To turn ``~il[guard] checking on,'' by which we mean
  that ~il[guard]s are checked at runtime, execute the top-level form
  ~c[:set-guard-checking t].  To allow guard violations, do
  ~c[:set-guard-checking nil], or do ~c[:set-guard-checking :none] to turn off
  all guard-checking, so that raw Lisp definitions of user-defined functions
  are avoided unless their ~il[guard] is ~c[t]. The status of guard-checking is
  reflected in the ~il[prompt].
  ~bv[]
  ACL2 !>
  ~ev[]
  means ~il[guard] checking is on and
  ~bv[]
  ACL2 >
  ~ev[]
  means ~il[guard] checking is off.  The exclamation mark can be thought of
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
  will return ~c[nil].

  We will return at the end of this documentation topic to discuss two other
  values, ~c[:all] and ~c[:nowarn], for ~c[:set-guard-checking].  We also note
  that evaluation of built-in ~c[:program] mode functions always takes place in
  raw Lisp.

  Whether ~il[guard]s are checked during evaluation is independent of the
  ~ilc[default-defun-mode].  We note this simply because it is easy to
  confuse ``~c[:]~ilc[program] mode'' with ``evaluation in Common Lisp'' and
  thus with ``~il[guard] checking on;'' and it is easy to confuse
  ``~c[:]~ilc[logic] mode'' with ``evaluation in the logic'' and with ``~il[guard]
  checking off.''  But the ~ilc[default-defun-mode] determines whether
  newly submitted definitions introduce programs or add logical
  axioms.  That mode is independent of whether evaluation checks
  ~il[guard]s or not.  You can operate in ~c[:]~ilc[logic] mode with runtime ~il[guard]
  checking on or off.  Analogously, you can operate in ~c[:]~ilc[program]
  mode with runtime ~il[guard] checking on or off.

  For further discussion on evaluation and guards ~pl[guards-and-evaluation],
  in particular the exception for safe-mode in the ``Aside'' there.  ~l[guard]
  for a general discussion of ~il[guard]s.

  Now we fulfill our promise above to discuss two other values for
  ~c[:set-guard-checking]:
  ~bv[]
  :set-guard-checking :nowarn
  :set-guard-checking :all
  ~ev[]
  The meaning of these values is perhaps best described by the following
  example provided by David Rager.
  ~bv[]
  ACL2 !>(defun my-test (expr)
           (declare (xargs :guard (true-listp expr)
                           :verify-guards nil))
           (if (atom expr)
               expr
             (cons (my-test (car expr))
                   (my-test (cdr expr)))))

  The admission of MY-TEST is trivial, using the relation O< (which is
  known to be well-founded on the domain recognized by O-P) and the measure
  (ACL2-COUNT EXPR).  We could deduce no constraints on the type of MY-
  TEST.  However, in normalizing the definition we used primitive type
  reasoning.

  Summary
  Form:  ( DEFUN MY-TEST ...)
  Rules: ((:FAKE-RUNE-FOR-TYPE-SET NIL))
  Warnings:  None
  Time:  0.01 seconds (prove: 0.00, print: 0.00, other: 0.01)
   MY-TEST
  ACL2 !>(my-test '(a b c))

  ACL2 Warning [Guards] in TOP-LEVEL:  Guard-checking will be inhibited
  on recursive calls of the executable counterpart (i.e., in the ACL2
  logic) of MY-TEST.  To check guards on all recursive calls:
    (set-guard-checking :all)
  To leave behavior unchanged except for inhibiting this message:
    (set-guard-checking :nowarn)

  (A B C)
  ACL2 !>
  ~ev[]
  If you think about evaluation of ~c[(my-test '(a b c))], you will see that it
  leads to the recursive call ~c[(my-test 'a)], which one might expect to cause
  a guard violation since the symbol ~c[a] is not a ~ilc[true-listp].  However,
  as the warning above explains, we do not by default check guards on recursive
  calls.  The reason is efficiency ~-[] imagine a simple definition with a
  guard that is slow to evaluate.  The values ~c[:nowarn] and ~c[:all] for
  ~c[:set-guard-checking] have been introduced as ways of dealing with the
  above warning.  The value ~c[:nowarn] simply turns off the warning above.
  The value ~c[:all] causes all guards to be checked, even on recursive calls
  and even on all calls of non-built-in ~c[:]~ilc[program] mode functions ~-[]
  unless, of course, a call is made of a function whose guard has been verified
  (~pl[verify-guards]), where the arguments satisfy the guard, in which case
  the corresponding call is made in raw Lisp without subsidiary guard-checking.
  We still say that ``guard-checking is on'' after ~c[:set-guard-checking] is
  invoked with values ~c[t], ~c[:nowarn], and ~c[:all], otherwise (after value
  ~c[nil]) we say ``guard-checking is off.

  For technical reasons, ~c[:all] does not have its advertised effect in the
  case of built-in ~c[:]~ilc[program]-mode functions.  If you are interested in
  this technical detail, see the comment ``In the boot-strap world...'' in
  source function ~c[oneify-cltl-code].

  We conclude with a remark about the use of ~c[:set-guard-checking] for
  experimenting with ACL2 as a logic or as a programming language.  If one
  views ACL2 as a logic, one may wish to use ~c[:set-guard-checking :none],
  while if instead one views ACL2 as a functional programming language, one may
  wish to use ~c[:set-guard-checking :all].  The following transcript
  illustrates this distinction by way of example.  Specifically, ~c[(car 3)] is
  equal to ~c[nil] in the ACL2 logic, but may be viewed as a programming
  error.  The default of ~c[:set-guard-checking t] is problematic for learning
  ACL2 using ~c[:]~ilc[program] mode functions, since one can get raw Lisp
  errors.  In the example below, the raw Lisp error occurs because ~c[foo]
  implicitly has a ~il[guard] of ~c[t], hence ~c[(foo 3)] is evaluated in raw
  Lisp, which leads to a raw Lisp call of c[(car 3)].
  ~bv[]
  ACL2 !>(defun foo (x)
           (declare (xargs :mode :program)) 
           (car x))

  Summary
  Form:  ( DEFUN FOO ...)
  Rules: NIL
  Warnings:  None
  Time:  0.01 seconds (prove: 0.00, print: 0.00, other: 0.01)
   FOO
  ACL2 !>(foo 3)
  Error: Attempt to take the car of 3 which is not listp.
    [condition type: TYPE-ERROR]

  Restart actions (select using :continue):
   0: Abort entirely from this (lisp) process.
  [Current process: Initial Lisp Listener]
  [1] ACL2(1): [RAW LISP] :pop
  ACL2 !>:set-guard-checking :none

  Turning off guard checking entirely.  To allow execution in raw Lisp
  for functions with guards other than T, while continuing to mask guard
  violations, :SET-GUARD-CHECKING NIL.  See :DOC set-guard-checking.

  ACL2 >(foo 3)
  NIL
  ACL2 >:set-guard-checking :all

  Turning guard checking on, value :ALL.

  ACL2 !>(foo 3)


  ACL2 Error in TOP-LEVEL:  The guard for the function symbol CAR, which
  is (OR (CONSP X) (EQUAL X NIL)), is violated by the arguments in the
  call (CAR 3).  See :DOC wet for how you might be able to get an error
  backtrace.  See :DOC set-guard-checking for information about suppressing
  this check with (set-guard-checking :none), as recommended for new
  users.

  ACL2 !>
  ~ev[]~/

  :cited-by guard"

  `(let ((current-flg (f-get-global 'guard-checking-on state))
         (flg ,(if (and (consp flg) (eq (car flg) 'quote) (consp (cdr flg)))
                   (cadr flg)
                 flg)))
     (cond
      ((and (raw-mode-p state) flg)
       (er soft 'set-guard-checking
           "It is illegal (and anyhow, would be useless) to attempt to modify ~
            guard checking while in raw mode, since guards are not checked in ~
            raw mode."))
      ((eq flg current-flg)
       (pprogn
        (fms "Guard-checking-on already has value ~x0.~%~%"
             (list (cons #\0 flg))
             *standard-co* state nil)
        (value :invisible)))
      ((null flg)
       (pprogn (f-put-global 'guard-checking-on nil state)
               (fms "Masking guard violations but still checking guards.  To ~
                     avoid guard checking entirely, :SET-GUARD-CHECKING ~
                     :NONE.  See :DOC set-guard-checking.~%~%"
                    nil *standard-co* state nil)
               (value :invisible)))
      ((eq flg :none)
       (pprogn (f-put-global 'guard-checking-on :none state)
               (fms "Turning off guard checking entirely.  To allow execution ~
                     in raw Lisp for functions with guards other than T, ~
                     while continuing to mask guard violations, ~
                     :SET-GUARD-CHECKING NIL.  See :DOC ~
                     set-guard-checking.~%~%"
                    nil *standard-co* state nil)
               (value :invisible)))
      (t (pprogn
          (f-put-global 'guard-checking-on flg state)
          (assert$ (and flg (not (eq flg current-flg)))
                   (cond ((member-eq current-flg '(nil :none))
                          (fms "Turning guard checking on, value ~x0.~%~%"
                               (list (cons #\0 flg))
                               *standard-co* state nil))
                         (t
                          (fms "Leaving guard checking on, but changing value ~
                                to ~x0.~%~%"
                               (list (cons #\0 flg))
                               *standard-co* state nil))))
          (value :invisible))))))

; The numeric fmt variables used in the home page are resolved as follows:
; 0 (@ acl2-version)
; 1 "acl2-doc-major-topics.html"
; 2 "acl2-doc-index.html"
; 3 version subdirectory, e.g., "v2-1" for ACL2 Version 2.1
; 4 build date month, e.g., "January"
; 5 build date day, e.g., 8
; 6 build date year, e.g., 1998
; 7 HREF for the Warning message explanation, e.g., "acl2-doc-47.html#Tiny..."

; These variables are set in write-home-page in doc/write-acl2-html.lisp.

; Alphabetic fmt variables used below are defined in the defconst for
; *home-page-references*, immediately following the one for
; *home-page*.

(defconst *home-page*
          "~
<HTML>

<HEAD><TITLE>~s0</TITLE></HEAD>

<BODY TEXT=\"#000000\" BGCOLOR=\"#FFFFFF\">

<TABLE>
<TR>
<TD>
<IMG SRC=\"logo.gif\" ALIGN=LEFT ALT=\"ACL2\">
</TD>
<TD>
<CENTER><H1>~s0</H1></CENTER>

ACL2 is both a programming language in which you can model computer systems
and a tool to help you prove properties of those models.<P>

ACL2 is part of the Boyer-Moore family of provers, for which its authors have
received the 2005 <A HREF=\"http://awards.acm.org/software_system/\">ACM
Software System Award</A>.<P>

<CENTER><H3><I><A HREF=\"http://www.cs.utexas.edu/users/moore/acl2/admin/forms/search.html\">SEARCH</A></I></H3></CENTER>

</TD>
</TR>
</TABLE>
<HR>

<TABLE CELLPADDING=3>

<TR>
<TD ALIGN=CENTER VALIGN=MIDDLE>
<A HREF=\"~sa\"><IMG SRC=\"door02.gif\" BORDER=0></A>
</TD>
<TD>
<A HREF=\"~sa\">Tours</A> and <A HREF=\"http://www.cs.utexas.edu/users/moore/publications/demos.html\">demos</A>
</TD>

<TD ALIGN=CENTER VALIGN=MIDDLE>
<A HREF=\"http://www.cs.utexas.edu/users/moore/acl2/workshops.html\"><IMG SRC=\"teacher2.gif\" BORDER=0></A>
</TD>
<TD>
<A HREF=\"http://www.cs.utexas.edu/users/moore/acl2/workshops.html\">ACL2 Workshops, UT ACL2 Seminar, and Upcoming Conferences</A>
</TD> 
<!--

The Workshops entry was added in place of the FAQ entry.

The FAQ was added in place of the one removed by this comment.

At one time we had a link to the tutorials.  But after the publication
of the first book, we decided that we should encourage people to read the
book rather than do the tutorials, which are not elementary enough.
I think we should write some appropriate tutorials.  Meanwhile, this
entry is left blank.

<TD ALIGN=CENTER VALIGN=MIDDLE>
<A HREF=\"~sb\"><IMG SRC=\"teacher2.gif\" BORDER=0></A>
</TD>
<TD>
<A HREF=\"~sb\">Tutorials (for those who have taken the tours)</A>
</TD>
-->

</TR>

<TR>
<TD ALIGN=CENTER VALIGN=MIDDLE>
<A HREF=\"http://www.cs.utexas.edu/users/moore/publications/acl2-papers.html\">
<IMG SRC=\"doc03.gif\" BORDER=0></A>
</TD>
<TD>
<A HREF=\"http://www.cs.utexas.edu/users/moore/publications/acl2-papers.html\">
Books and Papers about ACL2 and Its Applications</A>
</TD>

<TD ALIGN=CENTER VALIGN=MIDDLE>
<A HREF=\"#User's-Manual\"><IMG SRC=\"info04.gif\" BORDER=0></A>
</TD>
<TD>
<A HREF=\"#User's-Manual\">The User's Manual</A>
and <A HREF=\"http://www.cs.utexas.edu/users/moore/publications/hyper-card.html\">Hyper-Card</A>
</TD>
</TR>

<TR>
<TD ALIGN=CENTER VALIGN=MIDDLE>
<A HREF=\"#Tools\"><img src=\"tools3.gif\" BORDER=0></A>
</TD>
<TD>
<A HREF=\"#Tools\">Mathematical Tools (Lemma Libraries and Utilities); and, How
to Contribute</A>
</TD>

<TD ALIGN=CENTER VALIGN=MIDDLE>
<!-- This relative URL is made absolute in distributed tar file -->
<A HREF=\"installation.html#Addresses\"><img src=\"mailbox1.gif\"  BORDER=0></A>
</TD>
<TD>
<!-- This relative URL is made absolute in distributed tar file -->
<A HREF=\"installation.html#Addresses\">Mailing Lists</A>
</TD>
</TR>

<TR>
<TD ALIGN=CENTER VALIGN=MIDDLE>
<!-- This relative URL is made absolute in distributed tar file -->
<A HREF=\"new.html\">
<IMG SRC=\"new04.gif\" BORDER=0></A>
</TD>
<TD>
<!-- This relative URL is made absolute in distributed tar file -->
<A HREF=\"new.html\">
Recent changes to this page</A>
</TD>
<TD ALIGN=CENTER VALIGN=MIDDLE>
<!-- This relative URL is made absolute in distributed tar file -->
<A HREF=\"installation.html\"><img src=\"ftp2.gif\"  BORDER=0></A>
</TD>
<TD>
<!-- This relative URL is made absolute in distributed tar file -->
<A HREF=\"installation.html\">Obtaining and Installing Version 3.1</A>
</TD>

</TR>

<TR>
<TD ALIGN=CENTER VALIGN=MIDDLE>
<A HREF=\"~sg\"><IMG SRC=\"note02.gif\" BORDER=0></A>
</TD>
<TD>
<A HREF=\"~sg\">Differences with Version 3.0</A><A HREF=\"~s7\"> <IMG SRC=\"twarning.gif\"></A>
</TD>
<TD ALIGN=CENTER VALIGN=MIDDLE>
<A HREF=\"http://www.cs.utexas.edu/users/moore/acl2/v2-9/acl2-doc.html\">
<img src=\"file04.gif\"  BORDER=0></A>
</TD>
<TD>
<A HREF=\"other-releases.html\">
Other Releases</A>
</TD>
</TR>

<TR>
<TD ALIGN=CENTER VALIGN=MIDDLE></TD>
<TD NOWRAP>
<B><A HREF=\"mailto:kaufmann@cs.utexas.edu\">Matt Kaufmann</A> and <A HREF=\"mailto:moore@cs.utexas.edu\">J Strother Moore</A></B><BR>
<A HREF=\"http://www.utexas.edu\">University of Texas at Austin</A><BR>
~s4 ~f5, ~f6
</TD>
<TD ALIGN=CENTER VALIGN=MIDDLE></TD>
<TD>
<BR><BR>
We gratefully acknowledge substantial support from the following.
(These are included in a much more complete <A
href=\"ACKNOWLEDGMENTS.html\">acknowledgments page</A>.)
<UL>
<LI>DARPA</LI>
<LI>National Science Foundation (Any opinions, findings and conclusions or recomendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation (NSF).)</LI>
<LI>Advanced Micro Devices, Inc.</LI>
<LI>Rockwell Collins, Inc.</LI>
<LI>Sun Microsystems, Inc.</LI>
</UL>
</TD>

</TR>
</TABLE>
<BR><BR><HR><BR><BR>

<H2><A NAME=\"User's-Manual\">The User's Manual</A></H2>

ACL2's user manual is a vast hypertext document.  You can wander through
it here, in its HTML format.
<P>
Here are the two common entries to the documentation graph:
<TABLE CELLPADDING=3>
<TR>
<TD ALIGN=CENTER VALIGN=MIDDLE>
<A HREF=\"acl2-doc-major-topics.html\"><IMG SRC=\"file03.gif\" BORDER=0></A>
</TD>
<TD>
<A HREF=\"acl2-doc-major-topics.html\">Major Topics (Table of Contents)</A>
<A HREF=\"~s7\"><IMG SRC=\"twarning.gif\"></A>
</TD>
</TR>

<TR>
<TD ALIGN=CENTER VALIGN=MIDDLE>
<A HREF=\"acl2-doc-index.html\">
<IMG SRC=\"index.gif\" BORDER=0></A>
</TD>
<TD>
<A HREF=\"acl2-doc-index.html\">Index of all documented topics</A> <A HREF=\"~s7\"><IMG SRC=\"twarning.gif\"></A>
</TD>
</TR>
</TABLE>
The tiny warning signs, <A HREF=\"~s7\"><IMG SRC=\"twarning.gif\"></A>, indicate that the links lead out of
introductory level material and into user-level material.  It is easy for the
newcomer to get lost.
<P>
Here is how we recommend you use this documentation.
<P>
If you are a newcomer to ACL2, we do <EM>not</EM> recommend that you wander off
into the full documentation.  Instead start with
<A HREF=\"~sa\">the tours</A> and perhaps <A
HREF=\"http://www.cs.utexas.edu/users/moore/publications/demos.html\">the
demos</A>.<P>

If you are still interested in ACL2, we recommend that you get a copy of
<A HREF=\"http://www.cs.utexas.edu/users/moore/publications/acl2-books/car/index.html\">Computer-Aided Reasoning: An Approach</A> and read it carefully.  Work
the exercises with ACL2.  Learn ``The Method.''<P>

Less effective than reading the book and doing the exercises, but
still useful, is to read selected papers from <A
HREF=\"http://www.cs.utexas.edu/users/moore/publications/acl2-papers.html\">
Books and Papers about ACL2 and Its Applications</A>,
read the documentation topic <A HREF=\"~sh\">the-method</A><A HREF=\"~s7\"><IMG SRC=\"twarning.gif\"></A>, 
and the <A HREF=\"http://www.cs.utexas.edu/users/moore/publications/hyper-card.html\">Hyper-Card</A>
and then work
your way through <A HREF=\"~sb\">the tutorials</A>.<P>

Then, we recommend you
read selected topics from ``Major Topics'': <UL>

<LI><A HREF=\"~sc\">EVENTS</A>
<A HREF=\"~s7\"><IMG SRC=\"twarning.gif\"></A>
-- the top-level commands like <CODE>DEFUN</CODE> and <CODE>DEFTHM</CODE>

<LI><A HREF=\"~sd\">PROGRAMMING</A>
<A HREF=\"~s7\"><IMG SRC=\"twarning.gif\"></A>
-- all the Common Lisp functions ACL2 supports

<LI><A HREF=\"~se\">RULE-CLASSES</A>
<A HREF=\"~s7\"><IMG SRC=\"twarning.gif\"></A>
-- how you can influence ACL2's behavior with rules,

<LI><A HREF=\"~sf\">BOOKS</A>
<A HREF=\"~s7\"><IMG SRC=\"twarning.gif\"></A>
-- how to create re-usable databases of rules.

</UL>
<P>
Finally, experienced users tend mostly to use the ``Index'' to lookup concepts
mentioned in error messages or vaguely remembered from their past experiences
with ACL2.
<P>
<B>Note</B>:  If ACL2 has been installed on your local system, the HTML documentation
should also be available locally.  You can minimize network traffic by
browsing your local copy.  Suppose ACL2 was installed on
<CODE>/usr/jones/acl2/vi-j</CODE>.  Then the local URL for this page is
<BR><CODE>file:/usr/jones/acl2/vi-j/acl2-sources/doc/HTML/acl2-doc.html</CODE>.<BR>
Many ACL2 users set a browser bookmark to this file and use their browser to
access the documentation during everyday use of ACL2.  If you obtain ACL2,
please encourage users to use the local copy of the documentation rather than
access it across the Web.
<P>

<B>Note</B>: The documentation is available in four forms: Postscript (which
prints as a book over 1200 pages long), HTML, EMACS Texinfo, and ACL2's own
:DOC commands.  The documentation, in all but the Postscript form, is
distributed with the source code for the system.  So if you have already
obtained ACL2, you should look in the <CODE>doc/</CODE> subdirectory of the
directory upon which ACL2 is installed.  You may obtain the gzipped Postscript
form of the documentation by clicking <A HREF=
\"http://www.cs.utexas.edu/users/moore/publications/acl2-book.ps.gz\">here (1.4
MB)</A>.

<BR><HR><BR>
<H2><A NAME=\"Tools\">Mathematical Tools</A></H2>

The distribution of ACL2 includes some tools built by users.  Most of
these are ACL2 ``<A
HREF=\"http://www.cs.utexas.edu/users/moore/acl2/~s3/distrib/acl2-sources/books/Readme.html\">books</A>,''
which are collections of definitions and theorems you might find
useful in your models and proofs.  Most of the available books come with the distribution, but
the above link will also take you to additional books in support of the ACL2
Workshops (``<code>workshops</code>'') and non-standard analysis
(``<code>nonstd</code>'').

<p>

We strongly encourage users to submit additional books by following the <A
HREF=\"http://www.cs.utexas.edu/users/moore/acl2/books/index.html\">instructions for contributing books to ACL2</A>.

<p>

We also distribute a few interface
tools, such as support for infix printing.  For these, see the <A
HREF=\"http://www.cs.utexas.edu/users/moore/publications/acl2-papers.html#Utilities\">Utilities</A>
section of <A HREF=
\"http://www.cs.utexas.edu/users/moore/publications/acl2-papers.html\">
Books and Papers about ACL2 and Its Applications</A>.  Some of the
papers mentioned in that collection contain utilities, scripts, or
ACL2 books the problem domains in question.


<BR><HR><BR><BR><BR><BR><BR><BR>
</BODY>
</HTML>
")

(defconst *home-page-references*
  '(|The Tours|                         ;;; a
    ACL2-Tutorial                       ;;; b  ; This is not used right now.
    events                              ;;; c
    programming                         ;;; d
    rule-classes                        ;;; e
    books                               ;;; f
    note-3-1                            ;;; g
    the-method                          ;;; h
  ))

(deflabel |Pages Written Especially for the Tours|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Pages Written Especially for the Tours~/

  The ACL2 Home Page is generated from ACL2's online documentation strings.
  (How else could we achieve the total integration of ACL2's online
  documentation with the home page?)  This page is just an artifact of the
  structure of our documentation strings:  each string must belong to a ``major
  section'' of the documentation database.  This page is not structured to be
  used by a person browsing via the Web.  It contains, in an arbitrary order,
  the pages written specificially for the Web user.

  Furthermore, browsing the pages below via the ACL2 :DOC command or
  via TexInfo is often unsatisfying because those browsers do not
  support gif files and the notion of going ``back'' to a node just
  visited.  If you wish to look at the pages below, we strongly
  recommend that you do so via a HTML-based Web browser.  Indeed, you
  should simply visit ACL2's Home Page and take one of the Tours.~/

  Generally, the topics listed above will not be of use to the ACL2 user.")

(deflabel |Undocumented Topic|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  Undocumented Topic~/~/
  This topic has not yet been documented.  Sorry")

(deflabel |Common Lisp|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Common Lisp~/~/
  
  ~gif[common-lisp.gif]

  The logic of ACL2 is based on Common Lisp.

  Common Lisp is the standard list processing programming language.
  It is documented in:  Guy L. Steele, ~b[Common Lisp The Language],
  Digital Press, 12 Crosby Drive, Bedford, MA 01730, 1990.  See
  also http://www.cs.cmu.edu/Web/Groups/AI/html/cltl/cltl2.html.

  ACL2 formalizes only a subset of Common Lisp.  It includes such
  familiar Lisp functions as ~c[cons], ~c[car] and ~c[cdr] for creating
  and manipulating list structures, various arithmetic primitives such
  as ~c[+], ~c[*], ~c[expt] and ~c[<=], and ~c[intern] and ~c[symbol-name] for
  creating and manipulating symbols.  Control primitives include
  ~c[cond], ~c[case] and ~c[if], as well as function call, including
  recursion.  New functions are defined with ~c[defun] and macros with
  ~c[defmacro].  ~l[programming] ~warn[] for a list of the Common
  Lisp primitives supported by ACL2.

  ACL2 is a very small subset of full Common Lisp.  ACL2 does not
  include the Common Lisp Object System (CLOS), higher order
  functions, circular structures, and other aspects of Common Lisp
  that are ~b[non-applicative].  Roughly speaking, a language is
  applicative if it follows the rules of function application.  For
  example, ~c[f(x)] must be equal to ~c[f(x)], which means, among other
  things, that the value of ~c[f] must not be affected by ``global
  variables'' and the object ~c[x] must not change over time.

  ~click-here[|An Example Common Lisp Function Definition|] for a simple example of Common Lisp.

  ~walk[|An Example Common Lisp Function Definition|]")

(deflabel |An Example Common Lisp Function Definition|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  An Example Common Lisp Function Definition~/~/

  Consider the binary trees ~c[x] and ~c[y] below.

  ~terminal[x = (list 'a 'b)    y = (list 'c 'd 'e)]
  ~gif[binary-trees-x-y.gif]

  In Lisp, ~c[x] is written as the list ~c['(A B)] or, equivalently, as
  ~c['(A B . NIL)].  Similarly, ~c[y] may be written ~c['(C D E)].
  Suppose we wish to replace the right-most tip of ~c[x]
  by the entire tree ~c[y].   This is denoted ~c[(app x y)], where ~c[app]
  stands for ``append''.

  ~gif[binary-trees-app.gif]

  We can define ~c[app] with:
  ~bv[]
  ~b[(defun app (x y)]                           ~i[; Concatenate x and y.]
    ~b[(declare (type (satisfies true-listp) x))]~i[; We expect x to end in NIL.]
    ~b[(cond ((endp x) y)]                       ~i[; If x is empty, return y.]
          ~b[(t (cons (car x)]                   ~i[; Else, copy first node]
                   ~b[(app (cdr x) y)))))]       ~i[;  and recur into next.]
  ~ev[]

  If you defined this function in some Common Lisp, then to run
  ~c[app] on the ~c[x] and ~c[y] above you could then type
  ~bv[]
  (app '(A B) '(C D E))
  ~ev[]
  and Common Lisp will print the result ~c[(A B C D E)].

  ~walk[|An Example of ACL2 in Use|]")

(deflabel |The Tours|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The Tours~/~/

  ACL2 is a very large, multipurpose system.  You can use it as a
  programming language, a specification language, a modeling language,
  a formal mathematical logic, or a semi-automatic theorem prover,
  just to name its most common uses.

  This home page includes all of ACL2's online documentation, which is
  quite extensive.  To help ease your introduction to ACL2, we have
  built two tours through this documentation.

  Newcomers to ACL2 should first take the ``Flying Tour.''  Then, if
  you want to know more, take the ``Walking Tour.''

  To start a tour, click on the appropriate icon below.

  ~fly[|A Flying Tour of ACL2|] ~walk[|A Walking Tour of ACL2|]

  For readers using Web browsers:  This ``page'' actually contains many
  other pages of our documentation, organized alphabetically and separated
  by many blank lines.  Be careful when using the scroll bar!

  For readers using our :DOC or our TexInfo format in Emacs:  The tours
  will probably be unsatisfying because we use gif files and assume you
  can navigate ``back.''")

(deflabel |A Flying Tour of ACL2|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  A Flying Tour of ACL2~/~/
  
  ~gif[large-flying.gif]

  On this tour you will learn a little about what ACL2 is for rather than how
  ACL2 works.  At the bottom of the ``page'' (which may extend beyond the end
  of your screen) there is a small ``flying tour'' icon.  Click on it to go to
  the next page of the tour.

  The tour visits the following topics sequentially.

  ~bf[]
  ~b[The Flight Plan]
  * ~il[|About the ACL2 Home Page| This Documentation]
  * ~il[|What Is ACL2(Q)| What is ACL2?]
  * ~il[|What is a Mathematical Logic(Q)| Mathematical Logic]
  * ~il[|What is a Mechanical Theorem Prover(Q)| Mechanical Theorem Proving]
  * ~il[|About Models| Mathematical Models in General]
  * ~il[|Models of Computer Hardware and Software| Mathematical Models of Computing Machines]
       ~il[|A Typical State| Formalizing Models]
       ~il[|Running Models| Running Models]
       ~il[|Symbolic Execution of Models| Symbolic Execution of Models]
       ~il[|Proving Theorems about Models| Proving Theorems about Models]
  * Requirements of ACL2
       ~il[|What is Required of the User(Q)| The User's Skills]
       ~il[|How Long Does It Take to Become an Effective User(Q)| Training]   
       ~il[|Other Requirements| Host System]
  ~ef[] 

  We intend the tour to take about 10 minutes of your time.
  Some pages on the tour contain pointers to other documents.
  You need not follow these pointers to stay on the tour.

  ~fly[|About the ACL2 Home Page|]")

(deflabel |About the ACL2 Home Page|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  About the ACL2 Home Page~/~/

  The ACL2 Home Page is integrated into the ACL2 online documentation.
  Over 1.5 megabytes of text is available here.

  The vast majority of the text
  is user-level documentation.  For example, to find out about ~il[rewrite]
  ~warn[] rules you could click on the word ``~il[rewrite] ~warn[].''
  ~b[But before you do that] remember that you must then use your browser's
  ~b[back] commands to come back here.

  The tiny warning signs ~warn[] mark pointers that lead out of the
  introductory-level material and into the user documentation.  If you are
  taking the Flying Tour, we advise you to ~b[avoid] following such pointers the
  first time; it is easy to get lost in the full documentation unless
  you are pursuing the answer to a specific question.  If you do wander down
  these other paths, remember you can ~b[back] out to a page containing the
  Flying Tour icon to resume the tour.

  At the end of the tour you will have a chance to return to
  ~il[|A Flying Tour of ACL2| The Flight Plan] where you can revisit the main stops of
  the Flying Tour and explore the alternative paths more fully.

  Finally, every page contains two icons at the bottom.  The ACL2 icon leads
  you back to the ACL2 Home Page.  The Index icon allows you to browse an
  alphabetical listing of all the topics in ACL2's online documentation.

  ~fly[|What Is ACL2(Q)|]")

(deflabel |A Walking Tour of ACL2|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  A Walking Tour of ACL2~/~/
  
  ~gif[large-walking.gif]

  On this tour you will learn a little more about the ACL2 logic, the theorem
  prover, and the user interface.

  This time we will stick with really simple things, such as the
  associativity of list concatenation.

  We assume you have taken the Flying Tour but that you did not necessarily
  follow all the ``off-tour'' links.  So this tour may revisit some pages
  you've seen.  Just click on the Walking Tour icon at the bottom of each
  page.

  On this tour you will see many more links marked ~warn[].  We would
  like to ~b[discourage] you from following these links right
  now.  However we ~b[encourage] you to ~b[note them].  Basically, the
  ~warn[] sign here illustrates the documentation and introduces you to
  its main entry points.  Once you have started to ~b[use] ACL2 you can
  take the Walking Tour again but pursue more of the indicated links.

  ~walk[|Common Lisp|]")

(deflabel |What Is ACL2(Q)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  What Is ACL2?~/~/

  ACL2 is a ~b[mathematical logic] together with a
  ~b[mechanical theorem prover]  to help you reason in the logic.

  The logic is just a subset of applicative ~il[|Common Lisp| Common Lisp].

  The theorem prover is an ``industrial strength'' version of the Boyer-Moore
  theorem prover, Nqthm.

  ~b[Models] of all kinds of computing systems can be built in ACL2, just as
  in Nqthm, even though the formal logic is Lisp.

  Once you've built an ACL2 model of a system, you can run it.

  You can also use ACL2 to prove theorems about the model.

  ~fly[|What is a Mathematical Logic(Q)|]")

(deflabel |About Models|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  About Models~/~/

  ACL2 is used to construct mathematical models of computer hardware
  and software (i.e., ``digital systems'').

  ~gif[computing-machine.gif]

  A ~b[mathematical model] is a set of mathematical formulas used to
  predict the behavior of some artifact.

  The use of mathematical models allows ~b[faster] and ~b[cheaper] delivery
  of ~b[better] systems.

  Models need not be ~b[complete] or ~b[perfectly accurate] to be useful to the
  trained engineer.

  ~click-here[|Models in Engineering|] for more discussion of these assertions in an
  engineering context.

  ~fly[|Models of Computer Hardware and Software|]")

(deflabel |Models in Engineering|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Models in Engineering~/~/

  ~gif[bridge.gif]

  Frequently, engineers use mathematical models.  Use of such
  models frequently lead to

  ~b[better designs],

  ~b[faster completion of acceptable products], and

  ~b[reduced overall cost],

  because models allow the trained user to study the design before it
  is built and analyze its properties.  Usually, testing and analyzing
  a model is cheaper and faster than fabricating and refabricating the
  product.

  ~gif[bridge-analysis.gif]

  ~Click-here[|The Falling Body Model|] to continue.")

(deflabel |The Falling Body Model|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The Falling Body Model~/~/

  ~gif[pisa.gif]

  One particularly famous and very simple model is the equation of a
  falling body:  the distance d an object falls is proportional to the
  square of the time t.  If the time is measured in seconds and the
  distance in feet, the equation relating these two is
  ~bv[]
         2
  d = 16t
  ~ev[]  

  This equation is a ~b[model] of falling objects.  It can be used to
  predict how long it takes a cannonball to fall from the top of a 200
  foot tower (3.5 seconds).  This might be important if your product
  is designed to drop cannonballs on moving targets.

  ~click-here[|Corroborating Models|] to continue")

(deflabel |Corroborating Models|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Corroborating Models~/~/

  After producing a model, it must be ~b[corroborated] against
  reality.  The Falling Body Model has been corroborated by a vast
  number of experiments in which the time and distance were measured
  and compared according to the formula.  In general all models must
  be corroborated by experiment.

  The Falling Body Model can be derived from deeper models, namely
  Newton's laws of motion and the assertion that, over the limited
  distances concerned, graviation exerts a constant acceleration on
  the object.  When the model in question can be derived from other
  models, it is the other models that are being corroborated by our
  experiments.

  Because nature is not formal, we cannot ~b[prove] that our models
  of it are correct.  All we can do is test our models against
  nature's behavior.

  Such testing often exposes restrictions on the applicability of
  our models.  For example, the Falling Body Model is inaccurate
  if air resistance is significant.  Thus, we learn not to use that
  model to predict how long it takes a feather to fall from a 200
  foot tower in the earth's atmosphere.

  In addition, attempts at corroboration might reveal that the model
  is actually incorrect.  Careful measurements might expose the fact
  that the gravitational force increases as the body falls closer to
  earth.  Very careful measurements might reveal relativistic effects.
  Technically, the familiar Falling Body Model is just wrong, even
  under excessive restrictions such as ``in a perfect vacuum'' and
  ``over small distances.''  But it is an incredibly useful model
  nonetheless.

  There are several morals here.

  ~b[Models need not be complete to be useful.]

  ~b[Models need not be perfectly accurate to be useful.]

  ~b[The user of a model must understand its limitations.]

  ~fly[|Models of Computer Hardware and Software|]")

(deflabel |Models of Computer Hardware and Software|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Models of Computer Hardware and Software~/~/

  ~gif[computing-machine.gif]~par[]

  Computing machines, whether hardware or software or some combintation,
  are frequently modeled as ``state machines.''

  To so model a computing machine we must represent its ~b[states] as objects
  in our mathematical framework.

  ~b[Transitions] are functions or relations on state objects.

  In what language shall we define these objects, functions, and relations?

  The mathematical languages we were taught in high school

  ~b[algebra],

  ~b[geometry],

  ~b[trignometry], and

  ~b[calculus]

  are inappropriate for modeling digital systems.  They primarily let us talk
  about numbers and continuous functions.

  To see what kind of expressive power we need, take a closer look at
  what a typical state contains.

  ~fly[|A Typical State|]")

(deflabel |A Typical State|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  A Typical State~/~/

  ~gif[state-object.gif]

  Observe that the states in typical models talk about
  
  ~bf[]
  ~b[booleans]    ~b[integers]   ~b[vectors]     ~b[records]   ~b[caches]
  ~b[bits]        ~b[symbols]    ~b[arrays]      ~b[stacks]    ~b[files]
  ~b[characters]  ~b[strings]    ~b[sequences]   ~b[tables]    ~b[directories]
  ~ef[]

  These objects are ~b[discrete] rather than ~b[continuous];
  furthermore they are built incrementally or ~b[inductively] by
  repeatedly using primitive operations to put together smaller pieces.

  The functions we need to manipulate these objects do things like
  ~b[concatenate], ~b[reverse], ~b[sort], ~b[search], ~b[count], etc.

  ~fly[|Functions for Manipulating these Objects|]")

(deflabel |Functions for Manipulating these Objects|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Functions for Manipulating these Objects~/~/

  Consider a typical ``stack'' of control frames.

  ~gif[stack.gif]

  Suppose the model required that we express the idea of ``the most recent
  frame whose return program counter points into ~c[MAIN].''

  The natural expression of this notion involves

  ~b[function application] -- ``fetch the ~c[return-pc] of this frame''

  ~b[case analysis] --  ``~b[if] the pc is ~c[MAIN], ~b[then] ...''

  ~b[iteration] or ~b[recursion] -- ``pop this frame off and repeat.''

  The designers of ACL2 have taken the position that a ~b[programming]
  ~b[language] is the natural language in which to define such notions,
  ~b[provided] the language has a mathematical foundation so that
  models can be analyzed and properties derived logically.

  ~gif[common-lisp.gif] is the language supported by ACL2.
  ~click-here[|Common Lisp|] for an optional and very brief introduction
  to Common Lisp.

  ~fly[|Modeling in ACL2|]")

(deflabel |Common Lisp as a Modeling Language|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Common Lisp as a Modeling Language~/~/

  ~gif[common-lisp.gif]

  In ACL2 we have adopted Common Lisp as the basis of our modeling language.
  If you have already read our brief note on Common Lisp and recall
  the example of ~c[app], please proceed.  Otherwise
  ~pclick-here[|Common Lisp|] for an exceedingly brief introduction to
  Common Lisp and then come ~b[back] here.

  In Common Lisp it is very easy to write systems of formulas that
  manipulate discrete, inductively constructed data objects.  In building
  a model you might need to formalize the notion of sequences and define
  such operations as concatenation, length, whether one is a permutation
  of the other, etc.  It is easy to do this in Common Lisp.  Furthermore,
  if you have a Common Lisp ``theory of sequences'' you can ~b[run]
  the operations and relations you define.  That is, you can execute
  the functions on concrete data to see what results your formulas produce.

  If you define the function ~c[app] as shown above and then type
  ~bv[]
  (app '(A B) '(C D E))
  ~ev[]
  in any Common Lisp, the answer will be computed and will be
  ~c[(A B C D E)].

  The ~b[executable] nature of Common Lisp and thus of ACL2 is very handy
  when producing models.  

  But executability is not enough for a modeling language because the purpose
  of models is to permit analysis.  

  ~click-here[|Analyzing Common Lisp Models|] to continue.")

(deflabel |Analyzing Common Lisp Models|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Analyzing Common Lisp Models~/~/

  To analyze a model you must be able to reason about the operations
  and relations involved.  Perhaps, for example, some aspect of the
  model depends upon the fact that the concatenation operation is associative.

  In any Common Lisp you can confirm that
  ~bv[]
  (app '(A B) (app '(C D) '(E F)))
  ~ev[]
  and
  ~bv[]
  (app (app '(A B) '(C D)) '(E F)))
  ~ev[]
  both evaluate to the same thing, ~c[(A B C D E F)].

  But what distinguishes ACL2 (the logic) from applicative Common Lisp (the
  language) is that in ACL2 you can ~b[prove] that the concatenation
  function ~c[app] is associative when its arguments are true-lists,
  whereas in Common Lisp all you can do is test that proposition.

  That is, in ACL2 it makes sense to say that the following
  formula is a ``theorem.''
  ~bv[]
  ~b[Theorem] Associativity of App
  (implies (and (true-listp a)
                (true-listp b))
           (equal (app (app a b) c)
                  (app a (app b c))))
  ~ev[]

  Theorems about the properties of models are proved by symbolically
  manipulating the operations and relations involved.  If the
  concatenation of sequences is involved in your model, then you may
  well need the theorem above in order to that your model has some
  particular property.")

(deflabel |What is a Mathematical Logic(Q)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  What is a Mathematical Logic?~/~/

  ~gif[proof.gif]

  A mathematical logic is a formal system of formulas (~b[axioms]) and
  ~b[rules] for deriving other formulas, called ~b[theorems].

  A ~b[proof] is a derivation of a theorem.  To see a concrete proof
  tree, ~pclick-here[|A Trivial Proof|].

  Why should you care?  The neat thing about Theorems is that they are
  ``true.''  More precisely, if all the axioms are valid and the rules
  are validity preserving, then anything derived from the axioms via the
  rules is valid.

  So, if you want to determine if some formula is true, ~b[prove it].

  ~fly[|What is a Mechanical Theorem Prover(Q)|]")


(deflabel |A Trivial Proof|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  A Trivial Proof~/~/

  ~Terminal[This doc string displays a picture of a trivial proof.]

  ~gif[concrete-proof.gif]")

(deflabel |What is a Mechanical Theorem Prover(Q)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  What is a Mechanical Theorem Prover?~/~/

  A ~b[mechanical theorem prover] is a computer program that finds
  proofs of theorems.

  ~gif[automatic-theorem-prover.gif]

  The ideal mechanical theorem prover is ~b[automatic]:  you give
  it a formula and it gives you a proof of that formula or tells you
  there is no proof.

  Unfortunately, automatic theorem provers can be built only for very
  simple logics (e.g., ~b[propositional calculus]) and even then
  practical considerations (e.g., how many
  centuries you are willing to wait) limit the problems they can solve.

  ~fly[|What is a Mechanical Theorem Prover(Q) (cont)|]")

(deflabel |What is a Mechanical Theorem Prover(Q) (cont)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  What is a Mechanical Theorem Prover? (cont)~/~/
  
  To get around this, mechanical theorem provers often require help from
  the ~b[user].

  ~gif[interactive-theorem-prover-a.gif]

  ~click-here[|ACL2 as an Interactive Theorem Prover|] to continue downward.

  ~fly[|About Models|]")

(deflabel |ACL2 as an Interactive Theorem Prover|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  ACL2 as an Interactive Theorem Prover~/~/

  The ACL2 theorem prover finds proofs in the ACL2 logic.  It can be
  automatic.  But most often the user must help it.

  ~gif[interactive-theorem-prover.gif]

  The user usually guides ACL2 by suggesting that it first prove key
  ~b[lemmas].  Lemmas are just theorems used in the proofs of other theorems.

  ~click-here[|ACL2 as an Interactive Theorem Prover (cont)|] to continue.")
  
(deflabel |ACL2 as an Interactive Theorem Prover (cont)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  ACL2 as an Interactive Theorem Prover (cont)~/~/

  Theorems, lemmas, definitions, and advice of various sorts can be
  stored in ~il[books] ~warn[].

  ~gif[open-book.gif]

  When a theorem or definition is stored in a book, the user can
  specify ~b[how] it should be used in the future.  When viewed this
  way, theorems and definitions are thought of as ~b[rules].

  The ACL2 theorem prover is ~b[rule driven].  The rules are
  obtained from books.

  ~click-here[|ACL2 System Architecture|] to continue.

  ~walk[|ACL2 System Architecture|]")

(deflabel |ACL2 System Architecture|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  ACL2 System Architecture~/~/

  ~gif[acl2-system-architecture.gif]

  The user interacts with the theorem prover by giving it definitions,
  theorems and advice (e.g., ``use this lemma.''), often in the form
  of books ~il[books] ~warn[].

  The theorem prover uses the rules in the library of books the user has
  selected.

  The theorem prover prints its proof attempts to the user.

  When a theorem is proved it is converted to a rule under the control
  of the user's ~il[rule-classes] ~warn[].

  ~b[The informed user can make ACL2 do amazing things.]

  ~walk[|Rewrite Rules are Generated from DEFTHM Events|]")

(deflabel |Modeling in ACL2|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Modeling in ACL2~/~/

  ~gif[computing-machine-a.gif]~par[]

  Below we define ~b[mc(s,n)] to be the function that ~b[single-step]s ~b[n]
  times from a given starting state, ~b[s].
  In Common Lisp, ``mc(s,n)'' is written ~c[(mc s n)].

  ~bv[]
  ~b[(defun mc (s n)]                     ; To step ~b[s] ~b[n] times:
   ~b[(if (zp n)]                         ; If ~b[n] is 0
       ~b[s]                              ;    then return ~b[s]
       ~b[(mc (single-step s) (- n 1))))] ;    else step ~b[single-step(s)]
                                                        ~b[n-1] times.
  ~ev[]

  This is an example of a formal model in ACL2.

  ~fly[|Running Models|]")

(deflabel |Running Models|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Running Models~/~/

  Suppose the machine being modeled is some kind of arithmetic unit.
  Suppose the model can be initialized so as to ~b[multiply] ~b[x] times
  ~b[y] and leave the answer in ~b[z].  Then if we initialize ~b[s] to
  ~b[multiply] with ~b[x=5] and ~b[y=7] and run the machine long enough, we can
  read the answer ~b[35] in the final state.~par[]
  
  ~gif[computing-machine-5x7.gif]~par[]

  Because ACL2 is a programming language, our
  model can be ~b[run] or ~b[executed].

  If you defined the model in ACL2 and then typed

  ~bv[]
  > (lookup 'z (mc (s 'mult 5 7) 29))
  ~ev[]

  then ACL2 would compute 35.  You can ~b[emulate] or ~b[test] the
  model of your machine.

  This is ~b[obvious] because ACL2 is Common Lisp; and Common Lisp is a
  ~b[programming language].

  ~fly[|Symbolic Execution of Models|]")

(deflabel |Symbolic Execution of Models|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Symbolic Execution of Models~/~/

  But ACL2 is more than a programming language.  

  Initialize ~b[x] to 5 and let ~b[y] be ~b[any legal value].~par[]
  
  ~gif[computing-machine-5xy.gif]~par[]

  Because ACL2 is a mathematical language, we can simplify the expression

  ~bv[]
  (lookup 'z (mc (s 'mult 5 y) 29))
  ~ev[]

  and get (+ y y y y y).  This is ~b[symbolic execution] because not all
  of the parameters are known.

  ~fly[|Proving Theorems about Models|]")

(deflabel |Proving Theorems about Models|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Proving Theorems about Models~/~/

  But ACL2 is a ~b[logic].  We can ~b[prove theorems about the model].~par[]

  ~gif[computing-machine-xxy.gif]~par[]

  ~bv[]
  ~b[Theorem.  MC 'mult is a multiplier]
  (implies (and (natp x)
                (natp y))
           (equal (lookup 'z (mc (s 'mult x y) (mclk x)))
                  (* x y))).
  ~ev[]

  This theorem says that a certain program running on the ~b[mc] machine
  will correctly multiply ~b[any two natural numbers].

  It is a statement about an ~b[infinite] number of test cases!

  We know it is true about the model because we ~b[proved] it.

  ~fly[|What is Required of the User(Q)|]")

(deflabel |What is Required of the User(Q)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  What is Required of the User?~/~/
  
  It is not easy to build ACL2 models of complex systems.  To do so, the
  user must ~b[understand]
  ~bf[]
    * the system being modeled, and

    * ACL2, at least as a programming language.
  ~ef[]

  It is not easy to get ACL2 to prove hard theorems.  To do so, the user must
  ~b[understand]
  ~bf[]
    * the model,

    * ACL2 as a mathematical logic, and

    * be able to construct a proof (in interaction with ACL2).
  ~ef[]
  ACL2 will help construct the proof but its primary role is to prevent
  logical mistakes.  The creative burden -- the mathematical insight into
  ~b[why the model has the desired property] -- is the user's responsibility.

  ~fly[|How Long Does It Take to Become an Effective User(Q)|]")

(deflabel |How Long Does It Take to Become an Effective User(Q)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  How Long Does It Take to Become an Effective User?~/~/
  
  The training time depends primarily on the background of the user.

  We expect that a user who
  ~bf[]
    * has a bachelor's degree in ~b[computer science] or ~b[mathematics],

    * has some experience with ~b[formal methods],

    * has had some exposure to ~b[Lisp] programming and is ~b[comfortable] 
      with the Lisp ~b[notation],

    * is ~b[familiar] with and has ~b[unlimited access] to a Common Lisp
      ~b[host] processor, ~b[operating system], and ~b[text editor] (we
      use Sun workstations running Unix and GNU Emacs),

    * is willing to read and study the ~b[ACL2 documentation], and

    * is given the opportunity to start with ``~b[toy]'' projects ~b[before]
      being expected to tackle the company's Grand Challenge,
  ~ef[]
  will probably take ~b[several months] to become an effective ACL2 user.

  ~fly[|Other Requirements|]")

(deflabel |Other Requirements|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Other Requirements~/~/
  
  ACL2 is distributed on the Web without fee.

  There is a ~b[License] agreement, which is available via the ACL2 Home
  Page link below.

  ACL2 currently runs on ~b[Unix], ~b[Linux], ~b[Windows], and
  ~b[Macintosh] operating systems.

  It can be built in any of the following Common Lisps:
  ~bf[]
    * ~b[Allegro],
    * ~b[GCL] (Gnu Common Lisp),
    * ~b[Lispworks],
    * ~b[CLISP],
    * ~b[CMU Common Lisp],
    * ~b[SBCL],
    * ~b[OpenMCL], and
    * ~b[MCL] (Macintosh Common Lisp).
  ~ef[]

  ~fly[|The End of the Flying Tour|]") 

(deflabel |The End of the Flying Tour|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The End of the Flying Tour~/~/
  
  ~gif[landing.gif]~par[]

  This completes the Flying Tour.

  You may wish now to go back and revisit selected nodes of the Flying Tour
  so that you can explore some of the branches not on the Tour.  You can
  do so via ~il[|A Flying Tour of ACL2| The Flight Plan].  These branches
  mainly provide some background and motivational material, rather than
  details of ACL2.

  If you would like to learn more about ACL2 itself, we recommend that
  you now take the walking Tour.  You may do so by clicking on the
  Walking Tour icon below.

  Thanks.~nl[]
  Matt Kaufmann and J Moore

  ~walk[|A Walking Tour of ACL2|]")

(deflabel |An Example of ACL2 in Use|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  An Example of ACL2 in Use~/

  To introduce you to ACL2 we will consider the ~c[app] function discussed
  in the ~il[|Common Lisp| Common Lisp] page, ~b[except] we will omit
  for the moment the ~b[declare] form, which in ACL2 is called a ~b[guard].
    We will deal with guards later.  

  Here is the definition again
  ~bv[]
  ~b[(defun app (x y)]
    ~b[(cond ((endp x) y)]
          ~b[(t (cons (car x) ]
                   ~b[(app (cdr x) y)))))]
  ~ev[]~/
  
  ~par[]

  The next few stops along the Walking Tour will show you
  ~bf[]
    * how to use the ACL2 documentation,
    * what happens when the above definition is submitted to ACL2,
    * what happens when you evaluate calls of ~c[app],
    * what one simple theorem about ~c[app] looks like, 
    * how ACL2 proves the theorem, and
    * how that theorem can be used in another proof.
  ~ef[]
  Along the way we will talk about the ~b[definitional principle], ~b[types],
  the ACL2 ~b[read-eval-print loop], and how the ~b[theorem prover] works.

  When we complete this part of the tour we will introduce the notion of
  ~b[guards] and revisit several of the topics above in that context.

  ~walk[|How To Find Out about ACL2 Functions|]")

(deflabel |How To Find Out about ACL2 Functions|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  How To Find Out about ACL2 Functions~/~/
  
  Most ACL2 primitives are documented.  Here is the definition of
  ~c[app] again, with the documented topics highlighted.  ~warn[] All of the
  links below lead into the ACL2 online documentation, 1.5 megabytes of
  hyperlinked text.  So follow these links around, but remember to come
  back here!
  ~bv[]
  (~il[defun] app (x y)
    (~il[cond] ((~il[endp] x) y)
          (t (~il[cons] (~il[car] x)
                   (app (~il[cdr] x) y)))))
  ~ev[]

  By following the link on ~il[endp] ~warn[] we see that it is a
  Common Lisp function and is defined to be the same as ~il[atom]
  ~warn[], which recognizes non-conses.  But ~c[endp] has a guard.
  Since we are ignorning guards for now, we'll ignore the guard issue
  on ~c[endp].

  So this definition reads ``to ~c[app] ~c[x] and ~c[y]:  if ~c[x] is an
  atom, return ~c[y]; otherwise, ~c[app] ~c[(cdr x)] and ~c[y] and then
  cons ~c[(car x)] onto that.''

  ~walk[|How To Find Out about ACL2 Functions (cont)|]")

(deflabel |How To Find Out about ACL2 Functions (cont)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  How To Find Out about ACL2 Functions (cont)~/~/

  You can always use the Index ~warn[] to find the documentation of
  functions.  Try it.  Click on the Index icon below.  Then use the
  Find command of your browser to find ``endp'' in that document and
  follow the link.  But remember to come back here.

  The ACL2 documentation is also available via Emacs' TexInfo, allowing
  you to explore the hyperlinked documentation in the comfort of a text
  editor that can also interact with ACL2.

  In addition, runtime images of ACL2 have the hyperlinked text as a large ACL2
  data structure that can be explored with ACL2's ~b[:doc] command.  If you
  have ACL2 running, try the command ~b[:doc endp].

  Another way to find out about ACL2 functions, if you have an ACL2
  image available, is to use the command :~ilc[args] ~warn[] which
  prints the formals, type, and guard of a function symbol.

  Of course, the ACL2 documentation can also be printed out as 700 page
  book.  See the ACL2 Home Page to download the Postscript.

  Now let's continue with ~c[app].

  ~walk[|The Admission of App|]")

(deflabel |The Admission of App|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  The Admission of App~/~/

  Here is what it looks like to submit the definition of ~c[app] to ACL2:

  ~gif[green-line.gif]

  ~bv[]
  ACL2 !>~b[(defun app (x y)]
    ~b[(cond ((endp x) y)]
          ~b[(t (cons (car x) ]
                   ~b[(app (cdr x) y)))))]

  The admission of APP is trivial, using the relation O< (which
  is known to be well-founded on the domain recognized by O-P)
  and the measure (ACL2-COUNT X).  We observe that the type of APP is
  described by the theorem (OR (CONSP (APP X Y)) (EQUAL (APP X Y) Y)).
  We used primitive type reasoning.

  Summary
  Form:  ( DEFUN APP ...)
  Rules: ((:FAKE-RUNE-FOR-TYPE-SET NIL))
  Warnings:  None
  Time:  0.03 seconds (prove: 0.00, print: 0.00, other: 0.03)
   APP
  ~ev[]

  ~gif[green-line.gif]

  The text between the lines above is one interaction with the ACL2 command
  loop.

  Above you see the user's ~b[input] and how the system responds.
  This little example shows you what the syntax looks like and is a
  very typical ~b[successful] interaction with the definitional
  principle.

  Let's look at it a little more closely.

  ~walk[|Revisiting the Admission of App|]")

(deflabel |A Tiny Warning Sign|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
   
  A Tiny Warning Sign~/

   ~/

   ~gif[warning.gif]

   This warning sign, which usually appears as ``~gif[twarning.gif]'',
   indicates that the link it marks takes you into ACL2's online
   documentation.

   The documentation is a vast graph of documented topics intended to
   help the ~em[user] of ACL2 rather than the ~em[potential user].  If
   you are exploring ACL2's home page to learn about the system,
   perhaps you should go back rather than follow the link marked with
   this sign.  But you are welcome to explore the online documentation
   as well.  Good luck.")

(deflabel |Revisiting the Admission of App|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  Revisiting the Admission of App~/~/

  ~terminal[This :DOC item is useful only in HTML settings.]  Here is the
  definition of ~c[app] again with certain parts highlighted.  If you
  are taking the Walking Tour, please read the text carefully and
  click on each of the links below, ~b[except those marked] ~warn[].
  Then come ~b[back] here.

  ~gif[green-line.gif]

  ~bv[]
  ~il[|About the Prompt| ACL2 !>]~b[(defun app (x y)]
    ~b[(cond ((endp x) y)]
          ~b[(t (cons (car x) ]
                   ~b[(app (cdr x) y)))))]


  The ~il[|About the Admission of Recursive Definitions| admission] of APP is trivial, using the
  relation ~il[O<] ~warn[] (which is known to be well-founded on
  the domain recognized by ~il[O-P] ~warn[]) and the measure
  (~il[ACL2-COUNT] ~warn[] X).  We ~il[|Guessing the Type of a Newly Admitted Function| observe] that the
  ~il[|About Types| type] of APP is described by the theorem (OR
  (CONSP (APP X Y)) (EQUAL (APP X Y) Y)).  We used primitive type
  reasoning.

  ~il[|The Event Summary| Summary]
  Form:  ( DEFUN APP ...)
  Rules: ((:FAKE-RUNE-FOR-TYPE-SET NIL))
  Warnings:  None
  Time:  0.03 seconds (prove: 0.00, print: 0.00, other: 0.03)
   APP
  ~ev[]

  ~gif[green-line.gif]

  ~walk[|Evaluating App on Sample Input|]")

(deflabel |Evaluating App on Sample Input|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  Evaluating App on Sample Input~/~/

  ~gif[green-line.gif]

  ~bv[]
  ACL2 !>~b[(app nil '(x y z))]
  (X Y Z)

  ACL2 !>~b[(app '(1 2 3) '(4 5 6 7))]
  (1 2 3 4 5 6 7)

  ACL2 !>~b[(app '(a b c d e f g) '(x y z))]   ; ~pclick-here[|Conversion|] for an explanation
  (A B C D E F G X Y Z)

  ACL2 !>~b[(app (app '(1 2) '(3 4)) '(5 6))]
  (1 2 3 4 5 6)

  ACL2 !>~b[(app '(1 2) (app '(3 4) '(5 6)))]
  (1 2 3 4 5 6)

  ACL2!>~b[(let ((a '(1 2))]
              ~b[(b '(3 4))]
              ~b[(c '(5 6)))]
          ~b[(equal (app (app a b) c)]
                 ~b[(app a (app b c))))]
  T
  ~ev[]

  ~gif[green-line.gif]

  As we can see from these examples, ACL2 functions can be executed more or
  less as Common Lisp.  

  The last three examples suggest an interesting property of ~c[app].

  ~walk[|The Associativity of App|]")

(deflabel |Conversion|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  Conversion to Uppercase~/

  When symbols are read by Common Lisp they are converted to upper case.
  Note carefully that this remark applies to the characters in ~em[symbols].
  The characters in strings are not converted upper case.~/

  To type a symbol containing lower case characters you can enclose the
  symbol in vertical bars, as in ~c[|AbC|] or you can put a ``backslash''
  before each lower case character you wish to preserve, as in ~c[A\\bC].
  ~c[|AbC|] and ~c[A\\bC] are two different ways of writing the same
  symbol (just like 2/4 and 1/2 are two different ways of writing the
  same rational).  The symbol has three characters in its name, the
  middle one of which is a lower case b.")

#|
  

  ACL2 !>~b[(app '(a b c) 27)]  ; ~pclick-here[|ACL2 is an Untyped Language|] for an explanation.
  (A B C . 27)

  ACL2 !>~b[(app 7 27)]   ; ~pclick-here[|Hey Wait!  Is ACL2 Typed or Untyped(Q)|] for an explanation.

  ACL2 Error in TOP-LEVEL:  The guard for the function symbol ENDP, which
  is (OR (CONSP X) (EQUAL X NIL)), is violated by the arguments in the
  call (ENDP 7).

  ACL2 !>~b[:set-guard-checking nil]  ; ~pclick-here[|Undocumented Topic|] for an explanation.

  ACL2 >~b[(app 7 27)]  ; ~pclick-here[|Undocumented Topic|] for an explanation.
  27
|#

(deflabel |The Associativity of App|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  The Associativity of App~/~/

  ~gif[green-line.gif]

  ~bv[]
  ACL2!>~b[(let ((a '(1 2))]
              ~b[(b '(3 4))]
              ~b[(c '(5 6)))]
          ~b[(equal (app (app a b) c)]
                 ~b[(app a (app b c))))]
  T
  ~ev[]

  ~gif[green-line.gif]

  Observe that, for the particular ~c[a], ~c[b], and ~c[c] above,
  ~c[(app (app a b) c)] returns the same thing as ~c[(app a (app b c))].
  Perhaps ~c[app] is ~b[associative].  Of course, to be associative means
  that the above property must hold for all values of ~c[a], ~c[b], and ~c[c],
  not just the ones ~b[tested] above.

  Wouldn't it be cool if you could type
  ~bv[]
  ACL2!>~b[(equal (app (app a b) c)]
               ~b[(app a (app b c)))]
  
  ~ev[]
  and have ACL2 compute the value ~c[T]?  Well, ~b[you can't!]  If you try
  it, you'll get an error message!  The message says we can't evaluate
  that form because it contains ~b[free] variables, i.e., variables
  not given values.  ~click-here[|Free Variables in Top-Level Input|] to see the
  message.

  We cannot evaluate a form on an infinite number of cases.  But we can
  prove that a form is a theorem and hence know that it will always
  evaluate to true.

  ~walk[|The Theorem that App is Associative|]")

(deflabel |The Theorem that App is Associative|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  The Theorem that App is Associative~/~/
  ~bv[]
  ACL2!>~b[(defthm associativity-of-app]
          ~b[(equal (app (app a b) c)]
                 ~b[(app a (app b c))))]
  ~ev[]

  The formula above says ~c[app] is associative.  The ~ilc[defthm] ~warn[]
  command instructs ACL2 to prove the formula and to name it 
  ~c[associativity-of-app].  Actually, the ~c[defthm] command also
  builds the formula into the data base as a ~ilc[rewrite] ~warn[] rule,
  but we won't go into that just yet.

  What we will consider is how the ACL2 theorem prover proves this formula.

  If you proceed you will find the actual output of ACL2 in response to
  the command above.  Some of the text is highlighted for the purposes of
  the tour.  ACL2 does not highlight its output.  

  You will note that we sometimes highlight a single open parenthesis.  This
  is our way of drawing your attention to the subformula that begins with
  that parenthesis.  By clicking on the parenthesis you will get an
  explanation of the subformula or its processing.

  ~walk[|The Proof of the Associativity of App|]") ; uaap= Unguarded-App-Assoc-Proof

(deflabel |Free Variables in Top-Level Input|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Free Variables in Top-Level Input~/~/

  ~bv[]
  ACL2 !>~b[(equal (app (app a b) c)]
                ~b[(app a (app b c))))]
  
  ACL2 Error in TOP-LEVEL:  Global variables, such as C, B, and A, are 
  not allowed. See :DOC ASSIGN and :DOC @.

  ~ev[]

  ACL2 does not allow ``global variables'' in top-level input.  There is no
  ``top-level binding environment'' to give meaning to these variables.

  Thus, expressions involving no variables can generally be evaluated,
  ~bv[]
  ACL2 !>~b[(equal (app (app '(1 2) '(3 4)) '(5 6))]
                ~b[(app '(1 2) (app '(3 4) '(5 6))))]
  (1 2 3 4 5 6)
  ~ev[]
  but expressions containing variables cannot.

  There is an exception to rule.  References to ``single-threaded
  objects'' may appear in top-level forms.  ~l[stobj] ~warn[].  A
  single-threaded object is an ACL2 object, usually containing many
  fields, whose use is syntactically restricted so that it may be
  given as input only to certain functions and must be returned as
  output by certain functions.  These restrictions allow single-
  threaded objects to be efficiently manipulated.  For example, only a
  single copy of the object actually exists, even though from a
  logical perspective one might expect the object to be ``copied on
  write.''

  The most commonly used single-threaded object in ACL2 is the ACL2
  system state, whose current value is always held in the variable
  ~ilc[state] ~warn[].

  ACL2 provides a way for you to use ~c[state] to save values of
  computations at the top-level and refer to them later.  See
  ~il[assign] ~warn[] and ~il[@] ~warn[].")

(deflabel |The Proof of the Associativity of App|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The Proof of the Associativity of App~/~/

  Here is the theorem prover's output when it processes the ~b[defthm]
  command for the associativity of ~c[app].  We have highlighted text
  for which we offer some explanation, and broken the presentation into
  several pages.  Just follow the Walking Tour after exploring the
  explanations.

  ~gif[green-line.gif]

  ~bv[]
  ACL2!>~b[(defthm associativity-of-app]
          ~b[(equal (app (app a b) c)]
                 ~b[(app a (app b c))))]

  Name the formula above ~il[|Name the Formula Above| *1].

  ~il[|Perhaps| Perhaps] we can prove *1 by induction.  Three induction schemes are
  ~il[|Suggested Inductions in the Associativity of App Example| suggested] by this conjecture.  ~il[|Subsumption of Induction Candidates in App Example| Subsumption] reduces that number to two.
  However, one of these is ~il[|Flawed Induction Candidates in App Example| flawed] and so we are left with one viable
  candidate.  

  We will induct according to a scheme suggested by (APP A B).  If we
  let  (:P A B C) denote *1 above then the induction scheme we'll use
  is
  ~il[|The Induction Scheme Selected for the App Example| (]AND
     ~il[|The Induction Step in the App Example| (]IMPLIES (AND (NOT (ENDP A))
                   (:P (CDR A) B C))
              (:P A B C))
     ~il[|The Base Case in the App Example| (]IMPLIES (ENDP A) (:P A B C))).
  This induction is ~il[|The Justification of the Induction Scheme| justified] by the same argument used to admit APP,
  namely, the measure (ACL2-COUNT A) is decreasing according to the relation
  O< (which is known to be well-founded on the domain recognized
  by O-P).  When ~il[|The Instantiation of the Induction Scheme| applied] to the goal at hand the above induction
  scheme produces the following two ~il[|Nontautological Subgoals| nontautological subgoals].

  ~ev[]

  ~walk[|Overview of the Simplification of the Induction Step to T|]")

(deflabel |Name the Formula Above|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Name the Formula Above~/~/

  When the theorem prover explicitly assigns a name, like ~c[*1], to a
  formula, it has decided to prove the formula by induction.")

(deflabel |Perhaps|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Perhaps~/~/

  The theorem prover's proof is printed in real time.  At the time it
  prints ``Perhaps'' it does not know the proof will succeed.")  

(deflabel |Suggested Inductions in the Associativity of App Example|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Suggested Inductions in the Associativity of App Example~/~/

  To find a plausible induction argument, the
  system studies the recursions exhibited by the terms in the conjecture.

  Roughly speaking, a call of a recursive function ``suggests'' an
  induction if the argument position decomposed in recursion is occupied
  by a variable.

  In this conjecture, three terms suggest inductions:

  ~bv[]
  (app ~b[a] b)

  (app ~b[b] c)

  (app ~b[a] (app b c))
  ~ev[]
  The variable recursively decomposed is indicated in ~b[bold].")

(deflabel |Subsumption of Induction Candidates in App Example|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Subsumption of Induction Candidates in App Example~/~/

  After collecting induction suggestions from these three terms
  ~bv[]
  (app ~b[a] b)

  (app ~b[b] c)

  (app ~b[a] (app b c))
  ~ev[]
  the system notices that the first and last suggest the same decomposition
  of ~c[a].  So we are left with two ideas about how to induct:

  Decompose ~b[a] as we would to unwind (app ~b[a] b).

  Decompose ~b[b] as we would to unwind (app ~b[b] c).")

(deflabel |Flawed Induction Candidates in App Example|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Flawed Induction Candidates in App Example~/~/

  Induction on ~b[a] is unflawed:  every occurrence of ~b[a] in
  the conjecture

  ~bv[]
  (equal (app (app ~b[a] b) c)
         (app ~b[a] (app b c)))
  ~ev[]
  is in a position being recursively decomposed!

  Now look at the occurrences of ~c[b].  The first (shown in ~b[bold] below)
  is in a position that is held constant in the recursion of ~c[(app a b)].
  It would be ``bad'' to induct on ~c[b] here.

  ~bv[]
  (equal (app (app a ~b[b]) c)
         (app a (app b c)))
  ~ev[]")

(deflabel |The Induction Scheme Selected for the App Example|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The Induction Scheme Selected for the App Example~/~/

  ~bv[]
  (AND
     (IMPLIES (AND (NOT (ENDP A))         ; Induction Step: test 
                   (:P (CDR A) B C))      ;  and induction hypothesis
              (:P A B C))                 ;  implies induction conclusion.
     (IMPLIES (ENDP A) (:P A B C)))       ; Base Case 
  ~ev[]

  The formula beginning with this parenthesis is the induction scheme
  suggested by ~c[(APP A B)] applied to ~c[(P A B C)].

  It is a ~b[conjunction] (~ilc[AND] ~warn[]) of two formulas.

  The first is the ~b[induction step] and the second is the ~b[base case].")

(deflabel |The Induction Step in the App Example|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The Induction Step in the App Example~/~/

  This formula is the ~b[Induction Step].  It basically consists of
  three parts, a test identifying the inductive case, an induction
  hypothesis and an induction conclusion.

  ~bv[]
  (IMPLIES (AND (NOT (ENDP A))      ~b[; Test]
                (:P (CDR A) B C))   ~b[; Induction Hypothesis]
           (:P A B C))              ~b[; Induction Conclusion]
  ~ev[]
  
  When we prove this we can assume

  ~bf[]
    * ~c[A] is not empty, and that

    * the associativity conjecture holds for a ``smaller'' version of
      ~c[A], namely, ~c[(CDR A)].
  ~ef[]

  Under those hypotheses we have to prove the associativity conjecture
  for ~c[A] itself.")

(deflabel |The Base Case in the App Example|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The Base Case in the App Example~/~/

  This formula is the ~b[Base Case].  It consists of two parts, a test
  identifying the non-inductive case and the conjecture to prove.

  ~bv[]
  (IMPLIES (ENDP A)                 ~b[; Test]
           (:P A B C))              ~b[; Conjecture]
  ~ev[]
  
  When we prove this we can assume

  ~bf[]
    * ~c[A] is empty
  ~ef[]
  and we have to prove the conjecture for ~c[A].")

(deflabel |The Justification of the Induction Scheme|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The Justification of the Induction Scheme~/~/

  This paragraph explains why the induction selected is legal.  The
  explanation is basically the same as the explanation for why the
  recursion in ~c[(APP A B)] terminates.
  ")

(deflabel |The Instantiation of the Induction Scheme|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The Instantiation of the Induction Scheme~/~/

  The induction scheme just shown is just an abbreviation for our
  real goal.

  To obtain our actual goals we have to replace the schema ~c[:P] by
  the associativity conjecture (instantiated as shown in the scheme).

  This produces two actual goals, the induction step and the base case. ")

(deflabel |Nontautological Subgoals|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Prover output omits some details~/~/

  The theorem prover's proof output is intended to suggest an outline
  of the reasoning process employed by its proof engine, which is
  virtually always more than is necessary for the ACL2 user.  In
  particular, the output often omits subgoals that are sufficiently
  trivial, including tautologies.")

(deflabel |Overview of the Simplification of the Induction Step to T|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Overview of the Simplification of the Induction Step to T~/~/

  ~bv[]
  ~il[|On the Naming of Subgoals| Subgoal *1/2]
  (IMPLIES (AND (NOT (ENDP A))
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL (APP (APP A B) C)
                  (APP A (APP B C)))).

  By the simple :definition ~il[|Overview of the Expansion of ENDP in the Induction Step| ENDP] we reduce the conjecture to

  Subgoal *1/2'
  (IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL (APP (APP A B) C)
                  (APP A (APP B C)))).

  ~il[|Overview of the Simplification of the Induction Conclusion| But] simplification reduces this to T, using the :definition APP, the
  :rewrite rules CDR-CONS and CAR-CONS and primitive type reasoning.
  ~ev[]

  ~walk[|Overview of the Simplification of the Base Case to T|]")
  
(deflabel |On the Naming of Subgoals|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  On the Naming of Subgoals~/~/

  ~c[Subgoal *1/2] is the ~b[induction step] from the scheme, obtained by
  instantiating the scheme with our conjecture.

  We number the cases ``backward'', so this is case ``2'' of the
  proof of ``*1''.  We number them backward so you can look at a subgoal
  number and get an estimate for how close you are to the end.")

(deflabel |Overview of the Expansion of ENDP in the Induction Step|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  Overview of the Expansion of ENDP in the Induction Step~/~/

  In this message the system is saying that ~c[Subgoal *1/2] has been
  rewritten to the ~c[Subgoal *1/2'], by expanding the definition of ~b[endp].
  This is an example of ~b[simplification], one of the main proof
  techniques used by the theorem prover.

  ~click-here[|The Expansion of ENDP in the Induction Step (Step 0)|] if you would like to step through the
  simplification of ~c[Subgoal *1/2].")

(deflabel |The Expansion of ENDP in the Induction Step (Step 0)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  The Expansion of ENDP in the Induction Step (Step 0)~/~/

  ~bv[]
  Subgoal *1/2
  (IMPLIES (AND (NOT ~il[|The Expansion of ENDP in the Induction Step (Step 1)| (]ENDP A))
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL (APP (APP A B) C)
                  (APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above (the open parenthesis before ~c[ENDP])
  to replace ~c[(ENDP A)] by its definition.")

(deflabel |The Expansion of ENDP in the Induction Step (Step 1)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  The Expansion of ENDP in the Induction Step (Step 1)~/~/

  ~bv[]
  Subgoal *1/2
  (IMPLIES (AND ~il[|The Expansion of ENDP in the Induction Step (Step 2)| (]NOT ~b[(NOT (CONSP A))])
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL (APP (APP A B) C)
                  (APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  The ~b[bold] text is the instantiated definition of ~c[ENDP].

  Now click on the link above to simplify (NOT (NOT (CONSP A)))")

(deflabel |The Expansion of ENDP in the Induction Step (Step 2)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  The Expansion of ENDP in the Induction Step (Step 2)~/~/

  ~bv[]
  Subgoal *1/2'
  (IMPLIES (AND ~b[(CONSP A)]
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL (APP (APP A B) C)
                  (APP A (APP B C)))).

  ~ev[]

  ~gif[green-line.gif]

  Note that this is ~c[Subgoal *1/2'.]

  You may ~pclick-here[|Overview of the Simplification of the Induction Step to T|] to return to the main proof.")

(deflabel |Overview of the Simplification of the Induction Conclusion|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  Overview of the Simplification of the Induction Conclusion~/~/

  In this message the system is saying that ~c[Subgoal *1/2'] has been
  rewritten T using the rules noted.  The word ``~b[But]'' at the
  beginning of the sentence is a signal that the goal has been proved.

  ~Click-here[|The Simplification of the Induction Conclusion (Step 0)|] to step through the proof of ~c[Subgoal *1/2'].")

(deflabel |The Simplification of the Induction Conclusion (Step 0)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 0)~/~/

  ~bv[]
  Subgoal *1/2'
  (IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL (APP ~il[|The Simplification of the Induction Conclusion (Step 1)| (]APP A B) C)
                  (APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to replace ~c[(APP A B)] by its definition.")
  
(deflabel |The Simplification of the Induction Conclusion (Step 1)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 1)~/~/

  ~bv[]
  Subgoal *1/2'
  (IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL (APP ~b[(IF ]~il[|The Simplification of the Induction Conclusion (Step 2)| (]~b[CONSP A)]
                           ~b[(CONS (CAR A) (APP (CDR A) B))]
                           ~b[B)]
                       C)
                  (APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  Note that the ~c[IF] expression above is the simplified body of ~c[APP].
  But we know the test ~c[(CONSP A)] is true, by the first hypothesis.
  Click on the link above to replace the test by ~c[T].  Actually
  this step and several subsequent ones are done during the simplification
  of the body of ~c[APP] but we want to illustrate the basic principles of
  simplification without bothering with every detail.")

(deflabel |The Simplification of the Induction Conclusion (Step 2)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 2)~/~/

  ~bv[]
  Subgoal *1/2'
  (IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL (APP ~il[|The Simplification of the Induction Conclusion (Step 3)| (]IF ~b[T]
                           (CONS (CAR A) (APP (CDR A) B))
                           B)
                       C)
                  (APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to apply the Axiom ~c[(IF T x y) = x].")

(deflabel |The Simplification of the Induction Conclusion (Step 3)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 3)~/~/

  ~bv[]
  Subgoal *1/2'
  (IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL ~il[|The Simplification of the Induction Conclusion (Step 4)| (]APP ~b[(CONS (CAR A) (APP (CDR A) B))]
                       C)
                  (APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to apply the expand the definition of ~c[APP] here.")

(deflabel |The Simplification of the Induction Conclusion (Step 4)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 4)~/~/

  ~bv[]
  Subgoal *1/2'
  (IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL (IF ~il[|The Simplification of the Induction Conclusion (Step 5)| (]~b[CONSP (CONS (CAR A) (APP (CDR A) B)))]
                      ~b[(CONS (CAR (CONS (CAR A) (APP (CDR A) B)))]
                            ~b[(APP (CDR (CONS (CAR A) (APP (CDR A) B)))]
                                 ~b[C))]
                      ~b[C)]
                  (APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to apply the Axiom ~c[(CONSP (CONS x y)) = T].")

(deflabel |The Simplification of the Induction Conclusion (Step 5)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 5)~/~/

  ~bv[]
  Subgoal *1/2'
  (IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL (IF ~b[T]
                      (CONS ~il[|The Simplification of the Induction Conclusion (Step 6)| (]CAR (CONS (CAR A) (APP (CDR A) B)))
                            (APP (CDR (CONS (CAR A) (APP (CDR A) B)))
                                 C))
                      C)
                  (APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to apply the Axiom ~c[(CAR (CONS x y)) = x].")

(deflabel |The Simplification of the Induction Conclusion (Step 6)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 6)~/~/

  ~bv[]
  Subgoal *1/2'
  (IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL (IF T
                      (CONS ~b[(CAR A)]
                            (APP ~il[|The Simplification of the Induction Conclusion (Step 7)| (]CDR (CONS (CAR A) (APP (CDR A) B)))
                                 C))
                      C)
                  (APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to apply the Axiom ~c[(CDR (CONS x y)) = y].")

(deflabel |The Simplification of the Induction Conclusion (Step 7)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 7)~/~/

  ~bv[]
  Subgoal *1/2'
  (IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL ~il[|The Simplification of the Induction Conclusion (Step 8)| (]IF T
                      (CONS (CAR A)
                            (APP ~b[(APP (CDR A) B)]
                                 C))
                      C)
                  (APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to apply the Axiom ~c[(IF T x y) = x].")

(deflabel |The Simplification of the Induction Conclusion (Step 8)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 8)~/~/

  ~bv[]
  Subgoal *1/2'
  (IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           (EQUAL ~b[(CONS (CAR A)]
                        ~b[(APP (APP (CDR A) B)]
                             ~b[C))]
                  ~il[|The Simplification of the Induction Conclusion (Step 9)| (]APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to expand the definition of ~c[APP] here.
  This time, we'll do the whole expansion at once, including the
  simplification of the resulting ~c[IF].  This is how ACL2 actually
  does it.")

(deflabel |The Simplification of the Induction Conclusion (Step 9)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 9)~/~/

  ~bv[]
  Subgoal *1/2'
  (IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           ~il[|The Simplification of the Induction Conclusion (Step 10)| (]EQUAL (CONS (CAR A)
                        (APP (APP (CDR A) B)
                             C))
                  ~b[(CONS (CAR A)]
                        ~b[(APP (CDR A) (APP B C))]))).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to apply the Axiom that 
  ~c[(EQUAL (CONS x y) (CONS u v))] is equal to the conjunction of
  ~c[(EQUAL x u)] and ~c[(EQUAL y v)].  In this case, ~c[(EQUAL x u)]
  is trivial, ~c[(EQUAL (CAR A) (CAR A))].")

(deflabel |The Simplification of the Induction Conclusion (Step 10)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 10)~/~/

  ~bv[]
  Subgoal *1/2'
  (IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           ~il[|The Simplification of the Induction Conclusion (Step 11)| (]~b[EQUAL (APP (APP (CDR A) B) C)]
                  ~b[(APP (CDR A) (APP B C)))]).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to use the Induction Hypothesis (which is the
  second of the two hypotheses above and which is identical to the rewritten
  conclusion).")

(deflabel |The Simplification of the Induction Conclusion (Step 11)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 11)~/~/

  ~bv[]
  Subgoal *1/2'
  ~il[|The Simplification of the Induction Conclusion (Step 12)| (]IMPLIES (AND (CONSP A)
                (EQUAL (APP (APP (CDR A) B) C)
                       (APP (CDR A) (APP B C))))
           ~b[T])
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to use the definition of ~c[IMPLIES].  Since
  the conclusion of the implication is now identically ~c[T], the
  implication simplifies to ~t[T].")

(deflabel |The Simplification of the Induction Conclusion (Step 12)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Simplification of the Induction Conclusion (Step 12)~/~/

  ~bv[]
  Subgoal *1/2'
  ~b[T]
  ~ev[]

  ~gif[green-line.gif]

  So, indeed, ~c[Subgoal *1/2'] ~b[does] simplify to T!

  You can see that even in an example as simple as this one, quite
  a lot happens in simplification.

  You may ~pclick-here[|Overview of the Simplification of the Induction Step to T|] to return to the main proof.")

(deflabel |Overview of the Simplification of the Base Case to T|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Overview of the Simplification of the Base Case to T~/~/

  ~bv[]
  ~il[|Overview of the Expansion of ENDP in the Base Case| Subgoal *1/1]
  (IMPLIES (ENDP A)
           (EQUAL (APP (APP A B) C)
                  (APP A (APP B C)))).

  By the simple :definition ENDP we reduce the conjecture to

  Subgoal *1/1'
  (IMPLIES (NOT (CONSP A))
           (EQUAL (APP (APP A B) C)
                  (APP A (APP B C)))).

  ~il[|Overview of the Final Simplification in the Base Case| But] simplification reduces this to T, using the :definition APP and
  primitive type reasoning.

  ~ev[]

  ~walk[|The End of the Proof of the Associativity of App|]")

(deflabel |Overview of the Expansion of ENDP in the Base Case|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  Overview of the Expansion of ENDP in the Base Case~/~/

  ~c[Subgoal *1/1] is the ~b[Base Case] of our induction.  It
  simplifies to ~c[Subgoal *1/1'] by expanding the ~b[ENDP] term in the
  hypothesis, just as we saw in the earlier proof of ~c[Subgoal *1/2].")

(deflabel |Overview of the Final Simplification in the Base Case|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  Overview of the Final Simplification in the Base Case~/~/

  The ~b[But] is our signal that the goal is proved.

  ~click-here[|The Final Simplification in the Base Case (Step 0)|] to step through the proof.  It is very simple.")

(deflabel |The Final Simplification in the Base Case (Step 0)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Final Simplification in the Base Case (Step 0)~/~/

  ~bv[]
  Subgoal *1/1'
  (IMPLIES (NOT (CONSP A))
           (EQUAL (APP ~il[|The Final Simplification in the Base Case (Step 1)| (]APP A B) C)
                  (APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to replace ~c[(APP A B)] by its definition.
  Note that the hypothesis ~c[(NOT (CONSP A))] allows us to simplify
  the ~c[IF] in ~c[APP] to its ~b[false branch] this time.")

(deflabel |The Final Simplification in the Base Case (Step 1)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Final Simplification in the Base Case (Step 1)~/~/

  ~bv[]
  Subgoal *1/1'
  (IMPLIES (NOT (CONSP A))
           (EQUAL (APP ~b[B] C)
                  ~ilc[|The Final Simplification in the Base Case (Step 2)| (]APP A (APP B C)))).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to expand the definition of ~c[APP].  Again,
  we come out through the false branch because of the hypothesis.")

(deflabel |The Final Simplification in the Base Case (Step 2)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Final Simplification in the Base Case (Step 2)~/~/

  ~bv[]
  Subgoal *1/1'
  (IMPLIES (NOT (CONSP A))
           ~il[|The Final Simplification in the Base Case (Step 3)| (]EQUAL (APP B C)
                  ~b[(APP B C)])).
  ~ev[]

  ~gif[green-line.gif]

  Click on the link above to use the Axiom ~c[(EQUAL x x) = t]")

(deflabel |The Final Simplification in the Base Case (Step 3)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  the Final Simplification in the Base Case (Step 3)~/~/

  ~bv[]
  Subgoal *1/1'
  (IMPLIES (NOT (CONSP A))
           ~b[T])
  ~ev[]

  ~gif[green-line.gif]

  Now that its conclusion is identically ~c[T] the ~c[IMPLIES]
  will simplify to ~c[T] (not shown) and we are done with ~c[Subgoal *1/1'].

  You may ~pclick-here[|Overview of the Simplification of the Base Case to T|] to return to the main proof.")

(deflabel |The End of the Proof of the Associativity of App|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The End of the Proof of the Associativity of App~/~/

  ~bv[]
  That ~il[|Popping out of an Inductive Proof| completes] the proof of *1.

  ~il[|The Q.E.D. Message| Q.E.D.]

  Summary
  Form:  ( DEFTHM ASSOCIATIVITY-OF-APP ...)
  ~il[|The Rules used in the Associativity of App Proof| Rules]: ((:REWRITE CDR-CONS)
          (:REWRITE CAR-CONS)
          (:DEFINITION NOT)
          (:DEFINITION ENDP)
          (:FAKE-RUNE-FOR-TYPE-SET NIL)
          (:DEFINITION APP))
  Warnings:  None
  Time:  0.27 seconds (prove: ~il[|The Time Taken to do the Associativity of App Proof| 0.10], print: 0.05, other: 0.12)
   ASSOCIATIVITY-OF-APP
  ~ev[]

  ~gif[green-line.gif]

  ~walk[|Guiding the ACL2 Theorem Prover|]")

(deflabel |Popping out of an Inductive Proof|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  Popping out of an Inductive Proof~/~/

  Recall that our induction scheme (~pclick-here[|The Proof of the Associativity of App|] to revisit it) 
  had two cases, the induction step (~c[Subgoal *1/2]) and the base
  case (~c[Subgoal *1/1]).  Both have been proved!")

(deflabel |The Q.E.D. Message|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  The Q.E.D. Message~/~/

  ~b[Q.E.D.] stands for ``quod erat demonstrandum'' which is
  Latin for ``which was to be demonstrated'' and is the signal that
  a proof is completely done.")

(deflabel |The Rules used in the Associativity of App Proof|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  The Rules used in the Associativity of App Proof~/~/

  Note that under ~b[Rules] we list the ~il[rune runes] ~warn[] of all
  the rules used in the proof.  This list says that we used the
  rewrite rules ~c[CAR-CONS] and ~c[CDR-CONS], the definitions of the
  functions ~c[NOT], ~c[ENDP] and ~c[APP], and primitive type reasoning
  (which is how we simplified the ~c[IF] and ~c[EQUAL] terms).

  For what it is worth, ~c[IMPLIES] and ~c[AND] are actually
  ~il[defmacro macros] ~warn[] that are expanded into ~c[IF]
  expressions before the proof ever begins.  The use of macros is not
  reported among the rules.")

(deflabel |The Time Taken to do the Associativity of App Proof|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  The Time Taken to do the Associativity of App Proof~/~/

  The time it took us to explain this proof may leave the impression that
  the proof is complicated.  In a way, it is.  But it happens quickly.

  The time taken to do this proof is about 1/10 second.  The rest of
  the time (about 2/10 seconds) is spent in pre- and post-processing.

  Basically, this proof flashes across your screen before you can read
  it; you see the ~b[Q.E.D.] and don't bother to scroll back to
  read it.  You have more important things to do than read successful
  proofs.")

(deflabel |Guiding the ACL2 Theorem Prover|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Guiding the ACL2 Theorem Prover~/~/

  Now that you have seen the theorem prover in action you might be
  curious as to how you guide it.

  ~gif[interactive-theorem-prover.gif]

  The idea is that the user submits conjectures and advice and
  reads the proof attempts as they are produced.

  Most of the time, the conjectures submitted are ~b[lemmas] to be
  used in the proofs of other theorems.

  ~walk[|ACL2 as an Interactive Theorem Prover (cont)|]")

(deflabel |Rewrite Rules are Generated from DEFTHM Events|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Rewrite Rules are Generated from DEFTHM Events~/~/

  By reading the documentation of ~ilc[defthm] ~warn[] (and
  especially of its :~il[rule-classes] ~warn[] argument)
  you would learn that
  when we submitted the command
  ~bv[]
  ~b[(defthm associativity-of-app]
    ~b[(equal (app (app a b) c)]
           ~b[(app a (app b c))))]

  ~ev[]
  we not only command the system to prove that ~c[app] is an associative
  function but
  ~bf[]
    * ~b[we commanded it to use that fact as a rewrite rule].
  ~ef[]

  That means that every time the system encounters a term of the form
  ~bv[]
  (app (app ~b[x] ~b[y]) ~b[z])
  ~ev[]
  it will replace it with 
  ~bv[]
  (app ~b[x] (app ~b[y] ~b[z]))!
  ~ev[]

  ~walk[|You Must Think about the Use of a Formula as a Rule|]")

(deflabel |You Must Think about the Use of a Formula as a Rule|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  You Must Think about the Use of a Formula as a Rule~/~/

  This is ~b[good] and ~b[bad].

  The good news is that you can ~b[program] ACL2's simplifier.

  The bad news is that when you command ACL2 to prove a theorem you must
  give some thought to ~b[how that theorem is to be used as a rule]!

  For example, if you engaged in the mathematically trivial act of proving
  the associativity rule again, but with the equality reversed, you
  would have programmed ACL2's rewriter to loop forever.

  You can avoid adding any rule by using the command:
  ~bv[]
  (defthm associativity-of-app
    (equal (app (app a b) c)
           (app a (app b c)))
    ~b[:rule-classes nil])
  ~ev[]

  ~walk[|Using the Associativity of App to Prove a Trivial Consequence|]")

(deflabel |Using the Associativity of App to Prove a Trivial Consequence|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Using the Associativity of App to Prove a Trivial Consequence~/~/

  If we have proved the ~c[associativity-of-app] rule, then the
  following theorem is trivial:

  ~bv[]
  (defthm trivial-consequence
    (equal (app (app (app (app x1 x2) (app x3 x4)) (app x5 x6)) x7)
           (app x1 (app (app x2 x3) (app (app x4 x5) (app x6 x7))))))
  ~ev[]
  Below we show the proof

  ~walk[|Overview of the Proof of a Trivial Consequence|]")

(deflabel |Overview of the Proof of a Trivial Consequence|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Overview of the Proof of a Trivial Consequence~/~/

  ~gif[green-line.gif]

  ~bv[]
  ACL2 !>~b[(defthm trivial-consequence]
           ~b[(equal (app (app (app (app x1 x2) (app x3 x4)) (app x5 x6)) x7)]
                  ~b[(app x1 (app (app x2 x3) (app (app x4 x5) (app x6 x7))))))]

  ~il[|The WARNING about the Trivial Consequence| ACL2 Warning] [Subsume] in ( DEFTHM TRIVIAL-CONSEQUENCE ...):  The previously
  added rule ASSOCIATIVITY-OF-APP subsumes the newly proposed :REWRITE
  rule TRIVIAL-CONSEQUENCE, in the sense that the old rule rewrites a
  more general target.  Because the new rule will be tried first, it
  may nonetheless find application.

  By the simple :rewrite rule ~il[|The First Application of the Associativity Rule| ASSOCIATIVITY-OF-APP] we reduce the conjecture
  to

  Goal'
  (EQUAL (APP X1
              (APP X2
                   (APP X3 (APP X4 (APP X5 (APP X6 X7))))))
         (APP X1
              (APP X2
                   (APP X3 (APP X4 (APP X5 (APP X6 X7))))))).

  But we reduce the conjecture to T, by primitive type reasoning.

  Q.E.D.

  Summary
  Form:  ( DEFTHM TRIVIAL-CONSEQUENCE ...)
  Rules: ((:REWRITE ASSOCIATIVITY-OF-APP)
          (:FAKE-RUNE-FOR-TYPE-SET NIL))
  Warnings:  ~il[|The Summary of the Proof of the Trivial Consequence| Subsume]
  Time:  0.20 seconds (prove: 0.02, print: 0.00, other: 0.18)
   TRIVIAL-CONSEQUENCE
  ~ev[]

  ~gif[green-line.gif]

  You might explore the links before moving on.

  ~walk[|The End of the Walking Tour|]")

(deflabel |The WARNING about the Trivial Consequence|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The WARNING about the Trivial Consequence~/~/

  This ~b[Warning] alerts us to the fact that when treated as a
  ~b[rewrite] rule, the new rule ~c[TRIVIAL-CONSEQUENCE], rewrites
  terms of the same form as a rule we have already proved, namely
  ~c[ASSOCIATIVITY-OF-APP].

  When you see this warning you should ~b[think about your rules]!

  In the current case, it would be a good idea ~b[not] to make
  ~c[TRIVIAL-CONSEQUENCE] a rule at all.  We could do this with
  :~ilc[rule-classes] ~warn[] nil.

  ACL2 proceeds to try to prove the theorem, even though it printed
  some warnings.  The basic assumption in ACL2 is that the ~b[user]
  ~b[understands what he or she is doing] but may need a little reminding
  just to manage a complicated set of facts.")

(deflabel |The First Application of the Associativity Rule|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The First Application of the Associativity Rule~/~/

  So here we see our associativity rule being used!

  The rewriter sweeps the conjecture in a ~b[leftmost innermost] fashion,
  applying rewrite rules as it goes.

  The associativity rule is used many times in this sweep.  The
  first ``target'' is highlighted below.  Click on it to see what
  happens:
  ~bv[]
  ~b[Current Conjecture]:
  (equal (app (app ~il[|A Sketch of How the Rewriter Works| (app (app x1 x2) (app x3 x4))] (app x5 x6)) x7)
         (app x1 (app (app x2 x3) (app (app x4 x5) (app x6 x7)))))
  ~ev[]")

(deflabel |A Sketch of How the Rewriter Works|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  A Sketch of How the Rewriter Works~/~/

  Below we show the first target term, extracted from the
  current conjecture.  Below it we show the associativity rule.

  ~gif[uaa-rewrite.gif]

  The variables of the rewrite rule are ~b[instantiated] so that the
  ~b[left-hand side] of the rule matches the target:

  ~bv[]
       variable          term from target
         a                     x1
         b                     x2
         c                     (app x3 x4)
  ~ev[]

  Then the target is ~b[replaced] by the instantiated ~b[right-hand side]
  of the rule.

  Sometimes rules have ~b[hypotheses].  To make a long story short, 
  if the rule has hypotheses, then after matching the left-hand side,
  the rewriter instantiates the hypotheses and rewrites them recursively.
  This is called ~b[backchaining].  If they all rewrite to true, then
  the target is replaced as above.

  For more details on how the ACL2 rewriter works, see Boyer and Moore's
  book ~b[A Computational Logic], Academic Press, 1979.")

(deflabel |The Summary of the Proof of the Trivial Consequence|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The Summary of the Proof of the Trivial Consequence~/~/

  Note that at the conclusion of the proof, the system reminds you
  of the earlier ~b[Warning].

  It is a good idea, when the ~b[Q.E.D.] flys by, to see if there were
  any Warnings.")

(deflabel |The End of the Walking Tour|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  The End of the Walking Tour~/~/

  ~gif[sitting.gif]

  This completes the Walking Tour.  

  We intend to document many other parts of the system this way, but
  we just haven't gotten around to it.

  If you feel like reading more, ~pl[tutorial-examples] in the documentation.
  There you will find several challenging but simple applications.  At the
  conclusion of the examples is a simple challenge to try.

  We hope you enjoy ACL2.  We do.

  Matt Kaufmann and J Strother Moore")

  

(deflabel |ACL2 is an Untyped Language|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  ACL2 is an Untyped Language~/~/

  The example
  ~bv[]
  ACL2 !>~b[(app '(a b c) 27)]
  (A B C . 27)
  ~ev[]
  illustrates the fact that ACL2's logic is untyped (~pclick-here[|About Types|]
  for a brief discussion of the typed versus untyped nature of the logic).

  The definition of ~c[app] makes no restriction of the arguments to lists.
  The definition says that if the first argument satisfies ~ilc[endp] ~warn[]
  then return the second argument.  In this example, when ~c[app] has recursed
  three times down the ~c[cdr] of its first argument, ~c['(a b c)], it
  reaches the final ~c[nil], which satisfies ~c[endp], and so 27 is returned.
  It is naturally consed into the emerging list as the function returns from
  successive recursive calls (since ~c[cons] does not require its arguments
  to be lists, either).  The result is an ``improper'' list, ~c[(a b c . 27)].

  You can think of ~c[(app x y)] as building a binary tree by replacing
  the right-most tip of the tree ~c[x] with the tree ~c[y].")

(deflabel |Hey Wait!  Is ACL2 Typed or Untyped(Q)|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Hey Wait!  Is ACL2 Typed or Untyped?~/~/

  The example
  ~bv[]
  ACL2 !>~b[(app 7 27)]

  ACL2 Error in TOP-LEVEL:  The guard for the function symbol ENDP, which
  is (OR (CONSP X) (EQUAL X NIL)), is violated by the arguments in the
  call (ENDP 7).
  ~ev[]
  illustrates the fact that while ACL2 is an untyped language the ACL2
  evaluator can be configured so as to check ``types'' at runtime.
  We should not say ``types'' here but ``guards.''  ~click-here[|Undocumented Topic|]
  for a discussion of guards.

  The guard on ~ilc[endp] ~warn[] requires its argument to be a true
  list.  Since 7 is not a true list, and since ACL2 is checking guards
  in this example, an error is signaled by ACL2.  How do you know
  ACL2 is checking guards?  Because the prompt tells us
  (~pclick-here[|About the Prompt|]) with its ``!''.")

(deflabel |Guards|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Guards~/~/

  Common Lisp functions are partial; they are not defined for all
  possible inputs.  But ACL2 functions are total.  Roughly speaking,
  the logical function of a given name in ACL2 is a ~b[completion] of
  the Common Lisp function of the same name obtained by adding some
  arbitrary but ``natural'' values on arguments outside the ``intended
  domain'' of the Common Lisp function.

  ACL2 requires that every ACL2 function symbol have a ``guard,''
  which may be thought of as a predicate on the formals of the
  function describing the intended domain.  The guard on the primitive
  function ~ilc[car] ~warn[], for example, is ~c[(or (consp x) (equal x nil))],
  which requires the argument to be either an ordered pair or
  ~c[nil].  We will discuss later how to specify a guard for a
  defined function; when one is not specified, the guard is ~c[t] which
  is just to say all arguments are allowed.

  ~b[But guards are entirely extra-logical]:  they are not involved in
  the axioms defining functions.  If you put a guard on a defined
  function, the defining axiom added to the logic defines the function
  on ~b[all] arguments, not just on the guarded domain.

  So what is the purpose of guards?  

  The key to the utility of guards is that we provide a mechanism,
  called ``guard verification,'' for checking that all the guards in a
  formula are true.  ~l[verify-guards].  This mechanism will attempt
  to prove that all the guards encountered in the evaluation of a
  guarded function are true every time they are encountered.  

  For a thorough discussion of guards, see the paper [km97] in the
  ACL2 ~il[bibliography].")

(deflabel |About the Prompt|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  
  About the Prompt~/

  The string ``~c[ACL2 !>]'' is the ACL2 prompt.~/

  The prompt tells the user that an ACL2 ~il[command] ~warn[]is expected.  In
  addition, the prompt tells us a little about the current state of
  the ACL2 command interpreter.  We explain the prompt briefly below.
  But first we talk about the command interpreter.

  An ACL2 command is generally a Lisp expression to be evaluated.
  There are some unusual commands (such as :~il[q] ~warn[] for ~b[quitting]
  ACL2) which cause other behavior.  But most commands are read,
  evaluated, and then have their results printed.  Thus, we call the
  command interpreter a ``read-eval-print loop.''  The ACL2 command
  interpreter is named ~ilc[LD] ~warn[] (after Lisp's ``load'').

  A command like ~b[(defun app (x y) ...)] causes ACL2 to evaluate the
  ~il[defun] ~warn[] function on ~b[app], ~b[(x y)] and ~b[...].  When
  that command is evaluated it prints some information to the terminal
  explaining the processing of the proposed definition.  It returns
  the symbol ~c[APP] as its value, which is printed by the command
  interpreter.  (Actually, ~c[defun] is not a function but a ~il[defmacro macro]
  which expands to a form that involves ~ilc[state] ~warn[], a necessary
  precondition to printing output to the terminal and to ``changing''
  the set of axioms.  But we do not discuss this further here.)

  The ~c[defun] command is an example of a special kind of command
  called an ``event.''  ~il[Events] ~warn[] are those commands that
  change the ``logical world'' by adding such things as axioms or
  theorems to ACL2's data base.  ~l[world] ~warn[].  But not
  every command is an event command.

  A command like ~b[(app '(1 2 3) '(4 5 6 7))] is an example of a
  non-event.  It is processed the same general way:  the function
  ~b[app] is applied to the indicated arguments and the result is
  printed.  The function ~b[app] does not print anything and does not
  change the ``world.''

  A third kind of command are those that display information about the
  current logical world or that ``backup'' to previous versions of the
  world.  Such commands are called ``~il[history]'' ~warn[] commands.

  What does the ACL2 prompt tell us about the read-eval-print loop?
  The prompt ``~c[ACL2 !>]'' tells us that the command will be read
  with ~ilc[current-package] ~warn[] set to ~c[\"ACL2\"], that guard checking
  (~pl[set-guard-checking] ~warn[]) is on (``~c[!]''), and that we are at
  the top-level (there is only one ``~c[>]'').
  For more about the prompt, ~pl[default-print-prompt] ~warn[].

  You should now return to ~il[|Revisiting the Admission of App| the Walking Tour].")

(deflabel |The Event Summary|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  The Event Summary~/~/

  At the conclusion of most events (~pclick-here[|About the Prompt|] for a
  brief discussion of events or ~pl[events] ~warn[]), ACL2 prints a
  summary.  The summary for ~c[app] is:
  ~bv[]
  Summary
  Form:  ( DEFUN APP ...)
  Rules: ((:FAKE-RUNE-FOR-TYPE-SET NIL))
  Warnings:  None
  Time:  0.03 seconds (prove: 0.00, print: 0.00, other: 0.03)
   APP
  ~ev[]

  The ``rules'' listed are those used in function admission or proof
  summarized.  What is actually listed are ``runes'' (~pl[rune]) ~warn[])
  which are list-structured names for rules in the ACL2 data base or
  ``~il[world]'' ~warn[].  Using ~il[theories] ~warn[] you can ``enable'' and
  ``disable'' rules so as to make them available (or not) to the ACL2
  theorem prover.

  The ``warnings'' mentioned (none are listed for ~c[app]) remind the
  reader whether the event provoked any warnings.  The warnings
  themselves would have been printed earlier in the processing and
  this part of the summary just names the earlier warnings printed.

  The ``time'' indicates how much processing time was used and is
  divided into three parts:  the time devoted to proof, to printing,
  and to syntactic checks, pre-processing and data base updates.
  Despite the fact that ACL2 is an applicative language it is possible
  to measure time with ACL2 programs.  The ~ilc[state] ~warn[] contains a
  clock.  The times are printed in decimal notation but are actually
  counted in integral units.

  The final ~c[APP] is the value of the ~c[defun] command and was
  printed by the read-eval-print loop.  The fact that it is indented
  one space is a subtle reminder that the command actually returned an
  ``error triple'', consisting of a flag indicating (in this case)
  that no error occurred, a value (in this case the symbol ~c[APP]),
  and the final ~ilc[state] ~warn[]).  ~l[ld-post-eval-print]
  ~warn[] for some details.  If you really want to follow that link,
  however, you might ~pl[ld] ~warn[] first.

  You should now return to ~il[|Revisiting the Admission of App| the Walking Tour].")

(deflabel |About the Admission of Recursive Definitions|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  About the Admission of Recursive Definitions~/

  You can't just add any formula as an axiom or definition and expect
  the logic to stay sound!  The purported ``definition'' of ~c[APP]
  must have several properties to be admitted to the logic as a new
  axiom~/

  The key property a recursive definition must have is that the recursion
  terminate.  This, along with some syntactic criteria, ensures us that there
  exists a function satisfying the definition.

  Termination must be proved before the definition is admitted.  This is
  done in general by finding a measure of the arguments of the function and
  a well-founded relation such that the arguments ``get smaller'' every time
  a recursive branch is taken.

  For ~c[app] the measure is the ``size'' of the first argument, ~c[x],
  as determined by the primitive function ~ilc[acl2-count] ~warn[].  The
  well-founded relation used in this example is ~ilc[o-p]
  ~warn[], which is the standard ordering on the ordinals less than
  ``epsilon naught.''  These particular choices for ~c[app] were made
  ``automatically'' by ACL2.  But they are in fact determined by
  various ``default'' settings.  The user of ACL2 can change the
  defaults or specify a ``hint'' to the ~ilc[defun] ~warn[] command to
  specify the measure and relation.

  You should now return to ~il[|Revisiting the Admission of App| the Walking Tour].")

(deflabel |About Types|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  About Types~/

  The universe of ACL2 objects includes objects of many different
  types.  For example, ~c[t] is a ``symbol'' and 3 is an ``integer.''
  Roughly speaking the objects of ACL2 can be partitioned into the
  following types:

  ~bf[]
  ~il[|Numbers in ACL2| Numbers]                   ~c[3, -22/7, #c(3 5/2)]
  ~il[|ACL2 Characters| Characters]                ~c[#\\A, #\\a, #\\Space]
  ~il[|ACL2 Strings| Strings]                   ~c[\"This is a string.\"]
  ~il[|ACL2 Symbols| Symbols]                   ~c['abc, 'smith::abc]
  ~il[|ACL2 Conses or Ordered Pairs| Conses (or Ordered Pairs)] ~c['((a . 1) (b . 2))]
  ~ef[]~/

   When proving theorems it is important to know the types of object
   returned by a term.  ACL2 uses a complicated heuristic algorithm,
   called ~ilc[type-set] ~warn[], to determine what types of objects a
   term may produce.  The user can more or less program the
   ~c[type-set] algorithm by proving ~ilc[type-prescription] ~warn[]
   rules.

   ACL2 is an ``untyped'' logic in the sense that the syntax is not
   typed:  It is legal to apply a function symbol of n arguments to any
   n terms, regardless of the types of the argument terms.  Thus, it is
   permitted to write such odd expressions as ~c[(+ t 3)] which sums the
   symbol ~c[t] and the integer 3.  Common Lisp does not prohibit such
   expressions.  We like untyped languages because they are simple to
   describe, though proving theorems about them can be awkward because,
   unless one is careful in the way one defines or states things,
   unusual cases (like ~c[(+ t 3)]) can arise.

   To make theorem proving easier in ACL2, the axioms actually define a
   value for such terms.  The value of ~c[(+ t 3)] is 3; under the ACL2
   axioms, non-numeric arguments to ~c[+] are treated as though they
   were 0.

   You might immediately wonder about our claim that ACL2 is Common
   Lisp, since ~c[(+ t 3)] is ``an error'' (and will sometimes even
   ``signal an error'') in Common Lisp.  It is to handle this problem that
   ACL2 has ~b[guards].  We will discuss guards later in the Walking Tour.
   However, many new users simply ignore the issue of guards entirely.

   You should now return to ~il[|Revisiting the Admission of App| the Walking Tour].")

(deflabel |Numbers in ACL2|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Numbers in ACL2~/~/

   The numbers in ACL2 can be partitioned into the following subtypes:
   ~bf[]
   Rationals
    Integers
     Positive integers                ~c[3]
     Zero                             ~c[0]
     Negative Integers                ~c[-3]
    Non-Integral Rationals
     Positive Non-Integral Rationals  ~c[19/3]
     Negative Non-Integral Rationals  ~c[-22/7]
   Complex Rational Numbers           ~c[#c(3 5/2) ; = 3+(5/2)i]
   ~ef[]

   Signed integer constants are usually written (as illustrated above)
   as sequences of decimal digits, possibly preceded by ~c[+] or ~c[-].
   Decimal points are not allowed.  Integers may be written in binary,
   as in ~c[#b1011] (= 23) and ~c[#b-111] (= -7).  Octal may also be
   used, ~c[#o-777] = -511.  Non-integral rationals are written as a
   signed decimal integer and an unsigned decimal integer, separated by
   a slash.  Complex rationals are written as #c(rpart ipart) where
   rpart and ipart are rationals.

   Of course, 4/2 = 2/1 = 2 (i.e., not every rational written with a slash
   is a non-integer).  Similarly, #c(4/2 0) = #c(2 0) = 2.

   The common arithmetic functions and relations are denoted by ~c[+],
   ~c[-], ~c[*], ~c[/], ~c[=], ~c[<], ~c[<=], ~c[>] and ~c[>=].  However there
   are many others, e.g., ~c[floor], ~c[ceiling], and ~c[lognot].  We
   suggest you ~pl[programming] ~warn[] where we list all of the primitive
   ACL2 functions.  Alternatively, see any Common Lisp language
   documentation.

   The primitive predicates for recognizing numbers are illustrated
   below.  The following ACL2 function will classify an object, x,
   according to its numeric subtype, or else return 'NaN (not a
   number).  We show it this way just to illustrate programming in
   ACL2.

   ~bv[]
   (defun classify-number (x)
     (cond ((rationalp x)
            (cond ((integerp x)
                   (cond ((< 0 x) 'positive-integer)
                         ((= 0 x) 'zero)
                         (t 'negative-integer)))
                  ((< 0 x) 'positive-non-integral-rational)
                  (t 'negative-non-integral-rational)))
           ((complex-rationalp x) 'complex-rational)
           (t 'NaN)))
   ~ev[]")

(deflabel |ACL2 Characters|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  ACL2 Characters~/~/

   ACL2 accepts 256 distinct characters, which are the characters
   obtained by applying the function ~ilc[code-char] ~warn[] to each
   integer from 0 to 255.  Among these, Common Lisp designates certain
   ones as ~c[*standard-characters*], namely those of the form
   ~c[(code-char n)] where n is from 33 to 126, together with
   ~c[#\\Newline] and ~c[#\\Space].  The actual standard characters may
   be viewed by evaluating the constant expression
   ~c[*standard-chars*].

   The standard character constants are written by writing a hash mark
   followed by a backslash (#\\) followed by the character.

   The function ~ilc[characterp] ~warn[] recognizes characters.  For more
   details, ~l[characters] ~warn[].")

(deflabel |ACL2 Strings|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  ACL2 Strings~/~/

  Strings of ACL2 ~il[|ACL2 Characters| characters] are written
  as sequences of characters delimited by ``double quotation marks'' (\").
  To put a double quotation mark in a string (or, any other character such
  as backslash or newline that seems to cause problems), escape it by preceding
  it with a backslash (\\).

  The function ~ilc[stringp] ~warn[] recognizes strings and ~ilc[char]
  ~warn[] will fetch the nth character of a string.  There are many
  other primitives for handling strings, such as ~ilc[string<] ~warn[] for
  comparing two strings lexicographically.  We suggest you
  ~l[programming] ~warn[] where we list all of the primitive ACL2
  functions.  Alternatively, see any Common Lisp language
  documentation.")

(deflabel |ACL2 Symbols|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|
  ACL2 Symbols~/~/

  Common Lisp's symbols are a data type representing words.  They are
  frequently regarded as atomic objects in the sense that they are not
  frequently broken down into their constituents.  Often the only
  important properties of symbols is that they are not numbers,
  characters, strings, or lists and that two symbols are not equal if
  they look different (!).  Examples of symbols include ~c[PLUS] and
  ~c[SMITH::ABC].  All function and variable names in ACL2 are symbols.
  When symbols are used as constants they must be quoted, as in
  ~c['PLUS].

  The symbol ~c[T] is commonly used as the Boolean ``true.''  The
  symbol ~c[NIL] is commonly used both as the Boolean ``false'' and as
  the ``empty list.''  Despite sometimes being called the ``empty
  list'' ~c[NIL] is a ~b[symbol] not an ``empty cons.''  Unlike other
  symbols, ~c[T] and ~c[NIL] may be used as constants without quoting
  them.

  Usually, symbols are written as sequences of alphanumeric characters
  other than those denoting numbers.  Thus, ~c[A12], ~c[+1A] and ~c[1+]
  are symbols but ~c[+12] is a number.  Roughly speaking, when symbols
  are read lower case characters are converted to upper case, so we
  frequently do not distinguish ~c[ABC] from ~c[Abc] or ~c[abc].
  ~click-here[|Conversion|] for information about case conversion
  when symbols are read.  However, any character can be used in a
  symbol, but some characters must be ``escaped'' to allow the Lisp
  reader to parse the sequence as a symbol.  For example, ~c[|Abc|] is
  a symbol whose first character is capitalized and whose remaining
  characters are in lower case.  ~c[|An odd duck|] is a symbol
  containing two #\\Space characters.  See any Common Lisp documentation
  for the syntactic rules for symbols.

  Technically, a symbol is a special kind of pair consisting of a
  package name (which is a string) and a symbol name (which is also a
  string).  (~l[symbol-package-name] ~warn[] and ~pl[symbol-name]
  ~warn[].)  The symbol SMITH::ABC is said to be in package \"SMITH\"
  and to have the symbol name \"ABC\".  The symbol ~c[ABC] in package
  \"SMITH\" is generally not equal to the symbol ~c[ABC] in package
  \"JONES\".  However, it is possible to ``import'' symbols from one
  package into another one, but in ACL2 this can only be done when the
  package is created.  (~l[defpkg] ~warn[].)  If the
  ~ilc[current-package] ~warn[] is \"SMITH\" then ~c[SMITH::ABC] may be
  more briefly written as just ~c[ABC].  ~ilc[Intern] ~warn[] ``creates''
  a symbol of a given name in a given package.")

(deflabel |ACL2 Conses or Ordered Pairs|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  ACL2 Conses or Ordered Pairs~/~/

  The function ~ilc[cons] ~warn[] creates an ordered pair.  ~ilc[Car]
  ~warn[] and ~ilc[cdr] ~warn[] return the first and second components,
  respectively, of an ordered pair.  The function ~ilc[consp] ~warn[]
  recognizes ordered pairs.

  Ordered pairs are used to represent lists and trees.  See any Common Lisp
  documentation for a discussion of how list constants are written and for
  the many list processing functions available.  Also, ~pl[programming] ~warn[]
  where we list all the ACL2 primitive functions.

  Here are some examples of list constants to suggest their syntax.
  ~bv[]
  '(a . b)                ; a pair whose car is 'a and cdr is 'b
  '(a . nil)              ; a pair whose car is 'a and cdr is nil
  '(a)                    ; another way to write the same thing
  '(a b)                  ; a pair whose car is 'a and cdr is '(b)
  '(a b c)                ; a pair whose car is 'a and cdr is '(b c)
                          ;  i.e., a list of three symbols, a, b, and c.
  '((a . 1) (b . 2))      ; a list of two pairs
  ~ev[]

  It is useful to distinguish ``proper'' conses from ``improper'' ones,
  the former being those cons trees whose right-most branch terminates with
  ~c[nil].  A ``true list'' (~pl[true-listp] ~warn[]) is either ~c[nil]
  or a proper cons.  ~c[(A b c . 7)] is an improper cons and hence not a
  true list.")

(deflabel |Guessing the Type of a Newly Admitted Function|
  :doc
  ":Doc-Section |Pages Written Especially for the Tours|

  Guessing the Type of a Newly Admitted Function~/~/

  When a function is admitted to the logic, ACL2 tries to ``guess''
  what type of object it returns.  This guess is codified as a term
  that expresses a property of the value of the function.  For ~c[app]
  the term is
  ~bv[]
  (OR (CONSP (APP X Y))
      (EQUAL (APP X Y) Y))
  ~ev[]
  which says that ~c[app] returns either a cons or its second argument.
  This formula is added to ACL2's rule base as a ~ilc[type-prescription]
  ~warn[] rule.  Later we will discuss how rules are used by the ACL2
  theorem prover.  The point here is just that when you add a definition,
  the data base of rules is updated, not just by the addition of the
  definitional axiom, but by several new rules.

  You should now return to ~il[|Revisiting the Admission of App| the Walking Tour].")

; Essay on Metafunction Support, Part 2

; For the first part of this essay, see ``Metafunction Support, Part
; 1'' in axioms.lisp.  This code is here at the end of ld so that it
; can use all our utilities and functions.

; We here turn to the problem of defining the uninterpreted functions
; that can actually be executed within a meta-level function.  Review Part 1 of
; the essay for the background and basic strategy.  We take up from there.

; Note: You can add other uninterpreted functions linked to theorem
; prover :program functions.  However, you should obey the following
; rules.

; (1) Of course, the metafunction context must be rich enough (or made
; rich enough) to provide the necessary arguments.  If you change the
; structure of metafunction-context, you must modify the accessors
; like mfc-clause, in axioms.lisp.

; (2) Include STATE as an argument to the uninterpreted symbol,
; whether it is otherwise needed or not.

(defconst *meta-level-function-problem-1*
  "~%~%Meta-level function Problem:  Some meta-level function applied ~x0 to the ~
   non-term ~x1.  The meta-level function computation was ignored.~%~%")

(defconst *meta-level-function-problem-1a*
  "~%~%Meta-level function Problem:  Some meta-level function applied ~x0 to an ~
   alist argument with ~@1.  The meta-level function computation was ignored.~%~%")

(defconst *meta-level-function-problem-2*
  "~%~%Meta-level function Problem:  Some meta-level function applied ~x0 to a ~
   context different from the one passed to the meta-level function ~
   itself.  We cannot authenticate manufactured contexts.  The ~
   manufactured context was ~X12.  The meta-level function computation ~
   was ignored.~%~%")

(defconst *meta-level-function-problem-3*
  "~%~%Meta-level function Problem:  You or some meta-level function applied ~x0 but not ~
   from within the theorem prover's meta-level function handler.  This ~
   suggests you are trying to test a meta-level function and have evidently ~
   manufactured an allegedly suitable context.  Perhaps so. But that ~
   is so difficult to check that we don't bother.  Instead we cause ~
   this error and urge you to test your meta-level function by having the ~
   meta-level function handler invoke it as part of a test proof-attempt. To ~
   do this, assume the metatheorem that you intend eventually to ~
   prove.  You may do this by executing the appropriate DEFTHM event ~
   embedded in a SKIP-PROOFS form.  Then use THM to submit ~
   conjectures for proof and observe the behavior of your ~
   metafunction.  Remember to undo the assumed metatheorem before you ~
   attempt genuine proofs! If this suggestion isn't applicable to ~
   your situation, contact the authors.~%~%")

; The standard template for introducing an uninterpreted :logic mode
; function with execute-only-in-meta-level-functions semantics involving
; :program mode functions is shown below for mfc-ts.

#+acl2-loop-only
(defstub mfc-ts (term mfc state) t)

#-acl2-loop-only
(defun-one-output mfc-ts (term mfc state)
  (declare (xargs :guard (state-p state)))

; Type-set doesn't really use state.  But we use the presence of the
; live state as authorization to execute, since we know the live state
; object cannot arise in an execution on behalf of an evaluation of a
; subexpression in a theorem or proof.

  (cond
   ((not (live-state-p state))

; This function acts like an undefined function unless it is applied to the
; live state.  

    (throw-raw-ev-fncall '(ev-fncall-null-body-er mfc-ts)))

   (*metafunction-context*

; We are within the application of a meta-level function by the theorem prover.

    (cond
     ((eq mfc *metafunction-context*)
      (cond
       ((termp term (access metafunction-context mfc :wrld))

; At this point we can code freely.  In general, any data used below
; (i.e., any actuals passed in above) must be vetted as shown above.
; There is absolutely no reason to believe that the user has called
; mfc-ts correctly, even in a verified meta-level function and we must defend
; against hard errors.

        (mv-let
         (ts ttree)
         (type-set term
                   nil         ;;; force-flg
                   nil         ;;; dwp
                   (access metafunction-context mfc :type-alist)
                   (access metafunction-context mfc :ancestors)
                   (access rewrite-constant
                           (access metafunction-context mfc :rcnst)
                           :current-enabled-structure)
                   (access metafunction-context mfc :wrld)
                   nil         ;;; ttree
                   nil nil)
         (declare (ignore ttree))
         ts))
       (t (cw *meta-level-function-problem-1* 'mfc-ts term)
          (throw-raw-ev-fncall '(ev-fncall-null-body-er mfc-ts)))))
     (t (cw *meta-level-function-problem-2* 'mfc-ts mfc
            (default-evisc-tuple *the-live-state*))
        (throw-raw-ev-fncall '(ev-fncall-null-body-er mfc-ts)))))

; We are not within the application of a meta-level function by the theorem
; prover.  We don't actually know if we are in the theorem prover.
; This could be a proof-time evaluation of a subterm of a conjecture
; about MFC-TS (e.g., the proof of the metatheorem justifying a
; metafunction using MFC-TS, or the proof of a lemma involved in that
; metatheorem proof).  Or, this could be a top-level call of MFC-TS or
; some function using it, as part of the user's testing of a
; meta-level function's development.

   (*hard-error-returns-nilp*

; This evaluation is part of a conjecture being proved.  Quietly act
; as though mfc-ts is an undefined function.  It is believed that this
; can never happen, because STATE is live.

    (throw-raw-ev-fncall '(ev-fncall-null-body-er mfc-ts)))
        
   (t 

; This is a top-level call of mfc-ts or some function using it.  Cause an error
; no matter what context the user has supplied.  See the error message.

    (cw *meta-level-function-problem-3* 'mfc-ts)
    (throw-raw-ev-fncall '(ev-fncall-null-body-er mfc-ts)))))

(defun congruence-rule-listp (x wrld)
  (if (atom x)
      (null x)
    (and (let ((rule (car x)))
           (case-match rule
             ((nume equiv . rune)
              (and (equivalence-relationp equiv wrld)
                   (or (runep rune wrld)
                       (equal rune
                              *fake-rune-for-anonymous-enabled-rule*))
                   (eql (fnume rune wrld) nume)))))
         (congruence-rule-listp (cdr x) wrld))))

(defun term-alistp-failure-msg (alist wrld)

; Returns nil if alist is an alist binding variables to terms.  Otherwise,
; returns a message suitable for use in *meta-level-function-problem-1a*.

  (cond ((atom alist)
         (and alist
              (msg "a non-nil final cdr")))
        ((atom (car alist))
         (msg "a non-consp element, ~x0" (car alist)))
        ((not (and (termp (caar alist) wrld)
                   (variablep (caar alist))))
         (msg "an element, ~p0, whose car is not a variable" (caar alist)))
        ((not (termp (cdar alist) wrld))
         (msg "an element, ~p0, whose cdr is not a term" (cdar alist)))
        (t (term-alistp-failure-msg (cdr alist) wrld))))

#-acl2-loop-only
(defun-one-output mfc-rw-raw (term alist obj equiv-info mfc fn state)
  (declare (xargs :guard (state-p state)))
  (cond
   ((not (live-state-p state))
    (throw-raw-ev-fncall `(ev-fncall-null-body-er ,fn)))
   (*metafunction-context*
    (cond
     ((eq mfc *metafunction-context*)
      (let ((wrld  (access metafunction-context mfc :wrld))
            (rcnst (access metafunction-context mfc :rcnst)))
        (cond
         ((not (termp term wrld))
          (cw *meta-level-function-problem-1* fn term)
          (throw-raw-ev-fncall `(ev-fncall-null-body-er ,fn)))
         ((let ((msg (term-alistp-failure-msg alist wrld)))
            (when msg
              (cw *meta-level-function-problem-1a* fn msg)
              (throw-raw-ev-fncall `(ev-fncall-null-body-er ,fn)))))
         (t
          (cond
           ((member-eq obj '(t nil ?))
            (mv-let
              (rw ttree)
              (rewrite-entry
               (rewrite term alist 'meta)
               :rdepth (rewrite-stack-limit wrld)
               :type-alist (access metafunction-context mfc :type-alist)
               :geneqv (cond ((eq equiv-info t)
                              *geneqv-iff*)
                             ((eq equiv-info nil)
                              nil) ; nil means EQUAL
                             ((and (symbolp equiv-info)
                                   (equivalence-relationp equiv-info wrld)
                                   (car (geneqv-lst
                                         equiv-info nil
                                         (access rewrite-constant rcnst
                                                 :current-enabled-structure)
                                         wrld))))
                             (t (prog2$ (or (congruence-rule-listp
                                             equiv-info
                                             (w state))
                                            (er hard! fn
                                                "~x0 has been passed an ~
                                                 equiv-info argument that is ~
                                                 neither t, nil, a known ~
                                                 equivalence relation, nor a ~
                                                 list of congruence rules:~|  ~
                                                 ~x1"
                                                fn
                                                equiv-info))
                                        equiv-info)))
               :wrld wrld
               :fnstack (access metafunction-context mfc :fnstack)
               :ancestors (access metafunction-context mfc :ancestors)
               :backchain-limit (access metafunction-context mfc
                                        :backchain-limit)
               :simplify-clause-pot-lst (access metafunction-context mfc
                                                :simplify-clause-pot-lst)
               :rcnst rcnst
               :gstack (access metafunction-context mfc :gstack)
               :ttree nil)
              (declare (ignore ttree))
              rw))
           (t (cw "~%~%Metafunction Problem:  Some metafunction called ~x0 ~
                   with the OBJ argument set to ~x1.  That argument must be ~
                   one of the three symbols ?, T, or NIL."
                  fn
                  obj)))))))
     (t (cw *meta-level-function-problem-2* fn mfc
            (default-evisc-tuple *the-live-state*))
        (throw-raw-ev-fncall `(ev-fncall-null-body-er ,fn)))))
   (*hard-error-returns-nilp*
    (throw-raw-ev-fncall `(ev-fncall-null-body-er ,fn)))
   (t 
    (cw *meta-level-function-problem-3* fn)
    (throw-raw-ev-fncall `(ev-fncall-null-body-er ,fn)))))

#+acl2-loop-only
(defstub mfc-rw (term obj equiv-info mfc state)

; We introduced mfc-rw+ after Version_3.0.1.  It was tempting to eliminate
; mfc-rw altogether (and then use the name mfc-rw for what we now call
; mfc-rw+), but we decided to leave mfc-rw unchanged for backward
; compatibility.  Worth mentioning: An attempt to replace mfc-rw by
; corresponding calls of mfc-rw+ in books/arithmetic-3/ resulted in a failed
; proof (of floor-floor-integer in
; books/arithmetic-3/floor-mod/floor-mod.lisp).

  t)

#+acl2-loop-only
(defstub mfc-rw+ (term alist obj equiv-info mfc state) t)

#-acl2-loop-only
(defun-one-output mfc-rw (term obj equiv-info mfc state)
  (mfc-rw-raw term nil obj equiv-info mfc 'mfc-rw state))

#-acl2-loop-only
(defun-one-output mfc-rw+ (term alist obj equiv-info mfc state)
  (mfc-rw-raw term alist obj equiv-info mfc 'mfc-rw+ state))

#+acl2-loop-only
(defstub mfc-ap (term mfc state) t)

#-acl2-loop-only
(defun-one-output mfc-ap (term mfc state)
  (declare (xargs :guard (state-p state)))
  (cond
   ((not (live-state-p state))
    (throw-raw-ev-fncall '(ev-fncall-null-body-er mfc-ap)))
   (*metafunction-context*
    (cond
     ((eq mfc *metafunction-context*)
      (cond
       ((termp term (access metafunction-context mfc :wrld))
        (let ((linearized-list
               (linearize term
                          t   ;;; positivep
                          (access metafunction-context mfc :type-alist)
                          (access rewrite-constant
                                  (access metafunction-context mfc :rcnst)
                                  :current-enabled-structure)
                          nil   ;;; force-flg
                          (access metafunction-context mfc :wrld)
                          nil   ;;; ttree
                          state)))
          (cond ((null linearized-list)
                 nil)
                ((null (cdr linearized-list))
                 (mv-let (contradictionp new-arith-db)
                   (add-polys (car linearized-list)
                              (access metafunction-context
                                      mfc :simplify-clause-pot-lst)
                              (access rewrite-constant
                                      (access metafunction-context
                                              mfc :rcnst)
                                      :pt)
                              (access rewrite-constant
                                      (access metafunction-context
                                              mfc :rcnst)
                                      :nonlinearp)
                              (access metafunction-context
                                      mfc :type-alist)
                              (access rewrite-constant
                                      (access metafunction-context
                                              mfc :rcnst)
                                      :current-enabled-structure)
                              nil   ;;; force-flg
                              (access metafunction-context mfc :wrld))
                         (declare (ignore new-arith-db))
                         contradictionp))
                (t
                 (mv-let (contradictionp1 new-arith-db)
                   (add-polys (car linearized-list)
                              (access metafunction-context
                                      mfc :simplify-clause-pot-lst)
                              (access rewrite-constant
                                      (access metafunction-context
                                              mfc :rcnst)
                                      :pt)
                              (access rewrite-constant
                                      (access metafunction-context
                                              mfc :rcnst)
                                      :nonlinearp)
                              (access metafunction-context
                                      mfc :type-alist)
                              (access rewrite-constant
                                      (access metafunction-context
                                              mfc :rcnst)
                                      :current-enabled-structure)
                              nil   ;;; force-flg
                              (access metafunction-context mfc :wrld))
                   (declare (ignore new-arith-db))
                   (if contradictionp1
                       (mv-let (contradictionp2 new-arith-db)
                         (add-polys (cadr linearized-list)
                                    (access metafunction-context
                                            mfc :simplify-clause-pot-lst)
                                    (access rewrite-constant
                                            (access metafunction-context
                                                    mfc :rcnst)
                                            :pt)
                                    (access rewrite-constant
                                            (access metafunction-context
                                                    mfc :rcnst)
                                            :nonlinearp)
                                    (access metafunction-context
                                            mfc :type-alist)
                                    (access rewrite-constant
                                            (access metafunction-context
                                                    mfc :rcnst)
                                            :current-enabled-structure)
                                    nil   ;;; force-flg
                                    (access metafunction-context mfc :wrld))
                         (declare (ignore new-arith-db))
                         contradictionp2)
                     nil))))))
       (t (cw *meta-level-function-problem-1* 'mfc-ap term)
          (throw-raw-ev-fncall '(ev-fncall-null-body-er mfc-ap)))))
     (t (cw *meta-level-function-problem-2* 'mfc-ap mfc
            (default-evisc-tuple *the-live-state*))
        (throw-raw-ev-fncall '(ev-fncall-null-body-er mfc-ap)))))
   (*hard-error-returns-nilp*
    (throw-raw-ev-fncall '(ev-fncall-null-body-er mfc-ap)))
   (t 
    (cw *meta-level-function-problem-3* 'mfc-ap)
    (throw-raw-ev-fncall '(ev-fncall-null-body-er mfc-ap)))))

; Essay on Saved-output

; Starting with Version_2.9.2, ACL2 has the capability of running not only with
; output inhibited but also with output saved, to be printed upon demand by pso
; and pso! (see their documentation).  This capability is controlled by state
; global variables whose names start with SAVED-OUTPUT-, namely:
; 'saved-output-reversed, 'saved-output-token-lst, and 'saved-output-p.  State
; global 'print-clause-ids was also introduced at the same time, in order to
; allow printing of clause ids with output inhibited in order that the user can
; observe progress of the proof.

; Why do we need both 'saved-output-p and 'saved-output-token-lst?  The latter
; records the output that the user wants saved (typically, :all or nil).  The
; former activates the saving of output, which is why it is bound to t in
; with-ctx-summarized.  The idea is that we do not want to save output that
; comes from top-level calls by the user that are not event forms, so
; 'saved-output-p remains nil at the top level.

; Perhaps we should add a mechanical check that there are no nested calls of
; io?, since such calls could confuse our mechanism for saving output.

; Implementation note: Calls of io? on a given body take as an argument a
; listing of all the free variables of that body.  After the definitions below,
; a macro call (av body) will print out such a list.

#|
(defun all-vars-untrans (form state)
  (declare (xargs :mode :program :stobjs state))
  (mv-let (erp val bindings state)
    (translate1 form
                :stobjs-out
                '((:stobjs-out . :stobjs-out))
                t 'top-level
                (w state) state)
    (declare (ignore erp bindings))
    (value (remove1-eq 'state (all-vars val)))))

(defmacro av (form)
  `(all-vars-untrans ',form state))
|#

(defun trans-eval-lst (lst ctx state)
  (cond ((endp lst)
         (value :invisible))
        (t (er-progn (trans-eval (car lst) ctx state)
                     (trans-eval-lst (cdr lst) ctx state)))))

(defun print-saved-output (inhibit-output-lst state)
  (let ((saved-output
         (reverse (io-record-forms (f-get-global 'saved-output-reversed
                                                 state))))
        (channel (standard-co state))
        (ctx  'print-saved-output))
    (cond
     ((or (null saved-output)
          (and (null (cdr saved-output))
               (eq (access io-record
                           (car (f-get-global 'saved-output-reversed state))
                           :io-marker)
                   :ctx)))
      (er-progn (if saved-output
                    (trans-eval (car saved-output) ctx state)
                  (value nil))
                (pprogn (fms "There is no saved output to print.  ~
                              See :DOC set-saved-output.~|"
                             nil
                             channel state nil)
                        (value :invisible))))
     (t (state-global-let*
         ((saved-output-reversed nil) ; preserve this (value doesn't matter)
          (inhibit-output-lst inhibit-output-lst))
         (pprogn (initialize-summary-accumulators
                  state) ; print-warnings-summary ; will clear
                 (state-global-let*
                  ((proof-tree-ctx nil)
                   (saved-output-p nil))
                  (trans-eval-lst saved-output ctx state))))))))

(defmacro pso ()

  ":Doc-Section Other

  show the most recently saved output~/

  Evaluate ~c[:pso] in order to print output that was generated
  in an environment where output was being saved;  ~pl[set-saved-output] for
  details.  However, ~il[proof-tree] output will be suppressed; use
  ~c[:]~ilc[pso!] if you want that output to be printed as well.~/~/"

  '(print-saved-output '(proof-tree) state))

(defmacro pso! ()

  ":Doc-Section Other

  show the most recently saved output, including ~il[proof-tree] output~/

  Evaluate ~c[:pso] in order to print output that was generated in an
  environment where output was being saved; ~pl[set-saved-output] for details.
  Note that ~il[proof-tree] will be included; use ~c[:]~ilc[pso] if you want
  that output to be suppressed.~/~/"

  '(print-saved-output nil state))

(defmacro set-saved-output (save-flg inhibit-flg)

  ":Doc-Section Other

  save proof output for later display with ~c[:]~ilc[pso] or ~c[:]~ilc[pso!]~/
  ~bv[]
  Examples:
  (set-saved-output t t)    ; save proof output for later, but inhibit it now
  (set-saved-output :all t) ; same as the line above
  :set-saved-output t t     ; same as the two lines above
  (set-saved-output t nil)  ; save proof output for later, but print it now too
  (set-saved-output nil t)  ; do not save proof output, and print it now
  (set-saved-output nil nil); do not save or inhibit output
  (set-saved-output nil :normal)  ; default: do not save output, and only
                                  ; inhibit proof-tree output ~/
  General Form:
  (set-saved-output save-flg inhibit-flg)
  ~ev[]
  Parameter ~c[save-flg] is ~c[t] or ~c[:all] to cause output to be saved for
  later display using ~c[pso] or ~c[pso!]; ~pl[pso] and ~pl[pso!], and see the
  documentation for ~il[proof-checker] commands of the same names.  Set
  ~c[save-flg] to ~c[nil] to turn off this feature; except, it always stays on
  in proof-checker sessions entered with ~ilc[verify].  The other argument,
  ~c[inhibit-flg], controls whether output should be inhibited when it is
  created (normally, during a proof attempt).  So a common combination is to
  set both arguments to ~c[t], to indicate that output should be suppressed for
  now but saved for printing with ~ilc[pso] or ~ilc[pso!].  The examples above
  give a good summary of the functionality, including the meaning of values
  ~c[:all] and ~c[:normal] for the first and second arguments (respectively).

  Saved output is cleared at the start of every event, and also at the start of
  every ~il[proof-checker] commands that invoke the prover.  Note that
  interactive ~il[proof-checker] commands, that is, from a proof-checker
  session entered with ~ilc[verify], are always run with output saved.

  Also ~pl[set-print-clause-ids], which causes subgoal numbers to be printed
  during proof attempts when output is inhibited.~/"

  (let ((save-flg-actual (if (and (consp save-flg)
                                  (eq (car save-flg) 'quote))
                             (cadr save-flg)
                           save-flg))
        (inhibit-flg-actual (if (and (consp inhibit-flg)
                                     (eq (car inhibit-flg) 'quote))
                                (cadr inhibit-flg)
                              inhibit-flg)))
    `(pprogn ,(cond ((member-eq save-flg-actual '(t :all))
                     '(f-put-global 'saved-output-token-lst :all state))
                    ((null save-flg-actual)
                     '(f-put-global 'saved-output-token-lst nil state))
                    ((true-listp save-flg-actual)
                     `(f-put-global 'saved-output-token-lst ',save-flg-actual state))
                    (t (er hard 'set-saved-output
                           "Illegal first argument to set-saved-output (must ~
                            be ~x0, ~x1, or a true-listp): ~x2."
                           t :all save-flg)))
             (f-put-global 'inhibit-output-lst
                           ,(cond ((member-eq inhibit-flg-actual '(t :all))
                                   (list 'quote (set-difference-eq
                                                 *valid-output-names*
                                                 '(error warning!))))
                                  ((eq inhibit-flg-actual :normal)
                                   ''(proof-tree))
                                  ((true-listp inhibit-flg-actual)
                                   (list 'quote inhibit-flg-actual))
                                  (t (er hard 'set-saved-output
                                         "Illegal second argument to ~
                                          set-saved-output (must be ~x0, ~x1, ~
                                          ~x2, or a true-listp): ~x3."
                                         t :all :normal inhibit-flg)))
                           state)
             (value :invisible))))

(defmacro set-print-clause-ids (flg)

  ":Doc-Section Other

  cause subgoal numbers to be printed when ~c['prove] output is inhibited~/
  ~bv[]
  General Forms:
  (set-print-clause-ids t)
  :set-print-clause-ids t
  (set-print-clause-ids nil)
  :set-print-clause-ids nil
  ~ev[]
  This command affects output from the theorem prover only when ~c['prove]
  output is inhibited; ~pl[set-inhibit-output-lst].  Calling this macro with
  value ~c[t] as shown above will cause subsequent proof attempts with
  ~c['prove] output inhibited to print the subgoal number, so that you can see
  the progress of the proof; value ~c[nil] reverts to the default behavior,
  where this is not the case.  On a related note, we point out that you can
  cause output to be saved for later display; ~pl[pso] and ~pl[pso!].~/~/"

  (declare (xargs :guard (member-equal flg '(t 't nil 'nil))))
  (let ((flg (if (atom flg)
                 (list 'quote flg)
               flg)))
    `(f-put-global 'print-clause-ids ,flg state)))

; Saving an Executable Image

#-acl2-loop-only
(defparameter *initial-cbd* nil)

(defun save-exec (exec-filename extra-startup-string)

  ":Doc-Section Other

  save an executable image and (for most Common Lisps) a wrapper script~/

  ~l[saving-and-restoring] for an explanation of why one might want to use this
  function.
  ~bv[]
  Examples:
  ; Save an executable named my-saved_acl2:
  (save-exec \"my-saved_acl2\"
             \"This saved image includes Version 7 of Project Foo.\")

  ; Same as above, but with a generic comment instead:
  (save-exec \"my-saved_acl2\" nil)~/
  General Form:
  (save-exec exec-filename extra-startup-string)
  ~ev[]
  where ~c[exec-filename] is the filename of the proposed executable and
  ~c[extra-startup-string] is a non-empty string to be printed after the normal
  ACL2 startup message when you start up the saved image.  However,
  ~c[extra-startup-string] is allowed to be ~c[nil], in which case a generic
  string will be printed instead.

  ~st[Note]: For technical reasons, we require that you first execute ~c[:q], to
  exit the ACL2 read-eval-print loop, before evaluating a ~c[save-exec] call.

  For most Common Lisps, the specified file (e.g., ~c[\"my-saved_acl2\"] in the
  examples above) will be written as a small script, which in turn invokes a
  saved image to which an extension has been appended (e.g.,
  ~c[my-saved_acl2.gcl] for the examples above, when the underlying Common Lisp
  is GCL on a non-Windows system).~/"

  #-acl2-loop-only
  (progn
    (if (not (eql *ld-level* 0))
        (er hard 'save-exec
            "Please type :q to exit the ACL2 read-eval-print loop and then try ~
             again."))
    (if (equal extra-startup-string "")
        (er hard 'save-exec
            "The extra-startup-string argument of save-exec must be ~x0 or ~
             else a non-empty string."
            nil)
      (setq *saved-string*
            (format
             nil
             "~a~%MODIFICATION NOTICE:~%~%~a~%"
             *saved-string*
             (cond ((null extra-startup-string)
                    "This ACL2 executable was created by saving a session.")
                   (t extra-startup-string)))))
    #-(or gcl cmu sbcl allegro clisp openmcl)
    (er hard 'save-exec
        "Sorry, but save-exec is not implemented for this Common Lisp.~a0"
        #+lispworks "  If you care to investigate, see the comment in ~
                     acl2-init.lisp starting with: ``The definition of ~
                     save-exec-raw for lispworks (below) did not work.''"
        #-lispworks "")

; The forms just below, before the call of save-exec-raw, are there so that the
; initial (lp) will set the :cbd correctly.

    (f-put-global 'connected-book-directory nil *the-live-state*)
    (setq *initial-cbd* nil)
    (setq *lp-ever-entered-p* nil)
    (save-exec-raw exec-filename))
  #+acl2-loop-only
  (declare (ignore exec-filename extra-startup-string))
  nil ; Won't get to here in GCL and perhaps other lisps
  )
