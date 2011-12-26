; Utilities in support of building extensions and modifications to ACL2
; Peter Dillinger and Matt Kaufmann
; October, 2006

(in-package "ACL2")

(defmacro temporary-redef (&rest forms)

; Translation will check that each form in forms, except perhaps the last, will
; returns an error triple (mv erp val state).

  (let ((cleanup '(mv-let (erp val state)
                          (set-ld-redefinition-action old-redef-action state)
                          (declare (ignore erp val))
                          state)))
    `(let ((old-redef-action (@ ld-redefinition-action)))
       (acl2-unwind-protect
        "temporary-redef"
        (check-vars-not-free
         (old-redef-action)
         (er-progn
          (set-ld-redefinition-action '(:doit! . :overwrite) state)
          ,@forms))
        ,cleanup
        ,cleanup))))

(defmacro with-redef-allowed (&rest forms)

  ":Doc-Section Miscellaneous

  system hacker's redefinition in an event context~/
  ~bv[]
  Examples:
  (with-redef-allowed
     (defun foo (x) (cons x x)))
  (with-redef-allowed
     :action (:query . :overwrite)
     (defun foo (x) (cons x x)))~/

  General Forms:
  (with-redef-allowed
     form1
     ...
     formk)

  (with-redef-allowed
     :action action
     form1
     ...
     formk)
  ~ev[]
  where each ~c[formi] is arbitrary and if ~c[action] is supplied then it must
  be a legal value for the ~c[ld] special, ~c[ld-redefinition-action];
  ~pl[ld-redefinition-action].

  A ~c[with-redef-allowed] form is a legal event form
  (~pl[embedded-event-form]) and hence may go into a book, and
  ~ilc[encapsulate] event, or a ~ilc[progn] event.  However, redefinition is
  potentially dangerous ~-[] ~pl[ld-redefinition-action] and
  ~pl[redefining-programs] ~-[] and hence ~c[with-redef-allowed] requires an
  active trust tag; ~pl[defttag].

  In essence, ~c[(with-redef-allowed form1 ... formk)] is the same as
  ~c[(progn form1 ... formk)], except with redefinition enabled; ~pl[progn].
  Thus, each ~c[formi] must be a legal embedded event form
  (~pl[embedded-event-form]).  If you want to remove that requirement, consider
  instead ~c[(with-redef-allowed (progn! form1 ... formk))]."

  `(progn! (temporary-redef (progn ,@forms))))

(defmacro with-raw-mode (&rest forms)

  ":Doc-Section Events

  evaluate forms in raw mode~/
  ~bv[]
  Example Form:
  (with-raw-mode
    (format t \"Preparing to load file...~~%\")
    (load \"my-file.lisp\"))~/

  General Form:
  (with-raw-mode form1 form2 ... formk)
  ~ev[]
  where each ~c[formi] is processed as though all the forms are preceded by
  ~c[:]~ilc[set-raw-mode]~c[ t].  Thus, the forms need not be ~il[events]; they
  need not even be legal ACL2 forms.  ~l[set-raw-mode] for a discussion of the
  so-called ``raw mode'' under which the forms are evaluated ~-[] unless raw
  mode is disabled by one of the forms, for example, ~c[(set-raw-mode nil)], in
  which case evaluation resumes in non-raw mode.

  WARNING: Thus, a ~c[with-raw-mode] form is potentially very dangerous!  For
  example, you can use it to call the Common Lisp ~c[load] function to load
  arbitrary Common Lisp code, perhaps even overwriting definitions of ACL2
  system functions!  Thus, as with ~ilc[set-raw-mode], ~ilc[with-raw-mode] may
  not be evaluated unless there is an active trust tag in effect.  ~l[defttag].

  Note that the normal undoing mechanism (~pl[ubt]) is not supported when raw
  mode is used.

  Also ~pl[progn!].  ~c[(With-raw-mode form1 form2 ... formk)] is equivalent
  to:
  ~bv[]
  (progn! (set-raw-mode t)
          form1 form2 ... formk
          (set-raw-mode nil))
  ~ev[]"

  `(progn! (set-raw-mode t) ,@forms (set-raw-mode nil)))

