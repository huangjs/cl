(in-package :climacs-commands)

(define-command (com-inspect-buffer :name "Inspect Buffer" :command-table base-table) ()
		(clouseau:inspector (current-buffer)))

(define-command (com-inspect-frame :name "Inspect Frame" :command-table base-table) ()
		(clouseau:inspector *esa-instance*))

(define-command (com-inspect-window :name "Inspect Current Window" :command-table base-table) ()
		(clouseau:inspector (current-window)))

(define-command (com-inspect-syntax :name "Inspect Syntax" :command-table base-table) ()
		(clouseau:inspector (current-syntax)))

(set-key 'com-inspect-buffer
	 'base-table
	 '((#\c :control) (#\d :control) (#\b :control)))

(set-key 'com-inspect-window
	 'base-table
	 '((#\c :control) (#\d :control) (#\w :control)))

(set-key 'com-inspect-syntax
	 'base-table
	 '((#\c :control) (#\d :control) (#\s :control)))

;; The following only works if CL:ED invokes Climacs.

;; The following commands can be safely called from outside the editor:

(define-command (com-edit-symbol-definition :name t :command-table global-command-table)
    ((symbol 'symbol
      :prompt "Edit symbol"))
  "Edit the definition of a symbol as a given type.

If the symbol has been defined more than once (eg. to a function
as well as a class, or as numerous methods), a
mouse-click-sensitive list of available definitions will be
displayed."
  (ed symbol))

(define-command (com-edit-in-climacs :command-table global-command-table)
    ((thing t))
  (ed thing))

(define-presentation-to-command-translator global-edit-symbol-definition-translator
    (symbol com-edit-symbol-definition global-command-table
            :tester ((object presentation)
                     (declare (ignore object))
                     (and (not (eq (presentation-type presentation) 'unknown-symbol))))
            :gesture :edit
            :documentation "Edit Definition")
    (object)
    (list object))

(define-presentation-to-command-translator global-edit-class-name-definition-translator
    (class-name com-edit-symbol-definition global-command-table
            :gesture :edit
            :documentation "Edit Class Definition")
    (object)
    (list object))

(define-presentation-to-command-translator global-edit-command-name-definition-translator
    (command-name com-edit-function-definition global-command-table
                  :gesture :edit
                  :documentation "Edit Definition Of Command")
    (object)
    (list object))

(define-presentation-to-command-translator global-edit-command-definition-translator
    (command com-edit-function-definition global-command-table
             :gesture :edit
             :documentation "Edit Definition Of Command")
    (object)
    (list (command-name object)))

(define-presentation-to-command-translator global-edit-pathname-translator
    (pathname com-edit-in-climacs global-command-table
             :gesture :edit
             :documentation "Edit File")
    (object)
    (list object))

;; Translator for clicky goodness in Lisp Syntax:

(define-command (com-inspect-symbol :name t :command-table drei-lisp-syntax::lisp-table)
    ((symbol 'symbol :prompt "Inspect symbol"))
  (clouseau:inspector symbol :new-process t))

(define-presentation-to-command-translator inspect-symbol
    (symbol com-inspect-symbol drei-lisp-syntax::lisp-table
            :gesture :inspect
            :tester ((object presentation)
                     (declare (ignore object))
                     (not (eq (presentation-type presentation) 'drei-lisp-syntax::unknown-symbol)))
            :documentation "Inspect")
    (object)
    (list object))
