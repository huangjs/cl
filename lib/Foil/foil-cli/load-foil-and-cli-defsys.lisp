; Load the defsystem files for foil and foilcli
(defvar *dev-path* "/dev")
(defun load-foilcli-defsys (&key (compile t) (load t))
  (flet ((p-name (def-path)
           (format nil "~A/~A" (or *dev-path* (get-working-directory)) def-path)))
    (let ((flist `(,(p-name "foil/foil-sys.lisp")
                   ,(p-name "foil/foil-cli/foilcli-sys.lisp"))))
        (mapcar (lambda (f)
                (compile-file-if-needed f :load t))
              flist)
      (when (or compile load)
        (let ((*compile-verbose* nil)
              (*compile-print* nil))
          (compile-system 'foilcli-sys :load load)))
    )))



