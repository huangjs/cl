(use-package :foil)

(if (not (boundp '*dev-path*))
    (setf *dev-path* (get-working-directory)))

(defun p-name (def-path)
  (format nil "~A/~A" (or *dev-path* (get-working-directory)) def-path))
        
(defun gen-wrapper-file (target-file source-dll package-spec)
  (dump-wrapper-defs-to-file (p-name target-file) (get-assembly-classnames source-dll package-spec)))

(eval-when (:load-toplevel :execute)
(let ((sys-lst '(
                 ("foil/foil-cli/cli-system.lisp" "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/mscorlib.dll" "System/")
                 ("foil/foil-cli/cli-reflection.lisp" "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/mscorlib.dll" "System.Reflection")
                 ("foil/foil-cli/cli-system-data-sqlclient.lisp" "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/System.Data.dll" "System.Data.SqlClient/")
                 ("foil/foil-cli/cli-system-collections.lisp" "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/mscorlib.dll" "System.Collections")
                 ("foil/foil-cli/cli-system-windows-forms-toplevel.lisp" "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/System.Windows.Forms.dll" "System.Windows.Forms/")
                 ("foil/foil-cli/cli-system-componentmodel.lisp" "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/System.Windows.Forms.dll" "System.ComponentModel/")                 
                 ("foil/foil-cli/cli-system-drawing.lisp" "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/System.Drawing.dll" "System.Drawing/")
                 )))
  (when (some (lambda (l)
                (not (probe-file (p-name (car l)))))
              sys-lst)
    (unless t;bind-cli-server)
      (error "failed to bind to CLI Server.  Make sure the server is started and the *ip-port* matches"))
    (mapcar (lambda (l)
              (unless (probe-file (p-name (car l)))
                (format t "Calling gen-wrappers for ~A~%" (caddr l))
                (gen-wrapper-file (car l) (cadr l) (caddr l))
                (format t "gen-wrappers complete.~%")
                ))
            sys-lst))
#|
  (let ((*compile-print* nil)
        (*compile-verbose* nil))
    (mapcar (lambda (l)
              (compile-file-if-needed (p-name (car l)) :load t))
          sys-lst))
|#
))






