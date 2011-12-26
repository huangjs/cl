
;;; Patches to make phtml.cl load in portable aserve in Lispworks


(in-package :sys)

(defun defpatch (&rest l) l)

(export '(defpatch))

(in-package :cl-user)

(defun copy-symbols (from to l)
  (let ((syms (mapcar #'(lambda (x) (find-symbol (string x) from)) l)))
    (import syms to)
    (export syms to)))
                      

(unless (find-symbol "IF*" :excl)
  (copy-symbols :acl-compat.excl :excl
                '(#:*current-case-mode* #:if* #:intern*))
  (copy-symbols :acl-compat.mp :mp
                '(#:without-scheduling))
  )

(unless (find-package :net.html.parser)
  (load (merge-pathnames "xmlutils/phtml.cl" *load-pathname*))
  )

