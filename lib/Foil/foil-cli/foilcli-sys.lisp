(defvar *dev-path* "/dev")
(flet (( p-name (def-path)
         (format nil "~A/~A" (or *dev-path* (get-working-directory)) def-path)))
  (defsystem foilcli-sys
    (:default-pathname (p-name "foil/foil-cli")
     :default-type :lisp-file)
    :members (("FOIL-SYS" :type :system)
              "cli-system"
              "cli-system-collections"
              "cli-reflection"
              "cli-system-data-sqlclient"
              "cli-system-drawing"
              "cli-system-componentmodel"
              "cli-system-windows-forms-toplevel"
              "foil-cli")
    :rules
    ((:in-order-to :compile (:all)
      (:caused-by (:compile "foil-sys"))
      (:requires 
       (:load "foil-sys")
       ))))
  )



