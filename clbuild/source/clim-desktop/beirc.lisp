(in-package :beirc)

(clim-launcher:add-app "Beirc" 'beirc:beirc)

(define-beirc-command (com-edit-user-init-file :name t)
    ()
  (ed *beirc-user-init-file*))
