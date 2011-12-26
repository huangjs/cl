;;; Simple demo of MOP browsing using a toy memory.
;;;
;;; Load this file then call
;;;
;;; > (browse 'causal-1)
;;;
;;; Double-click items in the browser window that appears
;;; to browse those items.
;;;
;;; Note: if you want to add anything to memory,
;;; you need to be in the CLYDE-MEMORY package.

;;; MODULES
;;; -------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "clyde")
  (require "mop-browser")
  )

;;; PACKAGES
;;; --------

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package "CLYDE-MEMORY")
  (use-package "MOP-BROWSER")
  )