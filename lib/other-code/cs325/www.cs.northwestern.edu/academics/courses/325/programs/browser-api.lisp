;;; File: browser-api.lisp
;;; Author: Chris Riesbeck
;;; Last update: 10/14/99

;;; This file declares the generic method BROWSE so
;;; that it can be shared by platform-specific window 
;;; code (like browser-window.lisp) and application 
;;; code that wants browser windows, like (mop-browser.lisp)

;;; PACKAGES
;;; --------

(cl:defpackage "BROWSER-API"
  (:use "COMMON-LISP")
  (:export "BROWSE" "->STRING")
  )

(in-package "BROWSER-API")

;;; ---------
;;; MAIN CODE
;;; ---------

;;; (BROWSE item) 
;;;   Opens or brings to the front a browser window for item.
;;;   Called by: user, application code, window code
;;;   Defined by: application code

;;; (->STRING item) 
;;;   Returns a string representation for item.
;;;   Called by: window code
;;;   Defined by: application code

(defgeneric browse (item))
(defgeneric ->string (item))

(provide "browser-api")


  