;;; mop-browser.lisp 
;;; --------------------
;;; 
;;; Author: Chris Riesbeck (riesbeck@ils.nwu.edu)
;;; Last update: 10/14/99
;;; Platform: Allegro Common Lisp, v. 5 + for Windows
;;;           Macintosh Common Lisp, v 4.1 +
;;;
;;; This started out as a port of mop-browser.lisp
;;; by Tom McDougal (mcdougal@cs.uchicago.edu) from
;;; MACL v. 1.0. It's changed quite a bit, though. 
;;;
;;; To start it up, type (BROWSE <mop>) to the listener.
;;;
;;; This opens a window for <mop> with lists of its abstractions,
;;; slots, and specializations. To inspect the mop in any
;;; of the lists, just double-click on it.
;;;
;;; To close down all browsing windows, hold down the
;;; option key (Mac) or control key (Windows) and
;;; close one of the browsing windows.

;;; MODULES
;;; -------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "frames")
  (require "mops")
  (require "browser-api")
  (require #+:MCL"browser-window-mcl" #+:ALLEGRO"browser-window-acl")
  )

;;; PACKAGES
;;; --------

(cl:defpackage "MOP-BROWSER"
  (:use "COMMON-LISP" "FRAMES" "MOPS" 
        "BROWSER-API" "BROWSER-WINDOW")
  (:import-from "FRAMES" "SLOT" "SLOT-P")
  (:export "BROWSE")
  )

(in-package "MOP-BROWSER")

;;; ---------
;;; MAIN CODE
;;; ---------

;;; BROWSE
;;; ------

;;; (BROWSE item)                 [Generic function]
;;;     Browse the item.
;;;
;;; This can be called directly from the Listener. It is also
;;; called when items are selected from a browse list.
;;;
;;; The browse method for a symbol checks to see if the symbol
;;; names a mop, in which case it browses the mop.
;;;
;;; The browse method for a list creates a browse list window
;;; for that list.
;;;
;;; The browse method for everything else says "not browsable."

(defmethod browse ((item t))
  (browser-warn "~S is not browsable" item))

(defmethod browse ((item symbol))
   (cond ((or (mop-p item) (instance-p item))
          (get-browser-window item (get-item-items item) 
                              :reuse t))
         (t
          (browser-warn "~S is not browsable" item)
          nil)))

(defmethod browse ((contents cons))
  (let ((items (remove-if-not #'browsable-p contents)))
    (cond ((null items) (browser-warn "~S has nothing to browse" contents))
          (t
           (get-browser-window (list-window-name items) items
                               :reuse nil)))))

(defmethod browse ((item slot))
   (browse (slot-filler item)))


(defun browsable-p (item)
  (or (stringp item) (consp item) (mop-p item) (instance-p item) (slot-p item)))

(defmethod get-item-items (mop)
   (append (make-labelled-list "Absts" (absts-of mop))
     (make-labelled-list "Slots" (slots-of mop))
     (make-labelled-list "Specs" (specs-of mop))
     ))

(defun list-window-name (items)
  (format nil "(~a...)" (first items)))

(defun make-labelled-list (label items)
   (if (null items)
      nil
      (list label items)))


;;; MOP-specific functions
;;; -----------------------------


(defmethod ->string ((item symbol))
   (symbol-name item))

(defmethod ->string ((item slot))
   (let ((*print-case* :upcase))
      (format nil "~S = ~S" 
        (slot-role item) (slot-filler item))))

(defun all-specs-of (name)
  (cons name
        (remove-duplicates
         (mapcan #'(lambda (spec)
                     (copy-list (all-specs-of spec)))
                 (specs-of name)))))


(provide "mop-browser")


;;; -------- end of file -------

