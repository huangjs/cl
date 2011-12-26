;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A proto modular GBS module implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - File: modules.lisp
;;; - Author: Chris Riesbeck
;;; - Most recent update: 2/8/95
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)
  (unless (find-package :modules)
    (make-package :modules
                  :use (list (or (find-package :common-lisp)
                                 (find-package :lisp))))))


(in-package :modules)

(export '(module-class make-module get-module set-module
          enter-module exit-module
          finish-module logout-module restart-module))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *module-mappings* (make-hash-table :test #'equal)
  "Table of file/gbs module mappings.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass module-class () ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAKE-MODULE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Trivial, but Keene's view on this is correct, I think.

(defun make-module (class)
  (make-instance class))


;;; (GET-MODULE file-module) => gbs-module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GET-MODULE, SET-MODULE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (GET-MODULE file-module) => gbs-module
;;;    Returns the gbs-module assigned to file-module.
;;; (SET-MODULE file-module gbs-module) => gbs-module
;;;    Assigns gbs-module to file-module.

(defun get-module (file-module)
  (values (gethash file-module *module-mappings*)))

(defun set-module (file-module gbs-module)
  (setf (gethash file-module *module-mappings*)
        gbs-module))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entering and exiting modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (ENTER-MODULE module)                       [Comb -> Module]
;;;   Called by the comb to start a module.
;;;   Method implemented in the module mapping code.
;;;   
;;;   On first call, the module should initialize itself and start
;;;   running, e.g., grab keyboard events, display some screen, etc.
;;;   On subsequent calls, module should continue from where it 
;;;   left off.

(defgeneric enter-module (module)
  (:documentation "Used by comb to give control to a module."))

;;; (EXIT-MODULE module)                       [Module -> Comb]
;;;   Called by the module to tell comb that it's done.
;;;   Method implemented in the comb.
;;;
;;;   Before calling, module should release control, e.g., stop
;;;   handling keyboard events, close its screen, etc.
;;;   Comb should enter the next module, or whatever.

(defgeneric exit-module (module)
  (:documentation "Used by module to signal exit."))

;;; (RESTART-MODULE module)                       [Comb -> Module]
;;;   Called by the comb to reset a module.
;;;   Does not enter the module.
;;;   Method implemented in the module mapping code.
;;;   
;;;   The module should re-initialize itself to its starting
;;;   point.

(defgeneric restart-module (module)
 (:documentation "Used by comb to reset a module to its starting point"))

;;; (LOGOUT-MODULE module)                       [Module -> Comb]
;;;   Called by the module to tell comb that the user wants to quit.
;;;   Method implemented in the comb.
;;;
;;;   Before calling, module should release control, e.g., stop
;;;   handling keyboard events, close its screen, etc.
;;;   Comb should tell the other modules to finish before quitting.

(defgeneric logout-module (module)
  (:documentation "Used by module to signal user wants to quit."))

;;; (FINISH-MODULE module)                       [Comb -> Module]
;;;   Called by the comb to close a module.
;;;   Method implemented in the module mapping code.
;;;   
;;;   The module should release control, e.g., stop
;;;   handling keyboard events, close its screen, etc.

(defgeneric finish-module (module)
  (:documentation "Used by comb to close a module."))



(provide "modules")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
2/8/95 [CKR]
Problem: Too many globals in module mapping files.
Change: Defined GET- and SET-MODULE.

2/7/95 [CKR]
Problem: Make-package not working in Lucid 4.
Cause: No :CLTL2 in *FEATURES* in Lucid 4
Change: New "universal" make-package form.

2/7/95 [CKR]
Problem: RESTART too generic -- potential name conflicts in
         user packages.
         LOGOUT-MODULE called, LOGOUT defined.
Change: Added -MODULE to all module method names.

2/2/95 [CKR]
Change: Created file.
        Defined common module methods.
|#

