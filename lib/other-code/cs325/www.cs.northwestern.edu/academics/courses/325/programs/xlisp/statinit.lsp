;;; Initialization file for XlispStat 3.50

(debug)

(load "require")
(use-package :require)

(push "Riesbeck HD:Common Lisp:" *module-search-path*)
(push "Riesbeck HD:XLISP-STAT 50 Ä:" *module-search-path*)
(push "Riesbeck HD:CS:HTML:courses:c25:programs:" *module-search-path*)


(require "extendstat")

(require "loop")

;;; change default extension for require to .lisp so I can
;;; use the same files for MCL and XLisp.

(setq *default-module-source-pathname* 
      (make-pathname :type "lisp"))

