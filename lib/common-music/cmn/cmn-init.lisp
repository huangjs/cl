;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; Traditional western music notation (CMN=Common Music Notation)


#+excl (when (eq excl:*current-case-mode* :case-sensitive-upper)
	 (warn "you've chosen the one case (sensitive-upper) that is incompatible with cmn -- changing to insensitive-upper")
	 (excl:set-case-mode :case-insensitive-upper))

#+excl (defpackage :cmn (:use :common-lisp :clos) (:shadow rest) (:import-from :excl ratiop exit))
#+excl (in-package :cmn)


#+gcl (in-package :cmn :use '(:lisp :pcl :loop))
#+gcl (shadow 'system)
#+gcl (shadow 'make-system)
#+gcl (shadow 'double)


#+lispworks (defpackage :cmn (:use :common-lisp :clos) (:shadow rest) (:import-from :cl-user quit))
#+lispworks (in-package :cmn)

	      
#+clisp 
  (progn
    (defpackage :cmn 
      #-ansi-cl (:use :loop :lisp :clos)
      #+ansi-cl (:use :loop :lisp :clos)
      #-ansi-cl (:import-from :lisp bye shell)
      #+ansi-cl (:import-from :ext bye shell)
      #+(and ansi-cl syscalls) (:shadow posix:y0 posix:y1)
      (:shadow "REST")
      (:shadow "FINALIZE"))
    (in-package :cmn)
    )

#+clisp (if (and (find-package "EXT")
		 (find-symbol "WITHOUT-PACKAGE-LOCK" (find-package "EXT")))
	    (pushnew :have-without-package-lock *features*))
#+clisp (if (and (find-package "EXT")
		 (find-symbol "QUIT" (find-package "EXT")))
	    (pushnew :have-quit *features*))


#+cmu
  (progn
    (defpackage :cmn
      (:use :loop :pcl :lisp)
      (:shadow common-lisp:rest)
      (:import-from :extensions quit)
      (:shadowing-import-from "PCL" "BUILT-IN-CLASS" "CLASS-NAME" "CLASS-OF" "FIND-CLASS" "STANDARD-CLASS"))
    (in-package :cmn))


#+sbcl
  (progn
    (defpackage :cmn
      (:use :common-lisp)
      (:import-from :sb-ext quit)      
      (:shadow common-lisp:rest common-lisp:eighth))
    (in-package :cmn))


#+(and mcl (not openmcl))
  (progn
    (defpackage :cmn 
      (:use :common-lisp)
      (:shadow common-lisp:rest CCL:COPY CCL:CANCEL))
    (in-package :cmn)
    )


#+openmcl
  (progn
    (defpackage :cmn 
      (:use :common-lisp) 
      (:import-from :ccl quit)
      (:shadow common-lisp:rest))
    (in-package :cmn)
    )

#+ecl
  (progn
    (defpackage :cmn 
      (:use :common-lisp)
      (:shadow common-lisp:rest common-lisp:eighth))
    (in-package :cmn))


#-(or clisp cltl2) (shadow 'rest)


(pushnew :cmn *features*)

(defvar *cmn-binary-directory* "./")
(defvar *cmn-source-directory* "./")

(defvar *cmn-version* "Common Music Notation 15-Oct-06")
(defvar *cmn-news* 
  "
15-Oct: thickness message now affects bars locally.  Added *double-barline-thickness*.
15-Sep: added rad2cmn.lisp and example.rad, thanks to Kjetil Matheussen.
10-Aug: sign-name slot for accidental so that quarter-tone accidentals have names for cmn-store. (Bill Sack).
1-Aug:  stem-end reader added to rest class (AV).
18-Jul: note-line reader added to rest class.
3-July: bugfix in automatic ties, thanks KM.
")


