;;; -*- Lisp -*-
;;;
;;; this is a load file for CMN.
;;;
;;; To get this file to compile/load everything, first try (load "cmn-all.lisp").
;;; 99% of the time that is all that's needed.  If you're running a 20 year old
;;; version of Lisp, or embedding CMN in some other program, you may need to fix up various pathnames,
;;; and probably set some of the *features* to tell CMN where the files are and what
;;; kind of system it's running on.  The pathnames are pcl-directory, cmn-bin-directory,
;;; and cmn-directory.  In MCL, there are also two MCL library files we need.  For explicit
;;; instructions, see README.cmn.

#|
Most of the setup possibilities are handled via identifiers on the *features* list.
CMN itself adds:

     :cmn
     :windoze         windows

The other *features* that CMN currently notices (leaving aside loop.lisp) are:

     :next            NeXTStep OS.
     :mac             Macintosh.
     :sgi             SGI.
     :irix            SGI.
     :sun             Sun.
     :hpux            HPUX.
     :linux           linux
     :linuxppc        linuxppc
     :draft-ansi-cl-2 AL 4.2 cltl2
     :gcl             Gnu CL
     :excl            Franz Inc's Lisp (Allegro CL)
     :mcl             Macintosh CL
     :clisp           Clisp (assumed to be a version of May-97 or later)
     :cltl2           Current lisp is a version 2 CL -- that is, it uses the later package syntax, etc
     :pcl             Current lisp uses pcl, not a built-in CLOS.
     :unix            Current Clisp is running under Unix.
     :i386            Intel processor.
     :cmu             CMU CL.
     :allegro-cl-lite no compiler in free ACL for Windoze.
     :lispworks       Harlequin Lisp.
     :sbcl            Steel Bank CL
     :openmcl         OpenMCL
     :darwin          Mac OSX OpenMCL
     :macosx          ACL 7 on OSX
|#

#+(or win98 mswindows win32) (pushnew :windoze *features*)
#+lispworks (pushnew :cltl2 *features*)

;;; ---------- SET PATHNAMES ------------

#+(or cmu sbcl) (defvar cmn-directory)
#+(or cmu sbcl) (defvar cmn-bin-directory)
#+openmcl (defvar cmn-directory)
#+openmcl (defvar cmn-bin-directory)

(if (not (boundp 'cmn-directory))
    (progn
      (setf cmn-directory 
	    (namestring
	     (truename 
	      (directory-namestring 
	       (or #+(and gcl (not cltl2)) si:*load-pathname* 
		   #+(and excl (not cltl2)) excl:*source-pathname*
		   #+(or cltl2 (and (not excl) (not gcl))) *load-pathname* 
		   "./")))))
      #+openmcl (setf cmn-directory (concatenate 'string cmn-directory "/"))
      ))

#+(and mcl (not openmcl)) (progn
  ;; we need the MCL library files quickdraw and scrollers
  (ccl:require :quickdraw)
  (ccl:require :scrollers "ccl:library;scrollers"))

#+(and (not pcl) gcl) (if (not (boundp 'pcl-directory)) (setf pcl-directory "/usr/local/lisp/pcl/"))

(if (not (boundp 'cmn-bin-directory)) (setf cmn-bin-directory cmn-directory))


;;; PCL is needed in old Clisps (but no longer supported here), GCL, and very old versions of ACL (pre-4.0).



;;; ---------- END OF SETUP STUFF -------------

#+(and linux86 (not linux)) (pushnew :linux *features*)
#+(and excl linux86 little-endian) (setf *features* (remove :sun *features*))
#+irix (pushnew :sgi *features*)
#+(or :sun4 :sparc :sparcstation :sunos) (pushnew :sun *features*)

#+clisp (handler-bind
	 ((t 
	   #'(lambda (c) ; a condition argument
	       (when (find-restart 'continue)
					; cerror is supposed to provide this restart
		     (invoke-restart 'continue)))))
	 (let ((loop-name (concatenate 'string cmn-bin-directory "cmn-loop.fas")))
	   (when (not (probe-file loop-name))
	     (compile-file (concatenate 'string cmn-directory "cmn-loop.lisp") :output-file loop-name))
	   (load loop-name)))

#+(and gcl (not loop))
    (progn
      (defpackage :loop)
      (let ((loop-name (concatenate 'string cmn-bin-directory "cmn-loop.o")))
	(when (not (probe-file loop-name))
	  (compile-file (concatenate 'string cmn-directory "cmn-loop.lisp") :output-file loop-name))
	(load loop-name)))

#+(and excl (not loop)) (require :loop)

;;; if you need pcl, edit pathname and use:
#+(and (not pcl) gcl)
  (progn
    (when (not (probe-file (concatenate 'string pcl-directory "defsys.o")))
      (compile-file (concatenate 'string pcl-directory "defsys.lisp"))
      (load (concatenate 'string pcl-directory "defsys.o"))
      (funcall (find-symbol "COMPILE-PCL" :pcl)))
    (load (concatenate 'string pcl-directory "defsys.o"))
    (funcall (find-symbol "LOAD-PCL" :pcl)))

#+cmu (declaim (optimize (extensions:inhibit-warnings 3))) 
#+cmu (setf extensions::*gc-verbose* nil)
#+cmu (setf *compile-print* nil)
#+cmu (setf *compile-verbose* nil)

#+sbcl (setf *compile-print* nil)
#+sbcl (setf *compile-verbose* nil)

#+excl (load (concatenate 'string cmn-directory "acl.cl"))

#+gcl (compiler::emit-fn nil)
#+gcl (defvar ok-to-save-image t)
#+(and gcl pcl pcl-structures) 
    ;; pcl-gcl-1.1 as built by the standard makefile (saved_pcl) neglects to define structure-slotd-reader-function,
    ;;  but calls it in braid.lisp.  So, we take a wild guess at what it should be:
    (defun pcl::structure-slotd-reader-function (structure-slot-description)
      (third structure-slot-description))

#+gcl (if (not (probe-file (concatenate 'string cmn-directory "cmn0.o")))
	  (progn
	    (system:allocate 'SYMBOL 400)
	    (system:allocate 'CONS 4000))
	(progn
	  (setf pcl::*defmethod-times* '(compile load eval))
	  (setf pcl::*defclass-times* '(compile load eval))
	  (setf pcl::*defgeneric-times* '(compile load eval))))

(defun cmn-compile-and-load (name)
  (let* ((cname (concatenate 'string cmn-directory name ".lisp"))
	 (lname #-allegro-cl-lite
		(concatenate 'string cmn-bin-directory name
			     #-(or mcl clisp gcl cmu excl lispworks) ".fasl"
			     #+mcl (concatenate 'string "." (pathname-type ccl:*.fasl-pathname*))
			     #+excl (concatenate 'string "." excl:*fasl-default-type*)
			     #+clisp ".fas"
			     #+gcl ".o"
			     #+lispworks ".ufsl"
			     #+cmu (concatenate 'string "." (c:backend-fasl-file-type c:*backend*))
			     )
		#+allegro-cl-lite cname
		))
    #-allegro-cl-lite
    (if (probe-file cname)
	(if (or (not (probe-file lname))
		(> (file-write-date (truename cname))
                   (file-write-date (truename lname))))
	    #+mcl(compile-file cname :verbose t :output-file lname)
	    #-mcl (progn
		    #+gcl (setf ok-to-save-image nil)
		    (compile-file cname :output-file lname))))
    (load lname :verbose t)))


(let ((files
       '(
         "cmn-init"
	 "cmn-utils"
	 "cmn-objects"
	 "cmn0" 
	 "cmn-grfx"
	 "cmn-glyphs"
	 "cmn1" "cmn2" "cmn3" "cmn4"
	 ;; ---- others that are sometimes useful are:
	 "rqq" "wedge" "accent" "pedal"
	 "percussion"
	 "ring" "rests" "lyrics"
	 #-cmu19d "transpose" ; incomprehensible error during compilation in cmucl 19d
	 "pmn" "quarter"
	 ;; ---- all of these are optional -- cmn will work correctly without any of them
	 )))

  (dolist (file files)
    (cmn-compile-and-load file))
  )

(setf cmn::*cmn-binary-directory* (namestring (truename cmn-bin-directory)))
(setf cmn::*cmn-source-directory* (namestring (truename cmn-directory)))

#|
;;; in excl you can save cmn with
(gc t)
(in-package :cmn)
(excl:dumplisp :name "/zap/cmn"
	       :restart-function #'(lambda (&rest args)
				     (declare (ignore args))
				     (tpl:setq-default *package*
						       (find-package :cmn))))
;;; in clisp use (saveinitmem)
;;; in mcl, there's (save-application "cmn:cmn")
;;; in gcl (si:save-system "/zap/cmn")
;;; in cmucl (extensions:save-lisp "/zap/cmn")
|#

