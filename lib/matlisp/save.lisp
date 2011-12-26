;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (c) 2000 The Regents of the University of California.
;;; All rights reserved. 
;;;
;;; Permission is hereby granted, without written agreement and without
;;; license or royalty fees, to use, copy, modify, and distribute this
;;; software and its documentation for any purpose, provided that the
;;; above copyright notice and the following two paragraphs appear in all
;;; copies of this software.
;;;
;;; IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
;;; FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
;;; ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
;;; THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
;;; PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
;;; CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
;;; ENHANCEMENTS, OR MODIFICATIONS.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Save a Matlisp image.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: save.lisp,v 1.7 2005/01/27 19:32:46 rtoy Exp $
;;; $Log: save.lisp,v $
;;; Revision 1.7  2005/01/27 19:32:46  rtoy
;;; %TOP-LEVEL is in the LISP package, not CL.
;;;
;;; Revision 1.6  2003/12/07 15:03:44  rtoy
;;; Add support for SBCL.  I did not test if SBCL works, but CMUCL still
;;; works.
;;;
;;; From Robbie Sedgewick on matlisp-users, 2003-11-13.
;;;
;;; Revision 1.5  2002/01/08 00:36:42  rtoy
;;; Add SAVE-MATLISP-LIBRARY to concatenate all the fasl files into one
;;; giant fasl file for CMULISP.  This gives an intermediate form that is
;;; faster than loading up the individual fasl files but not as fast as
;;; using a dumped image.
;;;
;;; Revision 1.4  2000/10/06 18:59:00  simsek
;;; o Added CMUCLLIB to generated CMU startup script
;;;
;;; Revision 1.1  2000/10/04 22:47:11  simsek
;;; o Initial revision
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "MATLISP")

#+:cmu
(defmacro save-matlisp ()
  "
  Syntax
  ======
  (SAVE-MATLISP)

  Purpose
  =======
  Dumps a Lisp core image to the file \"matlisp.core\" in 
  the (EXT::DEFAULT-DIRECTORY), ie. the directory where 
  you loaded matlisp, and also dumps the script file 
  \"matlisp-cmu\" which loads Matlisp with that core.

  Notes
  =====
  The script file \"matlisp-cmu\" will use the lisp executable 
  that was used to dump the \"matlisp.core\" core image.
"
  `(progn
     (in-package "MATLISP")
     (let ((core-name (namestring 
		       (merge-pathnames "matlisp.core" 
					(ext::default-directory))))
	   (matlisp-name (namestring 
			(merge-pathnames "matlisp-cmu" 
					 (ext::default-directory))))
	   
	   )

       (format t "~&~% ** To start Matlisp after core dump \
execute the shell script")
       (format t "~%     ~a" matlisp-name)
       (format t "~% ** It may be necessary to:")
       (format t "~%      chmod +x ~a" matlisp-name)   
       (format t "~%~%")
       (force-output)
       
       (with-open-file (f matlisp-name :DIRECTION :OUTPUT :if-exists :supersede)
	 (format f "#!/bin/sh~%")
	 (let ((lib-path (assoc :cmucllib ext::*environment-list*)))
	   (if lib-path
	       (format f "CMUCLLIB=~s~%" (cdr lib-path))
	     (format f "CMUCLLIB=~%")))
	 (format f "export CMUCLLIB~%")
	 (write-string (car ext::*command-line-strings*) f)
	 (write-string " -core " f)
	 (write-string core-name f)
	 (write-string " $@ " f))
       (ext:save-lisp 
	   core-name
	   :print-herald nil
	   :init-function 
	   #'(lambda () 
	       (in-package "MATLISP-USER")
	       (matlisp::load-blas-&-lapack-binaries)
	       (format t "~%")
	       (ext::print-herald)
	       (format t "~%")
	       (defparameter sys::*command-index* 0)
	       (lisp::%top-level))))))

#+:sbcl
(defmacro save-matlisp ()
  "
  Syntax
  ======
  (SAVE-MATLISP)

  Purpose
  =======
  Dumps a Lisp core image to the file \"matlisp.core\" in 
  the (EXT::DEFAULT-DIRECTORY), ie. the directory where 
  you loaded matlisp, and also dumps the script file 
  \"matlisp-sbcl\" which loads Matlisp with that core.

  Notes
  =====
  The script file \"matlisp-sbcl\" will use the lisp executable 
  that was used to dump the \"matlisp.core\" core image.
"
  `(progn
     (in-package "MATLISP")
     (let ((core-name  "matlisp.core")
	   (matlisp-name "matlisp-sbcl" )
	   
	   )

       (format t "~&~% ** To start Matlisp after core dump \
execute the shell script")
       (format t "~%     ~a" matlisp-name)
       (format t "~% ** It may be necessary to:")
       (format t "~%      chmod +x ~a" matlisp-name)   
       (format t "~%~%")
       (force-output)
       
       (with-open-file (f matlisp-name :DIRECTION :OUTPUT :if-exists :supersede)
	 (format f "#!/bin/sh~%")
         #+nil
	 (let ((home-path (sb-ext:posix-getenv "SBCL_HOME")))
	   (if home-path
	       (format f "SBCL_HOME=~s~%" home-path)
               (format f "SBCL_HOME=~%")))
         (format f "export SBCL_HOME~%")
	 (write-string (car sb-ext:*posix-argv*) f)
	 (write-string " --core " f)
	 (write-string core-name f)
	 (write-string " $@" f)
         (format f "~%"))
       (sb-ext:save-lisp-and-die
	   core-name
	   :toplevel 
	   #'(lambda () 
	       (in-package "MATLISP-USER")
	       (matlisp::load-blas-&-lapack-binaries)
	       (format t "Loaded matlisp~%")
	       (format t "~%")
	       (sb-impl::toplevel-init))))))

#+:allegro
(defmacro save-matlisp ()
  "
  Syntax
  ======
  (SAVE-MATLISP)

  Purpose
  =======
  Dumps a Lisp core image to the file \"matlisp.dxl\" in 
  the (EXCL:CURRENT-DIRECTORY), ie. the directory where 
  you loaded Matlisp, and also dumps the script file 
  \"matlisp-acl\" which loads Matlisp with that core.

  Notes
  =====
  The script file \"matlisp-acl\" will use the lisp executable 
  that was used to dump the \"matlisp.dxl\" core image.
"
  `(progn
     (in-package "MATLISP-USER")
     (let ((core-name (namestring 
		       (merge-pathnames "matlisp.dxl" 
					(excl::current-directory))))
	   (matlisp-name (namestring 
			(merge-pathnames "matlisp-acl" 
					 (excl::current-directory))))
	   (script-name (namestring 
			 (merge-pathnames "startup.acl" 
					  (excl::current-directory))))
	   
	   )

       #-:mswindows
       (progn
	 (format t "~&~% ** To start Matlisp after core dump \
execute the shell script")
	 (format t "~%     ~a" matlisp-name)
	 (format t "~% ** It may be necessary to:")
	 (format t "~%      chmod +x ~a" matlisp-name)   
	 (format t "~%~%")
	 (force-output))

       #+:mswindows
       (progn
	 (format t "~&~% ** To start Matlisp after core dump \
execute the executable file")
	 (format t "~%     ~a" core-name)
	 (format t "~%~%")
	 (force-output))

       #-:mswindows
       (with-open-file (f matlisp-name :DIRECTION :OUTPUT :if-exists :supersede)
	 (format f "#!/bin/sh~%")
	 (write-string (car (sys:command-line-arguments)) f)
	 (write-string " -I " f)
	 (write-string core-name f)
	 (write-string " $@ " f)
	 )

       (tpl:setq-default *package* (find-package "MATLISP-USER"))
       (tpl:setq-default *read-default-float-format* 
	 *read-default-float-format*)
       (tpl:setq-default *readtable* *readtable*)
       (push (namestring
	      (translate-logical-pathname "matlisp:logical"))
	     (excl::logical-pathname-translations-database-pathnames))
       (setq excl::*restart-init-function*
	 #'(lambda () 
	     (matlisp::load-blas-&-lapack-binaries)
	     (load "matlisp:bin;reader" :verbose nil)
	     (format t "~%")
	     (format t "~a" (matlisp::matlisp-herald))
	     (format t "~%")))
       (matlisp::unload-blas-&-lapack-libraries)
       (excl:dumplisp 
	   :name core-name)
       )))

;; This concatenates all of the compiled lisp files into a giant fasl
;; file that can be used to load all of matlisp.  This is
;; significantly faster than loading the individual fasl files via
;; mk:oos.
;;
;; This works, but probably needs more work to get everything we need
;; done.
#+cmu
(defun save-matlisp-library ()
  (let ((output
	 (merge-pathnames "matlisp:matlisp-library"
			  (make-pathname :type (c::backend-fasl-file-type c::*target-backend*)))))
    (ext:run-program "cat"
		     (append (mk:files-in-system "matlisp-packages" t :binary)
			     (mk:files-in-system "lazy-loader" t :binary)
			     (mk:files-in-system "matlisp" t :binary))
		     :output output
		     :if-output-exists :supersede)))
