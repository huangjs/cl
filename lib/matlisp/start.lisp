;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10 -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Originally written by Tunc Simsek, University of California, Berkeley,
;;; 2000, simsek@eecs.berkeley.edu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  To compile and load MATLISP:
;;;
;;;      from the shell prompt (this needs to be done only once):
;;;
;;;                 $ make
;;;
;;;      and from within lisp:
;;;
;;;               (load "start.lisp")
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: start.lisp,v 1.11 2004/05/20 21:41:15 rtoy Exp $
;;;
;;; $Log: start.lisp,v $
;;; Revision 1.11  2004/05/20 21:41:15  rtoy
;;; Put the start up stuff in its own MATLISP-START package to prevent
;;; polluting COMMON-LISP-USER.
;;;
;;; Revision 1.10  2003/12/07 15:03:44  rtoy
;;; Add support for SBCL.  I did not test if SBCL works, but CMUCL still
;;; works.
;;;
;;; From Robbie Sedgewick on matlisp-users, 2003-11-13.
;;;
;;; Revision 1.9  2003/06/27 03:42:49  rtoy
;;; Clean up logical pathname translations for CMUCL.  Don't include the
;;; version part for the translation.  (Why doesn't this work anymore?)
;;;
;;; Revision 1.8  2001/02/26 22:45:37  rtoy
;;; There has to be a colon in the pathname to be a valid CMUCL
;;; search-list namestring.  Check for it.
;;;
;;; Revision 1.7  2001/02/26 19:57:13  rtoy
;;; o Make deflogicalpath handle CMUCL search lists.
;;; o Use keywords for mk:oos :matlisp so we put drop random symbols in
;;;   the CL-USER package. (Should we use strings?)
;;;
;;; Revision 1.6  2001/02/22 08:10:35  simsek
;;; o Added support for CMUCL 18c and Allegro 6.0
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage "MATLISP-START"
    (:use "COMMON-LISP")
    (:export "DEFLOGICALPATH")
    (:documentation "A package to hold Matlisp startup stuff"))
  )

(in-package "MATLISP-START")


(pushnew :matlisp *features*)

(let ((root "root"))

(defun setlogicalroot (r)
  (unless (stringp r)
    (error "argument ~a given to ~a should be a string"
	   r
	   'setlogicalroot))
  (setq root r))

(defun getlogicalroot ()
  root)

(defun deflogicalpath (name)
  "
  Syntax
  ======
  (DEFLOGICALPATH name)

  Purpose
  =======
  Defines a reasonable logical pathname translation for NAME, which
  must be a string.
  The translations are defined for the directory and subdirectories
  in which the file that contained the form was loaded.

  Implementation Notes
  ====================
  DEFLOGICALPATH must be called on argument \"root\" first.  This
  determines the root directory that will be used to dump the
  file \"logical\" containing all the logical pathname translations.
  In particular, a call to DEFLOGICALPATH on \"root\" clears
  all entries in the file \"logical\".
"
  (flet ((default-dir ()
	     #+:cmu (ext:default-directory)
	     #+:allegro (user::current-directory)
             #+:sbcl *default-pathname-defaults*))
    (flet ((load-pathname ()
	     (merge-pathnames 
	      (if *load-pathname* 
		  (cond ((subtypep (type-of *load-pathname*) 'logical-pathname)
			 (let ((pathname 
				(namestring
				 (translate-logical-pathname *load-pathname*))))
			   (make-pathname 
			    ;; Perhaps we don't need the conditional here,
			    ;; that is, the :device arg is needed on Allegro/Win
			    ;; but it may do no harm on Unix/Linux etc ...
			    #+(and :allegro :mswindows) :device
			    #+(and :allegro :mswindows) (pathname-device pathname)
			    :directory (pathname-directory pathname))))
			#+cmu
			((and (find #\: (namestring *load-pathname*))
			      (ext:search-list-defined-p *load-pathname*))
			 ;; CMUCL search lists must have a colon in the namestring.
			 (ext:enumerate-search-list (path *load-pathname*)
			   (let ((pathname (namestring path)))
			     (return (make-pathname
				      :directory (pathname-directory pathname))))))
			(t
			 (make-pathname
			  #+(and :allegro :mswindows) :device
			  #+(and :allegro :mswindows) (pathname-device *load-pathname*)		       
			  :directory (pathname-directory *load-pathname*))))
	      "")
	      (default-dir))))

      #+(or :cmu :sbcl)
      (setf (logical-pathname-translations name)
	(list 
	 (list "**;*.*.*"  
	       (namestring (merge-pathnames "**/*.*" (load-pathname))))
	 (list "*.*.*"
	       (namestring (merge-pathnames "*.*" (load-pathname))))))
      
      #+:allegro
      (prog1
	  (setf (logical-pathname-translations name)
	    (list 
	     (list "**;*.*"  
		   (namestring (merge-pathnames "**/*.*" (load-pathname))))
	     (list "*.*" 
		   (namestring (merge-pathnames "*.*" (load-pathname))))))
	(with-open-file (file
			 (concatenate 'string
			   (getlogicalroot) ":logical")
			 :direction :output
			 :if-exists (if (string= name root) 
					:supersede 
				      :append)
			 :if-does-not-exist :create)
	  (format file "~&~s '~s~%" 
		  name
		  (list "**;*.*"  
			(namestring 
			 (merge-pathnames "**/*.*" (load-pathname)))))
	  (format file "~&~s '~s~%" 
		  name
		  (list "*.*"
			(namestring 
			 (merge-pathnames "*.*" (load-pathname)))))
	  )
	)
      )))
)

(setlogicalroot "matlisp")
(deflogicalpath "matlisp")

(load "matlisp:system.dcl")
(load "matlisp:config.lisp")

(mk::operate-on-system :matlisp
		       :load
		       :minimal-load t
		       :verbose t
		       :compile-during-load 
		       #+:allegro-cl-lite nil
		       #-:allegro-cl-lite t)

(load "matlisp:save.lisp")

(format t "

 ** MATLISP is loaded.  Type (HELP MATLISP)
    to see a list of available symbols.
    To use matlisp:

          (use-package \"MATLISP\")

    or

          (in-package \"MATLISP-USER\")

 ** The logical pathname matlisp has been
    set to:

          ~a

"
	(namestring
	 (translate-logical-pathname "matlisp:")))


