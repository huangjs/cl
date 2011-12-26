;;;  directory.lisp --- filesystem directory access

;;;  Copyright (C) 2006, 2007 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: sclf

#+cmu (ext:file-comment "$Module: directory.lisp, Time-stamp: <2007-02-09 16:26:50 wcp> $")

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA


(cl:in-package :sclf)

(defun pathname-as-directory (pathname)
  "Converts PATHNAME to directory form and return it."
  (setf pathname (pathname pathname))
  (if (pathname-name pathname)
      (make-pathname :directory (append (or (pathname-directory pathname)
					    '(:relative))
					(list (file-namestring pathname)))
		     :name nil
		     :type nil
		     :defaults pathname)
      pathname))

(defun d+ (path &rest rest)
  "Concatenate directory pathname parts and return a pathname."
  (make-pathname :defaults path
		 :directory (append (pathname-directory path) rest)))

(defun delete-directory (pathname)
  "Remove directory PATHNAME.  Return PATHNAME."
  #+cmu (multiple-value-bind (done errno)
	     (unix:unix-rmdir (namestring pathname))
	   (unless done
	     (error "Unable to delete directory ~A (errno=~A)"
		    pathname errno)))
  #+sbcl (sb-posix:rmdir pathname)
  #+lispworks (lw:delete-directory pathname)
  #-(or cmu sbcl)
  (error "DELETE-DIRECTORY not implemented for you lisp system.")
  pathname)

(defun traverse-directory-tree (root-pathname proc &key truenamep)
  "Call PROC on all pathnames under ROOT-PATHNAME, both files and
directories.  Unless TRUENAMEP is true, this function doesn't try
to lookup the truename of files, as finding the truename may be a
superfluous and noxious activity expecially when you expect
broken symbolic links in your filesystem."
  (labels ((traverse (dir)
	     (loop
		for file in (directory dir :truenamep truenamep)
		do (funcall proc file)
		unless (pathname-name file)
		do (traverse file))))
    (traverse root-pathname)
    (values)))

(defmacro do-directory-tree ((file root-pathname &key truenamep) &body body)
  "Call TRAVERSE-DIRECTORY-TREE with BODY es procedure."
  `(traverse-directory-tree ,root-pathname
			    #'(lambda (,file)
				,@body)
			    :truenamep ,truenamep))

(defun map-directory-tree (pathname function)
  "Apply FUNCTION to every file in a directory tree starting from
PATHNAME.  Return the list of results."
  (be return-list '()
    (do-directory-tree (directory-entry pathname)
      (push (funcall function directory-entry) return-list))
    (nreverse return-list)))

(defun find-files (root-pathname matcher-function &key truenamep)
  "In the directory tree rooted at ROOT-PATHNAME, find files that
when the pathname is applied to MATCHER-FUNCTION will return
true.  Return the list of files found.  Unless TRUENAMEP is true
this function doesn't try to lookup the truename of
files. Finding the truename may be a superfluous and noxious
activity expecially when you expect broken symbolic links in your
filesystem.  (This may not apply to your particular lisp
system.)"
  (be files '()
    (do-directory-tree (file root-pathname :truenamep truenamep)
      (when (funcall matcher-function file)
	(push file files)))
    (nreverse files)))

(defun delete-directory-tree (pathname)
  "Recursively delete PATHNAME and all the directory structure
below it.

WARNING: depending on the way the DIRECTORY function is
implemented on your Lisp system this function may follow Unix
symbolic links and thus delete files outside the PATHNAME
hierarchy.  Check this before using it in your programs."
  (if (pathname-name pathname)
      (delete-file pathname)
      (progn
	(dolist (file (directory pathname :follow-links nil))
	  (delete-directory-tree file))
	(delete-directory pathname))))

;; I know, the MODE argument may not make sense on M$-filesystems but
;; I don't give a damn about inferior technologies.
(defun make-directory (pathname &optional (mode #o777))
  "Create a new directory in the filesystem.  Permissions MODE
will be assigned to it.  Return PATHNAME."
  #+cmu (multiple-value-bind (done errno)
	    (unix:unix-mkdir (namestring pathname) mode)
	  (unless done
	    (error "Unable to create directory ~A (errno=~A)." pathname errno)))
  #+sbcl (sb-posix:mkdir pathname mode)
  #-(or cmu sbcl)
  (error "MAKE-DIRECTORY is not implemented for this Lisp system.")
  pathname)

(defun make-temp-directory (&optional (default-pathname *tmp-file-defaults*) (mode #o777))
  "Create a new directory and return its pathname.
If DEFAULT-PATHNAME is specified and not NIL it's used as
defaults to produce the pathname of the directory.  Return the
pathname of the temporary directory."
  (loop
     for name = (pathname-as-directory (temp-file-name default-pathname))
     when (ignore-errors (make-directory name mode))
     return name))

(defmacro with-temp-directory ((path &rest make-temp-directory-args) &body body)
  "Execute BODY with PATH bound to the pathname of a new unique
temporary directory.  On exit of BODY the directory tree starting
from PATH will be automatically removed from the filesystem.
Return what BODY returns."
  `(be ,path (make-temp-directory ,@make-temp-directory-args)
     (unwind-protect
	  (progn ,@body)
       (delete-directory-tree ,path))))

(defun directory-p (pathname)
  "Return true if PATHNAME names a directory on the filesystem."
  #+cmu
  (be mode (nth 3 (multiple-value-list (unix:unix-stat (namestring pathname))))
    (and mode				; it may not exist at all
	 (not (zerop (logand mode #o40000)))))
  #+sbcl
  (be mode (nth 3 (multiple-value-list (sb-unix:unix-stat (namestring pathname))))
    (and mode				; it may not exist at all
	 (not (zerop (logand mode #o40000)))))
  #+clisp (ext:probe-directory (pathname-as-directory pathname))
  #-(or cmu sbcl clisp)
  (error "Don't know how to tell a directory from a file in this Common Lisp system."))
