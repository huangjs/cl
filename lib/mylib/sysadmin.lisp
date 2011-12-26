(in-package :cl-user)

(defpackage :hjs.util.sysadmin
  (:use :cl :iterate :hjs.meta.macro :hjs.util.os :hjs.meta.lisp :hjs.meta.essential
	:cl-fad)
  (:export #:check-asdf-consistency
	   #:update-asdf-links
	   #:update-asdf-packages
	   ))

(in-package :hjs.util.sysadmin)

(defvar *asdf-package-locations*
  (list (merge-pathnames ".sbcl/site/" (user-homedir-pathname))
	(merge-pathnames ".sbcl/lib/" (user-homedir-pathname))
	(merge-pathnames "slime/" (user-homedir-pathname))))

(defvar *asdf-system-file-location*
  (merge-pathnames ".sbcl/systems/" (user-homedir-pathname)))

(defun check-asdf-consistency (&key (locations *asdf-package-locations*)
			       (recursive t))
  (assert (listp locations))
  (let ((db (make-hash-table :test 'equal)))
    (labels ((find-asdf-files (location)
	       (loop for p in (list-directory location)
		  do (cond ((and (string-equal (pathname-type p) "asd")
				 (not (find "_darcs" (rest (pathname-directory p)) :test #'string-equal))
				 (not (find "{arch}" (rest (pathname-directory p)) :test #'string-equal)))
			    (let ((filename (pathname-name p)))
			      (push (cons filename p) (gethash filename db))))
			   ((directory-pathname-p p)
			    (when recursive
			      (find-asdf-files p))))))
	     (report-inconsistency ()
	       (loop for filename being the hash-key using (hash-value locations) of db
		  if (> (length locations) 1)
		  collect (cons filename locations))))
      (loop for p in locations
	 do (find-asdf-files p)
	 finally (return (values (report-inconsistency) db))))))

(defun update-asdf-links (&key (package-locations *asdf-package-locations*)
			  (asdf-file-location *asdf-system-file-location*))
  (multiple-value-bind (conflicts db)
      (check-asdf-consistency :locations package-locations)
    ;; detect conflicts
    (loop for c in conflicts
       do (progn
	    (format t "~%Package ~A conflicts at:" (car c))
	    (loop for p in (cdr c)
	       for i from 1
	       do (format t "~%~A) ~A" i (cdr p)))
	    (format t "~%Choose which file to use: ")
	    (setf (gethash (car c) db) (list (nth (1- (read)) (cdr c))))))
    ;; delete old asdf links in asdf-file-location
    (progn
      (format t "~%This will delete all the asdf files under ~A.~&Confirm?(y-or-n):" asdf-file-location)
      (loop for ans = (read-line)
	 do (cond ((string-equal ans "y")
		   (format t "~%Deleting system asdf files...~&")
		   (force-output)
		   ;; here we delete!
		   (asdf:run-shell-command "cd ~a && rm *.asd" asdf-file-location)
		   (return 'delete-complete))
		  ((string-equal ans "n")
		   (format t "~%Quit without updating.")
		   (return-from update-asdf-links nil))
		  (t
		   (format t "~%Y-or-N:") ; and go to loop
		   ))))
    ;; update asdf links
    (progn
      (format t "~%Updating asdf links...~&")
      (loop for ps being the hash-value of db
	 for p = (cdr (first ps))
	 for link = (make-pathname
		     :name (pathname-name p)
		     :type (pathname-type p)
		     :defaults asdf-file-location)
	 do (make-symlink p link))
      t)))

(defun update-asdf-packages (&key (package-locations (first *asdf-package-locations*)))
  (labels ((update (dir)
	     (format t "~&checking ~a:~%" dir)
	     (finish-output)
	     (let ((asdf::*verbose-out* *standard-output*))
	       (loop for f in (list-directory dir)
		  when (directory-pathname-p f)
		  do (cond ((find "_darcs" (pathname-directory f) :test #'string-equal)
			    #+sbcl (sb-ext:run-program "/bin/sh"
						       (list "-c" (format nil "cd ~a && darcs pull -a" dir))
						       :input t :output t)
			    (return 'darcs))
			   ((find ".git" (pathname-directory f) :test #'string-equal)
			    #+sbcl (sb-ext:run-program "/bin/sh"
						       (list "-c" (format nil "cd ~a && git pull" dir))
						       :input t :output t)
			    (return 'darcs))
			   ((find "CVS" (pathname-directory f) :test #'string-equal)
			    #+sbcl (sb-ext:run-program "/bin/sh"
						       (list "-c" (format nil "cd ~a && cvs up" dir))
						       :input t :output t)
			    (return 'cvs))
			   ((find ".svn" (pathname-directory f) :test #'string-equal)
			    #+sbcl (sb-ext:run-program "/bin/sh"
						       (list "-c" (format nil "cd ~a && svn up" dir))
						       :input t :output t)
			    (return 'svn)))))))
    (loop for f in (list-directory package-locations)
       when (directory-pathname-p f)
       do (update f))))
