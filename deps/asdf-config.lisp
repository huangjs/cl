(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (find-package :asdf))
    (error "ASDF not loaded.")))


(in-package :asdf)

(defparameter *implementation-name*
  #+sbcl "sbcl"
  #+allegro "allegro"
  #+lispworks "lispworks"
  #+(or ccl openmcl) "ccl"
  #+clisp "clisp"
  #+cmucl "cmucl"
  #+ecl "ecl"
  #+abcl "abcl")

(defparameter *source-root* (truename #p"/home/hjs/")
  "Root of source directories")

(defparameter *fasl-root* (truename
		     (merge-pathnames
		      (concatenate 'string "cl/fasls/" *implementation-name* "/")
		      (user-homedir-pathname)))
  "Root of implementation's directories of binary files")

;; (defparameter *third-part-systems* (truename (merge-pathnames #p"cl/third-party/" (user-homedir-pathname))))

;; ;; add system definition folder
;; (eval-when (:load-toplevel :execute)
;;   (pushnew *third-part-systems*
;;            (symbol-value (intern (symbol-name :*central-registry*)
;;                                  (find-package :asdf)))
;;            :test #'equalp))

(compile
 (defun pathname-prefix-p (prefix pathname)
   (not (equal (enough-namestring pathname prefix) (namestring pathname)))))

(defmethod output-files :around ((operation compile-op) (c source-file))
  (let ((source (component-pathname c))
        (paths (call-next-method)))
    (mapcar #'(lambda (path)
                (loop repeat 1
                      for from = *source-root*
                      for to = *fasl-root*
                      when (pathname-prefix-p from source)
                        do (return
                             (merge-pathnames
                              (make-pathname :type (pathname-type path))
                              (merge-pathnames (enough-namestring source from)
                                               to)))
                      finally (return path)))
            paths)))

(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
    (#+sbcl sb-ext:invalid-fasl 
      #+allegro excl::file-incompatible-fasl-error
      #+lispworks conditions:fasl-error
      #+cmu ext:invalid-fasl
      #-(or sbcl allegro lispworks cmu) error ()
      (asdf:perform (make-instance 'asdf:compile-op) c)
      (call-next-method))))

#-cmucl
(compile 'output-files)
#-cmucl
(compile 'asdf:perform)

