(use-package "System")
(use-package "System.Reflection")
(use-package "System.Collections")

(defmacro doenum ((e enum) &body body)
  (let ((genum (gensym)))
    `(let ((,genum ,enum))
       (do ()
           ((not (ienumerator.movenext ,genum)))
         (let ((,e (ienumerator.current ,genum)))
           ,@body)))))

(defun get-assembly-classnames (assembly-file-name &rest packages)
  "returns a list of strings, assemblies should be of the form \"system/io\"
  for recursive lookup and \"system/io/\" for non-recursive"
  (let* ((asm (assembly.loadfrom assembly-file-name))
         (types (ienumerable.GetEnumerator (assembly.GetTypes asm)))
         (dot-packages (mapcar (lambda (p)
                                 (substitute #\. #\/ p)) packages))
         (names ()))
    (doenum (e types)
      (let ((ename (|System|::Type.tostring e)))
          ;(format t "~A~%" ename)
        (flet ((matches (package)
                 (and (eql 0 (search package ename))
                      (or (not (eql #\. (schar package (1- (length package))))) ;recursive
                          (not (find #\. ename :start (length package))))))) ;non-subdirectory
          (when  (and ;don't grab implementation details classes
                  (not (or (find #\$ ename)
                           (find #\+ ename)
                           (search "__" ename)
                           (search "PrivateImplementationDetails" ename)))
                  (some #'matches dot-packages))
            (push ename names)))))
    names))





