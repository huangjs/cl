;;;; $Id: hyperspec-lookup.lisp,v 1.1.1.1 2003-11-13 19:12:22 eenge Exp $
;;;; $Source: /project/hyperspec-lookup/cvsroot/hyperspec-lookup/hyperspec-lookup.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :hs)

(defun read-sym-file (&optional (pathname *hyperspec-map-file*))
  "Read the sym file and return it as an alist."
  (let ((alist nil))
    (with-open-file (stream pathname :direction :input)
      (do ((symbol-name (read-line stream nil) (read-line stream nil))
           (symbol-url (read-line stream nil) (read-line stream nil)))
          ((eq symbol-name nil) nil)
        (let ((symbol-url (if (string-equal (subseq symbol-url 0 2) "..")
                              (subseq symbol-url 3)
                              symbol-url)))
        (setf alist (acons symbol-name symbol-url alist)))))
    alist))

(defun populate-table (alist &optional (table *hyperspec-table*))
  "Populate table with information from alist (as created by
read-sym-file."
  (dolist (symbol-pair alist)
    (let ((symbol-name (car symbol-pair))
          (symbol-url (cdr symbol-pair)))
      (setf (gethash symbol-name table) symbol-url)))
  table)

;; initializes the tables with default data
(populate-table (read-sym-file))
(populate-table (read-sym-file *mop-map-file*) *mop-table*)

(defun symbol-url-lookup (symbol url-root table)
  "Look up symbol in table and if it exist concatenate it with
url-root and return that."
  (let ((url (gethash symbol table)))
    (when url
      (concatenate 'string url-root url))))

(defun hyperspec-lookup (symbol &optional (root *hyperspec-root*)
                                (table *hyperspec-table*))
  "Look up symbol in the Hyperspec; will return the URL to the symbol
or nil if it could not be found."
  (symbol-url-lookup symbol root table))

(defun mop-lookup (symbol &optional (root *mop-root*)
                           (table *mop-table*))
  "Look up symbol in the MOP; will return the URL to the symbol or
nil if it could not be found."
  (symbol-url-lookup symbol root table))

(defun lookup (symbol)
  "Look up symbol in the Hyperspec, then MOP."
  (or (hyperspec-lookup symbol)
      (mop-lookup symbol)))

