;;;; Copyright (c) 2003, 2004 Nikodemus Siivola
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :hyperdoc
  (:use :cl)
  (:export 
   #:lookup
   #:base-uri
   #:generate-index
   #:*index-directory*
   ))

(in-package :hyperdoc)

;;;; Utility functions

(defun find-value (name package)
  "Returns the symbol-value of the symbol NAME in PACKAGE, and T as a
secondary value if the symbol is bound. Returns NIL, NIL if the symbol
doesn't exist or is not bound."
  (let ((symbol (find-symbol name package)))
    (if (and symbol (boundp symbol))
	(values (symbol-value symbol) t)
	(values nil nil))))

(defun find-function (name package)
  "Returns the symbol-funciton of the symbol NAME in PACKAGE, and T as
a secondary value if the symbol is fbound. Returns NIL, NIL if the
symbol doesn't exist or is not fbound."
  (let ((symbol (find-symbol name package)))
    (if (and symbol (fboundp symbol))
	(symbol-function symbol)
	nil)))

(defun merge-uris (relative base)
  "Merges RELATIVE to BASE."
  ;; Yuck. This is so WRONG.
  (concatenate 'string base relative))

(defun package-string (package)
  "Returns the name of the designated package."
  (etypecase package
    (string package)
    (symbol (symbol-name package))
    (package (package-name package))))

(defun hash-alist (hash)
  "Returns an alist corresponding to the HASH."
  (let (alist)
    (maphash (lambda (key value)
	       (push (cons key value) alist))
	     hash)
    alist))

(defun alist-hash (alist &key (test #'eql))
  "Returns a hash corresponding to the ALIST."
  (let ((hash (make-hash-table :test test)))
    (dolist (x alist)
      (setf (gethash (car x) hash) (cdr x)))
    hash))

;;;; Varaibles

(defvar *index* nil
  "In memory index. FIXME: This should be loaded when hyperdoc is
loaded -- or at least lazily on first call to X.")

(defvar *index-directory* (merge-pathnames ".hyperdoc/" (user-homedir-pathname))
  "The directory where Hyperdoc keeps the pregenerated indices.")

(defvar *name-index-version* "Hyperdoc Name Index -- Version 1"
  "Magic version indentifier used in the name index files.")

(defvar *package-index-version* "Hyperdoc Package Index -- Version 1"
  "Magic version indentifier used in the package index files.")

(defvar *index-herald* 
  ";;;; This is an automatically generated index -- do not edit by hand!
;;;; See http://www.common-lisp.net/project/hyperdoc/ for more information."
  "Herald printed in the beginning of the index file to discourage tampering.")

(defparameter *documentation-types* 
  (list "T" "SYMBOL-MACRO" "MACRO" "CONDITION" "METHOD"
	"GENERIC-FUNCTION" "CLASS" "TYPE" "FUNCTION" "COMPILER-MACRO" "SETF"
	"METHOD-COMBINATION" "TYPE" "STRUCTURE"
	"VARIABLE" "CONSTANT")
  "Names string of documentation types used by Hyperdoc. These
correspond to what DOCUMENTATION uses with a few additions.")

(defvar *base-uris* (make-hash-table :test #'equal)
  "Holds the locally defined base-uris of various packages. Accessed via BASE-URI and (SETF BASE-URI).")

;;;; The meat and the bones

(defun base-uri (package)
  "Base URI for hyperdocs for package."
  (or (gethash (package-string package) *base-uris*)
      (find-value "*HYPERDOC-BASE-URI*" package)
      (error "No base URI for package ~A." (package-string package))))

(defun (setf base-uri) (uri package)
  "Set new base URI for hyperdocs for PACKAGE."  
  (setf (gethash (package-string package) *base-uris*) uri))

(defun lookup (package-designator symbol-name &optional doc-type)
  "Look up documentation URI-string for the named symbol of doc-type
in the designated package. If the package is not loaded pregenerated
indices are used.

If package doesn't support Hyperdoc, or no documentation for the named
symbol is found, a call to hyperspec:lookup with the symbol-name is
attempted.

If the package supports Hyperdoc, but no doc-type is given and there
are multiple matches, a list of applicable (doc-type . uri-string)
pairs is returned -- if only single doc-type matches just the URI is
returned."
  (let* ((package (find-package package-designator))
	 (uris
	  (or (if package 
		  (introspective-lookup (intern symbol-name package) doc-type)
		  (index-lookup (package-string package-designator) 
				symbol-name doc-type))
	      (hyperspec:lookup symbol-name))))
    (if (and (listp uris) (null (cdr uris)))
	(cdr (first uris))
	uris)))

(defun introspective-lookup (symbol &optional doc-type)
  "Looks up hyperdocumentation for the symbol in the current image."
  (let ((base-uri (base-uri (symbol-package symbol))))
    (mapcar (lambda (pair)
	      (cons (car pair) (merge-uris (cdr pair) base-uri)))
	    (%lookup symbol doc-type))))

(defun index-lookup (package-name symbol-name doc-type)
  "Looks up hyperdocumentation for the symbol in the pregenerated indices."
  (unless *index*
    (setf *index* (read-index)))
  (let* ((name (gethash package-name (name-table *index*)))
	 (base-uri (gethash name (base-uri-table *index*))))
    (mapcar (lambda (pair)
	      (cons (car pair) (merge-uris (cdr pair) base-uri)))
	    (let ((uris	(gethash symbol-name 
				 (gethash name (package-table *index*)))))
	      (if doc-type
		  (assoc doc-type uris)
		  uris)))))

(defun %lookup (symbol &optional doc-type)
  "Primitive for introspective hyperdoc lookup. Doesn't merge the uris."
  (let* ((package (symbol-package symbol))
	 (lookup (find-symbol "HYPERDOC-LOOKUP" package)))
    (when lookup
      (remove-duplicates (mapcan (lambda (type)
				   (let ((uri (funcall lookup symbol type)))
				     (when uri 
				       (list (cons type uri)))))
				 (if doc-type
				     (list doc-type)
				     (all-documentation-types package)))
			 :test #'string=
			 :key #'cdr))))

(defun all-documentation-types (package)
  (union *documentation-types* 
	 (find-symbol "*HYPERDOC-DOCUMENTATION-TYPES*" package)
	 :test #'string=))

(defun name-index-pathname ()
  (merge-pathnames "names.sexp" *index-directory*))

(defun package-index-pathname (package)
  (merge-pathnames (make-pathname :name (package-string package) 
				  :type "sexp" )
		   (merge-pathnames "packages/" *index-directory*)))

;;;; Static indexes

(defclass index ()
  ((names :accessor name-table
	  :initform (make-hash-table :test #'equal))
   (base-uris :accessor base-uri-table
	      :initform (make-hash-table :test #'equal))
   (package-tables :accessor package-table
		   :initform (make-hash-table :test #'equal))))

(defun generate-index (package-designator)
  "Generate Hyperdoc index for the designated package."
  (unless *index*
    (setf *index* (read-index)))
  (let* ((package (or (find-package package-designator)
		     (error "No such package: ~S." package-designator)))
	 (name (package-name package))
	 (all-names (cons name (package-nicknames package)))
	 (base-uri (base-uri package))
	 (name-table (name-table *index*))
	 (base-uri-table (base-uri-table *index*))
	 (package-table (package-table *index*)))

    ;; Clear old entries
    (let (old-name)
      (maphash (lambda (key value)
		 (when (member key all-names :test #'string=)
		   (setf old-name value)
		   (remhash key name-table)))
	       name-table)
      (remhash old-name base-uri-table)
      (remhash old-name package-table)
      ;; Handle case where the canonical name has changed
      (when (gethash old-name name-table)
	(remhash old-name name-table)))

    ;; New entries
    (dolist (n all-names)
      (setf (gethash n name-table) name))
    (setf (gethash name base-uri-table) base-uri)
    (let ((symbol-table (make-hash-table :test #'equal)))
      (do-external-symbols (sym package)
	(let ((docs (%lookup sym)))
	  (when docs
	    (setf (gethash (symbol-name sym) symbol-table) docs))))
      (setf (gethash name package-table) symbol-table))

    ;; Save
    (ensure-directories-exist *index-directory*)
    (with-standard-io-syntax 
      (with-open-file (f (name-index-pathname)
			 :direction :output
			 :if-exists :rename)
	(write-line *index-herald* f)
	(prin1 *name-index-version* f)
	(terpri f)
	(prin1 (hash-alist name-table) f)
	(terpri f))
      (let ((package-index (package-index-pathname name)))
	(ensure-directories-exist package-index)
	(with-open-file (f package-index
			   :direction :output
			   :if-exists :rename)
	  (write-line *index-herald* f)
	  (prin1 *package-index-version* f)
	  (terpri f)
	  (prin1 `(("BASE-URI" . ,base-uri)
		   ("SYMBOLS" . ,(hash-alist (gethash name package-table))))
		 f)
	  (terpri f))))))

(defun read-index ()
  (let ((index (make-instance 'index))
	(names (with-open-file (f (name-index-pathname))
		 (unless (equal *name-index-version* (read f))
		   (error "Name index version mismatch. Oh dear."))
		 (read f))))
    (dolist (n names)
      (setf (gethash (car n) (name-table index)) (cdr n)))
    (maphash (lambda (nick name)
	       (declare (ignore nick))
	       (with-open-file (f (package-index-pathname name))
		 (unless (equal *package-index-version* (read f))
		   (error "Package index version mismatch. Opps."))
		 (let ((package-index (read f)))
		   (setf (gethash name (package-table index))
			 (alist-hash (cdr (assoc "SYMBOLS" package-index
						 :test #'string=))
				     :test #'equal))
		   (setf (gethash name (base-uri-table index))
			 (cdr (assoc "BASE-URI" package-index :test #'string=))))))
	     (name-table index))
    index))

;;;; Introspection

(defvar *hyperdoc-base-uri* "http://common-lisp.net/project/hyperdoc/doc/")

(defun hyperdoc-lookup (symbol doc-type)
  (declare (ignore doc-type))
  #+nil 
  (concatenate 'string "index.html#" (string-downcase (symbol-name symbol))))
