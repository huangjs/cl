;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extended REQUIRE package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Last Update: 2/16/95
;;;
;;; Usage:
;;;
;;; In your INIT file (whatever that is for your Lisp), put
;;;
;;;   (load "require")     ;;; put correct pathname here
;;;   (use-package :require)
;;;
;;; If your Lisp complains about name conflicts, use
;;; SHADOWING-IMPORT. E.g., MCL 2.0.1 already has require,
;;; provide, *modules* and *module-search-path*, so...
;;;
;;;   (load "require")
;;;   (let ((*warn-if-redefine-kernel* nil))
;;;    (do-external-symbols (sym :require)
;;;      (shadowing-import sym :common-lisp)
;;;      (shadowing-import sym :ccl)
;;;      (export sym :common-lisp)))

;;;
;;; REQUIRE defines the following functions and global variables:
;;;
;;; FUNCTIONS
;;; ---------
;;;
;;; (REQUIRE module [pathname])
;;;   module should be a string, e.g. "tables" or "require".
;;;   If module is already listed in *MODULES*, REQUIRE does nothing.
;;;   Otherwise, it loads pathname, if pathname is given.
;;;   Otherwise, loads the result of (MODULE-PATHNAME module),
;;;   described below.
;;;   
;;; REQUIRE forms should appear at the front of each module file to
;;; load other required module files.
;;; 
;;; (PROVIDE module)
;;;   module should be a string. PROVIDE adds module to list
;;;   *MODULES*.
;;;
;;; A PROVIDE form should appear at the end of each module file.
;;;
;;; (MODULE-PATHNAME module) => pathname
;;;   Returns the file that will be loaded to get module.
;;;
;;; MODULE-PATHNAME looks for both a source file and a binary (compiled)
;;; file, and loads the more recent of the two. It uses the following
;;; search algorithm:
;;;
;;;    1. Source file: The pathname is constructed by merging
;;;       (MODULE-SOURCE-PATHNAME module), the module name, and
;;;       *DEFAULT-MODULE-SOURCE-PATHNAME*. If the directory is
;;;       still unspecified, the pathname is merged with the current
;;;       directory and then each pathname in *MODULE-SEARCH-PATH*,
;;;       until a file is found. 
;;;       
;;;    2. Binary file: The pathname is constructed by  merging
;;;       (MODULE-BINARY-PATHNAME module), the module name,
;;;       *DEFAULT-MODULE-BINARY-PATHNAME*, and the pathname found
;;;       for the source file. If no source file was found, and the
;;;       directory is still unspecified, the current directory and
;;;       the pathnames in *MODULE-SEARCH-PATH* are searched, as in
;;;       Step 1.
;;;
;;; This algorithm allows explicit control over particular modules,
;;; (use MODULE-SOURCE-PATHNAME and MODULE-BINARY-PATHNAME),
;;; while supporting two standard default methods of storing source and
;;; compiled files: (1) source and compiled files sit in the same
;;; subdirectory, (2) compiled files all sit in one special directory
;;; (put a directory in *DEFAULT-MODULE-BINARY-PATHNAME*).
;;;
;;; (MODULE-SOURCE-PATHNAME module) => pathname
;;; (MODULE-DEFAULT-PATHNAME module) => pathname
;;;   These return the stored source and binary pathnames for the
;;;   module, if any.  Can be set with SETF.
;;;   
;;; (ADD-SEARCH-PATH pathname) => pathname
;;;   Adds the directory specified by the pathname to 
;;;   *MODULE-SEARCH-PATH*, unless it's already there.
;;;
;;; (REMOVE-SEARCH-PATH pathname) => true or false
;;;   Removes the directory specified by the pathname to 
;;;   *MODULE-SEARCH-PATH*. Returns true if the directory
;;;   was present in the search paths.
;;;   
;;; 
;;; GLOBAL VARIABLES
;;; ----------------
;;;
;;; *MODULES*
;;;   The list of modules loaded.
;;;
;;; *MODULE-SEARCH-PATH*
;;;   A list of pathnames, with a directory (absolute or relative) and
;;;   optional file type.
;;;
;;; The pathnames can be given with strings, in which case you need
;;; to use the syntax of the file system, e.g., 
;;;
;;;      "/foo/baz/.lisp" for an absolute path in Lucid
;;;      "foo:baz:.lisp" for the same path in MCL
;;;
;;; or you can use MAKE-PATHNAME for portability. See Steele.
;;;
;;; *DEFAULT-MODULE-BINARY-PATHNAME*
;;; *DEFAULT-MODULE-SOURCE-PATHNAME*
;;;   These are the default pathnames for source and binary files,
;;;   respectively.  They should have a file type and optional directory.
;;;   Defaults values depend on your Lisp. If your Lisp hasn't been 
;;;   defined for REQUIRE, the defaults are "lisp" and "fasl",
;;;   respectively.
;;;
;;; If you reset these, e.g., to point to a particular directory,
;;; don't forget the file type! The safest way to do this is
;;;
;;;   (SETQ *DEFAULT-MODULE-BINARY-PATHNAME*
;;;         (MERGE-PATHNAMES -new stuff-
;;;                          *DEFAULT-MODULE-BINARY-PATHNAME*))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)
  (unless (find-package :require)
    (make-package :require
                  :use (list (or (find-package :common-lisp)
                                 (find-package :lisp))))))



(in-package :require)

(export '(require provide module-pathname
	  module-source-pathname module-binary-pathname
   add-search-path remove-search-path             
	  *default-module-binary-pathname*
	  *default-module-source-pathname*
	  *module-search-path* *modules*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exported Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *modules* nil
  "List of loaded modules")

(defvar *module-search-path* nil
  "List of search paths for REQUIRE")

(defvar *default-module-binary-pathname*
  (make-pathname :type #+:lucid "sbin"
                       #+(or :aclpc :xlisp) "fsl"
                       #-(or :aclpc :lucid :xlisp) "fasl")
  "Pathname default for compiled Lisp files")

(defvar *default-module-source-pathname*
  (make-pathname :type #+(or :aclpc :xlisp) "lsp"
                       #-(or :aclpc :xlisp) "lisp")
  "Pathname default for source Lisp files")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *module-source-pathnames* (make-hash-table)
  "Table of source pathnames for REQUIRE")

(defvar *module-binary-pathnames* (make-hash-table)
  "Table of binary pathnames for REQUIRE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exported Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The pathname is for backward compatibility

(defun require (name &optional pathname)
  (if (member name *modules* :test #'string=)
      name
      (load-module name
                   (or pathname
		       (module-pathname name)))))

(defun load-module (name pathname)
  (cond ((null pathname)
         (error "No pathname found for module ~S"
                name))
        (t (load pathname))))

(defun provide (module-name)
  (unless (member module-name *modules* :test #'string=)
    (push module-name *modules*)))

(defun module-source-pathname (name)
  (gethash name *module-source-pathnames*))

(defsetf module-source-pathname (name) (value)
    `(setf (gethash ',name *module-source-pathnames*) ,value))

(defun module-binary-pathname (name)
  (gethash name *module-binary-pathnames*))

(defsetf module-binary-pathname (name) (value)
    `(setf (gethash ',name *module-binary-pathnames*) ,value))

(defun module-pathname (name)
  (let ((source (find-module-source name)))
    (select-newer-file source
		       (find-module-binary name source))))

(defun add-search-path (path)
  (let ((pathname (pathname path)))
    (pushnew (pathname path) *module-search-path*
             :key #'pathname :test #'equal)
    pathname))

(defun remove-search-path (path)
  (let ((entry (find (pathname path) *module-search-path*
                     :key #'pathname :test #'equal)))
    (unless (null entry)
      (setq *module-search-path* 
            (delete entry *module-search-path*)))
    (not (null entry))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-module-source (name)
  (let ((source (make-source-pathname name)))
    (cond ((unspecified-p (pathname-directory source))
	   (probe-paths source))
	  (t (probe-path source)))))

(defun make-source-pathname (name)
  (let ((source (module-source-pathname name))
	(defaults 
	 (merge-pathnames (make-pathname :name name)
			  *default-module-source-pathname*)))
    (cond ((null source) defaults)
	  (t (merge-pathnames source defaults)))))

(defun find-module-binary (name source)
  (let ((binary (make-binary-pathname name)))
    (cond ((not (null source))
	   (probe-path (merge-pathnames binary source)))
	  ((unspecified-p (pathname-directory binary))
	   (probe-paths binary))
	  (t (probe-path binary)))))

(defun make-binary-pathname (name)
  (let ((binary (module-binary-pathname name))
	(defaults 
	 (merge-pathnames (make-pathname :name name)
			  *default-module-binary-pathname*)))
    (cond ((null binary) defaults)
	  (t (merge-pathnames binary defaults)))))

(defun probe-paths (source)
  (or (probe-path source)
      (some #'(lambda (pathname)
		(probe-path (merge-pathnames source pathname)))
	    *module-search-path*)))

(defun probe-path (path)
  (let ((full-path (merge-pathnames path *default-pathname-defaults*)))
    (and (probe-file full-path) full-path)))

(defun select-newer-file (source binary)
  (cond ((null binary) source)
	((null source) binary)
	((>= (file-write-date binary)
	     (file-write-date source))
	 binary)
	(t source)))

;;; Need (:RELATIVE) because Xlisp's (MAKE-PATHNAME :NAME "FOO")
;;; puts (:RELATIVE) in the directory component.

(defun unspecified-p (component)
  (or (null component)
      (eql component :unspecified)
      (equal component '(:relative))))

#|
3/9/95 CKR
Problem: MODULE-PATHNAME would fail to find files LOAD could find.
Cause: PROBE-PATH wasn't using *DEFAULT-PATHNAME-DEFAULTS*.
Change: Added *DEFAULT-PATHNAME-DEFAULTS* to PROBE-PATH.

2/16/95 CKR
Problem: File didn't load properly.
Cause: Forgot closing comment characters to the change list.
Change: Added closing comment characters.

2/12/95 CKR
Problem: REQUIRE return value was useless for already loaded modules.
Change: Changed to return module name.

2/7/95 CKR
Problem: Make-package not working in Lucid 4.
Cause: No :CLTL2 in *FEATURES* in Lucid 4
Change: New "universal" make-package form.

2/7/95 CKR
Problem: Suggested MCL import/export patch not complete.
Change: New suggestion.

2/2/95 CKR
Change: Added load-module for better error message when
        no pathname found.

1/30/95 CKR
Change: Added default file types for ACLPC (Allegro CL
        for PC's).

1/25/95 CKR
Change: Made it less like MCL :-)
        Search for binaries where source is found,
        Dumped *REQUIRE-EXPORTS* for DO-EXTERNAL-SYMBOLS,
        rename WHERE-IS to MODULE-PATNAME.

1/24/95 CKR
Change: Made it just like MCL, except that it searches
        the current directory, and it doesn't allow
        more than one pathname in *MODULE-FILE-ALIST*,
        added *REQUIRE-EXPORTS* to simplify SHADOWING-IMPORT.

7/26/94 CKR
Changed: Renamed FIND-MODULE-FILE as WHERE-IS for export.

3/24/93 CKR
Change: Made PROVIDE use PUSHNEW.

?/?/91  CKR
Change: Created file.
|#

