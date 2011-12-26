; ACL2 Version 3.1 -- A Computational Logic for Applicative Common Lisp
; Copyright (C) 2006  University of Texas at Austin

; This version of ACL2 is a descendent of ACL2 Version 1.9, Copyright
; (C) 1997 Computational Logic, Inc.  See the documentation topic NOTE-2-0.

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

; Written by:  Matt Kaufmann               and J Strother Moore
; email:       Kaufmann@cs.utexas.edu      and Moore@cs.utexas.edu
; Department of Computer Sciences
; University of Texas at Austin
; Austin, TX 78712-1188 U.S.A.

; This file, acl2-init.lisp, is the inititialization file for ACL2.

; ACL2 is designed to run in any Common Lisp, although we have focused
; especially on AKCL, and we take advantage of a few aspects of AKCL
; to assist our development.  This file need not be distributed with
; ACL2 and is unimportant for the correct operation of ACL2.  This
; file is loaded automatically by ACKL when it starts up.

; This file cannot be compiled because it changes packages in the middle.

; Allow taking advantage of threads in SBCL and OpenMCL.
#+(or (and sbcl sb-thread) openmcl)
(push :acl2-mv-as-values *features*)

#+akcl
(setq si:*notify-gbc* t)

; #+lispworks ; 3.2.0
;(lw::extend-current-stack 1000)

; Create the packages we use.

 (load "acl2.lisp")

;  Now over to the "ACL2" package for the rest of this file.

(in-package "ACL2")

; It is a mystery why the following proclamation is necessary, but it
; SEEMS to be necessary in order to permit the interaction of tracing
; with the redefinition of si::break-level.

#+akcl
(declaim (special si::arglist))

#+akcl
(let ((v (symbol-function 'si::break-level)))
  (setf (symbol-function 'si::break-level)
        (function
         (lambda (&rest rst)
           (format t "~%Raw Lisp Break.~%")
           (apply v rst)))))

(defun system-call (string arguments)
  #+akcl
  (lisp::system
   (let ((result string))
     (dolist
      (x arguments)
      (setq result (concatenate 'string result " " x)))
     result))
  #+lucid (user::run-program string :arguments arguments)
  #+lispworks
  (system::call-system
   (let ((result string))
     (dolist
      (x arguments)
      (setq result (concatenate 'string result " " x)))
     result))
  #+allegro
  (let ((result string))
    (dolist
      (x arguments)
      (setq result (concatenate 'string result " " x)))
    #-unix
    (excl::shell result)
    #+unix

; In Allegro CL in Unix, we can avoid spawning a new shell by calling run-shell-command
; on a simple vector.  So we parse the resulting string "cmd arg1 ... argk" and
; run with the simple vector #(cmd cmd arg1 ... argk).

    (excl::run-shell-command
     (let ((lst nil)
           (len (length result))
           (n 0))
       (loop
        (if (>= n len) (return)) ; else get next word
        (let ((start n)
              (ch (char result n)))
          (cond
           ((member ch '(#\Space #\Tab))
            (setq n (1+ n)))
           (t (loop
               (if (or (>= n len)
                       (member (setq ch (char result n))
                               '(#\Space #\Tab)))
                   (return)
                 (setq n (1+ n))))
              (setq lst (cons (subseq result start n)
                              lst))))))
       (setq result (nreverse lst))
       (setq result (coerce (cons (car result) result) 'vector)))))
  #+cmu
  (common-lisp-user::run-program string arguments :output t)
  #+sbcl
  (sb-ext:run-program string arguments :output t :search t)
  #+clisp
  (ext:run-program string :arguments arguments)
  #+openmcl
  (ccl::run-program string arguments :output t)
  #-(or akcl lucid lispworks allegro cmu sbcl clisp openmcl)
  (declare (ignore string arguments))
  #-(or akcl lucid lispworks allegro cmu sbcl clisp openmcl)
  (error "SYSTEM-CALL is not yet defined in this Lisp."))

(defun copy-acl2 (dir)
  (system-call
   "cp"
   (append '("makefile"
             "acl2.lisp"
             "acl2-check.lisp"
             "acl2-fns.lisp"
             "init.lisp"
             "acl2-init.lisp")
           (append (let ((result (list (format nil "~a" dir))))
                     (dolist
                      (x *copy-of-acl2-files*)
                      (setq result
                            (cons (format nil "~a.lisp" x)
                                  result)))
                     result)))))

(defun copy-distribution (output-file source-directory target-directory
                                      &optional
                                      (all-files "all-files.txt")
                                      (use-existing-target nil))

; We check that all files and directories exist that are supposed to exist.  We
; cause an error if not, which ultimately will cause the Unix process that
; calls this function to return an error status, thus halting the make of which
; this operation is a part.  Wart:  Since probe-file does not check names with
; wildcards, we skip those.

; Note:  This function does not actually do any copying or directory creation;
; rather, it creates a file that can be executed.

; FIRST, we make sure we are in the expected directory.

  (cond ((not (and (stringp source-directory)
                   (not (equal source-directory ""))))
         (error "The source directory specified for COPY-DISTRIBUTION~%~
                 must be a non-empty string, but~%~s~%is not."
                source-directory)))
  (cond ((eql (char source-directory (1- (length source-directory))) #\/)

; In this code we treat all directories as names without the trailing slash.

         (setq source-directory
               (subseq source-directory 0 (1- (length source-directory))))))
  (cond ((not (equal (namestring (truename (format nil "~a/" source-directory)))
                     (namestring (truename ""))))
         (error "We expected to be in the directory~%~s~%~
                 but instead are apparently in the directory~%~s .~%~
                 Either issue, in Unix, the command~%~
                 cd ~a~%~
                 or else edit the file (presumably, makefile) from~%~
                 which the function COPY-DISTRIBUTION was called,~%~
                 in order to give it the correct second argument."
                source-directory
                (namestring (truename ""))
                source-directory)))

; Next, check that everything exists that is supposed to.

  (cond ((and (not use-existing-target)
              (probe-file target-directory))
         (error "Aborting copying of the distribution.  The target ~%~
                 distribution directory~%~s~%~
                 already exists!  You may wish to execute the following~%~
                 Unix command to remove it and all its contents:~%~
                 rm -r ~a"
                target-directory target-directory)))
  (format t "Checking that distribution files are all present.~%")
  (let (missing-files)
    (with-open-file
     (str (concatenate 'string source-directory "/" all-files)
          :direction :input)
     (let (filename (dir nil))
       (loop (setq filename (read-line str nil))
             (cond
              ((null filename) (return))
              ((or (equal filename "")
                   (equal (char filename 0) #\#)))
              ((find #\Tab filename)
               (error "Found a line with a Tab in it:  ~s" filename))
              ((find #\Space filename)
               (error "Found a line with a Space in it:  ~s" filename))
              ((find #\* filename)
               (format t "Skipping wildcard file name, ~s.~%" filename))
              ((eql (char filename (1- (length filename))) #\:)

; No need to check for directories here; they'll get checked elsewhere.  But
; it's harmless enough to do so.

               (let* ((new-dir (subseq filename 0 (1- (length filename))))
                      (absolute-dir
                       (format nil "~a/~a" source-directory new-dir)))
                 (cond
                  ((probe-file absolute-dir)
                   (setq dir new-dir))
                  (t
                   (setq missing-files
                         (cons absolute-dir missing-files))
                   (error "Failed to find directory ~a ."
                          absolute-dir)))))
              (t (let ((absolute-filename
                        (if dir
                            (format nil "~a/~a/~a" source-directory dir filename)
                          (format nil "~a/~a" source-directory filename))))
                   (cond
                    ((not (probe-file absolute-filename))
                     (setq missing-files
                           (cons absolute-filename missing-files))
                     (format t "Failed to find file ~a.~%" absolute-filename)))))))))
    (cond
     (missing-files
      (error "~%Missing the following files (and/or directories):~%~s"
             missing-files))
     (t (format t "Distribution files are all present.~%"))))

  (format t "Preparing to copy distribution files from~%~a/~%to~%~a/ .~%"
          source-directory target-directory)
  (let (all-dirs)

; In this pass, we look only for directory names.

    (with-open-file
     (str (concatenate 'string source-directory "/" all-files)
          :direction :input)
     (let (filename)
       (loop (setq filename (read-line str nil))
             (cond
              ((null filename) (return))
              ((or (equal filename "")
                   (equal (char filename 0) #\#)))
              ((find #\Tab filename)
               (error "Found a line with a Tab in it:  ~s" filename))
              ((find #\Space filename)
               (error "Found a line with a Space in it:  ~s" filename))
              ((eql (char filename (1- (length filename))) #\:)
               (setq all-dirs
                     (cons (subseq filename 0 (1- (length filename)))
                           all-dirs)))))))

; In the final pass we do our writing.

    (with-open-file
     (str (concatenate 'string source-directory "/" all-files)
          :direction :input)
     (with-open-file
      (outstr output-file :direction :output)
      (let (filename (dir nil))
        (if (not use-existing-target)
            (format outstr "mkdir ~a~%~%" target-directory))
        (loop (setq filename (read-line str nil))
              (cond
               ((null filename) (return))
               ((or (equal filename "")
                    (equal (char filename 0) #\#)))
               ((eql (char filename (1- (length filename))) #\:)
                (setq dir (subseq filename 0 (1- (length filename))))
                (format outstr "~%mkdir ~a/~a~%"
                        target-directory dir))
               ((null dir)
                (cond ((not (member filename all-dirs
                                    :test 'equal))
                       (format outstr "cp -p ~a/~a ~a~%"
                               source-directory
                               filename
                               target-directory))))
               (t
                (cond ((not (member (format nil "~a/~a"
                                            dir filename)
                                    all-dirs
                                    :test 'equal))
                       (format outstr "cp -p ~a/~a/~a ~a/~a~%"
                               source-directory
                               dir
                               filename
                               target-directory
                               dir)))))))))

    (format t "Finished creating a command file for copying distribution files.")))

(defun make-tags ()
  #-(or openmcl sbcl cmu)
; We disallow openmcl and sbcl for the following check.  We have found that the
; result of the system-call is a process, (typep <result> 'external-process) in
; openmcl and (typep <result> 'sb-impl::process) in sbcl, which can probably be
; made to yield the status.  But the status is 0 even for commands not found,
; so why bother?  Since cmucl seems to fall victim in the same way as cmucl, we
; treat these two the same here.
  (when (not (eql (system-call "which" '("etags")) 0))
    (format t "SKIPPING etags: No such program is in the path.")
    (return-from make-tags 1))
  (system-call "etags"
               #+(or cmu sbcl clisp openmcl)
               (append '("acl2.lisp"
                         "acl2-check.lisp"
                         "acl2-fns.lisp"
                         "init.lisp"
                         "acl2-init.lisp"
                         "akcl-acl2-trace.lisp"
                         "allegro-acl2-trace.lisp"
                         "openmcl-acl2-trace.lisp")
                       (let ((result nil))
                         (dolist
                          (x *copy-of-acl2-files*)
                          (setq result
                                (cons (format nil "~a.lisp" x)
                                      result)))
                         (reverse result)))
               #-(or cmu sbcl clisp openmcl)
               (append '("acl2.lisp"
                         "acl2-check.lisp"
                         "acl2-fns.lisp"
                         "init.lisp"
                         "acl2-init.lisp"
                         "akcl-acl2-trace.lisp"
                         "allegro-acl2-trace.lisp")
                       (let ((result nil))
                         (dolist
                          (x *copy-of-acl2-files*)
                          (setq result
                                (cons (format nil " ~a.lisp" x)
                                      result)))
                         (reverse result)))))

(defvar *saved-build-date*)
(defvar *saved-mode*)

(defvar *saved-string*
  (concatenate
   'string
   "~% ~a built ~a.~
   ~% Copyright (C) 2006  University of Texas at Austin~
   ~% ACL2 comes with ABSOLUTELY NO WARRANTY.  This is free software and you~
   ~% are welcome to redistribute it under certain conditions.  For details,~
   ~% see the GNU General Public License.~%~
    ~a"
   #+lispworks
   "~% Type (LP) to enter the ACL2 command loop;~
    ~% then see the documentation topic note-2-~a for recent changes.~%"
   #-lispworks
   "~% See the documentation topic ~a for recent changes.~
    ~% Note: We have modified the prompt in some underlying Lisps to further~
    ~% distinguish it from the ACL2 prompt.~%"

   "~% NOTE!!  Proof trees are disabled in ACL2.  To enable them in emacs,~
    ~% look under the ACL2 source directory in interface/emacs/README.doc; ~
    ~% and, to turn on proof trees, execute :START-PROOF-TREE in the ACL2 ~
    ~% command loop.   Look in the ACL2 documentation under PROOF-TREE.~%"))

(defun maybe-load-acl2-init ()

; There is not a true notion of home directory for Windows systems, as far as
; we know.  We may provide one at a future point, but for now, we simply act as
; though ~/acl2-init.lsp does not exist on such systems.


  #+mswindows
  nil
  #-mswindows
  (let ((fl (probe-file (merge-pathnames (user-homedir-pathname)
                                         "acl2-init.lsp"))))
    (if fl (load fl))))

#+akcl
(defun gcl-version-> (major minor extra)

; When true, this guarantees that the current GCL version is greater than
; major.minor.extra.  The converse holds for versions of GCL past perhaps 2.0.

  (and (boundp 'si::*gcl-major-version*)
       (integerp si::*gcl-major-version*)
       (if (= si::*gcl-major-version* major)
           (and (boundp 'si::*gcl-minor-version*)
                (integerp si::*gcl-minor-version*)
                (if (= si::*gcl-minor-version* minor)
                    (and (boundp 'si::*gcl-extra-version*)
                         (integerp si::*gcl-extra-version*)
                         (> si::*gcl-extra-version* extra))
                  (> si::*gcl-minor-version* minor)))
         (> si::*gcl-major-version* major))))

(defun chmod-executable (sysout-name)
  (system-call "chmod" (list "+x" sysout-name)))

(defmacro write-exec-file (stream string &rest args)
  `(progn (format ,stream
                  "#!/bin/sh~%~a~%"
                  (let ((books-dir (getenv$-raw "ACL2_SYSTEM_BOOKS")))
                    (if books-dir
                        (format nil "export ACL2_SYSTEM_BOOKS=~a" books-dir)
                      "")))
          (format ,stream ,string ,@args)))

#+akcl
(defun save-acl2-in-akcl-aux (sysout-name gcl-exec-name
                                          write-worklispext
                                          set-optimize-maximum-pages)
  (if (and write-worklispext (probe-file "worklispext"))
      (delete-file "worklispext"))
  (let* ((ext "gcl")
         (ext+

; We deal with the apparent fact that Windows implementations of GCL append
; ".exe" to the filename created by save-system.

          #+mswindows "gcl.exe"
          #-mswindows "gcl")
         (gcl-exec-file
          (concatenate 'string
                       (namestring (truename ""))
                       gcl-exec-name
                       "."
                       ext+)))
    (if write-worklispext
        (with-open-file (str "worklispext" :direction :output)
                        (format str ext+)))
    (if (probe-file sysout-name)
        (delete-file sysout-name))
    (if (probe-file gcl-exec-file)
        (delete-file gcl-exec-file))
    (with-open-file (str sysout-name :direction :output)
                    (write-exec-file str "exec ~s $*~%" gcl-exec-file))
    (cond ((and set-optimize-maximum-pages
                (boundp 'si::*optimize-maximum-pages*))

; We follow a suggestion of Camm Maguire by setting
; 'si::*optimize-maximum-pages* to t just before the save.  We avoid the
; combination of 'si::*optimize-maximum-pages* and sgc-on for GCL versions
; through 2.6.3, because of problematic interactions between SGC and
; si::*optimize-maximum-pages*.  This issue has been fixed starting with GCL
; 2.6.4.  Since si::*optimize-maximum-pages* is only bound starting with
; sub-versions of 2.6, the problem only exists there.

           (cond ((or (not (fboundp 'si::sgc-on))
                      (gcl-version-> 2 6 3))
                  (setq si::*optimize-maximum-pages* t)))))
    (chmod-executable sysout-name)
    (si::save-system (concatenate 'string sysout-name "." ext))))

#+akcl
(defun save-acl2-in-akcl (sysout-name gcl-exec-name
                                      &optional mode do-not-save-gcl)
  (setq *acl2-allocation-alist*

; If *acl2-allocation-alist* is rebound before allocation is done in
; si::*top-level-hook*, e.g., if it is bound in one's init.lisp or
; acl2-init.lsp file, then such binding will override this one.  The package
; name shouldn't matter for the keys in user's alist, but in the code below we
; need to keep 'hole in the ACL2 package because we refer to it below.

; Historical Comments:

; Where did these numbers come from?  At CLInc we have used the numbers from
; the non-small-p case for some time, and they seem satisfactory.  When we
; moved to a "small" image in Version 1.8, we wanted to have roughly the same
; number of free cells as we've had all along, as a default.  The cons number
; below is obtained by seeing how many pages we had free (pages in use
; multiplied by percent free, as shown by (room)) the last time we built ACL2,
; before modifying ACL2 to support small images, and adding that to the number
; of pages in use in the small image when no extra pages were allocated at
; start-up.  The total was 2917, so that is what we use below.  The relocatable
; size is rather arbitrary, and the hole size has been suggested by Bill
; Schelter.  Finally, the other numbers were unchanged when we used the same
; algorithm described above for cons (except for fixnum, which came out to 99
; -- close enough!).

; Warning:  as of this writing (5/95), there are versions of Linux in which the
; page size is half of that in GCL on a Sparc.  In that case, we should double
; the number of pages in each case in order to have the same amount of free
; objects available.  We do this below; see the next comment.  (We assume that
; there are still the same number of bytes per object; at least, in one
; instance in Linux that appears to be the case for cons, namely, 12 bytes per
; cons.)

; Additional comments during Version_2.9 development:

; When built with GCL 2.6.1-38 and *acl2-allocation-alist* = nil, we have:

;   ACL2>(room)
; 
;     4972/4972   61.7%         CONS RATIO LONG-FLOAT COMPLEX STRUCTURE
;      133/274    14.0%         FIXNUM SHORT-FLOAT CHARACTER RANDOM-STATE READTABLE NIL
;      210/462    97.5%         SYMBOL STREAM
;        1/2      37.2%         PACKAGE
;       69/265     1.0%         ARRAY HASH-TABLE VECTOR BIT-VECTOR PATHNAME CCLOSURE FAT-STRING
;     1290/1884    7.4%         STRING
;      711/779     0.9%         CFUN BIGNUM
;       29/115    82.8%         SFUN GFUN CFDATA SPICE NIL
; 
;     1302/1400                 contiguous (176 blocks)
;          13107                hole
;          5242    0.0%         relocatable
; 
;         7415 pages for cells
;        27066 total pages
;        93462 pages available
;        10544 pages in heap but not gc'd + pages needed for gc marking
;       131072 maximum pages
; 
;   ACL2>

; So as an experiment we used some really large numbers below (but not for hole or
; relocatable).  They seemed to work well, but see comment just below.

; End of Historical Comments.

        (cond
         ((gcl-version-> 2 6 1)

; In GCL 2.6.5, and in fact starting (we believe) with GCL 2.6.2, GCL does not
; need preallocation to do the job well.  We got this opinion after discussions
; with Bob Boyer and Camm Maguire.  In a pre-release of Version_2.9, we found
; there was no noticeable change in regression time or image size when avoiding
; preallocation.  So it seems reasonable to stop messing with such numbers so
; that they do not become stale and interfere with GCL doing its job.

          nil)
         (t
          `((hole)
            (relocatable)
            (cons . 10000)
            (fixnum . 300)

; Apparently bignums are in CFUN space starting with GCL 2.4.0.  So we make
; sure explicitly that there is enough room for bignums.  Before GCL 2.4.0,
; bignums are in CONS space so the following should be irrelevant.

            (bignum . 800)
            (symbol . 500)
            (package)
            (array  . 300)
            (string . 2000)
            ;;(cfun . 32) ; same as bignum
            (sfun . 200)))))

; Now adjust if the page size differs from that for GCL/AKCL running on a
; Sparc.  See comment above.

  (let ((multiplier (/ 4096 si::lisp-pagesize)))
    (cond ((not (= multiplier 1))
           (setq *acl2-allocation-alist*
                 (sloop::sloop for (type . n) in *acl2-allocation-alist*
                               collect
                               (cons type
                                     (and n
                                          (round (* multiplier n)))))))))
  (setq si::*top-level-hook*
        #'(lambda ()
            (format t *saved-string*
                    *copy-of-acl2-version*
                    *saved-build-date*
                    (cond (mode
                           (format nil "~% Initialized with ~a." mode))
                          (t ""))
                    (eval '(latest-release-note-string)) ; avoid possible warning
                    )
            (maybe-load-acl2-init)
            (cond
             (*acl2-allocation-alist*
;              (format
;               t
;               "Beginning allocations.  Set acl2::*acl2-allocation-alist* to NIL~%~
;                in ~~/acl2-init.lsp if you must make your running image smaller.~%")
              (sloop::sloop for (type . n) in *acl2-allocation-alist*
                            when n
                            do
;                            (format t "Allocating ~s to ~s.~%" type n)
                            (let ((x (symbol-name type)))
                              (cond
                               ((equal x "HOLE")
                                (si::set-hole-size n))
                               ((equal x "RELOCATABLE")
                                (si::allocate-relocatable-pages n))
                               (t (si::allocate type n t)))))))
            (in-package "ACL2")
            (lp)))
  (load "akcl-acl2-trace.lisp")

; The following is important so that ACL2 functions are efficient in certain
; situations.  For example, (aref1 'foo foo n) should avoid boxing a fixnum n.

  #-acl2-mv-as-values (proclaim-files)
  #+acl2-mv-as-values (load "acl2-proclaims.lisp")

; Return to normal allocation growth.  Keep this in sync with load-acl2, which
; had presumably already set the allocation growth to be particularly slow.

  (sloop::sloop
   for type in
   '(cons fixnum symbol array string cfun sfun

; In akcl, at least some versions of it, we cannot call allocate-growth on the
; following two types.

          #+gcl contiguous
          #+gcl relocatable
          )
   do
   (cond
    ((or (boundp 'si::*gcl-major-version*) ;GCL 2.0 or greater
         (and (boundp 'si::*gcl-version*) ;GCL 1.1
              (= si::*gcl-version* 1)))
     (si::allocate-growth type 0 0 0 0))
    (t (si::allocate-growth type 0 0 0))))

;  (print "Start (si::gbc nil)") ;debugging GC
  (si::set-hole-size 500) ; wfs suggestion

; Camm Maguire says (7/04) that "the gc algorithm skips over any pages which
; have not been written to since sgc-on was invoked.  So gc really needs to be
; done before turning [sgc] on (not off)...."

  (si::gbc t) ; wfs suggestion [at least if we turn on SGC] -- formerly nil
              ; (don't know why...)

  (cond ((fboundp 'si::sgc-on)
         (print "Executing (si::sgc-on t)") ;debugging GC
         (si::sgc-on t)))

; Set the hole to be sufficiently large so that ACL2 can do all the allocations
; quickly when it starts up, without any GC, leaving the desired size hole when
; finished.

  (let ((new-hole-size
         (or (cdr (assoc 'hole *acl2-allocation-alist*))
             (si::get-hole-size))))
    (sloop::sloop for (type . n) in *acl2-allocation-alist*
                  with space
                  when (and n
                            (not (equal (symbol-name type) "HOLE"))
                            (< (setq space
                                     #+gcl
                                     (cond ;2.0 or later?
                                      ((boundp 'si::*gcl-major-version*)
                                       (nth 1 (multiple-value-list
                                               (si::allocated type))))
                                      (t
                                       (caddr (si::allocated type))))
                                     #-gcl
                                     (cond
                                      ((equal (symbol-name type)
                                              "RELOCATABLE")
                                       (si::allocated-relocatable-pages))
                                      (t (si::allocated-pages type))))
                               n))
                  do (setq new-hole-size (+ new-hole-size (- n space))))
;    (print "Set hole size") ;debugging
    (si::set-hole-size new-hole-size))

; The calculation above is legacy.  Now we increment the hole size to 20% of
; max-pages instead of the default 10%.  Camm Maguire says that "Larger values
; allow quick allocation of pages without triggering gc" and that the hole is
; part of the virtual (not resident) memory size, rather than being saved to
; disk.

  (let ((new-size (floor si:*lisp-maxpages* 5)))
    (if (< (si:get-hole-size) new-size)
        (si::set-hole-size new-size)))

;  (print (true-listp (w *the-live-state*))) ;swap in the world's pages

;  (print "Save the system") ;debugging
  (when (not do-not-save-gcl)
    (save-acl2-in-akcl-aux sysout-name gcl-exec-name t t)))

#+akcl
(defun save-exec-raw (sysout-name)
  (setq *acl2-allocation-alist* nil) ; Don't meddle with allocations.
  (setq *acl2-default-restart-complete* nil)
  (save-acl2-in-akcl-aux sysout-name sysout-name nil nil))

(defvar *acl2-default-restart-complete* nil)

(defun acl2-default-restart ()
  (if *acl2-default-restart-complete*
      (return-from acl2-default-restart nil))

  #+openmcl
  (progn

; In OpenMCL, print greeting now, rather than upon first re-entry to ACL2 loop.
; Here we follow a suggestion from Gary Byers.

    (format t "~&Welcome to ~A ~A!~%"
            (lisp-implementation-type)
            (lisp-implementation-version))
    (setq ccl::*inhibit-greeting* t))
  (format t *saved-string*
          *copy-of-acl2-version*
          *saved-build-date*
          (cond (*saved-mode*
                 (format nil "~% Initialized with ~a." *saved-mode*))
                (t ""))
          (eval '(latest-release-note-string)) ; avoid possible warning
          )
  (maybe-load-acl2-init)
  (in-package "ACL2")

; The following two lines follow the recommendation in Allegro CL's
; documentation file doc/delivery.htm.

  #+allegro (tpl:setq-default *package* (find-package "ACL2"))
  #+allegro (rplacd (assoc 'tpl::*saved-package*
                           tpl:*default-lisp-listener-bindings*)
                    'common-lisp:*package*)

  #+allegro (lp)
  #+openmcl (eval '(lp)) ; using eval to avoid compiler warning

; See the comment in save-acl2-in-lispworks for why we need the following call.

  #+lispworks (mp:initialize-multiprocessing)

  ;;Lispworks 4.2.0 no longer recognizes the following:
  ;;#+lispworks (lw::extend-current-stack 1000)

  (setq *acl2-default-restart-complete* t)
  nil)

#+cmu
(defun cmulisp-restart ()
  (extensions::print-herald t)
  (acl2-default-restart)
  (lp))

#+sbcl
(defun sbcl-restart ()
  (acl2-default-restart)
; Use eval to avoid style-warning for undefined function LP.
  (eval '(lp)))

#+lucid
(defun save-acl2-in-lucid (sysout-name &optional mode)
  (setq *saved-mode* mode)
  (user::disksave sysout-name :restart-function 'acl2-default-restart
                  :full-gc t))

#+lispworks
(defun save-acl2-in-lispworks (sysout-name &optional mode)
  (setq *saved-mode* mode)

; Increase the stack size.  Without doing this, Version_2.7 (as it existed
; shortly before release) had a stack overflow involving collect-assumptions in
; the proof of bitn-lam0 from books/rtl/rel2/support/lop3.lisp.  As Lispworks
; support (Dave Fox) has pointed out, we need to be sure to call
; (mp:initialize-multiprocessing) when starting up.  See LP for that call.

; But even the following wasn't adequate for (verify-guards read-utf8-fast ...)
; in books/unicode/read-utf8.lisp:
; (setq sys:*sg-default-size* 128000)
; So we try the following.

  (setq sys:*sg-default-size* 1024000)

  (system::save-image sysout-name :restart-function 'acl2-default-restart
                      :gc t))

; The definition of save-exec-raw for lispworks did not work (Lispworks 4.4.5,
; and probably Lispworks 4.2).  An attempt resulted in the following error:

;   ACL2 5 > (save-exec "my-lw" "Changes made")

;   Error: You cannot save an image while multiprocessing or the environment is running.
;   Place the call to HARLEQUIN-COMMON-LISP:SAVE-IMAGE in a file and use the -init command line option.

; If it becomes important to save an ACL2 image for lispworks from an ACL2
; session, we can figure this out at that time (and perhaps get some help from
; the user, who may be knowledgeable about lispworks).

; #+lispworks
; (defun save-exec-raw (sysout-name)
;   (setq *acl2-default-restart-complete* nil)
;   (system::save-image sysout-name :restart-function 'acl2-default-restart
;                       :gc t))

#+lispworks
(defun save-exec-raw (sysout-name)
  (declare (ignore sysout-name))
  (error "Save-exec-raw is not implemented in lispworks.  Feel free to contact~%~
          the implementors, and feel free to read the comment above the~%~
          definition  of save-exec-raw for #+lispworks in ACL2 source file~%~
          acl2-init.lisp."))

#+cmu
(defun save-acl2-in-cmulisp-aux (sysout-name core-name)
  (let ((eventual-sysout-core
         (concatenate 'string
                      (namestring (truename ""))
                      core-name
                      ".core"))
        (sysout-core
         (concatenate 'string sysout-name ".core")))
    (if (probe-file sysout-name)
        (delete-file sysout-name))
    (if (probe-file eventual-sysout-core)
        (delete-file eventual-sysout-core))
    (with-open-file ; write to nsaved_acl2
     (str sysout-name :direction :output)
     (let* ((prog0 (car extensions::*command-line-strings*))
            (len (length prog0))
            (prog1 (cond ((< len 4)

; If cmucl is installed by extracting to /usr/local/ then the cmucl command is
; simply "lisp" (thanks to Bill Pase for pointing this out).

                          "lisp")

; The next two cases apply in 18e (and probably earlier) but not 19a (and
; probably later), which has the correct path (doesn't need "lisp" appended).

                         ((equal (subseq prog0 (- len 4) len) "bin/")
                          (concatenate 'string prog0 "lisp"))
                         ((equal (subseq prog0 (- len 3) len) "bin")
                          (concatenate 'string prog0 "/lisp"))
                         (t prog0))))
       (write-exec-file str
                        "~s -core ~s -eval '(acl2::cmulisp-restart)' $*~%"
                        prog1
                        eventual-sysout-core)))
    (chmod-executable sysout-name)
    (system::gc)
    (extensions::save-lisp sysout-core :load-init-file nil :site-init nil

; We call print-herald in cmulisp-restart, so that the herald is printed
; before the ACL2-specific information (and before the call of lp).

                           :print-herald nil)))

#+cmu
(defun save-acl2-in-cmulisp (sysout-name &optional mode core-name)
  (setq *saved-mode* mode)
  (if (probe-file "worklispext")
      (delete-file "worklispext"))
  (with-open-file (str "worklispext" :direction :output)
                  (format str "core"))
  (save-acl2-in-cmulisp-aux sysout-name core-name))

#+cmu
(defun save-exec-raw (sysout-name)
  (setq *acl2-default-restart-complete* nil)
  (save-acl2-in-cmulisp-aux sysout-name sysout-name))

#+sbcl
(defun save-acl2-in-sbcl-aux (sysout-name core-name)
  (declaim (optimize (sb-ext:inhibit-warnings 3)))
  (let ((eventual-sysout-core
         (concatenate 'string
                      (namestring (truename ""))
                      core-name
                      ".core"))
        (sysout-core
         (concatenate 'string sysout-name ".core")))
    (if (probe-file sysout-name)
        (delete-file sysout-name))
    (if (probe-file eventual-sysout-core)
        (delete-file eventual-sysout-core))
    (with-open-file ; write to nsaved_acl2
     (str sysout-name :direction :output)
     (let* ((prog (car sb-ext:*posix-argv*)))
       (write-exec-file
        str
        "~s --core ~s --userinit /dev/null --eval '(acl2::sbcl-restart)'"
        prog
        eventual-sysout-core)))
    (chmod-executable sysout-name)
    ;; In SBCL 0.9.3 the read-only space is too small for dumping ACL2 on x86,
    ;; so we have to specify :PURIFY NIL. This will unfortunately result in
    ;; some core file bloat, and slightly slower startup.
    (sb-ext:gc)
    (sb-ext:save-lisp-and-die sysout-core
                              :purify 
                              #+(or x86 x86-64) nil
                              #-(or x86 x86-64) t)))
                           
#+sbcl
(defun save-acl2-in-sbcl (sysout-name &optional mode core-name)
  (with-warnings-suppressed
   (setq *saved-mode* mode)
   (if (probe-file "worklispext")
       (delete-file "worklispext"))
   (with-open-file (str "worklispext" :direction :output)
                   (format str "core"))
   (save-acl2-in-sbcl-aux sysout-name core-name)))

#+sbcl
(defun save-exec-raw (sysout-name)
  (with-warnings-suppressed
   (setq *acl2-default-restart-complete* nil)
   (save-acl2-in-sbcl-aux sysout-name sysout-name)))

#+allegro
(defun save-acl2-in-allegro-aux (sysout-name dxl-name)
  (excl:gc t) ; Suggestions are welcome for better gc call(s)!
  #+(and allegro-version>= (version>= 4 3))
  (progn
    (tpl:setq-default *PACKAGE* (find-package "ACL2"))
    (setq EXCL:*RESTART-INIT-FUNCTION* 'acl2-default-restart)
    #+(and allegro-version>= (version>= 5 0))
    (progn

; Allegro 5.0 and later no longer supports standalone images.  Instead, one
; creates a .dxl file using dumplisp and then starts up ACL2 using the original
; Lisp executable, but with the .dxl file specified using option -I.  Our
; saved_acl2 executable is then a one-line script that makes this Lisp
; invocation.  Note that :checkpoint is no longer supported starting in 5.0.

      (let* ((save-dir
              (namestring (truename

; Before Version_2.5, we had the following comment here:
; We have found that Allegro 4.3 is not happy about the call (truename "").
; However, John Cowles has reported the following results when running Allegro
; 5.0.1 under Windows 98 on a PC.

;  Start allegro in directory C:\Program Files\acl501\

;  (truename "")
;  #p"C:\\Program Files\\acl501\\"

;  (truename "./")
;  #p"C:\\"

; Since we have already (before Version_2.5) been calling truename on "" in
; other places in the code, we get bold and do so here as well.  NOTE:  In
; MCL (both a version used by Warren hunt in May 2000 and a much older
; version), (truename "./") causes an error.

; Robert Krug tells us that this is also true of OpenMCL 0.13.3 under Darwin,
; and that this is fixed in 0.13.5.

                           "")))
             (eventual-sysout-dxl
              (if dxl-name
                  (concatenate 'string save-dir dxl-name ".dxl")
                (error "An image file must be specified when building ACL2 in Allegro 5.0 or later.")))
             (sysout-dxl
              (concatenate 'string sysout-name ".dxl")))
        (write-acl2rc save-dir)
        (with-open-file ; write to nsaved_acl2
         (str sysout-name :direction :output)
         (write-exec-file
          str

; We use ~s instead of ~a below because John Cowles has told us that in Windows
; 98, the string quotes seem necessary for the first string and desirable for
; the second.  The string quotes do not hurt on Unix platforms.

; We omit the trailing " -L ~s" for now from the following string, which would
; go with format arg (rc-filename save-dir), because we know of no way in
; Allegro 6.2 to avoid getting Allegro copyright information printed upon :q if
; we start up in the ACL2 read-eval-print loop.
          #|               "~s -I ~s -L ~s~%" |#

          "~s -I ~s $*~%"
          (system::command-line-argument 0)
          eventual-sysout-dxl))
        (chmod-executable sysout-name)
        (excl:dumplisp :name sysout-dxl)))
    #-(and allegro-version>= (version>= 5 0))
    (excl:dumplisp :name sysout-name :checkpoint nil))
  #-(and allegro-version>= (version>= 4 3))
  (progn
    (excl:dumplisp :name sysout-name :checkpoint t
                   :restart-function 'acl2-default-restart)))

#+allegro
(defun save-acl2-in-allegro (sysout-name &optional mode dxl-name)

; Note that dxl-name should, if supplied, be a relative pathname string, not
; absolute.

  (setq *saved-mode* mode)
  (if (probe-file "worklispext")
      (delete-file "worklispext"))
  (with-open-file (str "worklispext" :direction :output)
                  (format str "dxl"))
  (load "allegro-acl2-trace.lisp") ; Robert Krug's trace patch
  (save-acl2-in-allegro-aux sysout-name dxl-name))

#+allegro
(defun save-exec-raw (sysout-name)
  (setq *acl2-default-restart-complete* nil)
  (save-acl2-in-allegro-aux sysout-name sysout-name))

(defun rc-filename (dir)
  (concatenate 'string dir ".acl2rc"))

(defun write-acl2rc (dir)
  (let ((rc-filename
         (concatenate 'string dir ".acl2rc")))
    (if (not (probe-file rc-filename))
        (with-open-file ; write to .acl2rc
         (str (rc-filename dir) :direction :output)

; We call acl2-default-restart before lp so that the banner will be printed
; and (optionally) ~/acl2-init.lsp file will be loaded before entering the ACL2
; read-eval-print loop.

         (format str
                 "; enter ACL2 read-eval-print loop~%~
                  (ACL2::ACL2-DEFAULT-RESTART)~%~
                  #+ALLEGRO (EXCL::PRINT-STARTUP-INFO T)~%~
                  #+ALLEGRO (SETQ EXCL::*PRINT-STARTUP-MESSAGE* NIL)~%~
                  (ACL2::LP)~%")))))

#+clisp
(defun save-acl2-in-clisp-aux (sysout-name mem-name)
  (let* ((save-dir (namestring (truename "")))
         (eventual-sysout-mem
          (concatenate 'string save-dir mem-name ".mem"))
         (sysout-mem
          (concatenate 'string sysout-name ".mem")))
    (if (probe-file sysout-name)
        (delete-file sysout-name))
    (if (probe-file eventual-sysout-mem)
        (delete-file eventual-sysout-mem))
    (write-acl2rc save-dir)
    (with-open-file ; write to nsaved_acl2
     (str sysout-name :direction :output)
     (write-exec-file str

; We take Noah Friedman's suggestion of using "exec" since there is no reason
; to keep the saved_acl2 shell script in the process table.

                      "exec ~s -i ~s -p ACL2 -M ~s -m ~sMB $*~%"
                      (or (ext:getenv "LISP") "clisp")
                      (rc-filename save-dir)
                      eventual-sysout-mem

; Here we choose a value of 10 for the -m argument.  We have found that without
; setting -m, we get a stack overflow in books/unicode/read-utf8.lisp at
; (verify-guards read-utf8-fast ...) when running on CLISP 2.34 on Linux.
; CLISP documentation at http://clisp.cons.org/clisp.html#opt-memsize says that
; it is "common to specify 10 MB" for the value of -m; since that suffices to
; eliminate the stack overflow mentioned above, we use that value.

                      10))
    (chmod-executable sysout-name)
    (in-package "ACL2")
    (ext:gc)
    (ext:saveinitmem sysout-mem
                     :quiet nil

; We call acl2-default-restart here, even though above we take pains to call it
; in the .acl2rc file, because someone could edit that file but we still want
; the banner printed.

                     :init-function 'acl2-default-restart)))

#+clisp
(defun save-acl2-in-clisp (sysout-name &optional mode mem-name)
  (setq *saved-mode* mode)
  (if (probe-file "worklispext")
      (delete-file "worklispext"))
  (with-open-file (str "worklispext" :direction :output)
                  (format str "mem"))
  (save-acl2-in-clisp-aux sysout-name mem-name))

#+clisp
(defun save-exec-raw (sysout-name)
  (setq *acl2-default-restart-complete* nil)
  (save-acl2-in-clisp-aux sysout-name sysout-name))

#+openmcl
(defun save-acl2-in-openmcl-aux (sysout-name core-name)
  (let* ((current-dir0 (namestring (truename "")))
         (current-dir (if (eql (char current-dir0 (1- (length current-dir0)))
                               #\/)
                          current-dir0
                        (concatenate 'string
                                     current-dir0
                                     "/")))
         (core-name (concatenate 'string
                                 current-dir
                                 core-name
                                 "."
                                 (pathname-name ; Gary Byers suggestion
                                  (car ccl::*command-line-argument-list*))
                                 )))
    (when (probe-file sysout-name)

; At one point we supplied :if-exists :overwrite in the first argument to
; with-open-file below.  Robert Krug reported problems with that solution in
; OpenMCL 0.14.  A safe solution seems to be simply to delete the file before
; attempting to write to it.

      (delete-file sysout-name))
    (when (probe-file core-name)
      (delete-file core-name))
    (with-open-file ; write to nsaved_acl2
     (str sysout-name :direction :output)
     (write-exec-file str
                      "~a~%~s -I ~s -e \"(acl2::acl2-default-restart)\" $*~%"
                      (let ((default-dir
                              (ccl::getenv "CCL_DEFAULT_DIRECTORY")))
                        (if default-dir
                            (format nil
                                    "export CCL_DEFAULT_DIRECTORY=~s"
                                    default-dir)
                          ""))
                      (or (car ccl::*command-line-argument-list*) "openmcl")
                      core-name))
    (chmod-executable sysout-name)
    (ccl::gc)
    (ccl:save-application core-name)))

#+openmcl
(defun save-acl2-in-openmcl (sysout-name &optional mode core-name)
  (setq *saved-mode* mode)
  (in-package "ACL2")
  #-acl2-mv-as-values (proclaim-files)
  #+acl2-mv-as-values (load "acl2-proclaims.lisp")
  (load "openmcl-acl2-trace.lisp")
  (save-acl2-in-openmcl-aux sysout-name core-name))

#+openmcl
(defun save-exec-raw (sysout-name)
  (setq *acl2-default-restart-complete* nil)
  (save-acl2-in-openmcl-aux sysout-name sysout-name))

; Since saved-build-date-string is avoided for MCL, we avoid the following too
; (which is not applicable to MCL sessions anyhow).
#-(and mcl (not openmcl))
(defun save-acl2 (&optional mode other-info
                            
; Currently do-not-save-gcl is ignored for other than GCL.  It was added in
; order to assist in the building of Debian packages for ACL2 based on GCL, in
; case Camm Maguire uses compiler::link.

                            do-not-save-gcl)
  #-akcl (declare (ignore do-not-save-gcl))
  #-(or akcl allegro cmu sbcl clisp openmcl)
  (declare (ignore other-info))

  #+akcl
  (if (boundp 'si::*optimize-maximum-pages*)
      (setq si::*optimize-maximum-pages* nil)) ; Camm Maguire suggestion

; Consider adding something like
; (ccl::save-application "acl2-image" :size (expt 2 24))
; for the Mac.

  (load-acl2)
  (setq *saved-build-date*

; The call of eval below should avoid a warning in cmucl version 18d.  Note
; that saved-build-date-string is defined in interface-raw.lisp.

        (eval '(saved-build-date-string)))
  (eval mode)
  (princ "
******************************************************************************
          Initialization complete, beginning the check and save.
******************************************************************************
")
  (cond
   ((or (not (probe-file *acl2-status-file*))
        (with-open-file (str *acl2-status-file*
                             :direction :input)
                        (not (eq (read str nil)
                                 :initialized))))
    (error "Initialization has failed.  Try INITIALIZE-ACL2 again.")))

  #+akcl
  (save-acl2-in-akcl "nsaved_acl2" other-info mode do-not-save-gcl)
  #+lucid
  (save-acl2-in-lucid "nsaved_acl2" mode)
  #+lispworks
  (save-acl2-in-lispworks "nsaved_acl2" mode)
  #+allegro
  (save-acl2-in-allegro "nsaved_acl2" mode other-info)
  #+cmu
  (save-acl2-in-cmulisp "nsaved_acl2" mode other-info)
  #+sbcl
  (save-acl2-in-sbcl "nsaved_acl2" mode other-info)
  #+clisp
  (save-acl2-in-clisp "nsaved_acl2" mode other-info)
  #+openmcl
  (save-acl2-in-openmcl "nsaved_acl2" mode other-info)
  #-(or akcl lucid lispworks allegro clisp openmcl cmu sbcl)
  (error "We do not know how to save ACL2 in this Common Lisp.")
  (format t "Saving of ACL2 is complete.~%"))

(defun proclaim-files (&optional outfilename)

; IMPORTANT:  This function assumes that the defconst forms in the
; given files have already been evaluated.  One way to achieve this
; state of affairs, of course, is to load the files first.

  (if outfilename
      (format t
              "Writing proclaim forms for ACL2 source files to file ~s.~%"
              outfilename)
    (format t
              "Generating and evaluating proclaim forms for ACL2 source ~
               files.~%"))
  (let (str)
    (when outfilename
      (if (probe-file outfilename)
          (delete-file outfilename))
      (or (setq str (open outfilename :direction :output))
          (error "Unable to open file ~s for output." outfilename))
      (format str "(in-package \"ACL2\")~%"))
    (dolist (fl *copy-of-acl2-files*)
      (proclaim-file (format nil "~a.lisp" fl) str))))

(defun generate-acl2-proclaims ()
  (let ((filename "acl2-proclaims.lisp"))
    #-(and acl2-mv-as-values (or gcl openmcl))
    (progn
      (if (probe-file filename)
          (delete-file filename))
      (with-open-file
       (str filename :direction :output)
       (format str "(in-package \"ACL2\")~%~%")
       (format str "; No proclaims are generated here for this host Lisp.~%"))
      (return-from generate-acl2-proclaims nil))
    (format t "Beginning load-acl2 and initialize-acl2 on behalf of ~
               generate-acl2-proclaims.~%")
    (load-acl2 t)
; Use funcall to avoid compiler warning in (at least) OpenMCL.
    (funcall 'initialize-acl2 'include-book nil nil t)
    (proclaim-files filename)))

(defun acl2 nil
  (let ((*readtable* *acl2-readtable*))
    (dolist (name (remove "defpkgs" *copy-of-acl2-files* :test #'equal))
            (if (equal name "proof-checker-pkg")
                (load "proof-checker-pkg.lisp")
              (load-compiled (make-pathname :name name
                                            :type *compiled-file-extension*))))
    (load "defpkgs.lisp")
    (in-package "ACL2")
    "ACL2"))

; The following avoids core being dumped in certain circumstances
; resulting from very hard errors.

#+akcl
(si::catch-fatal 1)
