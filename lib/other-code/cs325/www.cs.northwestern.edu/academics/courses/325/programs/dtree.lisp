;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Discrimination tree for patterns
;;; 
;;; Adapted from AI Programming, 2nd ed., Chapters 8 and 11, with many
;;; changes, including
;;;
;;;   - Name changes
;;;   - Function reorganization
;;;   - Put into :DTREE package
;;;   - Separation of item being indexed from its index
;;;   - Multiple items can be stored under the same index
;;;   - List-encoding rather than car-cdr encoding
;;;
;;;   (DTREE:INDEX item pattern link) => item
;;;     Stores the pair (item pattern) in the dtree under the pattern.
;;;
;;;   (DTREE:FETCH input link) => list of (pattern item) pairs
;;;     Retrieves all items stored under patterns that might match
;;;     input. 
;;;
;;;   (DTREE:MAKE-LINK) => link
;;;     Makes an empty link. The link can then be used as the root of
;;;     the discrimination tree.
;;;
;;;   (DTREE:VAR-P pattern) => true or false
;;;     Returns true if pattern should be treated as something that
;;;     can match any single input item.  Default definition: true if
;;;     pattern is a symbol beginning with ?, e.g., ?X.
;;;
;;;   (DTREE:SEGMENT-P pattern) => true or false
;;;     Returns true if pattern should be treated as something that
;;;     can match any single input item. Default definition: true if
;;;     pattern is a list beginning with ?*, e.g., (?* ?X).
;;;
;;; DTREE:VAR-P and DTREE:SEGMENT-P may need to be redefined for the
;;; pattern matcher you're using.  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Example:
;;;
;;;   > (setq l (dtree:make-link))
;;;   #<NIL>
;;;   > (dtree:index 1 '(a ?x c) l)
;;;   1
;;;   > (dtree:index 2 '(?x b c) l)
;;;   2
;;;   > (dtree:fetch '(a b c) l)
;;;   ((2 (?X B C)) (1 (A ?X C)))
;;;   > (dtree:fetch '(a a c) l)
;;;   ((1 (A ?X C)))
;;;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; PACKAGES
;;; --------

(cl:defpackage "DTREE"
  (:use "COMMON-LISP")
  (:export "INDEX" "FETCH" "MAKE-LINK" "VAR-P" "SEGMENT-P")
  )

(in-package "DTREE")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structures and globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A LINK has a key and a list of sublinks.  A terminal link
;;; has some contents, stored in the sublinks field.

(defstruct (link (:print-function print-link))
  key sublinks)

(defun print-link (link stream depth)
  (declare (ignore depth))
  (format stream "#<~S>" (link-key link)))

(defconstant start-list-label (list 'start-list)
  "Link label for start of list")
(defconstant end-list-label (list 'end-list)
  "Link label for end of list")
(defconstant var-label (list 'var) 
  "Link label for variable")
(defconstant segment-label (list 'segment)
  "Link label for segment patterns")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FETCH and INDEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fetch (input link)
  (loop for sublink in (traverse-links input link)
        append (link-sublinks sublink)))

(defun index (item pat link)
  (let ((sublink (establish-links pat link)))
    (if (null sublink)
        (error "Failed to establish links for ~S" pat)
        (add-item item pat sublink))))

(defun add-item (item pat link)
  (push (list item pat) (link-sublinks link))
  item)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Traversing links for FETCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Primary function:
;;;
;;;   (TRAVERSE-LINKS input link) => list of links
;;;     Returns the terminal links retrieved by input.
;;;
;;; All the functions return a list of links. All take an input form
;;; and a link, except TRAVERSE-ELEMENTS and TRAVERSE-SEGMENT which
;;; takes a list of links. 

;;; This first set of functions traverses the links for a single input
;;; item, which may be a list or an atom. The relevant links are those
;;; that match the input, plus an VAR links.

(defun traverse-links (input link)
  (and (not (null link))
       (append (traverse-link var-label link)
               (traverse-sublinks input link))))

(defun traverse-sublinks (input link)
  (cond ((null link) nil)
        ((atom input) (traverse-link input link))
        (t (traverse-list input link))))

(defun traverse-link (input link)
  (and (not (null link))
       (let ((sublink (find-sublink input link)))
         (and (not (null sublink)) (list sublink)))))

(defun find-sublink (key link)
  (find key (link-sublinks link) :key #'link-key))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Traversing an input list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These functions traverse the links for an input list.  The
;;; relevant links are those that match elements of the input list,
;;; plus SEGMENT links, which basically allow the traverser to skip
;;; zero or more input elements.

(defun traverse-list (input link)
  (and (not (null link))
       (traverse-elements input
                          (traverse-link start-list-label link))))

(defun traverse-elements (input links)
  (loop for link in links
        if (null input)
           append (traverse-link end-list-label link)
        else
           append (traverse-elements (rest input)
                    (traverse-links (first input) link))
        append (traverse-segment input 
                 (traverse-link segment-label link))))

(defun traverse-segment (input links)
  (and (not (null links))
       (loop for l on input
             append (traverse-elements l links))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Establishing links for INDEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Primary function:
;;;
;;;   (ESTABLISH-LINKS pat link) => link
;;;     Adds the sublinks necessary to add pat to the dtree under
;;;     link. Returns the leaf link.
;;;
;;; All the ESTABLISH-... subfunctions take an input form and a link
;;; and return a link.
;;;
;;; The user can specify labels to be used for certain patterns, e.g.,
;;; *VAR* for all simple variables and *SEGMENT-VAR* for all segment
;;; variables. 

(defun establish-links (pat link)
  (cond ((var-p pat)
         (establish-link var-label link))
        ((segment-p pat)
         (establish-link segment-label link))
        ((atom pat)
         (establish-link pat link))
        (t (establish-list pat
             (establish-link start-list-label link)))))

(defun establish-link (pat link)
  (or (find-sublink pat link)
      (add-sublink pat link)))

(defun establish-list (pat link)
  (cond ((null pat)
         (establish-link end-list-label link))
        (t (establish-list (rest pat)
             (establish-links (first pat) link)))))

(defun add-sublink (pat link)
  (let ((sublink (make-link :key pat)))
    (push sublink (link-sublinks link))
    sublink))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default VAR-P and SEGMENT-P
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun var-p (pat)
  (and (symbolp pat)
       (prefix-p "?" (symbol-name pat))))

(defun prefix-p (seq1 seq2 &key (test #'eql))
  (and (<= (length seq1) (length seq2))
       (every test seq1 seq2)))

(defun segment-p (pat)
  (and (consp pat)
       (symbolp (first pat))
       (string-equal (first pat) '?*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pretty-print discrimination tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-links (link &optional (left-margin 0))
  (let ((sublinks (link-sublinks link)))
    (cond ((null sublinks) (values))
          ((link-p (first sublinks))
           (loop for sublink in sublinks
                 do (format t "~%~VT~S" left-margin (link-key sublink))
                    (display-links sublink (+ left-margin 3))))
          (t
           (format t "~%~VT =>~{ ~S~}" left-margin sublinks)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(defun test-dtree ()
  (let ((dtree (dtree:make-link)))
    (flet (
           (test-index (item input)
             (format t "Indexing ~S under ~S~%" 
                     item input (dtree:index item input dtree)))
           (test-fetch (input)
             (format t "Fetching ~S => ~S~%" 
                     input (mapcar #'car (dtree:fetch input dtree))))
           )

      (test-index 1 'a)
      (test-index 2 '(a b c))
      (test-index 3 '(a ?x c))
      (test-index 4 '?x)
      (test-index 5 '(?x ?y ?z))
      (test-index 6 nil)
      (test-index 7 '(a (?* ?x) c))

      (format t "++++++++++++++++++++++++++++++++~%")

      (test-fetch 'a)
      (test-fetch '(a c))
      (test-fetch '(a b c))
      (test-fetch '(a b c d))
      (test-fetch '(a b b b c))
      (test-fetch '(a))
      (test-fetch nil)


      dtree
      )))

|#
