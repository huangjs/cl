;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple MOP-based Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Last update: 11/10/95 CKR


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (unless (find-package :parse)
    (make-package :parse 
                  :use (list (or (find-package :common-lisp)
                                 (find-package :lisp))))))


(in-package :parse)

(require "mops")
(use-package :mops)

(export '(defphrase concept-phrases phrase-concepts map-phrases
          parse pprint-parse test-parse *parses* string->list 
         ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals and structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Concept phrase pairs join a concept with a phrase that can parse
;;; to that concept.

(defstruct cp concept phrase)

;;; *CONCEPT-PHRASES* holds all the concept-phrase pairs.

(defvar *concept-phrases* nil "All concept-phrase pairs")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFPHRASE, CONCEPT-PHRASES, PHRASE-CONCEPTS, MAP-PHRASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro defphrase (concept &rest phrase)
  (when (endp phrase)
     (error "~S not a legal phrase" phrase))
  `(add-phrase ',concept ',phrase))

(defun add-phrase (concept phrase)
  (when (endp phrase)
    (error "~S not a legal phrase" phrase))
  (pushnew (make-cp :concept concept :phrase phrase)
           *concept-phrases*
           :test #'equalp)
  phrase)

(defun phrase-concepts (x)
  (let ((phrase (if (listp x) x (list x))))
    (loop for cp in *concept-phrases*
          when (equal phrase (cp-phrase cp))
          collect (cp-concept cp))))

(defun concept-phrases (concept)
  (loop for cp in *concept-phrases*
        when (eql concept (cp-concept cp))
        collect (cp-phrase cp)))
 
(defun map-phrases (fn)
  (mapc #'(lambda (cp)
            (funcall fn (cp-concept cp) (cp-phrase cp)))
        *concept-phrases*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PARSE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (PARSE input) => list of parses
;;;   Returns a sorted list of parses the input.
;;; (TEST-PARSE input) => no values
;;;   Prints the concepts and bindings parsed from input. Also
;;;   sets *PARSES* to the list of parses.
;;;
;;; Ex. (TEST-PARSE '(CATCH A BUG))
;;;
;;; Parses are sorted based on amount of input matched (the more,
;;; the better), and specificity of concept identified (the more
;;; specific, the better).

(defun parse (input)
  (sort-parses (match-cp-list (fetch-cp-list input) input)))

(defvar *parses* nil
        "Most recent parse results, to help debugging.")

(defun test-parse (input)
  (setq *parses* (parse input))
  (mapc #'pprint-parse *parses*)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parses and Concept phrases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A parse has a concept, role bindings for the concept, the input
;;; words that were used, and the tail of the input that was
;;; not used, if any.

(defstruct parse concept bindings used input)

(defun pprint-parse (parse)
  (format t "~&~S~{~{~%  ~S ~S~}~}~%"
          (parse-concept parse) (parse-bindings parse)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FETCH-CP-LIST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (FETCH-CP-LIST input) => list of concept phrase pairs
;;;    Returns all concept phrases that might parse input, which
;;;    is a list of words.
;;; 
;;; This function should be cheap (low CONSing), return all phrases
;;; that might parse some part of input, but discriminating enough
;;; to avoid sending obviously unfit phrases to detailed matching.
;;;
;;; The implementation below returns every phrase that:
;;;
;;;   - is no longer than the input
;;;   - has no words not in the input
;;;   - has words in the same relative order as the input
;;;
;;; Note that the input may have words not in the phrase, and that
;;; a phrase with all keywords only rejects inputs shorter than it.
;;;
;;; The only CONSing is the building of the list of concept phrase
;;; pairs in FETCH-CP-LIST.

(defun fetch-cp-list (input)
  (loop for cp in *concept-phrases*
        when (phrase-possible-p (cp-phrase cp) input)
        collect cp))

(defun phrase-possible-p (phrase input)
  (cond ((null phrase) t)
    ((null input) nil)
    ((keywordp (first phrase))
     (phrase-possible-p (rest phrase) (rest input)))
    (t (word-possible-p phrase input))))

(defun word-possible-p (phrase input)
  (let ((l (member (first phrase) input)))
    (and (not (null l))
         (phrase-possible-p (rest phrase) (rest l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MATCH-CP-LIST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (MATCH-CP-LIST concept-phrase-list input) => list of parses
;;;   Returns all the successful parses, applying the concept phrases
;;;   to the input.

;;; (MATCH-CP-P concept-phrase input) => list of parses
;;;   Returns all the parses when concept-phrase is matched against
;;;   the input. 
;;;
;;; A phrase may contain words or role names. Words match directly.
;;; Role names require a recursive check for concept phrases that 
;;; could fill that role and that match the input.
;;;
;;; *CP-LIST* is dynamically bound to all the possibly matching
;;; concept phrases, as originally calculated by FETCH-CP-LIST. 
;;; It's the only list we need when checking for nested matches.
;;; We use a dynamic variable because (1) we're not changing
;;; the list, so order of side effects is not an issue, (2) we
;;; access relatively infrequently, so efficiency is not an 
;;; issue, and (3) it's long enough to mess up tracing the
;;; parsing functions if passed as a parameter.

(defvar *cp-list* nil)

(defun match-cp-list (*cp-list* input)
  (loop for cp in *cp-list*
        append (match-cp cp input)))

(defun match-cp (cp input)
  (match-phrase (cp-phrase cp) input (cp-concept cp)))

(defun match-phrase (phrase input concept &optional bindings used)
  (cond
    ((null phrase)
     (list (make-parse :concept concept :bindings bindings 
                       :used used :input input)))
    ((null input) nil)
    (t (match-item (first phrase) (rest phrase)
                   input concept bindings used))))

(defun match-item (item phrase input concept bindings used)
  (cond
    ((keywordp item)
     (match-role item phrase input concept bindings used))
    (t (match-word item phrase input concept bindings used))))
    
(defun match-role (role phrase input concept bindings used)
  (let ((constraint (inherit-filler concept role)))
    (loop for cp in *cp-list*
          when (abstp constraint (cp-concept cp))
          append (continue-parses
                  (match-cp cp input) role phrase concept bindings used))))

(defun continue-parses (parses role phrase concept bindings used)
  (loop for parse in parses
        append (match-phrase phrase (parse-input parse) concept
                             (add-parse-binding role parse bindings)
                             (append used (parse-used parse)))))

(defun add-parse-binding (role parse bindings)
  (cons (list role (cons (parse-concept parse)
                         (parse-bindings parse)))
        bindings))

(defun match-word (word phrase input concept bindings used)
  (let ((l (member word input)))
    (and (not (null l))
         (match-phrase phrase (rest l) concept bindings (cons word used)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SORT-PARSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (SORT-PARSES parse-list) => parse-list
;;;   Sorts the parse list, putting preferred items first.
;;;
;;; Parses are sorted based on amount of input matched (the more,
;;; the better), and, secondarily, by the specificity of concept
;;; identified (the more specific, the better).
;;;
;;; These are not very good rules. For example, for parses of
;;; (catch kangaroo rat), (m-catch-animal :object m-kangaroo-rat)
;;; is best, but (m-kangaroo-rat) and (m-catch-animal :object m-rat)
;;; are equivalent, because both use two input words, and neither is
;;; more specific than the other.

(defun sort-parses (parses)
  (sort parses 'prefer-parse-p))

(defun prefer-parse-p (parse1 parse2)
  (let ((parse1-len (length (parse-used parse1)))
        (parse2-len (length (parse-used parse2))))
    (or (> parse1-len parse2-len)
        (and (= parse1-len parse2-len)
             (strict-abstp (parse-concept parse2)
                           (parse-concept parse1))))))

(defun strict-abstp (abst spec)
  (and (not (eql abst spec))
       (abstp abst spec)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRING->LIST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (STRING->LIST string) => list of items

(defun string->list (string)
  (let ((eof (list nil)))
    (with-input-from-string (stream string)
      (do ((x (read stream nil eof) (read stream nil eof))
           (l nil (cons x l)))
          ((eq x eof) (nreverse l))))))
  


(provide "parse")


#| Change Log

11/10/95 CKR
Change: Exported *PARSES*.

11/10/95 CKR
Problem: (phrase-possible-p '(to to x) '(to x)) => T
Cause: Missing call to REST in WORD-POSSIBLE-P
Change: Add call to REST.
|#
