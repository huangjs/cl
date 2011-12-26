;;; Simple inefficient answer to Qual 96 programming question


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (PARSE input-words) => sorted list of scored target concepts
;;; 
;;; Example:
;;;
;;;   > (PARSE '(RAKE THE GARDEN))
;;;   ((30 M-RAKE-GARDEN) (5 M-REMOVE-WEEDS-FROM-GARDEN) (5 M-WATER-GARDEN))
;;;
;;; PARSE takes an input text (list of words) and returns a list
;;; of target concepts that had index sets that intersected with
;;; concepts referred to by the input. How well those index sets
;;; intersect with the input concepts is reflected in the score. 


(defun parse (input-words)
  (get-targets (get-input-concepts input-words)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; From input words to concepts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (GET-INPUT-CONCEPTS words) => list of concepts
;;;   Returns the concepts referred to by the phrases and concepts
;;;   in input-words. 

(defun get-input-concepts (input-words)
  (loop for words on input-words
        until (null words)
        append (get-initial-phrase-concepts words)))

;;; (GET-INITIAL-PHRASE-CONCEPTS words) => list of concepts
;;;   Returns the concepts referred to by all phrases that match
;;;   words, starting with the first word in words.
;;; 
;;; Data structures:
;;;   pc (phrase concepts) = a phrase and a list of concepts

(defun get-initial-phrase-concepts (words)
  (loop for pc in (get-all-phrases)
        when (phrase-match-p (pc-phrase pc) words)
        append (pc-concepts pc)))

;;; (PHRASE-MATCH-P phrase words) => true or false
;;;    Returns true if words starts with the first item in phrase
;;;    and contains the remaining items in the same order.

(defun phrase-match-p (phrase words)
  (or (null phrase)
      (and (not (null words))
           (eql (first phrase) (first words))
           (phrase-match-p (rest phrase)
                           (member (first (rest phrase)) words)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; From input concepts to target concepts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (GET-TARGETS concepts) => list of scored concepts
;;;   Returns a list of scored concepts, sorted by scores, highest
;;;   scores first.
;;;
;;; Data structures:
;;;   ic (indexed concepts = an index-set and a list of concepts
;;;   sc (scored concepts) = a score and a list of concepts

(defun get-targets (input-pool)
  (sort (get-scored-concepts input-pool)
        #'> :key #'sc-score))

(defun get-scored-concepts (input-pool)
  (loop for ic in (get-all-indexed-concepts)
        for score = (score-index-set (ic-index-set ic) input-pool)
        unless (null score)
          collect (make-sc score (ic-concepts ic))))

(defun score-index-set (indices input)
  (let ((indices-used (count-indices-used indices input)))        ;;; p+s
    (cond ((zerop indices-used) nil)
          (t (score-match indices-used 
                          (count-inputs-unused indices input)     ;;; p-s
                          (- (length indices) indices-used))))))  ;;; s-p

(defun count-indices-used (indices input)
  (count-if #'(lambda (c) (member c input :test #'abstp)) indices))

(defun count-inputs-unused (indices input)
  (count-if-not #'(lambda (c) (member c indices :test #'specp)) input))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; From here on, we're past the official part of the qual answer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scoring matches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fitzgerald's summation formula is more complex. It also takes into
;;; account the "information value" of each concept. This could
;;; be handled in this code by changing the counting functions above.

(defun score-match (p+s p-s s-p)
  (- (* 10 p+s) p-s s-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Memory functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Everything is stored in lists for easy initialization and
;;; iteration.

(defvar *absts* nil)
(defvar *indices* nil)
(defvar *phrases* nil)

;;; Abstractions

(defun abstp (a s)
  (or (eql a s)
      (member a (rest (absts-of s)))))

(defun specp (s a) (abstp a s))

(defun absts-of (s) (assoc s *absts*))


;;; Indexed concepts

(defun get-all-indexed-concepts () *indices*)

(defun ic-index-set (ic) (first ic))
(defun ic-concepts (ic) (rest ic))


;;; Scored concepts

(defun sc-score (sc) (first sc))
(defun sc-targets (sc) (rest sc))

(defun make-sc (score concepts) (cons score concepts))

;;; Phrases

(defun get-all-phrases () *phrases*)

(defun pc-phrase (pc) (first pc))
(defun pc-concepts (pc) (rest pc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Robot gardener memory base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *absts*
      '((m-rake m-object)
        (m-weed m-plant m-object)

        (m-move m-action)
        (m-move-rake m-move m-action)
        (m-rake-garden m-move-rake m-move m-action)

        (m-transfer m-action)
        (m-transfer-water m-transfer m-action)
        (m-water-garden m-transfer-water m-transfer m-action)

        (m-remove m-action)
        (m-remove-weeds m-remove m-action)
        (m-remove-weeds-from-garden m-remove-weeds m-remove m-action)
        ))

(setq *indices*
      '(((m-rake m-move-rake m-garden) m-rake-garden)
        ((m-weed m-remove m-garden) m-remove-weeds-from-garden)
        ((m-water m-transfer-water m-garden) m-water-garden)
        ))

(setq *phrases*
      '(((garden) m-garden)
        ((water) m-water m-transfer-water)
        ((water garden) m-water-garden)
        ((rake) m-rake m-move-rake)
        ((rake garden) m-rake-garden)
        ((weed) m-weed m-remove-weeds)
        ((weed garden) m-remove-weeds-from-garden)
        ))