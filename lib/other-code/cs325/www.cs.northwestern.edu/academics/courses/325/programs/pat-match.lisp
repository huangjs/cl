;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Super Simple Pattern Matcher 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (PAT-MATCH pattern input [list-of-binding-lists])
;;;   => list of binding lists
;;;
;;; Given a pattern -- an s-expression containing zero or more
;;; variables, such as (a ?x b) -- matches the pattern against
;;; the input and returns a list of binding lists, one binding
;;; list for each successful match.


(defun pat-match (pat input &optional (blists '(nil)))
  (cond ((var-p pat) (match-variable pat input blists))
	((eql pat input) blists)
	((or (atom pat) (atom input)) nil)
	(t (pat-match (cdr pat) (cdr input)
		      (pat-match (car pat) (car input) blists)))))

(defun match-variable (var input blists)
  (cond ((null blists) nil)
	((null (rest blists)) (extend-bindings var input blists))
	(t (loop for blist in blists
		 append (extend-bindings var input (list blist))))))


(defun instantiate-pattern (form blist)
  (sublis blist form))



(defun extend-bindings (var input blists)
  (let ((binding (get-binding var (first blists))))
    (cond ((null binding) (add-binding var input blists))
	  ((equal (binding-value binding) input) blists)
	  (t nil))))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun add-binding (var input blists)
  (list (cons (cons var input) (first blists))))

(defun binding-value (binding) (cdr binding))

(defun var-p (pat)
  (and (symbolp pat) (prefix-p "?" (symbol-name pat))))

(defun prefix-p (seq1 seq2 &key (test #'eql))
  (and (<= (length seq1) (length seq2))
       (every test seq1 seq2)))

(provide "pat-match")