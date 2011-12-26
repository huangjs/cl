;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.
;;;;  These routines write the ps code stream to a file.

;;;  This outputs a single token.  Beware of case problems.  A very hokey
;;;  algorithm is user for formatting.  Read the generated postscript
;;;  at your own risk!

(defun write-token (tok)
  (cond ((consp tok)  ; only for quoted items and frame sizes (& null)
	 (cond ((eq (car tok) 'quote)
		(write-token-1 (cadr tok) T))
	       ((eq (car tok) 'save-temp)
		(when (caddr tok)
		      (write-token 'dup)
		      (write-token `',(caddr tok))
		      (write-token 'exch)
		      (write-token 'store)))
	       ((eq (car tok) 'static-alloc)
		(write-frame (cadr tok) T)
		(write-token (att frame-table (cadr tok) 'size))
		(write-token 'dict)
		(write-token 'def))
	       ((eq (car tok) 'alloc-frame)
		(let ((frame (cadr tok)))
		  (if (or (att frame-table frame 'non-recursive)
			  (att frame-table frame 'parent))
		      (write-frame frame nil)
		    (let ((dependants nil))
		      (for (:in k (att frame-table frame 'children))
			   (:do
			    (when
			     (not (att frame-table k 'non-recursive))
			     (push k dependants))))
		      (write-token (+ (length dependants)
				      (att frame-table frame 'size)))
		      (write-token 'dict)
		      (for (:in d dependants)
			   (:do (init-dep d)))
		      ))))))
	(T (write-token-1 tok nil))))

;;; internal entry in case a quote is needed.  Quotes map to / in ps.

(defun write-token-1 (tok quote-it)
    (cond ((symbolp tok)  ; a symbol?
	   (write-str (or (get tok 'New-ps-name) (ps-symbol-name tok))
		   nil quote-it)); look for wired in name
	  ((stringp tok) ; strings get wrapped in parens
	   (write-str tok T nil))
	  ((numberp tok) ; numbers go out as just numbers!
	   (get-col 5)  ; crummy bound on size of a number
	   (format ps-output "~A" tok))
	  ((characterp tok)
	   (write-token-1 (char-code tok) nil))   ; convert chars to ints
	  ((or (vectorp tok) (consp tok)) ; vectors are wrapped in [ and ]
	   (write-token-1 '\[ nil)
	   (for (:from i 0 (1- (length tok)))
		(:do (write-token-1 (elt tok i) T)))
	   (write-token-1 '\] nil)))
    nil)

(defun write-frame (f quotit)
  (if (symbolp f)
      (progn
	(write-str (symbol-name f) nil quotit)
	(format ps-output "--frame")
	(incf current-col 7))
      (progn
	(let ((p (att frame-table f 'parent)))
	  (get-col 12)
	  (when quotit (format ps-output "/"))
	  (format ps-output "~A--FR-~A" (if (symbolp p) p 'TEMP) f)
	(incf current-col 10)))))
      

(defun init-dep (f)
  (write-token 'dup)
  (write-frame f t)
  (write-token (att frame-table f 'size))
  (write-token 'dict)
  (write-token 'put))


;;; Most output goes here.  Send out a string, possibly with / or ()
;;; Watch for special chars in strings.

(defun write-str (str paren quot)
    (get-col (+ 2 (length str))) ; make room for string + a little slop
    (when paren (write-char #\( ps-output))
    (when quot (write-char #\/ ps-output))
    (for (:from i 0 (1- (length str)))
	 (:do
	     (let ((ch (elt str i)))
		 (when (and paren (member ch '(#\( #\)))) 
		     (write-char #\\ ps-output))
		 (write-char ch ps-output))))
    (when paren (write-char #\) ps-output))
)

;;;  Write out a symbol.  The problem is that LISP looses case info.
;;;  Asume that a string which is all upper case (which is normal) should
;;;  be changed to lower case.  Otherwise, leave case alone.  You cant use
;;;  a name which is all caps without the new-ps-name property.

(defun ps-symbol-name (tok)
    (let* ((name (symbol-name tok))
	   (len (length name)))
        (prog (i)   ; the pcls compiler wont compile return-from
            (setf i 0)
	    nxt
	    (if (= i len) (return (string-downcase name)))
	    (if (lower-case-p (elt name i)) (return name))
	    (incf i)
	    (go nxt))))

;;;  Finish a line of output

(defun clear-line ()
   (write-char #\newline ps-output)
   (setf current-col 0))

;;; get ready for a token of some size.  Flush current line of it would
;;; run over the end.

(defun get-col (n)
  (if (= current-col 0)
    (setf current-col n)
    (if (> (+ current-col n) 70)
        (clear-line)
	(write-char #\space ps-output)))
  (setf current-col (+ current-col n 2)))

;;;  This is the top level write routine.  

(defun ps-write-code ()
    (setf current-col 0)  ; output column
    (format ps-output "%!~%")  ; our laser writer spooler needs this
    (for (:in x (nreverse dict-code))
	 (:do (write-token x)))
    (for (:in x (nreverse init-code))
	 (:do (write-token x)))
    (for (:in x (nreverse fn-code))
	 (:do (write-token x)))
    (for (:in x (nreverse main-code))
	 (:do (write-token x)))
    (clear-line))  ; finish up!

