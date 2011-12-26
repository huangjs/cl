;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; additional spacing criteria fed in via (spacing-hook score) to take care of lyrics,
;;;   or other complicated (normally text-related) justification requirements.
;;;   The spacing-hook function is called after the overall "time-line" has been
;;;   computed, giving all the score-wide spacing requests, timing info, and so on.
;;;   From that, we can run through, make sure each piece of our text has space,
;;;   and if not, add the space, rippling the entire time-line as we go. 
(in-package :cmn) 

(defclass lyrics (odb text)
  ((verse :initarg :verse :initform 1 :accessor verse)))

(defmethod lyrics-p ((obj t)) nil)
(defmethod lyrics-p ((obj lyrics)) t)

(deferred-action verse)

;;; now we have our own text subclass for lyrics that can handle the added "message" verse --
;;;   this gives us a way to handle vertical stacks of verses automatically.

(defun lyrics (&rest args)		;this is our version of "text" 
  (apply #'ur-text (make-instance 'lyrics) 
	 (font-name "Times-Roman") (font-scaler .4) 
	 (y #'(lambda (mark note score &optional justifying)
		(declare (ignore score justifying))
		(- (+ (staff-y0 note) (vis-dy mark)) (+ .75 (* (1- (verse mark)) (font-scaler mark))))))
	 (x #'(lambda (mark note score &optional justifying)
		(declare (ignore score justifying))
		(+ (vis-dx mark) (x0 note))))
	 args))

;;; now we can call something like (cmn staff treble c4 q (lyrics "old MacDonald" (verse 14)))

(defun lyric-length (lyr) 
  (* .65 (or (font-scaler lyr) 1.0) (length (letters lyr))))

(defun gather-lyrics (score)
  (let ((all-lyrs nil))
    (loop for sys in (systems score) do
      (loop for stf in (staves sys) do
	(loop for obj in (staff-data stf) do
	  (when (and (marks-p obj) (marks obj))
	    (let ((lyrs (loop for m in (marks obj) if (lyrics-p m) collect m)))
	      (when lyrs
		(loop for lyr in lyrs do
		  (setf (odb-onset lyr) (odb-onset obj))
		  (push lyr all-lyrs))))))))
    all-lyrs))

(defun lyrical-spacing (score)
  ;; our first task is to go gather all our lyrics, set up our own time-line with needed space info
  (let* ((all-lyrics (gather-lyrics score))
	 (all-lyrics-in-order (sort all-lyrics #'< :key #'odb-onset))
	 (lyric-time-line nil)
	 (lyric-onset (odb-onset (first all-lyrics-in-order)))
	 (lyric-space 0))
    (loop for lyr in all-lyrics-in-order do
      (if (= (odb-onset lyr) lyric-onset)
	  (setf lyric-space (max lyric-space (lyric-length lyr)))
	(progn
	  (push (list lyric-onset lyric-space) lyric-time-line)
	  (setf lyric-onset (odb-onset lyr))
	  (setf lyric-space (lyric-length lyr)))))
    (push (list lyric-onset lyric-space) lyric-time-line)
    (let* ((overall-spacing (nreverse lyric-time-line))
	   (tlb (time-line score))
	   (tlf nil)
	   (cur-fx 0.0)
	   (current-added-space 0.0))

      (loop for td in (time-line score) do
	(incf cur-fx (tld-fx0 td))
	(setf (tld-acc-x td) (+ (tld-cx td) cur-fx))
	(incf cur-fx (tld-fx1 td)))

      (loop while overall-spacing do
	(let* ((curlyr (pop overall-spacing))
	       (curonset (first curlyr))
	       (curspace (second curlyr))
	       (nxtlyr (first overall-spacing))
	       (nxtonset (or (and nxtlyr (first nxtlyr)) (and tlb (tld-time (first (last tlb)))) 10000))
	       (available-space 0))
	  (loop while (and tlb (> curonset (tld-time (first tlb)))) do (setf tlb (cdr tlb)))
	  (setf tlf (cdr tlb))
	  (loop while (and tlf (> nxtonset (tld-time (first tlf)))) do (setf tlf (cdr tlf)))
	  (setf available-space (- (tld-acc-x (first tlf)) (tld-acc-x (first tlb))))
	  (when (< available-space curspace)
	    (setf current-added-space (- curspace available-space))
	    (let ((cx0 (tld-cx (first tlb)))
		  (cx1 (tld-cx (first tlf)))
		  (x0 (tld-x (first tlb)))
		  (x1 (tld-x (first tlf))))
	      (setf tlb (cdr tlb))
	      (loop while (not (eq tlb tlf)) do
		(setf (tld-x (first tlb)) (+ x0 (* current-added-space (/ (- (tld-x (first tlb)) x0) (- x1 x0)))))
		(setf (tld-cx (first tlb)) (+ cx0 (* current-added-space (/ (- (tld-cx (first tlb)) cx0) (- cx1 cx0)))))
		(setf tlb (cdr tlb)))
	      (loop for td in tlf do 
		(incf (tld-x td) current-added-space)
		(incf (tld-cx td) current-added-space)))))))))

;;; further features:  automatically change the next staff's y placement to accommodate verses



(defmethod copy ((lyr lyrics) &optional object)
  (let ((new-lyr (if (not object) (make-instance 'lyrics)
		       (if (write-protected object) (copy object)
			 object))))
    (setf (verse new-lyr) (verse lyr))
    (if (next-method-p) (call-next-method lyr new-lyr))
    new-lyr))

(defmethod descry ((lyr lyrics) &optional stream controller)
  (format stream "(lyrics :verse ~A~A" 
	  (verse lyr)
	  (if (next-method-p) (call-next-method lyr stream (or controller lyr)) "")))

