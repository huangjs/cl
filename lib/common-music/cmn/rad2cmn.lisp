#||

  Kjetil Matheussen 2006.

These are functions to help create scores from music created with the
graphical music editor "Radium", http://www.notam02.no/radium/

Look at the bottom of the file for examples.

||#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar filename NIL)

(defvar streem NIL)
(defvar song NIL)
(defvar root NIL)
(defvar songdata NIL)
(defvar g-invisible-rests NIL)
(defvar g-transpose 0)
(defvar g-add-tempos t)
(defvar g-add-measures t)
(defvar g-add-dynamics t)
(defvar g-dynamics-dy -0.2)
(defvar g-filter-func nil)
(defvar g-add-linenums NIL)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Useful functions and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
       ,@body))


(defun c-scale (x x1 x2 y1 y2)
  (+ y1
     (/ (* (- x x1)
	   (- y2 y1))
	(- x2 x1))))

(defun c-last (l)
  (car (last l)))

(defun string-to-number (s &optional (b 10)) 
  (let* ((*read-base* b) 
	 (n (read-from-string s nil)))
    (if (numberp n) 
	n 
      (error (format t "String ~S does not contain a NUMBER." s)))) )







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Reading and analyzing radium files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Convert the content of a .rad file into a big s-expression.
(defun readsong (streem)
  (let ((ret '())
	(doloop t))
    (while doloop
      (let ((line (read-line streem NIL)))
	(cond ((or (not line)
		   (string= "/" line))
	       (setf doloop NIL))
	      ((string= "\\" line)
	       (push (readsong streem) ret))
	      ((string= "" line)
	       NIL)
	      (t 
	       (push (cond ((digit-char-p (aref line 0))
			    (string-to-number line))
			   ((alpha-char-p (aref line 0))
			    (read-from-string line nil))
			   (t line))
		     ret)))))
    (nreverse ret)))


;; Extract a value from an s-expression version of a rad-song.
;; "list" can be any part of a rad-song. (block, track, root, song, etc.)
;; "val" can either be a string, in which case it will return the next value after "val".
;; Or, "val" can be a list, which in case it will return the first sublist (of "list")
;; that "val" is equal to.
(defun get-rad-value (list val &optional (report-error t))
  (cond ((null list) 
	 (if report-error
	     (error (format t "get-rad-value: Could not find ~A." val))
	   nil))
	((and (consp val)
	      (consp (car list))
	      (symbolp (car (car list)))
	      (equal (butlast (car list) (- (length (car list)) (length val))) val))
	 (car list))
	((and (stringp val)
	      (stringp (car list))
	      (string-equal (car list) val))
	 (cadr list))
	(t
	 (get-rad-value (cdr list) val report-error))))


#||
(let* ((streem (open filename))
       (ret (readsong streem)))
  (close streem)
  (get-rad-value (get-rad-value song '(ROOT)) '(SONG)))

(get-rad-value (get-block 1) '(LPBs))
(get-rad-value (get-block 1) '(TRACK 3))
||#



(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push x acc))))
    (nreverse acc)))

#||
(filter #'atom '(2 3 4))
||#

(defun filtermap (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

#||
(filtermap #'atom '(2 3 4))
(floor (/ 10 2))
(mod 10 2)
||#

(defun get-block (n)
  (get-rad-value songdata `(BLOCK ,n)))

#||
(get-block 100)
||#

(defun get-track (block n)
  (get-rad-value block `(TRACK ,n)))

(defun get-notes (track)
  (filter #'(lambda (val)
	      (and (consp val)
		   (eq (car val) 'NOTE)))
	  track))
		   
#||
(cadr (get-block 2))
(get-notes (get-track (get-block 1) 1))
(get-track (get-block 2) 1)
||#




(defun get-stops (track)
  (labels ((build (stops)
		  (when stops
		    (cons (+ (first stops) (/ (second stops) (third stops)))
			  (build (nthcdr 3 stops))))))
	  (build (cdr (car (member-if #'(lambda (val)
					  (and (consp val)
					       (eq (car val) 'STOPS)))
				      track))))))

#||
(get-stops (get-track (get-block 1) 0))
(get-track (get-block 2) 1)
||#


(defun make-queue (alist)
  (lambda ()
    (let ((ret (car alist)))
      (setf alist (cdr alist))
      ret)))
(defun make-circular-queue (list)
  (let ((alist list))
    (lambda ()
      (let ((ret (car alist)))
	(setf alist (cdr alist))
	(if (null alist)
	    (setf alist list))
	ret))))
#||
(setf queue (make-circular-queue '(a b c d e f)))
(funcall queue)
||#


(defun c-display (&rest rest)
  (print (cons "c-display:" rest))
  nil)
#||
(c-display 1 2 3)
||#


(defun get-lpbs (block)
  (let ((lpbs (make-queue (cdr (get-rad-value block '(LPBs) nil))))
	(ret '())
	(doloop t))
    (while doloop
      (let* ((line (funcall lpbs))
	     (counter (funcall lpbs))
	     (dividor (funcall lpbs))
	     (val (funcall lpbs)))
	(if (not line)
	    (setf doloop nil)
	  (push (list (+ line (/ counter dividor)) val) ret))))
    (nreverse ret)))

#||
(get-lpbs (get-block 1))
||#



(defun get-bpms (block)
  (let ((bpms (make-queue (cdr (get-rad-value block '(TEMPOS) nil))))
	(ret '())
	(doloop t))
    (while doloop
      (let* ((line (funcall bpms))
	     (counter (funcall bpms))
	     (dividor (funcall bpms))
	     (val (funcall bpms)))
	(if (not line)
	    (setf doloop nil)
	  (push (list (+ line (/ counter dividor)) val)
		ret))))
    (nreverse ret)))

#||
(get-bpms (get-block 1))
(get-lpbs (get-block 1))
||#



(defun get-block-end (block &optional (measuretrack 0))
  (let* ((track (if (numberp measuretrack) (get-track block measuretrack) measuretrack))
	 (stop (first (get-stops track))))
    (if stop
	stop
      (get-rad-value block "?num_lines"))))

#||
(get-block-end (get-block 2))
(get-stops (get-track (get-block 1) 0))
||#	


(defun get-block-start (block &optional (measuretrack 0))
  (let ((note (first (member-if #'(lambda (val)
				    (and (consp val)
					 (eq (car val) 'NOTE)))
				(if (numberp measuretrack)
				    (get-track block measuretrack))))))
    (+ (second note) (/ (third note) (fourth note)))))


#||
(get-block-start (get-block 1))
||#

(defun get-root-bpm (root)
  (get-rad-value root "?tempo"))
(defun get-root-lpb (root)
  (get-rad-value root "?lpb"))



#||
(get-root-bpm root)
(get-root-lpb root)
||#


;;
;; Returns a list with the same length as the number of lines in the block.
;; Each element of the list contains the time it takes in seconds to play that line. (if we ignore reltempo and reltempolines)
;; Important: Putting a tempo-change between two lines will not work. (which probably is a good thing...)
(defun get-timings (block &optional (measuretrack 0))
  (let* ((bpm (get-root-bpm root))
	 (lpb (get-root-lpb root))
	 (next-bpm (make-queue (get-bpms block)))
 	 (next-lpb (make-queue (get-lpbs block)))
	 (bpm-next (funcall next-bpm))
	 (lpb-next (funcall next-lpb)))
    
    (mapcar #'(lambda (line)
	 	(while (and (not (null bpm-next))
		 	    (<= (car bpm-next) line))
		  (setf bpm (cadr bpm-next))
 		  (setf bpm-next (funcall next-bpm)))
		(while (and (not (null lpb-next))
	 		    (<= (car lpb-next) line))
		  (setf lpb (cadr lpb-next))
		  (setf lpb-next (funcall next-lpb)))
		(/ 60 (* bpm lpb)))
	    (loop for line from 0 to (get-block-end block measuretrack)
		  collect line))))


      

#||
(get-timings (get-block 0))
(nthcdr 42 (get-timings (get-block 1)))

(get-bpms (get-block 1))
(get-lpbs (get-block 1))
(sort (append '((0 a) (1 b)) '((0.5 c))) #'(lambda (a b) (< (car a) (car b))))
(loop for i from  0 to 10
      collect i)
(get-rad-value (get-block 1) "?num_lines")
||#


(defun get-chroma (notenum)
  (mod notenum 12))
(defun get-notechroma (notenum)
  (nth (get-chroma notenum) '(c c d d e f f g g a a h)))
(defun get-octave (notenum)
  (floor notenum 12))
#||
(list (get-chroma 41)
      (get-octave 41)
      (get-notechroma 43)
      (get-notechroma 43))
      
(symbol-name 'aiai)
||#



;; Returns '((a b c d)(a b c d)...) where a=placemenet, b=number of beats in measure, c=denuminator for measure, d=beat number in measure.
;;
;; Octate 1 -> 32-notes
;; Octeve 2 -> 16-notes
;; Octave 3 -> 8-notes
;; Octave 4 -> quarter-notes
(defun get-measures (block &optional (tracknum 0))
  (let* ((track (get-track block tracknum))
	 (block-end (get-block-end block track))
	 (firstpass (mapcar #'(lambda (note)
				(let* ((place (+ (nth 1 note) (/ (nth 2 note) (nth 3 note))))
				       (octave (get-octave (nth 4 note)))
				       (chroma (get-chroma (nth 4 note)))
				       (denuminator (nth octave '(-1 32 16 8 4 2 1)))
				       (beatnum (nth chroma '(0 7 1 8 2 3 9 4 10 5 11 6))))
				  (list place
					denuminator
					beatnum)))
			    (get-notes track))))

    ;; Removing end of meaure list in case its explicitly stopped with a STP, and builds up the beatnum list.
    (let ((lastdenuminator nil)
	  (lastbeat nil)
	  (numbeats '()))
      (labels ((remove-end (mlist)
			   ;;(c-display "aiai:" mlist)
			   (let* ((measure (car mlist))
				  (place (first measure))
				  (beatnum (third measure))
				  (denuminator (second measure)))
			     (cond ((null mlist) nil)
				   ((>= (first measure) block-end) nil)
				   (t (if (and lastbeat
					       (= 0 beatnum))
					  (push (1+ lastbeat) numbeats))
				      (if (and lastbeat
					       (and (not (= beatnum 0))
						    (not (= (1+ lastbeat) beatnum))))
					  (error (format t "get-measures: beats are not increasing, jumping from ~A to ~A at place ~A." lastbeat beatnum place)))
				      (if (and lastdenuminator
					       (not (= beatnum 0))
					       (not (= lastdenuminator denuminator)))
					  (error (format t "get-measures: Different denuminators, ~A and ~A, at place ~A, beatnum ~A." lastdenuminator denuminator place beatnum)))
				      (setf lastbeat beatnum)
				      (setf lastdenuminator denuminator)
				      (cons (car mlist)
					    (remove-end (cdr mlist))))))))
	      (setf firstpass (remove-end firstpass)))

      ;; Adding number of beats in measure, which can not be done in firstpass.
      (let ((numbeats-next (make-queue (nreverse (cons (1+ lastbeat) numbeats))))
	    (beatnum-use 0))
	(mapcar #'(lambda (measure)
		    ;;(c-display "hmm" measure)
		    (let ((beatnum (nth 2 measure)))
		      (if (= 0 beatnum)
			  (setf beatnum-use (funcall numbeats-next)))
		      (cons (car measure)
			    (cons beatnum-use
				  (cdr measure)))))
		firstpass)))))



#||
(get-measures (get-block 0))
||#


(defun get-measure-starts (measures)
  (mapcar #'car (filter #'(lambda (x) (= 0 (fourth x))) (if (consp measures)
							    measures
							  (get-measures (get-block measures))))))

#||
(get-measure-starts 0)
||#


(defun get-measure-changes (measures)
  (let ((numi -1)
	(denumi -1)
	(ret '()))
    (dolist (measure (if (consp measures)
			 measures
		       (get-measures (get-block measures))))
      (when (or (not (= (second measure) numi))
		(not (= (third measure) denumi)))
	(setf numi (second measure))
	(setf denumi (third measure))
	(push measure ret)))
    (nreverse ret)))

(defun get-measure-change-starts (measures)
  (mapcar #'car (get-measure-changes measures)))
			
#||
(get-measure-change-starts 0)
(get-measure-changes (get-measures (get-block 0)))
||#


(defun sublist (list start end)
  (butlast (nthcdr start list) (- (length list) end)))

#||
(sublist '(a b c d e f) 0 1)
(let ((val 12))
  (labels ((find-factor (val)
			(multiple-value-bind (a b) (floor val)
					     (if (not (= 0 b))
						 (/ val b)
					       (find-factor (/ val 2))))))
	  (find-factor 9)))

(floor (/ 3 2))
(floor (/ 10 2))

||#



(defun get-time-between (place1 place2 timings)
  (multiple-value-bind (line1 fraction1) (floor place1)
		       (multiple-value-bind (line2 fraction2) (floor place2)
					    ;;(c-display "a" (list line1 fraction1) (list line2 fraction2) (sublist timings line1 line2) "b")
					    (if (= line1 line2)
						(* (nth line1 timings)
						   (- place2 place1))
					      (let ((ret 0))
						(if (not (= 0 fraction1))
						    (progn
						      (setf ret (* (nth line1 timings)
								   (- 1 fraction1)))
						      (setf line1 (1+ line1))))
						(if (not (= 0 fraction2))
						    (setf ret (+ ret (* (nth line2 timings)
									fraction2))))
						;;(c-display ret (list line1 fraction1) (list line2 fraction2) (sublist timings line1 line2))
						(+ ret (apply #'+ (sublist timings line1 line2))))))))

#||

(progn (dodasread)
       (list (get-time-between 2 4 (get-timings (get-block 19)))
	     (get-time-between 4 6 (get-timings (get-block 19)))))


(get-time-between 43 45 (get-timings (get-block 0)))
(get-time-between 89 93 (get-timings (get-block 1)))
(get-time-between 93 379/4 (get-timings (get-block 1)))
(get-time-between 379/4 98 (get-timings (get-block 1)))


(nth 94 (get-timings (get-block 1)))
||#



;; Returns a list of quarters between measure places.
;; ((a b c)(a b c)) -> a=place1,b=place2,c=number of quarters
(defun get-quarters (block &optional (measuretracknum 0))
  (let* ((das-timings (get-timings block))
	 (timings das-timings)
	 (measures (get-measures block measuretracknum))
	 (ret '()))
    (labels ((aloop (measures)
		    (let* ((first-measure (car measures))
			   (next-measure (cadr measures))
			   (first-time (car first-measure))
			   (next-time (if (not next-measure)
					  (get-block-end block measuretracknum)
					(car next-measure)))
			   (denominator (nth 2 first-measure)))
		      (labels ((gq (first second)
				   (* 4
				      (/ (get-time-between first second timings)
					 (get-time-between first-time next-time timings)
					 denominator))))
			      (multiple-value-bind (line1 fraction1) (floor first-time)
						   (multiple-value-bind (line2 fraction2) (floor next-time)
									(if (= line1 line2)
									    (push (list first-time
											next-time
											(gq first-time next-time))
										  ret)
									  (progn
									    (if (not (= 0 fraction1))
										(progn
										  (push (list first-time
											      (1+ line1)
											      (gq first-time (1+ line1)))
											ret)
										  (setf line1 (1+ line1))))
									    (loop for line from line1 to (1- line2)
										  do (push (list line
												 (1+ line)
												 (gq line (1+ line)))
											   ret))
									    (if (not (= 0 fraction2))
										(push (list line2
											    next-time
											    (gq line2 next-time))
										      ret))))
									(if next-measure
									    (aloop (cdr measures)))))))))
	    (aloop measures))
    (nreverse ret)))

#||
(get-quarters (get-block 0))
||#


;; Returns number of quarter notes between place1 and place2
(defun get-quarters-between (block place1 place2 &key quarters (measuretracknum 0))
  (let* ((quarters (if quarters
		       quarters
		     (get-quarters block measuretracknum)))
	 (ret 0))
    (dolist (quarter quarters)
      (let* ((first-time (car quarter))
	     (next-time (cadr quarter))
	     (num-quarters (caddr quarter)))
	;;(c-display "a" first-time next-time)
	(if (or (and (>= place1 first-time) ;; start is inside
		     (<= place1 next-time))
		(and (>= place2 first-time) ;; end is inside
		     (<= place2 next-time))
		(and (<= place1 first-time) ;; start is before and end is after
		     (>= place2 next-time)))
	    (progn
					;		      (print (format NIL "got it ~A ~A ~A ~A" first-time next-time quarter (/ (- (min place2 next-time)
					;											       (max place1 first-time))
					;											    (- next-time first-time))))
	      (setf ret (+ ret (* (/ (- (min place2 next-time)
					(max place1 first-time))
				     (- next-time first-time))
				  num-quarters)))))))
    ret))


#||
(get-quarters-between (get-block 1) 0 6)

(get-quarters-between (get-block 1) 48 193/4)) ;; 195/4

(get-quarters-between (get-block 1) 48 51)) ;; 195/4

(get-quarters-between (get-block 1) 49 50)) ;; 195/4

(get-quarters-between (get-block 1) 0 3))
(progn
  (dodasread)
  (get-quarters-between (get-block 19) 0 1))
||#




;; Return a list of tempo-changes. '((a b)(a b)...) : a=place, b=tempo (in number of quarter notes per minute)
;;   bpm=(time between two measures)/denuminator*60*reltempo
(defun get-tempos (block &optional
			 (timings (get-timings block))
			 (measures (get-measures block)))
  (let ((ret '())
	(lasttempo -1)
	(reltempo (get-rad-value block "?reltempo")))
    (labels ((aloop (measures)
		    (let* ((first-measure (car measures))
			   (next-measure (cadr measures))
			   (first-time (car first-measure))
			   (next-time (if (not next-measure)
					  (get-block-end block)
					(car next-measure)))
			   (timebetween (get-time-between first-time next-time timings))
			   (denominator (nth 2 first-measure))
			   (tempo (/ (* 4 60 reltempo)
				     (* timebetween denominator))))
		      (if (not (= lasttempo tempo))
			  (push (list first-time tempo) ret))
		      (setf lasttempo tempo)
		      (if next-measure
			  (aloop (cdr measures))))))
	    (aloop measures))
    (nreverse ret)))


#||

(let ((block (get-block 0)))
  (get-tempos block))

(let* ((block (get-block 0))
      (reltempo (get-rad-value block "?reltempo"))
      (timings (get-timings block))
      (measures (get-measures block)))
  (list reltempo
	timings
	measures))

(* (* (/ (* 3 2/3) 4) 60) 2.1)

(get-octave 6)
(floor 8 12)

||#




(defun get-linenums (block)
  (mapcar #'(lambda (measure)
	      (list measure (format nil "~D" (+ 0.0 measure))))
	  (get-measure-starts (get-measures block))))

#||
(get-linenums (get-block 0))
||#






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Building up cmn scores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct ndata
  start
  end
  notes
  volume
  (cmn nil))

(defun ndata-copy (ndata &key
			 (start (ndata-start ndata))
			 (end (ndata-end ndata))
			 (notes (copy-list (ndata-notes ndata)))
			 (volume (ndata-volume ndata))
			 (cmn (copy-list (ndata-cmn ndata))))
  (make-ndata :start start
	      :end end
	      :notes notes
	      :volume volume
	      :cmn cmn))

(defun make-ndata2 (start end notes &optional (volume 0) (cmn nil))
  (make-ndata :start start :end end :notes notes :volume volume :cmn cmn))

(defun ndata-append-cmn (ndata cmn)
  (setf (ndata-cmn ndata) (append (ndata-cmn ndata) (list cmn)))
  ndata)

(defun ndata-push-cmn (cmn ndata)
  (push cmn (ndata-cmn ndata))
  ndata)

(defun cmn-chordp (cmn)
  (eq (car cmn) 'CHORD))

(defun cmn-get-notes (cmn)
  (if (cmn-chordp cmn)
      (cdr (second cmn))
    (list (car cmn))))

(defun cmn-set-notes (cmn notes)
  (setf notes (if (consp notes) notes (list notes)))
  (if (cmn-chordp cmn)
      (setf (second cmn) `(notes ,@notes))
    (setf (first cmn) (car notes)))
  cmn)

(defun cmn-append (cmn something)
  (append cmn (list something)))


;; Get-notelist (block tracknums)
;; tracknums is a list of tracks which contains scores that are either single notes or create chords.
;; Returns '((a b c d)(a b c d)) where a=startplace, b=endplace, c=list of scores (nil=rest), d=volume.
(defun get-notelist (block &rest tracknums)
  (let* ((tracks (mapcar #'(lambda (tracknum) (get-track block tracknum)) tracknums))
	 (notes (mapcar #'get-notes tracks))
	 (allnotes (apply #'append notes))
	 (prevplace (get-block-start block))
	 (blocklen (get-block-end block))
	 (ret '()))
    (labels ((push-ret (start end notes vol)
		       (push (make-ndata :start start :end end :notes notes :volume (/ vol 128.0))
			     ret))
	     (add-note-in-chord (start end noteval)
				(let ((ndata (car (member-if #'(lambda (ndata)
								 (and (= start (ndata-start ndata))
								      (= end (ndata-end ndata))))
							     ret))))
				  (if ndata
				      (progn
					(push noteval (ndata-notes ndata))
					t)
				    nil)))
	     (check-note-legality (start end noteval)
				  (dolist (ndata ret)
				    (let ((start2 (ndata-start ndata))
					  (end2 (ndata-end ndata)))
				      (if (or (and (> end start2)
						   (<= end end2))
					      (and (>= start start2)
						   (< start end2))
					      (and (<= start start2)
						   (>= end end2)))
					  (error (format t 
							 "get-notelist: note ~A at line ~A overlaps with other notes without creating chords. ~A" 
							 noteval start (list start end start2 end2))))))))
	    ;; Add notes
	    (dolist (note allnotes)
	      (let ((start (+ (second note) (/ (third note) (fourth note))))
		    (end (+ (nth 7 note) (/ (ninth note) (tenth note))))
		    (noteval (+ g-transpose (fifth note)))
		    (volume (seventh note)))
		(when (and (< start blocklen)
			   (not (add-note-in-chord start end noteval)))
		  (check-note-legality start end noteval)
		  (push-ret start end (list noteval) volume))))

	    ;; Add rests.
	    (dolist (ndata (sort (copy-list ret) #'(lambda (a b) (< (ndata-start a) (ndata-end b)))))
	      (if (> (ndata-start ndata) prevplace)
		  (push-ret prevplace (ndata-start ndata) nil 0))
	      (setf prevplace (ndata-end ndata)))
		  
	    
	    ;; Add last rest.
	    (if (< prevplace blocklen)
		(push-ret prevplace blocklen nil 0)))
    
    (sort ret #'(lambda (a b) (< (ndata-start a) (ndata-start b))))))
    


#||
(get-notelist (get-block 4) 1)
||#


;; splitlist is a sorted list of numbers telling where to split any note in the notelist.
;; If the note is split, it is marked by appending a 'begin-tie or 'end-tie.
(defun split-notelist (notelist splitlist)
  (if (or (null notelist)
	  (null splitlist))
      notelist
    (let* ((ndata (car notelist))
	   (start (ndata-start ndata))
	   (end (ndata-end ndata))
	   (splitpoint (car splitlist)))
      (cond ((<= splitpoint start)
	     (split-notelist notelist (cdr splitlist)))
	    ((< splitpoint end)
	     (cons (ndata-append-cmn (ndata-copy ndata :end splitpoint) 'begin-tie)
		   (split-notelist (cons (if (member 'end-tie (ndata-cmn ndata))
					     (ndata-copy ndata :start splitpoint)
					   (ndata-push-cmn 'end-tie (ndata-copy ndata :start splitpoint)))
					 (cdr notelist))
				   splitlist)))
	    (t (cons ndata
		     (split-notelist (cdr notelist) splitlist)))))))

#||
(split-notelist (list (make-ndata :start 0 :end 3 :notes '(66) :volume 5)) '(1/3 1/2))
(split-notelist (get-notelist (get-block 0) 1)
		(get-measure-starts 0))

		(mapcar #'car (filter #'(lambda (x) (= 0 (fourth x))) (get-measures (get-block 1)))))
||#	 



;; tempo changes (mm tempo beat) can not be put independentely, like (measure a b), but must be arguments to for example
;; notes, rests or bars. This functions first splits the notelist (so that mm's can be added to the correct position in the notelist) and
;; then adds them.
(defun add-tempo-change-to-notelist (notelist tempolist)
  (labels ((add (notelist tempolist)
		(let ((ndata (car notelist))
		      (tempo (car tempolist)))
		  ;;(c-display (list "ai" ndata tempo))
		  (cond ((null tempolist) notelist)
			((null notelist)
			 (error "add-temp-change-to-notelist: Ran out of notes. Could be more than one tempo change at same position."))
			((< (car tempo) (ndata-start ndata))
			 (error "add-temp-change-to-notelist: Somethings wrong, probably more than one tempo change at same position."))
			((= (ndata-start ndata) (car tempo))
			 (ndata-append-cmn ndata `(mm ,(cadr tempo)))
			 (add (cdr notelist) (cdr tempolist)))
			(t (add (cdr notelist) tempolist))))))
	  (let* ((tempo-starts (mapcar #'car tempolist))
		 (splitted-list (split-notelist notelist tempo-starts)))
	    (if g-add-tempos
		(add (split-notelist splitted-list tempo-starts) tempolist))
	    splitted-list)))

#|
(add-tempo-change-to-notelist (list (make-ndata2 0 1 '(60)) (make-ndata2 1 2 '(70))) '((1/4 50) (1/2 60)))
-> '((0 1/2 (60) begin-tie) (1/2 1 (60) end-tie (mm 50)))

(add-tempo-change-to-notelist (get-notelist (get-block 0) 1)
			      (get-tempos (get-block 0)))

(get-tempos (get-block 0))
||#


(defun add-linenums-to-notelist (notelist linenumlist)
  (labels ((add (notelist linenumlist)
		(let ((ndata (car notelist))
		      (linenum (car linenumlist)))
		  ;;(c-display (list "ai" ndata linenum))
		  (cond ((null linenumlist) notelist)
			((null notelist)
			 (error "add-temp-change-to-notelist: Ran out of notes. Could be more than one linenum change at same position."))
			((< (car linenum) (ndata-start ndata))
			 (error "add-temp-change-to-notelist: Somethings wrong, probably more than one linenum change at same position."))
			((= (ndata-start ndata) (car linenum))
			 (ndata-append-cmn ndata `(text ,(cadr linenum) (color '(1.0 0.0 0.0)) (dx 0.0) (dy 2.0)))
			 (add (cdr notelist) (cdr linenumlist)))
			(t (add (cdr notelist) linenumlist))))))
	  (let* ((linenum-starts (mapcar #'car linenumlist))
		 (splitted-list (split-notelist notelist linenum-starts)))
	    (if g-add-linenums
		(add (split-notelist splitted-list linenum-starts) linenumlist))
	    splitted-list)))

#||
(add-linenums-to-notelist (get-notelist (get-block 0) 1) (get-linenums (get-block 0)))
(get-notelist (get-block 0) 1)
(get-linenums (get-block 0))
||#


(defun get-track-volume (block tracknum)
  (let ((track (get-track block tracknum)))
    (/ (get-rad-value track "?volume")
       1000.0)))

#||
(get-track-volume (get-block 1) 2)
||#

;; Volume is a number between 0 and 1. If its less than 0, it returns pppp, if its more than 1, it returns ffff.
(defun volume->dynamic-symbol (das-volume)
  (let* ((dynamics '(pppp ppp pp p mp mf f ff fff ffff))
	 (volume (max 0 (min 1 das-volume)))
	 (s (nth (round (c-scale volume 0 1 0 (1- (length dynamics))))
		 dynamics)))
    (if (= 0 g-dynamics-dy)
	s
      `(,s (dy ,g-dynamics-dy)))))

#||
(volume->dynamic-symbol 0.99)
(round 1.0)
||#
  


;; Add dynamics based on volume value. All splitting should be done first
;; to avoid extra dynamic symbols for splitted notelists.
(defun convert-volumes-in-notelist (block tracknum notelist &optional (scalemin 0.3) (scalemax 0.8))
  (let ((lastdyn 'gakk)
	(trackvolume (get-track-volume block tracknum)))
    (dolist (ndata notelist)
      (let* ((volume (c-scale (* trackvolume (ndata-volume ndata)) 0 1 scalemin scalemax))
	     (notes (ndata-notes ndata))
	     (dynamic (volume->dynamic-symbol volume)))
	(if (and g-add-dynamics
		 (not (null notes))
		 (not (equal dynamic lastdyn)))
	    (progn
	      (setf lastdyn dynamic)
	      (ndata-push-cmn dynamic ndata))))))
  notelist)

#||
(convert-volumes-in-notelist (get-block 1) 1 (get-notelist (get-block 1) 1))
||#


(defun notenum->cmnnote (midinote)
  (if (< midinote 12)
      (error (format t "notenum->cmnnote: note too low: ~A" midinote)))
  (let ((cmnnotes '(c0 cs0 d0 ds0 e0 f0 fs0 g0 gs0 a0 as0 b0 c1 cs1 d1 ds1 e1 f1 fs1 g1 gs1 a1 as1 b1 c2
		       cs2 d2 ds2 e2 f2 fs2 g2 gs2 a2 as2 b2 c3 cs3 d3 ds3 e3 f3 fs3 g3 gs3 a3 as3 b3 c4
		       cs4 d4 ds4 e4 f4 fs4 g4 gs4 a4 as4 b4 c5 cs5 d5 ds5 e5 f5 fs5 g5 gs5 a5 as5 b5 c6
		       cs6 d6 ds6 e6 f6 fs6 g6 gs6 a6 as6 b6 c7 cs7 d7 ds7 e7 f7 fs7 g7 gs7 a7 as7 b7 c8
		       cs8 d8 ds8 e8 f8 fs8 g8 gs8 a8 as8 b8)))
    (if (>= (- midinote 12) (length cmnnotes))
	(error (format t "notenum->cmnnote: note too high: ~A" midinote)))
    (nth (- midinote 12) cmnnotes)))
(defun notenum->cmnnote-flats (midinote)
  (if (< midinote 12)
      (error (format t "notenum->cmnnote: note too low: ~A" midinote)))
  (let ((cmnnotes '(c0 ds0 d0 ef0 e0 f0 gf0 g0 af0 a0 bf0 b0 c1 df1 d1 ef1 e1 f1 gf1 g1 af1 a1 bf1 b1 c2
		       df2 d2 ef2 e2 f2 gf2 g2 af2 a2 bf2 b2 c3 df3 d3 ef3 e3 f3 gf3 g3 af3 a3 bf3 b3 c4
		       df4 d4 ef4 e4 f4 gf4 g4 af4 a4 bf4 b4 c5 df5 d5 ef5 e5 f5 gf5 g5 af5 a5 bf5 b5 c6
		       df6 d6 ef6 e6 f6 gf6 g6 af6 a6 bf6 b6 c7 df7 d7 ef7 e7 f7 gf7 g7 af7 a7 bf7 b7 c8
		       df8 d8 ef8 e8 f8 gf8 g8 af8 a8 bf8 b8)))
    (if (>= (- midinote 12) (length cmnnotes))
	(error (format t "notenum->cmnnote: note too high: ~A" midinote)))
    (nth (- midinote 12) cmnnotes)))


;; Put cmn note symbols created from the radium note numbers into the cmn slots. (60 -> c4 and so on)
(defun convert-notes-in-notelist (block notelist)
  (let ((quarters (get-quarters block 0)))
    (dolist (ndata notelist)
      (let ((data (ndata-notes ndata))
	    (quarter-length (get-quarters-between block (ndata-start ndata) (ndata-end ndata) :quarters quarters)))
	(cond ((eq nil data) ;; rest
	       (setf (ndata-cmn ndata)
		     `(rest (rq ,quarter-length) ,@(if g-invisible-rests '(invisible) nil) ,@(remove 'end-tie (remove 'begin-tie (ndata-cmn ndata))))))
	      ((consp data)
	       (ndata-push-cmn `(rq ,quarter-length) ndata)
	       ;;(c-display "ndata" ndata (length data))
	       (if (= 1 (length data))   ;; single note
		   (ndata-push-cmn (notenum->cmnnote (car data)) ndata)
		 (let ((notes (sort data #'<)))   ;;chord
		   (labels ((makecmn (notes)
				     (let ((a (first notes))
					   (b (second notes)))
				       (cond ((null notes) nil)
					     ((null b)
					      (list (notenum->cmnnote a)))
					     ((= a b)
					      (error (format t "Equal notes in chord ~A" ndata)))
					     ((> (abs (- a b)) 1)
					      (cons (notenum->cmnnote a)
						    (makecmn (cdr notes))))
					     (t (let ((nchroma-a (get-notechroma a))
						      (nchroma-b (get-notechroma b)))
						  (if (eq nchroma-a nchroma-b)
						      (cons (notenum->cmnnote a)
							    (cons (notenum->cmnnote-flats b)
								  (makecmn (cddr notes))))
						    (cons (notenum->cmnnote a)
							  (cons (notenum->cmnnote b)
								(makecmn (cddr notes)))))))))))
			   (ndata-push-cmn `(notes ,@(makecmn notes)) ndata))
		   (ndata-push-cmn `chord ndata))))))))
  notelist)

#||
(convert-notes-in-notelist (get-block 1) (get-notelist (get-block 1) 2 3 4))
||#







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Interface.
;;;;;;;;  * dodasread reads a new radium file or rereads an old one.
;;;;;;;;  * get-scores creates cmn scores.
;;;;;;;;  * get-scores2 returns cmn scores as an s-expression.
;;;;;;;;    (get-scores2 is useful for debugging)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-scores2 (blocknum &rest tracknums)
  (let* ((block (get-block blocknum))
	 (notes (convert-volumes-in-notelist block 
					     (car tracknums)
					     (add-linenums-to-notelist
					      (add-tempo-change-to-notelist (split-notelist (apply #'get-notelist (cons block tracknums))
											    (if g-add-linenums
												(get-measure-starts blocknum)
											      (get-measure-change-starts blocknum)))
									    (get-tempos block))
					      (get-linenums block))
					     ))
	 (measure-changes (mapcar #'(lambda (measure)
				      (make-ndata :start (car measure) :end (- (car measure)) :notes 'special :volume 0 :cmn  `(meter ,(second measure) ,(third measure))))
				  (get-measure-changes (get-measures block))))
	 (num -1)
	 (ret '())
	 (firstpass (filtermap
		     #'(lambda (ndata)
			 (setf num (1+ num))
			 (if g-filter-func 
			     (funcall g-filter-func ndata (ndata-cmn ndata) num)
			   (ndata-cmn ndata)))
		     (convert-notes-in-notelist block (sort (append notes (if g-add-measures measure-changes nil))
							    #'(lambda (a b)
								(or (< (ndata-start a) (ndata-start b))
								    (and (= (ndata-start a) (ndata-start b))
									 (> (ndata-end b) 0))))))))) ;; 'specials have negative end value -> sorted before non-specials.
    (mapc #'(lambda (cmn)
	      (if (and (consp cmn)
		       (eq 'mul (car cmn)))
		  (mapc #'(lambda (cmn)
			    (push cmn ret))
			(cdr cmn))
		(push cmn ret)))
	  firstpass)
    (nreverse ret)))

#||
(get-scores2 1 2 3 4)
(get-scores2 1 6)
||#

(defun get-scores (blocknum tracknums
			    &key
			    (invisible-rests nil)
			    (add-tempos t)
			    (add-measures t)
			    (add-dynamics t)
			    (transpose 0)
			    (dynamics-dy 0)
			    (filter-func nil)
			    (add-linenums nil))
  (setf g-transpose transpose)
  (setf g-invisible-rests invisible-rests)
  (setf g-add-tempos add-tempos)
  (setf g-add-measures add-measures)
  (setf g-add-dynamics add-dynamics)
  (setf g-dynamics-dy dynamics-dy)
  (setf g-filter-func filter-func)
  (setf g-add-linenums add-linenums)
  (eval `(list ,@(apply #'get-scores2 (cons blocknum (if (consp tracknums)
							 tracknums
						       (list tracknums)))))))



(defun dodasread (&optional (das-filename filename))
  (if (not das-filename)
      (error "please supply filename for dodasread")
    (progn
      (setf filename das-filename)
      (setf streem (open filename));; :direction :input :external-format :iso-8859-1)) ;; iso-8859-1 is needed for sbcl, I think. (I use cmucl)
      (setf song (readsong streem))
      (setf root (get-rad-value song '(ROOT)))
      (setf songdata (get-rad-value root '(SONG)))
      (close streem)
      NIL)))



#||
(compile-file "rad2cmn.lisp")
(load "rad2cmn")


(dodasread "example.rad")

(defvar sax-transpose 9)

(cmn (text "This is Block 0. Track 0 is for measures, track 1 and 2 are for the piano, and track 3 is for the bass." (font-size 12)) (title-separation 2)
     (size 21)
     (automatic-naturals t) ;; Must be used!
     (automatic-measure-numbers t)
     (system bracket
	     (apply #'staff (append (list (staff-name "Piano") bar treble)
				    (get-scores 0 '(1 2))
				    (list double-bar)
				    )))
     (system bracket
	     (apply #'staff (append (list (staff-name "Bass") bar bass)
				    (get-scores 0 3 :add-tempos nil)
				    (list double-bar)
				    )))
     )

     

(cmn (title "TO (block 1 and 2)") (title-separation 2)
     (implicit-accidental-style :paranoid)  (page-width 8.26771654) (page-height 11.6929134) (footer-margin 0.5) (header-margin 0.7)
     (all-output-in-one-file t) (line-separation 3) (staff-separation 1.99) (system-separation 2) (size 21) (accidental-to-note-head-space 0.0625)
     (automatic-naturals t) (free-expansion-factor 1.15)
     (automatic-measure-numbers t)
     (system bracket
	     (apply #'staff (append (list (staff-name "Sax Eb") bar treble)
				    (get-scores 1 1
						:transpose sax-transpose)
				    (get-scores 2 1 
						:transpose sax-transpose
						:filter-func #'(lambda (ndata cmn num)
								     (if (= 1 num)
									 (list (first cmn) (second cmn) `(mm ,(second (third cmn)) (dy -0.5)))
								       cmn)))
				    (list double-bar)
				    )))

     (system brace
	     (setf s1 (apply #'staff (append (list (staff-name "Piano") bar treble)
					     (get-scores 1 '(2 3 4) 
							 :add-tempos nil
							 :filter-func #'(lambda (ndata cmn num)
									  (cond ((equal '(g6 af6 a6) (cmn-get-notes cmn))
										 (cmn-set-notes cmn '(a6 (f6 (double-sharp)) (g6 (small-sharp (justification -8))))))
										((equal '(f4 CS6) (cmn-get-notes cmn))
										 (cmn-append cmn 'stem-up))
										(t cmn))))
					     (get-scores 2 '(2 3 4)
							 :add-tempos nil
							 :filter-func #'(lambda (ndata cmn num)
									  (cond ((equal '(g6 af6 a6) (cmn-get-notes cmn))
										 (cmn-set-notes cmn '(a6 (f6 double-sharp) (g6 small-sharp))))
										((equal '(f4 CS6) (cmn-get-notes cmn))
										 (cmn-append cmn 'stem-up))
										((equal '(f5 f6) (cmn-get-notes cmn))
										 (cmn-append cmn '(dx 0.3)))
										(t cmn))))
					     (list double-bar)
					     )))

	     (apply #'staff (append (list (tied-to s1))
				    (get-scores 1 5 :invisible-rests t :add-tempos nil :add-measures nil :add-dynamics nil)))
	     
	     (apply #'staff (append (list bar bass)
				    (get-scores 1 6 
						:add-tempos nil
						:filter-func (let ((up nil))
							       #'(lambda (ndata cmn num)
								   (cond ((equal '(c4) (cmn-get-notes cmn))
									  ;;(cmn-append cmn 'stem-down)
									  ;;(cmn-append cmn 'end-octave-up)
									  cmn)
									 (t cmn)))))
				    (get-scores 2 5 :add-tempos nil)
				    (list double-bar)
				    ))))


||#

