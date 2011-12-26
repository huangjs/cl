;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.

;;;  Find out where all non-recursive functions are.

(defun flow-analysis ()
  (setf unknown-frames nil)
  (maphash #'examine-frame frame-table)
  (let (next-frames)
      (while unknown-frames
	(setf next-frames nil)
	(for (:in f unknown-frames)
	     (:do
	        (let ((calls (att frame-table f 'uses)))
		  (while (and calls
			      (att frame-table (car calls) 'non-recursive))
		    (setf calls (cdr calls)))
		  (cond ((null calls)
			 (put-att frame-table f 'non-recursive T)
			 (for (:in c (att frame-table f 'blocks))
			      (:do (push c next-frames))))
			((not (eq (car calls) f))
			 (push-att frame-table (car calls) 'blocks f))))))
	(setf unknown-frames next-frames))))


(defun examine-frame (f v)
  (if (or (null (att frame-table f 'uses))
	  (eq (att frame-table f 'parent) '**main**))
      (put-att frame-table f 'non-recursive T)
      (push f unknown-frames)))
