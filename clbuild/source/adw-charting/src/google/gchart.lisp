;; Copyright (c) 2008 Accelerated Data Works, Ryan Davis

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :adw-charting)

(defun make-parameter-collection ()
  (make-hash-table :test 'equal))

(defclass gchart (chart)
  ((chart-type :accessor chart-type
	       :initarg :chart-type)
   (parameters :accessor parameters
	       :initform (make-parameter-collection)
	       :initarg :parameters)
   (x-axis :accessor x-axis
	   :initarg :x-axis
	   :initform nil
	   :documentation "an axis object to determine formatting for
the X axis")
   (y-axis :accessor y-axis
	   :initarg :y-axis
	   :initform nil
	   :documentation "and axis object to determine formatting for
the Y axis")
   (axes :accessor axes
	 :initform (make-hash-table))))

(defvar *chart-types* '((:pie . "p")
			(:pie-3d . "p3")
			(:line . "lxy")
			(:v-bar .  "bvs")
			(:h-bar . "bhs")
			(:v-gbar .  "bvg")
			(:h-gbar . "bhg")
			(:google-meter . "gom")))

(defparameter +google-chart-url+ "http://chart.apis.google.com/chart")

(defun make-color (html-color)
  "takes an html color and returns the closest (r g b) list equivalent"
  (let ((*read-base* 16))
    (loop
       for start in '(0 2 4)
       collect (interpolate 0 255.0 
		(read-from-string (subseq html-color start (+ 2 start)))
		:interpolated-max 1.0))))

(defun make-html-color (color)
  "takes a standard (r g b) color list and returns the closest HTML equivalent"
  (format nil "~{~2,'0X~}"
	  (mapcar #'(lambda (c)
		      (ceiling (interpolate 0 1.0 c :interpolated-max 255)))
		  color)))

(defmethod build-data ((chart gchart))
  "helper to build the list of data"
  (case (chart-type chart)
    (:google-meter (format nil
			   "t:~D"
			   (value (first (chart-elements chart)))))
    ((:pie :pie-3d) (format nil
			    "t:~{~F~^,~}"
			    (normalize-elements chart)))
    (:line
       ;;pairs of X | Y, normalized to 0-100 for google's chart algorithms
       (format nil "t:~{~a~^|~}"
	       (loop for (exes wyes series) in (normalized-series chart)
		     collect (format nil
				     "~{~,2F~^,~}|~{~,2F~^,~}"
				     exes wyes))))
    ((:v-bar :h-bar :v-gbar :h-gbar)
       ;;these want the bars specified as wyes1|wyes2|wyesN, so
       ;;get all the lists of wyes sorted out with 0s for the missing values
       (format nil "t:~{~a~^|~}"
	       (let ((xys (normalized-series chart))
		     (all-exes nil))
		 ;;assemble list of all exes
		 (dolist (xy xys)
		   (dolist (x (first xy))
		     (unless (member x all-exes)
		       (push x all-exes))))
		 (setf all-exes (sort all-exes #'<))
		 (loop for (exes wyes series) in xys
		       for idx from 0
		       do
		    (when (eql (mode series) :line)
		      (append-parameter :chm
					(format nil "D,~a,~D,0,2,1"
						(make-html-color (color series))
						idx)

					chart))
			 
		       collect
		    (format nil "~{~D~^,~}"
				(mapcar #'(lambda (x)					    
					    (or (when-let (idx (position x exes))
						  (truncate (nth idx wyes)))
						0))
					all-exes))))))))

(defun interpolate (min max val &key (interpolated-max 100) (interpolated-min 0))
  (+ interpolated-min
     (* (- interpolated-max interpolated-min) (/ (- val min)
			    (- max min)))))

(defun normalize-elements (chart)
  (let ((sum (reduce #'+
		     (chart-elements chart)
		     :key #'value)))
    (loop for elem in (chart-elements chart)
	  collect (/ (value elem) sum))))

(defun normalized-series (chart)
  (destructuring-bind ((min-x min-y) (max-x max-y))
      (find-chart-extremes chart)
    (loop for series in (chart-elements chart)
       for exes = nil then nil
       for wyes = nil then nil
       do
	 (loop for (x y) in (reverse (data series))
	    do
	      (push (interpolate min-x max-x x) exes)
	      (push (interpolate min-y max-y y) wyes))
       collect (list exes wyes series))))

(defmethod build-labels ((chart gchart))
  "helper to build the list of labels"
  (format nil "~{~a~^|~}"
	  (mapcar #'label
		  (chart-elements chart))))

(defun prepare-key (key)
  (string-downcase (princ-to-string key)))

(defmethod set-parameter ((chart gchart) key value)
  (setf (gethash key
		 (parameters chart))
	value))

(defmacro set-parameters ((chart) &body params)
  `(progn
    ,@(loop for (k v) in params
     collect 
       `(set-parameter ,chart ,k ,v))))

(defmethod ensure-default-parameters ((chart gchart))
  (set-parameters (chart)
    (:chs (format nil "~ax~a"
		  (width chart)
		  (height chart)))
    (:cht (cdr (assoc (chart-type chart)
		      *chart-types*)))
    (:chd (build-data chart))
    (:chco (format nil "~{~a~^,~}"
		   (mapcar #'make-html-color
			   (mapcar #'color (chart-elements chart)))))))

(defparameter +chart-features+ '(:label :transparent-background :adjusted-zero :data-scaling :label-percentages))

(defgeneric add-feature (feature-name))

(defmethod add-feature ((feature-name (eql :label)))
  (set-parameter *current-chart* (case (chart-type *current-chart*)
				   ((:pie :pie-3d :google-meter) :chl)
				   (T :chdl))
		 (build-labels *current-chart*)))

(defmethod add-feature ((feature-name (eql :transparent-background)))
  (set-parameter *current-chart*
		 :chf
		 "bg,s,00000000"))

(defmethod add-feature ((feature-name (eql :adjusted-zero)))
  (destructuring-bind ((min-x min-y) (max-x max-y))
      (find-chart-extremes *current-chart*)
    (declare (ignore min-x max-x))
    (set-parameter *current-chart*
		 :chp
		 (interpolate min-y max-y 0.0 :interpolated-max 1.0))))

(defmethod add-feature ((feature-name (eql :data-scaling)))
  (let ((totals (make-hash-table))
	(min-y 0))
    (loop for (exes wyes series) in (normalized-series *current-chart*)
	  do
	  (loop for x in exes
		for y in wyes
		do		
		(if (plusp y)
		    (incf (gethash x totals 0) y)
		    (if (< y min-y)
			(setf min-y y)))))
    (set-parameter *current-chart*
		   :chds
		   (format nil "~,2F,~,2F" min-y
			   (loop for k being the hash-keys in totals
				 using (hash-value v)
				 maximizing v into max
				 finally (return max))))))

(defmethod add-feature ((feature-name (eql :label-percentages)))
  (loop for elem in (chart-elements *current-chart*)
	for normalized in (normalize-elements *current-chart*)
	do
	(setf (label elem)
	      (format nil "~a - ~,2F%" (label elem) (* 100 normalized))))
  (add-feature :label))


(defmethod add-title (title)
  "adds the given title, ignores if the title is nil"
  (if title
      (set-parameter *current-chart*
		   :chtt
		   title)
      (warn "trying to set nil title")))

(defun bar-spacing (bar-width-px &optional bar-seperation-px group-seperation-px)
  (set-parameter *current-chart*
		 :chbh
		 (format nil "~{~D~^,~}"
			 (loop for x in (list bar-width-px bar-seperation-px group-seperation-px)
			      when x
			      collect x))))

(defun grid (x-step y-step line-length blank-length)
  (set-parameter *current-chart* :chg (list x-step y-step line-length blank-length)))

(defun append-parameter (key val &optional (chart *current-chart*))
  "adds an axis, and returns the index of that axis"
  (setf (gethash key (parameters chart))
	(append (gethash key (parameters chart))
		(list val)))
  (position val (gethash key (parameters chart))))


(defun add-axis (val valfn axis &optional (chart *current-chart*))
  "adds an axis, and returns the index of that axis"
    (let ((idx (append-parameter :chxt val chart))
	  (param (if (eql :auto (data-interval axis))
		     :chxr :chxl)))
      (setf (gethash idx (axes chart)) axis)
      (append-parameter param (list idx valfn (label-formatter axis) (draw-zero-p axis)))))

(defmethod (setf x-axis) :before (ax (chart gchart))
  (add-axis "x" #'x ax chart))

(defmethod (setf y-axis) :before (ax (chart gchart))
  (add-axis "y" #'y ax chart))

(defun add-features (&rest names)
  (mapc #'add-feature names))

(defmethod finalize-parameter (key val)
  (princ-to-string val))

(defmethod finalize-parameter (key (val float))
  (format nil "~,2F" val))

(defmethod finalize-parameter (key (val string))
  val)

(defmethod finalize-parameter (key (val list))
 (format nil "~{~a~^,~}" val))

(defun inline-break (format-string &rest args)
  "call BREAK with the given format and args, then return the args"
  (apply #'break format-string args)
  (apply #'values args))

(defmethod finalize-parameter ((key (eql :chxl)) val)
  (format nil "~{~a~^|~}"
	  (loop for (idx valfn formatfn draw-zero-p) in val
		collect (format nil "~D:|~{~a~^|~}" idx
				(mapcar formatfn
					(sort
					 (remove-duplicates
					  (let ((vals (loop for elem in (chart-elements *current-chart*)
							    nconc (mapcar valfn (data elem)))))
					    ;;if we want to draw 0, add it to the list
					    (when draw-zero-p (push 0 vals))
					    vals))
					 #'<))))))

(defmethod finalize-parameter ((key (eql :chxr)) val)
  (let ((all-data (mapcan #'data (chart-elements *current-chart*))))
    (format nil "~{~a~^|~}"
	    (loop for (idx valfn formatfn draw-zero-p) in val		 
	       collect (format nil "~D,~{~,2F~^,~}" idx			       

			       (let ((vals (mapcar valfn all-data)))
				 ;;add zero if we want to
				 (when draw-zero-p (push 0 vals))
				 ;;find the global min/max				 
				 (let ((minmax (loop for x in vals
						     minimizing x into min
						     maximizing x into max
						     finally (return (list min max)))))
				   
				   ;;find the function for scaling this axis, scale
				   (if-let (scalefn (scalefn (gethash idx (axes *current-chart*))))
				     (mapcar scalefn minmax)
				     minmax))))))))


(defmethod build-parameters ((chart gchart))  
  "returns an alist that defines to google what
it should be rendering"
  (build-parameters (parameters chart)))

(defmethod build-parameters ((params hash-table))  
  "returns an alist that defines to google what
it should be rendering"
  (loop for k being the hash-keys in params using (hash-value v)
	collect (cons (prepare-key k) (finalize-parameter k v))))

(defmethod save-chart-to-stream (stream (chart gchart))
  (ensure-default-parameters chart)
  (write-sequence (drakma:http-request
		   +google-chart-url+
		   :parameters (build-parameters chart))
		  stream))

(defmethod save-chart-to-file (filename (chart gchart))
  "makes the call to google, saves the result in the file"
  (with-open-file (dst filename :direction :output
		       :element-type 'unsigned-byte
		       :if-does-not-exist :create
		       :if-exists :supersede)
    (save-chart-to-stream dst chart)
    (truename filename)))

(defun chart-url ()
  "returns the URL for the current google chart"
  (build-chart-url *current-chart*))

(defgeneric build-chart-url (thing)
  (:method ((chart gchart))
    (ensure-default-parameters chart)
    (build-chart-url (parameters chart)))
  (:method ((params hash-table))
    (concatenate 'string
		 +google-chart-url+
		 "?"
		 (drakma::alist-to-url-encoded-string
		  (build-parameters params)
		  drakma:*drakma-default-external-format*))))

(defmacro with-gchart ((type width height) &body body)
  "creates a new context with a gchart of the given type, width, and height."
  `(let ((*current-chart*
	  (make-instance 'gchart
			 :chart-type ,type
			 :width ,width
			 :height ,height)))
     (with-color-stack ()
       ,@body)))


(defun google-o-meter (percentage width &key label colors show-percentage)
  (let ((params (make-parameter-collection))
	;;if the percentage is specifed as 0-1, multiply by 100
	(percentage (if (> 1 percentage)
			(* 100 percentage)
			percentage)))
    
    (flet ((add-param (k v)
	     (setf (gethash k params) v)))
      (add-param :chs (format nil "~ax~a" width (truncate (* width .5))))
      (add-param :cht "gom")
      (add-param :chd (format nil "t:~a" (truncate percentage)))
      (when (and colors (< 1 (length colors)))
	(add-param :chco (format nil "~{~a~^,~}" colors)))
      (if label
	  (add-param :chl label)
	  (when show-percentage
	    (add-param :chl (format nil "~D%" (truncate percentage)))))
      
      (build-chart-url params))))