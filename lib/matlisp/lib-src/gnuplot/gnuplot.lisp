(defvar *current-gnuplot-stream* nil)

(defvar *gnuplot-binary* "gnuplot")

(defclass gnuplot-plot-info ()
  ((title
    :initform "GNU PLOT"
    :accessor gnuplot-title)
   (x-label
    :initform "X"
    :accessor gnuplot-x-label)
   (y-label
    :initform "Y"
    :accessor gnuplot-y-label)
   (x-data
    :accessor gnuplot-x-data)
   (y-data
    :accessor gnuplot-y-data)
   (z-data
    :accessor gnuplot-z-data)))

(defun open-gnuplot-stream ()
  (#-:sbcl
   ext:run-program
   #+:sbcl
   sb-ext:run-program
   *gnuplot-binary* nil :input :stream :wait nil :output t))

(defun gnuplot-plot (info &key (stream (#-:sbcl
                                        ext:process-input
                                        #+:sbcl
                                        sb-ext:process-input
                                        *current-gnuplot-stream*)))
  (with-accessors ((title gnuplot-title)
		   (x-label gnuplot-x-label)
		   (y-label gnuplot-y-label)
		   (x-data gnuplot-x-data)
		   (y-data gnuplot-y-data)
		   (z-data gnuplot-z-data))
      info
    (format stream "~&set title '~S'~%" title)
    (format stream "~&set xlabel '~S'~%" x-label)
    (format stream "~&set ylabel '~S'~%" y-label)
    (finish-output stream)
    (map nil #'(lambda (x y z)
		 (with-open-file (s "/tmp/gnuplot.out" :direction :output
				    :if-exists :overwrite)
		   (map nil #'(lambda (xs ys zs)
				(if zs
				    (format s "~A ~A ~A~%" xs ys zs)
				    (format s "~A ~A~%" xs ys)))
			x y z)
		   (format stream "~A '/tmp/gnuplot.out'~%"
			   (if z "splot" "plot"))
		   (finish-output stream)
		   (sleep 5)))
	 x-data y-data z-data)
    ))
  
    
