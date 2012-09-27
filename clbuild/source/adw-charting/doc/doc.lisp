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

(require 'cl-who)
(require 'cl-ppcre)
(require 'adw-charting)

(defpackage :net.acceleration.documenter
    (:nicknames #:adw-doc)
  (:use #:cl #:cl-who #:cl-ppcre))

(in-package :adw-doc)

(defvar *root* (merge-pathnames #P"doc/"
				(asdf:component-pathname
				 (asdf:find-system :adw-charting))))

(defvar *tree* )
(defvar *stream* nil)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defhtmlfun (name lambda-list &rest body)
    `(defun ,name ,lambda-list
      (with-html-output (*stream*)
	,@body )))

  (defmacro defhtmlmethod (name lambda-list &rest body)
    `(defmethod ,name ,lambda-list
      (with-html-output (*stream*)
	,@body ))))

(defhtmlfun stub ()
  (:blink "STUB"))

(defhtmlfun overview ()
  (htm
   (:p "ADW-Charting is a library that provides a simple interface to the "
     (vecto-link)
     " vector drawing library. It supports drawing on a canvas and saving the
results to a PNG file. The API was designed to eliminate as many decisions as possible, and simply
produce a reasonable result with minimal fuss.  It tries to scale various elements of the chart to
fit nicely, but sometimes this goes awry." )
   (:p "ADW-Charting depends on the following libraries:"
     (:ul
	 (:li (vecto-link))))

   (:p "ADW-Charting's function interface is similar to "
     (vecto-link)
     "'s interface: you
create charts by setting up a chart context and adding or setting information on that chart.")
   (:p "There are many known limitations at this point. We've got some plans on how to solve
some of these, and other aren't priorities for me, and might stay around for ahwile.")
   (:ul 
       (:li "All colors are RGB, represented as a list of 3 numbers between 0 and 1, eg:"
	 (:code "'(1 .5 .3)"))
     (:li "The bounds on a pie chart are a bit goofy, as the radius of the pie is currently 
only determined by the height of the chart.  This means a square image will cut off the 
legend.")
     (:li "Another issue is with printing axis labels.  There's some code to try to keep 
those reasonably spaces, but sometimes the labels start overlapping.  Making the graph
in two passes should let us calculate everything before starting to draw on the canvas,
preventing this issue.")
     (:li "The font used for all the text is included in the distribution, some random .ttf file
pulled from the debian freefont library.  You can specify the font file using the 
*default-font-file* unexported variable.  I'm using a with-font macro internally that
could solve this one."))
   (:p "Related libraries"
     (:ul
	 (:li (:a :href "http://common-lisp.net/project/cl-plplot/" "cl-plplot"))))))

(defhtmlfun examples ()
  (htm
   (:p "All examples are available in "
     (:tt "test/examples.lisp")
     " in the distribution.")))

(defhtmlfun feedback ()
  (htm
   (:p "If you have any questions, comments, bug reports, or other feedback
regarding ADW-Charting, please email "
     (:a :href "mailto:ryan@acceleration.net" "Ryan Davis"))))

(defhtmlfun vecto-link ()
  (:a :href "http://www.xach.com/lisp/vecto/" "Vecto"))

(defhtmlfun acknowledgements ()
  (htm
   (:p "Thanks to:")
   (:ul (:li "Zach Beane for creating "
	  (vecto-link))
     (:li "Peter Seibel for his excellent book, "
       (:a :href "http://gigamonkeys.com/book/" "Practical Common Lisp"))
     (:li "Edi Weitz and Zach Beane for providing good examples on how to write and document lisp libraries")
     (:li "Co-workers Nathan, Russ, and Rebecca for advice and code reviews"))))

(defhtmlfun dictionary ()
  (:p "The following symbols are exported from the ADW-CHARTING package."))

(defclass section ()
  ((title :accessor title :initarg :title)
   (anchor :accessor anchor :initform (princ-to-string (gensym)))
   (children :accessor children :initarg :children :initform nil)
   (content-fn :accessor content-fn :initarg :content-fn :initform #'stub)))

(defclass code (section)
  ((code-type :accessor code-type :initarg :type)
   (args :accessor args :initarg :args)
   (return-val :accessor return-val :initarg :return-val)))

(defun make-section (title content-fn &rest children)
  (make-instance 'section :title title :children children :content-fn content-fn))

(defun make-code (title content-fn code-type args &optional (return-val nil))
  (make-instance 'code
		 :title title
		 :content-fn content-fn
		 :type code-type
		 :return-val return-val
		 :args args))
(defgeneric toc-entry (s))
(defgeneric heading (s depth))

(defhtmlmethod toc-entry ((s section))
  (str (title s)))

(defhtmlmethod toc-entry ((s code))
  (:tt (str (title s))))

(defhtmlfun toc (sections &optional (depth 0))
  (flet ((fn ()
	   (dolist (section sections)
	     (let ((sub-sect (children section)))
	       (htm (:li (:a :href (format nil "#~a" (anchor section))
			     (toc-entry section))
		      (when sub-sect (toc sub-sect (1+ depth)))))))))
    (if (eq 0 depth)
	(htm (:ol (fn)))
	(htm (:ul (fn))))))


(defhtmlmethod heading ((s section) depth)
  (htm (:a :name (anchor s))
       (cond
	 ((eq 0 depth) (htm (:h2 (str (title s)))))
	 ((eq 1 depth) (htm (:h3 (str (title s)))))
	 ((eq 2 depth) (htm (:h4 (str (title s)))))
	 (t (htm (:strong (str (title s))))))))

(defhtmlmethod heading ((s code) depth)
  (htm (:a :name (anchor s))
       (:div "[" (str (code-type s)) "]")
       (:strong (str (title s)))
       (when (args s)
	 (str " ")
	 (show-args (args s)))
       (when (return-val s)
	 (htm (str " => ")
	      (show-args (return-val s))))))

(defhtmlfun show-args (args)
  (loop for arg in args
	counting T into i
	do (show-arg arg)
	(when (< i (length args))
	  (htm (str " ")))))

(defhtmlfun show-arg (arg)
  (typecase arg
    (null (htm (:em "nil")))
    (list (if (symbolp (first arg))
	      (let ((name (symbol-name (first arg))))
		(cond 
		  ((equal "QUOTE" name) (htm "'("
					     (show-args (second arg))
					     ")"))
		  ((equal "FUNCTION" name) (htm "#'"
						(show-arg (second arg))))
		  (t (htm "("		
			  (show-args arg)
			  ")"))))
	      (htm "("
		   (show-args arg)
		   ")")))
    (number (str (princ-to-string arg)))
    (symbol (let ((name (string-downcase (symbol-name arg))))
	      (cond
		((equal #\& (aref name 0)) (htm (:tt (str name))))
		((equal "function" name) (str "#'"))
		(t (htm (:em (str name)))))))))

(defhtmlfun content (sections &optional (depth 0))
  (dolist (sec sections)
    (heading sec depth)
    (htm (:div (funcall (content-fn sec))))
    (when (children sec)
      (content (children sec) (1+ depth)))))

(defun get-sections ()
  (list (make-section "Overview and Limitations" #'overview)
	(make-section "Examples" #'examples
		      (make-section "Minimal Pie Chart" #'minimal-pie)
		      (make-section "Minimal Line Chart" #'minimal-line)
		      (make-section "Customized Line Chart" #'customized-line)
		      (make-section "Boinkmark" #'boinkmark)
		      (make-section "Stuart Mackey 1" #'stuart-mackey-1))
	(make-section "Dictionary" #'dictionary
		      (make-code "with-pie-chart" #'with-chart "Macro"
				 '((width height &key (background '(1 1 1))) &body body))
		      (make-code "add-slice" #'add-slice "Function"
				 '(label value &key color))
		      (make-code "with-line-chart" #'with-chart "Macro"
				 '((width height &key (background '(1 1 1))) &body body))
		      (make-code "add-series" #'add-series "Function"
				 '(label data &key (color nil)))
		      (make-code "set-axis" #'set-axis "Function"
				 '(axis title &key (draw-gridlines-p T)
				   (label-formatter #'princ-to-string)
				   (data-interval nil)))
		      (make-code "save-file" #'save-file "Function"
				 '(filename)
				 '(truename)))
	(make-section "Acknowledgements" #'acknowledgements)
	(make-section "Feedback" #'feedback)))

(defhtmlfun save-file ()
  (:blockquote "Draws the chart as a png file to the given path."))

(defhtmlfun set-axis ()
  (:blockquote "Sets the axis on the current line chart.  "
    (:em "axis")
    " must be either "
    (:tt ":x") " or " (:tt ":y") ".  The " (:tt "label-formatter")
    " must be either a format control string or a function of 1 argument that
returns a string with the desired axis label.  The axis printer will try to 
pick decent intervals for labels, but it's still pretty dumb.  You can specify
a data interval using the "
    (:tt ":data-interval")
    " parameter."))

(defhtmlfun add-series ()
  (:blockquote "Add another series to the line chart.  "
    (:em "data")
    " is a list of (x y) pairs.  A color will
be automatically assigned if none is specified."))

(defhtmlfun with-chart ()
  (:blockquote
      "Evaluates body with a chart established with the specified
dimensions as the target for chart commands, with the specified background."))

(defhtmlfun minimal-pie ()
  (let ((filename (file-namestring
		   (adw-charting-tests::minimal-pie-chart))))
    (htm (:pre :style "height:210px"
	       (:img :border 0 :align "right" :src (str filename))
	       "(with-pie-chart (300 200)
    (add-slice \"A\" 5.0d0)
    (add-slice \"B\" 2.0d0)
    (save-file \"minimal-pie-chart.png\"))"))))


(defhtmlfun minimal-line ()
  (let ((filename (file-namestring
		   (adw-charting-tests::minimal-line-chart))))
    (htm (:pre :style "height:310px"
	       (:img :border 0 :align "right" :src (str filename))
	       "(with-line-chart (400 300)
    (add-series \"A\" '((-1 -2) (0 4) (1 5) (4 6) (5 -3)))
    (add-series \"B\" '((-1 4) (0 -2) (1 6) (5 -2) (6 5)))
    (save-file \"minimal-line-chart.png\"))"))))


(defhtmlfun customized-line ()
  (let ((filename (file-namestring
		   (adw-charting-tests::customized-line-chart))))
    (htm (:pre :style "height:310px"
	       (:img :border 0 :align "right" :src (str filename))
	       "(with-line-chart (400 300 :background '(.7 .5 .7))
    (add-series \"A\" '((-.1 -.2) (0 .4) (.1 .5) (.4 .6) (.5 -.3)))
    (add-series \"B\" '((-.1 .4) (0 -.2) (.1 .6) (.5 -.2) (.6 .5)))
    (add-series \"C\"
		'((-.1 0) (0 .3) (.1 .1) (.2 .5) (.4 -.6))
		:color '(.3 .7 .9))
    (set-axis :y \"widgets\" :label-formatter \"~,2F\")
    (set-axis :x nil
	      :draw-gridlines-p nil
	      :label-formatter #'(lambda (v)
				   ;;could do something more interesting here
				   (format nil \"~,1F\" (expt 2 v))))
    (save-file \"customized-line-chart.png\"))"
	      ))))

(defhtmlfun boinkmark ()
  (let ((filename (file-namestring
		   (adw-charting-tests::boinkmark))))
    (htm (:pre :style "height:310px"
	       (:img :border 0 :align "right" :src (str filename))
"(with-line-chart (400 300)
    (add-series \"baker: SBCL\"
		(loop for row in +boink-data+
		      for i from 0
		      collect (list i (nth 4 row))))
    (set-axis :y \"seconds\" :label-formatter \"~,2F\")
    (set-axis :x nil
	      :draw-gridlines-p nil
	      :label-formatter #'(lambda (i)
				   (nth 3 (nth i +boink-data+))))
    (save-file \"boink.png\"))"
	      ))))

(defhtmlfun stuart-mackey-1()
  (let ((filename (file-namestring
		   (adw-charting-tests::stuart-mackey-1))))
    (htm (:pre :style "height:310px"
	       (:img :border 0 :align "right" :src (str filename))
"(with-line-chart (400 300)
    (add-series \"test\" '((1 0.0) (2 2.0) (3 3.0) (4 1.5)) :color '(0 0 1))
    (set-axis :y \"amount\" :label-formatter \"~,2f\")
    (set-axis :x \"days\" :data-interval 1
	      :draw-gridlines-p nil	   
	      :label-formatter (lambda (v) (format nil \"~d\" (round v))))
    (save-file \"stuart-mackey-1.png\"))"


	      ))))

(defhtmlfun add-slice ()
  (:blockquote "Adds a slice to the chart, with an optional color.  A color will
be automatically assigned if none is specified."))



(defun adw-charting-doc ()
  (let ((title "ADW-Charting - simple chart drawing with Common Lisp")
	(canonical-url "http://common-lisp.net/project/adw-charting/")
	(download-url "http://common-lisp.net/project/adw-charting/adw-charting.tar.gz")
	(sections (get-sections))
	(outfile (merge-pathnames *root* #P"./index.html")))
    (setf adw-charting-tests::*root* *root*)
    (with-open-file (*stream* outfile
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
      (with-html-output (*stream* nil :prologue T)
	(:html
	    (:head
		(:title (str title))
	      (:style :type "text/css"
		      (str "
      a, a:visited { text-decoration: none }
      a[href]:hover { text-decoration: underline }
      pre { background: #DDD; padding: 0.25em }
      p.download { color: red }
      a.top {font-size:smallest;}"))
	      )
	  (:body (:h1 (str title))
	    (:blockquote (:h2 "Abstract")
	      (:p "ADW-Charting is a simple chart drawing library for quickly
creating nice-looking pie charts and line charts.  It presents a function-oriented
interface similar to "
		(vecto-link)
		", and saves results to PNG.  Since ADW-Charting and all supporting
libraries are written completely in Common Lisp, without depending on external
non-Lisp libraries, it should work in any Common Lisp environment. ADW-Charting is
available under a BSD-like license. The 'ADW' in the name is referencing my
employer, "
		(:a :href "http://www.acceleration.net" "Acceleration.net")
		", who has sponsored much of this work.  The current version is 0.7,
released on January 28th, 2008.")
	      (:p "The canonical location for ADW-Charting is "
		(:a :href canonical-url (str canonical-url)))
	      (:p :class "download" "Download shortcut:")
	      (:a :href download-url (str download-url)))
	    (:h2 "Contents")
	    (toc sections)
	    (content sections)
	    ))))))