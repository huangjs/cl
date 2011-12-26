;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: listener.lisp,v 1.1.1.1 2004/03/25 06:43:11 mevins Exp $
;;; 
;;; the lisp listener

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "PTY"))

;;; ===================================================================
;;; Lisp Editor
;;; ===================================================================

(declaim
 (special *open-editor-documents* *open-editor-documents-lock*)
 (type list *open-editor-documents*)
 (type lock *open-editor-documents-lock*))
    
(defparameter *open-editor-documents* ())
(defparameter *open-editor-documents-lock* (make-lock))

(defstruct cocoa-editor-info
  (document nil)						; the NSDocument
  (controller nil)					 ; the NSWindowController (maybe).
  (listener nil)				 ; True (a lisp process) if a listener
  (modeline-plist nil)					; info from attribute line
  )
    
(defparameter *default-font-name* "Courier")
(defparameter *default-font-size* 12.0e0)
    
;;; Try to find the specified font.  If it doesn't exist (or isn't
;;; fixed-pitch), try to find a fixed-pitch font of the indicated size.
(defun default-font (&key (name *default-font-name*)
						  (size *default-font-size*))
  (setq size (float size 0.0f0))
  (with-cstrs ((name name))
			  (with-autorelease-pool
			   (let* ((fontname (send (@class ns-string) :string-with-c-string name))
					  (font (send (@class ns-font)
								  :font-with-name fontname :size size)))
				 (if (or (%null-ptr-p font)
						 (not (send font 'is-fixed-pitch)))
					 (setq font (send (@class ns-font)
									  :user-fixed-pitch-font-of-size size)))
				 font))))

(defun size-of-char-in-font (f)
  (let* ((sf (send f 'screen-font)))
    (if (%null-ptr-p sf) (setq sf f))
    (values (send sf 'default-line-height-for-font)
			(send sf :width-of-string #@" "))))
         
    
(defun get-size-for-textview (font nrows ncols)
  (multiple-value-bind (h w) (size-of-char-in-font font)
    (values (fceiling (* nrows h))
			(fceiling (* ncols w)))))

(defun size-textview-containers (tv height width)
  (let* ((scrollview (send (send tv 'superview) 'superview))
		 (window (send scrollview 'window)))
    (rlet ((tv-size :<NSS>ize :height height
					:width (+ width (* 2 (send (send tv 'text-container)
											   'line-fragment-padding)))))
		  (slet ((sv-size
				  (send (@class ns-scroll-view)
						:frame-size-for-content-size tv-size
						:has-horizontal-scroller
						(send scrollview 'has-horizontal-scroller)
						:has-vertical-scroller
						(send scrollview 'has-vertical-scroller)
						:border-type (send scrollview 'border-type))))
				(slet ((sv-frame (send scrollview 'frame)))
					  (incf (pref sv-size :<NSS>ize.height)
							(pref sv-frame :<NSR>ect.origin.y))
					  (send window :set-content-size sv-size))))))
      
(defun info-from-document (doc)
  (with-lock-grabbed (*open-editor-documents-lock*)
					 (find doc *open-editor-documents* :key #'cocoa-editor-info-document)))

(defun info-from-controller (controller)
  (with-lock-grabbed (*open-editor-documents-lock*)
					 (find controller *open-editor-documents* :key #'cocoa-editor-info-controller)))

(defparameter *tab-width* 8)

;;; Create a paragraph style, mostly so that we can set tabs reasonably.
(defun create-paragraph-style (font line-break-mode)
  (let* ((p (make-objc-instance 'ns-mutable-paragraph-style))
		 (charwidth (send (send font 'screen-font)
						  :width-of-string #@" ")))
    (send p
		  :set-line-break-mode
		  (ecase line-break-mode
			(:char #$NSLineBreakByCharWrapping)
			(:word #$NSLineBreakByWordWrapping)
			;; This doesn't seem to work too well.
			((nil) #$NSLineBreakByClipping)))
    ;; Clear existing tab stops.
    (send p :set-tab-stops (send (@class ns-array) 'array))
    (do* ((i 1 (1+ i)))
		((= i 100) p)
      (let* ((tabstop (make-objc-instance
					   'ns-text-tab
					   :with-type #$NSLeftTabStopType
					   :location  (* (* i *tab-width*)
									 charwidth))))
		(send p :add-tab-stop tabstop)
		(send tabstop 'release)))))
    
(defun create-text-attributes (&key (font (default-font))
									(line-break-mode :char)
									(color nil))
  (let* ((dict (make-objc-instance
				'ns-mutable-dictionary
				:with-capacity (if color 3 2))))
    (send dict 'retain)
    (send dict
		  :set-object (create-paragraph-style font line-break-mode)
		  :for-key #@"NSParagraphStyle")
    (send dict :set-object font :for-key #@"NSFont")
    (when color
      (send dict :set-object color :for-key #@"NSColor"))
    dict))
  
(define-objc-method ((:void :display-echo-area contents) lisp-editor-window-controller)
  (send echoarea :set-string-value contents))

(define-objc-method ((:void clear-echo-area)
					 lisp-editor-window-controller)
  (send echoarea :set-string-value #@""))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void :display-package-name name)
					   lisp-editor-window-controller)
	(send packagename :set-string-value name)))

(defun shortest-package-name (package)
  (let* ((shortest (package-name package))
		 (shortest-len (length shortest)))
    (declare (fixnum shortest-len))
    (dolist (nick (package-nicknames package) shortest)
      (let* ((nicklen (length nick)))
		(declare (fixnum nicklen))
		(if (< nicklen shortest-len)
			(setq shortest-len nicklen shortest nick))))))
	     
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void update-package-name)  lisp-editor-window-controller)
	(let* ((info (info-from-controller self))
		   (package (and info (getf (cocoa-editor-info-modeline-plist info)
									:package)))
		   (name (if (and package (typep package 'package))
					 (shortest-package-name package)
				   "#<PACKAGE unset>")))
	  (with-cstrs ((name name))
				  (send self
						:display-package-name (send (@class ns-string)
													:string-with-c-string name))))))
    
;;; The LispEditorWindowController is the textview's "delegate": it
;;; gets consulted before certain actions are performed, and can
;;; perform actions on behalf of the textview.

;;; Action methods implemented by the controller (in its role as the
;;; textview's delegate).

;;; If the first line of the buffer contains text between a pair of
;;; "-*-"s, treat the line as an attribute line. 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void :range-for-modeline-in-text-view tv
							  :result ((* :<NSR>ange) r))
					   lisp-editor-window-controller)
	(let* ((textstring (send tv 'string)))
	  (slet ((linerange
			  (send textstring :line-range-for-range (ns-make-range 0 0))))
			(when (> (pref linerange :<NSR>ange.length) 0)
			  (decf (pref linerange :<NSR>ange.length)))
			(slet ((matchrange1
					(send textstring
						  :range-of-string #@"-*-"
						  :options 0
						  :range linerange)))
				  (rlet ((matchrange2 :<NSR>ange))
						(if (and (> (pref matchrange1 :<NSR>ange.length) 0)
								 (progn
								   (incf (pref matchrange1 :<NSR>ange.location)
										 (pref matchrange1 :<NSR>ange.length))
								   (setf (pref matchrange1 :<NSR>ange.length)
										 (- (pref linerange :<NSR>ange.length)
											(pref matchrange1 :<NSR>ange.location)))
								   (send/stret matchrange2 textstring
											   :range-of-string #@"-*-"
											   :options 0
											   :range matchrange1)
								   (> (pref matchrange2 :<NSR>ange.length) 0)))  
							(setf (pref r :<NSR>ange.location)
								  (pref matchrange1 :<NSR>ange.location)
								  (pref r :<NSR>ange.length)
								  (- (pref matchrange2 :<NSR>ange.location)
									 (pref r :<NSR>ange.location)))
						  (setf (pref r :<NSR>ange.location) 0
								(pref r :<NSR>ange.length) 0))))))))

;;; Return a list whose elements are of the form:
;;;  (opt-name-keyword . (opt-value-start . opt-value-end))
;;;  for each option.  Options are separated colons semicolons;
;;;  option names are separated from option values by colons.
(defun extract-modeline-components (string)
  (let* ((start 0)
		 (end (length string))
		 (options ()))
    (if (find #\: string)
		(block parse-options
		  (do* ((opt-start start (1+ semi))
				semi
				colon)
			  (nil)
			(setq colon (position #\: string :start opt-start :end end))
			(unless colon
			  (return nil))
			(setq semi (or (position #\; string :start colon :end end) end))
			(push
			 (cons
			  (intern
			   (nstring-upcase (string-trim '(#\space #\tab)
											(subseq string opt-start colon)))
			   *keyword-package*)	    
			  (cons
			   (do* ((i (1+ colon) (1+ i)))
				   ((= i semi) (return-from parse-options nil))
				 (unless (whitespacep (schar string i))
				   (return i)))
			   (do* ((i semi j)
					 (j (1- i) (1- j)))
				   (())
				 (unless (whitespacep (schar string j))
				   (return i)))))
			 options)
			(when (= semi end) (return options)))))))

(defun process-modeline-components (components info)
  (let* ((plist ()))
    (dolist (c components (setf (cocoa-editor-info-modeline-plist info) plist))
      (let* ((indicator (car c))
			 (value (cdr c)))
		(case indicator
		  (:package (let* ((spec (let* ((*package* *keyword-package*))
								   (ignore-errors (read-from-string value)))))
					  (when spec
						(let* ((pkg (ignore-errors (find-package
													(if (atom spec)
														spec
													  (car spec))))))
						  (if pkg
							  (setf (getf plist indicator) pkg))))))
		  (t (setf (getf plist indicator) value)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:id :reparse-modeline tv)
					   lisp-editor-window-controller)
	(unless (%null-ptr-p tv)
	  (let* ((info (info-from-controller self)))
		(when info
		  (let* ((textstring (send tv 'string)))
			(rlet ((modelinerange :<NSR>ange))
				  (send self
						:range-for-modeline-in-text-view tv
						:result modelinerange)
				  (unless (zerop (pref modelinerange :<NSR>ange.length))
					(let* ((string (lisp-string-from-nsstring
									(send textstring
										  :substring-with-range modelinerange)))
						   (components
							(mapcar #'(lambda (x)
										(destructuring-bind (name start . end) x
										  (cons name
												(subseq string start end))))
									(extract-modeline-components string))))
					  (process-modeline-components components info)
					  (send self 'update-package-name))))))))
	self))
  

(define-objc-method ((:id :add-modeline tv)
					 lisp-editor-window-controller)
  (let* ((textstring (send tv 'string)))
    (rlet ((modelinerange :<NSR>ange)
		   (selrange :<NSR>ange))
		  (send self :range-for-modeline-in-text-view  tv :result modelinerange)
		  (when (= (pref modelinerange :<NSR>ange.length) 0)
			(let* ((info (info-from-document self))
				   (package (or (if info
									(getf
									 :package
									 (cocoa-editor-info-modeline-plist info)))
								(symbol-value-in-top-listener-process
								 '*package*)
								*package*))
				   (package-name (package-name package))
				   (namelen (length package-name)))
			  (with-cstrs ((pname package-name))
						  (with-nsstr (nsstr pname namelen)
									  (let* ((proto (send (@class ns-string) 
														  :string-with-format 
														  #@";;;-*- Mode: LISP; Package: %@ -*-
" 
														  (:id nsstr))))
										(send tv :set-selected-range (ns-make-range 0 0))
										(send tv :insert-text proto)
										(setf (pref modelinerange :<NSR>ange.location)
											  6
											  (pref modelinerange :<NSR>ange.length)
											  (- (send proto 'length) (+ 6 1 3))))))))
		  (let* ((components (extract-modeline-components
							  (lisp-string-from-nsstring
							   (send textstring
									 :substring-with-range modelinerange))))
				 (package-component (assoc :PACKAGE components)))
			(if package-component
				(destructuring-bind (start . end) (cdr package-component)
				  (setf (pref selrange :<NSR>ange.location)
						(+ start (pref modelinerange :<NSR>ange.location))
						(pref selrange :<NSR>ange.length)
						(- end start)))
			  (setf (pref selrange :<NSR>ange.location)
					(pref modelinerange :<NSR>ange.location)
					(pref selrange :<NSR>ange.length)
					0))
			(send tv :set-selected-range selrange)
			(send tv :scroll-range-to-visible selrange)
			(send tv 'display))))
  self)

;;; Interrupt/abort something.  When that means something ...
(define-objc-method ((:id :interrupt tv) lisp-editor-window-controller)
  (declare (ignore tv))
  self)

(define-objc-method ((:id :eval-defun tv)
					 lisp-editor-window-controller)
  (rlet ((workrange :<NSR>ange))
		(let* ((textbuf (send tv 'string))
			   (textlen (send textbuf 'length)))
		  (slet ((defunrange (send tv 'selected-range)))
				(let* ((pointpos (pref defunrange :<NSR>ange.location)))
				  (if (> (pref defunrange :<NSR>ange.length) 0)
					  (progn
						(setf (pref workrange :<NSR>ange.location)
							  (pref defunrange :<NSR>ange.location)
							  (pref workrange :<NSR>ange.length)
							  (pref defunrange :<NSR>ange.length))
						(multiple-value-bind (ok non-wsp)
							(balanced-expressions-in-range-forward workrange textbuf)
						  (unless (and ok non-wsp)
							(setf (pref defunrange :<NSR>ange.length) 0))))
					(let* ((defun-start (previous-start-of-defun textbuf pointpos)))
					  (when defun-start
						(setf (pref workrange :<NSR>ange.location) defun-start
							  (pref workrange :<NSR>ange.length) (- textlen defun-start))
						(if (forward-over-list workrange textbuf)
							(setf (pref defunrange :<NSR>ange.location)
								  defun-start
								  (pref defunrange :<NSR>ange.length)
								  (- (1+ (pref workrange :<NSR>ange.location))
									 defun-start))
						  (setf (pref defunrange :<NSR>ange.length)
								0)))))
				  (if (and (> (pref defunrange :<NSR>ange.length) 0)
										#|(> pointpos (+ (pref defunrange :<NSR>ange.location)
						   (pref defunrange :<NSR>ange.length)))|#)
	    (send-to-top-listener
	     (info-from-controller self)
	     (send textbuf :substring-with-range defunrange))
	    (#_NSBeep))))))
  self)


;;; Also a delegate method
(define-objc-method ((:<BOOL> :text-view tv
			      :do-command-by-selector (:<SEL> selector))
		     lisp-editor-window-controller)
  ;(#_NSLog #@"selector = %s, self = %@" :<SEL> selector :id self)
  (setq prev-history-count history-count
	history-count 0)
  (if (not (send self :responds-to-selector selector))
    #$NO
    (progn
      (send self :perform-selector selector :with-object tv)
      #$YES)))


;;; The LispEditorDocument class.


(define-objc-method ((:id window-nib-name) lisp-editor-document)
  #@"lispeditor")

(define-objc-method ((:void make-window-controllers) lisp-editor-document)
  (let* ((controller (make-objc-instance
		      'lisp-editor-window-controller
		      :with-window-nib-name (send self 'window-nib-name)
		      :owner self)))
    (send self :add-window-controller controller)
    (send controller 'release)))


(define-objc-method ((:id :data-representation-of-type ((* :char) type))
		      lisp-editor-document)
  (declare (ignorable type))
  ;(#_NSLog #@"dataRepresentationOfType: %s" :address type)
  (send (send textview 'string)
	:data-using-encoding #$NSASCIIStringEncoding
	:allow-lossy-conversion t))

	 
(define-objc-method ((:<BOOL> :load-data-representation data
			      :of-type type)
		     lisp-editor-document)
  (declare (ignorable type))
  ;(#_NSLog #@"loadDataRepresentation:ofType (listener) type = %@" :address type)
  (setq filedata data)
  (not (%null-ptr-p data)))

(define-objc-method ((:void :window-controller-did-load-nib acontroller)
		     lisp-editor-document)
  ;(#_NSLog #@"windowControllerDidLoadNib (editor document)")
  (send-super :window-controller-did-load-nib  acontroller)
  ;; Apple/NeXT thinks that adding extra whitespace around cut & pasted
  ;; text is "smart".  Really, really smart insertion and deletion
  ;; would alphabetize the selection for you (byChars: or byWords:);
  ;; sadly, if you want that behavior you'll have to do it yourself.
  ;; Likewise with the extra spaces.
  (send textview :set-alignment  #$NSNaturalTextAlignment)
  (send textview :set-smart-insert-delete-enabled nil)
  (send textview :set-rich-text nil)
  (send textview :set-uses-font-panel t)
  (send textview :set-uses-ruler nil)
  (with-lock-grabbed (*open-editor-documents-lock*)
    (push (make-cocoa-editor-info
	   :document (%setf-macptr (%null-ptr) self)
	   :controller (%setf-macptr (%null-ptr) acontroller)
	   :listener nil)
	  *open-editor-documents*))
  (set-objc-instance-variable acontroller "textview" textview)
  (set-objc-instance-variable acontroller "echoarea" echoarea)
  (set-objc-instance-variable acontroller "packagename" packagename)
  (send textview :set-delegate acontroller)
  (let* ((font (default-font)))
    (send textview
		  :set-typing-attributes
		  (create-text-attributes
		   :font font
		   :color (send (@class ns-color) 'black-color)))
    (unless (%null-ptr-p filedata)
      (send textview
			:replace-characters-in-range (ns-make-range 0 0)
			:with-string (make-objc-instance
						  'ns-string
			  :with-data filedata
			  :encoding #$NSASCIIStringEncoding))
      (send acontroller :reparse-modeline textview))))

(define-objc-method ((:void close) lisp-editor-document)
  (send-super 'close)
  (let* ((info (info-from-document self)))
    (when info
      (let* ((proc (cocoa-editor-info-listener info)))
        (when proc
	      (setf (cocoa-editor-info-listener info) nil)
	      (process-kill proc)))
      (with-lock-grabbed (*open-editor-documents-lock*)
	(setq *open-editor-documents*
	      (delete info *open-editor-documents*))))))

;;; Syntax utilities

;;; If range is non-empty, return the current char without affecting range.
(defun current-char-in-range (rangeptr textbuf)
  (let* ((width (pref rangeptr :<NSR>ange.length)))
    (declare (ingeger width))
    (if (zerop width)
      nil
      (code-char
       (send textbuf
	     :character-at-index (pref rangeptr :<NSR>ange.location))))))

(defun next-char-in-range (rangeptr textbuf)
  (let* ((width (pref rangeptr :<NSR>ange.length)))
    (declare (integer width))
    (unless (zerop width)
      (setf (pref rangeptr :<NSR>ange.length) (1- width)
	    (pref rangeptr :<NSR>ange.location)
	    (1+ (pref rangeptr :<NSR>ange.location)))
      (current-char-in-range rangeptr textbuf))))

;;; Try to extend the range backward, unless its location is
;;; already at (or below) limit.
(defun prev-char-in-range (rangeptr textbuf &optional (limit 0))
  (let* ((pos (pref rangeptr :<NSR>ange.location)))
    (when (> pos limit)
      (setf (pref rangeptr :<NSR>ange.location)
	    (1- (pref rangeptr :<NSR>ange.location))
	    (pref rangeptr :<NSR>ange.length)
	    (1+ (pref rangeptr :<NSR>ange.length)))
      (current-char-in-range rangeptr textbuf))))

(defun forward-over-#-comment (rangeptr textbuf)
  ;; We've just read a "#|" : the range points to the |.  Return
  ;; T if the number of open #| comments reaches 0 (with the range
  ;; pointing to the outermost closing #), NIL if we hit EOF first.
  (do* ((count 1)
	(pending-open nil)
	(pending-close nil))
       ((zerop count) t)
    (declare (fixnum count))		; Pretty unlikely not to be.
    (case (next-char-in-range rangeptr textbuf)
      ((nil) (return))
      (#\| (if pending-open
	     (progn (incf count) (setq pending-open nil))
	     (setq pending-close t)))
      (#\# (if pending-close
	     (progn (decf count) (setq pending-close nil))
	     (setq pending-open t))))))

(defun backward-over-#-comment (rangeptr textbuf &optional (limit 0))
  ;; We've just read a trailing "|#" : the range points to the |.  Return
  ;; T if the number of open #| comments reaches 0 (with the range
  ;; pointing to the outermost closing #), NIL if we hit EOF first.
  (do* ((count 1)
		(pending-open nil)
		(pending-close nil))
	  ((zerop count) t)
	(declare (fixnum count))			; Pretty unlikely not to be.
	(case (prev-char-in-range rangeptr textbuf limit)
	  ((nil) (return))
	  (#\| (if pending-open
			   (progn (incf count) (setq pending-open nil))
			 (setq pending-close t)))
	  (#\# (if pending-close
			   (progn (decf count) (setq pending-close nil))
			 (setq pending-open t))))))

(defun forward-until-match (rangeptr textbuf matchchar)
  (do* ((ch (next-char-in-range rangeptr textbuf)
			(next-char-in-range rangeptr textbuf)))
	  ((eql ch matchchar) t)
	(when (null ch)
	  (return nil))))

;;; Range points to #\; .  Win if we find a newline before EOF; leave
;;; range pointing to newline on success.
(defun forward-over-semi-comment (rangeptr textbuf)
  (forward-until-match rangeptr textbuf #\Newline))

;;; (Harder to find semi-comments backward ...)

;;; Range points to #\|; find match & leave range pointing there.
(defun forward-over-multi-escape (rangeptr textbuf)
  (forward-until-match rangeptr textbuf #\|))

;;; Advance over a string.  The range points to a leading (unescaped)
;;; #\".  If we find a trailing unescaped #\", return T with the
;;; range pointing to it, else return NIL.
(defun forward-over-string (rangeptr textbuf)
  (do* ((ch (next-char-in-range rangeptr textbuf)
			(next-char-in-range rangeptr textbuf)))
	  ((null ch))
	(if (eql ch #\")
		(return t)
	  (if (eql ch #\\)
		  (when (null (next-char-in-range rangeptr textbuf))
			(return nil))))))

;;; The range points to the trailing unescaped #\".  Back up until
;;; we find a matching unescaped #\".  (We have to back up an extra
;;; char, then move forward if the extra char wasn't a #\\.)  Return
;;; T (with the range pointing at the leading #\"), else NIL.
(defun backward-over-string (rangeptr textbuf &optional (limit 0))
  (do* ((ch (prev-char-in-range rangeptr textbuf limit)
			(prev-char-in-range rangeptr textbuf limit)))
	  ((null ch) nil)
	(when (eql ch #\")
	  (setq ch (prev-char-in-range rangeptr textbuf limit))
	  (if (null ch)
		  (return)
		(unless (eql ch #\\)
		  (next-char-in-range rangeptr textbuf)
		  (return t))))))

;;; Point the range to the first non-whitespace character.
(defun forward-skip-whitespace (rangeptr textbuf)
  (do* ((ch (current-char-in-range rangeptr textbuf)
			(next-char-in-range rangeptr textbuf)))
	  ((null ch))
	(unless (whitespacep ch)
	  (return t))))

;;; Range points to list-open character (e.g., open-paren.)  Return
;;; T if we can advance so that range points to list-close char,
;;; seeing nothing but balanced expressions along the way.
(defun forward-over-list (rangeptr textbuf &optional (close #\)))
  (loop
   (let* ((ch (next-char-in-range rangeptr textbuf)))
	 (if (eql ch close)
		 (return t)
	   (case ch
		 ((nil #\) #\] #\}) (return nil))
		 ;; I suppose that this could be made non-recursive.
		 ;; Anything nested more than a dozen or two levels
		 ;; deep probably means that the cat fell asleep
		 ;; on the keyboard ...
		 (#\( (unless (forward-over-list rangeptr textbuf #\))
				(return nil)))
		 (#\[ (unless (forward-over-list rangeptr textbuf #\])
				(return nil)))
		 (#\{ (unless (forward-over-list rangeptr textbuf #\})
				(return nil)))

		 (#\# (setq ch (next-char-in-range rangeptr textbuf))
		  (if (or (null ch)
				  (and (eql ch #\|)
					   (not (forward-over-#-comment rangeptr textbuf))))
			  (return nil)))
		 (#\" (unless (forward-over-string rangeptr textbuf)
				(return nil)))
		 (#\| (unless (forward-over-multi-escape rangeptr textbuf))
		  (return nil))
		 (#\\ (if (null (next-char-in-range rangeptr textbuf))
				  (return nil)))
		 (#\; (unless (forward-over-semi-comment rangeptr textbuf)
				(return nil))))))))

;;; Return (values T T) if all expressions in range are properly
;;; balanced and something other than semantic whitespace was
;;; seen, else return (values T NIL) if all expressions are
;;; balanced, else return (values NIL NIL) if some expression
;;; is unterminated but nothing's prematurely terminated, else
;;; return (values NIL T)
(defun balanced-expressions-in-range-forward (rangeptr textbuf)
  (do* ((ch (current-char-in-range rangeptr textbuf)
			(next-char-in-range rangeptr textbuf))
		(seen-something-interesting nil))
	  ((null ch) (return (values t seen-something-interesting)))
	(case ch
	  ((#\) #\] #\}) (return (values nil t)))
	  (#\( (if (forward-over-list rangeptr textbuf #\))
			   (setq seen-something-interesting t)
			 (return (values nil nil))))
	  (#\[ (if (forward-over-list rangeptr textbuf #\])
			   (setq seen-something-interesting t)
			 (return (values nil nil))))
	  (#\{ (if (forward-over-list rangeptr textbuf #\})
			   (setq seen-something-interesting t)
			 (return (values nil nil))))
	  (#\" (if (forward-over-string rangeptr textbuf)
			   (setq seen-something-interesting t)
			 (return (values nil nil))))
	  (#\| (if (forward-over-multi-escape rangeptr textbuf)
			   (setq seen-something-interesting t)
			 (return (values nil nil))))
	  (#\; (unless (forward-over-semi-comment rangeptr textbuf)
			 (return (values nil nil))))
	  (#\# (let* ((nextch (next-char-in-range rangeptr textbuf)))
			 (if (null nextch)
				 (return (values nil nil))
			   (if (eql nextch #\|)
				   (unless (forward-over-#-comment rangeptr textbuf)
					 (return (values nil nil)))))))
	  (t
	   (unless seen-something-interesting
		 (unless (whitespacep ch)
		   (setq seen-something-interesting t)))))))
  
(defun previous-start-of-defun (textbuf startpos)
  (rlet ((linerange :<NSR>ange)
		 (posrange :<NSR>ange :length 0))
		(do* ((pos startpos (1- (pref linerange :<NSR>ange.location))))
			((< pos 0))
		  (setf (pref posrange :<NSR>ange.location) pos)
		  (send/stret linerange textbuf :line-range-for-range posrange)
		  (if (eql (current-char-in-range linerange textbuf) #\()
			  (return (pref linerange :<NSR>ange.location))))))


;;; ===================================================================
;;; Listener
;;; ===================================================================

(define-objc-method ((:void :key-down (:id the-event))
					 alpaca-listener-text-view)
  (let ((keyname (get-event-name the-event)))
	(unless (alpaca-handle-keydown keyname *alpaca-listener-keymap*)
	  (send-super :key-down the-event))))


;;; Setup the server end of a pty pair.
(defun setup-server-pty (pty)
  pty)

;;; Setup the client end of a pty pair.
(defun setup-client-pty (pty)
  ;; Since the same (Unix) process will be reading from and writing
  ;; to the pty, it's critical that we make the pty non-blocking.
  (fd-set-flag pty #$O_NONBLOCK)
  (disable-tty-local-modes pty (logior #$ECHO #$ECHOCTL #$ISIG))
  pty)

(declaim (special *open-editor-documents*)
		 (type list *open-editor-documents*))

(defun new-listener-process (procname input-fd output-fd)
  (make-mcl-listener-process
   procname
   (make-fd-stream
	input-fd
	:elements-per-buffer (#_fpathconf
						  input-fd
						  #$_PC_MAX_INPUT))
   (make-fd-stream output-fd :direction :output
				   :elements-per-buffer
				   (#_fpathconf
					output-fd
					#$_PC_MAX_INPUT))
   #'(lambda ()
	   (let* ((info (with-lock-grabbed (*open-editor-documents-lock*)
									   (find *current-process* *open-editor-documents*
											 :key #'cocoa-editor-info-listener))))
		 (when info
		   (setf (cocoa-editor-info-listener info) nil)
		   (send (cocoa-editor-info-document info)
				 :perform-selector-on-main-thread (@selector "close")
				 :with-object (%null-ptr)
				 :wait-until-done nil))))
   #'(lambda ()
	   (setq *listener-autorelease-pool* (create-autorelease-pool))
	   (listener-function))))

(defloadvar *NSFileHandleNotificationDataItem*
  (%get-ptr (foreign-symbol-address "_NSFileHandleNotificationDataItem")))

(defloadvar *NSFileHandleReadCompletionNotification*
  (%get-ptr (foreign-symbol-address "_NSFileHandleReadCompletionNotification")))

(define-objc-method ((:void window-did-load) lisp-listener-window-controller)
  (multiple-value-bind (server client) (ignore-errors (open-pty-pair))
	(when server
	  (let* ((fh (make-objc-instance
				  'ns-file-handle
				  :with-file-descriptor (setup-server-pty server)
				  :close-on-dealloc t)))
		(setq filehandle fh)
		(setq clientfd (setup-client-pty client))
		(send (send (@class ns-notification-center) 'default-center)
			  :add-observer self
			  :selector (@selector "gotData:")
			  :name *NSFileHandleReadCompletionNotification*
			  :object fh)
		(send fh 'read-in-background-and-notify)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void update-package-name)
					   lisp-listener-window-controller)
	(let* ((info (info-from-controller self))
		   (proc (if info (cocoa-editor-info-listener info)))
		   (package (if proc (ignore-errors (symbol-value-in-process
											 '*package*
											 proc))))
		   (name (if (typep package 'package)
					 (shortest-package-name package)
				   "")))
	  (with-cstrs ((name name))
				  (send self :display-package-name (send (@class ns-string)
														 :string-with-c-string name))))))

(define-objc-method ((:void :got-data notification)
					 lisp-listener-window-controller)
  (let* ((data (send (send notification 'user-info)
					 :object-for-key *NSFileHandleNotificationDataItem*))
		 (tv textview)
		 (fh filehandle))
										;(#_NSLog #@"Gotdata: tv = %@, fh = %@" :address tv :address fh)
	(unless (%null-ptr-p tv)
	  (let* ((buffer-text (send tv 'text-storage))
			 (s (make-objc-instance 'ns-string
									:with-data data
									:encoding #$NSASCIIStringEncoding))
			 (str (make-objc-instance 'ns-attributed-string
									  :with-string s
									  :attributes systa)))
		(send buffer-text :append-attributed-string str)

		(let* ((textlen (send buffer-text 'length)))
		  (send tv :scroll-range-to-visible (ns-make-range textlen 0))
		  (setq outpos textlen))
		(send str 'release)))
	(send self 'update-package-name)
	(send fh 'read-in-background-and-notify)))

;;; The LispListenerWindowController is the textview's "delegate": it
;;; gets consulted before certain actions are performed, and can
;;; perform actions on behalf of the textview.

(define-objc-method ((:<BOOL> :text-view tv
							  :should-change-text-in-range (:<NSR>ange range)
							  :replacement-string replacement-string)
					 lisp-listener-window-controller)
  (declare (ignorable replacement-string))
  (if (< (pref range :<NSR>ange.location) outpos)
	  (progn
		(#_NSBeep)						;Overkill, maybe.
		nil)
	(progn
	  (send tv :set-typing-attributes userta)
	  t)))

;;; Action methods implemented by the controller (in its role as the
;;; textview's delegate).


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void :send-string string)
					   lisp-listener-window-controller)
	(send filehandle
		  :write-data (send string
							:data-using-encoding #$NSASCIIStringEncoding
							:allow-lossy-conversion t))))

(define-objc-method ((:id :insert-newline tv)
					 lisp-listener-window-controller)
  (let* ((textbuf (send tv 'text-storage))
		 (textlen (send textbuf 'length))
		 (textstring (send tv 'string)))
	(slet ((r (send tv 'selected-range)))
		  (let* ((curpos (pref r :<NSR>ange.location))
				 (curlen (pref r :<NSR>ange.length)))
			(cond ((>= curpos outpos)
				   ;; Insert the newline at the end of any selection.
				   (incf curpos (pref r :<NSR>ange.length))
				   (send tv :set-selected-range (ns-make-range curpos 0))
				   (send tv :insert-newline self)
				   (incf curpos)
				   (incf textlen)
				   (when (= curpos textlen)
					 (let* ((sendlen (- textlen outpos))
							(sendstring
							 (send textstring
								   :substring-with-range (ns-make-range outpos sendlen))))
					   (setf (pref r :<NSR>ange.location) 0
							 (pref r :<NSR>ange.length) sendlen)
					   (multiple-value-bind (ok second-value)
						   (balanced-expressions-in-range-forward r sendstring)
						 (if ok
							 (if second-value
								 (progn
								   (send self :send-string sendstring)
								   (setq outPos textlen)))
						   (if second-value
							   (#_NSBeep)))))))
				  ;; If there's a selection, copy it to the end of the
				  ;; buffer, then move to the end of the buffer.
				  ((> curlen 0)
				   (slet ((endrange (ns-make-range textlen 0)))
						 (send tv :set-selected-range endrange)
						 (send tv :insert-text
							   (send textstring :substring-with-range r))
						 (setf (pref endrange :<NSR>ange.location)
							   (send textbuf 'length))
						 (send tv :scroll-range-to-visible endrange)))
				  ;; No selection, insertion point is before outpos (in
				  ;; history or in output.  If in history, copy history
				  ;; item to end of buffer, otherwise, do nothing.
				  (t
				   (rlet ((lr :<NSR>ange)
						  (fullrange :<NSR>ange :location 0 :length textlen))
						 (let* ((attr
								 (send textbuf
									   :attribute #@"NSColor"
									   :at-index curpos
									   :longest-effective-range lr
									   :in-range fullrange)))
						   (when (send attr :is-equal  usercolor)
							 (let* ((history-start (pref lr :<NSR>ange.location))
									(history-len (pref lr :<NSR>ange.length)))
							   (when (eql
									  (send textstring
											:character-at-index 
											(+ history-start (1- history-len)))
									  (char-code #\NewLine))
								 (decf (pref lr :<NSR>ange.length)))
							   (unless (eql 0 history-len)
								 (setf (pref fullrange :<NSR>ange.location)
									   textlen
									   (pref fullrange :<NSR>ange.length)
									   0)
								 (send tv :set-selected-range  fullrange)
								 (send tv :insert-text
									   (send textstring :substring-with-range lr))
								 (setf (pref fullrange :<NSR>ange.location)
									   (send textbuf 'length))
								 (send tv :scroll-range-to-visible fullrange)))))))))))
  self)

;;; Force a break in the listener process.
(define-objc-method ((:id :interrupt tv) lisp-listener-window-controller)
  (declare (ignore tv))
  (let* ((info (info-from-controller self))
		 (proc (if info (cocoa-editor-info-listener info))))
	(when proc (force-break-in-listener proc))
	self))

;;; This exists solely for debugging.
(define-objc-method ((:id :log-attrs tv)  lisp-listener-window-controller)
  (slet ((selection (send tv 'selected-range)))
		(rlet ((lr :<NSR>ange))
			  (let* ((textbuf (send tv 'text-storage))
					 (attr
					  (send textbuf
							:attributes-at-index (pref selection :<NSR>ange.location)
							:longest-effective-range lr
							:in-range (ns-make-range 0 (send textbuf 'length)))))
				(#_NSLog #@"Attr = %@, range = [%d,%d]"
						 :address attr
						 :unsigned-fullword (pref lr :<NSR>ange.location)
						 :unsigned-fullword (pref lr :<NSR>ange.length)))
			  self)))

;;; If we're at the end of the buffer and at the start of a line (either
;;; at outpos or after a newline), send an EOF (0 bytes of data) to the
;;; listener.  Otherwise, have the textview do a "deleteForward:"
(define-objc-method ((:id :delete-forward tv)  lisp-listener-window-controller)
										;(#_NSLog #@"In deleteForwardOrSendEOF:")
  (slet ((selection (send tv 'selected-range)))
		(let* ((textbuf (send tv 'text-storage))
			   (length (send textbuf 'length)))
		  (if (and (eql length (pref selection :<NSR>ange.location))
				   (or (eql outpos length)
					   (and (> length 1)
							(= (send textbuf :character-at-index  (1- length))
							   (char-code #\NewLine)))))
			  (%stack-block ((buf 1))
							(setf (%get-byte buf 0) (logand (char-code #\d) #x1f))
							(send filehandle
								  :write-data (send (@class ns-data)
													:data-with-bytes buf
													:length 1))
							(send filehandle 'synchronize-file))
			(send tv :delete-forward self))
		  self)))

(define-objc-method ((:id :add-modeline tv) lisp-listener-window-controller)
  (declare (ignore tv))
  self)

(define-objc-method ((:id :reparse-modeline tv)
					 lisp-listener-window-controller)
  (declare (ignore tv))
  self)

(define-objc-method ((:void dealloc) lisp-listener-window-controller)
  (send (send (@class ns-notification-center) 'default-center)
		:remove-observer self)
  (send-super 'dealloc))
  
;;; The LispListenerDocument class.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-class-method ((:id top-listener) lisp-listener-document)
	(let* ((all-documents (send *NSApp* 'ordered-Documents)))
	  (dotimes (i (send all-documents 'count) (%null-ptr))
		(let* ((doc (send all-documents :object-at-index i)))
		  (when (eql (send doc 'class) self)
			(return doc)))))))

(defun symbol-value-in-top-listener-process (symbol)
  (let* ((listenerdoc (send (@class lisp-listener-document) 'top-listener))
		 (info (info-from-document listenerdoc))
		 (process (if info (cocoa-editor-info-listener info))))
	(if process
		(ignore-errors (symbol-value-in-process symbol process))
	  (values nil t))))

(define-objc-method ((:<BOOL> is-document-edited) lisp-listener-document)
  nil)

(define-objc-method ((:void make-window-controllers)  lisp-listener-document)
  (let* ((controller (make-objc-instance
					  'lisp-listener-window-controller
					  :with-window-nib-name (send self 'window-nib-name)
					  :owner self)))
	(send self :add-window-controller controller)
	(send controller 'release)))

(defloadvar *cocoa-listener-count* 0)

(define-objc-method ((:void :window-controller-did-load-nib acontroller)
					 lisp-listener-document)
  ;;(#_NSLog #@"windowControllerDidLoadNib (listener document)")
  (send-super :window-controller-did-load-nib acontroller)
  ;; We'll use attribute-change information to distinguish user
  ;; input from system output.  Be fascist about letting the
  ;; user change anything.
  (send textview :set-rich-text nil)
  (send textview :set-uses-font-panel nil)
  (let* ((listener-name (if (eql 1 (incf *cocoa-listener-count*))
							"Listener"
						  (format nil
								  "Listener-~d" *cocoa-listener-count*)))
		 (info (info-from-document self)))
	(setf (cocoa-editor-info-listener info)
		  (let* ((tty (%ptr-to-int (get-objc-instance-variable
									acontroller
									"clientfd"))))
			(new-listener-process listener-name tty tty)))
	(send self :set-file-name  (%make-nsstring listener-name)))
  (set-objc-instance-variable acontroller "textview" textview)
  (set-objc-instance-variable acontroller "echoarea" echoarea)
  (set-objc-instance-variable acontroller "packagename" packagename)
  (let* ((userta (send (send textview 'typing-attributes) 'retain))
		 (systa (create-text-attributes :color (send (@class ns-color)
													 'blue-color))))
	(set-objc-instance-variable acontroller "userta" userta)
	(set-objc-instance-variable acontroller "usercolor"
								(send userta :value-for-key #@"NSColor"))
	(set-objc-instance-variable acontroller "systa" systa))
  (send textview :set-delegate  acontroller)
  (unless (%null-ptr-p filedata)
	(send textview
		  :replace-characters-in-range (ns-make-range 0 0)
		  :with-rtfd filedata))
  ;; in the Alpaca listener we want to start out in the "CCL"
  ;; package, because that's where all the interesting symbools are
  (send acontroller :send-string #@"(in-package :ccl)
"
		))

;;; This is almost completely wrong: we need to ensure that the form
;;; is read in the correct package, etc.
(defun send-to-top-listener (sender-info nsstring &optional (append-newline t))
  (declare (ignorable sender-info))
										;(#_NSLog #@"sending string \"%@\"" :address nsstring)
  (let* ((listener
		  (info-from-document (send (@class lisp-listener-document)
									'top-listener))))
	(when listener
	  (let* ((controller (cocoa-editor-info-controller listener)))
		(send controller :send-string nsstring)
		(when append-newline
		  (send controller :send-string #@"
"
				))))))
