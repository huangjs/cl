;;; **********************************************************************
;;; Copyright (C) 2005 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.2 $
;;; $Date: 2006/03/29 15:55:06 $

(in-package :cm)

(defun bar (&optional (n 10) (r .2)) ;testing
  (process repeat n output (new midi :time (now) 
                                :keynum (between 60.0 80.0)) 
           wait r))

(defclass cmio ()
  ((pages :initform (make-hash-table :size 32) :accessor cmio-pages)
   (notebooks :initform (list nil nil nil nil)
              :accessor cmio-notebooks)
   ;; two label, the first holds a status image, the second is used as
   ;; text message buffer.
   (output :initform (list nil nil) :accessor cmio-output)
   (window :initform nil :accessor cmio-window)
   (flags :initform 0 :accessor cmio-flags)
   (colors :initform nil :accessor cmio-colors)))

(defconstant +cmio-command-active+ 1)

(defparameter *cmio-source-pages*
  '( :score :realtime))

(defparameter *cmio-target-pages*
  '(:clm :cmn :csound :fomus :midi :midishare :osc :plotter 
        :portmidi :sc :seq))

(defgeneric cmio-create-page (cmio page notebook pos))
(defgeneric cmio-page-data (cmio page &optional op))
(defgeneric cmio-set-page-data (cmio page data))
(defgeneric cmio-stream-data (cmio page stream))
(defgeneric cmio-ensure-event-stream (cmio target data))
(defgeneric cmio-write (cmio target objs starts))
(defgeneric cmio-notebook-switch-page (cmio notebook page))

;;; cmio-page-can-do? returns true if page supports an operation
;;; should make these real methods at some point

(defun cmio-page-can-do? (cmio page op) 
  (declare (ignore cmio ))
  (case page
    (( :clm :cmn :csound :fomus :midi :plotter :sc :seq )
     (case op
       (:write t)
       (t nil)))
    (( :midishare :portmidi )
     (case op
       (:write t)
       (:realtime t)
       (t nil)))
    (( :osc )
     (case op
       (:realtime t)
       (t nil)))))

;;;
;;; cmio-ensure-event-stream return 2 values: (1) a fully
;;; initialized stream for writing, (2) an error code or nil

(defmethod cmio-ensure-event-stream  (cmio target data)
  cmio target data
  (values nil nil))

;;; cmio-page-data returns a list whose car is the keyword page name
;;; and cdr is property list containing keyword names and values from
;;; each field.

(defmethod cmio-page-data (cmio (page t) &optional op)
  op
  (cmio-print cmio :warning "no data for page ~s" page)
  nil)

(defun get-data (data key &optional default)
  ;; get a value in data list, car of list is always keyword page name
  (getf (CDR data) key default))

(defun (setf get-data) (value data key)
  (setf (getf (CDR data) key) value))

;;;
;;; cmio-set-page-data sets page widget values given a plist of data.
;;; does not return values or signal errors

(defmethod cmio-set-page-data (cmio page data)
  cmio page data
  (values))

(defun set-page-fields (user &rest data)
  ;; user is a property list of page field names and values. set each
  ;; entry widget in data if its name in user is recognized, coercing
  ;; non-string values to strings if necessary.
  (do ((*print-case* ':downcase)
       name type widget value)
      ((null user) nil)
    (setq name (pop user)
          type nil 
          widget nil
          value nil)
    (loop with w for (n v) on data by #'cddr
       do (if (consp v)
            (setq type (first v) w (cdr v))
            (setq type ':entry w v))
       (when (eql n name)
         (setq widget w)
         (return)))
    (setq value (pop user))
    (when widget 
      ;(print (list :name-> name :type-> type :value-> value :widget-> widget))
      (ecase type
        (:entry
         (when value
           (unless (stringp value)
             (setq value (format nil "~S" value)))
           (gtk:entry-set-text widget value)))
        (:spin
         (multiple-value-bind (arg lo hi)
             (gtk:spin-button-get-range widget 0 0 )
           arg
           (when (and (numberp value) (<= lo value hi))
             (gtk:spin-button-set-value widget value))))
        (:check
         (gtk:toggle-button-set-active widget (if value t nil)))
        (:radio
         ;; widget is a PLIST of radio buttons (:name widget...)
         (when (setq widget (getf widget value))
           (gtk:toggle-button-set-active widget t)))
        (:menu
         ;; widget is (<menu> . items)
         (let ((p (position value (cdr widget) :test #'equal)))
           (when p (gtk:option-menu-set-history (car widget) p))))))))

;;;
;;; Utilities
;;;

(defun cmio-color (cmio name)
  (or (getf (cmio-colors cmio) name)
      (error "No cmio color named ~S" name)))

(defun cmio-command-active? (cmio)
  ;; currently unused...
  (logtest (cmio-flags cmio) +cmio-command-active+))

(defun (setf cmio-command-active?) (bool cmio)
  ;; currently unused...
  (let ((flags (cmio-flags cmio)))
    (setf (cmio-flags cmio) 
          (if bool (logior flags +cmio-command-active+)
              (logandc2 flags +cmio-command-active+)))))

(defun cmio-current-source-page (cmio)
  ;; return keyword name of current source or target page
  (elt *cmio-source-pages*
       (gtk:notebook-get-current-page (first (cmio-notebooks cmio)))))

(defun cmio-current-target-page (cmio)
  (elt *cmio-target-pages*
       (gtk:notebook-get-current-page (second (cmio-notebooks cmio)))))

(defun ensure-directory (dir)
  ;; this nonsese due to openmcl probe-file working for
  ;; paths like /tmp/test.mid/
  (if (not (char= (elt dir (1- (length dir))) #\/))
      (setq dir (format nil "~a/" dir)))
  (setq dir (probe-file dir))
  (if (not dir)
      nil
      (let ((str (namestring dir)))
        (if (not (char= (elt str (1- (length str))) #\/))
            nil
            str))))

;(ensure-directory "/bif")
;(ensure-directory "/bif/")
;(ensure-directory "/bif.mid")
;(ensure-directory "/tmp")
;(ensure-directory "/tmp/")
;(ensure-directory "/Users/hkt/test.mid")
;(ensure-directory "/Users/hkt/test.mid/")

;;; 
;;; Getting/setting page information. widgets are stored in an ad hoc
;;; manner according to the requirements of each page

(defstruct (notebook-page (:type list))
  page notebook widgets props)

(defun cmio-page-prop (cmio page prop)
  (getf (notebook-page-props (gethash page (cmio-pages cmio)))
        prop))

(defun (setf cmio-page-prop) (val cmio page prop)
  (setf (getf (notebook-page-props (gethash page (cmio-pages cmio)))
              prop)
        val))

(defun cmio-new-page (cmio &key notebook page widgets props)
  ;; page and notebook are keyword symbols
  (setf (gethash page (cmio-pages cmio))
        (make-notebook-page :page page :notebook notebook
                            :props props :widgets widgets)))

(defun cmio-page-widgets (cmio page &optional group)
  (let* ((data (or (gethash page (cmio-pages cmio))
                   (error "No data for page: ~s" page))))
    (setq data (notebook-page-widgets data))
    (if group
      (or (getf data group)
          (error "Page ~S: no data for group: ~s" page group))
      data)))

(defun cmio-set-page-widgets (cmio page widgets)
  (setf (notebook-page-widgets (gethash page (cmio-pages cmio)))
        widgets))

(defun set-subgroup-sensitivity (window target group active?
                                 &optional first?)
  ;; sets sensitivity on subgroups of widgets
  (let* ((cmio (widget->object window))
         (subs (cmio-page-widgets cmio target group)))
    ;; car of each sublist is normally a checkbox that should remain
    ;; sensitive
    (dolist (w (if (not first?) (cdr subs) subs))
      ;; HACK: handle subgroups in plist format or with
      ;; other
      (unless (or (symbolp w) (listp w) (functionp w))
        (gtk:widget-set-sensitive w active?)))
    active?))

(defparameter %bw 5)  ; border width
(defparameter %sw 5)  ; box spacing width
(defparameter %rs 2)  ; table row spacing
(defparameter %rc "blue") ; required color

;;;
;;; button utils
;;;

(defun make-iconic-button (icon name )
  (let ((bbox (gtk:hbox-new nil 2))
        (imag (gtk:image-new-from-stock icon gtk:icon-size-small-toolbar))
        (labl (gtk:label-new "Compose"))
        button)
    (gtk:label-set-text labl (format nil "~A" name))
    (setq button (gtk:button-new ))
    (gtk:container-add button bbox)
    (gtk:widget-show bbox)
    (gtk:box-pack-start bbox imag nil nil 0)
    (gtk:widget-show imag)
    (gtk:box-pack-start bbox labl nil nil 0)
    (gtk:widget-show labl)
    (values button bbox)))

;;;
;;; notebook utils
;;;

(defun cmio-notebook-widget (cmio notebook)
  ;; coerce notebook names to widgets
  (if (symbolp notebook)
      (let ((books (cmio-notebooks cmio)))
        (ecase notebook
          (:compose (first books))
          (:target (second books))
          (:executive (third books))))
      notebook))

(defun stub-notebook-page (cmio notebook page pos)
  cmio
  (let ((label (gtk:label-new page))
        (vbox (gtk:vbox-new nil 5))
        hbox entry check)
    hbox entry check
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show label)
    (gtk:widget-show vbox)
    (setq label (gtk:label-new "Not yet implemented."))
    (gtk:widget-set-sensitive label nil)
    (gtk:box-pack-start vbox label t t 0)
    (gtk:widget-show label)
    (values vbox)))

(defun set-notebook-page-sensitivity (cmio notebook pages active?)
  (let (nb pgs)
    (cond ((eq notebook :compose)
           (setq nb (first (cmio-notebooks cmio)))
           (setq pgs *cmio-source-pages*))
          ((eq notebook :targets)
           (setq nb (second (cmio-notebooks cmio)))
           (setq pgs *cmio-target-pages*))
          (t (error "notebook ~s not :targets or :compose" notebook)))
    (if (eql pages t) (setq pages pgs)
        (if (not (consp pages)) (setq pages (list pages))))
    (loop for p in pages
       for i = (or (position p pgs)
                   (error "Page ~s is not in ~s." p pgs))
       for c = (gtk:notebook-get-nth-page nb i)
       for l = (gtk:notebook-get-tab-label nb c)
         ;; dont allow unloaded systems to become active
       ;for f = (if (and active? (eql notebook :targets))
       ;            (target-system-ready? p)
       ;            active?)
       do
         (gtk:widget-set-sensitive c active?)
         (gtk:widget-set-sensitive l active?))))

(defun notebook-page-sensitive? (cmio notebook page)
  (let (nb pgs pg)
    (ecase notebook 
      (:compose
       (setq nb (first (cmio-notebooks cmio)))
       (setq pgs *cmio-source-pages*))
      (:targets
       (setq nb (second (cmio-notebooks cmio)))
       (setq pgs *cmio-target-pages*)))
    (setq pg  (gtk:notebook-get-nth-page 
               nb (or (position page pgs)
                      (error "Page ~s is not in ~s."
                             page pgs))))
    (not (logtest (gtk:widget.state pg)
                  gtk:state-insensitive))))

;;;
;;; Printing and error messaging
;;;

(defun cmio-print (cmio condition string &rest args)
  ;; mode is :append or :replace
  (let ((tyio (SECOND (cmio-output cmio)))
        (imag (FIRST (cmio-output cmio)))
        (colors (cmio-colors cmio))
        (icon nil)
        (width 90)
        (color nil))
    (ecase condition
      ((t :message)
       (setq color (getf colors ':green))
       (setq icon "gtk-yes"))
      (:error
       (setq color (getf colors ':red))
       (setq icon "gtk-dialog-error"))
      ((:warning :warn)
       (setq color (getf colors ':yellow))
       (setq icon "gtk-dialog-warning")))     ;#FF8D00
    (gtk:image-set-from-stock imag icon gtk:icon-size-small-toolbar)
    (gtk:widget-show imag)
    (when args (setq string (apply #'format nil string args)))
    (unless (< (length string) width)
      (setq string (concatenate 'string
                                (subseq string 0 (- width 4))
                                " ...")))
    (gtk:widget-modify-fg tyio gtk:state-normal color)
    (gtk:label-set-text tyio string)
    (values)))

(defun report-error (err &key (label "") widget entry window)
  (unless window
    (setq window (gtk:widget-get-toplevel (or entry widget))))
  (when entry (setq widget entry))
  (let* ((cmio (widget->object window))
         (str ""))
    (cond ((stringp err) (setq str err))
          ((eql err +se-nullstring+)
           (setq str " Missing value."))
          ((eql err +se-unreadable+)
           (setq str " Unreadable lisp value."))
          ((eql err +se-multiple+)
           (setq str " Only one value allowed."))
          ((eql err +se-not-number+)
           (setq str " Not a number."))
          ((eql err +se-not-symbol+)
           (setq str " Not a number."))
          ((eql err +se-not-cons+)
           (setq str " Not a list."))
          ((eql err +se-incorrect+)
           (setq str " Incorrect value."))
          ((eql err t)
           (setq str " Execution error.")))
    (setq label (concatenate 'string label str))
    ;; this could dispatch on clos object...
    (cmio-print cmio :error label)
    (if (and entry (not (eql err +se-nullstring+)))
      (gtk:editable-select-region entry 0 -1))
    (if widget (gtk:widget-grab-focus widget))
    err))

(defun eval-error-string (e)
  (format nil " Eval signaled '~(~A~)' error."
          (class-name (class-of e))))

(defun cmio-required-label (cmio text)
  ;;(gtk:widget-modify-text widget gtk:state-normal color)
  (let ((lab (gtk:label-new text)))
    (gtk:widget-modify-fg lab gtk:state-normal
                            (getf (cmio-colors cmio) ':blue))
    lab))

(defun cmio-show-widget-required (cmio widget)
  ;; called on labels whose fields are "required"
  (gtk:widget-modify-fg widget gtk:state-normal 
                        (cmio-color cmio :blue)))  

(defun cmio-show-widget-evalable (cmio widget)
  ;; called on entries and text views that are evalled.
  (gtk:widget-modify-base widget gtk:state-normal 
                          (cmio-color cmio :pale-yellow)))

(defun cmio-clear-message (cmio)
  (let ((output (cmio-output cmio)))
    (gtk:widget-hide (first output))
    (gtk:label-set-text (second output) "")))

(gtk:define-signal-handler cmio_noop :void (widget data)
  ;; widget is an uninplemented button, data is window.
  (cmio-print (widget->object data)
              :warning "Sorry, '~@(~A~)' not yet implemented."
              (gtk:button-get-label widget)))

(defun stub-button (button window)
  ;; stub button out with moderatly useful message.
  (g:signal-connect button "clicked" (g:callback cmio_noop) window))

;;;
;;; Page data getters
;;;

(defun add-check-data (data key widget)
  (nconc data (list key (gtk:toggle-button-get-active widget))))

(defun add-radio-data (data key widget onval &optional (offval nil op))
  (if (gtk:toggle-button-get-active widget)
    (nconc data (list key onval))
    (if op (nconc data (list key offval))
        nil)))

(defun add-spin-data (data key widget &key result)
  (let ((val (gtk:spin-button-get-value widget)))
    (nconc data (list key (if result (funcall result val) val)))))

(defun add-menu-data (data key widget items) 
  (let ((num (gtk:option-menu-get-history widget)))
    (nconc data (list key (elt items num)))))

(defun add-entry-data (data key widget &rest args)
  (multiple-value-bind (val err) (apply #'entry-expr widget args)
    (if err
      (nconc data (list key (list :error err :entry widget)))
      ;; if :read is nil AND user starts with #. then read
      (if (and (stringp val)
               (null (getf args :read t))
               (> (length val) 2)
               (char= (elt val 0) #\#)
               (char= (elt val 1) #\.))
        (let ((sav val))
          (multiple-value-setq (val err)
            (string->expr val))
          (if err
            (nconc data (list key (list :error
                                        (format nil "~S cannot be evaled."
                                                sav) 
                                        :entry widget)))
            (nconc data (list key val))))
        (nconc data (list key val))))))

(defun data-check-file-type (data prop entry &rest types)
  (let* ((file (get-data data prop))
         (type nil))
    (when (stringp file)
      (setq type (pathname-type file))
      (if (not (member type types :test #'equal))
          (setf (get-data data prop)
                (list ':error
                      (format nil
                              "~:[Missing file type~;Unknown file type: ~S~]."
                              type type)
                      ':entry entry))))))

;;;
;;; Error reporting

(defun keyword->label (keyw &optional (colon t))
  (let ((str (format nil "~@(~A~)~@[:~]" keyw colon)))
    (if (find #\- str) (substitute #\space #\- str)
        str)))

(defun data-errors? (data cdr?)
  (let ((check (if cdr? (cdr data) data)))
    ;; check property values for (:error ...)
    (loop for tail on (cdr check)
       if (and (consp (car tail)) (eql (car (car tail)) ':error))
       return t)))

(defmacro with-data-errors-aborted ((data &rest others) &body body)
  ;; process the properties in data and report/abort on
  ;; any error
  (let ((check (gensym)))
    `(error-block
      (let ((,check ,(if others
                         `(append (cdr ,data)
                                  ,@ (loop for l in others collect `(cdr ,l)))
                         `(cdr ,data))))
        (loop for (key val) on ,check by #'cddr
           if (and (consp val) (eql (car val) ':error))
           ;; val=(:error <err> :entry <widget>)
           do (progn (apply #'report-error (second val)
                            :label (keyword->label key)
                            (cddr val))
                     (error-abort)))
        ,@body))))

(defun strip-entry (entry)
  (let ((raw (gtk:entry-get-text entry)))
    (if (string= raw "") ""
        (string-trim '(#\space #\return #\tab) raw))))

;;;
;;; Stream-control widgets
;;; stream controls

(defmethod cmio-open-stream (cmio page)
  (let ((data (cmio-page-data cmio page ':write)))
    (with-data-errors-aborted (data)
      (multiple-value-bind (stream error?)
          (cmio-ensure-event-stream cmio page data)
        (when error?
          (report-error error? :label "Open:"
                        :window (cmio-window cmio))
          (error-abort))
        (open-io stream :output)
        (setf (cmio-page-prop cmio page ':open) stream)
        stream))))

(defmethod cmio-close-stream (cmio page)
  (let ((open (cmio-page-prop cmio page ':open)))
    (if (and open (io-open open))
        (progn (close-io open :force)
               (setf (cmio-page-prop cmio page ':open) nil)               
               t)
        (progn (setf (cmio-page-prop cmio page ':open) nil)
               nil))))

(defun update-stream-status (cmio widgets status)
  ;; toggle button to open or closed
  ;; widgets: (:name [ ] :icon <image> :open (Open) :hook [ ] :set (Set)
  ;;           :class class :openfn fn :hookfn fn)
  (let ((imag (getf widgets :icon))
        (butn (getf widgets :open))
        (but2 (getf widgets :set))
        (size gtk:icon-size-small-toolbar) 
        (sets '(:receiver :set :receive-rate))
        (name (getf widgets :name))
        (hook (getf widgets :hook)))
    (cond ((eq status :open)
           (gtk:widget-set-sensitive name nil)
           (gtk:button-set-label butn "Close")
           (gtk:image-set-from-stock imag "gtk-yes" size)
           (dolist (p sets)
             (gtk:widget-set-sensitive (getf widgets p) t)))
          ((eq status :close)
           (gtk:widget-set-sensitive name t)
           (gtk:button-set-label butn "Open")
           (gtk:image-set-from-stock imag "gtk-no" size)
           (dolist (p sets)
             (gtk:widget-set-sensitive (getf widgets p) nil)))
          ((eq status :set)
           (gtk:button-set-label but2 "Clear")
           (gtk:widget-show but2)
           (gtk:widget-set-sensitive (getf widgets :receiver) nil)
           (gtk:widget-set-sensitive (getf widgets :receive-rate) nil))
          ((eq status :clear)
           (gtk:button-set-label but2 "Set")
           (gtk:widget-show but2)
           (gtk:widget-set-sensitive (getf widgets :receiver) t)
           (gtk:widget-set-sensitive (getf widgets :receive-rate) t)))
    (when hook
      (funcall hook cmio status))
    (gtk:widget-show imag)))

(defun cmio-stream-open? (cmio page)
  (cmio-page-prop cmio page ':open))

(gtk:define-signal-handler stream_control_open/close :void (widget data)
  (let* ((cmio (widget->object data))
         (page (widget-get-id widget))
         (subs (getf (cmio-page-widgets cmio page) :stream)))
    (cond ((cmio-stream-open? cmio page)
           (if (cmio-close-stream cmio page)
               (update-stream-status cmio subs ':close)))
          (t
           (if (cmio-open-stream cmio page)
               (update-stream-status cmio subs ':open))))))

(gtk:define-signal-handler stream_control_set/clear :void (widget data)
  (let* ((cmio (widget->object data))
         (page (widget-get-id widget))
         (subs (getf (cmio-page-widgets cmio page) :stream))
         (open (cmio-stream-open? cmio page)))
    (if open
        (if (receiver? open)
            (progn
              (remove-receiver! open)
              (update-stream-status cmio subs ':clear))
            ;; data is always cdr of list
            (let ((data (cons :foo (stream-control-data subs t ':set))))
              (with-data-errors-aborted (data)
                (init-io open :receive-rate (get-data data :receive-rate))
                (when (get-data data :receiver)
                  
                  (set-receiver! (coerce (get-data data :receiver)
                                         'function)
                                 open)
                  (update-stream-status cmio subs ':set)))))
        nil)))

(gtk:define-signal-handler stream_control_rate :void (widget data)
  ;; widget is spin                           
  (let* ((cmio (widget->object data))
         (page (cmio-current-target-page cmio))
         (open (cmio-stream-open? cmio page)))
    (when open
      (setf (rt-stream-receive-rate open)
            (coerce (gtk:spin-button-get-value widget)
                    'single-float)))))

(defun stream-controls (cmio page hbox name hook &optional (rate .001))
  ;; widgets: (:name [ ] :icon <image> :open (Open) :receiver [ ] 
  ;;           :set (Set) :hook fn)
  (let* ((window (cmio-window cmio))
         (stream (find-object name nil))
         (open? (and stream (io-open stream) t))
         (image nil)
         (button nil)
         (label nil)
         (entry nil)
         (widgets (list)))
    (when open?
      (setf (cmio-page-prop cmio page :open) stream))
    (setq label (gtk:label-new "Stream:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)

    (setq entry (gtk:entry-new))
    (setq widgets (list* :name entry widgets))
    (gtk:entry-set-text entry name)
    (gtk:entry-set-width-chars entry 16)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    ;; image
    (setq image (gtk:image-new-from-stock (if open? "gtk-yes" "gtk-stop")
                                          gtk:icon-size-small-toolbar))
    (nconc widgets (list :icon image))
    (gtk:box-pack-start hbox image nil nil 0)
    (gtk:widget-show image)
    ;; buttons
    (setq button (gtk:button-new-with-label (if open? "Close " "Open")))
    (widget-set-id button page)
    (nconc widgets (list :open button))
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (g:signal-connect button "clicked" (g:callback stream_control_open/close)
                      window)
    (setq label (gtk:label-new "Receiver:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (when (and stream (receiver? stream))
      (gtk:entry-set-text entry "#<somefunc>"))
    (nconc widgets (list :receiver entry))
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:entry-set-width-chars entry 16)
    (gtk:widget-show entry)
    (setq button (gtk:button-new-with-label
                  (if (and stream (receiver? stream))
                       "Clear" " Set ")))
    (widget-set-id button page)
    (nconc widgets (list :set button))
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (g:signal-connect button "clicked" (g:callback stream_control_set/clear)
                      window)
    (setq label (gtk:label-new "Rate:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:spin-button-new-with-range 
                 .001 .100 rate))
    (when stream
      (gtk:spin-button-set-value entry (rt-stream-receive-rate stream)))
    (g:signal-connect entry "value-changed"
                      (g:callback stream_control_rate) window)
    (nconc widgets (list :receive-rate entry))
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (update-stream-status cmio widgets (if open? :open :close))
;    (update-stream-status cmio widgets (if (and stream (receiver? stream))
;                                           :set :clear))
    (setf (cmio-page-prop cmio page :rt-stream) t)
    (nconc widgets (list :hook hook))
    widgets))

(defun stream-control-data (widgets types &optional op)
  ;; widgets are stream controls:
  ;; (:name [] :icon <> :open () :receiver [] :receive-rate <>)
  (let ((data (list))
        (test nil)
        (prop nil)
        (set? (eql op ':set)) ; receiver required if called from set button
        )
    (setq prop ':name)
    (setq data (add-entry-data data prop (getf widgets prop) :nullok nil 
                               :read nil))
    (setq test (getf data prop))
    (when (stringp test)
      (unless (or (eql types t)
                  (member (pathname-type test) types :test #'equal))
        (setf (getf data prop )
              (list :error
                    (format nil "Unknown file type: ~a" test)
                    :entry (getf widgets prop)))
        (return-from stream-control-data (values data t))))
    (setq prop ':receiver)
    (add-entry-data data prop (getf widgets prop) 
                    :nullok (not set?)
                    :read t)
    (setq test (getf data prop))
    ;; abort if problem with receiver
    (when (and (consp test) (eql (car test) ':error))
      (return-from stream-control-data (values data t)))
    (unless (or (not set?)
                (functionp test)
                (and (symbolp test) (fboundp test))
                (and (consp test) (eql (car test) 'lambda)))
      (setf (getf data prop)
            (list :error
                  (format nil "~A is not a function." test)
                  :entry (getf widgets prop)))
      (return-from stream-control-data (values data t)))
    (setq prop ':receive-rate)
    (add-spin-data data prop (getf widgets prop)
                   :result (lambda (x) (decimals (coerce x 'single-float) 3)))
    (when (periodic-task-running?)
      (let ((time (/ (periodic-task-rate) 1000000.0)))
        ;(print (list time (periodic-task-rate)))
        (unless (= (getf data prop) time)
          (setf (getf data prop)
                (list :error 
                      (format nil
                              "a task is already running at ~s sec."
                              time)
                      :widget (getf widgets prop))))))
    data))

;;;
;;; system setup pages
;;;

(defun target-system-ready? (sys)
  (case sys
    (:clm (find ':clm *features*))
    (:cmn (find ':cmn *features*))
    (:fomus (find ':fomus *features*))
    (:portmidi (and pm:*portmidi* t))
    (:midishare (= (midishare) 1))
    (t t)))

(defun ensure-clm/cmn/fomus (cmio sys)
  (let* ((widgets (cmio-page-widgets cmio sys))
         (sd (first widgets))
         (bd (if (second widgets)
                 (gtk:entry-get-text (second widgets))
                 "")))
    (setq sd (gtk:entry-get-text sd))
    (if (equal sd "")
        (values nil "Source directory required.")
        (if (setq sd (ensure-directory sd))
            (progn
              (unless (equal bd "") 
                (setq bd (ensure-directory bd)))
              (if (not bd)
                  (values nil 
                          (format nil "~A: binary directory does not exist."
                                  sys))
                  (let ((fil (ecase sys 
                               (:clm (format nil "~aall.lisp" sd))
                               (:cmn (format nil "~acmn-all.lisp" sd))
                               (:fomus (format nil "~aload.lisp" sd)))))
                    (when (equal bd "") (setq bd nil))
                    (if (probe-file fil)
                        (cond ((eql sys ':clm)
                               (defparameter clm-directory sd)
                               (if bd (defparameter clm-bin-directory bd))
                               (load fil)
                               t)
                              ((eql sys ':cmn)
                               (defparameter cmn-directory sd)
                               (if bd (defparameter cmn-bin-directory bd))
                               (load fil)
                               t)
                              ((eql sys ':fomus)
                               (load fil)
                               t))
                        (values nil (format nil 
                                            "~A: file ~s does not exist."
                                            sys fil))))))
            (values nil 
                    (format nil "~A: source directory does not exist."
                            sys))))))

(gtk:define-signal-handler system_setup_action :void (widget window)
  widget
  (let* ((cmio (widget->object window))
         (book (second (cmio-notebooks cmio)))
         (pnum (gtk:notebook-get-current-page book))
         (page (elt *cmio-target-pages* pnum))
         (func (cmio-page-prop cmio page :setup))
         flag emsg)
    (multiple-value-setq (flag emsg) (funcall func cmio page))
    (cond ((null flag)
           (report-error emsg :window window))
          (t
           (gtk:notebook-remove-page book pnum)
           (cmio-create-page cmio page book pnum)
           (gtk:notebook-set-current-page book pnum)))))

(defun system-setup-page (cmio page message button-label vbox setup 
                          &optional sdir bdir )
  page
  (let ((window (cmio-window cmio))
        (hbox (gtk:hbox-new t %sw))
        data entry label image button)
    (gtk:box-pack-start vbox hbox t nil 0)
    (gtk:widget-show hbox)
    (setq vbox (gtk:vbox-new nil %sw))    
    (gtk:box-pack-start hbox vbox nil nil 0)
    (gtk:widget-show vbox)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq image (gtk:image-new-from-stock "gtk-dialog-warning"
                                          gtk:icon-size-large-toolbar))
    (gtk:box-pack-start hbox image nil nil 0)
    (gtk:widget-show image)
    (setq label (gtk:label-new message))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (when sdir
      (setq hbox (gtk:hbox-new nil %sw))
      (gtk:box-pack-start vbox hbox nil nil 0)
      (gtk:widget-show hbox)
      (setq label (gtk:label-new "Source directory:"))
      (cmio-show-widget-required cmio label)
      (gtk:box-pack-start hbox label nil nil 0)
      (gtk:widget-show label)
      (setq entry (gtk:entry-new ))
      (push entry data)
      (gtk:entry-set-width-chars entry 20)
      (gtk:box-pack-start hbox entry nil nil 0)
      (gtk:widget-show entry))
    (when bdir
      (setq label (gtk:label-new "Binary directory:"))
      (gtk:box-pack-start hbox label nil nil 0)
      (gtk:widget-show label)
      (setq entry (gtk:entry-new ))
      (push entry data)
      (gtk:entry-set-width-chars entry 20)
      (gtk:box-pack-start hbox entry nil nil 0)
      (gtk:widget-show entry))

    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq button (gtk:button-new-with-label button-label))
    (g:signal-connect button "clicked"
                      (g:callback system_setup_action)
                      window)
    (gtk:box-pack-start hbox button t nil 0)
    (gtk:widget-show button)
    (setf (cmio-page-prop cmio page :setup) setup)
    (cmio-set-page-widgets cmio page (reverse data))
    ))

;;;
;;; :score page
;;;

(defmethod cmio-notebook-switch-page (cmio (notebook (eql :compose))
                                      (page (eql :score)))
  (set-notebook-page-sensitivity cmio :targets t t)
  (set-notebook-page-sensitivity cmio :targets '(:osc) nil))

(gtk:define-signal-handler cmio_write :void (widget window)
  ;; widget is write button on the Event IO page.
  widget 
  (let* ((cmio (widget->object window))
         (tpage (cmio-current-target-page cmio)))
    (if (cmio-page-can-do? cmio tpage ':write)
        (if (target-system-ready? tpage)
            (let ((tdata (cmio-page-data cmio tpage ':write))
                  (edata (cmio-page-data cmio ':score ':write)))
              (with-data-errors-aborted (tdata edata)
                (cmio-clear-message cmio)
                (let ((widgets (cmio-page-widgets cmio ':score))
                      (stream (cmio-ensure-event-stream cmio tpage tdata))
                      (starts (get-data edata ':starts))
                      (events (get-data edata ':events))
                      output error?)
                  (multiple-value-setq (output error?)
                    (safecall #'events events stream starts))
                  (if error?
                      (report-error (eval-error-string error?)
                                    :label "Events:" 
                                    :entry (getf widgets :events))
                      (progn
                        (when (stringp output)
                          (setq output (namestring (truename output)))
                          (gtk:entry-set-text (getf widgets :path)
                                              output))
                        (cmio-print cmio :message "Output: ~S ok."
                                    output))))))
            (cmio-print cmio :error 
                        "Score: ~@(~A~) page is not ready."
                        tpage))
        (cmio-print cmio :warning
                    "Score: ~@(~A~) page does not support score output."
                    tpage))))

(defparameter *cmio-file-types*
  '((:audio "snd" "aiff" "wav")
    (:midi "mid" "midi")
    (:sco "sco")
    (:ins "ins")
    (:osc "osc")
    (:clm "clm")
    (:fomus "ly" "xml") ))

(gtk:define-signal-handler cmio_score_handler :void (widget window)
  ;; widget is execute button, data is window
  (let* ((cmio (widget->object window))
         (name (widget-get-id widget))
         ;;(data (cmio-page-prop cmio :score :output))
         (data (cmio-output-file-data cmio))
         (path nil)
         (type nil)
         )
    (with-data-errors-aborted (data)
      (setq path (get-data data :path))
      (setq type (get-data data :score-type))
      (cmio-print cmio :message "~@(~A~) ~S ok." name path)
      (case name
        (:import (cmio-import cmio path type))
        (:play (play path))
        (:view (cmio-view cmio path type))))))

(gtk:define-signal-handler cmio_cd :void (widget data)
  ;; widget is directory box, data is window
  widget data
  (let ((cmio (widget->object data))
        (str (gtk:entry-get-text widget)))
    (if (string= str "")
      (progn (cd) 
             (gtk:entry-set-text widget (pwd))
             (cmio-print cmio :message "cd ~A ok." (pwd))
             )
      (let ((dir (ensure-directory str)))
        (if (not dir)
          (progn
            (report-error (format nil "Directory ~S does not exist."
                                  str)
                          :entry widget))
          (progn (cd dir)
                 (gtk:entry-set-text widget (pwd))
                 (cmio-print cmio :message "cd ~A ok."
                             (pwd))))))
    (values)))

(defmethod cmio-create-page (cmio (source (eql :score)) notebook pos)
  (let ((label (gtk:label-new "Score"))
        (window (cmio-window cmio))
        (vbox (gtk:vbox-new nil 1))
        (widgets (list))
        hbox entry button)
    ;; widgets=(:events <e> :starts <e> :compose <b> :play <b> 
    ;;          :view <b> :import <b> :directory <e>)
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    ;; line1
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Events:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (cmio-show-widget-evalable cmio entry)
    (gtk:entry-set-width-chars entry 40)
    (setq widgets (list :events entry))
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Starts:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (gtk:entry-set-width-chars entry 10)
    (nconc widgets (list :starts entry))
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)

    (let ((hbox (gtk:hbox-new nil 0))
          (imag (gtk:image-new-from-stock "gtk-convert"
                                          gtk:icon-size-small-toolbar))
          (labl (gtk:label-new "Compose")))
      (gtk:label-set-text-with-mnemonic labl "_Compose")
      (setq button (gtk:button-new ))
      (gtk:container-add button hbox)
      (gtk:widget-show hbox)
      (gtk:box-pack-start hbox imag nil nil 0)
      (gtk:widget-show imag)
      (gtk:box-pack-start hbox labl nil nil 0)
      (gtk:widget-show labl))
    (nconc widgets (list :compose button))
    (gtk:box-pack-end hbox button  nil nil 0)
    (gtk:widget-show button)
    (g:signal-connect button "clicked" (g:callback cmio_write)
                      window)
    ;; line 2
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)

    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Output:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq button (gtk:button-new-with-label "Play"))
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (g:object-set-data button "id" (gtk:string->cstring (string :play)))
    (g:signal-connect button "clicked" (g:callback cmio_score_handler)
                      window)
    (setq button (gtk:button-new-with-label "View"))
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (g:object-set-data button "id" (gtk:string->cstring (string :view)))
    (g:signal-connect button "clicked" (g:callback cmio_score_handler)
                      window)
    (setq button (gtk:button-new-with-label "Import"))
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (g:object-set-data button "id" (gtk:string->cstring (string :import)))
    (g:signal-connect button "clicked" (g:callback cmio_score_handler)
                      window)
    (setq label (gtk:label-new "Path:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (gtk:box-pack-start hbox entry nil nil 0) 
    (nconc widgets (list :path entry))
    (gtk:entry-set-text entry (pwd))
    (gtk:entry-set-width-chars entry 31)
    (gtk:widget-show entry)  
    (g:signal-connect entry "activate" (g:callback cmio_cd)
                      window)
    (cmio-set-page-widgets cmio :score widgets)
    (values)))

; (cmio)

(defmethod cmio-page-data (cmio (source (eql :score)) &optional op)
  ;; widgets: [events] [starts] [file] [cd] (compose)
  (let ((widgets (cmio-page-widgets cmio ':score))
        (data (list source))
        (test nil))
    (compose-read-buffer cmio :score data 
                         :events (getf widgets :events) )
    (setq test (get-data data ':events))
    (if (and (consp test) (eql (car test) ':error))
        (return-from cmio-page-data data))
    (when (eql op ':write)
      (multiple-value-bind (events error)
          (safeeval (get-data data ':events))
        (if (not error)
            (setf (get-data data ':events) events)
            (progn
              (setf (get-data data ':events)
                    (list ':error (eval-error-string error)
                          ':entry (first widgets)))
              (return-from cmio-page-data data)))))
    (compose-read-buffer cmio :score data
                         :starts (getf widgets :starts) T)
    (setq test (get-data data :starts))
    (when (and (eql op :write) (null test))
      (setf (get-data data :starts) 0))
    (unless (or (null test)
                (numberp test)
                (and (consp test)
                     (every #'numberp test)))
      (setf (get-data data :starts)
            (list ':error
                  (format nil "Starts: not number or list: ~A."
                          test)
                  :entry (getf widgets :starts))))
    data))

(defun compose-read-buffer (cmio page data prop buffer &optional opt?)
  ;; page is :score, :realtime or :eval
  ;; prop is propname of buffer, ie :events, :sprout :expr
  page cmio
  (let (check value error)
    (flet ((saferead (s)
             (multiple-value-bind (a b) (string->expr s )
               (if b (list ':error b ':entry buffer)
                   a))))
      ;; get the raw buffer string
      (add-entry-data data prop buffer :nullok opt? :READ NIL) ; Don't read yet
       ;; now check it
       (setq check (get-data data prop))
       (if (and opt? (null check))
           (return-from compose-read-buffer nil))
       (if (listp check) ; its already error: nullstring or read error
           (setq value (if (null check) ; nullstring
                           (list :error +se-nullstring+ 
                                 :entry buffer
                                 check)
                           check))
           (multiple-value-setq (value error) (safecall #'saferead check)))
       ;; value might be :error list already
       (if error ; lisp reading reported an error
           (setq value (list ':error (eval-error-string error) 
                             ':entry buffer)))
       (setf (get-data data prop) value)
       data)))

(defmethod cmio-set-page-data (cmio (page (eql :score)) args)
  (let ((widgets (cmio-page-widgets cmio page)))
    ;; widgets is (<eventsbuf> <startsbuf> <writebutton>
    (set-page-fields args :events (getf widgets :events)
                     :starts (getf widgets :starts))))

(defun imports->string (l &optional (e t))
  (if (not (listp l))
    (format nil "~@[#&~A~]" (object-name l))
    (loop with s = (if e "(list" "(") for o in l
       for n = (object-name o)
       if (not n) return ""
       else do (setq s (concatenate 'string s (format nil " #&~A" n)))
       finally (return (concatenate 'string s ")")))))

(defun cmio-output-file-data (cmio)
  ;; returns (:score :directory <dir> :score <dir> :type <typ>
  ;;                      :pathname <true> :stream <obj>)
  (let* ((widgets (cmio-page-widgets cmio :score))
         (pathbuf (getf widgets :path))
         (data (list :score))
         (test nil))
    (add-entry-data data :path pathbuf :nullok nil :read nil)
    (setq test (get-data data :path))
    (when (stringp test)                ; else error already
      (setq test (namestring (merge-pathnames test (pwd))))
      (unless (probe-file test)
        (setf (get-data data :path)
              (list :error (format nil "File ~S does not exist."
                                   test)
                    :entry pathbuf))
        (return-from cmio-output-file-data data))
      (let* ((ext (pathname-type test))
             (typ (car (find ext *cmio-file-types* 
                             :test (lambda (x y) (find x (cdr y) 
                                                       :test #'string=))))))
        (cond ((null ext)
               (setf test
                     (list ':error "Missing file name or extension."
                           ':entry pathbuf)))
              ((null typ)
               (setf test
                     (list ':error
                           (format nil "Unknown score type: .~a" ext)
                           ':entry pathbuf)))
              (t 
               (setf (get-data data :score-type) typ))))
      (setf (get-data data :path) test)
      (when (stringp test)              ; else error already
        (setf (get-data data :stream)
              (find-object (format nil "~a.~a"
                                   (pathname-name test)
                                   (pathname-type test))
                           nil))))
    data))

(defun cmio-view (cmio file type)
  (case type
    (:osc (dumposc file))
    (:midi (midi-file-print file))
    (:fomus (print :fomus))
    (t 
     (cmio-print cmio :warning "Don't know how to view ~S."
                 file))))

(defun cmio-import (cmio file type)
  (let* ((args (list file))
         (flag nil)
         seqs err?)
    (cond ((eql type ':midi)
           (let* ((data (cmio-page-data cmio :midi))
                  (tmpo (get-data data ':tempo))
                  (meta (get-data data ':meta-exclude))
                  (trks (get-data data ':exclude-tracks)))
             (with-data-errors-aborted (data)
               (nconc args  (list ':time-format 
                                  (get-data data ':time-format)
                                  ':keynum-format 
                                  (get-data data ':keynum-format)))
               (when tmpo (nconc args (list ':tempo tmpo args)))
               (when meta (nconc args (list ':meta-exclude meta)))
               (when meta (nconc args (list ':exclude-tracks trks)))
               (setq flag t))))
          ((member type '(:osc :sco :clm)) (setq flag t))
          (t
           (cmio-print cmio :warning "Don't now how to import ~S."
                       file))
           )
    (when flag
      (multiple-value-setq (seqs err?)
        (safeapply #'import-events args))
      (cond ((and (not err?) seqs)
             (let ((buf (getf (cmio-page-widgets cmio ':score) :events)))
               (gtk:entry-set-text buf (imports->string seqs))
               (cmio-print cmio :message "Import ~s ok." file)))
            (err?
             (report-error (eval-error-string err?)
                           :window (cmio-window cmio)))
            ((not seqs)
             (cmio-print cmio :warning "Import: nothing imported."))
            ))))

;;;
;;; :realtime page
;;;

(defmethod cmio-notebook-switch-page (cmio (notebook (eql :compose))
                                      (page (eql :realtime)))
  (let ((notrt '(:clm :cmn :csound :fomus :midi
                 :plotter :portmidi :sc :seq)))
    (set-notebook-page-sensitivity cmio :targets notrt nil)
    (set-notebook-page-sensitivity cmio :targets '(:midishare :portmidi :osc)
                                   t)))

(gtk:define-signal-handler rts_start_stop :void (widget data)
  (let* ((cmio (widget->object data))
         (subs (cmio-page-widgets cmio ':realtime))
         (imag (getf subs :image))
         (reset (getf subs :reset))
         )
    ;; actions based on whether or not rts is now open
    ;; NB: The click does NOT toggle the state, this does.
    (cond ((and (find ':pthreads *features*) (not (rts?)))
           ;; if we are closed, then open
           (rts )
           (gtk:widget-set-sensitive reset nil)
           (gtk:widget-set-sensitive (getf subs :sprout) t)
           (set-notebook-page-sensitivity cmio :compose '(:score) nil)
           (gtk:button-set-label widget "Stop")
           (gtk:image-set-from-stock imag "gtk-yes"
                                     gtk:icon-size-small-toolbar)
           (gtk:widget-show imag))
          (t
           ;; if we are open then close: sensitize reset button and Start page
           (rts-stop)
           (gtk:widget-set-sensitive reset t)
           (gtk:widget-set-sensitive (getf subs :sprout) nil)
           (set-notebook-page-sensitivity cmio :compose '(:score) t)
           (gtk:button-set-label widget "Start")
           (gtk:image-set-from-stock imag "gtk-no"
                                     gtk:icon-size-small-toolbar)
           (gtk:widget-show imag)))))

; (cmio)

(gtk:define-signal-handler realtime_compose :void (widget window)
  ;; widget is write button on the RTS page.
  widget
  (let* ((cmio (widget->object window))
         (tpage (cmio-current-target-page cmio))
         (stream nil))
    (if (cmio-page-can-do? cmio tpage ':realtime)
        (if (target-system-ready? tpage)
            (if (setq stream (cmio-page-prop cmio tpage :open))
                (if (and (find ':pthreads *features*) (rts?))
                    (let ((edata (cmio-page-data cmio ':realtime ':write))
                          (stuff nil)
                          err res)
                      (with-data-errors-aborted (edata)
                        (cmio-clear-message cmio)
                        ;; HACK!!!
                        (setq *out* stream)
                        (setq stuff (get-data edata :sprout))
                                        ;(print (list :sprout-> ))
                        (multiple-value-setq (res err)
                          (safecall #'sprout stuff))
                        res
                        (if err
                            (report-error (eval-error-string err)
                                          :window window)
                            (cmio-print cmio :message "Sprout: ok")
                            )))
                    (cmio-print cmio :warning "Realtime: RTS is not runing."))
                (cmio-print cmio :warning "~@(~A~) stream not open."
                            tpage))
            (cmio-print cmio :warning "Realtime: ~@(~A~) system is not ready."
                        tpage))
        (cmio-print cmio :warning
                    "Realtime: ~@(~A~) system does not support realtime output."
                    tpage) )
    (values)))

; (cmio)

(defmethod cmio-create-page (cmio (target (eql :realtime)) notebook pos)
  ;; widgets are (RTS: <image> (open) (reset) Sprout: [entry] (compose) )
  (let (label vbox hbox
        (window (cmio-window cmio))
        (widgets (list))
        (rts? (and (find ':pthreads *features*) (rts?)))
        (entry nil)
        (image nil)
        (button nil))
    ;; widgets=(<RTS:> <image> (start) (reset) <Sprout:> [entry] (Compose))
    (when (not (find ':rts *features*))
      (stub-notebook-page cmio notebook "Realtime" pos)
      (return-from cmio-create-page nil))
    (setq label (gtk:label-new "Realtime"))
    (setq vbox (gtk:vbox-new nil 1))
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    (gtk:container-set-border-width vbox %bw)
    ;; line 1
    (gtk:box-pack-start vbox hbox nil T 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Sprout:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (gtk:widget-set-sensitive entry
			      (and (find ':pthreads *features*) (rts?)))
    (cmio-show-widget-evalable cmio entry)
    (setq widgets (list :sprout entry))
    (gtk:box-pack-start hbox entry t t 0)
    (gtk:widget-show entry)
    (setq button (make-iconic-button "gtk-convert" "Compose"))
    (gtk:box-pack-end hbox button nil nil 0)
    (gtk:widget-show button)
    (nconc widgets (list :compose button))
    (g:signal-connect button "clicked" (g:callback realtime_compose)
                      window)
    ;; line 2
    (setq hbox (gtk:hbox-new nil %sw))
    ;(gtk:box-pack-start vbox hbox nil T 0) ; hbox should fill its area
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "RTS:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq image (gtk:image-new-from-stock (if rts? "gtk-yes" "gtk-no")
                                          gtk:icon-size-small-toolbar))
    (gtk:box-pack-start hbox image nil nil 0)
    (nconc widgets (list :image image))
    (gtk:widget-show image)
    (setq button (gtk:button-new-with-label (if rts? "Stop " "Start")))
    (g:signal-connect button "clicked" (g:callback rts_start_stop)
                      window)
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (nconc widgets (list :start button))
    (setq button (gtk:button-new-with-label "Pause"))
    (gtk:widget-set-sensitive button nil)
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (nconc widgets (list :pause button))
    (setq button (gtk:button-new-with-label "Flush"))
    (gtk:widget-set-sensitive button nil)
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (nconc widgets (list :flush button))
    (setq button (gtk:button-new-with-label "Reset"))
    (gtk:widget-set-sensitive button (and (find ':pthreads *features*)
					  (not (rts?))))
    (nconc widgets (list :reset button))
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (cmio-set-page-widgets cmio ':realtime widgets)))

;(cmio)

(defmethod cmio-page-data (cmio (source (eql :realtime)) &optional op)
  ;; widgets is ( <image> (open) (reset) Sprout: [entry] (compose) )
  ;; since these two buffers hold random, unstructured data, users
  ;; will make so many typos and mistakes that we will both read and
  ;; (later) evaluate the data without errors, then check and report
  ;; any errors we find the usual way
  ;; widgets=(<RTS:> <image> (start) (reset) <Sprout:> [entry] (Compose))
  op
  (let* ((widgets (cmio-page-widgets cmio ':realtime))
         (sprout (getf widgets :sprout))
         (data (list source))
         (test nil))
    (compose-read-buffer cmio :realtime data :sprout sprout )
    (setq test (get-data data :sprout))
    (when (eql op :write)
      (unless (and (consp test)
                   (eql (car test) :error))
        (multiple-value-bind (events error) (safeeval test)
          (setf (get-data data :sprout)
                (if error
                    (list :error (eval-error-string error)
                          :entry sprout)
                    events)))))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :realtime)) args)
  (let ((widgets (cmio-page-widgets cmio page)))
    ;; widgets is (<eventsbuf> <startsbuf> <writebutton>
    (set-page-fields args :sprout (getf widgets :sprout))))

;;;
;;; CLM page
;;;

(gtk:define-signal-handler clm_score_group :void (widget data)
  (set-subgroup-sensitivity data ':clm ':score
                            (gtk:toggle-button-get-active widget)))

(gtk:define-signal-handler clm_audio_group :void (widget data)
  (set-subgroup-sensitivity data ':clm ':output
                            (gtk:toggle-button-get-active widget)))

(gtk:define-signal-handler clm_subgroup :void (widget data)
  (let ((id (widget-get-id widget)))
    (set-subgroup-sensitivity data ':clm id
                              (gtk:toggle-button-get-active widget))))

(defmethod cmio-create-page (cmio (target (eql :clm)) notebook pos)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "CLM"))
        (widgets (list))
        (vbox (gtk:vbox-new nil %sw))
        tops table hbox entry spin check data)
    (gtk:widget-show vbox)
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show label)
    (when (not (target-system-ready? :clm))
      (system-setup-page cmio :clm
                         "Common Lisp Music is not loaded."
                         "Load CLM" vbox 
                         (function  ensure-clm/cmn/fomus)
                         t t)
      (return-from cmio-create-page nil))
    (setq table (gtk:table-new 5 3 nil))
    (setq tops (logior gtk:fill gtk:expand))
    (gtk:box-pack-start vbox table nil nil 0)
    (gtk:widget-show label)
    (gtk:widget-show table)
    (gtk:table-set-row-spacings table %rs)
    (setq check (gtk:check-button-new))
    (setq data (list check))
    (gtk:table-attach table check 0 1 0 1   0 0 0 0 )
    (gtk:widget-show check) 
    (g:object-set-data check "id"
                       (gtk:string->cstring (string :score)))
    (g:signal-connect check "toggled" (g:callback clm_score_group)
                      window)
    (setq label (gtk:label-new "Score:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 0 1   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 0 1   tops 0 0 0)
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 32)
    (gtk:entry-set-text entry "test.clm")
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Versioning"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil 0)
    (gtk:widget-show check) 

    ;; (x_ScoreFile label entry x_versions (import))
    (setq widgets (list* :score (nreverse data) widgets))
    ;; Audio group
    (setq check (gtk:check-button-new))
    (setq data (list check))
    (gtk:toggle-button-set-active check t)
    (gtk:table-attach table check 0 1 1 2  0 0 0 0 )
    (gtk:widget-show check) 
    (g:object-set-data check "id"
                       (gtk:string->cstring (string :audio-file)))
    (g:signal-connect check "toggled" (g:callback clm_audio_group) window)
    (setq label (gtk:label-new "Output:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 1 2  0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 1 2  tops 0 0 0)
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-text entry (if (boundp 'clm:*clm-file-name*)
                                clm:*clm-file-name* "test.aiff"))
    (gtk:entry-set-width-chars entry 32)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Play"))
    (setq data (list* check data))
    (gtk:toggle-button-set-active check t)
    (gtk:box-pack-start hbox check nil nil 0)
    (gtk:widget-show check) 
    (setq label (gtk:label-new "Srate:"))
    (setq data (list* label data))
    (gtk:box-pack-start hbox label nil nil 0)   
    (gtk:widget-show label) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-text entry (format nil "~D"
                                      (if (boundp 'clm:*clm-srate*)
                                        clm:*clm-srate* 22050)))
    (gtk:entry-set-width-chars entry 6)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Channels:"))
    (setq data (list* label data))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label) 
    (setq spin (gtk:spin-button-new-with-range 
                1 9 (if (boundp 'clm:*clm-channels*)
                      clm:*clm-channels* 1)))
    (setq data (list* spin data))
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin nil nil 0)
    (gtk:widget-show spin) 
    ;; line 3
    (setq label (gtk:label-new "Scaled to:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 2 3   0 0 0 0 )
    (gtk:widget-show label) 
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 2 3   tops 0 0 0)
    (gtk:widget-show hbox) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 5)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Scaled by:"))
    (setq data (list* label data))
    (gtk:box-pack-start hbox label nil nil 0)   
    (gtk:widget-show label) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 5)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Clipped"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil 0)   
    (gtk:widget-show check) 
    (setq check (gtk:check-button-new-with-label "Statistics"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil 0)   
    (gtk:widget-show check) 
    (setq check (gtk:check-button-new-with-label "Verbose"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil 0)   
    (gtk:widget-show check) 
    ;; line 4 (comment)
    (setq label (gtk:label-new "Comment:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 3 4   0 0 0 0 )
    (gtk:widget-show label) 
    (setq hbox (gtk:hbox-new nil 5))
    (gtk:table-attach table hbox 2 3 3 4  tops 0 0 0)
    (gtk:widget-show hbox) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    ;; line 5 (reverb)
    (setq label (gtk:label-new "Reverb:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 4 5   0 0 0 0 )
    (gtk:widget-show label) 
    (setq hbox (gtk:hbox-new nil 5))
    (gtk:table-attach table hbox 2 3 4 5  tops 0 0 0)
    (gtk:widget-show hbox) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 8)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Decay time:"))
    (setq data (list* label data))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 5)
    ;;(gtk:entry-set-text entry "2.5")
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Reverb data:"))
    (setq data (list* LABEL data))
    (gtk:box-pack-start hbox label nil nil 0)   ; t t
    (gtk:widget-show label) 
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 16)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq widgets (list* :output (nreverse data) widgets))
    (cmio-set-page-widgets cmio ':clm widgets)
    (set-subgroup-sensitivity window ':clm ':score nil)
    ;;(pprint (cmio-page-widgets cmio ':clm))
    (values)))

(defmethod cmio-page-data (cmio (page (eql :clm)) &optional op)
  op
  (let ((widgets (cmio-page-widgets cmio page))
        (data (list page))
        test group)
    ;; widgets=
    ;; (:score (<c> <l> <e> <c>)
    ;;  :output (<c> <l> <e> <c> <l> <e> <l> <s> 
    ;;               <l> <e> <l> <e> <c> <c> <c>
    ;;               <l> <e>
    ;;               <l> <e> <l> <e> <l> <e>  )
    (setq group (getf widgets ':score))
    (when (or (null op)
              (gtk:toggle-button-get-active (first group)))
      (add-entry-data data :score (third group) :nullok nil 
                      :read nil)
      (add-check-data data :versioning (fourth group)))
    (setq test (get-data data :score))
    (when (stringp test)
      (unless (equal (pathname-type test) "clm")
        (setf (get-data data :score)
              (list :error "File type not .clm" :entry (first group)))))
    (setq group (getf widgets ':output))
    (when (or (null op)
              (gtk:toggle-button-get-active (first group)))
      (add-entry-data data :output (third group) :nullok nil 
                      :read nil)
      (setq test (get-data data :output))
      (unless (find (pathname-type test) '("aiff" "wav") :test #'equal)
        (setf (get-data data :output)
              (list :error "File type not .aiff or .wav" 
                    :entry (first group))))
      (add-check-data data :play (fourth group))
      (add-entry-data data :srate (sixth group) :test #'numberp)
      (add-spin-data data :channels (eighth group) :result #'floor)
      (add-entry-data data :scaled-to (ELT group 9) :test #'numberp)
      (add-entry-data data :scaled-by (ELT group 11) :test #'numberp)
      (add-check-data data :clipped (ELT group 12))
      (add-check-data data :statistics (ELT group 13))
      (add-check-data data :verbose (ELT group 14))
      (add-entry-data data :comment (ELT group 16) :read nil) ; comment
      (add-entry-data data :reverb (ELT group 18) :test #'symbolp)
      (add-entry-data data :decay-time (ELT group 20) :test #'numberp)
      (add-entry-data data :reverb-data (ELT group 22) :test #'listp)
      )
    ;; trying to write with score and audio groups deactivated...
    (when (and (eql op ':write) (null (cdr data)))
      (setf (cdr data)
            (list :score
                  (list :error "Missing score or audio file." ))))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :clm)) args)
  (let ((widg1 (cmio-page-widgets cmio page ':score))
        (widg2 (cmio-page-widgets cmio page ':output)))
    ;; widgets=
    ;; (:score (<c> <l> <e> <c>)
    ;;  :output (<c> <l> <e> <c> <l> <e> <l> <s> 
    ;;               <l> <e> <l> <e> <c> <c> <c>
    ;;               <l> <e>
    ;;               <l> <e> <l> <e> <l> <e>  )
    (set-page-fields args :score (elt widg1 2)
                      :versioning (cons :check (elt widg1 3))
                      :output (elt widg2 2)
                      :play (cons :check (elt widg2 3))
                      :srate (elt widg2 5)
                      :channels (cons :spin (elt widg2 7))
                      :scaled-to (elt widg2 9) :scaled-by (elt widg2 11)
                      :clipped (cons :check (elt widg2 12)) 
                      :statistics (cons :check (elt widg2 13))
                      :verbose (cons :check (elt widg2 14))
                      :comment (elt widg2 16) :reverb (elt widg2 18) 
                      :decay-time (elt widg2 20)
                      :reverb-data (elt widg2 22))))

(defmethod cmio-ensure-event-stream (cmio (target (eql :clm)) data)
  cmio
  (let* ((snd (get-data data ':output))
         (clm (get-data data ':score))
         ;; :play will be false unless audio AND button activated
         (args (list* :play (get-data data :play)
                      :versioning (get-data data :versioning)
                      ;; CAREFUL! :srate is first true audio arg in
                      ;; data.  this gathers just the true audio args
                      ;; into plist format.
                      (member ':srate data)))
         stream)
    (if (null clm)
        ;; writng sound file without .clm file.
        (setq stream (or (find-object snd nil)
                         (make-instance 'audio-file :name snd
                                        :trace-output nil)))
        (progn
          ;; we are writing a score file and maybe also a soundfile.
          ;; if the latter then use clm-load to generate it.
          (setq stream (or (find-object clm nil)
                           (make-instance 'clm-file :name clm)))
          (when snd (setq args (list* :output snd args)))))
    (values (apply #'init-io stream args) nil)))

;;;
;;; :CMN page
;;;

(gtk:define-signal-handler rem_staffing_row :void (widget tool)
  ;; widget is remove button, tool is window
  (let* (;; car of data is vbox holding rows.
         ;; car of each row is hbox containing row widgets
         (cmio (widget->object tool))
         (data (cmio-page-widgets cmio ':cmn ':staffing))
         (rows (cdr data))
         ;; return row holding our widget. Check pointer EQness in CMU!
         ;;(delr (OR (find widget rows :test #'member)
         ;;          (error "Fixme! Can't find widget in row!")))
         delr )
    ;; find the row that contains our remove button
    ;; row is (<hbox> [id] [name] [meter] <clef> {+} {-}) 
    (setq delr
          (loop with me = (ptr->int widget)
                for row in rows
                for it = (SEVENTH row)
                when (equal me (ptr->int it))
                return row))
    (unless delr
      (error "Fixme! Can't find widget in row!"))
    (when (cdr rows) ;; never delete a single remaining row
      (setf (cdr data)
            (loop for r in rows unless (eq r delr) collect r))
      ;; car of row is row's hbox
      (gtk:widget-destroy (car delr)))
    (values)))

(gtk:define-signal-handler add_staffing_row :void (widget tool)
  ;; widget is button, tool is window ; vbox to addto
  widget
  (add-staffing-row tool)
  (values))

(defparameter *cmn-clefs* '(:both :treble :bass :alto :tenor))

(defun add-staffing-row (window )
  (let* ((cmio  (widget->object window))
         (rows (cmio-page-widgets cmio ':cmn ':staffing))
         (vbox (car rows))  ;; (<vbox> {row}+)
         (hbox (gtk:hbox-new nil %sw))
         (hpad 0)
         data label entry menu)
    ;; row is (<hbox> [id] [name] [meter] <clef> {+} {-})
    (setq data (list hbox))
    (gtk:box-pack-start vbox hbox nil nil hpad)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Id:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq data (list* entry data))
    (gtk:widget-show entry)
    (gtk:entry-set-width-chars entry 3)
    (gtk:entry-set-text entry (format nil "~D" (length (cdr rows))))
    (gtk:box-pack-start hbox entry nil nil hpad)    
    (setq label (gtk:label-new "Staff name:"))
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))    
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-width-chars entry 12)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Meter:"))
    (gtk:box-pack-start hbox label nil nil hpad)    
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))    
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-text entry (if (cdr rows) 
                                (gtk:entry-get-text
                                 (FOURTH (car (last (cdr rows)))))
                                "4/4"))
    (gtk:entry-set-width-chars entry 5)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Clef:"))
    ;(setq data (list* label data))
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    ;(setq menu (create-option-menu '("Both" "Treble" "Bass" "Alto" "Tenor" )))
    (setq menu (create-option-menu (loop for c in *cmn-clefs*
                                      collect (format nil "~@(~A~)" c))))
    (setq data (list* menu data))
    (gtk:box-pack-start hbox menu nil nil hpad)
    (gtk:widget-show menu)
    ;; add + and - control buttons at end of row
    (let ((box (gtk:hbox-new t 0))
          add rem)
      (gtk:box-pack-end hbox box nil nil 0)
      (gtk:widget-show box)
      (setq add (gtk:button-new-with-label "+"))
      (setq data (list* add data))
      (g:signal-connect add "clicked" (g:callback add_staffing_row)
                        window)
      (gtk:box-pack-start box add t t 0)
      (gtk:widget-show add)
      (setq rem (gtk:button-new-with-label "-"))
      (setq data (list* rem data))
      (g:signal-connect rem "clicked" (g:callback rem_staffing_row)
                        window)
      (gtk:box-pack-start box rem t t 0)
      (gtk:widget-show rem))
    (nconc rows (list (nreverse data)))))

(defmethod cmio-create-page (cmio (target (eql :cmn)) notebook pos)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "CMN"))
        (widgets (list))
        (vbox (gtk:vbox-new nil 0))
        (hpad 0)
        tops table hbox entry frame check data spin )
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    ;; install setup page if CMN is not loaded
    (when (not (target-system-ready? :cmn))
      (system-setup-page cmio :cmn
                         "Common Music Notation is not loaded."
                         "Load CMN" vbox (function ensure-clm/cmn/fomus)
                         t t)
      (return-from cmio-create-page nil))
    ;; else install real page
    (setq tops (logior gtk:fill gtk:expand))
    (setq table (gtk:table-new 3 2 nil))
    (gtk:table-set-col-spacings table %sw)
    (gtk:widget-show label)
    (gtk:widget-show vbox)
    (gtk:box-pack-start vbox table nil nil 0)
    (gtk:widget-show table)
    ;; line1
    (setq label (gtk:label-new "Score:"))
    (cmio-show-widget-required cmio label)
    (gtk:table-attach table label 0 1 0 1   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil 4))
    (gtk:table-attach table hbox  1 2 0 1   tops 0 0 0)
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))    
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-text entry "test.eps")
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Versioning"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check)
    (setq check (gtk:check-button-new-with-label "View"))
    (gtk:toggle-button-set-active check t)
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check)
 
    (setq check (gtk:check-button-new-with-label "One file"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check) 
    (setq widgets (list* :score (nreverse data) widgets))
    ;; line 2
    (setq data nil)
    (setq label (gtk:label-new "Title:"))
    (gtk:table-attach table label 0 1 1 2   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox  1 2 1 2  tops 0 0 0)
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))    
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-text entry "HiHo!")
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Score size:"))
    (gtk:box-pack-start hbox label nil nil hpad)    
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))    
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-text entry "40")
    (gtk:entry-set-width-chars entry 3)
    (gtk:widget-show entry)
    (setq widgets (list* :options (nreverse data) widgets))
    ;; line 3
    (setq data nil)
    (setq label (gtk:label-new "Metronome:"))
    (gtk:table-attach table label 0 1 2 3   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox  1 2 2 3  tops 0 0 0)
    (gtk:widget-show hbox)
    (setq spin (gtk:spin-button-new-with-range 30 400 1))
    (gtk:spin-button-set-digits spin 0)
    (gtk:spin-button-set-value spin 60)
    (gtk:box-pack-start hbox spin nil nil hpad)
    (gtk:widget-show spin)
    (setq data (list* spin data))
    (setq check (gtk:check-button-new-with-label "Exact rhythms"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check) 
    (setq widgets (list* :timeline (nreverse data) widgets))
    (setq frame (gtk:frame-new "Staffing"))
    (gtk:box-pack-start vbox frame nil nil 0)
    (gtk:widget-show frame)
    ;; add vbox to hod staffing rows
    (let ((box (gtk:vbox-new nil 0)))
      (gtk:container-add frame box)
      (gtk:container-set-border-width box %bw)
      (gtk:widget-show box)
      (setq widgets (list* :staffing (list box) widgets)))
    (cmio-set-page-widgets cmio ':cmn widgets)
    (add-staffing-row window)
    (values)))

(defun parse-time-signature (str)
  (if (and (eql (elt str 0) #\()
           (eql (string-readable? str) 1))
      (let ((l (string-read str)))
        (if (and (= (length l) 2)
                 (numberp (car l))
                 (numberp (cadr l)))
            l
            (values str +se-incorrect+)))
      (let ((del (position #\/ str)))
        (if (not del)
            (values str +se-incorrect+)
            (let ((n (parse-integer str :end del :junk-allowed t)))
              (if (not n)
                  (values str +se-incorrect+)
                  (let ((d (parse-integer str :start (+ del 1)
                                          :junk-allowed t)))
                    (if d (list n d)
                        (values str +se-incorrect+)))))))))

(defmethod cmio-page-data (cmio (page (eql :cmn)) &optional op)
  op
  (let ((widgets (cmio-page-widgets cmio page))
        (data (list page))
        (staffs (list))
        (test nil)
        group)
    ;; widgets=
    ;; (:score (<e> <c> <c> <c>) :options (<e> <e>)
    ;;  :timeline (<s> <c>) :staffing (<vbox> ROW+ ))
    ;; each row is (<hbox> [id] [name] [meter] <clef> {+} {-})
    (setq group (getf widgets ':score))
    (add-entry-data data :score (first group) :nullok nil :read nil)
    (setq test (get-data data :score))
    (when (and (stringp test)
               (not (member (pathname-type test) '("eps" "cmn")
                            :test #'equal)))
      (setf (get-data data :score)
            (list :error "File type not .cmn or .eps: ~s"
                  (or (pathname-type test) ""))))
    (add-check-data data :versioning (second group))
    (add-check-data data :view (third group))
    (add-check-data data :all-output-in-one-file (fourth group))
    ;; score options
    (setq group (getf widgets ':options))
    (add-entry-data data :title (first group) :read nil)
    (add-entry-data data :size (second group) :test #'numberp)
    (setq group (getf widgets ':timeline))
    (add-spin-data data :metronome (first group) :result #'floor )
    (add-check-data data :exact (second group))
    (setq widgets (CDR (getf widgets ':staffing)))
    (error-block
     (dolist (row widgets)
       (let ((staff (list :staff)))
         (add-entry-data staff :id (second row) :nullok nil)
         (add-entry-data staff :name (third row) :read nil)
         (add-entry-data staff :meter (fourth row)
                         :read #'parse-time-signature)
         (add-menu-data staff :clef (fifth row) *cmn-clefs*)
         ;; hack sets entire staffs list to first error value so
         ;; that abort-error can discover it in top-level of the
         ;; return list. = (:staff {:prop value}+)
         (loop for x in (cddr staff) by #'cddr
            when (and (consp x) (eql (car x) ':error))
            do (progn (setq staffs x)
                      (error-abort)))
         ;; set staff to normal staffing format (<id> :name ...)
         (setq staffs (nconc staffs (list (CDDR staff))))
         )))
    (nconc data (list :staffing staffs))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :cmn)) args)
  (let ((widg1 (cmio-page-widgets cmio page ':score))
        (widg2 (cmio-page-widgets cmio page ':options))
        (widg3 (cmio-page-widgets cmio page ':timeline))
        (staff (second (member ':staffing args))))
    ;; widgets is
    ;; (:score (<e> <c> <c> <c>) :options (<e> <e>)
    ;;  :timeline (<s> <c>) :staffing (<vbox> ROW+ ))
    ;; each row is (<hbox> [id] [name] [meter] <clef> {+} {-})
    (set-page-fields args :score (elt widg1 0)
                      :versioning (cons :check (elt widg1 1))
                      :view  (cons :check (elt widg1 2))
                      :all-output-in-one-file (cons :check (elt widg1 3))
                      :title (elt widg2 0) :size (elt widg2 1)
                      :metronome (cons ':spin (elt widg3 0))
                      :exact (cons ':check (elt widg3 1)))
    (when staff
      (let ((wind (cmio-window cmio))
            (rows nil))
        ;; add more rows if more than 1 staff
        (dotimes (i (- (length staff) 1)) (add-staffing-row wind))
        (setq rows (CDR (cmio-page-widgets cmio page ':staffing)))
        (loop for r in rows
           for s in staff
           when (and (consp s) (oddp (length s)))
           do (let ((i (pop s))
                    (n (getf s ':name))
                    (m (getf s ':meter))
                    (c (getf s ':clef)))
                (when i (gtk:entry-set-text (second r) (format nil "~A" i)))
                (when n (gtk:entry-set-text (third r) (format nil "~A" n)))
                (when m (gtk:entry-set-text (fourth r) (format nil "~A" m)))
                (when c
                  (gtk:option-menu-set-history (fifth r)
                                               (position c *cmn-clefs*)))))))))

(defmethod cmio-ensure-event-stream (cmio (target (eql :cmn)) data)
  cmio
  (let* ((file (get-data data ':score))
         (args (list*
                :versioning (get-data data :versioning)

                :exact-rhythms (get-data data ':exact)
                :staffing (get-data data ':staffing)
                (loop for arg in '(:title :all-output-in-one-file :size
                                   :metronome)
                   collect arg collect (get-data data arg)))))
      (let ((stream (or (find-object file nil)
                        (make-instance 'cmn-file :name file))))
        (apply #'init-io stream args))))

;;;
;;; :Csound page
;;;

(gtk:define-signal-handler csound_command_group :void (widget data)
  (set-subgroup-sensitivity data ':csound ':args
                            (gtk:toggle-button-get-active widget))
  (values))

(gtk:define-signal-handler sco_import :void (widget data)
  ;; widget is button data in main window
  widget
  (cmio-import (widget->object data) ':sco)
  (values))

(defmethod cmio-create-page (cmio (target (eql :csound)) notebook pos)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "Csound"))
        (table (gtk:table-new 5 3 nil))
        (widgets (list))
        (tops (logior gtk:fill gtk:expand))
        (hpad 0) ;5
        hbox data entry check)
    ;; widgets=(:score (<e> <c> <e>) :args (<c> <l> <e> <l> <e> )
    (gtk:container-set-border-width table %bw)
    (gtk:table-set-row-spacings table %rs)
    (gtk:notebook-insert-page notebook table label pos)
    (gtk:widget-show label)
    (gtk:widget-show table)
    ;; line 1 (score file)
    (setq label (gtk:label-new "Score:"))
    (cmio-show-widget-required cmio label)
    (gtk:table-attach table label 1 2 0 1   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 0 1  tops 0 0 0 )
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new ))
    (setq data (list entry))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-text entry "test.sco")
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Versioning"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check) 
    ;; line 2 Header -- this should be multiline..
    (setq label (gtk:label-new "Header:"))
    (gtk:table-attach table label 1 2 1 2   0 0 0 0 )
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq data (list* entry data))
    (gtk:table-attach table entry 2 3 1 2   tops 0 0 0)
    (gtk:widget-show entry)
    (setq widgets (list* :score (nreverse data) widgets))

    ;; line 3 Orchestra
    (setq check (gtk:check-button-new))
    (setq data (list check))
    (gtk:table-attach table check 0 1 2 3  0 0 0 0 )
    (g:signal-connect check "toggled" (g:callback csound_command_group)
                      window)
    (gtk:widget-show check)
    (setq label (gtk:label-new "Orchestra:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 2 3   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 2 3  tops 0 0 0 )
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new ))
    (setq data (list* entry data))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    ;; line 3 Sound
    (setq label (gtk:label-new "Output:"))
    (setq data (list* label data))
    (gtk:table-attach table label 1 2 3 4  0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 3 4  tops 0 0 0 )
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new ))
    (gtk:entry-set-text entry "devaudio")
    (setq data (list* entry data))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    (setq widgets (list* :args (nreverse data) widgets))
    (cmio-set-page-widgets cmio ':csound widgets)
    (set-subgroup-sensitivity window ':csound ':args nil)
    ;;(pprint (cmio-page-widgets cmio :csound))
    (values)))

(defmethod cmio-page-data (cmio (source (eql :csound)) &optional op)
  (let ((widgets (cmio-page-widgets cmio ':csound))
        (data (list source))
        group1 group2)
    ;; widgets=(:score (<e> <c> <e>) :args (<c> <l> <e> <l> <e> )
    (setq group1 (getf widgets ':score))
    (setq group2 (getf widgets ':args))
    (add-entry-data data :score (first group1)
                    :nullok nil :read nil)      
    (add-check-data data :versioning (second group1))
    (add-entry-data data :header (third group1) :read nil)
    ;; if op is :write then its an error to have the Orchestra
    ;; checked w/out an actual file name specified.
    (when (or (null op)
              (gtk:toggle-button-get-active (first group2)))
      (add-entry-data data :orchestra (third group2)
                      :nullok (not op) :read nil)
      (add-entry-data data :output (fifth group2) :read nil))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :csound)) args)
  (let ((widg1 (cmio-page-widgets cmio page ':score))
        (widg2 (cmio-page-widgets cmio page ':args)))
    ;; widgets=(:score (<e> <c> <e>) :args (<c> <l> <e> <l> <e> )
    (set-page-fields args :score (first widg1)
                     :versioning (cons ':check (second widg1))
                     :header (third widg1)
                     :orchestra (third widg2)
                     :output (fifth widg2))))

(defmethod cmio-ensure-event-stream (cmio (target (eql :csound)) data)
  cmio
  (let ((name (get-data data ':score))
        (orc (get-data data ':orchestra))
        stream)
    (setq stream (or (find-object name nil)
                     (make-instance <sco-file> :name name)))
    (values (init-io stream :header (get-data data ':header)
             :orchestra orc :output (get-data data ':output)
             :versioning (get-data data :versioning))
            nil)))
;;;
;;; Fomus page
;;;

(gtk:define-signal-handler rem_part_row :void (widget tool)
  ;; widget is remove button, tool is window
  (let* ((cmio (widget->object tool))
         (data (cmio-page-widgets cmio ':fomus ':parts))
         ;; car of data is vbox holding rows.
         (rows (cdr data))
         ;; find row containing this remove button
         ;; row is (<hbox> [id] [inst] [name]  (+) (-) ) 
         (delr
          (loop with me = (ptr->int widget)
                for row in rows
                for it = (SIXTH row)
                when (equal me (ptr->int it))
                return row)))
    ;; never delete the last remaining row
    (when (cdr rows)
      (setf (cdr data)
            (loop for r in rows unless (eq r delr) collect r))
      ;; car of row is row's hbox
      (gtk:widget-destroy (car delr)))
    (values)))

(gtk:define-signal-handler add_part_row :void (widget data)
  ;; widget is button, tool is window ; vbox to addto
  widget
  (add-part-row data))

(defun add-part-row (window )
  (let* ((cmio  (widget->object window))
         (rows (cmio-page-widgets cmio ':fomus ':parts))
         (vbox (car rows))  ;; (<vbox> {row}+)
         (hbox (gtk:hbox-new nil %sw))
         data label entry)
    ;; row is (<hbox> [id] [Instr] [name] {+} {-})
    (setq data (list hbox))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "PartId:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq data (list* entry data))
    (gtk:widget-show entry)
    (gtk:entry-set-width-chars entry 5)
    (gtk:entry-set-text entry (format nil "~D" (length (cdr rows))))
    (gtk:box-pack-start hbox entry nil nil 0)

    (setq label (gtk:label-new "Instr:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq data (list* entry data))
    (gtk:widget-show entry)
    (gtk:entry-set-width-chars entry 12)
    (gtk:entry-set-text entry "Piano")
    (gtk:box-pack-start hbox entry nil nil 0)

    (setq label (gtk:label-new "Name:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))    
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:entry-set-width-chars entry 12)
    (gtk:widget-show entry)
    ;; add + and - control buttons at end of row
    (let ((box (gtk:hbox-new t 0))
          add rem)
      (gtk:box-pack-end hbox box nil nil 0)
      (gtk:widget-show box)
      (setq add (gtk:button-new-with-label "+"))
      (setq data (list* add data))
      (g:signal-connect add "clicked" (g:callback add_part_row)
                        window)
      (gtk:box-pack-start box add t t 0)
      (gtk:widget-show add)
      (setq rem (gtk:button-new-with-label "-"))
      (setq data (list* rem data))
      (g:signal-connect rem "clicked" (g:callback rem_part_row)
                        window)
      (gtk:box-pack-start box rem t t 0)
      (gtk:widget-show rem))
    (nconc rows (list (nreverse data)))))

(defmethod cmio-create-page (cmio (target (eql :fomus)) notebook pos)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "Fomus"))
        (vbox (gtk:vbox-new nil 0))
        (widgets (list))
        hbox entry frame check spin menu)
    ;; widgets=
    ;; (:score <e> :versioning <s> :view <c> :midi <c> :data <c>
    ;;  :quartertones <c> :timesig <e> :beat-division <m> :max-tuplet <s>
    ;;  :quality <s> :parts ((<hbox> <e> <e> <e> <b> <b>) ...))
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    ;; install setup page if Fomus is not loaded
    (when (not (target-system-ready? :fomus))
      (system-setup-page cmio :fomus
                         "Fomus is not loaded."
                         "Load Fomus" vbox (function ensure-clm/cmn/fomus)
                         t nil)
      (return-from cmio-create-page nil))
    ;; else install real page
    ;; line1
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Score:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq widgets (nconc widgets (list ':score entry)))
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:entry-set-text entry "test.ly")
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Versioning"))
    (nconc widgets (list ':versioning check))
    (gtk:box-pack-start hbox check nil nil 0)
    (gtk:widget-show check)
    (setq check (gtk:check-button-new-with-label "View"))
    (gtk:toggle-button-set-active check t)
    (nconc widgets (list ':view check))
    (gtk:box-pack-start hbox check nil nil 0)
    (gtk:widget-show check)

;    (setq label (gtk:label-new "Backend:"))
;    (gtk:box-pack-start hbox label nil nil 0)
;    (gtk:widget-show label)

    (setq check (gtk:check-button-new-with-label "Midi"))
    (nconc widgets (list ':midi check))
    (gtk:box-pack-start hbox check nil nil 0)
    (gtk:widget-show check)

   (setq check (gtk:check-button-new-with-label "Data"))
    (nconc widgets (list ':data check))
    (gtk:box-pack-start hbox check nil nil 0)
    (gtk:widget-show check)

    ;; line2 Global frame
    ;; <qual> <verbe> <maxtup> _qtones [4/4] 
    (setq frame (gtk:frame-new "Global"))
    (gtk:box-pack-start vbox frame nil nil 0)
    (gtk:widget-show frame)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:container-add frame hbox)
    (gtk:container-set-border-width hbox %bw)
    (gtk:widget-show hbox)

    (setq check (gtk:check-button-new-with-label "Quartertones"))
    (nconc widgets (list ':quartertones check))
    (gtk:box-pack-start hbox check nil nil 0)
    (gtk:widget-show check)

    (setq label (gtk:label-new "Timesig:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (nconc widgets (list ':timesig entry))
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (gtk:entry-set-text entry "4/4")
    (gtk:entry-set-width-chars entry 5)

    (setq label (gtk:label-new "Beat-division:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq menu (create-option-menu '("1" "2" "4" "8" "16"
                                     "32" "64" "128")
                                   :select 4))
    (gtk:box-pack-start hbox menu nil nil 0)
    (gtk:widget-show menu)
    (nconc widgets (list ':beat-division menu))

    (setq label (gtk:label-new "Max-tuplet:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq spin (gtk:spin-button-new-with-range 0 13 1))
    (nconc widgets (list ':max-tuplet spin))
    (gtk:box-pack-start hbox spin nil nil 0)
    (gtk:widget-show spin)
    (gtk:spin-button-set-digits spin 0)
    (gtk:spin-button-set-value spin 7)

    (setq label (gtk:label-new "Quality:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq spin (gtk:spin-button-new-with-range 0 5 .1))
    (nconc widgets (list ':quality spin))
    (gtk:box-pack-start hbox spin nil nil 0)
    (gtk:widget-show spin)
    (gtk:spin-button-set-value spin 1)
    (setq frame (gtk:frame-new "Parts"))
    (gtk:box-pack-start vbox frame nil nil 0)
    (gtk:widget-show frame)
    (setf (cmio-page-prop cmio :fomus :instr)
          (fomus:get-instr-syms))
    ;; widgets need to be there before adding rows
    (cmio-set-page-widgets cmio ':fomus widgets)
    ;; add vbox to Parts frame to hold rows
    (setq vbox (gtk:vbox-new nil 0))
    (gtk:container-add frame vbox)
    (gtk:container-set-border-width vbox %bw)
    (gtk:widget-show vbox)
    ;; car of :part rows is vbox
    (nconc widgets (list ':parts (list vbox)))
    (add-part-row window)
    ))

(defmethod cmio-page-data (cmio (page (eql :fomus)) &optional op)
  op
  (let ((widgets (cmio-page-widgets cmio page))
        (instrs (cmio-page-prop cmio :fomus :instr))
        (data (list page))
        (parts (list)))
    ;; widgets=
    ;; (:score <e> :versioning <s> :view <c> :midi <c> :data <c>
    ;;  :quartertones <c> :timesig <e> :beat-division <m> :max-tuplet <s>
    ;;  :quality <s> :parts ((<hbox> <e> <e> <e> <b> <b>) ...))
    (add-entry-data data :score (getf widgets :score) :nullok nil :read nil)
    (data-check-file-type data :score (getf widgets :score) "ly" "xml")
    (add-check-data data :versioning (getf widgets :versioning))
    (add-check-data data :view (getf widgets :view))
    (add-check-data data :midi (getf widgets :midi))
    (add-check-data data :data (getf widgets :data) )
    (add-check-data data :quartertones (getf widgets :quartertones))
    (add-entry-data data :timesig (getf widgets :timesig)
                         :read #'parse-time-signature)
    (add-menu-data data :beat-division (getf widgets :beat-division)
                   '(1 2 4 8 16 32 64 128))
    (add-spin-data data :max-tuplet (getf widgets :max-tuplet)
                   :result #'floor)
    (add-spin-data data :quality (getf widgets :quality)
                   :result (lambda (x) (coerce x 'single-float)))
    (setq widgets (CDR (getf widgets ':parts)))
    (error-block
     (dolist (row widgets)
       (let ((part (list :parts)))
         (add-entry-data part :partid (second row) :nullok nil)
         (add-entry-data part :instr (third row) :nullok nil) 
         (setf (get-data part :instr)
               (or (find (get-data part :instr)
                         instrs :test #'string-equal :key #'symbol-name)
                   (list :error (format nil "No Fomus instrument named ~S."
                                        (gtk:entry-get-text (third row)))
                         :entry (third row))))
         (add-entry-data part :name (fourth row) :read nil :nullok t)
         ;; hack sets entire parts list to first error value so
         ;; that abort-error can discover it in top-level of the
         ;; return list. = (:parts {:prop value}+)
         (loop for x in (cdr part) by #'cddr
            when (and (consp x) (eql (car x) ':error))
            do (progn (setq parts x)
                      (error-abort)))
         ;; set parts to normal partsing format (<id> :name ...)
         (setq parts (nconc parts (list (CDR part))))
         )))
    (nconc data (list :parts parts))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :fomus)) args)
  (let ((widgets (cmio-page-widgets cmio page)))
    ;; widgets=
    ;; (:score <e> :versioning <s> :view <c> :midi <c> :data <c>
    ;;  :quartertones <c> :timesig <e> :beat-division <m> :max-tuplet <s>
    ;;  :quality <s> :parts ((<hbox> <e> <e> <e> <b> <b>) ...))
    (set-page-fields args :score (getf widgets :score)
                     :versioning (cons ':check (getf widgets :versioning))
                     :view (cons ':check (getf widgets :view))
                     :midi (cons ':check (getf widgets :midi))
                     :data (cons ':check (getf widgets :data))
                     :quartertones (cons ':check (getf widgets :quartertones))
                     :timesig (getf widgets :timesig)
                     :beat-division
                     (cons :menu (list (getf widgets :beat-division)
                                                      1 2 4 8 16 32 64 128))
                     :max-tuplet (cons :spin (getf widgets :max-tuplet))
                     :quality (cons :spin (getf widgets :quality)))))

(defmethod cmio-ensure-event-stream (cmio (target (eql :fomus)) data)
  cmio
  (let* ((name (get-data data ':score))
         (meter (get-data data ':timesig))
         (bends (list))
         (globs (list))
         (parts (loop for p in (get-data data ':parts)
                   collect
                   (apply #'make-instance 'fomus:part p)))
         (stream (or (find-object name nil)
                     (make-instance <fomus-file> :name name))))

    ;; see if we need to cobble up a :backend spec
    (when (get-data data :midi)
      (push (list :midi (format nil "~A.mid" name)) bends))
    (when (get-data data :data)
      (push (list :data (format nil "~A.fms" name)) bends))
    (when bends
      (push (list (if (equal (pathname-type name) "ly")
                      :lilypond
                      :xml)
                  name :view (get-data data ':view))
            bends))
    (when meter
      (setq globs (list (make-instance 'fomus:timesig
                                       :off 0 :time meter))))
    (values (init-io stream :parts parts :global globs
                     :versioning (get-data data :versioning)
                     :view (get-data data :view)
                     :quartertones (get-data data ':quartertones)
                     :quality (get-data data ':quality)
                     :beat-division (get-data data :beat-division)
                     :max-tuplet (get-data data :max-tuplet)
                     :backend bends)
            nil)))

;;;
;;; :MIDI page
;;;

(gtk:define-signal-handler midi_tempo_sensitivity :void (widget data)
  (let* ((active? (gtk:toggle-button-get-active widget))
         (cmio (widget->object data))
         (widgets (getf (cmio-page-widgets cmio ':midi ':importing)
                        ':time-format)))
    ;; widgets is (<r> <r> <l> <e>), set <l> and <e> insensitive
    (dolist (w (cddr widgets))
      (gtk:widget-set-sensitive w active?))
    (values)))

(gtk:define-signal-handler micro_divisions :void (widget data)
  ;; widget is radio button, data is spin button
  (let ((active? (gtk:toggle-button-get-active widget)))
    (gtk:widget-set-sensitive data active?)))

(defun add-microtuning (hbox)
  ;; Microtuning
  (let (label button spin data)
    (setq label (gtk:label-new "Microtuning:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq button (gtk:radio-button-new-with-label
                  (g:nullptr) "None"))
    (push button data)
    (gtk:toggle-button-set-active button t)
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (setq button (gtk:radio-button-new-with-label-from-widget 
                  button "Half-step divisions:"))
    (push button data)
    ;; have to create spin here for callback
    (setq spin (gtk:spin-button-new-with-range 2 16 1))
    (g:signal-connect button "toggled" (g:callback micro_divisions)
                      spin)
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    ;; spin created earlier
    (push spin data)
    (gtk:widget-set-sensitive spin nil)
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin nil nil 0)
    (gtk:widget-show spin)
    (setq button (gtk:radio-button-new-with-label-from-widget
                  button "Note by note"))
    (push button data)
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (nreverse data)))

(defun microtuning-data (widgets)
  (if (gtk:toggle-button-get-active (first widgets))
      (list)
      (if (gtk:toggle-button-get-active (second widgets))
          (let ((data (add-spin-data (list) :microtuning 
                                     (third widgets) :result #'floor)))
            (nconc data (list :microtuning t))))))

(defmethod cmio-create-page (cmio (target (eql :midi)) notebook pos)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "MIDI"))
        (vbox (gtk:vbox-new nil 0))
        (widgets1 (list))
        (widgets (list))
        (hpad 0)
        label2 hbox hbox2 data entry check button frame)

    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    ;; Microtuning
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq data (add-microtuning hbox))
    (setq widgets (list* :microtuning data widgets))
    ;; line 1 (score file)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Score:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)    
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq data (list entry))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-text entry "test.mid")
    (gtk:entry-set-width-chars entry 32)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Versioning"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check) 
    (setq check (gtk:check-button-new-with-label "Play"))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:toggle-button-set-active check t)
    (gtk:widget-show check) 
    (setq widgets1 (list* :score (nreverse data) widgets))
    ;; Importing frame
    (setq widgets (list))
    (setq hpad 0)
    (setq frame (gtk:frame-new "Importing"))
    (gtk:box-pack-start vbox frame nil nil 0)
    (gtk:widget-show frame)
    (setq vbox (gtk:vbox-new nil 0))
    (gtk:container-add frame vbox)
    (gtk:container-set-border-width vbox %bw)
    (gtk:widget-show vbox)
    ;; line 1 time and keynum format
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Time format:"))
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq button (gtk:radio-button-new-with-label
                  (g:nullptr) "Beats"))
    (setq data (list button))
    (g:signal-connect button "toggled" (g:callback midi_tempo_sensitivity)
                      window)
    (gtk:box-pack-start hbox button nil nil hpad)
    (gtk:widget-show button)
    (setq button (gtk:radio-button-new-with-label-from-widget 
                  button "Ticks"))
    (setq data (list* button data))
    (gtk:box-pack-start hbox button nil nil hpad)
    (gtk:widget-show button)
    (setq label (gtk:label-new "Override tempo:"))
    (setq data (list* label data))
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq data (list* entry data))
    (gtk:entry-set-width-chars entry 4) 
    (gtk:widget-show entry)
    ;;LABEL AND ENTRY ADDED NEXT LINE
    (setq widgets (list* ':time-format (nreverse data) widgets))    
    (setq hbox2 (gtk:hbox-new nil %sw))
    (gtk:box-pack-end hbox hbox2 nil nil 0)
    (gtk:widget-show hbox2)
    (setq label2 (gtk:label-new "Keynum format:"))
    (gtk:box-pack-start hbox2 label2 nil nil hpad)
    (gtk:widget-show label2)
    (setq button (gtk:radio-button-new-with-label
                  (g:nullptr) "Keynum"))
    (setq data (list button))
    (gtk:box-pack-start hbox2 button nil nil hpad)
    (gtk:widget-show button)
    (setq button (gtk:radio-button-new-with-label-from-widget 
                  button "Note"))
    (setq data (list* button data))
    (gtk:box-pack-start hbox2 button nil nil hpad)
    (gtk:widget-show button)
    (setq button (gtk:radio-button-new-with-label-from-widget 
                  button "Hertz"))
    (setq data (list* button data))
    (gtk:box-pack-start hbox2 button nil nil hpad)
    (gtk:widget-show button)
    (setq widgets (list* ':keynum-format (nreverse data) widgets))    
    ;; line 2
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:box-pack-start hbox entry nil nil hpad)
    (setq hbox2 (gtk:hbox-new nil 10))    
    (gtk:box-pack-end hbox hbox2 nil nil 0)
    (gtk:widget-show hbox2)
    (setq check (gtk:check-button-new-with-label "Exclude meta messages."))
    (setq widgets (list* :meta-exclude check widgets))
    (gtk:box-pack-start hbox2 check nil nil hpad)
    (gtk:widget-show check)
    (setq label (gtk:label-new "Exclude tracks:"))
    (gtk:box-pack-start hbox2 label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq widgets (list* :exclude-tracks entry widgets))
    (gtk:box-pack-start hbox2 entry nil nil hpad)
    (gtk:entry-set-width-chars entry 12) 
    (gtk:widget-show entry)
    (cmio-set-page-widgets cmio ':midi (append widgets1
                                               (list :importing widgets)))
    ;;(pprint (cmio-page-widgets cmio ':midi))
    (values)))

(defmethod cmio-page-data (cmio (target (eql :midi)) &optional op)
  (let ((data (list target))
        (widgets (cmio-page-widgets cmio ':midi))
        group)
    ;; widgets is:
    ;; x_Microtuning o_note-by-note o_step divisions {1}
    ;; (:score (<e> <c> <c>)
    ;;  :microtuning (<r> <r> <s> <r>)
    ;;  :importing (:exclude-tracks <e> :meta-exclude <c>
    ;;              :keynum-format (<r> <r> <r>)
    ;;              :time-format (<r> <r> <l> <e>)
    (setq group (getf widgets ':score))
    (add-entry-data data :score (first group) :nullok nil :read nil)    
    (add-check-data data :versioning (second group))
    (add-check-data data :play (third group))
    ;; microtuning group
    (setq group (getf widgets ':microtuning))
    (if (gtk:toggle-button-get-active (first group))
      (nconc data (list :microtuning nil))
      (if (gtk:toggle-button-get-active (second group))
        (add-spin-data data :microtuning (third group) 
                       :result #'floor)
        (nconc data (list :microtuning t))))
    (when (or (not op) (eql op :importing))
      (setq widgets (getf widgets ':importing))
      (setq group (getf widgets ':time-format))
      (add-radio-data data :time-format (first group) ':beats)
      (add-radio-data data :time-format (second group) ':ticks)
      (if (eql (getf (CDR data) ':time-format) ':beats)
        (add-entry-data data :override-tempo (fourth group)
                        :test #'numberp))
      (setq group (getf widgets ':keynum-format))
      (add-radio-data data :keynum-format (first group) ':keynum)
      (add-radio-data data :keynum-format (second group) ':note)
      (add-radio-data data :keynum-format (third group) ':hertz)
      (add-check-data data :meta-exclude
                      (getf widgets ':meta-exclude))
      (add-entry-data data :exclude-tracks (getf widgets ':exclude-tracks)
                      :multiok t))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :midi)) args)
  (let ((widg1 (cmio-page-widgets cmio page ':score))
        (widg2 (cmio-page-widgets cmio page ':microtuning))
        (widg3 (cmio-page-widgets cmio page ':importing)))
    ;; widgets is:
    ;; (:microtuning (<r> <r> <s> <r>)
    ;;  :score (<e> <c> <c>)
    ;;  :importing (:exclude-tracks <e> :meta-exclude <c>
    ;;              :keynum-format (<r> <r> <r>)
    ;;              :time-format (<r> <r> <l> <e>))
    (set-page-fields args
                      :score (first widg1)
                      :microtuning (list :radio
                                         :none (first widg2)
                                         :divisions (second widg2)
                                         :note-by-note (third widg2))
                      :exclude-tracks (getf widg3 :exclude-tracks)
                      :meta-exclude (cons :check (getf widg3 :meta-exclude))
                      :keynum-format
                      (let ((fmat (getf widg3 :keynum-format)))
                        (list :radio :keynum (first fmat)
                              :note (second fmat)
                              :hertz (third fmat)))
                      :override-tempo
                      (let ((fmat (getf widg3 :time-format)))
                        (fourth fmat))
                      :time-format
                      (let ((fmat (getf widg3 :time-format)))
                        (list :radio :beats (first fmat)
                              :ticks (second fmat))))))

(defmethod cmio-ensure-event-stream (cmio (target (eql :midi)) data)
  cmio
  (let ((name (get-data data ':score))
        stream error?)
    (setq stream (or (find-object name nil)
                     (make-instance <midi-file> :name name)))
    (init-io stream :channel-tuning (get-data data :microtuning)
             :versioning (get-data data ':versioning) )
    (values stream error?)))

;;;
;;; :MidiShare page
;;;

(defun midishare-stream-hook (cmio status)
  (let ((win (cmio-window cmio)))
    (case status
      (:open 
       (set-subgroup-sensitivity win :midishare :microtuning nil t)
       (if (typep (cmio-page-prop cmio :midishare :open) 'player-stream)
           (set-subgroup-sensitivity win :midishare :player t t)
           (set-subgroup-sensitivity win :midishare :player nil t)))
      (:close
       (set-subgroup-sensitivity win :midishare :microtuning t t)
       (set-subgroup-sensitivity win :midishare :player nil t)
       )
      ((:set :clear)
       ;(let ((spin (getf (cmio-page-widgets cmio :midishare :stream)
       ;:receive-rate)))
       ;  (gtk:widget-set-sensitive spin (eql status :clear)))
       ))))

(gtk:define-signal-handler player_control :void (widget data)
  ;; ugly hack to get keyword button id of widget
  (let ((id (widget-get-id widget)))
    (print id)
    DATA
    ;; (do-player data id)
    ;;(do-player data ':tempo (gtk:spin-button-get-value widget))
    ))

(defun do-player (window op &optional arg)
  (let* ((cmio (widget->object window))
         (data (list op))
;; FIX
         (buff (THIRD (getf (cmio-page-widgets cmio ':midishare)
                            ':player)))
         )
    (add-entry-data data :player buff :nullok nil :read nil)
    (with-data-errors-aborted (data)
      (let* ((name (get-data data ':player))
             (stream (find-object name)))
        (if (or (not stream)
                (not (typep stream 'player-stream)))
          (report-error (format nil "~@(~A~): no player named ~S."
                                op name) :entry buff)
          (ecase op
            (:play (player-play stream))
            (:stop (player-stop stream))
            (:continue (player-cont stream))
            (:tempo (player-set-tempo stream (floor arg)))))))))
            
(defmethod cmio-create-page (cmio (source (eql :midishare)) notebook pos)
  (let ((window (cmio-window cmio))
        (widgets (list))
        (subs (list))
        frame vbox label hbox spin button )
    ;; widgets=
    ;; (:microtuning (<r> <r> <s> <r>)
    ;;  :stream (:name <e> :icon <e> :open <b> :receiver <e> :set <b>
    ;;           :receive-rate <s>)
    ;;  :player (<l> <s> <l> <s> <b> <b> <b>))
    (when (not (target-system-ready? :midishare))
      (stub-notebook-page cmio notebook "Midishare" pos)
      (return-from cmio-create-page nil))

    (setq label (gtk:label-new "Midishare"))
    (setq vbox (gtk:vbox-new nil 0))
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    ;; LINE 1: Microtuning
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq subs (add-microtuning hbox))
    (setq widgets (list* :microtuning subs widgets))
    ;; LINE 2: Stream controls
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil %SW)
    (gtk:widget-show hbox)
    (setq subs (stream-controls cmio :midishare hbox 
                                "midi-port.ms"
                                #'midishare-stream-hook))
    (setq widgets (list* :stream subs widgets))
    ;; LINE 3 midi player controlos
    (setq subs (list))
    ;; Player frame
    (setq frame (gtk:frame-new "Player"))
    (gtk:box-pack-end vbox frame nil nil 0)
    (gtk:widget-show frame)
    (setq vbox (gtk:vbox-new nil 0))
    (gtk:container-add frame vbox)
    (gtk:container-set-border-width vbox %bw)
    (gtk:widget-show vbox)
    ;; Player controls:
    ;; ("track" <> "tempo" <> (play) (stop) (cont) )
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:container-add vbox hbox)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Track:"))
    (push label subs)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq spin (gtk:spin-button-new-with-range 0 7 1))
    (push spin subs)
    (gtk:widget-show spin)
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin nil nil 0)
    (setq label (gtk:label-new "Tempo:"))
    (push label subs)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq spin (gtk:spin-button-new-with-range 30 400 1))
    (push spin subs)
    (g:object-set-data spin "id" (gtk:string->cstring (string :tempo)))
    (gtk:spin-button-set-value spin 120)
    (gtk:widget-show spin)
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin nil nil 0)
    ;; connect spin after so value doesnt trigger
    (g:signal-connect spin "value_changed" (g:callback player_control)
                      window)
    (setq button (gtk:button-new-with-label "Play"))
    (g:object-set-data button "id" (gtk:string->cstring (string :play)))
    (push button subs)
    (g:signal-connect button "clicked" (g:callback player_control)
                      window)    
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (setq button (gtk:button-new-with-label "Stop"))
    (g:object-set-data button "id" (gtk:string->cstring (string :stop)))
    (push button subs)
    (g:signal-connect button "clicked" (g:callback player_control)
                      window) 
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    (setq button (gtk:button-new-with-label "Continue"))
    (g:object-set-data button "id" (gtk:string->cstring (string :continue)))
    (push button subs)
    (g:signal-connect button "clicked" (g:callback player_control)
                      window) 
    (gtk:box-pack-start hbox button nil nil 0)
    (gtk:widget-show button)
    ;; add widgets
    (setq widgets (list* :player (nreverse subs) widgets))
    (cmio-set-page-widgets cmio ':midishare widgets)
    (let ((str (find-object "midi-port.ms" nil)))
      (when str (cmio-set-page-data cmio :midishare str)))
    ))

(defmethod cmio-page-data (cmio (page (eql :midishare)) &optional op)
  op
  (let* ((widgets (cmio-page-widgets cmio page))
         (data (list page)))
    ;; widgets=
    ;; (:microtuning (<r> <r> <s> <r>)
    ;;  :stream (:name <e> :icon <e> :open <b> :receiver <e> :set <b>
    ;;           :receive-rate <s>)
    ;;  :player (<l> <s> <l> <s> <b> <b> <b>))
    (multiple-value-bind (resl err?)
        (stream-control-data (getf widgets ':stream)
                             '("ms" "mp"))
      (nconc data resl)
      ;; abort immediately if there any stream errors
      (if err? (return-from cmio-page-data data)))
    ;; maybe add player data. widgets are:
    ;; ("track" <> "tempo" <> (play) (stop) (cont) )
    (when (equal (pathname-type (getf (cdr data) :name)) "mp")
      (let ((group (getf widgets ':player)))
        (add-spin-data data :track (SECOND group) :result #'floor)
        (add-spin-data data :tempo (FOURTH group) :result #'floor)))
    ;; add any microtuning data
    (nconc data (microtuning-data (getf widgets ':microtuning)))
    data))

(defmethod cmio-stream-data (cmio (page (eql :midishare))  stream)
  cmio
  (list :name (object-name stream)
        :open? (io-open stream)
        :player? (typep stream 'player-stream)
        :stream stream))

(defmethod cmio-set-page-data (cmio (page (eql :midishare)) data)
  (if (not (listp data))
      (setq data (cmio-stream-data cmio page data)))
  (let* ((widgets (cmio-page-widgets cmio page))
         (streams (getf widgets ':stream))
         (micros (getf widgets ':microtuning))
         (player (getf widgets ':player)))
    (set-page-fields data
                     :microtuning (list :radio
                                        :none (first micros)
                                        :divisions (second micros)
                                        :note-by-note (third micros))
                     :track (cons ':spin (SECOND player))
                     :tempo (cons ':spin (FOURTH player)))
    (update-stream-status cmio streams
                          (if (getf data :open?) :open :close))))


(defmethod cmio-ensure-event-stream (cmio (target (eql :midishare)) data)
  cmio
  (let* ((name (get-data data :name))
         (stream (or (find-object name nil)
                     (make-instance (if (equal (pathname-type name) "mp")
                                        'player-stream
                                        'midishare-stream)
                                    :name name))))
    (when (typep stream <player-stream>)
      (setf (player-stream-play stream) (get-data data ':play))
      (setf (player-stream-tempo stream) (get-data data ':tempo)) )
    (values (init-io stream 
                     :receive-rate (get-data data ':receive-rate)
                     :channel-tuning (get-data data ':microtuning))
            nil)))

;;;
;;; OSC page
;;;

(gtk:define-signal-handler osc_radio :void (widget data)
  (let ((id (widget-get-id widget)))
    (osc-set-defaults (widget->object data) id)))

(defun osc-stream-hook (cmio state)
  (let* ((widgets (cmio-page-widgets cmio ':osc))
         (apps (getf widgets ':application))
         ;; see if SC is current application
         (sc? (gtk:toggle-button-get-active (first apps)))
         (name (getf (getf widgets ':stream) ':name))
         (prop '(:latency :local-port :remote-port :remote-host))
         (flag t))
    (case state
      (:open (setq flag nil))
      (:close 
       (setq flag t)
       ;; sc name never  editable
       (if sc? (gtk:widget-set-sensitive name nil))
       )
      (:set
       )
      (:clear
       ))
    (dolist (a apps) (gtk:widget-set-sensitive a flag))
    (dolist (p prop) (gtk:widget-set-sensitive (getf widgets p) flag))))

(defun osc-set-defaults (cmio owner )
  (let* ((widgets (cmio-page-widgets cmio ':osc))
         (subs (getf widgets :stream))
         (buff (getf subs :name)) ; stream-controls
         inits edit name)
  (case owner
    (:supercollider
     (setq inits '(:remote-host "127.0.0.1" :remote-port "57110"
                   :local-port "57100" :latency "0.05"))
     (setq name "sc.udp")
     (setq edit nil))
    (:other
     (setq inits '(:remote-host "" :remote-port "" :local-port ""
                   :latency ""))
     (setq name "test.udp")
     (setq edit t)))
  (do ((tail inits (cddr tail))
       (buff nil))
      ((null tail) nil)
    (setq buff (getf widgets (car tail)))
    (gtk:entry-set-text buff (cadr tail)))
  ;; dont let sc stream name change
  (gtk:entry-set-text buff name)
  (gtk:widget-set-sensitive buff edit)))

(defmethod cmio-create-page (cmio (target (eql :osc)) notebook pos)
  ;; widgets (:application (<sc><other>) :remote-host [] :remote-port []
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "OSC"))
        (vbox (gtk:vbox-new nil %sw))
        (hbox (gtk:hbox-new nil %sw))
        (widgets (list))
        (radio nil)
        (subs nil)
        (entry nil) )
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    ;; LINE 1
    (setq label (gtk:label-new "Application:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    ;; radio buttons determine class of osc stream
    (setq radio (gtk:radio-button-new-with-label
                 (g:nullptr) "SuperCollider"))
    (setq subs (list radio))
    (gtk:box-pack-start hbox radio nil nil 0)
    (gtk:widget-show radio)
    (g:object-set-data radio "id"
                       (gtk:string->cstring (string :supercollider)))
    (setq radio (gtk:radio-button-new-with-label-from-widget 
                 radio "Other"))
    (nconc subs (list radio))
    (setq widgets (list* :application subs widgets))
    (gtk:box-pack-start hbox radio nil nil 0)
    (gtk:widget-show radio)
    (g:object-set-data radio "id"
                       (gtk:string->cstring (string :other)))
    ;; LINE 2
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)    
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Remote host:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq widgets (list* :remote-host entry widgets))
    (gtk:entry-set-width-chars entry 16)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Remote port:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq widgets (list* :remote-port entry widgets))
    (gtk:entry-set-width-chars entry 6)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Local port:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq widgets (list* :local-port entry widgets))
    (gtk:entry-set-width-chars entry 6)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Latency:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq widgets (list* :latency entry widgets))
    (gtk:entry-set-width-chars entry 6)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq subs (stream-controls cmio :osc hbox 
                                "sc.udp"
                                #'osc-stream-hook))
    (setq widgets (list* :stream subs widgets))
    (cmio-set-page-widgets cmio ':osc widgets)
    (let ((group (getf widgets :application)))
      (g:signal-connect (first group)
                        "toggled" (g:callback osc_radio) 
                        window)
      (g:signal-connect (second group) "toggled"
                        (g:callback osc_radio) 
                        window))
    (osc-set-defaults cmio :supercollider)))

(defmethod cmio-page-data (cmio (page (eql :osc)) &optional op)
  op cmio
  (let* ((widgets (cmio-page-widgets cmio page))
         (group (getf widgets :application))
         (data (list page)) )
    (add-radio-data data :application (first group) ':supercollider)
    (add-radio-data data :application (second group) ':other)
    (add-entry-data data :remote-host (getf widgets ':remote-host)
                    :nullok nil :read nil
                    :test (lambda (x) (= (count #\. x) 3)))
    (add-entry-data data :remote-port (getf widgets ':remote-port)
                    :nullok nil :test #'numberp)
    (add-entry-data data :local-port (getf widgets ':local-port)
                    :nullok nil :test #'numberp)
    (add-entry-data data :latency (getf widgets ':latency)
                    :test #'numberp)
    ;; stream-control
    (setq group (getf widgets :stream))
    (add-entry-data data :name (getf group :name) :nullok nil :read nil)
    data))

(defmethod cmio-ensure-event-stream (cmio (target (eql :osc)) data)
  cmio
  (let* ((appl (get-data data ':application))
         (class (find-class (if (eql appl :sc) 'sc-stream 'osc-stream)))
         (name (getf (get-data data ':stream) ':name))
         (stream (or (find-object name nil)
                     (make-instance class :name name))))
    (init-io stream :remote-host (get-data data :remote-host)
             :remote-port (get-data data :remote-port)
             :local-port (get-data data :local-port)
             :receive-rate (get-data data ':receive-rate)
             :latency (get-data data :latency 0.0))
    (values stream nil)))

;;;
;;; Portmidi page
;;;

(defmethod cmio-create-page (cmio (target (eql :portmidi)) notebook pos)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "Portmidi"))
        (vbox (gtk:vbox-new nil %sw))
        subs hbox menu entry widgets)
    window
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    ;; if its not loaded dont attempt
    (cond ((not (find :portmidi *features*))
           (stub-notebook-page cmio notebook "Portmidi" pos)
           (return-from cmio-create-page nil))
          ((not pm:*portmidi*)
           (system-setup-page cmio :portmidi 
                              "Portmidi library is not initialized."
                              "Initialize Portmidi"
                              vbox
                              (lambda (a b) a b (pm:portmidi)))
           (return-from cmio-create-page nil)))
    (flet ((pmdev (hbox ports id)
             (setq label (gtk:label-new (format nil "~:(~A~):" id)))
             (gtk:box-pack-start hbox label nil nil 0)
             (gtk:widget-show label)
             (setq menu (create-option-menu
                         (cons "None"  (loop for p in ports
                                          when (eql (getf p :type) id)
                                          collect (getf p :name)))))
             (setf widgets (list* id menu widgets))
             (gtk:box-pack-start hbox menu nil nil 0)
             (gtk:widget-show menu))
           (pmini (hbox id b c)
             (setq label (gtk:label-new (format nil "~:(~A~):" id)))
             (gtk:box-pack-start hbox label nil nil 0)
             (gtk:widget-show label)
             (setq entry (gtk:entry-new))
             (setf widgets (list* id entry widgets))
             (gtk:box-pack-start hbox entry nil nil 0)
             (gtk:entry-set-width-chars entry c)
             (gtk:entry-set-text entry (number->string b))
             (gtk:widget-show entry)))
      ;; LINE1: microtuning
      (setq hbox (gtk:hbox-new nil %sw))
      (gtk:box-pack-start vbox hbox nil nil 0)
      (gtk:widget-show hbox)
      (setq subs (add-microtuning hbox))
      (setq widgets (list* :microtuning subs widgets))
      ;; LINE2: IO devices
      (setq hbox (gtk:hbox-new nil %sw))
      (gtk:box-pack-start vbox hbox nil nil 0)
      (gtk:widget-show hbox)
      ;; input/output device menus
      (let ((ports (pm:getDeviceInfo)))
        (pmdev hbox ports :input)
        (pmdev hbox ports :output))
      ;; LINE3: Fields
      (setq hbox (gtk:hbox-new nil %sw))
      (gtk:box-pack-start vbox hbox nil nil 0)
      (gtk:widget-show hbox)
      (pmini hbox :latency *portmidi-default-latency* 6)
      (pmini hbox :inbuf-size *portmidi-default-inbuf-size* 6)
      (pmini hbox :outbuf-size *portmidi-default-outbuf-size* 6)
      (pmini hbox :filter *portmidi-default-filter* 6)
      (pmini hbox :mask *portmidi-default-mask* 6)
      (setq hbox (gtk:hbox-new nil %sw))
      (gtk:box-pack-start vbox hbox nil nil 0)
      (gtk:widget-show hbox)
      (setq subs (stream-controls cmio :portmidi hbox
                                  "midi-port.pm"
                                  #'portmidi-stream-hook))
      (setq widgets (list* :stream subs widgets)))
    (cmio-set-page-widgets cmio :portmidi widgets)
    (let ((str (find-object "midi-port.pm" nil)))
      (when str (cmio-set-page-data cmio :portmidi str)))))

(defun portmidi-stream-hook (cmio status)
  (let ((widgets (cmio-page-widgets cmio :portmidi))
        (props '(:mask :filter :outbuf-size :inbuf-size
                 :latency :input :output))
        (window (cmio-window cmio))
        (state t))
    (case status
      ((:open  :close)
       (setq state (eql status :close))
       (set-subgroup-sensitivity window :portmidi :microtuning
                                 state t))
      (:set )
      (:clear)
      )
    (dolist (p props)
      (gtk:widget-set-sensitive (getf widgets p) state))
    ))

(defmethod cmio-page-data (cmio (page (eql :portmidi)) &optional op)
  op
  (let* ((widgets (cmio-page-widgets cmio page))
         (devs (pm:getdeviceInfo))
         (data (list page)))
    ;; get stream data, abort immediately if problems
    (multiple-value-bind (resl err?)
        (stream-control-data (getf widgets ':stream)
                             '("pm"))
      (nconc data resl)
      (if err? (return-from cmio-page-data data)))
    (add-menu-data data :input (getf widgets :input)
                   (cons nil (loop for d in devs
                                when (eql (getf d :type) :input)
                                collect (getf d :name))))
    (add-menu-data data :output (getf widgets :output)
                   (cons nil (loop for d in devs
                                when (eql (getf d :type) :output)
                                collect (getf d :name))))
    ;; abort immediately if no input or output.
    (unless (or (get-data data :input) (get-data data :output))
      (setf (get-data data :output)
            (list :error "Missing input or output device."
                  :widget (getf widgets ':output)))
        (return-from cmio-page-data data))

    (add-entry-data data :inbuf-size (getf widgets :inbuf-size)
                    :test #'numberp)
    (add-entry-data data :outbuf-size (getf widgets :outbuf-size)
                    :test #'numberp)
    (add-entry-data data :latency (getf widgets :latency) :test #'integerp)
    (add-entry-data data :filter (getf widgets :filter) :test #'numberp)
    (add-entry-data data :mask (getf widgets :mask) :test #'numberp)
    data))

(defmethod cmio-stream-data (cmio (page (eql :portmidi)) stream)
  cmio
  (list :name (object-name stream)
        :input (portmidi-input stream)
        :output (portmidi-output stream)
        :latency (rt-stream-latency stream)
        :inbuf-size (portmidi-inbuf-size stream)
        :outbuf-size (portmidi-outbuf-size stream)
        :receive-rate (rt-stream-receive-rate stream)
        :receiver (receiver? stream)
        :mask (portmidi-channel-mask stream)
        :filter (portmidi-filter stream)
        :open? (io-open stream)))

(defmethod cmio-set-page-data (cmio (page (eql :portmidi)) data)
  ;; data is plist
  (unless (listp data)
    (setq data (cmio-stream-data cmio page data)))
  (let* ((widgets (cmio-page-widgets cmio page))
         (streamw (getf widgets :stream))
         (devs (pm:getdeviceInfo))
         (ins (loop for x in devs when (eql (getf x :type) :input)
                 collect x))
         (outs (loop for x in devs when (eql (getf x :type) :output)
                  collect x))
         id?)
    (when (setq id? (getf data :input))
      (let ((it (loop for x in ins as i from 0
                   if (or (eql id? (getf x :id)) 
                          (equal id? (getf x :name)))
                   return i)))
        (if it (gtk:option-menu-set-history (getf widgets :input)
                                            (+ it 1)))))
    (when (setq id? (getf data :output))
      (let ((it (loop for x in outs as i from 0
                   if (or (eql id? (getf x :id)) 
                          (equal id? (getf x :name)))
                   return i)))
        (if it (gtk:option-menu-set-history (getf widgets :output)
                                            (+ it 1)))))
    (set-page-fields data
                     :name (getf streamw :name)
                     :latency (getf widgets ':latency)
                     :inbuf-size (getf widgets ':inbuf-size)
                     :outbuf-size (getf widgets ':outbuf-size)
;                     :receive-rate (cons :spin (getf streamw ':receive-rate))
                     :filter (getf widgets :filter)
                     :mask (getf widgets :mask))
    (portmidi-stream-hook cmio (if (getf data :open?) :open :close))))


(defmethod cmio-ensure-event-stream (cmio (target (eql :portmidi)) data)
  cmio
  (let* ((name (get-data data :name))
         (stream (or (find-object name nil)
                     (make-instance 'portmidi-stream :name name))))
    (values (init-io stream :input (get-data data ':input)
                     :output (get-data data ':output)
                     :latency (get-data data ':latency)
                     :inbuf-size (get-data data ':inbuf-size)
                     :outbuf-size (get-data data ':outbuf-size)
                     :receive-rate (get-data data ':receive-rate)
                     :channel-tuning (get-data data ':microtuning))
            nil)))

;;;
;;; :plotter page
;;;

(gtk:define-signal-handler plotter_prototype_menu_changed
    :void (widget data)
  ;; widget is menu data is window
  (let* ((cmio (widget->object data))
         (widgets (cmio-page-widgets cmio ':plotter))
         (which (gtk:option-menu-get-history widget)))
    widgets which
  (values)))

(gtk:define-signal-handler plotter_slot2_sensitivity :void (widget data)
  ;; "vert" is true if widget belongs to vertical 
  (let* ((vert (not (g:nullptr? (g:object-get-data widget "y-axis"))))
         (cmio (widget->object data))
         (list (cmio-page-widgets cmio :plotter 
                                (if vert :y-axis :x-axis)))
         (subs (getf list ':slot2))
         (act? (gtk:toggle-button-get-active widget)))
    ;;(print (list :vert-> vert))
    (dolist (w (cdr subs)) (gtk:widget-set-sensitive w act?))
    (values)))

(gtk:define-signal-handler plotter_choose_prototype :void (widget data)
  ;; "vert?" is true if widget belongs to vertical 
  (let* ((vert? (not (g:nullptr? (g:object-get-data widget "y-axis"))))
         (cmio (widget->object data))
         (plist (cmio-page-widgets cmio ':plotter 
                                 (if vert? ':y-axis ':x-axis)))
         (types (loop for x in (cdr (getf plist ':prototypes)) ;car=menu
                   collect (first (third x))))
         (buffs (getf plist ':protoinits))
         (which (gtk:option-menu-get-history widget)))
    (prototype-fill-buffers (nth which types) buffs)
    (values)))

(defun prototype-fill-buffers (proto buffs)
  (let (val)
    (setq val (axis-minimum proto))
    (gtk:entry-set-text (first buffs) (if val (format nil "~S" val) ""))
    (setq val (axis-maximum proto))
    (gtk:entry-set-text (second buffs) (if val (format nil "~S" val) ""))
    (setq val (axis-increment proto))
    (gtk:entry-set-text (third buffs) (if val (format nil "~S" val) ""))
    (setq val (axis-ticks-per-increment proto))
    (gtk:spin-button-set-value (fourth buffs) (or val 1))
    (values)))

(defun create-axis-page (window nb dimension &key omit only slot1 slot2
                          title selected)
  (let ((protos (list))
        (cmio (widget->object window))
        (vert? (eql dimension ':y-axis))
        (vbox (gtk:vbox-new nil %rs))
        (label (gtk:label-new (keyword->label dimension nil)))
        (hpad 0)
        hbox data widgets menu check entry spin)
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-append-page nb vbox label)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    ;; get prototype data
    (maphash (lambda (k a) 
               (push (list (format nil "~@(~A~)" (or k "values")) k a) 
                     protos))
             *axis-prototypes*)
    (unless title (setf title (keyword->label dimension nil)))
    (if only
      (setq protos (loop for x in protos
                      when (member (second x) only) collect x))
      (if omit
        (setq protos (loop for x in protos 
                        unless (member (second x) omit) collect x))))
    ;; NIL selected returns default axis type (the nil prototype)
    (setq selected (or (find selected protos :key #'second)
                       (error "Not a prototype type: ~S." selected)))
    (setq protos (delete selected protos))
    (when (cdr protos)
      (setq protos (sort protos #'string-lessp :key #'first)))
    (push selected protos)
    ;; line 1
    ;; "X axis:" <menu> "From slot:" [time] [x] "and slot" [  ]
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil hpad)
    (gtk:widget-show hbox)
    ;;(setq label (gtk:label-new (if vert? "Y axis:" "X axis:")))
    ;;(gtk:box-pack-start hbox label nil nil hpad)
    ;;(gtk:widget-show label)
    (setq menu (create-option-menu
                (mapcar #'first protos)
                :changed (g:callback plotter_choose_prototype)
                :data window))
    ;; set "vertical" property so handler can distinguish dimensions
    (if vert? 
      (g:object-set-data menu "y-axis" menu))
    (gtk:box-pack-start hbox menu nil nil hpad)
    (gtk:widget-show menu)
    ;; :prototypes (<menu> protos)
    (setq widgets (list* ':prototypes (list* menu protos) widgets) )
    (setq label (gtk:label-new "Slot:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq data (list entry))
    (gtk:entry-set-width-chars entry 12)
    (when slot1 (gtk:entry-set-text entry (format nil "~(~A~)" slot1)))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry) 
    ;; :slot1 (<buffer>)
    (setq widgets (list* ':slot1 (nreverse data) widgets))
    (setq check (gtk:check-button-new))
    (setq data (list check))
    (gtk:toggle-button-set-active check nil)
    (g:signal-connect check "toggled"
                      (g:callback plotter_slot2_sensitivity)
                      window)
    ;; set vertical property so handler can distinguish dimensions
    (if vert? 
      (g:object-set-data check "y-axis" check))
    ;; optional slot2
    (gtk:box-pack-start hbox check nil nil hpad)
    (gtk:widget-show check)
    (setq label (gtk:label-new "and slot:"))
    (setq data (list* label data))
    (gtk:widget-set-sensitive label nil)
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new))
    (setq data (list* entry data))
    (gtk:widget-set-sensitive entry nil)
    (gtk:entry-set-width-chars entry 12)
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry) 
    ;; :slot2 (<check> <label> <buffer>)
    (setq widgets (list* ':slot2 (nreverse data) widgets)) 
    (when slot2
      (gtk:entry-set-text entry (format nil "~(~A~)" slot2))
      ;; this will sensitize label and entry
      (gtk:toggle-button-set-active check t))
    ;; line 2
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    (setq data nil)
    (dolist (s '("Minimum:" "Maximum:" "Increment:"))
      (setq label (gtk:label-new s))
      (gtk:box-pack-start hbox label nil nil hpad)
      (gtk:widget-show label)
      (setq entry (gtk:entry-new))
      (setq data (list* entry data))
      (gtk:entry-set-width-chars entry 10)
      (gtk:box-pack-start hbox entry nil nil hpad)
      (gtk:widget-show entry) )
    (setq label (gtk:label-new "Ticks:"))
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq spin (gtk:spin-button-new-with-range 1 24 1))
    (setq data (list* spin data))
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin nil nil hpad)
    (gtk:widget-show spin)
    ;; :protoinits (<menu> (protos))
    (setq data (nreverse data))
    (setq widgets (list* ':protoinits data widgets))
    (prototype-fill-buffers (first (third selected)) data)
    widgets))

; (cmio)

(defmethod cmio-create-page (cmio (target (eql :plotter)) notebook pos)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "Plotter"))
        (widgets (list))
        (hpad 0)
        (vbox (gtk:vbox-new nil 0))
        (hbox (gtk:hbox-new nil %sw))
        data entry check button menu)
    data check button 
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show vbox)
    (gtk:widget-show label)
    (gtk:box-pack-start vbox hbox nil nil 0)
    (gtk:widget-show hbox)
    ;; line 1 (score file)
    (setq label (gtk:label-new "Title:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:entry-set-width-chars entry 32) 
    (gtk:widget-show entry)
    (setq widgets (list* ':title entry widgets))
    (setq label (gtk:label-new "Event layering:"))
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq menu (create-option-menu '("Add as new layer"
                                     "Mix into focus layer" 
                                     "Overwrite focus events"
                                     "Replace focus layer")))
    (gtk:box-pack-start hbox menu nil nil hpad)
    (setq widgets (list* ':event-layering menu widgets))
    (gtk:widget-show menu)
    (let ((nb (gtk:notebook-new)))
      (gtk:container-add vbox nb)
      (gtk:widget-show nb)
      ;(gtk:container-set-border-width nb 5)
      (gtk:notebook-set-tab-pos notebook gtk:pos-top)
      (setq data (create-axis-page window nb ':y-axis
                                   :omit '(:seconds :milliseconds)
                                   :selected NIL))
      (setq widgets (list* :y-axis  data widgets))
      (setq data (create-axis-page window nb ':x-axis
                                   :slot1 'time
                                   :only '(:seconds ) ;:milliseconds
                                   :selected ':seconds))
      (setq widgets (list* :x-axis  data widgets)))
    ;;(setf (widget-property window ':plotter) widgets)
    (cmio-set-page-widgets cmio ':plotter widgets)
    ;;(pprint widgets)
    (values)))

(defmethod cmio-page-data (cmio (page (eql :plotter)) &optional op)
  op
  (let ((widgets (cmio-page-widgets cmio page))
        (data (list page))
        group axis)
    ;; widgets is:
    ;; (:title <e> :event-layering <m>
    ;;  :x-axis 
    ;;  (:prototypes (<m> . data)
    ;;   :slot1 (<e>) :slot2 (<c> <l> <e>)
    ;;   :protoinits (<e> <e> <e> <s>))
    ;;  :y-axis (...))
    (add-entry-data data :title (getf widgets ':title) 
                    :nullok nil :read nil)
    (add-menu-data data :event-layering (getf widgets ':event-layering)
                   '(:add :mix :overwrite :replace))
    ;; process the axis frame data. most of this hair is due to the
    ;; fact that axis data are returned as sublists but any errors
    ;; must be "propagated" into the top-level data list so that
    ;; report-errors can find them.
    (dolist (ax '(:x-axis :y-axis))
      (setq group (getf widgets ax))
      (error-block
        (let* ((protos (getf group ':prototypes))
               (temp (add-menu-data nil ax (first protos)
                                    (loop for x in (cdr protos)
                                       collect (second x))))
               (slot1 (getf group ':slot1))
               (slot2 (getf group ':slot2))
               slots)
          (setq axis (LIST (second temp)))
          (setq temp (add-entry-data nil :slot1 (first slot1)
                                     :nullok nil :test #'symbolp))
          (when (and (consp (second temp))
                     (eql (first (second temp)) ':error))
            ;; Set the entire axis data to the error so that
            ;; abort-errors will find it.
            (setq axis (second temp))
            (error-abort))
          (setq slots (list (second temp)))
          (when (gtk:toggle-button-get-active (first slot2))
            (setq temp (add-entry-data nil :slot2 (third slot2)
                                       :test #'symbolp))
            (when (and (consp (second temp))
                       (eql (first (second temp)) ':error))
              (setq axis (second temp))
              (error-abort))
            (nconc slots (list (second temp)))
            )
          (nconc axis (list :slots slots))
          (let ((inits (getf group ':protoinits)))
            (setq temp (list :inits))
            (add-entry-data temp :minimum (first inits) 
                            :test #'numberp)
            (add-entry-data temp :maximum (second inits) 
                            :test #'numberp)
            (add-entry-data temp  :increment (third inits) 
                            :test #'numberp)
            (add-spin-data temp :ticks-per-increment (fourth inits)
                           :result #'floor)
            (loop for x in (cddr temp) by #'cddr
                 do (if (and (consp x) (eql (car x) ':error))
                      (progn
                        (setq axis x)
                        (error-abort))))
            (nconc axis (cdr temp))
            )
          )
        ) ;; end block   
      (nconc data (list ax axis))
      )
    data))

(defmethod cmio-set-page-data (cmio (page (eql :plotter)) args)
  (let* ((widgets (cmio-page-widgets cmio page ))
         (x-axis (getf widgets ':x-axis))
         (xinits (cadr (member ':x-axis args)))
         (y-axis (getf widgets ':y-axis))
         (yinits (cadr (member ':y-axis args))))
    ;; widgets is:
    ;; (:title <e> :event-layering <c>
    ;;  :x-axis
    ;;  (:prototypes (<m> . data)
    ;;   :slot1 (<e>) :slot2 (<c> <l> <e>)
    ;;   :protoinits (<e> <e> <e> <s>)))
    ;;  :y-axis (...))
    (set-page-fields args :title (getf widgets ':title)
                     :event-layering (list :menu
                                           (getf widgets ':event-layering)
                                           :add :mix :overwrite :replace))
    (flet ((setone (plist args)
             (let* ((protos (getf plist ':prototypes))
                    (names (loop for x in (cdr protos)
                              collect (second x)))
                    (types (loop for x in (cdr protos)
                              collect (first (third x))))
                    (buffs (getf plist ':protoinits))
                    slots which)
               (if (consp args)
                 (setq which (if (oddp (length args))
                               (position (pop args) names)
                               nil))
                 (setq which (position args names) args nil))
               (when which
                 (prototype-fill-buffers (nth which types) buffs))
               (setq slots (getf args ':slot))
               (cond ((not slots) )
                     ((and (consp slots) (= (length slots) 2))
                      (gtk:entry-set-text (first (getf plist ':slot1))
                                          (format nil "~(~A~)" (car slots)))
                      (gtk:toggle-button-set-active
                       (first (getf plist ':slot2)) t)
                      (gtk:entry-set-text (third (getf plist ':slot2))
                                          (format nil "~(~A~)" (cadr slots))))
                     ((and (consp slots) (= (length slots) 1))
                      (gtk:entry-set-text (first (getf plist ':slot1))
                                          (format nil "~(~A~)" (car slots))))
                     ((symbolp slots)
                      (gtk:entry-set-text (first (getf plist ':slot1))
                                          (format nil "~(~A~)" slots)))
                     (t nil))
               (when args
                 (set-page-fields args :minimum (first buffs)
                                  :maximum (second buffs)
                                  :increment (third buffs)
                                  :ticks-per-increment 
                                  (cons ':spin (fourth buffs)))))))
      (when xinits (setone x-axis xinits))
      (when yinits (setone y-axis yinits))
      )))

(defmethod cmio-ensure-event-stream (cmio (target (eql :plotter)) data)
  (let* ((name (get-data data ':title))
         (stream (or (find-object name nil)
                     (plotter :title name :no-window t))))
    cmio
    (setf (plotter-event-layering stream)
          (get-data data ':event-layering))
    (plotter-set-axis-values stream :x-axis (get-data data ':x-axis)
                             :y-axis (get-data data ':y-axis)
                             :redraw nil)
    (values stream nil)))

;;;
;;; :seq page
;;;

(defmethod cmio-create-page (cmio (target (eql :seq)) notebook pos)
  (let ((label (gtk:label-new "Seq"))
        (vbox (gtk:vbox-new nil 5))
        (hpad 5)
        hbox data entry check)
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show label)
    (gtk:widget-show vbox)
    ;; line 1, Name: [  ]
    (setq hbox (gtk:hbox-new nil 5))
    (gtk:box-pack-start vbox hbox nil nil hpad)
    (gtk:widget-show hbox)
    (setq label (gtk:label-new "Name:"))
    (cmio-show-widget-required cmio label)
    (gtk:box-pack-start hbox label nil nil hpad)
    (gtk:widget-show label)
    (setq entry (gtk:entry-new ))
    (setq data (list entry))
    (gtk:box-pack-start hbox entry nil nil hpad)
    (gtk:widget-show entry)
    ;; line 2, 
    (setq hbox (gtk:hbox-new nil 5))
    (gtk:box-pack-start vbox hbox nil nil hpad)
    (gtk:widget-show hbox)
    (setq check (gtk:check-button-new-with-label "Replace existing contents."))
    (setq data (list* check data))
    (gtk:box-pack-start hbox check nil nil hpad)    
    (gtk:widget-show check)
    (cmio-set-page-widgets cmio ':seq (nreverse data))
    (values)))

(defmethod cmio-page-data (cmio (page (eql :seq)) &optional op)
  op
  (let ((widgets (cmio-page-widgets cmio page))
        (data (list page)))
    ;; widgets is: (<e> <c> )
    (add-entry-data data :name (first widgets) :nullok nil :read nil)
    (add-check-data data :replace (second widgets))
    data))

(defmethod cmio-set-page-data (cmio (page (eql :seq)) args)
  (let ((widgets (cmio-page-widgets cmio page)))
    ;; widgets is: (<e> <c> )
    (set-page-fields args :name (FIRST widgets)
                     :replace (cons ':check (SECOND widgets)))))

(defmethod cmio-ensure-event-stream (cmio (target (eql :seq)) data)
  cmio
  (let* ((name (get-data data ':name))
         (stream (or (find-object name  nil)
                     (make-instance <seq> :name name))))
    (when (get-data data ':replace)
      (remove-subobjects stream))
    (values stream nil)))

;;;
;;; :sc page

(defmethod cmio-create-page (cmio (target (eql :sc)) notebook pos)
  (let ((window (cmio-window cmio))
        (label (gtk:label-new "SC"))
        (vbox (gtk:vbox-new nil %sw))
        tops table hbox entry spin check data)
    window
    (gtk:widget-show vbox)
    (gtk:container-set-border-width vbox %bw)
    (gtk:notebook-insert-page notebook vbox label pos)
    (gtk:widget-show label)
    (setq table (gtk:table-new 5 3 nil))
    (setq tops (logior gtk:fill gtk:expand))
    (gtk:box-pack-start vbox table nil nil 0)
    (gtk:widget-show label)
    (gtk:widget-show table)
    (gtk:table-set-row-spacings table %rs)

    (setq label (gtk:label-new "Score: "))
    (cmio-show-widget-required cmio label)
    (gtk:table-attach table label 1 2 0 1   0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 0 1   tops 0 0 0)
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new))
    (setq data (list :score entry))
    (gtk:entry-set-width-chars entry 32)
    (gtk:entry-set-text entry "test.osc")
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Versioning"))
    (nconc data (list :versioning check))
    (gtk:box-pack-start hbox check nil nil 0)
    (gtk:widget-show check) 
    (setq label (gtk:label-new " End Pad:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label )
    (setq entry (gtk:entry-new))
    (nconc data (list :pad entry))
    (gtk:entry-set-width-chars entry 5)
    (gtk:entry-set-text entry "0.00")
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    ;; Audio group
    (setq label (gtk:label-new "Output: "))
    (cmio-show-widget-required cmio label)
    (gtk:table-attach table label 1 2 1 2  0 0 0 0 )
    (gtk:widget-show label)
    (setq hbox (gtk:hbox-new nil %sw))
    (gtk:table-attach table hbox 2 3 1 2  tops 0 0 0)
    (gtk:widget-show hbox)
    (setq entry (gtk:entry-new))
    (nconc data (list :output entry))
    (gtk:entry-set-text entry "test.aiff")
    (gtk:entry-set-width-chars entry 32)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq check (gtk:check-button-new-with-label "Play"))
    (nconc data (list :play check))
    (gtk:toggle-button-set-active check t)
    (gtk:box-pack-start hbox check nil nil 0)
    (gtk:widget-show check) 
    (setq label (gtk:label-new "Srate:"))
    (gtk:box-pack-start hbox label nil nil 0)   
    (gtk:widget-show label) 
    (setq entry (gtk:entry-new))
    (nconc data (list :srate entry))
    (gtk:entry-set-text entry "44100")
    (gtk:entry-set-width-chars entry 6)
    (gtk:box-pack-start hbox entry nil nil 0)
    (gtk:widget-show entry)
    (setq label (gtk:label-new "Channels:"))
    (gtk:box-pack-start hbox label nil nil 0)
    (gtk:widget-show label) 
    (setq spin (gtk:spin-button-new-with-range 1 9 1))
    (nconc data (list :channels spin))
    (gtk:spin-button-set-digits spin 0)
    (gtk:box-pack-start hbox spin nil nil 0)
    (gtk:widget-show spin) 
    (cmio-set-page-widgets cmio ':sc data)
    (values)
    ))

(defmethod cmio-page-data (cmio (page (eql :sc)) &optional op)
  op
  (let ((widgets (cmio-page-widgets cmio page))
        (data (list page)))
    (add-entry-data data :score (getf widgets :score)
                    :read nil :nullok nil)
    (data-check-file-type data :score (getf widgets :score) "osc")
    (add-check-data data :versioning (getf widgets :versioning))
    (add-entry-data data :pad (getf widgets :pad)
                    :nullok t :test #'numberp)
    (add-entry-data data :output (getf widgets :output)
                    :read nil :nullok nil)
    (data-check-file-type data :output (getf widgets :output) "aiff")
    (add-check-data data :play (getf widgets :play))
    (add-entry-data data :srate (getf widgets :srate) :test #'numberp
                    :nullok t)
    (add-spin-data data :channels (getf widgets :channels) :result #'floor)
    data))

(defmethod cmio-ensure-event-stream (cmio (target (eql :sc)) data)
  cmio
  (let* ((name (get-data data :score))
         (stream (or (find-object name nil)
                     (make-instance 'sc-file :name name))))
    (values (init-io stream :output (get-data data ':output)
                     :versioning (get-data data :versioning)
                     :play (get-data data :play)
                     :pad (get-data data :pad)
                     :srate (get-data data ':srate)
                     :channels (get-data data ':channels))
            nil)))

;;;
;;; Targets and Compose notebooks
;;; 

(defun cmio-create-output-line (cmio box)
  (let ((hbox (gtk:hbox-new nil 5))
        image label)
    (gtk:box-pack-end box hbox t t 0)
    (gtk:widget-show hbox)
    (setq image (gtk:image-new-from-stock "gtk-yes"
                                          gtk:icon-size-small-toolbar))
    (setf (FIRST (cmio-output cmio)) image)
    (gtk:box-pack-start hbox image nil nil 0)
    (setq label (gtk:label-new ""))
    (gtk:misc-set-alignment label 0 .5)
    (gtk:box-pack-start hbox label t t 0)
    (gtk:widget-show label)
    (setf (SECOND (cmio-output cmio)) label)
    (values)))

(defmethod cmio-notebook-switch-page (cmio notebook page)
  ;; default switch-page method does nothing
  cmio notebook page
  (values))

(gtk:define-signal-handler compose_switch_page :void (notebook page
                                                               (pnum :int)
                                                               data)
  ;; widget is button, data in window
  notebook page
  (let ((cmio (widget->object data)))
    (cmio-clear-message cmio)
    (if (< -1 pnum (length *cmio-source-pages*))
        (cmio-notebook-switch-page cmio
                                   :compose (elt *cmio-source-pages* pnum)))))

(gtk:define-signal-handler applications_switch_page :void (notebook page (pnum :int) data)
  ;; widget is button, data in window
  notebook page
  (let ((cmio (widget->object data)))
    (cmio-clear-message cmio)
    (if (< -1 pnum (length *cmio-target-pages*))
        (cmio-notebook-switch-page (widget->object data)
                                   :targets (elt *cmio-target-pages* pnum)))))

(defun cmio-create-compose-notebook (cmio box)
  (let (frame notebook)
    (setq frame (gtk:frame-new "Compose"))
    (gtk:box-pack-start box frame nil nil 0)
    (gtk:widget-show frame)
    (setq notebook (gtk:notebook-new))
    (gtk:container-set-border-width notebook %bw)
    (setf (FIRST (cmio-notebooks cmio)) notebook)
    (gtk:container-add frame notebook)
    (gtk:widget-show notebook)
    (gtk:notebook-set-tab-pos notebook gtk:pos-top)
    (dolist (s *cmio-source-pages*)
      (cmio-new-page cmio :notebook :compose :page s)
      (cmio-create-page cmio s notebook -1 ))
;    (gtk:notebook-set-current-page notebook page)
    (values)))

(defun cmio-create-targets-notebook (cmio box)
  (let (frame notebook)
    (setq frame (gtk:frame-new "Systems"))
    (gtk:box-pack-start box frame nil nil 0)
    (gtk:widget-show frame)
    (setq notebook (gtk:notebook-new))
    (gtk:container-set-border-width notebook %bw)
    (setf (SECOND (cmio-notebooks cmio)) notebook)
    (gtk:container-add frame notebook)
    (gtk:widget-show notebook)
    (gtk:notebook-set-tab-pos notebook gtk:pos-top)
    (dolist (s *cmio-target-pages*) 
      (cmio-new-page cmio :notebook :targets :page s)
      (cmio-create-page cmio s notebook -1 ))
    (values)))

;;;
;;; main function

(gtk:define-signal-handler cmio_quit :void (widget data)
  data
  (let ((cmio (widget->object widget)))
    (gtk-remove-toplevel cmio)
    (unless (gtk-open-toplevels?)
      (gtk-main-stop))
    (values)))

(defparameter *cmio-title* "Common Music")

(defun cmio-open (cmio args)
  (gtk:init-ensure)
  (let* ((window (gtk:window-new gtk:window-toplevel))
         (vbox (gtk:vbox-new nil 5)))
    (setf (cmio-window cmio) window)
    (setf (widget->object window) cmio)
    (gtk:window-set-title window *cmio-title*)
    (gtk:container-set-border-width window 10)
    (gtk:container-add window vbox)
    (gtk:widget-show vbox)
    (cmio-allocate-colors cmio)
    (cmio-create-targets-notebook cmio vbox)
    (cmio-create-compose-notebook cmio vbox)
 
    (cmio-create-output-line cmio vbox)
    ;; process user inits
    (dopairs (p v args)
      (when (consp v) (cmio-set-page-data cmio p v)))

    ;; connect page switching signal on compose pages now that
    ;; target notebook is ready
    (g:signal-connect (FIRST (cmio-notebooks cmio)) "switch-page"
                      (g:callback compose_switch_page)
                      window)
    ;; select first page (Score) and sensitize targes
    (gtk:notebook-set-current-page (FIRST (cmio-notebooks cmio)) 0)
    
    (g:signal-connect (SECOND (cmio-notebooks cmio)) "switch-page"
                      (g:callback applications_switch_page)
                      window)
    (let ((page (or (position (getf args ':target ':midi)
                              *cmio-target-pages*)
                    (position ':midi *cmio-target-pages*))))
      (gtk:notebook-set-current-page (SECOND (cmio-notebooks cmio)) page))

    (cmio-print cmio :message "Ready.")
    (gtk:widget-show window)
    (g:signal-connect window "destroy"
                      (g:callback cmio_quit)
                      (g:nullptr))
    (push cmio *gtk-open-toplevels*)
    (gtk-main-start )
    cmio))

(defun print-available-targets (cmio)
  (let ((str (concatenate 'string
                          "Active targets:"
                          (if (target-system-ready? :clm) " CLM," "")
                          (if (target-system-ready? :cmn) " CMN,"  "")
                          " Csound, Midi,"
                          (if (target-system-ready? :midishare)
                            " Midishare," "")
                          " Plotter, Seq.")))
    (cmio-print cmio :message str)))

(defun cmio-allocate-colors (cmio)
  ;; (GTK:WIDGET-MODIFY-TEXT)
  ;; (gtk:widget-modify-text widget gtk:state-normal color)
  (setf (cmio-colors cmio)
        (let ((map (gdk:colormap-get-system)))
          (loop for c in '((:red  "#CD2626")
                           (:green "#006400")
                           (:blue "blue")
                           (:yellow "#FF6103")
                           (:pale-yellow "#FFF8DC" ;"#EEE8AA" ;"#FFF68F" ;"#FAFAE8"
                            ))
             for s = (gtk:struct-alloc :<G>dk<C>olor)
             do (gdk:color-parse (second c) s)
             (gdk:colormap-alloc-color map s t t)
             collect (first c) collect s))))

(defun cmio (&rest args)
  #+darwin
  (unless (darwin-x11-running?)
    (return-from cmio nil))
  (let ((cmio (make-instance 'cmio)))
    (if *gtk-main*
      #+openmcl
      (gtk-call #'(lambda () (cmio-open cmio args)))
      #-openmcl
      (cmio-open cmio args)
      (cmio-open cmio args))
    cmio))


#|
(cmio :cmn '(:score "foo.eps" :versioning t :view t :all-output-in-one-file t :title "Bif" :size 80 :metronome 120 :exact t :staffing ((0 :name a :meter (3 4) :clef :both) (1 :name b :meter (4 4) :clef :alto) (2 :name c :meter "9/8" :clef :treble))))
(cmio :clm '(:score "foo.clm" :versioning t :output "bif.snd" :play t :srate 10000 :channels 3 :scaled-to 1 :scaled-by 1 :clipped t :statistics t :verbose t :comment "hiho!" :reverb rev :decay-time 2 :reverb-data (a b c) ))
(cmio-set-page-data foo :csound '(:score  "fig" :versioning t :Header "zzz" :orchestra "xcvxcvxvvvxcxvcv" :output "pif/ziggy"))
(cmio :score '(:events (Bar) :starts 1))
(cmio :files '(:audio "asd" :midi "asd a" :sco "vvcv" :eps "casd" :html "zxc" :ins (lopo) :clm "fof" :lisp (fasd)))
(cmio :midi '(:score "zuz.mid" :microtuning :divisions :exclude-tracks (0 1 2) :meta-exclude t :keynum-format :hertz :override-tempo 99 :time-format :ticks))
(cmio :midishare '(:connection :midi-player
                   :name "buf.mp" :track 3 :play nil))
(cmio :plotter '(:title "asd" :event-layering :overwrite
                 :x-axis (:minimum 30 :maximum 90
                          :slot fred :ticks-per-increment 3
                          :increment 10)
                 :y-axis (:keynum :minimum 60 :maximum 200
                          :slot (buf wuzzy))
                 ))
(cmio :seq '(:name zuz :replace t))


|#

