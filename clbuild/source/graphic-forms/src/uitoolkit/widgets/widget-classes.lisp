;;;;
;;;; widget-classes.lisp
;;;;
;;;; Copyright (C) 2006-2007, Jack D. Unrue
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 
;;;;     1. Redistributions of source code must retain the above copyright
;;;;        notice, this list of conditions and the following disclaimer.
;;;; 
;;;;     2. Redistributions in binary form must reproduce the above copyright
;;;;        notice, this list of conditions and the following disclaimer in the
;;;;        documentation and/or other materials provided with the distribution.
;;;; 
;;;;     3. Neither the names of the authors nor the names of its contributors
;;;;        may be used to endorse or promote products derived from this software
;;;;        without specific prior written permission.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS "AS IS" AND ANY
;;;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DIS-
;;;; CLAIMED.  IN NO EVENT SHALL THE AUTHORS AND CONTRIBUTORS BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;

(in-package :graphic-forms.uitoolkit.widgets)

(defclass display (gfs:native-object) ()
  (:documentation "Instances of this class describe characteristics of monitors attached to the system."))

(defclass event-dispatcher () ()
  (:documentation "Instances of this class receive events on behalf of user interface objects."))

(defclass scrolling-helper (event-dispatcher)
  ((horizontal-policy
    :accessor horizontal-policy-of
    :initarg :horizontal-policy
    :initform :always)
   (step-increments
    :accessor step-increments
    :initarg :step-increments
    :initform (gfs:make-size :width 1 :height 1))
   (vertical-policy
    :accessor vertical-policy-of
    :initarg :vertical-policy
    :initform :always)
   (viewport-origin
    :initform (gfs:make-point)))
  (:documentation "Instances of this class manage scrolling behavior in addition to other event processing."))

(defvar *default-dispatcher* (make-instance 'event-dispatcher))

(defclass layout-managed ()
  ((layout-p
    :reader layout-p
    :initform t)
   (layout
    :accessor layout-of
    :initarg :layout
    :initform nil)
   (status-bar
    :reader status-bar-of
    :initform nil))
  (:documentation "Instances of this class employ a layout manager to organize their children."))

(defclass group (layout-managed)
  ((location
    :accessor location-of
    :initarg :location
    :initform nil)
   (size
    :accessor size-of
    :initarg :size
    :initform nil)
   (style
    :accessor style-of
    :initarg :style
    :initform nil))
  (:documentation "Instances of this class act as logical containers for other objects."))

(defclass event-source (gfs:native-object)
  ((dispatcher
    :accessor dispatcher
    :initarg :dispatcher
    :initform *default-dispatcher*)
   (callback-event-name
    :accessor callback-event-name-of
    :initform nil
    :allocation :class)) ; subclasses will shadow this slot
  (:documentation "This is the base class for user interface objects that generate events."))

(defclass item (event-source)
  ((item-id
    :accessor item-id
    :initarg :item-id
    :initform 0)
   (data
    :accessor data-of
    :initarg :data
    :initform nil)
   (callback-event-name
    :accessor callback-event-name-of
    :initform 'event-select
    :allocation :class)) ; shadowing same slot from event-source
  (:documentation "The item class is the base class for all non-windowed user interface objects."))

(defclass list-item (item) ()
  (:documentation "A subclass of item representing an element of a list-box."))

(defclass menu-item (item) ()
  (:documentation "A subclass of item representing a menu item."))

(defclass standard-scrollbar (event-source)
  ((orientation
    :reader orientation-of
    :initarg :orientation
    :initform nil))
  (:documentation "This class encapsulates a scrollbar attached to a window."))

(defclass widget (event-source)
  ((cursor
    :initform nil)
   (style
    :accessor style-of
    :initarg :style
    :initform nil))
  (:documentation "The widget class is the base class for all windowed user interface objects."))

(defclass item-manager ()
  ((sort-predicate
    :accessor sort-predicate-of
    :initarg :sort-predicate
    :initform nil)
   (items
    ;; FIXME: allow subclasses to set initial size?
    :initform (make-array 7 :fill-pointer 0 :adjustable t))
   (text-provider
    :accessor text-provider-of
    :initarg :text-provider
    :initform nil)
   (image-provider
    :accessor image-provider-of
    :initarg :image-provider
    :initform nil))
  (:documentation "A mix-in for objects composed of sub-elements."))

(defclass control (widget)
  ((brush-color
    :accessor brush-color-of
    :initform nil)
   (brush-handle
    :accessor brush-handle-of
    :initform (cffi:null-pointer))
   (font
    :accessor font-of
    :initform nil)
   (text-color
    :accessor text-color-of
    :initform nil)
   (pixel-point
    :accessor pixel-point-of
    :initform nil)
   (max-size
    :initarg :maximum-size
    :initform nil)
   (min-size
    :initarg :minimum-size
    :initform nil)
   (system-classname
    :accessor system-classname-of
    :initform nil
    :allocation :class)) ; subclasses will shadow this slot
  (:documentation "The base class for widgets having pre-defined native behavior."))

(defmacro define-control-class (classname system-classname callback-event-name &optional docstring mixins)
  `(defclass ,classname `,(control ,@mixins)
     ((callback-event-name
       :accessor callback-event-name-of
       :initform ,callback-event-name
       :allocation :class)
      (system-classname
       :reader system-classname-of
       :initform ,system-classname
       :allocation :class))
    ,(if (typep docstring 'string) `(:documentation ,docstring) `(:documentation ""))))

(define-control-class
  button
  "button"
  'event-select
  "This class represents selectable controls that issue notifications when clicked.")

(define-control-class
  edit
  "edit"
  'event-modify
  "This class represents a control in which the user may enter and edit text.")

(define-control-class
  label
  "static"
  'event-select
  "This class represents non-selectable controls that display a string or image.")

(define-control-class
  list-box
  "listbox"
  'event-select
  "The list-box class represents a listbox control."
  (item-manager))

(define-control-class
  progress-bar
  "msctls_progress32"
  'event-select
  "This class represents controls that provide visual feedback for progress.")

(define-control-class
  scrollbar
  "scrollbar"
  'event-scroll
  "This class represents an individual scrollbar control.")

(define-control-class
  slider
  "msctls_trackbar32"
  'event-scroll
  "This class represents a slider (or trackbar) control.")

(defclass color-dialog (widget) ()
  (:documentation "This class represents the standard color chooser dialog."))

(defclass file-dialog (widget)
  ((open-mode
    :reader open-mode
    :initform t))
  (:documentation "This class represents the standard file open/save dialog."))

(defclass font-dialog (widget) ()
  (:documentation "This class represents the standard font dialog."))

(defclass menu (widget item-manager)
  ((callback-event-name
    :accessor callback-event-name-of
    :initform 'event-activate
    :allocation :class)) ; shadowing same slot from event-source
  (:documentation "The menu class represents a container for menu items (and submenus)."))

(defclass status-bar (control item-manager layout-managed)
  ((system-classname
    :reader system-classname-of
    :initform "msctls_statusbar32"
    :allocation :class))
  (:documentation "This class represents the status bar widget configured within top-level windows."))

(defclass window (widget layout-managed)
  ((max-size
    :initarg :maximum-size
    :initform nil)
   (min-size
    :initarg :minimum-size
    :initform nil))
  (:documentation "Base class for user-defined widgets that serve as containers."))

(defclass dialog (window) ()
  (:documentation "The dialog class is the base class for both system-defined and application-defined dialogs."))

(defclass panel (window) ()
  (:documentation "Base class for windows that are children of top-level windows (or other panels)."))

(defclass root-window (window) ()
  (:documentation "This class encapsulates the root of the desktop window hierarchy."))

(defclass top-level (window) ()
  (:documentation "Base class for windows that can be moved and resized by the user, and which normally have title bars."))

(defclass timer (event-source)
  ((id
    :reader id-of
    :initform 0)
   (initial-delay
    :reader initial-delay-of
    :initarg :initial-delay
    :initform 1000)
   (delay
    :accessor delay-of
    :initarg :delay
    :initform 1000))
  (:documentation "A timer is a non-windowed object that generates events at a regular (adjustable) frequency."))
