;;;;
;;;; event-generics.lisp
;;;;
;;;; Copyright (C) 2006, Jack D. Unrue
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

(defgeneric event-activate (dispatcher widget)
  (:documentation "Implement this to respond to an object being activated.")
  (:method (dispatcher widget)
    (declare (ignorable dispatcher widget))))

(defgeneric event-arm (dispatcher item)
  (:documentation "Implement this to respond to an object about to be selected.")
  (:method (dispatcher item)
    (declare (ignorable dispatcher item))))

(defgeneric event-close (dispatcher widget)
  (:documentation "Implement this to respond to an object being closed.")
  (:method (dispatcher widget)
    (declare (ignorable dispatcher widget))))

(defgeneric event-collapse (dispatcher item)
  (:documentation "Implement this to respond to an object (or item within) being collapsed.")
  (:method (dispatcher item)
    (declare (ignorable dispatcher item))))

(defgeneric event-deactivate (dispatcher widget)
  (:documentation "Implement this to respond to an object being deactivated.")
  (:method (dispatcher widget)
    (declare (ignorable dispatcher widget))))

(defgeneric event-default-action (dispatcher widget)
  (:documentation "Implement this to respond to the widget-specific default action.")
  (:method (dispatcher widget)
    (declare (ignorable dispatcher widget))))

(defgeneric event-deiconify (dispatcher widget)
  (:documentation "Implement this to respond to an object being deiconified.")
  (:method (dispatcher widget)
    (declare (ignorable dispatcher widget))))

(defgeneric event-dispose (dispatcher widget)
  (:documentation "Implement this to respond to an object being disposed (via dispose, not the GC).")
  (:method (dispatcher widget)
    (declare (ignorable dispatcher widget))))

(defgeneric event-expand (dispatcher item)
  (:documentation "Implement this to respond to an object (or item within) being expanded.")
  (:method (dispatcher item)
    (declare (ignorable dispatcher item))))

(defgeneric event-focus-gain (dispatcher widget)
  (:documentation "Implement this to respond to an object gaining keyboard focus.")
  (:method (dispatcher widget)
    (declare (ignorable dispatcher widget))))

(defgeneric event-focus-loss (dispatcher widget)
  (:documentation "Implement this to respond to an object losing keyboard focus.")
  (:method (dispatcher widget)
    (declare (ignorable dispatcher widget))))

(defgeneric event-hide (dispatcher widget)
  (:documentation "Implement this to respond to an object being hidden.")
  (:method (dispatcher widget)
    (declare (ignorable dispatcher widget))))

(defgeneric event-iconify (dispatcher widget)
  (:documentation "Implement this to respond to an object being iconified.")
  (:method (dispatcher widget)
    (declare (ignorable dispatcher widget))))

(defgeneric event-key-down (dispatcher widget keycode char)
  (:documentation "Implement this to respond to a key down event.")
  (:method (dispatcher widget keycode char)
    (declare (ignorable dispatcher widget keycode char))))

(defgeneric event-key-up (dispatcher widget keycode char)
  (:documentation "Implement this to respond to a key up event.")
  (:method (dispatcher widget keycode char)
    (declare (ignorable dispatcher widget keycode char))))

(defgeneric event-modify (dispatcher widget)
  (:documentation "Implement this to respond to content (e.g., text) in an object being modified.")
  (:method (dispatcher widget)
    (declare (ignorable dispatcher widget))))

(defgeneric event-mouse-double (dispatcher widget point button)
  (:documentation "Implement this to respond to a mouse double-click.")
  (:method (dispatcher widget point button)
    (declare (ignorable dispatcher widget point button))))

(defgeneric event-mouse-down (dispatcher widget point button)
  (:documentation "Implement this to respond to a mouse down event.")
  (:method (dispatcher widget point button)
    (declare (ignorable dispatcher widget point button))))

(defgeneric event-mouse-enter (dispatcher widget point button)
  (:documentation "Implement this to respond to a mouse passing into the bounds of an object.")
  (:method (dispatcher widget point button)
    (declare (ignorable dispatcher widget point button))))

(defgeneric event-mouse-exit (dispatcher widget point button)
  (:documentation "Implement this to respond to a mouse leaving the bounds an object.")
  (:method (dispatcher widget point button)
    (declare (ignorable dispatcher widget point button))))

(defgeneric event-mouse-hover (dispatcher widget point button)
  (:documentation "Implement this to respond to a mouse that stops moving for a period of time within an object.")
  (:method (dispatcher widget point button)
    (declare (ignorable dispatcher widget point button))))

(defgeneric event-mouse-move (dispatcher widget point button)
  (:documentation "Implement this to respond to a mouse move event.")
  (:method (dispatcher widget point button)
    (declare (ignorable dispatcher widget point button))))

(defgeneric event-mouse-up (dispatcher widget point button)
  (:documentation "Implement this to respond to a mouse up event.")
  (:method (dispatcher widget point button)
    (declare (ignorable dispatcher widget point button))))

(defgeneric event-move (dispatcher widget point)
  (:documentation "Implement this to respond to an object being moved within its parent's coordinate system.")
  (:method (dispatcher widget point)
    (declare (ignorable dispatcher widget point))))

(defgeneric event-paint (dispatcher widget gc rect)
  (:documentation "Implement this to respond to paint requests.")
  (:method (dispatcher widget gc rect)
    (declare (ignorable dispatcher widget gc rect))))

(defgeneric event-pre-modify (dispatcher widget keycode char span new-content)
  (:documentation "Implement this to respond to content (e.g., text) in an object about to be modified.")
  (:method (dispatcher widget keycode char span new-content)
    (declare (ignorable dispatcher widget keycode char span new-content))))

(defgeneric event-pre-move (dispatcher widget rect)
  (:documentation "Implement this to modify widget's move drag rectangle.")
  (:method (dispatcher widget rect)
    (declare (ignorable dispatcher widget rect))))

(defgeneric event-pre-resize (dispatcher widget rect type)
  (:documentation "Implement this to modify widget's resize drag rectangle.")
  (:method (dispatcher widget rect type)
    (declare (ignorable dispatcher widget rect type))))

(defgeneric event-resize (dispatcher widget size type)
  (:documentation "Implement this to respond to widget being resized.")
  (:method (dispatcher widget size type)
    (declare (ignorable dispatcher widget size type))))

(defgeneric event-scroll (dispatcher widget axis detail)
  (:documentation "Implement this to respond to scrolling within widget.")
  (:method (dispatcher widget axis detail)
    (declare (ignorable dispatcher widget axis detail))))

(defgeneric event-select (dispatcher item)
  (:documentation "Implement this to respond to an object (or item within) being selected.")
  (:method (dispatcher item)
    (declare (ignorable dispatcher item))))

(defgeneric event-session (dispatcher window phase reason)
  (:documentation "Implement this to participate in the session shutdown protocol.")
  (:method (dispatcher window phase reason)
    (declare (ignorable dispatcher window phase reason))))

(defgeneric event-timer (dispatcher timer)
  (:documentation "Implement this to respond to a tick from a specific timer.")
  (:method (dispatcher timer)
    (declare (ignorable dispatcher timer))))
