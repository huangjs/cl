;;;;
;;;; widget-generics.lisp
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

(defgeneric accelerator (self)
  (:documentation "Returns a bitmask indicating the key and any modifiers corresponding to the accelerator set for this object."))

(defgeneric activate (self)
  (:documentation "If the object is visible, move it to the top of the display z-order and request the window manager to set it active."))

(defgeneric alignment (self)
  (:documentation "Returns a keyword symbol describing the position of internal content within the object."))

(defgeneric ancestor-p (ancestor descendant)
  (:documentation "Returns T if ancestor is an ancestor of descendant; nil otherwise."))

(defgeneric append-item (self thing dispatcher &optional checked disabled classname)
  (:documentation "Adds a new item encapsulating thing to self, and returns the newly-created item."))

(defgeneric append-separator (self)
  (:documentation "Add a separator item to the object, and returns the newly-created item."))

(defgeneric append-submenu (self text submenu dispatcher &optional checked disabled)
  (:documentation "Adds a submenu anchored to a parent menu and returns the corresponding item."))

(defgeneric auto-hscroll-p (self)
  (:documentation "Returns T if automatic horizontal scrolling is enabled; nil otherwise."))

(defgeneric auto-vscroll-p (self)
  (:documentation "Returns T if automatic vertical scrolling is enabled; nil otherwise."))

(defgeneric bar-position (self)
  (:documentation "Returns the position of self's progress display."))

(defgeneric (setf bar-position) (position self)
  (:documentation "Sets the position of self's progress display."))

(defgeneric border-width (self)
  (:documentation "Returns the object's border width."))

(defgeneric cancel-widget (self)
  (:documentation "Returns the widget that will be activated when the ESC key is pressed."))

(defgeneric (setf cancel-widget) (widget self)
  (:documentation "Sets the widget that will be activated when the ESC key is pressed."))

(defgeneric caret (self)
  (:documentation "Returns the object's caret."))

(defgeneric caret-position (self)
  (:documentation "Returns a point describing the line number and character position of the caret."))

(defgeneric center-on-owner (self)
  (:documentation "Position self such that it is centrally located relative to its owner."))

(defgeneric center-on-parent (self)
  (:documentation "Position self such that it is centrally located relative to its parent."))

(defgeneric check (self flag)
  (:documentation "Sets the object into the checked state."))

(defgeneric check-all (self flag)
  (:documentation "Sets all items in this object to the checked state."))

(defgeneric checked-p (self)
  (:documentation "Returns T if the object is in the checked state; nil otherwise."))

(defgeneric client-size (self)
  (:documentation "Returns a size object that describes the region of the object that can be drawn within or can display data."))

(defgeneric column-at (self index)
  (:documentation "Returns the column object at the zero-based index."))

(defgeneric column-count (self)
  (:documentation "Returns the number of columns displayed by the object."))

(defgeneric column-index (self col)
  (:documentation "Return the zero-based index of the location of the column in this object."))

(defgeneric column-order (self)
  (:documentation "Returns a list of zero-based indices, each of whose positions represents the column creation order and whose element value represents the current column order."))

(defgeneric columns (self)
  (:documentation "Returns the column objects displayed by the object."))

(defgeneric compute-style-flags (self &rest extra-data)
  (:documentation "Convert a list of keyword symbols to a pair of native bitmasks; the first conveys normal/standard flags, whereas the second any extended flags that the system supports."))

(defgeneric compute-outer-size (self desired-client-size)
  (:documentation "Return a size object describing the dimensions of the area required to enclose the specified desired client area and this object's trim."))

(defgeneric copy-text (self)
  (:documentation "Copies the current text selection to the clipboard."))

(defgeneric cut-text (self)
  (:documentation "Copies the current text selection to the clipboard and removes it from self."))

(defgeneric default-widget (self)
  (:documentation "Returns the widget or item that will be selected when self is active."))

(defgeneric (setf default-widget) (self widget)
  (:documentation "Sets the widget or item that will be selected when self is active."))

(defgeneric delete-all (self)
  (:documentation "Removes all content from the object."))

(defgeneric delete-item (self index)
  (:documentation "Removes the item at the zero-based index from the object."))

(defgeneric delete-selection (self)
  (:documentation "Removes items from self that are in the selected state."))

(defgeneric delete-span (self span)
  (:documentation "Removes the sequence of items represented by the specified span object."))

(defgeneric disabled-image (self)
  (:documentation "Returns the image used to render this item with a disabled look."))

(defgeneric display-to-object (self pnt)
  (:documentation "Return a point that is the result of transforming the specified point from display-relative coordinates to this object's coordinate system."))

(defgeneric echo-character (self)
  (:documentation "Returns the character that will be displayed when the user types text, or nil if no echo character has been set."))

(defgeneric enable (self flag)
  (:documentation "Enables or disables the object, causing it to be redrawn with its default look and allows it to be selected."))

(defgeneric enable-auto-scrolling (self horizontal vertical)
  (:documentation "Enables or disables automatic scrolling in either dimension."))

(defgeneric enable-layout (self flag)
  (:documentation "Cause the object to allow or disallow layout management."))

(defgeneric enable-redraw (self flag)
  (:documentation "Cause the object to resume or suspend painting."))

(defgeneric enabled-p (self)
  (:documentation "Returns T if the object is enabled; nil otherwise."))

(defgeneric enable-scrollbars (self horizontal vertical)
  (:documentation "Shows or hides scrollbars for the widget in either dimension."))

(defgeneric expand (self deep flag)
  (:documentation "Set the object (and optionally it's children) to the expanded or collapsed state."))

(defgeneric expanded-p (self)
  (:documentation "Returns T if the object is in the expanded state; nil otherwise."))

(defgeneric focus-index (self)
  (:documentation "Return a zero-based index of the object's sub-item that has focus; nil otherwise."))

(defgeneric focus-p (self)
  (:documentation "Returns T if this object has the keyboard focus; nil otherwise."))

(defgeneric give-focus (self)
  (:documentation "Causes this object to have the keyboard focus."))

(defgeneric grid-line-width (self)
  (:documentation "Returns the width of a grid line."))

(defgeneric header-height (self)
  (:documentation "Returns the height of the item's header."))

(defgeneric header-visible-p (self)
  (:documentation "Returns T if the object's header is visible; nil otherwise."))

(defgeneric horizontal-scrollbar-p (self)
  (:documentation "Returns T if this object currently has a horizontal scrollbar; nil otherwise."))

(defgeneric iconify (self flag)
  (:documentation "Set the object to the iconified or restored state."))

(defgeneric iconified-p (self)
  (:documentation "Returns T if the object is in its iconified state."))

(defgeneric image (self)
  (:documentation "Returns self's image object if it has one, or nil otherwise."))

(defgeneric (setf image) (image self)
  (:documentation "Sets self's image object."))

(defgeneric inner-limits (self)
  (:documentation "Returns the lowest and highest allowed positions of self's indicator."))

(defgeneric (setf inner-limits) (span self)
  (:documentation "Sets the lowest and highest allowed positions of self's indicator."))

(defgeneric item-count (self)
  (:documentation "Returns the number of items contained within self."))

(defgeneric item-height (self)
  (:documentation "Return the height of the area if one of the object's items were displayed."))

(defgeneric item-index (self item)
  (:documentation "Return the zero-based index of the location of the other object in this object."))

(defgeneric items-of (self)
  (:documentation "Returns a list of item subclasses representing the content of self."))

(defgeneric (setf items-of) (items self)
  (:documentation "Accepts a list of application data (or list subclasses) to set the content of self."))

(defgeneric layout (self)
  (:documentation "Set the size and location of this object's children."))

(defgeneric line-count (self)
  (:documentation "Returns the total number of lines (e.g., of text)."))

(defgeneric lines-visible-p (self)
  (:documentation "Returns T if the object's lines are visible; nil otherwise."))

(defgeneric location (self)
  (:documentation "Returns a point object describing the coordinates of the top-left corner of self in its parent's coordinate system."))

(defgeneric (setf location) (point self)
  (:documentation "Sets a point describing the coordinates of self in its parent's coordinate system."))

(defgeneric lock (self flag)
  (:documentation "Prevents or enables modification of the object's contents."))

(defgeneric locked-p (self)
  (:documentation "Returns T if this object's contents are locked from being modified."))

(defgeneric mapchildren (self func)
  (:documentation "Executes func for each direct child of self."))

(defgeneric maximize (self flag)
  (:documentation "Set the object (or restore it from) the maximized state (not necessarily the same as the maximum size)."))

(defgeneric maximized-p (self)
  (:documentation "Returns T if the object is in its maximized state (not necessarily the same as the maximum size); nil otherwise."))

(defgeneric maximum-size (self)
  (:documentation "Returns a size object describing the largest dimensions to which the user may resize self."))

(defgeneric (setf maximum-size) (size self)
  (:documentation "Sets the largest dimensions to which the user may resize self."))

(defgeneric menu-bar (self)
  (:documentation "Returns the menu object serving as the menubar self."))

(defgeneric (setf menu-bar) (menu self)
  (:documentation "Sets the menu object to serve as the menubar for self."))

(defgeneric minimum-size (self)
  (:documentation "Returns a size object describing the smallest supported dimensions of self."))

(defgeneric (setf minimum-size) (size self)
  (:documentation "Sets the smallest supported dimensions of self."))

(defgeneric mouse-over-image (self)
  (:documentation "Returns the image displayed when the mouse is hovering over this object."))

(defgeneric move-above (self other)
  (:documentation "Moves this object above the other object in the drawing order."))

(defgeneric move-below (self other)
  (:documentation "Moves this object below the other object in the drawing order."))

(defgeneric moveable-p (self)
  (:documentation "Returns T if the object is moveable; nil otherwise."))

(defgeneric obtain-horizontal-scrollbar (self)
  (:documentation "Returns a scrollbar object if self has been configured to have one horizontally."))

(defgeneric obtain-vertical-scrollbar (self)
  (:documentation "Returns a scrollbar object if self has been configured to have one horizontally."))

(defgeneric outer-limit (self)
  (:documentation "Returns the zero-based highest possible position of self's indicator."))

(defgeneric (setf outer-limit) (limit self)
  (:documentation "Sets the zero-based highest possible position of self's indicator."))

(defgeneric owner (self)
  (:documentation "Returns self's owner (which is not necessarily the same as parent)."))

(defgeneric pack (self)
  (:documentation "Causes the object to be resized to its preferred size."))

(defgeneric page-increment (self)
  (:documentation "Return an integer representing the configured page size for the object."))

(defgeneric (setf page-increment) (amount self)
  (:documentation "Configures self's page size for scrolling."))

(defgeneric parent (self)
  (:documentation "Returns the object's parent."))

(defgeneric paste-text (self)
  (:documentation "Copies text from the clipboard into self"))

(defgeneric peer (self)
  (:documentation "Returns the visual object associated with this object (not the underlying window system handle)."))

(defgeneric preferred-size (self width-hint height-hint)
  (:documentation "Returns a size object representing the object's 'preferred' size."))

(defgeneric redo-available-p (self)
  (:documentation "Returns T if self can redo an operation; nil otherwise."))

(defgeneric redraw (self)
  (:documentation "Causes the entire bounds of the object to be marked as needing to be redrawn"))

(defgeneric redrawing-p (self)
  (:documentation "Returns T if the object is set to allow processing of paint events."))

(defgeneric reparentable-p (self)
  (:documentation "Returns T if the window system allows this object to be reparented; nil otherwise."))

(defgeneric replace-selection (self content)
  (:documentation "Replaces the content of the current selection with new content."))

(defgeneric resizable-p (self)
  (:documentation "Returns T if the object is resizable; nil otherwise."))

(defgeneric (setf resizable-p) (flag self)
  (:documentation "Pass nil to disable user resizing of self, or non-nil to enable user resizing."))

(defgeneric retrieve-span (self)
  (:documentation "Returns the span object indicating the range of values that are valid for the object."))

(defgeneric scroll (self delta-x delta-y children-p millis)
  (:documentation "Scrolls the contents of self a specified number of pixels."))

(defgeneric select (self flag)
  (:documentation "Set self into (or out of) the selected state."))

(defgeneric select-all (self flag)
  (:documentation "Set all items of this object into (or out of) the selected state."))

(defgeneric selected-count (self)
  (:documentation "Returns the number of this object's items that are selected."))

(defgeneric selected-items (self)
  (:documentation "Returns a list of item subclasses representing selected items in self, or nil if no items are selected."))

(defgeneric (setf selected-items) (items self)
  (:documentation "Updates self's visual display such that the specified items are selected."))

(defgeneric selected-p (self)
  (:documentation "Returns T if the object is in the selected state; nil otherwise."))

(defgeneric selected-span (self)
  (:documentation "Returns a span describing the range of data selected in self, and the selected data."))

(defgeneric (setf selected-span) (span self)
  (:documentation "Updates self's visual display such that the data within span is selected."))

(defgeneric show (self flag)
  (:documentation "Causes the object to be visible or hidden on the screen, but not necessarily top-most in the display z-order."))

(defgeneric show-column (self col)
  (:documentation "This object's colums are scrolled until the specified column is visible."))

(defgeneric show-header (self flag)
  (:documentation "Causes the object's header to be made visible or hidden."))

(defgeneric show-item (self index)
  (:documentation "This object's items are scrolled until the specified item is visible."))

(defgeneric show-lines (self flag)
  (:documentation "Causes the object's lines to be made visible or hidden."))

(defgeneric show-selection (self)
  (:documentation "This object's items are scrolled until the selection is visible."))

(defgeneric size (self)
  (:documentation "Returns the size of self in its parent's coordinate system."))

(defgeneric step (self)
  (:documentation "Causes self's display to animate the next step increment."))

(defgeneric (setf size) (size self)
  (:documentation "Sets the size of self in its parent's coordinate system."))

(defgeneric step-increment (self)
  (:documentation "Return an integer representing the configured step size for self."))

(defgeneric (setf step-increment) (amount self)
  (:documentation "Configures self's step size for scrolling."))

(defgeneric text (self)
  (:documentation "Returns self's text."))

(defgeneric (setf text) (text self)
  (:documentation "Sets self's text."))

(defgeneric text-baseline (self)
  (:documentation "Returns the y coordinate of the baseline of self's text component, if any."))

(defgeneric text-for-pasting-p (self)
  (:documentation "Returns T if the clipboard has data in text format; nil otherwise."))

(defgeneric text-height (self)
  (:documentation "Returns the height of the object's text field."))

(defgeneric text-limit (self)
  (:documentation "Returns the number of characters that the object's text field is capable of holding."))

(defgeneric text-modified-p (self)
  (:documentation "Returns true if the text component has been modified; nil otherwise."))

(defgeneric (setf text-modified-p) (modified self)
  (:documentation "Sets self's modified flag."))

(defgeneric thumb-position (self)
  (:documentation "Returns the position of self's thumb component."))

(defgeneric (setf thumb-position) (position self)
  (:documentation "Sets the position of self's thumb component."))

(defgeneric thumb-track-position (self)
  (:documentation "Returns self's current track position."))

(defgeneric tooltip-text (self)
  (:documentation "Returns the text that will appear within a tooltip when the mouse hovers over this object."))

(defgeneric top-index (self)
  (:documentation "Returns the zero-based index of the item currently at the top of the object."))

(defgeneric traverse (self arg)
  (:documentation "Execute a traversal action within this object."))

(defgeneric traverse-order (self)
  (:documentation "Returns a list of this object's layout-managed children in the order in which tab traversal would visit them."))

(defgeneric undo-available-p (self)
  (:documentation "Returns T if self can undo an operation; nil otherwise."))

(defgeneric update (self)
  (:documentation "Forces all outstanding paint requests for the object to be processed before this function returns."))

(defgeneric update-from-items (self)
  (:documentation "Rebuilds the native control's model of self from self's item list."))

(defgeneric update-native-style (self flags)
  (:documentation "Modifies self's native style flags and refreshes self's visual appearance."))

(defgeneric vertical-scrollbar-p (self)
  (:documentation "Returns T if this object currently has a vertical scrollbar; nil otherwise."))

(defgeneric visible-item-count (self)
  (:documentation "Return the number of items that are currently visible in the object."))

(defgeneric visible-p (self)
  (:documentation "Returns T if the object is visible (not necessarily top-most); nil otherwise."))

(defgeneric window->display (self)
  (:documentation "Return the display object representing the monitor that is nearest to self."))
