(in-package :screen)

;;; initialization

(defgeneric initialize-screen (screen &key &allow-other-keys)
  (:documentation
   "Lambda-list: screen &key &allow-other-keys

Initialize the screen for the given screen object. Extra keyword
arguments are defined by the given screen object type."))
(defgeneric release-screen (screen)
  (:documentation
   "Lambda-list: screen

Release the given screen object. What this means is defined by the
type of the supplied screen object."))

;;; cursor positioning
(defgeneric set-cursor (screen row col)
  (:documentation
   "Lambda-list: screen row col

Move the user's insertion cursor to the given row and column."))
(defgeneric get-cursor (screen)
  (:documentation
   "Lambda-list: screen

Return, in multiple values, the row and column that the user's insertion point is at."))

;;; screen drawing functions

(defgeneric clear-screen (screen)
  (:documentation
   "Lambda-list: screen

Clear the given screen."))
(defgeneric finish-screen (screen)
  (:documentation
   "Lambda-list: screen

Finish all output operations to the given screen."))
(defgeneric write-string-at-cursor (screen string)
  (:documentation
   "Lambda-list: screen string

Write the given string to the screen at the cursor position. If the
input includes a newline character or exceeds the number of columns
following the cursor position, the output will be truncated, not
wrapped."))
(defgeneric erase-from-cursor-to-eol (screen)
  (:documentation
   "Lambda-list: screen

Erase the screen between the cursor point and the end of the row."))
(defgeneric erase-from-cursor-to-eos (screen)
  (:documentation
   "Lambda-list: screen

Erase the screen between the cursor point and the end of the screen."))
(defgeneric draw-line-at-cursor (screen direction length)
  (:documentation
   "Lambda-list: screen direction length

If possible, draw a line of the given length and running in the supplied direction
on the screen. Valid directions are :horizontal and :vertical."))

;;; color functions
(defgeneric valid-color-p (screen name type)
  (:documentation
   "Lambda-list: screen name type

Returns true iff the color designated by the symbol name names a valid
color of the specified type. Valid values for type are :foreground and
:background."))
(defgeneric valid-colors (screen type)
  (:documentation
   "Lambda-list: screen type

Returns a possibly non-exhaustive list of colors which are valid for
the given type on the given screen. Valid types are :foreground and :background."))
(defgeneric set-color (screen foreground background)
  (:documentation
   "Lambda-list: screen foreground background

Set the color of the following text drawing operations to the given
colors named by the symbols foreground and background."))
(defgeneric set-to-default-color (screen)
  (:documentation
   "Lambda-list: screen

Set the color of the following text drawing operations to the system
default foreground and background."))

;;; window sizing
(defgeneric get-screen-size (screen)
  (:documentation
   "Lambda-list: screen

Return multiple values containing the number of rows and columns of
the screen's current size."))
(defgeneric window-resize-hook (screen)
  (:documentation
   "Lambda-list: screen

Return the current hook which is invoked when the window is
resized. Use (setf window-resize-hook) to set the hook or remove it
(by setting it to nil)."))
(defgeneric (setf window-resize-hook) (new-hook screen)
  (:documentation
   "Lambda-list: new-hook screen

Set the window resize hook to the supplied hook, or remove it when passed nil."))

;;; key handling

(defgeneric encode-key (screen key)
  (:documentation
   "Lambda-list: screen key

Encode the given representation of a key into a numeric value."))
(defgeneric decode-key (screen key)
  (:documentation
   "Lambda-list: screen key

Decode the given number into a representation of it as a key."))
(defgeneric key-hook (screen)
  (:documentation
   "Lambda-list: screen

Returns the current hook that is invoked when a key is pressed. Use
(setf key-hook) to set the hook or clear it (by setting it to nil)."))
(defgeneric (setf key-hook) (new-value screen)
  (:documentation
   "Lambda-list: new-value screen

Set the hook which is invoked when a key is pressed, or clear it (when
set to nil)."))

