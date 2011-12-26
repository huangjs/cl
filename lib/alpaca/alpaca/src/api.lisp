;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: api.lisp,v 1.1.1.1 2004/03/25 06:43:07 mevins Exp $
;;; 
;;; the editing api

(in-package "CCL")

(defun key-target ()
  (send (send *NSApp* 'key-window)
		'first-responder))

;;; ----------------------------------------------------------------------
;;; editing and movement commands
;;; ----------------------------------------------------------------------

;;; cancelOperation:
(defun cancel-operation (&optional target)
  (let ((target (or target (key-target))))
	(send target :cancel-operation (%null-ptr))
	t))

;;; capitalizeWord:
(defun capitalize-word (&optional target)
  (let ((target (or target (key-target))))
	(send target :capitalize-word (%null-ptr))
	t))

;;; centerSelectionInVisibleArea:
(defun center-selection (&optional target)
  (let ((target (or target (key-target))))
	(send target :center-selection-in-visible-area (%null-ptr))
	t))

;;; changeCaseOfLetter:
;;; complete:
;;; deleteBackward:
;;; deleteBackwardByDecomposingPreviousCharacter:
(defun delete-backward (&optional target)
  (let ((target (or target (key-target))))
	(send target :delete-backward-by-decomposing-previous-character (%null-ptr))
	t))

;;; deleteForward:
(defun delete-forward (&optional target)
  (let ((target (or target (key-target))))
	(send target :delete-forward (%null-ptr))
	t))

;;; deleteToBeginningOfLine:
;;; deleteToBeginningOfParagraph:
;;; deleteToEndOfLine:
(defun kill (&optional target)
  (let ((target (or target (key-target))))
	(send target :delete-to-end-of-line (%null-ptr))
	t))

;;; deleteToEndOfParagraph:
;;; deleteToMark:
(defun delete-to-mark (&optional target)
  (let ((target (or target (key-target))))
	(send target :delete-to-mark (%null-ptr))
	t))

;;; deleteWordBackward:
(defun delete-word-backward (&optional target)
  (let ((target (or target (key-target))))
	(send target :delete-word-backward (%null-ptr))
	t))

;;; deleteWordForward:
(defun delete-word-forward (&optional target)
  (let ((target (or target (key-target))))
	(send target :delete-word-forward (%null-ptr))
	t))

;;; indent:
;;;   [not implemented]
;;; insertBacktab:
;;; insertNewline:
(defun insert-newline (&optional target)
  (let ((target (or target (key-target))))
	(send target :insert-newline (%null-ptr))
	t))

;;; insertNewlineIgnoringFieldEditor:
;;; insertParagraphSeparator:
;;; insertTab:
(defun insert-tab (&optional target)
  (let ((target (or target (key-target))))
	(send target :insert-tab (%null-ptr))
	t))

;;; insertTabIgnoringFieldEditor:
;;; insertText:
(defun insert-text (string &optional target)
  (let ((target (or target (key-target)))
		(nsstring (%make-nsstring string)))
	(send target :insert-text nsstring)
	t))

;;; lowercaseWord:
(defun lowercase-word (&optional target)
  (let ((target (or target (key-target))))
	(send target :lowercase-word (%null-ptr))
	t))

;;; moveBackward:
(defun backward-char (&optional target)
  (let ((target (or target (key-target))))
	(send target :move-backward (%null-ptr))
	t))

;;; moveBackwardAndModifySelection:
;;; moveDown:
(defun next-line (&optional target)
  (let ((target (or target (key-target))))
	(send target :move-down (%null-ptr))
	t))

;;; moveDownAndModifySelection:
;;; moveForward:
(defun forward-char (&optional target)
  (let ((target (or target (key-target))))
	(send target :move-forward (%null-ptr))
	t))

;;; moveForwardAndModifySelection:
;;; moveLeft:
;;; moveLeftAndModifySelection:
;;; moveRight:
;;; moveRightAndModifySelection:
;;; moveToBeginningOfDocument:
(defun beginning-of-document (&optional target)
  (let ((target (or target (key-target))))
	(send target :move-to-beginning-of-document (%null-ptr))
	t))

;;; moveToBeginningOfLine:
(defun beginning-of-line (&optional target)
  (let ((target (or target (key-target))))
	(send target :move-to-beginning-of-line (%null-ptr))
	t))

;;; moveToBeginningOfParagraph:
(defun beginning-of-paragraph (&optional target)
  (let ((target (or target (key-target))))
	(send target :move-to-beginning-of-paragraph (%null-ptr))
	t))

;;; moveToEndOfDocument:
(defun end-of-document (&optional target)
  (let ((target (or target (key-target))))
	(send target :move-to-end-of-document (%null-ptr))
	t))

;;; moveToEndOfLine:
(defun end-of-line (&optional target)
  (let ((target (or target (key-target))))
	(send target :move-to-end-of-line (%null-ptr))
	t))

;;; moveToEndOfParagraph:
(defun end-of-paragraph (&optional target)
  (let ((target (or target (key-target))))
	(send target :move-to-end-of-paragraph (%null-ptr))
	t))

;;; moveUp:
;;; moveUpAndModifySelection:
;;; moveWordBackward:
(defun backward-word (&optional target)
  (let ((target (or target (key-target))))
	(send target :move-word-backward (%null-ptr))
	t))

;;; moveWordBackwardAndModifySelection:
;;; moveWordForward:
(defun forward-word (&optional target)
  (let ((target (or target (key-target))))
	(send target :move-word-forward (%null-ptr))
	t))

;;; moveWordForwardAndModifySelection:
;;; moveWordLeft:
;;; moveWordRight:
;;; moveWordRightAndModifySelection:
;;; moveWordLeftAndModifySelection:
;;; pageDown:
;;; pageUp:
;;; scrollLineDown:
;;; scrollLineUp:
;;; scrollPageDown:
(defun page-down (&optional target)
  (let ((target (or target (key-target))))
	(send target :scroll-page-down (%null-ptr))
	t))

;;; scrollPageUp:
(defun page-up (&optional target)
  (let ((target (or target (key-target))))
	(send target :scroll-page-up (%null-ptr))
	t))

;;; selectAll:
(defun select-all (&optional target)
  (let ((target (or target (key-target))))
	(send target :select-all (%null-ptr))
	t))

;;; selectLine:
(defun select-line (&optional target)
  (let ((target (or target (key-target))))
	(send target :select-line (%null-ptr))
	t))

;;; selectParagraph:
(defun select-paragraph (&optional target)
  (let ((target (or target (key-target))))
	(send target :select-paragraph (%null-ptr))
	t))

;;; selectToMark:
(defun select-to-mark (&optional target)
  (let ((target (or target (key-target))))
	(send target :select-to-mark (%null-ptr))
	t))

;;; selectWord:
(defun select-word (&optional target)
  (let ((target (or target (key-target))))
	(send target :select-word (%null-ptr))
	t))

;;; setMark:
(defun set-mark (&optional target)
  (let ((target (or target (key-target))))
	(send target :set-mark (%null-ptr))
	t))

;;; showContextHelp:
;;; swapWithMark:
(defun swap-with-mark (&optional target)
  (let ((target (or target (key-target))))
	(send target :swap-with-mark (%null-ptr))
	t))

;;; transpose:
(defun transpose (&optional target)
  (let ((target (or target (key-target))))
	(send target :transpose (%null-ptr))
	t))

;;; transposeWords:
;;;  [not implemented]

;;; uppercaseWord:
(defun uppercase-word (&optional target)
  (let ((target (or target (key-target))))
	(send target :uppercase-word (%null-ptr))
	t))

;;; yank:
(defun yank (&optional target)
  (let ((target (or target (key-target))))
	(send target :yank (%null-ptr))
	t))

;;; ----------------------------------------------------------------------
;;; listener commands
;;; ----------------------------------------------------------------------

(defun insert-previous-history-form (&optional target)
  (let* ((target (or target (key-target)))
		 (max (1- (length *read-history*))))
	(when (> *last-read-history-access-index* max)
	  (setq *last-read-history-access-index* max))
	(let* ((form (elt *read-history* *last-read-history-access-index*))
		   (form-string (%make-nsstring (format nil "~S" form)))
		   (has-marked-text? (send target 'has-marked-text)))
	  (if has-marked-text?
		  (slet ((marked-range (send target 'marked-range)))
				(send target
					  :set-marked-text form-string
					  :selected-range marked-range))
		(slet ((selected-range (send target 'selected-range)))
				(send target
					  :set-marked-text form-string
					  :selected-range selected-range))))
	(incf *last-read-history-access-index*)
	t))

(defun insert-next-history-form (&optional target)
  (let* ((target (or target (key-target)))
		 (min 0))
	(decf *last-read-history-access-index*)
	(when (< *last-read-history-access-index* min)
	  (setq *last-read-history-access-index* min))
	(let* ((form (elt *read-history* *last-read-history-access-index*))
		   (form-string (%make-nsstring (format nil "~S" form)))
		   (has-marked-text? (send target 'has-marked-text)))
	  (if has-marked-text?
		  (slet ((marked-range (send target 'marked-range)))
				(send target
					  :set-marked-text form-string
					  :selected-range marked-range))
		(slet ((selected-range (send target 'selected-range)))
				(send target
					  :set-marked-text form-string
					  :selected-range selected-range))))
	t))

;;; ----------------------------------------------------------------------
;;; keymaps
;;; ----------------------------------------------------------------------

(defclass keymap ()
  ((entries :accessor entries :initform nil :initarg :entries)))

(defparameter *alpaca-keymap* (make-instance 'keymap))

(defmethod copy-keymap ((map keymap))
  (make-instance 'keymap :entries (copy-tree (entries map))))

(defmethod define-key ((map keymap)(signature STRING)(fun FUNCTION))
  (let ((entry (assoc signature (entries map) :test #'equalp)))
	(if entry
		(setf (cdr entry) fun)
	  (setf (entries map)
			(acons signature fun (entries map))))))

(defmethod define-key ((map keymap)(signature STRING)(fun SYMBOL))
  (let ((entry (assoc signature (entries map) :test #'equalp)))
	(if entry
		(setf (cdr entry) (symbol-function fun))
	  (setf (entries map)
			(acons signature (symbol-function fun) (entries map))))))

(defmethod get-key-function ((map keymap)(signature STRING))
  (let ((entry (assoc signature (entries map) :test #'equalp)))
	(when entry (cdr entry))))

(defun key-signature (nsevent)
  (let* ((chars (lisp-string-from-nsstring (send nsevent 'characters-ignoring-modifiers)))
		 (char (elt chars 0))
		 (char-name (or (char-name char) (string char))))
	(concatenate 'string
				 (if (shift-key-p nsevent)
					 "S-"
				   "")
				 (if (control-key-p nsevent)
					 "C-"
				   "")
				 (if (alt-key-p nsevent)
					 "M-"
				   "")
				 char-name)))

(defun get-function-key-name (nsevent)
  (let* ((nschars (send nsevent 'characters-Ignoring-Modifiers))
		 (key-char (send nschars :character-At-Index 0)))
	(cond
	 ((= key-char #$NSUpArrowFunctionKey) "<UP>")
	 ((= key-char #$NSDownArrowFunctionKey) "<DOWN>")
	 ((= key-char #$NSLeftArrowFunctionKey) "<LEFT>")
	 ((= key-char #$NSRightArrowFunctionKey) "<RIGHT>")
	 ((= key-char #$NSF1FunctionKey) "<F1>")
	 ((= key-char #$NSF2FunctionKey) "<F2>")
	 ((= key-char #$NSF3FunctionKey) "<F3>")
	 ((= key-char #$NSF4FunctionKey) "<F4>")
	 ((= key-char #$NSF5FunctionKey) "<F5>")
	 ((= key-char #$NSF6FunctionKey) "<F6>")
	 ((= key-char #$NSF7FunctionKey) "<F7>")
	 ((= key-char #$NSF8FunctionKey) "<F8>")
	 ((= key-char #$NSF9FunctionKey) "<F9>")
	 ((= key-char #$NSF10FunctionKey) "<F10>")
	 ((= key-char #$NSF11FunctionKey) "<F11>")
	 ((= key-char #$NSF12FunctionKey) "<F12>")
	 ((= key-char #$NSF13FunctionKey) "<F13>")
	 ((= key-char #$NSF14FunctionKey) "<F14>")
	 ((= key-char #$NSF15FunctionKey) "<F15>")
	 ((= key-char #$NSF16FunctionKey) "<F16>")
	 ((= key-char #$NSF17FunctionKey) "<F17>")
	 ((= key-char #$NSF18FunctionKey) "<F18>")
	 ((= key-char #$NSF19FunctionKey) "<F19>")
	 ((= key-char #$NSF20FunctionKey) "<F20>")
	 ((= key-char #$NSF21FunctionKey) "<F21>")
	 ((= key-char #$NSF22FunctionKey) "<F22>")
	 ((= key-char #$NSF23FunctionKey) "<F23>")
	 ((= key-char #$NSF24FunctionKey) "<F24>")
	 ((= key-char #$NSF25FunctionKey) "<F25>")
	 ((= key-char #$NSF26FunctionKey) "<F26>")
	 ((= key-char #$NSF27FunctionKey) "<F27>")
	 ((= key-char #$NSF28FunctionKey) "<F28>")
	 ((= key-char #$NSF29FunctionKey) "<F29>")
	 ((= key-char #$NSF30FunctionKey) "<F30>")
	 ((= key-char #$NSF31FunctionKey) "<F31>")
	 ((= key-char #$NSF32FunctionKey) "<F32>")
	 ((= key-char #$NSF33FunctionKey) "<F33>")
	 ((= key-char #$NSF34FunctionKey) "<F34>")
	 ((= key-char #$NSF35FunctionKey) "<F35>")
	 ((= key-char #$NSInsertFunctionKey) "<INSERT>")
	 ((= key-char #$NSDeleteFunctionKey) "<DEL>")
	 ((= key-char #$NSHomeFunctionKey) "<HOME>")
	 ((= key-char #$NSBeginFunctionKey) "<BEGIN>")
	 ((= key-char #$NSEndFunctionKey) "<END>")
	 ((= key-char #$NSPageUpFunctionKey) "<PGUP>")
	 ((= key-char #$NSPageDownFunctionKey) "<PGDN>")
	 ((= key-char #$NSPrintScreenFunctionKey) "<PRINTSCREEN>")
	 ((= key-char #$NSScrollLockFunctionKey) "<SCROLLLOCK>")
	 ((= key-char #$NSPauseFunctionKey) "<PAUSE>")
	 ((= key-char #$NSSysReqFunctionKey) "<SYSREQ>")
	 ((= key-char #$NSBreakFunctionKey) "<BREAK>")
	 ((= key-char #$NSResetFunctionKey) "<RESET>")
	 ((= key-char #$NSStopFunctionKey) "<STOP>")
	 ((= key-char #$NSMenuFunctionKey) "<MENU>")
	 ((= key-char #$NSUserFunctionKey) "<USER>")
	 ((= key-char #$NSSystemFunctionKey) "<SYSTEM>")
	 ((= key-char #$NSPrintFunctionKey) "<PRINT>")
	 ((= key-char #$NSClearLineFunctionKey) "<CLEARLINE>")
	 ((= key-char #$NSClearDisplayFunctionKey) "<CLEARDISPLAY>")
	 ((= key-char #$NSInsertLineFunctionKey) "<INSERTLINE>")
	 ((= key-char #$NSDeleteLineFunctionKey) "<DELETELINE>")
	 ((= key-char #$NSInsertCharFunctionKey) "<INSERTCHAR>")
	 ((= key-char #$NSDeleteCharFunctionKey) "<DELETECHAR>")
	 ((= key-char #$NSPrevFunctionKey) "<PREV>")
	 ((= key-char #$NSNextFunctionKey) "<NEXT>")
	 ((= key-char #$NSSelectFunctionKey) "<SELECT>")
	 ((= key-char #$NSExecuteFunctionKey) "<EXECUTE>")
	 ((= key-char #$NSUndoFunctionKey) "<UNDO>")
	 ((= key-char #$NSRedoFunctionKey) "<REDO>")
	 ((= key-char #$NSFindFunctionKey) "<FIND>")
	 ((= key-char #$NSHelpFunctionKey) "<HELP>")
	 ((= key-char #$NSModeSwitchFunctionKey) "<MODESWITCH>")
	 (t "<unknown function key>"))))

(defun get-modified-key-name (nsevent)
  (format nil "~A~A~A~A~A~A"
		  (or (and (capslock-key-p nsevent) "<CAPSLOCK>-")
			  "")
		  (or (and (shift-key-p nsevent) "S-")
			  "")
		  (or (and (control-key-p nsevent) "C-")
			  "")
		  (or (and (alt-key-p nsevent) "M-")
			  "")
		  (or (and (command-key-p nsevent) "<COMMAND>-")
			  "")
		  (if (function-key-p nsevent)
			  (get-function-key-name nsevent)
			(get-simple-key-name nsevent))))

(defun get-del-key-name (nsevent)
  (format nil "~A~A~A~A~A~A"
		  (or (and (capslock-key-p nsevent) "<CAPSLOCK>-")
			  "")
		  (or (and (shift-key-p nsevent) "S-")
			  "")
		  (or (and (control-key-p nsevent) "C-")
			  "")
		  (or (and (alt-key-p nsevent) "M-")
			  "")
		  (or (and (command-key-p nsevent) "<COMMAND>-")
			  "")
		  "DEL"))

(defun get-simple-key-name (nsevent)
  (if (function-key-p nsevent)
	  (get-function-key-name nsevent)
	  (let ((nschars (send nsevent 'characters-Ignoring-Modifiers)))
		(string-downcase (lisp-string-from-nsstring nschars)))))

(defun get-event-name (nsevent)
  (let ((event-type (send nsevent 'type)))
	(cond
	 ;; mouse events
	 ((= event-type #$NSLeftMouseDown) "<down-mouse-1>")
	 ((= event-type #$NSLeftMouseUp) "<up-mouse-1>")
	 ((= event-type #$NSRightMouseDown) "<down-mouse-2>")
	 ((= event-type #$NSRightMouseUp) "<up-mouse-2>")
	 ((= event-type #$NSOtherMouseDown) "<down-mouse-3>")
	 ((= event-type #$NSOtherMouseUp) "<up-mouse-3>")
	 ((= event-type #$NSMouseMoved) "<moved-mouse>")
	 ((= event-type #$NSLeftMouseDragged) "<moved-mouse-1>")
	 ((= event-type #$NSRightMouseDragged) "<moved-mouse-2>")
	 ((= event-type #$NSOtherMouseDragged) "<moved-mouse-3>")
	 ((= event-type #$NSMouseEntered) "<mouse-entered>")
	 ((= event-type #$NSMouseExited) "<mouse-exited>")
	 ((= event-type #$NSCursorUpdate) "<cursor-update>")
	 ((= event-type #$NSKeyDown)
	  ;; key events
	  (let ((simple-name (get-simple-key-name nsevent)))
		(cond
		 ((eql 127 (char-code (elt simple-name 0))) (get-del-key-name nsevent))
		 ((help-key-p nsevent) "HELP")
		 ((modifier-keys-p nsevent)(get-modified-key-name nsevent))
		 (t simple-name))))
	 ((= event-type #$NSKeyUp) "<up-key>")
	 ((= event-type #$NSFlagsChanged) "<changed-flags>")
	 ((= event-type #$NSAppKitDefined) "<appkit-event>")
	 ((= event-type #$NSSystemDefined) "<system-event>")
	 ((= event-type #$NSApplicationDefined) "<app-event>")
	 ((= event-type #$NSPeriodic) "<periodic-event>")
	 ((= event-type #$NSScrollWheel) "<scroll-wheel>"))))

(defun alpaca-handle-keydown (keyname &optional (keymap *alpaca-keymap*))
  ;; look up the key characters and flags in the Alpaca command table
  ;; that is appropriate to the current mode (when I get around to
  ;; implementing support for modes...) and if a match is found
  ;; execute the key function , otherwise return nil
  ;; if the key function wants keyDown processing to stop when it's done,
  ;; it returns t; if it returns nil then the event will be passed on
  ;; to the Cocoa event-processing code
  (let* ((keyfun (get-key-function keymap keyname)))
	(if keyfun
		(funcall keyfun)
	  nil)))

(defparameter *alpaca-listener-keymap* nil)

(defun init-alpaca-keymaps ()
  (define-key *alpaca-keymap* "C-g" 'cancel-operation)
  (define-key *alpaca-keymap* "M-c" 'capitalize-word) 
  (define-key *alpaca-keymap* "C-l" 'center-selection)
  (define-key *alpaca-keymap* "DEL" 'delete-backward)
  (define-key *alpaca-keymap* "C-d" 'delete-forward)
  (define-key *alpaca-keymap* "C-k" 'kill)
  (define-key *alpaca-keymap* "C-w" 'delete-to-mark)
  (define-key *alpaca-keymap* "C-DEL" 'delete-word-backward)
  (define-key *alpaca-keymap* "M-DEL" 'delete-word-forward)
  (define-key *alpaca-keymap* "C-o" 'insert-newline)
  (define-key *alpaca-keymap* "TAB" 'insert-tab)
  (define-key *alpaca-keymap* "M-l" 'lowercase-word)
  (define-key *alpaca-keymap* "C-b" 'backward-char)
  (define-key *alpaca-keymap* "C-n" 'next-line)
  (define-key *alpaca-keymap* "C-f" 'forward-char)
  (define-key *alpaca-keymap* "S-M-<" 'beginning-of-document)
  (define-key *alpaca-keymap* "C-a" 'beginning-of-line)
  (define-key *alpaca-keymap* "S-M-{" 'beginning-of-paragraph)
  (define-key *alpaca-keymap* "S-M->" 'end-of-document)
  (define-key *alpaca-keymap* "C-e" 'end-of-line)
  (define-key *alpaca-keymap* "S-M-}" 'end-of-paragraph)
  (define-key *alpaca-keymap* "M-b" 'backward-word)
  (define-key *alpaca-keymap* "M-f" 'forward-word)
  (define-key *alpaca-keymap* "M-v" 'page-up)
  (define-key *alpaca-keymap* "C-v" 'page-down)
  (define-key *alpaca-keymap* "M-h" 'select-paragraph)
  (define-key *alpaca-keymap* "C- " 'set-mark)
  (define-key *alpaca-keymap* "C-m" 'select-to-mark)
  (define-key *alpaca-keymap* "C-t" 'transpose)
  (define-key *alpaca-keymap* "M-u" 'uppercase-word)
  (define-key *alpaca-keymap* "C-y" 'yank)
  (setf *alpaca-listener-keymap* (copy-keymap *alpaca-keymap*))
  (define-key *alpaca-listener-keymap* "M-p" 'insert-previous-history-form)
  (define-key *alpaca-listener-keymap* "M-n" 'insert-next-history-form))

