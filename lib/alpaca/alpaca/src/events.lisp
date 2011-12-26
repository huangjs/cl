;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: events.lisp,v 1.1.1.1 2004/03/25 06:43:08 mevins Exp $
;;; 
;;; event utilities

(in-package "CCL")

;;; ----------------------------------------------------------------
;;; utilities for use with NSEvents
;;; ----------------------------------------------------------------

(defun capslock-key-p (nsevent)
  (let ((flags (send nsevent 'modifier-flags)))
	(not (zerop (logand #$NSAlphaShiftKeyMask
						flags)))))

(defun shift-key-p (nsevent)
  (let ((flags (send nsevent 'modifier-flags)))
	(not (zerop (logand #$NSShiftKeyMask
						flags)))))

(defun control-key-p (nsevent)
  (let ((flags (send nsevent 'modifier-flags)))
	(not (zerop (logand #$NSControlKeyMask
						flags)))))

(defun alt-key-p (nsevent)
  (let ((flags (send nsevent 'modifier-flags)))
	(not (zerop (logand #$NSAlternateKeyMask
						flags)))))

;;; doesn't work because command-keys pass from the
;;; top of the containment hierarchy down (backwards)
;;; so that the app gets first crack at handling
;;; menu equivalents. have to look into intercepting
;;; command keys - maybe a custom input server?
(defun command-key-p (nsevent)
  (let ((flags (send nsevent 'modifier-flags)))
	(not (zerop (logand #$NSCommandKeyMask
						flags)))))

;;; true if any key on the numeric keypad is pressed
(defun keypad-key-p (nsevent)
  (let ((flags (send nsevent 'modifier-flags)))
	(not (zerop (logand #$NSNumericPadKeyMask
						flags)))))

(defun help-key-p (nsevent)
  (let ((flags (send nsevent 'modifier-flags)))
	(not (zerop (logand #$NSHelpKeyMask
						flags)))))

;;; true if any function key is pressed
;;; function keys include F1-F12, plus the navigation keys
;;; (Help, Forward Delete, Home End, PgUP, PgDn, arrows
(defun function-key-p (nsevent)
  (let ((flags (send nsevent 'modifier-flags)))
	(not (zerop (logand #$NSFunctionKeyMask
						flags)))))

(defun modifier-keys-p (nsevent)
  (or (capslock-key-p nsevent)
	  (shift-key-p nsevent)
	  (control-key-p nsevent)
	  (alt-key-p nsevent)
	  (command-key-p nsevent)))