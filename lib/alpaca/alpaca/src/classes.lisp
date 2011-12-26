;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: classes.lisp,v 1.1.1.1 2004/03/25 06:43:07 mevins Exp $
;;; 
;;; objective c class definitions

(in-package "CCL")

;;; ======================================================================
;;; CLASSES
;;; ======================================================================

(def-objc-class alpaca-text-view ns-text-view
  document)

(def-objc-class alpaca-listener-text-view alpaca-text-view
  document)

(def-objc-class alpaca-page-view ns-view
  document
  pager
  printinfo
  textcontainer
  textview
  headerview
  footerview
  pagenumberstring)

(def-objc-class alpaca-pager-view ns-view
  (document :id))

(def-objc-class alpaca-document ns-document
  (tag :int)
  scrollview
  filedata)

(def-objc-class alpaca-rtf-document alpaca-document
  pagerview
  (pagecount :int)
  (startpage :int)
  printinfo
  textstorage
  layoutmanager)

(def-objc-class alpaca-text-document alpaca-document
    textview)

(def-objc-class lisp-application-delegate ns-object)

(def-objc-class lisp-editor-window-controller ns-window-controller
  textview								;The (primary) textview
  packagename					   ;Textfield for package name display
  echoarea							   ;Textfield for message display.
  ((history-count "histcount") :int) ;current history count (for prev/next)
  ((prev-history-count "prevhist") :int) ;value of history-count before last cmd
  )

(def-objc-class lisp-editor-document ns-document
  textview
  filedata
  packagename
  echoarea)

(def-objc-class lisp-listener-window-controller lisp-editor-window-controller
  filehandle							;Filehandle for I/O
  (clientfd :int)					  ;Client (listener)'s side of pty
  (outpos :unsigned)					;Position in textview buffer
  userta							 ;Typing attributes for user input
  systa							  ;Typing attributes for system output
  usercolor						 ;Text foreground color for user input
  )

(def-objc-class lisp-listener-document lisp-editor-document)
