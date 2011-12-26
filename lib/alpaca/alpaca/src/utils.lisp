;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: utils.lisp,v 1.1.1.1 2004/03/25 06:43:11 mevins Exp $
;;; 
;;; utility functions

(in-package "CCL")

(defun copy-macptr (mptr)
  (%int-to-ptr (%ptr-to-int mptr)))

