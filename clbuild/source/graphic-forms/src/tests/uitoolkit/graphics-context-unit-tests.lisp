;;;;
;;;; graphics-context-unit-tests.lisp
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

(in-package :graphic-forms.uitoolkit.tests)

(define-test pen-styles-test
  (let ((style1 nil)
        (style2 '(:solid))
        (style3 '(:dash :flat-endcap))
        (style4 '(:dot :miter-join))
        (style5 '(:alternate :flat-endcap :bevel-join)))
    (dotimes (width 3)
      (assert-equal (logior gfs::+ps-cosmetic+
                            gfs::+ps-null+)
                    (gfg::compute-pen-style style1 width)
                    (list style1 width))
      (assert-equal (logior (if (< width 2) gfs::+ps-cosmetic+ gfs::+ps-geometric+)
                            gfs::+ps-solid+)
                    (gfg::compute-pen-style style2 width)
                    (list style2 width))
      (assert-equal (logior gfs::+ps-geometric+
                            gfs::+ps-dash+
                            gfs::+ps-endcap-flat+)
                    (gfg::compute-pen-style style3 width)
                    (list style3 width))
      (assert-equal (logior gfs::+ps-geometric+
                            gfs::+ps-dot+
                            gfs::+ps-join-miter+)
                    (gfg::compute-pen-style style4 width)
                    (list style4 width))
      (assert-equal (logior gfs::+ps-geometric+
                            gfs::+ps-alternate+
                            gfs::+ps-endcap-flat+
                            gfs::+ps-join-bevel+)
                    (gfg::compute-pen-style style5 width)
                    (list style5 width)))))
