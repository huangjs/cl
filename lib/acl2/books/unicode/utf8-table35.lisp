;; Processing Unicode Files with ACL2
;; Copyright (C) 2005-2006 by Jared Davis <jared@cs.utexas.edu>
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

(in-package "ACL2")
(include-book "uchar")
(include-book "unsigned-byte-listp")
(local (include-book "signed-byte-listp"))
(local (in-theory (enable unsigned-byte-p)))
                          


;; UTF-8 sequences are required to satisfy the informal constraints imposed by
;; Table 3-5, which we recreate below:
;;
;;  Row   Scalar Value                1st Byte  2nd Byte  3rd Byte  4th Byte
;;  1     00000000 0xxxxxxx           0xxxxxxx
;;  2     00000yyy yyxxxxxx           110yyyyy  10xxxxxx
;;  3     zzzzyyyy yyxxxxxx           1110zzzz  10yyyyyy  10xxxxxx
;;  4     000uuuuu zzzzyyyy yyxxxxx   11110uuu  10uuzzzz  10yyyyyy  10xxxxxx
;; 
;; To ensure that these constraints are respected by our UTF-8 functions, we
;; formalize the constraints of this table rigorously below.  
;;
;; Because we are formalizing the Unicode specification here, the thorough
;; reader should check to ensure that our formalization is in agreement with
;; the Unicode standard.  In particular, one should check that our copy of
;; Table 3-5 above is an accurate copy of Table 3-5 from the Standard, and
;; should also convince themselves that the functions below accurately capture
;; the meaning of matching these bit patterns.



;; We begin by explaining which code points are acceptable entries for each row
;; (i.e., in the "scalar value" column).  Note that the code points accepted by
;; each row are subsumed by the later rows, i.e., any codepoint which is
;; acceptable under row 1 is also acceptable under row 2 by simply letting
;; yyyyy = 00000.  We do nothing to try to require least row constraints, since
;; these are handled by Table 3-6.

(defund utf8-table35-codepoint-1? (x)
  "Return true iff x matches 0xxxxxxx, i.e., if it could be a codepoint for
   Row 1 in Table 3-5."
  (declare (xargs :guard (uchar? x)))
  (and (mbt (uchar? x))
       (in-range? (the-fixnum x) 0 #x7F))) ; match 0xxxxxxx

(defund utf8-table35-codepoint-2? (x)
  "Return true iff x matches 00000yyy yyxxxxxx, i.e., if it could be a 
   codepoint for Row 2 in Table 3-5."
  (declare (xargs :guard (uchar? x)))
  (and (mbt (uchar? x))
       (in-range? (the-fixnum x) 0 #x7FF))) ; match 00000yyy yyxxxxxx

(defund utf8-table35-codepoint-3? (x)
  "Return true iff x matches zzzzyyyy yyxxxxxx, i.e., if it could be a 
   codepoint for Row 3 in Table 3-5."
  (declare (xargs :guard (uchar? x)))
  (and (mbt (uchar? x))
       (in-range? (the-fixnum x) 0 #xFFFF))) ; match zzzzyyyy yyxxxxxx

(defund utf8-table35-codepoint-4? (x)
  "Return true iff x matches 000uuuuu zzzzyyyy yyxxxxxx, i.e., if it could
   be a codepoint for Row 4 in Table 3-5."
  (declare (xargs :guard (uchar? x)))
  (and (mbt (uchar? x))
       (in-range? (the-fixnum x) 0 #x1FFFFF))) ; match 000uuuuu zzzzyyyy yyxxxxxx

(deftheory utf8-table35-codepoints
  '(utf8-table35-codepoint-1?
    utf8-table35-codepoint-2?
    utf8-table35-codepoint-3?
    utf8-table35-codepoint-4?))



;; We now rigorously explain what the valid entries are for the "Byte" columns
;; in teh table.  Note that we really only have five bit patterns to consider:
;; one for each row in the 1st Byte column, and the pattern 10xxxxxx which is
;; used universally for all trailing bytes.

(defund utf8-table35-byte-1/1? (x)
  "Return true iff x matches 0xxxxxxx, i.e., it is the first byte of a one-
   byte UTF-8 sequence, as specified by Table 3-5."
  (declare (type (unsigned-byte 8) x))
  (and (mbt (unsigned-byte-p 8 x))
       (in-range? (the-fixnum x) 0 #x7F))) ; match 0xxxxxxx

(defund utf8-table35-byte-1/2? (x)
  "Return true iff x matches 110yyyyy, i.e., if it is the first byte of a 
   two-byte UTF-8 sequence, as specified by Table 3-5."
  (declare (type (unsigned-byte 8) x))
  (and (mbt (unsigned-byte-p 8 x))
       (in-range? (the-fixnum x) #xC0 #xDF))) ; match 110yyyyy

(defund utf8-table35-byte-1/3? (x)
  "Return true iff x matches 1110zzzz, i.e., if it is the first byte of a 
   three-byte UTF-8 sequence, as specified by Table 3-5."
  (declare (type (unsigned-byte 8) x))
  (and (mbt (unsigned-byte-p 8 x))
       (in-range? (the-fixnum x) #xE0 #xEF))) ; match 1110zzzz

(defund utf8-table35-byte-1/4? (x)
  "Return true iff x matches 11110uuu, i.e., if it is the first byte of a
   four-byte UTF-8 sequence, as specified by Table 3-5."
  (declare (type (unsigned-byte 8) x))
  (and (mbt (unsigned-byte-p 8 x))
       (in-range? (the-fixnum x) #xF0 #xF7))) ; match 11110uuu

(defund utf8-table35-trailing-byte? (x)
  "Return true iff x matches 10xxxxxx, i.e., if it is a trailing byte in 
   any of the rows in Table 3-5."
  (declare (type (unsigned-byte 8) x))
  (and (mbt (unsigned-byte-p 8 x))
       (in-range? (the-fixnum x) #x80 #xBF))) ; match 10xxxxxx

(deftheory utf8-table35-bytes
  '(utf8-table35-byte-1/1?
    utf8-table35-byte-1/2?
    utf8-table35-byte-1/3?
    utf8-table35-byte-1/4?
    utf8-table35-trailing-byte?))




;; We now combine our individual cell checking functions (codepoint and byte
;; matching) into checks of full rows.  In particular, we write functions which
;; answer the question, given some codepoint and byte sequence, do these match
;; a particular row of Table 3-5?

(defund utf8-table35-row-1? (cp x)
  "True iff cp is a codepoint and x is a byte sequence which match the
   first row in Table 3-5."
  (declare (xargs :guard (and (uchar? cp)
                              (unsigned-byte-listp 8 x)
                              (equal (len x) 1))))
  (and 
   ;; input checking (these can be inferred from guards)
   (mbt (uchar? cp))
   (mbt (unsigned-byte-listp 8 x))
   (mbt (equal (len x) 1))

   ;; check that the codepoint/bytes are acceptable
   (utf8-table35-codepoint-1? cp)
   (utf8-table35-byte-1/1? (first x)) 
   
   ;; check that the codepoint/bytes correspond as expected
   (= (the-fixnum cp) 
      (the-fixnum (first x)))))

(defund utf8-table35-row-2? (cp x)
  "True iff cp is a codepoint and x is a byte sequence which match the
   second row in Table 3-5."
  (declare (xargs :guard (and (uchar? cp)
                              (unsigned-byte-listp 8 x)
                              (equal (len x) 2))))
  (and 
   ;; input checking (these can be inferred from guards)
   (mbt (uchar? cp))
   (mbt (unsigned-byte-listp 8 x))
   (mbt (equal (len x) 2))

   ;; check that the codepoint/bytes are acceptable
   (utf8-table35-codepoint-2? cp)
   (utf8-table35-byte-1/2? (first x))
   (utf8-table35-trailing-byte? (second x))

   ;; check that the codepoint/bytes correspond as expected
   (mbe :logic (let ((000yyyyy (logand (first x) #x1F))
                     (00xxxxxx (logand (second x) #x3F)))
                 (equal (logior (ash 000yyyyy 6) 
                                00xxxxxx)
                        cp))
        :exec  (let ((000yyyyy (the-fixnum 
                                (logand (the-fixnum (first x)) #x1F)))
                     (00xxxxxx (the-fixnum 
                                (logand (the-fixnum (second x)) #x3F))))
                 (= (the-fixnum 
                     (logior (the-fixnum (ash (the-fixnum 000yyyyy) 6))
                             (the-fixnum 00xxxxxx)))
                    (the-fixnum cp))))))

(defund utf8-table35-row-3? (cp x)
  "True iff cp is a codepoint and x is a byte sequence which match the
   third row in Table 3-5."
  (declare (xargs :guard (and (uchar? cp)
                              (unsigned-byte-listp 8 x)
                              (equal (len x) 3))))
  (and 
   ;; input checking (these can be inferred from guards)
   (mbt (uchar? cp))
   (mbt (unsigned-byte-listp 8 x))
   (mbt (equal (len x) 3))
   
   ;; check that the codepoint/bytes are acceptable
   (utf8-table35-codepoint-3? cp)
   (utf8-table35-byte-1/3? (first x))
   (utf8-table35-trailing-byte? (second x))
   (utf8-table35-trailing-byte? (third x))

   ;; check that the codepoint/bytes correspond as expected
   (mbe :logic (let ((0000zzzz (logand (first x) #x0F))
                     (00yyyyyy (logand (second x) #x3F))
                     (00xxxxxx (logand (third x) #x3F)))
                 (equal (logior (ash 0000zzzz 12)             
                                (ash 00yyyyyy 6)
                                00xxxxxx)
                        cp))
        :exec (let ((0000zzzz (the-fixnum (logand (the-fixnum (first x)) #x0F)))
                    (00yyyyyy (the-fixnum (logand (the-fixnum (second x)) #x3F)))
                    (00xxxxxx (the-fixnum (logand (the-fixnum (third x)) #x3F))))
                (= (the-fixnum 
                    (logior (the-fixnum (ash (the-fixnum 0000zzzz) 12))
                            (the-fixnum (ash (the-fixnum 00yyyyyy) 6))
                            (the-fixnum 00xxxxxx)))
                   (the-fixnum cp))))))

(defund utf8-table35-row-4? (cp x)
  "True iff cp is a codepoint and x is a byte sequence which match the
   third row in Table 3-5."
  (declare (xargs :guard (and (uchar? cp)
                              (unsigned-byte-listp 8 x)
                              (equal (len x) 4))))
  (and 
   ;; input checking (these can be inferred from guards)
   (mbt (uchar? cp))
   (mbt (unsigned-byte-listp 8 x))
   (mbt (equal (len x) 4))

   ;; check that the codepoint/bytes are acceptable
   (utf8-table35-codepoint-4? cp)
   (utf8-table35-byte-1/4? (first x))
   (utf8-table35-trailing-byte? (second x))
   (utf8-table35-trailing-byte? (third x))
   (utf8-table35-trailing-byte? (fourth x))
         
   ;; check that the codepoint/bytes correspond as expected
   (mbe :logic (let ((00000uuu (logand (first x)  #x07))
                     (00uuzzzz (logand (second x) #x3F))
                     (00yyyyyy (logand (third x)  #x3F))
                     (00xxxxxx (logand (fourth x) #x3F)))
                 (equal (logior (ash 00000uuu 18)             
                                (ash 00uuzzzz 12)
                                (ash 00yyyyyy 6)
                                00xxxxxx)
                        cp))
        :exec (let ((00000uuu (the-fixnum (logand (the-fixnum (first x)) #x07)))
                    (00uuzzzz (the-fixnum (logand (the-fixnum (second x)) #x3F)))
                    (00yyyyyy (the-fixnum (logand (the-fixnum (third x)) #x3F)))
                    (00xxxxxx (the-fixnum (logand (the-fixnum (fourth x)) #x3F))))
                (= (the-fixnum
                    (logior (the-fixnum (ash (the-fixnum 00000uuu) 18))
                            (the-fixnum (ash (the-fixnum 00uuzzzz) 12))
                            (the-fixnum (ash (the-fixnum 00yyyyyy) 6))
                            (the-fixnum 00xxxxxx)))
                   (the-fixnum cp))))))

(deftheory utf8-table35-rows
  '(utf8-table35-row-1?
    utf8-table35-row-2?
    utf8-table35-row-3?
    utf8-table35-row-4?))


;; Finally, we combine our row checking functions into a single, universal
;; check to ensure that a codepoint and a sequence of bytes are acceptable
;; under table 3-5.

(defthm utf8-table35-length-lemmas
  (and (implies (utf8-table35-row-1? cp x) (equal (len x) 1))
       (implies (utf8-table35-row-2? cp x) (equal (len x) 2))
       (implies (utf8-table35-row-3? cp x) (equal (len x) 3))
       (implies (utf8-table35-row-4? cp x) (equal (len x) 4)))
  :hints(("Goal" :in-theory (enable utf8-table35-rows))))

(defund utf8-table35-ok? (cp x)
  "True iff cp is a codepoint and x is a byte sequence which match some
   row in Table 3-5."
  (declare (xargs :guard (and (uchar? cp)
                              (unsigned-byte-listp 8 x)
                              (<= 1 (len x))
                              (<= (len x) 4))))
  (mbe :logic (or (utf8-table35-row-1? cp x)
                  (utf8-table35-row-2? cp x)
                  (utf8-table35-row-3? cp x)
                  (utf8-table35-row-4? cp x))
       :exec (case (len x)
               (1 (utf8-table35-row-1? cp x))
               (2 (utf8-table35-row-2? cp x))
               (3 (utf8-table35-row-3? cp x))
               (4 (utf8-table35-row-4? cp x))
               (t nil))))
