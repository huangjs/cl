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
(include-book "utf8-decode")
(include-book "take-bytes")
(local (include-book "open-input-channel"))
(local (include-book "close-input-channel"))
(set-state-ok t)


;; We now want to recreate our utf8=>ustring function but directly using the
;; file reading operations.  We begin by writing equivalents of "take" and 
;; "nthcdr" that operate on byte streams.  We will use (read-byte$-all ...)
;; effectively as the file's contents, and relate all of these operations to
;; it.

(defund read-utf8-fast (channel state acc)
  (declare (xargs :guard (and (state-p state)
                              (symbolp channel)
                              (open-input-channel-p channel :byte state)
                              (ustring? acc))
                  :measure (file-measure channel state)
                  :verify-guards nil))
  (mbe 
   :logic
   (if (and (state-p state)
            (symbolp channel)
            (open-input-channel-p channel :byte state))
       (mv-let (x1 state)
               (read-byte$ channel state)
               (if (not x1)
                   (mv (reverse acc) state)
                 (let ((len1 (utf8-table35-expected-length x1)))
                   (if (not len1)
                       (mv 'fail state)
                     (mv-let (x2-x4 state)
                             (take-bytes (1- len1) channel state)
                             (let* ((x1-x4 (cons x1 x2-x4))
                                    (first (utf8-char=>uchar x1-x4)))
                               (if (not first)
                                   (mv 'fail state)
                                 (read-utf8-fast channel state (cons first acc)))))))))
     (mv 'fail state))
   :exec
   (mv-let 
    (x1 state) 
    (read-byte$ channel state)
    (if (not x1)
        (mv (reverse acc) state)
      (cond 
       
       ((<= (the-fixnum x1) 127)
        ;; Expected length 1.  We don't need to do any further checking; we can
        ;; just recur very quickly.  Note that this will give us very good
        ;; performance for English text, where characters are typically only a
        ;; single byte.
        (read-utf8-fast channel state (cons x1 acc)))

       ((in-range? (the-fixnum x1) 194 223)
        ;; Expected length 2.  (We excluded 192,193 because they are not
        ;; permitted under Table 3-6.)
        (mv-let (x2 state) (read-byte$ channel state)
           (if (and x2 (in-range? (the-fixnum x2) 128 191))
               ;; Manually-inlined utf8-combine2 operation.
               (read-utf8-fast 
                channel state
                (cons 
                 (the-fixnum
                  (logior 
                   (the-fixnum (ash (the-fixnum (logand (the-fixnum x1) 31)) 6))
                   (the-fixnum (logand (the-fixnum x2) 63))))
                 acc))
             (mv 'fail state))))
       
       ((in-range? (the-fixnum x1) 224 239)
        ;; Expected length 3.  (We cover all options here.)
        (mv-let (x2 state) (read-byte$ channel state)
         (mv-let (x3 state) (read-byte$ channel state)
           (if (and x2 x3
                    (cond ((= (the-fixnum x1) 224)  
                           (in-range? (the-fixnum x2) 160 191))
                          ((= (the-fixnum x1) 237)  
                           (in-range? (the-fixnum x2) 128 159))
                          (t 
                           (in-range? (the-fixnum x2) 128 191)))
                    (in-range? (the-fixnum x3) 128 191))
               (read-utf8-fast
                channel state
                (cons 
                 (the-fixnum
                  (logior 
                   (the-fixnum 
                    (ash (the-fixnum (logand (the-fixnum x1) 15)) 12))
                   (the-fixnum 
                    (logior
                     (the-fixnum 
                      (ash (the-fixnum (logand (the-fixnum x2) 63)) 6))
                     (the-fixnum (logand (the-fixnum x3) 63))))))
                 acc))
             (mv 'fail state)))))
                       
       ((in-range? (the-fixnum x1) 240 244)
        ;; Expected length 4.  (We only accept 240-244 because of Table 3-6;
        ;; i.e., we exclude 245, 246, and 247.
        (mv-let (x2 state) (read-byte$ channel state)
         (mv-let (x3 state) (read-byte$ channel state)
          (mv-let (x4 state) (read-byte$ channel state)
            (if (and x2 x3 x4
                     (cond ((= (the-fixnum x1) 240)
                            (in-range? (the-fixnum x2) 144 191))
                           ((= (the-fixnum x1) 244)
                            (in-range? (the-fixnum x2) 128 143))
                           (t 
                            (in-range? (the-fixnum x2) 128 191)))
                     (in-range? (the-fixnum x3) 128 191)
                     (in-range? (the-fixnum x4) 128 191))
                (read-utf8-fast 
                 channel state
                 (cons 
                  (the-fixnum
                   (logior 
                    (the-fixnum 
                     (ash (the-fixnum (logand (the-fixnum x1) 7)) 18))
                    (the-fixnum 
                     (logior 
                      (the-fixnum
                       (ash (the-fixnum (logand (the-fixnum x2) 63)) 12))
                      (the-fixnum
                       (logior 
                        (the-fixnum 
                         (ash (the-fixnum (logand (the-fixnum x3) 63)) 6))
                        (the-fixnum 
                         (logand (the-fixnum x4) 63))))))))
                  acc))
              (mv 'fail state))))))       
       
       ;; This is a little obscure.  As an optimization above, we did not
       ;; consider cases for first byte = 192, 193, 245, 246, and 247, because
       ;; these are not allowed under Table 3-6.
       ;;
       ;; However, utf8-table35-expected-length predics the lengths of these
       ;; as 2, 2, 4, 4, and 4, respectively.  So, for our MBE equivalence, we 
       ;; need to make sure to advance the stream just like we do in the
       ;; :logic mode.
       ((or (= (the-fixnum x1) 192)
            (= (the-fixnum x1) 193))
        (mv-let (x2 state)
                (read-byte$ channel state)
                (declare (ignore x2))
                (mv 'fail state)))

       ((or (= (the-fixnum x1) 245)
            (= (the-fixnum x1) 246)
            (= (the-fixnum x1) 247))
        (mv-let (x2 state)
                (read-byte$ channel state)
                (declare (ignore x2))
                (mv-let (x3 state)
                        (read-byte$ channel state)
                        (declare (ignore x3))
                        (mv-let (x4 state)
                                (read-byte$ channel state)
                                (declare (ignore x4))
                                (mv 'fail state)))))

       (t 
        (mv 'fail state)))))))

(defthm state-p1-of-mv-nth-1-of-read-utf8-fast
  (implies (and (force (state-p1 state))
                (force (open-input-channel-p1 channel :byte state))
                (force (symbolp channel)))
           (state-p1 (mv-nth 1 (read-utf8-fast channel state acc))))
  :hints(("Goal" :in-theory (enable read-utf8-fast))))

(defthm open-input-channel-p1-of-mv-nth-1-of-read-utf8-fast
  (implies (and (force (state-p1 state))
                (force (open-input-channel-p1 channel :byte state))
                (force (symbolp channel)))
           (open-input-channel-p1 channel :byte 
                                  (mv-nth 1 (read-utf8-fast channel state acc))))
  :hints(("Goal" :in-theory (enable read-utf8-fast))))


;; Correctness of read-utf8-fast
;; 
;; We think of (read-byte$-all channel state) as returning the file's contents.
;; We will show that under the appropriate hypotheses, the data returned by
;; (read-utf8-fast channel state acc) is exactly the same as what we would get
;; if we were to first read the entire file's contents, and then apply our UTF8
;; decoding function, utf8=>ustring, to the result.
;;
;; The proof is sort of cute.  We rewrite everything to be in terms of
;; read-byte$-all.  For example, we first show that read-byte$ is nothing more
;; than the car of read-byte$-all.  Similarly, in take-bytes.lisp, we have
;; shown that take-bytes is just the simpler-take of read-byte$-all.

(local (defthm car-of-read-byte$
         (implies (and (force (state-p state))
                       (force (symbolp channel))
                       (force (open-input-channel-p channel :byte state)))
                  (equal (car (read-byte$ channel state))
                         (caar (read-byte$-all channel state))))
         :hints(("Goal" :in-theory (enable read-byte$-all)))))

(local (theory-invariant
        (incompatible (:rewrite car-of-read-byte$)
                      (:definition read-byte$-all))))

(local (defthm read-byte$-all-of-mv-nth-1-of-read-byte$
         (implies (and (force (state-p state))
                       (force (symbolp channel))
                       (force (open-input-channel-p channel :byte state)))
                  (equal 
                   (car (read-byte$-all channel (mv-nth 1 (read-byte$ channel state))))
                   (cdr (car (read-byte$-all channel state)))))
         :hints(("Goal" :in-theory (e/d (read-byte$-all)
                                        (car-of-read-byte$))))))

(local (defthm car-of-read-byte$-all-when-not-caar
         (implies (and (state-p1 state)
                       (symbolp channel)
                       (open-input-channel-p1 channel :byte state)
                       (not (caar (read-byte$-all channel state))))
                  (equal (car (read-byte$-all channel state))
                         nil))
         :hints(("goal" :in-theory (e/d (read-byte$-all)
                                        (car-of-read-byte$))))))

(defthm car-of-read-utf8-fast-is-utf8=>ustring-fast-of-read-byte$-all
  (implies (and (force (state-p1 state))
                (force (open-input-channel-p1 channel :byte state))
                (force (symbolp channel))
                (true-listp acc))
           (equal (car (read-utf8-fast channel state acc))
                  (utf8=>ustring-fast (car (read-byte$-all channel state))
                                      acc)))
  :hints(("Goal" 
          :in-theory (e/d (read-utf8-fast utf8=>ustring-fast)
                          (nthcdr-bytes-2
                           nthcdr-bytes-3
                           nthcdr-bytes-4))
          :induct (read-utf8-fast channel state acc))))





;; Guard verification for read-utf8-fast.
;;
;; This is really messy, because the MBE equivalence is so dramatic.  Note that
;; a lot of this is the same as what we needed for utf8=>ustring-fast.

(encapsulate
 ()

 (local (defthm terrible-lemma-1
          (implies (and (integerp x)
                        (<= 0 x)
                        (<= x 127))
                   (uchar? x))
          :hints(("Goal" :in-theory (enable uchar?)))))

 (local (defthm terrible-lemma-2
          (IMPLIES (AND (force (integerp x1))
                        (force (integerp x2))
                        (< 127 X1)
                        (<= 194 X1)
                        (<= X1 223)
                        (<= 128 X2)
                        (<= X2 191))
                   (UCHAR? (LOGIOR (ASH (LOGAND X1 31) 6)
                                   (LOGAND X2 63))))
          :hints(("Goal" 
                  :in-theory (enable utf8-combine2-guard
                                     utf8-combine2
                                     utf8-table35-bytes
                                     utf8-table36-bytes)          
                  :use ((:instance uchar?-of-utf8-combine2))))))

 (local (defthm terrible-lemma-3
          (IMPLIES (AND (force (integerp x2))
                        (force (integerp x3))
                        (<= 160 X2)
                        (<= X2 191)
                        (<= 128 X3)
                        (<= X3 191))
                   (UCHAR? (LOGIOR 0 (ASH (LOGAND X2 63) 6)
                                   (LOGAND X3 63))))
          :hints(("Goal" 
                  :in-theory (enable utf8-combine3-guard
                                     utf8-combine3
                                     utf8-table35-bytes
                                     utf8-table36-bytes)
                  :use ((:instance uchar?-of-utf8-combine3
                                   (x1 224)))))))

 (local (defthm terrible-lemma-4
          (IMPLIES (AND (force (integerp X1))
                        (force (integerp X2))
                        (force (integerp X3))
                        (<= 224 X1)
                        (<= X1 239)
                        (NOT (EQUAL X1 224))
                        (NOT (EQUAL X1 237))
                        (<= 128 X2)
                        (<= X2 191)
                        (<= 128 X3)
                        (<= X3 191))
                   (UCHAR? (LOGIOR (ASH (LOGAND X1 15) 12)
                                   (ASH (LOGAND X2 63) 6)
                                   (LOGAND X3 63))))
          :hints(("Goal" 
                  :in-theory (enable utf8-combine3-guard
                                     utf8-combine3
                                     utf8-table35-bytes
                                     utf8-table36-bytes)
                  :use ((:instance uchar?-of-utf8-combine3))))))

 (local (defthm terrible-lemma-5
          (IMPLIES (AND (force (integerp x2))
                        (force (integerp x3))
                        (<= 128 X2)
                        (<= X2 159)
                        (<= 128 X3)
                        (<= X3 191))
                   (UCHAR? (LOGIOR 53248 (ASH (LOGAND X2 63) 6)
                                   (LOGAND X3 63))))
          :hints(("Goal" 
                  :in-theory (enable utf8-combine3-guard
                                     utf8-combine3
                                     utf8-table35-bytes
                                     utf8-table36-bytes)
                  :use ((:instance uchar?-of-utf8-combine3
                                   (x1 237)))))))

 (local (defthm terrible-lemma-6
          (IMPLIES (AND (force (integerp x2))
                        (force (integerp x3))
                        (force (integerp x4))
                        (<= 144 X2)
                        (<= X2 191)
                        (<= 128 X3)
                        (<= X3 191)
                        (<= 128 X4)
                        (<= X4 191))
                   (UCHAR? (LOGIOR 0 (ASH (LOGAND X2 63) 12)
                                   (ASH (LOGAND X3 63) 6)
                                   (LOGAND X4 63))))
          :hints(("Goal" 
                  :in-theory (enable utf8-combine4-guard
                                     utf8-combine4
                                     utf8-table35-bytes
                                     utf8-table36-bytes)
                  :use ((:instance uchar?-of-utf8-combine4
                                   (x1 240)))))))
          
 (local (defthm terrible-lemma-7
          (IMPLIES (AND (force (integerp x1))
                        (force (integerp x2))
                        (force (integerp x3))
                        (force (integerp x4))
                        (<= 240 X1)
                        (<= X1 244)
                        (NOT (EQUAL X1 240))
                        (NOT (EQUAL X1 244))
                        (<= 128 X2)
                        (<= X2 191)
                        (<= 128 X3)
                        (<= X3 191)
                        (<= 128 X4)
                        (<= X4 191))
                   (UCHAR? (LOGIOR (ASH (LOGAND X1 7) 18)
                                   (ASH (LOGAND X2 63) 12)
                                   (ASH (LOGAND X3 63) 6)
                                   (LOGAND X4 63))))
          :hints(("Goal" 
                  :in-theory (enable utf8-combine4-guard
                                     utf8-combine4
                                     utf8-table35-bytes
                                     utf8-table36-bytes)
                  :use ((:instance uchar?-of-utf8-combine4))))))
                 
 (local (defthm terrible-lemma-8
          (IMPLIES (AND (force (integerp x2))
                        (force (integerp x3))
                        (force (integerp x4))
                        (<= 128 x2)
                        (<= x2 143)
                        (<= 128 x3)
                        (<= x3 191)
                        (<= 128 x4)
                        (<= x4 191))
                   (UCHAR? (LOGIOR 1048576 (ASH (LOGAND x2 63) 12)
                                   (ASH (LOGAND x3 63) 6)
                                   (LOGAND x4 63))))
          :hints(("Goal" 
                  :in-theory (enable utf8-combine4-guard
                                     utf8-combine4
                                     utf8-table35-bytes
                                     utf8-table36-bytes)
                  :use ((:instance uchar?-of-utf8-combine4
                                   (x1 244)))))))

 (local (include-book "signed-byte-listp"))

 (local (defthm unsigned-byte-listp-8-of-car-of-read-byte$-all-forward
          (implies (and (force (state-p1 state))
                        (force (open-input-channel-p1 channel :byte state))
                        (force (symbolp channel)))
                   (unsigned-byte-listp 8 (car (read-byte$-all channel state))))
          :rule-classes ((:forward-chaining :trigger-terms ((read-byte$-all channel state))))))

 (local (defthm unsigned-byte-listp-8-of-cdr-when-unsigned-byte-listp-8
          (implies (unsigned-byte-listp 8 x)
                   (unsigned-byte-listp 8 (cdr x)))
          :rule-classes ((:forward-chaining))))

 (local (defthm crock
          (implies (unsigned-byte-listp bytes x)
                   (iff (consp x) 
                        x))))

 (local (defthm hideous-lemma-1
          (implies (and (force (state-p1 state))
                        (force (open-input-channel-p1 channel :byte state))
                        (force (symbolp channel))
                        (car (read-byte$-all channel state)))
                   (unsigned-byte-p 8 (caar (read-byte$-all channel state))))
          :rule-classes ((:rewrite)
                         (:forward-chaining 
                          :trigger-terms ((car (read-byte$-all channel state)))))))

 (local (defthm hideous-lemma-2
          (implies (and (force (state-p1 state))
                        (force (open-input-channel-p1 channel :byte state))
                        (force (symbolp channel))
                        (cdar (read-byte$-all channel state)))
                   (unsigned-byte-p 8 (cadar (read-byte$-all channel state))))
          :rule-classes ((:rewrite)
                         (:forward-chaining 
                          :trigger-terms ((cdar (read-byte$-all channel state)))))))

 (local (defthm hideous-lemma-3
          (implies (and (force (state-p1 state))
                        (force (open-input-channel-p1 channel :byte state))
                        (force (symbolp channel))
                        (cddar (read-byte$-all channel state)))
                   (unsigned-byte-p 8 (caddar (read-byte$-all channel state))))
          :rule-classes ((:rewrite)
                         (:forward-chaining 
                          :trigger-terms ((cddar (read-byte$-all channel state)))))
          :hints(("Goal"
                  :in-theory (e/d (read-byte$-all)
                                  (car-of-read-byte$))
                  :expand ((read-byte$-all channel state)
                           (read-byte$-all channel (mv-nth 1 (read-byte$ channel state)))
                           (read-byte$-all 
                            channel 
                            (mv-nth 1 (read-byte$ channel 
                                                  (mv-nth 1 (read-byte$ channel
                                                                        state))))))))))

 (local (defthm hideous-lemma-4
          (implies (and (force (state-p1 state))
                        (force (open-input-channel-p1 channel :byte state))
                        (force (symbolp channel))
                        (cdddar (read-byte$-all channel state)))
                   (unsigned-byte-p 8 (car (cdddar (read-byte$-all channel state)))))
          :rule-classes ((:rewrite)
                         (:forward-chaining 
                          :trigger-terms ((cdddar (read-byte$-all channel state)))))
          :hints(("Goal"
                  :in-theory (e/d (read-byte$-all)
                                  (car-of-read-byte$))
                  :expand ((read-byte$-all channel state)
                           (read-byte$-all channel 
                                           (mv-nth 1 (read-byte$ channel state)))
                           (read-byte$-all 
                            channel 
                            (mv-nth 1 (read-byte$ channel 
                                                  (mv-nth 1 (read-byte$ channel state)))))
                           (read-byte$-all 
                            channel 
                            (mv-nth 1 (read-byte$ 
                                       channel
                                       (mv-nth 1 (read-byte$ 
                                                  channel 
                                                  (mv-nth 1 (read-byte$ channel
                                                                        state))))))))))))

 (local (defthm integerp-when-unsigned-byte-p-8
          (implies (unsigned-byte-p 8 x)
                   (integerp x))))

 (local (defthm signed-byte-p-from-unsigned-byte-p-8
          (implies (and (unsigned-byte-p 8 x)
                        (< 8 (nfix n)))
                   (signed-byte-p n x))))

 (local (defthm len-zero-when-true-listp
          (implies (true-listp x)
                   (equal (equal (len x) 0)
                          (not x)))))

 (local (defthm integer-squeeze-lemma
          (implies (and (syntaxp (quotep n))
                        (integerp n)
                        (< (1- n) x)
                        (< x (1+ n)))
                   (equal (equal x n) 
                          (integerp x)))
          :rule-classes ((:rewrite :backchain-limit-lst 1))))

 (local (defthm unsigned-byte-p-8-when-valid-integer
          (implies (and (<= 0 x)
                        (< x 255))
                   (equal (unsigned-byte-p 8 x)
                          (integerp x)))
          :rule-classes ((:rewrite :backchain-limit-lst 1))
          :hints(("Goal" :in-theory (enable unsigned-byte-p)))))

 (local (include-book "arithmetic-3/bind-free/top" :dir :system))

 (local (defthm nthcdr-bytes-hack
          (implies (and (force (state-p1 state))
                        (force (open-input-channel-p1 channel :byte state))
                        (force (symbolp channel))
                        (force (natp n)))
                   (equal (nthcdr-bytes n channel (mv-nth 1 (read-byte$ channel state)))
                          (nthcdr-bytes (+ 1 n) channel state)))
          :hints(("Goal" 
                  :expand (nthcdr-bytes (+ 1 n) channel state)
                  :in-theory (enable nthcdr-bytes)
                  :do-not-induct t))))

 (local (in-theory (enable unsigned-byte-listp
                           utf8-char=>uchar
                           utf8-table35-bytes
                           utf8-table36-bytes
                           utf8-combine2
                           utf8-combine3
                           utf8-combine4
                           utf8-combine2-guard
                           utf8-combine3-guard
                           utf8-combine4-guard)))

 (verify-guards read-utf8-fast
                :hints(("Subgoal 2"
                        :in-theory (disable unsigned-byte-p-8-when-valid-integer
                                            nthcdr-bytes-hack))))

 )




(defun read-utf8 (filename state)
  (declare (xargs :guard (and (state-p state)
                              (stringp filename))
                  :stobjs state))
  (mv-let (channel state)
          (open-input-channel filename :byte state)
          (if channel
              (mv-let (data state)
                      (read-utf8-fast channel state nil)
                      (let ((state (close-input-channel channel state)))
                        (mv data state)))
            (mv "Error opening file." state))))

(defthm state-p1-of-mv-nth-1-of-read-utf8
  (implies (and (force (state-p1 state))
                (force (stringp filename)))
           (state-p1 (mv-nth 1 (read-utf8 filename state))))
  :hints(("Goal" :in-theory (enable read-utf8))))

(defthm car-of-read-utf8-when-file-cannot-be-opened
  (implies (and (force (state-p1 state))
                (force (stringp filename))
                (stringp (car (read-file-bytes filename state))))
           (equal (car (read-utf8 filename state))
                  (car (read-file-bytes filename state))))
  :hints(("Goal" :in-theory (enable read-file-bytes read-utf8))))
           
(defthm car-of-read-utf8-when-file-can-be-opened
  (implies (and (force (state-p1 state))
                (force (stringp filename))
                (not (stringp (car (read-file-bytes filename state)))))
           (equal (car (read-utf8 filename state))
                  (utf8=>ustring (car (read-file-bytes filename state)))))
  :hints(("Goal" :in-theory (enable read-utf8 read-file-bytes))))
