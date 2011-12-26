; ACL2 books using the bdd hints
; Copyright (C) 1997  Computational Logic, Inc.

; This book is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This book is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this book; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

; Written by:  Matt Kaufmann
; email:       Matt_Kaufmann@aus.edsr.eds.com
; Computational Logic, Inc.
; 1717 West Sixth Street, Suite 290
; Austin, TX 78703-4776 U.S.A.

(in-package "ACL2")

; These definitions are based on definitions in the FM9001 events files, and
; lead up to a version of the theorem CORE-ALU-IS-V-ALU from
; "~hunt/fm9001-replay/core-alu.events", which had previously proved by Boyer
; and Hunt using Moore's BDD package.

(include-book "bdd-primitives")

(defun p-cell (a an b pa pan pb)
  (b-nand3 (b-nand a pa)
           (b-nand an pan)
           (b-nand b pb)))

(defun g-cell (a an bn ga gan gbn)
  (b-and3 (b-nand a ga)
          (b-nand an gan)
          (b-nand bn gbn)))

(defun alu-cell (c a b mpg)
  (let ((gbn (car mpg))
        (gan (cadr mpg))
        (ga  (caddr mpg))
        (pb  (cadddr mpg))
        (pan (caddddr mpg))
        (pa  (cadddddr mpg))
        (m   (caddddddr mpg)))
    (let ((an (b-not a))
          (bn (b-not b)))
      (let ((p (p-cell a an b pa pan pb))
            (g (g-cell a an bn ga gan gbn))
            (mc (b-nand c m)))
        (let ((z (b-equv3 mc p g)))
          (list p g z))))))

(defun tv-alu-help (c a b mpg tree)
  (if (nlistp tree)
      (alu-cell c (car a) (car b) mpg)
    (let ((a-car (tfirstn a tree))
          (b-car (tfirstn b tree))
          (a-cdr (trestn  a tree))
          (b-cdr (trestn  b tree)))
      (let ((lhs (tv-alu-help c a-car b-car mpg (car tree))))
        (let ((p-car (car lhs))
              (g-car (cadr lhs))
              (sum-car (cddr lhs)))
          (let ((c-car (t-carry c p-car g-car)))
            (let ((rhs (tv-alu-help c-car a-cdr b-cdr mpg (cdr tree))))
              (let ((p-cdr (car rhs))
                    (g-cdr (cadr rhs))
                    (sum-cdr (cddr rhs)))
                (cons (b-and p-car p-cdr)
                      (cons (t-carry g-car p-cdr g-cdr)
                            (append sum-car sum-cdr)))))))))))

(defun shift-or-buf-cntl (c an zero op0 op1 op2 op3)
  (let ((op0- (b-not op0))
        (op1- (b-not op1))
        (op2- (b-not op2)))
    (let ((decode-ror (b-and op0- op1-))
          (decode-asr op0))
      (let ((ror-si (b-and decode-ror c))
            (asr-si (b-and decode-asr an)))
        (let ((si (b-or asr-si ror-si))
              (t1 (b-nand op2- op3))
              (t2 (b-and op0 op1)))
          (list (b-or3 t2 t1 zero)
                si))))))

(defun shift-or-buf (c a an zero op0 op1 op2 op3)
  (let ((pass (car (shift-or-buf-cntl c an zero op0 op1 op2 op3)))
        (si   (cadr (shift-or-buf-cntl c an zero op0 op1 op2 op3))))
    (v-if pass a (v-shift-right a si))))

(defun carry-out-help (a0 result zero op0 op1 op2 op3)
  (let ((result- (b-not result))
        (zero-   (b-not zero))
        (op0-    (b-not op0))
        (op1-    (b-not op1))
        (op2-    (b-not op2))
        (op3-    (b-not op3)))
    (let ((op0    (b-not op0-))
          (op1    (b-not op1-))
          (op2    (b-not op2-))
          (op3    (b-not op3-)))
      
      (b-and (b-nand3 (b-nand4 op3- (b-nand op0- op1-) op2- result)
                      (b-nand3 op3- op2 result-)
                      (b-nand4 op3 op2- (b-nand op0 op1) a0))
             zero-))))

(defun overflow-help (rn an bn zero op0 op1 op2 op3)
  (let ((an-   (b-not an))
        (zero- (b-not zero))
        (op1-  (b-not op1))
        (op2-  (b-not op2))
        (op3-  (b-not op3)))
    (let ((an   (b-not an-))
          (op2  (b-not op2-)))
      (b-if rn
            (b-nor (b-nand (b-nor (b-nand3 op3-
                                           (b-or3 op1- op2- (b-xor an bn))
                                           (b-nand3 op1- op2 an-))
                                  (b-nand (b-nand3 op1 op2- (b-xor an bn))
                                          (b-nand3 op1- op2- an)))
                           zero-)
                   (b-nand3 (b-nand op2 an-)
                            (b-nand3 op0 op1- an)
                            (b-nand op2- an)))
            (b-nor (b-nand (b-nor (b-nand3 op3-
                                           (b-or3 op1- op2- (b-xor an bn))
                                           (b-nand3 op1- op2 an-))
                                  (b-nand (b-nand3 op1 op2- (b-xor an bn))
                                          (b-nand3 op1- op2- an)))
                           zero-)
                   (b-not (b-nand3 (b-nand op2 an-)
                                   (b-nand3 op0 op1- an)
                                   (b-nand op2- an)))))

      )))

(defun core-alu (c a b zero mpg op tree)
  (let ((op0 (car op))
        (op1 (cadr op))
        (op2 (caddr op))
        (op3 (cadddr op)))
    (let ((last-bit (sub1 (len a))))
      (let ((alu-help (tv-alu-help c a b mpg tree)))
        (let ((alu-p   (car alu-help))
              (alu-g   (cadr alu-help))
              (alu-sum (cddr alu-help)))
          (let ((alu-carry (t-carry c alu-p alu-g))
                (out (shift-or-buf c alu-sum (nth (sub1 (len a)) a)
                                   zero op0 op1 op2 op3)))
            (cons (carry-out-help (nth 0 a) alu-carry zero op0 op1 op2 op3)
                  (cons (overflow-help (nth last-bit alu-sum)
                                       (nth last-bit a)
                                       (nth last-bit b)
                                       zero op0 op1 op2 op3)
                        (cons (v-zerop out)
                              out)))))))))

;;;;;;;;;;;; v-alu

(defun cvzbv (carry overflow vector)
  (cons carry (cons overflow (cons (v-zerop vector) vector))))

(defun v-adder (c a b)
  (if (nlistp a)
      (cons (boolfix c) nil)
    (cons (b-xor3 c (car a) (car b))
          (v-adder (b-or (b-and (car a) (car b))
                         (b-or (b-and (car a) c)
                               (b-and (car b) c)))
                   (cdr a)
                   (cdr b)))))

(defun v-adder-carry-out (c a b)
  (nth (len a) (v-adder c a b)))

(defun v-adder-output (c a b)
  (firstn (len a) (v-adder c a b)))

(defun v-adder-overflowp (c a b)
  (b-and (b-equv (nth (sub1 (len a)) a)
                 (nth (sub1 (len b)) b))
         (b-xor (nth (sub1 (len a)) a)
                (nth (sub1 (len a)) (v-adder-output c a b)))))

(defun cvzbv-v-adder (c a b)
  (cvzbv (v-adder-carry-out c a b)
         (v-adder-overflowp c a b)
         (v-adder-output    c a b)))

(defun cvzbv-inc (a)
  (cvzbv-v-adder t a (nat-to-v 0 (len a))))

(defun v-subtracter-carry-out (c a b)
  (b-not (v-adder-carry-out (b-not c) (v-not a) b)))

(defun v-subtracter-overflowp (c a b)
  (v-adder-overflowp (b-not c) (v-not a) b))

(defun v-subtracter-output (c a b)
  (v-adder-output (b-not c) (v-not a) b))

(defun cvzbv-v-subtracter (c a b)
  (cvzbv (v-subtracter-carry-out c a b)
         (v-subtracter-overflowp c a b)
         (v-subtracter-output    c a b)))

(defun cvzbv-neg (a)
  (cvzbv-v-subtracter nil a (nat-to-v 0 (len a))))

(defun cvzbv-dec (a)
  (cvzbv-v-subtracter t (nat-to-v 0 (len a)) a))

(defun v-ror (a si)
  (v-shift-right a si))

(defun cvzbv-v-ror (c a)
  (cvzbv (if (nlistp a) c (nth 0 a)) nil (v-ror a c)))

(defun v-asr (a)
  (v-shift-right a (nth (sub1 (len a)) a)))

(defun cvzbv-v-asr (a)
  (cvzbv (if (listp a) (nth 0 a) nil) nil (v-asr a)))

(defun v-lsr (a)
  (v-shift-right a nil))

(defun cvzbv-v-lsr (a)
  (cvzbv (if (listp a) (nth 0 a) nil) nil (v-lsr a)))

(defun cvzbv-v-not (a)
  (cvzbv nil nil (v-not a)))

(defun v-alu (c a b op)
  (cond ((equal op '(nil nil nil nil)) (cvzbv nil nil (v-buf a)))
        ((equal op '(  t nil nil nil)) (cvzbv-inc a))                   
        ((equal op '(nil   t nil nil)) (cvzbv-v-adder c a b))           
        ((equal op '(  t   t nil nil)) (cvzbv-v-adder nil a b))           
        ((equal op '(nil nil   t nil)) (cvzbv-neg a))                   
        ((equal op '(t   nil   t nil)) (cvzbv-dec a))                   
        ((equal op '(nil   t   t nil)) (cvzbv-v-subtracter c a b))      
        ((equal op '(t     t   t nil)) (cvzbv-v-subtracter nil a b))      
        ((equal op '(nil nil nil   t)) (cvzbv-v-ror c a))               
        ((equal op '(  t nil nil   t)) (cvzbv-v-asr a))   
        ((equal op '(nil   t nil   t)) (cvzbv-v-lsr a))
        ((equal op '(  t   t nil   t)) (cvzbv nil nil (v-xor a b)))         
        ((equal op '(nil nil   t   t)) (cvzbv nil nil (v-or  a b)))
        ((equal op '(  t nil   t   t)) (cvzbv nil nil (v-and a b)))         
        ((equal op '(nil   t   t   t)) (cvzbv-v-not a))           
        (t                 (cvzbv nil nil (v-buf a)))))

(defun carry-in-help (czop)
  (let ((c   (car czop))
        (z   (cadr czop))
        (op0 (caddr czop))
        (op1 (cadddr czop))
        (op2 (caddddr czop))
        (op3 (cadddddr czop)))
    (declare (ignore z))
    (let ((c-   (b-not c))
          (op0- (b-not op0))
          (op1- (b-not op1))
          (op2- (b-not op2))
          (op3- (b-not op3)))
      (let ((c   (b-not c-))
            (op0 (b-not op0-))
            (op1 (b-not op1-))
            (op2 (b-not op2-))
            (op3 (b-not op3-)))

        (b-or (b-nand3 (b-nand3 op1- op2- op3-)
                       (b-nand3 op0- op1- op2)
                       (b-nand3 op0 op1 op2))
              (b-nand3 (b-nand op3 c)
                       (b-nand3 op0- op2- c)
                       (b-nand3 op0- op2 c-)))))))

(defun decode-gen (zero swap op0 op1 op2 op3)
  (let ((zero- (b-not zero))
        (swap- (b-not swap))
        (op0-  (b-not op0))
        (op1-  (b-not op1))
        (op2-  (b-not op2))
        (op3-  (b-not op3)))
    (let ((zero (b-not zero-))
          (swap (b-not swap-))
          (op0  (b-not op0-))
          (op1  (b-not op1-))
          (op2  (b-not op2-))
          (op3  (b-not op3-)))
      (declare (ignore swap))
      (list (b-nand3 (b-nand3 op0 op3 (b-xor op1 op2))
                     (b-nand3 op2 op3- (b-nand op1- swap-))
                     (b-nand3 op1 op2- op3-))
            (b-nor (b-nand (b-nand4 op0 op1 op2- op3)
                           (b-nand3 op2 op3- (b-nand op1- swap-)))
                   zero)
            (b-nor (b-nand3 (b-nand3 op0 op3 (b-xor op1 op2))
                            (b-nand3 op0 op1- op2)
                            (b-nand3 op1 op2- op3-))
                   zero)))))

(defun decode-prop (zero swap op0 op1 op2 op3)
  (let ((zero- (b-not zero))
        (swap- (b-not swap))
        (op0-  (b-not op0))
        (op1-  (b-not op1))
        (op2-  (b-not op2))
        (op3-  (b-not op3)))
    (let ((zerop (b-not zero-))
          (swap  (b-not swap-))
          (op0   (b-not op0-))
          (op1   (b-not op1-))
          (op2   (b-not op2-))
          (op3   (b-not op3-)))
      (declare (ignore zerop))
      (list (b-nand3 (b-nand4 op0- op1- op2 op3) 
                     (b-nand op1 op3-)
                     (b-nand3 op2- op3- swap))
            (b-nor op2- (b-nor op3- (b-nor op0 op1-)))
            (b-and (b-nand3 (b-nand op3 (b-equv op0 op1))
                            (b-nand op2- (b-nand swap op3-))
                            (b-nand4 op0 op1- op2 op3-))
                   zero-)))))

(defun decode-mode (op0 op1 op2 op3)
  (b-nor (b-nor3 op0 op1 op2)
         op3))

(defun mpg (zsop)
  (let ((zero (car zsop))
        (swap (cadr zsop))
        (op0 (caddr zsop))
        (op1 (cadddr zsop))
        (op2 (caddddr zsop))
        (op3 (cadddddr zsop)))
    (append (decode-gen zero swap op0 op1 op2 op3)
            (append (decode-prop zero swap op0 op1 op2 op3)
                    (list (decode-mode op0 op1 op2 op3))))))
