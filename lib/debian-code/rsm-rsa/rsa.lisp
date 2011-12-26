;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsa.lisp
;;;; Purpose:       RSA Encryption Utilities.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsa.lisp,v 1.6 2003/10/21 21:00:31 rscottmcintire Exp $
;;;; *************************************************************************


(in-package rsm.rsa)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;;;; VARS AND CONSTANTS.

(defconstant +alphabet-size+ 97
  "The number of characters of the alphabet.")

(defconstant +digits+ "0123456789"
  "All the digits.")

(defconstant +pos-digits+ "123456789"
  "All digits except 0.")

(defconstant +odd-digits+ "13579"
  "All odd digits.")

(defvar *name-key-hash* (make-hash-table :test #'equal)
  "Associates a name with an rsa-keys structure.")



;;;; STRUCTURES.

(defstruct (encrypt-key (:conc-name ek-))
  "Encryption information."
  name                                  ; Name of the key.
  n                                     ; Product of two primes.
  e                                     ; Encryption number.
  block-size                            ; Block size of message text.
  )

(defstruct (decrypt-key (:conc-name dk-))
  "Decryption information."
  name                                  ; Name of the key.
  n                                     ; Product of two primes.
  d                                     ; Decryption number.
  e-phi                                 ; Euler Phi function of n.
  )


(defstruct (rsa-keys (:conc-name rsa-))
  "Encryption and Decryption keys."
  name                                  ; Name of the key.
  encrypt-key                           ; Encryption key.
  decrypt-key                           ; Decryption key.
  )



;;;; FUNCTIONS

(defun get-rand-rel-prime (n)
  "Generate a random number relatively prime to <n> larger than a third of 
<n>, but less than a half of <n>."
  (loop :for rnd = (rsm.mod:+ (floor n 3) 
                          (random (floor n 4)))
                   :then (rsm.mod:+ (floor n 3) (random (floor n 4)))
      :until (= (gcd n rnd) 1)
      :finally (return rnd)))



(defun prob-prime-p (p &key (trials 100))
  "Use Fermat's little theorem: a^(p-1) = 1 mod p if p is prime. 
Raise <trials> random values to the (p-1) power and check that it is 1 mod <p>. 
If this is true of all values, then <p> is likely prime. The probability 
increases when the number of trials, <trials>, is larger."
  (do ((k 1))
      ((= k trials))
    (let ((rnd (random p)))
      (unless (= rnd 0)
        (incf k)
        (unless (= (rsm.mod:^ rnd (1- p) p) 1)
          (return-from prob-prime-p nil)))))
  t)


(defun get-next-prob-prime (p-start &key (trials 100))
  "Find a probable prime starting at <p-start> (p-start assumed odd) and
incrementing by 2 until a probable prime is found.  Test each value
using <trials> number of random values with the function prob-prime-p."
  (when (= (mod p-start 2) 0)
    (incf p-start))
  (loop 
    (when (prob-prime-p p-start :trials trials)
      (return-from get-next-prob-prime p-start))
    (incf p-start 2)))


(defun char->num% (ch)
  "Convert a character to a number in the range [1, +alphabet-size+ - 1]."
  (case ch
    (#\a 96)
    (#\b 1)
    (#\c 2)
    (#\d 3)
    (#\e 4)
    (#\f 5)
    (#\g 6)
    (#\h 7)
    (#\i 8)
    (#\j 9)
    (#\k 10)
    (#\l 11)
    (#\m 12)
    (#\n 13)
    (#\o 14)
    (#\p 15)
    (#\q 16)
    (#\r 17)
    (#\s 18)
    (#\t 19)
    (#\u 20)
    (#\v 21)
    (#\w 22)
    (#\x 23)
    (#\y 24)
    (#\z 25)
    (#\` 26)
    (#\~ 27)
    (#\1 28)
    (#\! 29)
    (#\2 30)
    (#\@ 31)
    (#\3 32)
    (#\# 33)
    (#\4 34)
    (#\$ 35)
    (#\5 36)
    (#\% 37)
    (#\6 38)
    (#\^ 39)
    (#\7 40)
    (#\& 41)
    (#\8 42)
    (#\* 43)
    (#\9 44)
    (#\( 45)
    (#\0 46)
    (#\) 47)
    (#\- 48)
    (#\_ 49)
    (#\= 50)
    (#\+ 51)
    (#\Q 52)
    (#\W 53)
    (#\E 54)
    (#\R 55)
    (#\T 56)
    (#\Y 57)
    (#\U 58)
    (#\I 59)
    (#\O 60)
    (#\P 61)
    (#\[ 62)
    (#\{ 63)
    (#\] 64)
    (#\} 65)
    (#\\ 66)
    (#\| 67)
    (#\A 68)
    (#\S 69)
    (#\D 70)
    (#\F 71)
    (#\G 72)
    (#\H 73)
    (#\J 74)
    (#\K 75)
    (#\L 76)
    (#\; 77)
    (#\: 78)
    (#\' 79)
    (#\" 80)
    (#\Z 81)
    (#\X 82)
    (#\C 83)
    (#\V 84)
    (#\B 85)
    (#\N 86)
    (#\M 87)
    (#\, 88)
    (#\< 89)
    (#\. 90)
    (#\> 91)
    (#\/ 92)
    (#\? 93)
    (#\Space 94)
    (#\NewLine 95)
    (t 90)))


(defun num->char% (num)
  "Convert a number in the range [1, +alphabet-size+ - 1] to a character."
  (case num
    (96 #\a)
    (1 #\b)
    (2 #\c)
    (3 #\d)
    (4 #\e)
    (5 #\f)
    (6 #\g)
    (7 #\h)
    (8 #\i)
    (9 #\j)
    (10 #\k)
    (11 #\l)
    (12 #\m)
    (13 #\n)
    (14 #\o)
    (15 #\p)
    (16 #\q)
    (17 #\r)
    (18 #\s)
    (19 #\t)
    (20 #\u)
    (21 #\v)
    (22 #\w)
    (23 #\x)
    (24 #\y)
    (25 #\z)
    (26 #\`)
    (27 #\~)
    (28 #\1)
    (29 #\!)
    (30 #\2)
    (31 #\@)
    (32 #\3)
    (33 #\#)
    (34 #\4)
    (35 #\$)
    (36 #\5)
    (37 #\%)
    (38 #\6)
    (39 #\^)
    (40 #\7)
    (41 #\&)
    (42 #\8)
    (43 #\*)
    (44 #\9)
    (45 #\()
    (46 #\0)
    (47 #\))
    (48 #\-)
    (49 #\_)
    (50 #\=)
    (51 #\+)
    (52 #\Q)
    (53 #\W)
    (54 #\E)
    (55 #\R)
    (56 #\T)
    (57 #\Y)
    (58 #\U)
    (59 #\I)
    (60 #\O)
    (61 #\P)
    (62 #\[)
    (63 #\{)
    (64 #\])
    (65 #\})
    (66 #\\)
    (67 #\|)
    (68 #\A)
    (69 #\S)
    (70 #\D)
    (71 #\F)
    (72 #\G)
    (73 #\H)
    (74 #\J)
    (75 #\K)
    (76 #\L)
    (77 #\;)
    (78 #\:)
    (79 #\')
    (80 #\")
    (81 #\Z)
    (82 #\X)
    (83 #\C)
    (84 #\V)
    (85 #\B)
    (86 #\N)
    (87 #\M)
    (88 #\,)
    (89 #\<)
    (90 #\.)
    (91 #\>)
    (92 #\/)
    (93 #\?)
    (94 #\Space)
    (95 #\NewLine)
    (t #\.)))


(defun num->block% (num)
  "Convert a number to a string. The number is considered in base 97."
  (let (res)
    (loop :while t :do
     (let ((rem (mod num +alphabet-size+)))
        (setf num (- num rem))
        (push (num->char% rem) res)
        (when (= num 0)
          (return (coerce res 'string)))
        (setf num (/ num +alphabet-size+)))
      :finally (return (coerce res 'string)))))


(defun block->num% (block)
  "Convert a string to a number base 97."
  (let ((sum 0))
    (loop 
      :for chr :across block :do
      (setf sum (cl:+ (cl:* +alphabet-size+ sum) 
                      (char->num% chr))))
    sum))


(defun encrypt (e-key text)
  "Encrypt <text> into a list of numbers. Each number is formed by using the RSA
algorithm on a block of text. The key, <e-key>, contains the numbers n and e
which represent the RSA key encryption pair as well as the block size of the
text to encrypt."
  (unless (encrypt-key-p e-key)
    (error "First argument is not of type encrypt-key."))
  (let ((len (length text))
        encrypt
        (e (ek-e e-key))
        (n (ek-n e-key))
        (block-size (ek-block-size e-key)))
    (loop 
      :with block = nil
        for i :from 0 :below len :do
            (push (char text i) block)
            (when (or (= 0 (mod (1+ i) block-size))
                      (= i (1- len)))
              (push (rsm.mod:^ (block->num% (coerce (reverse block) 'string))
                        e n)
                    encrypt)
              (setf block nil)))
    encrypt))


(defun decrypt (d-key encrypt-list)
  "Decrypt the list of numbers <encrypt-list> into a string. The key, <d-key>,
contains the numbers n and d which represent the RSA key decryption pair. It
also contains an Euler Phi number that is the value of the Euler Phi function 
of n."
  (unless (decrypt-key-p d-key)
    (error "First argument is not of type decrypt-key."))
  (let (decrypt
        (n (dk-n d-key))
        (d (dk-d d-key))
        (euler-phi (dk-e-phi d-key)))
    (loop 
      :with e-num = 0
        for num :in encrypt-list :do
            (setf e-num (rsm.mod:^ num d n :e-phi euler-phi))
            (push (num->block% e-num) decrypt))
  (reduce #'(lambda (x y) (concatenate 'string x y)) decrypt)))
  


(defun generate-keys (p q name &key e)
  "Generate an rsa-key structure given the two primes, <p> and <q>."
  (let* ((n (* p q))
         (e-phi (* (1- p) (1- q)))
         (e (or e (get-rand-rel-prime e-phi)))
         (d (rsm.mod:inverse e e-phi))
         (block-size (floor (log (min p q) 100.0))))
    (let ((e-key (make-encrypt-key :name name :n n :e e :block-size block-size))
          (d-key (make-decrypt-key :name name :n n :d d :e-phi e-phi)))
      (make-rsa-keys :encrypt-key e-key :decrypt-key d-key :name name))))

(defun clear-key-ring ()
  "Clear the key ring."
  (clrhash *name-key-hash*))

(defun remove-key-from-key-ring (key-name)
  "Remove the key named, <key-name>, from the key-ring."
  (remhash key-name *name-key-hash*))

(defun key->key-ring (rsa-keys)
  "Put key, <rsa-keys> on the key ring. That is, store (associate) the 
name of the rsa-keys structure, <rsa-keys>, with its name."
  (let ((key-name (rsa-name rsa-keys)))
    (setf (gethash key-name *name-key-hash*) rsa-keys)))

(defun get-keys (key-name)
  "Get the encryption/decryption structure rsa-keys that is associated with the
name, <name>."
  (gethash key-name *name-key-hash*))

(defun key-ring->file (file-name)
  "Write out the key ring of rsa-keys structures to the file, <file-name>."
  (with-open-file (str file-name :direction :output :if-exists :supersede)
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (format str "~s~%" v)) *name-key-hash*)))

(defun file->key-ring (file-name)
  "Load rsa-keys structures from file, <file-name>, and place on the key ring."
  (with-open-file (str file-name :direction :input)
    (loop with val = (read str nil :eof)
        until (eql :eof val) do          
          (key->key-ring val)
          (setf val (read str nil :eof)))))


(defun find-keys (name)
  "Get an rsa-keys structure associated with name, <name>. This structure 
contains the encryption and decryption keys."
  (gethash name *name-key-hash*))

(defun find-encryption-key (name)
  "Get the rsa encryption structure associated with name, <name>."
  (rsa-encrypt-key (find-keys name)))

(defun find-decryption-key (name)
  "Get the rsa encryption structure associated with name, <name>."
  (rsa-decrypt-key (find-keys name)))

(defun generate-prime-pair (len1 len2 &key (trials 100))
  "Generate two probable primes p,q, of lengths <len1> and <len2>. If used for
RSA encryption, the lengths should differ by a wide margin. This function will
choose p and q so that gcd(p-1, q-1) is small. Use a larger value for <trials>
for more probable primes. It should also (but does not) ensure 
that (p-1) and (q-1) both have large prime factors. This function can be  
time consuming. This function takes 15-20 seconds on a Pentium IV 1.8 GHZ 
when <len1> and <len2> are roughly 100. However, finding primes of lengths 
150 and 170 takes 45 seconds, while finding primes of lengths 250 and 270 
takes ever 3 minutes."
  (let ((time (get-universal-time)))
    (loop :repeat (mod time 10000) :do (random 10))
    (let ((str1 (loop :repeat (1- len1) 
                    collect (char +digits+ (random 10))))
          (str2 (loop :repeat (1- len2) 
                    collect (char +digits+ (random 10)))))
      (setf str1 (cons (char +odd-digits+ (random 5)) str1))
      (setf str2 (cons (char +odd-digits+ (random 5)) str2))
      (let ((p (get-next-prob-prime 
                (parse-integer (coerce (nreverse str1) 'string))
                :trials trials))
            (q (get-next-prob-prime 
                (parse-integer (coerce (nreverse str2) 'string))
                :trials trials)))
        (let* ((gcd (gcd (1- p) (1- q)))
               (min-gcd gcd)
               (min-p p) (min-q q))
          (loop :repeat 10 :do
            (setf p (get-next-prob-prime p :trials 10))
            (setf q (get-next-prob-prime q :trials 10))
            (setf gcd (gcd (1- p) (1- q)))
            (when (< gcd min-gcd)
              (setf min-gcd gcd)
              (setf min-p p)
              (setf min-q q)))
          (if (and (prob-prime-p min-p :trials trials)
                   (prob-prime-p min-q :trials trials))
              (cons min-p min-q)
            (generate-prime-pair len1 len2 :trials trials)))))))


(defun make-new-keys (len1 len2 name &key (trials 100))
  "Make new RSA encryption/decryption pair returned as an rsa-keys structure.
The probable primes used will be randomly generated (use a larger value of
<trials> for more confidence in the primes) and have digit lengths of <len1> and
<len2> respectively. This function can be time consuming. 
This function takes 15-20 seconds on a Pentium IV 1.8 GHZ when <len1> and 
<len2> are roughly 100. However, finding primes of lengths 
150 and 170 takes 45 seconds, while finding primes of lengths 250 and 270 
takes ever 3 minutes."
  (when (< (abs (- len1 len2)) 6)
    (error "Lengths of primes should differ by at least 6."))
  (let ((p-pair (generate-prime-pair len1 len2 :trials trials)))
    (generate-keys (car p-pair) (cdr p-pair) name)))


