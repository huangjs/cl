;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.rsa.examples -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsa-examples.lisp
;;;; Purpose:       Examples of RSA encryption.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsa-examples.lisp,v 1.3 2003/09/10 22:19:26 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)


(defpackage rsm.rsa.examples
  (:use #:cl #:rsm.rsa #:rsm.string))

(in-package rsm.rsa.examples)


;; Example1: RSA encryption.

;; The primes, 1097050686241906813 and 1852326530172341711, 
;; were found by using the function get-next-prob-prime.

(defun example1 ()
  (let (string                          ; String variable.
        e-key                           ; Encryption key.
        d-key                           ; Decryption key.
        encrypt                         ; Encrypted string.
        decrypt                         ; Decrypted string.
        (rsa-keys                       ; RSA keys.
         ;; Generate the encryption and decryption keys based on two primes.    
         (generate-keys 1097050686241906813 
                        1852326530172341711 
                        "my-keys1")))
  
    ;; Put the keys (encrypt/decrypt pair) on the key ring.
    (key->key-ring rsa-keys)
  
    ;; Get the encryption key.
    (setf e-key (rsa-encrypt-key rsa-keys))
  
    ;; Get the decryption key.
    (setf d-key (rsa-decrypt-key rsa-keys))
  
    ;; Read in the file "rsa.lisp" as a single string, string.
    (setf string (rsm.string:file->string "rsa.lisp"))
    
    ;; Encrypt the string.
    (setf encrypt (encrypt e-key string))
    
    ;; Decrypt the string.
    (setf decrypt (decrypt d-key encrypt))
  
    ;; Write out the string to a file.
    (string->file decrypt "rsa-check.lisp")))



  ;; Example2: RSA encryption. (Use make-new-keys to do generate new keys.

(defun example2 ()
  (let (string                          ; String variable.
        e-key                           ; Encryption key.
        d-key                           ; Decryption key.
        encrypt                         ; Encrypted string.
        decrypt                         ; Decrypted string.
        (rsa-keys                       ; RSA keys.
    
         ;; Generate the encryption and decryption based on randomly generated 
         ;; primes of length 20 and 30.
         (make-new-keys 20 30 "my-keys2")))

    ;; Put the keys (encrypt/decrypt pair) on the key ring.
    (key->key-ring rsa-keys)
    
    ;; Get the encryption key.
    (setf e-key (rsa-encrypt-key rsa-keys))
    
    ;; Get the decryption key.
    (setf d-key (rsa-decrypt-key rsa-keys))
    
    ;; Read in the file "rsa.lisp" as a single string, string.
    (setf string (rsm.string:file->string "rsa.lisp"))
    
    ;; Encrypt the string.
    (setf encrypt (encrypt e-key string))
    
    ;; Decrypt the string.
    (setf decrypt (decrypt d-key encrypt))
    
    ;; Write out the string to a file.
    (string->file decrypt "rsa-check.lisp")))
  


;; Example3:  RSA encryption.

;; The two primes below were
;; found by using the function get-next-prob-prime.

(defun example3 ()
  (let (string                          ; String variable.
        e-key                           ; Encryption key.
        d-key                           ; Decryption key.
        encrypt                         ; Encrypted string.
        decrypt                         ; Decrypted string.
        (rsa-keys                       ; RSA keys.  
         ;; Generate the encryption and decryption keys based on two primes.
         (generate-keys 38476500287348273938425793847573747573728374728375840043403232434517131030642101782160894651072231532170521741 
                        239492873492837487683458762348929384827348734587234876283487683645873452339875398745982398749873454564563453398475938459823948799387534598734598723587
                        "my-keys3")))

    ;; Put the keys (encrypt/decrypt pair) on the key ring.
    (key->key-ring rsa-keys)

    ;; Get the encryption key.
    (setf e-key (rsa-encrypt-key rsa-keys))
    
    ;; Get the decryption key.
    (setf d-key (rsa-decrypt-key rsa-keys))
    
    ;; Read in the file "rsa.lisp" as a single string, string.
    (setf string (rsm.string:file->string "rsa.lisp"))
    
    ;; Encrypt the string.
    (setf encrypt (encrypt e-key string))
    
    ;; Decrypt the string.
    (setf decrypt (decrypt d-key encrypt))
    
    ;; Write out the string to a file.
    (string->file decrypt "rsa-check.lisp")))


