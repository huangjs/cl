;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for the RSA Encryption Utilities.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.3 2003/09/10 22:19:26 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.rsa
  (:use #:cl)
  (:documentation
   "This package provides RSA encryption functions.

REQUIRES: package rsm.mod.

Some of the functions use an rsa-keys structure that contains an RSA
encryption/decryption pair in the form of two slots. One slot contains the
encrypt-key structure and the other the decrypt-key structure. There is also a
key ring to store RSA encryption/decryption keys (rsa-keys structure).

Export Summary:

decrypt: RSA decryption of a message.
encrypt: RSA encryption of a message.

get-next-prob-prime: Get the next probable prime from a starting position.
get-rand-rel-prime : Generate a random number that is relatively prime.
generate-keys      : Forms the encryption and decryption keys given primes.
generate-prime-pair: Generates two primes of a given length.
prob-prime-p       : Return true if a number is very likely to be prime.

clear-key-ring : Clear the key ring.
remove-key-from-key-ring: Remove a key from the key ring.
key->key-ring  : Associate a name with an rsa-keys structure - add to key ring.
key-ring->file : Write the key ring out to a file.
file->key-ring : Load a key ring from a file.
make-new-keys  : Makes a new RSA encryption/decryption pair(rsa-keys structure).
find-keys      : Return an rsa-keys structure given its name.

find-encryption-key: Find the encryption key associated with a name.
find-decryption-key: Find the decryption key associated with a name.")
   (:export 
    #:decrypt
    #:encrypt
    #:get-next-prob-prime
    #:get-rand-rel-prime
    #:generate-keys
    #:generate-prime-pair
    #:key-ring->file
    #:prob-prime-p
    #:clear-key-ring
    #:remove-key-from-key-ring
    #:file->key-ring
    #:find-keys
    #:find-encryption-key
    #:find-decryption-key
    #:make-new-keys
    #:rsa-encrypt-key
    #:rsa-decrypt-key
    #:key->key-ring))



