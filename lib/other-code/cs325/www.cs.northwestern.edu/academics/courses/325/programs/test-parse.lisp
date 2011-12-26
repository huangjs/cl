;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MOPs and phrases to test parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Defines the following phrases (and associated mops):
;;;
;;;    butterflies => m-butterfly
;;;    bug => m-insect, m-disease
;;;    rat =>  m-rat
;;;    kangaroo => m-kangaroo
;;;    kangaroo rat => m-kangaroo-rat
;;;    catch :object => m-catch-animal, m-get-disease
;;;
;;; Test cases:
;;;
;;;  (test-parse '(catch a butterfly))
;;;  (test-parse '(catch a cold))
;;;  (test-parse '(catch a bug))

(require "mops")
(use-package :mops)

(require "parse")
(use-package :parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indices and stories to test retriever
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmop m-insect (m-animal))
(defmop m-butterfly (m-insect))

(defmop m-rat (m-animal))
(defmop m-kangaroo (m-animal))
(defmop m-kangaroo-rat (m-rat))

(defmop m-disease)
(defmop m-cold-disease (m-disease))

(defmop m-catch-animal (m-action) :object m-animal)
(defmop m-get-disease (m-action) :object m-disease)

(defphrase m-insect bug)
(defphrase m-butterfly a butterfly)
(defphrase m-butterfly butterflies)

(defphrase m-kangaroo kangaroo)
(defphrase m-rat rat)
(defphrase m-kangaroo-rat kangaroo rat)

(defphrase m-disease bug)
(defphrase m-cold-disease cold)

(defphrase m-catch-animal catch :object)
(defphrase m-get-disease catch :object)

(provide "test-parse")


