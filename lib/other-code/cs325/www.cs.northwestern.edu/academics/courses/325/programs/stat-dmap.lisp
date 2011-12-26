
;;; MODULES
;;; -------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "dmap-demo")
  )


;;; PACKAGES
;;; --------

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package "MOP-BROWSER")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clear memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(frames::clear-memory)
(remove-all-cseqs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmop m-statistic (m-root))

(defmop m-question (m-root))

(defmop m-stat-question (m-question)
  :focus m-statistic)

(defmop m-percent-stat-q (m-stat-question)
  :focus m-percent-stat)



(defmop m-percent-stat (m-statistic)
  :group m-root
  :subgroup m-root
  :value nil)

(defmop m-person (m-root))

(defmop m-male-person (m-person)
  :gender m-male)

(defmop m-female-person (m-person)
  :gender m-female)

(defmop m-caucasian-person (m-person)
  :race m-caucasian)

(defmop m-white-female-person
  (m-female-person m-caucasian-person))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define phrases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defphrase m-person people)

(defphrase m-white-female-person
  white women)

(defphrase m-percent-stat
  percentage of (:group) are (:subgroup))

(defphrase m-percent-stat-q
  what (:focus))


(provide "stat-dmap")
