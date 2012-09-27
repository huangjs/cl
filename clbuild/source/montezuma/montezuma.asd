;;; ------------------------------------------------- -*- Mode: LISP -*-
;;; Montezuma -- A text search engine based on Ferret/Lucene.
;;;
;;; Copyright 2006 John Wiseman
;;; jjwiseman@yahoo.com
;;; 2006-07-13
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE.txt
;;; file.
;;;
;;; ASDF system definition.

(in-package #:asdf)

(defsystem #:montezuma
    :name "Montezuma"
    :author "John Wiseman <jjwiseman@yahoo.com> et al."
    :maintainer "Leslie P. Polzer <polzer@gnu.org>"
    :version "0.1.3"
    :licence "MIT/Expat"
    :description "Montezuma is a port of the Lucene text search engine library."
    :long-description "Montezuma is a port of the Ferret text search engine library, which is itself a port of the Lucene engine."
    :depends-on (#:cl-ppcre #:cl-fad #:babel)
    :components
    ((:module "src"
	:components
	((:file "package")
	 (:module "util"
		  :components ((:file "while")
			       (:file "porter-stemmer")
			       (:file "streams")
			       (:file "pipes")
			       (:file "mop")
			       (:file "priority-queue"      :depends-on ("while"))
			       (:file "strings")
			       (:file "tables")
			       (:file "bit-vector")
			       (:file "files")
			       (:file "comparable")
			       (:file "parser")
			       (:file "utilities"))
		  :depends-on ("package"))
	 (:module "store"
		  :components ((:file "api")
			       (:file "directory"           :depends-on ("api"))
			       (:file "index-io"            :depends-on ("api"))
			       (:file "buffered-index-io"   :depends-on ("index-io"))
			       (:file "ram-store"           :depends-on ("buffered-index-io" "directory"))
			       (:file "fs-store"            :depends-on ("buffered-index-io" "directory")))
		  :depends-on ("package" "util"))
	 (:module "document"
		  :components ((:file "field")
			       (:file "document"))
		  :depends-on ("package"))
	 (:module "analysis"
		  :components ((:file "token")
			       (:file "token-stream")
			       (:file "token-filters"       :depends-on ("token" "token-stream"))
			       (:file "tokenizers"          :depends-on ("token" "token-stream"))
			       (:file "standard-tokenizer"  :depends-on ("tokenizers"))
			       (:file "analyzers"           :depends-on ("standard-tokenizer")))
		  :depends-on ("package" "store" "util"))
	 (:module "query-parser"
		  :components ((:file "query-parser"))
		  :depends-on ("package" "search"))
	 (:module "search"
		  :components ((:file "api")
			       (:file "similarity" :depends-on ("api"))
			       (:file "boolean-clause" :depends-on ("api"))
			       (:file "scorer" :depends-on ("api"))
			       (:file "score-doc" :depends-on ("api"))
			       (:file "score-doc-comparator" :depends-on ("api"))
			       (:file "filter" :depends-on ("api"))
			       (:file "weight" :depends-on ("api"))
			       (:file "hit-queue" :depends-on ("api"))
			       ;;(:file "explanation")
			       (:file "query" :depends-on ("api"))
			       (:file "term-query" :depends-on ("query"))
			       (:file "boolean-query" :depends-on ("query"))
			       (:file "term-scorer"  :depends-on ("scorer"))
			       (:file "disjunction-sum-scorer" :depends-on ("scorer"))
			       (:file "conjunction-scorer" :depends-on ("scorer"))
			       (:file "req-opt-sum-scorer" :depends-on ("scorer"))
			       (:file "req-excl-scorer" :depends-on ("scorer"))
			       (:file "non-matching-scorer")
			       (:file "boolean-scorer" :depends-on ("disjunction-sum-scorer"
								    "conjunction-scorer"))
			       (:file "filtered-term-enum" :depends-on ("api"))
			       (:file "multi-term-query" :depends-on ("query"))
			       (:file "wildcard-query" :depends-on ("multi-term-query"))
			       (:file "wildcard-term-enum" :depends-on ("filtered-term-enum"))
			       (:file "phrase-query" :depends-on ("query"))
			       (:file "range-query" :depends-on ("query"))
			       (:file "top-docs" :depends-on ("api"))
			       (:file "index-searcher" :depends-on ("api"))
			       ;;                           (:file "sort" :depends-on ("api" "sort-field"))
			       ;;                           (:file "sort-field" :depends-on ("api"))
			       (:file "phrase-positions" :depends-on ("api"))
			       (:file "phrase-scorer" :depends-on ("phrase-positions" "boolean-scorer"))
			       (:file "sloppy-phrase-scorer" :depends-on ("phrase-scorer"))
			       (:file "exact-phrase-scorer" :depends-on ("phrase-scorer")))
		  :depends-on ("package" "index" "store" "util"))
	 (:module "index"
		  :components ((:file "api")
			       (:file "index-filenames")
			       (:file "term"                :depends-on ("api"))
			       (:file "term-info"		 :depends-on ("api"))
			       (:file "term-buffer"         :depends-on ("term"))
			       (:file "field-infos"         :depends-on ("api"))
			       (:file "term-enum"           :depends-on ("api"))
			       (:file "term-doc-enum"       :depends-on ("api"))
			       (:file "term-infos-io"       :depends-on ("api"))
			       (:file "multiple-term-doc-pos-enum" :depends-on ("api"))
			       (:file "term-vector-offset-info" :depends-on ("api"))
			       (:file "segment-term-vector" :depends-on ("api"))
			       (:file "term-vectors-io"     :depends-on ("segment-term-vector"))
			       (:file "segment-term-enum"   :depends-on ("term-infos-io"))
			       (:file "fields-io"           :depends-on ("api"))
			       (:file "compound-file-io"    :depends-on ("api"))
			       (:file "segment-merge-info"  :depends-on ("api"))
			       (:file "segment-merge-queue" :depends-on ("api"))
			       (:file "segment-infos"       :depends-on ("api"))
			       (:file "segment-reader"      :depends-on ("index-reader"
									 "index-filenames"))
			       (:file "multi-reader"        :depends-on ("index-reader"))
			       (:file "index-writer"        :depends-on ("segment-reader"))
			       (:file "document-writer"     :depends-on ("index-writer"))
			       (:file "segment-merger"      :depends-on ("index-filenames" "index-writer"
											   "fields-io" "term-buffer"))
			       (:file "index-reader"        :depends-on ("api"))
			       (:file "index"               :depends-on ("api")))
		  :depends-on ("package" "analysis" "store" "util"))))))

(defmethod perform ((o test-op) (c (eql (find-system '#:montezuma))))
  (declare (ignore o) (ignore c))
  (oos 'load-op '#:montezuma-tests)
  (oos 'test-op '#:montezuma-tests :force t))

(defmethod operation-done-p ((o test-op) (c (eql (find-system '#:montezuma))))
  (declare (ignore o) (ignore c))
  NIL)



(defsystem #:montezuma-tests
  :description "Tests for Montezuma."
  :depends-on (#:montezuma #:trivial-timeout)
  :components
  ((:module "tests"
     :components
      ((:module "unit"
	  :components ((:file "tests")
		       (:module "util"
			 :components ((:file "tc-priority-queue")
				      (:file "tc-tables")
                                      (:file "strings"))
			 :depends-on ("tests"))
                       (:module "regression"
                                :components ((:file "tc-m2k"))
                         :depends-on ("tests"))
		       (:module "store"
			 :components ((:file "tc-store")
				      (:file "tc-ram-store")
				      (:file "tc-fs-store"))
			 :depends-on ("tests"))
		       (:module "document"
			 :components ((:file "tc-field")
				      (:file "tc-document"))
			 :depends-on ("tests"))
		       (:module "analysis"
			 :components ((:file "tc-lowercase-filter")
				      (:file "tc-stop-filter")
				      (:file "tc-porter-stem-filter")
				      (:file "tc-letter-tokenizer")
				      (:file "tc-whitespace-tokenizer")
				      (:file "tc-lowercase-tokenizer")
				      (:file "tc-standard-tokenizer")
				      (:file "tc-analyzer")
				      (:file "tc-stop-analyzer")
				      (:file "tc-whitespace-analyzer")
				      (:file "tc-standard-analyzer"))
			 :depends-on ("tests"))
                       (:module "search"
                         :components ((:file "tc-similarity")
				      (:file "tc-index-searcher")
				      (:file "tc-boolean-subscorer"))
                         :depends-on ("tests"))
		       (:module "query-parser"
			 :components ((:file "tc-query-parser"))
			 :depends-on ("tests"))
		       (:module "index"
			 :components ((:file "tc-term")
				      (:file "tc-term-info")
				      (:file "tc-term-buffer")
				      (:file "tc-field-infos")
				      (:file "tc-term-infos-io")
				      (:file "tc-term-vectors-io")
				      (:file "tc-fields-io" :depends-on ("th-doc"))
				      (:file "tc-compound-file-io")
				      (:file "tc-segment-term-enum")
				      (:file "tc-segment-term-vector")
				      (:file "tc-segment-infos")
				      (:file "th-doc")
				      (:file "tc-index-writer" :depends-on ("th-doc"))
				      (:file "tc-index-reader" :depends-on ("th-doc"))
				      (:file "tc-multiple-term-doc-pos-enum" :depends-on ("th-doc"))
				      (:file "tc-index"))
			 :depends-on ("tests"))))))))

(defmethod perform ((o test-op) (c (eql (find-system '#:montezuma-tests))))
  (declare (ignore o))
  (or (funcall (intern (symbol-name '#:run-tests)
                       (find-package '#:montezuma)))
      (error "test-op on ~S failed." c)))

(defmethod operation-done-p ((o test-op) (c (eql (find-system '#:montezuma-tests))))
  (declare (ignore o) (ignore c))
  NIL)
