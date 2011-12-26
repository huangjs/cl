(in-package :cl-user)

(defpackage :hjs.util.database
  (:use :cl :cl-fad :hjs.data.tree :hjs.meta.essential :hjs.data.sequence :hjs.meta.lisp)
  (:nicknames :hjs.util.db)
  (:export  #:*default-table-size*
            #:delete-all-rows
            #:delete-rows
            #:do-rows
            #:member-of
            #:insert-row
	    #:make-table
            #:make-column
            #:make-schema
            #:map-rows
            #:matching
            #:not-nullable
            #:nth-row
            #:random-selection
            #:schema
	    #:rows
            #:select
            #:shuffle-table
            #:sort-rows
            #:table
            #:table-size
            #:with-column-values))

(in-package :hjs.util.database)


(defparameter *default-table-size* 10)

(defclass table ()
  ((rows   :accessor rows   :initarg :rows :initform (make-rows))
   (schema :accessor schema :initarg :schema)))

(defun make-table (&key (rows (make-rows)) schema)
  (make-instance 'table :rows rows :schema schema))

(defun make-rows (&optional (size *default-table-size*))
  (make-array size :adjustable t :fill-pointer 0 :initial-element nil))

(defun always-first (&rest args)
  (first args))

(defclass column ()
  ((name               
    :reader name
    :initarg :name
    :initform (error "must provide a name."))

   (equality-predicate
    :reader equality-predicate
    :initarg :equality-predicate
    :initform (error "must provide a equality-predicate."))

   (comparator
    :reader comparator
    :initarg :comparator
    :initform (error "must provide a comparator.")
    :documentation "Note: compare by denormalized values.")

   (default-value
     :reader default-value
     :initarg :default-value
     :initform (error "must provide a default value."))

   (value-normalizer
    :reader value-normalizer
    :initarg :value-normalizer
    :initform #'always-first))
  (:documentation
   "The equality-predicate and comparator slots of a column object hold functions used to compare values from the given column for equivalence and ordering. Thus, a column containing string values might have STRING= as its equality-predicate and STRING< as its comparator, while a column containing numbers might have = and <.

The default-value and value-normalizer slots are used when inserting rows into the database and, in the case of value-normalizer, when querying the database. When you insert a row into the database, if no value is provided for a particular column, you can use the value stored in the column's default-value slot. Then the value--defaulted or otherwise--is normalized by passing it and the column object to the function stored in the value-normalizer slot. You pass the column in case the value-normalizer function needs to use some data associated with the column object. (You'll see an example of this in the next section.) You should also normalize values passed in queries before comparing them with values in the database.

Thus, the value-normalizer's responsibility is primarily to return a value that can be safely and correctly passed to the equality-predicate and comparator functions. If the value-normalizer can't figure out an appropriate value to return, it can signal an error.

The other reason to normalize values before you store them in the database is to save both memory and CPU cycles. For instance, if you have a column that's going to contain string values but the number of distinct strings that will be stored in the column is small--for instance, the genre column in the MP3 database--you can save space and speed by using the value-normalizer to intern the strings (translate all STRING= values to a single string object). Thus, you'll need only as many strings as there are distinct values, regardless of how many rows are in the table, and you can use EQL to compare column values rather than the slower STRING="))

(defgeneric make-column (name type &optional default-value))

(defmethod make-column (name (type (eql 'string)) &optional default-value)
  (make-instance
   'column 
   :name name
   :comparator #'string< 
   :equality-predicate #'string=
   :default-value default-value
   :value-normalizer #'not-nullable))

(defmethod make-column (name (type (eql 'number)) &optional default-value)
  (make-instance 
   'column
   :name name
   :comparator #'< 
   :equality-predicate #'=
   :default-value default-value
   :value-normalizer #'not-nullable))

(defun not-nullable (value column)
  "The following function, not-nullable, used as the value-normalizer for string columns, simply returns the value it's given unless the value is NIL, in which case it signals an error.
  This is important because STRING< and STRING= will signal an error if called on NIL; it's better to catch bad values before they go into the table rather than when you try to use them."
  (or value (error "Column ~a can't be null" (name column))))

(defclass interned-values-column (column)
  ((interned-values
    :reader interned-values
    :initform (make-hash-table :test #'equal))
   (equality-predicate :initform #'eql)
   (value-normalizer   :initform #'intern-for-column)))

(defun intern-for-column (value column)
  (let ((hash (interned-values column)))
    (or (gethash (not-nullable value column) hash)
	(setf (gethash value hash) value))))

(defmethod make-column (name (type (eql 'interned-string)) &optional default-value)
  (make-instance 
   'interned-values-column
   :name name
   :comparator #'string<	  ; comparator with denormalized value
   :default-value default-value))

(defun make-schema (spec)
  "For instance, you can define the schema for the table you'll use to store data extracted from MP3s like this:

\(defparameter *mp3-schema* 
  (make-schema 
   '((:file     string)
     (:genre    interned-string \"Unknown\")
     (:artist   interned-string \"Unknown\")
     (:album    interned-string \"Unknown\")
     (:song     string)
     (:track    number 0)
     (:year     number 0)
     (:id3-size number))))
To make an actual table for holding information about MP3s, you pass *mp3-schema* as the :schema initarg to MAKE-INSTANCE.

\(defparameter *mp3s* (make-instance 'table :schema *mp3-schema*))"
  (mapcar #'(lambda (column-spec) (apply #'make-column column-spec)) spec))

(defun insert-row (names-and-values table)
  (vector-push-extend (normalize-row names-and-values (schema table)) (rows table)))

(defun normalize-row (names-and-values schema)
  (loop
     for column in schema
     for name  = (name column)
     for value = (or (getf names-and-values name) (default-value column))
     collect name
     collect (normalize-for-column value column)))

(defun normalize-for-column (value column)
  (funcall (value-normalizer column) value column))

(defun column-names (table)
  "return all the columns in a table or schema."
  (loop
     for column in (schema table)
     for name = (name column)
     collect name))

(defun select (&key (columns t) (from (error "must provide a from.")) where distinct order-by)
  "Here are some examples of using select:

;; Select all rows where the :artist column is \"Green Day\"
\(select :from *mp3s* :where (matching *mp3s* :artist \"Green Day\"))

;; Select a sorted list of artists with songs in the genre \"Rock\"
\(select
  :columns :artist
  :from *mp3s*
  :where (matching *mp3s* :genre \"Rock\")
  :distinct t
  :order-by :artist)"
  (let ((rows (rows from))
        (schema (schema from))
	(result '()))
    (let* ((where-fn (or where (constantly t)))
	   (column-names (if (eql columns 't)
			     (column-names from)
			     (ensure-list columns)))
	   (order-by (if order-by (ensure-list order-by)))
	   (schema (extract-schema column-names schema)))
      (setf result
	    (loop
	       with result = (make-rows)
	       for row across rows
	       when (funcall where-fn row)
	       do (vector-push-extend (project-row-on-columns row column-names) result)
	       finally
	       (progn
		 (when distinct
		   (setf result (remove-duplicates result :test (row-equality-tester schema))))
		 (return (if order-by
			     (sort result (row-comparator order-by schema))
			     result)))))
      ;; finally 
      (make-instance 'table :rows result :schema schema))))

(defun project-row-on-columns (row columns)
  (if (eql columns 't)
      row
      (loop for c in columns
	 collect c
	 collect (getf row c))))

(defun extract-schema (column-names schema)
  (loop for c in column-names collect (find-column c schema)))

(defun find-column (column-name schema)
  (or (find column-name schema :key #'name)
      (error "No column: ~a in schema: ~a" column-name schema)))

(defun row-equality-tester (schema)
  (let ((names (mapcar #'name schema))
        (tests (mapcar #'equality-predicate schema)))
    #'(lambda (a b)
        (loop for name in names and test in tests
           always (funcall test (getf a name) (getf b name))))))

(defun row-comparator (column-names schema)
  (let ((comparators (mapcar #'comparator (extract-schema column-names schema))))
    #'(lambda (a b)
        (loop
           for name in column-names
           for comparator in comparators
           for a-value = (getf a name)
           for b-value = (getf b name)
           when (funcall comparator a-value b-value) return t
           when (funcall comparator b-value a-value) return nil
           finally (return nil)))))

;;; common idioms
(defun matching (table &rest names-and-values)
  "Build a where function that matches rows with the given column values.

The workhouse query-function constructor will be matching, which returns a function that will match rows with specific column values. You saw how it was used in the earlier examples of select. For instance, this call to matching:

\(matching *mp3s* :artist \"Green Day\")
returns a function that matches rows whose :artist value is \"Green Day\". You can also pass multiple names and values; the returned function matches when all the columns match. For example, the following returns a closure that matches rows where the artist is \"Green Day\" and the album is \"American Idiot\": ;

\(matching *mp3s* :artist \"Green Day\" :album \"American Idiot\")
"
  (let ((matchers (column-matchers (schema table) names-and-values)))
    #'(lambda (row)
        (every #'(lambda (matcher) (funcall matcher row)) matchers))))

(defun column-matcher (column value)
  (let ((name (name column))
        (predicate (equality-predicate column))
        (normalized (normalize-for-column value column)))
    #'(lambda (row) (funcall predicate (getf row name) normalized))))

(defun column-matchers (schema names-and-values)
  (loop for (name value) on names-and-values by #'cddr
     when value collect
     (column-matcher (find-column name schema) value)))

(defun member-of (column-name table)
  "Another matching function that you'll occasionally find useful is in, which returns a function that matches rows where a particular column is in a given set of values. You'll define in to take two arguments: a column name and a table that contains the values you want to match. For instance, suppose you wanted to find all the songs in the MP3 database that have names the same as a song performed by the Dixie Chicks. You can write that where clause using in and a subselect like this:4

\(select
 :columns '(:artist :song)
 :from *mp3s*
 :where (in :song 
			(select
			 :columns :song
			 :from *mp3s*
			 :where (matching *mp3s* :artist \"Dixie Chicks\"))))"
  (let ((test (equality-predicate (find-column column-name (schema table))))
        (values (map 'list #'(lambda (r) (getf r column-name)) (rows table))))
    #'(lambda (row)
        (member (getf row column-name) values :test test))))

(defmacro do-rows ((row table) &body body)
  "(do-rows (row table)
  (with-column-values (song artist album) row
					  (format t \"~a by ~a from ~a~%\" song artist album)))"
  `(loop for ,row across (rows ,table) do ,@body))

(defun map-rows (fn table)
  (loop for row across (rows table) collect (funcall fn row)))

(defun column-value (row column-name)
  (getf row column-name))

(defmacro with-column-values ((&rest vars) row &body body)
  (once-only (row)
    `(let ,(column-bindings vars row) ,@body)))

(defun column-bindings (vars row)
  (loop for v in vars collect `(,v (column-value ,row ,(as-keyword v)))))

(defun as-keyword (symbol)
  (intern (symbol-name symbol) :keyword))

(defun table-size (table)
  (length (rows table)))

(defun nth-row (n table)
  (aref (rows table) n))

(defun delete-rows (&key from where)
  (loop
     with rows = (rows from)
     with store-idx = 0
     for read-idx from 0
     for row across rows
     do (setf (aref rows read-idx) nil)
     unless (funcall where row) do
     (setf (aref rows store-idx) row)
     (incf store-idx)
     finally (setf (fill-pointer rows) store-idx)))

(defun delete-all-rows (table)
  (setf (rows table) (make-rows *default-table-size*)))

(defun sort-rows (table &rest column-names)
  (setf (rows table) (sort (rows table) (row-comparator column-names (schema table))))
  table)

;;; for mp3 database 
(defun shuffle-table (table)
  (nshuffle (rows table))
  table)

(defun random-selection (table n)
  (make-instance
   'table
   :schema (schema table)
   :rows (nshuffle (random-sample (rows table) n))))

(defun random-sample (vector n)
  "Based on Algorithm S from Knuth. TAOCP, vol. 2. p. 142"
  (loop with selected = (make-array n :fill-pointer 0)
     for idx from 0
     do
     (loop
	with to-select = (- n (length selected))
	for remaining = (- (length vector) idx)
	while (>= (* remaining (random 1.0)) to-select)
	do (incf idx))
     (vector-push (aref vector idx) selected)
     when (= (length selected) n) return selected))

