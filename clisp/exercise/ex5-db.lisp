(in-package :cl-user)

(defpackage :mp3-db
  (:nicknames "db")
  (:use :cl))

(in-package :mp3-db)



(defclass table ()
  ((rows
    :initarg :rows
    :accessor rows
    :initform (make-rows))
   (schema  ; the schema here holds the colum specs
    :initarg :schema
    :accessor schema)))

;; ============= print object
(defgeneric print-object (obj stream)
  (:documentation "print the row object"))

(defmethod print-object ((obj table) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((row-cnt rows) (schema-val schema)) obj
      (format stream "~&rows:~a, schema:~a" row-cnt schema-val))))
;; ============= print object end ===============

(defparameter *default-table-size* 100)
(defun make-rows (&optional (default-size *default-table-size*))
  (make-array default-size :fill-pointer 0 :adjustable t))


(defclass column ()
  ((name
    :reader name
    :initarg :name)
   (equality-predicate
    :reader equality-predicate
    :initarg :equality-predicate)
   (comparator
    :reader comparator
    :initarg :comparator)
   (default-value
    :reader default-value
    :initarg :default-value
    :initform nil)
   (value-normalizer
    :reader value-normalizer
    :initarg :value-normalizer
    :initform #'(lambda (v column) (declare (ignore column)) v))))

(defmethod print-object ((obj column) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name equality-predicate comparator default-value value-normalizer) obj
      (format stream "name:~a. ep:~a. comp:~a. dv:~a. vn:~a"
	      name equality-predicate comparator default-value value-normalizer))))

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
   :default-value default-value))

(defun not-nullable (value column)
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
   :comparator #'string<
   :default-value default-value))

(defun make-schema (spec)
  (mapcar #'(lambda (column-spec) (apply #'make-column column-spec)) spec))

(defparameter *mp3-schema*
  (make-schema
   '((:file     string)
     (:genre    interned-string "Unknown")
     (:artist   interned-string "Unknown")
     (:album    interned-string "Unknown")
     (:song     string)
     (:track    number 0)
     (:year     number 0)
     (:id3-size number))))

(defparameter *mp3s* (make-instance 'table :schema *mp3-schema*))
;; ===================== still follow here.


(list
 :file   "file value"
 :genre  "genre value"
 :artist "artist"
 :album  "album"
 :song   "songs"
 :track  12
 :year   2020
 :id3-size 2000)

(insert-row
 (list
  :file   "file1"
  :genre  "pop"
  :artist "jay"
  :album  "new"
  :song   "songs"
  :track  12
  :year   2020
  :id3-size 2000) *mp3s*)


(type-of *mp3s*)
(rows *mp3s*)
(schema *mp3s*)


(defun view-db (db)
  (let ((col-name (loop for s in (schema db) collect (name s)))
	(row-vals (loop for row across (rows db) collect row)))
    (progn
      (format t "|~{~a~15t|~}~%" col-name)
      (format t "~{|~{~*~a~15t|~}~%~}" row-vals))))

(view-db *mp3s*)



;; ===================== still follow here.
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

;;; here to insert into a db


(defun file->row (file)
  (let ((id3 (read-id3 file)))
    (list
     :file   (namestring (truename file))
     :genre  (translated-genre id3)
     :artist (artist id3)
     :album  (album id3)
     :song   (song id3)
     :track  (parse-track (track id3))
     :year   (parse-year (year id3))
     :id3-size (size id3))))
(defun parse-track (track)
  (when track (parse-integer track :end (position #\/ track))))

(defun parse-year (year)
  (when year (parse-integer year)))

(defun load-database (dir db)
  (let ((count 0))
    (walk-directory
     dir
     #'(lambda (file)
         (princ #\.)
         (incf count)
         (insert-row (file->row file) db))
     :test #'mp3-p)
    (format t "~&Loaded ~d files into database." count)))




==
;; ============= test code
(defparameter *test-row*
  (make-instance 'row
		 :schema "schema1"
		 :rows (make-rows 10)))
*test-row*
(make-array 2 :initial-element 1)
(make-array 2 :fill-pointer 1)
(make-array (list 2 2) :initial-element 1)

(defparameter *test-col* (make-instance 'column))

;; loop over a vector use across!!
(loop for i across #('a 'b 'c 'd)
      do (print i))
