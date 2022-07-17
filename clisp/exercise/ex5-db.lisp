(in-package :cl-user)

(defpackage :database
  (:nicknames "db")
  (:use :cl))

(in-package :database)

(defclass table ()
  ((cols
    :initarg :cols
    :accessor cols)
   (rows
    :initarg :rows
    :initform (make-rows)
    :accessor rows)))

(defgeneric print-object (obj stream))
(defmethod print-object ((obj table) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((cols cols) (rows rows)) obj
      (format stream "~&Cols:~%~a~&Rows:~%~a" cols rows))))

(defparameter *default-size* 10)
(defun make-rows (&optional (default-size *default-size*))
  (make-array default-size :adjustable t :fill-pointer 0))

(defclass col ()
  ((name
    :initarg :name
    :reader name)
   (col-type
    :accessor col-type
    :initarg :col-type)
   (default-value
    :initarg :default-value
    :accessor default-value
    :initform nil)
   (comparator
    :initarg :comparator
    :accessor comparator
    :initform #'<)))

(defmethod print-object ((obj col) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((name name)
		     (col-type col-type)
		     (comparator comparator)
		     (default-value default-value)) obj
      (format stream "Name:~a. Type:~a. Comparator:~a. Default:~a"
	      name col-type comparator default-value))))

(defun make-col (name col-type default-value)
  (make-instance 'col
		 :name name
		 :col-type col-type
		 :default-value default-value))

(defun fill-comparator (col)
  (with-accessors ((comparator comparator) (col-type col-type)) col
    (if (equal col-type 'string) (setf comparator #'string<))))

(defmethod initialize-instance :after ((obj col) &key)
  (fill-comparator obj))

;; (comparator (make-col "abc" 'string  0))
;; (make-col "abc" 'string  0)
;; (type-of (comparator (make-col "abc" 'string  0)))
;; (funcall (comparator (make-col "abc" 'string  0)) "abc" "b")


(defun init-table (cols)
  (make-instance 'table :cols cols))

(defun insert-row (to-insert-r table)
  (vector-push-extend
   (loop for c in (cols table)
	 for name = (name c)
	 for value = (or (getf to-insert-r name) (default-value c))
	 collect name
	 collect value)
   (rows table)))

(defun create-cols (col-def-literal)
  (mapcar #'(lambda (x) (apply #'make-col x))
	  col-def-literal))

;;========================================
;;========================================
;;========================================

;; (defparameter *for-col-defs*
;;   '((:name :age :col-type 'number :comparator #'< :default-value 0)
;;     (:name :name :col-type 'string :comparator #'string< :default-value "")
;;     (:name :hobby :col-type 'string :comparator #'string< :default-value "")))

(defparameter *for-col-defs*
  '((:age number 0)
    (:name string "")
    (:address string "")
    (:hobby string "")))

(defparameter *test-row*
  '((:age 20 :name "mike" :hobby "cs" :address "abc str")
    (:age 30 :name "jack" :hobby "basketball" :address "bca")
    (:age 10 :name "lucy" :hobby "paint" :address "yoyo")))


(defparameter *col-def* (create-cols *for-col-defs*))
(defparameter *test-table* (init-table *col-def*))

;; insert table
(loop repeat 3
      do (loop for r in *test-row*
	       do (insert-row r *test-table*)))

*test-table*
(cols *test-table*)
(rows *test-table*)

(defun nth-row (table n)
  (aref (rows table) n))

(defun shape-table (table)
  (list (length (rows table)) (length (cols table))))

(shape-table *test-table*)

(defun mklist (ele)
  (if (listp ele) ele (list ele)))

(defun test-name-in-cols (names)
  #'(lambda (c)
      (loop for n in (mklist names)
	      thereis (equal (name c) n))))

;; (let ((names '(:hobby :age :address :hobby))
;;       (cols (cols *test-table*)))
;;   (loop for i in names
;; 	collect (loop for c in cols
;; 		      when (funcall (extract-col i) c) return c)))

;; (defun get-col (names table)
;;   (remove-if-not (extract-col (mklist names)) (cols table)))

(defun get-cols (names cols)
  (loop for i in (mklist names)
	collect (loop for c in cols
		      when (funcall (test-name-in-cols i) c) return c)))


(defun extract-row (column)
  (let ((col (mklist column)))
    #'(lambda (row)
	(loop for c in col
	      collect c collect (getf row c)))))

(cols *test-table*)
(rows *test-table*)
(nth-row *test-table* 2)

(defun sort-by-col (col-names table)
  "the col-names and table must have the same columns"
  (let* ((cols (get-cols col-names (cols table)))
	 (comp-mth (mapcar #'comparator cols)))
    #'(lambda (a b)
	(loop for i in (mklist col-names)
	      for comparator in comp-mth
	      for var-a = (getf a i)
	      for var-b = (getf b i)
	      when (funcall comparator var-a var-b) return t
		when (not (funcall comparator var-a var-b)) return nil
		  finally (return nil)))))


(defparameter cols (list (car (cols *test-table*))))
*test-table*

(cols *test-table*)

(defun select (&key columns from sort-by distinct)
  (let ((cols (cols from))
	(rows (rows from)))

    (when columns
      (setf cols (get-cols columns cols))
      (setf rows (map 'vector (extract-row columns) rows)))

    (when distinct
      (setf rows (remove-duplicates rows :test #'equal)))

    (when sort-by
      (setf rows
	    (sort (copy-seq rows) (sort-by-col sort-by from))))

    (make-instance 'table :rows rows :cols cols)))

;;============= testing ==================

(select :columns '(:name :age) :from *test-table*)
(cols (select :columns '(:name :age) :from *test-table*))

(select :columns '(:age) :from *test-table*)
(select :columns '(:age) :from *test-table* :distinct t)
(select :from *test-table*)

(select :from *test-table* :sort-by '(:age :address))
(select :from *test-table* :sort-by '(:name :address))

(select
 :columns '(:hobby :address)
 :from *test-table*
 :sort-by '(:hobby :address))

(select
 :columns '(:hobby :address)
 :from *test-table*
 :sort-by '(:address))

(select
 :from *test-table*
 :sort-by '(:address))

(select
 :from *test-table*
 :sort-by '(:address)
 :distinct t)
