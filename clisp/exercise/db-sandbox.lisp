(in-package :cl-user)

(defpackage :sandbox.database
  (:nicknames "sd-db")
  (:use :cl))

(in-package :sandbox.database)


;; so accessor defines both a :reader and :writer.
;; if only :reader or :writer, then only one of them can be performed
(defclass table ()
  ((rows
    :accessor rows
    :initarg :rows
    :initform (make-rows))
   (cols-spec
    :accessor cols-spec
    :initarg :cols-spec)))

(defun make-rows (&optional (default-size 10))
  (make-array default-size :fill-pointer 0 :adjustable t))

(defgeneric print-object (obj stream)
  (:documentation "print something"))
(defmethod print-object ((obj table) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((rows rows) (col-specs cols-spec)) obj
      (format stream "Columns Specification:~&~a~%Rows:~&~a~%" col-specs rows))))

(defclass col ()
  ((name
    :initarg :name
    :reader name)
   (col-type
    :initarg :col-type
    :initform nil
    :reader col-type)
   (normalizer
    :initarg :normalizer
    :reader :normalizer
    :initform nil)
   (default-value
    :initarg :default-value
    :reader default-value
    :initform nil)))

(defmethod print-object ((obj col) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name col-type normalizer default-value) obj
      (format stream "Name:~a. Col type:~a. Normalizer:~a. Default:~a"
	      name col-type normalizer default-value))))


(defparameter *test-col* (make-instance'col
			  :name "fname"
			  :col-type 'string))
*test-col*

;; (defun create-one-col (name var-type &optional default-value)
;;   (make-instance 'col
;; 		 :name name
;; 		 :var-type var-type
;; 		 :default-value default-value))

(defun make-col (name col-type &optional default-value)
  (make-instance 'col
		 :name name
		 :col-type col-type
		 :default-value default-value))

(apply #'make-col '(:first-name 'string))

;; pay attention to mapcar
(defparameter *cols-specs*
  (mapcar #'(lambda (col) (apply #'make-col col))
          '((:first-name 'string)
            (:last-name 'string)
	    (:age 'number)
            (:address 'string "unknown")
            (:zip 'string)
	    (:country 'string "US")
            (:state 'string "CT"))))


;; test record
(defparameter *test-record*
  (list
   :first-name "mike"
   :last-name "zhang"
   :age 9999
   :country "france"))


(defparameter *table* (make-instance 'table :cols-spec *cols-specs*))

(defun insert-row (row table)
  (let ((col-spec (cols-spec table)))
    (vector-push-extend
     (loop for c in col-spec
	   for name = (name c)
	   for value = (or (getf row name) (default-value c))
	   collect name
	   collect value)
     (rows table))))

(insert-row *test-record* *table*)
;; *table*
;; (rows *table*)
;; (cols-spec *table*)

;;==============================
(defparameter *cust-cols-specs*
  (mapcar #'(lambda (col) (apply #'make-col col))
          '((:first-name 'string)
            (:last-name 'string)
	    (:age 'number)
            (:address 'string "unknown")
	    (:city 'string)
	    (:country 'string "US")
            (:state 'string "CT"))))
(defparameter *customer-table* (make-instance 'table :cols-spec *cust-cols-specs*))

(defparameter *header*
  (list :first-name :last-name :address :city :state :country))

(defparameter *test-raw*
  '(("mike" "jack" "kate" "lucy")
    ("g" "z" "c" "d")
    ("23 main st" "1 c rd" "23 d st" "99 abc")
    ("nyc" "sfo" "bos" "ord")
    ("fl" "ga" "nj" "ca")
    ("usa" "italy" "german" "frence")))

;;transpose
(apply #'mapcar #'list *test-raw*)

(defparameter *format-input*
  (loop for i in (apply #'mapcar #'list *test-raw*)
	collect
	(mapcan #'list *header* i)))

(dolist (v *format-input*)
  (insert-row v *customer-table*))

*customer-table*
(rows *customer-table*)

;;==============================
(elt (rows *customer-table*) 0)

(loop for i across (rows *customer-table*)
      do (remove-if-not
	  #'(lambda (x) (equal (getf x :first-name) "mike")) i))

(remove-if-not #'(lambda (x) (equal (getf x :first-name)) "mike")
	       '((:first-name "z")))


;; equal
(remove-if-not #'(lambda (x) (equal (get-col-value x :first-name) "mike"))
	       (rows *customer-table*))

;; select
(defun get-col-value (row column-name)
  (getf row column-name))

;; useful loop
(defmacro loop-rows ((row table) &body body)
  `(loop for ,row across (rows ,table) do ,@body))

(loop-rows (r *customer-table*)
  (print (get-col-value r :city)))

(loop-rows (r *customer-table*)
  (let ((fname (get-col-value r :first-name))
	(lname (get-col-value r :last-name))
	(city (get-col-value r :city)))
    (format t "fname:~a, last name:~a, city:~a~%" fname lname city)))

(defmacro with-column-value ((&rest vars) row &body body)
  `(let* ((col-val ',(loop for i in vars collect (as-keywords i)))
	  (r-val (loop for i in col-val collect (get-col-value ,row i))))
     (destructuring-bind (first-name last-name) r-val
       ,@body)))

(loop-rows (r *customer-table*)
  (with-column-value (first-name last-name) r
    (format t "first name: ~a. last name: ~a~%" first-name last-name)))


;;======

(defun extract-col-specs (column-name cols-spec)
  (let ((v (if (listp column-name) column-name (list column-name))))
    (remove-if-not
     #'(lambda (col) (member (name col) v))
     cols-spec)))

(extract-col-specs '(:first-name :last-name) (cols-spec *customer-table*))
(extract-col-specs :first-name (cols-spec *customer-table*))

(defun extractor (schema)
  (let ((names (mapcar #'name schema)))
    #'(lambda (row)
        (loop for c in names collect c collect (getf row c)))))

(defun extract-col (cols-spec rows)
  (map 'vector (extractor cols-spec) rows))

*customer-table*

(defun select (&key columns from)
  (let ((rows (rows from))
	(cols-spec (cols-spec from)))

    (when columns
      (setf cols-spec (extract-col-specs columns cols-spec))
      (setf rows (extract-col cols-spec rows)))

    (make-instance 'table :rows rows :cols-spec cols-spec)))

(select :columns '(:first-name :last-name :city :country)
	:from *customer-table* )




(let ((keys '(:first-name :last-name)))
  (loop for k in keys
	collect k collect
		  (getf (elt (rows *customer-table*) 1) k)))

(defun fn ()
  (let ((keys '(:first-name :last-name)))
    #'(lambda (r) ;why #' here
	(loop for k in keys collect k collect
				      (getf r k)))))

(map 'vector #'(lambda (x)(getf x :first-name)) (rows *customer-table*))
(map 'vector fn (rows *customer-table*))


==
;;==================================================
;; ============ two different ways to `with`
;;==================================================
(defun as-keywords (symbol)
  (intern (symbol-name symbol) :keyword))
(defun column-bindings (vars row)
  (loop for v in vars collect `(,v (get-col-value ,row ,(as-keywords v)))))

;; basically is var1, var2, var3 ... inside of let
(defmacro test-with-1 ((&rest vars) row &body body)
  `(let ,(loop for v in vars collect
	       `(,v (get-col-value ,row ,(as-keywords v))))
     ,@body))

(test-with-1 (first-name last-name) (elt (rows *customer-table*) 1)
  (print (list first-name last-name)))

;;============
(defmacro with-column-value ((&rest vars) row &body body)
  `(let* ((col-val ',(loop for i in vars collect (as-keywords i)))
	  (r-val (loop for i in col-val collect (get-col-value ,row i))))
     (destructuring-bind (first-name last-name) r-val
       ,@body)))

(macroexpand-1 '(with-column-value1 (first-name last-name)
		 (elt (rows *customer-table*) 1)
		 (print (list first-name last-name))))

(with-column-value1 (first-name last-name)
		    (elt (rows *customer-table*) 1)
  (print (list first-name last-name)))
;;==================================================
;; ============ two different ways to `with` end
;;==================================================




(defparameter *test-table* (make-instance 'table :cols *cols-specs*))

*test-table*
(cols *test-table*)

(defparameter *test-row*
  (list
   :first-name "mike"
   :last-name "zhang"
   :address "abc streat"
   :zip "06123"))

(vector-push-extend *test-row* (rows *test-table*))

(defparameter *test-array* (make-array 10 :adjustable t :fill-pointer 0))
(vector-push 'a *test-array*)
*test-array*

(loop for v across (rows *test-table*)
      do (print (getf v :first-name)))

(loop for v in (cols *test-table*)
      do (print (name v)))


(apply #'+ '((1 2) (2 3)))

(mapcar
 #'(lambda (a b) (+ a b 1))
 '(1 2) '(2 3))

(mapcar
 #'(lambda (l) (apply #'(lambda (a b) (+ a b 1)) l))
 '((1 2) (2 3)))


(lambda (a b) (+ a b 1))(1 2)

(null nil)
(funcall #'(lambda (a b c) (+ 2 a b c)) 1 2 3)

(funcall #'+ 1)
(apply #'+ 12 3 nil)
(apply #'+ '(1 2 3))



;;============================================================
;;=========== some testing code
;;============================================================
(name (make-instance 'cols :name :hahaha :default-value 999))
(default-value (make-instance 'cols :name :hahaha :default-value 999))

(name (create-one-col :state "ct"))
(default-value (create-one-col :state "ct"))

(default-value
 (nth 1 (mapcar #'create-one-col '((:fname) (:state "CT")))))

(apply #'create-one-col '(:state "ct"))

;;(inspect *cols-specs*)

;;====================


;;============================================================
;;=========== play with macro.
;;======== the comma(,) in front of a breaket
;;======== will result in every single form has a (,), so cannot double it.
;;============================================================
(defmacro with-column-value1 ((&rest vars) row &body body)
  `(let* ((col-val ',(loop for i in vars collect (as-keywords i)))
	  (r-val (loop for i in col-val collect (get-col-value ,row i))))
     (destructuring-bind (first-name last-name) r-val
       ,@body)))

(macroexpand-1 '(with-column-value1 (first-name last-name)
		 (elt (rows *customer-table*) 1)
		 (print (list first-name last-name))))

(with-column-value1 (first-name last-name)
		    (elt (rows *customer-table*) 1)
  (print (list first-name last-name)))

;;============================================================
;;=========== play with macro end
;;============================================================


(defun extract-col (cols-spec rows)
  (map 'vector (extractor cols-spec) rows))

(map 'vector (extractor
	      (extract-col-specs '(:first-name :last-name) (cols-spec *customer-table*)))
     (rows *customer-table*))

(mapcar #'name (cols-spec *customer-table*))

(defun extractor (schema)
  (let ((names (mapcar #'name schema)))
    #'(lambda (row)
        (loop for c in names collect c collect (getf row c)))))

(let ((row (elt (rows *customer-table*) 0))
      (names (list :first-name :last-name)))
  (loop for c in names collect c collect (getf row c)))
