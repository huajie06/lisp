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
   (cols
    :accessor cols
    :initarg :cols)))

(defgeneric print-object (obj stream)
  (:documentation "print something"))
(defmethod print-object ((obj table) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((rows rows) (cols cols)) obj
      (format stream "rows:~a. cols:~b" rows cols))))

(defclass cols ()
  ((name
    :initarg :name
    :accessor name)
   (default-value
    :initarg :default-value
    :reader default-value
    :initform nil)))

(defparameter *test-col* (make-instance'cols :name "first name"))

(defun create-one-col (name &optional default-value)
  (make-instance 'cols :name name :default-value default-value))

(create-one-col :first-name "mike")

(defparameter *cols-specs*
  (mapcar #'create-one-col
          '((:first-name)
            (:last-name)
            (:address)
            (:zip)
            (:state "CT"))))

(defun make-rows (&optional (default-size 10))
  (make-array default-size :fill-pointer 0 :adjustable t))

(defparameter *test-table* (make-instance 'table :cols *cols-specs*))

*test-table*
(cols *test-table*)

(defparameter *test-row*
  (list :first-name "mike" :last-name "zhang" :address "abc streat" :zip "06123"))

(vector-push-extend *test-row* (rows *test-table*))


(defparameter *test-array* (make-array 10 :adjustable t :fill-pointer 0))
(vector-push 'a *test-array*)
*test-array*

(loop for v across (rows *test-table*)
      do (print (getf v :first-name)))


