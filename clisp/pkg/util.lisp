(in-package :cl-user)

(defpackage :util
  (:use :cl)
  (:export :sum-a-lot))

(in-package :util)

(defun sum-a-lot (x)
  (loop for i from 1 to 10
	do (format t "Do something ~a" x)))


