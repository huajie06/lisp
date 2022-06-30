(in-package :cl-user)

(defpackage :pkg
  (:use :cl)
  (:export :hello-world))

(load "funcs.lisp")

;;*default-pathname-defaults*
