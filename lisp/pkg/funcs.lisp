(in-package :pkg)

(defun hello-world (x)
  (loop for i from 1 to 5
	do (format t "hello! ~a" x)))
