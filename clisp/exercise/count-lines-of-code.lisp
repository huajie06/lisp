(in-package :cl-user)

(defparameter *file-p* #p"/Users/huajiezhang/repo/lisp/clisp")

(defun dir-p (pathname)
  (and
   (not (pathname-type pathname))
   (not (pathname-name pathname))
   pathname))

(defun file-to-dir (file-pname)
  (if (dir-p file-pname) file-pname
      (make-pathname
       :directory (append (pathname-directory file-pname) (list (file-namestring file-pname)))
       :name nil
       :type nil)))


(defun list-dir (dir &optional fname ftype)
  (let ((filep (make-pathname
		:defaults (file-to-dir dir)
		:name (if fname fname :wild)
		:type (if ftype ftype :wild))))
    (loop for i in (directory filep)
	  collect i)))

(defun walk-dir (dir)
  (let ((filep (make-pathname
		:defaults (file-to-dir dir)
		:name :wild
		:type :wild)))
    (loop for i in (directory filep)
	  collect (if (dir-p i) (list i (walk-dir i)) i))))


(defun get-file-lines (f)
  "get the number of lines in a file, excluding blank lines"
  (with-open-file (stream f :if-does-not-exist nil)
    (if stream
	(let ((n 0))
	  (loop for line = (read-line stream nil)
		while line
		do (unless (string= line "") (incf n)))
	  n)
	0)))


(defparameter *counter* 0)
(defun travers-list (l)
  (loop for i in l do
    (if (listp i)
	(travers-list i)
	(when (string= (pathname-type i) "lisp")
	  (format t "Fname: ~@(~a~) ~50t Lines:~5d~%"
		  (file-namestring i) (get-file-lines i))
	  (incf *counter* (get-file-lines i))))))


(defun wrapper (l)
  (defparameter *counter* 0)
  (travers-list l)
  (format t "-----------------~%Total lines: ~a~%" *counter*))
;;====================

(wrapper (list-dir *file-p*))

(wrapper (walk-dir *file-p*))
