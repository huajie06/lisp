(let ((in (open "/Users/huajiezhang/repo/lisp/lisp/test-db.txt" :if-does-not-exist nil)))
  (when in 
    (loop for line = (read-line in nil)
	  while line do (format t "~a~%" line))
    (close in)))


(let ((in (open "/Users/huajiezhang/repo/lisp/lisp/test-db1.txt" :if-does-not-exist nil)))
  (if in 
      (progn
	(loop for line = (read-line in nil)
	      while line do (format t "~a~%" line))
	(close in))
      (print "not exist")))
  



;; with-open-file takes care of the close
(defparameter *fname* "~/repo/lisp/lisp/hello1.lisp")
(with-open-file (in *fname* :if-does-not-exist nil) ;; only do when T
  (if in
      (loop for line = (read-line in nil 'eof) ; the nil = not return error if eof but 'eof or whatever the 3rd parameter, or nil if no exist
	    until (eq line 'eof) do (format t "~a~%" line))
      (print "file can not be found!")))


;; another method
(with-open-file (stream "~/repo/apps/line/SendPushMsg.sh")
  (do ((line (read-line stream nil)
	     (read-line stream nil)))
      ((null line))
    (print line)))

(with-open-file (stream "~/repo/apps/google/1credentials.json" :if-does-not-exist nil)
  (if stream
      (loop for line = (read-line stream nil 'foo)
	    until (eq line 'foo) do (print line))
      (print "not found")))


;;;;;;;;;== some loop
(loop for i in (list 1 2 3)
      do (print i))

(loop for i in (list 1 2 3)
      for y = (* 2 i) ;; use = for some intermediate calc
      collect y)


(let ((x 4))
  (loop while (<= x 6)
	do (print x)
	   (setf x (incf x))))

(loop for i in (list 1 2 3)
      while (< i 4) do (print i))

(loop for i in (list 1 2 3)
      for y = (* 100 i)
      while (<= y 200) do (print i))

(loop for i in (list 1 2 3)
      for y = (* 100 i)
      collect y)

(loop for i in (list 1 2 3)
      for y = (* 100 i)
      while (<= y 200) collect y)
