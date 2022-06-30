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

;;;; this will yield eof file error without `nil`
;;;; with the nil, the process will no stop by keep yielding nil at eof
(with-open-file (stream "~/repo/apps/google/credentials.json" :if-does-not-exist nil)
  (if stream
      (loop for line = (read-line stream nil)
	    while line do (print line))
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


(defparameter file-name "~/repo/lisp/clojure/hello.clj")
(with-open-file (s file-name :if-does-not-exist nil :element-type '(unsigned-byte 8))
  (if s
      (loop for b = (read-byte s nil)
	    while b do(print b))
      (print "file does not exist")))


;; this is not a loop, simply get create a string vector, alter the value of it
(defparameter infile "~/repo/lisp/clojure/hello.clj")
(with-open-file (instream infile :direction :input :if-does-not-exist nil)
  (when instream
    (let ((string-result (make-string (file-length instream))))
      (read-sequence string-result instream)
      string-result)))


(with-open-file (instream infile :if-does-not-exist nil)
  (when instream
    (file-length instream)))

(with-open-file (instream infile :if-does-not-exist nil)
  (when instream
    (loop for line = (read-line instream nil)
	  for i from 1
	  while line do
	    (format t "line #~a length: ~a~%" i (length line)))))



;;;;;;write to file

;;(let ((outstream (open "test-file-out.txt" :direction :output :if-exists nil)))
(let ((outstream (open "test-file-out.txt" :direction :output :if-exists :append)))
  (when outstream
    (progn
      (write-line "hello world!" outstream)
      (close outstream))))

(let ((outstream (open "test-file-out.txt" :direction :output :if-exists :supersede)))
  (when outstream
    (progn
      (write-line "hello world!" outstream)
      (close outstream))))


(with-open-file (outstream "test-file-out2.txt" :direction :output :if-exists :supersede)
  (when outstream
    (write-sequence "what the heck" outstream)))




(pathname-device (pathname "~/repo/apps/sec/get_13f.py"))
(pathname-directory (pathname "~/repo/apps/sec/get_13f.py"))
(pathname-name (pathname "~/repo/apps/sec/get_13f.py"))
(pathname-type (pathname "~/repo/apps/sec/get_13f.py"))


(namestring (pathname "~/repo/apps/sec/get_13f.py"))
(file-namestring (pathname "~/repo/apps/sec/get_13f.py"))
(directory-namestring (pathname "~/repo/apps/sec/get_13f.py"))

(make-pathname
 :directory '(:absolute "repo" "app" "sec"); cannot be "~/repo/apps/sec/get_13f.py"
 :name "something"
 :type "lisp")

(namestring (make-pathname
	     :directory '(:absolute "repo" "apps" "sec")
	     :name "something"
	     :type "lisp"))

(merge-pathnames #p"~/bacd/abc/" #p"abd/123.py")


(let ((stream (make-string-input-stream "test string")))
  (loop for line = (read-line stream nil)
	while line
	do (format t "~a" line))
  (close stream))

(with-input-from-string (str "Foobar")
  (loop for i from 0
        for line = (read-line str nil)
        while line
        do (format t "~d: ~a~%" i line)))
