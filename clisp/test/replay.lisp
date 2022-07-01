(defvar *var*) ;define a global var
(list :name "jack" :age 19) ;plist: property list
(getf (list :name "jack" :age 19) :name) ; getf to access plist

(remove-if-not #'(lambda (x) (> x 3)) '(3 4 5))
(mapcar #'(lambda (x) (+ x 2)) '(1 2 3))
;;; readin file
(defparameter *fname* "/Users/huajiezhang/.vim/vimrc")
(with-open-file (stream *fname*
			:direction :input
			:if-does-not-exist nil)
  (if stream
      (loop for line = (read-line stream nil)
	    while line do (print line))
      (print "file does not exist!")))

;;; read into a variable
(defparameter *fname* "/Users/huajiezhang/.vim/vimrc")
(with-open-file (stream *fname*
			:direction :input
			:if-does-not-exist nil)
  (if stream
      (let ((result (make-string (file-length stream))))
	(read-sequence result stream)
	result)
      (print "file does not exist!")))

;;; write file
(defparameter *out-file-name* "out-test-nogit.txt")
(with-open-file (stream *out-file-name*
			:direction :output
			:if-exists :supersede)
  (loop for line in '("hello" "world" "my" "name" "is" "xxx")
	do (write-line line stream)))

;;; write with sequence
(with-open-file (stream *out-file-name*
			:direction :output
			:if-exists :supersede)
  (write-sequence (concatenate 'string "a string" '(#\newline)' "2nd line")
		  stream))
;;; use format
(with-open-file (stream *out-file-name*
			:direction :output
			:if-exists :supersede)
  (format stream "~a" "hello world"))


;;; functions
(defun test-fun (x &key (y 0))
  (+ x y))
(defun test-fun1 (x y &rest rest) ;rest will come in as a list
  (list x y rest))
(defun test-fun2 (x &optional y)
  (list x y))

(test-fun 1 :y 2)
(test-fun1 1 2 3 4)
(test-fun2 1)

;;; define prime
(defun primep (x)
  (loop for i from 2 to (isqrt x)
	never (zerop (mod x i))))
(defun next-prime (x)
  (loop for i from (+ x 1)
	when (primep i) return i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macro to loop prime. step is to write a normal func first
(do ((i 2 (next-prime i)))
    ((> i 13))
  (print i))

(defmacro loop-prime (x)
  `(do ((i 2 (next-prime i)))
       ((> i ,x))
     (print i)))
(loop-prime 10)
(macroexpand-1 '(loop-prime 10))

;;; gensym normally can use with let
(defmacro loop-prime2 ((var start end) &body body)
  (let ((end-val (gensym)))
    `(do ((,var (next-prime ,start) (next-prime ,var))
	  (,end-val ,end))
	 ((> ,var ,end-val))
       ,@body)))
(loop-prime2 (x 4 30) (print x))
(macroexpand-1 '(loop-prime2 (x 4 30) (print x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; do loop, pay attention to format
(do ((x 1 (+ x 1))
     (y 1))
    ((> x 4))
  (format t "X:~a, Y:~a~%" x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; built in functions,types

(string= "abc" "Bcd")
(vector 1 2 3)
(vector "abc" 2 3)

(type-of "abc")
(type-of (vector "a" "b" "c"))

;;; get value by index
(elt (vector 1 2 3) 1)
(elt (list 1 2 3) 1)


;;; hashtable

(defparameter *hash-example* (make-hash-table :test 'equalp)) ;;; define the gethash method
(setf (gethash 'key1 *hash-example*) 1)
*hash-example*
(gethash 'key1 *hash-example*) ; this return two results
(nth-value 1 (gethash 'key1 *hash-example*)) ;vs nth, only works with list
(nth 1 (list 1 2 3)) ; no vector

;;; loop hashtable
(maphash #'(lambda (k v) (format t "key: ~a, value: ~a~%" k v)) *hash-example*)

(loop for k being each hash-key of *hash-example*
      do (print k))
(loop for v being each hash-value of *hash-example*
      do (print v))


;;; list is chained cons, the second is linked to next cons
(cons 1 2)
(car (list 1 2 3))
(cdr (list 1 2 3))
;; list is chained cons, the last cons is (xxx . nil)
(equal (cons 1 nil) (list 1))
(cons 1 (cons 2 (cons 3 nil))) ; == (1 2 3)

(append (list 'a 1) (list 1 2 9 3))


;;;; defclass, exception or condition(in lisp world)

(defclass range ()
  ((width
    :initarg :width
    :initform 0
    :accessor with)
   (height
    :initarg :height-targ
    :initform 0
    :accessor :height)))

(defparameter *class-example*
  (make-instance 'range :width 10 :height-targ 20))

;;; different ways of access slots
*class-example*
(with-slots (width height) *class-example*
  (print (list width height)))
(:height *class-example*)
(slot-value *class-example* 'height)
(with *class-example*)


(defmethod calc-area ((obj range)) ;; be careful with the paren
  (with-slots (height width) obj
    (* height width)))

(calc-area *class-example*)

(defmethod print-object ((obj range) stream) ; don't forget the stream
  (print-unreadable-object (obj stream)
    (with-slots (height width) obj
      (format stream "height is :~a, width is :~a" height width))))
(make-instance 'range :width 10 :height-targ 20); the message now changes


(defun validate-range (range)
  (with-accessors ((width with) (height :height)) range ;pay attend to paren
    (unless (<= width height) (error "something is wrong"))))

(validate-range *class-example*) ;; here the error is not defined

(defmethod initialize-instance :after ((obj range) &key) ;need key because generic accept &key
  (validate-range obj))

(make-instance 'range :width 10 :height-targ 20)
(make-instance 'range :width 30 :height-targ 20);; this will give error because above

;; some restart-case 

;; https://github.com/huajie06/lisp/blob/master/clisp/practical-common-lisp-ch19.lisp

;;flet and label are quick functions, one line func, flet and be recursive
(flet ((do-something (x)
	 (+ x 1)))
  (do-something 1))

(labels ((do-something1 (x)
	   (+ x 1)))
  (do-something1 1))


;;tagbody
(setf x 1)
(tagbody
 a (print 'a) (incf x) (if (> x 3) (go e) (go b))
 b (print 'b) (go a)
 e (print 'end))

;;block

(block block-name
  (loop for i from 1
	do (when (> i 10) (return-from block-name i))
	   (print i)))

(block block-name
  (print "hello")
  2) ; return the last 


(loop for i across "helllo"
      do (print i))

(loop repeat 5 do
  (format t "hello~%"))


;;; path
(directory "/Users/huajiezhang/repo/lisp/clisp/*.lisp") ; only work with wildcard
;; convert to string from pathname
(namestring  #P"/Users/huajiezhang/repo/lisp/clisp/unit-test.lisp")
;; convert to parth name
(pathname "/Users/huajiezhang/")

;; use discrible if do not know pathname
(describe (pathname "/Users/bar/*.text"))

(make-pathname
 :defaults "/Users/huajiezhang"
 :name "abc"
 :type "lisp")

(make-pathname
 :directory '(:absolute "Users" "huajiezhang") ; it takes a list
 :name "abc"
 :type "lisp")

(pathname-directory #P"/Users/huajiezhang/abc.lisp") ; this can be used
(pathname-name #P"/Users/huajiezhang/abc.lisp")
(pathname-type #P"/Users/huajiezhang/abc.lisp")
(file-namestring #P"/Users/huajiezhang/abc.lisp") 
(search "abc" (file-namestring #P"/Users/huajiezhang/1abc.lisp")) ;return match position

(probe-file "/Users/huajiezhang")
(probe-file "/Users/huajiezhang1") ;return nil if not exist





