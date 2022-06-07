;;; symbol

'foo

;;; single and double

3.1455s0
3.1455d0

(+ 1 2)

;;; below two are the same
(quote (+ 1 2))
'(+ 1 2)

(or t 1)
(or nil ()) ;empty () is all nil

;;; characters, and strings are arrays of chars
#\a
"hello"

;;; nil will return value, t will print and return nil
(format nil "~A, ~A!" "hello" " world")
(format t "~A, ~A!" "hello" " world")

;;(write-line (let 'me "dance"))


;;; structs
(defstruct dog name breed age)
(defparameter *rover*
    (make-dog :name "rover"
              :breed "collie"
              :age 5))

(defparameter *max*
  (make-dog :name "xxx"
	    :breed "shiba inu"
	    :age 100))

(dog-p *rover*)
(dog-age *rover*)
(dog-name *max*)


;;; pairs

(cons 'abc 'bac)

;car ("contents of the address part of register number") 
;cdr ("contents of the decrement part of register number")
(car (cons 'abc 'bac))
(cdr (cons 'abc 'bac))


;same
(list 1 2 3)
'(1 2 3)

;;; list
(append '(1 2) '(3 4))
(concatenate 'list '(1 23 4) '(1 2))
(remove-if-not #'evenp '(1 2 3 4))


;;; vector
#(1 2)


;;; functions

(lambda () "hello world")

(defun hello-world () "Hello World")
(hello-world)

(defun hello (name)
  (format nil "Hello, ~A" name))
(hello "xxx")

(defun hello1 (name &key from timenow)
  (format nil "~A from: ~A, at: ~A " name from timenow))
(hello1 "jim" :from "us" :timenow "11:11")


;;; control flow
(if (or 1 nil)
    "this is real"
    "not real")


;; (let ((s 9))
;; (cond ((> s 100)(write-line "greater than 2"))
;;        ((<= s 0)(write-line "less than 2"))
;;        (t (write-line "1-100"))))

(let ((a 1)(b 2)(c 3))
  (if (and (> b 1) (> a 1) (> c 1))
      "all of the 3 >1"
      "osomething else"))

(let ((s -100))
  (cond
    ((> s 100)(format nil "~A > 100" s))
    ((< s 0)(format nil "~A is negtive" s))
    (t (write-line "something else"))))




(defun foo ()
  (dotimes (i 10)
    (format t "~d. hello~%" i) ) )
(foo)
