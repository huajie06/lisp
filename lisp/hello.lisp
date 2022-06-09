;; this is the first time learning
;; (write-line "hello world")

(defun hello ()
  (format t "hello, world!~%"))

(hello)

(defun h ()
  (print "hello"))

(h)

;; (print (1 23 3)) this is not valid because the list is not valid

(setq a 5)

(a)


(let ((a 6)) a)
a

(+ a 5)


(if t 5 6)

(if nil 1 4)

:this-is-a-keyword
:something-new
;; this is a keyword

(setq a (list 4 1))
a

(setq a (list 4 1 5))
(print a)

(setq b (cons 1 2))
(print b)

;; CAR "Contents of Address portion of Register"
;; CDR "Contents of Decrement portion of Register
(car b)
(cdr b)



(car (cons "a" "b"))

(setq a (cons 4 (cons 5 (cons 6 nil))))
(car a)
(cdr a)

(setq a (list 10 20 30))

(car a)


(setq a (list 1 23 44 435 "nil" "bdc" nil))
(print a)


;; push and pop both apply to the first element

(setq a nil)
(push 5 a)
(push 12 a)
(print a)
(pop a)


(push "A" a)
(print a)


(defun foo (x y)
  (+ x y))
(foo 1 2)


(defun f (x)
  (if (> x 1)
      (print "x >1")
      (print "x <1")))

(f -10)



(defun fact (x)
  (if (> x 0)
      (* x (fact (- x 1)))
      1))

(fact 4)


(defun cumSum (x)
  (if (> x 0)
      (+ (cumSum (- x 1)) x)
      0))

(cumSum 4)


;; anything after &optional is optinal
(defun test (a &optional b)
  (if b (+ a b) a))

(test 2 1)
(test 2)
(defun test1 (a &optional (b 1))
  (+ a b))
(test1 1)
(test1 2 3)

;; y is (3 2) so needs to transfer somehow into (+ 1 2)
(defun test2 (a b &rest y)
  (+ a b (apply '+ y)))
(test2 1 2 3 2)

(apply '+ (list 1 2))
(apply '+ '(1 2))
(apply '+ (quote (1 2)))

;; key arguments
(defun test (&key a b)
  (+ a b)))
(test :a 1 :b 2)

;; key arguments with default
(defun t1 (&key (c 2)) c)
(t1 :c 222)
(t1)


(defun t2 (&key (a 2) b)
  (+ a b))
(t2 :b 2)
(t2 :a 12 :b 2)

(defun t3 (&key (a1 1) (a2 2))
  (+ a1 a2))
(t3 :a1 20 :a2 23)

;; print
(print 3)
(format t "An atom: ~S~%and a list: ~S~%and an integer: ~D~%"
        nil (list 5) 6)
(format t "abc hehehe~%~S" (list 123))

(setq a 1)
a
'a
(quote a)
;;'a = (quote a)

(eq 'a (quote a))

;; binding
;; inside of let, ((a 1) (b 2)), a and b are not accessible through each other
(let ((a 1) (b 2)) (setq c (+ a b)))


(let ((a 1) (b 2) (c 3))
  (setq cc (+ a b c))
  cc)
cc

(let ((a 3)) a)

(setq x 1)
(let ((x 1)
      (y (+ x 1)))
  y)

;;;;;;;;;;;;;;;;;
;; dynamic scope
;;;;;;;;;;;;;;;;;

;; lexical scope
;; dynamically scope

;; lexical scope
(setq regular 5)
(defun check-regular ()
  regular)
(check-regular)
(let ((reguar 6)) (check-regular))
regular

;; dynamically scope
;; by convention, the name of the special variable begins and end with a *
(defvar *sp* 6)
(defun check-special ()
  *sp*)
(check-special)
(let ((*sp* 112)) (check-special))
*sp*
;;

(defvar sp1 6)
(defun check-special1 ()
  sp1)
(check-special1)
(let ((sp1 112)) (check-special1))
sp1

;; array
(make-array 3)

(char "abc" 0)
(aref "abc" 0)

(setq aa (concatenate 'list "abc" "ccc"))
aa

(setf bb (concatenate 'list "abc" "ccc"))
bb


;; struct is data type
(defstruct foo
  bar
  baaz
  qqqq)

;; the way is to assign something first
(setf bb (make-foo :bar 1200120 :baaz "hello"))
bb
;; foo-bar is to access the instance of a struct foo at field foo-bar
(foo-bar bb)
(setf (foo-qqqq bb) "bbb")
bb

;; aref = array element refer
(setf a (make-array 3))
(print a)
(aref a 1)
(aref a 2)


(setf (aref a 1) 3)
(print a)
(aref a 1)

(defstruct foo1
  bar
  qqq)

(setf a (make-foo1))
a
(foo1-bar a)

(setf (foo1-bar a) 111)
(foo1-bar a)
a


(setf a (make-array 1))
(print a)
(push 5 (aref a 0))
(aref a 0)
(print a)

;; booleans and conditionals
(if t 5 6)
(if nil 5 6)

(setf a 10)
(if (> a 4)
    (progn ; to evaluate one more multiple forms
      (print "i'm true")
      (print "i'm going to do something dumb")
      (setq ccc "1000")
      (concatenate 'string ccc " what the heck???"))
    (print "i'm false"))


(when t 3)
(when t
  (progn
    (setf w "today is a good day")
    (concatenate 'string w " yay!!!")))

(when t
  (setf w "today is a good day")
  (concatenate 'string w " yay!!!"))




(unless nil 4)
(unless t 4)

;; when/unless allow any number of statements
(when t
  (setf cc "sdkfksjf ")
  (setf b (concatenate 'string cc "yyy"))
  (print b))

;; if elseif else
(setf c 3)
(cond
  ((evenp c) (print "i'm even"))
  ((> c 100) (print "i'm not even and i'm >2"))
  (t "this is else"))


(defun hotp (x y)
  (cond
    ((= x 1) y)
    ((oddp x) (hotp (+ 1 x) (+ 1 y)))
    (t "end")))
(hotp 3 2)


(defun hotpo (x steps)
  (cond
    ((= x 1) steps)
    ((oddp x) (hotpo (+ 1 (* x 3)) (+ 1 steps)))
    (t (hotpo (/ x 2) (+ 1 steps)))))
(hotpo 7 0)


;; case statement
(setf x 'd)
(case x
  (a 4)
  ((b c) '1)
  (d "hello")
  (otherwise 11111))


(first (list 1 2))
(first '(1 2))
(first (quote (1 2)))
(first '(1 2))
(first (quote (1 2)))

(first (list 'a 'b))
(list 'a 'b)

'a



(setq a 5)
(loop
  (setq a (+ a 1))
  (incf a)
  (when (> a 7)
    (progn
      (print a)
      (return "hahaha"))))


(loop
(setq x 1)
(if (< x 10)(setq x (+ x 1))(return)))



a
(if (< a 5)(print "<7")(print ">=7"))

;; bind a list to variable and return nil when hits end. so it always return nil
(dolist (x '(a b c)) (print x))

;; do (initial value)
;; (termination condition with a return)
;; (body)

(do ((x 1 (+ x 2))
     (y 1 (* y 2)))
    ((> x 20) y)
  (print x)
  (print y))




;; funcall first argument on its remaining
(funcall #'+ 3 4)
;; apply is same but last argument should be a list
(apply #'+ 3 4 12 '(1 2))


;; lambda
((lambda (x) (+ x 12)) 10)

((lambda (x y)(* x y)) 10 20)
(funcall (lambda (x y)(* x y)) 10 20)



(funcall (lambda (x) (* x 3)) 2)
(mapcar (lambda (x) (+ x 2)) '(1 2 3 4))


;; sort is not stable which means if two equal items may appear differently
;; sort-stable on the other hand is a stable one
(sort '(1 2 3 4) #'>)

(print (> 2 4))


(eq 'a 'a)

(= 12 32)



;; list functions

(setf a (append '(1 2) '(32 2 "a")))
a
(reverse a)

(setq x 1)
(loop
  (when (> x 9)(return)(incf x)))

(setq a 11)
(loop
  (if (< a 7)(return "over")
      (progn(print a) (decf a))))

(setq x 1)
(loop (when (evenp x)
	(progn (print x)(return)))
      (incf x))


