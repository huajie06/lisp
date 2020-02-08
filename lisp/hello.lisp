;; (write-line "hello world")

(defun hello ()
  (format t "hello, world!~%"))

(hello)

(defun h ()
  (print "hello"))

(h)

;; (print (1 23 3)) this is not valid because the list is not valid

(setq a 5)


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
(defun test1 (a &optional (b 1))
  (+ a b))


(test1 1)

(test1 2 3)

;; y is (1 2) so needs to transfer somehow into (+ 1 2)
(defun test2 (a b &rest y)
  (+ a b (apply '+ y)))

(test2 1 2 3 2)

;; key arguments
(defun test (&key a b)
  (+ a b))

(test :a 1 :b 2)

;; key arguments with default
(defun t1 (&key (c 2)) c)
(t1 :c 222)
(t1)


(defun t2 (&key (a 2) b)
  (+ a b))
(t2 :b 2)
(t2 :a 12 :b 2)

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



;; binding
;; inside of let, ((a 1) (b 2)), a and b are not accessible through each other
(let ((a 1) (b 2)) (setq c (+ a b)))


(let ((a 1)
  (b 2)
  (c 3))
  (setq cc (+ a b c))
  cc)

(let ((a 3)) a)

