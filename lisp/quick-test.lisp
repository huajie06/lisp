;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;;                    this is chapter 3                 ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (x '(a b c))
  (print x))

(defun do-something (&key name)
  (print name))

(do-something :name "abc")

(setf var1
      (list :name "abc" :address "40 main st"))
var1
(getf var1 :address)

(defvar *var-global* nil)

*var-global*
(push 'a *var-global*)
*var-global*
(push 'b *var-global*)

(dolist (x *var-global*)
  (print x))

(setq name-list (quote ("jack" "mike" "alex")))

*var-global*

(print '("jack" 10))

(loop for age in (quote (1 2 3))
      do (print age))

(defun make-user (&key name age)
  (list :name name :age age))


(dolist (x name-list)
  (push (make-user :name x :age 10) *var-global*))

(setf *var-global* nil)
*var-global*

(loop for name in (quote ("jack" "mike" "alex"))
      for age in (quote (10 11 21))
      do (push (make-user :name name :age age) *var-global*))


*var-global*

(getf (make-user :name "hello" :age 20) :age)

;; lambda functions
(remove-if-not #'evenp '(1 2 3 4))

((lambda (x) (/ x 2)) 100)
(remove-if-not #'(lambda (x) (= (mod x 2) 1)) '(1 2 3 4))

*var-global*
(dolist (x *var-global*)
  (print (getf x :name)))

(list :a 1 :b '(c b))
(getf (list :a 1 :b '(c b)) :b)


(remove-if-not
 #'(lambda (user) (equal (getf user :name) "mike")) *var-global*)

(defun where (name)
  (remove-if-not
   #'(lambda (user) (equal (getf user :name) name)) *var-global*))
(where "mike")

(defun select (selector-fn)
  (remove-if-not selector-fn *var-global*))

(select #'(lambda (user) (equal (getf user :name) "mike")))

*var-global*
;; it will do all the search really and find the values
(defun where-new (&key name age)
  #'(lambda (user)
      (and
       (if name (equal (getf user :name) name) t)
       (if age (equal (getf user :age) age) t))))


(where-new :name "mike")
(select (where-new :name "mike"))
(select (where-new :age 21))
;;(select (where-new :gender "male"))


(select (where-new :name "mike" :age 11))

*var-global*

(mapcar
 #'(lambda (row)
     (when (equal (getf row :name) "mike")
       (print row))) *var-global*)


(remove-if-not
 #'(lambda (user) (equal (getf user :name) "mike")) *var-global*)

(defmacro backwards (expr) (reverse expr))
(backwards ((1 2 3) quote))
(backwards ("hello, world" t format))


;; (defun apply-filter (field value)
;;   (remove-if-not
;;    #'(lambda (user) (equal (getf user :field) value)) *var-global*))

;;;backtick
`(1 2 3)
'(1 2 3)
`(1 2 (+ 1 2))
`(1 2 ,(+ 1 2) (+ 2 3))
`(1 2 ,(+ 1 2) ,(+ 2 3))

;;; ,@
`(and ,@(list 1 2 3))
`(and ,(list 1 2 3))

()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;                       ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  need to revisit      ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;                       ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'user field) value))
(make-comparison-expr :age 10)

(defun make-comparison-expr1 (field value)
  `(equal (getf user ,field) ,value))
(make-comparison-expr1 :age 10)

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))
;;(make-comparisons-list :age 10 :name "jack")

(defmacro where2 (&rest clauses)
  `#'(lambda (user) (and ,@(make-comparisons-list clauses))))

(select (where2 :age 10 :name "jack"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;;                syntax and semantics                  ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf x nil)
(if x (format t "~A" "yes") (format t "~A" "no"))


(let ((a 1) (b 2))
  (+ a b)
  a)

;;use c-m-q to indent when cursor at opening paren
;;or c-c m-q to indent whole func
(defun print-list (list)
  (dolist (i list)
    (format t "~a~%" i)))
(print-list '(a xxx bds kwek fjk))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;;                     functions                        ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun foo (a b &optional c d) (list a b c d))

(foo 1 2)
(foo 1 2 3)
(foo 1 2 3 'r)

(defun foo1 (a b &optional (c 10) d) (list a b c d))
(foo1 1 2)

;;;last one indicate if c was user entered
(defun foo2 (a &optional (b 3) (c 1 c-supplied-p11))
  (list a b c c-supplied-p11))
(foo2 1 2 3)
(foo2 1 2 1)
(foo2 1 2)

(defun foo3 (a &rest b)
  (list a b))
(foo3 1 23 21 231 1)


(defun foo4 (a &optional a1 &key b c)
  (list a a1 b c))
(foo4 1 2 :b 2 :c 3)
(foo4 2 :b 2 :c 3) ;will error, don't use mix


;;; apply a function on a list
(defun test (a b &rest y)
  (+ a b (apply '+ y)))
(test 1 2 3 2)
(apply #'+ (list 1 2))
(apply #'+ '(1 2))
(apply #'+ (quote (1 2)))
(apply #'/ (list 12 3))

(defun sum-all(&rest y)
  (let ((x 0))
    (dolist (i y)
      (setf x (+ x i))) x))

(sum-all 1 2 3)

;;; notice the difference
(apply #'sum-all '(1 23 3))
(funcall #'sum-all 1 23 3)

;; this is apply a function to individual element of a list
(mapcar #'exp '(1 2))

(exp 3)
(expt 2 3)

(mapcar #'(lambda (x) (+ 3 x)) '(1 2))

(funcall #'+ 1 2)
(+ 1 2)
(funcall #'exp 2)
(exp 2)

;; the function takes 1 parameter then should be len 1 list
(apply #'+ '(1 2 3))


;;; notice the difference
(apply #'exp '(1))
(funcall #'exp 1)

(apply #'max 4 '(1 2 3))

;; notice paren about the lambda
((lambda (x y)(+ x y)) 1 2)

()


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;;                     variables                        ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((x 1) (y 2))
  (+ x y))

(setf y 100)
;; notice y will be
(let ((x 10) (y (+ x 10)))
  (print y)
  (list x y))

(let* ((x 10) (y (+ x 10)))
  (print y)
  (list x y))

;; closure -> The key thing to understand about closures is that it's
;; the binding, not the value of the variable


(setf x 0)
(funcall #'(lambda () (setf x (+ x 1)))) ; the x value will be retained
(funcall #'(lambda () (setf x (+ x 1))))
(funcall #'(lambda () (setf x (+ x 1))))


;;; global variables

(defvar *g-var-defvar* nil)
(push 'a *g-var-defvar*)
*g-var-defvar*
;; if you re-defvar, the value doesn't change, kind of the binding
;; doesn't change

(defvar *g-var-defvar* nil)
*g-var-defvar*

(defparameter *g-var-defpar* nil)
(push 'b *g-var-defpar*)
*g-var-defpar*
;; now it will reassign the binding
(defparameter *g-var-defpar* nil)
*g-var-defpar*

;; wrap `+` around is also just a convention
(defconstant +c+ 100)
+c+

(setf x 1)
(incf x)
(decf x)


(setf a 1)
(setf b 2)
a
b
(rotatef a b)
b
a


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;;                       macros                         ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if t (format t "this is true")
    (format t "this is false"))

(if t "true" "false")
(if nil "true" "false")

;;; if only allows to do like one-form at a time, you need to use progn

(if t ((print 1) (print 2)) "false") ; this will give error

;; so instead you need to do this
(if t (progn
        (setf x 1)
        (incf x)) "this is where false happen")

;;; a close look at the `when` macro
;;; it's defined in this way
;;; (defmacro when (condition &rest body)
;;;        `(if ,condition (progn ,@body)))
(when t "a")
(when t (setf a 1)
      (setf b (/ a 2))
      (setf c (* b 100)))

;;; so we can define a second when similarly
(defmacro when2 (condition &rest body)
  `(if ,condition (progn ,@body)))

(when2 t (write-line "hello"))
(when2 t (setf x "hello"))

(unless nil "do something only when nil")
(when t "do something only when t" )

(setf a 2)
(cond ((= a 1) (write-line "this is 1"))
      ((= a 2) (write-line "this is 2"))
      (t (write-line "this is else")))

;; and or not

(and t t nil)
(or t t nil)
(and (not t) (not t) nil)
(or (not t) (not t) nil)


(dolist (x '(1 2 3))
  (print x))

;; start from 0, use return to breakout
(dotimes (i 10)
  (if (< i 5) (print i) (return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;                       ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  need to revisit      ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;                       ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; loop form, the initial value and step-form

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;;              chapter 8: define macro                 ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; integer
(isqrt 10)
(sqrt 10)

(setf x 10)
(loop for fac from 2 to (isqrt x)
      never (zerop (mod x fac)))

;;; never => if every single form is evaluted as `nil` then return T,
;;; otherwise, as long as one has `T` it will return nil
(loop for i from 2 to 10
      never (= i 20))


;; function to determine if it's prime
;;(setf q (floor (sqrt 10)))
(defun primep (x)
  (loop for i from 2 to (floor (sqrt x))
  never (= (mod x i) 0)))

(defun primep2 (x)
  (loop for i from 2 to (isqrt x)
  never (zerop (mod x i))))

(primep 7)
(primep2 7)

(loop for i from 2 to (sqrt 10)
      (print i))


;;; function to find next prime
;;; the return will break out
(loop for i from 1 when (> i 10) return i)

(loop for i from 100 when (primep i) return i)

(primep 2)

(defun next-prime (x)
  (loop for i from (incf x) when (primep i) return i))

(next-prime 101)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         macros         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf a (list 1 2))      ;this is what i want

(defmacro setf2 (a b c)
  `(setf a (list ,b ,c)))

(setf2 a 1 2)

(macroexpand-1 '(setf2 a 2 3))
(macroexpand '(setf2 a 2 3))

;; example below
(list 'setf x 3)
(defmacro play (x)
  (list 'setq x 3))

(macroexpand '(play x))
(play x)

;;; thinking of doing this
(setf x y 1)
;;; but it should be
(progn (setf x 1) (setf y 1))

(list x y)
(defmacro setf3 (x y val)
  `(progn (setq x ,val) (setq y , val)))

(setf3 x y 100)
(macroexpand '(setf3 x y 100))

'(list 1 2)
`(list 1 2)
`(,list 1 2) ; this will give error

(setf x 10)
`(list x 2)
`(list ,x 2)

;;; do loop, initial value, value step
(do ((var1 3 (1+ var1)))
    ((> var1 10) (* var1 2)) ;; the * is the final statement, only 1 exist paren
  (print var1)               ;; this is the action during loop, can have as many
  (print "---")
  (print (+ var1 2)))

(macroexpand '(do ((var1 1 (1+ var1))) ((> var1 3)(+ var1 1000))))

(macroexpand '(setf x 1))
(macroexpand '(setq x 1)) ;; this is not a macro




