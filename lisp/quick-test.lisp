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

(apply #'exp '(1))
(funcall #'exp 1)

(apply #'max 4 '(1 2 3))

()







