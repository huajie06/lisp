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
;;; it will check key present, if yes -> do the search
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

