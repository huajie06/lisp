
(1 2 3)
'(1 2 3)
(list 1 2 3)

(equal '(1 2 3) (list 1 2 3))

;; funcall vs. apply

(funcall #'(lambda (x)(+ x 1)) 1)
(apply #'(lambda (x)(+ x 1)) '(1))

(funcall #'(lambda (x y)(+ x y)) 1 2)
(apply #'(lambda (x y)(+ x y)) '(1 2))

;; apply takes a list, funcall not a list
(apply #'+ '(1 2 3))
(apply #'+ 12 3 nil)
(apply #'(lambda (x y) (* x y)) '(1 2))

(lambda (x y) (* x y))(1 2) ; this will not eval!!

;;===================================================================
;; ========= mapcar, mapc and mapcan on element of list =============
;;========== maplist, mapl, mapcon on sublist =======================
;;===================================================================


;; mapcar, the FN must take as many parameters as there are lists
(mapcar #'+ '(1 2) '(2 3 4)) ; + takes as many
(mapcar #'car '((1 a) (2 b) (3 c))) ; car takes 1
(mapcar #'(lambda (x) (+ 1 x)) '(1 2 3)) ; takes 1
(mapcar #'(lambda (x y) (+ x y)) '(1 2) '(6 3)) ; takes 2
(mapcar #'+ '(1 2 3) '(2 3 4))

(mapcar #'+ '('(1 2 3) '(2 3 4))) ; no work

;; mapc on process the input for side effect, and return the first input
(mapc #'(lambda (x y) (print (* x y))) (list 1 0 2) (list 3 4 5))
(mapc #'+ '(1 2 3))
(mapc #'+ '(1 2 3) '(2 3 4))

(mapcar #'list (list 1 2 3) (list 4 5 6))

;; compbine apply and mapcar, you can do this
(mapcar
 #'(lambda (x) (apply #'+ x))
 '((1 2 3) (3 4 5)))

(mapcar
 #'(lambda (x) (apply #'(lambda (a b c) (* a b c)) x))
 '((1 2 3) (3 4 5)))
(mapcar
 #'(lambda (x) (apply #'* x))
 '((1 2 3) (3 4 5)))


;;map
(map 'list #'(lambda (x y) (mod (+ x y) 16))
     '(1 2 3 4)
     '(10 9 8 7))

(map 'vector #'(lambda (x y) (mod (+ x y) 16))
     '(1 2 3 4)
     '(10 9 8 7))

(map 'list #'(lambda (x y) (or (equal y 10) y))
     '(1 2 3 4)
     '(10 9 8 7))

(map 'vector #'(lambda (x y) (and (equal y 10) x))
     '(1 2 3 4)
     '(10 9 8 7))


;; some interesting cases?
(mapcar '#+ '((1 2 3) (3 4 5 6))) ;; this doesn't work
;;=========================================
(apply #'mapcar #'+ '((1 2 3) (3 4 5 6)))
(mapcar #'(lambda (x) (apply #'+ x)) '((1 2 3) (2 3 4)))
;;=========================================

(funcall #'mapcar #'+ '(1 2 3) '(1 2))
(mapcar #'+ '(1 2 3) '(1 2))

(apply #'mapcar #'(lambda (&rest rest)(print rest)) '((1 2 3) (3 4 5 6)))
(apply #'(lambda (x y) (print (list x y))) '('a 'b))

;;;
(mapcar #'(lambda (rest) (apply #'+ rest)) '((1 2 3) (2 3 4)))
(mapcar #'(lambda (a b c) (+ a b c)) '(1 2 3) '(2 3 4) '(1 0 0))
(mapcar #'(lambda (&rest rest) (apply #'+ rest)) '(1 2 3))
(mapcar #'(lambda (&rest rest) (print rest)) '(1 2 3))

(apply #'mapcar #'list '((1 2 3) (3 4 5 6)))

;; transpose
(defparameter *lists* '((a0 a1 a2 a3) (b0 b1 b2 b3) (c0 c1 c2 c3)))
(apply #'mapcar #'list *lists*)
(funcall #'mapcar #'list '(a0 a1 a2 a3) '(b0 b1 b2 b3) '(c0 c1 c2 c3))
(mapcar #'list '(a0 a1 a2 a3) '(b0 b1 b2 b3) '(c0 c1 c2 c3))

(apply #'(lambda (&rest rest) (apply #'+ rest)) '(1 2 3 4))

;; mapcan
(mapcan #'(lambda (x) (+ 1 x)) '(1 2 9))
(mapcan #'list (list 1 2 3) (list 4 5 6))


;; maplist
(maplist #'(lambda (x) (cons 'foo x))
         '(a b c d))

;; multiple collect
(loop for i from 0 to 12 by 3
      collect i)

(loop for i from 1 to 3
      collect i collect (* 2 i))

(loop for i from 1 to 3
      collect i collect (* 2 i) collect i)
;; ====
(loop for i in '(1 2 3 4) by #'cddr
      do (print i))
(cddr '(1 2 3 4))

(loop for (a b) on '(1 2 3 4) by #'cddr
      do (print (list a b)))

(loop for (a b) on '(:first-name ("jack" "mike" "new")) by #'cddr
      do (format t "a:~a~%b:~b" a b))

(loop for (a b) on '(:first-name "jack" "mike" "new") by #'cddr
      do (format t "a:~a~% b:~b" a b))

(loop for i downfrom 10 to 1
      do (print i))

(loop for i downfrom 10 to 1
	thereis (> i 5))

(loop for i downfrom 10 to 1
      never (> i 10))

(loop for i in '(t t t)
      always (eq i t))

(loop for i in '(t 4 t)
	thereis (eq i t))

(loop for i in '(1 2 3 nil 1)
      always (evenp i))

(loop for i from 2 to 100
      always (> i 1))

(every #'(lambda (x) (eql x t)) '(t t t))

(loop for ch across "foobar"
      always (eq ch #\a))


==
;; ====
(loop repeat 10
      for x = (random 1000)
      maximizing x into biggest
      minimizing x into smallest
      summing x into total
      collecting x into xs
      ;;when (> x 500) return nil
      finally (return (values biggest smallest total xs)))
(loop for n from 1 to 10
      when (evenp n) collect n into evens
	else collect n into odds
      finally (return (values evens odds)))

(loop repeat 5
      for x = (random 10)
      collect x
      finally (return nil))

(loop repeat 5
      for x = (random 10)
      collect x)


(list 1 b c) ; this will give B unbound error
'(1 b c) ; but this will work fine
'(print 'a)
(eval '(print 'a))
(eval '(print a)) ; a is unbound

(loop for i in '(1 b c) ; this works
      do (print i))



(let ((a 1))
  (print a)
  (print (+ a 1))
  (print (+ a 2))
  0)


(loop for i in '(1 2 3)
      do (print (+ i 2)))

;; this is a let if look at macro
(do ((i 0 (+ 1 i))
     (j 0 (+ 2 j)))
    ((> (* i j) 50))
  (format t "i = ~a, j = ~a~%"i j))


(nconc '(12 2) '(2 3))
(append '(1 2) '(3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;;                   built-in data types                ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; numbers operations

(floor 1.123)
(ceiling 1.123)
(round 1.23)

(mod 10 3)
(rem 10 3)

(setf x 1)
(incf x)
(decf x)
(incf x 10)
(decf x 3)

(zerop 9)
(zerop 0)
(minusp 1)
(minusp -1)
(plusp 1)
(plusp -1)
(evenp 9)
(oddp 10)


;;; chars
(string= "hello" "qqq")
(string/= "ac" "bb")
(string/= "ac" "ab") ; return the first position of a non-match

(string= "what is today" "abcd is" :start1 6 :end1 7 :start2 6 :end2 7)
(string= "what" "what")


;;; vectors
(vector 1 2 3)
(make-array 3 :initial-element 1)

(defparameter *x* (make-array 5 :fill-pointer 0))
(vector-push 'a *x*)
*x*

(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)

(defparameter *x* (vector 1 2 3))
(length *x*)
(elt *x* 0)
(elt *x* 1)
(elt *x* 6) ; will give error
(aref *x* 1)
(setf (elt *x* 0) 9999)
*x*

(count 1 (vector 1 1 2))
(remove 1 (vector 1 1 2 3 21 2 3))
(remove 1 '(1 1 2 3 21 2 3))
(remove 'a (vector 'a 1 2 3 21 2 3))
(remove #\a "foobarbaz")
(substitute 10 1 '(1 2 1 2 3 1 2 3 4))


(count "foo" (vector "foo" "foo" "xxx"))
(count "foo" (vector "foo" "foo" "xxx") :start 1 :test #'string=)

(count-if #'evenp #(1 2 3 4 5))
(count-if-not #'evenp #(1 2 3 4 5))

(remove-if-not #'(lambda (x) (char= (elt x 0) #\h)) (vector "hello" "nihao" "xhello"))
(remove-duplicates #(1 2 1 2 3 1 2 3 4))


(concatenate 'vector (vector 1 2) (vector "hello" "world"))
(concatenate 'list '(1 2 3) '(a b))
(concatenate 'string "abc" "bcd")
(concatenate 'string "abc" '(#\a))

(sort (vector "a" "c" "baz") #'string<)
(sort (vector "a" "c" "baz") #'string>)
;;(sort (vector "a" "c" "baz") #'char>)

(setq tester (list 1 2 3 4 5 6 7 8 9 0)) =>  (1 2 3 4 5 6 7 8 9 0)
(stable-sort tester #'(lambda (x y) (and (oddp x) (> y 3))))
(stable-sort tester #'>)
;; say want to sort list of list
(defparameter *test-list
  '((:a 99 :b "a" :c 3)
    (:a 0 :b "b" :c 3)
    (:a 0 :b "c" :c 3)))

(let ((names '(:c :b :b))
      (la '(:a 99 :b 1 :c 3))
      (lb '(:a 0 :b 2 :c 3)))
  (loop for n in names
	for val-a = (getf la :a)
	for val-b = (getf lb :b)
	do
	   (print (list n val-a val-b))
	when (> val-a val-b) return t
	  when (< val-a val-b) return nil
	    finally (return nil)))

(defun sort-fn (vars)
  (let ((var vars))
    #'(lambda (a b)
	(loop for i in var
	      for val-a = (getf a i)
	      for val-b = (getf b i)
	      when (< val-a val-b) return t
		when (> val-a val-b) return nil
		  finally (return nil)))))
;;(sort-fn (list :a :b))
(sort *test-list (sort-fn (list :a :c)))
==

(merge 'list '(1 3 5) '(2 4 6) #'<)
(merge 'vector #("ab" "b") #("a" "z") #'string<)

(defparameter *x* (copy-seq "foo=bar=baz"))
(subseq *x* 0 3)
(setf (subseq *x* 4 7) "xxx")
*x*

(position #\b "foobarbaz") ; search for a single item
(search "bar" "foobarbaz") ; search for a sequence

(member "jack" '("jack") :test #'equal)
(eql "jack" "jack")
(equal "jack" "jack")

(member "jack" '("jack" "mike") :test #'string=)
(member 3 '(1 2 3) :test #'=)

(every #'evenp #(1 2 3 4 5))    ==> NIL
(some #'evenp #(1 2 3 4 5))     ==> T
(notany #'evenp #(1 2 3 4 5))   ==> NIL
(notevery #'evenp #(1 2 3 4 5)) ==> T

(every #'(lambda (x) (if (> x 3) t nil)) #(1 2 3 4 5))
(some #'(lambda (x) (if (> x 3) t nil)) #(1 2 3 4 5))

(some #'> #(1 2 3 4) #(5 4 3 2))
(every #'> #(1 2 3 4 5) #(5 4 3 2))

(map 'vector #'max #(1 4 293 2 3) #(1 2 3 4 5))
(reduce #'max #(1 4 293 2 3))

;;; gethash returns two things
(setf tb (make-hash-table))
(gethash 'too tb)
(setf (gethash 'too tb) 'xxx)
(setf (gethash 'too1 tb) nil)
(setf (gethash 'too2 tb) 'hello)
tb
(gethash 'too tb)
(if (gethash 'too tb) 'find 'no-find)
(if (gethash 'too1 tb) 'find 'no-find)
(gethash 'too1 tb)
(remhash 'too1 tb)

(multiple-value-bind (result find-or-not) (gethash 'too1 tb)
  (if find-or-not
      (format t "---value found: ~a" result)
      (format t "---not find")))


(maphash #'(lambda (k v) (format t "key: ~a, value: ~a~%" k v)) tb)
