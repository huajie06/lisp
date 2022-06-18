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

(merge 'list '(1 3 5) '(2 4 6) #'<)
(merge 'vector #("ab" "b") #("a" "z") #'string<)

(defparameter *x* (copy-seq "foo=bar=baz"))
(subseq *x* 0 3)
(setf (subseq *x* 4 7) "xxx")
*x*

(position #\b "foobarbaz") ; search for a single item
(search "bar" "foobarbaz") ; search for a sequence

(every #'evenp #(1 2 3 4 5))    ==> NIL
(some #'evenp #(1 2 3 4 5))     ==> T
(notany #'evenp #(1 2 3 4 5))   ==> NIL
(notevery #'evenp #(1 2 3 4 5)) ==> T


(print "a")
