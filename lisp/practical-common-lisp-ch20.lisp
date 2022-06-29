;;;;;

;; flet, label

(defun do-something (x)
  (flet ((do-sub-something (y)
	   (incf y 1)))
    (do-sub-something x)))

(do-something 10)


(tagbody
 a (print 'a) (if (zerop (random 2)) (go c))
 b (print 'b) (if (zerop (random 2)) (go a))
 c (print 'c) (if (zerop (random 2)) (go b)))


(defun do-something2 (x y)
  (if (oddp y)(return-from do-something2 x)
      (* x y)))
(do-something2 1 2)
(do-something2 1 3)

(block
    some-stuff
  (format t "i'm printing ~a" "pring")
  999)


(block
    some-stuff
  (setf x 99)
  (format t "i'm printing ~a" "pring")
  (when (> x 10) (return-from some-stuff 9))
  999)


;;; block -> return-from
;;; loop -> return 


(block
    block-999
  (loop for i from 1 to 100
	while (> i 5)  collect i))

(loop for i from 1 to 10 
      while (> i 5))

(loop for i from 1 to 10 
      do (print i))

;;; loop while will terminate the loop immediately!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop for i from 1 to 10 
      when (> i 5) do (print i))

(loop for i from 1 to 10 
      when (> i 5) do (break))

(loop for i from 1 to 10 
      when (> i 5) do (return i))

(loop for i from 1 to 10 
      while (> i 5) do (return i))


(loop for x in '(1 2 3 4 5)
      while (evenp x)
      collect x)

(loop for x in '(1 2 3 4 5)
      while (< x 4) do (print x))

(loop for i from 1 to 10 
      while (< i 4) do (print i))

(loop for i in '(3 2 3 32 3)
      while (= i 3) collect i)

(loop for i in '(1 3 2 3 32 3)
      while (= i 3) collect i)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((x 0))
  (tagbody
   a
     (incf x)
     (when (> x 3) (progn (print x)(go end)))
   b
     (decf x 0.5)
     (print x)
     (go a)
   end)
  x)



(tagbody
   (let ((x 3))
     (unwind-protect
	  (if (numberp x) (go out))
       (print x)) "this is let")
 out)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(find-symbol "map")
(intern "map")



*package*
cl:*package*

:a
keyword:a
(eql :a keyword:a)
