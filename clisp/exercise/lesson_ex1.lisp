;; with
;; format t = tabulation
(loop for i from 2 to 5
      for s in '("today" "tomorrow" "this is next week")
      with y = 100 do
	(format t "~&~a:~20t~,2f%" s (/ (* i 100) y)))

;; values give a multi-return type of form
;; so use multiple-value-bind
(multiple-value-bind (a b) (values 1 2)
  (list a b))

;; when return value is a list and wants to break it
(destructuring-bind (a b) (list 'a 'b)
  (list a b))

;; it can take &key and allow-other-keys, it works with plist
(destructuring-bind (&key a b &allow-other-keys)
    (list :a 'val1 :b 'val2 :c "ddd")
  (list a b))


;; array :fill-pointer reserves value if nill
(defparameter *test-array* (make-array 10 :adjustable t))
(defparameter *test-array1* (make-array 10 :adjustable t :fill-pointer 3))
(defparameter *test-array2* (make-array 10 :adjustable t :fill-pointer 0))
*test-array*  ==>10 zeros, it can not push
*test-array1* ==>only 3 zeros
*test-array2* ==>no zeros 
(vector-push (list 1 2) *test-array2*)
*test-array2*
(aref *test-array2* 0)


;; use make-hash-table with :test to tell how to match on keys
(defparameter *test-hash-table* (make-hash-table :test 'equal))


;;; ecase will give error if there's a `otherwise` class
(let ((x 9))
  (ecase x
    (1 1)
    (2 2)))

(let ((x 9))
  (case x
    (1 1)
    (2 2)
    (otherwise nil)))


;; some useful loop and let
;; it returns ==> ((A . 0) (B . 0) (C . 0))
(let* ((vars (list 'a 'b 'c))
       (var-cons (loop for i in vars collect (cons i 0))))
  var-cons)

;; can do some interesting stuff
(let* ((vars (list 'a 'b 'c))
       (var-cons (loop for i in vars collect (cons i 0))))
  (loop for i from 1 to 10
	for j from 100 downto 1
	with k = 10
	do
	   (incf (cdr (assoc 'a var-cons)) i)
	   (incf (cdr (assoc 'b var-cons)) j)
	   (incf (cdr (assoc 'c var-cons)) k))
  var-cons)

;; assoc works with `a list` == list of list and return based on first element match
(assoc 'a '((a 999)(b 'b)(c 'c c)))
(assoc 'b '((a 999)(b 'b)(c 'c c)))
(assoc 'c '((a 999)(b 'b)(c 'c c)))


;; subseq works like list indexing
(subseq (list 1 2 3 4) 0 1)
(subseq (list 1 2 3 4) 1)
(subseq (list 1 2 3 4) 0 (length (list 1 2)))

;; remove dups
(delete-duplicates '(1 2 3 1 2))

;; syntaxt of loop hashtable
(loop for k being each hash-key of *feature-database*
      do (format t "key: ~a, value:~a~%" k (gethash k *feature-database*)))

(loop for v being each hash-value of *feature-database*
      do (print v))

