(ql:quickload :drakma)
(ql:quickload :yason)
(ql:quickload :lquery)
(ql:quickload :cl-ppcre)



;; https://www.megamillions.com/

(defparameter *file-name* "html_string.html")
(defparameter *html-string* nil)

;; (with-open-file
;;     (stream *file-name* :direction :input :if-does-not-exist nil)
;;   (if stream
;;       (loop for line = (read-line stream nil)
;; 	    for counter from 0
;; 	    while (and line (< counter 10)) do (print line))
;; (print "file does not exist")))


(with-open-file
    (stream *file-name* :direction :input :if-does-not-exist nil)
  (if stream
      (let ((string-result (make-string (file-length stream))))
	(read-sequence string-result stream)
	(setf *html-string* string-result))
      (print "file does not exist")))

*html-string*


(defun single-arr->atom (single-arr)
  (aref single-arr 0))

(defun parse-atag-to-list (a-tag)
  (list
   :date (aref (lquery:$ a-tag "h5.drawItemDate" (text)) 0)
   :ball1 (aref (lquery:$ a-tag "li[class=\"ball pastNum1\"]" (text)) 0)
   :ball2 (aref (lquery:$ a-tag "li[class=\"ball pastNum2\"]" (text)) 0)
   :ball3 (aref (lquery:$ a-tag "li[class=\"ball pastNum3\"]" (text)) 0)
   :ball4 (aref (lquery:$ a-tag "li[class=\"ball pastNum4\"]" (text)) 0)
   :ball5 (aref (lquery:$ a-tag "li[class=\"ball pastNum5\"]" (text)) 0)
   :yellow (aref (lquery:$ a-tag "li[class=\"ball yellowBall pastNumMB\"]" (text)) 0)))

(defparameter *result-1* nil)
(let* ((to-parse (lquery:$ (initialize *html-string*)))
       (array-of-a-tags (lquery:$ to-parse "a")))
  (setf *result-1*
	(loop for elem across array-of-a-tags
	      collect (parse-atag-to-list elem))))

(defun occurrences (lst)
  (let ((table (make-hash-table :test 'equal)))      ; [1]
    (loop for e in lst
          do (incf (gethash e table 0)))             ; [2]
    (sort (loop for k being the hash-key of table    ; [3]
                  using (hash-value v)
                collect (cons k v))
          #'>= :key #'cdr)))                         ; [4]


(nth 0 *result-1*)



(loop for ele in *result-1*
      collect (getf ele :yellow))

(occurrences (loop for ele in *result-1*
		   collect (getf ele :yellow)))

(defun get-balls (&key ball)
  (loop for ele in *result-1*
	collect (list (getf ele :date)(getf ele ball))))

(defun freq-balls (&key ball)
  (occurrences (loop for ele in *result-1*
		     collect (getf ele ball))))

(get-balls :ball :ball1)
(freq-balls :ball :ball1)

;; ball1 6

(freq-balls :ball :ball2)
(get-balls :ball :ball2)

;; ball2 15

(get-balls :ball :ball3)
(freq-balls :ball :ball3)

;; ball3 38

(get-balls :ball :ball4)
(freq-balls :ball :ball4)

;; ball4 46

(freq-balls :ball :ball5)


;; ball5 53

(get-balls :ball :yellow)
(freq-balls :ball :yellow)
;; yellow 22


(remove-if-not '(lambda (x) (cons x)))


(defun select (selector-fn &key in-list)
  (remove-if-not selector-fn in-list))

(defun eq-ym (&key (year 2022) (month 1) )
  #'(lambda (ele)
      (and
       (= year (parse-integer
		(nth 2 (cl-ppcre:split "/" (car ele))) :junk-allowed t))
       (= month (parse-integer
		 (nth 0 (cl-ppcre:split "/" (car ele))) :junk-allowed t))
       )))

(defun le-ym (&key (year 2022) (month 1) )
  #'(lambda (ele)
      (and
       (>= year (parse-integer
		 (nth 2 (cl-ppcre:split "/" (car ele))) :junk-allowed t))
       (>= month (parse-integer
		  (nth 0 (cl-ppcre:split "/" (car ele))) :junk-allowed t))
       )))

(defun ge-ym (&key (year 2022) (month 1) )
  #'(lambda (ele)
      (and
       (<= year (parse-integer
		 (nth 2 (cl-ppcre:split "/" (car ele))) :junk-allowed t))
       (<= month (parse-integer
		  (nth 0 (cl-ppcre:split "/" (car ele))) :junk-allowed t))
       )))


(load "/Users/huajiezhang/repo/lisp/clisp/utilities/utl.lisp")

(select (where :month 11)
	:in-list (get-balls :ball :yellow))

(select (ge-ym :month 9 :year 2022)
	:in-list (get-balls :ball :yellow))

(format-print
 (select (ge-ym :month 8 :year 2022)
	 :in-list (get-balls :ball :yellow))
 :align :right)

==

(defparameter test1
  (aref (lquery:$ (lquery:$ (initialize *html-string*)) "a") 0))

(defparameter _tmp_ (list :DATE #("11/8/2022") :BALL1 #("5") :BALL2 #("13") :BALL3 #("29") :BALL4
			  #("38") :BALL5 #("59") :YELLOW #("23")))

;; _tmp_

;; (loop for (key value) on _tmp_ by #'cddr
;;       collect (list key (single-arr->atom value)))


==
(format-print
 *result-1*)

(equal (nth 1 *result-1*) (nth 1 *result-1*))

(equal (list 1 2) (list 2 1))
(eql (list 1 2) (list 2 1))
(eq (list 1 2) (list 2 1))

(eq (list 1 2) (list 1 2))
(eql (list 1 2) (list 1 2))
(equal (list 1 2) (list 1 2))



(defun draw-balls (b1 b2 b3 b4 b5 &key y)
  (cons (cons 'y y)
	(loop for i in (sort (list b1 b2 b3 b4 b5) #'<)
	      for ind from 1
	      collect (cons ind i))))

(assoc 'y (draw-balls 1 2 3 4 5 :y 29))
(assoc 1 (draw-balls 1 2 3 4 5 :y 29))

(defun winning-balls (b1 b2 b3 b4 b5 &key y date)
  (cons (cons 'date date)
	(cons (cons 'y y)
	      (loop for i in (sort (list b1 b2 b3 b4 b5) #'<)
		    for ind from 1
		    collect (cons ind i)))))

==
(winning-balls 1 5 17 30 70 :y 22 :date "11/11/2022")

(defun get-white-balls (ball-list)
  (loop for i from 1 to 5
	with draw = ball-list
	collect (cdr (assoc i draw))))

(defun find-match-ball (draw win)
  (let ((result (sort
		 (intersection (get-white-balls draw)
			       (get-white-balls win)) #'<)))
    (if (equal (assoc 'y draw) (assoc 'y win))
	(cons (cons 'Yellow (cdr (assoc 'y draw))) result)
	result)))

(find-match-ball
 (draw-balls 5 17 38 46 53 :y 2)
 (winning-balls 1 5 17 30 70 :y 22 :date "11/11/2022")
 )



(defun html->ball (row)
  (let ((white-balls
	  (sort (list
		 (parse-integer (getf row :ball1))
		 (parse-integer (getf row :ball2))
		 (parse-integer (getf row :ball3))
		 (parse-integer (getf row :ball4))
		 (parse-integer (getf row :ball5))) #'<))
	(yellow-ball (parse-integer (getf row :yellow))))
    ;;(date (getf row :date)))
    ;;(draw-balls white-balls yellow-ball)
    ;;(list white-balls yellow-ball)
    (apply #'draw-balls
	   (concatenate 'list white-balls
			(list ':y yellow-ball)))))


(defun html->win-ball (row)
  (when row
    (let ((white-balls
	    (sort (list
		   (parse-integer (getf row :ball1))
		   (parse-integer (getf row :ball2))
		   (parse-integer (getf row :ball3))
		   (parse-integer (getf row :ball4))
		   (parse-integer (getf row :ball5))) #'<))
	  (yellow-ball (parse-integer (getf row :yellow)))
	  (date (getf row :date)))
      ;;(draw-balls white-balls yellow-ball)
      ;;(list white-balls yellow-ball)
      (apply #'winning-balls
	     (concatenate 'list white-balls
			  (list ':y yellow-ball)
			  (list ':date date))))))



(nth 1 *result-1*)

(html->ball (nth 1 *result-1*))

(find-match-ball
 (draw-balls 6 15 38 46 53 :y 22)
 (html->win-ball (nth 1 *result-1*)))

(find-match-ball
 (draw-balls 6 15 38 46 59 :y 22)
 (html->win-ball (nth 1 *result-1*)))

(get-white-balls
 (html->win-ball (nth 1 *result-1*)))


(format-print
 (let ((draw (draw-balls 6 15 38 46 59 :y 22)))
   (loop for i in *result-1*
	 when (find-match-ball draw (html->win-ball i))
	   collect (list
		    (cdr (assoc 'date (html->win-ball i)))
		    (cdr (assoc 'y (html->win-ball i)))
		    (get-white-balls (html->win-ball i))
		    (find-match-ball draw (html->win-ball i))
		    ))))

===

(defun find-all-matches (draw-result)
  (format-print
   (let ((draw draw-result))
     (loop for i in *result-1*
	   when (find-match-ball draw (html->win-ball i))
	     collect (list
		      (cdr (assoc 'date (html->win-ball i)))
		      (cdr (assoc 'y (html->win-ball i)))
		      (get-white-balls (html->win-ball i))
		      (find-match-ball draw (html->win-ball i)))))
   :align :right))

(find-all-matches (draw-balls 1 5 17 37 70 :y 22))
