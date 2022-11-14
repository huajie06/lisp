(ql:quickload :drakma)
(ql:quickload :yason)
(ql:quickload :lquery)
(ql:quickload :cl-ppcre)

;; https://www.megamillions.com/

(defparameter *file-name* "previous_draw_20190101_20221108.html")
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
	(setf *html-string* string-result)
	(print "string created"))
      (print "file does not exist")))

;;*html-string*


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
	      collect (parse-atag-to-list elem)))
  (if (> (length *result-1*) 1)
      (print "succeed")))

(defun occurrences (lst)
  (let ((table (make-hash-table :test 'equal)))      ; [1]
    (loop for e in lst
          do (incf (gethash e table 0)))             ; [2]
    (sort (loop for k being the hash-key of table    ; [3]
                  using (hash-value v)
                collect (cons k v))
          #'>= :key #'cdr)))                         ; [4]


;; (loop for ele in *result-1*
;;       collect (getf ele :yellow))

(loop for i in
	    (occurrences (loop for ele in *result-1*
			       collect (getf ele :yellow)))
      summing (cdr i) into sum-val
      finally (return sum-val))


(defun get-balls (&key ball)
  (loop for ele in *result-1*
	collect (list (getf ele :date)(getf ele ball))))

(defun freq-balls (&key ball)
  (occurrences (loop for ele in *result-1*
		     collect (getf ele ball))))

(defun pct-ball (ball &optional (end-ind 10))
  (format-print
   (subseq
    (loop for i in
		(occurrences (loop for ele in *result-1*
				   collect (getf ele ball)))
	  collect (list (car i) (format nil  "~,vf" 3 (/ (cdr i) 404))))
    0 end-ind
    )))

;; all ball being selected
(loop for i in
	    (occurrences
	     (concatenate 'list
			  (loop for i in *result-1* collect (getf i :ball1))
			  (loop for i in *result-1* collect (getf i :ball2))
			  (loop for i in *result-1* collect (getf i :ball3))
			  (loop for i in *result-1* collect (getf i :ball2))
			  (loop for i in *result-1* collect (getf i :ball5))))
      summing (cdr i) into sum-val
      finally (return sum-val)) ; 2020

(format-print
 (subseq
  (loop for i in
	      (occurrences
	       (concatenate 'list
			    (loop for i in *result-1* collect (getf i :ball1))
			    (loop for i in *result-1* collect (getf i :ball2))
			    (loop for i in *result-1* collect (getf i :ball3))
			    (loop for i in *result-1* collect (getf i :ball2))
			    (loop for i in *result-1* collect (getf i :ball5))))
	collect (list (car i) (format nil "~,vf" 3(/ (cdr i) 2020))))
  0 30))




;; (get-balls :ball :ball1)
;; (freq-balls :ball :ball1)

(pct-ball :yellow) ; 22
(pct-ball :ball1)  ; 3
(pct-ball :ball2)  ; 14
(pct-ball :ball3)  ; 31
(pct-ball :ball4)  ; 48
(pct-ball :ball5)  ; 64

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

(defun get-white-balls (ball-list)
  (loop for i from 1 to 5
	with draw = ball-list
	collect (cdr (assoc i draw))))

(defun find-match-ball (draw win)
  "find a match between ball drawed and target"
  (let ((result (sort
		 (intersection (get-white-balls draw)
			       (get-white-balls win)) #'<)))
    (if (equal (assoc 'y draw) (assoc 'y win))
	;; (cons (cons 'Yellow (cdr (assoc 'y draw))) result)
	(cons 'y result)
	result)))

(find-match-ball
 (draw-balls 5 17 38 46 53 :y 2)
 (winning-balls 1 5 17 30 70 :y 22 :date "11/11/2022"))

(defun filter-match-cnt (match-result &key (cnt 2))
  "only return when match ball # greater than 2"
  (unless (< (length match-result) cnt)
    match-result))

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

;; (html->ball (nth 1 *result-1*))

;; (find-match-ball
;;  (draw-balls 6 15 38 46 53 :y 22)
;;  (html->win-ball (nth 1 *result-1*)))

;; (find-match-ball
;;  (draw-balls 6 15 38 46 59 :y 22)
;;  (html->win-ball (nth 1 *result-1*)))

;; (get-white-balls
;;  (html->win-ball (nth 1 *result-1*)))


;; (format-print
;;  (let ((draw (draw-balls 6 15 38 46 59 :y 22)))
;;    (loop for i in *result-1*
;; 	 when (find-match-ball draw (html->win-ball i))
;; 	   collect (list
;; 		    (cdr (assoc 'date (html->win-ball i)))
;; 		    (cdr (assoc 'y (html->win-ball i)))
;; 		    (get-white-balls (html->win-ball i))
;; 		    (find-match-ball draw (html->win-ball i))))))

===

(defun find-all-matches (draw-result)
  (format-print
   (let ((draw draw-result))
     (loop for i in *result-1*
	   when (filter-match-cnt (find-match-ball draw (html->win-ball i)))
	     collect (list
		      (cdr (assoc 'date (html->win-ball i)))
		      (cdr (assoc 'y (html->win-ball i)))
		      (get-white-balls (html->win-ball i))
		      (find-match-ball draw (html->win-ball i)))))
   :align :right))

;; (find-all-matches (draw-balls 1 5 17 37 70 :y 22))
(find-all-matches (draw-balls 3 14 31 48 64 :y 22))

(find-all-matches (draw-balls 3 14 31 48 69 :y 24))

(find-all-matches (draw-balls 3 14 31 48 64 :y 25))

(find-all-matches (draw-balls 14 15 17 20 24 :y 25))


(defun combination (n lst)
  (cond
    ((zerop n) nil)
    ((= n 1) (mapcar #'list lst))
    (t (mapcan #'(lambda (x)
		   (mapcar #'(lambda (y) (append (list x) y))
			   (combination (1- n) (setf lst (delete x lst)))))
	       lst))))

;;(combination 3 '(1 2 3 4))



;; get all white-balls
(loop for i in *result-1*
      for cnt from 0
      when (< cnt 10)
	collect (list
		 (cdr (assoc 1 (html->win-ball i)))
		 (cdr (assoc 2 (html->win-ball i)))
		 (cdr (assoc 3 (html->win-ball i)))
		 (cdr (assoc 4 (html->win-ball i)))
		 (cdr (assoc 5 (html->win-ball i)))))

(let* ((draw '(1 5 31 37 49))
       (target '(1 5 17 37 70))
       (match-result (loop for i in (combination 3 draw)
			   collect (intersection i target))))
  (mapcar #'(lambda(x)(sort x #'<))
	  (remove-if-not #'(lambda (x) (> (length x) 2)) match-result)))


(defun print-lb (list-of-things)
  (loop for i in list-of-things
	do (format t "~a~%" i)))

;; most freq 3 number combo
(print-lb
 (let* ((all-white-ball-comb
	  (loop for i in *result-1*
		for cnt from 0
		when (< cnt 30)
		  collect (list
			   (cdr (assoc 1 (html->win-ball i)))
			   (cdr (assoc 2 (html->win-ball i)))
			   (cdr (assoc 3 (html->win-ball i)))
			   (cdr (assoc 4 (html->win-ball i)))
			   (cdr (assoc 5 (html->win-ball i))))))
	(all-3-white-balls (apply #'append(loop for i in all-white-ball-comb
						collect (combination 2 i)))))
   (subseq
    (occurrences
     all-3-white-balls) 0 20)))


(let* ((all-white-ball-comb
	 (loop for i in *result-1*
	       for cnt from 0
	       when (< cnt 30)
		 collect (list
			  (cdr (assoc 1 (html->win-ball i)))
			  (cdr (assoc 2 (html->win-ball i)))
			  (cdr (assoc 3 (html->win-ball i)))
			  (cdr (assoc 4 (html->win-ball i)))
			  (cdr (assoc 5 (html->win-ball i))))))
       (all-3-white-balls (apply #'append(loop for i in all-white-ball-comb
					       collect (combination 2 i)))))
  (remove-duplicates
   (apply #'append
	  (loop for i in (subseq (occurrences all-3-white-balls) 0 20)
		collect (car i)))))


(find-all-matches (draw-balls 0 1 25 38 59 :y 25))
(find-all-matches (draw-balls 0 1 30 25 66 :y 25))


(find-all-matches (draw-balls 0 1 2 17 27 :y 25))
(find-all-matches (draw-balls 0 1 2 4 64 :y 25))

(find-all-matches (draw-balls 15 25 30 38 66 :y 25))
(find-all-matches (draw-balls 10 25 30 38 66 :y 25))
(find-all-matches (draw-balls 10 25 30 38 64 :y 25))
(find-all-matches (draw-balls 14 25 30 38 64 :y 25))

(find-all-matches (draw-balls 10 14 25 38 64 :y 25))
(find-all-matches (draw-balls 5 14 25 38 64 :y 25))

(find-all-matches (draw-balls 14 25 35 38 66 :y 25))


(find-all-matches (draw-balls 14 25 35 38 66 :y 25))



(loop for i in
	    (loop for i in (combination 5
					(list 10 14 50 64 15 45 66 35 21 30 2 55 59 13 38 29 5 17 37 25 63))
		  collect (loop for i in (sort i #'<)
				for idx from 1
				collect (cons idx i)))
      do (find-all-white-matches i))


(nth 20 *result-1*)

(draw-balls 14 25 35 38 66 :y 25)

(find-all-white-matches '((1 . 14) (2 . 25) (3 . 35) (4 . 38) (5 . 66)))

(defun find-all-white-matches (draw-result)
  (format-print
   (let ((draw draw-result))
     (loop for i in *result-1*
	   when (filter-match-cnt (find-match-ball draw (html->win-ball i)) :cnt 3)
	     collect (list
		      (cdr (assoc 'date (html->win-ball i)))
		      (get-white-balls (html->win-ball i))
		      (find-match-ball draw (html->win-ball i)))))
   :align :right))
