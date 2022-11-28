(ql:quickload :drakma)
(ql:quickload :yason)
(ql:quickload :lquery)
(ql:quickload :cl-ppcre)

(load "/Users/huajiezhang/repo/lisp/clisp/utilities/utl.lisp")
;; https://www.megamillions.com/

(defparameter *file-name* "previous_draw_20190101_20221108.html")
(defparameter *html-string* nil)

(with-open-file
    (stream *file-name* :direction :input :if-does-not-exist nil)
  (if stream
      (let ((string-result (make-string (file-length stream))))
	(read-sequence string-result stream)
	(setf *html-string* string-result)
	(print "string created"))
      (print "file does not exist")))


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

(defparameter *parse-result* nil)
(let* ((to-parse (lquery:$ (initialize *html-string*)))
       (array-of-a-tags (lquery:$ to-parse "a")))
  (setf *parse-result*
	(loop for elem across array-of-a-tags
	      collect (parse-atag-to-list elem)))
  (if (> (length *parse-result*) 1)
      (print "succeed")))

;;; result will be in *parse-result* variable

(defun occurrences (lst)
  ;; create occurent of a list element
  (let ((table (make-hash-table :test 'equal)))      ; [1]
    (loop for e in lst
          do (incf (gethash e table 0)))             ; [2]
    (sort (loop for k being the hash-key of table    ; [3]
                  using (hash-value v)
                collect (cons k v))
          #'>= :key #'cdr)))                         ; [4]


(defun select (selector-fn &key in-list)
  (remove-if-not selector-fn in-list))

(defun eq-ym (&key (year 2022) (month 1) )
  #'(lambda (ele)
      (and
       (= year (parse-integer
		(nth 2 (cl-ppcre:split "/" (getf ele :date))) :junk-allowed t))
       (= month (parse-integer
		 (nth 0 (cl-ppcre:split "/" (getf ele :date))) :junk-allowed t))
       )))

(defun le-ym (&key (year 2022) (month 1) )
  #'(lambda (ele)
      (and
       (>= year (parse-integer
		 (nth 2 (cl-ppcre:split "/" (getf ele :date))) :junk-allowed t))
       (>= month (parse-integer
		  (nth 0 (cl-ppcre:split "/" (getf ele :date))) :junk-allowed t))
       )))

(defun ge-ym (&key (year 2022) (month 1) )
  #'(lambda (ele)
      (and
       (<= year (parse-integer
		 (nth 2 (cl-ppcre:split "/" (getf ele :date))) :junk-allowed t))
       (<= month (parse-integer
		  (nth 0 (cl-ppcre:split "/" (getf ele :date))) :junk-allowed t))
       )))

(defun ball->table (ball)
  (format-print
   (loop for i in ball
	 collect
	 (list (getf i :date)
	       (list
		(getf i :ball1)
		(getf i :ball2)
		(getf i :ball3)
		(getf i :ball4)
		(getf i :ball5))
	       (getf i :yellow)))))

;;;;;;;; this section below can be rewritten ;;;;;;;;;;;

(defun draw-balls (b1 b2 b3 b4 b5 &key y)
  (cons (cons 'y y)
	(loop for i in (sort (list b1 b2 b3 b4 b5) #'<)
	      for ind from 1
	      collect (cons ind i))))

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



(defun find-match-ball (draw win)
  "find a match between ball drawed and target"
  (let ((result (sort
		 (intersection (get-white-balls draw)
			       (get-white-balls win)) #'<)))
    (if (equal (assoc 'y draw) (assoc 'y win))
	;; (cons (cons 'Yellow (cdr (assoc 'y draw))) result)
	(cons 'y result)
	result)))

(defun filter-match-cnt (match-result &key (cnt 2))
  "only return when match ball # greater than 2"
  (unless (< (length match-result) cnt)
    match-result))


(defun find-all-matches (draw-result)
  (let ((draw draw-result))
    (loop for i in *result-1*
	  when (filter-match-cnt (find-match-ball draw (html->win-ball i)))
	    collect (list
		     (cdr (assoc 'date (html->win-ball i)))
		     (cdr (assoc 'y (html->win-ball i)))
		     (get-white-balls (html->win-ball i))
		     (find-match-ball draw (html->win-ball i))))))


======
(ball->table
 (select (eq-ym :month 11 :year 2022)
	 :in-list *result-1*))

(ball->table (subseq *result-1* 0 10))


(find-all-matches (draw-balls 13 23 24 25 43 :y 2))
