(ql:quickload :drakma)
(ql:quickload :yason)
(ql:quickload :lquery)

https://www.megamillions.com/

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


==

(defparameter test1
  (aref (lquery:$ (lquery:$ (initialize *html-string*)) "a") 0))

(defparameter _tmp_ (list :DATE #("11/8/2022") :BALL1 #("5") :BALL2 #("13") :BALL3 #("29") :BALL4
			  #("38") :BALL5 #("59") :YELLOW #("23")))

;; _tmp_

;; (loop for (key value) on _tmp_ by #'cddr
;;       collect (list key (single-arr->atom value)))
