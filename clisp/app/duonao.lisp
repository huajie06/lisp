(ql:quickload :drakma)
(ql:quickload :yason)
(ql:quickload :lquery)

(setf drakma:*header-stream* *standard-output*)

(defvar *file-to-store-path* "/Users/huajiezhang/repo/lisp/clisp/app/movie.txt")
(defparameter *dn-url* "https://m10.iyf.tv/api/list/Search?cinema=1&page=1&size=32&orderby=0&desc=1&cid=0,1,3&isserial=-1&isIndex=-1&isfree=-1&vv=85e9833da2681447cc127711c8561e38&pub=1655779288262")

(defun get-duonao-result-from-str (result-string)
  "parse ify json results hashtable
get the necessary part
"
  (gethash "result" (car (gethash "info" (gethash "data" result-string)))))

(defun fetch-duonao-url (url)
  "the actual GET"
  (let* ((stream (drakma:http-request url :user-agent :firefox))
	 (result-string (yason:parse (babel:octets-to-string stream))))
    (get-duonao-result-from-str result-string)))

(defun parse-result (json-result)
  "get only title name, type and year"
  (loop for i in json-result
	collect
	(list :cid (gethash "cid" i)
	      :title (gethash "title" i)
	      :year (gethash "year" i))))

(defun sort-by-score ()
  #'(lambda (a b)(if (> (str-to-float (getf a :score))
			(str-to-float (getf b :score))) t nil)))

(defun sort-by-proc-dt ()
  #'(lambda (a b)(if (string> (or (getf a :proc-ts) "0000")
			      (or (getf b :proc-ts) "0000")) t nil)))

(defun read-in-movies (file-names)
  (let ((result nil))
    (with-open-file (stream file-names)
      (with-standard-io-syntax
	(setf result (read stream))))
    (sort (copy-seq result) (sort-by-proc-dt))))

(defparameter *current-movie-db*
  (read-in-movies *file-to-store-path*))

(defparameter *duonao-response*
  (parse-result (fetch-duonao-url *dn-url*)))

*current-movie-db*
*duonao-response*

(defun compare-with-db (current-db-list duonao-response)
  (let ((current-movies
	  (delete-duplicates (loop for i in current-db-list
				   collect (getf i :title))))
	(to-proc duonao-response))
    (loop for i in to-proc
	  unless (member (getf i :title) current-movies :test #'equal)
	    collect i)))

(defparameter *movies-to-fetch*
  (compare-with-db *current-movie-db* *duonao-response*))

*movies-to-fetch*

(defun db-url-build (parm)
  "build a url and it needs type conversion because of drakma:request"
  (coerce
   (format nil "https://www.douban.com/search?q=~a"
	   (drakma:url-encode parm :utf-8))
   '(simple-array character (*))))

(defun fetch-douban-search-url (url)
  (drakma:http-request url :user-agent :firefox))

(defun parse-douban-result-str (result-string)
  (let* ((to-parse (lquery:$ (initialize result-string)))
	 (p1 (lquery:$ to-parse
	       "div.result .content .title"
	       #'(lambda (els) (subseq els 0 1)))))
    (list
     :url (aref (lquery:$ p1 "h3 a" (attr :href)) 0)
     :genre (aref (lquery:$ p1 "h3 span:not([class])" (text)) 0)
     :weight (aref (lquery:$ p1 "div.rating-info > span:not([class])" (text)) 0)
     :score (aref (lquery:$ p1 "div.rating-info > span.rating_nums" (text)) 0))))

;;(defparameter *db-result* (fetch-douban-search-url (db-url-build "信号")))
;;(parse-db-result-str *db-result*)

(defun get-db-score (name)
  (let ((result-string
	  (parse-douban-result-str
	   (fetch-douban-search-url (db-url-build name)))))
    (setf (getf result-string :name) name)
    result-string))

;; (defparameter *duonao-result* (fetch-duonao-url url))
;; (parse-result *duonao-result*)
;; (length *duonao-result*)


(defun loop-all-duonao-list (duonao-result)
  "loop all the movies in www.ify.tv at once
then ping douban.com to get scores "
  (loop for i in (subseq
		  (copy-seq (parse-result duonao-result))
		  0 (length duonao-result))
	with l = (length duonao-result)
	for j from 0
	do (format t "Current index: ~a. Total: ~a.~%" j l)
	   (sleep (random 10))
	collect
	(concatenate 'list i (get-db-score (getf i :title)))))

;; this loops everything
;; (defparameter *parsed-result*
;;   (loop-all-duonao-list *duonao-result*))

(defun loop-list-of-movies (list-of-movies)
  (loop for i in list-of-movies
	for idx from 0
	with l = (length list-of-movies)
	do (format t "Current index: ~a. Total: ~a.~%" idx l)
	   (sleep (random 10))
	collect
	(concatenate 'list i (get-db-score (getf i :title)))))

(defparameter *new-results*
  (loop-list-of-movies *movies-to-fetch*))

*new-results*

(defun get-date-ts-raw ()
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (list :second second
	  :minute minute
	  :hour hour
	  :date date
	  :month month
	  :year year
	  :day-of-week day-of-week
	  :dsp-p dst-p
	  :tz tz)))

(defun get-str-ts ()
  (let* ((ts (get-date-ts-raw))
	 (year (getf ts :year))
	 (month (getf ts :month))
	 (date (getf ts :date))
	 (hour (getf ts :hour))
	 (minute (getf ts :minute)))
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month date hour minute)))


*new-results*
*current-movie-db*

(defun add-ts-to-plist (plist)
  (let ((in-list plist))
    (loop for i in in-list
	  do (setf (getf i :proc-ts) (get-str-ts))
	  collect i)))

(defparameter *final-to-file*
  (concatenate 'list
	       (add-ts-to-plist *new-results*)
	       *current-movie-db*))


;; ============= utl funcs =============

(defun str-to-float(in)
  (if
   (equal (type-of in) 'bit)
   in (string->float in)))

(defun string->float (string)
  (with-input-from-string (s string)
    (read s nil nil)))

(defun parse-string-to-float (line)
  (with-input-from-string (s line)
    (loop
      for num = (read s nil nil)
      while num
      collect num)))

;; =========== utl funcs end ===========


(with-open-file (stream *file-to-store-path*
			:direction :output
			:if-does-not-exist :create
			:if-exists :overwrite)
  (when stream
    (print *final-to-file* stream)))



;; ==================== test ====================
;; (with-open-file (stream *file-to-store-path*)
;;   (let ((string-result (make-string (file-length stream))))
;;     (read-sequence string-result stream)
;;     string-result))

(defparameter *db* nil)
(with-open-file (stream *file-to-store-path*)
(with-standard-io-syntax
  (setf *db* (read stream)))

(with-open-file (stream *file-to-store-path*)
  (setf *db* (read stream)))
