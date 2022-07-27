(ql:quickload :drakma)
(ql:quickload :yason)
(ql:quickload :lquery)

(setf drakma:*header-stream* *standard-output*)

(defparameter url "https://m10.iyf.tv/api/list/Search?cinema=1&page=1&size=32&orderby=0&desc=1&cid=0,1,3&isserial=-1&isIndex=-1&isfree=-1&vv=85e9833da2681447cc127711c8561e38&pub=1655779288262")

(defun get-duonao-result-from-str (result-string)
  (gethash "result" (car (gethash "info" (gethash "data" result-string)))))

(defun fetch-duonao-url (url)
  (let* ((stream (drakma:http-request url :user-agent :firefox))
	 (result-string (yason:parse (babel:octets-to-string stream))))
    (get-duonao-result-from-str result-string)))

(defun parse-result (json-result)
  (loop for i in json-result
	collect
	(list :cid (gethash "cid" i)
	      :title (gethash "title" i)
	      :year (gethash "year" i))))

(defun db-url-build (parm)
  "build a url and it needs type conversion because of drakma:request"
  (coerce
   (format nil "https://www.douban.com/search?q=~a"
	   (drakma:url-encode parm :utf-8))
   '(simple-array character (*))))

(defun fetch-douban-search-url (url)
  (drakma:http-request url :user-agent :firefox))

(defun parse-db-result-str (result-string)
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
	  (parse-db-result-str
	   (fetch-douban-search-url (db-url-build name)))))
    (setf (getf result-string :name) name)
    result-string))

(defparameter *duonao-result* (fetch-duonao-url url))
(parse-result *duonao-result*)
(length *duonao-result*)


(defun loop-duonao-list (duonao-result)
  (loop for i in (subseq (parse-result duonao-result)
			 0 (length duonao-result))
	do (sleep (random 30))
	collect
	(concatenate 'list i (get-db-score (getf i :title)))))


(defparameter *parsed-result*
  (loop-duonao-list *duonao-result*))

;; (defparameter *parsed-result*
;;   (loop for i in (subseq (parse-result *duonao-result*) 0 15)
;; 	do (sleep (random 30))
;; 	collect
;; 	(concatenate 'list i (get-db-score (getf i :title)))))

*parsed-result*

(with-open-file (stream "/Users/huajiezhang/repo/lisp/clisp/app/movie.txt"
			:direction :output
			:if-does-not-exist :create
			:if-exists :append)
  (when stream
    (print *parsed-result* stream)))
