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
  (loop for i in (subseq
		  (copy-seq (parse-result duonao-result))
		  0 (length duonao-result))
	with l = (length duonao-result)
	for j from 0
	do (format t "Current index: ~a. Total: ~a.~%" j l)
	   (sleep (random 30))
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

(defvar *file-to-store-path* "/Users/huajiezhang/repo/lisp/clisp/app/movie.txt")

;; =========== utl funcs

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

;; =========== utl funcs end


(with-open-file (stream *file-to-store-path*
			:direction :output
			:if-does-not-exist :create
			:if-exists :overwrite)
  (when stream
    (print *parsed-result* stream)))


(defun sort-by-score ()
  #'(lambda (a b)(if (> (str-to-float (getf a :score))
			(str-to-float (getf b :score))) t nil)))

(sort (copy-seq *db*) (sort-by-score))

(defun read-in-movies (file-names)
  (let ((result nil))
    (with-open-file (stream file-names)
      (with-standard-io-syntax
	(setf result (read stream))))
    (sort (copy-seq result) (sort-by-score))))

(read-in-movies *file-to-store-path*)

=====
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
