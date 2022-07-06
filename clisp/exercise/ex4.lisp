;;chapter 26
(in-package :cl-user)

(ql:quickload "aserve")

(defpackage :book.web
  (:nicknames "web")
  (:use :cl :net.aserve :net.html.generator))

(in-package :book.web)


(start :port 3000)

(publish-file :path "/hello.html" :file "~/repos/functional/clisp/exercise/ex4_hello.html")
(publish-file :path "/hello.html" :remove t)

(publish-directory :prefix "/" :destination "/home/huajie/repos/functional/clisp/exercise/")
(publish-directory :prefix "/" :remove t)

(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (format
       (request-reply-stream request)
       "<html>~@
        <head><title>Random</title></head>~@
        <body>~@
        <p>Random number: ~d</p>~@
        </body>~@
        </html>~@
       "
       (random 1000)))))

(publish :path "/random-number" :function 'random-number)

(html
  (:head (:title "random"))
  (:body (:p "randome number: " (:princ-safe (random 1000)))))


(defun random-number1 (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (net.html.generator:html
        (:html
          (:head (:title "random"))
          (:body (:p "randome number: " (:princ-safe (random 10)))))))))

(publish :path "/show-query-params" :function 'random-number1)
(publish :path "/show-query-params" :remove t)

(defun random-number-2 (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (let* ((limit-string (or (request-query-value "limit" request) ""))
             (limit (or (parse-integer limit-string :junk-allowed t) 10)))
        (html
          (:html
            (:head (:title "random number page"))
            (:body
             (:p "range picked: "(:princ-safe limit))
             (:p "randome number: " (:princ-safe (random limit))))))))))

(publish :path "/show-random-number" :function 'random-number-2)
==
(defun simple-form (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (net.html.generator:html
        (:html
          (:head (:title "simple form"))
          (:body
           ((:form :method "post" :action "/show-query-result")
            (:table
             (:tr (:td "food")
                  (:td ((:input :name "foo" :size 20))))
             (:tr (:td "passowrd")
                  (:td ((:input :name "password" :type "password" :size 20)))))
            (:p ((:input :name "submit" :type "submit" :value "okay"))
                ((:input :name "submit" :type "reset" :value "Rest"))))
           ))))))

(publish :path "/simple-form" :function 'simple-form)

(defun show-query-result (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (html
        (:html
          (:body (:title "Query results")
                 (if (request-query request)
                     (progn
                       (princ (request-query request))
                       (loop for (k . v) in (request-query request)
                             do (html
                                  ((:table :border "1")
                                   (:tr (:td (:princ-safe k)) (:td (:princ-safe v)))))))
                     (html (:p "No query results")))))))))

(publish :path "/show-query-result" :function 'show-query-result)


(defun show-cookies (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (html
        (:html
          (:body
           (:title "cookies")
           (if (null (get-cookie-values request))
               (html (:p "No cookies"))
               (progn (princ (get-cookie-values request))
                      (html
                        ((:table :border 1)
                         (loop for (k . v) in (get-cookie-values request)
                               do (html (:tr (:td (:princ-safe k)) (:td (:princ-safe v)))))))))))))))

(publish :path "/show-cookies" :function 'show-cookies)

(defun set-cookies (request entity)
  (with-http-response (request entity :content-type "text/html")
    (set-cookie-header request :name "my cookie" :value "a cookie value")
    (with-http-body (request entity)
      (html
        (:html (:body (:title "cookies set")
                      (:p "cookie set")
                      (:p ((:a :href "/show-cookies") "look at the cookies"))))))))
(publish :path "/set-cookies" :function 'set-cookies)


(html
  (:html (:body (:title "cookies set")
                (:p "cookie set")
                (:p ((:a :href "/show-cookies") "look at the cookies")))))

(defmacro defun-url-function (name)
  (let ((entity (gensym)))
    `(progn
       (defun ,name (request ,entity)
         (with-http-response (request ,entity :content-type "text/html")
           (set-cookie-header request :name ,(format nil "/~(~a~)" name)
                                      :value ,(format nil "/~(~a~)" name))
           (with-http-body (request ,entity)
             (html
               (:html (:body (:title "this is my page")
                             (:p "fine")))))))
       (publish :path ,(format nil "/~(~a~)" name) :function ',name))))

(defmacro defun-url-function2 (name))

(let ((param (list 'a 'b)))
  (etypecase param
    (list param)
    (symbol `(,param string nil nil))))

(defun-url-function test1)
(macroexpand-1 '(defun-url-function test1))
==
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(html
  ((:form :method "post" :action "/show-query-result")
   (:table
    (:tr (:td "food")
         (:td ((:input :name "foo" :size 20))))
    (:tr (:td "passowrd")
         (:td ((:input :name "password" :type "password" :size 20)))))
   (:p ((:input :name "submit" :type "submit" :value "okay"))
       ((:input :name "submit" :type "reset" :value "Rest")))))

