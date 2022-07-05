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

