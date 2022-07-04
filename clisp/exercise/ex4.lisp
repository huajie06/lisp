;;chapter 26
(in-package :cl-user)

(defpackage :book.web
  (:nicknames "web")
  (:use :cl :net.aserve))

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

(net.html.generator:html
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
