;; first download the file
;; load into lisp
(load "/usr/share/common-lisp/source/quicklisp/quicklisp.lisp")
;; run the install
(quicklisp-quickstart:install)

;; each time in order to run quicklisp
(load "/home/huajie/quicklisp/quicklisp/setup.lisp")
;; or add a cmd like this, it will modify .sbclrc file
(ql:add-to-init-file)

;; NOT SURE OTHER PACKAGES!
;; https://lispcookbook.github.io/cl-cookbook/getting-started.html



;; docker image
;; https://hub.docker.com/r/clfoundation/sbcl
