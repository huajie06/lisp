;; both will work
(print "hello world")
(print "中华人民共和国")


;; use sbcl implementation to convert into bytes
(sb-ext:string-to-octets "中华人民共和国" :external-format :utf-8)


;; in unicode, Chinese character are in three byte so 228 184 173 = 中.
;; And in general unicode can have 1,2,3,4 bytes per character.

;; code-char and char-code function
(char-code (aref "h" 0))
;; or like this
(char-code  #\h)
(char-code  #\中)  ; this is unicode code points

(code-char 104)
(code-char 20013) ; not give you 中, but return #\U4E2D, this is the unicode
(princ #\U4E2D) ; this will give 中

;; use babel package, it seems will detect encoding
(ql:quickload "babel")
(babel:octets-to-string #(104 101)) ;; this will get `type` error

;; use this to convert
(map '(vector (unsigned-byte 8)) #'+ #(104 101))

;; then you can do
(babel:octets-to-string
 (map '(vector (unsigned-byte 8)) #'+ #(104 101)))  ;; give "he"

;; string to octets
(babel:string-to-octets "hello")
(babel:string-to-octets "中华人民共和国")
(defun vector-to-unsigned-b8(vector)
  (map '(vector (unsigned-byte 8)) #'+ vector))

;; this will give the right results
(babel:octets-to-string
 (vector-to-unsigned-b8
  #(228 184 173 229 141 142 228 186 186 230 176 145 229 133 177 229 146 140 229 155 189)
  ))



;; TODO
;; how does byte translate into Unicode for chinese


(char-code #\h)
(char-code #\中)


;; (with-input-from-string (var "strings")
;;   (loop for line = (read-byte var nil)
;; 	while line collect line))

(with-input-from-string (var "strings")
  (print (read-char var))
  (print (read-char var))
  (print (read-char var)))


(let*
    ((proc-var (sb-ext:run-program "echo" '("中") :search t :output :stream))
     (output (process-output proc-var)))
  (loop for byte = (read-byte output nil)
	while byte collect byte))

(char-code #\中)
(babel:string-to-octets "中")

(map '(vector (unsigned-byte 8)) #'char-code "hello world" )



==
(ql:quickload "flexi-streams")

(with-input-from-string (s "hello world")

  (let ((flex (flexi-streams:make-flexi-stream s :element-type '(unsigned-byte 8))))
    (read-line flex)))
