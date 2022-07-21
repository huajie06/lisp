;; ~$ for float
(format t "~$" pi)

;; equivalent with ~v, takes from augument
(format t "~5$" pi)
(format t "~v$" 5 pi)

(format t "~v@{~a~:*~}" 20 "-")

;;The difference between the two is that ~% always emits a newline,
;;while ~& emits one only if it's not already at the beginning of a
;;line.

(format nil "~d" 1000000) ==> "1000000"
(format nil "~:d" 1000000)


;;padding

(format nil "~12d" 123)
(format nil "~12,'0d" 123) ;,' => pad with 0

(format t "~12d" 123)
(format t "~12:d" 12300)


(block r
  (format t "~a ~a~%" "what the heck" "yeah")
  (format t "~a ~40a~%" "what the heck" "yeah")
  (format t "~a ~40a" "what" "yeah")
  nil)

(format nil "|~vA|"       10 "Test")
(format nil "|~v:@<~A~>|" 10 "Test")
(format nil "|~v@A|"      10 "Test")


(format nil "|~10A|" "Test")
(format nil "|~10:@<~A~>|" "Test")
(format nil "|~10@A|" "Test")
==
(format nil "~(~a~)" "XXDFADFEWF")
(format nil "~:@(~a~)" "xxdfadfewf")


(format nil "~[cero~;uno~;dos~]" 0) ==> "cero"
(format nil "~[cero~;uno~;dos~]" 1) ==> "uno"
(format nil "~[cero~;uno~;dos~]" 2) ==> "dos"

;; last is the default
(format nil "~[cero~:;uno~:;dos~]" 100) ==> "dos"

;; if nil the second, all else first
(format nil "~:[FAIL~;pass~]" nil)
(format nil "~:[FAIL~;pass~]" 'a)
(format nil "~:[FAIL~;pass~]" t)


(format nil "~@[kuanqi~a~]--wulen wulen--~@[xxx ~a~]" "haha" "hehe")
(format nil "~@[kuanqi~a~]--wulen wulen--~@[xxx ~a~]" nil "hehe")
(format nil "~@[kuanqi~a~]--wulen wulen--~@[xxx ~a~]" "haha" nil)

(format nil "~{~a ~}" '(1 2 3))
(format nil "~{~a, ~}" '(1 2 3))
(format nil "~{~a~^, ~}" '(1 2 3)) ;; nothing for last one

;; @ will take everything as a list
(format nil "~@{~a, ~}" 1 2 3)
(format nil "~@{~a~^, ~}" 1 2 3)


(ignore-errors
 (/ 3 0))


;; this is like try/catch
(handler-case (/ 3 0)
  (division-by-zero (c)
    (format t "error: ~a~%" c)))

(handler-case (/ 3 0)
  (t (c)
    (format t "all error go here error: ~a~%" c)))

;;;;;;;;;;;;;;;;;;some condition creation

;; so step is make a condition, then signial a condition
(define-condition parse-string-len-le1 (error)
  ((message :initarg :message
	    :initform nil))
  (:report (lambda (condition stream) ; this print stuff to debugger
	     (format stream "your input strings only has 1 char")))
  (:documentation "Customized error message"))

(defun parse-some-string (strs)
  (if (<= (length strs) 1)
      (error 'parse-string-len-le1 :message strs)
      (format t "parse result: ~a~%" strs)))

(parse-some-string "what today is today")
(parse-some-string "w")

(realp 3)
(assert (realp 3))
(assert (not (realp 3)))


(defun divide (x y)
  (assert (not (zerop y)))
  (/ x y))
(divide 1 2)
(divide 10 0)

(defun divide2 (x y)
  (assert (not (zerop y))
          (y)   ;; list of values that we can change.
          "Y can not be zero. Please change it") ;; custom error message.
  (/ x y))
(divide2 10 1)
(divide2 10 0)
