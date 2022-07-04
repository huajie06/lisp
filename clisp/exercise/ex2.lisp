;; chapter 24
(in-package :cl-user)

(defpackage :parse-binary
  (:nicknames "pb")
  (:use :cl))

(in-package :parse-binary)

;; unsigned 16 bits interger
(defun read-u2 (in)
  (+ (* (read-byte in) 256) (read-byte in)))


(code-char 97)
(char-code #\a)
(loop for c across "ID3"
      do (print (char-code c)))

(with-open-file (stream "~/repos/functional/clisp/exercise/test.txt"
                        :direction :input
                        :element-type '(unsigned-byte 8))
  (when stream
    (loop for line = (read-byte stream nil)
          while line do (print (code-char line)))))

;; reading bytes
(with-open-file (stream "~/repos/functional/clisp/exercise/test.txt"
                        :direction :input
                        :element-type '(unsigned-byte 8))
  (when stream
    (with-output-to-string (s)
      (loop for char = (read-byte stream nil)
            while char do (write-char (code-char char) s)))))


(let ((s "hell world"))
  (loop for char across s
        do (format t "~&~a" (char-code char))))
;;do (write-byte (char-code char) *query-io*)))



(format *standard-output* "helloworld!")
(format *query-io* "helloworld!")

====




















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  some bytes knowledge
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;https://web.stanford.edu/class/cs101/bits-bytes.html
;;https://en.wikipedia.org/wiki/Bit_numbering

(byte 2 0) ; it's a specification to tell ldb TO EXTRACT 2 bits, and from position 0(right most)

(let ((byte-size-var 3)
      (byte-position-var 1))
  (loop for i from 0 to 15
        for emit-result = (ldb (byte byte-size-var byte-position-var) i)
        do
           (format t "~&I:~2,'0d. In B:~4,'0b. Result:~a. Result in B:~4,'0b"
                   i i emit-result emit-result)))

;; inverse calc
(ldb (byte 8 8) 255) ==>0
(ldb (byte 8 8) 65408) ==>255, so how to get 65408?

(setf x 0)
(setf (ldb (byte 8 8) x) 65408)



(with-open-file (stream "~/repos/functional/clisp/exercise/test.txt"
                        :direction :input)
  (when stream
    (let* ((f-length (file-length stream))
           (result (make-string f-length))
           (read-len (read-sequence result stream)))
      (if (< read-len f-length) (subseq result 0 read-len) result))))

(loop for i from 0 to 10
      for j = (* i 3)
      while (< j 29) do (print j)) ; once nil then terminate

