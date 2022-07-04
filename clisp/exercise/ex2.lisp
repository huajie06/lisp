(in-package :cl)

(defpackage :parse-binary
  (:nicknames "pb")
  (:use :cl-user))

(in-package :parse-binary)




































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  some bytes knowledge
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;https://web.stanford.edu/class/cs101/bits-bytes.html

;;Each bit in the binary system has a position and a weight value assigned to it. Binary is a base-2 number system, therefore the weight of each bit is 2 raised to the power of the bit position.

;;BYTE takes two arguments, the number of bits to extract (or set) and the position of the rightmost bit where the least significant bit is at position zero.
(byte 2 0)
00
01
10
11
;;https://en.wikipedia.org/wiki/Bit_numbering
1 => 1
2 => 10
3 => 11
4 => 100
5 => 101

(byte 2 0)
;;2^(0+2-1) to 2^0 =>2 to 1
(format t "~a" (byte 2 0))

(loop for i from 0 to 15 do
  (format t "~&index: ~a, in byte: ~a" i (ldb (byte 2 0) i)))

(loop for i from 0 to 15 do
  (format t "~&index: ~a, in byte: ~a" i (ldb (byte 2 1) i)))

(byte-size (byte 2 0))
(byte-position (byte 2 0))

(code-char 65)
(char-code #\a)
