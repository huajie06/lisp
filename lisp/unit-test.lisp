;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop for i in '(1 2 3)
      collect (* 2 i))

(loop for i in '(1 2 3)
      do (print i))


(format t "~{the value is: ~A~%~}" '(1 2 3))
(format t "~[a~;b~;c~]" 0)
(format t "~[a~;b~;c~]" 1)
(format t "~:[a~;b~]" 1) ; if ~:[~], then it can only have two element
(format t "~:[a~;b~]" nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun report-result-1 (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)


(report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
(report-result-1 (= (+ 1 2) 3) '(= (+ 1 2) 3))

;; test a func

;;(check (= (+ 1 3) 3)) =>  (= (+ 1 3) 3) == '(= (+ 1 3) 3)

(defmacro check (form)
  `(report-result-1 ,form ',form))
;;(macroexpand-1 '(check (= (+ 1 1) 2)))

(defun plus2 (x)
  (+ 2 x))
(check (= (plus2 3) 5))

;;;;;;;;;;;;;;
;;;;;;;;;;;;;;
;;;;;;;;;;;;;;


(defmacro multi-check (&body forms)
  `(loop for f in ',forms
         collect `(report-result-1 ,f ',f)))

(report-result-1 (= (+ 1 2) 3) '(= (+ 1 2) 3))
(multi-check (= (+ 1 2) 3) (= (* 1 2) 3))

(macroexpand-1 '(multi-check (= (+ 1 2) 3) (= (* 1 3) 3)))

(loop for i in (REPORT-RESULT-1 (= (+ 1 2) 3) '(= (+ 1 2) 3))
               (REPORT-RESULT-1 (= (* 1 2) 3) '(= (* 1 2) 3))
      do (i))


(defun if-len-3 (&optional (b '(1 2)))
  (if (= (length b) 3) t nil))
;;(if-len-3 '(1 2 3))

(defmacro test-loop-in (&rest forms)
  `(loop for f in ',forms
         do (print f)))
(macroexpand-1 '(test-loop-in (1 2 3) (4 5 6)))
(test-loop-in (1 2 3) (4 5 6))

(defmacro test-loop-in2 (&rest forms)
  `(loop for f in ',forms
         ;;collect `(if-len-3 ',f)))
         collect (if-len-3 f)))
(test-loop-in2 (1 2 3) (4 5 6) (1 2))


;;;test-loop-in2 will give below
(test-loop-in2 (1 2 3) (4 5 6) (1 2))

(macroexpand-1 (test-loop-in2 (1 2 3) (1 2)))


;;;;;;;;;;;;;;
;;;;;;;;;;;;;;
;;;;;;;;;;;;;;


(defun test-cases()
  (check (= (+ 1 1) 2))
  (check (= (* 1 1) 2))
  (check (= (+ 1 4) 5)))

(test-cases)

(unless nil (print "hello"))
(unless t (print "hello"))

(macroexpand-1 '(unless t (print "hello") (print "yes")))
(unless nil (print "hello") (print "yes"))

(let ((result t))
  (unless nil (setf result nil))
  (unless t (setf result nil))
  result)


