;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop for i in '(1 2 3)
      collect (* 2 i))

(loop for i in '(1 2 3)
      do (print i))


(format t "~{the value is: ~A~%~}" '(1 2 3)
)
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
         collect (if-len-3 f)))  ; this is a very good example, i can
                                 ; really run it

(test-loop-in2 (1 2 3) (4 5 6) (1 2))

;;;test-loop-in2 will give below
(test-loop-in2 (1 2 3) (4 5 6) (1 2))
(macroexpand-1 '(test-loop-in2 (1 2 3) (1 2)))


(check '(= (+ 1 2) 3))

(defun report-result-1 (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defmacro result-check-1 (&body form)
  `(loop for f in ',form
         do (check 'f)))

(LOOP FOR F IN '((= 1 2) (= 1 2))
      DO (CHECK1 'F))

(check1 '(= 1 2))

(result-check-1 (= (+ 1 2) 3) (= 1 2))
(macroexpand-1 '(result-check-1 (= 1 2) (= 1 2)))

(defmacro check1 (form)
  `(report-result ,form ',form))

(check1 '(= 1 2))
(check '(= 1 2))

;; ending comments


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



;;;;; start over

(defun report-result (result form)
  (format t "~:[Fail~;pass~] ... ~a: ~a~%" result *test-name* form) result)

;; (defmacro report-result-m (form)
;;   `(format t "~:[Fail~;pass~] ... ~a~%" ,form ',form))
;; (report-result-m (= (+ 1 2) 3))

(defmacro check (&body forms)
  `(loop for f in ',forms
         collect `(report-result ,f ',f)))
         ;;do (report-result f f)))
         ;;do (print f)))
         ;;collect (print f)))

(defmacro combine-results(&body forms)
    `(let ((result t))
       ,@(loop for f in forms collect `(unless ,f (setf result nil)))
       result))


(defmacro check-new (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(check-new
  (= (+ 1 2) 3))


(macroexpand-1 '(check-new
                 (= (* 1 2) 2)
                 (= (/ 10 2) 3)))

(macroexpand-1 '(COMBINE-RESULTS-1
  (REPORT-RESULT (= (* 1 2) 2) '(= (* 1 2) 2))
  (REPORT-RESULT (= (/ 10 2) 3) '(= (/ 10 2) 3))))

(check-new
  (= (+ 1 2) 3)
  (= (- 1 2) 3)
  (= (* 1 2) 3)
  (= (/ 10 2) 3))

(check-new
  (= (plus2 100) 102)
  (= (+ 1 2) 3)
  (= (- 1 2) -1)
  (= (* 1 2) 2)
  (= (/ 10 2) 5))

(defun plus2 (x)
  (+ 2 x))


(defvar *test-name* nil)


(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-random ()
  (check-new
  (= (plus2 100) 102)
  (= (+ 1 2) 3)
  (= (- 1 2) -1)
  (= (* 1 2) 2)
  (= (/ 10 2) 5)))

(deftest test-plus ()
  (check-new
  (= (plus2 100) 102)
  (= (+ 1 2) 3)
  (= (+ 1 2) -1)
  (= (+ 1 2) 2)
  (= (+ 10 2) 5)))


(test-plus)
(test-random)

(deftest test-arithmetic ()
  (combine-results
   (test-plus)
   (test-random)))
(test-arithmetic)
