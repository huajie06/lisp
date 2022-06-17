(defun report-result (result form)
  (format t "~:[Fail!~;pass~] ... ~a: ~a~%" result *test-name* form) result)

;; (defmacro report-result-m (form)
;;   `(format t "~:[Fail~;pass~] ... ~a~%" ,form ',form))
;; (report-result-m (= (+ 1 2) 3))

(defmacro check-play (&body forms)
  `(loop for f in ',forms
         collect `(report-result ,f ',f)))
         ;;do (report-result f f)))
         ;;do (print f)))
         ;;collect (print f)))

(defmacro combine-results(&body forms)
    `(let ((result t))
       ,@(loop for f in forms collect `(unless ,f (setf result nil)))
       result))


(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))



;; (macroexpand-1 '(check
;;                  (= (* 1 2) 2)
;;                  (= (/ 10 2) 3)))

;; (macroexpand-1 '(COMBINE-RESULTS-1
;;   (REPORT-RESULT (= (* 1 2) 2) '(= (* 1 2) 2))
;;   (REPORT-RESULT (= (/ 10 2) 3) '(= (/ 10 2) 3))))

(check
  (= (+ 1 2) 3)
  (= (- 1 2) 3)
  (= (* 1 2) 3)
  (= (/ 10 2) 3))


(defun plus2 (x)
  (+ 2 x))

(check
  (= (plus2 100) 102)
  (= (+ 1 2) 3)
  (= (- 1 2) -1)
  (= (* 1 2) 2)
  (= (/ 10 2) 5))

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-random ()
  (check
  (= (plus2 100) 102)
  (= (+ 1 2) 3)
  (= (- 1 2) -1)
  (= (* 1 2) 2)
  (= (/ 10 2) 5)))

(macroexpand-1 '(deftest test-plus ()
  (check
  (= (plus2 100) 102)
  (= (+ 1 2) 3)
  (= (+ 1 2) -1)
  (= (+ 1 2) 2)
  (= (+ 10 2) 5))))


(test-plus)
(test-random)

(deftest test-arithmetic ()
  (combine-results
   (test-plus)
   (test-random)))

(test-arithmetic)



