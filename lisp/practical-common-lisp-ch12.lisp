;;https://gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html


;; a dotted pair
(cons 1 2)
;; first value is car and second value is cdr
(cons 'a 'b)

(car (cons '1 'c))
(cdr (cons 'bbb 'c))

;; car and cadr are setfable

(setf x (cons 1 2))
x
(setf (car x) 999)
x

;; list is really a chain of cons
(cons 1 nil)
(cons 1 (cons 2 nil))
(cons 1 (cons 2 (cons 3 nil)))

;; when dealing with list, no need to use car or cdr
(defparameter *list* (list 1 2 3 4))
(first *list*)        ==> 1
*list*
(rest *list*)         ==> (2 3 4)
(first (rest *list*)) ==> 2

;;append take the first list and link to next
(append (list 'a 1) (list 1 2 9 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;;                   destructive data                   ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; recycle operations

;; if the exsiting one(list variable) are not referenced,
;; now they are eligible to be garbage collected, some
;; lisp implementations, it's more efficient to use existing ones 

(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list-3* (append *list-1* *list-2*))

*list-1* ===> (1 2)
*list-2* ===> (3 4)
*list-3* ===> (1 2 3 4)

(setf (first *list-2*) 999)
;; *list-2* ===> (999 4)
;;*list-3 ;this one also change, because *list-3* is a link on 1&2


(defparameter *x* (list 1 2 3))
*x*
(nconc *x* (list 4 5 6)) ==> (1 2 3 4 5 6)
*x* ==> (1 2 3 4 5 6)

(defparameter *y* (list 1 2 3))
(append *y* (list 4 5 6))
*y*

;;; you can use this way
(defvar y1 nil)
(setf y1 (append *y* (list 4 5 6)))
y1


(let ((result nil))
  (dotimes (i 10)
    (push i result))
  (reverse result))

(let ((result nil))
  (dotimes (i 10)
    (push i result))
  (nreverse result))

;;; previous example
*list-2* ===> (3 4) ;1 -> 2 nil
*list-3* ===> (1 2 3 4);1 -> 2 -> 3 -> 4 -> nil
(setf *list-3* (delete 4 *list-3*)) ; cause 1 -> 2 -> 3 -> nil
*list-3* ===> (1 2 3)
*list-2* ===> (3);; 3 -> nil


;;; so destructive function will cause the original variable change
;;; non-destructive function only perform operation on the fly

;; equivalent *essential* of a list!!!!
(list 1 2 3)
(cons 1 (cons 2 (cons 3 nil)))

(car (list 1 2 3))
(cdr (list 1 2 3))
(cddr (list 1 2 3))


(last (list 1 2 3))
(butlast (list 1 2 3) 1) ;; exclude last 2
(last (list 1 2 3) 2)

(second (list 1 2 3))
(third (list 1 2 3))
(nth 2 (list 1 2 3))

(listp (list 1 2))
(listp (cons 1 2))
(listp  1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;;                      mapping                         ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(mapcar #'(lambda (x) (* 2 x)) (list 1 2 3)) ==> (2 4 6)
(mapcar #'+ (list 1 2 3) (list 10 20 30 3)) ==> (11 22 33)


