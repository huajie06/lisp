;; create a class called range
(defclass range ()
  ((low
    :initarg :low
    :accessor range-low)
   (high
    :initarg :high
    :accessor range-high)))

;; create an instance of range
(defparameter *rng* (make-instance 'range :low 5 :high 10))

*rng*
(range-high *rng*)
(range-low *rng*)

;; print an object
(defmethod print-object ((obj range) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((low range-low) (high range-high)) obj
      (format stream ":LOW ~s :HIGH ~s" low high))))

(print-object *rng* t)

;; now define an condition for the range
(define-condition endpoints-error (error)
  ((low
    :initarg :low)
   (high
    :initarg :high)))


;; print the condition
(defmethod print-object ((obj endpoints-error) stream)
  (print-unreadable-object (obj stream)
    (with-slots (low high) obj
      (format stream "Endpoint error. LOW: ~s, HIGH: ~s" low high))))



;; test the print
(defparameter *temp-err* (make-condition 'endpoints-error :low 10 :high 10))
(print-object *temp-err* t)



(define-condition input-type-error (error)
  ((whichpoint
    :initarg :whichpoint)
   (value
    :initarg :value
    :initform nil)))

(defmethod print-object ((obj input-type-error) stream)
  (print-unreadable-object (obj stream)
    (with-slots (whichpoint value) obj
      (format stream "Input: *~a* endpoint not real number. Value is ~a" whichpoint value))))

;; (format t "~a" (type-of "abc"))
;; (format t "~a" (type-of 10))

(defparameter *temp-err1* (make-condition 'input-type-error :whichpoint "high" :value (type-of 10)))
(print-object *temp-err1* t)



(defun prompt-for-point (point-value)
  (format *query-io* "Enter new value for ~A endpoint~%" point-value)
  (force-output *query-io*)
  (list (read *query-io*)))


(defun validate-range (range)
  (with-accessors ((low range-low) (high range-high)) range
    (restart-case
	(progn
	  (assert (realp low) (low))
	  (assert (realp high) (high))
	  (unless (<= low high)
	    (error 'endpoints-error :low low :high high)))
      (enter-low-point (value)
	:report "enter a low point value"
	:interactive (lambda () (prompt-for-point "low"))
	(setf low value)
	(validate-range range))
      (enter-high-point (value)
	:report "enter a high point value"
	:interactive (lambda () (prompt-for-point "high"))
	(setf high value)
	(validate-range range))
      (enter-for-both-points (new-low new-high)
	:report "Please enter a new pair."
	:interactive (lambda () (append (prompt-for-point "low")
					(prompt-for-point "high")))
	(setf low new-low
	      high new-high)
	(validate-range range))
      (swap-endpoints ()
	:report "Switch the two points"
	(rotatef low high)))))

(defun validate-range1 (range)
  (with-accessors ((low range-low) (high range-high)) range
    (progn
      (assert (realp low) (low))
      (assert (realp high) (high))
      (unless (<= low high)
	(error 'endpoints-error :low low :high high)))))

(defun validate-range2 (range)
  (with-accessors ((low range-low) (high range-high)) range
    (restart-case 
	(progn
	  (unless (realp low) (error 'input-type-error :whichpoint "low" :value low))
	  (unless (realp high) (error 'input-type-error :whichpoint "high" :value high))
	  (unless (<= low high) (error 'endpoints-error :low low :high high)))
      ;; just copying from above
      (enter-low-point (value)
	:report "enter a low point value"
	:interactive (lambda () (prompt-for-point "low"))
	(setf low value)
	(validate-range2 range))
      (enter-high-point (value)
	:report "enter a high point value"
	:interactive (lambda () (prompt-for-point "high"))
	(setf high value)
	(validate-range2 range))
      (enter-for-both-points (new-low new-high)
	:report "Please enter a new pair."
	:interactive (lambda () (append (prompt-for-point "low")
					(prompt-for-point "high")))
	(setf low new-low
	      high new-high)
	(validate-range2 range))
      (swap-endpoints ()
	:report "Switch the two points"
	(rotatef low high)
	(validate-range2 range)))))



(defmethod initialize-instance :after ((obj range) &key)
  (validate-range2 obj))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-instance 'range :low 5 :high 10)
(make-instance 'range :low 5 :high 3)
(make-instance 'range :low "6" :high 6)
(make-instance 'range :low 6 :high "6")

(defun create-range3 (&key (low 0) (high 1))
  (handler-bind
      ((endpoints-error #'(lambda (c) (invoke-restart 'swap-endpoints)))
       (input-type-error #'(lambda (c) (invoke-restart 'enter-for-both-points 0 1))))
    (make-instance 'range :low low :high high)))

(create-range3 :low 3 :high 10)
(create-range3 :low 3 :high 1)
(create-range3 :low 3 :high "1")
(create-range3 :low "x" :high 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(handler-bind ((endpoints-error #'(lambda (exception)
				    (declare (ignore exception))
				    (invoke-restart 'swap-endpoints)))
	       (simple-error #'(lambda (exception)
				 (invoke-restart 'enter-for-both-points 0 1))))

  (make-instance 'range :low 10 :high 6))



;;;;;;;;;;;;;;;;;;

(defun create-range (&key (low 0) (high 1))
  (handler-bind ((endpoints-error #'(lambda (exception)
				      (declare (ignore exception))
				      (invoke-restart 'swap-endpoints)))
		 (simple-error #'(lambda (exception)
				   (invoke-restart 'enter-for-both-points 0 1))))

    (make-instance 'range :low low :high high)))

(create-range :low 5 :high 2)
(create-range :low "x" :high 2)
(create-range :low 0 :high "x")



(defun create-range2 (&key (low 0) (high 1))
  (handler-bind
      ((endpoints-error #'(lambda (c) (invoke-restart 'swap-endpoints))))
    (make-instance 'range :low low :high high)))

(create-range2 :low 5 :high 2)
(create-range2 :low "x" :high 2)


