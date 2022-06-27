;;; https://lispcookbook.github.io/cl-cookbook/clos.html
;;; a better reference


(defclass bank-account-1 ()
  ((customer-name
    :initarg :customer-name
    :accessor customer-name)
   (customer-age
    :initarg :age
    :accessor agexxx)
   (balance-amount
    :initarg :balance-amountxxx
    :initform 0)))


(defparameter *account-1*
  (make-instance 'bank-account-1
		 :customer-name "qu ni ma de"
		 :balance-amountxxx -100
		 :age 99))

;;(balance-amount *account-1*)

(customer-name *account-1*)
(agexxx *account-1*)
*account-1*

(slot-value *account-1* 'customer-name)
(slot-value *account-1* 'balance-amount)

(find-class 'bank-account-1)
(class-name *)
(class-of 'bank-account-1)
(find-class 'cons)

(defmethod withdraw ((obj bank-account-1) &key (amount 0))
  (decf (slot-value obj 'balance-amount) amount))

(withdraw *account-1* :amount 300)

;;;;;;;;;;;;;;;;;

(defvar *account-numbers* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))))

(setf x (make-instance 'bank-account :customer-name "wu la wu la"))


(setf y (make-instance 'bank-account :customer-name "john"))
(slot-value x 'customer-name)
(slot-value x 'account-number)
(slot-value x 'balance)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   account-type))

(setf y2 (make-instance 'bank-account
			:opening-bonus-percentage 10
			:customer-name "john"
			:balance 100))

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name.")
    :accessor customer-name
    :documentation "Customer's name")
   (balance
    :initarg :balance
    :initform 0
    :accessor balance
    :documentation "Current account balance")
   (account-number
    :initform (incf *account-numbers*)
    :reader account-number
    :documentation "Account number, unique within a bank.")
   (account-type
    :reader account-type
    :documentation "Type of account, one of :gold, :silver, or :bronze.")))

(defmethod initialize-instance :after ((account bank-account)
				       &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
          (* (slot-value account 'balance) (/ opening-bonus-percentage 100))))
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
          (cond ((>= balance 100000) "gold")
		((>= balance 50000) "silver")
		(t "bronze")))))


(defmethod withdraw ((account bank-account))
  (decf (slot-value account 'balance) 100))

(defmethod withdraw-3 ((account bank-account) &key amount)
  (with-slots (balance) account
    (decf balance amount)))

(balance yy)
(withdraw-3 yy :amount 90000)
(balance yy)


(defgeneric customer-name (any-name))
(defmethod customer-name ((any-name bank-account))
  (slot-value any-name 'customer-name))


(setf yy (make-instance 'bank-account
			:customer-name "yy lu"
			:balance 9999
			:opening-bonus-percentage 10))

(customer-name yy)
(setf (customer-name yy) "hz")
(setf (slot-value yy 'customer-name) "hz")

(balance yy)
(slot-value yy 'balance)
(slot-value yy 'account-type)
(slot-value yy 'account-number)

(setf (slot-value yy 'account-type) "xxxx")

(setf (slot-value yy 'balance) 100000)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass person ()
  ((name
    :initarg :name
    :accessor name)
   (test
    :initarg :fuckme
    :accessor test)
   (test1
    :initarg :fuckme1)
   (lisper
    :initform nil
    :accessor lisper)))


(defparameter p1 (make-instance 'person
				:name "me"
				:fuckme "hello"
				:fuckme1 "fuckme again"))

p1

;; (slot-value p1 'name)
;; (slot-value p1 'test)
;; (slot-value p1 'lisper)

;;; now you can direct access, what does accessor do????
(name p1)
(test p1)
(lisper p1)
(test1 p1)

(with-slots ((n name)
             (l lisper))
    p1 
  (format t "got ~a, ~a~&" n l))

(defmethod greet (obj)
  (format t "Are you a person ? You are a ~a.~&" (type-of obj)))

(greet :anything)

(greet p1)
(defgeneric greet (obj)
  (:documentation "say hello"))

(defmethod greet ((obj person))
  (format t "Hello ~a !~&" (name obj)))
(greet p1)

(defmethod greet1 ((obj person) &rest x)
  (format t "hello ~a!~a~%" (test1 p1) x))
(greet1 p1 1 2)


(defmethod greet2 ((obj person) &key x)
  (format t "hello ~a!~a~%" (test1 p1) x))
(greet2 p1 :x 1)

(defmethod greet3 ((obj person) &key (x 1))
  (format t "hello ~a!~a~%" (test1 p1) x))
(greet3 p1)


(setf p4 (make-instance 'bank-account
			:customer-name "john"))

p4
(greet3 p4)


(defclass foo ()
  ((a
    :initarg :a
    :initform (error "you didn't supply an initial value for slot a"))))

(make-instance 'foo :a 'whatheheck) 

(slot-value (make-instance 'foo :a 'wahtdkf) 'a)


