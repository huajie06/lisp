(in-package :cl-user)

;;(ql:quickload :cl-ppcre)

(defpackage :spam-filter
  (:nicknames "sf")
  (:use :cl :cl-ppcre))

(in-package :spam-filter)

;; end goal is to say if the FULL text is spam 
(defun classify (text)
  (label-text (score (process-text text))))

;; the threshold of the scores
(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

;; label the results
(defun label-text (score)
  (values 
   (cond
     ((<= score *max-ham-score*) 'ham)
     ((>= score *min-spam-score*) 'spam)
     (t 'unsure))
   score))

;; text will have many words, so extract each word
;; then score each word to get a score, then a text will get a score


;; each word will contain scores
(defclass word-feature ()
  ((word
    :initarg :word
    :accessor :word
    :initform nil)
   (spam-count
    :initarg :spam-count
    :accessor :spam-count
    :initform 0)
   (ham-count
    :initarg :ham-count
    :accessor :ham-count
    :initform 0)))

(defmethod print-object ((obj word-feature) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((word :word)
		     (spam-count :spam-count)
		     (ham-count :ham-count)) obj
      (format stream "Word: ~a. Spam count: ~a. Ham count ~a." word spam-count ham-count))))

;; a var to hold the each word scores
(defvar *feature-database* (make-hash-table :test 'equalp))


;; clear db, do why just don't use defparameter?
(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)))

;; look for a word and if no exist then create it 
(defun get-or-make-word (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))


;; here it extract a list of words from a text
(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

;; takes words, extract and put it into db
(defun process-text (text)
  (mapcar #'get-or-make-word (extract-words text)))

(process-text "foo today is a good day! buy me some stuff!")

;; define a function to create train set, if it's spam, spam count +
;; this is really a manual labeling step
(defun train (text type)
  (dolist (feature (process-text text))
    (increment-count feature type))
  (increment-total-count type))

(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defun increment-count (feature type)
  (case type
    (ham (incf (:ham-count feature)))
    (spam (incf (:spam-count feature)))
    (otherwise "Type not ham or spam")))

;; (defun increment-count (feature type)
;;   (ecase type
;;     (ham (incf (:ham-count feature)))
;;     (spam (incf (:spam-count feature)))))

(defun increment-total-count (type)
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))

;; define a function to clear the variables, reset
(defun clear-database ()
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-spams* 0
   *total-hams* 0))

;; each word feature would have a probability of spam
;;(spam-probability (gethash "some" *feature-database*))
(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
	  (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))


(defun bayesian-spam-probability (feature &optional
					    (assumed-probability 1/2)
					    (weight 1))
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (:spam-count feature) (:ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))


(defun score (features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))

(defun fisher (probs number-of-probs)
  (inverse-chi-square 
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min 
   (loop with m = (/ value 2)
	 for i below (/ degrees-of-freedom 2)
	 for prob = (exp (- m)) then (* prob (/ m i))
	 summing prob)
   1.0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; testing code below
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(clear-database)
(train "make some money today!" 'spam)
(train "make some appointment for my wife!" 'ham)

(format t "total spam:~a, total ham:~a" *total-spams* *total-hams*)

(classify "make some ")
(classify "make some today")
(classify "make appointment")

*feature-database*
(loop for v being each hash-value of *feature-database*
      do (print v))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; get file from out source
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *wd* "/Users/huajiezhang/repo/lisp/clisp/exercise/email_spam")

(defun list-directory (path-to-folder)
  (directory (make-pathname
	      :directory (list :absolute *wd* path-to-folder)
	      :type :wild
	      :name :wild)))

(defun add-file-to-corpus (fname type corpus)
  (vector-push-extend (list fname type) corpus))

(defun add-directory-to-corpus (dir type corpus)
  (dolist (fname (list-directory dir))
    (add-file-to-corpus fname type corpus)))

(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))

(add-directory-to-corpus "easy_ham/easy_ham" 'ham *corpus*)
(add-directory-to-corpus "spam_2/spam_2" 'spam *corpus*)

;;;

(defun train-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) do
    (destructuring-bind (file type) (aref corpus idx)
      (train (start-of-file file *max-chars*) type))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; my own idea
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(clear-database)
(aref *corpus* 0)
(let ((idx 0))
  (destructuring-bind (file type) (aref *corpus* idx)
    (train (read-from-file file) type)))

(let ((idx 0))
  (destructuring-bind (file type) (aref *corpus* idx)
    (multiple-value-bind (pred score)(classify (read-from-file file))
      (list
       ;;:file file
       :actual type
       :predicted pred
       :score score))))

;;;;;

(defun train-from-corpus (corpus &key (start 0) end)
  (loop for i from start below (or end (length corpus)) do
    (destructuring-bind (fname type) (aref corpus i)
      (train (start-of-file fname *max-read-length*) type))))

(defun test-from-corpus (corpus &key (start 0) end)
  (loop for i from start below (or end (length corpus)) do
    (destructuring-bind (fname type) (aref corpus i)
      (multiple-value-bind (pred score) (classify (start-of-file fname *max-read-length*))
	(list
	 :actual type
	 :predicted pred
	 :score score)))))

(clear-database)
(train-from-corpus *corpus*)
(test-from-corpus *corpus*)

(defparameter *max-read-length* (* 10 1024))

(read-from-file "/Users/huajiezhang/repo/lisp/clisp/exercise/email_spam/easy_ham/easy_ham/0008.20bc0b4ba2d99aae1c7098069f611a9b" *max-read-length*)


(defun read-from-file (fname max-length)
  (with-open-file (stream fname :if-does-not-exist nil)
    (if stream
	(let* ((valid-length (min (file-length stream) max-length))
	       (result (make-string valid-length))
	       (read-result (read-sequence result stream)))
	  (if (< read-result valid-length)
	      (subseq result 0 read-result) read-result))
	(print "file does not exist"))))

(defun start-of-file (file max-chars)
  (with-open-file (in file)
    (let* ((length (min (file-length in) max-chars))
           (text (make-string length))
           (read (read-sequence text in)))
      (if (< read length)
	  (subseq text 0 read)
	  text))))


(start-of-file "/Users/huajiezhang/repo/lisp/clisp/exercise/email_spam/easy_ham/easy_ham/0008.20bc0b4ba2d99aae1c7098069f611a9b" *max-read-length*)

(let ((fname "/Users/huajiezhang/repo/lisp/clisp/exercise/email_spam/easy_ham/easy_ham/0008.20bc0b4ba2d99aae1c7098069f611a9b"))
  (with-open-file (stream fname :external-format :us-ascii)
    (when stream 
      (loop for line = (read-line stream nil)
	    while line do (print line)))))

===
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; random code below
;; c-c m-o
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop for k being each hash-key of *feature-database*
      do (format t "key: ~a, value:~a~%" k (gethash k *feature-database*)))

(loop for v being each hash-value of *feature-database*
      do (print v))

(delete-duplicates '(1 2 3 1 2)) ; remove dups


(dolist (k '(1 2 3))
  (format t "~A~%"
	  (case k (1 'case1)
		(2 'ddd)
		(3 'case3)
		(otherwise 'other))))


(gethash "some" *feature-database*)
