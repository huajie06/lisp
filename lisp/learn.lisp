(defvar *db* nil)

(defun make-cd(title author)
  (list :title title :author author))

(defun add-cd(cd)
  (push cd *db*))

;(make-cd "title1" "zhou")

(add-cd(make-cd "title1" "zhou"))
(add-cd(make-cd "title2" "zhouzhou"))
(add-cd(make-cd "liqixiang" "zhou jielun"))

(defun dump-cd (*db*)
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(dump-cd *db*)

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))

(save-db "test-db.txt")

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;;(setq *db* nil)
(load-db "test-db.txt")
*db*


(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where (&key title author)
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if author (equal (getf cd :author) author) t))))

(select (where :title "qilixiang"))
(select (where :title "liqixiang"))
(select (where :author "zhou"))
*db*


(defun update (selector-fn &key title author)
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title (setf (getf row :title) title))
	       (if author (setf (getf row :author) author)))
	     row) *db*)))
*db*
(update (where :author "zhou") :title "fantexi")
(save-db "test-db.txt")



(select (where :author "zhou jielun"))
