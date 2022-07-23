
(defun format-print (list-of-list)
  (let* ((data list-of-list)
	 (fmt-align '(:left   "~vA"
		      :center "~v:@<~A~>"
		      :right  "~v@A"))

	 (cols-count (loop for r in data
			   with cell-count = 0
			   do (setf cell-count (max cell-count (length r)))
			   finally (return cell-count)))

	 (str-data (loop for r in data
			 collect (loop for i from 0 to (1- cols-count)
				       collect (cond
						 ((equal (nth i r) nil) "")
						 ((stringp (nth i r)) (nth i r))
						 (t (format nil "~a" (nth i r)))))))
	 (width (loop for r in str-data
		      with cell-w = (make-array cols-count :initial-element 0)
		      do (loop for i in r
			       for idx from 0 do
				 (setf (aref cell-w idx)
				       (max (length i) (aref cell-w idx))))
		      finally (return cell-w)))
	 (widths (loop for i across width collect i))
	 (row-fmt (format nil "| ~{~A~^ | ~} |~~%"
			  (loop for i from 1 to cols-count
				with align = :center
				collect (getf fmt-align align)))))
    (dolist (row str-data)
      (apply #'format t row-fmt (mapcan #'list widths row)))))


(defun str->keyword (str)
  (if (not (stringp str)) (error "Input not string")
      (intern (string-upcase str) "KEYWORD")))

(defun str->keyword2 (str &key (type-v nil))
  (if (stringp str)
      (intern (format nil "~:@(~a~)" str) "KEYWORD")
      (if type-v
	  (intern (format nil "~:@(~a~)" str) "KEYWORD")
	  (error "ERROR: Input not string"))))
