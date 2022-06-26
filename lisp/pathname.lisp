;;(defparameter *file-path* "/Users/huajiezhang/repos")
(defparameter *file-path* 
  (make-pathname
   :directory '(:absolute "repo" "app" "sec")
   :name "something"
   :type "lisp"))



(directory (pathname "/Users/huajiezhang/"))

(describe (pathname "/Users/bar/*.text"))

(directory "/Users/huajiezhang/repo/lisp/lisp/*.lisp")

(loop for f in
	    (directory "/Users/huajiezhang/repo/lisp/lisp/*.*")
      do (print (namestring f)))



(defun list-directory (path-to-list)
  (loop for f in
	      (directory path-to-list)
	do (print (namestring f))))

(list-directory "/Users/huajiezhang/repo/lisp/lisp/*.*")

(setf home-dir "/Users/huajiezhang/repo/")
(directory
 (make-pathname
  :name :wild
  :type :wild
  :defaults home-dir))


(let ((home-dir "/Users/huajiezhang/repo/"))
  (loop for f in
	      (directory (make-pathname
			  :name :wild
			  :type :wild
			  :defaults home-dir))
	do (print (namestring f))))



(let ((home-dir "/Users/huajiezhang/repo/lisp/"))
  (loop for f in
	      (directory (make-pathname
			  :name :wild
			  :type :wild
			  :defaults home-dir))
	do (print (namestring f))))


(let ((home-dir "/Users/huajiezhang/repo/lisp"))
  (make-pathname
   :name :wild
   :type :wild
   :defaults home-dir))

(wild-pathname-p #p"/Users/huajiezhang/")
(wild-pathname-p #p"/Users/huajiezhang/*.txt")




(eql "/abc/ddd/" :unspecific)
(eql "/abc/ddd" :unspecific)



(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type :wild
   :default ))



(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(component-present-p "abc")

(defun directory-pathname-p  (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(directory-pathname-p "/abc/bb/text.tt")

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name      nil
	 :type      nil
	 :defaults pathname)
	pathname)))

(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))
(list-directory "/Users/huajiezhang/repo/")

(pathname-name "/abc/bcd/") (directory-pathname-p "/abc/bcd")
(pathname-as-directory "/abc//bcd/atbc.xt")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;;             usefull funcs in file system             ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pathname-name return the name portion and type return the ext
(pathname-name "/abc/bcd/")
(pathname-name "/abc/bcd")
(pathname-type "/abc/bcd.txt")
(pathname-type "/abc/bcd")
(wild-pathname-p #p"/abc/bcd/*.txt")

(eql "x" :unspecific)
(directory "/Users/huajiezhang/repo/*.*")


(defparameter test-pathname "/Users/huajiezhang/repo/abc/")
(pathname-name test-pathname)
(pathname-type test-pathname)

(defun directory-name-p (name)
  (and ;and will eval 1-by-1, if all T return last
   (not (pathname-name name))
   (not (pathname-type name))
   name))

(loop for fn in (list "abc/abc" "abc/abc/" "abc/abc.txt" "abc/abc/.txt")
      collect (directory-name-p fn))

(loop for fn in (list "abc/bcd" "abc/bcd/" "abc/bcd.txt" "abc/bcd/.txt")
      collect (pathname-directory fn))

collect (pathname fn))



(defun process-value-to-directory (name)
(let ((pathname-return (pathname name)))
  (if (not (directory-name-p name))
      (make-pathname
       :directory (append (pathname-directory name) (list (file-namestring name)))
       :name nil
       :type nil) pathname-return)))


(defun directory-to-dwild (dirname)
(make-pathname
 :defaults (pathname-as-directory dirname)
 :name :wild
 :type :wild))


(loop for fn in (list "abc/bcd" "abc/bcd/" "abc/bcd.txt" "abc/bcd/.txt")
collect (process-value-to-directory fn))

(loop for fn in (list "abc/bcd" "abc/bcd/" "abc/bcd.txt" "abc/bcd/.txt")
collect (directory-to-dwild (process-value-to-directory fn)))



(pathname "abc/bcd")
(pathname-directory "abc/bacd")
(file-namestring "abc/bcd")
(append 
 (pathname-directory "abc/bacd") (list (file-namestring "Abc/bacd")))

(let* ((test-nm "abc/bacd/")
       (df (append 
	    (pathname-directory test-nm) (list (file-namestring test-nm)))))
  (make-pathname
   :directory df
   :name nil
   :type nil))


(make-pathname
 :defaults "ab/bc"
 :name :wild 
 :type :wild)

(defun list-directory-2 (name)
  (when (pathname-type name) (error "input not a dir"))
  
  (when (wild-pathname-p name) (directory name)))

(list-directory-2 "/Users/huajiezhang/repo/*.*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;;                       my own version                 ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pathname-name "/abc/bcd/")
(pathname-name "/abc/bcd")
(pathname-type "/abc/bcd.txt")
(pathname-type "/abc/bcd")
(wild-pathname-p #p"/abc/bcd/*.txt")




(defun dir-name-p (name)
  ;; check if a name is a dir
  (and ;and will eval 1-by-1, if all T return last
   (not (pathname-name name))
   (not (pathname-type name))
   name))


(defun process-v-to-dir (name)
  ;; convert a non-standard format of directory into standard dir
  (let ((pathname-return (pathname name)))
    (if (not (dir-name-p name))
	(make-pathname
	 :directory (append (pathname-directory name) (list (file-namestring name)))
	 :name nil
	 :type nil) pathname-return)))


(defun dir-to-dwild (dirname)
  ;;convert a directory into a wildcard directory
  (make-pathname
   :defaults (pathname-as-directory dirname)
   :name :wild
   :type :wild))


(loop for fn in (list "abc/abc" "abc/abc/" "abc/abc.txt" "abc/abc/.txt")
      collect (dir-to-dwild fn))

(loop for fn in (list "abc/abc" "abc/abc/." "abc/abc.txt" "abc/abc/.txt")
      collect (process-v-to-dir fn))


(loop for fn in (list "abc/abc" "abc/abc/." "abc/abc.txt" "abc/abc/.txt")
      collect (pathname-as-directory fn))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(probe-file "/Users/huajiezhang")
(probe-file "/Users/huajiezhang/repo/apps/google/credentials.json")


(dir-name-p "/Users/huajiezhang/repo/golang-start/douban_movie/douban/")


(defun test-walk-dir (dirpath)
  (loop for d in (directory (dir-to-dwild dirpath))
	do (unless (search ".git" (directory-namestring d))
	     (if (dir-name-p d)
		 (progn
		   (format t "----~%~a~%" d)
		   (test-walk-dir d))
		 (format t "~a~%" d)))))


(let ((pathname "/Users/huajiezhang/repo/golang-start"))
  (test-walk-dir pathname))



(let ((p #P"/Users/huajiezhang/repo/golang-start/.git/objects/f1/314acdad93e3f5ab8af8f44a9fd34fd1ee1385"))
  (search "..git" (directory-namestring p)))
