;; Pulled from metatilities

(in-package #:test-asdf-install)

(defun delete-directory-and-files (directory-spec 
				   &key (verbose? nil) (dry-run? nil)
				   (if-does-not-exist :error))
  (unless (directory-pathname-p directory-spec)
    (setf directory-spec (pathname-as-directory directory-spec)))
  (cond
    ((probe-file directory-spec)
     (let* ((wild-directory 
	     (make-pathname
	      :name :wild
	      :type :wild
	      :directory (append (pathname-directory directory-spec)
				 (list :wild-inferiors))
	      :defaults directory-spec))      
	    (contents (sort
		       (directory wild-directory)
		       #'>
		       :key (lambda (file)
			      (length (namestring file))))))
       (mapc (lambda (file-or-directory)
	       (cond ((directoryp file-or-directory)
		      (when verbose?
			(format *debug-io* "~%;;; deleting directory ~A"
				file-or-directory))
		      (unless dry-run?
			(delete-directory file-or-directory)))
		     (t
		      ;; probably a file
		      (handler-case
			  (progn
			    (when verbose?
			      (format *debug-io* "~%;; deleting ~A" 
				      file-or-directory))
			    (unless dry-run?
			      (delete-file file-or-directory)))
			(file-error (c)
			  (declare (ignore c))
			  ;; perhaps it is an empty directory
			  (delete-directory 
			   (pathname-as-directory file-or-directory) 
			   :errorp nil))))))
	     (append contents
		     (list directory-spec)))
       (values t)))
    ((eq if-does-not-exist :error)
     (error "Directory ~a does not exist" directory-spec))
    (t
     (values nil))))


(defun pathname-as-directory (pathname &key (errorp t))
  (when errorp
    (assert (not (directory-pathname-p pathname))))
  (if (directory-pathname-p pathname)
      pathname
      (make-pathname
       :name :unspecific
       :type :unspecific
       :directory (append (pathname-directory pathname)
			  (list (namestring (pathname-name+type pathname))))
       :defaults pathname)))
  
(defun directory-pathname-p (directory-spec)
  (and (or (null (pathname-name directory-spec))
	   (eq (pathname-name directory-spec) :unspecific))
       (or (null (pathname-type directory-spec))
	   (eq (pathname-type directory-spec) :unspecific))))
       
(defun directoryp (directory-spec)
  (or (directory-pathname-p directory-spec)
      ;; if it has sub-contents, it must be a directory
      (directory (make-pathname 
		  :name :wild
		  :type :wild
		  :directory `(,@(pathname-directory directory-spec)
				 ,(pathname-name+type directory-spec))
		  :defaults directory-spec))))
       
(defun delete-directory (file &key (errorp t))
  (when errorp
    (assert (directory-pathname-p file)))
  #+allegro
  (excl.osi:delete-directory-and-files file)
  #+:lispworks (lw:delete-directory file)
  #+:cmu (multiple-value-bind (ok err-number)
	     (unix:unix-rmdir (namestring (truename file)))
	   (unless ok
	     (error "Error number ~A when trying to delete ~A"
		    err-number file)))
  #+:sbcl (sb-posix:rmdir file)
  #+:clisp (ext:delete-dir file)
  #+:openmcl (ccl:delete-directory file)
  #+:cormanlisp (win32:delete-directory file)
  #+:ecl (si:rmdir file)
  #+(or :abcl :digitool) (delete-file file))

(defun pathname-name+type (pathname)
  (cond ((pathname-type pathname)
	 (concatenate 
	  'string (pathname-name pathname) "." (pathname-type pathname)))
	(t (pathname-name pathname))))
