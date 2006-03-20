(in-package :asdf-install)

(pushnew :asdf-install *features*)

(defun installer-msg (stream format-control &rest format-arguments)
  (apply #'format stream ";;; ASDF-INSTALL: ~@?~%" format-control format-arguments))


#+:digitool
(defparameter *home-volume-name*
  (second (pathname-directory (truename (user-homedir-pathname))))
  "Digitool MCL retains the OS 9 convention that ALL volumes have a
name which includes the startup volume. OS X doesn't know about this.
This figures in the home path and in the normalization for system
namestrings.")

(defvar *proxy* (get-env-var "http_proxy"))

(defvar *cclan-mirror*
  (or (get-env-var "CCLAN_MIRROR")
      "http://ftp.linux.org.uk/pub/lisp/cclan/"))

#+(or :win32 :mswindows)
(defvar *cygwin-bin-directory*
  (pathname "C:\\PROGRA~1\\Cygwin\\bin\\"))

#+(or :win32 :mswindows)
(defvar *cygwin-bash-program*
  "C:\\PROGRA~1\\Cygwin\\bin\\bash.exe")

(defvar *gnu-tar-program*
  #-(or :netbsd :freebsd) "tar"
  #+(or :netbsd :freebsd) "gtar"
  "Path to the GNU tar program")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *supported-defsystems*
    (list :mk-defsystem
          :asdf

          ;; Add others.
          ;; #+lispworks :common-defsystem
          ))
          

  (unless (some (lambda (defsys-tag)
                  (member defsys-tag *features*))
                *features*)
    (error "ASDF-INSTALL requires one of the following \"defsystem\" utilities to work: ~A"
           *supported-defsystems*)))



(defun directorify (name)
  ;; input name may or may not have a trailing #\/, but we know we
  ;; want a directory
  (let ((path (pathname name)))
    (if (pathname-name path)
	(merge-pathnames
	 (make-pathname :directory `(:relative ,(pathname-name path))
			:name "")
	 path)
	path)))

(defvar *asdf-install-dirs*
  (directorify (or #+sbcl (get-env-var "SBCL_HOME")
                   (get-env-var "ASDF_INSTALL_DIR")
                   (make-pathname :directory
                                  `(:absolute
                                    #+digitool ,*home-volume-name*
                                    "usr" "local" "asdf-install")))))

#+sbcl ; Deprecated.
(define-symbol-macro *sbcl-home* *asdf-install-dirs*)


(defvar *private-asdf-install-dirs*
  #+:sbcl
  (merge-pathnames (make-pathname :directory '(:relative ".sbcl"))
		   (truename (user-homedir-pathname)))
  #-:sbcl
  (cond ((get-env-var "PRIVATE_ASDF_INSTALL_DIR")
          (directorify (get-env-var "PRIVATE_ASDF_INSTALL_DIR")))
        (t
          (merge-pathnames (make-pathname :directory '(:relative ".asdf-install-dir"))
                           (truename (user-homedir-pathname))))))

#+sbcl ; Deprecated.
(define-symbol-macro *dot-sbcl* *private-asdf-install-dirs*)


(defvar *trusted-uids* nil)

(defvar *verify-gpg-signatures* t)

(defvar *safe-url-prefixes* nil)

(defvar *preferred-location* nil)

(defun verify-gpg-signatures-p (url)
  (labels ((prefixp (prefix string)
	     (let ((m (mismatch prefix string)))
	       (or (not m) (>= m (length prefix))))))
    (case *verify-gpg-signatures*
      ((nil) nil)
      ((:unknown-locations)
       (notany
	(lambda (x) (prefixp x url))
	*safe-url-prefixes*))
      (t t))))
	  
(defparameter *locations*
  `((,(merge-pathnames (make-pathname :directory '(:relative "site"))
                       *asdf-install-dirs*)
     ,(merge-pathnames (make-pathname :directory '(:relative "site-systems"))
                       *asdf-install-dirs*)
     "System-wide install")
    (,(merge-pathnames (make-pathname :directory '(:relative "site"))
                       *private-asdf-install-dirs*)
     ,(merge-pathnames (make-pathname :directory '(:relative "systems"))
                       *private-asdf-install-dirs*)
     "Personal installation")))


#+(and (not :sbcl) :asdf)
(pushnew `(merge-pathnames ,(make-pathname :directory '(:relative "site-systems"))
                           ,*asdf-install-dirs*)
         asdf:*central-registry*
         :test #'equal)

#+(and (not :sbcl) :asdf)
(pushnew `(merge-pathnames ,(make-pathname :directory '(:relative "systems"))
                           ,*private-asdf-install-dirs*)
         asdf:*central-registry*
         :test #'equal)

#+mk-defsystem
(mk:add-registry-location
 (merge-pathnames (make-pathname :directory '(:relative "site-systems"))
                  *private-asdf-install-dirs*))

#+mk-defsystem
(mk:add-registry-location
 (merge-pathnames (make-pathname :directory '(:relative "systems"))
                  *private-asdf-install-dirs*))


;;; Fixing the handling of *LOCATIONS*

(defun add-locations (loc-name site system-site)
  (declare (type string loc-name)
           (type pathname site system-site))
  #+asdf
  (progn
    (pushnew site asdf:*central-registry* :test #'equal)
    (pushnew system-site asdf:*central-registry* :test #'equal))

  #+mk-defsystem
  (progn
    (mk:add-registry-location site)
    (mk:add-registry-location system-site))
  (setf *locations*
        (append *locations* (list (list site system-site loc-name)))))



(eval-when (:load-toplevel :execute)
  (let* ((*package* (find-package :asdf-install-customize))
         (file (probe-file (merge-pathnames
			    (make-pathname :name ".asdf-install")
			    (truename (user-homedir-pathname)))))
         )
    (when file (load file))))


;;;---------------------------------------------------------------------------
;;; Conditions.

(define-condition download-error (error)
  ((url :initarg :url :reader download-url)
   (response :initarg :response :reader download-response))
  (:report (lambda (c s)
	     (format s "Server responded ~A for GET ~A"
		     (download-response c) (download-url c)))))

(define-condition signature-error (error)
  ((cause :initarg :cause :reader signature-error-cause))
  (:report (lambda (c s)
	     (format s "Cannot verify package signature:  ~A"
		     (signature-error-cause c)))))

(define-condition gpg-error (error)
  ((message :initarg :message :reader gpg-error-message))
  (:report (lambda (c s)
	     (format s "GPG failed with error status:~%~S"
		     (gpg-error-message c)))))

(define-condition shell-error (error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Call to GPG failed. Perhaps GPG is not installed or not~
 in the path."))))

(define-condition no-signature (gpg-error) ())

(define-condition key-not-found (gpg-error)
  ((key-id :initarg :key-id :reader key-id))
  (:report (lambda (c s)
	     (format s "No key found for key id 0x~A. ~
                        Try some command like ~%  gpg  --recv-keys 0x~A"
		     (key-id c) (key-id c)))))

(define-condition key-not-trusted (gpg-error)
  ((key-id :initarg :key-id :reader key-id)
   (key-user-name :initarg :key-user-name :reader key-user-name))
  (:report (lambda (c s)
	     (format s "GPG warns that the key id 0x~A (~A) is not fully trusted"
		     (key-id c) (key-user-name c)))))

(define-condition author-not-trusted (gpg-error)
  ((key-id :initarg :key-id :reader key-id)
   (key-user-name :initarg :key-user-name :reader key-user-name))
  (:report (lambda (c s)
	     (format s "~A (key id ~A) is not on your package supplier list"
		     (key-user-name c) (key-id c)))))
  

;;;---------------------------------------------------------------------------
;;; URL handling.

(defun url-host (url)
  (assert (string-equal url "http://" :end1 7))
  (let* ((port-start (position #\: url :start 7))
	 (host-end (min (or (position #\/ url :start 7) (length url))
			(or port-start (length url)))))
    (subseq url 7 host-end)))

(defun url-port (url)
  (assert (string-equal url "http://" :end1 7))
  (let ((port-start (position #\: url :start 7)))
    (if port-start (parse-integer url :start (1+ port-start) :junk-allowed t) 80)))

; This is from Juri Pakaste's <juri@iki.fi> base64.lisp
(defparameter *encode-table*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")

(defun base64-encode (string)
  (let ((result (make-array
                 (list (* 4 (truncate (/ (+ 2 (length string)) 3))))
                 :element-type 'base-char)))
    (do ((sidx 0 (+ sidx 3))
         (didx 0 (+ didx 4))
         (chars 2 2)
         (value nil nil))
        ((>= sidx (length string)) t)
      (setf value (ash (logand #xFF (char-code (char string sidx))) 8))
      (dotimes (n 2)
        (when (< (+ sidx n 1) (length string))
          (setf value
                (logior value
                        (logand #xFF (char-code (char string (+ sidx n 1))))))
          (incf chars))
        (when (= n 0)
          (setf value (ash value 8))))
      (setf (elt result (+ didx 3))
            (elt *encode-table* (if (> chars 3) (logand value #x3F) 64)))
      (setf value (ash value -6))
      (setf (elt result (+ didx 2))
            (elt *encode-table* (if (> chars 2) (logand value #x3F) 64)))
      (setf value (ash value -6))
      (setf (elt result (+ didx 1))
            (elt *encode-table* (logand value #x3F)))
      (setf value (ash value -6))
      (setf (elt result didx)
            (elt *encode-table* (logand value #x3F))))
    result))

(defvar *proxy-user* nil)
(defvar *proxy-passwd* nil)

(defun request-uri (url)
  (assert (string-equal url "http://" :end1 7))
  (if *proxy*
      url
      (let ((path-start (position #\/ url :start 7)))
        (subseq url path-start))))

(defun url-connection (url)
  (let ((stream (make-stream-from-url (or *proxy* url)))
        (host (url-host url)))
    (format stream "GET ~A HTTP/1.0~C~CHost: ~A~C~CCookie: CCLAN-SITE=~A~C~C"
            (request-uri url) #\Return #\Linefeed
            host #\Return #\Linefeed
            *cclan-mirror* #\Return #\Linefeed)
    (when (and *proxy-passwd* *proxy-user*)
      (format stream "Proxy-Authorization: Basic ~A~C~C"
              (base64-encode (format nil "~A:~A" *proxy-user* *proxy-passwd*))
              #\Return #\Linefeed))
    (format stream "~C~C" #\Return #\Linefeed)
    (force-output stream)
    (flet (#-:digitool
           (read-header-line ()
             (read-line stream))
           #+:digitool
           (read-header-line (&aux (line (make-array 16
                                                     :element-type 'character
                                                     :adjustable t
                                                     :fill-pointer 0))
                                   (byte nil))
             (print (multiple-value-bind (reader arg)
                        (ccl::stream-reader stream)
                      (loop (setf byte (funcall reader arg))
                            (case byte
                              ((nil)
                                (return))
                              ((#.(char-code #\Return)
                                  #.(char-code #\Linefeed))
                                (case (setf byte (funcall reader arg))
                                  ((nil #.(char-code #\Return) #.(char-code #\Linefeed)))
                                  (t (ccl:stream-untyi stream byte)))
                                (return))
                              (t
                                (vector-push-extend (code-char byte) line))))
                      (when (or byte (plusp (length line)))
                        line)))))
      (list
       (let* ((l (read-header-line))
              (space (position #\Space l)))
         (parse-integer l :start (1+ space) :junk-allowed t))
       (loop for line = (read-header-line)
             until (or (null line)
                       (zerop (length line))
                       (eql (elt line 0) (code-char 13)))
             collect
             (let ((colon (position #\: line)))
               (cons (intern (string-upcase (subseq line 0 colon)) :keyword)
                     (string-trim (list #\Space (code-char 13))
                                  (subseq line (1+ colon))))))
       stream))))


(defun download-files-for-package (package-name-or-url file-name)
  (let ((url (if (= (mismatch package-name-or-url "http://") 7)
               package-name-or-url
               (format nil "http://www.cliki.net/~A?download"
                       package-name-or-url)))
        )
    (destructuring-bind (response headers stream)
	                (block got
	                  (loop
	                    (destructuring-bind (response headers stream) (url-connection url)
	                      (unless (member response '(301 302))	       
	                        (return-from got (list response headers stream)))
	                      (close stream)
	                      (setf url (cdr (assoc :LOCATION headers))))))
      (when (>= response 400)
        (error 'download-error :url url :response response))
      (let ((length (parse-integer (or (cdr (assoc :CONTENT-LENGTH headers)) "")
		                   :junk-allowed t)))
	(installer-msg t "Downloading ~A bytes from ~A to ~A ..."
		       (or length "some unknown number of")
                       url
                       file-name)
	(force-output)
        #+:clisp (setf (stream-element-type stream)
                       '(unsigned-byte 8))
        (with-open-file 
          #-(and allegro-version>= (not (version>= 8 0)))
          (o file-name :direction :output
             #+(or :clisp :digitool (and :lispworks :win32))
             :element-type
             #+(or :clisp :digitool (and :lispworks :win32))
             '(unsigned-byte 8)
             #+:sbcl #+:sbcl :external-format :latin1
             :if-exists :supersede)
          ;; for Allegro  versions  < 8.0,  the above  #+sbcl #+sbcl
          ;; will cause an error [2006/01/09:rpg]
          #+(and allegro-version>= (not (version>= 8 0)))
          (o file-name :direction :output :if-exists :supersede)
          #+(or :cmu :digitool)
          (copy-stream stream o)
          #-(or :cmu :digitool)
	  (if length
            (let ((buf (make-array length
                                   :element-type
                                   (stream-element-type stream))))
              #-:clisp (read-sequence buf stream)
              #+:clisp (ext:read-byte-sequence buf stream :no-hang nil)
              (write-sequence buf o))
            (copy-stream stream o))))
      (close stream)
      (terpri)
      (restart-case 
        (verify-gpg-signature/url url file-name)
	(skip-gpg-check (&rest rest)
	                :report "Don't check GPG signature for this package"
                        (declare (ignore rest))
	                nil)))))


(defun read-until-eof (stream)
  (with-output-to-string (o)
    (copy-stream stream o)))

  
(defun verify-gpg-signature/string (string file-name)
  (let ((gpg-stream (make-stream-from-gpg-command string file-name))
        tags)
    (unwind-protect
      (loop for l = (read-line gpg-stream nil nil)
            while l
            do (print l)
            when (> (mismatch l "[GNUPG:]") 6)
            do (destructuring-bind (_ tag &rest data)
                                   (split-sequence:split-sequence-if (lambda (x)
                                                                       (find x '(#\Space #\Tab)))
                                                                     l)
	         (declare (ignore _))
                 (pushnew (cons (intern (string-upcase tag) :keyword)
			        data) tags)))
      (ignore-errors
       (close gpg-stream)))
    ;; test that command returned something 
    (unless tags
      (error 'shell-error))
    ;; test for obvious key/sig problems
    (let ((errsig (assoc :ERRSIG tags)))
      (and errsig (error 'key-not-found :key-id (second errsig))))
    (let ((badsig (assoc :BADSIG tags)))
      (and badsig (error 'key-not-found :key-id (second badsig))))
    (let* ((good (assoc :GOODSIG tags))
	   (id (second good))
	   (name (format nil "~{~A~^ ~}" (nthcdr 2 good))))
      ;; good signature, but perhaps not trusted
      (unless (or (assoc :TRUST_ULTIMATE tags)
		  (assoc :TRUST_FULLY tags))
	(cerror "Install the package anyway"
		'key-not-trusted
		:key-user-name name
		:key-id id))
      (loop
        (when
          (restart-case
            (or (assoc id *trusted-uids* :test #'equal)
                (error 'author-not-trusted
                       :key-user-name name
                       :key-id id))
            (add-key (&rest rest)
	             :report "Add to package supplier list"
                     (declare (ignore rest))
	             (pushnew (list id name) *trusted-uids*)))
	  (return))))))


(defun verify-gpg-signature/url (url file-name)
  (when (verify-gpg-signatures-p url)
    (destructuring-bind (response headers stream)
        (url-connection (concatenate 'string url ".asc"))
      (unwind-protect
        (flet (#-:digitool
               (read-signature (data stream)
                 (read-sequence data stream))
               #+:digitool
               (read-signature (data stream)
                 (multiple-value-bind (reader arg)
                     (ccl:stream-reader stream)
                   (let ((byte 0))
                     (dotimes (i (length data))
                       (unless (setf byte (funcall reader arg))
                         (error 'download-error :url  (concatenate 'string url ".asc")
                                :response 200))
                       (setf (char data i) (code-char byte)))))))
          (if (= response 200)
            (let ((data (make-string (parse-integer
                                      (cdr (assoc :CONTENT-LENGTH headers))
                                      :junk-allowed t))))
              (read-signature data stream)
              (verify-gpg-signature/string data file-name))
            (error 'download-error :url  (concatenate 'string url ".asc")
                   :response response)))
        (close stream)))))


(define-condition installation-abort (condition)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (installer-msg s "Installation aborted."))))


(defun where ()
  (loop with n-locations = (length *locations*)
        for response = (or *preferred-location*             
                           (progn
                             (format t "Install where?~%")
                             (loop for (source system name) in *locations*
                                   for i from 0
                                   do (format t "~A) ~A: ~%   System in ~A~%   Files in ~A ~%"
                                              i name system source))
                             (format t "~D) Abort installation.~% --> " n-locations)
                             (force-output)
                             (read)))
        when (and (numberp response)
                  (<= 0 response (1- n-locations)))
           return (elt *locations* response)
        when (and (numberp response)
                  (= response n-locations))
           do (abort (make-condition 'installation-abort))))


;;; install-package --

(defun install-package (source system packagename)
  "Returns a list of system names (ASDF or MK:DEFSYSTEM) for installed systems."
  (ensure-directories-exist source)
  (ensure-directories-exist system)
  (let* ((tar
          (or #-(or :win32 :mswindows)
              (return-output-from-program *gnu-tar-program*
                                          (list "-C" (namestring (truename source))
                                                "-xzvf" (namestring (truename packagename))))
              #+(or :win32 :mswindows)
              (return-output-from-program *cygwin-bash-program*
                                          (list "-l"
                                                "-c"
                                                (format nil "\"tar -C \\\"`cygpath '~A'`\\\" -xzvf \\\"`cygpath '~A'`\\\"\""
                                                        (namestring (truename source))
                                                        (namestring (truename packagename)))))
              (error "ASDF-INSTALL: can't untar ~S." packagename)))
	 (pos-slash (or (position #\/ tar)
                        (position #\Return tar)
                        (position #\Linefeed tar)))
	 (*default-pathname-defaults*
	  (merge-pathnames
	   (make-pathname :directory
			  `(:relative ,(subseq tar 0 pos-slash)))
	   source))
         )
    (princ tar)
    (loop for sysfile in (append
                          (directory
		           (make-pathname :defaults (print *default-pathname-defaults*)
                                          :name :wild
                                          :type "asd"))
                          (directory
		           (make-pathname :defaults (print *default-pathname-defaults*)
                                          :name :wild
                                          :type "system")))
          #-(or :win32 :mswindows)
          do
	  #-(or :win32 :mswindows)
          (let ((target (merge-pathnames
                         (make-pathname :name (pathname-name sysfile)
                                        :type (pathname-type sysfile))
                         system)))
            (when (probe-file target)
              (unlink-file target))
            (symlink-files sysfile target))
	  collect sysfile)))

(defun temp-file-name (p)
  (let* ((pos-slash (position #\/ p :from-end t))
	 (pos-dot (position #\. p :start (or pos-slash 0))))
    (merge-pathnames
     (make-pathname
      :name (subseq p (if pos-slash (1+ pos-slash) 0) pos-dot)
      :type "asdf-install-tmp")
     #+:clisp (user-homedir-pathname))))


;;; install
;;; This is the external entry point.

(defun install (&rest packages)
  (let* ((*temporary-files* nil)
         (trusted-uid-file 
          (merge-pathnames "trusted-uids.lisp" *private-asdf-install-dirs*))
	 (*trusted-uids*
          (when (probe-file trusted-uid-file)
            (with-open-file (f trusted-uid-file) (read f))))
         (old-uids (copy-list *trusted-uids*)))
    (unwind-protect
      (destructuring-bind (source system name) (where)
        (declare (ignore name))
        (labels ((one-iter (packages)
                   (let ((installed-package-sysfiles
                          (loop for p in (mapcar #'string packages)
                                unless
                                #+(or :sbcl :alisp) (probe-file p)
                                #-(or :sbcl :alisp) (and (/= (mismatch p "http://") 7)
                                                         (probe-file p))
                                do (let ((tmp (temp-file-name p)))
                                     (pushnew tmp *temporary-files*)
                                     (download-files-for-package p tmp)
                                     (setf p tmp))
                                end
                                do (installer-msg t "Installing ~A in ~A, ~A"
                                                  p source system)
                                append (install-package source system p)))
                         )
                     (declare (ignore installed-package-sysfiles))
                     (dolist
                       ;; 20 Mar 2006
                       ;; only install the packages we asked for
                       (package packages) 
                       #+old-asdf-behavior
                       ;; install every package we downloaded
                       (sysfile installed-package-sysfiles)
                       (handler-bind
                         (
                          #+asdf
                          (asdf:missing-dependency
                           (lambda (c) 
                             (installer-msg t
                                            "Downloading package ~A, required by ~A~%"
                                            (asdf::missing-requires c)
                                            (asdf:component-name
                                             (asdf::missing-required-by c)))
                             (one-iter (list
                                        (asdf::coerce-name
                                         (asdf::missing-requires c))))
                             (invoke-restart 'retry)))
                          
                          #+mk-defsystem
                          (make:missing-component
                           (lambda (c) 
                             (installer-msg t
                                            "Downloading package ~A, required by ~A~%"
                                            (make:missing-component-name c)
                                            package
                                            )
                             (one-iter (list (make:missing-component-name c)))
                             (invoke-restart 'retry))))
                         
                         (loop (multiple-value-bind (ret restart-p)
                                                    (with-simple-restart
                                                      (retry "Retry installation")
                                                      (load-package package))
                                 (declare (ignore ret))
                                 (unless restart-p (return))))
                         )))))
          (one-iter packages)))
      
      ;;; cleanup
      (unless (equal old-uids *trusted-uids*)
        (let ((create-file-p nil))
	  (unless (probe-file trusted-uid-file)
	    (installer-msg t "Trusted UID file ~A does not exist"
			   (namestring trusted-uid-file))
	    (setf create-file-p
		  (y-or-n-p "Do you want to create the file?")))
          (when (or create-file-p (probe-file trusted-uid-file))
	    (with-open-file (out trusted-uid-file
                                 :direction :output
                                 :if-exists :supersede)
	      (with-standard-io-syntax
	        (prin1 *trusted-uids* out))))))
      (dolist (l *temporary-files* t)
	(when (probe-file l) (delete-file l))))))

(defun load-package (package)
  #+asdf
  (asdf:operate 'asdf:load-op package))

#+Old
(defun load-system-definition (sysfile)
  (declare (type pathname sysfile))
  #+asdf
  (when (or (string-equal "asd" (pathname-type sysfile))
            (string-equal "asdf" (pathname-type sysfile)))
    (installer-msg t "Loading system ~S via ASDF." (pathname-name sysfile))
    ;; just load the system definition
    (load sysfile)
    #+Ignore
    (asdf:operate 'asdf:load-op (pathname-name sysfile)))

  #+mk-defsystem
  (when (string-equal "system" (pathname-type sysfile))
    (installer-msg t "Loading system ~S via MK:DEFSYSTEM." (pathname-name sysfile))
    (mk:load-system (pathname-name sysfile))))


;;; uninstall --

(defun uninstall (system &optional (prompt t))
  #+asdf
  (let* ((asd (asdf:system-definition-pathname system))
	 (system (asdf:find-system system))
	 (dir (asdf::pathname-sans-name+type
	       (asdf::resolve-symlinks asd))))
    (when (or (not prompt)
	      (y-or-n-p
	       "Delete system ~A~%asd file: ~A~%sources: ~A~%Are you sure?"
	       system asd dir))
      #-(or :win32 :mswindows)
      (delete-file asd)
      (asdf:run-shell-command "rm -r '~A'" (namestring (truename dir)))))

  #+mk-defsystem
  (multiple-value-bind (sysfile sysfile-exists-p)
      (mk:system-definition-pathname system)
    (when sysfile-exists-p
      (let ((system (ignore-errors (mk:find-system system :error))))
        (when system
          (when (or (not prompt)
	            (y-or-n-p
	             "Delete system ~A.~%system file: ~A~%Are you sure?"
	             system
                     sysfile))
            (mk:clean-system system)
            (delete-file sysfile)
            (dolist (f (mk:files-in-system system))
              (delete-file f)))
          ))
      )))

      
;;; some day we will also do UPGRADE, but we need to sort out version
;;; numbering a bit better first

#+(and :asdf (or :win32 :mswindows))
(defun sysdef-source-dir-search (system)
  (let ((name (asdf::coerce-name system)))
    (dolist (location *locations*)
      (let* ((dir (first location))
             (files (directory (merge-pathnames
                                (make-pathname :name name
                                               :type "asd"
                                               :version :newest
                                               :directory '(:relative :wild)
                                               :host nil
                                               :device nil)
                                dir))))
        (dolist (file files)
          (when (probe-file file)
            (return-from sysdef-source-dir-search file)))))))

;;; end of file -- install.lisp --
