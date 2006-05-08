(in-package asdf-install)

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

#+:digitool
(defparameter *home-volume-name*
  (second (pathname-directory (truename (user-homedir-pathname))))
  "Digitool MCL retains the OS 9 convention that ALL volumes have a
name which includes the startup volume. OS X doesn't know about this.
This figures in the home path and in the normalization for system
namestrings.")

(defvar *proxy* (get-env-var "http_proxy"))

(defvar *proxy-user* nil)

(defvar *proxy-passwd* nil)

(defvar *trusted-uids* nil)

(defvar *verify-gpg-signatures* t)

(defvar *safe-url-prefixes* nil)

(defvar *preferred-location* nil)

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

(defvar *asdf-install-dirs*
  (directorify (or #+sbcl (get-env-var "SBCL_HOME")
                   (get-env-var "ASDF_INSTALL_DIR")
                   (make-pathname :directory
                                  `(:absolute
                                    #+digitool ,*home-volume-name*
                                    "usr" "local" "asdf-install")))))

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

(defvar *systems-installed-this-time* nil
  "Used during installation propagation \(see *propagate-installation*\) to keep track off which systems have been installed during the current call to install.")

(defvar *propagate-installation* nil
  "If true, then every required system will be re-asdf-installed.")



