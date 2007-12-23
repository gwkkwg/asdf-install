(in-package #:asdf-install)

(defun directorify (name)
  ;; On unix-like system, the input name may or may not have a
  ;; trailing #\/, and thus be interpreted as a file rather than a
  ;; directory. But we know we want a directory. So we assume that any
  ;; of file name, type, or version might actually be part of the
  ;; final directory element. This isn't 100% portable to all
  ;; imaginable systems, but ought to be pretty good.
  ;;
  ;; Note that paths ending with something that looks like a file type
  ;; will not be handled correctly. [dwm]
  (let ((path (pathname name)))
    (if (or (pathname-name path) (pathname-type path))
        (let ((file-sans-directory
               (namestring (make-pathname :name (pathname-name path)
                                          :type (pathname-type path)
                                          :version (pathname-version path)))))
          ;; Note that make-pathname will not combine a relative
          ;; :directory with a directory taken from a :defaults
          ;; argument. We can only rely on merge-pathnames to do that.
          (merge-pathnames
           (make-pathname :directory (list :relative file-sans-directory))
           (make-pathname :defaults path :name nil :type nil :version nil)))
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

(defvar *verify-gpg-signatures* t
  "Can be t, nil, or :unknown-locations. If true, then the signature of all packages will be checked. If nil, then no signatures will be checked. If :unkown-locations, then only packages whose location is not a prefix of any `*safe-url-prefixes*` will be tested.")

(defvar *safe-url-prefixes* nil)

(defvar *preferred-location* nil)

(defvar *cclan-mirror*
  (or (get-env-var "CCLAN_MIRROR")
      "http://ftp.linux.org.uk/pub/lisp/cclan/"))

#+(or :win32 :mswindows)
(defvar *cygwin-bin-directory*
  (pathname "C:\\PROGRA~1\\Cygwin\\bin\\"))

(defvar *program-directories*
  #-(or :win32 :mswindows)
  ;; bin first
  (list (make-pathname :directory '(:absolute "bin"))
        (make-pathname :directory '(:absolute "usr" "bin")))

  ;; On Windows, there's no notion of standard paths containing other
  ;; than OS components. Simply use the same path that the user does.
  #+(or :win32 :mswindows)
  (loop
     for path = (get-env-var "PATH")
     then (subseq path (1+ (or (position #\; path) (1- (length path)))))
     for elem = (subseq path 0 (position #\; path))
     while (plusp (length elem))
     collect (directorify elem))
  "A list of places to look for shell commands, as pathnames.")

(defvar *gnu-tar-program*
  #-(or :netbsd :freebsd :solaris :win32 :mswindows) "tar"
  #+(or :netbsd :freebsd :solaris) "gtar"
  #+(or :win32 :mswindows) "tar.exe"
  "Path to the GNU tar program")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *supported-defsystems*
    (list :mk-defsystem
          :asdf

          ;; Add others.
          ;; #+lispworks :common-defsystem
          ;; #+gbbopen :mini-module
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
          (merge-pathnames (make-pathname 
                            :directory '(:relative ".asdf-install-dir"))
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

(defvar *tar-extractors*
  '(extract-using-tar))

(defvar *systems-installed-this-time* nil
  "Used during installation propagation \(see *propagate-installation*\) to keep track off which systems have been installed during the current call to install.")

(defvar *propagate-installation* nil
  "If true, then every required system will be re-asdf-installed.")

(defvar *temporary-directory* 
  (pathname-sans-name+type (user-homedir-pathname)))

;;; Local variables:
;;; indent-tabs-mode:nil
;;; End:
