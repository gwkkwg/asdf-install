(in-package #:asdf-install)

(pushnew :asdf-install *features*)

(defun ignore-level ()
  (- 30 (case *verbosity*
          (:quiet 0)
          (:verbose 20)
          (:debug 30)
          (t 10))))

(defun installer-msg (importance format-control &rest format-arguments)
  (when (>= importance (ignore-level))
    (apply #'format *asdf-install-message-stream*
           "~&;;; ASDF-INSTALL: ~@?~%"
           format-control format-arguments)))

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
          
(defun same-central-registry-entry-p (a b)
  (flet ((ensure-string (x)
           (typecase x
             (string x)
             (pathname (namestring (translate-logical-pathname x)))
             (t nil))))
    (and (setf a (ensure-string a))
         (setf b (ensure-string b))
         a b (string-equal a b))))

(defun add-registry-location (location)
  (let ((location-directory (pathname-sans-name+type location)))
    #+asdf
    (pushnew location-directory
             asdf:*central-registry*
             :test #'same-central-registry-entry-p)
  
    #+mk-defsystem
    (mk:add-registry-location location-directory)))

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
    (if port-start 
        (parse-integer url :start (1+ port-start) :junk-allowed t) 80)))

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

(defun request-uri (url)
  (assert (string-equal url "http://" :end1 7))
  (if *proxy*
      url
      (let ((path-start (position #\/ url :start 7)))
        (assert (and path-start) nil "url does not specify a file.")
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
    (list
     (let* ((l (read-header-line stream))
            (space (position #\Space l)))
       (parse-integer l :start (1+ space) :junk-allowed t))
     (loop for line = (read-header-line stream)
           until (or (null line)
                     (zerop (length line))
                     (eql (elt line 0) (code-char 13)))
           collect
           (let ((colon (position #\: line)))
             (cons (intern (string-upcase (subseq line 0 colon)) :keyword)
                   (string-trim (list #\Space (code-char 13))
                                (subseq line (1+ colon))))))
     stream)))

(defun download-link-for-package (package-name-or-url)
  (if (= (mismatch package-name-or-url "http://") 7)
    package-name-or-url
    (format nil "http://www.cliki.net/~A?download"
            package-name-or-url)))

(defun download-link-for-signature (url)
  (concatenate 'string url ".asc"))

(defun download-files-for-package (package-name-or-url)
  "Downloads package file. Returns temporary package file name, and
either nil or the package's signature file name."
  (installer-msg 0 "Download files for ~a" package-name-or-url)
  (multiple-value-bind (package-url package-file) 
      (progn
        (installer-msg 0 "  Download ~a" 
                       (download-link-for-package package-name-or-url))
        (download-url-to-temporary-file
         (download-link-for-package package-name-or-url)))
    (if (verify-gpg-signatures-p package-name-or-url)
        (restart-case 
            (multiple-value-bind (signature-url signature-file) 
                (progn
                  (download-url-to-temporary-file
                   (download-link-for-signature package-url))
                  (installer-msg 0 "Download files for ~a" 
                                 (download-link-for-signature package-url)))  
              (declare (ignore signature-url))
              (values package-file signature-file))
          (ignore-signature-for-this-package (&rest rest)
            :report "Ignore signature file and install anyway"
            (declare (ignore rest))
            (values package-file nil)))
        (values package-file nil))))
  
(defun verify-gpg-signature (file-name signature-name)
  (installer-msg 0 "Verify GPG signature ~a, ~a" 
                 file-name signature-name)
  (block verify
    (loop
      (restart-case
          (let ((tags (gpg-results file-name signature-name)))
            ;; test that command returned something 
            (unless tags
              (error 'gpg-shell-error))
            ;; test for obvious key/sig problems
            (let ((errsig (header-value :errsig tags)))
              (and errsig (error 'key-not-found :key-id errsig)))
            (let ((badsig (header-value :badsig tags)))
              (and badsig (error 'key-not-found :key-id badsig)))
            (let* ((good (header-value :goodsig tags))
                   (id (first good))
                   (name (format nil "~{~A~^ ~}" (rest good))))
              ;; good signature, but perhaps not trusted
              (restart-case
                  (let ((trusted? (or (header-pair :trust_ultimate tags)
                                      (header-pair :trust_fully tags)))
                        (in-list? (assoc id *trusted-uids* :test #'equal)))
                    (cond ((or trusted? in-list?)
                           ;; ok
                           )
                          ((not trusted?)
                           (error 'key-not-trusted 
                                  :key-user-name name :key-id id))
                          ((not in-list?)
                           (error 'author-not-trusted
                                  :key-user-name name :key-id id))))
                (add-key (&rest rest)
                  :report "Add to package supplier list"
                  (declare (ignore rest))
                  (pushnew (list id name) *trusted-uids*))))
            (return-from verify t))
        (install-anyways
            (&rest rest)
          :report "Don't check GPG signature for this package"
          (declare (ignore rest))
          (return-from verify t))
        (retry-gpg-check
            (&rest args)
          :report "Retry GPG check \(e.g., after downloading the key\)"
          (declare (ignore args))
          nil)))))

(defun gpg-results (package signature)
  (let ((tags nil))
    (with-input-from-string
        (gpg-stream 
         (return-output-from-program
          (find-program *gpg-program*)
          (list "--status-fd" "1"
                "--verify"
                (namestring signature)
                (namestring package))))
      (loop for l = (read-line gpg-stream nil nil)
         while l
         do (print l)
         when (> (mismatch l "[GNUPG:]") 6)
         do (destructuring-bind (_ tag &rest data)
                (split-sequence-if (lambda (x)
                                     (find x '(#\Space #\Tab)))
                                   l)
              (declare (ignore _))
              (pushnew (cons (intern (string-upcase tag) :keyword)
                             data) tags)))
      tags)))

(defun header-value (name headers)
  "Searchers headers for name _without_ case sensitivity. Headers should be an alist mapping symbols to values; name a symbol. Returns the value if name is found or nil if it is not."
  (cdr (header-pair name headers)))

(defun header-pair (name headers)
  "Searchers headers for name _without_ case sensitivity. Headers should be an alist mapping symbols to values; name a symbol. Returns the \(name value\) pair if name is found or nil if it is not."
  (assoc name headers 
         :test (lambda (a b) 
                 (string-equal (symbol-name a) (symbol-name b)))))

(defun validate-preferred-location ()
  (typecase *preferred-location*
    (null t)
    ((integer 0) 
     (assert (<= 1 *preferred-location* (length *locations*)) 
             (*preferred-location*)
             'invalid-preferred-location-number-error
             :preferred-location *preferred-location*))
    ((or symbol string) 
     (assert (find *preferred-location* *locations* 
                   :test (if (typep *preferred-location* 'symbol)
                             #'eq #'string-equal) :key #'third)
             (*preferred-location*)
             'invalid-preferred-location-name-error 
             :preferred-location *preferred-location*))
    (t
     (assert nil 
             (*preferred-location*)
             'invalid-preferred-location-error 
             :preferred-location *preferred-location*)))
  *preferred-location*)

(defun select-location ()
  (loop with n-locations = (length *locations*)
     for response = (progn
                      (format t "Install where?~%")
                      (loop for (source system name) in *locations*
                         for i from 1
                         do (format t "~A) ~A: ~%   System in ~A~%   Files in ~A ~%"
                                    i name system source))
                      (format t "0) Abort installation.~% --> ")
                      (force-output)
                      (read))
     when (and (numberp response)
               (<= 1 response n-locations))
     return response
     when (and (numberp response)
               (zerop response))
     do (abort (make-condition 'installation-abort))))

(defun install-location ()
  (validate-preferred-location)
  (let ((location-selection (or *preferred-location*
                                (select-location))))
    (etypecase location-selection
      (integer 
       (elt *locations* (1- location-selection)))
      ((or symbol string)
       (find location-selection *locations* :key #'third
             :test (if (typep location-selection 'string) 
                      #'string-equal #'eq))))))


;;; install-package --

(defun find-program (command)
  "Returns the fully-qualified namestring of the first instance of a
program with name COMMAND which is found in the list of directories in
*PROGRAM-DIRECTORIES*, or NIL if it cannot be found. COMMAND should
include the filename and extension, if applicable."
  (installer-msg 0 "find program for ~a" command)
  (loop for directory in *program-directories* do
       (let ((target (merge-pathnames (pathname command) directory)))
         (when (probe-file target)
           (return-from find-program (namestring target)))))
  (values nil))

(defun tar-command ()
  (find-program *gnu-tar-program*))

(defun extract-using-tar (to-dir tarball)
  (installer-msg 0 "extract with tar ~a to ~a" to-dir tarball)
  (let ((tar-command (tar-command)))
    (if tar-command
        (return-output-from-program tar-command
                                    (list "-C" (tar-argument to-dir)
                                          "-xzvf" (tar-argument tarball)))
        (warn "Cannot find tar command ~S." tar-command))))

(defun extract (to-dir tarball)
  (or (funcall *tar-extractor* to-dir tarball)
      (error "Unable to extract tarball ~A." tarball)))

(defun install-package (source-directory system-directory package-file-name)
  "Returns a list of system names (ASDF or MK:DEFSYSTEM) for installed systems."
  (ensure-directories-exist source-directory)
  (ensure-directories-exist system-directory)
  (let* ((tar (extract source-directory package-file-name))
         (pos-slash (or (position #\/ tar)
                        (position #\Return tar)
                        (position #\Linefeed tar)))
         (package-dir
          (merge-pathnames
           (make-pathname :directory
                          `(:relative ,(subseq tar 0 pos-slash)))
           source-directory)))
    ;(princ tar)
    (loop for sysfile in (append
                          (directory
                           (make-pathname :defaults package-dir
                                          :name :wild
                                          :type "asd"))
                          (directory
                           (make-pathname :defaults package-dir
                                          :name :wild
                                          :type "system")))
       do (funcall *system-file-installer* system-directory sysfile)
       do (installer-msg 10 "Found system definition: ~A" sysfile)
       do (maybe-update-central-registry sysfile)
       collect sysfile)))

(defun maybe-update-central-registry (sysfile)
  ;; make sure that the systems we install are accessible in case
  ;; asdf-install:*locations* and asdf:*central-registry* are out
  ;; of sync
  (add-registry-location sysfile))

(defun temp-file-name (p)
  (declare (ignore p))
  (let ((pathname nil))
    (loop for i = 0 then (1+ i) do
         (setf pathname 
               (merge-pathnames
                (make-pathname
                 :name (format nil "asdf-install-~d" i)
                 :type "asdf-install-tmp")
                *temporary-directory*))
         (unless (probe-file pathname)
           (return-from temp-file-name pathname)))))

(defun canonical-system-name (name)
  "Return a canonical representation of a system name for use within
ASDF-INSTALL."
  ;; We assume that if a string is given, case matters, otherwise it
  ;; does not. Some defsystems care, others don't.
  (intern 
   (etypecase name
     (symbol (string-upcase (string name)))
     (string name))
   '#:asdf-install))

(defun handle-download-package (package)
  "Determine the package and package-file for a downloaded ASDF package."
  (multiple-value-bind (package-file signature)
      (download-files-for-package package)
    (when (and signature (verify-gpg-signatures-p package))
      (verify-gpg-signature package-file signature))
    (values (typecase package
              (pathname nil)
              (string (if (looks-like-url-p package)
                          nil (canonical-system-name package)))
              (t (canonical-system-name package)))
            package-file)))

(defun handle-local-package (package)
  "Determine the package and package-file for a local ASDF package. We
can't determine the package-name until after we've extracted the contents
of the local file. So we do this in the call to XXX after we've installed
the package."
  (values nil package))

(defun determine-package-file (package)
  (if (local-archive-p package)
      (handle-local-package package)
      (handle-download-package package)))

(defun determine-system-names (package package-file system-defs)
  ;; see determine-package-file too
  (let ((best-system-name nil))
    (cond
      ;; either we have a package already (it was a CLiki downloaded system)
      ((and (not (null package))
            (symbolp package)) 
       (list package))
      ;; OR, we need to determine the package based on the system-defs 
      ;; we found and the name of the package-file.
      ((let ((package-filename (namestring package-file))
             (best-match 0))
         (loop for system-def in system-defs 
            for system-name = (pathname-name system-def) do
            (when (> (mismatch system-name package-filename) best-match)
              (setf best-match (mismatch system-name package-file)
                    best-system-name system-name)))
         best-system-name)
       (list best-system-name))
      ;; OR, that didn't work so we'll have to do all of the systems
      (t
       (loop for system-def in system-defs collect
            (pathname-name system-def))))))

(defun install-packages (packages source system-directory)
  (let ((systems-to-load nil))
    ;; First download all the packages and collect a list of
    ;; top-level systems that need to be loaded.
    (loop
       for p in (mapcar (lambda (package)
                          (typecase package
                            (pathname (namestring package))
                            (t (string package))))
                        packages) do
       (multiple-value-bind (package package-file) 
           (determine-package-file p)
         (installer-msg 20 "Installing ~A in ~A, ~A"
                        package-file source system-directory)
         (let ((system-defs
                (install-package source system-directory package-file)))
           (loop for system in 
                (determine-system-names package package-file system-defs) do
              ;; we only add the systems that were explicitly requested
              ;; otherwise, we'll load too many systems (like test systems 
              ;; and platform specific ones).
                (pushnew system systems-to-load :test #'equal)))))
    
    (setf systems-to-load (nreverse systems-to-load))
    
    (dolist (system systems-to-load)
      (setf system (canonical-system-name system))
      (handler-bind
          ((need-more-packages
            (lambda (c) 
              (install-packages (packages-needed c)
                                source
                                system-directory)
              (invoke-restart 'retry))))
        (loop (multiple-value-bind (ret restart-p)
                  (with-simple-restart
                      (retry "Retry installation")
                    (pushnew system *systems-installed-this-time* :test #'equal)
                    (load-package system))
                (declare (ignore ret))
                (unless restart-p (return))))))))

;;; install
;;; This is the external entry point.

(defun install (packages &key (propagate nil) (where *preferred-location*)
                (verbose nil))
  (let* ((*verbosity* verbose)
         (*preferred-location* where)
         (*temporary-files* nil)
         (trusted-uid-file 
          (merge-pathnames "trusted-uids.lisp" *private-asdf-install-dirs*))
         (*trusted-uids*
          (when (probe-file trusted-uid-file)
            (with-open-file (f trusted-uid-file) (read f))))
         (old-uids (copy-list *trusted-uids*))
         (packages (if (atom packages) (list packages) packages))
         (*propagate-installation* propagate)
         (*systems-installed-this-time* nil))
    (unwind-protect
      (destructuring-bind (source system name) (install-location)
        (declare (ignore name))
        (install-packages packages source system))
      ;;; cleanup
      (unless (equal old-uids *trusted-uids*)
        (let ((create-file-p nil))
          (unless (probe-file trusted-uid-file)
            (installer-msg 30 "Trusted UID file ~A does not exist"
                           (namestring trusted-uid-file))
            (setf create-file-p
                  (y-or-n-p "Do you want to create the file?")))
          (when (or create-file-p (probe-file trusted-uid-file))
            (ensure-directories-exist trusted-uid-file)
            (with-open-file (out trusted-uid-file
                                 :direction :output
                                 :if-exists :supersede)
              (with-standard-io-syntax
                (prin1 *trusted-uids* out))))))
      (dolist (l *temporary-files* t)
        (when (probe-file l) (delete-file l))))
    (nreverse *systems-installed-this-time*)))

(defun looks-like-url-p (string)
  (= (mismatch string "http://") 7))

(defun load-package (system)
  #+asdf
  (handler-bind
      ((asdf:missing-dependency
        (lambda (c) 
          (installer-msg
           10
           "Downloading package ~A, required by ~A~%"
           (asdf::missing-requires c)
           (asdf:component-name 
            (asdf::missing-required-by c)))
          (error 'need-more-packages
                 :defsystem :asdf
                 :failed-component (asdf::missing-required-by c)
                 :packages-needed (list (asdf::coerce-name
                                         (asdf::missing-requires c)))))))
    (installer-msg 10 "Loading system ~S via ASDF." system)
    (asdf:operate 'asdf:load-op system))
       
  #+mk-defsystem
  (handler-bind
      ((make:missing-component
        (lambda (c) 
          (installer-msg 
           10
           "Downloading package ~A, required by ~A~%"
           (make:missing-component-name c)
           system)
          (error 'need-more-packages
                 :defsystem :mk-defsystem
                 :failed-component (make:missing-component-component c)
                 :packages-needed (list (make:missing-component-name c))))))
    (installer-msg 10 "Loading system ~S via MK:DEFSYSTEM." system)
    (mk:load-system system)))

;;; uninstall --

(defun uninstall (system &optional (prompt t))
  #+asdf
  (let* ((asd (asdf:system-definition-pathname system))
         (system (asdf:find-system system))
         (dir (pathname-sans-name+type
               (asdf::resolve-symlinks asd))))
    (when (or (not prompt)
              (y-or-n-p
               "Delete system ~A~%asd file: ~A~%sources: ~A~%Are you sure?"
               system asd dir))
      #-(or :win32 :mswindows)
      (delete-file asd)
      (let ((dir (#-scl namestring #+scl ext:unix-namestring (truename dir))))
        (when dir
          (asdf:run-shell-command "rm -r '~A'" dir)))))

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

;;; This auxiliary method forces an ASDF system to appear to be
;;; missing if *propagate-installation* is true, and if the system has
;;; been (or is being) specifically installed by the current
;;; invocation of ASDF-INSTALL:INSTALL.
(defmethod asdf:find-component :around 
    ((module (eql nil)) name #-asdf2 &optional #-asdf2 version)
  #-asdf2 (declare (ignore version))
  (when (or (not *propagate-installation*)
            (and (typep name '(or string (and symbol (not null))))
                 (member name *systems-installed-this-time*
                         :test #'string-equal)))
    (call-next-method)))

(defun show-version-information ()
  (let ((version (asdf-install-version)))
    (if version
      (format *standard-output* "~&;;; ASDF-Install version ~A"
              version)
      (format *standard-output* "~&;;; ASDF-Install version unknown; unable to find ASDF system definition."))
  (values)))

(defun asdf-install-version ()
  "Returns the ASDf-Install version information as a string or nil if it cannot be determined."
  (let ((system (asdf:find-system 'asdf-install)))
    (when system (asdf:component-version system))))

;; load customizations if any
(eval-when (:load-toplevel :execute)
  (let* ((*package* (find-package :asdf-install-customize))
         (file (probe-file (merge-pathnames
                            (make-pathname :name ".asdf-install")
                            (truename (user-homedir-pathname))))))
    (when file (load file))))

(provide 'asdf-install)

;;; Local variables:
;;; indent-tabs-mode:nil
;;; End:
