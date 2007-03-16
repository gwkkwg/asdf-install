(in-package #:test-asdf-install)

(defvar *working-directory*
  (translate-logical-pathname "user-home:temporary;test-asdf-install;"))

(deftestsuite test-asdf-install ()
  ()
  (:dynamic-variables 
   (*locations* 
   `((,(merge-pathnames (make-pathname :directory '(:relative "site"))
			*working-directory*)
       ,(merge-pathnames (make-pathname :directory '(:relative "site-systems"))
			 *working-directory*)
       "temporary install")))
   (*preferred-location* "temporary install")
   (asdf:*central-registry* 
   (list 
    (merge-pathnames (make-pathname :directory '(:relative "site-systems"))
		     *working-directory*)))))

(deftestsuite test-asdf-install-basic-installation (test-asdf-install) 
  ()
  (:dynamic-variables 
   (*verify-gpg-signatures* t))
  (:setup 
   (delete-directory-and-files *working-directory* :if-does-not-exist :ignore)))

(addtest (test-asdf-install-basic-installation)
  test-non-existent-package
  (ensure-condition 'download-error
    (install 'i-hope-this-package-does-not-exist)))

(addtest (test-asdf-install-basic-installation)
  test-download-two-at-once
  (let ((handled-error-count 0))
    (handler-bind 
	((gpg-error (lambda (c) 
		      (incf handled-error-count)
		      (let ((r (find-restart 'install-anyways)))
			(when r (invoke-restart r)))
		      (error c))))
      (ensure-same (install '(lw-compat asdf-binary-locations))
		   '(lw-compat asdf-binary-locations))
      (ensure-same handled-error-count 2))))

(deftestsuite test-asdf-install-no-gpg-verification (test-asdf-install) 
  ()
  (:dynamic-variables 
   (*verify-gpg-signatures* nil))
  (:setup 
   (delete-directory-and-files *working-directory* :if-does-not-exist :ignore)))

(addtest (test-asdf-install-no-gpg-verification)
  test-non-existent-package
  (ensure-condition 'download-error
    (install 'i-hope-this-package-does-not-exist)))

(addtest (test-asdf-install-no-gpg-verification)
  test-simple-download
  (install 'lw-compat))

(addtest (test-asdf-install-no-gpg-verification)
  test-download-with-dependencies
  (install 'moptilities))

(addtest (test-asdf-install-no-gpg-verification)
  test-download-two-at-once
  (ensure-same (install '(lw-compat asdf-binary-locations))
	       '(lw-compat asdf-binary-locations)))

;;;;;

(deftestsuite test-with-tar-file (test-asdf-install)
  ())

(addtest (test-with-tar-file)
  (let ((result 
	 (install
	  (namestring (asdf:system-relative-pathname 
		       'test-asdf-install 
		       "tests/data/log5_latest.tar.gz")))))
    (ensure-same (length result) 2)
    (ensure (member 'log5 result :test 'string-equal))
    (ensure (member 'log5-test result :test 'string-equal))))

#|
(asdf-install::local-archive-p 
 (namestring (asdf:system-relative-pathname 
	      'test-asdf-install 
	      "tests/data/log5_latest.tar.gz")))

(install
 (namestring (asdf:system-relative-pathname 
	      'test-asdf-install 
	      "tests/data/log5_latest.tar.gz"))
 :where 2)


(install
 'log5
 :where 2)

(asdf-install::local-archive-p (string 'log5))

(trace asdf-install::install-package)
|#