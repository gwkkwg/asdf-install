(in-package #:test-asdf-install)

(defun ensure-install-results-same (expected actual)
  "Ensure that the list of systems installed is equal to the expected
  list. Order does not matter."
  (ensure-null (set-difference expected actual :test 'string-equal))
  (ensure-null (set-difference actual expected :test 'string-equal)))

(defvar *working-directory*
  (asdf:system-relative-pathname 
   'test-asdf-install 
   "scratch/"))

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
   (asdf-install::*private-asdf-install-dirs* "")
   ;; HACK: We bind this unexported variable in order to fool ASDF
   ;; into thinking that nothing is installed. This isolates the tests
   ;; from the environment, and from each other.
   (asdf::*defined-systems*
    (make-hash-table :test (hash-table-test asdf::*defined-systems*)))
   (asdf:*central-registry* 
    (list 
     (merge-pathnames (make-pathname :directory '(:relative "site-systems"))
                      *working-directory*))))
  (:setup
   (delete-directory-and-files *working-directory* :verbose? t 
                                                   :if-does-not-exist :ignore)
   (ensure-directories-exist *working-directory*))
  (:timeout 35))

(deftestsuite test-asdf-install-basic-installation (test-asdf-install) 
  ()
  (:dynamic-variables 
   (*verify-gpg-signatures* t)))
   
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
             (ensure-install-results-same
              '(lw-compat cl-fad)
              (install '(lw-compat cl-fad)))
             (ensure-same handled-error-count 2))))

(deftestsuite test-asdf-install-no-gpg-verification (test-asdf-install) 
  ()
  (:dynamic-variables 
   (*verify-gpg-signatures* nil)))

(addtest (test-asdf-install-no-gpg-verification)
         test-non-existent-package
         (ensure-condition 'download-error
                           (install 'i-hope-this-package-does-not-exist)))

(addtest (test-asdf-install-no-gpg-verification)
         test-simple-download
         (ensure-install-results-same
          '(lw-compat)
          (install 'lw-compat)))

(addtest (test-asdf-install-no-gpg-verification)
         test-download-with-dependencies
         (ensure-install-results-same
          '(moptilities moptilities-test)
          (install 'moptilities)))

(addtest (test-asdf-install-no-gpg-verification)
         test-download-two-at-once
         (ensure-install-results-same 
          '("lw-compat" "asdf-binary-locations")
          (install '(lw-compat asdf-binary-locations))))

;;;;;

(deftestsuite space-in-working-directory (test-asdf-install)
  ()
  ;; ugh -- compare with test-asdf-install, basically the same code...
  (:dynamic-variables 
   (*working-directory* (asdf:system-relative-pathname 
                         'test-asdf-install 
                         "two words/"))
   (*locations* 
    `((,(merge-pathnames (make-pathname :directory '(:relative "site"))
                         *working-directory*)
        ,(merge-pathnames (make-pathname :directory '(:relative "site-systems"))
                          *working-directory*)
        "temporary install")))
   (*preferred-location* "temporary install")
   (*verify-gpg-signatures* nil)
   (asdf:*central-registry* 
    (list 
     (merge-pathnames (make-pathname :directory '(:relative "site-systems"))
                      *working-directory*))))
  (:setup 
   (delete-directory-and-files *working-directory* 
                               :if-does-not-exist :ignore)))


(addtest (space-in-working-directory)
         test-1
         (ensure-install-results-same
          '(moptilities moptilities-test closer-mop)
          (install 'moptilities)))

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

/Users/gwking/darcs/asdf-install-unstable/tests/data/log5.tar.gz

(asdf-install::local-archive-p "http://common-lisp.net/project/cl-containers/asdf-binary-locations/asdf-binary-locations_latest.tar.gz")

(asdf-install::handle-download-package "http://common-lisp.net/project/cl-containers/asdf-binary-locations/asdf-binary-locations_latest.tar.gz")


(Trace asdf-install::install-package)
|#
