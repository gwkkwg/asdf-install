#|
install non-existence package

install package with no dependencies

install package with several dependencies

make sure only the requested packages are installed

test with / without verification

test trusted uids

test proxy

|#

(in-package #:common-lisp-user)
(defpackage #:test-asdf-install
  (:use #:common-lisp #:asdf-install #:lift #:cl-fad))
(in-package #:test-asdf-install)

(defvar *working-directory*
  (translate-logical-pathname "user-home:temporary;test-asdf-install;"))

(deftestsuite test-asdf-install () 
  ((old-locations *locations*)
   (old-prefered *preferred-location* )
   (old-registry asdf:*central-registry*))
  (:setup (setf *locations*
                `((,(merge-pathnames (make-pathname :directory '(:relative "site"))
                                     *working-directory*)
                   ,(merge-pathnames (make-pathname :directory '(:relative "site-systems"))
                                     *working-directory*)
                   "temporary install"))
                *preferred-location* 0
                asdf:*central-registry* 
                (list 
                 (merge-pathnames (make-pathname :directory '(:relative "site-systems"))
                                  *working-directory*))))
  (:teardown (setf *locations* old-locations
                   *preferred-location* old-prefered
                   asdf:*central-registry* old-registry)))

(deftestsuite test-asdf-install-no-verify (test-asdf-install) 
  ((old-verify *verify-gpg-signatures*))
  (:setup (setf *verify-gpg-signatures* nil)
          (delete-directory-and-files *working-directory* :if-does-not-exist :ignore))
  (:teardown (setf *verify-gpg-signatures* old-verify)))

(addtest (test-asdf-install-no-verify)
  test-non-existent-package
  (ensure-condition 'download-error
    (install 'i-hope-this-package-does-not-exist)))

(addtest (test-asdf-install-no-verify)
  test-simple-download
  (install 'lw-compat))

(addtest (test-asdf-install-no-verify)
  test-download-with-dependencies
  (install 'moptilities))