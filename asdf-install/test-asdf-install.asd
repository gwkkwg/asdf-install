;;; -*-  Lisp -*-

(defpackage #:test-asdf-install-system 
  (:use #:cl #:asdf))

(in-package #:test-asdf-install-system)

(defsystem test-asdf-install
  :components 
  ((:module 
    "setup"
    :pathname "../tests/"
    :components ((:file "package")))
   (:module
    "tests"
    :pathname "../tests/"
    :depends-on ("setup")
    :components ((:file "utilities")
		 (:file "unit-tests"
			:depends-on ("test-asdf-install"))
		 (:file "test-asdf-install")
		 (:file "test-propagate"
			:depends-on ("test-asdf-install")))))
  :depends-on (:asdf-install 
	       :lift))

(defmethod perform ((o test-op) (c (eql (find-system :test-asdf-install))))
  t)
