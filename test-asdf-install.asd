;;; -*-  Lisp -*-

(defpackage #:test-asdf-install-system 
  (:use #:cl #:asdf))

(in-package #:test-asdf-install-system)

(defsystem test-asdf-install
  :components 
  ((:module "tests"
	    :components ((:file "package")
			 (:file "utilities"
				:depends-on ("package"))
			 (:file "unit-tests"
				:depends-on ("package" "test-asdf-install"))
			 (:file "test-asdf-install"
				:depends-on ("package")))))
  :depends-on (:asdf-install 
	       :lift))

(defmethod perform ((o test-op) (c (eql (find-system :test-asdf-install))))
  t)
