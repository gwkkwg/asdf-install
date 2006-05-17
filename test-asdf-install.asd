;;; -*-  Lisp -*-

(defpackage #:test-asdf-install-system 
  (:use #:cl #:asdf))

(in-package #:test-asdf-install-system)

(defsystem test-asdf-install
  :version "0.5.2"
  
  :components ((:module "tests"
                        :components ((:file "test-asdf-install"))))
  :depends-on (asdf-install cl-fad lift))

(defmethod perform ((o test-op) (c (eql (find-system :test-asdf-install))))
  t)
