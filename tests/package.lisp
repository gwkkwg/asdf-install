(in-package #:common-lisp-user)

(defpackage #:test-asdf-install
  (:use #:common-lisp #:asdf-install #:lift 
	#+(or) #:cl-fad))

