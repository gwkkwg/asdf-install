(in-package asdf-install)

(define-condition download-error (error)
  ((url :initarg :url :reader download-url)
   (response :initarg :response :reader download-response))
  (:report (lambda (c s)
	     (format s "Server responded ~A for GET ~A"
		     (download-response c) (download-url c)))))

(define-condition signature-error (error)
  ((cause :initarg :cause :reader signature-error-cause))
  (:report (lambda (c s)
	     (format s "Cannot verify package signature:  ~A"
		     (signature-error-cause c)))))

(define-condition gpg-error (error)
  ((message :initarg :message :reader gpg-error-message))
  (:report (lambda (c s)
	     (format s "GPG failed with error status:~%~S"
		     (gpg-error-message c)))))

(define-condition gpg-shell-error (gpg-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Call to GPG failed. Perhaps GPG is not installed or not ~
in the path."))))

(define-condition no-signature (gpg-error) ())

(define-condition key-not-found (gpg-error)
  ((key-id :initarg :key-id :reader key-id))
  (:report (lambda (c s)
	     (format s "No key found for key id 0x~A. ~
                        Try some command like ~%  gpg  --recv-keys 0x~A"
		     (key-id c) (key-id c)))))

(define-condition key-not-trusted (gpg-error)
  ((key-id :initarg :key-id :reader key-id)
   (key-user-name :initarg :key-user-name :reader key-user-name))
  (:report (lambda (c s)
	     (format s "GPG warns that the key id 0x~A (~A) is not fully trusted"
		     (key-id c) (key-user-name c)))))

(define-condition author-not-trusted (gpg-error)
  ((key-id :initarg :key-id :reader key-id)
   (key-user-name :initarg :key-user-name :reader key-user-name))
  (:report (lambda (c s)
	     (format s "~A (key id ~A) is not on your package supplier list"
		     (key-user-name c) (key-id c)))))
  
(define-condition installation-abort (condition)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (installer-msg s "Installation aborted."))))
