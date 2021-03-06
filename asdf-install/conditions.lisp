(in-package #:asdf-install)

(define-condition http-transfer-error (error)
   ((url :initarg :url :reader download-url)))

(define-condition download-error (http-transfer-error)
  ((response :initarg :response :reader download-response))
  (:report (lambda (c s)
             (format s "Server responded ~A for GET ~A"
                     (download-response c) (download-url c)))))

(define-condition content-length-missing (http-transfer-error)
  ()
  (:report (lambda (c s)
	     (format s "No content-length header, expected in transfer from ~A"
		     (download-url c)))))

(define-condition content-length-parse-error (http-transfer-error)
  ((header-text :initarg :header-text :reader header-text))
  (:report (lambda (c s)
	     (format s "Unable to parse content-length header in transfer from ~A: header-text: ~A"
		     (download-url c) (header-text c)))))

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
             (let* ((*print-circle* nil)
                    (key-id (key-id c))
                    (key-id (if (and (consp key-id) 
                                     (> (length key-id) 1))
                                (car key-id) key-id)))
               (format s "~&No key found for key id 0x~A.~%" key-id)
               (format s "~&Try some command like ~%  gpg  --recv-keys 0x~A"
                       (format nil "~a" key-id))))))

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
             (let ((*asdf-install-message-stream* s))
               (installer-msg 30 "Installation aborted.")))))

(defun report-valid-preferred-locations (stream &optional attempted-location)
  (when attempted-location
    (let ((*asdf-install-message-stream* stream))
      (installer-msg 30 "~s is not a valid value for *preferred-location*"
                     attempted-location))
    (installer-msg 30 "*preferred-location* may either be nil, a number between 1 and ~d \(the length of *locations*\) or the name of one of the *locations* \(~{~s~^, ~}\). If using a name, then it can be a symbol tested with #'eq or a string tested with #'string-equal."
                   (length *locations*)
                   (mapcar #'third *locations*))))

(define-condition invalid-preferred-location-error (error)
  ((preferred-location :initarg :preferred-location))
  (:report (lambda (c s)
             (report-valid-preferred-locations 
              s (slot-value c 'preferred-location)))))

(define-condition invalid-preferred-location-number-error 
    (invalid-preferred-location-error) ())

(define-condition invalid-preferred-location-name-error 
    (invalid-preferred-location-error) ())

(define-condition need-more-packages ()
  ((defsystem :type symbol
              :initarg :defsystem
              :reader defsystem)
   (failed-component :initarg :failed-component
                     :reader failed-component)
   (packages-needed :type list
                    :initarg :packages-needed
                    :reader packages-needed))
  (:documentation
   "This condition can occur when loading system, due to an inability
 to load prerequisite subsystems. Accessor PACKAGES returns a list of
 designators for additional packages that need to be installed before
 the system can be successfully loaded. Accessor FAILED-COMPONEENT
 returns a designator for the component that failed to load.  The
 latter may or may not be the immediately dependent component; use it
 only for diagnostic purposes. Accessor DEFSYSTEM indicates the
 defsystem that encountered the error."))

;;; Local variables:
;;; indent-tabs-mode:nil
;;; End:
