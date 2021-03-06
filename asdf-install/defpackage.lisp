(cl:in-package :cl-user)

(defpackage #:asdf-install
  (:use #:common-lisp)
  
  (:export

   ;; Customizable variables.
   #:*shell-path*                       ; TODO: Undocumented
   #:*proxy*
   #:*cclan-mirror*
   #:asdf-install-dirs                  ; TODO: Undocumented
   #:private-asdf-install-dirs          ; TODO: Undocumented
   #:*tar-extractor*                    ; TODO: Undocumented

   #:*program-directories*              ; TODO: Undocumented
   #:*verify-gpg-signatures*
   #:*locations*
   #:*safe-url-prefixes*
   #:*preferred-location*
   #:*temporary-directory*              ; TODO: Undocumented
   #:*system-file-installer*
   
   ;; External entry points.   
   #:add-locations                      ; TODO: Undocumented
   #:add-registry-location              ; TODO: Undocumented
   #:uninstall
   #:install
   #:asdf-install-version               ; TODO: Undocumented

   #+(and asdf (or :win32 :mswindows))
   #:sysdef-source-dir-search
   
   ;; proxy authentication
   #:*proxy-user*
   #:*proxy-passwd*
   
   ;; conditions
   #:download-error
   #:signature-error                    ; TODO: Undocumented
   #:gpg-error                          ; TODO: Undocumented
   #:gpg-shell-error                    ; TODO: Undocumented
   #:key-not-found
   #:key-not-trusted
   #:author-not-trusted
   #:installation-abort                 ; TODO: Undocumented

   ;; restarts
   #:install-anyways                    ; TODO: Undocumented
   ))
  
(defpackage #:asdf-install-customize
  (:use #:common-lisp #:asdf-install)
  (:documentation "Used internally for loading user customization file."))

;;; Local variables:
;;; indent-tabs-mode:nil
;;; End:
