(in-package #:test-asdf-install)

(deftestsuite test-propagate (test-asdf-install)
  ()
  (:dynamic-variables 
   (*verify-gpg-signatures* nil))
  (:run-setup :once-per-suite)
  (:setup
   (install 'moptilities)))

#+(or)
;;?? this might make sense if/when we do some sort of version checking / upgrade
(addtest (test-propagate)
  donot-download-unless-required
  (ensure-install-results-same
   '()
   (install 'moptilities)))

(addtest (test-propagate)
  propagate-gets-everything
  (ensure-install-results-same
   '(moptilities closer-mop #-lispworks #:lw-compat)
   (install 'moptilities :propagate t)))
