(in-package #:test-asdf-install)

(deftestsuite test-install-location (test-asdf-install) ())

(addtest (test-install-location)
  (let ((*preferred-location* -34))
    (ensure-error (asdf-install::install-location))))

(addtest (test-install-location)
  (let ((*preferred-location* 'hello))
    (ensure-error (asdf-install::install-location))))

(addtest (test-install-location)
  (let ((*preferred-location* 'home)
	(*locations* 
	 `((a a home)
	   (b b away)
	   (c c far))))
    (ensure-same (asdf-install::install-location) '(a a home) :test #'equal)))

(addtest (test-install-location)
  (let ((*preferred-location* 'away)
	(*locations* 
	 `((a a home)
	   (b b away)
	   (c c far))))
    (ensure-same (asdf-install::install-location) '(b b away) :test #'equal)))

(addtest (test-install-location)
  (let ((*preferred-location* 3)
	(*locations* 
	 `((a a home)
	   (b b away)
	   (c c far))))
    (ensure-same (asdf-install::install-location) '(c c far) :test #'equal)))

(addtest (test-install-location)
  (let ((*preferred-location* 'far)
	(*locations* 
	 `((a a home)
	   (b b away)
	   (c c far))))
    (ensure-same (asdf-install::install-location) '(c c far) :test #'equal)))
