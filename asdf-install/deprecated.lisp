(in-package asdf-install)

#+(and ignore sbcl) ; Deprecated.
(define-symbol-macro *sbcl-home* *asdf-install-dirs*)

#+(and ignore sbcl) ; Deprecated.
(define-symbol-macro *dot-sbcl* *private-asdf-install-dirs*)

;; uncalled
(defun read-until-eof (stream)
  (with-output-to-string (o)
    (copy-stream stream o)))
