(in-package cl-markdown)

;; anchors
;; command line arguments, restructure into smaller pieces

(defun md (name)
  (let ((input (dsc:system-relative-pathname 
		'asdf-install 
		(make-pathname 
		 :directory '(:relative :back "website" "source" "tutorial")
		 :name name
		 :type "md")))
	(output (system-relative-pathname 
		 'asdf-install 
		 (make-pathname 
		  :directory '(:relative :back "website" "output" "tutorial")
		  :name name
		  :type "html"))))
    (markdown input :stream output :format :html
	      :additional-extensions '(docs today now))))

(defun md-all ()
  (dolist (file
	    (directory (dsc:system-relative-pathname 
			'asdf-install 
			(make-pathname 
			 :directory '(:relative :back "website" "source" 
				      "tutorial")
			 :name :wild
			 :type "md"))))
    (when (probe-file file)
      (print file)
      (md (pathname-name file)))))

(md-all)

(md "index")

(md "foo")
(untrace)

;;;;;;;;; what about PDF?

{set-property document-heading-level 4}
{set-property document-heading-for-variable "Special variable"}
{set-property document-package asdf-install}

{document variable *gnu-tar-program*}

<div class="documentation">
< anchor too >
<h4>Special variable <code>*gnu-tar-program*</code></h4>
<div class="documentation-variable">
The path to the GNU `tar` program as a string - the default is `"tar"`. Changing this variable has no effect if Cygwin is used. 
</div>
</div>

{set-property document-heading-level 6}
{set-property document-heading-for-variable "Variable"}

<h6>Variable <code>*gnu-tar-program*</code></h6>


(markdown "
You can for example from CMUCL issue the command 
    
    
    (asdf-install:install :osicat)
    

and watch how ASDF-INSTALL not only downloads and installs [Osicat][120] but also [UFFI][121].   
   
Fun!

   [120]: http://common-lisp.net/project/osicat/
   [121]: http://uffi.b9.com/

")