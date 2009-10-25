 (in-package #:common-lisp-user)

(defpackage #:lift-documentation-system
  (:use #:common-lisp #:asdf))
(in-package #:lift-documentation-system)

;; just ignore for now... sigh.
(defsystem lift-documentation
  :author "Gary King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Documentation for LIFT"
  :components (
	       #+(or)
		 (:module "setup"
			:pathname "docs/" 
			:components ((:file "package")
				     (:file "setup" 
				      :depends-on ("package"))))
	       #+(or)
	       (:module 
		"docs"
		:depends-on ("setup")
		:pathname "website/source/"
		:components
		((:docudown-source "index.md")
		 (:docudown-source "user-guide.md"))))
  :depends-on (:lift #+(or) :docudown))
