#|

Author: Gary King

Code forked from Kevin Rosenberg's KMRCL and borrowed from
Alexander Repenning's Apple event code.
|#

(defpackage :trivial-shell-system (:use #:cl #:asdf))
(in-package :trivial-shell-system)

(defsystem trivial-shell
  :version "0.1.5"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "OS and Implementation independent access to the shell"
  :components ((:module 
		"dev"
		:components 
		((:static-file "notes.text")
				     
		 (:file "package")
		 (:file "definitions"
			:depends-on ("package"))
		 (:file "macros"
			:depends-on ("package"))
		 (:file "shell"
			:depends-on ("definitions" "macros" #+digitool "mcl"))

		 #+allegro
		 (:file "allegro" :depends-on ("shell"))
		 #+clisp
		 (:file "clisp" :depends-on ("shell"))
		 #+cmu
		 (:file "cmucl" :depends-on ("shell"))
		 #+digitool
		 (:file "digitool" :depends-on ("shell"))
		 #+lispworks
		 (:file "lispworks" :depends-on ("shell"))
		 #+openmcl
		 (:file "openmcl" :depends-on ("shell"))
		 #+sbcl
		 (:file "sbcl" :depends-on ("shell"))

		 #-(or allegro clisp cmu digitool lispworks openmcl sbcl)
		 (:file "unsupported")
                                     
		 #+digitool
		 (:module "mcl"
			  :components ((:file "eval-apple-script")))))
               (:module 
		"website"
		:components
		((:module "source"
			  :components ((:static-file "index.lml"))))))
  :in-order-to ((test-op (load-op trivial-shell-test)))
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic))
  :depends-on ())

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'trivial-shell))))
  (values nil))


