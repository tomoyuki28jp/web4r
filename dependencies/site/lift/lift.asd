(defpackage #:lift-system (:use #:common-lisp #:asdf))
(in-package #:lift-system)

(defsystem lift
  :version "1.7.0"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License; see file COPYING for details"
  :description "LIsp Framework for Testing"
  :long-description "LIFT is an SUnit variant and much much more."  
  :components ((:module
		"timeout"
		:pathname "timeout/"
		:components 
		((:file "package")
		 (:file "with-timeout" :depends-on ("package"))))
	       (:module 
		"setup"
		:pathname "dev/"
		:depends-on ("timeout")
		:components
		((:file "packages")
		 (:file "utilities" 
			:depends-on ("packages"))
		 (:file "macros"
			:depends-on ("packages"))))
	       (:module
		"dev" 
		:depends-on ("setup")
		:components 
		((:static-file "notes.text")
		 (:file "lift"
			:depends-on ("measuring" "port"))
		 (:file "copy-file"
			:depends-on ())
		 (:file "random-testing" 
			:depends-on ("lift"))
		 (:file "port" 
			:depends-on ())
		 (:file "measuring" 
			:depends-on ("port"))
		 (:file "config" 
			:depends-on ("port" "lift"))
		 (:file "reports" 
			:depends-on ("port" "lift"))
		 (:file "introspection" 
			:depends-on ("lift"))
		  #+Ignore
		 (:file "prototypes"
			:depends-on ("lift"))))
               
	       #+(or)
               (:module 
		"website"
		:components ((:module "source"
				      :components 
				      ((:static-file "index.md"))))))
  
  :in-order-to ((test-op (load-op lift-test)))
  :depends-on ()
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic)))

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'lift))))
  (values nil))

(when (find-system 'asdf-system-connections nil)
  (asdf:operate 'asdf:load-op 'asdf-system-connections))

#+asdf-system-connections
(asdf:defsystem-connection lift-report-locations
  :requires (:lift :asdf-binary-locations)
  :components ((:module "dev"
			:components ((:file "report-locations")))))
