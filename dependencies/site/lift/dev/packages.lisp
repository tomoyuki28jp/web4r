(in-package #:common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:lift)
    (defpackage #:lift
      (:use #:common-lisp #:com.metabang.trivial-timeout)
      (:import-from		     
       #+allegro #:mop
       #+clisp #:clos
       #+lispworks #:clos
       #+(or mcl ccl) #:ccl
       #+cmu #:clos-mop
       #+sbcl #:sb-mop
       #+scl #:clos
       #:class-direct-subclasses
       #:class-direct-superclasses
       #:class-precedence-list)
      (:export
       #:*lift-report-detail-hook*
       #:*lift-report-header-hook*
       #:*lift-report-footer-hook*
       #:describe-test-result
       #:with-timeout

       #:testsuite-ambiguous
       #:testsuite-not-defined)
      (:export
       #:test-mixin
       #:test-result
       #:testsuite-p
       #:*test-result*
       #:*current-test*
       #:last-test-status
       #:suite-tested-p
       #:failures
       #:expected-failures
       #:errors
       #:expected-errors
       #:ensure-cases
       #:ensure-random-cases
       #:deftestsuite
       #:addtest
       #:remove-test
       #:run-test
       #:run-tests

       #:defmeasure
       #:undefmeasure
       #:measure-space
       #:measure-seconds
       #:while-measuring

       #:measure-time
       #:measure-conses

       #:with-profile-report
       #:write-profile-information
       #:profiling-threshold*
       #:*benchmark-log-path*
       #:count-repetitions
       #:while-counting-repetitions
       #:while-counting-events

       ;; Variables
       #:*test-ignore-warnings?*
       #:*test-break-on-errors?*
       #:*test-break-on-failures?*
       #:*test-print-length*
       #:*test-print-level*
       #:*test-print-when-defined?*
       #:*test-evaluate-when-defined?*
       #:*test-describe-if-not-successful?*
       #:*test-maximum-time*
       #:*test-print-testsuite-names*
       #:*test-print-test-case-names*
       #:*test-maximum-error-count*
       #:*test-maximum-failure-count*
       #:*lift-dribble-pathname*
       #:*lift-report-pathname*
       #:*current-asdf-system-name*
       #:*test-scratchpad*
       #:*test-notepad*
       #:*lift-equality-test*
       #:*lift-debug-output*
       #:*test-show-expected-p*
       #:*test-show-details-p*
       #:*test-show-code-p*
          
       ;; Other
       #:ensure
       #:ensure-null
       #:ensure-same
       #:ensure-different
       #:ensure-condition
       #:ensure-warning
       #:ensure-error
       #:ensure-no-warning
          
       ;;?? Not yet
       ;; with-test
          
       #:list-tests
       #:print-tests
       #:map-testsuites
       #:testsuites
       #:testsuite-tests
	    
       #:suite
       #:find-testsuite
       #:find-test-case
       #:ensure-random-cases-failure
       #:random-instance-for-suite
       #:defrandom-instance
       #:ensure-random-cases
       #:ensure-random-cases+
       #:random-element
       #:random-number
       #:an-integer
       #:a-double-float
       #:a-single-float
       #:a-symbol

       #:lift-result
       #:lift-property
       #:liftpropos

       #:handle-config-preference
       ))))

(unless (and (find-package :asdf)
	     (find-symbol (symbol-name 'system-relative-pathname) :asdf)
	     (fboundp (find-symbol
		       (symbol-name 'system-relative-pathname) :asdf)))
  (warn "LIFT uses asdf:system-relative-pathname which your version of ASDF 
doesn't seem to include. LIFT will define these for now but you may want to consider updating to the most recent version of ASDF (see http://www.cliki.net/asdf for details).")
  (intern (symbol-name 'system-source-file) :asdf)
  (intern (symbol-name 'system-source-directory) :asdf)
  (intern (symbol-name 'system-relative-pathname) :asdf)
  (export 'asdf::system-relative-pathname :asdf) 
  (defun asdf::system-source-file (system-name)
    (let ((system (asdf:find-system system-name)))
      (make-pathname 
       :type "asd"
       :name (asdf:component-name system)
       :defaults (asdf:component-relative-pathname system))))

  (defun asdf::system-source-directory (system-name)
    (make-pathname :name nil
		   :type nil
		   :defaults (asdf::system-source-file system-name)))

  (defun asdf::system-relative-pathname (system pathname &key name type)
    (let ((directory (pathname-directory pathname)))
      (when (eq (car directory) :absolute)
	(setf (car directory) :relative))
      (merge-pathnames
       (make-pathname :name (or name (pathname-name pathname))
		      :type (or type (pathname-type pathname))
		      :directory directory)
       (asdf::system-source-directory system)))))
  