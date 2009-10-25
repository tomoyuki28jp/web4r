(in-package #:common-lisp-user)

(defpackage #:lift-test 
  (:use #:common-lisp #:lift)
  (:import-from #:lift
                #:failures
                #:errors
                #:tests-run
                #:test-mode
                #:test-interactive?
                #:make-test-result
                #:testsuite-test-count
		#:*test-environment*
		#:*test-maximum-error-count*
		#:*test-maximum-failure-count*
		#:failures
		#:errors))
