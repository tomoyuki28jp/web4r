(in-package #:lift-test)

(deftestsuite test-config-files (lift-test) ())

(addtest (test-config-files)
  unknown-test-suite
  (let ((r 
	 (lift:run-tests 
	  :config (asdf:system-relative-pathname 
		   'lift-test "test/bad-config-1.config"))))
    (ensure-same (length (lift::suites-run r)) 2)))

(addtest (test-config-files)
  unknown-config-option
  (let ((r 
	 (lift:run-tests 
	  :config (asdf:system-relative-pathname 
		   'lift-test "test/bad-config-2.config"))))
    (ensure-same (length (lift::suites-run r)) 2)))
  
