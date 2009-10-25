(in-package #:lift-test)

(deftestsuite test-maximum-problems (lift-test)
  ()
  (:function 
   (do-it (name)
     ;; ignore cancel-testing-from-configuration into cancel-testing
     (restart-case
	 (run-tests :suite name :report-pathname nil)
       (lift::cancel-testing-from-configuration ()
	 :test (lambda (c) (declare (ignore c)) t)
	 nil
	 #+(or)
	 (invoke-restart (find-restart 'lift::cancel-testing)))))))

(addtest (test-maximum-problems) 
  all-nil
  (let* ((*test-maximum-error-count* nil)
	 (*test-maximum-failure-count* nil)
	 (r (do-it 'test-maximum-problems-helper)))
    (ensure-same (length (tests-run r)) 12)
    (ensure-same (length (errors r)) 6)
    (ensure-same (length (failures r)) 6)))

(addtest (test-maximum-problems) 
  max-3-failures
  (let* ((*test-maximum-error-count* nil)
	 (*test-maximum-failure-count* 3)
	 (r (do-it 'test-maximum-problems-helper)))
    (cond ((= (length (tests-run r)) 3)
	   (ensure-same (length (failures r)) 3))
	  ((= (length (tests-run r)) 9)
	   (ensure-same (length (errors r)) 6)
	   (ensure-same (length (failures r)) 3))
	  (t
	   (ensure-null t :report "Wrong number of tests run: ~d."
			:arguments ((length (tests-run r))))))))

(addtest (test-maximum-problems) 
  max-2-errors
  (let* ((*test-maximum-error-count* 2)
	 (*test-maximum-failure-count* nil)
	 (r (do-it 'test-maximum-problems-helper)))
    (cond ((= (length (tests-run r)) 2)
	   (ensure-same (length (errors r)) 2))
	  ((= (length (tests-run r)) 8)
	   (ensure-same (length (errors r)) 2)
	   (ensure-same (length (failures r)) 6))
	  (t
	   (ensure-null t :report "Wrong number of tests run: ~d."
			:arguments ((length (tests-run r))))))))

(deftestsuite test-maximum-problems-helper ()
  ())

(deftestsuite test-maximum-problems-helper-failures 
    (test-maximum-problems-helper)
  ()
  (:test (a (ensure-same 1 2)))
  (:test (b (ensure-same 1 2)))
  (:test (c (ensure-same 1 2)))
  (:test (d (ensure-same 1 2)))
  (:test (e (ensure-same 1 2)))
  (:test (f (ensure-same 1 2))))

(deftestsuite test-maximum-problems-helper-errors 
    (test-maximum-problems-helper)
  ()
  (:test (a (error "no good")))
  (:test (b (error "no good")))
  (:test (c (error "no good")))
  (:test (d (error "no good")))
  (:test (e (error "no good")))
  (:test (f (error "no good"))))
