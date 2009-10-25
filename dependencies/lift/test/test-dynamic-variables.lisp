(in-package #:lift-test)

(defvar *a* 1)

(deftestsuite test-dynamic-variables (lift-test)
  ())

(deftestsuite test-dynamic-variables-1 (test-dynamic-variables)
  ()
  (:dynamic-variables (*a* 2))
  (:test (test-1 (ensure-same *a* 2))))

(deftestsuite test-dynamic-variables-2 (test-dynamic-variables)
  ()
  (:test (test-1 (ensure-same *a* 1)))
  (:test (test-2 (ensure *a*))))

;;;; syntax

(deftestsuite test-dynamic-variables-syntax-1 (test-dynamic-variables)
  ()
  (:dynamic-variables (*a* 0) (*b* 1))
  (:test (test-1 (ensure-same (list :dv *a* *b*) '(:dv 0 1))))
  )

(deftestsuite test-dynamic-variables-syntax-2 (test-dynamic-variables)
  ()
  (:dynamic-variables *a* *b*)
  (:test (test-1 (ensure-same (list :dv *a* *b*) '(:dv nil nil))))
  )

;;;;

(deftestsuite dynamic-variables-helper-1 ()
  ()
  (:documentation "It's important that the dynamic variable is not
marked as special in the global environment.")
  (:dynamic-variables *unknown-dynamic-variable*))

(deftestsuite dynamic-variables-helper-2 (dynamic-variables-helper-1)
  ((my-slot *unknown-dynamic-variable*)))

(addtest (dynamic-variables-helper-2)
  try-it
  (ensure (+ 1 2)))

(deftestsuite test-non-special-dynamic-variables (test-dynamic-variables)
  ())

(addtest (test-non-special-dynamic-variables)
  try-it
  (let ((r (run-tests :suite 'dynamic-variables-helper-2)))
    (ensure-same (length (tests-run r)) 1)
    (ensure-null (lift::errors r))
    (ensure-null (lift::failures r))))


