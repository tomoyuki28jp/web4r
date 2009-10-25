(in-package #:lift-test)

;;;;;;;;;;;;;;;;;;

(deftestsuite setup-and-slots-hierarchy-parent ()
  ((slot-parent (progn (push :slot-parent *test-scratchpad*) :a)))
  :setup (push :setup-parent *test-scratchpad*)
  :teardown (push :teardown-parent *test-scratchpad*))

(deftestsuite setups-and-slots-hierarchy-child
    (setup-and-slots-hierarchy-parent)
  ((slot-child (progn (push :slot-child *test-scratchpad*) :a)))
  :setup (push :setup-child *test-scratchpad*)
  :teardown (push :teardown-child *test-scratchpad*))



;;;;;;;;;;;;;;;;;;;;;

(run-test :break-on-errors? t)

(deftestsuite warnings-and-errors ()
  ())

(defun warnings-and-errors-function (mode)
  (ecase mode
    (:warn (warn "this is a warning all by itself"))
    (:error (error "this is an error all by itself"))
    (:warn-error (warn "first we warn") (error "then we error"))
    (:error-warn (error "first we error") (warn "then we warn"))))

(addtest (warnings-and-errors)
  warning-does-not-hide-error-1
  (ensure-error (warnings-and-errors-function :warn-error)))

(addtest (warnings-and-errors)
  warning-does-not-hide-error-2
  (ensure-warning (warnings-and-errors-function :warn-error)))

(addtest (warnings-and-errors)
  just-error
  ;; the error is eaten even when you run the test with break-on-errors? t
  (ensure-error (warnings-and-errors-function :error)))

(addtest (warnings-and-errors)
  just-error
  ;; the error is eaten even when you run the test with break-on-errors? t
  (warnings-and-errors-function :error))

;;;;;;


(deftestsuite tests-20081021b ()
  ((foo "123")
   (bar "456"))
  (:setup
   (print (list :te-a lift::*test-environment*))
   (print (list foo bar))))

(addtest (tests-20081021b)
  test-1
  (print foo))
