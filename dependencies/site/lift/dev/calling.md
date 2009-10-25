# who calls what / who does what

run-tests
  run-tests-internal
  or run-tests-from-file

run-test 
  do-testing with run-test-internal

------

run-tests-from-file
  run-tests

run-tests-internal
  make-testsuite
  do-testing with testsuite-run

do-testing (suite)
  testsuite-setup *
  foreach prototype
    initialize-test
    <fn> (= testsuite-run)
  testsuite-teardown *

run-test-internal
  start-test - push, name, value onto test-placeholder *
  setup-test *
    (initialize slots)
  lift-test *
  teardown-test *
  end-test - setf :end-time *
  (add test-data to tests-run of result)

testsuite-run
  foreach method in suite, run-test-internal
  if children, foreach direct-subclass, run-tests-internal


# Stuff

in start-test (result test name)
   (push `(,name ,(current-values test)) (tests-run result))

if fails / errors, will get problem appended 

current-values comes from prototype stuff

use property-list format 
  start-time
  end-time
  time
  space??

:created
:testsuite-setup
:testing
