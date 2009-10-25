(in-package #:common-lisp-user)

(defpackage #:trivial-shell-test
  (:use #:common-lisp #:lift #:trivial-shell))

(in-package #:trivial-shell-test)

(deftestsuite trivial-shell-test ()
  ())

(addtest (trivial-shell-test)
  test-1
  (ensure-same (parse-integer (shell-command "expr 1 + 1") :junk-allowed t) 2))

(addtest (trivial-shell-test)
  test-input
  (ensure-same (parse-integer 
		(shell-command "wc -c" :input "hello")
		:junk-allowed t) 
	       5 :test '=))


