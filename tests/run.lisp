; run tests
; 1. replace the parameter of open-store
;    http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started
; 2. compile and load this file
; 3. run any test you want

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r-tests))

(in-package :web4r-tests)

(defmacro run-test (test)
  (let ((s (make-server
            :public-dir *test-public-dir* :port 8080 :timeout-sec 3)))
    `(progn
       (setf *srv* (start-server ,s))
       (ele:open-store
        '(:clsql (:postgresql "localhost" "test" "postgres" "pgpass")))
       (unwind-protect
            (5am:run! ,test)
         (progn
           (ele:close-store)
           (stop-server *srv*))))))

; run each test
;(run-test 'get-params)

; run all tests
;(run-test 'web4r)
