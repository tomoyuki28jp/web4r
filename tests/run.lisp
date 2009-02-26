; run tests
; 1. replace the parameter of open-store
;    http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started
; 2. compile and load this file
; 3. run any test you want

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r-tests))

(in-package :web4r-tests)

(defpclass testdb1 ()
    ((name         :length 50 :label "Full Name" :size 30)
     (password     :input :password :length (8 12) :hide t)
     (email        :type :email :unique t)
     (sex          :input :radio :options ("Male" "Female"))
     (marriage     :input :select :options ("single" "married" "divorced"))
     (hobbies      :input :checkbox :options ("sports" "music" "reading"))
     (birth-date   :type :date)
     (nickname     :length 50 :nullable t)
     (phone-number :type (:regex "^\\d{3}-\\d{3}-\\d{4}$"))
     (zip-code     :type :integer :length 5)
     (note         :length 3000 :rows 5 :cols 30)
     (image        :input :file :type :image :length (1000 500000) :nullable t)))

(defmacro run-test (test)
  `(progn
     (setf *srv* (start-server (make-server :public-dir *test-public-dir*
                                            :port 8080 :timeout-sec 3)))
     (ele:open-store
      '(:clsql (:postgresql "localhost" "test" "postgres" "pgpass")))
     (unwind-protect
          (5am:run! ,test)
       (progn
         (web4r::drop-class-instances 'testdb1)
         (ele:close-store)
         (stop-server *srv*)))))

; run each test
;(run-test 'form-input)

; run all tests
;(run-test 'web4r)
