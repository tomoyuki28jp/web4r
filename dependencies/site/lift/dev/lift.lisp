;;;-*- Mode: Lisp; Package: lift -*-

(in-package #:lift)

;;; ---------------------------------------------------------------------------
;;; global environment thingies
;;; ---------------------------------------------------------------------------

(defparameter *make-testsuite-arguments*
  '(:run-setup :test-slot-names :equality-test :log-file :timeout
    :default-initargs :profile :expected-failure :expected-error))

(defvar *current-testsuite-name* nil)
(defvar *current-test-case-name* nil)

(defvar *test-is-being-defined?* nil)
(defvar *test-is-being-compiled?* nil)
(defvar *test-is-being-loaded?* nil)
(defvar *test-is-being-executed?* nil)

(defvar *test-maximum-error-count* nil
  "The maximum numbers of errors to allow during a [run-tests][].

If `*test-maximum-error-count*` is nil, then a call to run-tests 
will continue regardless of the number of errors. If it a positive
integer, then run-tests will stop as soon as the number of test-errors
if greater than or equal to its value. Setting this to some small 
value can help prevent running lengthly test-suites when there are many
errors. See also [\\*test-maximum-failure-count\\*][].")

(defvar *test-maximum-failure-count* nil
  "The maximum numbers of failures to allow during a [run-tests][].

If `*test-maximum-failure-count*` is nil, then a call to run-tests 
will continue regardless of the number of failures. If it a positive
integer, then run-tests will stop as soon as the number of test-failures
if greater than or equal to its value. Setting this to some small 
value can help prevent running lengthly test-suites when there are many
failures. See also [\\*test-maximum-error-count\\*][].")

(defvar *lift-debug-output* *debug-io*
  "Messages from LIFT will be sent to this stream. It can set to nil or 
to an output stream. It defaults to *debug-io*.")

(defvar *test-maximum-time* 2
  "Maximum number of seconds a process test is allowed to run before we give up.")

(defvar *test-break-on-errors?* nil)
(defvar *test-break-on-failures?* nil)
(defvar *test-do-children?* t)
(defparameter *test-ignore-warnings?* nil
  "If true, LIFT will not cause a test to fail if a warning occurs while
the test is running. Note that this may interact oddly with ensure-warning.")
(defparameter *test-print-when-defined?* nil)
(defparameter *test-evaluate-when-defined?* t)
(defparameter *test-scratchpad* nil
  "A place to put things. This is set to nil before every test.")
(defparameter *test-notepad* nil
  "Another place to put things \(see {ref *test-scratchpad*}\).")

(defparameter *lift-equality-test* 'equal
  "The function used in ensure-same to test if two things are equal. If metatilities is loaded, then you might want to use samep.")

(defvar *test-describe-if-not-successful?* nil
  ;; Was t, but this behavior was extremely annoying since each
  ;; time a test-restul appears in a stack backtrace it is printed
  ;; over many unstructured lines.
  "If true, then a complete test description is printed when there are any test warnings or failures. Otherwise, one would need to explicity call describe.")

(defvar *test-print-length* :follow-print
  "The print-length in effect when LIFT prints test results. It works exactly like `*print-length*` except that it can also take on the value :follow-print. In this case, it will be set to the value of  `*print-length*`.")
(defvar *test-print-level* :follow-print
  "The print-level in effect when LIFT prints test results. It works exactly like `*print-level*` except that it can also take on the value :follow-print. In this case, it will be set to whatever `*print-level*` is.")

(defvar *test-print-testsuite-names* t
  "If true, LIFT will print the name of each test suite to *debug-io* before it begins to run the suite. See also: *test-print-test-case-names*.")

(defvar *test-print-test-case-names* nil
  "If true, LIFT will print the name of each test-case before it runs. See also: *test-print-testsuite-names*.")

(defparameter *lift-tests-to-skip* nil
  "A lift of test-suites and (testsuite test-case) pairs that LIFT will ignore
during calls to run-tests.")

(defvar *test-result* nil
  "Set to the most recent test result by calls to run-test or run-tests.")

(defvar *test-environment* nil)

(defvar *test-metadata* (list)
  "A place for LIFT to put stuff.")

(defvar *current-test* nil
  "The current testsuite.")

(defvar *testsuite-test-count* nil
  "Temporary variable used to 'communicate' between deftestsuite and addtest.")

(defvar *lift-dribble-pathname* nil
  "If bound, then test output from run-tests will be sent to this file in  
in addition to *lift-standard-output*. It can be set to nil or to a pathname.")

(defvar *lift-report-pathname* nil
  "If bound to a pathname or stream, then a summary of test information will
be written to it for later processing. It can be set to:

* `nil` - generate no output
* pathname designator - send output to this pathname
* `t` - send output to a pathname constructed from the name of the system 
being tested (this only works if ASDF is being used to test the system).

As an example of the last case, if LIFT is testing a system named ...
")

(defvar *lift-standard-output* *standard-output*
  "Output from tests will be sent to this stream. If can set to nil or 
to an output stream. It defaults to *standard-output*.")

(defvar *lift-if-dribble-exists* :append
  "Specifies what to do to any existing file at *lift-dribble-pathname*. It 
can be :supersede, :append, or :error.")

(defvar *test-show-expected-p* t)

(defvar *test-show-details-p* t)

(defvar *test-show-code-p* t)
 
;;; ---------------------------------------------------------------------------
;;; Error messages and warnings
;;; ---------------------------------------------------------------------------

(defparameter +lift-test-name-not-supplied-with-test-class+
  "if you specify a test-class, you must also specify a test-name.")

(defparameter +lift-test-class-not-found+
  "test class '~S' not found.")

(defparameter +lift-confused-about-arguments+
  "I'm confused about what you said?!")

(defparameter +lift-no-current-test-class+
  "There is no current-test-class to use as a default.")

(defparameter +lift-could-not-find-test+
  "Could not find test: ~S.~S")

(defparameter +run-tests-null-test-case+
  "There is no current testsuite (possibly because 
   none have been defined yet?). You can specify the 
   testsuite to test by evaluating (run-tests :suite <suitename>).")

(defparameter +lift-unable-to-parse-test-name-and-class+ 
  "")


;;; ---------------------------------------------------------------------------
;;; test conditions
;;; ---------------------------------------------------------------------------

(define-condition lift-compile-error (error)
                  ((msg :initform "" 
                        :reader msg
                        :initarg :lift-message))
  (:report (lambda (c s)
             (format s "Compile error: '~S'" (msg c)))))

(define-condition testsuite-not-defined (lift-compile-error)
                  ((testsuite-name :reader testsuite-name
                                    :initarg :testsuite-name))
  (:report (lambda (c s)
             (format s "Test class ~A not defined before it was used."
                     (testsuite-name c)))))

(define-condition testsuite-ambiguous (lift-compile-error)
                  ((testsuite-name :reader testsuite-name
                                    :initarg :testsuite-name)
		   (possible-matches :reader possible-matches
				     :initarg :possible-matches))
  (:report 
   (lambda (c s)
     (format s "There are several test suites named ~s: they are ~{~s~^, ~}"
	     (testsuite-name c)
	     (possible-matches c)))))

(define-condition test-case-not-defined (lift-compile-error)
                  ((testsuite-name :reader testsuite-name
				   :initarg :testsuite-name)
		   (test-case-name :reader test-case-name
				   :initarg :test-case-name))
  (:report (lambda (c s)
             (format s "Testsuite ~s has no test-case named ~s."
                     (testsuite-name c)
		     (test-case-name c)))))

(define-condition test-case-ambiguous (lift-compile-error)
                  ((testsuite-name :reader testsuite-name
				   :initarg :testsuite-name)
		   (test-case-name :reader test-case-name
				   :initarg :test-case-name)
		   (possible-matches :reader possible-matches
				     :initarg possible-matches))
  (:report 
   (lambda (c s)
     (format s "There are several test cases named ~s.~s: they are ~{~s~^, ~}"
                     (testsuite-name c)
		     (test-case-name c)
		     (possible-matches c)))))

(define-condition test-condition (warning) 
                  ((message :initform ""
                            :initarg :message
                            :accessor message))
  (:report (lambda (c s)
             (when (message c)
               (format s "~%~A" (message c))))))

(define-condition test-timeout-condition (test-condition) 
                  ((maximum-time :initform *test-maximum-time* 
                                 :accessor maximum-time
                                 :initarg :maximum-time))
  (:report (lambda (c s)
             (format s "Test ran out of time (longer than ~S-second~:P)" 
                     (maximum-time c)))))

(define-condition ensure-failed-error (test-condition) 
                  ((assertion :initform "" 
                              :accessor assertion
                              :initarg :assertion))
  (:report (lambda (c s)
             (format s "Ensure failed: ~S ~@[(~a)~]" 
		     (assertion c) (message c)))))

(define-condition ensure-null-failed-error (ensure-failed-error)
  ((value :initform "" 
	  :accessor value
	  :initarg :value)
   (assertion :initform "" 
	      :accessor assertion
	      :initarg :assertion))
  (:report (lambda (c s)
             (format s "Ensure null failed: ~s evaluates to ~s ~@[(~a)~]" 
		     (assertion c) (value c) (message c)))))

(define-condition ensure-expected-condition (test-condition) 
  ((expected-condition-type
    :initform nil
    :accessor expected-condition-type
    :initarg :expected-condition-type)
   (the-condition
    :initform nil
    :accessor the-condition
    :initarg :the-condition))
  (:report (lambda (c s)
	     (let ((the-condition (the-condition c)))
	       (format s "Expected ~S but got ~S~@[:~_   ~A~]" 
		       (expected-condition-type c)
		       (type-of the-condition)
		       (and (typep the-condition 'condition)
			    the-condition))))))

(define-condition ensure-expected-no-warning-condition (test-condition) 
  ((the-condition
    :initform nil
    :accessor the-condition
    :initarg :the-condition))
  (:report (lambda (c s)
             (format s "Expected no warnings but got ~S" 
                     (the-condition c)))))

(define-condition failed-comparison-condition (test-condition) 
  ((first-value :accessor first-value
		:initarg :first-value)
   (second-value :accessor second-value
		 :initarg :second-value)
   (test :accessor test
	 :initarg :test)))

(define-condition ensure-not-same (failed-comparison-condition) 
  ()
  (:report (lambda (c s)
             (format s "Ensure-same: ~S is not ~S to ~S~@[ (~a)~]"
                     (first-value c) (test c) (second-value c)
		     (message c)))))

(define-condition ensure-same (failed-comparison-condition) 
  ()
  (:report (lambda (c s)
             (format s "Ensure-different: ~S is ~S to ~S~@[ (~a)~]"
                     (first-value c) (test c) (second-value c)
		     (message c)))))

(define-condition ensure-cases-failure (test-condition)
  ((total :initarg :total :initform 0)
   (problems :initarg :problems :initform nil))
  (:report (lambda (condition stream)
	     (format 
	      stream 
	      "Ensure-cases: ~d out of ~d cases failed. Failing cases are:"
	      (length (slot-value condition 'problems))
	      (slot-value condition 'total))
	     (format 
	      stream 
	      "~&~@<  ~@;~{~%  ~{~20s ~3,8@t~a~}~^, ~}~:>" 
		     (slot-value condition 'problems)))))

(define-condition unexpected-success-failure (test-condition)
  ((expected :reader expected :initarg :expected)
   (expected-more :reader expected-more :initarg :expected-more))
  (:report (lambda (c s)
	     (format s "Test succeeded but we expected ~s (~s)"
		     (expected c)
		     (expected-more c)))))

(defun build-lift-error-message (context message &rest arguments)
  (format nil "~A: ~A" 
          context
          (apply #'format nil message arguments)))

(defun signal-lift-error (context message &rest arguments)
  (let ((c (make-condition  
            'lift-compile-error
            :lift-message (apply #'build-lift-error-message
				 context message arguments))))
    (unless (signal c)
      (error c))))

(defun report-lift-error (context message &rest arguments)
  (format *debug-io* "~&~A."
          (apply #'build-lift-error-message context message arguments))
  (values))

(defun lift-report-condition (c)
  (format *debug-io* "~&~A." c))

(defmacro ensure (predicate &key report arguments)
  "If ensure's `predicate` evaluates to false, then it will generate a 
test failure. You can use the `report` and `arguments` keyword parameters
to customize the report generated in test results. For example:

    (ensure (= 23 12) 
     :report \"I hope ~a does not = ~a\" 
     :arguments (12 23))

will generate a message like

    Warning: Ensure failed: (= 23 12) (I hope 12 does not = 23)
"
  (let ((gpredicate (gensym)))
    `(let ((,gpredicate ,predicate))
       (if ,gpredicate
	   (values ,gpredicate)
	   (let ((condition (make-condition 
			     'ensure-failed-error 
			     :assertion ',predicate
			     ,@(when report
				     `(:message 
				       (format nil ,report ,@arguments))))))
	     (if (find-restart 'ensure-failed)
		 (invoke-restart 'ensure-failed condition) 
		 (warn condition)))))))

(defmacro ensure-null (predicate &key report arguments)
  "If ensure-null's `predicate` evaluates to true, then it will generate a 
test failure. You can use the `report` and `arguments` keyword parameters
to customize the report generated in test results. See [ensure][] for more 
details."
  (let ((g (gensym)))
    `(let ((,g ,predicate))
       (if (null ,g)
	   t
	 (let ((condition (make-condition 'ensure-null-failed-error
			    :value ,g
			    :assertion ',predicate
			    ,@(when report
				`(:message (format nil ,report ,@arguments))))))
	   (if (find-restart 'ensure-failed)
	       (invoke-restart 'ensure-failed condition) 
	     (warn condition)))))))

(defmacro ensure-condition (condition &body body)
  "This macro is used to make sure that body really does produce condition."
  (setf condition (remove-leading-quote condition))
  (destructuring-bind (condition &key report arguments)
                      (if (consp condition) condition (list condition))
    (let ((g (gensym)))
      `(let ((,g nil))
         (unwind-protect
           (handler-case 
             (progn ,@body)
             (,condition (cond) 
                         (declare (ignore cond)) (setf ,g t))
             (condition (cond) 
                        (setf ,g t)
                        (let ((c (make-condition 
                                  'ensure-expected-condition
                                  :expected-condition-type ',condition
                                  :the-condition cond
                                  ,@(when report
                                      `(:message 
					(format nil ,report ,arguments))))))
                          (if (find-restart 'ensure-failed)
                            (invoke-restart 'ensure-failed c) 
                            (warn c)))))
           (when (not ,g)
             (if (find-restart 'ensure-failed)
               (invoke-restart
		'ensure-failed 
		(make-condition 
		 'ensure-expected-condition
		 :expected-condition-type ',condition
		 :the-condition nil
		 ,@(when report
			 `(:message (format nil ,report ,arguments))))) 
               (warn "Ensure-condition didn't get the condition it expected."))))))))

(defmacro ensure-no-warning (&body body)
  "This macro is used to make sure that body produces no warning."
  (let ((g (gensym))
	(gcondition (gensym)))
    `(let ((,g nil)
	   (,gcondition nil))
       (unwind-protect
	    (handler-case 
		(progn ,@body)
	      (warning (c)
		(setf ,gcondition c ,g t)))
	 (when ,g
	   (let ((c (make-condition 
		    'ensure-expected-no-warning-condition
		    :the-condition ,gcondition)))
	    (if (find-restart 'ensure-failed)
		(invoke-restart 'ensure-failed c) 
		(warn c))))))))

(defmacro ensure-warning (&body body)
  "Ensure-warning evaluates its body. If the body does *not* signal a 
warning, then ensure-warning will generate a test failure."
  `(ensure-condition warning ,@body))

(defmacro ensure-error (&body body)
  "Ensure-error evaluates its body. If the body does *not* signal an 
error, then ensure-error will generate a test failure."
  `(ensure-condition error ,@body))

(defmacro ensure-same
    (form values &key (test nil test-specified-p) 
     (report nil) (arguments nil)
     (ignore-multiple-values? nil))
  "Ensure same compares value-or-values-1 value-or-values-2 or 
each value of value-or-values-1 value-or-values-2 (if they are 
multiple values) using test. If a problem is encountered 
ensure-same raises a warning which uses report as a format string
and arguments as arguments to that string (if report and arguments 
are supplied). If ensure-same is used within a test, a test failure 
is generated instead of a warning"
  (%build-ensure-comparison form values 'unless 
			    test test-specified-p report arguments
			    ignore-multiple-values?))

(defmacro ensure-different
    (form values &key (test nil test-specified-p) 
     (report nil) (arguments nil)
     (ignore-multiple-values? nil))
  "Ensure-different compares value-or-values-1 value-or-values-2 or each value of value-or-values-1 and value-or-values-2 (if they are multiple values) using test. If any comparison returns true, then ensure-different raises a warning which uses report as a format string and `arguments` as arguments to that string (if report and `arguments` are supplied). If ensure-different is used within a test, a test failure is generated instead of a warning"
  (%build-ensure-comparison form values 'when
			    test test-specified-p report arguments
			    ignore-multiple-values?))

(defun %build-ensure-comparison
    (form values guard-fn test test-specified-p report arguments
     ignore-multiple-values?)
  (setf test (remove-leading-quote test))
  (when (and (consp test)
             (eq (first test) 'function))
    (setf test (second test)))
  (let ((gblock (gensym "block-"))
	(ga (gensym "a-"))
	(gb (gensym "b-"))
	(gtest (gensym "test-")))
    `(block ,gblock
       (flet ((,gtest (,ga ,gb)
		(,@(cond (test-specified-p
			  (if (atom test) 
			      (list test)
			      `(funcall ,test)))
			 (t
			  `(funcall *lift-equality-test*)))
		   ,ga ,gb)))
	 (loop for value in (,(if ignore-multiple-values? 
				  'list 'multiple-value-list) ,form)
	    for other-value in (,(if ignore-multiple-values? 
				     'list 'multiple-value-list) ,values) do
	    (,guard-fn (,gtest value other-value)
	      (,(ecase guard-fn 
		       (unless 'maybe-raise-not-same-condition)
		       (when 'maybe-raise-ensure-same-condition))
	       value other-value
	       ,(if test-specified-p (list 'quote test) '*lift-equality-test*)
	       ,report ,@arguments)
	      (return-from ,gblock nil))))
       (values t))))

(defun maybe-raise-not-same-condition (value-1 value-2 test 
				       report &rest arguments)
  (let ((condition (make-condition 'ensure-not-same 
                                   :first-value value-1
                                   :second-value value-2
                                   :test test
                                   :message (when report
                                              (apply #'format nil 
						     report arguments)))))
    (if (find-restart 'ensure-failed)
      (invoke-restart 'ensure-failed condition) 
      (warn condition))))

(defun maybe-raise-ensure-same-condition (value-1 value-2 test 
				       report &rest arguments)
  (let ((condition (make-condition 'ensure-same 
                                   :first-value value-1
                                   :second-value value-2
                                   :test test
                                   :message (when report
                                              (apply #'format nil 
						     report arguments)))))
    (if (find-restart 'ensure-failed)
      (invoke-restart 'ensure-failed condition) 
      (warn condition))))

(defmacro ensure-cases ((&rest vars) (&rest cases) &body body)
  (let ((case (gensym))
	(total (gensym))
	(problems (gensym))
	(single-var-p (= (length vars) 1)))
    `(let ((,problems nil) (,total 0))
       (loop for ,case in ,cases do
	    (incf ,total)
	    (destructuring-bind ,vars ,(if single-var-p `(list ,case) case)
	      (restart-case
		  (progn ,@body)
		(ensure-failed (cond)
		  (push (list ,case cond) ,problems)))))
       (if ,problems
	 (let ((condition (make-condition 
			   'ensure-cases-failure
			   :total ,total
			   :problems ,problems)))
	   (if (find-restart 'ensure-failed)
	       (invoke-restart 'ensure-failed condition) 
	       (warn condition)))
	 ;; return true if we're happy
	 t))))


;;; ---------------------------------------------------------------------------
;;; test-mixin
;;; ---------------------------------------------------------------------------

(defclass test-mixin ()
  ((name :initform nil :initarg :name :accessor name :reader testsuite-name)
   (run-setup :reader run-setup :initarg :run-setup)
   (done-setup? :initform nil :reader done-setup?)
   (done-dynamics? :initform nil :reader done-dynamics?)
   (test-slot-names :initform nil :initarg :test-slot-names 
		    :reader test-slot-names)
   (current-step :initform :created :accessor current-step)
   (current-method :initform nil :accessor current-method)
   (save-equality-test :initform nil  :reader save-equality-test)
   (log-file :initform nil :initarg :log-file :reader log-file)
   (test-data :initform nil :accessor test-data)
   (expected-failure-p :initform nil :initarg :expected-failure-p
		       :reader expected-failure-p)
   (expected-error-p :initform nil :initarg :expected-error-p
		     :reader expected-error-p)
   (expected-problem-p :initform nil :initarg :expected-problem-p
		       :reader expected-problem-p)
   (suite-initargs
    :initform nil
    :accessor suite-initargs)
   (profile 
    :initform nil
    :initarg :profile
    :accessor profile))
  (:documentation "A test suite")
  (:default-initargs
    :run-setup :once-per-test-case))

(defclass test-result ()
  ((results-for :initform nil 
		:initarg :results-for 
		:accessor results-for)
   (tests-run :initform nil :accessor tests-run)
   (suites-run :initform nil :accessor suites-run)
   (failures :initform nil :accessor failures)
   (expected-failures :initform nil :accessor expected-failures)
   (errors :initform nil :accessor errors)
   (expected-errors :initform nil :accessor expected-errors)
   (test-mode :initform :single :initarg :test-mode :accessor test-mode)
   (test-interactive? :initform nil 
                      :initarg :test-interactive? :accessor test-interactive?)
   (real-start-time :initarg :real-start-time :reader real-start-time)
   (start-time :accessor start-time :initform nil)
   (end-time :accessor end-time)
   (real-end-time :accessor real-end-time)
   (real-start-time-universal
    :initarg :real-start-time-universal :reader real-start-time-universal)
   (start-time-universal :accessor start-time-universal :initform nil)
   (end-time-universal :accessor end-time-universal)
   (real-end-time-universal :accessor real-end-time-universal)
   (properties :initform nil :accessor test-result-properties)
   (tests-to-skip :initform nil
		  :initarg :tests-to-skip
		  :reader tests-to-skip
		  :writer %set-tests-to-skip))
  (:documentation 
"A `test-result` instance contains all of the information collectd by 
LIFT during a test run.")
  (:default-initargs
    :test-interactive? *test-is-being-defined?*
    :real-start-time (get-internal-real-time)
    :real-start-time-universal (get-universal-time)
    :tests-to-skip *lift-tests-to-skip*))

(defmethod initialize-instance :after
    ((result test-result) &key tests-to-skip)
  (when tests-to-skip
    (%set-tests-to-skip 
     (mapcar (lambda (datum)
	       (cond ((or (atom datum)
			  (= (length datum) 1))
		      (cons (find-testsuite datum) nil))
		     ((= (length datum) 2)
		      (cons (find-testsuite (first datum))
			    (or (and (keywordp (second datum)) (second datum))
				(find-test-case (find-testsuite (first datum))
						(second datum)))))
		     (t
		      (warn "Unable to interpret skip datum ~a. Ignoring." 
			    datum))))
	     tests-to-skip)
     result)))

(defun test-result-property (result property &optional default)
  (getf (test-result-properties result) property default))

(defun (setf test-result-property) (value result property)
  (setf (getf (test-result-properties result) property) value))

(defun print-lift-message (message &rest args)
  (apply #'format *lift-debug-output* message args)
  (force-output *lift-debug-output*))

(defgeneric testsuite-setup (testsuite result)
  (:documentation "Setup at the testsuite-level")
  (:method ((testsuite test-mixin) (result test-result))
           (values))
  (:method :before ((testsuite test-mixin) (result test-result))
	   (when (and *test-print-testsuite-names*
		      (eq (test-mode result) :multiple))
	     (print-lift-message "~&Start: ~a" (type-of testsuite)))
	   (push (type-of testsuite) (suites-run result))
           (setf (current-step testsuite) :testsuite-setup)))

(defgeneric testsuite-expects-error (testsuite)
  (:documentation "Returns whether or not the testsuite as a whole expects an error.")
  (:method ((testsuite test-mixin))
    nil))

(defgeneric testsuite-expects-failure (testsuite)
  (:documentation "Returns whether or not the testsuite as a whole expects to fail.")
  (:method ((testsuite test-mixin))
    nil))

(defgeneric testsuite-run (testsuite result)
  (:documentation "Run the cases in this suite and it's children."))

(defgeneric testsuite-teardown (testsuite result)
  (:documentation "Cleanup at the testsuite level.")
  (:method ((testsuite test-mixin) (result test-result))
    ;; no-op
    )
  (:method :after ((testsuite test-mixin) (result test-result))
    (setf (current-step testsuite) :testsuite-teardown
	  (real-end-time result) (get-internal-real-time)
	  (real-end-time-universal result) (get-universal-time))))

(defgeneric setup-test (testsuite)
  (:documentation "Setup for a test-case. By default it does nothing."))

(defgeneric test-case-teardown (testsuite result)
  (:documentation "Tear-down a test-case. By default it does nothing.")
  (:method-combination progn :most-specific-first))

(defgeneric testsuite-methods (testsuite)
  (:documentation "Returns a list of the test methods defined for test. I.e.,
the methods that should be run to do the tests for this test."))

(defgeneric lift-test (suite name)
  (:documentation ""))

(defgeneric do-testing (testsuite result fn)
  (:documentation ""))

(defgeneric end-test (result case method-name)
  (:documentation ""))

(defgeneric run-test-internal (suite name result &rest args)
  (:documentation ""))

(defgeneric run-tests-internal (suite &rest args
 			       &key &allow-other-keys)
  (:documentation ""))

(defgeneric start-test (result case method-name)
  (:documentation ""))

(defgeneric test-report-code (testsuite method)
  (:documentation ""))

(defgeneric testsuite-p (thing)
  (:documentation "Determine whether or not `thing` is a testsuite. Thing can be a symbol naming a suite, a subclass of `test-mixin` or an instance of a test suite. Returns nil if `thing` is not a testsuite and the symbol naming the suite if it is."))

(defgeneric testsuite-name->gf (case name)
  (:documentation ""))

(defgeneric testsuite-name->method (class name)
  (:documentation ""))

(defgeneric flet-test-function (testsuite function-name &rest args)
  (:documentation ""))

(defgeneric equality-test (testsuite)
  (:documentation ""))

(defgeneric do-testing-in-environment (testsuite result function)
  (:documentation ""))  

(defgeneric skip-test-case (result suite-name test-case-name)
  )

(defgeneric describe-test-result (result stream &key &allow-other-keys)
  )

(defgeneric write-profile-information (testsuite))

(defmethod write-profile-information ((suite t))
  )

(defmethod equality-test ((suite test-mixin))
  #'equal)

(defmethod setup-test :before ((test test-mixin))
  (setf *test-scratchpad* nil
	(current-step test) :test-setup))

(defmethod setup-test ((test test-mixin))
  (values))

(defmethod test-case-teardown progn ((test test-mixin) (result test-result))
  (values))

(defmethod test-case-teardown :around ((test test-mixin) (result test-result))
  (setf (current-step test) :test-teardown)
  (call-next-method))

(defmethod initialize-instance :after ((testsuite test-mixin) &rest initargs 
				       &key &allow-other-keys)
  (when (null (testsuite-name testsuite))
    (setf (slot-value testsuite 'name) 
	  (symbol-name (type-of testsuite))))
  ;; FIXME - maybe remove LIFT standard arguments?
  (setf (suite-initargs testsuite) initargs))

(defmethod print-object ((tc test-mixin) stream)
  (print-unreadable-object (tc stream :identity t :type t)
    (format stream "~a" (testsuite-name tc))))

;;; ---------------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------------

(defvar *current-definition* nil
  "An associative-container which saves interesting information about
the thing being defined.")

(defun initialize-current-definition ()
  (setf *current-definition* nil))

(defun set-definition (name value)
  (let ((current (assoc name *current-definition*)))
    (if current
      (setf (cdr current) value)
      (push (cons name value) *current-definition*)))
  
  (values value))

(defun def (name &optional (definition *current-definition*))
  (when definition (cdr (assoc name definition))))

(defun (setf def) (value name)
  (set-definition name value))

(defvar *code-blocks* nil)

(defstruct (code-block (:type list) (:conc-name nil))
  block-name (priority 0) filter code operate-when)

(defgeneric block-handler (name value)
  (:documentation "")
  (:method ((name t) (value t))
           (error "Unknown clause: ~A" name)))

(defun add-code-block (name priority operate-when filter handler code)
  (let ((current (assoc name *code-blocks*))
        (value (make-code-block
                :operate-when operate-when
                :block-name name
                :priority priority
                :filter filter
                :code code)))
    (if current
      (setf (cdr current) value)
      (push (cons name value) *code-blocks*))  
    (eval 
     `(defmethod block-handler ((name (eql ',name)) value)
        (declare (ignorable value))
        ,@handler)))    
    (setf *code-blocks* (sort *code-blocks* #'< 
			      :key (lambda (name.cb)
				     (priority (cdr name.cb))))))

(defmacro with-test-slots (&body body)
  `(symbol-macrolet ((lift-result (getf (test-data *current-test*) :result)))   
     ;; case111 - LW complains otherwise
     (declare (ignorable lift-result)
	      ,@(when (def :dynamic-variables)
		      `((special ,@(mapcar #'car (def :dynamic-variables))))))
     (symbol-macrolet
	 ,(mapcar #'(lambda (local)
		      `(,local (test-environment-value ',local)))
		  (test-slots (def :testsuite-name)))
       (declare (ignorable ,@(test-slots (def :testsuite-name))))
       (macrolet
	   ,(mapcar (lambda (spec)
		      (destructuring-bind (name arglist) spec
			`(,name ,arglist 
				`(flet-test-function 
				  *current-test* ',',name ,,@arglist))))
		    (def :function-specs))
	 (progn ,@body)))))

(defvar *deftest-clauses*
  '(:setup :teardown :test :documentation :tests :export-p :export-slots
    :run-setup :dynamic-variables :equality-test :categories :function))

(defmacro deftest (testsuite-name superclasses slots &rest
                                  clauses-and-options) 
  "The `deftest` form is obsolete, see [deftestsuite][]."
  
  (warn "Deftest is obsolete, use deftestsuite instead.")
  `(deftestsuite ,testsuite-name ,superclasses ,slots ,@clauses-and-options))

(setf *code-blocks* nil)

(add-code-block
 :setup 1 :methods
 (lambda () 
   (or (def :setup) (def :direct-slot-names))) 
 '((setf (def :setup) (cleanup-parsed-parameter value)))
 'build-setup-test-method)

(add-code-block
 :teardown 100 :methods
 (lambda () (or (def :teardown) (def :direct-slot-names))) 
 '((setf (def :teardown) (cleanup-parsed-parameter value)))
 'build-test-teardown-method)

(add-code-block
 :function 0 :methods
 (lambda () (def :functions))
 '((push value (def :functions)))
 'build-test-local-functions)

(add-code-block
 :documentation 0 :class-def 
 nil 
 '((setf (def :documentation) (first value)))
 nil)

(add-code-block
 :export-p 0 :class-def
 nil 
 '((setf (def :export-p) (first value)))
 nil)

(add-code-block
 :export-slots 0 :class-def
 nil 
 '((setf (def :export-slots) (first value)))
 nil)

(add-code-block
 :run-setup 0 :class-def
 nil 
 '((push (first value) (def :default-initargs))
   (push :run-setup (def :default-initargs))
   (setf (def :run-setup) (first value)))
 'check-run-setup-value)

(defun %valid-run-setup-values ()
  '(:once-per-session :once-per-suite
    :once-per-test-case :never))

(defun check-run-setup-value ()
  (when (def :run-setup)
    (unless (member (def :run-setup) (%valid-run-setup-values))
      (error "The :run-setup option must be one of ~{~a~^, ~}." 
	     (%valid-run-setup-values)))))
    
(add-code-block
 :equality-test 0 :methods
 (lambda () (def :equality-test))
 '((setf (def :equality-test) (cleanup-parsed-parameter value)))
 'build-test-equality-test)

(add-code-block
 :expected-error 0 :methods
 (lambda () (def :expected-error))
 '((setf (def :expected-error) (cleanup-parsed-parameter value)))
 'build-testsuite-expected-error)

(add-code-block
 :expected-failure 0 :methods
 (lambda () (def :expected-failure))
 '((setf (def :expected-failure) (cleanup-parsed-parameter value)))
 'build-testsuite-expected-failure)

(add-code-block
 :log-file 0 :class-def
 nil 
 '((push (first value) (def :default-initargs))
   (push :log-file (def :default-initargs)))
 nil)

(add-code-block
 :dynamic-variables 0 :class-def
 nil 
 '((setf (def :direct-dynamic-variables) value))
 nil)

(add-code-block
 :categories 0 :class-def
 nil 
 '((push value (def :categories)))
 nil)

(add-code-block
 :default-initargs 1 :class-def
 (lambda () (def :default-initargs))
 '((dolist (x (reverse (cleanup-parsed-parameter value)))
   (push x (def :default-initargs))))
 nil)

(defmacro deftestsuite (testsuite-name superclasses slots &rest
			clauses-and-options) 
  "
Creates a testsuite named `testsuite-name` and, optionally, the code required for test setup, test tear-down and the actual test-cases. A testsuite is a collection of test-cases and other testsuites.

Test suites can have multiple superclasses (just like the classes that they are). Usually, these will be other test classes and the class hierarchy becomes the test case hierarchy. If necessary, however, non-testsuite classes can also be used as superclasses.

Slots are specified as in defclass with the following additions:

* Initargs and accessors are automatically defined. If a slot is named`my-slot`, then the initarg will be `:my-slot` and the accessors will be `my-slot` and `(setf my-slot)`. 
* If the second argument is not a CLOS slot option keyword, then it will be used as the `:initform` for the slot. I.e., if you have

        (deftestsuite my-test ()
          ((my-slot 23)))

    then `my-slot` will be initialized to 23 during test setup.

Test options are one of :setup, :teardown, :test, :tests, :documentation, :export-p, :dynamic-variables, :export-slots, :function, :categories, :run-setup, or :equality-test. 

* :categories - a list of symbols. Categories allow you to groups tests into clusters outside of the basic hierarchy. This provides finer grained control on selecting which tests to run. May be specified multiple times.

* :documentation - a string specifying any documentation for the test. Should only be specified once.

* :dynamic-variables - a list of atoms or pairs of the form (name value). These specify any special variables that should be bound in a let around the body of the test. The name should be symbol designating a special variable. The value (if supplied) will be bound to the variable. If the value is not supplied, the variable will be bound to nil. Should only be specified once.

* :equality-test - the name of the function to be used by default in calls to ensure-same and ensure-different. Should only be supplied once. 

* :export-p - If true, the testsuite name will be exported from the current package. Should only be specified once.

* :export-slots - if true, any slots specified in the test suite will be exported from the current package. Should only be specified once.

* :function - creates a locally accessible function for this test suite. May be specified multiple times. 

* :run-setup - specify when to run the setup code for this test suite. Allowed values are 

    * :once-per-test-case or t (the default)
    * :once-per-session
    * :once-per-suite
    * :never or nil

    :run-setup is handy when a testsuite has a time consuming setup phase that you do not want to repeat for every test.

* :setup - a list of forms to be evaluated before each test case is run.  Should only be specified once.

* :teardown - a list of forms to be evaluated after each test case is run. Should only be specified once.

* :test - Define a single test case. Can be specified multiple times.

* :tests - Define multiple test cases for this test suite. Can be specified multiple times.
"
  #+no-lift-tests
  `(values)
  #-no-lift-tests
  (let ((test-list nil)
        (options nil)
        (return (gensym)))
    ;; convert any clause like :setup foo into (:setup foo)
    (setf clauses-and-options 
          (convert-clauses-into-lists clauses-and-options *deftest-clauses*))
    (initialize-current-definition)
    (setf (def :testsuite-name) testsuite-name)
    (setf (def :superclasses) (mapcar #'find-testsuite superclasses))
    (setf (def :deftestsuite) t)
    ;; parse clauses into defs
    (loop for clause in clauses-and-options do
	 (typecase clause
	   (symbol (pushnew clause options))
	   (cons (destructuring-bind (kind &rest spec) clause
		   (case kind
		     (:test (push (first spec) test-list))
		     (:tests 
		      (loop for test in spec do
			   (push test test-list)))
		     (t (block-handler kind spec)))))
	   (t (error "When parsing ~S" clause))))
    (let ((slot-names nil) (slot-specs nil))
      (loop for slot in (if (listp slots) slots (list slots)) do 
	   (push (if (consp slot) (first slot) slot) slot-names)
	   (push (parse-brief-slot slot) slot-specs))
      (setf (def :slot-specs) (nreverse slot-specs)
            (def :direct-slot-names) (nreverse slot-names)
            (def :slots-parsed) t))
    ;;?? issue 27: breaks 'encapsulation' of code-block mechanism
    (setf (def :function-specs)
	  (loop for spec in (def :functions) collect
	       (destructuring-bind (name arglist &body body) (first spec)
		 (declare (ignore body))
		 `(,name ,arglist))))
    ;;?? needed
    (empty-test-tables testsuite-name)
    (compute-superclass-inheritence)
    (prog2
	(setf *testsuite-test-count* 0)
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   (eval-when (:compile-toplevel)
	     (push ',return *test-is-being-compiled?*))
	   (eval-when (:load-toplevel)
	     (push ',return *test-is-being-loaded?*))
	   (eval-when (:execute)
	     (push ',return *test-is-being-executed?*))
	   ;; remove previous methods (do this _before_ we define the class)
	   (unless (or *test-is-being-compiled?*
		       *test-is-being-loaded?*)
	     #+(or)
	     (print (list :cle *test-is-being-compiled?* 
			  *test-is-being-loaded?*
			  *test-is-being-loaded?*))
	     (remove-previous-definitions ',(def :testsuite-name)))
	   ,(build-test-class)
	   (unwind-protect
		(let ((*test-is-being-defined?* t))
		  (setf *current-test-case-name* nil)
		  (setf *current-testsuite-name* ',(def :testsuite-name)
			(test-slots ',(def :testsuite-name)) 
			',(def :slot-names)
			(testsuite-dynamic-variables ',(def :testsuite-name))
			',(def :dynamic-variables)
			;;?? issue 27: breaks 'encapsulation' of code-block
			;; mechanism
			(testsuite-function-specs ',(def :testsuite-name))
			',(def :function-specs))
		  ,@(when (def :export-p)
			  `((export '(,(def :testsuite-name)))))
		  ,@(when (def :export-slots?)
			  `((export ',(def :direct-slot-names))))
		  ;; make a place to save test-case information
		  (empty-test-tables ',(def :testsuite-name))
		  ;; create methods
		  ;; setup :before
		  ,@(loop for (nil . block) in *code-blocks* 
		       when (and block 
				 (code block)
				 (eq (operate-when block) :methods)
				 (or (not (filter block))
				     (funcall (filter block)))) collect
		       (funcall (code block)))
		  ,@(when (def :dynamic-variables)
			  `((defmethod do-testing :around
				((suite ,(def :testsuite-name)) result fn) 
			      (declare (ignore result fn)
				       (special 
					,@(mapcar 
					   #'car (def :dynamic-variables))))
			      (cond ((done-dynamics? suite)
				     (call-next-method))
				    (t
				     (setf (slot-value suite 'done-dynamics?) t)
				     (let* (,@(def :dynamic-variables))
				       (declare (special 
						 ,@(mapcar 
						    #'car (def :dynamic-variables))))
				       (call-next-method)))))))
		  ;; tests
		  ,@(when test-list
			  `((let ((*test-evaluate-when-defined?* nil))
			      ,@(loop for test in (nreverse test-list) collect
				     `(addtest (,(def :testsuite-name)) 
					,@test))
			      (setf *testsuite-test-count* nil))))
		  ,(if (and test-list *test-evaluate-when-defined?*)
		       `(unless (or *test-is-being-compiled?*
				    *test-is-being-loaded?*)
			  (let ((*test-break-on-errors?* *test-break-on-errors?*))
			    (run-tests :suite ',testsuite-name)))
		       `(find-class ',testsuite-name)))
	     ;; cleanup
	     (setf *test-is-being-compiled?* 
		   (remove ',return *test-is-being-compiled?*))
	     (setf *test-is-being-loaded?* 
		   (remove ',return *test-is-being-loaded?*))
	     (setf *test-is-being-executed?* 
		   (remove ',return *test-is-being-executed?*)))))))
 
(defun compute-superclass-inheritence ()
  ;;?? issue 27: break encapsulation of code blocks
  ;;?? we assume that we won't have too deep a hierarchy or too many 
  ;; dv's or functions so that having lots of duplicate names is OK
  (let ((slots nil)
	(dynamic-variables nil)
	(function-specs nil))
    (dolist (super (def :superclasses))
      (cond ((find-testsuite super)
	     (setf slots (append slots (test-slots super))
		   dynamic-variables 
		   (append dynamic-variables 
			   (testsuite-dynamic-variables super))
		   function-specs
		   (append function-specs 
			   (testsuite-function-specs super))))
	    (t
	     (error 'testsuite-not-defined :testsuite-name super))))
    (setf (def :slot-names) 
	  (remove-duplicates (append (def :direct-slot-names) slots))
	  (def :dynamic-variables)
	  (remove-duplicates 
	   (append (%build-pairs (def :direct-dynamic-variables))
		   dynamic-variables)
	   :key #'car)
	  (def :function-specs)
	  (remove-duplicates 
	   (append (def :function-specs) function-specs)))
    (setf (def :superclasses)
	  (loop for class in (def :superclasses) 
	     unless (some (lambda (oter)
			    (and (not (eq class oter))
				 (member class (superclasses oter))))
			  (def :superclasses)) collect
	     class))))

(defun %build-pairs (putative-pairs)
  (let ((result nil))
    (dolist (putative-pair putative-pairs)
      (if (atom putative-pair)
        (push (list putative-pair nil) result)
        (push putative-pair result)))
    (nreverse result)))

(defmacro addtest (name &body test)
  "Adds a single new test-case to the most recently defined testsuite."
  #+no-lift-tests
  `nil
  #-no-lift-tests
  (let ((body nil)
	(return (gensym))
	(options nil)
	(looks-like-suite-name (looks-like-suite-name-p name))
	(looks-like-code (looks-like-code-p name)))
    (cond ((and looks-like-suite-name looks-like-code)
	   (error "Can't disambiguate suite name from possible code."))
	  (looks-like-suite-name
	   ;; testsuite given
	   (setf (def :testsuite-name) (first name) 
		 options (rest name)
		 name nil body test))
	  (t
	   ;; the 'name' is really part of the test...
	   (setf body (cons name test))))
    (unless (def :testsuite-name)
      (when *current-testsuite-name*
	(setf (def :testsuite-name) *current-testsuite-name*)))
    (unless (def :testsuite-name)
      (signal-lift-error 'add-test +lift-no-current-test-class+))
    (unless (or (def :deftestsuite) 
		(find-testsuite (def :testsuite-name)))
      (signal-lift-error 'add-test +lift-test-class-not-found+
			 (def :testsuite-name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (eval-when (:compile-toplevel)
	 (push ',return *test-is-being-compiled?*))
       (eval-when (:load-toplevel)
	 (push ',return *test-is-being-loaded?*))
       (eval-when (:execute)
	 (push ',return *test-is-being-executed?*))
       (unwind-protect
	    (let ((*test-is-being-defined?* t))
	      (muffle-redefinition-warnings
		,(build-test-test-method (def :testsuite-name) body options))
	      (setf *current-testsuite-name* ',(def :testsuite-name))
	      (if *test-evaluate-when-defined?*
		  (unless (or *test-is-being-compiled?*
			      *test-is-being-loaded?*)
		    (let ((*test-break-on-errors?* (testing-interactively-p)))
		      (run-test)))
		  (values)))
	   ;; cleanup
	   (setf *test-is-being-compiled?* 
		 (remove ',return *test-is-being-compiled?*)
		 *test-is-being-loaded?*
		 (remove ',return *test-is-being-loaded?*)
		 *test-is-being-executed?*
		 (remove ',return *test-is-being-executed?*))))))

(defun looks-like-suite-name-p (form)
  (and (consp form)
       (atom (first form))
       (find-testsuite (first form))
       (property-list-p (rest form))))

(defun property-list-p (form)
  (and (listp form)
       (block check-it
	 (let ((even? t))
	   (loop for x in form 
	      for want-keyword? = t then (not want-keyword?) do
		(when (and want-keyword? (not (keywordp x)))
		  (return-from check-it nil))
		(setf even? (not even?)))
	   (return-from check-it even?)))))

#|
(property-list-p '(:a :b))
(property-list-p '(:a 2 :b 3 :c 5 :d 8))
(property-list-p nil)

(property-list-p 3)
(property-list-p '(3))
(property-list-p '(3 :a))
(property-list-p '(:a 3 :b))
|#

(defun looks-like-code-p (name)
  (declare (ignore name))
  ;; FIXME - stub
  nil)

(defun remove-test (&key (test-case *current-test-case-name*)
                         (suite *current-testsuite-name*))
  (assert suite nil "Test suite could not be determined.")
  (assert test-case nil "Test-case could not be determined.")
  (setf (testsuite-tests suite)
	(remove test-case (testsuite-tests suite))))

(defun run-test (&rest args
		 &key (test-case *current-test-case-name*)
		 (name test-case name-supplied-p)
		 (suite *current-testsuite-name*) 
		 (break-on-errors? *test-break-on-errors?*)
		 (break-on-failures? *test-break-on-failures?*)
		 (do-children? *test-do-children?*)
		 (result nil)
		 (profile nil)
                 (testsuite-initargs nil))
  "Run a single testcase in a test suite. Will run the most recently defined or run testcase unless the name and suite arguments are used to override them."
  (when name-supplied-p
    (setf test-case name))
  (assert suite nil "Test suite could not be determined.")
  (assert test-case nil "Test-case could not be determined.")
  (let ((args-copy (copy-list args)))
    (declare (ignore args-copy))
    (remf args :suite)
    (remf args :break-on-errors?)
    (remf args :break-on-failures?)
    (remf args :run-setup)
    (remf args :dribble)
    (remf args :config)
    (remf args :report-pathname)
    (remf args :do-children?)
    (remf args :tests-to-skip)
    (remf args :testsuite-initargs)
    (let* ((*test-break-on-errors?* break-on-errors?)
	   (*test-break-on-failures?* break-on-failures?)
	   (*test-do-children?* do-children?)
	   (*current-test*
	    (make-testsuite 
	     suite 
	     (if (find :profile testsuite-initargs)
		 testsuite-initargs
		 (setf testsuite-initargs
		       `(:profile ,profile ,@testsuite-initargs))))))
      (unless result
	(setf result (make-test-result suite :single)))
      (prog1
	  (let ((*current-test-case-name* (find-test-case suite test-case))
		(*current-testsuite-name* suite)
		(*test-result* result))
	    (do-testing-in-environment
		*current-test* result 
		(lambda () 
		  (apply #'run-test-internal
		   *current-test* *current-test-case-name* result nil))))
	(setf *test-result* result)
	(setf *current-test-case-name* (find-test-case suite test-case)
	      *current-testsuite-name* suite)))))

(defun make-testsuite (suite-name args)
  (let ((testsuite (find-testsuite suite-name :errorp t)))
    (if testsuite
	(apply #'make-instance testsuite args)
	(error "Testsuite ~a not found." suite-name))))

(defmethod do-testing-in-environment :around ((suite test-mixin) result fn)
  (declare (ignore fn))
  (tagbody 
   :test-start
     (restart-case
	 (handler-bind ((warning #'muffle-warning)       
					; ignore warnings... 
			(error 
			 (lambda (condition)
			   (report-test-problem
			    'testsuite-error result suite
			    *current-test-case-name* condition
			    :backtrace (get-backtrace condition))
			   (if *test-break-on-errors?*
			       (invoke-debugger condition)
			       (go :test-end)))))
	   (unwind-protect
		(let ((*lift-equality-test* (equality-test suite)))
		  (testsuite-setup suite result)
		  (call-next-method)
		  result)
	     ;; cleanup
	     (testsuite-teardown suite result)))
       (ensure-failed (condition) 
	 (report-test-problem
	  'testsuite-failure result suite 
	  *current-test-case-name* condition))
       (retry-test () :report "Retry the test." 
		   (go :test-start)))
   :test-end)
  (values result))

(defmethod do-testing-in-environment ((suite test-mixin) result fn)
  (do-testing suite result fn)
  (values result))

(defmethod do-testing ((suite test-mixin) result fn)
  (funcall fn)
  (values result))

(defmethod run-tests-internal ((suite symbol) &rest args
			       &key &allow-other-keys)
  (let ((*current-test* (make-testsuite suite args))
	(passthrough-arguments nil))
    (loop for arg in '(:result :do-children?) 
       when (getf args arg) do
	 (push (getf args arg) passthrough-arguments)
	 (push arg passthrough-arguments))
    (apply #'run-tests-internal *current-test* passthrough-arguments)))

(defmethod run-tests-internal 
    ((case test-mixin) &key 
     (result (make-test-result (class-of case) :multiple))
     (do-children? *test-do-children?*))
  (let ((*test-do-children?* do-children?))
    (do-testing-in-environment
	case result
	(lambda ()
	  (testsuite-run case result)))
    (setf *test-result* result)))

(defun run-tests (&rest args &key 
		  (suite nil)
		  (break-on-errors? *test-break-on-errors?*)
		  (break-on-failures? *test-break-on-failures?*)
		  (config nil)
		  (dribble *lift-dribble-pathname*)
		  (report-pathname t)
		  (profile nil)
		  ;(timeout nil)
		  (do-children? *test-do-children?*)
		  (testsuite-initargs nil) 
		  result
		  &allow-other-keys)
  "Run all of the tests in a suite." 
  (declare (ignore profile))
  (prog1
      (let ((args-copy (copy-list args)))
	(remf args :suite)
	(remf args :break-on-errors?)
	(remf args :break-on-failures?)
	(remf args :run-setup)
	(remf args :dribble)
	(remf args :config)
	(remf args :report-pathname)
	(remf args :do-children?)
	(remf args :tests-to-skip)
	(remf args :testsuite-initargs)
	(let* ((*lift-report-pathname*
		(cond ((null report-pathname) nil)
		      ((eq report-pathname t)
		       (report-summary-pathname))))
	       (*test-do-children?* do-children?)
	       (report-pathname *lift-report-pathname*))
	  (when report-pathname
	    (ensure-directories-exist report-pathname))
	  (cond ((and suite config)
		 (error "Specify either configuration file or test suite 
but not both."))
		(config
		 (unless result
		   (setf result
			 (apply #'make-test-result config :multiple args)))
		 (when report-pathname
		   (write-report-header report-pathname result args-copy))
		 (let* ((*test-result* result))
		   (setf result (run-tests-from-file config))))
		((or suite (setf suite *current-testsuite-name*))
		 (unless result
		   (setf result
			 (apply #'make-test-result suite :multiple args)))
		 (when report-pathname
		   (write-report-header report-pathname result args-copy))
		 (let* ((*test-break-on-errors?* break-on-errors?)
			(*test-break-on-failures?* break-on-failures?)
			(*test-result* result)
			(dribble-stream
			 (when dribble
			   (open dribble
				 :direction :output
				 :if-does-not-exist :create
				 :if-exists *lift-if-dribble-exists*)))
			(*standard-output* 
			 (maybe-add-dribble 
			  *lift-standard-output* dribble-stream))
			(*error-output* (maybe-add-dribble 
					 *error-output* dribble-stream))
			(*debug-io* (maybe-add-dribble 
				     *debug-io* dribble-stream)))
		   (unwind-protect
			(with-simple-restart (cancel-testing 
					      "Cancel testing of ~a"
					      *current-testsuite-name*)
			  (dolist (testsuite (if (consp suite) 
						 suite (list suite)))
			    (let ((*current-testsuite-name* testsuite))
			      (apply #'run-tests-internal testsuite
				     :result result
				     testsuite-initargs))
			    (setf *current-testsuite-name* testsuite)))
		     ;; cleanup
		     (when dribble-stream 
		       (close dribble-stream)))
		   ;; FIXME -- ugh!
		   (setf (tests-run result) (reverse (tests-run result)))
		   (when report-pathname
		     (write-report-footer report-pathname result))
		   (values result)))
		(t
		 (error "There is not a current test suite and neither suite 
nor configuration file options were specified.")))))
	(setf *test-result* result)))

(defun maybe-add-dribble (stream dribble-stream)
  (if dribble-stream
      (values (make-broadcast-stream stream dribble-stream) t)
      (values stream nil)))

(defun skip-test-case-p (result suite-name test-case-name)
  (find-if (lambda (skip-datum)
	     (and (eq suite-name (car skip-datum))
		  (or (null (cdr skip-datum))
		      (eq test-case-name (cdr skip-datum)))))
	   (tests-to-skip result)))

(defmethod skip-test-case (result suite-name test-case-name)
  (declare (ignore result suite-name test-case-name))
  )

(defun skip-test-suite-children-p (result testsuite)
  (let ((suite-name (class-name (class-of testsuite))))
    (find-if (lambda (skip-datum)
	       (and (eq suite-name (car skip-datum))
		    (eq :including-children (cdr skip-datum))))
	     (tests-to-skip result))))

(defmethod testsuite-run ((testsuite test-mixin) (result test-result))
  (unless (start-time result)
    (setf (start-time result) (get-internal-real-time)
	  (start-time-universal result) (get-universal-time)))
  (unwind-protect
       (let* ((methods (testsuite-methods testsuite))
	      (suite-name (class-name (class-of testsuite)))
	      (*current-testsuite-name* suite-name))
	 (loop for method in methods do
	      (if (skip-test-case-p result suite-name method)
		  (skip-test-case result suite-name method)
		  (run-test-internal testsuite method result)))
	 (when (and *test-do-children?*
		    (not (skip-test-suite-children-p result testsuite)))
	   (loop for subclass in (direct-subclasses (class-of testsuite))	
	      when (and (testsuite-p subclass)
			(not (member (class-name subclass) 
				     (suites-run result)))) do
	      (run-tests-internal (class-name subclass)
				  :result result))))
    (setf (end-time result) (get-universal-time))))

(defmethod run-test-internal ((suite symbol) (name symbol) result
			       &rest args &key &allow-other-keys)
  (let ((*current-test* (make-testsuite suite args))
	(passthrough-arguments nil))
    (loop for arg in '(:result :do-children?) 
       when (getf args arg) do
	 (push (getf args arg) passthrough-arguments)
	 (push arg passthrough-arguments))
    (apply #'run-test-internal 
	   *current-test* name result passthrough-arguments)))

(defmethod run-test-internal ((suite test-mixin) (name symbol) result
			      &rest _)
  (declare (ignore _))
  (let ((result-pushed? nil)
	(*current-test-case-name* name)
	(error nil))
    (flet ((maybe-push-result ()
					;(print (list :mpr result-pushed? (test-data suite)))
	     (let ((datum (list (type-of suite)
				*current-test-case-name* (test-data suite))))
	       (cond ((null result-pushed?)
		      (setf result-pushed? t)
		      (push datum (tests-run result)))
		     (t
		      ;; replace
		      (setf (first (tests-run result)) datum))))))
      (when (and *test-print-test-case-names*
		 (eq (test-mode result) :multiple))
	(print-lift-message "~&  run: ~a" name))
      (tagbody 
       :test-start
	 (restart-case
	     (handler-bind ((warning #'muffle-warning)       
					; ignore warnings... 
			    (error 
			     (lambda (condition)
			       (report-test-problem
				'test-error result suite
				*current-test-case-name* condition
				:backtrace (get-backtrace condition))
			       (if (and *test-break-on-errors?*
					(not (testcase-expects-error-p)))
				   (invoke-debugger condition)
				   (go :test-end)))))
	       (setf (current-method suite) name)
	       (start-test result suite name)
	       (unwind-protect
		    (progn
		      (setup-test suite)
		      (setf (current-step suite) :testing)
		      (multiple-value-bind (result measures error-condition)
			  (while-measuring (t measure-space measure-seconds)
			    (lift-test suite name))
			(declare (ignore result))
			(setf error error-condition)
			(destructuring-bind (space seconds) measures
			  (setf (getf (test-data suite) :seconds) seconds
				(getf (test-data suite) :conses) space)))
		      (when error
			(error error))
		      (check-for-surprises suite))
		 ;; cleanup
		 (maybe-push-result)
		 (test-case-teardown suite result)
		 (end-test result suite name)))
	   (ensure-failed (condition) 
	     (report-test-problem
	      'test-failure result suite 
	      *current-test-case-name* condition)
	     (if (and *test-break-on-failures?*
		      (not (testcase-expects-failure-p)))
		 (invoke-debugger condition)
		 (go :test-end)))
	   (retry-test () :report "Retry the test." 
		       (go :test-start)))
       :test-end)
      (maybe-push-result))
    (when *lift-report-pathname*
      (let ((current (first (tests-run result))))
	(summarize-single-test  
	 :save (first current) (second current) (third current)
	 :stream *lift-report-pathname*))))
  (setf *current-test-case-name* name
	*test-result* result))

(defun testcase-expects-error-p (&optional (test *current-test*))
  (let* ((options (getf (test-data test) :options)))
    (or (testsuite-expects-error test)
	(second (member :expected-error options)))))

(defun testcase-expects-failure-p (&optional (test *current-test*))
  (let* ((options (getf (test-data test) :options)))
    (or (testsuite-expects-failure test)
	(second (member :expected-failure options)))))

(defun testcase-expects-problem-p (&optional (test *current-test*))
  (let* ((options (getf (test-data test) :options)))
    (second (member :expected-problem options))))

(defun check-for-surprises (testsuite)
  (let* ((expected-failure-p (testcase-expects-failure-p testsuite))
	 (expected-error-p (testcase-expects-error-p testsuite))
	 (expected-problem-p (testcase-expects-problem-p testsuite))
	 (condition nil))
    (cond 
      (expected-failure-p
       (setf (slot-value testsuite 'expected-failure-p) expected-failure-p))
      (expected-error-p
       (setf (slot-value testsuite 'expected-error-p) expected-error-p))
      (expected-problem-p
       (setf (slot-value testsuite 'expected-problem-p) expected-problem-p)))
    (cond
      ((expected-failure-p testsuite)
       (setf condition 
	     (make-condition 'unexpected-success-failure
			     :expected :failure
			     :expected-more (expected-failure-p testsuite))))
      ((expected-error-p testsuite)
       (setf condition 
	     (make-condition 'unexpected-success-failure
			     :expected :error
			     :expected-more (expected-error-p testsuite))))
      ((expected-problem-p testsuite)
       (setf condition 
	     (make-condition 'unexpected-success-failure
			     :expected :problem
			     :expected-more (expected-problem-p testsuite)))))
    (when condition
      (if (find-restart 'ensure-failed)
	  (invoke-restart 'ensure-failed condition)
	  (warn condition)))))
	
(defun report-test-problem (problem-type result suite method condition
			    &rest args)
  ;; ick
  (let ((docs nil)
	(option nil))
    (declare (ignorable docs option))
    (cond ((and (eq problem-type 'test-failure)
		(not (typep condition 'unexpected-success-failure))
		(testcase-expects-failure-p suite))
	   (setf problem-type 'test-expected-failure 
		 option :expected-failure))
	  ((and (eq problem-type 'test-error)
		(testcase-expects-error-p suite))
	   (setf problem-type 'test-expected-error
		 option :expected-error))
	  ((and (or (eq problem-type 'test-failure) 
		    (eq problem-type 'test-error))
		(testcase-expects-problem-p suite))
	   (setf problem-type (or (and (eq problem-type 'test-failure) 
				       'test-expected-failure)
				  (and (eq problem-type 'test-error)
				       'test-expected-error))
		 option :expected-problem)))
    (let ((problem (apply #'make-instance problem-type
			  :testsuite suite
			  :test-method method 
			  :test-condition condition
			  :test-step (current-step suite) args)))
      (setf (getf (test-data suite) :problem) problem)
      (etypecase problem
	((or test-failure testsuite-failure) (push problem (failures result)))
	(test-expected-failure (push problem (expected-failures result)))
	((or test-error testsuite-error) (push problem (errors result)))
	(test-expected-error (push problem (expected-errors result))))
      (when (and *test-maximum-failure-count*
		 (numberp *test-maximum-failure-count*)
		 (>= (length (failures result)) *test-maximum-failure-count*))
	(cancel-testing :failures))
      (when (and *test-maximum-error-count*
		 (numberp *test-maximum-error-count*)
		 (>= (length (errors result)) *test-maximum-error-count*))
	(cancel-testing :errors))
      problem)))

(defun cancel-testing (why)
  (declare (ignore why))
  (flet ((do-it (name)
	   ;; should just use find-restart but I was experimenting
	   (let* ((restarts (compute-restarts))
		  (it (find name restarts :key #'restart-name :from-end nil)))
	     (when it 
	       (invoke-restart it)))))
    (do-it 'cancel-testing-from-configuration)
    (do-it 'cancel-testing)))

;;; ---------------------------------------------------------------------------
;;; test-result and printing
;;; ---------------------------------------------------------------------------

(defun get-test-print-length ()
  (let ((foo *test-print-length*))
    (if (eq foo :follow-print) *print-length* foo)))

(defun get-test-print-level ()
  (let ((foo *test-print-level*))
    (if (eq foo :follow-print) *print-level* foo)))

(defmethod start-test ((result test-result) (suite test-mixin) name) 
  (declare (ignore name))
  (setf (current-step suite) :start-test
	(test-data suite) 
	`(:start-time ,(get-internal-real-time)
	  :start-time-universal ,(get-universal-time))))

(defmethod end-test ((result test-result) (suite test-mixin) name)
  (declare (ignore name))
  (setf (current-step suite) :end-test
	(getf (test-data suite) :end-time) (get-internal-real-time)
	(end-time result) (get-internal-real-time)
	(getf (test-data suite) :end-time-universal) (get-universal-time)
	(end-time-universal result) (get-universal-time)))

(defun make-test-result (for test-mode &rest args)
  (apply #'make-instance 'test-result
	 :results-for for
	 :test-mode test-mode 
	 args))

(defun testing-interactively-p ()
  (values nil))

(defmethod print-object ((tr test-result) stream)
  (let ((complete-success? (and (null (errors tr))
                                (null (failures tr))
				(null (expected-failures tr))
				(null (expected-errors tr)))))
    (let* ((*print-level* (get-test-print-level))
           (*print-length* (get-test-print-length))
	   (non-failure-failures
	    (count-if 
	     (lambda (failure) 
	       (member (class-of (test-condition failure))
		       (subclasses 'unexpected-success-failure :proper? nil)))
	     (expected-failures tr)))
	   (expected-failures (- (length (expected-failures tr))
				 non-failure-failures)))	     
      (print-unreadable-object (tr stream)
        (cond ((and (null (tests-run tr)) complete-success?)
               (format stream "~A: no tests defined" (results-for tr)))
              ((eq (test-mode tr) :single)
               (cond ((test-interactive? tr)
                      ;; interactive
                      (cond (complete-success?
                             (format stream "Test passed"))
                            ((errors tr)
                             (format stream "Error during testing"))
                            ((expected-errors tr)
                             (format stream "Expected error during testing"))
                            ((failures tr)
                             (format stream "Test failed"))
			    ((plusp non-failure-failures)
                             (format stream "Test succeeded unexpectedly"))
                            (t
                             (format stream "Test failed expectedly"))))
                     (t
                      ;; from run-test
                      (format stream "~A.~A ~A" 
                              (results-for tr) 
                              (first (first (tests-run tr)))
                              (cond (complete-success?
                                     "passed")
                                    ((errors tr)
                                     "Error")
                                    (t
                                     "failed")))
		      (when (or (expected-errors tr) (expected-failures tr))
			(format stream "(~[~:;, ~:*~A expected failure~:P~]~[~:;, ~:*~A succeeded unexpectedly~]~[~:;, ~:*~A expected error~:P~])" 
				expected-failures non-failure-failures
				(expected-errors tr))))))
              (t
               ;; multiple tests run
               (format stream "Results for ~A " (results-for tr))
               (if complete-success?
                 (format stream "[~A Successful test~:P]"
                         (length (tests-run tr)))
                 (format stream "~A Test~:P~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Error~:P~]~[~:;, ~:*~A Expected failure~:P~]~[~:;, ~:*~A Expected error~:P~]" 
                         (length (tests-run tr))
                         (length (failures tr))
                         (length (errors tr))
                         (length (expected-failures tr))
                         (length (expected-errors tr))))))
        ;; note that suites with no tests think that they are completely 
        ;; successful. Optimistic little buggers, huh?
        (when (and (not complete-success?) *test-describe-if-not-successful?*)
          (format stream "~%") 
          (print-test-result-details stream tr t t))))))

(defmethod describe-object ((result test-result) stream)
  (describe-test-result result stream))

(defmethod describe-test-result (result stream 
				 &key
				 (show-details-p *test-show-details-p*)
				 (show-expected-p *test-show-expected-p*)
				 (show-code-p *test-show-code-p*))
  (let* ((number-of-failures (length (failures result)))
	 (number-of-errors (length (errors result)))
	 (number-of-expected-errors (length (expected-errors result)))
	 (non-failure-failures
	  (count-if 
	   (lambda (failure) 
	     (member (class-of (test-condition failure))
		     (subclasses 'unexpected-success-failure :proper? nil)))
	   (expected-failures result)))
	 (number-of-expected-failures (- (length (expected-failures result))
					 non-failure-failures))
	 (*print-level* (get-test-print-level))
	 (*print-length* (get-test-print-length)))
    (unless *test-is-being-defined?*
      (print-test-summary result stream)
      (when (and show-details-p
		 (or (plusp number-of-failures)
                     (plusp number-of-expected-failures)
	             (plusp number-of-errors)
                     (plusp number-of-expected-errors)))
	(format stream "~%~%")             
	(print-test-result-details
	 stream result show-expected-p show-code-p)
	(print-test-summary result stream)))))

(defun print-test-summary (result stream)
  (let* ((number-of-failures (length (failures result)))
	 (number-of-errors (length (errors result)))
	 (number-of-expected-errors (length (expected-errors result)))
	 (non-failure-failures
	  (count-if 
	   (lambda (failure) 
	     (member (class-of (test-condition failure))
		     (subclasses 'unexpected-success-failure :proper? nil)))
	   (expected-failures result)))
	 (number-of-expected-failures (- (length (expected-failures result))
					 non-failure-failures)))	     
    (format stream "~&Test Report for ~A: ~D test~:P run" 
	    (results-for result) (length (tests-run result)))
    (cond ((or (failures result) (errors result)
	       (expected-failures result) (expected-errors result))
	   (format stream "~[~:;, ~:*~A Error~:P~]~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Expected error~:P~]~[~:;, ~:*~A Expected failure~:P~]~[~:;, ~:*~A Successful Surprise~:P~]." 
		   number-of-errors
		   number-of-failures
		   number-of-expected-errors
		   number-of-expected-failures
		   non-failure-failures))
	  ((or (expected-failures result) (expected-errors result))
	   (format stream ", all passed *~[~:;, ~:*~A Expected error~:P~]~[~:;, ~:*~A Expected failure~:P~])." 
		   number-of-expected-errors
		   number-of-expected-failures))
	  (t
	   (format stream ", all passed!")))))

(defun print-test-result-details (stream result show-expected-p show-code-p)
  (loop for report in (errors result) do
       (print-test-problem "ERROR  : " report stream
			   show-code-p))  
  (loop for report in (failures result) do
       (print-test-problem "Failure: " report stream 
			   show-code-p))
  (when show-expected-p
    (loop for report in (expected-failures result) do
	 (print-test-problem "Expected failure: " report stream
			     show-code-p))
    (loop for report in (expected-errors result) do
	 (print-test-problem "Expected Error : " report stream
			     show-code-p))))

(defun print-test-problem (prefix report stream show-code-p)
  (let* ((suite (testsuite report))
         (method (test-method report))
         (condition (test-condition report))
         (code (test-report-code suite method))
         (testsuite-name method)	 
	 (*print-level* (get-test-print-level))
	 (*print-length* (get-test-print-length)))
    (let ((*package* (symbol-package method)))
      (format stream "~&~A~(~A : ~A~)" prefix (type-of suite) testsuite-name)
      (let ((doc-string (gethash testsuite-name
				 (test-case-documentation 
				  (class-name (class-of suite))))))
	(when doc-string 
	  (format stream "~&~A" doc-string)))
      (if show-code-p
	  (setf code (with-output-to-string (out)
		       (pprint code out)))
	  (setf code nil))
      (format stream "~&~<  ~@;~
                    ~@[Condition: ~<~@;~A~:>~]~
                    ~@[~&Code     : ~a~]~
                    ~&~:>" (list (list condition) code)))))


;;; ---------------------------------------------------------------------------
;;; test-reports
;;; ---------------------------------------------------------------------------

(defclass test-problem-mixin ()
  ((testsuite :initform nil :initarg :testsuite :reader testsuite)
   (test-method :initform nil :initarg :test-method :reader test-method)
   (test-condition :initform nil
		   :initarg :test-condition 
		   :reader test-condition)
   (test-problem-kind :reader test-problem-kind :allocation :class)
   (test-step :initform nil :initarg :test-step :reader test-step)))

(defmethod print-object ((problem test-problem-mixin) stream)
  (print-unreadable-object (problem stream)
    (format stream "TEST-~@:(~A~): ~A in ~A" 
            (test-problem-kind problem) 
            (name (testsuite problem))
	    (test-method problem))))

(defclass generic-problem (test-problem-mixin)
  ((test-problem-kind :initarg :test-problem-kind
		      :allocation :class)))

(defclass expected-problem-mixin ()
  ((documentation :initform nil 
		  :initarg :documentation
		  :accessor failure-documentation)))

(defclass test-expected-failure (expected-problem-mixin generic-problem)
  ()
  (:default-initargs 
   :test-problem-kind "Expected failure"))

(defclass test-failure (generic-problem)
  ()
  (:default-initargs 
   :test-problem-kind "failure"))

(defclass test-error-mixin (generic-problem) 
  ((backtrace :initform nil :initarg :backtrace :reader backtrace)))
  
(defclass test-expected-error (expected-problem-mixin test-error-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Expected error"))

(defclass test-error (test-error-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Error"))

(defclass testsuite-error (test-error-mixin)
  ()
  (:default-initargs 
   :test-problem-kind "Testsuite error"))

(defclass testsuite-failure (generic-problem)
  ()
  (:default-initargs 
   :test-problem-kind "Testsuite failure"))

(defmethod test-report-code ((testsuite test-mixin) (method symbol))
  (let* ((class-name (class-name (class-of testsuite))))
    (gethash method
             (test-name->code-table class-name))))

;;; ---------------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------------

(defun remove-test-methods (test-name)
  (prog1
      (length (testsuite-tests test-name))
    (setf (testsuite-tests test-name) nil)))

(defun remove-previous-definitions (classname)
  "Remove the methods of this class and all its subclasses."
  (let ((classes-removed nil)
        (class (find-class classname nil))
        (removed-count 0))
    (when class
      (loop for subclass in (subclasses class :proper? nil) do
            (push subclass classes-removed)
            (incf removed-count
                  (remove-test-methods (class-name subclass)))
            #+Ignore
            ;;?? causing more trouble than it solves...??
            (setf (find-class (class-name subclass)) nil))
      
      (unless (length-1-list-p classes-removed)
        (format *debug-io* 
                "~&;;; Removed Test suite ~(~A~) and its subclasses (~{~<~s~>~^, ~})."
                classname (sort 
                           (delete classname 
				   (mapcar #'class-name classes-removed))
                           #'string-lessp)))
      (unless (zerop removed-count)
        (format *debug-io* 
                "~&;;; Removed ~D methods from test suite ~(~A~)~@[ and its subclasses~]."
                removed-count classname 
		(not (length-1-list-p classes-removed)))))))

(defun (setf test-environment-value) (value name)
  (push (cons name value) *test-environment*)
  (values value))

(defun test-environment-value (name)
  (cdr (assoc name *test-environment*)))

(defun remove-from-test-environment (name)
  (setf *test-environment* 
        (remove name *test-environment* :key #'car :count 1)))

(defun build-test-local-functions ()
  `(progn
     ,@(mapcar 
	(lambda (function-spec)
	  (destructuring-bind (name arglist &body body) (first function-spec)
	    `(defmethod flet-test-function ((testsuite ,(def :testsuite-name))
					    (function-name (eql ',name))
					    &rest args)
	       (with-test-slots 
		 ,(if arglist
		      `(destructuring-bind ,arglist args
			 ,@body)
		      `(progn ,@body))))))
	(def :functions))))

(defun build-test-equality-test ()
  (let ((test-name (def :testsuite-name))
        (equality-test (def :equality-test)))
    `(progn
       (defmethod equality-test ((testsuite ,test-name))
	 ,equality-test))))

(defun build-testsuite-expected-error ()
  (let ((test-name (def :testsuite-name))
        (expected-error (def :expected-error)))
    `(progn
       (defmethod testsuite-expects-error ((testsuite ,test-name))
	 (with-test-slots
	   ,expected-error)))))

(defun build-testsuite-expected-failure ()
  (let ((test-name (def :testsuite-name))
        (expected-failure (def :expected-failure)))
    `(progn
       (defmethod testsuite-expects-failure ((testsuite ,test-name))
	 (with-test-slots
	   ,expected-failure)))))

(defun build-test-teardown-method ()
  (let ((test-name (def :testsuite-name))
        (slot-names (def :direct-slot-names))
        (teardown (def :teardown)))
    (when teardown
      (unless (consp teardown)
        (setf teardown (list teardown)))
      (when (length-1-list-p teardown)
        (setf teardown (list teardown)))
      (when (symbolp (first teardown))
        (setf teardown (list teardown))))
    (let* ((teardown-code `(,@(when teardown
                                `((with-test-slots ,@teardown)))))
           (test-code `(,@teardown-code
                        ,@(mapcar (lambda (slot)
                                    `(remove-from-test-environment ',slot))
                                  slot-names))))
      `(progn
         ,@(when teardown-code
             `((defmethod test-case-teardown progn ((testsuite ,test-name)
						    (result test-result))
		 (when (run-teardown-p testsuite :test-case)
		   ,@test-code))))
         ,@(when teardown-code
             `((defmethod testsuite-teardown ((testsuite ,test-name)
					      (result test-result))
                 (when (run-teardown-p testsuite :testsuite)
		   ,@test-code))))))))

(defun build-setup-test-method ()
  (let ((test-name (def :testsuite-name))
        (setup (def :setup)))
    ;;?? ewww, this smells bad
    (when setup
      (unless (consp setup)
        (setf setup (list setup)))
      (when (length-1-list-p setup)
        (setf setup (list setup)))
      (when (symbolp (first setup))
        (setf setup (list setup))))
    (let ((ginitargs (gensym "initargs-")))
      (multiple-value-bind (slots initforms)
	  (%gather-up-initforms)
	(when (or setup slots)
	  `(progn
	     (defmethod setup-test :after ((testsuite ,test-name))
	       (with-test-slots
		 ,@(when slots
			 `((let ((,ginitargs (suite-initargs testsuite)))
			     ,@(loop for slot-name in slots
				  for initform in initforms
				  for keyword = (intern (symbol-name slot-name)
							:keyword) 
				  collect
				    `(setf (test-environment-value ',slot-name)
					   (or (getf ,ginitargs ,keyword)
					       ,initform))))))
		 ,@setup))))))))

(defun %gather-up-initforms ()
  (let ((initforms nil)
        (slot-names nil)
        (slot-specs (def :slot-specs)))
    (loop for slot in (def :direct-slot-names)
       for spec = (assoc slot slot-specs) do
	 (push (getf (rest spec) :initform) initforms)
	 (push (first spec) slot-names))
    (values (nreverse slot-names) (nreverse initforms))))    

(defmethod setup-test :around ((test test-mixin))
  (when (run-setup-p test)
    (call-next-method)
    (setf (slot-value test 'done-setup?) t)))

(defun run-setup-p (testsuite)
  (case (run-setup testsuite)
    (:once-per-session (error "not implemented"))
    (:once-per-suite (not (done-setup? testsuite)))
    ((:once-per-test-case t) t)
    ((:never nil) nil)
    (t (error "Don't know about ~s for run-setup" (run-setup testsuite)))))

(defun run-teardown-p (testsuite when)
  (ecase when
    (:test-case
     (ecase (run-setup testsuite)
       (:once-per-session nil)
       (:once-per-suite nil)
       ((:once-per-test-case t) t)
       ((:never nil) nil)))
    (:testsuite
     (ecase (run-setup testsuite)
       (:once-per-session nil)
       (:once-per-suite t)
       ((:once-per-test-case t) nil)
       ((:never nil) nil)))))
     
(defun build-test-test-method (test-class test-body options)
  (multiple-value-bind (test-name body documentation name-supplied?)
                       (parse-test-body test-body)
    (declare (ignorable name-supplied?))
    (unless (consp (first body))
      (setf body (list body)))
    `(progn
       (setf (gethash ',test-name (test-name->code-table ',test-class)) ',body
             (gethash ',body (test-code->name-table ',test-class)) ',test-name)
       ,(when documentation
          `(setf (gethash ',test-name (test-case-documentation ',test-class))
                 ,documentation))
       #+(or mcl ccl)
       ,@(when name-supplied?
           `((ccl:record-source-file ',test-name 'test-case)))
       (unless (find ',test-name (testsuite-tests ',test-class))
	 (setf (testsuite-tests ',test-class)
	       (append (testsuite-tests ',test-class) (list ',test-name))))
       (defmethod lift-test ((testsuite ,test-class) (case (eql ',test-name)))
	 ,@(when options
		 `((setf (getf (test-data testsuite) :options) 
			 (list ,@(loop for (k v) on options by #'cddr append
				      (list k v))))))
	 (with-test-slots ,@body))
       (setf *current-test-case-name* ',test-name)
       (when (and *test-print-when-defined?*
                  (not (or *test-is-being-compiled?*
                           )))
         (format *debug-io* "~&;Test Created: ~(~S.~S~)." 
		 ',test-class ',test-name))
       *current-test-case-name*)))

(defun parse-test-body (test-body)
  (let ((test-name nil)
        (body nil)
        (parsed-body nil)
        (documentation nil)
        (test-number (1+ (testsuite-test-count *current-testsuite-name*)))
        (name-supplied? nil))
    ;; parse out any documentation
    (loop for form in test-body do
          (if (and (consp form)
                   (keywordp (first form))
                   (eq :documentation (first form)))
            (setf documentation (second form))
            (push form parsed-body)))
    (setf test-body (nreverse parsed-body))
    (setf test-name (first test-body))
    (cond ((symbolp test-name)
           (setf test-name 
		 (intern (format nil "~A" test-name))
                 body (rest test-body)
                 name-supplied? t))
          ((and (test-code->name-table *current-testsuite-name*)
                (setf test-name 
                 (gethash test-body
			  (test-code->name-table *current-testsuite-name*))))
           (setf body test-body))
          (t
           (setf test-name 
		 (intern (format nil "TEST-~A" 
				 test-number))
                 body test-body)))
    (values test-name body documentation name-supplied?)))

(defun build-test-class ()
  ;; for now, we don't generate code from :class-def code-blocks
  ;; they are executed only for effect.
  (loop for (nil . block) in *code-blocks* 
     when (and block 
	       (code block)
	       (eq (operate-when block) :class-def)
	       (or (not (filter block))
		   (funcall (filter block)))) collect
     (funcall (code block)))
  (unless (some (lambda (superclass)
		  (testsuite-p superclass))
		(def :superclasses))
    (pushnew 'test-mixin (def :superclasses)))
  ;; build basic class and standard class
  `(defclass ,(def :testsuite-name) (,@(def :superclasses))
     nil
     ,@(when (def :documentation)
	     `((:documentation ,(def :documentation))))
     (:default-initargs
	 :test-slot-names ',(def :slot-names)
       ,@(def :default-initargs))))

(defun parse-test-slots (slot-specs)
  (loop for spec in slot-specs collect
        (let ((parsed-spec spec))
          (if (member :initform parsed-spec)
            (let ((pos (position :initform parsed-spec)))
              (append (subseq parsed-spec 0 pos)
                      (subseq parsed-spec (+ pos 2))))
            parsed-spec))))

(defmethod testsuite-p ((classname symbol))
  (let ((class (find-class classname nil)))
    (handler-case
      (and class
           (typep (allocate-instance class) 'test-mixin)
	   classname)
      (error (c) (declare (ignore c)) (values nil)))))

(defmethod testsuite-p ((object standard-object))
  (testsuite-p (class-name (class-of object))))

(defmethod testsuite-p ((class standard-class))
  (testsuite-p (class-name class)))

(defmethod testsuite-methods ((classname symbol))
  (testsuite-tests classname))

(defmethod testsuite-methods ((test test-mixin))
  (testsuite-methods (class-name (class-of test))))

(defmethod testsuite-methods ((test standard-class))
  (testsuite-methods (class-name test)))


;; some handy properties
(defclass-property test-slots)
(defclass-property test-code->name-table)
(defclass-property test-name->code-table)
(defclass-property test-case-documentation)
(defclass-property testsuite-tests)
(defclass-property testsuite-dynamic-variables)

;;?? issue 27: break encapsulation of code blocks
(defclass-property testsuite-function-specs)

(defun empty-test-tables (test-name)
  (when (find-class test-name nil)
    (setf (test-code->name-table test-name)
          (make-hash-table :test #'equal)
          (test-name->code-table test-name)
          (make-hash-table :test #'equal)
          (test-case-documentation test-name)
          (make-hash-table :test #'equal))))

(pushnew :timeout *deftest-clauses*)

(add-code-block
 :timeout 1 :class-def
 (lambda () (def :timeout)) 
 '((setf (def :timeout) (cleanup-parsed-parameter value)))
 (lambda ()
   (unless (some (lambda (super)
		   (member (find-class 'process-test-mixin)
			   (superclasses super)))
		 (def :superclasses))
     (pushnew 'process-test-mixin (def :superclasses)))
   (push (def :timeout) (def :default-initargs))
   (push :maximum-time (def :default-initargs))
   nil))

(defclass process-test-mixin (test-mixin)
  ((maximum-time :initform *test-maximum-time* 
                 :accessor maximum-time
                 :initarg :maximum-time)))

(defclass test-timeout-failure (test-failure)
  ((test-problem-kind :initform "Timeout" :allocation :class)))

(defmethod lift-test :around ((suite test-mixin) name)
  (if (profile suite)
      (with-profile-report ((format nil "~a-~a" 
				    (testsuite-name suite) name) 
			    (profile suite))
	(call-next-method))
      (call-next-method)))

(defmethod do-testing :around ((testsuite process-test-mixin) result fn)
  (declare (ignore fn))
  (handler-case
      (with-timeout ((maximum-time testsuite))
	(call-next-method))
    (timeout-error 
	(c)
      (declare (ignore c))
      (report-test-problem
       'test-timeout-failure result testsuite (current-method testsuite)
       (make-instance 'test-timeout-condition
		      :maximum-time (maximum-time testsuite))))))     

;;?? might be "cleaner" with a macrolet (cf. lift-result)
(defun lift-property (name)
  (when *current-test*
    (getf (getf (test-data *current-test*) :properties) name)))

#+(or)
(setf (getf (getf (third (first (tests-run *test-result*))) :properties) :foo)
      3)

(defun (setf lift-property) (value name)
  (when *current-test*
    (setf (getf (getf (test-data *current-test*) :properties) name) value)))


#+Later
(defmacro with-test (&body forms)
  "Execute forms in the context of the current test class."
  (let* ((testsuite-name *current-testsuite-name*)
         (test-case (make-instance test-class)))
    `(eval-when (:execute)
       (prog2
        (setup-test ,test-case)
        (progn
          (with-test-slots ,@forms))
        (test-case-teardown ,test-case result)))))
