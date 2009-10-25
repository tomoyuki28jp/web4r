(in-package #:lift)

(defgeneric find-testsuite (suite &key errorp)
  (:documentation "Search for a testsuite named `suite`. 

The search is conducted across all packages so `suite` can be 
a symbol in any package. I.e., find-testsuite looks for testsuite 
classes whose symbol-name is string= to `suite`. If `errorp` is 
true, then find-testsuite can raise two possible errors:

 * If more than one matching testsuite is found, 
then an error of type `testsuite-ambiguous` will be raised. 
 * If no matching testsuites are found, then an error of type 
`testsuite-not-defined` will be raised. 

The default for `errorp` is nil."))

(defgeneric find-test-case (suite name &key errorp)
  (:documentation "Search for a test-case named `name` in a 
testsuite named `suite`. 

The search is conducted across all packages so `suite` and `name` 
can be symbols in any package. I.e., find-test-case looks for a
testsuites and test-cases whose symbol-names are string= to
`suite` and `name`. If `errorp` is 
true, then find-test-case can raise two possible errors:

 * If more than one matching test-case is found, 
then an error of type `test-case-ambiguous` will be raised. 
 * If no matching test-cases are found, then an error of type 
`test-case-not-defined` will be raised. 

The default for `errorp` is nil. If `suite` is nil, then
find-test-case will search for matching test-cases across 
all suites. This is equivalent to the behavior of [find-test-cases][]."))

(defgeneric find-test-cases (name &key errorp)
  )

;;;;;
;; some introspection

(defun liftpropos (string &key (include-cases? nil))
  "Returns a list of testsuites whose name contains `string`."
  (let ((result nil)
	(name-as-string (ensure-string string)))
    (flet ((add-if-match (suite-name &optional (to-add suite-name))
	     (when (search name-as-string (ensure-string suite-name)
			   :test #'char-equal)
	       (push to-add result))))
      (map-testsuites
       (lambda (suite level)
	 (declare (ignore level))
	 (let ((suite-name (class-name suite)))
	   (add-if-match suite-name)
	   (when include-cases?
	     (loop for method-name in (testsuite-tests suite-name) do
		  (add-if-match 
		   method-name (cons suite-name method-name))))))
       'test-mixin))
    (sort result #'string-lessp :key (lambda (it)
				       (typecase it
					 (atom it)
					 (cons (cdr it)))))))

(defun map-testsuites (fn start-at)
  (let ((visited (make-hash-table)))
    (labels ((do-it (suite level)
	       (unless (gethash suite visited)
		 (setf (gethash suite visited) t)
		 (funcall fn suite level)
		 (loop for subclass in (subclasses suite :proper? t) do
		      (do-it subclass (1+ level))))))
    (do-it (find-class (find-testsuite start-at) nil) 0))))

(defun testsuites (&optional (start-at 'test-mixin))
  "Returns a list of testsuite classes. The optional parameter provides
control over where in the test hierarchy the search begins."
  (let ((result nil))
    (map-testsuites (lambda (suite level)
		      (declare (ignore level))
		      (push suite result))
		    start-at)
    (nreverse result)))

(defun print-tests (&key (include-cases? t) (start-at 'test-mixin) (stream t))
  "Prints all of the defined test classes from :start-at on down." 
  (map-testsuites
   (lambda (suite level)
     (let ((indent (coerce (make-list (* level 3) :initial-element #\Space)
			   'string))
	   (name (class-name suite)))
       (format stream "~&~a~s (~:d)" 
	       indent
	       name
	       (length (testsuite-methods name)))
       (when include-cases?
	 (loop for method-name in (testsuite-tests name) do
	      (format stream "~&~a  ~a" indent method-name)))))
   start-at))
     
(defun list-tests (&key (include-cases? t) (start-at 'test-mixin) (stream t))
  "Lists all of the defined test classes from :start-at on down." 
  (mapc (lambda (subclass)
	  (let ((subclass-name (class-name subclass)))
	    (format stream "~&~s (~:d)" 
		    subclass-name
		    (length (testsuite-methods subclass-name)))
	    (when include-cases?
	      (loop for method-name in (testsuite-tests subclass-name) do
		   (format stream "~&  ~a" method-name)))))
        (testsuites start-at))
  (values))

(defun testsuite-test-count (testsuite)
  (or (and *testsuite-test-count* 
           (prog1 *testsuite-test-count* (incf *testsuite-test-count*))) 
      (length (testsuite-methods testsuite))))

(defmethod find-testsuite ((suite symbol) &key (errorp nil))
  (or (testsuite-p suite)
      (find-testsuite (symbol-name suite) :errorp errorp)))

(defmethod find-testsuite ((suite-name string) &key (errorp nil))
  (let* ((temp nil)
	 (possibilities (remove-duplicates 
			 (loop for p in (list-all-packages) 
			    when (and (setf temp (find-symbol suite-name p))
				      (find-class temp nil)
				      (subtypep temp 'test-mixin)) collect
			    temp))))
    (cond ((null possibilities) 
	   (when errorp
	     (error 'testsuite-not-defined :testsuite-name suite-name)))
	  ((= (length possibilities) 1)
	   (first possibilities))
	  (t 
	   (if errorp
	     (error 'testsuite-ambiguous
		    :testsuite-name suite-name 
		    :possible-matches possibilities))
	   possibilities))))
			     
(defun test-case-p (suite-class name)
  (find-method #'lift-test nil `(,suite-class (eql ,name)) nil)) 

#+(or)
(test-case-p 
 (find-class (find-testsuite 'test-cluster-indexing-locally) nil)
 'db.agraph.tests::index-them)

#+(or)
(find-test-case (find-class (find-testsuite 'test-cluster-indexing-locally))
		'index-themxx)

(defmethod find-test-case ((suite symbol) name &key (errorp nil))
  (find-test-case (find-class (find-testsuite suite)) name :errorp errorp)) 

(defmethod find-test-case ((suite null) name &key (errorp nil))
  (find-test-cases name :errorp errorp)) 

(defmethod find-test-case ((suite test-mixin) name &key (errorp nil))
  (find-test-case (class-of suite) name :errorp errorp))

(defmethod find-test-case ((suite-class standard-class) (name symbol)
			    &key (errorp nil))
  (or (and (test-case-p suite-class name) name)
      (find-test-case suite-class (symbol-name name) :errorp errorp)))

(defmethod find-test-case ((suite test-mixin) (name string)
			   &key (errorp nil))
  (find-test-case (class-of suite) name :errorp errorp))

(defmethod find-test-case ((suite-class standard-class) (name string)
			    &key (errorp nil))
  (let* ((temp nil)
	 (possibilities (remove-duplicates 
			 (loop for p in (list-all-packages) 
			    when (and (setf temp (find-symbol name p))
				      (test-case-p suite-class temp)) collect
			    temp))))
    (cond ((null possibilities) 
	   (when errorp
	     (error 'test-case-not-defined 
		    :testsuite-name suite-class :test-case-name name)))
	  ((= (length possibilities) 1)
	   (first possibilities))
	  (t 
	   (when errorp
	     (error 'test-case-ambiguous
		    :testsuite-name suite-class
		    :test-case-name name
		    :possible-matches possibilities))))))
			     
(defmethod find-test-cases ((name symbol) &key (errorp nil))
  (find-test-cases (symbol-name name) :errorp errorp))

(defmethod find-test-cases ((name string) &key (errorp nil))
  (let ((result nil))
    (dolist (testsuite (testsuites))
      (let* ((suitename (class-name testsuite))
	     (testname (find-symbol name (symbol-package suitename))))
	(when (and testname 
		   (test-case-p testsuite testname))
	  (push (cons suitename testname) result))))
    (unless result
      (when errorp
	(error "not test-cases found")))
    result))

(defun last-test-status ()
  (cond ((typep *test-result* 'test-result)
	 (cond ((and (null (errors *test-result*))
		     (null (failures *test-result*)))
		:success)
	       ((and (errors *test-result*)
		     (failures *test-result*))
		:errors-and-failures)
	       ((errors *test-result*)
		:errors)
	       ((failures *test-result*)
		:failures)))
	(t
	 nil)))

(defun suite-tested-p (suite &key (result *test-result*))
  (and result
       (typep *test-result* 'test-result)
       (slot-exists-p result 'suites-run)
       (slot-boundp result 'suites-run)
       (consp (suites-run result))
       (find suite (suites-run result))))
