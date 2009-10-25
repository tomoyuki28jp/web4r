(in-package #:lift)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 1))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmeasure :measure-seconds 
    :value 'get-internal-real-time
    :finally '(coerce (/ it internal-time-units-per-second) 
			'double-float)
    :type integer
    :documentation 
    "Measure how long something takes using {hs get-internal-real-time}.

The accuracy can be no greater than {hs internal-time-units-per-second}.")

(defmeasure :measure-space
    :value 'total-bytes-allocated
    :type integer
    :documentation 
    "Measure how many conses cells a computation generates.")

)

#+(or)
(while-measuring-1 (conses measure-space)
  (while-measuring-1 (time measure-seconds)
    blay))

#+(or)
(let ((time 0))
  (while-measuring-1 (time measure-seconds)
    (sleep 1))
  time)

#+(or)
(let ((conses 0))
  (while-measuring-1 (conses measure-space)
    (sleep 1))
  conses)

#+(or)
(while-measuring (measure-seconds)
  (sleep 1))

(defmacro with-measuring ((var measure-fn) &body body)
  (let ((ginitial (gensym "value-"))
	(gcondition (gensym "condition-")))
    `(let ((,ginitial (,measure-fn))
	   (,gcondition nil))
       (prog1
	   (handler-case 
	       (progn ,@body)
	     (error (c) (setf ,gcondition c)))
	 (setf ,var (- (,measure-fn) ,ginitial))
	 (when ,gcondition (error ,gcondition))))))


(defmacro measure-time ((var) &body body)
  `(while-measuring-1 (,var measure-seconds) ,@body))

#+(or)
(let ((time 0))
  (while-measuring-1 (time measure-seconds)
    (sleep 1))
  time)

(defmacro measure-conses ((var) &body body)
  `(while-measuring-1 (,var measure-space) ,@body))

(defun measure-fn (fn &rest args)
  (declare (dynamic-extent args))
  (let ((bytes 0) (seconds 0) result)
    (measure-time (seconds)
      (measure-conses (bytes)
	(setf result (apply fn args))))
    (values seconds bytes result)))

(defmacro measure (seconds bytes &body body)
  (let ((result (gensym)))
    `(let (,result)
       (measure-time (,seconds)
	 (measure-conses (,bytes)
	   (setf ,result (progn ,@body))))
       (values ,result))))

(defmacro measure-time-and-conses (&body body)
  (let ((seconds (gensym))
	(conses (gensym))
	(results (gensym)))
    `(let ((,seconds 0) (,conses 0) ,results) 
       (setf ,results (multiple-value-list 
			    (measure ,seconds ,conses ,@body)))
       (values-list (nconc (list ,seconds ,conses)
			   ,results)))))

(defvar *functions-to-profile* nil)

(defvar *additional-markers* nil)

(defvar *profiling-threshold* nil)

(defun make-profiled-function (fn)
  (lambda (style count-calls-p)
    (declare (ignorable style count-calls-p))
    #+allegro
    (prof:with-profiling (:type style :count count-calls-p)
      (funcall fn))
    #-allegro
    (funcall fn)))

(defun generate-profile-log-entry (log-name name seconds conses results error)
  (ensure-directories-exist log-name)
  ;;log 
  (with-open-file (output log-name
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :append)
    (with-standard-io-syntax
      (let ((*print-readably* nil))
	(terpri output)
	(format output "\(~11,d ~20,s ~10,s ~10,s ~{~s~^ ~} ~s ~s ~a\)"
		(date-stamp :include-time? t) name 
		seconds conses *additional-markers*
		results (current-profile-sample-count)
		error)))))

(defun count-repetitions (fn delay &rest args)
  (declare (dynamic-extent args))
  (let ((event-count 0))
    (handler-case
	(with-timeout (delay) 
	  (loop  
	     (apply #'funcall fn args)
	     (incf event-count)))
      (timeout-error (c)
	(declare (ignore c))
	(if (plusp event-count)
	    (/ event-count delay)
	    event-count)))))

#+test
(defun fibo (n)
  (cond ((< n 2)
	 1)
	(t
	 (+ (fibo (- n 1)) (fibo (- n 2))))))

#+test
(with-profile-report ('test :time) 
  (loop for i from 1 to 10 do
       (fibo i))
  (loop for i from 10 downto 1 do
       (fibo i)))

