(in-package #:lift)

(defvar *measures* nil
  "A list of defineded measures")

(defparameter *benchmark-log-path*
  (asdf:system-relative-pathname 
   'lift "benchmark-data/benchmarks.log"))

(defvar *count-calls-p* nil)

(defmacro undefmeasure (name)
  (let ((gname (gensym "name-")))
    `(let ((,gname ,(form-keyword name)))
       (if (find ,gname *measures* :key #'first)
	   (setf *measures* (remove ,gname *measures* :key #'first))
	   (error "Measure ~a not found." ,gname))
       ,gname)))
  
(defmacro defmeasure (name &key (value nil) (finally nil) (type nil)
		      (documentation nil))
  (declare (ignore documentation))
  (unless value
    (error "A value must be specified to define a measure."))
  (cond ((atom name)
	 ;; all is well
	 )
	((and (eq (first name) 'quote)
	      (eq (length name) 2))
	 (warn "Name does not need to be quoted.")
	 (setf name (second name)))
	(t
	 (error "Name should be a symbol.")))
  (cond ((eq (first finally) 'quote)
	 (setf finally (second finally))))
  (let ((gname (gensym "name-")))
    `(let ((,gname ,(form-keyword name)))
       (setf *measures* (remove ,gname *measures* :key #'first))
       (push (list ,gname
		   :value ,value 
		   :finally ',finally
		   :type ',type)		   
	     *measures*)
       ,gname)))

(defmacro while-measuring ((catch-errors-p &rest measures) &body body)
  (let ((vars (loop for measure in measures collect
		   (gensym (format nil "~a-" measure))))
	(gcondition (gensym "condition-"))
	(gresult (gensym "result-"))
	(gcatch-errors-p (gensym "catch-errors-p-")))
    (labels ((measure-1 (vars measures)
	       (cond ((null measures) body)
		     (t
		      `((while-measuring-1 (,(first vars) ,(first measures))
			 ,@(measure-1 (rest vars) (rest measures))))))))
      `(let ((,gcondition nil)
	     (,gresult nil)
	     (,gcatch-errors-p ,catch-errors-p)
	     ,@vars)
	 (setf ,gresult
	       (handler-case 
		   ,@(measure-1 vars measures)
		 (error (c)
		   (setf ,gcondition c)
		   (unless ,gcatch-errors-p
		     (error c)))))
	 (values ,gresult (list ,@vars) ,gcondition)))))

#+notyet
;; returns error as third value (and probably as first too!)
(defmacro while-measuring ((&rest measures) (&body error-body)
			   &body body)
  (let ((vars (loop for measure in measures collect
		   (gensym (format nil "~a-" measure))))
	(gcondition (gensym "condition-"))
	(gresult (gensym "result-")))
    (labels ((measure-1 (vars measures)
	       (cond ((null measures) body)
		     (t
		      `((while-measuring-1 (,(first vars) ,(first measures))
			 ,@(measure-1 (rest vars) (rest measures))))))))
      `(let ((,gcondition nil)
	     (,gresult nil)
	     ,@vars)
	 (setf ,gresult
	       (handler-case 
		   ,@(measure-1 vars measures)
		 (error (c) 
		   ,@(if error-body
			 `((progn ,error-body
				 (error c)))
			 `((setf ,gcondition c))))))
	 (values ,gresult (list ,@vars) ,gcondition)))))

#+(or) 
(while-measuring (space seconds)
    nil
  (sleep 1)
  (signal "hi"))

#+(or)
(measure-time-and-conses 
  (sleep 1)
  (signal "hi"))

(defmacro while-measuring-1 ((var measure) &body body)
  (let ((ginitial (gensym "value-"))
	(gresult (gensym "result-"))
	(metadata (find (form-keyword measure) *measures* :key 'first)))
    (unless metadata
      (error "Measure `~a` not defined." measure))
    (destructuring-bind (&key value finally type &allow-other-keys) 
	(rest metadata)
      `(let ((,ginitial (,value))
	     (,gresult nil))
	 ,@(when type
		 `((declare (type ,type ,ginitial))))
	 (unwind-protect
	      (setf ,gresult (progn ,@body))
	   (setf ,var ,@(if finally
			    `((funcall (lambda (it) ,finally)
				       (- (,value) ,ginitial)))
			    `((- ,(if type `(the ,type (,value)) `(,value))
				 ,ginitial)))))
	 ,gresult))))

(defmacro with-profile-report 
    ((name style &key 
	   (log-name *benchmark-log-path* ln-supplied?)
	   (count-calls-p *count-calls-p* ccp-supplied?)
	   (timeout nil timeout-supplied?))
     &body body)
  `(with-profile-report-fn 
       ,name ,style 
       (lambda () (progn ,@body))
       ',body
       ,@(when ccp-supplied? 
	       `(:count-calls-p ,count-calls-p))
       ,@(when ln-supplied?
	       `(:log-name ,log-name))
       ,@(when (and timeout-supplied? timeout)
	       `(:timeout ,timeout))))

(defmacro while-counting-repetitions ((&optional (delay 1.0)) &body body)
  "Execute `body` repeatedly for `delay` seconds. Returns the number
of times `body` is executed per second. Warning: assumes that `body` will not
be executed more than a fixnum number of times. The `delay` defaults to
1.0."
  (let ((gevent-count (gensym "count-"))
	(gdelay (gensym "delay-"))
	(gignore (gensym "ignore-"))
	(gfn (gensym "fn-")))
    `(let ((,gfn
	    (compile
	     nil
	     (lambda () 
	       (let ((,gevent-count 0)
		     (,gdelay ,delay))
		 (declare (type fixnum ,gevent-count))
		 (handler-case
		     (lift::with-timeout (,gdelay)
		       (loop
			  (progn ,@body)
			  (setf ,gevent-count (the fixnum (1+ ,gevent-count)))))
		   (lift::timeout-error (,gignore)
		     (declare (ignore ,gignore))
		     (if (plusp ,gevent-count)
			 (float (/ ,gevent-count ,gdelay))
			 ,gevent-count))))))))
	   (funcall ,gfn))))
  
(defmacro while-counting-events ((&optional (delay 1.0)) &body body)
  "Returns the count of the number of times `did-event` was called during 
`delay` seconds. See also: [while-counting-repetitions][]."
  (let ((gevent-count (gensym "count")))
    `(let ((,gevent-count 0))
       (flet ((did-event ()
		(incf ,gevent-count)))
	 (declare (type fixnum ,gevent-count)
		  (ignorable (function did-event)))
	 (handler-case
	     (with-timeout (,delay) 
	       (loop  
		  (progn ,@body)))
	   (timeout-error (c)
	     (declare (ignore c))
	     (float (/ ,gevent-count ,delay))))))))  

;; stolen from metatilities
(defmacro muffle-redefinition-warnings (&body body)
  "Evaluate the body so that redefinition warnings will not be 
signaled. (suppored in Allegro, Clozure CL, CLisp, and Lispworks)"
  #+allegro
  `(excl:without-redefinition-warnings
     ,@body)
  #+(or ccl mcl)
  `(let ((ccl::*warn-if-redefine* nil)
	 ;;?? FIXME not sure if this should be here or not...
	 (ccl::*record-source-file* nil))
     ,@body)
  #+clisp
  `(let ((custom:*suppress-check-redefinition* t))
    ,@body)
  #+lispworks
  `(let ((lw:*handle-warn-on-redefinition* :quiet))
    ,@body)
  #+sbcl
  ;; from http://www.sbcl.info/manual/Controlling-Verbosity.html
  `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note
					       sb-ext::style-warning))
    ,@body)
  #-(or allegro ccl clisp mcl sbcl)
  `(progn ,@body))


(defmacro defconfig-variable (name var &optional docstring)
  (declare (ignore docstring))
  `(defmethod handle-config-preference ((name (eql ,name)) args)
     (setf ,var (first args))))

(defmacro defconfig (name &body body)
  (let ((docstring nil))
    (declare (ignorable docstring))
    (when (stringp (first body))
      (setf docstring (first body)
	    body (rest body)))
    `(defmethod handle-config-preference ((name (eql ,name)) args)
       ,@body)))
