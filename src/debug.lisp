(in-package :web4r)

; --- Util ------------------------------------------------------

(defun ps (instance)
  "Prints each slot's name and value of the INSTANCE for debugging"
  (loop for slot in (ele::class-slots (class-of instance))
        as name = (ele::slot-definition-name slot)
        do (print (list name
                        (or (ignore-errors
                              (slot-value instance name))
                            :unbound)))))

(defmacro with-post-parameters (parameters &rest body)
  "Executes the BODY with the alist of post PARAMETERS"
  `(let* ((*acceptor* (make-instance 'acceptor))
          (*reply*    (make-instance (acceptor-reply-class *acceptor*)))
          (*request*  (make-instance 'request :headers-in '())))
     (set-post-parameters ,parameters)
     ,@body))

(defmacro with-get-parameters (parameters &rest body)
  "Executes the BODY with the alist of get PARAMETERS"
  `(let* ((*acceptor* (make-instance 'acceptor))
          (*reply*    (make-instance (acceptor-reply-class *acceptor*)))
          (*request*  (make-instance 'request :headers-in '())))
     (set-get-parameters ,parameters)
     ,@body))

; --- Logging ---------------------------------------------------

(defun debug-log (&rest contents)
  "Write a log CONTENTS to the *debug-log-file*"
  (let ((*message-log-pathname* *debug-log-file*))
    (log-message :debug (join " " contents))))

(defun backtrace-log (error)
  (when *debug-mode*
    (with-open-file (s *debug-log-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
      (trivial-backtrace::print-backtrace error :output s))))

; --- Debug mode ------------------------------------------------

(defun debug-mode-on ()
  "Turns on debug-mode"
  (setf *debug-mode* t
        *show-lisp-errors-p* t))

(defun debug-mode-off ()
  "Turns off debug-mode"
  (setf *debug-mode* nil
        *show-lisp-errors-p* nil))
