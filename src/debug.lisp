(in-package :web4r)

; --- Util ------------------------------------------------------

(defun ps (instance)
  "Prints names and values of the slots in the INSTANCE."
  (loop for slot in (ele::class-slots (class-of instance))
        as name = (ele::slot-definition-name slot)
        do (print (list name
                        (or (ignore-errors
                              (slot-value instance name))
                            :unbound)))))

(defmacro with-post-parameters (parameters &rest body)
  "Executes BODY with post PARAMETERS. PARAMETERS must be an alist
 of a key/value pairs."
  `(let* ((*acceptor* (make-instance 'acceptor))
          (*reply*    (make-instance (acceptor-reply-class *acceptor*)))
          (*request*  (make-instance 'request :headers-in '())))
     (set-post-parameters ,parameters)
     ,@body))

(defmacro with-get-parameters (parameters &rest body)
  "Executes BODY with get PARAMETERS. PARAMETERS must be an alist
 of a key/value pairs."
  `(let* ((*acceptor* (make-instance 'acceptor))
          (*reply*    (make-instance (acceptor-reply-class *acceptor*)))
          (*request*  (make-instance 'request :headers-in '())))
     (set-get-parameters ,parameters)
     ,@body))

; --- Logging ---------------------------------------------------

(defun debug-log (&rest contents)
  "Writes CONTENTS to the file, *debug-log-file*."
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
  "Turns on debug-mode."
  (setf *debug-mode* t
        *show-lisp-errors-p* t))

(defun debug-mode-off ()
  "Turns off debug-mode."
  (setf *debug-mode* nil
        *show-lisp-errors-p* nil))
