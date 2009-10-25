
(defpackage elephant-tutorial 
  (:use :cl :elephant))

(in-package :elephant-tutorial)

(defclass simple-plog ()
  ((timestamp :accessor plog-timestamp :initarg :timestamp :index t)
   (type :accessor plog-type :initarg :type :index t)
   (data :accessor plog-data :initarg :data)
   (user :accessor plog-user :initarg :user :index t))
  (:metaclass persistent-metaclass)
  (:documentation "Simple persistent log"))

(defclass url-record ()
  ((url :accessor url-record-url :initarg :url :initform "")
   (fetched :accessor url-record-fetched :initarg :fetched :initform nil)
   (analyzed :accessor url-record-analyzed :initarg :analyzed :initform nil))
  (:documentation "An application object, declared persistent but not indexed"))

(defmethod print-object ((obj url-record) stream)
  "Pretty print program objects so they're easy to inspect"
  (format stream "<url: ~A ~A ~A>" (url-record-url obj) (url-record-fetched obj) (url-record-analyzed obj)))

(defclass url-log (simple-plog) ()
  (:metaclass persistent-metaclass)
  (:documentation "This class tracks events that transform our program object state"))

(defmethod print-object ((obj url-log) stream)
  "Structured printing of log entries so they're easy to inspect at the repl"
  (format stream "#plog[~A :: ~A]" (plog-type obj) (plog-data obj)))

(defun log-event (user type data)
  "A helper function to generically log various events by user"
  (make-instance 'url-log
		 :timestamp (get-universal-time)
		 :type type
		 :data data
		 :user user))

(defun report-events-by-time (user start end)
  "A custom reporting function for our logs - pull out a time range.  A real
   implementation might do it by dates or by dates + times using one of the
   lisp time libraries"
  (let ((entries1 (get-instances-by-range 'url-log 'timestamp start end))
	(entries2 (get-instances-by-value 'url-log 'user user)))
    (format t "Event logs for ~A (~A range, ~A user):~%" user (length entries1) (length entries2))
    (format t "~{~A~%~}" (nreverse (intersection entries1 entries2)))))

;;
;; This code is the skeleton of a program
;;

(defvar *start-timestamp* nil)
(defvar *end-timestamp* nil)

(defun generate-events (user count &optional delay)
  (setf *start-timestamp* (get-universal-time))
  (loop for i from 1 upto count do
       (let ((url (get-a-url user i)))
	 (sleep delay)
	 (fetch-url url user)
	 (sleep delay)
	 (analyze-url url user)
	 (sleep delay)))
  (setf *end-timestamp* (get-universal-time)))

(defun get-a-url (user seq)
  (let ((url (make-instance 'url-record :url (format nil "http://www.common-lisp.net/~A/" seq))))
    (log-event user :received-url url)
    url))

(defun fetch-url (url user)
  (setf (url-record-fetched url) t)
  (log-event user :fetched-url url))

(defun analyze-url (url user)
  (setf (url-record-analyzed url) t)
  (log-event user :analyzed-url url))
       
;; Top Level Test Code

(defun test-generate-and-report (name store-spec)
  (open-store store-spec)
  (generate-events name 10 0.2)
  (report-events name)
  (close-store))
			 
(defun report-events (name)
  (let ((first-third-start *start-timestamp*)
	(first-third-end (+ *start-timestamp*
			   (/ (- *end-timestamp* *start-timestamp*) 3))))
    (report-events-by-time name first-third-start first-third-end)))

