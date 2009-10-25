
(in-package :elephant-tests)

(defparameter *spec* '(:bdb "/Users/eslick/Work/db/test"))

(defparameter *names* '("David" "Jim" "Peter" "Thomas"
                        "Arthur" "Jans" "Klaus" "James" "Martin"))

(defclass person ()
  ((name :initform (elt *names* (random (length *names*)))
         :accessor name
         :index t) 
;; Actually the index t shouldn't be needed, but since elephant
;; sometimes complained that "person is not an index class", I try if this fixes it.
   (age :initform (random 100) :accessor age :index t)
;;   (made-by :initform (elephant-utils::ele-thread-hash-key))
   (updated-by :initform nil :accessor updated-by))
  (:metaclass elephant:persistent-metaclass))

(defparameter *nr-persons* 10000)  ;; Should be 10000, but for me elephant can't allocate memory after 3000.
;; I think the problem it is becuase the number of locks (999) is = max 1000. see db_stat -e

(defparameter +age+ 50)

;; I have tried different places for with-transaction below
(defun make-persons (nr-objects &optional (batch-size 500))
  (loop for i from 1 to (/ nr-objects batch-size) do
       (elephant:with-transaction () 
	 (loop for j from 1 to batch-size do
	      (let ((person (make-instance 'person)))
		(when (zerop (mod (+ (* i batch-size) j) 1000))
		  (format t "~D ~a " (+ (* i batch-size) j) (name person))))))))

(defun ensure-clean-store ()
  t)
;;  (let ((dir (cl-fad:pathname-as-directory (second *spec*))))
;;    (when (cl-fad:directory-exists-p dir)
;;      (cl-fad:delete-directory-and-files dir))
;;    (ensure-directories-exist dir)))

(defun my-test-create ()
  (ensure-clean-store)
  (elephant:with-open-store (*spec*)
    (make-persons *nr-persons*)))

(defun subsets (size list)
  (let ((subsets nil))
    (loop for elt in list 
	  for i from 0 do
       (when (= 0 (mod i size))
	 (setf (car subsets) (nreverse (car subsets)))
	 (push nil subsets))
       (push elt (car subsets)))
    (setf (car subsets) (nreverse (car subsets)))
    (nreverse subsets)))

(defmacro do-subsets ((subset subset-size list) &body body)
  `(loop for ,subset in (subsets ,subset-size ,list) do
	,@body))

(defun my-test-update (&key (new-age 27))
  "Test updating all persons by changing their age."
  (elephant:with-open-store (*spec*)
      (do-subsets (subset 500 (elephant:get-instances-by-class 'person))
	(format t "Doing subset~%")
	(elephant:with-transaction ()
	  (mapcar #'(lambda (person)
		      (setf (age person) new-age))
		  subset)))))

(defun my-test-load ()
  "Test loading all persons by computing their average age."
  (let ((nr-persons 0)
        (total-age 0)
        (show-first nil))
    (elephant:with-open-store (*spec*)
      (elephant:with-transaction ()
        (mapcar #'(lambda (person)
                    (incf nr-persons)
                    (print nr-persons)
                    (when (and show-first (> show-first))
                      (format t "Sample person ~a~%F" show-first)
                      (describe person)
                      (decf show-first))
                    (incf total-age (age person)))
                (elephant:get-instances-by-class 'person))))
    (values (coerce (/ total-age nr-persons) 'float)
            nr-persons
            total-age)))

(defun check-basic-setup ()
  (my-test-update :new-age +age+)
  (multiple-value-bind (average nr-persons)
      (my-test-load)
    (assert (= +age+ average))
    (assert (= nr-persons *nr-persons*))))

