(in-package "DCM")

(use-package "SB-THREAD")

(defmacro init-director (cls dirclass &rest x)
  `(let ((dir (make-instance ,cls ,@x)))
    (initialize dir ,cls ,dirclass)
    (setf (gethash ,cls *director-class-map*) dir)
    (load-all dir)
    dir))

(defmacro init-director-noload (cls dirclass &rest x)
  `(let ((dir (make-instance ,cls ,@x)))
    (initialize dir ,cls ,dirclass)
    (setf (gethash ,cls *director-class-map*) dir)
;;    (load-all dir)
    dir))

(defvar *dcm-mutexes* (make-hash-table :test 'equal))

(defvar *a-mutex* (sb-thread::make-mutex :name "my lock"))

(defun insure-mutex (name)
   (let ((mtx (gethash name *dcm-mutexes*))
 	)
     (or mtx (setf (gethash name *dcm-mutexes*) (sb-thread:make-mutex :name name)))
     )
  )

;; This assumes that the the variable "dir" is being defined and that we can can 
;; create 
(defmacro defmethodex (mname dir args &body body)
  `(defmethod ,mname ,(cons dir args)
;;    (format t "Thread ~A running ~%" sb-thread::*current-thread*)    
	(sb-thread:with-mutex ((insure-mutex (format nil "mutex-~A" ,(car dir))))
;;        (format t "Thread ~A got the lock~%" sb-thread::*current-thread*)
	(let ((ret
	,@body))
;;        (format t "Thread ~A dropping lock~%" sb-thread::*current-thread*)
	  ret
	)
    )
  )
  )
  
