;;; openmcl-mop-patches.lisp
;;;
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2006 by Andrew Blumberg 
;;; <ablumberg@common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package :CCL)

(let ((*warn-if-redefine-kernel* nil))
  (defun extract-instance-and-class-slotds (slotds)
    (collect ((instance-slots)
	      (shared-slots))
	     (dolist (s slotds (values (instance-slots) (shared-slots)))
	       (let ((alloc (%slot-definition-allocation s)))
		 (if (or (eq alloc :class)
			 (eq alloc :database))
		     (shared-slots s)
		   (instance-slots s)))))))

(defun extract-persistent-effective-slotds (class)
  (extract-slotds-with-allocation :database (%class-slots class)))

(in-package :inspector)

(defun standard-object-line-n (i n)
  (let* ((instance (inspector-object i))
         (class (class-of instance))
         (wrapper (ccl::standard-object-p instance))
	 (instance-start 2))
    (if (< n instance-start)
      (if (eql n 0)
	(values class "Class: " :normal)
	(values wrapper "Wrapper: " :static))
      (let* ((slotds (ccl::extract-instance-effective-slotds class))
             (instance-count (length slotds))
             (shared-start (+ instance-start instance-count
                              (if (eql 0 instance-count) 0 1))))
        (if (< n shared-start)
          (if (eql n instance-start)
            (values nil "Instance slots" :comment)
            (let ((slot-name (slot-definition-name
                              (elt slotds (- n instance-start 1)))))
              (values (slot-value-or-unbound instance slot-name)
                      slot-name
                      :colon)))
	  (let* ((slotds (ccl::extract-class-effective-slotds class))
                 (shared-count (length slotds))
                 (shared-end (+ shared-start shared-count
                                (if (eql shared-count 0) 0 1))))
            (if (< n shared-end)
              (if (eql n shared-start)
                (values nil "Class slots" :comment)
                (let ((slot-name (slot-definition-name 
                                  (elt slotds (- n shared-start 1)))))
                  (values (slot-value-or-unbound instance slot-name)
                           slot-name
                           :colon)))
	      (let* ((slotds (ccl::extract-persistent-effective-slotds class))
		     (persistent-count (length slotds))
		     (persistent-end (+ shared-end persistent-count
				       (if (eql persistent-count 0) 0 1))))
		(if (< n persistent-end)
		    (if (eql n shared-end)
			(values nil "Persistent slots" :comment)
		      (let ((slot-name (slot-definition-name 
					(elt slotds (- n shared-start 1)))))
			(values (slot-value-or-unbound instance slot-name)
				slot-name
				:colon)))
		  (if (and (eql 0 instance-count) (eql 0 shared-count) (eql n shared-end))
		      (values nil "No Slots" :comment)
		    (line-n-out-of-range i n)))))))))))
