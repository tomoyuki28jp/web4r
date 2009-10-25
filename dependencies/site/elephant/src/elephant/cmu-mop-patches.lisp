;;; cmu-mop-patches.lisp
;;;
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; Portions Copyright (c) 2005-2007 by Robert Read and Ian Eslick
;;; <rread common-lisp net> <ieslick common-lisp net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;


#+cmu
(in-package :PCL)

#+sbcl
(in-package :SB-MOP)

#+sbcl
(declaim (sb-ext:disable-package-locks sb-mop:compute-slots 
				       sb-mop:class-slots
				       sb-pcl::update-slots))

(defmethod find-nonstandard-slot-definition-location ((allocation (eql :database)) slot)
  (declare (ignore slot))
  nil)

(defmethod compute-slots :around ((class standard-class))
  (loop with slotds = (call-next-method) and location = -1
	for slot in slotds 
	for allocation = (slot-definition-allocation slot) 
	do (progn
	     (setf (slot-definition-location slot)
		   (case allocation
		     (:instance
		      (incf location))
		     (:class
		      (let* ((name (slot-definition-name slot))
			     (from-class (slot-definition-allocation-class slot))
			     (cell (assq name (class-slot-cells from-class))))
			(assert (consp cell))
			cell))
		     (t
		      (find-nonstandard-slot-definition-location allocation slot)))) 
	     (#+cmu
	      initialize-internal-slot-functions
	      #+sbcl
	      sb-pcl::initialize-internal-slot-functions slot))	
	finally (return slotds)))

#+sbcl
(in-package :SB-PCL)

(defun update-slots (class eslotds)
  (collect ((instance-slots) (class-slots))
    (dolist (eslotd eslotds)
      (case (slot-definition-allocation eslotd)
	(:instance (instance-slots eslotd))
	(:class (class-slots eslotd))))
    ;;
    ;; If there is a change in the shape of the instances then the
    ;; old class is now obsolete.
    (let* ((nlayout (mapcar #'slot-definition-name
			    (sort (instance-slots) #'<
				  :key #'slot-definition-location)))
	   (nslots (length nlayout))
	   (nwrapper-class-slots (compute-class-slots (class-slots)))
	   (owrapper (when (class-finalized-p class)
		       (class-wrapper class)))
	   (olayout (when owrapper
		      (wrapper-instance-slots-layout owrapper)))
	   (nwrapper
	    (cond ((null owrapper)
		   (make-wrapper nslots class))
		  ;;
		  ;; We cannot reuse the old wrapper easily when it
		  ;; has class slot cells, even if these cells are
		  ;; EQUAL to the ones used in the new wrapper.  The
		  ;; class slot cells of OWRAPPER may be referenced
		  ;; from caches, and if we don't change the wrapper,
		  ;; the caches won't notice that something has
		  ;; changed.  We could do something here manually,
		  ;; but I don't think it's worth it.
		  ((and (equal nlayout olayout)
			(null (wrapper-class-slots owrapper)))
		   owrapper)
		  (t
		   ;;
		   ;; This will initialize the new wrapper to have the same
		   ;; state as the old wrapper.  We will then have to change
		   ;; that.  This may seem like wasted work (it is), but the
		   ;; spec requires that we call make-instances-obsolete.
		   (make-instances-obsolete class)
		   (class-wrapper class)))))

      (with-slots (wrapper slots finalized-p) class
	(update-lisp-class-layout class nwrapper)
	(setf slots eslotds
	      (wrapper-instance-slots-layout nwrapper) nlayout
	      (wrapper-class-slots nwrapper) nwrapper-class-slots
	      (wrapper-no-of-instance-slots nwrapper) nslots
	      wrapper nwrapper
	      finalized-p t))

      (unless (eq owrapper nwrapper)
	(update-inline-access class)
	(update-pv-table-cache-info class)
	(maybe-update-standard-class-locations class)))))
