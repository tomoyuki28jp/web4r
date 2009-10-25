;;; testconditions.lisp
;;;
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2007 by Ian Eslick
;;; <ieslick common-lisp net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :ele-tests)

(deftest cross-store-reference-condition
  (if (or (not (boundp '*test-spec-secondary*))
	  (null *test-spec-secondary*))
      (progn 
	(format t "~%Second store spec missing: ignoring")
	(values t t t t))
      (let ((sc2 (open-store *test-spec-secondary* :recover t :deadlock-detect nil))
	    (*store-controller* *store-controller*)
	    (sc1 *store-controller*))
	(unwind-protect
	     (let ((inst1 (make-instance 'pfoo :slot1 100 :sc sc1))
		   (inst2 (make-instance 'pfoo :slot1 200 :sc sc2)))
	       (values 
		(is-not-null (add-to-root 'inst1 inst1 :sc sc1))
		(is-not-null (add-to-root 'inst2 inst2 :sc sc2))
		(signals-specific-condition (cross-reference-error)
		  (add-to-root 'inst1 inst1 :sc sc2))
		(signals-specific-condition (cross-reference-error)
		  (add-to-root 'inst2 inst2 :sc sc1))))
	  (close-store sc2))))
  t t t t)

(deftest unindexed-class-condition
    (let ((inst (make-instance 'pfoo :slot1 1)))
      (signals-specific-condition (persistent-class-not-indexed)
	(find-class-index 'pfoo)))
  t)