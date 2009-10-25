;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; syntax.lisp -- Implement syntax for the elephant query engine
;;; 
;;; Copyright (c) 2007 by  Ian S. Eslick
;;; <ieslick at common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Limited General Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package :elephant)

;;
;; Operations and aggregators
;;

(defvar *legal-operators*
  '(= /= < > <= >= 
    string= string/= string< string> string>= string<=
    string-equalp string-not-equal string-lessp string-not-lessp string-greaterp string-not-greaterp
    map member
    type-p subtype-p))

(defun legal-operator-p (op)
  (member op *legal-operators*))

(defparameter *legal-aggregation-operators*
  '(and or xor))

(defun aggregation-operator-p (atom)
  (member op *legal-aggregation-operators*))

;;
;; Parser
;;
;; Transform surface expressions to RA expressions with set object-relation closure property

;; (map-query fn (a b)
;;   (:with ((mgr person) (job job)))
;;   (:declare (type person emp)
;; 	    (type project proj)
;; 	    (type department (department person)))
;;   (:constraints or :where
;;    (between (start-date proj) (convert-date "July 5th 1996") (convert-date "November 1st 1996"))
;;    (eq (project emp) proj)
;;    (member (name (department emp)) '("Marketing" "Administration"))
;;    (eq (supervisor emp) mgr)
;;    (>= (slot salary mgr) 100000)))

;; Parser entry

(defun get-clause (namelist clauses &optional error)
  (assert namelist)
  (setf namelist (mklist namelist))
  (aif (assoc (car namelist) clauses)
       (cdr it)
       (if (null (cdr namelist))
	   (when error (error error))
	   (get-clause (cdr namelist) clauses error))))

(defun parse-query-syntax (query)
  (construct-ra-graph
   (parse-constraints 
      (get-clause '(:constraints constraints :where where) query)
    (parse-declarations 
       (get-clause '(:declare declare) query)
     (parse-targets (get-clause '(:with with) query)
		    (make-relation-dictionary))))))

;; Dictionary

(defun make-relation-dictionary ()
  (cons nil 0))

(defun add-set (name class stmt dictionary &optional annotations)
  (push (list name class stmt annotations) (car dictionary)))

(defun add-anonymous-set (class dict)
  (let ((name (format nil "?class~A" (incf (cdr dict)))))
    (add-set name class nil dict)
    name))

(defun lookup-set (name dict)
  (awhen (assoc name (car dict) :test #'equal)
    it))

(defun set-name (setrec)
  (first setrec))

(defun set-type (setrec)
  (second setrec))

(defun set-statement (setrec)
  (third setrec))

(defun set-annotations (setrec)
  (fourth setrec))



;; Constraints
    
(defun parse-constraints (exprs dictionary)
  (mapcar (lambda (expr) 
	    (parse-constraint expr dictionary))
	  exprs))

(defun parse-constraint (expr dictionary)
  (assert (not (null expr)))
  (destructuring-bind (op &rest rest) expr
    (cond ((legal-operator-p op)
	   (parse-operator-expression op rest dictionary))
	  (t (error "Unrecognized clause head ~A in ~A." op expr)))))

(defun parse-operator-expression (op body dictionary)
  (destructuring-bind (ref1 ref2) body
    (let ((rec1 (parse-reference ref1 dictionary))
	  (rec2 (parse-reference ref2 dictionary)))
      (cond ((simple-expression-p rec1 rec2)
	     (make-simple-select-statement op rec1 rec2))
	    ((join-expression-p rec1 rec2)
	     (make-join-statement op rec1 rec2 dictionary))
	    (t (error "Unrecognized syntax: (~A~{ ~A~})" op body))))))

(defun simple-expression-p (rec1 rec2)
  (or (value-record-p rec1)
      (value-record-p rec2)
      (eq (reference-setname rec1) (reference-setname rec2))))

(defun join-expression-p (rec1 rec2)
  (or (not (eq (reference-setname rec1) (reference-setname rec2)))
      (nested-record-p rec1)
      (nested-record-p rec2)))

(defun simple-reference-form-p (ref dictionary)
  (and (listp ref) 
       (= (length ref) 2)
       (not (listp (second ref)))
       (lookup-set (second ref) dictionary)))

(defun nested-reference-form-p (ref dictionary)
  (and (listp ref)
       (>= (length ref) 2)
       (listp (second ref))
       (or (simple-reference-form-p (second ref) dictionary)
	   (nested-reference-form-p (second ref) dictionary))))

;; Reference records

(defclass reference-record ()
  ((type :accessor reference-type :initarg :type) 
   (slot :accessor reference-slot :initarg :slot)
   (setname  :accessor reference-setname :initarg :setname)
   (value :accessor reference-value :initarg :value)
   (form :accessor reference-form :initarg :form))
  (:documentation "Type is one of :simple-value, :simple-reference, :nested-reference"))

(defmethod print-object ((rec reference-record) stream)
  (case (reference-type rec)
    (:simple-value (format stream "#<REFERENCE-RECORD value=~A>" (reference-value rec)))
    (:simple-reference (format stream "#<REFERENCE-RECORD slot=~A setname=~A>" (reference-slot rec) (reference-setname rec)))
    (:nested-reference (format stream "#<REFERENCE-RECORD slot=~A nested-value>" (reference-slot rec)))
    (t (format stream "#<REFERENCE-RECORD type=~A" (reference-type rec)))))

(defun nested-record-p (rec)
  (eq (reference-type rec) :nested-reference))

(defun value-record-p (rec)
  (eq (reference-type rec) :simple-value))

(defun simple-record-p (rec)
  (eq (reference-type rec) :simple-reference))
  

(defun make-value-rec (value)
  (make-instance 'reference-record :type :simple-value :value value))

(defun make-ref-rec (slot class)
  (make-instance 'reference-record 
		 :type :simple-reference 
		 :slot slot
		 :setname class))

(defun make-nested-rec (slot rest dict)
  (make-instance 'reference-record
		 :type :nested-reference
		 :slot slot
		 :form (if (simple-reference-form-p rest dict)
			   (make-ref-rec (first rest) (second rest))
			   (make-nested-rec (first rest) (second rest) dict))))

;; Parsing references

(defun parse-reference (ref dictionary)
  (cond ((symbolp ref)
	 (if (lookup-set ref dictionary)
	     (make-ref-rec nil ref)
	     (make-value-rec ref)))
	((atom ref)
	 (make-value-rec ref))
	((simple-reference-form-p ref dictionary)
	 (make-ref-rec (first ref) (second ref)))
	((nested-reference-form-p ref dictionary)
	 (make-nested-rec (first ref) (second ref) dictionary))
	(t (error "Unrecognized reference: ~A" ref))))

;; Build RA clauses

(defun make-simple-select-statement (op rec1 rec2)
  "One record is a value, the other is a simple reference"
  (flet ((setname ()
	   (if (simple-record-p rec1)
	       (reference-setname rec1)
	       (reference-setname rec2))))
    `(select (,op ,(reference-slot-or-value rec1) 
		  ,(reference-slot-or-value rec2))
	     ,(setname))))

(defun make-join-statement (op rec1 rec2 dictionary)
  (cond ((and (simple-record-p rec1) (simple-record-p rec2))
	 `(theta-join ,op 
		      ,(reference-slot rec1) ,(reference-setname rec1)
		      ,(reference-slot rec2) ,(reference-setname rec2)))
	((and (nested-record-p rec1) (value-record-p rec2))
	 (make-nested-join op rec1 rec2))
	((and (value-record-p rec1) (nested-record-p rec1))
	 (make-nested-join op rec2 rec1 :reverse t))
	(t (error "Cannot construct complex join statement with ~A and ~A" rec1 rec2))))

(defun make-nested-join (op rec-nest rec-value dict &key reverse)
  (let* ((slot (reference-slot rec-nest))
	 (sc-list (assign-join-types nil (reference-form rec-nest)))
	 (select `(select (,op ,@(when reverse 
				       (list value slot)
				       (list slot value))
			       ,(second (first sc-list))))))
    (nest-joins (rest sc-list) select)))

(defun nest-joins (sc-list inner-stmt)
  "Wraps a cascade of joins with anonymous classes"
  (if (null sc-list)
      inner-stmt
      (let ((slot-class (first sc-list)))
	(nest-joins (cdr sc-list)
		    `(join ,(first slot-class) ,(second slot-class) oid ,inner-stmt)))))

(defun assign-join-types (accessor nested-form dict)
  (if (simple-reference-form-p nested-form dict)
      (list nested-form)
      (let* ((list (assign-join-types (first nested-form) (second nested-form) dict))
	     (type-form (first list)))
	(cons (list accessor
		    (get-set-type (list (first type-form) (get-set-type (second type-form) dict)) dict))
	      list))))

(defun get-set-type (form dict)
  (let ((setrec (lookup-set form dict)))
    (if setrec (set-type setrec)
	(ifret (infer-type (first form) (second form)) nil))))
	       
	       

(defun infer-type (slot class)
  "Determine the type "

  ((nil namerec)
   (name person)
   (manager department)
   (department emp))

	 

(= (name (manager (department emp))) "George")
(department emp) = foo
(manager foo) = foo1
(name foo1) 

(join department emp oid 
      (project (oid) (join manager ?class1 oid 
			   (project oid (select (= name "George") ?class2)))))

(defun reference-slot-or-value (rec)
  (cond ((value-record-p rec)
	 `(value ,(reference-value rec)))
	((or (simple-record-p rec)
	     (nested-record-p rec))
	 (if (reference-slot rec)
	     `(slot ,(reference-slot rec))
	     (reference-setname rec)))
	(t (error "Record has neither value nor slot: ~A" rec))))

