;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; query-syntax.lisp -- Implement syntax for the elephant query engine
;;; 
;;; Copyright (c) 2007 by  Ian S. Eslick
;;; <ieslick at common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Limited General Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package :elephant)

;; Want syntax such that we can recursively walk the definition and
;; produce a query graph that can be subject to algebraic optimization
;; How to handle nested queries?

(string= (name (department inst)) "Marketing")

;; Name unique, no idx on dept
inst := (class people)
v1 := (lookup-select string= (name department) "marketing")
s1 := (class-select ref= department people v1)
s2 := (class-select < salary people 100k)
s3 := (join s1 s2)
s4 := (objects s3)

;; graph of instance sets and selections against them


;;
;; Constraint graph
;;

(defclass constraint-graph ()
  ((variables :accessor constraint-variables :initarg :vars :initform nil)
   (edges :accessor constraint-edges :initarg :edges :initform nil)))

(defmethod find-constraint-variable ((graph constraint-graph) var-name)
  (find var-name (constraint-variables graph) :test #'variable-name))

(defmethod find-constraint-edge ((graph constraint-graph) var1 var2)
  (when (symbolp var1)
    (setf var1 (find-constraint-variable graph var1)))
  (when (symbolp var2)
    (setf var2 (find-constraint-variable graph var2)))
  (flet ((match-p (edge)
	   (and (or (eq (edge-src edge) var1)
		    (eq (edge-dst edge) var1))
		(or (eq (edge-dst edge) var2)
		    (eq (edge-src edge) var2)))))
    (find nil (constraint-edges graph) :test #'match-p)))

(defmethod add-constraint ((graph constraint-graph) var constraint bindings)
  (unless (find-constraint-variable graph var)
    (make-instance 'constraint-variable 
		   :class (get-var-class bindings var)
		   (push constraint (constraint-variables (find-constraint-variable graph var)))

(defclass constraint-variable ()
  ((name :accessor variable-name :initarg :name)
   (class :accessor variable-class :initarg :class)
   (constraints :accessor variable-constraints :initarg :constraints :initform nil)))

(defclass edge ()
  ((src :accessor edge-src :initarg :src)
   (dst :accessor edge-dst :initarg :dst)
   (constraint :accessor destination-constraint :initarg :constraint)))

(defclass constraint ()
  ((class :accessor constraint-class :initarg :class)
   (fn :accessor constraint-fn :initarg :test-fn
       :documentation "Predicate accepting an instance and returns 
                       whether it was accepted (t) or rejected (nil)")
   (expr :accessor constraint-expr :initarg :test-expr
	 :documentation "Predicate expression for inline expansion")))

(defclass value-constraint (constraint)
  ((slot :accessor constraint-slot :initarg :slot :initform nil)
   (indexed-p :accessor indexed-p :initarg :indexed-p :initform nil)
   (index-type :accessor index-type :initarg :type :initform t)
   (range-p :accessor range-p :initarg :range-p :initform nil)
   (value :accessor constraint-value :initarg :value)))

(defmethod initialize-index-info ((c value-constraint))
  (when (constraint-slot c)
    (let ((idx (find-inverted-index (constraint-class c) (constraint-slot c) :null-on-fail t)))
      (when idx (setf (indexed-p c) t)))))

(defclass range-constraint (constraint)
  ((range :accessor constraint-range :initarg :range)))

(defclass and-constraint (constraint)
  ((constraints :accessor constraints :initarg :constraints)))
(defclass or-constraint (constraint) ())
(defclass xor-constraint (constraint) ())

;;
;; Constraint patterns for parsing
;;

(defvar *constraint-dictionary*
  '((= parse-numeric)
    (< parse-numeric range)
    (> parse-numeric range)
    (>= parse-numeric range)
    (<= parse-numeric range)
    (string= parse-string)
    (string< parse-string range)
    (string> parse-string range)
    (string>= parse-string range)
    (string<= parse-string range)
    (member parse-member)
    (fn parse-function)
    (between parse-range)
    (or parse-or)
    (and parse-and)
    (eq parse-equiv)))

(defun parse-constraint (expr bindings graph)
  (let ((op (first expr)))
    (multiple-value-bind (var constraint) 
	(funcall (symbol-function (second (assoc op *constraint-dictionary*)))
		 expr binding)
      (add-constraint graph var constraint bindings))))
	     

(defun parse-numeric (expr bindings)
  (destructuring-bind (rel (slot var) value) expr
    (assert (numberp value))
    (values var
	    (make-instance 'value-constraint 
			   :slot slot
			   :range-p nil
			   :class (binding-class var)
			   :value value
			   :expr `((inst) (,rel (slot-value inst ,slot) ,value))))))

;;
;; Bindings
;;

(defun make-constraint-bindings ()
  (make-hash-table :test #'equal))

(defun add-binding (name rec bindings)
  (setf (gethash name bindings) rec))

(defun get-binding (name bindings)
  (gethash name bindings))

(defun make-binding (type name)
  (list type name))

(defun binding-type (rec)
  (first rec))

(defun binding-target (rec)
  (second rec))


