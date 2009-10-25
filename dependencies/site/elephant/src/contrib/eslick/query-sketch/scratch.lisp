;;
;; Constraint definitions
;;
;; A constraint is an expression that requires the value of an object slot
;; to be one or more values or a reference to an object satisfying a constraint.  
;; The object slot reference and the value slot reference can be complex.
;;

(defvar *constraint-definitions*
  (make-hash-table :size 40))

(defmacro define-constraint (name &body body)
  `(progn
     (push ,(generate-constraint-pattern body) 
	   (gethash ',name *constraint-dispatch*))))

(defun generate-constraint-pattern (expr)


;;
;; Constraint types
;;  
;; While parsing a constraint form, we need to identify the type of arguments
;; so we can properly construct an object from the result
;; 

(defun parse-constraint-argument (position

;;
;; Standard constraints
;;

;; symbols are normal symbols
;; <ref> means a slot or class reference
;; 

(defmacro define-value-constraints (methods target-type)
  `(progn
     ,@(mapcar (lambda (method)
		 `(define-constraint ,method
		      (,method <ref> ,target-type) =>
		      (,method <ref> ,target-type))
	       methods))))

(defun != (a b) (not (= a b)))

;; i.e.
;;(define-constraint =
;;   (= <ref> <number>))
(define-value-constraints (= < > <= >= !=) <number>)

(defun string!= (a b) (not (string= a b)))

;; i.e.
;; (define-constraint string=
;;   (string= <ref> <number>))
(define-value-constraints (string= string< string> string<= string>= string!=) <string>)

;;
;; Rewrite rules
;;
;; Pattern matches against constraint expressions to convert 
;; 

(defvar *rewrite-rules*
  (make-hash-table :size 40))

(defmacro define-rewrite-rule (name &body body)
  "Simple pattern matching rewrite"
  `(progn
     (setf (gethash ',name *rewrite-rules*)
	   ,(generate-rewrite-rule body))))

(defun generate-rewrite-rule (expr)
  "The constraint name dispatches this lambda on it's contents
   The lhs is put into a multiple-value-bind.  The rhs is a set
   of lisp expressions using the bound values"
  (with-gensyms (body)
    `(lambda (,body)
       (destructuring-bind (,@(first expr)) ,body
	 (declare (ignorable ,@(first expr)))
	 ,(third expr)))))

(defun test-rewrite-constraint (cname rule)
  (assert (gethash cname *constraint-dispatch*))
  (funcall (gethash cname *constraint-dispatch*) rule))

(defun generate-test-constraint (expr)
  (lambda (body)
    (multiple-value-bind (op <ref> value) body
      `(funcall #',op 
		,(resolve-reference <ref>) 
		,(resolve-value value)))))

;;
;; Standard rewrite rules
;;

(defmacro generate-type-rewrite-rules (type generic-ops type-ops)
  "Map an operation name where the constraining value is of a particular
   type to a type specific operation, such as = to string="
  `(progn
     ,@(mapcar (lambda (gop top)
		 `(define-rewrite-rule ,gop
		      (,gop <ref> ,type) =>
		      (,top <ref> ,type)))
	       generic-ops type-ops)))

(generate-value-type-rewrite-rules 
 <string> 
 (= < > <= >= !=) 
 (string= string< string> string<= string>= string!=))

(define-rewrite-rule member 
    (member <ref> <list>) => 
    `(or ,@(mapcar (lambda (elt) `(= ,<ref> ,elt)) list)))

;;
;; Parsing constraint expressions to constraint objects
;;

(defun constraint-expr-classes (constraint-expr)
  (mapcar #'second (second constraint-expr)))

(defun parse-constraints (constraint-expr)
  (destructuring-bind (sym classes-expr maybe-decl &rest constraints) constraint-expr
    (declare (dynamic-extent constraints))
    (assert (eq sym 'with))
  (let* ((classes (constraint-expr-classes constraint-expr))
	 (body (third constraint-expr))
	 (stmt (first body))
	 (bindings (make-constraint-bindings)))
    (cond ((eq stmt 'declare)
	   (parse-declarations body bindings))
	  (t (mapcar (lambda (expr) (parse-constraint expr bindings)) body)))))

(defun parse-declarations (expr bindings)
  (assert (eq (first expr) 'declare))
  (flet ((dispatch-declaration (declaration)
	   (let ((decl-type (first declaration)))
	     (cond ((eq decl-type 'type)
		    (parse-type-declaration (cdr declaration) bindings))
		   (t (error "Unknown type declaration: ~A~%" declaration))))))
    (mapcar #'dispatch-declaration (cdr expr))))


(defun parse-type-declaration (body bindings)
  "Parse declarations of the form (type type-name var) where
   var can be a class or slot/class reference"
  (destructuring-bind (name &rest vars) body
    (declare (dynamic-extent vars))
    (mapcar (lambda (var)
	      (add-binding var 
			   (make-binding (if (consp var) 'slot 'class)
					 name)
			   bindings))
	    vars)))

(defun parse-constraint (expr bindings)
  (funcall (constraint-handler (first expr)) expr bindings))

(defun parse-reference (expr bindings)
  
(defun parse-value (expr bindings)
  (cond ((consp (car expr))
	 (

(defclass reference ()
  ((type :accessor reference-type :initarg :type)
   (
   
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

;; ==================================
;; NOTES:

;; Example query:
;;
;; All managers making over 100k with subordinates on projects
;; started between july 5th and november 1st who are either
;; in "Marketing" or "Administration"

(map-constraints 
(with ((mgr person) (job job))
   (declare (type person emp)
	    (type project proj)
	    (type department (department person)))
   (between (start-date proj) (convert-date "July 5th 1996") (convert-date "November 1st 1996"))
   (eq (project emp) proj)
   (member (name (department emp)) '("Marketing" "Administration"))
   (eq (supervisor emp) mgr)
   (>= (slot salary mgr) 100000)))

project
- name = "Query"

department
- name = "Marketing"

person
- name
- department ->
- supervisor ->
- project(s)

;; Query syntax
;; - What kind of queries?
;; - express joins

(map-instances fn 


(map-query 

;;
;; Constraint syntax
;;

;; context-sensitive syntax
;; syntax -> constraint rep -> planning/reordering -> query sequence



(defmacro def-constraint (name (types) &body body)

eval at build time
eval at run time
eval at extraction time



(def-constraints standard-numeric-constraints
     (<slot-ref> rel (or <number> <string>)))

(defparameter *relations*
  '(= != < > <= >= between member))

(defun map-constrained-instances (fn constraints)
  )

(map (class 
(instances (slot-constraint
(intersect-constraint set1 set2)

(defun siblings (x)
  (with-collector (c)
    (map-constraints c 
       (with people where
	     (parent = (parent x))))))

(defun all-relations (x)
  (close (parent x) over people x))

(map-constrained-instances 
 #'print-name
 (with people where
       (name < "Darryl")
       (salary > 200000)
       (department manager (where (name = "Greg")))

(people name < "Darryl")
(people salary between 100000 150000)
  (people salary => 100000)
  (people salary <= 150000)
(people department manager name = "Greg") ;; join

;;and, or, xor clause modifiers

;;slot not-eq =/>=/<= value
;;slot between value1 value2 (sugar)
;;slot member (set) (sugar)

