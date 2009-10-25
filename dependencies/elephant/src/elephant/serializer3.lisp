(in-package :elephant-serializer2)

;; Protocol for backend
;;
;; What is a serializer?
;; - Determines a common serial format for objects; custom to common-lisp by helping to
;;   bridge two environments and help folks not shoot themselves in the foot.
;; - For example, the class signature can be shot across on the first instance of a class
;;   to validate against a remote class signature or we can use the local cached signature.
;; - Functions can be extracted and sent over in s-exp form.  How about closures?  

(defun serialize (obj bs &aux cache obj-id)
  (declare (optimize (speed 3) (safety 0))
	   (type buffer-stream bs))
  (labels (;; Circularity cache
	   (reset-circularity-cache ()
	     (if (> (hash-table-size cache) 100)
		 (setf cache (make-hash-table :test 'eq :size 50))
		 (clrhash cache))
	     (setf obj-id 0))
	   (caching-serializer (obj)
	     (aif (gethash obj cache) 
		  (int sid bs)
		  (progn
		    (int (incf obj-id bs))
		    (setf (gethash obj cache) obj-id)
		    (%serialize-cached obj))))
	   ;; Helper functions
	   (byte (obj)
	     (buffer-write-byte obj bs))
	   (int (obj)
	     (buffer-write-int obj bs))
	   (float (obj)
	     (buffer-write-float obj bs))
	   (double (obj)
	     (buffer-write-double obj bs))
	   (string (obj)
	     (buffer-write-string obj bs))
	   (uint (obj)
	     (buffer-write-uint obj bs))
	   ;; Main dispatch
	   (%serialize (obj)
	     (etypecase obj
	       (null   (byte +nil+))
	       (character    (byte +char+)
			     (uint (char-code obj)))
	       (fixnum (byte +fixnum+) int)
	       (single-float (byte +single-float+)
			     (float obj))
	       (double-float (byte +double-float+)
			     (double obj))
	       (integer      (mvbind (val size words) (bignum-features obj)
			       (int words)
			       (loop for i fixnum from 0 below size do
				    #+(or cmu sbcl)
				    (uint (%bignum-ref val i))
				    #+(or allegro lispworks openmcl)
				    (uint (ldb (int-byte-spec i) val)))))
	       (rational     (byte +rational+)
			     (%serialize (numerator obj))
			     (%serialize (denominator obj)))
	       (string (byte (string-type obj))
		       (int (string-length obj))
		       (string obj))
	       (symbol (byte +symbol+)
		       (serialize (symbol-name obj))
		       (aif (symbol-package obj)
			    (%serialize (package-name obj))
			    (%serialize nil)))
	       (pathname (byte +pathname+)
			 (%serialize (namestring obj)))
	       (cons     (byte +cons+)
			 (caching-serializer obj))
	       (hash-table (byte +hash-table+)
			   (caching-serializer obj))
	       (array      (byte +array+)
			   (caching-serializer obj))
	       (standard-object (byte +object+)
				(caching-serializer obj))
;;	       (structure-object (byte +struct+)
;;				 (caching-serializer obj))
;; 	       (standard-class   (byte +class+)
;;                               name:symbol
;; 				 superclasses
;; 				 metaclasses?
;; 				 direct slots (as defs)
;; 	       (direct-slot      (byte +class-slot+)
;; 				 name:symbol
;; 				 documentation
;; 				 type
;; 				 initform
;; 				 initfunction
;; 				 initargs
;; 				 allocation
;; 				 readers
;; 				 writers
;; 				 fixed-index?
	       (persistent (byte +persistent+)
			   (int (oid obj)))))
	   ;; Compound objects that need circularity cache detection
	   (%serialize-cached (obj)
	     (etypecase (obj)
	       (cons (%serialize (car obj))
		     (%serialize (cdr obj)))
	       (hash-table (%serialize (hash-table-test obj))
			   (%serialize (hash-table-rehash-size obj))
			   (%serialize (hash-table-rehash-threshold obj))
			   (%serialize (hash-table-count obj))
			   (loop for key being the hash-key of obj
				 using (hash-value value) do
				 (%serialize key)
				 (%serialize value)))
	       (array (mvbind (type-byte fill adjust rank size) (array-properties obj)
			(byte (logior type-byte 
				      (if fill +fill-pointer-p+ 0)
				      (if adjust +adjustable-p+ 0)))
			(int rank)
			(loop for i fixnum from 0 below rank do 
			     (int (array-dimension obj i)))
			(when fill (int (fill-pointer obj)))
			(loop for i fixnum from 0 below (array-total-size obj) do
			     (%serialize (row-major-aref obj i)))))
	       (standard-object (let ((rec (get-class-record obj)))
				  (int (record-id rec))
				  (loop for slot in (record-slots rec) do
				    (%serialize (slot-value obj slot))))))))
    (reset-circularity-cache)
    (%serialize obj)
    bs))
			   
			   
	      
	
