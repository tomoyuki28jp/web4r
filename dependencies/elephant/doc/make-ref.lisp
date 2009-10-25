(require 'asdf)
(asdf:operate 'asdf:load-op 'elephant :force t)
(in-package :elephant)

(load (merge-pathnames 
       #p"src/elephant/query"
       (asdf:component-pathname (asdf:find-system 'elephant))))

(defparameter include-dir-path 
  (namestring 
   (merge-pathnames 
    #p"doc/includes/"
    (asdf:component-pathname (asdf:find-system 'elephant)))))

(defparameter docstrings-path
  (namestring 
   (merge-pathnames 
    #p"doc/docstrings.lisp"
    (asdf:component-pathname (asdf:find-system 'elephant)))))

(sb-posix:chdir include-dir-path)
(load docstrings-path)

(defclass simple-store-controller (store-controller)
  ())

(defun make-docs ()
  (let ((sc (make-instance 'simple-store-controller)))
    (setf (controller-spec sc) nil)
    (make-instance 'elephant::persistent-collection :sc sc :from-oid 10)
    (make-instance 'elephant::secondary-cursor)
    (make-instance 'elephant::indexed-btree :sc sc :from-oid 11)
    (make-instance 'elephant::pset  :sc sc :from-oid 12)
;;    (sb-texinfo:generate-includes #p"/Users/eslick/Work/fsrc/elephant-cvs/doc/includes/" 
    (sb-texinfo:generate-includes include-dir-path
				  (find-package :elephant)
				  (find-package :elephant-data-store)
				  (find-package :elephant-memutil)
				  (find-package :elephant-system))))

(make-docs)
