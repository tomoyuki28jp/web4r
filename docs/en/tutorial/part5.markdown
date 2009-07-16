Tutorial Part 5. Persistent Object Database
============================================
web4r uses a persistent object database named [Elephant](http://common-lisp.net/project/elephant/).

Download and Install
---------------------
web4r depends on Elephant. If you haven't downloaded or installed it, [download](http://common-lisp.net/project/elephant/downloads.html) and [configure](http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started) Elephant. (currently only works with v0.9)

Example Usage
--------------
See the [Elephant user manual](http://common-lisp.net/project/elephant/doc/elephant.html) for detail.

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :elephant))
    (use-package :elephant)
    
    ; Opening a store
    (open-store '(:BDB "/path/to/db"))
    
    ; Defining a persistent class
    (defpclass blog ()
        ((title :initarg :title :accessor title :index t)
         (body  :initarg :body)))
    
    ; An instance of a persistent class is permanently stored in the data store.
    (dotimes (x 10)
      (make-instance 'blog :title x :body x))
    
    (let ((all (get-instances-by-class 'blog))) ; getting all instances by a class
      (print (subseq all 0 3)) ; getting the first three
      (print (sort all #'< :key 'title))) ; sorting instances by a slot
    
    ; Retrieving an instance from a slot index by value
    (get-instances-by-value 'blog 'title 5)
    
    ; Getting all instances that match values between start and end
    (get-instances-by-range 'blog 'title 2 4)
    
    ; updating an instance
    (let ((i (get-instance-by-value 'blog 'title 5)))
      (setf (title i) 0))
    
    ; Removing all instances by a class
    (drop-instances (get-instances-by-class 'blog))
