

(in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel)
  (require :acache))

(eval-when (:load-toplevel)
  (warn "Allegrocache support is incomplete and should be considered as an example only"))

(defpackage elephant-acache
  (:documentation "A low-level UFFI-based interface to
   Berkeley DB / Sleepycat to implement the elephant front-end
   framework.  Uses the libsleepycat.c wrapper.  Partly intended 
   to be usable outside Elephant, but with some magic for Elephant.  
   In general there is a 1-1 mapping from functions here and 
   functions in Sleepycat, so refer to their documentation for details.")
  (:use common-lisp elephant elephant-backend)
  (:import-from #:db.allegrocache
		#:ac-map
		#:ac-map-name
		#:doclass
		#:commit
		#:retrieve-from-index
		#:map-map
		#:map-value
		#:remove-from-map))


