;;; Copyright (c) 2006 by Robert L. Read
;;; <rread@common-lisp.net> 

;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(asdf:operate 'asdf:load-op :elephant-tests)

(in-package "ELEPHANT-TESTS")

(setf *default-spec* *testbdb-spec*)

(do-backend-tests)

