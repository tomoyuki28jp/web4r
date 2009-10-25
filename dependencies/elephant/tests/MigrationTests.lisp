;;; MigrationTests.lisp
;;;
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2005,2006 by Robert L. Read
;;; <rread@common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.


;; This file is an example of how to perform the 
;; migration tests.  You will have to modify it 
;; slightly depending on the systems that want to test...
;; You can test migration even between two BDB respositories if you wish
(asdf:operate 'asdf:load-op :elephant)
(asdf:operate 'asdf:load-op :ele-clsql)
(asdf:operate 'asdf:load-op :clsql-postgresql-socket)
(asdf:operate 'asdf:load-op :ele-bdb)
(asdf:operate 'asdf:load-op :elephant-tests)


;; For sqlite-3..
;; (asdf:operate 'asdf:load-op :ele-sqlite3)


(in-package "ELEPHANT-TESTS")


;; The primary and secondary test-paths are 
;; use for the migration tests.

;; This this configuration for testing between BDB and SQL....
;;(setq *test-path-primary* *testpg-spec*)
;; (setq *test-path-primary* *testsqlite3-path*)
;;(setq *test-path-secondary* *testbdb-spec*)


;; This this configuration for testing from one BDB repository to another...
(setq *test-path-primary* *testbdb-spec*)
;; (setq *test-path-primary* *testsqlite3-path*)
(setq *test-path-secondary* *testbdb-spec2*)

(do-migration-tests *testbdb-spec* *testbdb-spec2*)
;;(do-migration-tests *testbdb-spec2* *testpg-spec*)

