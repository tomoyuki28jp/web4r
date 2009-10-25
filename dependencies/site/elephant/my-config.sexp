
;; Linux defaults
#+(and (or sbcl allegro openmcl lispworks) (not (or mswindows windows)) (not (or macosx darwin)))
((:compiler . :gcc)
 (:berkeley-db-include-dir . "/usr/local/BerkeleyDB.4.5/include/")
 (:berkeley-db-lib-dir . "/usr/local/BerkeleyDB.4.5/lib/")
 (:berkeley-db-lib . "/usr/local/BerkeleyDB.4.5/lib/libdb-4.5.so")
 (:berkeley-db-deadlock . "/usr/local/BerkeleyDB.4.5/bin/db_deadlock")
 (:berkeley-db-cachesize . 20971520)
 (:berkeley-db-map-degree2 . t)
 (:clsql-lib-paths . nil)
 (:prebuilt-libraries . nil))

;; OSX Defaults 
#+(and (or sbcl allegro openmcl lispworks) (not (or mswindows windows)) (or macosx darwin))
((:compiler . :gcc)
 (:berkeley-db-include-dir . "/usr/local/BerkeleyDB.4.5/include/")
 (:berkeley-db-lib-dir . "/usr/local/BerkeleyDB.4.5/lib/")
 (:berkeley-db-lib . "/usr/local/BerkeleyDB.4.5/lib/libdb-4.5.dylib")
 (:berkeley-db-deadlock . "/usr/local/BerkeleyDB.4.5/bin/db_deadlock")
 (:berkeley-db-cachesize . 20971520)
 (:berkeley-db-map-degree2 . t)
 (:clsql-lib-paths . nil)
 (:prebuilt-libraries . nil))

;; Windows defaults (assumes prebuild libraries)
#+(or mswindows windows)
((:compiler . :cygwin)
 (:berkeley-db-include-dir . "C:/Program Files/Oracle/Berkeley DB 4.5.20/include/")
 (:berkeley-db-lib-dir . "C:/Program Files/Oracle/Berkeley DB 4.5.20/bin/")
 (:berkeley-db-lib . "C:/Program Files/Oracle/Berkeley DB 4.5.20/bin/libdb45.dll")
 (:berkeley-db-deadlock . "C:/Program Files/Oracle/Berkeley DB 4.5.20/bin/db_deadlock.exe")
 (:berkeley-db-cachesize . 20971520)
 (:berkeley-db-map-degree2 . t)
 (:clsql-lib-paths . nil)
 (:prebuilt-libraries . t))

;; Berkeley 4.5 is required, each system will have different settings for
;; these directories, use this as an indication of what each key means
;;
;; :prebuilt-libraries is true by default for windows machines.  It causes
;; the library loader to look in the elephant root directory for the shared 
;; libraries.  (nil or t)
;;
;; :clsql-lib-paths tell clsql where to look for which ever SQL distribution
;; library files you need it to look for.  For example...
;;
;;  (:clsql-lib-paths . ("/Users/me/Work/SQlite3/" "/Users/me/Work/Postgresql/"))
;;
;; :pthread-lib is deprecated, for old versions of sbcl prior to 0.9.17 that 
;; did not have pthreads compiled in.  If you are using an old version, we 
;; recommend that you upgrade!  
;; Typical pthread settings were: /lib/tls/libpthread.so.0
;;
;; :compiler options are 
;;           :gcc (default: for unix platforms with /usr/bin/gcc)
;;           :cygwin (for windows platforms with cygwin/gcc)
;;           :msvc (unsupported)
;;
;; Additional supported parameters include:
;;
;;
;; :berkeley-db-cachesize
;;          An integer indicating the number of bytes
;;          for the page cache, default 20MB which is
;;          about enough storage for 10k-30k indexed
;;          persistent objects
;;
;; :berkeley-db-map-degree2 
;;          Boolean parameter that indicates whether map
;;          operations lock down the btree for the entire
;;          transaction or whether they allow other
;;          transactions to add/delete/modify values
;;          before the map operation is completed.  The
;;          map operation remains stable and any writes
;;          are kept transactional, see user manual as
;;          well as berkeley DB docs for more details.
