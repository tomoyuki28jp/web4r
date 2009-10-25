(in-package :cl-user)

(defpackage :rucksack-elephant
  (:use :cl :rucksack)
  (:export
   ;; controller
   #:open-rucksack #:close-rucksack #:with-rucksack #:current-rucksack
   #:rucksack #:standard-rucksack
   #:rucksack-cache
   #:rucksack-directory
   #:rucksack-commit #:rucksack-rollback
   #:add-rucksack-root #:map-rucksack-roots #:rucksack-roots
   #:commit #:rollback
   ;; class indexing
;;   #:add-class-index #:add-slot-index
;;   #:remove-class-index #:remove-slot-index
;;   #:map-class-indexes #:map-slot-indexes
   #:rucksack-add-class-index #:rucksack-add-slot-index
   #:rucksack-make-class-index
   #:rucksack-remove-class-index #:rucksack-remove-slot-index
   #:rucksack-class-index #:rucksack-slot-index
   #:rucksack-map-class-indexes #:rucksack-map-slot-indexes
   #:rucksack-maybe-index-changed-slot #:rucksack-maybe-index-new-object
   #:rucksack-map-class #:rucksack-map-slot
   ;; Transactions
;;   #:current-transaction
;;   #:transaction-start #:transaction-commit #:transaction-rollback
;;   #:with-transaction
;;   #:transaction #:standard-transaction
;;   #:transaction-start-1 #:transaction-commit-1
;;   #:transaction-id

   ;; Cache
   #:cache #:standard-cache
   #:open-cache #:close-cache #:with-cache
   #:cache-size #:cache-count
   #:cache-create-object #:cache-get-object #:cache-touch-object
   #:cache-commit #:cache-rollback #:cache-recover
   #:open-transaction #:close-transaction #:map-transactions
   
   ;; Conditions
   #:rucksack-error #:simple-rucksack-error #:transaction-conflict
   #:btree-error #:btree-search-error #:btree-insertion-error
   #:btree-key-already-present-error #:btree-type-error
   #:btree-error-btree #:btree-error-key #:btree-error-value
   ;; Heaps
   #:heap #:free-list-heap #:mark-and-sweep-heap #:simple-free-list-heap
   #:open-heap #:close-heap
   #:heap-stream #:heap-end
   ;; BTree IF
;;   #:btree
   #:btree-key< #:btree-key= #:btree-value=
   #:btree-max-node-size #:btree-unique-keys-p
   #:btree-key-type #:btree-value-type
   #:btree-node-class #:btree-node
   ;; Indexes
   #:map-index #:index-insert #:index-delete #:make-index
   ;; BTrees
   #:btree-search #:btree-insert 
;; #:map-btree

   ;; Objects
;;   #:persistent-object
   #:persistent-data #:persistent-array #:persistent-cons
   #:object-id
   #:p-cons #:p-array
   #:p-eql
   #:p-car #:p-cdr #:p-list
   #:p-make-array #:p-aref #:p-array-dimensions
   #:p-length #:p-find #:p-replace #:p-position
   ))

(defpackage :db-lisp
  (:use :cl :elephant :elephant-backend :rucksack-elephant))

;; file
;; octet-stream
;; binary-data
;; binary-types
;; buffers
;; btree
