
(in-package :db-lisp)

;;
;; Btree Locks and transaction log
;;

(defclass mt-btree (btree)
  ((log :accessor btree-log :initarg :log
	:documentation "The transaction log")))


