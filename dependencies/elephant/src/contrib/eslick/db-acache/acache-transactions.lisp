

(in-package :elephant-acache)

(defmethod controller-start-transaction ((sc acache-store-controller) &key parent &allow-other-keys)
  "Allegrocache has implicit transactions whenever there's a write"
  (when parent
    (error "ACache backend does not allow nested transactions...a commit will commit everything
            since the last commit"))
  t)

(defmethod controller-commit-transaction ((sc acache-store-controller) &key &allow-other-keys)
  (db.allegrocache:commit :db (controller-db sc)))

(defmethod controller-abort-transaction ((sc acache-store-controller) &key &allow-other-keys)
  (db.allegrocache:rollback :db (controller-db sc)))

(defmethod execute-transaction ((sc acache-store-controller) closure &key parent retries &allow-other-keys)
  (db.allegrocache:with-transaction-restart (:count retries)
    (funcall closure)
    (db.allegrocache:commit :db sc)))