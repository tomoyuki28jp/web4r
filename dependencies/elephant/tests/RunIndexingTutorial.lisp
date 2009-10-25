(asdf:operate 'asdf:load-op :elephant)
(asdf:operate 'asdf:load-op :ele-bdb)
(asdf:operate 'asdf:load-op :elephant-tests)

(compile-file "indexing.lisp")
(load "index-tutorial.lisp")

(in-package "ELEPHANT-TUTORIAL")
(defconstant KILO 1000)
(defun test-generate-and-report-big (num name store-spec)
  (open-store store-spec)
  (generate-events name num 0.0 )
  (report-events name)
  (close-store))

(defun find-mid-event (name)
  (let ((midpoint (floor (/ (+ *start-timestamp*
			*end-timestamp*) 2))))
    (report-events-by-time-only name 
			   midpoint
			   (+ midpoint))
  )
)

(defun report-events-by-time-only (user start end)
  "A custom reporting function for our logs - pull out a time range.  A real
   implementation might do it by dates or by dates + times using one of the
   lisp time libraries"
  (let ((entries1 (time (get-instances-by-range 'url-log 'timestamp start end)))
	(entries2  nil))
    (mapc #'(lambda (x) (if (equal (plog-user x) user) (push x entries2))) entries1)
    (format t "Event logs for ~A (~A range, ~A user):~%" user (length entries1) (length entries2))
))


(time (test-generate-and-report-big (* 10 KILO) "bud" ele-tests::*test-path-primary*))
(open-store ele-tests::*test-path-primary*)
(time (find-mid-event "bud"))
(close-store)
