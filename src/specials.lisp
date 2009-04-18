(in-package :web4r)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *web4r-dir*
    (awhen (load-time-value #.*compile-file-pathname*)
      (truename (merge-pathnames
                 "../" (directory-namestring it))))
    "Pathname of the web4r directory")

  (defvar *public-dir* (merge-pathnames "public/" *web4r-dir*)
    "Pathname of the public directory")

  (defvar *sml-dir*    (merge-pathnames "sml/"    *web4r-dir*)
    "Pathname of the sml directory"))

(defvar *upload-save-dir* "/tmp/web4r/upload/"
  "Directory path to save upload files")
(ensure-directories-exist *upload-save-dir* :verbose nil)

(defvar *tmp-save-dir* "/tmp/web4r/tmp/"
  "Directory path to save temporary uploaded files")
(ensure-directories-exist *tmp-save-dir*    :verbose nil)

(defvar *tmp-files-gc-lifetime* 1440
  "Temporary saved files lifetime")

(defvar *tmp-files-gc-probability* 100
  "Probability to start a gc process for temporary saved files")

(defvar *image-public-dirs*
  `(("upload" . *upload-save-dir*)
    ("tmp"    . *tmp-save-dir*))
  "Alist of image public directories")

(defvar *debug-log-file* #P"/tmp/web4r/debug.log"
  "Pathname of the debug log file")

(defvar *debug-mode* nil
  "Debug mode flag")

(defvar *sid->cid*  (make-hash-table)
  "sid(session-id) -> cid(continuation-id) : mapping index")

(defvar *cid->cont* (make-hash-table :test 'equal)
  "cid(continuation-id) -> instance of the cont structure")

(defvar *cid-generated-order*
  (make-array 0 :fill-pointer 0 :adjustable t)
  "cids by the order of their generated time.
   this will be used to destroy expired continuations")

(defvar *cont-gc-lifetime* 1440
  "continuation lifetime")

(defvar *cont-gc-probability* 100
  "probability to start a gc process")

(defvar *cont-sessions* (make-hash-table :test 'equal)
  "continuations based session data")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *last-posts* nil))

(defvar *page-uri-paths* 0
  "The number of current page uri paths")

(defvar *pages* nil
  "Page paths -> handler alist")

(defvar *msgs* nil
  "Instance of the msgs structure")

(defvar *error-formats*
  '((:invalid      . "~A is invalid")
    (:empty        . "~A can't be empty")
    (:too-long     . "~A is too long (maximum is ~D characters)")
    (:too-short    . "~A is too short (minimum is ~D characters)")
    (:too-big      . "~A is too big (maximum is ~D bytes)")
    (:too-small    . "~A is too small (minimum is ~D bytes)")
    (:not-a-number . "~A is not a number")
    (:not-alpha    . "~A must contains only alphabetic characters")
    (:not-alnum    . "~A must contains only alphabetic and digit characters")
    (:not-a-unique . "The same ~A has already been registered")
    (:not-a-image  . "~A must be a jpeg, png or gif image file.")))

(defvar *validators* (make-hash-table))

(defvar *valid-email-format*
  (remove #\Newline
"^(?:(?:(?:(?:[a-zA-Z0-9_!#\$\%&'*+/=?\^`{}~|\-]+)
(?:\.(?:[a-zA-Z0-9_!#\$\%&'*+/=?\^`{}~|\-]+))*)|
(?:\"(?:\\[^\r\n]|[^\\\"])*\")))\@
(?:(?:(?:[a-zA-Z0-9_!#\$\%&'*+/=?\^`{}~|\-]+)
(?:\.(?:[a-zA-Z0-9_!#\$\%&'*+/=?\^`{}~|\-]+))*))$"))

(defvar *page-param* "page"
  "Name of a page get parameter")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *slots* (make-hash-table)))

(defvar *unique-slots* (make-hash-table :test 'equal))

(defvar *slot-indices* (make-hash-table :test 'equal))

(defvar *with-slots* nil)

(defvar *without-slots* nil)

(defvar *login-msgs*
  '((:login-failed      . "Wrong username and password combination")
    (:login-succeeded   . "Logged in")
    (:logged-out        . "Logged out")
    (:already-logged-in . "You are already logged in")))

(defvar *thumbnail-width*  100)
(defvar *thumbnail-height* 100)
