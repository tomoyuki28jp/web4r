(in-package :web4r)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *web4r-dir*
    (asdf:component-pathname (asdf:find-system 'web4r))
    "A pathname of the web4r directory")

  (defvar *public-dir* (merge-pathnames "public/" *web4r-dir*)
    "A pathname of the public directory")

  (defvar *sml-dir*    (merge-pathnames "sml/"    *web4r-dir*)
    "A pathname of the sml directory")

  (defvar *example-bdb*
    `(:BDB ,(merge-pathnames "examples/bdb/" *web4r-dir*))
    "A pathname of the BDB database directory for example applications")

  (defvar *web4r-package* (find-package :web4r)
    "The web4r package designator"))

(defvar *upload-save-dir* #P"/tmp/web4r/upload/"
  "A pathname of the directory to save uploaded files.
 The default is #P\"/tmp/web4r/upload/\".")
(ensure-directories-exist *upload-save-dir* :verbose nil)

(defvar *tmp-save-dir* #P"/tmp/web4r/tmp/"
  "A pathname of the directory to save temporary uploaded files.
 The default is #P\"/tmp/web4r/tmp/\".")
(ensure-directories-exist *tmp-save-dir*    :verbose nil)

(defvar *tmp-files-gc-lifetime* 1440
  "The lifetime of temporary saved files in number of seconds.
 The default is 1440 (24 minutes).")

(defvar *tmp-files-gc-probability* 100
  "The probability to start a gc process for temporary saved files.
 The default is 100.")

(defvar *image-public-dirs*
  `(("upload" . *upload-save-dir*)
    ("tmp"    . *tmp-save-dir*))
  "An alist of image public directories: a string name -> a pathname of the
 public directory")

(defvar *debug-log-file* #P"/tmp/web4r/debug.log"
  "A pathname of the debug log file. The default is #P\"/tmp/web4r/debug.log\".")

(defvar *debug-mode* nil
  "If this is non-nil, debug-mode is turned on and off otherwise.")

(defvar *sid->cid*  (make-hash-table)
  "A mapping hash table: sid(session-id) -> cid(continuation-id)")

(defvar *cid->cont* (make-hash-table :test 'equal)
  "A mapping hash table: cid(continuation-id) -> an instance of the cont structure")

(defvar *cid-generated-order*
  (make-array 0 :fill-pointer 0 :adjustable t)
  "cids(continuation-ids) order by their generated time. This is used to destroy
 expired continuations.")

(defvar *cont-gc-lifetime* 1440
  "The lifetime of continuations in number of seconds.
 The default is 1440 (24 minutes).")

(defvar *cont-gc-probability* 100
  "The probability to start a gc process for expired continuations.
 The default is 100.")

(defvar *cont-sessions* (make-hash-table :test 'equal)
  "A hash table of continuation based session data: a string cid(continuation-id)
 -> alist of key and value pairs.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *last-posts* nil
    "This is only used on the process to expand last-post inside a continuation"))

(defvar *page-uri-paths* 0
  "The number of the current page uri paths.
  Example: (defpage one/two/three () web4r::*page-uri-paths*) ;=> 3")

(defvar *pages* nil
  "An alist of page handlers: nested page uri paths -> a handler function.
  Example: (defpage onw/two/three () ...) => (\"onw\" (\"two\" (\"three\" (NIL . #))")

(defvar *msgs* nil
  "This is an instance of msgs/error-msgs if there are messages to display
 and nil otherwise")

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
    (:not-a-image  . "~A must be a jpeg, png or gif image file."))
  "An alist of validation error messages: a key -> a string error message")

(defvar *validators* (make-hash-table)
  "A hash table: a keyword name of validator -> a validator function")

(defvar *valid-email-format*
  (remove #\Newline
"^(?:(?:(?:(?:[a-zA-Z0-9_!#\$\%&'*+/=?\^`{}~|\-]+)
(?:\.(?:[a-zA-Z0-9_!#\$\%&'*+/=?\^`{}~|\-]+))*)|
(?:\"(?:\\[^\r\n]|[^\\\"])*\")))\@
(?:(?:(?:[a-zA-Z0-9_!#\$\%&'*+/=?\^`{}~|\-]+)
(?:\.(?:[a-zA-Z0-9_!#\$\%&'*+/=?\^`{}~|\-]+))*))$")
  "The regular expression used to validate a email address")

(defvar *page-param* "page"
  "The get parameter name denotate the current page number.
 The default is 'page'.")

(defvar *items-per-page* 10
  "The default number to display items per page. The default is 10.")

(defvar *links-per-page* 10
  "The default number to display page links per page. The default is 10.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *slots* (make-hash-table)
    "A hash table: a symbol name of a pclass => a list of slot-options instances"))

(defvar *with-slots* nil
  "If this is :all, all pclass slots are showed. Otherwise a list of pclass
 slots to show for a page.")

(defvar *without-slots* nil
  "A list of pclass slots to hide for a page.")

(defvar *max-items-per-page* 50
  "The maximum number to display items per page. This value is only used when a
 user changes a value of the get parameter named items_per_page and the value
 exceeeds this maximum number. The default is 50.")

(defvar *max-links-per-page* 30
  "The maximum number to display page links per page. This value is only used when
 a user changes a value of the get parameter named links_per_page and the value
 exceeeds this maximum number. The default is 30.")

(defvar *user* nil
  "An instance of the user* class used for authentication")

(defvar *login-msgs*
  '((:login-failed      . "Wrong username and password combination")
    (:login-succeeded   . "Logged in")
    (:logged-out        . "Logged out")
    (:already-logged-in . "You are already logged in"))
  "An alist of log in/out messages: a keyword key -> a string message")

(defvar *thumbnail-width*  100
  "The default thumbnail width. The default is 100.")

(defvar *thumbnail-height* 100
  "The default thumbnail height. The default is 100.")
