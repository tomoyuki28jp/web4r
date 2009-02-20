(in-package :web4r)

(defvar *crlf* (format nil "~C~C" #\Return #\Linefeed))

(defvar *nl*   (format nil "~%"))

(defparameter *the-random-state* (make-random-state t))

(defmacro pm  (expr)
  `(pprint (macroexpand   ',expr)))

(defmacro pm1 (expr)
  `(pprint (macroexpand-1 ',expr)))

(defun slots (instance)
  "returns a list of slots' name and value for debugging"
  (mapcar #'(lambda (slot)
              (let ((name (ele::slot-definition-name slot)))
                (list name
                      (if (slot-boundp instance name)
                          (slot-value instance name)
                          :unbound))))
          (ele::class-slots (class-of instance))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concat (&rest args)
    (apply #'concatenate
           (append '(string)
                   (remove nil args)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args)
        (when a
          (princ a s))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (values (intern (apply #'mkstr args)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-keyword (x)
    (if (keywordp x)
        x
        (let ((str (if (stringp x)
                       x
                       (mkstr x))))
          (intern (string-upcase str) :keyword)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ->list (x)
    (if (listp x)
        x
        (list x))))

(defun ->int (x)
  (if (integerp x)
    x
    (ignore-errors (parse-integer x))))

(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (symbol-macrolet
           ,(mapcar #'(lambda (f)
                        `(,f (,(symb name "-" f) ,gs)))
                    fields)
         ,@body))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun position-str (delimiter str &key (start 0))
    (when (plusp start)
      (setq str (subseq str start (length str))))
    (let ((del-len  (length delimiter))
          (str-len  (length str))
          (1st-char (char delimiter 0)))
      (loop for i = 0 then (1+ j)
         as  j = (position 1st-char str :start i)
         when (and j
                   (<= (+ j del-len) str-len)
                   (string= delimiter (subseq str j (+ j del-len))))
         return (+ j start)
         end
         while j))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric split (delimiter str)
    (:documentation "Split a string by a delimiter"))

  (defmethod split ((delimiter character) (str string))
    (loop for i = 0 then (1+ j)
       as  j = (position delimiter str :start i)
       collect (subseq str i j)
       while j))

  (defmethod split ((delimiter string) (str string))
    (if (or (string= delimiter "")
            (string= delimiter str))
        (list str)
        (let ((del-len (length delimiter)))
          (loop for i = 0 then (+ j del-len)
             as  j = (position-str delimiter str :start i)
             collect (subseq str i j)
             while j)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun replace-str (search replace str)
    (let ((s (split search str)))
      (if (plusp (length s))
          (join replace s)
          str))))

(defun assoc-ref (item alist &rest args)
  (awhen (apply #'assoc item alist args)
       (cdr it)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun hash (&rest lst)
    (let ((hash (make-hash-table)))
      (while (<= 2 (length lst))
        (setf (gethash (pop lst) hash) (pop lst)))
      hash)))

(defmacro sethash (key hash value)
  `(setf (gethash ,key ,hash) ,value))

(defun hash-list (hash-table)
  (declare (hash-table hash-table))
  (loop for key being the hash-key of hash-table
     using (hash-value value)
     collect (cons key value)))

(defun parse-float (string)
  (let ((*read-eval* nil)
        (float (ignore-errors (read-from-string string)))
        (types '(short-float single-float double-float long-float)))
    (when (member (type-of float) types)
      float)))

(defmacro preg-match (regexp str)
  `(cl-ppcre:scan-to-strings
    (cl-ppcre:create-scanner ,regexp) ,str))

(defmacro preg-match-all (regexp str)
  `(cl-ppcre:all-matches-as-strings
    (cl-ppcre:create-scanner ,regexp) ,str))

(defun is-readable (file)
  (let ((file (namestring file)))
    (unless (not (probe-file file))
      (handler-case
          (with-open-file (stream file)
            t)
        (error () nil)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun join (joiner lst)
    (format nil (mkstr "~{~A~^" joiner "~}") lst)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun substr (str start &optional end)
    (when (minusp start)
      (setf start (+ (length str) start)))
    (when (and (integerp end) (minusp end))
      (setf end (+ (length str) end)))
    (subseq str start end)))

(defun substrlen (str max &optional (omark "..."))
  (let ((str (if (stringp str) str (mkstr str))))
    (if (<= (length str) max)
        str
        (mkstr (subseq str 0 max) omark))))

(defun nl->br (str)
  (when str
    (replace-str *nl* (br) str)))

(defun remseq (seq start end)
  (remove-if (constantly t) seq :start start :end end))

(defun iso-time (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

(defun time-format (format &optional (time (get-universal-time)))
  (multiple-value-bind (s i h d m y) (decode-universal-time time)
    (let ((y (format nil "~4,'0d" y))
          (m (format nil "~2,'0d" m))
          (d (format nil "~2,'0d" d))
          (h (format nil "~2,'0d" h))
          (i (format nil "~2,'0d" i))
          (s (format nil "~2,'0d" s)))
      (loop for f in '("~y" "~m" "~d" "~h" "~i" "~s")
            as  r in  (list y m d h i s)
            do  (setf format (replace-str f r format)))
      format)))

(defun ensure-file-exist (file)
  (handler-case
      (with-open-file
          (stream file :direction :output :if-exists :append
                  :if-does-not-exist :create)
        t)
    (error () nil)))

(defun random-hex-string (length)
  (format nil "~v,'0x"
          length (random (expt 16 length) *the-random-state*)))

(defun qw (str)
  "returns a quoted string"
  (if str
      (format nil "\"~A\"" str)
      "\"\""))

(defun uri-encode (str)
  (with-output-to-string (s)
    (loop for c across str
          for i from 0
          do (if (or (char<= #\0 c #\9)
                     (char<= #\a c #\z)
                     (char<= #\A c #\Z)
                     (find c "'.-*()_" :test #'char=))
                 (write-char c s)
                 (loop for o across
                       (string-to-octets str :start i :end (1+ i)
                                         :external-format :utf-8)
                       do (format s "%~2,'0x" o))))))

(defun uri-decode (str)
  (let* ((len (length str))
         (vec (make-array
               len :element-type '(unsigned-byte 8) :fill-pointer 0))
         (idx 0))
    (flet ((vec-push (x) (vector-push x vec)))
      (while (< idx len)
        (let ((c (char str idx)))
          (cond ((char= c #\%)
                 (vec-push
                  (parse-integer
                   str :start (1+ idx) :end (+ 3 idx) :radix 16))
                 (incf idx 2))
                ((char= c #\+) (vec-push 32))
                (t (vec-push
                    (elt (string-to-octets
                          (string c) :external-format :utf-8) 0)))))
        (incf idx)))
    (octets-to-string vec :external-format :utf-8)))

(defmacro with-flexi-stream ((stream external-format) &rest body)
  `(let ((,stream (make-flexi-stream
                   ,stream :external-format ,external-format)))
     ,@body))

(defun build-query (args)
  (concat "?" (join "&" (loop for a in args
                              collect (mkstr (car a) "=" (nth 1 a))))))

(defun add-get-param (link key value)
  (aif (position-str (concat key "=") link)
    (let ((len (1+ (length key))))
      (replace-str
       (subseq link (+ len it) (position #\& link :start (+ len it)))
       value link))
    (mkstr link (if (position #\? link) "&" "?") key "=" value)))

(defun add-get-params (link &rest params)
  (loop for (k v) on params by #'cddr
        do (setf link (add-get-param link k v)))
  link)

(defun rem-get-param (link key)
  (aif (position-str (concat key "=") link)
       (let* ((len (1+ (length key)))
              (rem (replace-str
                    (subseq link it
                            (awhen (position #\& link :start (+ len it))
                              (1+ it)))
                    ""
                    link)))
         (if (string= (substr rem -1) "&")
             (substr rem 0 -1)
             rem))
       link))

(defun uniq-file-name (dir &optional (name-length 10))
  (dotimes (x 5)
    (let ((file (concat dir (random-hex-string name-length))))
      (unless (probe-file file)
        (return-from uniq-file-name file)))))

(defun merge-cons-tree (old new)
  (cond ((null new) old)
        ((listp new)
         (if (listp old)
             (loop for k in (union (mapcar #'car old)
                                   (mapcar #'car new)
                                   :test #'equal)
                   collect (cons k (merge-cons-tree
                                    (assoc-ref k old :test #'equal)
                                    (assoc-ref k new :test #'equal))))
             new))
        (t new)))
