(in-package :cl-user)
(defpackage :inflector
  (:use :cl :cl-ppcre)
  (:export :*irregular*
           :*plural*
           :*singular*
           :pluralize
           :singularize))
(in-package :inflector)

(defvar *irregular*
  '(("person" "people")
    ("man"    "men")
    ("child"  "children")
    ("sex"    "sexes")
    ("move"   "moves")
    ("cow"    "kine")))

(defvar *uncountable*
  '("equipment" "information" "rice" "money"
    "species" "series" "fish" "sheep"))

(defvar *plural*
  '(("(quiz)$" . "\\1zes")
    ("^(ox)$" . "\\1en")
    ("(m|l)ouse$" . "\\1ice")
    ("(matr|vert|ind)(?:ix|ex)$" . "\\1ices")
    ("(x|ch|ss|sh)$" . "\\1es")
    ("([^aeiouy]|qu)y$" . "\\1ies")
    ("(hive)$" . "\\1s")
    ("(?:([^f])fe|([lr])f)$" . "\\1\\2ves")
    ("sis$" . "ses")
    ("([ti])um$" . "\\1a")
    ("(buffal|tomat)o$" . "\\1oes")
    ("(bu)s$" . "\\1ses")
    ("(alias|status)$" . "\\1es")
    ("(octop|vir)us$" . "\\1i")
    ("(ax|test)is$" . "\\1es")
    ("s$" . "s")
    ("$" . "s")))

(defvar *singular*
  '(("(quiz)zes$" . "\\1")
    ("(matr)ices$" . "\\1ix")
    ("(vert|ind)ices$" . "\\1ex")
    ("^(ox)en" . "\\1")
    ("(alias|status)es$" . "\\1")
    ("(octop|vir)i$" . "\\1us")
    ("(cris|ax|test)es$" . "\\1is")
    ("(shoe)s$" . "\\1")
    ("(o)es$" . "\\1")
    ("(bus)es$" . "\\1")
    ("(m|l)ice$" . "\\1ouse")
    ("(x|ch|ss|sh)es$" . "\\1")
    ("(m)ovies$" . "\\1ovie")
    ("(s)eries$" . "\\1eries")
    ("([^aeiouy]|qu)ies$" . "\\1y")
    ("([lr])ves$" . "\\1f")
    ("(tive)s$" . "\\1")
    ("(hive)s$" . "\\1")
    ("([^f])ves$" . "\\1fe")
    ("(^analy)ses$" . "\\1sis")
    ("((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)ses$" . "\\1\\2sis")
    ("([ti])a$" . "\\1um")
    ("(n)ews$" . "\\1ews")
    ("s$" . "")))

(defun concat (&rest args)
  (apply #'concatenate 'string args))

(defun uncountable (word)
  (car (member word *uncountable* :test #'equal)))

(defun irregular (word irregular)
  (mapcar #'(lambda (x)
              (multiple-value-bind (mstart)
                  (scan (concat "(" (car x) ")$") word)
                (when mstart
                  (return-from irregular
                    (concat (subseq word 0 mstart) (cadr x))))))
          irregular)
  nil)

(defun inflect (word rules)
  (mapcar #'(lambda (x)
              (multiple-value-bind (s match?)
                  (regex-replace (car x) word (cdr x))
                (when match? (return-from inflect s))))
          rules)
  nil)

(defun pluralize (word)
  (let ((w (string-downcase word)))
    (or (uncountable w)
        (irregular w *irregular*)
        (inflect w *plural*)
        w)))

(defun singularize (word)
  (let ((w (string-downcase word)))
    (or (uncountable w)
        (irregular w (mapcar #'reverse *irregular*))
        (inflect w *singular*)
        w)))
