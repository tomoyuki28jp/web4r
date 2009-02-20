; -*- Coding: utf-8 -*-
(in-package :web4r-tests)
(in-suite web4r-tests)

(test slots
  (is (equal (progn
               (defclass test ()
                 ((x :initarg :x)
                  (y :initarg :y)))
               (slots
                (make-instance 'test :x "x" :y "y")))
             '((x "x") (y "y"))))
  (is (equal (progn
               (defclass test ()
                 ((x :initarg :x)
                  (y :initarg :y)))
               (slots
                (make-instance 'test :x 1 :y '(l i s t))))
             '((x 1) (y (l i s t))))))

(test while
  (is (let* ((before '(1 2 3))
             (before* before)
             after)
        (while before*
          (push (pop before*) after))
        (equal before (reverse after))))
  (is (let ((before '(1 2 3 4 5))
            after)
        (while before
          (let ((x (pop before)))
            (when (oddp x)
              (push x after))))
        (equal '(1 3 5) (reverse after)))))

(test for
  (is (let (lst)
        (for (x 1 3)
          (push x lst))
        (equal '(1 2 3) (reverse lst))))
  (is (let ((start 1)
            lst)
        (for (x start (+ start 2))
          (push x lst))
        (equal '(1 2 3) (reverse lst)))))

(test aif
  (is (= (aif 3
              it)
         3))
  (is (= (aif (+ 1 1)
              (+ 1 it))
         3))
  (is (let ((x 0))
        (aif (incf x)
             it)
        1)))

(test awhen
  (is (= (awhen 3
           it)
         3))
  (is (= (let ((x 0))
           (awhen (incf x)
             (+ 1 it)))
         2))
  (is (= (let ((x 0))
           (or (awhen (not (incf x))
                 t)
               x))
         1)))

(test acond
  (is (= (acond ((+ 1 2) it)
                (t nil))
         3))
  (is (equal (acond ((null 1) (list :no it))
                    ((+ 1 2) (list :yes it))
                    (t :no))
             '(:yes 3)))
  (is (eq (acond ((= 1 2) :no)
                 (nil :no)
                 (t :yes))
          :yes)))

(test aand
  (is (= (aand (+ 1 1)
               (+ 1 it))
         3))
  (is (multiple-value-bind (*1 *2)
          (aand 1 (values it 2))
        (and (eq *1 1)
             (eq *2 2))))
  (is (multiple-value-bind (*1 *2)
          (aand 1 (values t it))
        (and (eq *1 t)
             (eq *2 1)))))

(test awhile
  (is (let* ((before '(1 2 3))
             (before* before)
             after)
        (awhile (pop before*)
          (push it after))
        (equal before (reverse after))))
  (is (let ((before '(1 2 3 4 5))
            after)
        (awhile (pop before)
          (when (oddp it)
            (push it after)))
        (equal '(1 3 5) (reverse after)))))

(test concat
  (is (string= (concat "a" "i" "u" "e" "o") "aiueo"))
  (is (string= (concat nil "a" "i" "u" "e" "o") "aiueo"))
  (is (string= (concat nil "a" "i" "u" "e" "o" nil) "aiueo"))
  (is (string= (concat nil "a" nil "i" nil "u" nil "e" nil "o" nil) "aiueo")))

(test mkstr
  (is (string= (mkstr "a" "i" "u" "e" "o") "aiueo"))
  (is (string= (mkstr nil "a" "i" "u" "e" "o") "aiueo"))
  (is (string= (mkstr nil "a" "i" "u" "e" "o" nil) "aiueo"))
  (is (string= (mkstr nil "a" nil "i" nil "u" nil "e" nil "o" nil) "aiueo"))
  (is (string= (mkstr 1 "a" 2 "i" 3) "1a2i3")))

(test symb
  (is (eq (symb "SYMB1") 'symb1))
  (is (eq (symb 'symb2)  'symb2))
  (is (eq (symb :symb3)  'symb3)))

(test make-keyword
  (is (eq (make-keyword "k1") :k1))
  (is (eq (make-keyword 'k2)  :k2))
  (is (eq (make-keyword :k3)  :k3)))

(test ->list
  (is (equal (->list 1) '(1)))
  (is (equal (->list '(1)) '(1))))

(test ->int
  (is (eq (->int "5") 5))
  (is (eq (->int  5 ) 5))
  (is (eq (->int "c") nil))
  (is (eq (->int nil) nil)))

(test with-struct
  (defstruct str
    (s1)
    (s2))
  (let ((str (make-str :s1 "s1" :s2 "s2")))
    (is (string= (str-s1 str) "s1"))
    (is (string= (str-s2 str) "s2")))
  (let ((str (make-str)))
    (with-struct (str s1 s2) str
      (setf s1 :s1)
      (setf s2 :s2)
      (is (eq (str-s1 str) :s1))
      (is (eq (str-s2 str) :s2)))))

(test with-gensyms
  (with-gensyms (g1 g2 g3)
    (is (not (symbol-package g1)))
    (is (not (symbol-package g2)))
    (is (not (symbol-package g3)))))

(test position-str
  (is (=    (position-str "1" "12345") 0))
  (is (=    (position-str "3" "12345") 2))
  (is (=    (position-str "5" "12345") 4))
  (is (null (position-str "6" "12345")))
  (is (=    (position-str "123" "12345") 0))
  (is (=    (position-str "345" "12345") 2))
  (is (null (position-str "456" "12345")))
  (is (=    (position-str "12345" "12345") 0))
  (is (null (position-str "123456" "12345")))
  )

(test split
  ; split by a character
  (is (tree-equal (split #\Space "123 45") '("123" "45") :test #'equal))
  (is (tree-equal (split #\Space " 12345") '("" "12345") :test #'equal))
  (is (tree-equal (split #\Space "12345 ") '("12345" "") :test #'equal))
  (is (tree-equal (split #\Space "12345") '("12345") :test #'equal))
  (is (tree-equal (split #\Space "") '("") :test #'equal))
  ; split by a string
  (is (tree-equal (split "1" "12345") '("" "2345") :test #'equal))
  (is (tree-equal (split "5" "12345") '("1234" "") :test #'equal))
  (is (tree-equal (split "6" "12345") '("12345") :test #'equal))
  (is (tree-equal (split ""  "12345") '("12345") :test #'equal))
  (is (tree-equal (split "123" "12345") '("" "45") :test #'equal))
  (is (tree-equal (split "345" "12345") '("12" "") :test #'equal))
  (is (tree-equal (split "12345" "12345") '("12345") :test #'equal))
  (is (tree-equal (split "12345" "123456789") '("" "6789") :test #'equal))
  (is (tree-equal (split "123" "123456789") '("" "456789") :test #'equal))
  (is (tree-equal (split "789" "123456789") '("123456" "") :test #'equal))
  )

(test replace-str
  (is (string= (replace-str "2"  "" "1212121") "1111"))
  (is (string= (replace-str "22" "" "1212212122") "121121"))
  (is (string= (replace-str "222" "" "12212221222212222") "12211212"))
  (is (string= (replace-str "222" "" "22212221222") "11"))
  )

(test assoc-ref
  (let ((lst '((200 . "number"))))
    (is (assoc-ref 200 lst)) "number")
  (let ((lst '(("200" . "string"))))
    (is (assoc-ref "200" lst :test 'equal)) "string")
  )

(test hash
  (let ((hash #.(hash 1 "one" 2 "two")))
    (is (string= (gethash 1 hash) "one"))
    (is (string= (gethash 2 hash) "two"))
    (is (null    (gethash 3 hash)))
    ))

(test sethash
  (is (equal (let ((h (make-hash-table)))
               (sethash :k1 h "v1")
               (gethash :k1 h))
             "v1"))
  (is (equal (let ((h (make-hash-table)))
               (sethash 'k2 h '(v2))
               (gethash 'k2 h))
             '(v2))))

(test hash-list
  (let ((h (hash 1 "one" 2 "two")))
    (is (equal (hash-list h) '((1 . "one") (2 . "two")))))
  (let ((h (hash :k1 "v1" :k2 nil)))
    (equal (hash-list h) '((:k1 . "v1") (:k2 . nil)))))

(test parse-float
  (is (= (parse-float "1.1") 1.1))
  (is (= (parse-float "1.0") 1.0))
  (is (eq (parse-float "1") nil))
  (is (= (parse-float "3.14159") 3.14159)))

(test preg-match
  (let ((n "12345"))
    (is (string= (preg-match "[0-9]" n) "1")))
  (let ((n "12345"))
    (is (string= (preg-match "^[0-9]+$" n) n)))
  (let ((tel "408-644-6198"))
    (is (string= (preg-match "^\\d{3}-\\d{3}-\\d{4}$" tel) tel)))
  (let ((s "asdfgjkl"))
    (is (string= (preg-match "^[a-z]+$" s) s)))
  (let ((s "ADLBDB"))
    (is (string= (preg-match "^[A-Z]+$" s) s))))

(test preg-match-all
  (let ((n "12345"))
    (is (equal (preg-match-all "[0-9]" n) '("1" "2" "3" "4" "5"))))
  (let ((s "aIuEo"))
    (is (equal (preg-match-all "[a-z]" s) '("a" "u" "o"))))
  (let ((s "aIuEo"))
    (is (equal (preg-match-all "[A-Z]" s) '("I" "E")))))

(test is-readable
  (is (eq t (is-readable "/etc/"))))

(test join
  (is (string= (join "|" '("a" "i" "u")) "a|i|u"))
  (is (string= (join ""  '("a" "i" "u")) "aiu")))

(test substr
  (is (string= (substr "12345" -2)     "45"))
  (is (string= (substr "12345" -4 -2)  "23"))
  (is (string= (substr "12345"  0 -2) "123"))
  (is (string= (substr "12345"  0  3) "123"))
  (is (string= (substr "12345"  1  3)  "23")))

(test substrlen
  (is (string= (substrlen "aiueo" 5) "aiueo"))
  (is (string= (substrlen "aiueo" 3) "aiu..."))
  (is (string= (substrlen 12345   5) "12345"))
  (is (string= (substrlen 12345   3) "123...")))

(test nl->br
  (flet ((nl->br* (str) (replace-str *nl* "" (nl->br str))))
    (is (equal (nl->br* (concat "a" *nl* "u")) "a<br>u"))
    (is (equal (nl->br* (concat "au" *nl*)) "au<br>"))
    (is (equal (nl->br* (concat *nl* "au")) "<br>au"))
    (is (equal (nl->br* (concat *nl* *nl*)) "<br><br>"))))

(test remseq
  (is (equal (remseq '(1 2 3 4 5) 1 3) '(1 4 5)))
  (is (equal (remseq '(1 2 3 4 5) 2 4) '(1 2 5)))
  (is (equal (remseq '(1 2 3 4 5) 0 2) '(3 4 5))))

(test iso-time
  (is (equal (iso-time 3161620249) "2000-03-10 04:50:49"))
  (is (equal (iso-time 3361620000) "2006-07-12 00:20:00"))
  (is (equal (iso-time 3443620249) "2009-02-15 02:10:49")))

(test time-format
  (let ((time 3443621047))
    (is (equal (time-format "~y/~m/~d" time) "2009/02/15"))
    (is (equal (time-format "~y/~m/~d ~h:~i" time) "2009/02/15 02:24"))
    (is (equal (time-format "~y-~m-~d ~h:~i:~s" time) "2009-02-15 02:24:07"))))

(test ensure-file-exist
  (loop for i from 1 to 3
        as f = (mkstr "/tmp/web4r.util.test" i)
        do (progn
             (when (is-readable f)
               (trivial-shell:shell-command (mkstr "rm -f " f)))
             (when (is-readable f)
               (error (format nil "file ~A already exists" f)))
             (ensure-file-exist f)
             (is (eq t (is-readable f)))
             (trivial-shell:shell-command (mkstr "rm -f " f)))))

(test random-hex-string
 (let ((lst (loop for i from 1 to 10
                 collect (random-hex-string 32))))
   (loop for r in lst
         as  i from 0
         do (progn
              (is (string= (preg-match "^[0-9A-F]+$" r) r))
              (is (null (member r (remseq lst i (1+ i)))))))))

(test qw
  (is (string= (qw "str") "\"str\""))
  (is (string= (qw nil) "\"\""))
  (is (string= (qw "\"") "\"\"\"")))

(test uri-encode
  (is (string= (uri-encode "ニュース速報")
               "%E3%83%8B%E3%83%A5%E3%83%BC%E3%82%B9%E9%80%9F%E5%A0%B1"))
  (is (string= (uri-encode "'.-*()_") "'.-*()_"))
  (is (string= (uri-encode "UTF-8") "UTF-8"))
  (is (string= (uri-encode " ") "%20")))

(test uri-decode
  (is (string= (uri-decode
                "%E3%83%8B%E3%83%A5%E3%83%BC%E3%82%B9%E9%80%9F%E5%A0%B1")
               "ニュース速報"))
  (is (string= (uri-decode "'.-*()_") "'.-*()_"))
  (is (string= (uri-decode "UTF-8") "UTF-8"))
  (is (string= (uri-decode "%20") " ")))

(test with-flexi-stream
  (with-open-file (in "/tmp/flexi-test" :if-does-not-exist :create)
    (with-flexi-stream (in :utf-8)
      (is (typep (flexi-streams:flexi-stream-external-format in)
                 'FLEXI-STREAMS::FLEXI-UTF-8-FORMAT)))
    (with-flexi-stream (in :iso-8859-1)
      (is (typep (flexi-streams:flexi-stream-external-format in)
                 'FLEXI-STREAMS::FLEXI-LATIN-1-FORMAT)))))

(test build-query
  (is (equal (build-query nil) "?"))
  (is (equal (build-query '(("k1" "v1"))) "?k1=v1"))
  (is (equal (build-query '(("k1" "v1") ("k2" "v2"))) "?k1=v1&k2=v2")))

(test add-get-param
  (is (equal (add-get-param "http://localhost:8080/" "k1" "new")
             "http://localhost:8080/?k1=new"))
  (is (equal (add-get-param "http://localhost:8080/?k1=v1&k2=v2" "k1" "new")
             "http://localhost:8080/?k1=new&k2=v2"))
  (is (equal (add-get-param "http://localhost:8080/?k1=v1&k2=v2&k3=v3" "k2" "new")
             "http://localhost:8080/?k1=v1&k2=new&k3=v3")))

(test add-get-params
  (is (equal (add-get-params "http://localhost:8080/" "k1" "n1" "k2" "n2")
             "http://localhost:8080/?k1=n1&k2=n2"))
  (is (equal (add-get-params "http://localhost:8080/?k1=v1&k2=v2&k3=v3"
                             "k1" "n1" "k2" "n2")
             "http://localhost:8080/?k1=n1&k2=n2&k3=v3")))

(test rem-get-param
  (is (equal (rem-get-param "http://localhost:8080/" "k4")
             "http://localhost:8080/"))
  (is (equal (rem-get-param "http://localhost:8080/?k1=v1&k2=v2&k3=v3" "k4")
             "http://localhost:8080/?k1=v1&k2=v2&k3=v3"))
  (is (equal (rem-get-param "http://localhost:8080/?k1=v1&k2=v2&k3=v3" "k1")
             "http://localhost:8080/?k2=v2&k3=v3"))
  (is (equal (rem-get-param "http://localhost:8080/?k1=v1&k2=v2&k3=v3" "k2")
             "http://localhost:8080/?k1=v1&k3=v3"))
  (is (equal (rem-get-param "http://localhost:8080/?k1=v1&k2=v2&k3=v3" "k3")
             "http://localhost:8080/?k1=v1&k2=v2")))

(test uniq-file-name
  (let ((dir "/tmp/web4r-test/")
        (times 10))
    (when (probe-file dir)
      (cl-fad:delete-directory-and-files dir))
    (ensure-directories-exist dir :verbose nil)
    (let (files)
      (dotimes (x times)
        (let ((file (uniq-file-name dir)))
          (push file files)
          (ensure-file-exist file)))
      (is (eq (length (cl-fad:list-directory dir)) times))
      (loop for f in files
            do (is (probe-file f)))
      (cl-fad:delete-directory-and-files dir))))

(test merge-cons-tree
  (labels ((key-sort (x) (sort x #'(lambda (x y) (string< (car x) (car y)))))
           (sort-equal (x y) (equal (key-sort x) (key-sort y))))
    (is (sort-equal
         (merge-cons-tree
          '(("k1" . "v1") ("k2" . "v2") ("k3" . "v3"))
          '(("k2" . "n2")))
         '(("k1" . "v1") ("k2" . "n2") ("k3" . "v3"))))
    (is (sort-equal
         (merge-cons-tree
          '(("k1" . "v1") ("k2" . (("k2-1" . "v2-1") ("k2-2" . "v2-2"))) ("k3" . "v3"))
          '(("k2" . (("k2-1" . "v2-1") ("k2-2" . "changed")))))
         '(("k1" . "v1") ("k2" . (("k2-1" . "v2-1") ("k2-2" . "changed"))) ("k3" . "v3"))))))
