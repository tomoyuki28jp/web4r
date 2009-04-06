(in-package :web4r-tests)
(in-suite web4r)

(test assoc*
  (is (eq    (assoc* '(1 2 3)   '((1 (2 (3 . 123))))) 123))
  (is (eq    (assoc* '(1 2 3 4) '((1 (2 (3 . 123))))) nil))
  (is (equal (assoc* '(1)       '((1 (2 (3 . 123))))) '((2 . ((3 . 123))))))
  (is (equal (assoc* '(1 2)     '((1 (2 (3 . 123))))) '((3 . 123)))))

(test replace-assoc*
  (let (alist)
    (setf alist (replace-assoc* '(1 nil)     alist 1))
    (setf alist (replace-assoc* '(1 2 nil)   alist 12))
    (setf alist (replace-assoc* '(1 2 3 nil) alist 123))
    (is (eq (assoc* '(1 nil)     alist) 1))
    (is (eq (assoc* '(1 2 nil)   alist) 12))
    (is (eq (assoc* '(1 2 3 nil) alist) 123))))

(test add-parameter
  (is (string= (add-parameter "http://localhost/" "k1" "new")
               "http://localhost/?k1=new"))
  (is (string= (add-parameter "http://localhost/?" "k1" "new")
               "http://localhost/?k1=new"))
  (is (string= (add-parameter "http://localhost/?k1=v1" "k1" "new")
               "http://localhost/?k1=new"))
  (is (string= (add-parameter "http://localhost/?k1=v1&k2=v2&k3=v3" "k1" "new")
               "http://localhost/?k1=new&k2=v2&k3=v3"))
  (is (string= (add-parameter "http://localhost/?k1=v1&k2=v2&k3=v3" "k2" "new")
               "http://localhost/?k1=v1&k2=new&k3=v3"))
  (is (string= (add-parameter "http://localhost/?k1=v1&k2=v2&k3=v3" "k3" "new")
               "http://localhost/?k1=v1&k2=v2&k3=new")))

(test add-parameters
  (is (string= (add-parameters "http://localhost/" "k1" "v1")
               "http://localhost/?k1=v1"))
  (is (string= (add-parameters "http://localhost/" "k1" "v1" "k2" "v2" "k3" "v3")
               "http://localhost/?k1=v1&k2=v2&k3=v3"))
  (is (string= (add-parameters "http://localhost/?k1=v1&k2=v2&k3=v3"
                               "k1" "n1" "k2" "n2" "k3" "n3" "k4" "n4")
               "http://localhost/?k1=n1&k2=n2&k3=n3&k4=n4")))

(test rem-parameter
  (is (string= (rem-parameter "http://localhost/?k1=v1" "k1")
               "http://localhost/"))
  (is (string= (rem-parameter "http://localhost/?k1=v1&k2=v2" "k1")
               "http://localhost/?k2=v2"))
  (is (string= (rem-parameter "http://localhost/?k1=v1&k2=v2" "k2")
               "http://localhost/?k1=v1"))
  (is (string= (rem-parameter "http://localhost/?k1=v1&k2=v2&k3=v3" "k1")
               "http://localhost/?k2=v2&k3=v3"))
  (is (string= (rem-parameter "http://localhost/?k1=v1&k2=v2&k3=v3" "k2")
               "http://localhost/?k1=v1&k3=v3"))
  (is (string= (rem-parameter "http://localhost/?k1=v1&k2=v2&k3=v3" "k3")
               "http://localhost/?k1=v1&k2=v2")))

(test omit
  (is (string= (omit "12345" 5) "12345"))
  (is (string= (omit "12345" 3) "123..."))
  (is (string= (omit "12345" 4) "1234...")))

(test time-format
  (let ((time 3443621047))
    (is (equal (time-format "~y/~m/~d" time)
               "2009/02/15"))
    (is (equal (time-format "~y/~m/~d ~h:~i" time)
               "2009/02/15 02:24"))
    (is (equal (time-format "~y-~m-~d ~h:~i:~s" time)
               "2009-02-15 02:24:07"))))
