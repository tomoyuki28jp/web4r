(in-package :cl-user)

(defpackage :my-util-tests
  (:use :cl :my-util :5am))

(in-package :my-util-tests)

(def-suite my-util)
(in-suite  my-util)

(test ->string
  (is (string= (->string 1)    "1"))
  (is (string= (->string "1")  "1"))
  (is (string= (->string nil)  ""))
  (is (string= (->string 'a)   "A"))
  (is (string= (->string :a)   "A"))
  (is (string= (->string '(1)) "(1)")))

(test ->string-down
  (is (string= (->string-down "A") "a"))
  (is (string= (->string-down "a") "a"))
  (is (string= (->string-down 'a)  "a"))
  (is (string= (->string-down :a)  "a")))

(test ->string-up
  (is (string= (->string-up "A") "A"))
  (is (string= (->string-up "a") "A"))
  (is (string= (->string-up 'a)  "A"))
  (is (string= (->string-up :a)  "A")))

(test ->list
  (is (equal '(1) (->list 1)))
  (is (equal '(1) (->list '(1)))))

(test ->int
  (is (eq (->int 1)     1))
  (is (eq (->int "1")   1))
  (is (eq (->int '(1))  nil))
  (is (eq (->int "1.1") nil)))

(test ->keyword
  (is (eq :keyword (->keyword "keyword")))
  (is (eq :keyword (->keyword "KEYWORD")))
  (is (eq :keyword (->keyword 'keyword)))
  (is (eq :keyword (->keyword :keyword))))

(test ->symbol
  (is (eq 'symbol (->symbol "symbol")))
  (is (eq 'symbol (->symbol "SYMBOL")))
  (is (eq 'symbol (->symbol 'symbol)))
  (is (eq 'symbol (->symbol :symbol))))

(test hash->alist
  (let ((hash (make-hash-table)))
    (setf (gethash 'k1 hash) 'v1)
    (setf (gethash 'k2 hash) 'v2)
    (setf (gethash 'k3 hash) 'v3)
    (is (equal (hash->alist hash)
               #+:allegro '((k3 . v3) (k2 . v2) (k1 . v1))
               #-:allegro '((k1 . v1) (k2 . v2) (k3 . v3))))))

(test concat
  (is (string= (concat 1 2 3)                      "123"))
  (is (string= (concat "1" "2" "3")                "123"))
  (is (string= (concat nil 1 nil 2 nil 3 nil)      "123"))
  (is (string= (concat nil "1" nil "2" nil "3"nil) "123")))

(test join
  (is (string= (join "," 1 2 3) "1,2,3"))
  (is (string= (join "," nil 1 nil 2 nil 3 nil) "1,2,3"))
  (is (string= (join "," "1" "2" "3") "1,2,3"))
  (is (string= (join "," nil "1" nil "2" nil "3"nil) "1,2,3")))

(test hooks
  (let (var)
    (defun init-test-var () (setf var nil))
    (defun test-fn1 ()  (push 1 var))
    (defun test-fn2 ()  (push 2 var))
    (defun test-fn3 ()  (push 3 var))
    (defun test-fn4 (x) (push x var))
    (defun get-test-var () var))
  (progn
    (init-test-var)
    (add-hook 't1 #'test-fn1)
    (add-hook 't1 #'test-fn2)
    (add-hook 't1 #'test-fn3)
    (run-hooks 't1)
    (is (equal (get-test-var) '(3 2 1))))
  (progn
    (init-test-var)
    (add-hook 't2 #'test-fn1)
    (add-hook 't2 #'test-fn2)
    (add-hook 't2 #'test-fn3)
    (rem-hook 't2 #'test-fn2)
    (run-hooks 't2)
    (is (equal (get-test-var) '(3 1))))
  (progn
    (init-test-var)
    (add-hook 't3 #'test-fn4)
    (run-hook-with-args 't3 1)
    (run-hook-with-args 't3 2)
    (run-hook-with-args 't3 3)
    (is (equal (get-test-var) '(3 2 1)))))

