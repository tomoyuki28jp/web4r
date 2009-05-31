Tutorial Part 5. 永続化クラス
==============================
web4rは[Elephant](http://common-lisp.net/project/elephant/)を利用して、永続化クラスをサポートしています。

ダウンロードとインストール
---------------------------
web4rはElephantに依存しています。もしまだElephantをダウンロードしていない場合、[このページ](http://common-lisp.net/project/elephant/downloads.html)からv0.9をダウンロードして、[設定(インストール)](http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started)して下さい。  

利用例
-------
Elephantの詳細は[マニュアル](http://common-lisp.net/project/elephant/doc/elephant.html)をご覧下さい。

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :elephant))
    (use-package :elephant)
    
    ; データベースに接続
    (open-store '(:BDB "/path/to/db"))
    
    ; 永続化クラスの定義
    (defpclass blog ()
        ((title :initarg :title :accessor title :index t)
         (body  :initarg :body)))
    
    ; 永続化クラスのインスタンスは自動的に永続化される
    (dotimes (x 10)
      (make-instance 'blog :title x :body x))
    
    (let ((all (get-instances-by-class 'blog))) ; 全てのクラスインスタンスを取得
      (print (subseq all 0 3)) ; はじめの３つだけを取り出す
      (print (sort all #'< :key 'title))) ; slotでインスタンスを並び替え
    
    ; slotの値に該当するインスタンスを取得
    (get-instances-by-value 'blog 'title 5)
    
    ; slotの値の範囲に該当するインスタンスを取得
    (get-instances-by-range 'blog 'title 2 4)
    
    ; インスタンスの更新
    (let ((i (get-instances-by-value 'blog 'title 5)))
      (setf (title (car i)) 0))
    
    ; 全てのクラスインスタンスを破棄
    (drop-instances (get-instances-by-class 'blog))
