Tutorial Part 10. HTTP経由でアプリケーションをテスト
=====================================================

HTTP経由でアプリケーションをテストをする為の関数とマクロ
---------------------------------------------------------
- [with-new-cookie](http://web4r.org/en/api#with-new-cookie): 引数のフォームを新しいクッキーと共に実行
- [http-regist](http://web4r.org/en/api#http-regist): HTTP経由でユーザー登録
- [http-test-regist](http://web4r.org/en/api#http-test-regist): http-registを実行し、結果をテスト
- [http-login](http://web4r.org/en/api#http-login): HTTP経由でユーザーログイン
- [http-test-login](http://web4r.org/en/api#http-test-login): http-loginを実行し、結果をテスト
- [http-logout](http://web4r.org/en/api#http-logout): HTTP経由でユーザーログアウト
- [http-test-logout](http://web4r.org/en/api#http-test-logout): http-logoutを実行し、結果をテスト
- [http-make-instance](http://web4r.org/en/api#http-make-instance): HTTP経由ででインスタンス作成
- [http-test-make-instance](http://web4r.org/en/api#http-test-make-instance): http-make-instanceを実行し、結果をテスト
- [http-update-instance](http://web4r.org/en/api#http-update-instance): HTTP経由でインスタンス更新
- [http-test-update-instance](http://web4r.org/en/api#http-test-update-instance): http-update-instanceを実行し、結果をテスト
- [http-get-instance-by-oid](http://web4r.org/en/api#http-get-instance-by-oid): HTTP経由でoidからインスタンスのスロットの値を取得
- [http-test-get-instance-by-oid](http://web4r.org/en/api#http-test-get-instance-by-oid): http-get-instance-by-oidを実行し、結果をテスト
- [http-drop-instance-by-oid](http://web4r.org/en/api#http-drop-instance-by-oid): HTTP経由でoidからインスタンスを削除
- [http-test-drop-instance-by-oid](http://web4r.org/en/api#http-test-drop-instance-by-oid): http-drop-instance-by-oidを実行し、結果をテスト

*※ http-get-instance-by-oidを利用するには、id属性にslot idを指定したタグでslotの値を囲む必要があります。slot idは[slot-id](http://web4r.org/en/api#slot-id)にて取得可能です。*

Testマクロ
-----------
- (test (http-regist &rest args))は`(http-test-regist ,@args))と等価。
- (test (http-login &rest args)) は`(http-test-login ,@args))と等価。
- (test (http-logout &rest args))は`(http-test-logout ,@args))と等価。
- (test (http-make-instance &rest args))は`(http-test-make-instance ,@args))と等価。
- (test (http-update-instance &rest args))は`(http-test-update-instance ,@args))と等価。
- (test (http-get-instance-by-oid &rest args))は`(http-test-get-instance-by-oid ,@args))と等価。
- (test (http-drop-instance-by-oid &rest args))は`(http-test-drop-instance-by-oid ,@args))と等価。

genpagesマクロで作成したアプリケーションのテスト
---------------------------------------------------
### アプリケーションコード

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    
    (in-package :cl-user)
    (defpackage :wiki (:use :cl :web4r))
    (in-package :wiki)
    
    (ele:open-store *example-bdb*)
    
    (defpclass wiki ()
      ((title :length 256 :index t)
       (body  :length 3000)))
    (genpages wiki)
    
    (start-server)

### テストコード

    (let ((class 'wiki))
      (with-new-cookie
        (let ((oid (test (http-make-instance class :title "title1" :body "body1"))))
          (test (http-get-instance-by-oid class oid))
          (test (http-update-instance class oid '((title . "title1c") (body . "body1c"))))
          (test (http-drop-instance-by-oid class oid)))))

[Tutorial Part 9](http://web4r.org/en/tutorial9)で作成したアプリケーションのテスト
-----------------------------------------------------------------------------------
### テストコード

    (let ((class 'blog)
          (id   "user1")
          (pass "pass1"))
      (with-new-cookie
        (test (http-regist `((id    . ,id)
                             (pass  . ,pass)
                             (email . "1@1.com")
                             (blog-title . "user1's blog"))))
        (test (http-login :id id :pass pass))
        (let ((oid (test (http-make-instance class :title "title1" :body "body1"))))
          (test (http-get-instance-by-oid class oid))
          (test (http-update-instance class oid '((title . "title1c") (body . "body1c"))))
          (test (http-drop-instance-by-oid class oid)))
        (test (http-logout))))

デバッグモード
---------------
[debug-mode-on](http://web4r.org/en/api#debug-mode-on)はデバッグモードを開始し、[debug-mode-off](http://web4r.org/en/api#debug-mode-off)はデバッグモードを終了します。

デバッグモードはエラー発生時に次の２つのことを行います。

1. Lispエラーの内容をHTTP経由でHTMLとして表示
2. Lispエラーとそのバックトレースを[\*debug-log-file\*](http://web4r.org/en/api#*debug-log-file*)に記録
