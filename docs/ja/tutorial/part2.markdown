Tutorial Part 2. HTTP Server
=============================
Version 0.1.0からデフォルトHTTPサーバーは[Hunchentoot](http://www.weitz.de/hunchentoot/)になりました。

HTTPサーバーの起動と終了
-------------------------

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)

    (defparameter *srv* (start-server)) ; 起動
    (stop-server  *srv*) ; 終了

web4r:start-serverとweb4r:stop-serverはhunchentoot:startとhunchentoot:stopに対するラッパー関数です。start-server関数でサーバーを起動した場合のデフォルトのポートは8080です。詳細は[web4r:start-server](api#start-server)と[hunchentoot:acceptor](http://www.weitz.de/hunchentoot/#acceptors)をご覧下さい。

パフォーマンス
---------------
( Apache + mod\_proxy + Hunchentoot ) は ( Apache + mod\_php + php ) と同等の速度が出ます。

動的コンテンツのベンチマーク:

    * ベンチマークコマンド:
    - 'ab -c 10 -n 100 "url"'
    
    * ハードウェア:
    - CPU: 1.20GHz Intel Core 2 Duo
    - Memory: 4GB
    - Storage: 64GB SSD
    
    * ソフトウェア:
    - OS: Ubuntu 8.10 32bit
    - SBCL: 1.0.18
    - Apache: 2.2.9
    - hunchentoot 1.0.0
    - PHP: 5.2.6

    * 結果:
    +--------------------------------------------------------------+
    | HTTP Server                      | small (28B) | big (477KB) |
    +--------------------------------------------------------------+
    | Hunchentoot stand alone          | 0.217       | 128.460     |
    | Apache + mod_proxy + Hunchentoot | 0.105       |   5.077     |
    | Apache + mod_php + php           | 0.083       |   5.095     |
    +--------------------------------------------------------------+
    ※ bigは100000回ループでコンテンツを生成しています

