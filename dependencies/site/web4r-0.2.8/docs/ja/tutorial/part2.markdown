Tutorial Part 2. HTTPサーバー
==============================
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
    - OS: Ubuntu 9.04 32bit
    - SBCL: 1.0.18
    - Apache: 2.2.11
    - hunchentoot 1.0.0
    - PHP: 5.2.6

    * 結果:
    +----------------------------------------------------------------------+
    | HTTP Server                              |  small (2B) | big (477KB) |
    +----------------------------------------------------------------------+
    | Hunchentoot stand alone                  |      3.001  |      4.561  |
    | Apache + mod_proxy + Hunchentoot         |      0.108  |      5.694  |
    | Apache + mod_php + php                   |      0.099  |      5.102  |
    | Hunchentoot + web4r                      |      3.000  |     12.376  |
    | Apache + mod_proxy + Hunchentoot + web4r |      0.175  |     13.875  |
    +----------------------------------------------------------------------+
     bigは100000回ループでコンテンツを生成しています

