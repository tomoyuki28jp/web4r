Install
========

プラットフォーム
--------------------
Linux x86 32bit上のSBCL 1.0.18 - 1.0.19とAllegro CL 8.1でのみ動作確認済。

依存ライブラリ
---------------------
- [asdf](http://www.cliki.net/asdf)
- [my-util](http://github.com/tomoyuki28jp/my-util/tree/master)
- [sml](http://github.com/tomoyuki28jp/sml/tree/master)
- [inflector](http://github.com/tomoyuki28jp/inflector/tree/master)
- [hunchentoot](http://www.weitz.de/hunchentoot/) (v1.0.0以上)
- [bordeaux-threads](http://common-lisp.net/project/bordeaux-threads/)
- [elephant](http://common-lisp.net/project/elephant/) (v0.9のみサポート)
- [trivial-shell](http://common-lisp.net/project/trivial-shell/)
- [cl-gd](http://weitz.de/cl-gd/)
- [flexi-streams](http://www.weitz.de/flexi-streams/)
- [trivial-backtrace](http://common-lisp.net/project/trivial-backtrace/)
- [drakma](http://weitz.de/drakma/)
- [closure-html](http://common-lisp.net/project/closure/closure-html/)
- [fiveam](http://common-lisp.net/project/bese/FiveAM.html) (web4r-testsでのみ利用)

インストール
------------
1. 上記の依存ライブラリを全てインストール
2. [Elephantを設定](http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started)
3. web4rを[ダウンロード](http://web4r.org/en/download) してインストール
   (web4r/*.asdへのシンボリックリンクをロードパスに作成)

Emacs Lisp
----------------
[web4r.el](http://github.com/tomoyuki28jp/web4r-el)
はemacs上でweb4rコードを編集する為のelispです。
