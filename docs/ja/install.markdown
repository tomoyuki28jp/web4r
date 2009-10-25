Install
========

プラットフォーム
-----------------
Linux x86 32bit上のSBCL 1.0.18 - 1.0.19とAllegro CL 8.1でのみ動作確認済。

Debian/Ubuntuにインストール
----------------------------

    sudo apt-get update && sudo apt-get install build-essential sbcl cl-gd git-core

    git clone git://github.com/tomoyuki28jp/web4r.git && cd web4r
    git branch all-in-one origin/all-in-one && git checkout all-in-one

    cd dependencies && tar -zxvf db-4.5.20.tar.gz && cd db-4.5.20/build_unix/ 
    ../dist/configure && make && sudo make install

    cd ../..; echo "(pushnew \"`pwd`/systems/\" asdf:*central-registry* :test #'equal)" >> ~/.sbclrc

Emacs Lisp
-----------
[web4r.el](http://github.com/tomoyuki28jp/web4r-el)
はemacs上でweb4rコードを編集する為のelispです。
