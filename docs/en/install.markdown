Install
========

Supported Platforms
--------------------
Only tested with SBCL 1.0.18 - 1.0.19 and Allegro CL 8.1 on Linux x86 32bit 

Library Dependencies
---------------------
- [asdf](http://www.cliki.net/asdf)
- [my-util](http://github.com/tomoyuki28jp/my-util/tree/master)
- [sml](http://github.com/tomoyuki28jp/sml/tree/master)
- [inflector](http://github.com/tomoyuki28jp/inflector/tree/master)
- [hunchentoot](http://www.weitz.de/hunchentoot/) (v1.0.0 or higher)
- [bordeaux-threads](http://common-lisp.net/project/bordeaux-threads/)
- [elephant](http://common-lisp.net/project/elephant/) (only works with v0.9)
- [trivial-shell](http://common-lisp.net/project/trivial-shell/)
- [cl-gd](http://weitz.de/cl-gd/)
- [flexi-streams](http://www.weitz.de/flexi-streams/)
- [trivial-backtrace](http://common-lisp.net/project/trivial-backtrace/)
- [drakma](http://weitz.de/drakma/)
- [closure-html](http://common-lisp.net/project/closure/closure-html/)
- [fiveam](http://common-lisp.net/project/bese/FiveAM.html) (required by web4r-tests)

Installing on Debian/Ubuntu
----------------------------
    sudo apt-get install build-essential sbcl cl-gd cl-base64
    
    # Install Berkeley DB 4.5
    wget http://download.oracle.com/berkeley-db/db-4.5.20.tar.gz
    tar -zxvf db-4.5.20.tar.gz && rm -f db-4.5.20.tar.gz
    cd db-4.5.20/build_unix/
    ../dist/configure
    make && sudo make install
    
    # Install Elephant 0.9
    mkdir -p ~/.sbcl/site/; mkdir -p ~/.sbcl/systems/; cd ~/.sbcl/site/
    wget http://www.common-lisp.net/project/elephant/dist/elephant-0.9.tar.gz
    tar -zxvf elephant-0.9.tar.gz && rm -f elephant-0.9.tar.gz
    ln -s ~/.sbcl/site/elephant/*.asd ~/.sbcl/systems/
    cp ~/.sbcl/site/elephant/config.sexp ~/.sbcl/site/elephant/my-config.sexp
    
    sbcl
    (require :asdf-install)
    (asdf-install:install :web4r)

For Emacs Users
----------------
[web4r.el](http://github.com/tomoyuki28jp/web4r-el)
 is elisp for editing web4r code in emacs
