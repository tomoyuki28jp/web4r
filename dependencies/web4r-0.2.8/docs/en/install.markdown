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

Install
--------
1. Install all the depending libraries above
2. [Configure elephant](http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started)
3. [Download](http://web4r.org/en/download) and install web4r
   (make sure to create a symbolic link to *.asd in your load path)

For Emacs Users
----------------
[web4r.el](http://github.com/tomoyuki28jp/web4r-el)
 is elisp for editing web4r code in emacs
