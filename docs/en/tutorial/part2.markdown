Tutorial Part 2. HTTP Server
=============================
The default HTTP server has been [Hunchentoot](http://www.weitz.de/hunchentoot/) since version 0.1.0. 

Starting and Stopping your HTTP server
---------------------------------------

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)

    (defparameter *srv* (start-server)) ; starts your server
    (stop-server  *srv*) ; stops your server

start-server and stop-server are wrapper functions defined in the web4r package for hunchentoot:start and hunchentoot:stop. The default port is 8080. See [web4r:start-server](api#start-server) and [hunchentoot:acceptor](http://www.weitz.de/hunchentoot/#acceptors) for detail.

Performance
------------
( Apache + mod\_proxy + Hunchentoot ) is as fast as ( Apache + mod\_php + php ).

Dynamic content benchmark:

    * Benchmark command:
    - 'ab -c 10 -n 100 "url"'
    
    * Hardware:
    - CPU: 1.20GHz Intel Core 2 Duo
    - Memory: 4GB
    - Storage: 64GB SSD
    
    * Software:
    - OS: Ubuntu 8.10 32bit
    - SBCL: 1.0.18
    - Apache: 2.2.9
    - hunchentoot 1.0.0
    - PHP: 5.2.6

    * Result:
    +--------------------------------------------------------------+
    | HTTP Server                      | small (28B) | big (477KB) |
    +--------------------------------------------------------------+
    | Hunchentoot stand alone          | 0.217       | 128.460     |
    | Apache + mod_proxy + Hunchentoot | 0.105       |   5.077     |
    | Apache + mod_php + php           | 0.083       |   5.095     |
    +--------------------------------------------------------------+
    Note: The big content is generated by 100000 times loop