Tutorial Part 3. Defining a web page
=================================
[defpage](http://web4r.org/en/api#defpage) is a macro to define a web page. Users can visit a defined web page by accessing an uri like 'http://localhost:8080/PATHS'.

Syntax:

    defpage paths ([parg ...] [:get garg ...] [:post parg ...]
                  :auth :default :redirect redirect) body

Arguments and Values:

    paths---a base path of a web page
    parg---a relative uri path from the base PATHS
    garg---a symbol name of a get parameter
    parg---a symbol name of a post parameter
    :auth---if :auth is supplied and the current user hasn't logged in,
            redirects the user to a login page
    :default---if :default is supplied, the page procedure is set to *default-handler*
    redirect---an uri to redirect users after logging in
    body---a form

Example1:  
*You can easily get a path or a get/post parameter.*

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)

    (start-server)

    (defpage test (path1 path2 :get get1 get2)
      [p path1 " " path2 " " get1 " " get2])

    ; wget "http://localhost:8080/test/p1/p2/" => '<p>p1 p2  </p>'
    ; wget "http://localhost:8080/test/p1/p2/?get1=g1&get2=g2" => '<p>p1 p2 g1 g2</p>'

Example2:  
*You can set a default value of a path or a get/post parameter.*

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)

    (start-server)

    (defpage hello (:get (name "world"))
      [p "Hello, " name])

    ; wget "http://localhost:8080/hello" => '<p>Hello, world</p>'
    ; wget "http://localhost:8080/hello?name=tomo" => '<p>Hello, tomo</p>'

Example3:  
*PATHS can be multiple paths. In that case, PARG becomes a relative path from the base PATHS.*

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)

    (start-server)

    (defpage one/two (path1 path2) [p "two " path1 " " path2])
    (defpage one/two/three (path1 path2) [p "three " path1 " " path2])

    ; wget "http://localhost:8080/one/two/foo/bar/" => '<p>two foo bar</p>'
    ; wget "http://localhost:8080/one/two/three/foo/bar/" => '<p>three foo bar</p>'

