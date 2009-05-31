Tutorial Part 3. Defining a page
=================================
[defpage](http://web4r.org/en/api#defpage) is a macro to define a new page. Users can visit a defined page by accessing an uri like 'http://localhost:8080/PAGE'.

Syntax:

    defpage page ([path ...] [:get garg ...] [:post parg ...]
                  :auth :redirect uri :default) body

Arguments and Values:

    page---a name of the page
    path---an uri path
    garg---a symbol name of a get parameter
    parg---a symbol name of a post parameter
    :auth---if :auth is supplied and the current user hasn't logged in,
            redirects the user to the login page
    uri---an uri to redirect users after logging in
    :default---if :default is supplied, the page procedure is set to *default-handler*
    body---a form

Example1:

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)

    (start-server)

    (defpage hello (:get (name "world"))
      [p "Hello, " name])

    ; wget http://localhost:8080/hello => '<p>Hello, world</p>'
    ; wget http://localhost:8080/hello?name=tom => '<p>Hello, tom</p>'

Example2:

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)

    (start-server)

    (defpage test (path1 path2 :get get1 get2)
      [p path1 " " path2 " " get1 " " get2])

    ; wget http://localhost:8080/test/1/2/ => '<p>1 2  </p>'
    ; wget http://localhost:8080/test/1/2/?get1=3&get2=4 => '<p>1 2 3 4</p>'
