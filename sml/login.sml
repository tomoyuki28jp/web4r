(with-sml-file (sml-path "template.sml")
  :title [title "login page"]
  :body  [body
          [h1 "Login"]
          (msgs)
          (form-for/cont (login/cont redirect-uri)
           :class 'user :submit "login")])
