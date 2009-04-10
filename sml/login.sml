(with-sml-file (sml-path "template.sml")
  (replace title [title "login page"])
  (replace body  [body
                  [h1 "Login"]
                  (msgs)
                  (form-for/cont (login/cont redirect-uri)
                    :class 'user :submit "login")]))
