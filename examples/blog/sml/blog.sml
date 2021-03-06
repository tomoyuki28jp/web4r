(with-sml-file (sml-path "template.sml")
  (replace title [title "Listing blogs"])
  (replace body  [body
                  (msgs)
                  [h1 "Listing blogs"]
                  (aif items
                      (progn
                        (page-summary pager)
                        (page-links pager)
                        [table
                         (loop for i in items do
                               [tr [td [a :href (page-uri "blog" "index" (id i))
                                          (slot-value i 'blog-title)]]
                                   [td (time-format "~y/~m/~d" (updated-at i))]])])
                    [p "There is no blog"])
                  (if (login-user)
                      [div [a :href (page-uri "blog" "index" (login-user-id)) "My Blog"] " | "
                           [a :href (page-uri "logout") "Logout"]]
                      [div [a :href (page-uri "login")  "Login"] " | "
                           [a :href (page-uri "regist") "Sign up"]])]))
