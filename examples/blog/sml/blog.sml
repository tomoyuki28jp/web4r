(with-sml-file (sml-file-path "common/template.sml")
  :title [title "Listing blog"]
  :body [body
         (msgs)
         [h1 "Listing blog"]
         (aif items
              (progn
                (page-summary pager)
                (page-links pager)
                [table
                 (loop for i in items do
                      [tr [td [a :href (page-uri "blog" "index" (id i))
                                    (slot-value i 'blog-title)]]
                          [td (time-format "~y/~m/~d" (updated-at i))]])])
              [p "There are no blog yet"])
         (if (login-user)
             [p [a :href (page-uri "blog" "index" (login-user-id)) "My Blog"] " | "
                [a :href (page-uri "logout") "Logout"]]
             [p [a :href (page-uri "login")  "Login"] " | "
                [a :href (page-uri "regist") "Sign up"]])])
