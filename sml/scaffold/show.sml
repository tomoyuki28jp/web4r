(with-sml-file (sml-file-path "common/template.sml")
  :title [title "Show " cname]
  :body [body
         (aif ins
              (progn
                [table
                 (loop for s in slots do
                       [tr [th (slot-label s)]
                           [td (slot-display-value it s :nl->br t)]])])
           [p "That page doesn't exist"])])
