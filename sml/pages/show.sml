(with-sml-file (sml-path "template.sml")
  (replace title [title "Show " cname])
  (replace body  [body (aif ins
                           [table :id "table_show"
                                  (loop for s in slots do
                                        [tr [th (slot-label s)]
                                            [td (slot-display-value it s :nl->br t)]])]
                         [p "That page doesn't exist"])]))
