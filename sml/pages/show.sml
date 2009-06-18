(with-sml-file (sml-path "template.sml")
  (replace title [title "Show " cname])
  (replace body  [body (aif ins
                           [table :id "table_show"
                                  (dolist (s slots)
                                    [tr [th (slot-label s)]
                                        [td :id (slot-id s)
                                            (slot-display-value it s :nl->br t)]])]
                         [p "That page doesn't exist"])]))
