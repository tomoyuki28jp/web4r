(with-sml-file (sml-path "template.sml")
  (replace title [title "Listing blog posts"])
  (replace body  [body
                  (msgs)
                  [h1 "Listing blog posts"]
                  (aif items
                      (progn
                        (page-summary pager)
                        (page-links pager)
                        [table
                         [tr (mapcar #'(lambda (s) [th (slot-label s)]) slots)
                             [th (safe "&nbsp;")]
                             (when owner-p
                               (dotimes (x 2) [th (safe "&nbsp;")]))]
                         (mapcar
                          #'(lambda (x)
                              [tr
                               (loop for s in slots do
                                     [td (omit (slot-display-value x s) 20)]
                                     finally
                                     (progn
                                       [td [a :href (page-uri "blog" "show" (oid x)) "Show"]]
                                       (when owner-p
                                         [td [a :href (w/p (page-uri "blog" "edit" (oid x))) "Edit"]]
                                         [td [a :href (page-uri "blog" "delete" (oid x)) "Delete"]]
                                         )))])
                          it)])
                    [p "There is no blog post"])
                  (when owner-p
                    [div [a :href (w/p (page-uri "blog" "edit")) "New post"]])
                  [div [a :href (page-uri "blog") "Top"]]]))
