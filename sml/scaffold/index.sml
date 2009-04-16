(with-sml-file (sml-path "template.sml")
  (replace title [title "Listing " plural])
  (replace body  [body
                  (msgs)
                  [h1 "Listing " plural]
                  (aif items
                      (progn
                        (page-summary pager)
                        (page-links pager)
                        [table
                         [tr (loop for s in slots do [th (slot-label s)]
                                   finally (dotimes (x 3) [th (safe "&nbsp;")]))]
                         (mapcar
                          #'(lambda (x)
                              [tr
                               (loop for s in slots do
                                     [td (omit (slot-display-value x s) maxlength)]
                                     finally
                                     (progn
                                       [td [a :href (w/p (page-uri cname "show" (oid x))) "Show"]]
                                       [td [a :href (w/p (page-uri cname "edit" (oid x))) "Edit"]]
                                       [td [a :href (page-uri cname "delete" (oid x)) "Delete"]]))])
                          it)])
                    [p "There is no " cname])
                  [div [a :href (w/p (page-uri cname "edit")) "New " cname]]]))
