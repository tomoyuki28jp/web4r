(with-sml-file (sml-path "template.sml")
  (replace title [title "Listing " plural])
  (append  head  [script :type "text/javascript" :src "/js/jquery-1.3.2.min.js"])
  (append  head  [script :type "text/javascript" :src "/js/index-page.js"])
  (replace body  [body
                  (msgs)
                  [h1 :id "title" :class cname "Listing " plural]
                  (aif items
                      (progn
                        (page-summary pager)
                        (page-links pager)
                        [table :id "table_list"
                          [thead :class (concat cname "_updated-at desc")
                           [tr (loop for s in slots
                                     as c  = (when (indexed-slot-p class (slot-symbol s)) "sort")
                                     do [th :class c :id (when c (slot-id s))
                                            (slot-label s) (when c [img :src "/images/order_no.gif" :alt "order"])]
                                     finally (dotimes (x 3) [th (safe "&nbsp;")]))]]
                          [tbody
                           (mapcar
                            #'(lambda (x)
                                [tr
                                 (loop for s in slots do
                                       [td (omit (slot-display-value x s) maxlength)]
                                       finally
                                       (progn
                                         [td [a :href (w/p (page-uri cname "show" (oid x))) "Show"]]
                                         [td [a :href (w/p (page-uri cname "edit" (oid x))) "Edit"]]
                                         [td [a :href (page-uri cname "delete" (oid x))
                                                :class "delete" "Delete"]]))])
                            it)]])
                    [p "There is no " cname])
                  [div [a :href (w/p (page-uri cname "edit")) "New " cname]]]))
