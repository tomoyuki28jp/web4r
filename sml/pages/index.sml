(with-sml-file (sml-path "template.sml")
  (replace title [title "Listing " plural])
  (append  head  [script :type "text/javascript" :src "/js/jquery-1.3.2.min.js"])
  (append  head  [script :type "text/javascript" :src "/js/jquery.pager.js"])
  (append  head  [script :type "text/javascript" :src "/js/index-page.js"])
  (replace body  [body
                  (msgs)
                  [h1 :id "title" :class cname "Listing " plural]
                  (aif items
                      (progn
                        (page-summary pager)
                        (page-links pager
                                    (concat "&slot=" (order-slot-id class)
                                            "&order=" (list-order)
                                            per-page))
                        [table :id "table_list"
                          [thead :class (concat (order-slot-id class) " " (list-order))
                           [tr (loop for s in slots
                                     as id = (slot-id s)
                                     as order = (order-param id)
                                     if (indexed-slot-p class (slot-symbol s))
                                          do [th :id id :class "sort"
                                                 [a :href (concat "?slot=" id
                                                                  "&order=" order
                                                                  per-page)
                                                    (slot-label s)]
                                                 [img :src
                                                      (if (string= id (get-parameter "slot"))
                                                          (if (string= (list-order) "asc")
                                                              "/images/order_asc.gif"
                                                              "/images/order_desc.gif")
                                                          "/images/order_no.gif")
                                                      :alt "order"]]
                                     else do [th (slot-label s)]
                                     finally (dotimes (x 3) [th (safe "&nbsp;")]))]]
                          [tbody
                           (load-sml-path "pages/list.sml" #.*web4r-package*)]])
                    [p "There is no " cname])
                  [div [a :href (w/p (page-uri cname "edit")) "New " cname]]]))
