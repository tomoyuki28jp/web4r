(with-sml-file (sml-path "template.sml")
  (replace title [title "Listing " plural])
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
                           [tr (dolist (s slots)
                                 (let* ((id    (slot-id s))
                                        (order (order-param id)))
                                   (if (indexed-slot-p class (slot-symbol s))
                                       [th :id id :class "sort"
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
                                       [th (slot-label s)])))
                               (dotimes (x 3) [th (safe "&nbsp;")])]]
                          [tbody
                           (load-sml-path "pages/list.sml" #.*web4r-package*)]])
                    [p "There is no " cname])
                  [div [a :href (w/p (page-uri cname "edit")) "New " cname]]]))
