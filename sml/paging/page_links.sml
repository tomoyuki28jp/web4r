[div :class "page_links"
     [ul (prev-link* pager)
         (loop for page from link-start to link-end
               do (if (= page current-page)
                      [li [span page]]
                      [li [a :href (concat "?" *page-param* "=" page) page]]))
         (next-link* pager)]]
