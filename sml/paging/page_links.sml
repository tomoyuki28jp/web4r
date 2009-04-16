[div :class "page_links"
     (loop for page from link-start to link-end
           do (if (= page current-page)
                  [span page]
                  [a :href (concat "?page=" page) page]))]
