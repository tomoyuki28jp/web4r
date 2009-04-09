(if file
    (progn [p "change: " (input-file id :id id)]
           [p "delete: " (input-checked "checkbox" nil :value "t"
                                        :name (concat id "-delete") :id id)
              [img :src (thumbnail-uri file :type type) :alt id /]])
    (input-file id :id id))
