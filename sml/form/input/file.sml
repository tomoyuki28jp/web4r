(if file
    (progn [p [label :for id "change: "]
              (input-file id :id id)]
           [p [label :for del-id "delete: "]
              (input-checked "checkbox" nil :value "t"
                             :name del-id :id del-id)
              [img :src (thumbnail-uri file :type type) :alt id /]])
    [input (attr (form-valid-attr slot))
           :type "file" :name id :id id])
