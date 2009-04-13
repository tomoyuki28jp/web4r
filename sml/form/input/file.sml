(if file
    (progn [p [label :for id "change: "]
              [input :type "file" :name id :id id /]]
           [p [label :for del-id "delete: "]
              [input :type "checkbox" :value "t" :name del-id :id del-id /]
              [img :src (thumbnail-uri file :type type) :alt id /]])
    [input :type "file" :name id :id id (attr (form-valid-attr slot)) /])
