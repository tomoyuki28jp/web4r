[label :for (case type*
              (:date   (concat id "_y"))
              (:member (case input
                         ((:radio :checkbox)
                          (concat id "_" (car (slot-options slot))))
                         (t id)))
              (t id))
       label]
