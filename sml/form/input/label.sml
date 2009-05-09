[label :for (cond ((eq format :date) (concat id "_y"))
                  ((member input '(:radio :checkbox))
                   (concat id "_" (car (slot-options slot))))
                  (t id))
       label]
