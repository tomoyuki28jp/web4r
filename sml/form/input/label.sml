(if (eq type :date)
    [label :for (concat id "-Y") label]
    (when (and label id)
      [label :for id label]))
