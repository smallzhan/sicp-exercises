;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(rule (great? (grandson . ())))
(rule (great? (?u . ?v))
      (great? ?v))
