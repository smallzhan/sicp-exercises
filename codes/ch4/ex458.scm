;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 换成 “上司在其他部门”似乎就可以了
(rule (big-shot ?p ?d)
      (and (job ?p (?d . ?t1))
           (job ?p2 (?d2 . ?t2))
           (supervisor ?p ?p2)
           (not (same ?d ?d2))))
