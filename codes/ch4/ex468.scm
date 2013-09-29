;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

(rule (reverse (() ())))
(rule (reverse (?u . ()) (?u .())))

(rule (reverse (?u ?v . ?z) ?rvu)
      (and (reverse (?v . ?z) ?rv)
           (reverse (?u) ?ru)
           (append-to-form ?rv ?ru ?rvu)))

;; (rule (reverse ?x ?y)
      ;; (reverse ?y ?x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 只能回答 (reverse (1 2 3) ?x) 不能回答 (reverse ?x (1 2 3))
