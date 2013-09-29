;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(wheel ?who)

;;;; 产生重复结果的原因是通过不同的路径和框架进行查询的。
;;;; 例如 (wheel (Warbucks Oliver)) 可以是
;;;; (and (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
;;;;      (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
;;;; 也可以是
;;;; (and (supervisor (Tweakit Lem E) (Bitdiddle Ben))
;;;;      (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
;;;; 正好 Bitdiddle Ben 管理了 4 个人，因此出现了 4 次。
