;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; 产生多个的问题在于规则是通过不同的分支从不同的框架得到的。
;;;; 例如对 lives-near ?person-1 ?person-2 可以对称的匹配到对应的两个人。
;;;; 可以采用 p1 p2 排序的方式，让他们只出现一次。

(rule (lives-near-unique ?p1 ?p2)
      (and (address ?p1 (?town . ?rest-1))
           (address ?p2 (?town . ?rest-2))
           (lisp-value ge ?p1 ?p2)))

;;;; ge 是大于的意思，必须作为基本过程安装在查询器中。