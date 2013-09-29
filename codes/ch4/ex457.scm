;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; person-1 能替代 person-2
(rule (can-substitute ?person-1 ?person-2)
      (and (job ?person-1 ?job-1)
           (job ?person-2 ?job-2)
           (and (not (same ?person-1 ?person-2))
                (or (same ?job-1 ?job-2)
                    (can-do-job ?job-1 ?job-2)))))

;;;; a.
(can-substitute ?person (Fect Cy D))

;;;; b.
(and (can-substitute ?p1 ?p2)
     (salary ?p1 ?s1)
     (salary ?p2 ?s2)
     (lisp-value > ?s2 ?s1))
