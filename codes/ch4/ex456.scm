;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;;a .
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?address))

;;;;b.
(and (salary (Bitdiddle Ben) ?b-salary)
     (salary ?person ?salary)
     (lisp-value < ?salary ?b-salary))

;;;;c.
(and (job ?person ?job)
     (not (job ?person (computer ?part)))
     (supervisor ?person ?boss))
