;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (eval-and exp)
  (eval-and-terms (preds exp))

(define (first-entry preds)
  (car preds))

(define (rest-entry preds)
  (cdr preds))

(define (preds exp)
  (cdr exp))

(define (token exp)
  (car exp))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-or exp)
  (eval-or-terms (preds exp))

(define (eval-and-terms terms)
  (if (null? terms)
      #t
      (if (first-entry terms)
          (eval-and-terms (rest-entry terms))
          #f)))

(define (eval-or-terms terms)
  (if (null? terms)
      #f
      (if (first-entry terms)
          #t
          (eval-or-terms (rest-entry terms)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (and->if exp)
  (expand-and-terms (preds exp)))

(define (or->if exp)
  (expand-or-terms (preds exp)))

(define (expand-and-terms terms)
  (if (null? terms)
      #t
      (make-if (first-entry terms)
               (expand-and-terms (rest-entry terms))
               #f)))

(define (expand-or-terms terms)
  (if (null? terms)
      #f
      (make-if (first-entry terms)
               #t
               (expand-or-terms (rest-entry terms)))))
