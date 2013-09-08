(define (reverse-r sequence)
  (accumulate (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))


(cons v 5)
(cons 5 v)

(reverse-l v)
(reverse-r v)

;; the question is that "cons" when cons a num and a list it returns a list, but
;; when cons a list and a number , it returns a ....(tree??) i don't know what
;; it is..... just like ((3 4) . 5) hmmmm....

