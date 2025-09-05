#lang racket

(define (cons a b) 
  (lambda (selection) 
    (cond [(eq? selection 'first) a]
          [(eq? selection 'second) b] 
          [else (error "Invalid selection: use 'first or 'second")])
))

(define p (cons 'x 'y))

(p 'first)
(p 'second)


(define (car gg) (gg 'first))
(define (cdr gg) (gg 'second))



(car p)   ; => x
(cdr p)  ; => y

(define q (cons 1 '(2 3 4 5)))

(car q)
(cdr q)
      