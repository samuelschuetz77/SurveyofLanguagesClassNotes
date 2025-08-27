#lang racket

;; lookup-by-name : Symbol (Listof (List Symbol Any)) -> Any or #f
(define (lookup-by-name name dictionary)
  (cond
    [(empty? dictionary) #f]
    [(eq? name (caar dictionary)) (cadar dictionary)]
    [else (lookup-by-name name (cdr dictionary))]))

(define (my-eval code)
  ;; association list: (list (list 'symbol procedure) ...)
  (define primitives
    (list (list '+ +)
          (list '- -)
          (list '* *)
          (list '/ /)))

  (define op (lookup-by-name (car code) primitives))
  (unless op
    (error 'my-eval "unknown operator: ~a" (car code)))

  ;; apply the operator to all operands (supports any arity)
  (apply op (cdr code)))

;; tests
(displayln (my-eval '(+ 2 3)))        ; 5
(displayln (my-eval '(* 2 3 4)))      ; 24
(displayln (my-eval '(- 10 1 2 3)))   ; 4
