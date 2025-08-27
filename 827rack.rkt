#lang racket

(define (lookup-by-name name dictionary)
    (if (null? dictionary)
    null
    (if (eq? name (car(car dictionary)))
        (cadr (car dictionary))
        (lookup-by-name name (cdr dictionary)))))

(define (eval code)
    
    (define primitves 
        (list 
            (list(quote +)+)
            (list(quote -)-)
        )
    )
    (define operator (lookup-by-name (car code) primitves))
    (operator
        (car code)
        (cadr code)(caddr code))
)



(displayln (eval(quote(+ 2 3))))
; (displayln (eval (quote 10)))
