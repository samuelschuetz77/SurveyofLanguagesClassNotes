#lang racket 

(define (lookup symbl env)
        (cond
           [(null? env) (error "no environment to look in, or enviroment not right")]
           [(eq? symbl (caar env)) (cdar env)]
           [else (lookup symbl (cdr env))]
        )           
)

; (define sample-env '((a . apple)(b . bear)(c . can)(d . dove)))
(define _enviroment
    '( (+ . ,+) ;arithmetic
       (- . ,-) ; , means take the literal function, sorta
       (* . ,*)
       (/ . ,/)
    )
)

;(lookup 'c sample-env) ; => 'can

(define  (sam-eval exprn env)
    (cond 
    [(number? exprn) exprn]
    [(symbol? exprn) 
        (lookup exprn env)
    ]
    [else (error "Don't know what to do next")])
)

(sam-eval (+ 1 2) _enviroment)