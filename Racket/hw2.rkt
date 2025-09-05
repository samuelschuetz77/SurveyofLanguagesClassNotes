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
    `( (+ . ,+) ;arithmetic       ` =quasiquote
       (- . ,-) ; , =unquote operator
       (* . ,*)
       (/ . ,/)
       (= . ,=)
       (eq? . ,eq?)
    )
)

;(lookup 'c sample-env) ; => 'can

(define  (sam-eval exprn env)
    (cond 
    [(number? exprn) exprn]
    [(symbol? exprn) (lookup exprn env)]
    [(list? exprn) (cond
                    [(eq? (car exprn) 'quote) (cadr exprn)]
                    [(eq? (car exprn) 'if) (if  ;(if test then-expr else-expr)
                            (sam-eval (cadr exprn) env)
                            (sam-eval (caddr exprn) env)
                            (sam-eval (cadddr exprn) env) )
                    ]
                    [(eq? (car exprn) 'lambda) ;also chatgpt
                            (λ (arg)
                         (sam-eval (caddr exprn) 
                            (cons (cons (car (cadr exprn)) arg) env)))] 
                    [else
                        (apply (sam-eval (car exprn) env)         ; evaluate function, this part is all chatgpt did not have time to understand
                        (map (λ (e) (sam-eval e env))      ; evaluate args
                        (cdr exprn))
                    )]
    )]
    
    [else (error "Don't know what to do next")])
)

 


 ;;test cases for proff
(sam-eval '(quote hello) _enviroment)
(sam-eval '(quote (1 2 3)) _enviroment)

(sam-eval '(if (= 2 2) 100 200) _enviroment)
(sam-eval '(if (= 3 4) 7 8) _enviroment)

(sam-eval '(+ 10 5) _enviroment)
(sam-eval '(* 3 7) _enviroment)

(sam-eval '((lambda (x) (+ x 2)) 4) _enviroment)
(sam-eval '((lambda (y) (* y y)) 6) _enviroment)
 ;lambda test cases

;(sam-eval '(lambda (x) (+ x 1)) _enviroment)
;; => #<procedure>

;(sam-eval '((lambda (x) (+ x 1)) 5) _enviroment)
;; => 6

;(sam-eval '((lambda (y) (* y y)) 4) _enviroment)
;; => 16











