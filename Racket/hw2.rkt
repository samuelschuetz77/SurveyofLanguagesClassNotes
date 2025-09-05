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


;smallest case transform:  '(a+b) would need tranform => '(+ a b)

(define (transform1 expn) (
    if (= (length expn) 3) 
    (list (cadr expn) (car expn) (caddr expn))
    (transform1 (list (list (cadr expn) (car expn) (caddr expn))
                        (cadddr expn)
                            (cadddr (cdr expn))  
              )
    )
))
;;precedance tranform2, respector of PEMDAS sorta

(define (transform2 expn) (cond
    [(= (length expn) 1)
     (car expn)]
    [else
     (let ([pos (or (find-op expn '(+ -))
                    (find-op expn '(* /)))])
       (if pos
           (list (list-ref expn pos)
                 (transform2 (take expn pos))
                 (transform2 (drop expn (+ pos 1))))
           expn))]))

;;fin-op for transform2
(define (find-op expr ops)
  (let loop ([lst expr] [i 0])
    (cond
      [(null? lst) #f]
      [(member (car lst) ops) i]
      [else (loop (cdr lst) (+ i 1))]
    )
  )
)


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
; (sam-eval '(quote hello) _enviroment)
; (sam-eval '(quote (1 2 3)) _enviroment)

; (sam-eval '(if (= 2 2) 100 200) _enviroment)
; (sam-eval '(if (= 3 4) 7 8) _enviroment)

; (sam-eval '(+ 10 5) _enviroment)
; (sam-eval '(* 3 7) _enviroment)

; (sam-eval '((lambda (x) (+ x 2)) 4) _enviroment)
; (sam-eval '((lambda (y) (* y y)) 6) _enviroment)

;transform test cases

(transform1 '(3 + 4))          ; => '(+ 3 4)
(transform1 '(1 + 2 * 3))       ; => '(* (+ 1 2) 3)
(transform2 '(1 + 2 * 3))       ; => '(+ 1 (* 2 3) )
(transform1 '(10 - 5 + 2))     ; => '(+ (- 10 5) 2)

 ;lambda test cases

;(sam-eval '(lambda (x) (+ x 1)) _enviroment)
;; => #<procedure>

;(sam-eval '((lambda (x) (+ x 1)) 5) _enviroment)
;; => 6

;(sam-eval '((lambda (y) (* y y)) 4) _enviroment)
;; => 16











