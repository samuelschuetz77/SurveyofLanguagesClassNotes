
#lang racket

;; car, cdr, cons
(define (cons a b)
  (define (dispatch msg)
    (if (eq? msg 'car)
        a
        (if (eq? msg 'cdr)
            b
            (error "unknown selector" msg))))
  dispatch)


(define (car pair) (pair 'car))
(define (cdr pair) (pair 'cdr))

;;test car,cdr,cons
(define p (cons 3 '(4 5 6 7)))
(car p) ; => 3
(cdr p) ; => 4


; ;; filter funtion TO USE COMMENT OUT EVERYTHING ABOVE
; (define (myfilter pred lst)
;   (cond
;     [(empty? lst) '()]                        
;     [(pred (first lst))                      
;      (cons (first lst) (myfilter pred (rest lst)))]
;     [else
;      (myfilter pred (rest lst))]))  
     
; (myfilter even? '(1 2 3 4 5 6 7 8))

;; Reduce function COMMENT OUT EVERYTHING ABOVE TO USE
; (define (reduce op values)
;   (if (null? (cdr values))
;       (car values)
;       (reduce op (cons (op (car values) (cadr values)) (cddr values)))))

; (reduce + '(3 2 3 6))

; Map function dependencies
(define (square x) (* x x))
(define (add1 x) (+ x 1))

;Map function

(define (mymap f lst)
    (if (null? lst)
        null
        (cons (f (car lst))
              (mymap f (cdr lst)))))
 
(displayln (mymap square '(2 2 2 4 38)))
(displayln (mymap add1 '(2 2 2 4 38)))
