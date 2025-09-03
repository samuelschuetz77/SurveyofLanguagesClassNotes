#lang racket
;support quote, if, lambda0, lambda1, call function 0 args, call function 1 arg
(define (is-quoted code)
    (eq? (car code) (quote quote)))

(define (eval code)
    (cond
    [(eq? (car code) 'quote) (cadr code)]
    [(eq? (car code) '+) (apply + (map)) ()]
    )
)

(eval '(quote (hello this (is cool) code))) ;; test if quote works