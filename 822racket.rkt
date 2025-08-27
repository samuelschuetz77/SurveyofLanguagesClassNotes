#lang racket

(define (square x) (* x x))

(define (mymap f lis) 
    (if (null? lis)
        null
        (cons (f (car lis))
            (mymap f (cdr lis))
)))

(mymap square '(2 2 2 4 38))
