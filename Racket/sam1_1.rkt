#lang racket 

(define (sum-of-squares a b)
  (define (square x)
     (* x x))
  (+ (square a)
     (square b))
)

(sum-of-squares 3 4) ; expect 25
(sum-of-squares -2 5) ; expect 29

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average-of-squares x y z)
 
  (define(square x) 
    (* x x))
    (define (total x y z) (+ (square x) (square y) (square z)))
    (/ (total x y z) 3)
 
)

(average-of-squares 2 2 2)   ; expect 4
(average-of-squares 3 4 5)   ; expect (9+16+25)/3 = 50/3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(car '("hi" "there"))             ; → "hi"
(cdr '("hi" "there"))             ; → '("there")

(define (prepend-tag str lst)
  (cons str lst)
)

(prepend-tag "TODO" '())                      ; expect '("TODO")
(prepend-tag "TODO" '("fix" "tests"))         ; expect '("TODO" "fix" "tests")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; recursion

(define (str-lengths strList)
    (if(null? strList)
      '()
      (cons (string-length(car strList))
            (str-lengths (cdr strList))))
)


;; Use string-append and string-upcase.

(string-append "swag" "poo")

(define (shout-all lst)
 (if (null? lst)
  '()
   (cons (string-upcase(car lst))
        (shout-all (cdr lst))))
)

(shout-all '("hello" "world")) ; expect '("HELLO!" "WORLD!")
(shout-all '())                ; expect '()


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



