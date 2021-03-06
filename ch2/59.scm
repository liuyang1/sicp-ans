#lang racket

; set: based on unsorted list

; element-of-set? :: a -> [a] -> Bool
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; union-set :: [a] -> [a] -> [a]
(define (union-set set0 set1)
  (cond ((null? set0) set1)
        ((element-of-set? (car set0) set1) (union-set (cdr set0) set1))
        (else (cons (car set0) (union-set (cdr set0) set1)))))

(displayln (union-set '(1 3 5) '(2 4 6 3)))
