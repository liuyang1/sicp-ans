#lang racket

; set: based on sorted list
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((element-of-set? x set) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))))

(displayln (adjoin-set 3 '(1 5)))
(displayln (adjoin-set 3 '(1 3 5)))
(displayln (adjoin-set 3 '()))
