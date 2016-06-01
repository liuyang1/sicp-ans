#lang racket
(define (fringe x)
  (cond ((not (list? x)) (list x))
        ((null? x) x)
        (else (append (fringe (car x)) (fringe (cdr x))))))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))
