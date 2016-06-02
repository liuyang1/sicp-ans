#lang racket
(define (square-tree-direct x)
  (cond ((not (list? x)) (square x))
        ((null? x) x)
        (else (cons (square-tree-direct (car x))
                      (square-tree-direct (cdr x))))))

(define (square-tree x)
  (cond ((not (list? x)) (square x))
        (else (map square-tree x))))

(define (square x) (* x x))
(define lst (list 1 (list 2 (list 3 4) 5)))
(displayln lst)
(displayln (square-tree-direct lst))
(displayln (square-tree lst))
