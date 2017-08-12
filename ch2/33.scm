#lang racket
;;; This exercise show the relationship between
;;;     fold/accmulate with generic function on list
(define (accumulate op unit xs)
  (if (null? xs)
    unit
    (op (car xs)
        (accumulate op unit (cdr xs)))))

(define (mymap f xs)
  (accumulate (lambda (head tail) (cons (f head) tail))
              '()
              xs))

(define (myappend seq0 seq1)
  (accumulate cons seq1 seq0))

(define (mylength seq)
  (accumulate (lambda (head tail) (+ 1 tail))
              0
              seq))

(define (square x) (* x x))

(define lst '(1 2 3))
(define lst1 '(4 5 6))
lst
(mymap square lst)
(myappend lst lst1)
(mylength lst)
(mylength (myappend lst lst))
