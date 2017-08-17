#lang racket
(define (my-for-each func lst)
  (if (null? lst)
    #t
    (begin (func (car lst))
           (my-for-each func (cdr lst)))))

(my-for-each (lambda (x) (displayln x))
             (list 1 2 3))
