#lang racket
(define (subsets xs)
  (displayln xs)
  (if (null? xs)
    '(())
    (let ((x (car xs))
          (rest (subsets (cdr xs))))
     (append (map (lambda (a) (cons x a))
                  rest)
             rest))))

(define a '(1 2 3))

a
(subsets a)
