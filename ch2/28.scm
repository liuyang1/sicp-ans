#lang racket
(define (fringe x)
  (cond ((not (list? x)) (list x))
        ((null? x) x)
        (else (append (fringe (car x)) (fringe (cdr x))))))

(define *x* (list (list 1 2) (list 3 4)))
*x*
(fringe *x*)
(fringe (list *x* *x*))
(fringe (list *x* *x* *x*))

;;; It's kind of flatten function
