#lang racket
(define (last-pair lst)
  (define (last-pair-ip lst)
    (let ((newlst (cdr lst)))
     (if (null? newlst)
       lst
       (last-pair-ip newlst))))
  (if (null? lst)
    '()
    (last-pair-ip lst)))

(last-pair '())
(last-pair (list 0))
(last-pair (list 1 0))
(last-pair (list 2 1 0))
(last-pair (list 23 72 149 34))
(last-pair (list 1 (cons 2 3)))
