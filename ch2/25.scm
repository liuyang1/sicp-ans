#lang racket
(define lst0 '(1 3 (5 7) 9))
(define lst1 '((7)))
(define lst2 '(1 (2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (cdr lst0)))))
(cadr (caddr lst0))
(car (car lst1))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr lst2))))))))))))
(cadr (cadr (cadr (cadr (cadr (cadr lst2))))))
