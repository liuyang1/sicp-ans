#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence) initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (my-fold-left op init seq)
  (define (iter result rest)
    (if (null? rest) result
      (iter (op result (car rest)) (cdr rest))))
  (iter init seq))

(define my-fold-right accumulate)

(my-fold-right / 1 (list 1 2 3))
;;; (/ 1 (/ 2 (/ 3 1)))
(my-fold-left / 1 (list 1 2 3))
;;; (/ (/ (/ 1 1) 2) 3)
(my-fold-right list '() (list 1 2 3))
;;; (list (1 (list 2 (list 3 '()))))
(my-fold-left list '() (list 1 2 3))
;;; (list (list (list '() 1) 2) 3)
