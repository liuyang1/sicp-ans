#lang racket
(define (accumulate op init sequence)
  (if (null? sequence) init
    (op (car sequence)
        (accumulate op init (cdr sequence)))))

(define (my-fold-left op init seq)
  (define (iter result rest)
    (if (null? rest) result
      (iter (op result (car rest)) (cdr rest))))
  (iter init seq))

(define my-fold-right accumulate)

(define (reverse0 seq)
  (my-fold-right (lambda (x y) (append y (list x))) '() seq))

(define (reverse1 seq)
  (my-fold-left (lambda (x y) (cons y x)) '() seq))

(define *test-obj* (list 1 2 3))
(reverse1 *test-obj*)
(reverse0 *test-obj*)
