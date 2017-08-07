#lang racket
(define (max2 a b)
  (if (> a b) a b))

(define (max32 a b c)
  (max2 (+ a b) (max2 (+ b c) (+ c a))))

(displayln (max32 1 2 3))

;;; antoher solution for max32
(define (min2 a b)
  (if (> a b) b a))

(define (fold1 f lst)
  (if (null? (cdr lst)) (car lst)
    (f (car lst)
       (fold1 f (cdr lst)))))

(define (min_ lst) (fold1 min2 lst))

(define (sum lst) (fold1 + lst))

(define (max32_ a b c)
  (- (sum (list a b c))
     (min_ (list a b c))))

(displayln (max32_ 1 2 3))

(define (square x) (* x x))

(define (max32square a b c)
  (max32 (square a)
         (square b)
         (square c)))

(display (max32square 1 2 3))
(newline)
