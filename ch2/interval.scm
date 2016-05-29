#lang racket

(provide (all-defined-out))
(define (make-interval a b)
  (cons (min a b)
        (max a b)))
(define lower-bound car)
(define upper-bound cdr)

(define (eq-interval? x y)
  (and (= (lower-bound x) (lower-bound y))
       (= (upper-bound x) (upper-bound y))))

(define (neg-interval x)
  (make-interval (- (upper-bound x))
                 (- (lower-bound x))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval-trival x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (postive? x) (>= (lower-bound x) 0))
(define (negtive? x) (<= (upper-bound x) 0))
(define (zeroint? x) (< (* (lower-bound x) (upper-bound x)) 0))

(define (mul-interval x y)
  (cond ((postive? x)
         (cond ((postive? y)
                (make-interval (* (lower-bound x) (lower-bound y))
                               (* (upper-bound x) (upper-bound y))))
               ((zeroint? y)
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (upper-bound x) (upper-bound y))))
               ((negtive? y)
                (neg-interval (mul-interval x (neg-interval y))))))
        ((zeroint? x)
         (cond ((postive? y)
                (mul-interval y x))
               ((zeroint? y)
                (mul-interval-zero x y))
               ((negtive? y)
                (neg-interval (mul-interval x (neg-interval y))))))
        (else (neg-interval (mul-interval (neg-interval x) y)))))

; This is trival, and I think it's hard possible, but too many condition.
(define (mul-interval-zero x y)
  (mul-interval-trival x y))

(define (div-interval-zero x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (div-interval x y)
  (if (< (* (lower-bound y) (upper-bound y)) 0)
    (begin (displayln "cannot divide zero") #f)
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

(define (width-interval x)
  (- (upper-bound x) (lower-bound x)))
