#lang racket

(provide (all-defined-out))
(define make-point cons)
(define x-point car)
(define y-point cdr)
(define (sub-point A B)
  (make-point (- (x-point A) (x-point B))
              (- (y-point A) (y-point B))))

(define (show-point p)
  (format "(~a, ~a)" (x-point p) (y-point p)))
(define (print-point p)
  (displayln (show-point p)))

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
(define (show-segment seg)
  (format "(~a, ~a)"
          (show-point (start-segment seg))
          (show-point (end-segment seg))))
(define (print-segment seg)
  (displayln (show-segment seg)))

(define (midpoint-segment seg)
  (define (avg a b) (/ (+ a b) 2))
  (let ((A (start-segment seg))
        (B (end-segment seg)))
    (make-point (avg (x-point A) (x-point B))
                (avg (y-point A) (y-point B)))))

(define (test-case)
  (define *seg* (make-segment (make-point 1 2)
                              (make-point 2 3)))
  (print-segment *seg*)
  (print-point (midpoint-segment *seg*)))

(test-case)
