#lang racket

(provide fixed fixed-show)

(define tolerance 0.00001)
(define (close? v1 v2) (< (abs (/ (- v1 v2) (+ v1 v2) 2))
                          tolerance))

(define (fixed f initial)
  (define (try guess)
    (let ((next (f guess)))
     (if (close? guess next)
       next
       (try next))))
  (try initial))

(define (fixed-show f initial)
  (define (try guess)
    (begin (displayln guess)
           (let ((next (f guess)))
            (if (close? guess next)
              next
              (try next)))))
  (try initial))
