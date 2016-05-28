#lang racket

(provide fixed fixed-show)

(define tolerance 0.00001)
(define (close? v1 v2) (< (abs (/ (- v1 v2) (+ v1 v2) 2))
                          tolerance))

(define (fixed f first)
  (define (try guess)
    (let ((next (f guess)))
     (if (close? guess next) next
       (try next))))
  (try first))

(define (fixed-show f first)
  (define (try guess)
    (begin (displayln guess)
           (let ((next (f guess)))
            (if (close? guess next)
              next
              (try next)))))
  (try first))
