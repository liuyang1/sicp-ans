#lang racket
(define (cube x) (* x x x))

(define (iter x) (- (* 3 x) (* 4 (cube x))))

; actually tresh need decrease to 0.0001 to get 10e-6 accuracy
(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (iter (sine (/ angle 3)))))

(define (sine-t angle)
  (define (helper x cnt)
    (if (not (> (abs x) 0.1))
      (begin (displayln (format "(sine ~A) iter ~A times" angle cnt))
             x)
      (iter (helper (/ x 3) (+ 1 cnt)))))
  (helper angle 0))

(sine-t 12.15)
(sine-t (+ 12.15 (* 100 pi)))
; 12.15 / 3 = 4.05
; 4.05 / 3 = 1.35
; 1.35 / 3 = 0.45
; 0.45 / 3 = 0.15
; 0.15 / 3 = 0.05 STOP
; FIVE times.

; log3(x)
