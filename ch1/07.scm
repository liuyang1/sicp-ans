(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2.0))

(define (good-enough?-bad guess x)
  (< (abs (- (square guess) x)) 0.001))
; good solution
; This version work well, no master abs of x is too large or too tiny
(define (good-enough?-good guess x)
  (< (abs (- 1 (/ (square guess) x))) 0.001))

(define (sqrt x good-enough?)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(displayln (sqrt 1000 good-enough?-bad))
(displayln (sqrt 0.001 good-enough?-bad))
(displayln (sqrt 1000 good-enough?-good))
(displayln (sqrt 0.001 good-enough?-good))
