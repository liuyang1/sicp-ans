(define (square x) (* x x))
(define (cubic x) (* x x x))

; how to find cube-root with formual
(define (cube-root x)
  (define (improve y x)
    (/ (+ (/ x (square y)) y y) 3))
  (define (good-enough? guess)
    (< (abs (- 1 (/ (cubic guess) x))) 0.001))
  (define (cube-root-iter guess)
    (if (good-enough? guess)
      guess
      (cube-root-iter (improve guess x))))
  (cube-root-iter 1.0))

(displayln (cube-root 10))
(displayln (cube-root 100))
(displayln (cube-root 1000))
