(define (max2 a b)
  (if (> a b) a b))

(define (max32 a b c)
  (max2 (+ a b) (max2 (+ b c) (+ c a))))

(displayln (max32 1 2 3))

(define (square x) (* x x))

(define (max32square a b c)
  (max32 (square a)
         (square b)
         (square c)))

(display (max32square 1 2 3))
(newline)
