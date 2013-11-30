(define (max2 a b)
  (if (> a b) a b))

(define (max32 a b c)
  (max2 (+ a b) (max2 (+ b c) (+ c a))))

(display (max32 1 2 3))
(newline)
