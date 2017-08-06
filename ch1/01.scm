#lang racket
(displayln 10)
(displayln (+ 5 3 4))
(displayln (- 9 1))
(displayln (/ 6 2))
(displayln (+ (* 2 4) (- 4 6)))

; NO output
(define a 3)
(define b (+ a 1))

(displayln (+ a b (* a b)))
(displayln (= a b))
(displayln (if (and (> b a) (< b (* a b)))
             b
             a))

(displayln (cond ((= a 4) 6)
                 ((= b 4) (+ 6 7 a))
                 (else 25)))

(displayln (+ 2 (if (> b a) b a)))

(displayln (* (cond ((> a b) a)
                    ((< a b) b)
                    (else -1))
              (+ a 1)))
