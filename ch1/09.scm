(define (inc a) (+ a 1))
(define (dec a) (- a 1))

(define (plus a b)
  (if (= a 0) b (inc (plus (dec a) b))))

(display (plus 3 4))
(newline)

(define (plus1 a b)
  (if (= a 0) b (plus1 (dec a) (inc b))))

(display (plus1 3 4))
(newline)
