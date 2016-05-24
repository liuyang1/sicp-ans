(define (inc a) (+ a 1))
(define (dec a) (- a 1))

(define (plus a b)
  (if (= a 0) b (inc (plus (dec a) b))))
; recursion style
; function need keep INC function to wait next PLUS finish.

(display (plus 3 4))
(newline)

(define (plus1 a b)
  (if (= a 0) b (plus1 (dec a) (inc b))))
; iterate style
; when iterate to next PLUS1, all status pass to next PLUS1, don't need current
; PLUS1's environment again.

(display (plus1 3 4))
(newline)
