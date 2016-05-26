(define (Ackermann x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else    (Ackermann (- x 1)
                            (Ackermann x (- y 1))))))

(define (displayln x) (display x) (newline))

(displayln (Ackermann 1 10))
(displayln (Ackermann 2 4))
(displayln (Ackermann 3 3))

(define (range start stop step)
  (if (> start stop) '()
    (cons start (range (+ start step) stop step))))
(define *seq* (range 0 5 1))
(displayln *seq*)

(define (f n) (Ackermann 0 n))
(define (g n) (Ackermann 1 n))
(define (h n) (Ackermann 2 n))
(define (k n) (* 5 n n))

; 2n
(displayln (map f *seq*))
; 2^n
(displayln (map g *seq*))
; 2^(2^(...^2))
; ......... count n
(displayln (map h *seq*))
; 5n^2
(displayln (map k *seq*))
