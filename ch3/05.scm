; iter TRIALS times EXPERIMENT
; return PASSED times / all times
(define (monte-carlo trials experiment)
  (define (iter remain passed)
    (cond ((= remain 0) (/ passed  trials))
          ((experiment)
           (iter (- remain 1) (+ passed 1)))
          (else
            (iter (- remain 1) passed))))
  (iter trials 0))

; PI-Exprement code
; random return (0~1) random float number
; !! (random INTEGER) to return [0,INTEGER) integer number.
; !! so in text book, (random range) this is ERROR, because range is float number
; !! if range is INTEGER,
; !! this is also ERROR
; Area(x^2 + y^2 <= 1, 0<x<1, 0<y<1 ) = pi / 4
(define (pi-expr)
  (define (square x) (* x x))
  (let ((x (random))
        (y (random)))
    (if (<= (+ (square x) (square y)) 1) #t
      #f)))

; test code
(define *pi* (* 4.0 (monte-carlo (* 1000 1000) pi-expr)))
(display *pi*)
(newline)
