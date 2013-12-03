(define (func init)
  (let ((mem init))
   (lambda (x)
     (begin (set! mem (+ mem x))
            mem))))


; same proc (func -1/2)
(define f1 (func -1/2))
(define f2 (func -1/2))

; test code
(display (+ (f1 0) (f1 1)))       ; 0
(newline)
; swap call order
(display (+ (f2 1) (f2 0)))       ; 1
(newline)
