; impl make-accumulator
; just as make-draw as example
; using REG as internal state
; and using set! to change state
(define (make-accumulator init)
  (let ((reg init))
   (lambda (incval)
     (begin (set! reg (+ reg incval))
            reg))))

(define A (make-accumulator 5))

(display (A 10))
(newline)
(display (A 10))
(newline)
