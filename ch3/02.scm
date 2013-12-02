; also using reg as internal state

; !! this example is kind of DECORATOR
; and we support a HOW-MANY-CALLS? method
(define (make-monitored proc)
  (let ((reg 0))
   (lambda (x)
     (if (eq? x 'how-many-call?)
       reg
       (begin (set! reg (+ reg 1))
              (proc x))))))

; test code
(define s (make-monitored sqrt))

(display (s 100))
(newline)
(display (s 'how-many-call?))
(newline)
(display (s 100))
(newline)
(display (s 'how-many-call?))
(newline)
