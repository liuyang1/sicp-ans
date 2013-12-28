(define (myreverse x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
       (set-cdr! x y)
       (loop temp x))))
  (loop x '()))

(define *v* '(a b c d))
(define (displayln x) (display x) (newline))
(displayln *v*)
(define *w* (myreverse *v*))
(displayln *v*)
(displayln *w*)
