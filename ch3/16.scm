(define (count-pairs x)
  (if (not (pair? x)) 0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define (displayln x) (display x) (newline))

; 3
(define *3* '(a b c))
(displayln *3*)
(displayln (count-pairs *3*))

(define s2 (cons 'b 'c))
(define s4 (cons s2 s2))

; 4
(define *4* (cons 'a s4))
(displayln *4*)
(displayln (count-pairs *4*))

; 7
(define *7* (cons s4 s4))
(displayln *7*)
(displayln (count-pairs *7*))
