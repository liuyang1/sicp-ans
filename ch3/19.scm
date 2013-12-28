(define (displayln x) (display x) (newline) x)

; fast and slow pointer algo
(define (cycle? p)
  (define (compare p0 p1)
    (if (or (null? p0) (null? p1) (null? (cdr p1))) #f
      (if (eq? p0 p1) #t
      (compare (cdr p0) (cdr (cdr p1))))))
  (compare p (cdr p)))

; test code
(displayln (cycle? '(a)))
(displayln (cycle? '(a b)))
(displayln (cycle? '(a b c)))

(define *c0* '(a))
(set-cdr! *c0* *c0*)
(displayln (cycle? *c0*))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define (last-pair x)
  (if (null? (cdr x)) x
    (last-pair (cdr x))))
(define *c1* '(a b c))
(make-cycle *c1*)
(displayln (cycle? *c1*))
