; self define a rand-update
; just pick prime as a b m
(define a 97)
(define b 7)
(define m 101)

; x' = (ax + b) % m
(define (rand-update x)
  (remainder (+ (* a x) b) m))

; make a random-proc
(define (make-rand)
  (let ((seed 0))
   (define (generate)
     (begin (set! seed (rand-update seed))
            seed))
   (define (reset new-seed)
     (set! seed new-seed))
   (define (dispatch m)
     (cond ((eq? m 'generate) (generate)); directly call generate proc
           ((eq? m 'reset) reset)))
   dispatch))

; test code
(define rand (make-rand))
(display (rand 'generate))
(newline)
(display (rand 'generate))
(newline)
; reset seed to zero
((rand 'reset) 0)
(display (rand 'generate))
(newline)
(display (rand 'generate))
(newline)
