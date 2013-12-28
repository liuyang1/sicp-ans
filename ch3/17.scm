(define (displayln x) (display x) (newline) x)

(define (count-pairs x)
  (define (in? x lst)
    (cond ((null? lst) #f)
          ((eq? x (car lst)) #t)
          (else (in? x (cdr lst)))))
  (define (add node queue)
    (if (or (not (pair? node)) (in? node queue)) queue
      (append queue (list node))))
  (define (helper node queue)
    (if (not (pair? node)) (length queue)
      (let ((nq (add (car node) (add (cdr node) (add node queue)))))
       (max (helper (car node) nq) (helper (cdr node) nq)))))
  (helper x '()))


; 3
(define *3* '(a b c))
(displayln *3*)
(displayln (count-pairs *3*))
;
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
