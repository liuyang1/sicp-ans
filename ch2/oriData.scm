(define *env* '())
(define (build-proc-tag op type)
  (cons op type))
(define (put op type item)
    (set! *env* (cons (cons (build-proc-tag op type) item) *env*)))
(define (get op type)
  (cdr (assoc (build-proc-tag op type) *env*)))

(define (displayln x) (display x) (newline) x)
