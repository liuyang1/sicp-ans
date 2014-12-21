(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(displayln (new-if (= 2 3) 0 5))
(displayln (new-if (= 1 1) 0 5))
