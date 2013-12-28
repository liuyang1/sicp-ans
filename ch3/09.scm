; (f #0 n=6)
; (f #0 n=6) (f #1 n=5)
; ...
; (f #0 n=6) (f #1 n=5) ... 4 3 2 1
; (f #0 n=6) (f #1 n=5) 24*5
; (f #0 n=6) 120 * 6
; 720
(define (factorial-rec n)
  (if (= n 1) 1
    (* n (factorial-rec (- n 1)))))

; factorial-iter 6
; factorial-iter 6 fact-iter 1 1 6
; factorial-iter 6 fact-iter 1 2 6
; factorial-iter 6 fact-iter 2 3 6
; fact-iter 6 4 6
; ...
; fact-iter 720 7 6
; 720
(define (factorial-iter n)
  (define (fact-iter prod counter max-count)
    (if (> counter max-count) prod
      (fact-iter (* counter prod) (+ counter 1) max-count)))
  (fact-iter 1 1 n))
