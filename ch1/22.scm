(define (square a) (* a a))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)    n)
        ((divides? test-divisor n)      test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (test-smallest-diviso n)
  (begin (display (smallest-divisor n)) (newline)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime n start-time (current-inexact-milliseconds))
    #f)) 

(define (report-prime n start-time end-time)
  (display n)
  (display "\t")
  (display (- end-time start-time))
  (newline))


(define (search-for-primes begin-value num)
  (cond ((= num 0) #t)
        ((not (timed-prime-test begin-value)) (search-for-primes (+ begin-value 1) num))
        (else (search-for-primes (+ begin-value 1) (- num 1)))))

(search-for-primes 1000 3)
(search-for-primes (* 1000 10) 3)
(search-for-primes (* 1000 100) 3)
(search-for-primes (* 1000 1000) 3)
