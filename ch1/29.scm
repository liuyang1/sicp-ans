#lang racket
(define (cube x) (* x x x))
(define (pow4 x) (* x x x x))

; default option [n 100], n default to 100
(define (simph f a b [n 100])
  (define (sum f a b h)
    (if (= a b)
      (f b)
      (+ (* 2 (f a))
         (* 4 (f (+ a h)))
         (sum f (+ a (* h 2)) b h))))
  (define (integral f a b h)
    (* (/ h 3) (- (sum f a b h) (f a))))
  (integral f a b (/ (- b a) n)))

(define (myinterg f a b [n 100])
  (define (sum term a next b)
    (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
  (define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2)) add-dx b)
       dx))
  (integral f a b (/ (- b a) n)))

(myinterg cube 0 1)
; simph algo have 3rd order accuracy, so it could get RIGHT ans for CUBE func
(simph cube 0 1)
(myinterg cube 0 1 1000)
(simph cube 0 1 1000)

; but for 4th order function, it fail
(myinterg pow4 0 1)
(simph pow4 0 1)
