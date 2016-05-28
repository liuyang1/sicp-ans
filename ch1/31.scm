#lang racket

; on POSITIVE-INTEGER sequence
(define (inc x) (+ 1 x))

; abstract to group set
; group is on OP and UNIT
; 1. (X op Y) op Z = X op (Y op Z)
; 2. exist UNIT, X op UNIT = X
;       for MUL(*), UNIT is 1; for ADD(+), UNIT is 0.
; 3. exist INV element. X op (INV X) = UNIT
;       for MUL(*), INV is DIV; for ADD(+), INV is NEGATIVE.
(define (gen-group-iter op unit)
  (define (iter-func func a next b)
    (define (iter a acc)
      (if (> a b)
        acc
        (iter (next a) (op acc (func a)))))
    (iter a unit))
  iter-func)

(define product-iter (gen-group-iter * 1))

(define (product-rec func a next b)
  (if (> a b)
    1
    (* (func a)
       (product-rec func (next a) next b))))

(define (func-pi n)
  (if (even? n)
    (/ n (+ n 1.0))
    (/ (+ n 1.0) n)))

(define (call-pi-fn pi-fn end)
  (* 4 (pi-fn func-pi 2 inc end)))

(call-pi-fn product-rec 100000)
(call-pi-fn product-iter 100000)
