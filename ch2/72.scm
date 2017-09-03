#lang racket
; using WEIGHT as weight, tree height as value.

; for example
; tree ((A 4) (B 2) (C 1) (D 1))
; if check left or right branch only once.(mean DONOT check huffmann is wrong condition
; for this condition, checking complexity is equal to length of encoded answer.
; so this is MINimum
(define *avg-height*
(/ (+ (* 4 2); A weight and height
      (* 2 3); B ...
      (* 1 4)
      (* 1 4))
   8))
(displayln *avg-height*)

; check every node if encoded symbol is in the symbol-list or not
(define *avg-height-1*
  (/ (+ (* 4 2)
        (* 2 4)
        (* 1 6)
        (* 1 7))
     8))
(displayln *avg-height-1*)

; for 2.71 condition

; for most frequently symbol, check step is constant 2.
; for least frequently symbol, check step is (n-1) steps
; or (2n-1) steps
