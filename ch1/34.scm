#lang racket

(define (f g) (g 2))

(f f)
; It will calc with (2 2), so fail as 2 is not a procedure
