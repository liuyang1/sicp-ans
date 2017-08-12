#lang racket

; x(1 + a) * y (1 + b) = xy(1 + a)(1 + b) = xy(1 + a + b + ab)
; x(1 - a) * y (1 - b) = xy(1 - a)(1 - b) = xy(1 - a - b + ab)

; \phi = (a + b) / (1 + ab)
