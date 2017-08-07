; prove Fib(n) = (phi ^ n - gammar ^ n) / sqrt(5)

; phi - gamma = 1
; phi * gamma = 1

; Fib(0) = ((1 + sqrt(5) / 2) - (1 - sqrt(5) / 2)) / sqrt(5)
;        = 1
; Fib(1) = ((1 + sqrt(5) / 2) ^ 2 - (1 - sqrt(5) / 2) ^ 5) / sqrt(5)
;        = 4 * sqrt(5) / 4 / sqrt(5)
;        = 1
; phi*(Fib(n-1)+gammaFib(n-2)) = phi * Fib(n-1) + Fib(n - 2)
;                              = Fib(n-1) + Fib(n-2) + (phi - 1)Fib(n-1)
;                              = Fib(n) + gammaFib(n-1)
; ->
; Fib(n)+gammaFib(n-1) = (1+gamma)*phi^(n-2) = phi^(n-1)
; ->
; Fib(n) = phi^(n-1) - gamma(phi^(n-1) - gamma^(n-1))/sqrt(5)
;        = 1/sqrt(5)*{sqrt(5) * phi^(n-1) - gamma*phi^(n-1)+gamma^(n-1))}
;        = 1/sqrt(5)*{phi^n-gamma^n}
