(define (move from to)
  (display "move ") (display from) (display " to ") (display to) (newline))
(define (hanoi n from to by)
  (cond ((= n 1) (move from to))
        (else (hanoi (- n 1) from by to)
              (move from to)
              (hanoi (- n 1) by to from))))

(hanoi 5 'a 'b 'c)
