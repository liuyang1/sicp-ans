; this is so stupid ...
(define (find-min lst)
  (let ((mini (filter (lambda (x) (< x (car lst))) (cdr lst))))
   (if (= 0 (length mini)) (car lst)
     (find-min mini))))

(define *lst* '(1 5 2 4 8 -1 6 3))
(display (find-min *lst*))
(newline)

(define (find-min-1 lst)
  (foldl (lambda (x y) (if (< x y) x y)) (car lst) (cdr lst)))

(display (find-min-1 *lst*))
(newline)
