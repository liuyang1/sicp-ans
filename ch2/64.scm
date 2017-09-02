#lang racket

(require "63.scm")
(provide (all-defined-out))
(define (list->tree lst)
  (car (partial-tree lst (length lst))))

; partial-tree :: [a] -> n -> (tree, [a])
; retrive first N elements of [a] to build a Tree, and return (Tree, remain-elts)
(define *counter* 0)
(define (partial-tree elts n)
  (set! *counter* (+ *counter* 1))
  (if (= n 0)
    (cons '() elts)
    (let* ((left-size (quotient (- n 1) 2))
           (right-size (- n left-size 1))
           (left-result (partial-tree elts left-size))
           (left-tree (car left-result))
           (non-left-elts (cdr left-result))
           (this-entry (car non-left-elts))
           (right-result (partial-tree (cdr non-left-elts) right-size))
           (right-tree (car right-result))
           (remaining-elts (cdr right-result)))
      (cons (make-tree this-entry left-tree right-tree)
            remaining-elts))))

(define *result* (list->tree '(1 3 5 7 9 11)))

; answer:
; a) check below result:
; (disp *result*)

; b)
; O(n)
; (define (reset-counter) (set! *counter* 0))
; (reset-counter)
; (define (test-complexity n)
;   (reset-counter)
;   (list->tree (range n))
;   (display (format "N=~a counter=~a counter/N=~a~n"
;                    n *counter* (/ *counter* n))))
; (test-complexity 10)
; (test-complexity 100)
