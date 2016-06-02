#lang racket

(require "42.scm")

(define (queens-ex board-size)
  (define empty-board '())
  (define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter safe?
              (flatmap (lambda (new-row)
                         (map (lambda (rest-of-queens)
                                (adjoin-position new-row k rest-of-queens))
                              (queen-cols (- k 1))))
                       (range 1 board-size)))))
  (queen-cols board-size))

(define (queens-ex-1 board-size)
  (define empty-board '())
  (define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter safe?
              ; Change to this style, it need almost same time
              (let ((next (queen-cols (- k 1))))
                (flatmap (lambda (new-row)
                           (map (lambda (rest-of-queens)
                                  (adjoin-position new-row k rest-of-queens))
                                next))
                         (range 1 board-size))))))
  (queen-cols board-size))

; (map displayln (queens-ex 8))
(map displayln (queens-ex-1 8))
