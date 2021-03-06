#lang racket
(define (make-mobile left right) (cons left right))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (make-branch len struct) (cons len struct))
(define (branch-len branch) (car branch))
(define (branch-struct branch) (cdr branch))

(define (mobile? x) (pair? x))

(define (total-weight mobile)
  (if (mobile? mobile)
    (+ (total-weight (branch-struct (left-branch mobile)))
       (total-weight (branch-struct (right-branch mobile))))
    mobile))

(define (balance? mobile)
  (if (mobile? mobile)
    (let ((left-struct (branch-struct (left-branch mobile)))
          (right-struct (branch-struct (right-branch mobile)))
          (left-len (branch-len (left-branch mobile)))
          (right-len (branch-len (right-branch mobile))))
      (if (or (not (balance? left-struct)) (not (balance? right-struct)))
        #f
        (= (* (total-weight left-struct) right-len)
           (* (total-weight right-struct) left-len))))
    #t))

(define mo1 (make-mobile (make-branch 3 (make-mobile (make-branch 4 5)
                                                     (make-branch 8 10)))
                         (make-branch 1 5)))
(displayln mo1)
(displayln (total-weight mo1))
(define sm1 (make-mobile (make-branch 1 2) (make-branch 1 2)))
(displayln (balance? sm1))
(define sm2 (make-mobile (make-branch 3 1) (make-branch 1 2)))
(displayln (balance? sm2))
(displayln (balance? mo1))
