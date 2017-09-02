#lang racket

(require "63.scm")
(require "64.scm")

; TODO:
(define (union set0 set1)
  (define (make-leaf entry) (make-tree entry '() '()))
  (cond ((null? set0) set1)
        ((null? set1) set0)
        (else
          (let ((entry0 (entry set0))
                (entry1 (entry set1))
                (left0 (left-branch set0))
                (left1 (left-branch set1))
                (right0 (right-branch set0))
                (right1 (right-branch set1)))
            (cond ((= entry0 entry1)
                   (make-tree entry0
                              (union left0 left1)
                              (union right0 right1)))
                  ((< entry0 entry1)
                   (make-tree entry0
                              (union left0 left1)
                              (union (union right0 (make-leaf entry1))
                                     right1)))
                  (else (union set1 set0)))))))


(define *obj0* (list->tree '(1 3 5 7 9 11)))
(disp *obj0*)
(define *obj1* (list->tree '(1 4 7 9 13)))
(disp *obj1*)
(disp (union *obj0* *obj1*))

(disp (union '(7 () ()) '(9 (7 () ()) (11 () ()))))
