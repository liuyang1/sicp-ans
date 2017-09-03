#lang racket

(require "63.scm")
(require "64.scm")

(define tree->list tree->list-2)

(define (merge xs ys)
  (cond ((null? xs) ys)
        ((null? ys) xs)
        (else
          (let ((x (car xs))
                (y (car ys))
                (xt (cdr xs))
                (yt (cdr ys)))
            (cond ((= x y) (cons x (merge xt yt)))
                  ((< x y) (cons x (merge xt ys)))
                  (else (cons y (merge xs yt))))))))

; (define (union set0 set1)
;   (list->tree (merge (tree->list set0)
;                      (tree->list set1))))

(define (on f g)
  (lambda (x y) (f (g x) (g y))))

(define (.: f g)
  (lambda (x y) (f (g x y))))

;; Haskell style :)
;; list->tree .: (merge `on` tree->list)
(define union
  (.: list->tree (on merge tree->list)))


(define *obj0* (list->tree '(1 3 5 7 9 11)))
(disp *obj0*)
(define *obj1* (list->tree '(1 4 7 9 13)))
(disp *obj1*)
(disp (union *obj0* *obj1*))

(disp (union '(7 () ()) '(9 (7 () ()) (11 () ()))))
