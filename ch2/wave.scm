#lang racket
(require (planet "sicp.ss"("soegaard""sicp.plt" 2 1)))
; (define wave einstein)
; (wave 2)
(define nil '())
(define (connect vect-list)
  (define (iter segment-list remaining)
    (if (null? (cdr remaining))
        (reverse segment-list)
        (iter (cons (make-segment (car remaining)
                                  (cadr remaining))
                    segment-list)
              (cdr remaining))))
  (iter nil vect-list))
(define (wave frame)
  ((segments->painter (append (connect (list (make-vect 0.4  0.0)
                                             (make-vect 0.5  0.33)
                                             (make-vect 0.6  0.0))) ;inside legs
                              (connect (list (make-vect 0.25 0.0)
                                             (make-vect 0.33 0.5)
                                             (make-vect 0.3  0.6)
                                             (make-vect 0.1  0.4)
                                             (make-vect 0.0  0.6))) ;lower left
                              (connect (list (make-vect 0.0  0.8)
                                             (make-vect 0.1  0.6)
                                             (make-vect 0.33 0.65)
                                             (make-vect 0.4  0.65)
                                             (make-vect 0.35 0.8)
                                             (make-vect 0.4  1.0))) ;upper left
                              (connect (list (make-vect 0.75 0.0)
                                             (make-vect 0.6  0.45)
                                             (make-vect 1.0  0.15)));lower right
                              (connect (list (make-vect 1.0  0.35)
                                             (make-vect 0.8  0.65)
                                             (make-vect 0.6  0.65)
                                             (make-vect 0.65 0.8)
                                             (make-vect 0.6  1.0)))));upper right
   frame))

(paint wave)
; (paint (beside wave (flip-vert wave)))
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
;(paint (right-split wave 4))
; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
(paint (up-split wave 4))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;(paint (corner-split wave 4))

; 2.45
(define (split x y)
  (define (fn painter n)
    (if (= n 0)
        painter
        (let ((smaller (fn painter (- n 1))))
          (x painter (y smaller smaller)))))
  fn)
(define up-split-1 (split below beside))
(paint (up-split-1 wave 3))
(paint ((split beside below) wave 3))