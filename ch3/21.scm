(load "queue.scm")

(define (print-queue queue)
  (displayln (front-ptr queue)))

; test code
(define q1 (make-queue))
(displayln q1)
(displayln (insert-queue! q1 'a))
(displayln (insert-queue! q1 'b))
(displayln (delete-queue! q1))
(displayln (delete-queue! q1))
(newline)

(define q2 (make-queue))
(print-queue q2)
(print-queue (insert-queue! q2 'a))
(print-queue (insert-queue! q2 'b))
(print-queue (delete-queue! q2))
(print-queue (delete-queue! q2))
