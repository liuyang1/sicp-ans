;   这道题目是经典的囚徒困境问题.
;   就是设计策略,来如何在囚徒困境下尽可能取得较好的成绩.
;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;  play-loop过程,使用两个策略过程作为参数.
;;  迭代进行大概100回合.
;;  每个策略过程,以自己的历史选择以及对手的历史选择作为参数.
;;  策略过程,返回c表示合作,d表示拒绝合作
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;  print-out-results 输出结果
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	   (list score0 score1))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

(define (extract-entry game ass-list)
  (let ((entry (car ass-list)))
   (if (equal? game (car entry)) entry
     (extract-entry game (cdr ass-list)))))
;; note that you will need to write extract-entry

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

; Problem 7
(define (make-higher-order-spastic strat-list)
  (let ((strat-num (length strat-list)))
   (lambda (my-history other-history)
     ((list-ref strat-list (remainder (length my-history) strat-num))
      my-history
      other-history))))

; recuration version
(define (EGALITARIAN0  my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))
; iteration version and only scan once
(define (EGALITARIAN my-history other-history)
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) "d" "c"))
          ((string=? (most-recent-play hist) "c")
           (majority-loop (+ 1 cs) ds (rest-of-plays hist)))
          (else
            (majority-loop cs (+ 1 ds) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

(define (count-recent strat hist n)
  (define (loop ans hist n)
    (if (or (empty-history? hist) (= n 0)) ans
      (if (equal? (most-recent-play hist) strat)
        (loop (+ 1 ans) (cdr hist) (- n 1))
        (loop ans (cdr hist) (- n 1)))))
  (loop 0 hist n))

(define (EYE-FOR-TWO-EYE my-history other-history)
    (if (= (count-recent "d" other-history 2) 2)
      "d" "c"))

(define (make-eye-for-n-eyes n)
  (define (eye-for-n-eyes my-history other-history)
    (if (= (count-recent "d" other-history n) n)
      "d" "c"))
  eye-for-n-eyes)

(define EYE-FOR-TWO-EYE2 (make-eye-for-n-eyes 5))

(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  (let ((freq (+ freq0 freq1)))
   (lambda (my-history other-history)
     (if (< (remainder (length my-history) freq) freq0)
       (strat0 my-history other-history)
       (strat1 my-history other-history)))))


(define (gentle strat gentleness-factor)
  (lambda (my-history other-history)
    (if (and (equal? "d" (strat my-history other-history))
             (> (random) gentleness-factor))
      "d"
      "c")))

(define gentle-Nasty (gentle NASTY 0.1))
(define gentle-Eye (gentle EYE-FOR-EYE 0.05))
; play strat-list each other
(define (play-one strat1 strat-list)
  (if (null? strat-list) '()
      (begin (displayln strat1 ) (displayln (car strat-list))
             (play-loop strat1 (car strat-list))
             (play-one  strat1 (cdr strat-list)))))
(define (play-list list1 list2)
  (if (null? list1) '()
      (begin (play-one (car list1) list2)
             (play-list (cdr list1) list2))))
(define (play-test strat-list)
  (play-list strat-list strat-list))

(play-loop NASTY NASTY)
; (play-test (list NASTY PATSY SPASTIC EGALITARIAN EYE-FOR-EYE))
; (play-list (list (make-rotating-strategy EGALITARIAN EYE-FOR-EYE 2 2))
;            (list NASTY PATSY SPASTIC EGALITARIAN EYE-FOR-EYE EYE-FOR-TWO-EYE))
; (play-list (list (make-higher-order-spastic (list NASTY PATSY)))
;            (list NASTY PATSY SPASTIC EGALITARIAN EYE-FOR-EYE))
; (play-list (list gentle-Eye)
;           (list NASTY PATSY SPASTIC EGALITARIAN EYE-FOR-EYE))
