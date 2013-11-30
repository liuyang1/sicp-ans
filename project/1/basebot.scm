;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ (* t (+ (* t (/ a 2)) v)) u)))

;; you need to complete this procedure, then show some test cases

; (displayln (position 0 0 0 0))
; (displayln (position 0 0 20 0))
; (displayln (position 0 5 10 10))
; (displayln (position 2 2 2 2))
; (displayln (position 5 5 5 5))
(displayln "Problem 1 ok")


;; Problem 2
(define (delta a b c) (- (square b) (* 4 a c)))
(define root1
  (lambda (a b c)
    (/ (+ (- b) (sqrt (delta a b c))) 2 a)))

(define root2
  (lambda (a b c)
    (/ (- (- b) (sqrt (delta a b c))) 2 a)))

;; complete these procedures and show some test cases
;(displayln (root1 1 -5 6))
;(displayln (root2 1 -5 6))
;(displayln (root1 5 3 6))
;(displayln (root2 5 3 6))
;(displayln (root1 4.9 -9.8 0))
;(displayln (root2 4.9 -9.8 0))
(displayln "Problem 2 ok")

;; Problem 3

; initial upward velocity and initial height or elevation
; XXX gravity is a DOWNWARD acceleration. and UPWARD is positive orien.
(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root2 (/ (- gravity) 2) vertical-velocity elevation)))

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (root1 (/ (- gravity) 2) vertical-velocity (- elevation target-elevation))))

; (displayln (time-to-impact 9.8 0))
; (displayln (time-to-impact 9.8 4.9))
; (displayln (time-to-impact 9.8 5))
; (displayln (time-to-height 9.8 0 4.9))
(displayln "Problem 3 ok")
;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (let ((rad (degree2radian angle)))
    (* (* (cos rad) velocity) 
      (time-to-impact (* (sin rad) velocity) elevation)))))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

;; what is the distance traveled in each case?
;; record both in meters and in feet

; (displayln (travel-distance-simple 1 45 0))
; (displayln (travel-distance-simple 1 45 45))
; (displayln (travel-distance-simple 1 45 90))
(displayln "Problem 4 ok")
; liuyang1, skip feet unit
;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.1)
(define right-angle 90)

(define find-best-angle
  (lambda (velocity elevation)
    (define (loopcheck angle max-dist best-angle)
      (if (> angle right-angle) best-angle
        (let ((new-dist (travel-distance-simple elevation velocity angle)))
                             (if (> new-dist max-dist)
              (loopcheck (+ angle alpha-increment) new-dist angle)
              (loopcheck (+ angle alpha-increment) max-dist best-angle)))))
    (loopcheck 0 0 0)))

; (displayln (find-best-angle 4.9 0))
; (displayln (find-best-angle 4 3))
(displayln "Problem 5 ok")

;; find best angle
;; try for other velocities
;; try for other heights

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
    (if (< y0 0) x0
      (let
       ((drag (- (* (/ 1 m) beta (sqrt (+ (square u0) (square v0)))))))
       (integrate (+ x0 (* u0 dt))
                  (+ y0 (* v0 dt))
                  (+ u0 (* dt (* drag u0)))
                  (+ v0 (* dt (- (* drag v0) g)))
                  dt g m beta)))))

(define (travel-distance velocity elevation angle)
  (integrate 0 
             elevation
             (* (sin (degree2radian angle)) velocity)
             (* (cos (degree2radian angle)) velocity)
             0.01 gravity mass beta))

; (displayln (travel-distance 45 1 45))
; (displayln (travel-distance 40 1 45))
; (displayln (travel-distance 35 1 45))
; (displayln (travel-distance 45 1 60))
; (displayln (travel-distance 45 1 55))
; (displayln (travel-distance 45 1 50))
; (displayln (travel-distance 45 1 45))
; (displayln (travel-distance 45 1 40))
(displayln "Problem 6 ok")
;; RUN SOME TEST CASES

;; what about Denver?

;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance


;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s
(define (radian2degree radian) (* 180 (/ radian pi)))

(define (find-best-angle distance height velocity)
  (define (close dist) (abs (- dist distance)))
  (define (close-enough? error) (< error 0.01))
  (define (loopcheck angle best-angle best-dist-error)
    (if (or (> angle right-angle) (close-enough? best-dist-error))
      best-angle
    (let ((dist (travel-distance velocity height angle)))
     (if (< (close dist) best-dist-error)
       (loopcheck (+ angle alpha-increment) angle (close dist))
       (loopcheck (+ angle alpha-increment) best-angle best-dist-error)))))
  (loopcheck -30 0 1000))

(define (find-best-time distance height velocity)
  (/ distance
     (* velocity (cos (degree2radian (find-best-angle distance height velocity))))))

(displayln (find-best-time 36 1 45))
(displayln (find-best-time 36 1 35))
(displayln (find-best-time 80 1 35))


; TODO:
;; Problem 8

;; Problem 9
