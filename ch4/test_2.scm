; test seq
(load "eval.scm")
(display "load -- eval.scm")
(newline)

(myeval '(define x 3) the-empty-env)
