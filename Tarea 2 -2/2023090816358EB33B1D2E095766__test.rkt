#lang play
(require "202309081635B0A2EE4D9C18F725__T2.rkt")

(print-only-errors #t)





(test (eval (num 42) empty-env) (numV 42))
(test (eval (add (num 2) (num 3)) empty-env) (numV 5))
(test (eval (sub (num 5) (num 3)) empty-env) (numV 2))
(test (eval (mul (num 2) (num 4)) empty-env) (numV 8))
(test (eval (leq (num 3) (num 5)) empty-env) (boolV #t))
(test (eval (ifc (leq (num 3) (num 5)) (num 2) (num 4)) empty-env) (numV 2))
(test (eval (fun 'x (add (id 'x) (num 1))) empty-env) (closureV 'x (add (id 'x) (num 1)) empty-env))
(test (eval (tupl (list (num 1) (num 2) (num 3))) empty-env) (tupl (list (numV 1) (numV 2) (numV 3))))


(define (foo* x y)
  (num* x y))

(define curried-foo (curry* foo*))

(define foo-5 (curried-foo (numV 5)))

(test (foo-5 (numV 10)) (numV 50)) ; devuelve NumV 50


(define (add* x y)
  (num+ x y))

(define curried-add (curry* add*))

(define uncurried-add (uncurry* curried-add))

(test (uncurried-add (numV 2) (numV 3)) (numV 5)) ; devuelve (numV 5)

(define (xd x y z)
  (+ x y z))

(define partial-add (partial* xd 2 3))

(test (partial-add 4) 9) ; devuelve 9
  

(test ((swap* +) 2 3) 5)

(test ((uncurry* (curry* +)) 2 3) 5)
(test ((partial* + 2 3) 4) 9)

(test ((partial* * 2 3) 4) 24)
