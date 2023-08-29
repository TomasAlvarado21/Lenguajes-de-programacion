#lang play
(require "T1.rkt")

(print-only-errors #t)

;; B) occurrences
(test (occurrences (varp "a") "b") 0)
(test (occurrences (andp (varp "a") (varp "b")) "a") 1)
(test (occurrences (andp (varp "a") (varp "a")) "a") 2)

;; C) vars

(test (vars (varp "a")) ( list "a"))
(test (vars (andp (varp "a") (varp "b"))) (list "a" "b"))
(test (vars (andp (varp "a") (varp "a"))) (list "a"))

;; D) all-envs

(test (all-environments ( list )) ( list ( list )))
(test (all-environments ( list "a")) ( list
( list (cons "a" #t))
( list (cons "a" #f)))
)
(test (all-environments ( list "a" "b"))
( list ( list (cons "a" #t) (cons "b" #t))
( list (cons "a" #t) (cons "b" #f))
( list (cons "a" #f) (cons "b" #t))
( list (cons "a" #f) (cons "b" #f))))

;; E) eval
(test (eval (varp "a") ( list (cons "a" #t))) #t)
(test (eval (varp "a") ( list (cons "a" #f))) #f)
(test/exn (eval (varp "a") ( list )) "eval: variable a is not defined in environment")

(test (tautology? (orp (varp "a") (notp (varp "a")))) #t)
(test (tautology? (andp (varp "a") (notp (varp "a")))) #f)

; ejemplo de uso de DNF
(test (DNF (andp (orp (varp "a") (varp "b")) (orp (varp "c") (varp "d"))))
(orp (orp (andp (varp "a") (varp "c")) (andp (varp "a") (varp "d"))) (orp (andp (varp "b") (varp "c")) (andp (varp "b") (varp "d")))))

(test (DNF (andp (orp (varp "a") (varp "b")) (orp (varp "c") (varp "d"))))
(orp (orp (andp (varp "a") (varp "c")) (andp (varp "a") (varp "d"))) (orp (andp (varp "b") (varp "c")) (andp (varp "b") (varp "d")))))
