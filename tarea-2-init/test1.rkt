;; test de core-base

#lang play
(require "core.rkt")
;(print-only-errors #t)
;; tests

(test (interp-p (parse-cl '{printn 10})) (result (numV 10) '(10)))
(test (interp-p (parse-cl '{printn {printn 10}}) (result (numV 10) '(10 10))))

(test (run-cl '{+ 2 3}) 5)

(test (run-cl '{if0 0 10 20}) 10)

(test (run-cl '{if0 1 10 20}) 20)

;; test with
(test (run-cl '{with {x 1} x}) 1)

(test (run-cl '{with {x 5} {+ x 2}}) 7)

(test (run-cl '{fun {x} x}) 'procedure)

(test (run-cl '{{fun {x} x} 5}) 5)

(test (run-cl '{printn 10}) 10)

;(test (run-cl '{with {x 5} {printn x} x}) 5)

(test (run-cl '{with {addn {fun {n} {fun {m} {+ n m}}}} {{addn 10} 4}}) 14)





(test (run-cl '{with {addn {fun {n}
                          {fun {m}
                            {+ n m}}}}
                 {{addn 10} 4}})
      14)
;; ...
(test (run-cl '{+ 1 2}) 3)

(test (run-cl '{if0 0 10 20}) 10)

(test (run-cl '{if0 1 10 20}) 20)

(test (run-cl '{with {x 5} {+ x 2}}) 7)

(test (run-cl '{with {x 5}
                 {with {y 10}
                   {+ x y}}}) 15)

(test (run-cl '{fun {x} {+ x 1}}) 'procedure)

(test (run-cl '{{fun {x} {+ x 1}} 5}) 6)

(test (run-cl '{printn 10}) 10)

;(test (run-cl '{with {x 5}
                ; {printn x}
                 ;{+ x 2}}) 7)
