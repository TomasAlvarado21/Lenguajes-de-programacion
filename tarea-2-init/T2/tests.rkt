;; test de core-base

#lang play
(require "core.rkt")
(print-only-errors #t)
;; tests p1

(test (interp-p (parse-cl '{printn 10})) (result (numV 10) '(10)))
(test (interp-p (parse-cl '{printn {printn 10}})) (result (numV 10) '(10 10)))



(test (run-cl '{+ 2 3}) 5)

(test (run-cl '{if0 0 10 20}) 10)

(test (run-cl '{if0 1 10 20}) 20)

;; test with
(test (run-cl '{with {x 1} x}) 1)

(test (run-cl '{with {x 5} {+ x 2}}) 7)

(test (run-cl '{fun {x} x}) 'procedure)

(test (run-cl '{{fun {x} x} 5}) 5)

(test (run-cl '{printn 10}) 10)

(test (run-cl '{with {x 5} {printn x}}) 5)

(test (run-cl '{with {addn {fun {n} {fun {m} {+ n m}}}} {{addn 10} 4}}) 14)





(test (run-cl '{with {addn {fun {n}
                          {fun {m}
                            {+ n m}}}}
                 {{addn 10} 4}})
      14)

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


;; tests de p2
; dame un test de println que use interp-p
(test (interp-p (parse-cl '{printn 10}))
      (result (numV 10) (list 10)))

(test (interp-p (parse-cl '{printn {+ 1 2}}))
      (result (numV 3) (list 3)))



;;(test (interp-p (parse-cl '{with {addn {mfun {n} {mfun {m} {+ {printn n} m}}}} {+ {{addn 10} 4} {{addn 10} 4}}}))      (result (numV 28) (list 10 4 10 4 28)))

;; tests para probar los mcloV y el hash y que en efecto se guardan los valores
(test (interp-p (parse-cl '{with {addn {mfun {n} {mfun {m} {+ {printn n} m}}}} {+ {{addn 10} 4} {{addn 10} 4}}}))      (result (numV 28) '(10)))
(test (interp-p (parse-cl '{with {addn {mfun {n} {mfun {m} {+ {printn n} m}}}} {+ {{addn 10} 4} {{addn 10} 4}}}))      (result (numV 28) '()))


(test (run-cl '{with {x 10} x}) 10)

(test (interp-p (parse-cl '{printn 10})) (result (numV 10) '(10)))


(test (interp-p (parse-cl '{with {add {mfun {x} {mfun {y} {printn {+ x y}}}}}
               {+ {{add 3} 4} {{add 3} 4}}})) (result (numV 14) '(7)))


; este test no imprime el 7 e imprime el resultado guardado en el hash
(test (interp-p (parse-cl '{with {add {mfun {x} {mfun {y} {printn {+ x y}}}}}
               {+ {{add 3} 4} {{add 3} 4}}})) (result (numV 14) '()))

(test (interp-p (parse-cl '{with {add {mfun {g} {mfun {h} {printn {+ {printn g} h}}}}}
                           {+ {{add 5} 7} {{add 3} 2}}}))
      (result (numV 24) '(12 5)))

(test (interp-p (parse-cl '{with {add {mfun {g} {mfun {h} {printn {+ {printn g} h}}}}}
                           {+ {{add 5} 7} {{add 3} 2}}}))
      (result (numV 24) '()))


