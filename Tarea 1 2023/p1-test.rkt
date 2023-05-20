#lang play
(require "p1.rkt")
(print-only-errors #t)
(require "env.rkt")






; test booleanos
(test (parse-expr #t) (bool #t))
(test (parse-expr #f) (bool #f))

;test add
(test (parse-expr '(+ 1 2)) (add (num 1) (num 2)))

;test add1
(test (parse-expr '(add1 1)) (add1 (num 1)))

;test lt
(test (parse-expr '(< x 5)) (lt (id 'x) (num 5)))

;test =
(test (parse-expr '(= (+ 1 2) 3)) (eq (add (num 1) (num 2)) (num 3)))


;test !
(test (parse-expr '(! #f)) (neq (bool #f)))


;test and
(test (parse-expr '(&& #t #f)) (and0 (bool #t) (bool #f)))

;test or
(test (parse-expr '(|| #t #f)) (or0 (bool #t) (bool #f)))

;test if
(test (parse-expr '(if #t 1 2)) (if0 (bool #t) (num 1) (num 2)))

;test with con add
(test (parse-expr '(with ((x 1) (y 2)) (+ x y)))
      (with (list (list 'x (num 1)) (list 'y (num 2))) (add (id 'x) (id 'y))))

;test cons
(test (parse-expr '(cons 1 2)) (cons0 (num 1) (num 2)))

;test fst y cons
(test (parse-expr '(fst (cons 1 2))) (fst (cons0 (num 1) (num 2))))

;test snd y cons
(test (parse-expr '(snd (cons 1 2))) (snd (cons0 (num 1) (num 2))))


;test integracion with, cons, snd, fst y app
(test (parse-expr    '{with {{x 9} {y {cons 1 {cons 3 4}}}}
        {sum x {fst y} {snd y}} })
      (with (list (list 'x (num 9)) (list 'y (cons0 (num 1) (cons0 (num 3) (num 4)))))
            (app 'sum (list (id 'x) (fst (id 'y)) (snd (id 'y))))))



;test unitario cons
(test (parse-expr '{cons 1 2})
      (cons0 (num 1) (num 2)))

; test conjunto with con cons
(test (parse-expr '{with {{y 9}{x {cons 1 {cons 2 3}}}} x})
      (with (list (list 'y (num 9))(list 'x (cons0 (num 1) (cons0 (num 2) (num 3))))) (id 'x)))

;test interp
;|
;v

;test num
(define env1 '())
(define funs1 '())
(test (interp (num 33) env1 funs1) (numV 33))

;test interp bool
(define env2 '())
(define funs2 '())
(test (interp (bool #t) env2 funs2) (boolV #t))

;test interp sum
(define env3 '())
(define funs3 '())
(test (interp (add (num 10) (num 20)) env3 funs3) (numV 30)) ;; Debería retornar (numV 30)



;test interp con if y bool
(define env5 '())
(define funs5 '())
(test (interp (if0 (neq (bool #t)) (num 2) (num 0)) env5 funs5) (numV 0)) 

;test interp con cons
(define env6 '())
(define funs6 '())
(test (interp (cons0 (num 1) (num 2)) env6 funs6) (pairV (numV 1) (numV 2)))






(test (run '{ #t }) (boolV #t))
;; Salida esperada: boolV true
(test (run '{ (+ 3 4) })(numV 7))
;; Salida esperada: numV 7

(test (run '{ (< 3 5) })(boolV #t))
;; Salida esperada: boolV true



(test (run '{5}) (numV 5))
(test (run '{#t}) (boolV #t))


(define env11 '())
(define funs11 '())

(test (interp (with (list (list 'x (num 5)) (list 'z (add (num 11) (num -3)))) (id 'z))
          empty-env
          '())
      (numV 8))

(test (interp (with (list (list 'x (num 5)) (list 'z (cons0 (num 11) (num -3)))) (id 'z))
        empty-env
          '())
      (pairV (numV 11) (numV -3)))


(test (interp (with (list (list 'x (num 5)) (list 'z (cons0 (num 11) (num -3)))) (snd (id 'z)))
        empty-env
          '())
      (numV -3))

(test (interp (with (list (list 'x (num 5)) (list 'z (cons0 (num 11) (num -3)))) (fst (id 'z)))
        empty-env
          '())
      (numV 11))




(test (interp (with (list (list 'x (num 5)) (list 'z (cons0 (num 5) (cons0 (add (num 2)(num 11)) (num -3))))) (id 'z))
        empty-env
          '())
      (pairV (numV 5) (pairV (numV 13) (numV -3))))


(test (interp (parse-expr '{with {{x 9} {y {cons 1 {cons 3 4}}}}
             {+ {fst y} {snd {snd y}}}})
              empty-env
          '())
      (numV 5))

(test (run '{ ;; Programa de Ejemplo 1
             {define {sum x y z} {+ x {+ y z}}}
             {define {cadr x} {fst {snd x}}}
             {with {{x 9} {y {cons 1 {cons 3 4}}}}
                   {sum x {fst y} {cadr y}} }
             })
      (numV 13))



(test (run '{ ;; Programa de Ejemplo 2
             {with {{x 5} {y 23} {z {cons 11 -3}}}
                   z}
             })
      (pairV (numV 11) (numV -3)))

(test (run '{ ;; Programa de Ejemplo 3
             {define {triple x} {+ x {+ x x}}}
             {define {add2 x} {+ 2 x}}
             {add2 {triple 2}}
             })
      (numV 8))

(test (run '{ ;; Programa de Ejemplo 4
             {with {{x 3} {y {+ 1 2}}}
                   {if {= x y} x y}}
             })
      (numV 3))
