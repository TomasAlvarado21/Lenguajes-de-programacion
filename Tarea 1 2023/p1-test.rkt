#lang play
(require "p1.rkt")

;test integracion with, cons, snd, fst y app
(test (parse-expr    '{with {{x 9} {y {cons 1 {cons 3 4}}}}
        {sum x {fst y} {snd y}} })
      (with (list (list (id 'x) (num 9)) (list (id 'y) (cons0 (num 1) (cons0 (num 3) (num 4)))))
            (app 'sum (list (id 'x) (fst (id 'y)) (snd (id 'y))))))



;test unitario cons
(test (parse-expr '{cons 1 2})
      (cons0 (num 1) (num 2)))

; test conjunto with con cons
(test (parse-expr '{with {{y 9}{x {cons 1 {cons 2 3}}}} x})
      (with (list (list (id 'y) (num 9))(list (id 'x) (cons0 (num 1) (cons0 (num 2) (num 3))))) (id 'x)))

#|




(test (run '{5}) (numV 5))
(test (run '{#t}) (boolV #t))

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

|#