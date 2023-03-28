#lang play

#|
<Expr> ::= (num <number>)
         | (add <Expr> <Expr>)
         | (sub <Expr> <Expr>)
(deftype Expr
  (num n)
  (add l r)
  (sub l r))


;; parse : s-expr -> Expr
(define (parse s)
  (match s
    [(? number?) (num s)]
    [(list '+ e1 e2) (add (parse e1) (parse e2))]
    [(list '- e1 e2) (sub (parse e1) (parse e2))]))

;; calc : Expr -> number
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]))

;; P1| a)Agregue al lenguaje la funcion simplify, que elimine todas las apariciones
;;     literales de 0 al sumar o en caso de restar 0.
;;     Ej: (simplify (parse '(+ 0 1))) -> (num 1)
;;         (simplify (parse '(- 3 0))) -> (num 3)

;; simplify: Expr -> Expr
;; retorna una expresion sin neutro aditivo
(define (simplify e)
  (match e
    [(num n) e]
    [(add l r)
     (def new-l (simplify l))
     (def new-r (simplify r))
     (if (equal? (num 0) new-r)
         new-l
         (if (equal? (num 0) new-l)
             new-r
             (add new-l new-r)))]
    [(sub l r)
     (begin (def new-l (simplify l))
        (begin (def new-r (simplify r))
          (if (equal? (num 0) new-r)
              new-l
              (sub new-l new-r))))]
))
(test (simplify (parse '(+ 0 1))) (num 1))
(test (simplify (parse '(- 3 0))) (num 3))
(test (simplify (parse '(+ 0 0))) (num 0))
(test (simplify (parse '(- 0 0))) (num 0))
(test (simplify (parse '(+ 3 2))) (add (num 3) (num 2)))
(test (simplify (parse '(+ 2 (- 1 0)))) (add (num 2) (num 1)))
(test (simplify (parse '(+ 1 (+ 0 0)))) (num 1))
|#
;; P1| b)Agregue al lenguaje un nuevo constructor (bool <boolean>), para representar
;;     valores booleanos, y una nueva operacion (my-if <Expr> <Expr> <Expr>).

#|
<Expr> ::= (num <number>)
         | (bool <boolean>)
         | (add <Expr> <Expr>)
         | (sub <Expr> <Expr>)
         | (my-if <Expr> <Expr> <Expr>)
|#
(deftype Expr
  (num n)
  (bool b)
  (add l r)
  (sub l r)
  (my-if c t f))

;; parse : s-expr -> Expr
(define (parse s)
  (match s
    [(? number?) (num s)]
    [(? boolean?) (bool s)]
    [(list '+ e1 e2) (add (parse e1) (parse e2))]
    [(list '- e1 e2) (sub (parse e1) (parse e2))]
    [(list 'if e1 e2 e3) (my-if (parse e1) (parse e2) (parse e3))]))

;; calc : Expr -> number | boolean 
(define (calc expr)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    [(my-if c t f) (if (calc c) (calc t) (calc f))]))

#|
<AB> ::= (hoja <number>)
         | (nodo <AB> <AB>)
|#
;; P2| a) Defina el tipo de los arboles binarios

(deftype AB
  (hoja n)
  (nodo l r))

;; P2| b) Defina la funcion map-ab que si no recibe un arbol entrega un error.
;;     Hint: existen AB? y error.

;; map-ab: AB | Any * (Number -> Number) -> AB | Error
;; Cambia los valores de las hojas, usando la funcion
;; que recibe como argumento.
(define (map-ab ab f)
  (if (AB? ab) 
    (match ab
      [(hoja n) (hoja (f n))]
      [(nodo l r) (nodo (map-ab l f) (map-ab r f))])
    (error "no es un arbol")
  ))

(test (map-ab (hoja 1) add1) (hoja 2))
(test/exn (map-ab 1 add1) "no es un arbol")
(test (map-ab (nodo (hoja 3) (hoja 2)) add1) (nodo (hoja 4) (hoja 3)))
