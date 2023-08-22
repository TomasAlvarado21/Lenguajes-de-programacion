#lang play

(map add1 (list 1 2 3 4))

(map (lambda (x) (+ x 2))(list  1 2 3))


;; version currificada de la adicion
;; (+) Number: * Number -> Number
;; addn : Number -> (Number -> Number)
(define (addn n)
  (lambda (m)
    (+ n m)))


(define (applyTo1 f)
  (f 1))
;; Num * Num * Num -> Num
(define (foo x y z)
  (/ (+ x y) z))

(define (foo2 x)
  (λ (y)
    (λ (z)
      (/ (+ x y) z))))

; version con todo anidado (curryficada)
; Num -> Num -> Num -> Num
(define foo5
  (λ (x)
    (λ (y)
      (λ (z)
        (/ (+ x y) z)))))

; azucar sintactica para λ anidados
(define (((foo4 x) y) z)
  (/ (+ x y) z))

; uncurry:: (Num -> Num -> Num) -> (Num * Num -> Num)
; curry:: (Num * Num -> Num) -> (Num -> Num -> Num)

; Primero tenemos que tener en cuenta estas cosas para tener bien hecha la funcion
; 1- Firma
; 2- Descripcion
; 3- Ejemplos/test
; 4- Implementar


; BNF backus naur form
; <expr> ::= <num>
;          | (add <exp> <exp>)

; <list> ::= exmpty | (cons <val> <list>)
; <bintree> ::= (leaf <val>) | (node <val> <bintree> <bintree>

; BNF -> AST(arbol de sintaxis abstracta)
; osea es un parser

; <nat> ::= zero | (suc <nat>) Logico
; Vn P(n) por induccion
; P(z)
; Vn P(n) -> P(S(n))
; hipotesis inductiva

; Programacion por recursion
; match  n
; z -> ...
; S(n) -> ... f(n)

(define (even? n)
  (if (zero? n)
      #t
      (not (even? (sub1 n)))))

(define (f s)
  (match s
    ["hola" 1]
    ["chao" 2]
    [_ 5]))

#| funcion general
(define (f l)
  (match l
    ['() ...]
    [(cons v rst) ... (f rst) ...]))
|#


(define (len l)
  ( match l
     ['() 0]
     [(cons v rst) (add1 (len rst))]))

; contains? :: List * Val -> Bool
(define (contains? l v)
  (match l
    ['() #f]
    [(cons w rst) (or (equal? v w)
                      (contains? rst v))]))