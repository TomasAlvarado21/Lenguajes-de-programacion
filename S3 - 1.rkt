#lang play
; el pipeline de un lenguaje seria hacer: src ->(quote) S-expr ->(parse) Expr ->(Interp) Numb/Val
; Interp(parse(prog))
#| BNF mezcla elementos de sintaxis abstracta (<number>, <Expr>) y concreta ('{ '+ '})
<Expr> ::= <number>
          | {+ <Expr> <Expr>}
          | {- <Expr> <Expr>}
          | <id>
          | {with {id expr} Expr}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id x)
  (with id named-expr body))

; parse : s-expr -> Expr
(define (parse s)
  (match s
    [(? number?) (num s)]
    [(? symbol?) (id s)]
    [(list '+ e1 e2)(add (parse e1)(parse e2))]
    [(list '- e1 e2)(add (parse e1)(parse e2))]
    [(list 'with (list id ne) body) (with id (parse ne) (parse body))]))

; Interprete
(define (interp expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l) (interp r))]
    [(sub l r) (+ (interp l) (interp r))]))

; Subsr :: Id val Expr -> Expr
; remplaza las ocurrencias del Id en Expr por los Val y retorna una Expr




; Identificadores (variables matematicas)
#|
1. Sintaxis
{with {x 0}
     {+ x x}}
falta una categoria sintactica para los identificadores <id> y agregar la expresion with

Sea x = 2
y queremos hacer x + x -> lo que nos pide es que hagamos substitucion, cosa de que se remplacen ambos x por el valor(2) osea
que f(x) = x + x -> 2 + 2 -> 4
|#

(let ([x 2]) x)
(let ([y (+ 1 3)]) y)

(let ([z 10])
  (+ (let ([x 1])
       (+ z x))
     (let ([x z])
       (- x z))))