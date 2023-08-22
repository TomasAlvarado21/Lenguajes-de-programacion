#lang play
;; Semana 2-2

(deftype MyList
  (empty)
  (cns v r))

#|
<bt> ::= (leaf <val>)
      | (node

|#

(deftype BinTree
  (leaf v)
  (node v l r))

; predicado de tipo: BinTree?
; constructores: leaf node
; accesores: leaf-v node-v node-l node-r
; predicados de variantes: leaf? node?



; max: BinTree -> Val
; retorna el elemento maximo del arbol (asumiendo que < esta definido)
(define (max-bt bt)
  (match bt
    [(leaf x) x]
    [(node x y z) (max x (max-bt y)(max-bt z))]
    [_ (error "not a BinTree")]))

; test positivos y negativos(con test/exn)
; expected 10
(test (max-bt (leaf 10)) 10)

; expected 10
(test (max-bt (node 5 (leaf 10)(leaf 2))) 10)

; test con excepcion
(test/exn (max-bt 10) "not a BinTree")

; hacer matching de estructuras
; definimos un arbol b
(define b (node 5 (leaf 1) (node 4 (leaf 2) (leaf 3))))

; hacemos el matching
(def (node val left right) b)

; se matchea val con 5, left con (leaf 1)...

#|
otra opcion para mas comoda para esto:
(define val (node-v b))
(define left (node-l b))
(define right (node-r b))
|#

; error vs return
; (+ 1 "not a bintree"))
; (+1 (error "not a bintree"))

;;;;;;;;;;;;;;;

#|<Expr> ::= (num <number>)
           | (add <Expr> <Expr>)
           | (sub <Expr> <Expr>)

|#

(define  (calc expr)
  (match expr
    [(num n) n]
    [(add l r)(+ (calc l)(calc r))]
    [(sub l r)(+ (calc l)(calc r))]))

(test (calc (num 10)) 10)
(test (calc (add (num 10)(sub (num 20) (num 5)))) 25)
; quote convierte cualquier programa en una lista subyacente??? ' <- eso xd por ejemplo '(define x 10) es una lista con los elementos "define" x 10
