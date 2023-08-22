#lang play
(print-only-errors #t)

;; vamos a definir los arboles binarios, para esto tenemos que definir los nodos y las hojas

#|
    <BinTree> ::= (leaf <Value>)
                | (node <BinTree> <BinTree>)
|#

(deftype BinTree
    (leaf value)
    (node value left right))

;; constructores 
(define my-tree
    (node 1
        (node 2
            (leaf 3)
            (leaf 4))
        (node 5
            (leaf 6)
            (leaf 7))))

;; destructores
(node-value my-tree)
(node-left my-tree)
(node-right my-tree)

;; vemos si son arboles binarios
(BinTree? my-tree)
(BinTree? 5)

;; height ::= <BinTree> -> <int>
;; devuelve la altura del arbol
(define (height bt)
    (match bt
        [(leaf _) 0]
        [(node _ left right)
            (+ 1 (max (height left) (height right)))]))

;; esta funcion solo puede recibir un bintree, esta garantiza que funciona bien para todas las entradas
;; que sean BinTree

;; max-bt :: BinTreeof Integer -> Integer
;; devuelve el maximo de un arbol binario de enteros

(define (max-bt bt)
    (match bt
    ((leaf v) v)
    ((node v left right)
        (max v (max (max-bt left) (max-bt right))))))

;; test 
(test (max-bt my-tree) 7)

;; esquema de recursion general

(define (foo bt)
    (match bt
        ((leaf v) (f v))
        ((node v l r) (g v (foo l) (foo r)))))

;; ahora vamos a hacer un fold para arboles binarios

(define (fold-bt f g)
    (lambda (bt)
        (match bt
            ((leaf v) (f v))
            ((node v l r) (g v (fold-bt f g l) (fold-bt f g r))))))

