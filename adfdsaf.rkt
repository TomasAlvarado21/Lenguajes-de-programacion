#lang play

;;(print-only-errors #t)

#|
<BinTree> ::=  (leaf <value>)
            |  (in-node <value> <BinTree> <BinTree>)
|#
;; Inductive type for representing binary trees 
(deftype BinTree
  (leaf value)
  (in-node value left right))


;; Constructors
(define my-bintree
  (in-node 5
           (in-node 4 (leaf 1) (leaf 8))
           (leaf 3)))

;; Accesor functions
(in-node-value my-bintree)
(in-node-left my-bintree)

;; Type predicate
(BinTree? my-bintree)
(BinTree? "hola")


;; Recursive functions (via pattern
;; matching against the inductive type)

;; height :: BinTree -> Integer
;; Returns the height of the binary tree
(define (height bt)
  (match bt
    [(leaf _) 0]
    [(in-node _ l r) (+ 1 (max (height l) (height r)))]))

(test (height (leaf 7)) 0)
(test (height (in-node 3 (leaf 1) (leaf 2))) 1)


;; contains? :: BinTree Number -> Boolean
;; Determines whether a binary tree contains a given numeric value
(define (contains? bt n)
  (match bt
    [(leaf v) (equal? v n)]
    [(in-node v l r) (or (equal? v n)
                      (contains? l n)
                      (contains? r n))]))

(test (contains? my-bintree 4) #t)
(test (contains? my-bintree 2) #f)


;; fold-bintree :: (Number -> A) (Number A A -> A) -> (Bintree -> A)
;; fold over numeric binary trees
(define (fold-bintree f g)
  (λ (bt)
    (match bt
    [(leaf v) (f v)]
    [(in-node v l r) (g v
                        ((fold-bintree f g) l)
                        ((fold-bintree f g) r))])))


;; max-bintree :: BinTree -> Number
;; Returns the maximum element of a (numeric) binary tree
(define max-bintree
  (fold-bintree identity max))

(test (max-bintree my-bintree) 8)


;; sum-bintree :: Bintree -> Number
;; Returns the sum of the elements of a numeric binary tree
(define sum-bintree
  (fold-bintree identity +))

(test (sum-bintree my-bintree) 21)