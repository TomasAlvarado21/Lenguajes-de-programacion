#lang play

#| P1 |#

#| Parte A |#
;; Definir el tipo de datos recursivo Prop y escribir la gramatica
;; Gramatica:
;; Prop ::= VarP
;;        | (andp Prop Prop)
;;        | (orp Prop Prop)
;;        | (notp Prop)

(deftype Prop
    [varp n]
    [andp p q]
    [orp p q]
    [notp p])

#| Parte B |#
;; usando el tipo de datos recursivo Prop, definir las siguientes funciones:

;; occurrences :: Prop String -> Number
;; funcion que devuelve la cantidad de veces que aparece una variable en una proposicion

(define (occurrences Prop String)
    (match Prop
        [(varp n) (if (string=? n String) 1 0)]
        [(andp p q) (+ (occurrences p String) (occurrences q String))]
        [(orp p q) (+ (occurrences p String) (occurrences q String))]
        [(notp p) (occurrences p String)]))


#| Parte C |#

;; vars :: Prop -> (Listof String)
;; funcion que devuelve una lista con todos los nombres de variables que ocurren en la proposicion
;; no debe tener duplicados, usa la funcion inlist? para verificar si un string esta en una lista de strings
;; antes de agregarlo a la lista de variables hay que ejecutar la parte izquierda y luego la derecha
(define (vars Prop)
  ;; creamos una lista vacia en la que vamos a ir agregando los nombres de las variables
  (define vars-list '())
  (match Prop
    [(varp n) (if (inlist? n vars-list) vars-list (cons n vars-list))]
    [(andp p q) 
        (define left-vars (vars p))
        (define right-vars (vars q))
        (append left-vars (filter (lambda (x) (not (inlist? x left-vars))) right-vars))]
    [(orp p q) 
        (define left-vars (vars p))
        (define right-vars (vars q))
        (append left-vars (filter (lambda (x) (not (inlist? x left-vars))) right-vars))]
    [(notp p) (vars p)]))

;; inlist? :: String (Listof String) -> Boolean
;; funcion que verifica si un string esta en una lista de strings
(define (inlist? String List)
    (match List
        ['() #f]
        [(cons x xs) (if (string=? x String) #t (inlist? String xs))]))

#| Parte D |#

;; all-environments :: (Listof String) -> (Listof (Listof (Pair String Boolean)))
;; funcion que dada una lista de variables, sin duplicados, crea todos los ambientes de evaluacion posible

(define (all-environments l)
    (match l
        ['() (list '())]
        [(cons x xs) (append (map (lambda (e) (cons (cons x #t) e)) (all-environments xs))
                             (map (lambda (e) (cons (cons x #f) e)) (all-environments xs)))]))

#| Parte E |#

;; eval :: Prop (Listof (Pair String Boolean)) -> Boolean
;; funcion que evalua una proposicion p, obteniendo los valores de cada varaibles desde un ambiente env, devolviendo
;; el valor de verdad de la proposicion.

;; ocupar funcion assoc para obtener el valor de una variable en un ambiente
;; si no se encuentra la variable en el ambiente, se entrega un mensaje de error

(define (eval p env)
    (match p
        ;el error se muestra de la siguiente forma: (error "eval: variable a is not defined in environment")
        [(varp n) (if (assoc n env) (cdr (assoc n env)) ((error (format "eval: variable ~a is not defined in environment" n))))]
        [(andp p q) (and (eval p) (eval q))]
        [(orp p q) (or (eval p) (eval q))]
        [(notp p) (not (eval p))]))

#| Parte F |#

;; tautology? :: Prop -> Boolean
;; funcion que retorna true si la proposicion es una tautologia, es decir, si es verdadera para todos los ambientes de evaluacion

(define (tautology? p)
    (match p
        [(varp n) #f]
        [(andp p q) (and (tautology? p) (tautology? q))]
        [(orp p q) (or (tautology? p) (tautology? q))]
        [(notp p) (not (tautology? p))]))




#| P2 |#

#| Parte A |#

;; simplify-negations :: Prop -> Prop

#| Parte B |#

;; distribute-and :: Prop -> Prop

#| Parte C |#

;; apply-until :: (a -> a) (a a -> Boolean) -> a -> a

#| Parte D |#

;; DNF :: Prop -> Prop



#| P3 |#

#| Parte A |#

;; fold-prop :: (String -> a) (a a -> a) (a a -> a) (a -> a) -> Prop -> a

#| Parte B |#

;; occurrences-2 :: Prop String -> Number

;; vars-2 :: Prop -> (Listof String)

;; eval-2 :: Prop (Listof (Pair String Boolean)) -> Boolean

;; simplify-negations-2 :: Prop -> Prop

;; distribute-and-2 :: Prop -> Prop
