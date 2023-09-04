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
;; funcion que simplifica las negaciones de una proposicion, es decir, elimina las dobles negaciones y las negaciones de and y or
(define (simplify-negations p)
    (match p
        [(varp n) (varp n)]
        [(andp p q) (andp (simplify-negations p) (simplify-negations q))]
        [(orp p q) (orp (simplify-negations p) (simplify-negations q))]
        [(notp p) (match p
                        [(varp n) (notp (varp n))]
                        [(andp p q) (orp (simplify-negations (notp p)) (simplify-negations (notp q)))]
                        [(orp p q) (andp (simplify-negations (notp p)) (simplify-negations (notp q)))]
                        [(notp p) (simplify-negations p)])]))

; ejemplo de uso de simplify-negations
;(simplify-negations (notp (notp (orp (varp "a") (andp (varp "b") (varp "c"))))))
;(simplify-negations (notp (orp (varp "a") (varp "b"))))
#| Parte B |#

;; distribute-and :: Prop -> Prop
;; aplicar la distributividad de and cada vez que sea posible

(define (distribute-and p)
    (match p
        [(varp n) (varp n)]
        [(andp p q) (match (distribute-and p)
                        [(orp p1 p2) (orp (distribute-and (andp p1 q)) (distribute-and (andp p2 q)))]
                        [p (match (distribute-and q)
                                [(orp q1 q2) (orp (distribute-and (andp p q1)) (distribute-and (andp p q2)))]
                                [q (andp p q)])])]
        [(orp p q) (orp (distribute-and p) (distribute-and q))]
        [(notp p) (notp (distribute-and p))]))

; ejemplo de uso de distribute-and

;(distribute-and (orp (andp (varp "a") (varp "b")) (andp (varp "c") (varp "d"))))
;(distribute-and (andp (orp (varp "p") (varp "q")) (varp "r")))

#| Parte C |#

;; apply-until :: (a -> a) (a a -> Boolean) -> a -> a
;; dada una funcion f y un predicado p, se retorna una funcion nueva. A esta nueva funcion se le da un
;; elemento x, y se aplica f a x hasta que se cumpla p, es decir, se aplica f a x hasta que p f(x) sea true

(define (apply-until f p)
  (letrec
      ((apply-rec
        (lambda (x prev)
          (if (p x prev)
              x
              (apply-rec (f x) x)))))
    (lambda (x)
      (apply-rec (f x) x))))

 (( apply-until
(\lambda (x) (/ x (add1 x)))
(\lambda (x new-x) (<= (- x new-x) 0.1))) 1)


#| Parte D |#

;; DNF :: Prop -> Prop
;; dada una proposición, le aplica las transformaciones ya definidas tantas veces
;; sea necesario para lograr la forma normal disyuntiva.

(define (DNF p)
    (letrec
        ((simplify-negations-rec (apply-until simplify-negations (lambda (x y) (equal? x y)))))
        (letrec
            ((distribute-and-rec (apply-until distribute-and (lambda (x y) (equal? x y)))))
            (distribute-and-rec (simplify-negations-rec p)))))

#| P3 |#

#| Parte A |#

;; fold-prop :: (String -> a) (a a -> a) (a a -> a) (a -> a) -> Prop -> a
;; funcion que captura el esquema de recursión de Prop
; (define (fold-prop varp andp orp notp)
;     (lambda (p)
;         (match p
;             [(varp n) (varp n)]
;             [(andp p q) (andp (fold-prop varp andp orp notp p) (fold-prop varp andp orp notp q))]
;             [(orp p q) (orp (fold-prop varp andp orp notp p) (fold-prop varp andp orp notp q))]
;             [(notp p) (notp (fold-prop varp andp orp notp p))])))

;; ejemplo de uso de fold-prop


#| Parte B |#

;; occurrences-2 :: Prop String -> Number

;; vars-2 :: Prop -> (Listof String)

;; eval-2 :: Prop (Listof (Pair String Boolean)) -> Boolean

;; simplify-negations-2 :: Prop -> Prop

;; distribute-and-2 :: Prop -> Prop
