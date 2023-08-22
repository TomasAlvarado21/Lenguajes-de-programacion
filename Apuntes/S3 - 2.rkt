#lang play
; Alcance de los ids (Scope)

; Estatico(lexico) = region de texto
; Dinamico = region de computo
; Global
; Estas 3 son distintas entre sí

{with {x {+ 1 1}} ; biding oc (ocurrencia enlazada)
      {+ {x 12} ; biding oc
         x} ; bound oc
      {+ x z}} ; z es un identificador libre y x es bound
; el alcance se lo damos nosotros o se lo da el mismo lenguaje?
; el with es lo que nos da ese limite? es el with una funcion propia de scheme?

; Ahora lo ponemos en practica
; subst : id val expr -> expr

(define (subst x val expr)
  (match expr
    [(num n) expr]
    [(add l r) (add (subst x val l)(subst x val r))]
    [(sub l r) (sub (subst x val l)(subst x val r))]
    [(with y ne body)
     (with y ; no se toca
           (subst x val ne) ; siempre (named expr)
           (if (equal? x y)
               body ; no se toca, scope anidado!
               (subst c val body)))] ; propagar
    [(id y) (if (equal? x y)
                val ; substituir!!!
                expr)])) ; nada que hacer
           
    [(id y) ...]