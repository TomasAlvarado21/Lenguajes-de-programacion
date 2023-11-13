#lang play

(define n
  (box 0))

(define (f x)
  (begin
    (set-box! n (+ (unbox n) x))
    (unbox n)))

(define (g x)
  (begin
    (set-box! n (+ (unbox n) x))
    (unbox n)))

;; equal? de funciones hace referencia a ubicaciones de memoria.
;; Hay un concepto: "observablemente equivalentes".
;; Para los mismos argumentos, se comportan igual...


;(let ([a (box 1)])
  ;(begin (let ([b 3]) b)
   ;      b))

;; con scope dinamico y mutacion?

#|
(define m 0)

(define (ff x)
  (+ m x))

(equal? (ff 1) (let-dynamic ([m 1]) (ff 1)))
|#

(let ([b (box 4)])
  (+ (let ([dummy (set-box! b 5)]) (unbox b))
     (unbox b)))

#|
- tiempo 0: se crea loc #0 y valor 4 para la caja "b". (store= [ #0 -> 4])

Elegir: interpretar el lado izquierdo/derecho de la suma primero.

- Opción 1: Primero el lado izquierdo:
  - Se agrega loc #1, y se asocia 'dummy'->#1 en el ambiente. (store= [ #0 -> 5, #1 -> ?]) 
    - Se interpreta el cuerpo del let: (unbox b) ---> se interpreta con store = [#0 -> 5, #1 -> ?]
    - Al interpretar (unbox b), se busca "b" en el ambiente.
    - b -> #0
    - y ahora se busca el valor de la ubicación #0 en el store. Y en este momento es el valor 5.
    - El resultado del lado izquierdo retorna un Value*Store que es (5, [#0 -> 5, #1 -> ?]) 
  - Evaluamos el lado derecho con store "inicial" recibido desde la evaluación del lado izquierdo,
    es decir [#0 -> 5, #1 -> ?].
  - Evaluamos (unbox b) en el store [#0 -> 5, #1 -> ?], y da resultado 5.
    Genera un value*store (5, [#0 -> 5, #1 -> ?])
  - Luego, sumamos los valores de cada lado: 5+5 --> 10.

- Opción 2: Primero el lado derecho
  - Evaluamos el lado derecho (unbox b) en el store [ #0 -> 4], y da resultado 4.
    Genera un value*store (4, [ #0 -> 4])
  
  - Evaluar lado izquierdo partiendo del store generado en el lado derecho [ #0 -> 4]:
    - Se agrega loc #1, y se asocia 'dummy'->#1 en el ambiente. (store= [ #0 -> 5, #1 -> ?]) 
    - Se interpreta el cuerpo del let: (unbox b) ---> se interpreta con store = [#0 -> 5, #1 -> ?]
    - Al interpretar (unbox b), se busca "b" en el ambiente.
    - b -> #0
    - y ahora se busca el valor de la ubicación #0 en el store. Y en este momento es el valor 5.
    - El resultado del lado izquierdo retorna un Value*Store que es (5, [#0 -> 5, #1 -> ?]) 
  - Evaluamos el lado derecho con store "inicial" recibido desde la evaluación del lado izquierdo,
    es decir [#0 -> 5, #1 -> ?].
  
  - Luego, sumamos los valores de cada lado: 5+4 --> 9.



Los estados se consideran como efectos secundarios.

Para poder compatibilizar el alcance estatico con estructuras de datos mutables, tenemos que usar 
los ambientes y los stores. (si o si)








|#



