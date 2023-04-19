#lang play
(define (prueba list)
  [match list
    [(list (list a b c) ... z)
     (foldr + z a)]])
#|
(define (prueba list)
  [match list
    [(list a b ... (list c d) ... body)  (a b (list c d) body )]])
|#
(define (bla list)
  [match list
    (list 

(define lista-prueba (list (list 2 3 6) (list 4 5 8) 7)) 
(prueba lista-prueba)


(define (sumatoria lista)
  (foldr + 0 lista));; foldr(recibe 2 argumentos) le entregamos una operacion, el valor inicial y los argumentos que queremos procesar

(sumatoria lista-prueba)

(define (doble lista)
  (map (λ (x) (* x 2)) lista));; map(recibe solo un argumento) aplica funcion a todos los argumentos

(doble lista-prueba)
