#lang play
;; definimos la funcion union que toma 2 listas y devuelve la union de ambas, intercalando los elementos de la primera con la segunda
;; tiene que ser recursiva por la cola

(define (union l1 l2 accumulador)
    (if (null? l1)
        (append accumulador l2)
        (if (null? l2)
            (append accumulador l1)
            (union (cdr l1) (cdr l2) (append accumulador (list (car l1) (car l2)))))))

(union (list 1 2 3) (list 10 20 30 40 50) (list))