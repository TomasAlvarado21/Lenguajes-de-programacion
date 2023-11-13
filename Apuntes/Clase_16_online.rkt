#lang play

;; optimizacion de llamadas recursivas por la cola(TCO)


;; llamadas de funcion y stacks
;; para cada llamada de funcion se crea un nuevo stack frame y se guarda en el stack,
;; el stack frame es un bloque de memoria que contiene los argumentos de la funcion 
;; y las variables locales, cuando la funcion termina se elimina el stack frame y se recupera el stack frame anterior

;; la optimizacion nos permite tener un tamaño de stack constante, ya que no se crean nuevos stack frames(se reutiliza el mismo)
;; esto es porque el invocador no necesita realizar ningun computo adicional despues de la llamada recursiva

;; cualquier funcion recursiva puede ser optimizada por la cola

;; es facil ver cuando una funcion es TCO o no, si la funcion tiene una llamada recursiva y no hay computo adicional despues de la llamada

;; una funcion recursiva es TCO si la llamada recursiva es la ultima operacion que se realiza en la funcion


;; ahora veremos TRO (optimizacion de llamadas recursivas por la cola)
;; es una optimizacion que se puede aplicar a funciones que no son TCO
;; la idea es convertir una llamada recursiva en un bucle, para que no se creen nuevos stack frames

;; TRO no es una propiedad del lenguaje, sino que es una propiedad de la implementacion del lenguaje




;; las funciones que se implementan en la cola no necesitan crear nuevos stack frames(se reutiliza el mismo)
;; se reutiliza el stack frame para tener un tamaño de stack constante
;; todas las funciones recursivas se pueden transformar en funciones recursivas por la cola


;; Estructuras mutables: son aquellas cuyos valores pueden cambiar durante la ejecucion del programa
;; Stores: son las estructuras en las que se almacenan los valores de las variables o referencias a objetos

;; Alcance estatico: en un programa con alcance estatico, la asignacion de variables se hace en tiempo de compilacion,
;; esto significa que se puede determinar a que variable se refiere un identificador en una parte especifica del programa


;; Alcance dinamico: en un programa con alcance dinamico, la asignacion de variables se hace en tiempo de ejecucion,
;; basandose en el estado actual del programa


;; Perdida de alcance estatico: esto ocurre cuando se pasa una funcion como argumento a otra funcion, y luego se llama a la funcion pasada como argumento
;; esto puede ocurrir en lenguajes con alcance estatico y funciones de primera clase
;; tambien puede ocurrir cuando se ocupan stores mutables, porque se puede modificar el valor de una variable en cualquier parte del programa
;; lo que hace que sea dificil determinar a que variable se refiere un identificador en una parte especifica del programa

;; que un lenguaje sea puro implica que no tiene efectos secundarios, y que no tiene perdida de alcance estatico

;; los efectos secundarios son cambios en el estado del programa que no son el resultado de una funcion

;; bucles while proporcionado como una funcion implica control de flujo, que es un efecto secundario

;; ahora, si se intenta implementar mutaciones en un lenguaje puro con whiles, se puede perder el alcance estatico que previamente se tenia con el while



(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

(define (tail-fact accum n)
  (if (zero? n)
      accum
      (tail-fact (* n accum) (- n 1))))

;(fact 6)
;(tail-fact 1 6)

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;(fib 15)

(define (tail-fib acc1 acc2 n)
  (if (zero? n)
      acc1
      (tail-fib acc2 (+ acc1 acc2) (- n 1))))

;(tail-fib 0 1 15)


(deftype TailCall
  (call rest)
  (done val))

(define (even-tr n)
  (match n
    [0 (done #t)]
    [1 (done #f)]
    [else (call (λ () (odd-tr (- n 1))))]))

(define (odd-tr n)
  (match n
    [0 (done #f)]
    [1 (done #t)]
    [else (call (λ () (even-tr (- n 1))))]))

(define (result tc)
  (match tc
    [(done val) val]
    [(call rest) (result (rest))]))


(result (odd-tr 100))




;; pregunta 1 control 2 2023 1
; a) indique como soportar recursion en un lenguaje que posee funciones de:
  ; i) primer order sin funciones de primera clase
  ; ii) primera clase con alcance dinamico
  ; III) primera clase con alcance estatico

;; I) al no poder pasarse funciones como argumentos ni devolverlas como resultado,
;; se puede implementar la recursion con una funcion auxiliar que reciba como argumentos

;; ejemplo:
(define (fact n)
  (define (fact-aux n acc)
    (if (zero? n)
        acc
        (fact-aux (- n 1) (* n acc))))
  (fact-aux n 1))


;; II) al tener alcance dinamico, se puede implementar la recursion pasando funciones como argumentos
;; y devolviendolas como resultado

;; ejemplo:
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

;; aqui fact se llama a si misma con un argumento diferente

;; III) al tener alcance estatico, pero con funciones de primera clase, se puede implementar la recursion
;; pasando funciones como argumentos y devolviendolas como resultado, igual que en el caso anterior






(tail-fact 1 500000)


