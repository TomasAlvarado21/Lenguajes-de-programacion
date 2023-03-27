#lang play

;1. Conceptos:
;; (a) ¿Cuál es la diferencia entre (cons 'a 'b) y (list 'a 'b)?
; El primero es un par, el segundo es una lista

;; ¿Cómo se representaría el segundo con notación de pares?
(cons 'a (cons 'b '()))


;; (b) ¿Cuál es la notación de lista equivalente a '((a b) c) ?
(list (list 'a 'b) 'c)


;; (c) Dado
(define l (list '(a b c) '(d e f) '(g h i)))
;; ¿Como se accesaria el elemento 'c y el 'e en l? Por ejemplo, 'b es accesado por (car (cdr (car l)))
;; acceder a 'c
(car(cdr(cdr (car l ))))

;; acceder a 'e
(car (cdr (car (cdr l))))


;; (d) Usando solo cons, la lista vacia y simbolos, muestre como construir las siguientes expresiones
;; '(c), '(a b), '((a b) (c))

;; '(c)
(cons 'c '() )

;; '(a b)
(cons 'a (cons 'b '()))

;; '((a b) (c))
(cons (cons 'a (cons 'b '())) (cons 'c '()))

;; 2. Defina la función pair-add1 p que recibe un par de números y retorna un nuevo par
;; dónde los dos elementos fueron incrementados en 1.

;; pair-add1 :: Pair (Num, Num) -> Par (Num,Num)
(define (pair-add1 x) (cons (+ (car x) 1)
                            (+ (cdr x) 1)))


;(b) Usted tiene un monedero. El monedero solo puede contener monedas de 50, 100
; y 500 pesos. Defina la función sums-coins que recibe 3 enteros representando la
; cantidad de monedas de 50, 100 y 500 respectivamente Ly retorna la cantidad
; de dinero total que hay en el monedero
(define (sums-coins L C Q)
  (+ (* L 50) (* C 100) (* Q 500)))

;(c) Defina la función tax, que recibe como argumento el sueldo bruto y retorna el
;impuesto a pagar. Para un sueldo menor de $500,000 el impuesto es de 0 %,
;entre $500,000 y $750,000 el impuesto es de %15 y para más o igual a $750,000
;es de %28
(define (tax sueldo)
  (cond [(< sueldo 500000) 0]
        [(< sueldo 750001) (* 0.15 sueldo)]
        [else (* 0.28 sueldo)]))


;(d) Defina la función netpay que recibe la cantidad de horas trabajadas en un mes
;y retorna el sueldo líquido del trabajador, asuma que el pago por hora es de
;$5,000
(define (netpay h)
  (let [(sueldo (* h 5000))]
    (- sueldo (tax sueldo))))

;(e) Implemente la función quicksort que recibe una lista y retorne la lista ordenada
;en forma ascendente. Tome siempre como pivote el primer elemento, Ejemplo:
;> (quicksort '(3 2 9 1))
;'(1 2 3 9)
(define (qsort l)
  (if (cons? l)
      (append (qsort (filter (lambda (x) (< x (car l))) (cdr l)))
              (list (car l))
              (qsort (filter (lambda (x) (>= x (car l))) (cdr l))))
      '()))
(qsort '(0 32 5 64 9 7 4))

;; 3. Defina las siguientes funcionalidades utilizando map, foldl, or filter:
;;(a) Dado una lista de enteros, retorne una lista con los elementos incrementados en uno.

;; add1-all :: List of (Num) -> Listof (Num)
(define (add1-all l)
 (map add1 l))

(define (add1-all2 l)
  (if (cons? l)
       (cons (add1 (car l)) (add1-all2 (cdr l)))
       '()))


;;(b) Dado una lista de strings, retorne una lista con la longitud de cada cadena.
;; lens :: List of (string) -> List of (Num)
(define (lens l)
  (map string-length l))


;;(c) Dado una lista de enteros, retorne la suma de todos su elementos.
;; sums :: List of (Num) -> Num
(define (sums l)
  (foldl + 0 l))

;;(d) Dado una lista de string, retorne el string resultante de la concatenacion de todas
;;las cadenas de la lista.
;; append-all :: List of (String) -> String
(define (append-all l)
  (foldl string-append "" l))


;;(e) Dado un lista de enteros, retorne la lista de todos los elementos mayores que cero.
;; mayores-que-cero :: List of (Int) -> List of (Int)
(define (mayores-que-cero l)
  (filter (lambda (x)(< 0 x)) l))


