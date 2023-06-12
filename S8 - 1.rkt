#lang play
(print-only-errors #t)

#| 
<Expr> ::= <number>
         | {+ <Expr> <Expr>}
         | {- <Expr> <Expr>}
         | <symbol>
         | {with {<symbol> <Expr>} <Expr>} ; azucar sintactico
         | {<Expr> <Expr>}
         ;; importante: cualquier expr en posicion de funcion
         | {fun {<symbol>} <Expr>}
         ;; funciones anonimas / 1a clase
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id x)
  (app fun-expr arg-expr)
  (fun param body))

; parse : s-expr -> Expr
(define (parse s)
  (match s
    [(? number?) (num s)]
    [(? symbol?) (id s)]
    [(list '+ e1 e2) (add (parse e1) (parse e2))]
    [(list '- e1 e2) (sub (parse e1) (parse e2))]
    [(list 'with (list name ne) body) ;; with --> app/fun
     (app (fun name (parse body))
          (parse ne))]
    [(list 'fun (list x) e) (fun x (parse e))]
    [(list f e) (app (parse f) (parse e))]))

;; Environment Abstract Data Type (ADT)
;; Env
;; empty-env : -> Env
;; extend-env : Id Val Env -> Env
;; lookup-env : Id Env -> Val o Error
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env mtEnv)

(define extend-env aEnv)

(define (lookup-env x env)
  (match env
    [(mtEnv) (error "free identifier:" x)]
    [(aEnv y v next)
     (if (equal? x y)
         v
         (lookup-env x next))]))

; posibles valores
(deftype Val
  (numV n)
  (closureV param body env)
  (exprV expr env)) ;; promesa de evaluar expr en env si necesario

;; interp : Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(fun p b) (closureV p b env)]
    [(add l r) (numV+ (strict (interp l env))
                      (strict (interp r env)))]
    [(sub l r) (numV- (strict (interp l env))
                      (strict (interp r env)))]
    [(id x) (lookup-env x env)] 
    [(app f e)
     (def (closureV arg body fenv) (strict (interp f env)))
     (interp body
             (extend-env arg
                         ;(interp e env) ;; evaluacion temprana
                         (exprV e env) ;; evaluación perezosa
                         fenv))])) 

#|
{with {f {fun {x} {+ x x}}
{with {y 10}
 {f {+ y y}}      [x --> <{+ y y}, [y -> 10]> ]
|#

(define (numV+ n1 n2) 
  (numV (+ (numV-n n1) (numV-n n2))))

(define (numV- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))

;; Val: numV / closureV / exprV
;; VdV: numV / closureV
; strict : Val -> VdV
(define (strict v)
  (match v
    [(exprV expr env) (strict (interp expr env))]
    [_ v]))


; top-level run
(define (run prog) 
  (match (strict (interp (parse prog) (empty-env)))
    [(numV res) res]
    [(closureV p b e) (closureV p b e)]))


 
; tests
(test (run 10) 10)
(test (run '{+ 10 {- 20 5}}) 25)
(test
 (run '{with {x 1} {+ x x}})
 2)
(test (run '{with {x 10}
                  {+ {with {x x}
                           x}
                     x}})
      20)

(test/exn (run '{+ {with {x 1}
                         x}
                   x})
          "free identifier")

(test/exn (run '{with {y 10}
                  {+ {with {x x}
                           x}
                     x}})
          "free identifier")

(test (run '{with {x {+ 10 20}}
               {with {y {+ x 2}}
                  {+ x y}}})
      62)


(test (run '{{fun {a} {+ a a}} 1})
      2)

(test (run '{with {f {fun {a} {+ a a}}}
               {f 1}})
      2)

(test (run '{with {f {fun {a} {+ a a}}}
                  {with {f 4}
                        f}})
      4)

(test (run '{with {add1 {fun {n} {+ n 1}}}
              {with {f {fun {x} {+ x x}}}
                 {with {x {+ 2 2}}
                    {add1 {f x}}}}})
      9)

(test (run '{with {addn {fun {n}
                             {fun {m}
                                  {+ n m}}}}
                  {{addn 10} 20}})
      30)

(test (run '{with {applyTo1 {fun {f} {f 1}}}
                  {applyTo1 {with {delta 10}
                               {fun {a} {+ a delta}}}}})
      11)


;; no tenemos recursion (ojo: con scope dinamico, sí!)
(test/exn (run '{with {f {fun {x} {f x}}}
                   {f 0}})
          "free")


; este test permite determinar si el interp usa
; evaluación antes de substituir o no
; (evaluación "temprana" vs. "perezosa")
;(test/exn (run '{with {x z} 1}) "free") ; temprana
(test (run '{with {x z} 1}) 1) ; perezosa


; Apuntes de la clase Semana 8 - 2
; Estrategias de evaluacion:
; - Temprana
; Ejemplo:
; {f {+ 1 2}}
; {f 3}

; - Perezosa (lazy)
; Ejemplo:
; lazy val x = foo()
; la evaluacion perezosa no implica alcance dinamico
; se tienen que evaluar en algun momento las promesas, para ello se usa el strict


; Que es una funcion top level?
; es una funcion que no esta dentro de otra funcion (no es una funcion anidada)


; Puntos de Strictness
; 1. top level: Las expresiones que no estan dentro de una funcion, es decir, las expresiones que se evaluan al principio
; 2. primitivas: las primitivas de los lenguajes (ej: +, -, etc), los argumentos de las primitivas se evaluan antes de que la primitiva se evalúe
; 3. flujos de control (posicion de fun en app, condicional en if, while, etc) 

; Los puntos de strictness son los lugares del programa donde se produce la evaluacion estricta de las expresiones.
; La evaluacion estricta implica que todas las evaluaciones se evaluan completamente antes de que la expresion principal 
; sea evaluada.


; call by name: evaluacion perezosa sin memoizacion (perezosa 1)
; esto es que si se evalua una expresion, se evalua cada vez que se la llama
; Sed search/replace

; call by need: evaluacion perezosa con memoizacion (perezosa 2)
; esto es que si se evalua una expresion, se guarda el resultado para no tener que volver a evaluarla
; esto es util para evitar loops infinitos o para evitar evaluar x xexpresiones mas de una vez
; Haskell usa call by need (cachea los resultados de las funciones)

; call by value: evaluacion temprana
; esto es que si se evalua una expresion, se evalua una sola vez y se guarda el resultado
; esto es util para evitar loops infinitos o para evitar evaluar expresiones mas de una vez
; pero se evalua antes de la llamada a la funcion


; Scala usa los 3 tipos de evaluacion

; la ventaja de usar el call by name sobre el call by need es que el call by name no necesita
; memoria para guardar los resultados y evita loops infinitos

; para un def while(cond, body):
;    if cond:
;      body
;      while(cond, body)


; while(i < 10) (print(i) , i ++)
; para esta funcion en una evalicion call by value se evalua el cond solo una vez y se guarda en el caché,
; por lo que si el cond es True, se evalua el body pero se quedaria en un loop infinito y se imprimirá solo una vez

; en Scala el cond en vez de ser Bool es una funcion que devuelve un Bool(=> Bool) (call by name) 

; para la tarea 2 hay que hacer prints en el codigo, testear efectos de la evaluacion, haciendo los prints, pero sin
; necesariamente mostrarlos, osea guardarlos en una lista y mostrarlos al final, con scope dinamico

; cache: expr env <==> val (REFERENTIAL TRANSPARENCY) esto no se cumple siempre, si en programacion imperativa

; una funcion pura es una funcion que no tiene efectos secundarios, es decir, que no modifica el estado del programa


; en python se ocupa para memorizacion el @lru_cache(maxsize=None) (least recently used cache)
; lo cual es una funcion que recibe una funcion y devuelve una funcion, que es la misma funcion pero con memoizacion,
; solo sirve para funciones puras, no para funciones con efectos secundarios

;; vamos a modificar el lenguaje para que acepte evaluacion perezosa (call by need)

; strict nos muestra como es que se esta evaluando el programa


(define (strict v)
  (match v
    [(exprV expr env cache) 
    (def content (unbox cache))
    (if (not content)
        (let ([val (strict (interp expr env))])
          (set-box! val cache)
          val))
          (begin (printf "using cache value") content)
          content]))

;; definimos set cache, guardamos el valor v en el cache, con box
(define (set-cache v cache)
  (match cache
    [(exprV expr env) (exprV expr env (box v))]))


; Mutacion: es la modificacion del estado del programa (cambiar el valor de una variable, etc)
; 