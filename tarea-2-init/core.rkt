#lang play

(print-only-errors #t)

(require "env.rkt")

#|
<CL> ::= <num>
         | {+ <CL> <CL>}
         | {if0 <CL> <CL> <CL>}
         | {with {<sym> <CL>} <CL>}
         | <id>
         | {<CL> <CL>}
         | {fun {<sym>} <CL>}
         | {printn <CL>}
|#
(deftype CL
  (num n)
  (add l r)
  (if0 c t f)
  (with x e b)
  (id s)
  (app fun-expr arg-expr)
  (fun id body)
  (printn e))


;; parse :: s-expr -> CL
(define (parse-cl s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse-cl l) (parse-cl r))]
    [(list 'if0 c t f) (if0 (parse-cl c)
                            (parse-cl t)
                            (parse-cl f))]
    [(list 'with (list (list x e) b))
     (with x (parse-cl e) (parse-cl b))]
    [(list 'fun (list x) b) (fun x (parse-cl b))]
    [(list 'printn e) (printn (parse-cl e))]
    [(list f a) (app (parse-cl f) (parse-cl a))]))


;; values
(deftype Val
  (numV n)
  (closV id body env))

;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closV id body env)]
    [(add l r) (num+ (interp l env) (interp r env))]
    [(if0 c t f)
     (if (num-zero? (interp c env))
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]
    [(printn e) 
      (def (numV n) (interp e env))
      (println n)
      (numV n)]
    [(app fun-expr arg-expr)
     (match (interp fun-expr env)
       [(closV id body fenv)
        (interp body
                (extend-env id
                            (interp arg-expr env)
                            fenv))])]))

(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))
 
(define (num-zero? n)
  (zero? (numV-n n)))
 
;; interp-top :: CL -> number
;; interpreta una expresión y retorna el valor final
(define (interp-top expr)
  (match (interp expr empty-env)
    [(numV n) n]
    [_ 'procedure]))
    
;; run-cl :: s-expr -> number
(define (run-cl prog)
  (interp-top (parse-cl prog)))

;; tests
(test (run-cl '{with {addn {fun {n}
                          {fun {m}
                            {+ n m}}}}
                 {{addn 10} 4}})
      14)
;; ...
;; Definición del nuevo tipo de dato Result
(deftype Result 
  (result val log))

;; Creación del parámetro para la función de impresión
(define print-param (make-parameter println))

;; Función de impresión personalizada que registra las impresiones en el log
(define (println-g value)
  (let ([log (print-param)])
    (set-box! log (cons value (unbox log))))
  (print-param value))

;; Modificación de la función interp para utilizar println-g en lugar de println
(define (interp expr env)
  (match expr
    ;; Resto del código del intérprete
    [(printn e)
     (let ([val (interp e env)])
       (println-g (numV-n val))
       (result val (unbox log)))]))

;; Definición de la función interp-p que utiliza alcance dinámico y registra impresiones en un log local
(define (interp-p expr env)
  (parameterize ([print-param println-g])
    (let ([log (box '())])
      (let ([val (interp expr env)])
        (result val (unbox log))))))

;; Ejemplo de test para verificar la salida de las impresiones
(define (test-printn)
  (let ([expr '{+ 1 {printn {+ 1 2}}}])
    (let ([result (interp-p (parse-cl expr) empty-env)])
      (test (result-val result) 4)
      (test (result-log result) '(3))))))
