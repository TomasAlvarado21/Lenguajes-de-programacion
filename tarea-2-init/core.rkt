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
         | {mfun {<id>} <CL>}
|#
(deftype CL
  (num n)
  (add l r)
  (if0 c t f)
  (with x e b)
  (id s)
  (app fun-expr arg-expr)
  (fun id body)
  (printn e)
  (mfun id body))


;; parse :: s-expr -> CL
(define (parse-cl s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse-cl l) (parse-cl r))]
    [(list 'if0 c t f) (if0 (parse-cl c)
                            (parse-cl t)
                            (parse-cl f))]
    [(list 'with (list x e) b)
     (app (fun x (parse-cl b)) (parse-cl e))]
    [(list 'fun (list x) b) (fun x (parse-cl b))]
    [(list 'printn e) (printn (parse-cl e))]
    [(list f a) (app (parse-cl f) (parse-cl a))]
    [(list 'mfun (list x) b) (mfun x (parse-cl b))]))


;; values
(deftype Val
  (numV n)
  (closV id body env)
  (mcolsV id body env))
  
(define my-table (make-hash))

(define param (make-parameter println))
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
      ((param) n)
      (numV n)]
    [(app fun-expr arg-expr)
     (match (interp fun-expr env)
       [(closV id body fenv)
        (interp body
                (extend-env id
                            (interp arg-expr env)
                            fenv))])]
      ;; mfun y mcolsV, aqui se ocupa la tabla de hash, si no esta el id en la tabla se agrega
      ;; si esta se imprime el valor de la tabla para no tener que calcularlo de nuevo
      [(mfun id body) 
        (if (hash-has-key? my-table id)
            (hash-ref my-table id)
            (begin
              (def (mcolsV id body env) (mcolsV id body env))
              (hash-set! my-table id (mcolsV id body env))
              (hash-ref my-table id)))]))



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


;; Definición del nuevo tipo de dato Result
(deftype Result 
  (result val log))



;; Función de impresión personalizada que registra las impresiones en el log
;; se tiene que guardar los valores en una caja para que no se pierdan, es la caja log local

(define (interp-p expr)
  (define log (box '()))

  (define (println-g x)
    (set-box! log (cons x (unbox log))))

  (parameterize ([param println-g])
    (define val (interp expr empty-env))
    (result val (unbox log))))


        

; dame un test de println que use interp-p
(test (interp-p (parse-cl '{printn 10}))
      (result (numV 10) (list 10)))

(test (interp-p (parse-cl '{printn {+ 1 2}}))
      (result (numV 3) (list 3)))


;; dame el test de la función interp-p que use la función mfun con el ejemplo de arriba
(test (interp-p (parse-cl '{with {addn {mfun {n} {mfun {m} {+ {printn n} m}}}} {+ {{addn 10} 4} {{addn 10} 4}}}))
      (result (numV 28) (list 10 4 10 4 28)))