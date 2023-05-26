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
  (fun id body)
  (id s)
  (app fun-expr arg-expr)
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
    [(list 'with (list x e) b)
     (app (fun x (parse-cl b)) (parse-cl e))]
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
      (let ([n (interp e env)])
        (println-g (numV-n n))
        (result n (unbox log)))]
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
    [(result val _) val]))

    
;; run-cl :: s-expr -> number
;; interpreta una expresión y retorna el valor final
(define (run-cl s-expr)
  (interp-top (parse-cl s-expr)))

;; Result :: (struct result (val log))
;; val :: number
(deftype Result 
  (result val log))

;; log :: (listof number)
;; lista de números que se han impreso
(define log (box '()))

;; println-g :: number -> void
;; imprime un número en la consola
(define (println-g n)
  (set-box! log (cons n (unbox log))))

;; interp-p :: CL -> Result
;; interpreta una expresión y retorna el valor final
(define (interp-p expr)
  (parameterize ([log (box '())])
    (match (interp expr empty-env)
      [(result val _) (result val (reverse (unbox log)))])))

