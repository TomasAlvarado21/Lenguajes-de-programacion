#lang play

(require "env.rkt")

;; Parte 1

#|
<prog>   ::= {<fundef>* <expr>}

<fundef> ::= {define {<id> <id>*} <expr>}

<expr>   ::= <num>
           | <id>
           | <bool>
           | {cons <expr> <expr>}
           | {add1 <expr>}
           | {+ <expr> <expr>}
           | {< <expr> <expr>}
           | {= <expr> <expr>}
           | {! <expr>}
           | {&& <expr> <expr>}
           | {|| <expr> <expr>}
           | {fst <expr>}
           | {snd <expr>}
           | {if <expr> <expr> <expr>}
           | {with {{<id> <expr>}*} <expr>}
           | {<id> <expr>*}
|#
(deftype Prog
  (prog fundefs main))

(deftype Fundef
  (fundef name arg body))

(deftype Expr
  (num n)
  (id x)
  (bool b)
  (cons0 expr expr2)
  (add1 expr)
  (add l r)
  (lt l r)
  (eq l r)
  (neq expr)
  (and0 l r)
  (or0 l r)
  (fst expr)
  (snd expr)
  (if0 con t f)
  (with list-expr body)
  (app f-name body)
  )

;; tipo inductivo para los valores del lenguaje
(deftype Val
  (numV n)
  (boolV b)
  (pairV lV rV)
  (closureV id body env))

(define (numV-val val)
  (match val
    [(numV n) n]))

(define (pairV-val par)
  (match par
    [(pairV e1 e2) (cons e1 e2)]))

(define (boolV-bool val)
  (match val
    [(boolV b) b]))

(define (pairV-list val)
  (match val
    [(pairV l r) (list l r)]))

;; parse :: ...
(define (parse sp)
  (match sp
    [(list ds ... e) (prog (map parse-fundef ds) (parse-expr e))] ;; ds es la lista de definiciones, e es la expresion principal
    ))

;; parse-expr :: ...
(define (parse-expr se)
  (match se
    [(? number?) (num se)]
    [(? symbol?) (id se)]
    [(? boolean?) (bool se)]
    [(list 'add1 e1) (add1 (parse-expr e1))] 
    [(list '+ e1 e2) (add (parse-expr e1) (parse-expr e2))]
    [(list '< e1 e2) (lt (parse-expr e1) (parse-expr e2))]
    [(list '= e1 e2) (eq (parse-expr e1) (parse-expr e2))]
    [(list '! e1) (neq (parse-expr e1))]
    [(list '&& e1 e2) (and0 (parse-expr e1) (parse-expr e2))]
    [(list '|| e1 e2) (or0 (parse-expr e1) (parse-expr e2))]
    [(list 'fst e1) (fst (parse-expr e1))]
    [(list 'snd e1) (snd (parse-expr e1))]
    [(list 'if con e1 e2) (if0 (parse-expr con) (parse-expr e1)(parse-expr e2))]
    
    [(list 'with lista body) (with (map (λ (par) (list (car par) (parse-expr (car (cdr par))))) lista) (parse-expr body))]

    [(list 'cons e1 e2) (cons0 (parse-expr e1) (parse-expr e2))]
    [(list f a ...) (app f (map parse-expr a))]
    [_ (error "not yet implemented")]
    ))

;; parse-fundef :: ...
(define (parse-fundef sf)
  (match sf
    [(list 'define (list name arg ...) body) (fundef name arg (parse-expr body))]))




;; interp :: Expr → Env → list Fundef → Val
(define (interp e env funs)
  (match e
    [(num n) (numV n)]
    [(id x) (env-lookup x env)]
    [(bool b) (boolV b)]
    [(cons0 l r) (pairV (interp l env funs) (interp r env funs))]
    ;[(cons l r) (pairV (interp l env funs) (interp r env funs))]
    [(lt l r) (boolV (< (numV-val (interp l env funs)) (numV-val (interp r env funs))))]
    [(eq e1 e2) (= (interp e1 env funs) (interp e2 env funs))]
    [(add e1 e2) (numV (+ (numV-val(interp e1 env funs)) (numV-val (interp e2 env funs))))]
    [(neq e1) (not (interp e1 env funs))]
    [(and0 e1 e2) (and (interp e1 env funs) (interp e2 env funs))]
    [(or0 e1 e2) (or (interp e1 env funs) (interp e2 env funs))]
    [(fst e1) (car (pairV-val (interp e1 env funs)))]
    [(snd e1) (cdr (pairV-val (interp e1 env funs)))]
    [(if0 con e1 e2) (if (interp con env funs) (interp e1 env funs) (interp e2 env funs))]
    [(add1 e1) (+ (interp e1 env funs) 1)]


    [(with lista body)
     (interp body
             (foldr (λ (par env) (act par env funs)) env lista)
             funs)]
     

    
     ;(interp body funs (extend-env (map first lista) (map (lambda (e) (interp e env funs)) (map second lista)) env))]
    [(app f val-lista)
     (def (fundef _ param-list fbody) (lookup-fundef f funs))
     (interp fbody
             (extend-env-list funs param-list env)
             funs)]


         
    ;[_ (error "not yet implemented")] ;; usar esto https://users.dcc.uchile.cl/~etanter/play-interps/Functions_with_Environments.html
    ))
;vamos a hacer un interprete auxiliar para el with 
(define (act par env funs)
  (extend-env (car par) (interp (car (cdr par)) env funs) env))



; vamos a hacer una funcion auxiliar para ir tomando el primer elemento de dos listas y que
; con esto guardemos ese par en el env
(define (extend-env-list list1 list2 env)
  (extend-env-list (cdr list1) (cdr list2)
                           (extend-env (car list1) (car (numV-val list2)) env)))



;; lookup-fundef :: sym Listof(FunDef) -> FunDef
(define (lookup-fundef f funs)
  (match funs
    ['() (error 'lookup-fundef "function not found: ~a" f)]
    [(cons (fundef fn args body) rest)
     (if (symbol=? fn f)
         (fundef fn args body)
         (lookup-fundef f rest))]))


(define (run sp)
  (def (prog funs main) (parse sp))
  (interp main empty-env funs))




