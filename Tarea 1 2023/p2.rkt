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
  (pairV lV rV))

;; numV-val :: numV → num
;; esta funcion recibe un numV y devuelve un numero
(define (numV-val val)
  (match val
    [(numV n) n]))

;; pairV-val :: pairV → pair
;; esta funcion recibe un pairV y devuelve un pair
(define (pairV-val par)
  (match par
    [(pairV e1 e2) (cons e1 e2)]))

;; boolV-bool :: boolV → bool
;; esta funcion recibe un boolV y devuelve un bool
(define (boolV-bool val)
  (match val
    [(boolV b) b]))

;; pairV-list :: pairV → list
;; esta funcion recibe un pairV y devuelve una lista
(define (pairV-list val)
  (match val
    [(pairV l r) (list l r)]))

;; parse :: s-expr → Prog
;; esta funcion recibe una lista de s-expr y devuelve un Prog
(define (parse sp)
  (match sp
    [(list ds ... e) (prog (map parse-fundef ds) (parse-expr e))] ;; ds es la lista de definiciones, e es la expresion principal
    ))

;; parse-expr :: s-expr → Expr
;; esta funcion recibe una expresion y devuelve un Expr

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
    [(list 'with ': lista body) (with (map (λ (par) (list (car par) (parse-expr (car (cdr par))))) lista) (parse-expr body))]
    [(list 'with lista body) (with (map (λ (par) (list (car par) (parse-expr (car (cdr par))))) lista) (parse-expr body))]
    

    [(list 'cons e1 e2) (cons0 (parse-expr e1) (parse-expr e2))]
    [(list f a ...) (app f (map parse-expr a))]
    [_ (error "not yet implemented")]
    ))

;; parse-fundef :: s-fundef → fundef
;; esta funcion recibe una definicion de funcion y devuelve un fundef
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
    [(eq e1 e2) (= (numV-val(interp e1 env funs)) (numV-val (interp e2 env funs)))]
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
     ;ahora para el app tenemos que hacer lo mismo que con el with
     ; usamos el extend-env-list para extender el env con los argumentos
    [(app f arg-expr)
     (def (fundef _ farg fbody) (lookup-fundef f funs))
      (interp fbody (extend-env-list (map (λ (arg) (interp arg env funs)) arg-expr) farg env) funs)]
    
  
         
    ;[_ (error "not yet implemented")] ;; usar esto https://users.dcc.uchile.cl/~etanter/play-interps/Functions_with_Environments.html
    ))

; extend-env :: sym Val Env → Env
;vamos a hacer un interprete auxiliar para el with 
(define (act par env funs)
  (extend-env (car par) (interp (car (cdr par)) env funs) env))

; extend-env-list :: Listof(sym) Listof(Val) Env → Env
; hacemos un extend env para el app usando la misma idea que con el with
; tenemos una lista con los argumentos y otra con los parametros y nos da el env
(define (extend-env-list args params env)
  (match args
    ['() env]
    [(cons arg rest) (extend-env-list rest (cdr params) (extend-env (car params) arg env))]))


;; lookup-fundef :: sym Listof(FunDef) -> FunDef
;; busca una funcion en la lista de funciones
(define (lookup-fundef f funs)
  (match funs
    ['() (error 'lookup-fundef "function not found: ~a" f)]
    [(cons (fundef fn args body) rest)
     (if (symbol=? fn f)
         (fundef fn args body)
         (lookup-fundef f rest))]))


;; run :: String → Val
;; corre el programa
(define (run sp)
  (def (prog funs main) (parse sp))
  (interp main empty-env funs))


#|
<fundef> ::= {define {<id> {arg}*} [: <type>] <expr>}

<arg>    ::= {<id> : <type>}

<expr>   ::= ... | {with { {<id> [: <type>] <expr>}* } <expr>}  ; los otros casos no cambian

<type>   ::= Num | Bool | {Pair <type> <type>}
|#

(deftype Type
  (numT)
  (boolT)
  (pairT lT rT))

;; typecheck-expr :: Expr → Type
;; hay que ver si el tipo de la expresion es correcto, sino da error, usando los respectivos typecheck-<tipo>
(define (typecheck-expr e)
  (match e
    [(num n) (typecheck-bool n)]
    [(id x) (typecheck-num x)]
    [(bool b) (typecheck-bool b)]
    [(cons0 l r) (pairT (typecheck-expr l) (typecheck-expr r))]
    [(lt l r) (typecheck-bool l)]
    [(eq e1 e2) (typecheck-bool e1)]
    [(add e1 e2) (typecheck-num e1)]
    [(neq e1) (typecheck-bool e1)]
    [(and0 e1 e2) (typecheck-bool e1)]
    [(or0 e1 e2) (typecheck-bool e1)]
    [(fst e1) (typecheck-expr e1)]
    [(snd e1) (typecheck-expr e1)]
    [(if0 con e1 e2) (typecheck-expr e1)]
    [(add1 e1) (typecheck-num e1)]
    [(with lista body) (typecheck-expr body)]
    [(app f arg-expr) (typecheck-expr f)]\
    [_ (error "not yet implemented")]
    ))

;; typecheck-fundef :: ...
(define (typecheck-fundef f)
  (match f
    [(fundef name arg body) (fundef name arg (typecheck-expr body))]
  (error "not yet implemented")))


;; haremos una funcion que reciba una expresion y nos devuelva el tipo
;; typecheck-expr :: Expr → Type
;; hay que ver si el tipo de la expresion es correcto, sino da error


(define (typecheck-bool e)
  (if (boolean? e)
      (boolT)
      (error "Runtime type error: expected Bool found Num")))

(define (typecheck-num e)
  (if (number? e)
      (numT)
      (error "Runtime type error: expected Num found Bool")))



;; typecheck :: ...
(define (typecheck p)
  (def (prog funs main) p)
  (begin
    (map typecheck-fundef funs)
    (typecheck-expr main)))

