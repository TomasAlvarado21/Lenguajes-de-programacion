#lang play

#|
<expr> :: = (num <num>)
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (mult <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | (id <id>)
        | (fun <sym> <expr>)
        | (app <expr> <expr>)
|#

(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (mult l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f-name f-arg))

;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mult (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list 'with (list x e) b) #:when (symbol? x)
                               (app (fun x (parse b)) (parse e))]))

#|
(deftype Env
  (mtEnv)
  (aEnv i v env))

(define empty-env mtEnv)

(define extend-env aEnv)

(define (env-lookup x env)
  (match env
    [(mtEnv) (error "env-lookup: Identificador libre: ~a" x)]
    [(aEnv i v e)
     (if (equal? x i)
         v
         (env-lookup x e))]))
|#

(define (empty-env) (λ (id) (error "free identifier: ~a" id)))

(define (env-lookup id env) (env id))

(define (extend-env new-id value env)
  (λ (id)
    (if (symbol=? id new-id)
        value
        (env id))))

#|
(deftype Value
  (numV n)  
  (closureV f))

(define (num+ n1 n2)
  (def (numV v1) n1)
  (def (numV v2) n2)
  (numV (+ v1 v2)))

(define (num- n1 n2)
  (def (numV v1) n1)
  (def (numV v2) n2)
  (numV (- v1 v2)))

(define (num* n1 n2)
  (def (numV v1) n1)
  (def (numV v2) n2)
  (numV (* v1 v2)))

(define (num-zero? n)
  (def (numV v) n)
  (zero? v))
|#
    
;; eval :: Expr Env -> Value
(define (eval expr env)
  (match expr
    ;[(num n) (numV n)]
    [(num n) n]    

    ;[(fun id body) (λ (arg-val) (eval body (extend-env id arg-val env)))
    ;               (closureV fun-val)]
    [(fun id body) (λ (arg-val) (eval body (extend-env id arg-val env)))]
    
    ;[(add l r) (num+ (eval l env) (eval r env))]
    [(add l r) (+ (eval l env) (eval r env))]
    
    ;[(sub l r) (num- (eval l env) (eval r env))]
    [(sub l r) (- (eval l env) (eval r env))]
    
    ;[(mult l r) (num* (eval l env) (eval r env))]
    [(mult l r) (* (eval l env) (eval r env))]
    
    [(id x) (env-lookup x env)]
    
    ;[(if0 c t f) (if (num-zero? (eval c env))
    ;                 (eval t env)
    ;                 (eval f env))]
    [(if0 c t f) (if (zero? (eval c env))
                     (eval t env)
                     (eval f env))]
    
    ;[(app f e) (def (closureV fun-val) (eval f env))
    ;           (def arg-val (eval e env)) ;; si aqui volviesemos a extender el ambiente, esto implicaria que el alcance de las variables es dinamico
    ;           (fun-val arg-val)]
    [(app f e) ((eval f env) (eval e env))]

    ))

(define (run e)
  (eval (parse e) (empty-env)))

(define p1 (run '{with {f {fun {n} {+ n 1}}} {f 10}}))



;; tipos de interprete: 
;; - Sintactico:interpreta todo el comportamiento de manera explicita
;; - meta interprete: se apoya en el lenguaje base 
;; - meta circular: es el cual cuando el lenguaje interpretado y el lenguaje base son el mismo