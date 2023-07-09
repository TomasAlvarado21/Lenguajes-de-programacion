#lang play

#|
<expr> ::= <num>
         | <id>
         | <bool>
         | {if <expr> <expr> <expr>}
         | {+ <expr> <expr>}
         | {< <expr> <expr>}
         | {* <expr> <expr>}
         | {= <expr> <expr>}
         | {- <expr> <expr>}
         | {and <expr> <expr>}
         | {or <expr> <expr>}
         | {not <expr> <expr>}         
         | {begin <expr> <expr>}
         | {with {<def>+} <expr>}

<def>    ::= {<id> <expr>}


;EXTENSION PARA CLASE Y OBJETOS
<expr> ::= ... (expresiones del lenguage entregado) ...
        | {class {<id>*} <method>*}
        | {new <expr> {<expr>*}}
        | {get <expr> <id>}
        | {set <id> <expr>}
        | {-> <expr> <id> <expr>*}
        | self
 
<method> ::= {def <id> {<id>*} <expr>}
|#


(deftype Expr
  (num n)
  (bool b)
  (id s)   
  (binop f l r)
  (unop f s)
  (my-if c tb fb)  
  (begn expr1 expr2)  
  (with defs body)
  (class ids methods)
  (new expr exprs)
  (get expr id)
  (set id expr)
  (-> expr id exprs)
  (self)
  )

(deftype Def ;; Used inside with
  (my-def id expr))

;; values
(deftype Val
  (numV n)
  (boolV b)
  (closureV closure)
  (classV class)
  (objectV object)
  (voidV void)
  (selfV self)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Environment abstract data type
 
empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env 


representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv hash env)) 

(def empty-env (mtEnv))

#|
env-lookup:: Sym Env -> Val
Busca un símbolo en el ambiente, retornando su valor asociado.
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv hash rest)
     (if (hash-has-key? hash x)
         (hash-ref hash x)
         (env-lookup x rest))]))

#|
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociando los símbolos a sus valores.
|#
(define (multi-extend-env ids exprs env)
  (if (= (length ids) (length exprs))
      (aEnv (make-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths")))

#|
extend-frame-env!:: Sym Val Env -> Void
Agrega un nuevo par (Sym, Val) al ambiente usando mutación.
Este método no crea un nuevo ambiente.
|#
(define (extend-frame-env! id val env)
  (match env
    [(mtEnv) (aEnv (make-hash (list (cons id val))) env)]
    [(aEnv h rEnv) (let* ([l (hash->list h)]
                          [la (append l (list (cons id val)))])
                     (set-aEnv-hash! env (make-hash la)))]))

;; objectV-env :: Object Env
(define (objectV-env object env)
  (match object
    [(objectV o) o]))

;; classV-env :: Class Env
(define (classV-env clase env)
  (match clase
    [(classV c) c]))

;; classV-ids :: Class -> List<id>
;; Retorna la lista de ids de la clase
(define (classV-ids clase)
  (match clase
    [(classV c) c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]    
    [(? boolean?) (bool s-expr)]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '< l r) (binop < (parse l) (parse r))]
    [(list '= l r) (binop = (parse l) (parse r))]    
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l) (parse r))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l) (parse r))]
    [(list 'not b) (unop not (parse b))]
    [(list 'if c t f) (my-if (parse c)
                             (parse t)
                             (parse f))]
    [(list 'begin e1 e2) (begn (parse e1) (parse e2))]  

    [(list 'with (list e ...)  b) (with (map parse-def e) (parse b))]

    [(list 'class (list ids ...) (list methods ...)) (class ids (map parse-def methods))]

    [(list 'new e ...) (new (map parse e))]

    [(list 'get e id) (get (parse e) id)]

    [(list 'set id e) (set id (parse e))]

    [(list '-> e id exprs ...) (-> (parse e) id (map parse exprs))]

    [(list 'self) self]
    [_ (error "not yet implemented")]))


;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list id b) (my-def id (parse b))]))

;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]    
    [(bool b) (boolV b)]    
    [(binop f l r) (make-val (f (open-val (interp l env))
                                (open-val (interp r env))))]
    [(unop f s) (make-val (f (open-val (interp s env))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env))
     (if cnd
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]        
    [(begn expr1 expr2) (begin 
                          (interp expr1 env)
                          (interp expr2 env))]
    [(with defs body)
     (let* ([new-env (multi-extend-env '() '() env)])
       (for-each (λ (x)
                   (let ([in-def (interp-def x new-env)])
                     (extend-frame-env! (car in-def) (cdr in-def) new-env)
                     #t)) defs)       
       (interp body new-env))]
    [(class ids methods) (classV ids methods)]
    [(list 'new e exprs ...) ; Utilizar la sintaxis (list 'new e exprs ...) para el patrón
     (let* ([class (interp (parse e) env)]
            [class-env (classV-env class)]
            [object-env (make-hash (map cons (classV-ids class) (map (λ (x) (interp x env)) exprs)))])
       (set-aEnv-env! class-env object-env)
       (objectV class object-env))]
    [(get e id)
     (let* ([object (interp e env)]
            [object-env (objectV-env object)])
       (env-lookup id object-env))]
    [(set id e)
     (let* ([object (env-lookup 'self env)]
            [object-env (objectV-env object)])
       (hash-set! object-env id (interp e env))
       voidV)]
    [(list '-> e id exprs ...) ; Utilizar la sintaxis (list '-> e id exprs ...) para el patrón
     (let* ([object (interp (parse e) env)]
            [object-env (objectV-env object)]
            [method (env-lookup id object-env)])
       (apply method object (map (λ (x) (interp x env)) exprs)))]

    [_ (error "not yet implemented")]))

;; open-val :: Val -> Scheme Value
;; Extrae el valor de un Val
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    [(voidV void) void]
    [(classV _) (error "class value")]
    [(objectV env) (error "constructor not found exception")]
    [_ (error "not yet implemented")]
    ))

;; make-val :: Scheme Value -> Val
;; Construye un Val a partir de un scheme value
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    [(? void?) voidV]
    [_ (error "not yet implemented")]
    ))

;; interp-def :: Def, Env -> Expr
;; Evalúa una definición y retorna un par con el id y el valor
(define (interp-def a-def env)
  (match a-def
    [(my-def id body) (cons id (interp body env))]))

;; run :: s-expr -> Val
;; Evalúa una expresión de MiniScheme y retorna un Val
(define (run s-expr)
  (interp (parse s-expr) empty-env))

#|
run-val:: s-expr -> Scheme-Val + Val
Versión alternativa de run, que retorna valores de scheme para primitivas y
valores de MiniScheme para clases y objetos
|#
(define (run-val s-expr)
  (define val (interp (parse s-expr) empty-env))
  (match val
    [(numV n) n]
    [(boolV b) b]
    [x x]))