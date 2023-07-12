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
         | {class {<id>*} <method>*}
         | {new <expr> <expr>*}
         | {get <expr> <id>}
         | {set <id> <expr>}
         | {-> <expr> <id> <expr>*}
         | self
         | null

<def>    ::= {<id> <expr>}
 
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
  (class ids metodos)
  (new clase args)
  (get clase idc)
  (set id arg)
  (-> clase idm args)
  (self)
  (null)
  )

(deftype Method
  (defm id args body))

(deftype Def
  (my-def id expr))

;; values
(deftype Val
  (numV n)
  (boolV b)
  (objV clase campos metodos)
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
    [(list 'with (list e ...)  b)
     (with (map parse-def e) (parse b))]
    [(list 'class ids metodos ...) (class ids (map methodparser metodos))]
    [(list 'new clase) (new (parse clase) '())]
    [(list 'new clase args) (new (parse clase) (map parse args))]
    [(list 'get clase idc) (get (parse clase) idc)]
    [(list 'set id arg) (set id (parse arg))]
    [(list '-> clase idm args ...) (-> (parse clase) idm (map parse args))]
    ))

(define (methodparser x)
  (match x
    [(list 'def nombre args body) (defm nombre args (parse body))]
    )
  )

;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list id b) (my-def id (parse b))]))

;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]    
    [(bool b) (boolV b)]
    [(self) (env-lookup 'self env)]
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
       (for-each (λ(x)
                   (let ([in-def (interp-def x new-env)])
                     (extend-frame-env! (car in-def) (cdr in-def) new-env)
                     #t)) defs)       
       (interp body new-env))     
     ]

    [(class ids metodos)
     (def mt (getmetodos metodos '()))
     (def camposh (make-hash))
     (for ([i ids]) (hash-set! camposh i (null)))
     (let ([m mt])
       (letrec ([class
                    (λ (msg . arg)
                      (match msg
                        ['-crear
                         (def objeto (objV class camposh m))
                         (def construct (findf (lambda (x)
                                                 (and (equal? 'init (first x))(equal? (length (second x)) (length (car arg)))))
                                               m))
                         (if construct
                             (let ([argc (second construct)] [body (third construct)])
                               (begin
                                 (def newenvi (multi-extend-env argc (car arg) env))
                                 (extend-frame-env! 'self objeto newenvi)
                                 (interp body newenvi)
                                 objeto))
                             (if (empty? (car arg))
                                 objeto
                                 (error "error: constructor not found")))]
                        ['-get
                         (def v (dict-ref (objV-campos (first arg)) (second arg) #f))
                         (if v
                             v
                             (error 'get "field not found ~a" (second arg)))]
                        ['-set
                         (def v (dict-ref (objV-campos (first arg)) (second arg) #f))
                         (if v
                             (dict-set! (objV-campos (first arg)) (second arg) (third arg))
                             (error 'set "field not found ~a" (second arg)))]
                        ['-invoke
                         (def meto
                           (findf (lambda (x)
                                    (equal? (car x) (second arg)))
                                  m))
                         (if meto
                             (let ([argmeto (second meto)] [bodyy (third meto)])
                               (begin
                                 (def new-env (multi-extend-env argmeto (third arg) env))
                                 (extend-frame-env! 'self (car arg) new-env)
                                 (interp bodyy new-env)))
                             (error '->: "method not found: ~a" (second arg)))]))])
                             class))]

    [(new clase args) ((interp clase env) '-crear (map (lambda (x) (interp x env)) args))]
    [(set id arg) (def obj (interp (self) env))
                  ((objV-clase obj) '-set obj id (interp arg env))]
    [(get clase id) (def obj (interp clase env))
                    ((objV-clase obj) '-get obj id)]
    [(-> clase idm args) (def obj (interp clase env))
                         ((objV-clase obj) '-invoke obj idm (map (lambda (x) (interp x env)) args))]
    ))



;; getmetodos :: [Defm] -> [Defm] -> [Defm]
(define (getmetodos metodos l)
  (if (empty? metodos)
      l
  (match (car metodos)
    [(defm 'init argum cuerpo)
     (def flag (findf (lambda (x) (and (equal? 'init (first x)) (equal? (length (second x)) (length argum)))) l))
     (if flag
         (error "error: same arity constructor error")
         (getmetodos (cdr metodos) (cons (list 'init argum cuerpo) l)))
     ]
    [(defm idm argum cuerpo) (getmetodos (cdr metodos) (cons (list idm argum cuerpo) l))]
    )))



;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    ))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    ))

;; interp-def :: Def, Env -> Expr
(define (interp-def a-def env)
  (match a-def
    [(my-def id body) (cons id (interp body env))]))

;; run :: s-expr -> Val
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


