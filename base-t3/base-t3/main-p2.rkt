#lang play

#|
RECUERDEN INCLUIR SU CODIGO DE LA P1
|#

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
         | {new <expr> {<expr>*}}
         | {get <expr> <id>}
         | {set <id> <expr>}
         | {-> <expr> <id> <expr>*}
         | self

<def>    ::= {<id> <expr>}

<method> ::= {def <id> {<id>*} <expr>}


;EXTENSION PARA HERENCIA SIMPLE
<expr> ::= ... (todo lo anterior) ...
         ; Cambio para agregar super clase
         | {class [: <expr>] {<id>*} <method>*} 
         | {super <id> <expr>*}
|#
(deftype method
  (def id args body))

(deftype Expr
  (num n)
  (bool b)
  (id s)   
  (binop f l r)
  (unop f s)
  (my-if c tb fb)  
  (begn expr1 expr2)  
  (with defs body)
  (class super classes methods)
  (new class args)
  (get obj field)
  (set field val)
  (-> obj method args)
  (self)
  )

(deftype Def ;; Used inside with
  (my-def id expr))

;; values
(deftype Val
  (numV n)
  (boolV b)
  (objectV clase super fields methods)
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

    [(list 'class ': super ids metodos ...) 
      (class (parse super) ids (map parse-meth metodos))]
    [(list 'class ids metodos ...) (class ids 'super (map parse-meth metodos))]

    [(list 'super id exprs ...) (super id (map parse exprs))]
    

    [(list 'class ids methods ...) (class ids (map parse-meth methods))]

    [(list 'new e) (new (parse e) empty-env)]

    [(list 'new e exprs) (new (parse e) (map parse exprs))]

    [(list 'get e id) (get (parse e) id)]

    [(list 'set id e) (set id (parse e))]

    [(list '-> e id exprs ...) (-> (parse e) id (map parse exprs))]

    [_ (error "not yet implemented")]))


;; parse-meth :: s-expr -> Def
;; funcion que parsea un metodo y retorna un Def
(define (parse-meth s-expr)
  (match s-expr
    [(list 'def id (list ids ...) b) (my-def id (lambda (ids ...) (parse b)))]))



;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list id b) (my-def id (parse b))]))




; tenemos que verificar estos errores:
; La invocación de set fuera de un método debe lanzar el error “set outside method exception”.
; La invocación de self fuera de un método debe lanzar el error “self outside method exception”.
; El acceso a un campo inexistente de un objeto debe arrojar el error “field not found exception”.
; El acceso a un campo no inicializado debe arrojar el error “field not initialized exception”.
; La invocación de un método inexistente debe lanzar el error “method not found exception”.
; La creación de un objeto con un número invalido de argumentos debe lanzar el error “constructor not found exception”.
; Al crear una clase con 2 o más constructores de igual aridad, debe lanzar el error “same arity constructor exception”


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
    ;; parte que tengo que modificar yo
    ;; aqui vere los errores:
    [(class ids methods)
     (let ([new-env (multi-extend-env '() '() env)])
       (for-each (λ (x)
                   (let ([in-def (interp-def x new-env)])
                     (extend-frame-env! (car in-def) (cdr in-def) new-env)
                     #t)) methods)
       (classV (make-classV ids methods)))]


    [(new clase super args)
      (def super-aux 
        (if (eq? super 'super)
            'super)
            ((interp super env) '-crear '()))
     (let ([class (interp clase env)])
       (if (val-class? class)
           (let ([constructor class])
             (if constructor
                 (let ([args-interp (map (λ (x) (interp x env)) args)])
                  class)
                 (error "error: constructor not found")))
           (error "new: class not found")))]
    [(set id arg)
     (let ([obj (interp (self) env)])
       (if (val-obj? obj)
           (let ([clase (objV-clase obj)])
             (if (class '-set)
                 ((class '-set) obj id (interp arg env))
                 (error "set: field not found")))
           (error "set: invalid object")))]
    [(get clase id)
     (let ([obj (interp clase env)])
       (if (val-obj? obj)
           (let ([clase (objV-clase obj)])
             (if (class '-get)
                 ((class '-get) obj id)
                 (error "get: field not found")))
           (error "get: invalid object")))]
    [(-> clase idm args)
     (let ([obj (interp clase env)])
       (if (val-obj? obj)
            (let ([clase (objV-clase obj)])	
              (if (class '-invoke)
                  ((class '-invoke) obj idm (map (λ (x) (interp x env)) args))
                  (error "->: method not found")))
           (error "->: invalid object")))]))

;; getMeth :: List<Def> -> Env -> Env
;; funcion que retorna un env con los metodos de la clase
;; si el metodo esta repetido, tira error "same arity constructor exception"
(define (getMeth metodos l)
  (if (empty? metodos)
      l
      (let ([met (first metodos)])
        (if (member (first met) l)
            (error "same arity constructor exception")
            (getMeth (rest metodos) (cons (first met) l))))))

;; make-classV :: List<Id> List<Def> -> ClassV
;; funcion que retorna un ClassV
(define (make-classV ids metodos)
  (classV (getMeth metodos '())))



;; val-obj? :: Val -> Boolean
;; funcion que retorna true si el valor es un objeto
(define (val-obj? v)
  (match v
    [(objV clase campos metodos) #t]
    [else #f]))    

;; objV-clase :: Val -> Val
;; funcion que retorna la clase de un objeto
(define (objV-clase v)
  (match v
    [(objV clase campos metodos) clase]))

;; val-class? :: Val -> Boolean
;; funcion que retorna true si el valor es una clase
(define (val-class? v)
  (match v
    [(classV ids) #t]
    [else #f]))


;; open-val :: Val -> Scheme Value
;; Extrae el valor de un Val
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    ))

;; make-val :: Scheme Value -> Val
;; Construye un Val a partir de un scheme value
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
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
