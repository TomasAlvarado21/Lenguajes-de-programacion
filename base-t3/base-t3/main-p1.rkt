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
  (objV classes fields methods)
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

;; objV-env :: Object Env
(define (objV-env object env)
  (match object
    [(objV c f m) env]))

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

    [(list 'with (list e ...)  b)
     (with (map parse-def e) (parse b))]

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
    [(list 'def id (list ids ...) b) (my-def id (λ (ids ...) (parse b)))]))



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
    ; ocupa los otros archivos en la carpeta
    ;; ocupamos el getMethod para obtener el metodo y verificamos que exista
    ;; si no existe, tiramos error "method not found exception"
    [(class ids methods)
      (def met (findf (λ (x) (equal? 'init (first x))) methods))
      (if met
          (let* ([param-names (second met)]
                 [body (third met)]
                 [new-env (multi-extend-env param-names '() env)])
            (extend-frame-env! 'self (classV ids new-env methods) new-env)
            (interp body new-env)
            (classV ids new-env methods))
          (classV ids env methods))]

    [(self) (unbox (env-lookup 'self env))]
    [(new clase args)
      (def lmetodos (getMeth metodos '()))
      (let* ([object (interp clase env)]
            (def construct (findf (lambda (x)
              (and (equal? 'init (first x))
              (equal? (length (second x)) (length (car args)))))
              lmetodos))
        (if constructor
            (let* ([param-names (second constructor)]
                    [body (third constructor)]
                    [new-env (multi-extend-env param-names args env)])
              (extend-frame-env! 'self object new-env)
              (interp body new-env)
              object)
            (if (empty? args)
                object
                (error "-new: constructor not found"))))]

    [(set id arg)
      (let* ([obj (interp self env)]
              [class (classV-class obj)]
              [field (get-field id class)])
        (if field
            (field-set! obj id (interp arg env))
            (error 'set "field not found: ~a" id)))]


    [(-> clase idm args)
      (let* ([obj (interp clase env)]
              [class (classV-class obj)]
              [method (getMeth idm class)])
        (if method
            (apply method (cons obj (map (lambda (x) (interp x env)) args)))
            (error "->: method not found: ~a" idm)))]

      
  ))

;; classV-methods :: Val -> List<Def>
;; funcion que retorna los metodos de una clase
(define (classV-methods obj)
  (match obj
    [(classV ids methods) methods]))



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

;; field-set! :: Val -> String -> Val -> Void
;; funcion que setea el valor de un campo de un objeto
(define (field-set! obj id val)
  (match obj
    [(objV clase campos metodos) (env-update! id val campos)]))

;; field-get :: Val -> String -> Val
;; funcion que retorna el valor de un campo de un objeto
(define (field-get obj id)
  (match obj
    [(objV clase campos metodos) (env-lookup id campos)]))


;; objV-metodos :: Val -> List<Def>
;; funcion que retorna los metodos de un objeto
(define (objV-metodos v)
  (match v
    [(objV clase campos metodos) metodos]))

;; getV :: Val -> String -> Val
;; funcion que retorna el valor de un campo de un objeto
(define (getV v id)
  (match v
    [(objV clase campos metodos) (env-lookup id campos)]))
  
;; objV-campos :: Val -> List<String>
;; funcion que retorna los campos de un objeto
(define (objV-campos v)
  (match v
    [(objV clase campos metodos) (map first campos)]))
  
;; setV :: Val -> String -> Val -> Val
;; funcion que setea el valor de un campo de un objeto
(define (setV v id val)
  (match v
    [(objV clase campos metodos) (multi-extend-env (list id) (list val) campos)]))

;; newV :: Val -> List<Val> -> Val
;; funcion que crea un objeto
(define (newV clase vals)
  (match clase
    [(classV ids) (objV clase (map (λ (x) (list x (numV 0))) ids) '())]))

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


;; Tests
;; dame un ejemplo parser que funcione de clases
#;
(run-val '{with {{A {class {}}}
                   {o {new A {}}}}
              {-> o m}})