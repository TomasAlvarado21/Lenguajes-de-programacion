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
        | {new <expr> <expr>*}
        | {get <expr> <id>}
        | {set <id> <expr>}
        | {-> <expr> <id> <expr>*}
        | self
        | null
 
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
  (class flds mthds)
  (new cls args)
  (set fld val)
  (get obj fld)
  (-> obj mthd args)
  (self))

(deftype Def
  (my-def id expr))

(deftype Method
  (method id params body))

;; values
(deftype Val
  (null) ;; Default object field value
  (numV n)
  (boolV b)
  (classV fields methods)
  (objV cref oenv))

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
;; Prints (key - value) bindings from env
;; Env -> Void
(define (penv env)
  (when (mtEnv? env)
        (printf "~a\n\n"env))
  (when (not (mtEnv? env))
        (for ([(key value) (in-hash (aEnv-hash env))])
          (printf "key: ~a\nvalue: ~a\n" key value))
        (penv(aEnv-env env))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Returns methods with method-id name
;; classV Symbol -> List[Method]
(define(search-methods c name)
  (define class_methods (classV-methods c))
  (filter (lambda(m) (eq? (id-s (method-id m)) name)) class_methods))

;; Check if field defined by class
;; classV Id -> Boolean
(define(is-field-valid? c fid)
  (define class_fields (classV-fields c))
  (when (empty? (filter (lambda(f) (equal? f fid)) class_fields))
    (error "field not found exception")))


;; Returns method with given arity
;; List[Method] Length -> List[Method]
(define (get_method_by_arity method_list arity)
  (define res 
    (filter
      (lambda(m)
        (equal? 
          (length (method-params m)) 
          arity))
      method_list))
  (if (empty? res)
    #f
    (first res)))


;; Initialize Self for given object
;; objV -> Void
(define (addSelf obj)
  (extend-frame-env! self obj (objV-oenv obj))
  obj)


;; Binds method args to their params in a given env
;; Method List[Val] -> Void
(define (bind-method-param-to-args m args env)
    (for-each 
          (lambda(p-arg) 
            (extend-frame-env! (car p-arg) (cdr p-arg) env))
          (map 
            (lambda(p arg)(cons (id-s p) arg))
            (method-params m)
            args)))


;; Find number of required params for method
;; Method -> scheme number
(define (find-method-params-number method)
  (length (method-params method)))


;; Validate Class Constructors
;; classV -> boolean
(define (validate-class c)
  (define init-methods (search-methods c 'init))
  (if (check-duplicates (map find-method-params-number init-methods))
    (error "error: same arity constructor error")
    c))


;; Compare if two id's are identical
;; Id Id -> Boolean
(define (equal-ids? id1 id2)
  (eq? (id-s id1) (id-s id2)))


;; Return [<method>*] with (method-id mid)
;; classV Id -> List[Method]
(define (search-methods-by-id c mid)
  (define res (filter 
    (lambda(m)
      (equal-ids? (method-id m) mid)) 
      (classV-methods c)))
    (if (empty? res)
      #f
      res))


;; Search for method in class
;; Class Id List[Val] -> List[method]
(define (find-method c mid args)
  ;; Method List Parameter(RACKET)
  (define m-list (make-parameter '()))
  ;; Class {mid} methods
  (define methods (search-methods-by-id c mid))
  (if 
    methods ;; Available Methods
      (parameterize 
        ([m-list methods])
        (get_method_by_arity (m-list) (length args)))
      (error "method not found exception")))


;; Return env with class field null bindings
;; classV Env -> Env
(define (initialize-null-fields c env)
  (multi-extend-env
    (map (lambda(f) (id-s f)) (classV-fields c))
    (map (lambda(f) null) (classV-fields c))
    env)) 

;; Initialize object with the correct init method if available
;; objV classV List[Val] -> Void
(define (run-init-obj o c c-args)
  ;; Get Init methods
  (define init_methods  (search-methods c 'init))
  ;; Get inti method
  (define valid-init  (make-parameter #f))
  (if (empty? init_methods)
        ;; Unavailable Init Methods
        (if (empty? c-args)
          ;; Unvailaable Constructor Args
          #t
          ;; Available Constructor Args
          (error "error: constructor not found exception"))
        ;; Available Init Methods
        (if (empty? c-args)
          ;; Unvailaable Constructor Args
          (parameterize ([valid-init (get_method_by_arity init_methods (length c-args))])
            (if (valid-init)
              (interp (method-body (valid-init)) (objV-oenv o))  
              (error "error: constructor not found exception")
              ))
          ;; Available Constructor Args
          (parameterize ([valid-init (get_method_by_arity init_methods (length c-args))])
            (when (not (valid-init))
              (error "error: constructor not found exception"))
            ;; Bind init args to objV-oenv
            (bind-method-param-to-args (valid-init) c-args  (objV-oenv o))
            ;; Override objV-oenv with init execution
            (interp (method-body (valid-init)) (objV-oenv o)))))
  )

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
    [(list 'class fields methods ...) (class (map parse fields) (map parse methods))]
    [(list 'def name params body) (method (parse name) (map parse params) (parse body))]
    [(list 'new cid args ) (new (parse cid) (map parse args))]
    [(list 'new cid) (new (parse cid) '())]
    [(list '-> oid mid args ...) (-> (parse oid) (parse mid) (map parse args))]
    [(list 'get oid field) (get (parse oid) (parse field))]
    [(list 'set fid val) (set (parse fid) (parse val))]
    ))


;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list id b) (my-def id (parse b))]))

;; interp :: Expr Env -> Val
(define (interp expr env)
  ; (printf "interp ---\n")
  ; (printf "expr ~a\n" expr)
  ; (printf "env ---\n")
  ; (penv env)
  (match expr
    [(null) (error "error: null pointer exception")]
    [(num n) (numV n)]    
    [(bool b) (boolV b)]
    [(class flds mthds) (validate-class (classV flds mthds))]    
    [(binop f l r)
      (make-val (f (open-val (interp l env))
                  (open-val (interp r env))))]
    [(unop f s) (make-val (f (open-val (interp s env))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env))
     (if cnd
         (interp t env)
         (interp f env))]
    [(id 'self) (env-lookup self env)]
    [(id x) (env-lookup x env)]        
    [(begn expr1 expr2) (begin 
                          (interp expr1 env)
                          (interp expr2 env))]
    [(new cid args)
      ;; Get Class
      (define c (interp cid env))
      ;; Null Class Field Bindings
      (define init-env (initialize-null-fields c env))
      ;; Constructor args
      (define c-args (map (lambda(arg) (interp arg env)) args))
      ;; Initialize objV
      (define obj (objV c init-env))
      ;; Bind given obj to Self in obj-oenv
      (addSelf obj)
      ;; Run init method
      (run-init-obj obj c c-args)
      obj]
    [(set fid val)
      (is-field-valid? (objV-cref (env-lookup self env)) fid)
      (extend-frame-env! (id-s fid) (interp val env) env)]
    [(-> oid mid args)
      ;; object
      (define obj (interp oid env))
      ;; object class
      (define obj-c (objV-cref obj))
      ;; valid method
      (define m (find-method obj-c mid args))
      ;; method args
      (define m-args (map (lambda(arg) (interp arg env)) args))
      ;; Bind arg to method params in object env
      (bind-method-param-to-args m m-args (objV-oenv obj))
      ;; Evualate method-body using object env
      (interp (method-body m) (objV-oenv obj))]
    [(get oid f)
      ;; object
      (define obj (interp oid env))
      ;; Check if field is valid inside object
      (is-field-valid? (objV-cref obj) f)
      (when (equal? null (interp f (objV-oenv obj)))
        (error "null pointer exception")
      )
      (interp f (objV-oenv obj))
      ]
    [(with defs body)
     (let* ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (let ([in-def (interp-def x new-env)])
                     (extend-frame-env! (car in-def) (cdr in-def) new-env)
                     #t)) defs)       
       (interp body new-env))     
     ]))

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