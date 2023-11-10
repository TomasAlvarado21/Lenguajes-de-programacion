#lang play


#|
  Expr  ::= <num>
          | (+ <Expr> <Expr>)
          | (- <Expr> <Expr>)
          | (* <Expr> <Expr>)
          | <id>
          | (fun (<id> : <Type>) <Expr>)
          | (<Expr> <Expr>);
|#
(deftype Expr
  ;; core
  (num n)
  (binop op l r)
  ;; unary first-class functions
  (id x)
  (fun binder binderType body)
  (app callee arg)
  ;; new constructors
  (bool b)
  (leq l r)
  )

#| BEGIN P1 |#

;; Type ::= <numT>
;;         | <boolT>
;;         | <arrowT>
(deftype Type 
  (numT)
  (boolT)
  (arrowT T1 T2) ; (arrowT <Type> <Type>)
  )

;; parse-type : ...
(define (parse-type t) 
  (match t
    ['Number (numT)]
    ['Boolean (boolT)]
    [(list '-> l r) (arrowT (parse-type l) (parse-type r))]
    [_ (error 'parse-type "invalid type: ~a" t)]))

(test (parse-type 'Number) (numT))
(test (parse-type 'Boolean) (boolT))
(test (parse-type '(-> Number Boolean)) (arrowT numT boolT))

;; ambiente de tipos
;; TypeEnv ::= ⋅ | <TypeEnv>, <id> : <Type>
(deftype TypeEnv (mtTenv) (aTenv id type env))
(define empty-tenv (mtTenv))
(define extend-tenv aTenv)

(define (tenv-lookup x env)
  (match env
    [(mtTenv) (error 'tenv-lookup "free identifier: ~a" id)]
    [(aTenv id type rest) (if (symbol=? id x) type (tenv-lookup x rest))]
    ))

;; infer-type : Expr TypeEnv -> Type
;; al darle un expr y un ambiente de tipos, devuelve el tipo del expr
;; tenemos literales numericos, adicionales(si ambos operandos tienen tipo numT, entonces el resultado es numT),  identificadores, funciones, aplicaciones
(define (infer-type expr tenv) 
  (match expr
    [(num n) (numT)]
    [(bool b) (boolT)]
    [(binop op l r) (match (cons (infer-type l tenv) (infer-type r tenv))
                      [(cons (numT) (numT)) (numT)]
                      [(cons (boolT) (boolT)) (boolT)]
                      [_ (error 'infer-type "invalid operand type for ~a" op)])]
    [(id x) (tenv-lookup x tenv)]
    [(fun binder binderType body) (arrowT binderType (infer-type body (extend-tenv binder binderType tenv)))]
    [(app call arg) (match (infer-type call tenv)
                        [(arrowT T1 T2) (match (cons T1 (infer-type arg tenv))
                                          [(cons T1 T2) T2]
                                          [_ (error 'infer-type "invalid argument type")])]
                        [_ (error 'infer-type "function application to a non-function")])]
    [(leq l r) (match (cons (infer-type l tenv) (infer-type r tenv))
                 [(cons (numT) (numT)) (boolT)]
                 [_ (error 'infer-type "invalid operand type for <=")])]
    [(app (app (id 'if) c) th) (match (cons (infer-type c tenv) (infer-type th tenv))
                                 [(cons (boolT) T) T]
                                 [_ (error 'infer-type "invalid argument type for if")])]
    ))
(infer-type (app (fun 'x (numT) (id 'x)) (fun 'x (numT) (id 'x))) empty-tenv)
;  (infer-type (num 1) empty-tenv)
;  (infer-type (fun 'x (numT) (id 'x)) empty-tenv)
;  ( infer-type (fun 'x (arrowT (numT) (numT)) (id 'x)) empty-tenv)
;;( infer-type (binop '+ (num 1) (fun 'x (numT) (id 'x))) empty-tenv)
 ;;( infer-type (app (num 1) (num 2)) empty-tenv)


#| END P1 |#

#| BEGIN P2 PREAMBLE |#

;; ambiente de sustitución diferida
(deftype Env
  (mtEnv)
  (aEnv id val env))

;; interface ADT (abstract data type) del ambiente
(define empty-env (mtEnv))

;; "Simplemente" asigna un nuevo identificador para aEnv
;(define extend-env aEnv)
;;
;; es lo mismo que definir extend-env así:
;; (concepto técnico 'eta expansion')

;; parse : s-expr -> Expr
(define (parse s)
  (match s
    [n #:when (number? n) (num n)]
    [(list 'true) (bool #t)]
    [(list 'false) (bool #f)]
    [x #:when (symbol? x) (id x)]
    [(list '+ l r) (binop '+ (parse l) (parse r))]
    [(list '- l r) (binop '- (parse l) (parse r))]
    [(list '* l r) (binop '* (parse l) (parse r))]
    [(list '<= l r) (leq (parse l) (parse r))]
    [(list 'if c th el) (app (app (id 'if) (parse c)) (parse th))]
    [(list 'fun (list binder ': type) body) (fun binder (parse-type type) (parse body))]
    [(list callee arg) (app (parse callee) (parse arg))]
    [_ (error 'parse "invalid syntax: ~a" s)]))

#| END P2 |#
;; parse : s-expr -> Expr
(define (parse s)
  (match s
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    [(list '+ l r) (binop '+ (parse l) (parse r))]
    [(list '- l r) (binop '- (parse l) (parse r))]
    [(list '* l r) (binop '* (parse l) (parse r))]
    [(list 'fun (list binder ': type) body) (fun binder (parse-type type) (parse body))]
    [(list callee arg) (app (parse callee) (parse arg))]
    [_ (error 'parse "invalid syntax: ~a" s)]))

;; Implementación de ambientes de tipos
;; (análoga a la de ambientes de valores)

;; TypeEnv ::= ⋅ | <TypeEnv>, <id> : <Type>
(deftype TypeEnv (mtTenv) (aTenv id type env))
(define empty-tenv (mtTenv))
(define extend-tenv aTenv)

(define (tenv-lookup x env)
  (match env
    [(mtTenv) (error 'tenv-lookup "free identifier: ~a" id)]
    [(aTenv id type rest) (if (symbol=? id x) type (tenv-lookup x rest))]
    ))

;; infer-type : Expr TypeEnv -> Type
;; al darle un expr y un ambiente de tipos, devuelve el tipo del expr
;; tenemos literales numericos, adicionales(si ambos operandos tienen tipo numT, entonces el resultado es numT),  identificadores, funciones, aplicaciones

(define (infer-type expr tenv) 
  (match expr
    [(num n) (numT)]
    [(binop op l r) (match (cons (infer-type l tenv) (infer-type r tenv))
                      [(cons (numT) (numT)) (numT)]
                      [_ (error 'infer-type "invalid operand type for ~a" op)])]
    [(id x) (tenv-lookup x tenv)]
    [(fun binder binderType body) (arrowT binderType (infer-type body (extend-tenv binder binderType tenv)))]
    [(app call arg) (match (infer-type call tenv)
                        [(arrowT T1 T2) (match (cons T1 (infer-type arg tenv))
                                          [(cons T1 T2) T2]
                                          [_ (error 'infer-type "invalid argument type")])]
                        [_ (error 'infer-type "function application to a non-function")])]
    ))
(infer-type (app (fun 'x (numT) (id 'x)) (fun 'x (numT) (id 'x))) empty-tenv)
;  (infer-type (num 1) empty-tenv)
;  (infer-type (fun 'x (numT) (id 'x)) empty-tenv)
;  ( infer-type (fun 'x (arrowT (numT) (numT)) (id 'x)) empty-tenv)
;;( infer-type (binop '+ (num 1) (fun 'x (numT) (id 'x))) empty-tenv)
 ;;( infer-type (app (num 1) (num 2)) empty-tenv)


#| END P1 |#

#| BEGIN P2 PREAMBLE |#

;; ambiente de sustitución diferida
(deftype Env
  (mtEnv)
  (aEnv id val env))

;; interface ADT (abstract data type) del ambiente
(define empty-env (mtEnv))

;; "Simplemente" asigna un nuevo identificador para aEnv
;(define extend-env aEnv)
;;
;; es lo mismo que definir extend-env así:
;; (concepto técnico 'eta expansion')
(define (extend-env id val env) (aEnv id val env))

(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x) val (env-lookup x rest))]))

;; num2num-op : (Number Number -> Number) -> Val Val -> Val
(define (num2num-op op)
  (λ (l r)
    (match (cons l r)
      [(cons (num n) (num m)) (num (op n m))]
      [_ (error 'num-op "invalid operands")])))


(define num+ (num2num-op +))
(define num- (num2num-op -))
(define num* (num2num-op *))

#| END P2 PREAMBLE |#

#| BEGIN P2 |#

;; final? : ...
(define (final? e) '???)

(deftype Kont
  (mt-k) ; empty kont
  ;; ...
  )

(define empty-kont (mt-k))

;; State ::= (<Expr>, <Env>, <Kont>)
(deftype State
  (st expr env kont)
  )

;; inject : ...
(define (inject expr) '???)

;; step : ...
(define (step c) '???)

;; eval : Expr -> Expr
(define (eval expr)
  (define (eval-until-final state)
    (def (st expr _ kont) state)
    (if (and (final? expr) (mt-k? kont))
        expr
        (eval-until-final (step state))))
  (eval-until-final (inject expr)))

;; run : ...
(define (run s-expr) '???)
