#lang play
(print-only-errors #t)


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
  (tt)
  (ff)
  (ifc c t e)
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



;; parse : s-expr -> Expr
(define (parse s)
  (match s
    [(quote true) (tt)]
    [(quote false) (ff)]
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    [(list '+ l r) (binop '+ (parse l) (parse r))]
    [(list '- l r) (binop '- (parse l) (parse r))]
    [(list '* l r) (binop '* (parse l) (parse r))]
    [(list '<= l r) (binop '<= (parse l) (parse r))]
    [(list 'if c t f) (ifc (parse c) (parse t) (parse f))]
    [(list 'fun (list binder ': type) body) (fun binder (parse-type type) (parse body))]
    [(list callee arg) (app (parse callee) (parse arg))]
    [_ (error 'parse "invalid syntax: ~a" s)]))


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
    [(tt) (boolT)]
    [(ff) (boolT)]
    [(binop op l r) (match op
                      ['<= (match (cons (infer-type l tenv) (infer-type r tenv))
                             [(cons (numT) (numT)) (boolT)]
                             [_ (error 'infer-type "invalid operand type for ~a" op)])]
                      [_ (match (cons (infer-type l tenv) (infer-type r tenv))
                            [(cons (numT) (numT)) (numT)]
                            [_ (error 'infer-type "invalid operand type for ~a" op)])])]
    [(id x) (tenv-lookup x tenv)]
    ;; ifc retorna el tipo de t si c es true, el tipo de e si c es false
    [(ifc c t e)  
     (match (infer-type c tenv)
       [(boolT) (match (cons (infer-type t tenv) (infer-type e tenv))
                  [(cons (numT) (numT)) (numT)]
                  [_ (error "infer-type: if branches type mismatch")])]
       [_ (error "infer-type: if condition must be a boolean")])]
                    


    [(fun binder binderType body) (arrowT binderType (infer-type body (extend-tenv binder binderType tenv)))]
    [(app callee arg) (match (infer-type callee tenv)
                        [(arrowT T1 T2) (match (infer-type arg tenv)
                                      (if (eq? T1 (infer-type arg tenv)) T2 (error 'infer-type "function argument type mismatch")))]
                        [_ (error 'infer-type "function application to a non-function")])]
    ))



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

;; final? : Expr -> Boolean
;; determina si un expr es final
(define (final? e) 
  (match e
    [(num n) #t]
    [(bool b) #t]
    [(fun binder binderType body) #t]
    [(tt) #t]
    [(ff) #t]
    [_ #f]))



(deftype Kont
  (mt-k) ; empty kont
  (binop-l-k op r env kont)
  )

(define empty-kont (mt-k))

;; State ::= (<Expr>, <Env>, <Kont>)
(deftype State
  (st expr env kont)
  )

;; inject : Expr -> State
;; inyecta un expr en un estado inicial
(define (inject expr) 
  (st expr empty-env empty-kont))



;; step : State -> State
;;  recibe un estado de la maquina CEK, y produce uno nuevo. no es recursiva ni llama a eval
(define (step c) 
  (match c
    [(st (num n) env kont) (match kont
                             [(mt-k) (st (num n) env kont)]
                             [(binop-l-k op r env kont) (st r env (binop-r-k op n kont))])]
    [(st (binop op l r) env kont) (st l env (binop-l-k op r env kont))]
    [(st (id x) env kont) (st (env-lookup x env) env kont)]
    [(st (fun binder binderType body) env kont) (st (fun binder binderType body) env kont)]
    [(st (tt) env kont) (st (tt) env kont)]
    [(st (ff) env kont) (st (ff) env kont)]
    [(st (ifc c t e) env kont) (st c env (ifc-k t e env kont))]
    [(st (app callee arg) env kont) (st callee env (app-k arg env kont))]
    [(st (leq l r) env kont) (st l env (leq-l-k r env kont))]
    [(st (binop '+ l r) env kont) (st l env (binop-l-k '+ r env kont))]
    [(st (binop '- l r) env kont) (st l env (binop-l-k '- r env kont))]
    [(st (binop '* l r) env kont) (st l env (binop-l-k '* r env kont))]
    [(st (binop '<= l r) env kont) (st l env (binop-l-k '<= r env kont))]

    [(st (ifc (tt) t e) env kont) (st t env kont)]
    [(st (ifc (ff) t e) env kont) (st e env kont)]
    [(st (ifc c t e) env kont) (st c env (ifc-k t e env kont))]
    [(st (app (fun binder binderType body) arg) env kont) (st body (extend-env binder arg env) kont)]
    [(st (leq (num n) (num m)) env kont) (st (ifc (binop '<= (num n) (num m)) (tt) (ff)) env kont)]

    [(st (app 
    [(st (leq-l r env kont) env kont) (match kont
                                          [(mt-k) (st r env kont)]
                                          [(binop-l-k op r env kont) (st r env (binop-l-k op r env kont))])]
    
    
    
    ))



;; eval : Expr -> Expr
; (define (eval expr)
;   (define (eval-until-final state)
;     (def (st expr _ kont) state)
;     (if (and (final? expr) (mt-k? kont))
;         expr
;         (eval-until-final (step state))))
;   (eval-until-final (inject expr)))

;; run : s-expr -> Expr
;; corre el programa
; (define (run s-expr) 
;   (eval (parse s-expr)))
