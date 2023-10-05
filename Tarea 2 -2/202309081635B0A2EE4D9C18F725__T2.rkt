#lang play
(print-only-errors)

;; PARTE 1A, 1B, 1F

#|
  Expr ::= ...
|#
(deftype Expr
  ;; core
  (num n)
  (add l r)
  (sub l r)
  (mul l r)
  (tt)
  (ff)
  (tupl lst)
  (leq l r)
  (ifc c t e)
  (id x)
  (fun x body)
  (app f arg)
  )

;; parse :: s-expr -> Expr
;; esta funcion recibe un s-expr y devuelve un Expr
;; ejemplo true -> tt (parse 'true) --> (tt)
(define (parse s-expr) 
  (match s-expr
    ;; 'false -> ff
    ;; 'true -> tt
    [(quote true) (tt)]
    [(quote false) (ff)]
    [(? number?)(num s-expr)]
    [(? symbol?)(id s-expr)]
    [(list 'tuple a ...)(tupl (map parse a))]
    [(list '+ l r)(add (parse l) (parse r))]
    [(list '- l r)(sub (parse l) (parse r))]
    [(list '* l r)(mul (parse l) (parse r))]
    [(list '<= l r)(leq (parse l) (parse r))]
    [(list 'if c t e)(ifc (parse c) (parse t) (parse e))]
    [(list 'fun x body)(fun x (parse body))]
    [(list f a ...)(app (parse f) (map parse a))]
  )
)
(test (parse 'true) (tt))
(test (parse 'false) (ff))
(test (parse ' ( if (<= 3 5) 2 4)) (ifc (leq (num 3) (num 5)) (num 2) (num 4)))
(test (parse ' (<= 3 5)) (leq (num 3) (num 5)))
(test (parse '(fun (x y) (+ x y))) 
(fun ( list 'x 'y) (add (id 'x) (id 'y))))

(test (parse ' (my-function 2 3 4))
(app (id 'my-function) ( list (num 2) (num 3) (num 4))))


(test (parse ' (tuple 1 2 3)) 
(tupl ( list (num 1) (num 2) (num 3))))


;; PARTE 1C, 1G


(deftype Val
  (numV n)
  (boolV b)
  (closureV x body env)
  )

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

;; PARTE 1D

;; num2num-op :: (Number Number -> Number)-> (Val Val -> Val)
(define (num2num-op simbolo)
  (lambda (v1 v2)
    (match (list v1 v2)
      [(list (numV n1) (numV n2)) (numV (simbolo n1 n2))]
      [else (error "num-op: invalid operands ")])))

;; num2bool-op :: (Number Number -> Boolean)-> (Val Val -> Val)
(define (num2bool-op simbolo) 
  (lambda (v1 v2)
    (match (list v1 v2)
      [(list (numV n1) (numV n2)) (boolV (simbolo n1 n2))]
      [else (error "num-op: invalid operands ")])))

(define num+ (num2num-op +))
(define num- (num2num-op -))
(define num* (num2num-op *))
(define num<= (num2bool-op <=))

(test (num+ (numV 3) (numV 4)) (numV 7))
(test/exn (num+ (numV 4) (boolV #t)) "num-op: invalid operands ")


;; PARTE 1E, 1G

;; eval :: Expr Env -> Val
;; funcion que recibe un Expr y un Env y devuelve un Val, osea que evalua la expresion en el ambiente y retorna el resultado
(define (eval exp env)
  (match exp
    [(num n)(numV n)]
    [(add l r)(num+ (eval l env) (eval r env))]
    [(sub l r)(num- (eval l env) (eval r env))]
    [(mul l r)(num* (eval l env) (eval r env))]
    [(tt)(boolV #t)]
    [(ff)(boolV #f)]
    [(leq l r)(num<= (eval l env) (eval r env))]
    [(ifc c t e)(if (eval c env) (eval t env) (eval e env))]
    [(tupl lst)(tupl (map (lambda (x) (eval x env)) lst))]
    [(id x)(env-lookup x env)]
    [(fun x body) (closureV x body env)]
    [(app f arg)
    (def (closureV x body env) (eval f env))
    (def extended-env (extend-env x (eval arg env) env))
    (eval body extended-env)]
  ))


(test (eval (num 42) empty-env) (numV 42))
(test (eval (add (num 2) (num 3)) empty-env) (numV 5))
(test (eval (sub (num 5) (num 3)) empty-env) (numV 2))
(test (eval (mul (num 2) (num 4)) empty-env) (numV 8))
(test (eval (leq (num 3) (num 5)) empty-env) (boolV #t))
(test (eval (ifc (leq (num 3) (num 5)) (num 2) (num 4)) empty-env) (numV 2))
(test (eval (fun 'x (add (id 'x) (num 1))) empty-env) (closureV 'x (add (id 'x) (num 1)) empty-env))
(test (eval (tupl (list (num 1) (num 2) (num 3))) empty-env) (tupl (list (numV 1) (numV 2) (numV 3))))

(test (eval (app (fun (list 'x 'y) (add (id 'x) (id 'y))) (list (num 1) (num 2))) empty-env)
      (numV 3))



;; PARTE 2A

(define swap* '???)
(define curry* '???)
(define uncurry* '???)
(define partial* '???)

;; PARTE 2B

;; run :: ...
(define (run) '???)
