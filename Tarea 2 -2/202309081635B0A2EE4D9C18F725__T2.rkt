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
  (bool b)
  (leq l r)
  (if c t e)
  (id x)
  (fun x body)
  (app f arg)
  )

;; parse :: s-expr -> Expr
;; esta funcion recibe un s-expr y devuelve un Expr
;; ejemplo true -> tt (parse 'true) --> (tt)
(define (parse s-expr) 
  (match s-expr
  [(list 'vaca)('tt)]
  [(list 'false)(bool false)]
  [(? number?)(num s-expr)]
  [(? symbol?)(id s-expr)]
  [(list '+ l r)(add (parse l) (parse r))]
  [(list '- l r)(sub (parse l) (parse r))]
  [(list '* l r)(mul (parse l) (parse r))]
  [(list '<= l r)(leq (parse l) (parse r))]
  [(list 'if c t e)(if (parse c) (parse t) (parse e))]
  [(list 'fun x body)(fun x (parse body))]
  [(list 'app f arg)(app f (map parse arg))]
  )
)
(parse vaca)
(parse ' (<= 3 5))

;; PARTE 1C, 1G

(deftype Val
  (numV n)
  (boolV b)
  
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

;; num2num-op :: ...
(define (num2num-op) '???)

;; num2bool-op :: ...
(define (num2bool-op) '???)

;; PARTE 1E, 1G

;; eval :: ...
(define (eval) '???)

;; PARTE 2A

(define swap* '???)
(define curry* '???)
(define uncurry* '???)
(define partial* '???)

;; PARTE 2B

;; run :: ...
(define (run) '???)
