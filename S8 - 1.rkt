#lang play
(print-only-errors #t)

#| 
<Expr> ::= <number>
         | {+ <Expr> <Expr>}
         | {- <Expr> <Expr>}
         | <symbol>
         | {with {<symbol> <Expr>} <Expr>} ; azucar sintactico
         | {<Expr> <Expr>}
         ;; importante: cualquier expr en posicion de funcion
         | {fun {<symbol>} <Expr>}
         ;; funciones anonimas / 1a clase
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id x)
  (app fun-expr arg-expr)
  (fun param body))

; parse : s-expr -> Expr
(define (parse s)
  (match s
    [(? number?) (num s)]
    [(? symbol?) (id s)]
    [(list '+ e1 e2) (add (parse e1) (parse e2))]
    [(list '- e1 e2) (sub (parse e1) (parse e2))]
    [(list 'with (list name ne) body) ;; with --> app/fun
     (app (fun name (parse body))
          (parse ne))]
    [(list 'fun (list x) e) (fun x (parse e))]
    [(list f e) (app (parse f) (parse e))]))

;; Environment Abstract Data Type (ADT)
;; Env
;; empty-env : -> Env
;; extend-env : Id Val Env -> Env
;; lookup-env : Id Env -> Val o Error
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env mtEnv)

(define extend-env aEnv)

(define (lookup-env x env)
  (match env
    [(mtEnv) (error "free identifier:" x)]
    [(aEnv y v next)
     (if (equal? x y)
         v
         (lookup-env x next))]))

; posibles valores
(deftype Val
  (numV n)
  (closureV param body env)
  (exprV expr env)) ;; promesa de evaluar expr en env si necesario

;; interp : Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(fun p b) (closureV p b env)]
    [(add l r) (numV+ (strict (interp l env))
                      (strict (interp r env)))]
    [(sub l r) (numV- (strict (interp l env))
                      (strict (interp r env)))]
    [(id x) (lookup-env x env)] 
    [(app f e)
     (def (closureV arg body fenv) (strict (interp f env)))
     (interp body
             (extend-env arg
                         ;(interp e env) ;; evaluacion temprana
                         (exprV e env) ;; evaluación perezosa
                         fenv))])) 

#|
{with {f {fun {x} {+ x x}}
{with {y 10}
 {f {+ y y}}      [x --> <{+ y y}, [y -> 10]> ]
|#

(define (numV+ n1 n2) 
  (numV (+ (numV-n n1) (numV-n n2))))

(define (numV- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))

;; Val: numV / closureV / exprV
;; VdV: numV / closureV
; strict : Val -> VdV
(define (strict v)
  (match v
    [(exprV expr env) (strict (interp expr env))]
    [_ v]))


; top-level run
(define (run prog) 
  (match (strict (interp (parse prog) (empty-env)))
    [(numV res) res]
    [(closureV p b e) (closureV p b e)]))


 
; tests
(test (run 10) 10)
(test (run '{+ 10 {- 20 5}}) 25)
(test
 (run '{with {x 1} {+ x x}})
 2)
(test (run '{with {x 10}
                  {+ {with {x x}
                           x}
                     x}})
      20)

(test/exn (run '{+ {with {x 1}
                         x}
                   x})
          "free identifier")

(test/exn (run '{with {y 10}
                  {+ {with {x x}
                           x}
                     x}})
          "free identifier")

(test (run '{with {x {+ 10 20}}
               {with {y {+ x 2}}
                  {+ x y}}})
      62)


(test (run '{{fun {a} {+ a a}} 1})
      2)

(test (run '{with {f {fun {a} {+ a a}}}
               {f 1}})
      2)

(test (run '{with {f {fun {a} {+ a a}}}
                  {with {f 4}
                        f}})
      4)

(test (run '{with {add1 {fun {n} {+ n 1}}}
              {with {f {fun {x} {+ x x}}}
                 {with {x {+ 2 2}}
                    {add1 {f x}}}}})
      9)

(test (run '{with {addn {fun {n}
                             {fun {m}
                                  {+ n m}}}}
                  {{addn 10} 20}})
      30)

(test (run '{with {applyTo1 {fun {f} {f 1}}}
                  {applyTo1 {with {delta 10}
                               {fun {a} {+ a delta}}}}})
      11)


;; no tenemos recursion (ojo: con scope dinamico, sí!)
(test/exn (run '{with {f {fun {x} {f x}}}
                   {f 0}})
          "free")


; este test permite determinar si el interp usa
; evaluación antes de substituir o no
; (evaluación "temprana" vs. "perezosa")
;(test/exn (run '{with {x z} 1}) "free") ; temprana
(test (run '{with {x z} 1}) 1) ; perezosa


