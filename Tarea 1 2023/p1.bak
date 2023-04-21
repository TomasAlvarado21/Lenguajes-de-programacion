#lang play

(require "env.rkt")

;; Parte 1

#|
<prog>   ::= {<fundef>* <expr>}

<fundef> ::= {define {<id> <id>*} <expr>}

<expr>   ::= <num>
           | <id>
           | <bool>
           | {cons <expr> <expr>}
           | {add1 <expr>}
           | {+ <expr> <expr>}
           | {< <expr> <expr>}
           | {= <expr> <expr>}
           | {! <expr>}
           | {&& <expr> <expr>}
           | {|| <expr> <expr>}
           | {fst <expr>}
           | {snd <expr>}
           | {if <expr> <expr> <expr>}
           | {with {{<id> <expr>}*} <expr>}
           | {<id> <expr>*}
|#
(deftype Prog
  (prog fundefs main))

(deftype Fundef
  (fundef name arg body))

(deftype Expr
  (num n)
  (id x)
  (bool b)
  (cons expr expr2)
  (add1 expr)
  (add l r)
  (lt l r)
  (eq l r)
  (neq expr)
  (and l r)
  (or l r)
  (fst expr)
  (snd expr)
  (if0 con t f)
  (with list-expr body)
  (app expr)
  )

;; tipo inductivo para los valores del lenguaje
(deftype Val
  (numV n)
  (boolV b)
  (pairV lV rV))

;; parse :: ...
(define (parse sp)
  (match sp
    [(list ds ... e) (prog (map parse-fundef ds) (parse-expr e))] ;; ds es la lista de definiciones, e es la expresion principal
    ))

;; parse-expr :: ...
(define (parse-expr se)
  (match se
    [(? number?) (num se)]
    [(? symbol?) (id se)]
    [(? boolean?) (bool se)]
    [(list 'add1 e1) (add1 (parse-expr e1))] 
    [(list identificador valor) (list (parse-expr identificador) (parse-expr valor))] 
    [(list '+ e1 e2) (add (parse-expr e1) (parse-expr e2))]
    [(list '< e1 e2) (lt (parse-expr e1) (parse-expr e2))]
    [(list '= e1 e2) (eq (parse-expr e1) (parse-expr e2))]
    [(list '! e1) (neq (parse-expr e1))]
    [(list '&& e1 e2) (and (parse-expr e1) (parse-expr e2))]
    [(list '|| e1 e2) (or (parse-expr e1) (parse-expr e2))]
    [(list 'fst e1) (fst (parse-expr e1))]
    [(list 'snd e1) (snd (parse-expr e1))]
    [(list 'if con e1 e2) (if0 (parse-expr con) (parse-expr e1)(parse-expr e2))]
    [(list 'with lista body) (with (map (λ (par) (map parse-expr par)) lista) (parse-expr body))]
    [(list f a) (app f (parse-expr a))]
    [_ (error "not yet implemented")]
    ))

;; parse-fundef :: ...
(define (parse-fundef sf)
  (match sf
    [(list 'define name arg ... body) (fundef (parse-expr name) (map parse-expr arg) (parse-expr body))]))


;; interp :: Expr → Env → list Fundef → Val
(define (interp e env funs)
  (match e
    [(num n) (numV n)]
    [(id x) (env-lookup x env)]
    [(bool b) (boolV b)]
    ;[(cons l r) (consV lV rV)]
    [(lt l r) (< (interp l env funs) (interp r env funs))]
    [(eq e1 e2) (= (interp e1 env funs) (interp e2 env funs))]
    [(add  e1 e2) (+ (interp e1 env funs) (interp e2 env funs))]
    [(neq e1) (not (interp e1 env funs))]
    [(and e1 e2) (and (interp e1 env funs) (interp e2 env funs))]
    ;[(or e1 e2) (or (interp e1 env funs) (interp e2 env funs))]
    [(fst e1) (car (interp e1 env funs))]
    [(snd e1) (cdr (interp e1 env funs))]
    [(if0 con e1 e2) (if (interp con env funs) (interp e1 env funs) (interp e2 env funs))]
    [(add1 e1) (+ (interp e1 env funs) 1)]
    [(with lista body)
     ((interp
       (map extend-env lista) env funs)
      (interp body env funs))] ;; usar esto https://users.dcc.uchile.cl/~etanter/play-interps/Functions_with_Environments.html
    [_ (error "not yet implemented")]
    ))

(define (run sp)
  (def (prog funs main) (parse sp))
  (interp main empty-env funs))

(parse-expr '{with {{x 9} {y {+ 1 4}}}
        {+ x y}})


(parse-expr '{with {{x 1}{y 2}{z 3}}
              {+ x z}})

(parse-expr '{= {x 1} {y 2}})
(parse-expr '{add1 x})
(parse-fundef '{define {triple x} {+ x {+ x x}}})

#|
(with (list ((id x) (num 9)) ((id y) (add (num 1) (num 4)))) (add (id x) (id y)))





{+ x {+ y z}}
e1 = x
e2 = {+ y z}
(add (parse-expr x) (parse-expr {+ y z}))
(add (id x) (add (parse-expr y) (parse-expr z)))
(add (id x) (add (id y) (id z)))
|#