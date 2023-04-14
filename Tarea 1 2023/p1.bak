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
  (pair expr expr2)
  (add1 expr)
  (add l r)
  (lt l r)
  (eq l r)
  (neq expr)
  (and l r)
  (or l r)
  (fst expr)
  (snd expr)
  (if con t f)
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
    [(list '+ e1 e2) (add (parse-expr e1) (parse-expr e2))]
    [(list '< e1 e2) (lt (parse-expr e1) (parse-expr e2))]
    [(list '! e1) (neq (parse-expr e1))]
    [(list '&& e1 e2) (and (parse-expr e1) (parse-expr e2))]
    [(list '|| e1 e2) (or (parse-expr e1) (parse-expr e2))]
    [(list 'fst e1) (fst (parse-expr e1))]
    [(list 'snd e1) (snd (parse-expr e1))]
    [(list 'if con e1 e2) (if (parse-expr con) (parse-expr e1)(parse-expr e2))]

    [_ (error "not yet implemented")]
    ))

;; parse-fundef :: ...
(define (parse-fundef sf)
  ; ...
  (error "not yet implemented"))


;; interp :: ...
(define (interp e env funs)
  (match e
    [(num n) (numV n)]
    [(id x) (env-lookup x env)]
    [(bool b) (boolV b)]
    ; ...
    [_ (error "not yet implemented")]
    ))

(define (run sp)
  (def (prog funs main) (parse sp))
  (interp main empty-env funs))


(parse-expr '{with {{x 9} {y {+ 1 4}}}
        {+ x y}})


#|
(with (list ((id x) (num 9)) ((id y) (add (num 1) (num 4)))) (add (id x) (id y)))





{+ x {+ y z}}
e1 = x
e2 = {+ y z}
(add (parse-expr x) (parse-expr {+ y z}))
(add (id x) (add (parse-expr y) (parse-expr z)))
(add (id x) (add (id y) (id z)))
|#