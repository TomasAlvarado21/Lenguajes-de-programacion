#lang play

#|
<expr> :: = (num <num>)
        | (add <expr> <expr>)        
        | (if0 <expr> <expr> <expr>)
        | (id <id>)
        | (fun <sym> <expr>)
        | (refun <sym> <expr>)
        | (app <expr> <expr>)
        | (set <id> <expr>)
|#

(deftype Expr
  (num n)
  (add l r)  
  (if0 c t f)
  (id x)
  (fun arg body)     ;; -- call-by-value
  (refun arg body)   ;; -- call-by-reference
  (app f-name f-arg)
  (set id val-expr)  ;; 
  ;(newbox val-expr)
  ;(openbox box-expr)
  ;(setbox box-expr val-expr)
  (seqn expr1 expr2))
  

;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    ;[(list 'newbox v) (newbox (parse v))]
    ;[(list 'openbox b) (openbox (parse b))]
    ;[(list 'setbox b v) (setbox (parse b) (parse v))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list 'set id e) (set id (parse e))]
    [(list '+ l r) (add (parse l) (parse r))]    
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list 'refun (list x) b) (refun x (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list 'with (list x e) b) #:when (symbol? x)
                               (app (fun x (parse b)) (parse e))]))

(deftype Env
  (mtEnv)
  (aEnv i v env))

(define empty-env mtEnv)

(define extend-env aEnv)

(define (lookup-env x env)
  (match env
    [(mtEnv) (error "env-lookup: Identificador libre: ~a" x)]
    [(aEnv i v e)
     (if (equal? x i)
         v
         (lookup-env x e))]))

(deftype Store
  (mtSto)
  (aSto loc val sto))

(define empty-sto mtSto)

(define extend-sto aSto)

(define (lookup-sto x sto)
  (match sto
    [(mtSto) (error 'lookup-sto "No value at location ~a" x)]
    [(aSto loc val rest)
     (if (equal? loc x)
         val
         (lookup-sto x rest))]))

(define (next-location sto)
  (match sto
    [(mtSto) 0]
    [(aSto _ _ rest) (+ 1 (next-location rest))]))

(deftype Value
  (numV n)  
  (closureV id body env)
  (refclosureV id body env)
  ;(boxV loc)
  )

(deftype Value*Store
  (v*s val sto))

(define (num+ n1 n2)
  (def (numV v1) n1)
  (def (numV v2) n2)
  (numV (+ v1 v2)))

(define (num-zero? n)
  (def (numV v) n)
  (zero? v))
    
;; interp :: Expr Env Store -> Value*Store
(define (interp expr env sto)
  (match expr
    [(num n) (v*s (numV n) sto)]
    [(id x) (v*s (lookup-sto (lookup-env x env) sto) sto)]    
    [(fun id body) (v*s (closureV id body env) sto)]
    [(refun id body) (v*s (refclosureV id body env) sto)]
    
    [(if0 c t f)
     (def (v*s c-val c-sto) (interp c env sto))
     (if (num-zero? c-val)
         (interp t env c-sto)
         (interp f env c-sto))]
    [(add l r)
     (def (v*s l-val l-sto) (interp l env sto))
     (def (v*s r-val r-sto) (interp r env l-sto))
     (v*s (num+ l-val r-val) r-sto)]

    [(app fun-expr arg-expr)
     (def (v*s fun-val fun-sto) (interp fun-expr env sto))
     (match fun-val
       [(closureV id body fenv)
        (def (v*s arg-val arg-sto) (interp arg-expr env fun-sto))
        (def new-loc (next-location arg-sto))
        (interp body
                (extend-env id new-loc fenv)
                (extend-sto new-loc arg-val arg-sto))]

       ;; Se asume que arg-expr es un identificador.
       ;; Es decir arg-expr calza con (id x)
       [(refclosureV id body fenv)
        ;; Buscar la ubicación de memoria del identificador dado como argumento
        (def loc (lookup-env (id-x arg-expr) env))
        ;; Interpretar cuerpo de la función en un ambiente
        ;; donde el nombre del parámetro se asocia a la locación del
        ;; identificador dado como argumento.
        (interp body
                (extend-env id loc fenv)
                fun-sto)]
       )]

    [(seqn expr1 expr2)
     (def (v*s _ sto1) (interp expr1 env sto)) ;; aqui interpreta la primera expresion, guardando solo el store pq no nos interesa guardar el valor
     (interp expr2 env sto1)] ;; aqui interpretamos el segundo expr con el store que hicimos update anterior 

    [(set id val-expr) ;; asigna un valor a una variable
     (def (v*s val-val val-sto) (interp val-expr env sto)) ;; primero interpreta el valor y lo asigna a val en el value*store  
     (def loc (lookup-env id env)) ;; busca la locación de la variable en el ambiente
     (v*s val-val
          (extend-sto loc val-val val-sto))] ;; extiende el store con la nueva locación y el valor v   

    #|
    [(newbox val-expr)
     (def (v*s val-val val-sto) (interp val-expr env sto))
     (def new-loc (next-location val-sto))
     (v*s (boxV new-loc)
          (extend-sto new-loc val-val val-sto))]

    [(openbox box-expr)
     (def (v*s (boxV loc) box-sto) (interp box-expr env sto))
     (v*s (lookup-sto loc box-sto) box-sto)]

    [(setbox box-expr val-expr)
     (def (v*s (boxV loc) box-sto) (interp box-expr env sto))
     (def (v*s val-val val-sto) (interp val-expr env box-sto))
     (v*s val-val
          (extend-sto loc val-val val-sto))]
    |#    

    ))

(define (run e)
  (def (v*s result final-store)
    (interp (parse e) (empty-env) (empty-sto)))
  result)

(define p1 '{with {b 0} {seqn {set b {+ 1 b}} b}})

(define p2 '{with {b 0} {if0 {seqn {set b 5} b} 1 b}})

(define p3 '{with {v 0}
                  {with {f {fun {y} {set y 5}}}
                        {seqn {f v} v}}})

(define p4 '{with {v 0}
                  {with {f {fun {y} {set y 5}}}
                        {seqn {f v} v}}})

(define p5 '{with {v 0}
                  {with {f {refun {y} {set y 5}}}
                        {seqn {f v} v}}})

(run p4)
#|
{with {f {fun {a {ref b}}
             {seq {set a 5}
                  {set b 10}}}}}

int a  : por valor
int &b : por referencia
-----------------------
void f(int a, int &b) {
   a = 5;
   b = 10;
}

...
int x = 0;
int y = 1;
f(x, y); <---- ¿cambian los valores de x e y?
printf(x) <-- ¿qué imprime?
printf(y) <-- ¿qué imprime?
|#