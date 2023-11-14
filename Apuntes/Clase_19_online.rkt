#lang play

#|
<expr> :: = (num <num>)
        | (add <expr> <expr>)        
        | (if0 <expr> <expr> <expr>)
        | (id <id>)
        | (fun <sym> <expr>)
        | (app <expr> <expr>)
|#

(deftype Expr
  (num n)
  (add l r)  
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f-name f-arg)
  (newbox val-expr)
  (openbox box-expr)
  (setbox box-expr val-expr)
  (seqn expr1 expr2))
  

;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    [(list 'newbox v) (newbox (parse v))]
    [(list 'openbox b) (openbox (parse b))]
    [(list 'setbox b v) (setbox (parse b) (parse v))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list '+ l r) (add (parse l) (parse r))]    
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
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
  (boxV loc))

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
    [(if0 c t f)
     (def (v*s c-val c-sto) (interp c env sto)) ;; primero interpretamos la condicion y ocupamos ese store updated en las branches del if
     (if (num-zero? c-val) ;; aqui ocupamos el valor que interpretamos arriba
         (interp t env c-sto) ;; aqui ocupamos el store que se define al interpretar la condicion
         (interp f env c-sto))] ;; aqui tambien ocupamos el store que se define al interpretar la condicion
    ;; duda: aqui no tengo al interpretar el branch del if, no tengo que hacer un update del store?


    [(add l r) ;; aqui la situacion es un poco distinta ya que tenemos que definir en que orden vamos a interpretar y re definir los stores
     (def (v*s l-val l-sto) (interp l env sto)) ;; como vamos a ir evaluando de izq a der primero tenemos que hacer evaluacion de l y hacer el update del store en l-sto
     (def (v*s r-val r-sto) (interp r env l-sto)) ;; como ahora tenemos que interpertar el valor de la derecha para luego hacer la suma, necesitamos el l-sto, y lo guardamos en el r-sto
     (v*s (num+ l-val r-val) r-sto)] ;; ahora simplemente hacemos la suma de los valores junto con el store actualizado (r-sto)
    
    [(app fun-expr arg-expr) ;; aplicacion de funcion
     (def (v*s (closureV id body fenv) fun-sto) (interp fun-expr env sto)) ;; aqui se define el store de la clausura d la funcion junto con la interpretacion de la fun-expr y el sto
     (def (v*s arg-val arg-sto) (interp arg-expr env fun-sto)) ;; ahora tenemos que el los argumentos se interpretan junto con el fun-sto que fue updated en la linea anterior y se guarda en el nuevo store arg-sto
     (def new-loc (next-location arg-sto)) ;; aqui se agrega el nuevo loc al store
     (interp body
             (extend-env id new-loc fenv) ;; se extiende el env con el nuevo loc y el fenv, junto al id
             (extend-sto new-loc arg-val arg-sto))] ;; se extiende el sto con el nuevo loc, el arg-val y el arg-sto

    [(seqn expr1 expr2)
     (def (v*s _ sto1) (interp expr1 env sto)) ;; aqui funciona de la misma forma, primero se interpreta la expr 1 y se guarda en el sto1
     (interp expr2 env sto1)] ;; ahora se devuelve la interpretacion de la expr2 junto con la sto1

    [(newbox val-expr)
     (def (v*s val-val val-sto) (interp val-expr env sto)) ;; aqui se interpreta el val-expr y se guarda en el val-sto
     (def new-loc (next-location val-sto))  ;; aqui se agrega el nuevo loc al store
     (v*s (boxV new-loc)
          (extend-sto new-loc val-val val-sto))] ;; se extiende el sto con el nuevo loc, el val-val y el val-sto y se devuelve el v*s con el boxV y el nuevo sto

    [(openbox box-expr)
     (def (v*s (boxV loc) box-sto) (interp box-expr env sto))  ;; aqui se interpreta el box-expr y se guarda en el box-sto
     (v*s (lookup-sto loc box-sto) box-sto)] ;; aqui simplemente se devuelve el valor que se encuentra en el loc del box-sto

    [(setbox box-expr val-expr)
     (def (v*s (boxV loc) box-sto) (interp box-expr env sto)) ;; aqui se interpreta el box-expr y se guarda en el box-sto
     (def (v*s val-val val-sto) (interp val-expr env box-sto));; aqui se interpreta el val-expr con el box-sto y se guarda en el val-sto
     (v*s val-val
          (extend-sto loc val-val val-sto))] ;; aqui extendemos el sto con el loc y el nuevo val-sto y se devuelve el v*s con el val-val y el nuevo sto

    ))

(define (run e)
  (def (v*s result final-store) (interp (parse e) (empty-env) (empty-sto)))
  result)

(define p1 '{with {a {newbox 1}}
                  {with {f {fun {x} {+ x {openbox a}}}}
                        {seqn {setbox a 2} {f 5}}}})


;; Repaso
; El ambiente mapea los identificadores con las celdas de memoria ->>> esto mantiene el alcance estatico
; El Store mapea las celdas de memoria con los valores ->> traquea dianmicamente los cambios 
; Cada una de las mutaciones se deben propagarse


;; ----
;; 