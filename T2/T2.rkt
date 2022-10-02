#lang play
(print-only-errors #t)
#| ==============================
            EJERCICIO 1
   ============================== |#

#| PARTE A |#

#|
s-Cond ::=
    | < sym num
    | > sym num
    | = sym num
    | & s-Cond s-Cond
    | or s-Cond s-Cond
|#
;;Aqui se define el deftype de la expresion s-Cond

(deftype s-Cond
  (< a x)
  (> b y)
  (= c z)
  (& Cond1 Cond2)
  (orb Cond1 Cond2))


#|
s-Cmd ::=
    |CREATE <sym> (list sym) s-Cmd
    |INSERT (list sym) <sym> s-Cmd
    |FROM <sym> SELECT regs WHERE s-Cond
|#
;;Aqui se define el deftype de la expresion s-Cmd

(deftype s-Cmd
  (CREATE tab lista s-Cmd)
  (INSERT lista in s-Cmd)
  (FROM symb SELECT regs WHERE s-Cond))




#| PARTE B |#
;; parse :: s-Cmd -> Cmd
;;Aqui definimos el parse para la expresion s-Cmd que nos deberia retornar un Cmd

(define (parse-Cmd s-Cmd)
  (match s-Cmd
    [(list 'CREATE tab lista s-Cmd) (CREATE tab lista (parse-Cmd s-Cmd))]
    [(list 'INSERT lista in s-Cmd) (INSERT lista in (parse-Cmd s-Cmd))]
    [(list 'FROM symb 'SELECT 'regs 'WHERE s-Cond) (FROM symb 'SELECT 'regs 'WHERE (parse-Cond s-Cond))]))



;; parse :: s-Cond -> Cond

(define (parse-Cond s-Cond)
  (match s-Cond
    [(list '< a x) (< a x)]
    [(list '> b y) (> b y)]
    [(list '= c z) (= c z)]
    [(list Cond1 '& Cond2) (and Cond1 Cond2)]
    [(list Cond1 'orb Cond2) (or Cond1 Cond2)]))

             

#| ==============================
            EJERCICIO 2
   ============================== |#
;; <env> ::= mtEnv
;; | (aEnv <tabs> <cols-name> <cols> <env>)
;; Aqui definimos el ambiente sobre el cual trabajaremos, tendra las tablas, los nombres de las columnas,
;; las columnas y el ambiente
(deftype env
  (mtEnv)
  (aEnv tabs cols-name cols env))


#| PARTE A |#
;; check-table :: Cmd -> Boolean / Error
;; esta funcion sirve para verificar si la tablas existen o no

(define (check-table Cmd)
  (match Cmd
    [(CREATE tab lista Cmd)(if (eq? INSERT-in tab)#t ())]))

;Empty-env
(define empty-env  (mtEnv))

;Extend-env
(define extend-env aEnv) 

;Env-lookup LISTO
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv tabs cols)
     (if (eq? tabs x)
         val
         (env-lookup x rest))]))
#| PARTE B |#
;; check-arity :: Cmd -> Boolean / Error



#| PARTE C |#
;; check-column :: Cmd -> Boolean / Error



#| PARTE D |#
;; static-check :: Cmd -> Boolean / Error



#| PARTE F |#
;; end-time :: TaskSchedule string -> integer



#| ==============================
            EJERCICIO 3
   ============================== |#

#| PARTE A |#
;; interp-cmd :: Cmd Env -> List[Reg] 

 
#| PARTE B |#
;; run :: <s-Cmd> -> List[Reg] / Error

