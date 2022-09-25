#lang play

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
(deftype s-Cond
  (menor a x)
  (mayor b y)
  (igual c z)
  (and s-Cond s-Cond)
  (or s-Cond s-Cond))


#|
s-Cmd ::=
    |CREATE <sym> (list sym) s-Cmd
    |INSERT (list sym) <sym> s-Cmd
    |FROM <sym> SELECT regs WHERE s-Cond
|#
(deftype s-Cmd
  (create cr list s-Cmd)
  (insert list in s-Cmd)
  (from fr SELECT regs WHERE s-Cond)


#| PARTE B |#
;; parse :: s-Cmd -> Cmd


#| ==============================
            EJERCICIO 2
   ============================== |#

#| PARTE A |#
;; check-table :: Cmd -> Boolean / Error




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

