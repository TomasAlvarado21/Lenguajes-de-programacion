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
(deftype s-Cmd
  (CREATE tab lista s-Cmd)
  (INSERT lista in s-Cmd)
  (FROM symb SELECT regs WHERE s-Cond))




#| PARTE B |#
;; parse :: s-Cmd -> Cmd

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

(test (parse-Cond '(#t orb #f)) #t)
(test (parse-Cond '(#f & #f)) #f)
(test (parse-Cond '(= a 1)) (= 'a 1))
(test (parse-Cond '(< b 3)) (< 'b 3))
(test (parse-Cond '(> c 12)) (> 'c 12))

(test (parse-Cmd '(CREATE nombre (a b c)
                          (FROM s SELECT regs WHERE (= s 1))))
      (CREATE 'nombre '(a b c)
              (FROM 's 'SELECT 'regs 'WHERE (= 's 1))))


(test (parse-Cmd '(FROM s SELECT regs WHERE (= s 1))) (FROM 's 'SELECT 'regs 'WHERE (= 's 1)))


(test (parse-Cmd '(INSERT (3 2 1) blabla
                          (FROM blabla SELECT regs WHERE (= blabla 1))))
      (INSERT '(3 2 1) 'blabla
              (FROM 'blabla 'SELECT 'regs 'WHERE (= 'blabla 1))))



(test (parse-Cmd '(CREATE toki-toki-ti (mesa terremotos empanadas)
                         (INSERT (3 5 4) toki-toki-ti
                                 (INSERT (2 2 4) toki-toki-ti
                                         (INSERT (1 2 3) toki-toki-ti
                                                 (FROM toki-toki-ti SELECT regs WHERE (= empanadas 4)))))))
      (CREATE 'toki-toki-ti '(mesa terremotos empanadas)
               (INSERT '(3 5 4) 'toki-toki-ti
                       (INSERT '(2 2 4) 'toki-toki-ti
                               (INSERT '(1 2 3) 'toki-toki-ti
                                       (FROM 'toki-toki-ti 'SELECT 'regs 'WHERE (= 'empanadas 4)))))))
             

#| ==============================
            EJERCICIO 2
   ============================== |#

#| PARTE A |#
;; check-table :: Cmd -> Boolean / Error

(define (check-table Cmd)
  (match parse
    []))


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

