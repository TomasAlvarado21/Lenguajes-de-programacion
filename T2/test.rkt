#lang play
(require "T2.rkt")

(print-only-errors #t)


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
                                                 (FROM toki-toki-ti SELECT regs WHERE (< empanadas 4)))))))
      (CREATE 'toki-toki-ti '(mesa terremotos empanadas)
               (INSERT '(3 5 4) 'toki-toki-ti
                       (INSERT '(2 2 4) 'toki-toki-ti
                               (INSERT '(1 2 3) 'toki-toki-ti
                                       (FROM 'toki-toki-ti 'SELECT 'regs 'WHERE (< 'empanadas 4)))))))
