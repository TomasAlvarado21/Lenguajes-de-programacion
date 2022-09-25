#lang play
(require "T1.rkt")
(print-only-errors #t)

;;definicion de schedules para los test

(define schedule (seq (par (seq (task "t1" 2) (task "t2" 4)) (task "t3" 3))(par (seq (task "t4" 2) (par (task "t6" 2) (task "t7" 1))) (task "t5" 6))))
(define schedule1 (seq (par (seq (task "t1" 2) (task "t2" 4)) (task "t3" 3))(seq (task "t4" 2) (par (task "t6" 2) (task "t7" 1)))))

;;Test is-in

(test (is-in schedule "t1") #t)
(test (is-in schedule "collect") #f)
(test (is-in schedule1 "t3") #t)
(test (is-in schedule1 "t5") #f)

;;Test length

(test (length schedule) 12)
(test (length schedule1)10)

;;Test longest

(test (longest schedule) (cons "t5" 6))
(test (longest schedule1) (cons "t2" 4))

;;Test length2 (version con fold)

(test (length2 schedule) 12)
(test (length2 schedule1) 10)



