#lang play

#|
<TaskSchedule> ::= (task <String> <Integer> | (seq <TaskSchedule> <TaskSchedule>) | (par <TaskSchedule> <TaskSchedule>))
|#

;;Inductive definition of TaskSchedule
(deftype TaskSchedule
  [task name duration]
  [seq t1 t2]
  [par t1 t2]);

; ; is-in :: TaskSchedule string -> bool
;; funcion encargada de verificar si existe una tarea dentro del TaskSchedule

(define (is-in schedule name)
  (match schedule
    [(task n1 duration) (equal? n1 name) ]
    [(seq t1 t2) (or (is-in t1 name) (is-in t2 name))]
    [(par t1 t2) (or (is-in t1 name) (is-in t2 name))]
    ))


; ; length :: TaskSchedule -> integer
;; funcion encargada de devolver la duracion total del TaskSchedule

(define (length schedule)
  (match schedule
    [(task name duration) duration]
    [(seq t1 t2) (+ (length t1) (length t2))]
    [(par t1 t2) (max (length t1) (length t2))]
    ))

; ; longest :: TaskSchedule -> cons string integer
;; funcion encargada de entregar el nombre y duracion de la tarea mas larga del Taskschedule

(define (longest schedule)
  (match schedule
    [(task name duration) (cons name duration)]
    [(seq t1 t2) (if (> (cdr(longest t1)) (cdr(longest t2))) (longest t1) (longest t2))]
    [(par t1 t2) (if (> (cdr(longest t1)) (cdr(longest t2))) (longest t1) (longest t2))]
    ))


; ; sequest :: TaskSchedule -> integer
;; funcion encargada de entregar el largo de la secuencia mas larga del TaskSchedule
#|
(define (sequest schedule)
  (match schedule
    [(task name duration) duration]
    [(seq t1 t2)]))



; ; end-time :: TaskSchedule string -> integer
;; funcion encargada de devolver el instante de tiempo en que termina la tarea dada, que esta en el TaskSchedule

(define (end-time-aux schedule name)
  (match schedule
    [(task _ duration) duration]
    [(seq t1 t2) (+ (end-time-aux t1 name) (end-time-aux t2 name))]
    [(par t1 t2) 

(define (end-time schedule name) (if (is-in schedule name) (end-time-aux schedule name) (error "tarea no encontrada")))
|#

; ; fold :: (string integer-> A) (A A -> A) (A A -> A) -> (TaskSchedule -> A)

(define (fold tfun sfun pfun)
  (λ (schedule)
    (match schedule
    [(task name duration) (tfun name duration)]
    [(seq t1 t2) (sfun ((fold tfun sfun pfun) t1) ((fold tfun sfun pfun) t2))]
    [(par t1 t2) (pfun ((fold tfun sfun pfun) t1) ((fold tfun sfun pfun) t2))])))

; ; obtain-duration :: string integer -> integer
;; funcion encargada de obtener la duracion de una tarea en especifico
(define (obtain-duration n d) d)



;;(define (is-in2 name) (fold (is-in-aux2 name) (or) (or)))
  
(define length2 (fold obtain-duration  + max))

