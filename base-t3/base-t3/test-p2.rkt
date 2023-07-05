#lang play
(require "main-p2.rkt")
(print-only-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 TESTS BASE                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run-val '{+ 1 2}) 3)
(test (run-val '{< 1 2}) #t)
(test (run-val '{- 2 1}) 1)
(test (run-val '{* 2 3}) 6)
(test (run-val '{= {+ 2 1} {- 4 1}}) #t)
(test (run-val '{and #t #f}) #f)
(test (run-val '{or #t #f}) #t)
(test (run-val '{not {not #t}}) #t)
(test (run-val '{if {not #f} {+ 2 1} 4}) 3)
(test (run-val '{with {{x 5} {y 3}}
              {begin {+ x 1}
                    {+ x y}}}) 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                TESTS ENUNCIADO                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|RECUERDE DESCOMENTAR LOS TEST|#

;; el método f se busca en c y luego en su superclase
#; (test (run-val '{with {{A {class {} 
                           {def f {z} {< z 7}}}}
                   {B {class : A {}}}
                   {o {new B {}}}}
              {-> o f 20}})
         #f)

;; llamada a super de método no definido en el padre directo
#; (test (run-val '{with {{A {class {}
                         {def h {x} {+ x 1}}}}
                 {B {class : A {}
                         {def f {} #f}}}
                 {C {class : B {}                     
                         {def g {} {super h 10}}
                         {def h {} {+ 1 x}}}}
                 {o {new C {}}}}
            {-> o g}})
         11)

#; (test (run-val '{with {{A {class {x y}
                           {def init {} {begin {set x 1} {set y 0}}}
                           {def ax {} {get self x}}}}
                   {B {class : A {x}
                           {def init {} {set x 2}}
                           {def bx {} {get self x}}}}
                   {b {new B {}}}}
              {-> b ax}})
         1)

;; No se puede usar super fuera de un método
#;(test/exn (run-val 'super)
            "error: super outside method exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  SUS TESTS                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

