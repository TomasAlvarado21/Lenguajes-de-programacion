#lang play
(require "main-p1.rkt")
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

;; comportamiento esperado
#;(test (run-val '{with {{c {class {x y}
                        {def init {}
                          {begin {set x 1} {set y 2}}}
                        {def init {init-x init-y}
                          {begin {set x init-x} {set y init-y}}}  
                        {def sum {z} {+ {get self x} {+ {get self y} z}}}
                        {def set-x {val} {set x val}}}}
                   {o {new c {3 4}}}}
              {begin
                {-> o set-x {+ 1 3}}
                {+ {-> o sum 3} {get o y}}}})
        15)

;; las clases son valores
#; (test (run-val '{with {{A {class {}
                        {def apply {c} {-> {new c {}} m}}}}
                   {o {new A {}}}}
              {-> o apply {class {x}
                            {def init {} {set x 2}}
                            {def m {} {get self x}}}}})
         2)

;;la definición de la clase tiene scope léxico
#; (test/exn (run-val '{begin {with {{A {class {x}
                               {def init {} {set x 2}}
                               {def init {init-x} {set x init-x}}
                               {def m {} {get self x}}}}}
                     10}
                   {new A {}}})
         "free identifier: A")

;; los identificadores dentro de una clase tienen scope léxico 
;; (note el uso de la “x” en la definición del método “m”
 (test (run-val '{with {{x 10}
                   {A {class {}
                        {def m {y} {+ x y}}}}
                   {o {new A {}}}}
              {-> o m 1}})
         11)

;; No se puede usar set fuera de un método
#; (test/exn (run-val '{set x 1})
             "error: set outside method exception")

;; No se puede usar self fuera de un método
#; (test/exn (run-val 'self)
             "error: self outside method exception")

;; Acceder a un campo no definido de una clase
 (test/exn (run-val '{with {{A {class {}}}
                   {o {new A {}}}}
              {get o m}})
             "error: field not found exception")

;; Acceder a un campo no inicializado de una clase
 (test/exn (run-val '{with {{A {class {x y} {def init {init-x init-y} {set x init-x}}}}
                   {o {new A {1 2}}}}
              {get o y}})
             "error: field not initialized exception")

;; Invocar un método no definido de una clase
#; (test/exn (run-val '{with {{A {class {}}}
                   {o {new A {}}}}
              {-> o m}})
             "error: method not found exception")

;; Una clase sin constructores puede ser creado solo con {new class}, sin argumentos
#; (test/exn (run-val '{with {{x 10}
                   {A {class {x}}}
                   {o {new A {x}}}}
              1})
             "error: constructor not found exception")

;; Tener 2 init con la misma aridad es un error en tiempo de creación de la clase
#; (test/exn (run-val '{begin {with {{A {class {x}
                               {def init {init-x} {set x init-x}}
                               {def init {init-x} {set x 12}}}}}
                     10}
                   {new A {}}})
             "error: same arity constructor exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  SUS TESTS                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ejemplo parser
; (parse '{with {{x 5} {y 3}}
;           {begin {+ x 1}
;                {+ x y}}})
; (parse '{- 2 1})
; (parse '{= {+ 2 1} {- 4 1}})
; (parse '{and #t #f})
; (parse '{or #t #f})
; (parse '{not {not #t}})
; (parse '{if {not #f} {+ 2 1} 4})
; (parse '{< 1 2})
; (parse '{* 2 3})
; parse con las clases de ejemplo
(run-val '{with {{c {class {x y}
                      {def init {}
                        {begin {set x 1} {set y 2}}}
                      {def init {init-x init-y}
                        {begin {set x init-x} {set y init-y}}}  
                      {def sum {z} {+ {get self x} {+ {get self y} z}}}
                      {def set-x {val} {set x val}}}}
                 {o {new c {3 4}}}}
                {begin
                  {-> o set-x {+ 1 3}}
                  {+ {-> o sum 3} {get o y}}}})