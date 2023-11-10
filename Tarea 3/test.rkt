#lang play
(require "T3.rkt")

(print-only-errors #t)


(parse-type 'Number)

(parse-type ' (-> Number Number))

(parse-type ' (-> (-> Number Number) Number))

(test (parse-type 'Number) (numT))
(test (parse-type 'Boolean) (boolT))
(test (parse-type '(-> Number Boolean)) (arrowT (numT) (boolT)))


(test/exn (infer-type (app (fun 'x (numT) (id 'x)) (fun 'x (numT) (id 'x))) empty-tenv) "function argument type mismatch")
(test (parse-type 'Boolean) (boolT))
(test (parse ' ( if (<= 5 6) true false )) (ifc (binop '<= (num 5) (num 6)) (tt) (ff)))

(test (infer-type ( ifc (binop '<= (num 5) (num 6)) (num 2) (num 3))empty-tenv) (numT))
(test/exn (infer-type ( ifc (num 5) (num 2) (num 3)) empty-tenv) "infer-type: if condition must be a boolean")

(test/exn (infer-type ( ifc (binop '<= (num 5) (num 6)) (num 2) (tt)) empty-tenv) "infer-type: if branches type mismatch")


(test (infer-type (num 1) empty-tenv) (numT))
(test (infer-type (fun 'x (numT) (id 'x)) empty-tenv) (arrowT (numT) (numT)))
(test (infer-type (fun 'x (arrowT (numT) (numT)) (id 'x)) empty-tenv) (arrowT (arrowT (numT) (numT)) (arrowT (numT) (numT))))
(test/exn (infer-type (binop '+ (num 1) (fun 'x (numT) (id 'x))) empty-tenv) "invalid operand type for +")
(test/exn (infer-type (app (num 1) (num 2)) empty-tenv) "function application to a non-function")



(final? (num 1))
(final? (fun 'x (numT) (id 'x)))
(final? (binop '+ (num 1) (num 2)))