#lang play
(require "T3.rkt")

(print-only-errors #t)


(test (parse-type 'Number) (numT))

(test (parse-type ' (-> Number Number)) (arrowT (numT) (numT)))

(test (parse-type ' (-> (-> Number Number) Number)) (arrowT (arrowT (numT) (numT)) (numT)))

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



(test (final? (num 1)) #t)
(test (final? (fun 'x (numT) (id 'x))) #t)
(test (final? (binop '+ (num 1) (num 2))) #f)

(test (step (st (binop '+ (num 1) (num 2)) (mtEnv) (mt-k))) (st (num 1) (mtEnv) (binop-r-k '+ (num 2) (mtEnv) (mt-k)))) 
(test (step (st (num 1) (mtEnv) (binop-r-k '+ (num 2) (mtEnv) (mt-k)))) (st (num 2) (mtEnv) (binop-l-k '+ (num 1) (mtEnv) (mt-k))))
(test (step (st (num 2) (mtEnv) (binop-l-k '+ (num 1) (mtEnv) (mt-k)))) (st (num 3) (mtEnv) (mt-k)))

(test (step (st (app (fun 'x (numT) (id 'x)) (num 2)) (mtEnv) (mt-k))) (st (fun 'x (numT) (id 'x)) (mtEnv) (arg-k (num 2)(mtEnv)(mt-k))))

;; primer step
(test (step (st (app (fun 'x (numT) (id 'x)) (num 2))
(mtEnv)
(mt-k))) 
(st (fun 'x (numT) (id 'x)) (mtEnv) (arg-k (num 2)
(mtEnv)
(mt-k))))


