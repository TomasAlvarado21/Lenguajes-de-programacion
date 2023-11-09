#lang play
(require "T3.rkt")

(print-only-errors #t)


(parse-type 'Number)

(parse-type ' (-> Number Number))

(parse-type ' (-> (-> Number Number) Number))

