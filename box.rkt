#lang racket

(define b (box 10))

(unbox b)

(set-box! b 20)

(unbox b)