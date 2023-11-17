#lang play

(define (my-time e)
  (let ([begin-time (current-milliseconds)])
    (begin
      e
      (- (current-milliseconds) begin-time))))

(define (my-time2 e-thunk)
  (let ([begin-time (current-milliseconds)])
    (begin
      (e-thunk)
      (- (current-milliseconds) begin-time))))

(defmac (my-time3 e)
  (let [(begin-time (current-milliseconds))]
    (begin
      e
      (- (current-milliseconds) begin-time))))
;; dame una funcion que tome mucho tiempo 
(define (my-fun)
  (let loop ([n 1000000])
    (if (zero? n)
        0
        (loop (- n 1)))))


(my-time (my-fun))

(defmac (check c then e1 else e2)
  #:keywords then else
  (if c e1 e2))

(define (my-or-fun e1 e2)
  (let ([result e1])
    (if result
        result
        e2)))

(define (my-or-fun-thunks e1 e2-thunk)
  (let ([result e1])
    (if result
        result
        (e2-thunk))))

(defmac (my-or e1 e2)
  (let ([result e1])
    (if result
        result
        e2)))

(let ([result #t])
    (my-or #f result))


(define count-1
(lambda ()
(let ([b (box 0)])
(begin
(set-box! b (+ 1 (unbox b)))
(unbox b)))))

(define count-2
(let ([b (box 0)])
(lambda ()
(begin
(set-box! b (+ 1 (unbox b)))
(unbox b)))))


(define (count-3)
(let ([b (box 0)])
(begin
(set-box! b (+ 1 (unbox b)))
(unbox b))))


(define L1 (list 1 2 3 4 5))
(define L2 (list 6 7 10))

(define (inter-aux L1 L2 acc)
  (match L1
    [(list) (append acc L2)]
    [(cons x rest) (inter-aux L2 rest (append acc (list x)))]))

(define (inter-xd L1 L2 acc)







(define (inter L1 L2 acc)
  (match L1
    [(list) (append acc L2)]
    [(cons x rest1) (match L2
                      [(list) (append acc L1)]
                      [(cons y rest2) (inter rest1 rest2 (append acc (list x y)))] )]))

(inter L1 L2 (list))
(inter-aux L1 L2 (list))