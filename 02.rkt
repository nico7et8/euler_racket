#lang racket/base
(require racket/stream)

(require "fibonacci.rkt")

(define (even-fibo-sum limit)
  (for/sum ([f (in-stream fibonacci)]
            #:when (even? f)
            #:break (>= f limit))
    f))

(even-fibo-sum 4e6)
