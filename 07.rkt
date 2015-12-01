#lang racket/base

(require racket/stream
         "primes.rkt")

(stream-ref primes 10000)
