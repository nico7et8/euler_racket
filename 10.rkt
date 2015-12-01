#lang racket/base

(require "primes.rkt"
         racket/stream)

(define (sum-of-primes biggest)
  (for/sum ([p (in-stream primes)]
            #:break (> p biggest)) p))

(sum-of-primes 2000000)
