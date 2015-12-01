#lang racket/base

(require "primes.rkt")

(apply max (prime-factors 600851475143))
