#lang racket/base

(define (divisors n)
  (let ([max-div (add1 (if (even? n) (quotient n 2) (quotient n 3)))])
    (for/list ([i (in-range 1 max-div)] #:when (zero? (remainder n i))) i)))

(define (sum-divisors n)
  (apply + (divisors n)))

(define (amicable? n)
  (let ([r (sum-divisors n)])
  (and (not (= r n)) 
    (= (sum-divisors r) n))))

(for/sum ([i (in-range 1 10000)] #:when (amicable? i)) i)
