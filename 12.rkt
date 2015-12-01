#lang racket
(require "primes.rkt"
         racket/list)

(define (tri-factor-decomp i)
  (match 
    (append (prime-factor-decomp i) (prime-factor-decomp (add1 i)))
    [(list-no-order (cons 2 1) r ...) r]
    [(list-no-order (cons 2 n) r ...) (cons (cons 2 (sub1 n)) r)]))

(define (get-exponents factor-decomp)
  (let loop ([fd factor-decomp]
             [exps '()])
    (if (empty? fd)
      exps
      (loop (rest fd) (cons (cdar fd) exps)))))

(define (tri-divisor-number i)
  (apply * (map add1 (get-exponents (tri-factor-decomp i)))))

(for/first ([i (in-naturals 1)]
            #:when (> (tri-divisor-number i) 500))
           (* i (add1 i) 1/2))
