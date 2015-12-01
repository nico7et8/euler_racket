#lang racket/base

(define (sum-multiples multiples limit)
  (for/sum ([i (in-range limit)]
            #:when (for/or ([m (in-list multiples)])
                     (zero? (remainder i m))))
            i))

(sum-multiples '(3 5) 1000)
