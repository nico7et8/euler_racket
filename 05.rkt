#lang racket/base

(require racket/list)

(define (smallest-multiple limit)
  (apply lcm (range 2 limit)))

(smallest-multiple 21)

#|
SOLUTION WITHOU LCM

(require "primes.rkt")

(define (merge-factors factors new-factors)
  (let loop ([factors factors]
             [new-factors new-factors]
             [added '()])
    (if (empty? new-factors)
      (append factors added)
      (let ([candidate (car new-factors)])
        (if (member candidate factors)
          (loop (remove candidate factors)
                (remove candidate new-factors)
                (cons candidate added))
          (loop factors
                (remove candidate new-factors)
                (cons candidate added)))))))

(define (smallest-multiple limit)
  (apply * (foldl merge-factors 
                  '() 
                  (map prime-factors (range 2 limit)))))

(smallest-multiple 20)
|#
