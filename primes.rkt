#lang racket/base
(require racket/list
         racket/stream)

(provide (all-defined-out))

(define primes
  (stream-cons
    2
    (let loop ([candidate 3])
      (define limit (integer-sqrt candidate))
      (if
        (for/or ([p (in-stream primes)]
                 #:break (> p limit))
          (zero? (remainder candidate p)))
        (loop (+ 2 candidate))
        (stream-cons candidate (loop (+ 2 candidate)))))))

(define (prime? n)
  (and
    (> n 1)
    (for/and ([p (in-stream primes)]
              #:break (> p (integer-sqrt n)))
      (not (zero? (remainder n p))))))

(define (prime-factors n)
  (let loop ([n n]
             [factors '()]
             [primes primes])
    (define p (stream-first primes))
    (if (> p (/ n (if (even? n) 2 3)))
      (if (= n 1) factors (cons n factors))
      (if (zero? (remainder n p))
        (loop (/ n p) (cons p factors) primes)
        (loop n factors (stream-rest primes))))))

(define (unique-prime-factors n)
  (remove-duplicates (prime-factors n)))

(define (prime-factor-decomp n)
  (define factors (prime-factors n))
  (for/list ([f (in-list (remove-duplicates factors))])
    (cons f (- (length factors) (length (remove* (list f) factors))))))

