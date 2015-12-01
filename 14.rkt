#lang racket/base

(define (next-collatz n)
  (if (even? n) (/ n 2) (+ 1 (* 3 n))))

(define (collatz-steps n)
  (let loop ([steps 1]
             [n n])
    (if (= n 1)
      steps
      (loop (add1 steps) (next-collatz n)))))

(define-values (euler14 maxlen)
  (for/fold ([index 1] [biggest 1]) ([i (in-range 1 1000000)])
    (let ([steps (collatz-steps i)])
      (if (> steps biggest)
        (values i steps)
        (values index biggest)))))

euler14
