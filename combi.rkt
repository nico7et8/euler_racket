#lang racket/base

(provide (all-defined-out))

(define (! n)
  (let loop ([n n]
             [agg 1])
    (if (zero? n)
      agg
      (loop (sub1 n) (* agg n)))))

(define (A n p)
  (/ (! n) (! (- n p))))

(define (C n p)
  (/ (A n p) (! p)))
