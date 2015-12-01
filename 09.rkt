#lang racket/base

(define (pytri? a b c)
  (and
    (< a b c)
    (= (+ (* a a) (* b b)) (* c c))))

(for*/first ([a (in-range 1000)]
             [b (in-range 1000)]
             #:when (and (> 1000 (+ a b))
                         (pytri? a b (- 1000 a b))))
            (* a b (- 1000 a b)))
