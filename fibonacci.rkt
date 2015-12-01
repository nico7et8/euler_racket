#lang racket/base
(require racket/stream "streams.rkt")

(provide (all-defined-out))

(define fibonacci
  (let loop ([a 1] [b 1])
    (stream-cons a (loop b (+ a b)))))

