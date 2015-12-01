#lang racket/base

(require racket/stream)

(provide (all-defined-out))

(define (stream-take a-stream n)
  (let loop ([s a-stream]
             [l '()]
             [n n])
    (if (= 0 n)
      l
      (loop (stream-rest s)
            (append l (list (stream-first s)))
            (sub1 n)))))

             
