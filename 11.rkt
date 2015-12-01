#lang racket/base
(require racket/file
         racket/function
         racket/list)

(define grid (file->list "11-grid.txt"))

(define (grid-ref x y)
  (list-ref grid (+ (* 20 y) x)))

(define (line-prod x y)
  (if (and (<= 0 x 16) (<= 0 y 19))
     (apply * (map (curryr grid-ref y) (range x (+ x 4))))
    0))

(define (col-prod x y)
  (if (and (<= 0 x 19) (<= 0 y 16))
    (apply * (map (curry grid-ref x) (range y (+ y 4))))
    0))

(define (rdiag-prod x y)
  (if (and (<= 0 x 16) (<= 0 y 16))
    (apply * (map (curry grid-ref) (range x (+ x 4)) (range y (+ y 4))))
    0))

(define (ldiag-prod x y)
  (if (and (<= 3 x 19) (<= 0 y 16))
    (apply * (map (curry grid-ref) (range x (- x 4) -1) (range y (+ y 4))))
    0))

(define prods (list line-prod col-prod rdiag-prod ldiag-prod))

(for*/fold ([max-prod 0]) ([i (in-range 20)] [j (in-range 20)])
  (apply max (cons max-prod (map (λ(f) (f i j)) prods))))
