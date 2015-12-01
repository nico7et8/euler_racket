#lang racket/base

(require racket/function
         racket/list)

(define (sum-of-squares limit)
  (apply + (map (curryr expt 2) (range (add1 limit)))))

(define (square-of-sum limit)
  (expt (apply + (range (add1 limit))) 2))

(define (sum-diff limit)
  (- (square-of-sum limit) (sum-of-squares limit)))

(sum-diff 100)
