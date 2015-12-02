#lang typed/racket/base

(: divide? (-> Flonum Flonum Boolean))
(define (divide? x y)
   (let ([r (/ x y)])
     (< (abs (- r (floor r))) 1e-6)))

(: divisors (-> Flonum (Listof Flonum)))
(define (divisors n)
  (let: loop ([divs : (Listof Flonum) '()]
             [candidate : Flonum 1.0])
    (if (> candidate (/ n 2.0))
      divs
      (if (divide? n candidate)
        (loop (cons candidate divs) (+ 1.0 candidate))
        (loop divs (+ 1.0 candidate))))))

(: sum-divisors (-> Flonum Flonum))
(define (sum-divisors n)
  (foldl + 0.0  (divisors n)))

(: amicable? (-> Flonum Boolean))
(define (amicable? n)
  (let ([r (sum-divisors n)])
    (and (not (= r n))
         (= (sum-divisors r) n))))

(let: loop ([i : Flonum 1.0]
            [sum : Flonum 0.0])
      (if (>= i 10000.0)
        sum
        (if (amicable? i)
          (loop (+ 1.0 i) (+ sum i))
          (loop (+ 1.0 i) sum))))
