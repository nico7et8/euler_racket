#lang racket/base

(define (palindrome? a-string)
  (let ([len (string-length a-string)])
    (if (<= len 1)
      #t
      (if (char=? (string-ref a-string 0)
                  (string-ref a-string (sub1 len)))
        (palindrome? (substring a-string 1 (sub1 len)))
        #f))))

(apply max
       (for*/list ([i (in-range 1000 0 -1)]
                   [j (in-range 1000 0 -1)]
                   #:when (palindrome? (number->string (* i j))))
         (* i j)))

