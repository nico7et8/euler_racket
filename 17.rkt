#lang racket/base

(define units (list "" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
(define teens (list  "ten" "eleven" "twelve" "thriteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))
(define dozens (list "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))

(define (number->word n [sep " "])
  (cond
    [(= n 0) "zero"]
    [(< 0 n 20) (list-ref (append units teens) n)]
    [(<= 20 n 99) (string-append (list-ref dozens (- (quotient n 10) 2))
                                 sep
                                 (list-ref units (remainder n 10)))]
    [(<= 100 n 999) (string-append (list-ref units (quotient n 100))
                                   sep "hundred" 
                                   (if (= (remainder n 100) 0)
                                     ""
                                     (string-append sep "and" sep (number->word (remainder n 100) sep))))]
    [(= n 1000) (string-append "one" sep "thousand")]))

(for/sum ([i (in-range 1 1001)])
  (string-length (number->word i "")))
