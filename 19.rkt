#lang racket/base

(define (leap-year? a-year)
  (cond
    [(zero? (remainder a-year 400)) #t]
    [(zero? (remainder a-year 100)) #f]
    [(zero? (remainder a-year 4)) #t]
    [else #f]))

(define (month-days a-month a-year)
  (cond
    [(member a-month (list 1 3 5 7 8 10 12)) 31]
    [(member a-month (list 4 6 9 11)) 30]
    [(= a-month 2) (if (leap-year? a-year) 29 28)]))

(length 
  (for/list
    ([day (in-list
            (for*/list ([year (in-range 1901 2000)]
                        [month (in-range 1 13)]
                        [day (in-range 1 (add1 (month-days month year)))])
              day))]
     [week-day (in-cycle 7)]
     #:when (and (= day 1) (= week-day 6)))
    day))
