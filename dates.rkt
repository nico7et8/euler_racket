#lang racket

(provide (all-defined-out))

(define (valid-year? year)
  (and (exact-positive-integer? year) (>= year 1582) (<= year 3000)))

(define (valid-month? month)
  (and (exact-positive-integer? month) (<= month 12)))

(define (valid-day? day)
  (and (exact-positive-integer? day) (<= day 31)))

(define (leap-year? year)
  (and (valid-year? year)
       (cond
         [(zero? (remainder year 400)) #t]
         [(zero? (remainder year 100)) #f]
         [(zero? (remainder year 4)) #t]
         [else #f])))

(define (month-days month [year 2001])
  (let ([31-days-months (list 1 3 5 7 8 10 12)]
        [30-days-months (list 4 6 9 11)])
    (cond
      [(not (valid-month? month)) #f]
      [(member month 31-days-months) 31]
      [(member month 30-days-months) 30]
      [else (if (leap-year? year) 29 28)])))

(define (valid-date? day month year)
  (and
   (valid-day? day) (valid-month? month) (valid-year? year)
   (<= day (month-days month year))))

(define (next-month month)
  (cond
    [(not (valid-month? month)) #f]
    [(equal? month 12) 1]
    [else (add1 month)]))

(define (prev-month month)
  (cond
    [(not (valid-month? month)) #f]
    [(equal? month 1) 12]
    [else (sub1 month)]))

(struct date (day month year)
  #:transparent
  #:guard (lambda (day month year name)
            (if (valid-date? day month year)
                (values day month year)
                (error "Invalid date !"))))

(define (morrow a-date)
  (cond
    [(valid-date? (add1 (date-day a-date)) (date-month a-date) (date-year a-date))
     (struct-copy date a-date [day (add1 (date-day a-date))])]
    [(equal? (date-month a-date) 12) 
     (date 1 1 (add1 (date-year a-date)))]
    [else
     (struct-copy date a-date [day 1] [month (next-month (date-month a-date))])]))

(define (f^n f n)
  (define (iter g n)
    (if (equal? n 0) g
        (iter (lambda (x) (f (g x))) (sub1 n))))
  (iter (lambda (x) x) n))

(define (morrow^n n a-date)
  ((f^n morrow n ) a-date))

(define (yester a-date)
  (cond
    [(valid-date? (sub1 (date-day a-date)) (date-month a-date) (date-year a-date))
     (struct-copy date a-date [day (sub1 (date-day a-date))])]
    [(equal? (date-month a-date) 1) 
     (date 31 12 (sub1 (date-year a-date)))]
    [else
     (struct-copy date a-date
                  [day (month-days (prev-month (date-month a-date)) (date-year a-date))] 
                  [month (prev-month (date-month a-date))])]))

(define (yester^n n a-date)
  ((f^n yester n ) a-date))

(define (before? date1 date2)
  (cond
    [(> (date-year date1) (date-year date2)) #f]
    [(< (date-year date1) (date-year date2)) #t]
    [else
     (cond
       [(> (date-month date1) (date-month date2)) #f]
       [(< (date-month date1) (date-month date2)) #t]
       [else
        (cond
          [(> (date-day date1) (date-day date2)) #f]
          [(< (date-day date1) (date-day date2)) #t]
          [else #f])])]))

(define (after? date1 date2)
  (if (equal? date1 date2) #f (not (before? date1 date2))))

(define (date-stream date [step 1])
  (let ([next 
         (if (positive? step)
             (lambda (d) (morrow^n step d))
             (lambda (d) (yester^n (- step) d)))])
    (stream-cons date (date-stream (next date) step))))

(struct in-date-range (date1 date2 step)
  #:methods gen:stream
  [(define (stream-empty? stream)
     (cond
       [(positive? (in-date-range-step stream))
        (or (after? (in-date-range-date1 stream) (in-date-range-date2 stream))
            (equal? (in-date-range-date1 stream) (in-date-range-date2 stream)))]
       [(negative? (in-date-range-step stream))
        (or (before? (in-date-range-date1 stream) (in-date-range-date2 stream))
            (equal? (in-date-range-date1 stream) (in-date-range-date2 stream)))]))
   (define (stream-first stream)
     (in-date-range-date1 stream))
   (define (stream-rest stream)
     (let ([next 
            (if (positive? (in-date-range-step stream))
                (lambda (d) (morrow^n (in-date-range-step stream) d))
                (lambda (d) (yester^n (- (in-date-range-step stream)) d)))])
       (in-date-range (next (in-date-range-date1 stream))
                      (in-date-range-date2 stream)
                      (in-date-range-step stream))))])

(define (in-dates date1 date2 [step 1])
  (in-date-range date1 date2 step))

(define (date-range date1 date2 [step 1])
  (for/list ([date (in-dates date1 date2 step)]) date))

(define (days-between date1 date2)
  (cond
    [(before? date1 date2) (length (date-range date1 date2))]
    [(after? date1 date2) (-(length (date-range date2 date1)))]
    [else 0]))

(define (date-counter a-date)
  (lambda ()
    (set! a-date (morrow a-date))
    a-date))
