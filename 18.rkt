#lang racket/base
(require racket/file
         racket/list
         racket/function
         racket/string)

(define (file->tri-list a-path)
  (let ([string-list (map (curryr string-split " ")
                      (file->lines a-path))])
    (for/list ([l (in-list string-list)])
      (map string->number l))))

(define tri-list (file->tri-list "18-nums.txt"))

(define (merge-tri-lines line next-line)
  (if (= (add1 (length line)) (length next-line))
    (for/list ([(l1 i) (in-indexed line)])
      (max (+ l1 (list-ref next-line i))
           (+ l1 (list-ref next-line (add1 i)))))
    (error "merge-tri-lines : second list must be one element longer than first")))

(define (max-sum a-tri-list)
  (if (= 1 (length a-tri-list))
    (caar a-tri-list)
    (max-sum (append (drop-right a-tri-list 2)
                     (list (apply merge-tri-lines (take-right a-tri-list 2)))))))

(max-sum tri-list)
