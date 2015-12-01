#lang racket/base
(require racket/file)

(define nums (file->list "13-nums.txt"))

(string->number (substring (number->string (apply + nums)) 0 10))
