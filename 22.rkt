#lang racket/base

(require racket/file
         racket/string)

(define s (file->string "22-names.txt"))
(string-split s "\",\"")
 
