#lang racket/base

(apply + (map (compose string->number string) (string->list (number->string (expt 2 1000)))))
