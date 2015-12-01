#lang racket/base

(require "combi.rkt")

(apply + (for/list ([c (in-string (number->string (! 100)))]) (string->number (string c))))
