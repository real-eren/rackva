#lang racket/base
(require rackunit
         "interpreter-extension.rkt"
         "interpreter.rkt")

(define i1 interpret-v1-file)
(define i2 interpret-v2-file)
(define i3 interpret)

;;;; tiny subset of tests to check that interpret handles file input correctly
(check-equal? (i1 "file-tests/v1/1.txt")
              11)

(check-equal? (i2 "file-tests/v2/1.txt")
              2000400)

(check-equal? (i3 "file-tests/v3/1.txt" "A")
              420)
