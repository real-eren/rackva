#lang racket/base
(require "util/testing.rkt"
         "interpreter-extension.rkt")

(define test-file-v1 (make-tester interpret-v1-file))
(define test-file-v2 (make-tester interpret-v2-file))
(define test-file-v3 (make-tester interpret-v3-file))

;;;; tiny subset of tests to check that interpret handles file input correctly
(test-file-v1 11 "file-tests/v1/1.txt")

(test-file-v2 2000400 "file-tests/v2/1.txt")

(test-file-v3 420 "file-tests/v3/1.txt" #:args (list "A"))