#lang racket/base
(require rackunit
         "../src/interpreter.rkt")

;;;; tiny subset of tests to check that interpret handles file input correctly
(check-equal? (interpret (file-module "file-tests/v1/1.txt") #:mode mode:script)
              11)

(check-equal? (interpret (file-module "file-tests/v2/1.txt") #:mode mode:main-func)
              2000400)

(check-equal? (interpret (file-module "file-tests/v3/1.txt") #:mode (mode:class "A"))
              420)
