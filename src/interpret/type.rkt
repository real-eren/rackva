#lang racket/base
(require "state/instance.rkt")
(provide type-of)

;;;; functions related to the type system

(define (type-of val)
  (cond
    [(boolean? val)      'boolean]
    [(number? val)       'int]
    [(is-instance? val)  (instance:class val)]))

(module+ test
  (require rackunit)
  (check-equal? (type-of 4)  'int)
  (check-equal? (type-of #T) 'boolean)
  (check-equal? (type-of #F) 'boolean)
  (check-equal? (type-of '((fields) (class . A))) 'A)
  (check-equal? (type-of '((fields (k . v) (a . b)) (class . Foo))) 'Foo))
