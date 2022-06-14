#lang racket/base
(require "state/instance.rkt"
         "state/state.rkt")
(provide type-of
         compatible?
         (prefix-out type: (combine-out any bool int)))

;;;; constants and functions related to the type system

(define any 'any)
(define bool 'bool)
(define int 'int)

(define (type-of val)
  (cond
    [(boolean? val)      bool]
    [(number? val)       int]
    [(is-instance? val)  (instance:class val)]))

(module+ test
  (require rackunit)
  (define sample-object-1 '((fields) (class . A)))
  (define sample-object-2 '((fields ((k . v)) ((a . 5))) (class . Foo)))
  (check-equal? (type-of 4)  int)
  (check-equal? (type-of #T) bool)
  (check-equal? (type-of #F) bool)
  (check-equal? (type-of sample-object-1) 'A)
  (check-equal? (type-of sample-object-2) 'Foo))

; is v accepted where a value of type t is required?
(define (compatible? v t context state)
  (not (not (case t
              [(any)       #T]
              [(bool)      (boolean? v)]
              [(int)       (number? v)]
              ; else object
              [else        (and (is-instance? v)
                                (state:subclass? (type-of v) t context state))]))))

(module+ test
  (define-check (check-all-true pred vs)
    (when (not (null? vs))
      (check-true (pred (car vs)))
      (check-all-true pred (cdr vs))))
  (check-true (compatible? 5 int #F new-state))
  (check-true (compatible? 0 int #F new-state))
  (check-true (compatible? -5 int #F new-state))
  (check-true (compatible? #T bool #F new-state))
  (check-true (compatible? #F bool #F new-state))

  (check-all-true (λ (v) (not (compatible? v bool #F new-state)))
                  '(5 0 -5 sample-object-1 sample-object-2))
  (check-all-true (λ (v) (not (compatible? v int #F new-state)))
                  '(#T #F sample-object-1 sample-object-2))
  (check-all-true (λ (v) (not (compatible? v 'A #F new-state)))
                  '(5 0 -5 #T #F sample-object-2))
  (check-all-true (λ (v) (compatible? v any #F new-state))
                  '(5 0 -5 #T #F sample-object-1 sample-object-2)))
