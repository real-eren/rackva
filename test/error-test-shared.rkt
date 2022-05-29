#lang racket/base
(require "../src/state/state.rkt"
         "../src/user-errors.rkt"
         rackunit
         racket/list)

(provide cs-types
         test-user-exn
         check-exn-result)


(define (test-user-exn exn s)
  (list exn (state:call-stack s)))

(define (cs-types result)
  (map (λ (e)
         (if (string? e) e (first e)))
       (second result)))


(define-check (check-exn-result result exn-type call-stack)
  (check-equal? (ue:exn:type (first result)) exn-type)
  (check-equal? (cs-types result) call-stack))
