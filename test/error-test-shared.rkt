#lang racket
(require "../src/state/state.rkt"
         "../src/user-errors.rkt"
         rackunit)

(provide cs-types
         test-user-exn
         check-exn-result)


(define (test-user-exn exn s)
  (list exn (state:context-stack s)))

(define (cs-types result)
  (map (λ (e)
         (if (string? e) e (first e)))
       (second result)))


(define-check (check-exn-result result exn-type context-stack)
  (check-equal? (ue:type (first result)) exn-type)
  (check-equal? (cs-types result) context-stack))
