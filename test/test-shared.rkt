#lang racket/base
(require "../src/interpret/state/state.rkt"
         "../src/interpret/interpreter.rkt"
         "../src/interpret/user-errors.rkt"
         rackunit
         racket/list)

(provide i-str
         i-exn-str
         cs-types
         testing-user-exn
         check-exn-result
	 mode:script
	 mode:class
	 mode:main-func)

;; for regular testing
(define (i-str prog-str mode)
  (interpret (string-module prog-str)
             #:mode mode))

;; for error testing
(define (i-exn-str prog-str mode)
  (interpret (string-module prog-str)
             #:mode mode
             #:return (λ (v s) (fail-check "expected an error"))
             #:user-exn testing-user-exn
             #:throw (λ (e s)
                       (testing-user-exn (ue:uncaught-exception e) s))))

; user exn continuation used in (error) tests
(define (testing-user-exn exn s)
  (list exn (state:call-stack s)))

; for error testing, normalizes context stack
(define (cs-types result)
  (map (λ (e)
         (if (string? e) e (first e)))
       (second result)))


(define-check (check-exn-result result exn-type call-stack)
  (check-equal? (ue:exn:type (first result)) exn-type)
  (check-equal? (cs-types result) call-stack))
