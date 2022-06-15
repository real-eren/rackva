#lang racket/base

(require "../test-shared.rkt"
         "../../src/interpret/user-errors.rkt"
         "../../src/interpret/type.rkt"
         rackunit)


(define (i program)
  (i-exn-str program mode:script))

(define-check (check-type-error exn-result expected-type expected-val)
  (define exn (car exn-result))
  (check-equal? (ue:exn:type exn) ue:type:type-mismatch)
  (check-equal? (ue:exn:property 'expected-type exn)
                expected-type)
  (check-equal? (ue:exn:property 'val exn)
                expected-val))

; ; SHORT-CIRCUIT ||, &&

(test-case
 "int literal in ||"
 (check-type-error (i "return 1 || false;")
                   type:bool
                   1)
 (check-type-error (i "return false || 1;")
                   type:bool
                   1))

(test-case
 "int literal in &&"
 (check-type-error (i "return 1 && true;")
                   type:bool
                   1)
 (check-type-error (i "return true && 1;")
                   type:bool
                   1))

(test-case
 "int expression in ||"
 (check-exn-result (i "return (1 + 2) || false;")
                   ue:type:expected-boolean-expr
                   '(return))
 (check-exn-result (i "return false || (2 + 1);")
                   ue:type:expected-boolean-expr
                   '(return)))

(test-case
 "int expression in &&"
 (check-exn-result (i "return (1 + 2) && false;")
                   ue:type:expected-boolean-expr
                   '(return))
 (check-exn-result (i "return true && (2 + 1);")
                   ue:type:expected-boolean-expr
                   '(return)))

(test-case
 "LHS type error in || detected before RHS"
 (check-type-error (i "return 1 || (1 + 1);")
                   type:bool
                   1)
 (check-exn-result (i "return (1 + 1) || 1;")
                   ue:type:expected-boolean-expr
                   '(return)))

(test-case
 "LHS type error in && detected before RHS"
 (check-type-error (i "return 1 || (1 + 1);")
                   type:bool
                   1)
 (check-exn-result (i "return (1 + 1) || 1;")
                   ue:type:expected-boolean-expr
                   '(return)))

(test-case
 "chained || and &&"
 (check-type-error (i "return 1 || 1/0 || 1/0;")
                   type:bool
                   1)
 (check-type-error (i "return 1 && 1/0 && 1/0;")
                   type:bool
                   1))
