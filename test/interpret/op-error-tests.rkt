#lang racket/base

(require "../test-shared.rkt"
         "../../src/interpret/user-errors.rkt"
         "../../src/interpret/type.rkt"
         rackunit)


(define (i program)
  (i-exn-str program mode:script))

(define-check (check-bool-expr-exn exn-result expr-string)
  (check-equal? (ue:exn:type (car exn-result)) ue:type:expected-boolean-expr)
  (check-equal? (ue:exn:property 'expr (car exn-result)) expr-string))

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
 (check-bool-expr-exn (i "return 1 || false;") "1")
 (check-bool-expr-exn (i "return false || 1;") "1"))

(test-case
 "int literal in &&"
 (check-bool-expr-exn (i "return 1 && true;") "1")
 (check-bool-expr-exn (i "return true && 1;") "1"))

(test-case
 "int expression in ||"
 (check-bool-expr-exn (i "return (1 + 2) || false;") "1 + 2")
 (check-bool-expr-exn (i "return false || (2 + 1);") "2 + 1"))

(test-case
 "int expression in &&"
 (check-bool-expr-exn (i "return (1 + 2) && false;") "1 + 2")
 (check-bool-expr-exn (i "return true && (2 + 1);") "2 + 1"))

(test-case
 "LHS type error in || detected before RHS"
 (check-bool-expr-exn (i "return 1 || (1 + 1);") "1")
 (check-bool-expr-exn (i "return (1 + 1) || 1;") "1 + 1"))

(test-case
 "LHS type error in && detected before RHS"
 (check-bool-expr-exn (i "return 1 || (1 + 1);") "1")
 (check-bool-expr-exn (i "return (1 + 1) || 1;") "1 + 1"))

(test-case
 "chained || and &&"
 (check-bool-expr-exn (i "return 1 || 1/0 || 1/0;") "1")
 (check-bool-expr-exn (i "return 1 && 1/0 && 1/0;") "1"))
