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

(define-check (check-type-error exn-result op expected-type expected-val)
  (define exn (car exn-result))
  (check-equal? (ue:exn:type exn) ue:type:type-mismatch)
  (check-equal? (ue:exn:property 'op-symbol exn)
                op)
  (check-equal? (ue:exn:property 'expected-types exn)
                expected-type)
  (check-equal? (ue:exn:property 'vals exn)
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

; ; ARITHMETIC OPERATORS

(test-case
 "bool in +-*/%"
 (check-type-error (i "return true + 2;")
                   '+
                   (list (list type:int type:int))
                   '(#T 2))
 (check-type-error (i "return -false;")
                   '-
                   (list (list type:int) (list type:int type:int))
                   '(#F))
 (check-type-error (i "return 5 * (1 == 2);")
                   '*
                   (list (list type:int type:int))
                   '(5 #F))
 (check-type-error (i "return true / (1 == 2);")
                   '/
                   (list (list type:int type:int))
                   '(#t #f))
 (check-type-error (i "function foo() { return false; } return foo() % 0;")
                   '%
                   (list (list type:int type:int))
                   '(#F 0)))

(test-case
 "divide by zero"
 (check-exn-result (i "return 1 / 0;")
                   ue:type:divide-by-zero
                   '(return)))

; ; NUM COMPARISON

(test-case
 "bool in < where bool expected"
 (check-type-error (i "if (5 < (1 == 2)) return 6;")
                   '<
                   (list (list type:int type:int))
                   '(5 #F))
 (check-type-error (i "return ((1 == 2) >= 5) && false;")
                   '>=
                   (list (list type:int type:int))
                   '(#F 5)))

