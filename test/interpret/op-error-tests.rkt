#lang racket/base

(require "../test-shared.rkt"
         "../../src/interpret/interpreter.rkt"
         "../../src/interpret/user-errors.rkt"
         rackunit)


(define (i program)
  (i-exn-str program mode:script))


; ; SHORT-CIRCUIT ||, &&

(test-case
 "int literal in ||"
 (check-exn-result (i "return 1 || false;")
                   ue:type:expected-boolean-val
                   '(return))
 (check-exn-result (i "return false || 1;")
                   ue:type:expected-boolean-val
                   '(return)))
 
(test-case
 "int literal in &&"
 (check-exn-result (i "return 1 && true;")
                   ue:type:expected-boolean-val
                   '(return))
 (check-exn-result (i "return true && 1;")
                   ue:type:expected-boolean-val
                   '(return)))
 
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
 (check-exn-result (i "return 1 || (1 + 1);")
                   ue:type:expected-boolean-val
                   '(return))
 (check-exn-result (i "return (1 + 1) || 1;")
                   ue:type:expected-boolean-expr
                   '(return)))

(test-case
 "LHS type error in && detected before RHS"
 (check-exn-result (i "return 1 || (1 + 1);")
                   ue:type:expected-boolean-val
                   '(return))
 (check-exn-result (i "return (1 + 1) || 1;")
                   ue:type:expected-boolean-expr
                   '(return)))

(test-case
 "chained || and &&"
 (check-exn-result (i "return 1 || 1/0 || 1/0;")
                   ue:type:expected-boolean-val
                   '(return))
 (check-exn-result (i "return 1 && 1/0 && 1/0;")
                   ue:type:expected-boolean-val
                   '(return)))
