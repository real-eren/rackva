#lang racket/base

(require "../test-shared.rkt"
         "../../src/interpret/user-errors.rkt"
         rackunit)


(define (im prog-str)
  (i-exn-str prog-str mode:main-func))

(define (ic prog-str class)
  (i-exn-str prog-str (mode:class class)))


(test-case
 "can't access class member without dot in top-level"
 (check-exn-result (im "
class A { static var a = 5; }
function main() { return a; }")
                   ue:type:reference-undeclared-var
                   '(return "main()")))

(test-case
 "top level return in main-func or class mode"
 (check-exn-result (im "return 5;") ue:type:unexpected-return '(return))
 (check-exn-result (ic "return 5;" "A") ue:type:unexpected-return '(return)))

(test-case
 "top level return in if in main-func or class mode"
 (check-exn-result (im "if (true) { return 5; }") ue:type:unexpected-return '(return begin if))
 (check-exn-result (ic "if (true) { return 5; } " "A") ue:type:unexpected-return '(return begin if)))
