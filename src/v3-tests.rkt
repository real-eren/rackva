#lang racket
(require "interpreter-extension.rkt"
         "util/testing.rkt")

; white space / newlines do not affect the parser,
; but are included for readability


(define test-file (make-tester interpret-v3-file))

(define test-str (make-tester interpret-v3-str))


(test-str #:id "single class, no fields, only main method"
          10
          #:args (list "A") "
class A {
  static function main() {
    return 10;
  }
}")

(test-str #:id "single class, no fields, only static methods"
          11
          #:args (list "A") "
class A {
  static function foo(x) {
    return 10 + x;
  }

  static function main() {
    return foo(1);
  }
}")

(test-str #:id "static field and main method, same class"
          11
          #:args (list "TheClass") "
class TheClass {
  static var x = 10;

  static function main() {
    return x + 1;
  }
}")