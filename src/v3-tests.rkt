#lang racket
(require "interpreter-extension.rkt"
         "util/testing.rkt")

; white space / newlines do not affect the parser,
; but are included for readability


(define test-file (make-tester interpret-v3-file))

(define test-str (make-tester interpret-v3-str))


((const 0) test-str #:id "single class, no fields, only main method"
          10
          #:args (list "A") "
class A {
  static function main() {
    return 10;
  }
}")


