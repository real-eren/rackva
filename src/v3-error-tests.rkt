#lang racket

(require "interpreter-extension.rkt"
         "util/testing.rkt")

(define error-file (make-error-tester interpret-v3-file))
(define error-str (make-error-tester interpret-v3-str))

(error-str #:id "static local vars don't persist"
           #:args (list "ClassName")
           #:catch #T
           "
class ClassName {
  static function foo() {
    var x = 5;
  }
  static function main() {
    foo();
    return x;
  }
}")