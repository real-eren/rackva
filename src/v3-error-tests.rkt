#lang racket

(require "interpreter-extension.rkt"
         "util/testing.rkt")

(define error-file (make-error-tester interpret-v3-file))
(define error-str (make-error-tester interpret-v3-str))

(error-str #:id "static local vars don't persist"
           #:args (list "ClassName")
           #:catch #t
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

(error-str #:id "static field declared twice"
           #:args (list "ClassName")
           #:catch #t
           "
class ClassName {
  static var x;
  static var x = 5;
  static function main() { return x; }
}")

(error-str #:id "subclass doesn't override parent's abstract methods"
           #:args (list "Child")
           #:catch #t
           "
class Parent { function abstractMethod(); }
class Child extends Parent { }")

(error-str #:id "subclass declares method with similar signature to parent's abstract methods, but does not override"
           #:args (list "Child")
           #:catch #t
           "
class Parent { function overrideMe(x, y); }
class Child extends Parent { function overrideMe(&x) { } }")


(error-str #:id "static methods don't count as overriding"
           #:args (list "Child")
           #:catch #t
           "
class Parent { function overrideMe(x, y, z); }
class Child extends Parent { static function overrideMe(x, y, z) { } }")

