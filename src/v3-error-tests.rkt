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

(error-str #:id "Can't access static field of unrelated class w/out dot"
           #:args (list "ClassName")
           #:catch #t
           "
class OtherClass { static var x = 5; }
class ClassName {
  static function main() { return x; }
}")

(error-str #:id "Can't access static method of unrelated class w/out dot"
           #:args (list "ClassName")
           #:catch #t
           "
class OtherClass { static function foo() { return 5; } }
class ClassName {
  static function main() { return foo(); }
}")

; ; Overriding and Abstracts

(error-str #:id "subclass doesn't override parent's abstract methods"
           #:args (list "Child")
           #:catch #t
           "
class Parent { function abstractMethod(); }
class Child extends Parent { }")

(error-str #:id "subclass overrides parent's concrete with abstract"
           #:args (list "Child")
           #:catch #t
           "
class Parent { function foo(x) { } }
class Child extends Parent {
  function foo(x);
}")

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


(error-str #:id "static methods collide with instance methods"
           #:args (list "A")
           #:catch #t
           "
class A {
  function foo(x, y, z) { }
  static function foo(a, b, c) { }
}")

; ; Dots

(error-str #:id "this on RHS of dot"
           #:args (list "A")
           #:catch #t
           "
class A {
  static function main() { return A.this; }
}")

(error-str #:id "super on RHS of dot"
           #:args (list "A")
           #:catch #t
           "
class A {
  static function main() { return A.super; }
}")

(error-str #:id "this in static context"
           #:args (list "A")
           #:catch #t
           "
class A {
  static function main() { return this; }
}")

(error-str #:id "non-existent class in LHS of dot during funcall"
           #:args (list "A")
           #:catch #t
           "
class A {
  static function main() { return B.c(); }
}")

(error-str #:id "non-existent class in LHS of dot during field lookup"
           #:args (list "A")
           #:catch #t
           "
class A {
  static function main() { return B.c; }
}")

(error-str #:id "non-existent class in LHS of dot during assignment"
           #:args (list "A")
           #:catch #t
           "
class A {
  static function main() { B.c = 2; return B.c; }
}")
