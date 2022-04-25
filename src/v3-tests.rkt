#lang racket
(require "interpreter-extension.rkt"
         "util/testing.rkt")

; white space / newlines do not affect the parser,
; but are included for readability


(define test-file (make-tester interpret-v3-file))

(define test-str (make-tester interpret-v3-str))


; ; STATIC METHODS

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

(test-str #:id "single class, no fields, only static methods, call using dot"
          11
          #:args (list "A") "
class A {
  static function foo(x) {
    return 10 + x;
  }

  static function main() {
    return A.foo(1);
  }
}")


(test-str #:id "sub class can call static method of super class w/out dot"
          6
          #:args (list "A") "
class Parent {
  static function foo() { return 6; }
}
class A extends Parent {
  static function main() {
    return foo();
  }
}")

(test-str #:id "sub class can call static method of super class with dot"
          6
          #:args (list "A") "
class Parent {
  static function foo() { return 6; }
}
class A extends Parent {
  static function main() {
    return Parent.foo();
  }
}")

(test-str #:id "class can call static method of other class with dot"
          6
          #:args (list "A") "
class B {
  static function foo() { return 6; }
}
class A {
  static function main() {
    return B.foo();
  }
}")

(test-str #:id "sub class static method has precedence over static method of super class"
          7
          #:args (list "A") "
class Parent {
  static function foo() { return 6; }
}
class A extends Parent {
  static function foo() { return 7; }
  static function main() {
    return foo();
  }
}")


; ; STATIC FIELDS

(test-str #:id "static field and main method, same class"
          11
          #:args (list "TheClass") "
class TheClass {
  static var x = 10;

  static function main() {
    return x + 1;
  }
}")

(test-str #:id "local vars have priority over static fields"
          5
          #:args (list "MyClass") "
class MyClass {
  static var x = 10;

  static function main() {
    var x = 5;
    return x;
  }
}")

(test-str #:id "params have priority over static fields"
          6
          #:args (list "MyClass") "
class MyClass {
  static var x = 10;

  static function echo(x) { return x; }

  static function main() { return echo(6); }
}")

(test-str #:id "writes to shadowing local vars don't affect shadowed static fields"
          10
          #:args (list "MyClass") "
class MyClass {
  static var x = 10;

  static function foo() {
    var x;
    x = 6;
  }

  static function main() {
    foo();
    return x;
  }
}")

(test-str #:id "accessing static field of super class w/out dot"
          1
          #:args (list "Child") "
class Parent {
  static var x = 1;
}
class Child extends Parent {
  static function main() {
    return x;
  }
}")

(test-str #:id "accessing static field of super super class w/out dot"
          1
          #:args (list "Child") "
class GrandParent { static var x = 1; }

class Parent extends GrandParent { }

class Child extends Parent {
  static function main() {
    return x;
  }
}")

(test-str #:id "more recent static field takes precedence"
          2
          #:args (list "Child") "
class GrandParent { static var x = 1; }

class Parent extends GrandParent { static var x = 2; }

class Child extends Parent {
  static function main() {
    return x;
  }
}")


; ; ABSTRACT METHODS

(test-str #:id "subclass can override parent's abstract method with another abstract method"
          0
          #:args (list "Child") "
class Parent { function foo(x, y); }

class Child extends Parent {
  function foo(x, y);
  static function main() {
    return 0;
  }
}")


