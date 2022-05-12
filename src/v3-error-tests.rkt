#lang racket

(require "interpreter-extension.rkt"
         "util/testing.rkt")

(define error-file (make-error-tester interpret-v3-file))
(define error-str (make-error-tester interpret-v3-str))

; ; Static Members

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
class Child extends Parent { static function main() { return 0; } }")

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


(error-str #:id "Can't invoke abstract method"
           #:args (list "A")
           #:catch #t
           "
class A {
  function overrideMe();
  static function main() {
    var a = new A();
    a.overrideMe();
    return 0;
  }
}")


(error-str #:id "Can't invoke abstract method of parent"
           #:args (list "Child")
           #:catch #t
           "
class Parent { function overrideMe(x, y); }
class Child extends Parent {
  function overrideMe(x, y);
  static function main() {
    var b = new Child();
    var a = b.overrideMe(0, 1);
    return 0;
  }
}")


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

; ; Dots, this, super

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

(error-str #:id "Static non-instance var in LHS of dot"
           #:args (list "A")
           #:catch #t
           "
class A {
  static var x = 2;
  static function main() { return x.y; }
}")

(error-str #:id "Instance Non-instance boolean field in LHS of dot"
           #:args (list "A")
           #:catch #t
           "
class A {
  var x = true;
  static function main() {
    var a = new A();
    return a.x.y;
  }
}")

(error-str #:id "Function return value non-instance var in LHS of dot"
           #:args (list "A")
           #:catch #t
           "
class A {
  function getX() { return 2; }
  static function main() {
    var a = new A();
    return a.getX().y;
  }
}")

(error-str #:id "this in static context called from instance context"
           #:args (list "A")
           #:catch #t "
class A {
  var x = 10;

  static function nowork(x) {
    return this.x;
  }

  function mightwork() {
    return x + nowork(x);
  }

  static function main() {
    var a = new A();
    return a.mightwork();
  }
}")

(error-str #:id "declaring an instance field named this"
           #:args (list "A")
           #:catch #t "
class A {
  var this;
  static function main() {
    return 0;
  }
}
")

(error-str #:id "declaring an instance field named super"
           #:args (list "A")
           #:catch #t "
class A {
  var super;
  static function main() {
    return 0;
  }
}
")

(error-str #:id "declaring a static field named this"
           #:args (list "A")
           #:catch #t "
class A {
  static var this;
  static function main() {
    return 0;
  }
}")

(error-str #:id "declaring a static field named super"
           #:args (list "A")
           #:catch #t "
class A {
  static var super;
  static function main() {
    return 0;
  }
}")

(error-str #:id "declaring a local variable named this"
           #:args (list "A")
           #:catch #t "
class A {
  static function main() {
    var this;
    return 0;
  }
}")

(error-str #:id "declaring a local variable named super"
           #:args (list "A")
           #:catch #t "
class A {
  static function main() {
    var super;
    return 0;
  }
}")

(error-str #:id "assigning to this"
           #:args (list "A")
           #:catch #t "
class A {

  function nowork() {
    this = 2;
    return this;
  }

  static function main() {
    var a = new A();
    return a.nowork();
  }
}")

(error-str #:id "assigning to super"
           #:args (list "A")
           #:catch #t "
class A {

  function mightwork() {
    super = 2;
    return this;
  }

  static function main() {
    var a = new A();
    return a.mightwork();
  }
}")

(error-str #:id "Invoking non-existent function when many similar ones exist"
           #:args (list "C")
           #:catch #t "
class A {
  function foo() { return 3; }
  static function foo(a) { return 4; }
}
class B extends A {
  static function foo (a, b) { return 5; }
  function foo(a, b, c) { return 6; }
}
class C extends B {
  static function foo(a, &b, c, &d, e, f, g) { return 2; }

  static function main() {
    {
      function foo(a, &b, c, &d) {
        return 7;
      }
      return foo(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    }
  }
}
")

; ; Constructors

(error-str #:id "Calling super() with no parent class"
           #:args (list "A")
           #:catch #t "
class A {
  A() {
    super();
  }
  static function main() {
    return new A();
  }
}")

(error-str #:id "Calling this(x) when no such constructor exists"
           #:args (list "A")
           #:catch #t "
class A {
  A() {
    this(2, 3, 4);
  }
  static function main() {
    return new A();
  }
}")

(error-str #:id "Calling super(x) when no such constructor exists in parent."
           #:args (list "B")
           #:catch #t "
class A {
  A() {
    this(2, 3, 4);
  }
}
class B extends A {
  B() {
    super(2, 3);
  }
  static function main() {
    return new B();
  }
}")

(error-str #:id "Parent doesn't have default ctor, child only has default ctor"
           #:args (list "B")
           #:catch #t "
class A {
  A(x, y) { }
}
class B extends A {
  static function main() {
    return new B();
  }
}")

(error-str #:id "Parent doesn't have default ctor, no explicit super in user-defined ctor."
           #:args (list "B")
           #:catch #t "
class A {
  A(x, y) { }
}
class B extends A {
  B() { }
  static function main() {
    return new B();
  }
}")

(error-str #:id "Calling this() after first line in constructor"
           #:args (list "A")
           #:catch #t "
class A {
  var x;
  A() {
    var x = 2 + 3;
    this(x);
  }
  A(v) {
    this.x = v;
  }
  static function main() {
    return new A();
  }
}")

(error-str #:id "Calling this(...) twice, same ctor"
           #:args (list "A")
           #:catch #t "
class A {
  var x;
  A() {
    this(1);
    this(2);
  }
  A(v) {
    this.x = v;
  }
  static function main() {
    return new A();
  }
}")

(error-str #:id "Calling this(...) twice, different ctor"
           #:args (list "A")
           #:catch #t "
class A {
  var x;
  A() {
    this(1);
    this(1, 2);
  }
  A(v) {
    this.x = v;
  }
  A(u, v) { }
  static function main() {
    return new A();
  }
}")

(error-str #:id "Calling super() after first line in constructor"
           #:args (list "A")
           #:catch #t "
class Parent {
  Parent() { }
}
class A extends Parent {
  var x;
  A() {
    var x = 2 + 3;
    super();
  }
  A(v) {
    this.x = v;
  }
  static function main() {
    return new A();
  }
}")

(error-str #:id "Calling super() then this()"
           #:args (list "A")
           #:catch #t "
class Parent {
  Parent() { }
  Parent(x) { }
}
class A extends Parent {
  var x;
  A() {
    super(3);
    this(3);
  }
  A(v) {
    this.x = v;
  }
  static function main() {
    return new A();
  }
}")

(error-str #:id "Calling this() in same constructor, 1 total"
           #:args (list "A")
           #:catch #t "
class A {
  var x;
  A() {
    this();
  }
  static function main() {
    return new A();
  }
}")

(error-str #:id "Calling this() in same constructor, 2 total"
           #:args (list "A")
           #:catch #t "
class A {
  var x;
  A() {
    this();
  }
  A(v) {
    this.x = v;
  }
  static function main() {
    return new A();
  }
}")

(error-str #:id "Calling this() in same constructor, many"
           #:args (list "A")
           #:catch #t "
class A {
  var x;
  A() {
    this();
  }
  A(v) {
    this.x = v;
  }
  A(v, w) { }
  A(a, b, c) { }
  static function main() {
    return new A();
  }
}")

(error-str #:id "Cycle of constructors when chaining, revisits initial constructor"
           #:args (list "A")
           #:catch #t "
class A {
  A() { this(1); }
  A(x) { this(1, 2); }
  A(x, y) { this(); }
  static function main() { return new A(); }
}")

(error-str #:id "Cycle of constructors when chaining, does not revisit initial constructor"
           #:args (list "A")
           #:catch #t "
class A {
  A() { this(1); }
  A(x) { this(1, 2); }
  A(x, y) { this(); }
  A(x, y, z) { this(); }
  static function main() { return new A(1, 2, 3); }
}")

(error-str #:id "Declaring a function named `this`"
           #:args (list "A")
           #:catch #t "
class A {
  var x;
  function this() {
    return 5;
  }
  A() {
    x = this();
  }
  static function main() { return new A(); }
}
")

(error-str #:id "Declaring a function named `super`"
           #:args (list "A")
           #:catch #t "
class A {
  var x;
  function super(x, y) {
    return 5;
  }
  A() {
    x = super();
  }
  static function main() { return new A(); }
}
")
