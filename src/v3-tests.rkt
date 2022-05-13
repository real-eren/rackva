#lang racket
(require "interpreter-extension.rkt"
         "util/testing.rkt")

; white space / newlines do not affect the parser,
; but are included for readability


(define test-file (make-tester interpret-v3-file))

(define test-str (make-tester interpret-v3-str))

; ; INSTANCE CREATION AND INSTANCE FIELDS

(test-str #:id "instance fields w/out initializer should be initialized to zero, no parent"
          'true
          #:args (list "A") "
class A {
  var x;
  var y;
  static function main() {
    var a = new A();
    return (a.x == 0) && (a.y == 0);
  }
}")

(test-str #:id "instance fields w/out initializer should be initialized to zero, with super class"
          'true
          #:args (list "A") "
class Parent { var w; }
class A extends Parent {
  var x;
  var y;
  static function main() {
    var a = new A();
    return (a.x == 0) && (a.y == 0) && (a.w == 0);
  }
}")

(test-str #:id "instance field w/ initializer"
          10
          #:args (list "A") "
class A {
  var x = 10;
  static function main() {
    var a = new A();
    return a.x;
  }
}")

(test-str #:id "setting field in constructor"
          109
          #:args (list "A") "
class A {
  var x;
  A() {
    x = 109;
  }
  static function main() {
    var a = new A();
    return a.x;
  }
}")

(test-str #:id "ctor body runs after init"
          20
          #:args (list "A") "
class A {
  static var counter = 0;

  var x = (counter = counter + 10);

  A() {
    x = (counter = counter * 2);
  }

  static function main() {
    var a = new A();
    return a.x;
  }
}")

(test-str #:id "ctor calls super()"
          251
          #:args (list "Child") "
class Parent {
  var x;
  Parent(v) {
    x = v;
  }
}
class Child extends Parent {
  Child(v) {
    super(v);
  }
  static function main() {
    var c = new Child(251);
    return c.x;
  }
}")

(test-str #:id "init runs only once when ctor chaining"
          20
          #:args (list "A") "
class A {
  static var counter = 0;

  var x = (counter = counter + 10);

  A() {
    this(2);
  }
  A(n) {
    x = (counter = counter * n);
  }

  static function main() {
    var a = new A();
    return a.x;
  }
}")

(test-str #:id "dot expr in arg"
          124
          #:args (list "A") "
class A {
  static var counter = 0;

  var x = (counter = counter + 10);

  A(n) {
    counter = counter * n;
    x = n;
  }

  static function main() {
    var a1 = new A(2);
    var a2 = new A(a1.x * 2);
    return a2.x + counter;
  }
}")

(test-str #:id "return statement in constructor, still returns instance"
          765
          #:args (list "A") "
class A {
  var x = 765;

  A(n) {
    return 5;
    var shouldNotRun = 1/0;
  }

  static function main() {
    var a = new A(2);
    return a.x;
  }
}")



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

(test-str #:id "Assigning to and reading form static field of super class w/out dot"
          4
          #:args (list "Child") "
class Parent {
  static var x = 1;
}
class Child extends Parent {
  static function main() {
    x = 2 + (x = x + 1);
    return x;
  }
}")

(test-str #:id "Assigning to and reading from static fields of super class with dot"
          4
          #:args (list "Child") "
class Parent {
  static var x = 1;
  static var y = 3;
}
class Child extends Parent {
  static function main() {
    Parent.y = Parent.x = 2;
    return Parent.x + Parent.y;
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

; ; DOT

(test-str #:id "Nested dot and side effect in inst field expr"
          4
          #:args (list "List") "
class List {
  static var counter = 0;
  var x = (counter = counter + 1);
  var next;

  static function main() {
    var l1 = new List();
    var l2 = new List();
    var l3 = new List();
    var l4 = new List();
    l1.next = l2;
    l2.next = l3;
    l3.next = l4;
    return l1.next.next.next.x;
  }
}
")

(test-str #:id "dotted field passed by reference"
          4321
          #:args (list "A") "
class A {

  var x = 0;

  static function inc(&intvar) {
    intvar = intvar + 1;
  }

  static function main() {
    var a = new A();
    var acc = 0;
    var i = 1;
    while (i < 1001) {
      inc(a.x);
      acc = acc + i * a.x;
      i = i * 10;
    }
    return acc;
  }
}
")

(test-str #:id "Instance yielding function call in LHS of dot"
          4
          #:args (list "A")
          "
class A {
  var n;

  A(v) { n = v; }

  function successor() { return new A(n+1); }

  static function main() {
    var a = new A(3);
    
    return a.successor().n;
  }
}")

; ; SUPER FIELDS & METHODS
(test-str #:id "todo"
          5
          #:args (list "A") "
class A {
  static function main() {
    return 5;
  }
}
")

; ; PASSING INSTANCES
(test-str #:id "storing inst in variable, then var to ref param"
          3
          #:args (list "A") "
class A {
  var x = 2;
  function refFun(&inst) {
    var a = new A();
    a.x = 3;
    inst = a;
  }
  static function main() {
    var a = new A();
    var b = new A();
    a.refFun(b);
    return b.x;
  }
}")

