#lang racket/base
(require "test-shared.rkt"
         "../src/interpreter.rkt"
         rackunit)

(define (i prog-str class)
  (i-str prog-str (mode:class class)))

; ; INSTANCE CREATION AND INSTANCE FIELDS

(test-equal? "instance fields w/out initializer should be initialized to zero, no parent"
             (i "
class A {
  var x;
  var y;
  static function main() {
    var a = new A();
    return (a.x == 0) && (a.y == 0);
  }
}" "A")
             'true)

(test-equal? "instance fields w/out initializer should be initialized to zero, with super class"
             (i "
class Parent { var w; }
class A extends Parent {
  var x;
  var y;
  static function main() {
    var a = new A();
    return (a.x == 0) && (a.y == 0) && (a.w == 0);
  }
}" "A")
             'true)

(test-equal? "instance field w/ initializer"
             (i "
class A {
  var x = 10;
  static function main() {
    var a = new A();
    return a.x;
  }
}" "A")
             10)

(test-equal? "setting field in constructor"
             (i "
class A {
  var x;
  A() {
    x = 109;
  }
  static function main() {
    var a = new A();
    return a.x;
  }
}" "A")
             109)

(test-equal? "ctor body runs after init"
             (i "
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
}" "A")
             20)

(test-equal? "ctor calls super()"
             (i "
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
}" "Child")
             251)

(test-equal? "init runs only once when ctor chaining"
             (i "
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
}" "A")
             20)

(test-equal? "dot expr in arg"
             (i "
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
}" "A")
             124)

(test-equal? "return statement in constructor, still returns instance"
             (i "
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
}" "A")
             765)

(test-equal? "return statement in this-chain-ctor, still runs original ctor"
             (i "
class A {
  var a;
  A(n) {
    return 5;
    var shouldNotRun = 1/0;
  }
  A() {
    this(0);
    a = 10;
  }
  static function main() {
    var v = new A();
    return v.a;
  }
}" "A")
             10)

(test-equal? "return statement in parent ctor, still runs child ctor"
             (i "
class A {
  A(n) {
    return 5;
    var shouldNotRun = 1/0;
  }
}
class B extends A {
  var b;
  B() {
    super(0);
    b = 10;
  }
  static function main() {
    var v = new B();
    return v.b;
  }
}" "B")
             10)


; ; STATIC METHODS

(test-equal? "single class, no fields, only main method"
             (i "
class A {
  static function main() {
    return 10;
  }
}" "A")
             10)

(test-equal? "single class, no fields, only static methods"
             (i "
class A {
  static function foo(x) {
    return 10 + x;
  }

  static function main() {
    return foo(1);
  }
}" "A")
             11)

(test-equal? "single class, no fields, only static methods, call using dot"
             (i "
class A {
  static function foo(x) {
    return 10 + x;
  }

  static function main() {
    return A.foo(1);
  }
}" "A")
             11)

(test-equal? "sub class can call static method of super class w/out dot"
             (i "
class Parent {
  static function foo() { return 6; }
}
class A extends Parent {
  static function main() {
    return foo();
  }
}" "A")
             6)

(test-equal? "sub class can call static method of super class with dot"
             (i "
class Parent {
  static function foo() { return 6; }
}
class A extends Parent {
  static function main() {
    return Parent.foo();
  }
}" "A")
             6)

(test-equal? "class can call static method of other class with dot"
             (i "
class B {
  static function foo() { return 6; }
}
class A {
  static function main() {
    return B.foo();
  }
}" "A")
             6)

(test-equal? "sub class static method has precedence over static method of super class"
             (i "
class Parent {
  static function foo() { return 6; }
}
class A extends Parent {
  static function foo() { return 7; }
  static function main() {
    return foo();
  }
}" "A")
             7)


; ; STATIC FIELDS

(test-equal? "static field and main method, same class"
             (i "
class TheClass {
  static var x = 10;

  static function main() {
    return x + 1;
  }
}" "TheClass")
             11)

(test-equal? "local vars have priority over static fields"
             (i "
class MyClass {
  static var x = 10;

  static function main() {
    var x = 5;
    return x;
  }
}" "MyClass")
             5)

(test-equal? "params have priority over static fields"
             (i "
class MyClass {
  static var x = 10;

  static function echo(x) { return x; }

  static function main() { return echo(6); }
}" "MyClass")
             6)

(test-equal? "writes to shadowing local vars don't affect shadowed static fields"
             (i "
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
}" "MyClass")
             10)

(test-equal? "Assigning to and reading form static field of super class w/out dot"
             (i "
class Parent {
  static var x = 1;
}
class Child extends Parent {
  static function main() {
    x = 2 + (x = x + 1);
    return x;
  }
}" "Child")
             4)

(test-equal? "Assigning to and reading from static fields of super class with dot"
             (i "
class Parent {
  static var x = 1;
  static var y = 3;
}
class Child extends Parent {
  static function main() {
    Parent.y = Parent.x = 2;
    return Parent.x + Parent.y;
  }
}" "Child")
             4)

(test-equal? "accessing static field of super super class w/out dot"
             (i "
class GrandParent { static var x = 1; }

class Parent extends GrandParent { }

class Child extends Parent {
  static function main() {
    return x;
  }
}" "Child")
             1)

(test-equal? "more recent static field takes precedence"
             (i "
class GrandParent { static var x = 1; }

class Parent extends GrandParent { static var x = 2; }

class Child extends Parent {
  static function main() {
    return x;
  }
}" "Child")
             2)

(test-equal? "access static field of unrelated class with dot"
             (i "
class A { static var a = 5; }
class B {
  static function div(&num, denom) {
    num = num / denom;
  }

  static function main() {
    A.a = A.a + 2;
    div(A.a, 3);
    return A.a;
  }
}" "B")
             2)


; ; ABSTRACT METHODS

(test-equal? "subclass can override parent's abstract method with another abstract method"
             (i "
class Parent { function foo(x, y); }

class Child extends Parent {
  function foo(x, y);
  static function main() {
    return 0;
  }
}" "Child")
             0)

; ; DOT

(test-equal? "Nested dot and side effect in inst field expr"
             (i "
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
" "List")
             4)

(test-equal? "dotted field passed by reference"
             (i "
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
" "A")
             4321)

(test-equal? "Instance yielding function call in LHS of dot"
             (i "
class A {
  var n;

  A(v) { n = v; }

  function successor() { return new A(n+1); }

  static function main() {
    var a = new A(3);
    
    return a.successor().n;
  }
}" "A")
             4)

(test-equal? "assign to dotted field"
             (i "
class Point {
  var x;
  var y;

  function setX(x) { this.x = x; }
  function setY(y) { this.y = y; }

  function sqrDistTo(otherPoint) {
    var dx = otherPoint.x - this.x;
    var dy = otherPoint.y - this.y;
    return dx*dx + dy*dy;
  }

  static function main() {
    var p1 = new Point();
    p1.setX(15);
    p1.setY(5);
    var p2 = new Point();
    p2.setX(10);
    p2.setY(6);
    return p1.sqrDistTo(p2);
  }
}" "Point")
             26)

(test-equal? "dotted field passed by reference"
             (i "
class A {
  static var a = 5;
  static var b = 6;

  static var inst2 = AFactory(9, 0);

  var x = 1;
  var y = 2;

  static function AFactory(x, y) {
    var a = new A();
    a.x = x;
    a.y = y;
    return a;
  }

  static function rotate(&i, &j, &k, &l, &m) {
    var temp = m;
    m = l;
    l = k;
    k = j;
    j = i;
    i = temp;
  }

  static function main() {
    var inst1 = AFactory(7, 8);
    rotate(A.a, A.b, inst1.x, inst1.y, inst2.x);
    return A.a * 10000 + A.b * 1000 + inst1.x * 100 + inst1.y * 10 + inst2.x;
  }
}" "A")
             95678)

(test-equal? "state changes during fun param evaluation happen left to right, mix of val and ref"
             (i "
class A {
  var x;

  static var logger = 0;
  static var pos = 0;

  static function decimalShiftLeft(val, n) {
    if (n > 0) return decimalShiftLeft(val * 10, n - 1);
    else return val;
  }
  static function numDigits(val) {
    if (val < 10) return 1;
    else return 1 + numDigits(val/10);
  }

  static function log(val) {
    logger = logger + decimalShiftLeft(val, pos);
    pos = pos + numDigits(val);
  }

  static function logAndEcho(val) {
    log(val);
    return val;
  }

  static function sideEffectObj(val) {
    logAndEcho(val);
    return new A();
  }
  static function sideEffectInt(val) {
    logAndEcho(val);
    return 0;
  }

  static function evalAndGetLogger(&a, b, &c, d) {
    return logger;
  }

  static function main() {
    return evalAndGetLogger( sideEffectObj(1).x, sideEffectInt(2), sideEffectObj(3).x, sideEffectInt(4) );
  }
}" "A")
             4321)

; ; SUPER FIELDS & METHODS
(test-equal? "todo"
             (i "
class A {
  static function main() {
    return 5;
  }
}
" "A")
             5)

; ; PASSING INSTANCES
(test-equal? "storing inst in variable, then var to ref param"
             (i "
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
}" "A")
             3)

(test-not-exn "Returning an instance should not cause an error"
              (λ () (i "
class A {
  static function main() {
    return new A();
  }
}" "A")))
