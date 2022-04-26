#lang racket
(require "interpreter-extension.rkt"
         "util/testing.rkt")

; white space / newlines do not affect the parser,
; but are included for readability


(define test-file (make-tester interpret-v3-file))

(define test-str (make-tester interpret-v3-str))


; ; INSTANCE CREATION

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

(test-str #:id "instance fields w/out initializer should be initialized to zero, with super class"
          10
          #:args (list "A") "
class A {
  var x = 10;
  static function main() {
    var a = new A();
    return a.x;
  }
}")

; ; INSTANCE FIELDS



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



;  ; Test 1 should return 15 when running A's main.

 (test-str #:id "Test 1"
           15
           #:args (list "A") "
 class A {
   var x = 5;
   var y = 10;

   static function main() {
     var a = new A();
     return a.x + a.y;
   }
 }")

;  Test 2 should return 12 when running A's main.

 (test-str #:id "Test 2"
           12
           #:args (list "A") "
 class A {

   function add(g, h) {
     return g + h;
   }

   static function main() {
     var a = new A();
     return a.add(10, 2);
   }
 }")

  ;Test 3 should return 125 when running A's main.

 (test-str #:id "Test 3"
           125
           #:args (list "A") "
 class A {

   var x = 100;

   function add(x) {
     return this.x + x;
   }

   static function main() {
     var a = new A();
     return a.add(25);
   }
 }")

  ;Test 4 should return 36 when running A's main.

 (test-str #:id "Test 4"
           36
           #:args (list "A") "
 class A {

   var x = 100;

   function setX(x) {
     this.x = x;
   }

   function add(a) {
     return a.x + this.x;
   }

   static function main() {
     var a1 = new A();
     var a2 = new A();
     a1.setX(30);
     a2.setX(6);
     return a1.add(a2);
   }
 }")

  ;Test 5 should return 54 when running A's main.

 (test-str #:id "Test 5"
           54
           #:args (list "A") "
 class A {

   var x = 100;

   function setX(x) {
     this.x = x;
   }

   function getX() {
     return this.x;
   }

   function add(a) {
     return a.getX() + this.getX();
   }

   static function main() {
     var a1 = new A();
     var a2 = new A();
     a1.setX(50);
     a2.setX(4);
     return a1.add(a2);
   }
 }")

  ;Test 6 should return 110 when running A's main.

 (test-str #:id "Test 6"
           110
           #:args (list "A") "
 class A {

   var x = 100;
   var y = 10;

   function add(g, h) {
     return g + h;
   }

   static function main() {
     return new A().add(new A().x, new A().y);
   }
 }")

  ;Test 7 should return 26 when running C's main.

 (test-str #:id "Test 6"
           26
           #:args (list "C") "
 class A {
   var x = 1;
   var y = 2;

   function m() {
     return this.m2();
   }

   function m2() {
     return x+y;
   }
 }

 class B extends A {
   var y = 22;
   var z = 3;

   function m() {
     return super.m();
   }

   function m2() {
     return x+y+z;
   }
 }

 class C extends B {
   var y = 222;
   var w = 4;

   function m() {
     return super.m();
   }

   static function main() {
     return new C().m();
   }
 }")

  ; Test 8 should return 117 when running Square's main.

 (test-str #:id "Test 8"
           117
           #:args (list "Square") "
 class Shape {
   function area() {
     return 0;
   }
 }

 class Rectangle extends Shape {
   var height;
   var width;

   function setHeight(h) {
     height = h;
   }

   function setWidth(w) {
     width = w;
   }

   function getHeight() {
     return height;
   }

   function getWidth() {
     return width;
   }

   function area() {
     return getWidth() * getHeight();
   }
 }

 class Square extends Rectangle {
   function setSize(size) {
     super.setWidth(size);
   }

   function getHeight() {
     return super.getWidth();
   }

   function setHeight(h) {
     super.setWidth(h);
   }

   static function main() {
     var s = new Square();
     var sum = 0;
     s.setSize(10);
     sum = sum + s.area();
     s.setHeight(4);
     sum = sum + s.area();
     s.setWidth(1);
     sum = sum + s.area();
     return sum;
   }
 }")
    
  ;Test 9 should return 32 when running Square's main.

 (test-str #:id "Test 9"
           32
           #:args (list "Square") "
 class Shape {
   function area() {
     return 0;
   }

   function largerThan(s) {
     return this.area() > s.area();
   }
 }

 class Rectangle extends Shape {
   var height;
   var width;

   function setHeight(h) {
     height = h;
   }

   function setWidth(w) {
     width = w;
   }

   function getHeight() {
     return height;
   }

   function getWidth() {
     return width;
   }

   function area() {
     return getWidth() * getHeight();
   }
 }

 class Square extends Rectangle {
   function setSize(size) {
     super.setWidth(size);
   }

   function getHeight() {
     return super.getWidth();
   }

   function setHeight(h) {
     super.setWidth(h);
   }

   static function main() {
     var s1 = new Square();
     var s2 = new Rectangle();
     var s3 = new Square();
     s1.setSize(5);
     s2.setHeight(8);
     s2.setWidth(4);
     s3.setWidth(3);

     var max = s1;
     if (s2.largerThan(max))
       max = s2;
     if (s3.largerThan(max))
       max = s3;
 
     return max.area();
   }
 }")
   
  ;Test 10 should return 15 when running List's main.

 (test-str #:id "Test 10"
           15
           #:args (list "List") "
 class List {
   var val;
   var next;

   function getNext() {
     return next;
   }

   function setNext(x) {
     if (x == 0)
       next = 0;
     else {
       next = new List();
       next.setVal(val+1);
       next.setNext(x-1);
     }
   }

   function setVal(x) {
     val = x;
   }

   static function main() {
     var l = new List();
     l.setVal(10);
     l.setNext(5);
     return l.getNext().getNext().getNext().getNext().getNext().val;
   }
 }")

  ;Test 11 should return 123456 when running List's main

 (test-str #:id "Test 11"
           123456
           #:args (list "List") "
 class List {
   var val;
   var next;

   function getNext() {
     return next;
   }

   function setNext(next) {
     this.next = next;
   }

   function makeList(x) {
     if (x == 0)
       next = 0;
     else {
       next = new List();
       next.setVal(val+1);
       next.makeList(x-1);
     }
   }

   function setVal(x) {
     val = x;
   }

   function reverse() {
     if (getNext() == 0)
       return this;
     else
       return getNext().reverse().append(this);
   }

   function append(x) {
     var p = this;
     while (p.getNext() != 0)
       p = p.getNext();
     p.setNext(x);
     x.setNext(0);
     return this;
   }

   static function main() {
     var l = new List();
     l.setVal(1);
     l.makeList(5);
     l = l.reverse();

     var result = 0;
     var p = l;
     var c = 1;
     while (p != 0) {
       result = result + c * p.val;
       c = c * 10;
       p = p.getNext();
     }
     return result;
   }
 }")

  ;Test 12 should return 5285 when running List's main.

 (test-str #:id "Test 12"
           5285
           #:args (list "List") "
 class List {
   var val;
   var next;

   function getNext() {
     return next;
   }

   function makeList(x) {
     if (x == 0)
       next = 0;
     else {
       next = new List();
       next.setVal(getVal()+1);
       next.makeList(x-1);
     }
   }

   function setVal(x) {
     val = x;
   }

   function getVal() {
     return val;
   }

   function expand() {
     var p = this;
     while (p != 0) {
       function exp(a) {
         while (a != 0) {
           this.setVal(this.getVal() + p.getVal() * a.getVal());
           a = a.getNext();
         }
       }
       exp(p);
       p = p.getNext();
     }
   }


   static function main() {
     var l = new List();
     l.val = 1;
     l.makeList(5);
     l.expand();
     return l.getVal();
   }
 }")

  ;Test 13 should return -716 when run with C's main.

 (test-str #:id "Test 13"
           -716
           #:args (list "C") "
 class A {
   var count = 0;

   function subtract(a, b) {
     if (a < b) {
        throw b - a;
     }
     else
        return a - b;
   }
 }

 class B extends A {
   function divide(a, b) {
     if (b == 0)
       throw a;
     else
       return a / b;
   }

   function reduce(a, b) {
     while (a > 1 || a < -1) {
       try {
         a = divide(a, b);
         if (a == 2)
           break;
       }
       catch (e) {
         return subtract(a, b); 
       }
       finally {
         count = count + 1;
       }
     }
     return a;
   }
 }

 class C {
   static function main() {
     var x;
     var b;

     b = new B();

     try {
       x = b.reduce(10, 5);
       x = x + b.reduce(81, 3);
       x = x + b.reduce(5, 0);
       x = x + b.reduce(-2, 0);
       x = x + b.reduce(12, 4);
     }
     catch (a) {
       x = x * a;
     }
     finally {
       x = -1 * x;
     }
     return x - b.count * 100;
   }
 }")


; Test 31 should return 20 when running A's main.

(test-str #:id "Test 31"
           20
           #:args (list "A") "
class A {
  static var x = 10;
  static function main() {
    return A.x + x;
  }
}")

; Test 32 should return 530 when running B's main.

(test-str #:id "Test 32"
           530
           #:args (list "B") "
class A {
  static var x = 10;
  static var y = 20;

  static function add(a, b) {
    return a + b;
  }

  static function main() {
    return A.add(x, A.y);
  }
}

class B extends A {
  static var y = 200;
  static var z = 300;

  static function main() {
    return add(B.x+A.y,B.z+y);
  }
}")


; Test 33 should return 615 when running B's main.

(test-str #:id "Test 33"
           615
           #:args (list "B") "
class A {
  static var a = 1;
  static var b = 10;

  static function getSum() {
    return a + b;
  }
}

class B {
  static function main() {
    A.a = 5;

    return A.getSum() + C.x + C.timesX(A.a);
  }
}

class C {
  static var x = 100;
  static function timesX(a) {
    return a * x;
  }
")
; Test 34 should return 16 when running Box's main.
(test-str #:id "Test 34"
           16
           #:args (list "Box") "
class Box {
  static var countAccesses = 0;
  var size = 1;

  function setSize(s) {
    this.size = s;
    countAccesses = countAccesses + 1;
  }

  static function main() {
    var x = 0;
    var c;

    while (x < 10) {
      var a = new Box();
      a.setSize(x + countAccesses);
      if (a.size % 4 == 0)
        c = a;
      x = x + 1;
    }

    return c.size;
  }
}")

; Test 35 should return 100 when running A's main.

(test-str #:id "Test 35"
           100
           #:args (list "A") "
class A {
  static function divide(x, y) {
    if (y == 0)
      throw new Zero();
    return x / y;
  }

  static function main() {
    var x;

    try {
      x = divide(10, 5) * 10;
      x = x + divide(5, 0);
    }
    catch(e) {
      x = e.getValue();
    }
    finally {
      x = x + 100;
    }
    return x;
  }
}

class Zero {
  var value = 0;

  function getValue() {
    return value;
  }
}")

; Test 36 should return 420 when running A's main.

(test-str #:id "Test 36"
           420
           #:args (list "A") "
class A {
  static function divide(x, y) {
    if (y == 0)
      throw new Zero();
    return x / y;
  }

  static function main() {
    var x = 0;
    var j = 1;

    try { 
     while (j >= 0) {
      var i = 10;
      while (i >= 0) {
        try {
          x = x + divide(10*i, i);
        }
        catch(e) {
          x = x + divide(e.getValue(), j);
        }
        i = i - 1;
      }
      j = j - 1;
     }
    }
    catch (e2) {
      x = x * 2;
    }
    return x;
  }
}

class Zero {
  var value = 10;

  function getValue() {
    return value;
  }
}")