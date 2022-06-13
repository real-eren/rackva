#lang racket/base
(require "../test-shared.rkt"
         rackunit)

;; Tests provided in the assignment

(define (i prog-str class)
  (i-str prog-str (mode:class class)))

(test-equal? "Test 1"
             (i "
class A {
  var x = 5;
  var y = 10;

  static function main() {
    var a = new A();
    return a.x + a.y;
  }
}" "A")
             15)

(test-equal? "Test 2"
             (i "
class A {

  function add(g, h) {
    return g + h;
  }

  static function main() {
    var a = new A();
    return a.add(10, 2);
  }
}" "A")
             12)

(test-equal? "Test 3"
             (i "
class A {

  var x = 100;

  function add(x) {
    return this.x + x;
  }

  static function main() {
    var a = new A();
    return a.add(25);
  }
}" "A")
             125)

(test-equal? "Test 4"
             (i "
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
}" "A")
             36)

(test-equal? "Test 5"
             (i "
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
}" "A")
             54)

(test-equal? "Test 6"
             (i "
class A {

  var x = 100;
  var y = 10;

  function add(g, h) {
    return g + h;
  }

  static function main() {
    return new A().add(new A().x, new A().y);
  }
}" "A")
             110)

(test-equal? "Test 6"
             (i "
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
}" "C")
             26)

(test-equal? "Test 8"
             (i "
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
}" "Square")
             117)
    
(test-equal? "Test 9"
             (i "
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
}" "Square")
             32)
  
(test-equal? "Test 10"
             (i "
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
}" "List")
             15)

(test-equal? "Test 11"
             (i "
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
}" "List")
             123456)

(test-equal? "Test 12"
             (i "
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
}" "List")
             5285)

(test-equal? "Test 13"
             (i "
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
}" "C")
             -716)

; ; OVERLOADED FUNS, CALL-BY-REF, SIDE-EFFECTS IN EXPR

(test-equal? "Test 21"
             (i "
class A {

  function add(a, b) {
    return a + b;
  }

  function add(a,b,c) {
    return a + b + c;
  }

  static function main() {
    var x = 10;
    var y = 20;
    return new A().add(x, y) + new A().add(x, y, y) * 10;
  }
}" "A")
             530)

(test-equal? "Test 22"
             (i "
class A {
  var x = 10;
  var y = 20;

  function add(a, b) {
    return a + b;
  }

  function add(a,b,c) {
    return a + b + c;
  }
}

class B extends A {
  var x = 2;
  var y = 30;

  function add(a,b) {
    return a*b;
  }

  static function main() {
    var b = new B();
    return b.add(b.x,b.y) + b.add(b.x,b.x,b.x);
  }
}" "B")
             66)

(test-equal? "Test 23"
             (i "
class A {
  var x = 5;

  function swap(& a, & b) {
    var temp = a;
    a = b;
    b = temp;
  }

  static function main() {
    var y = 10;
    var sum = 0;
    var a = new A();

    a.swap(a.x, y);
    sum = a.x * 100 + y;
    a.x = 1;
    y = 2;
    a.swap(a.x, y);
    sum = sum + a.x * 10 + y;
    return sum;
  }
}" "A")
             1026)

(test-equal? "Test 24"
             (i "
class A {
  var x = 0;

  function setSum(limit) {
    var sum = 0;
    while ((x = x + 1) < limit) {
      sum = sum + x;
    }
    return sum;
  }

  static function main () {
    var a = new A();
    var j = a.setSum(10);
    return (a.x * 200 + j);
  }
}" "A")
             2045)

; ; STATIC MEMBERS

(test-equal? "Test 31"
             (i "
class A {
  static var x = 10;
  static function main() {
    return A.x + x;
  }
}" "A")
             20)

(test-equal? "Test 32"
             (i "
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
}" "B")
             530)

(test-equal? "Test 33"
             (i "
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
}" "B")
             615)

(test-equal? "Test 34"
             (i "
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
}" "Box")
             16)

(test-equal? "Test 35"
             (i "
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
}" "A")
             100)

(test-equal? "Test 36"
             (i "
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
}" "A")
             420)

; ; ABSTRACT METHODS

(test-equal? "Test 41"
             (i "
class Shape {
  function area();

  function changeSize(factor);
}

class Circle extends Shape {
  var radius;

  function setRadius(radius) {
    this.radius = radius;
  }

  function area() {
    return radius * radius * 3;
  }

  function changeSize(factor) {
    this.radius = this.radius * factor;
  }

  static function main() {
    var s = new Circle();
    s.setRadius(5);
    s.changeSize(2);
    return s.area();
  }
}" "Circle")
             300)

; ; USER-DEFINED CONSTRUCTORS

(test-equal? "Test 51"
             (i "
class Shape {
  function area() {
    return 0;
  }
}

class Rectangle extends Shape {
  var height;
  var width;

  Rectangle(h, w) {
    this.height = h;
    this.width = w;
  }

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
  Square(size) {
    super(size, size);
  }

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
    var s = new Square(20);
    var sum = 0;
    sum = sum + s.area();
    s.setHeight(4);
    sum = sum + s.area();
    s.setWidth(1);
    sum = sum + s.area();
    return sum;
  }
}" "Square")
             417)

(test-equal? "Test 52"
             (i "
class A {
  var x;

  A(val) {
    x = val;
  }

  static function main() {
    var a = new A(10);
    return a.x;
  }
}" "A")
             10)

(test-equal? "Test 53"
             (i "
class A {
  var x;
  var y;

  A() {
    x = 10;
    y = 2;
  }
}

class B extends A {
  var factor;

  B(f) {
    factor = f;
  }

  static function main() {
    var b = new B(4);
    return b.factor * (b.x + b.y);
  }
}" "B")
             48)

(test-equal? "Test 54"
             (i "
class A {
  var x = 1;
  var y = x + 1;
  var z = x + y + 1;

  A(a) {
    x = a;
  }

  A(a, b) {
    x = a;
    y = b;
  }

  static function main() {
    var a = new A(10);
    var b = new A(20, 5);

    return (a.x + a.y + a.z) * 100 + (b.x + b.y + b.z);
  }
}" "A")
             1629)
