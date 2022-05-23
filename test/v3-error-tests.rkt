#lang racket

(require "../src/interpreter-extension.rkt"
         "../src/user-errors.rkt"
         rackunit)

(define i
  (lambda (program class)
    (define user-exn (λ (exn s) exn))
    (interpret-v3-str program
                      class
                      #:return (λ (v s) (fail-check "expected an error"))
                      #:user-exn user-exn
                      #:throw (λ (e s) (user-exn (ue:uncaught-exception e) s)))))

(test-case
 "user-exn raised as user error in normal interpret"
 (check-exn exn:fail:user?
            (λ () (interpret-v3-str "
class A {
  static function main() {
    return 5;
  }
}" "NotAClass"))))

(test-case
 "throwing instance should successfully raise user-exn"
 (define exn (i "
class A {
  static function main() {
    throw new A();
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:uncaught-exception))


; ; CLASSES

(test-case
 "Non-existent class as entry-point"
 (define exn (i "
class A {
  static function main() {
    return 5;
  }
}" "NotAClass"))
 (check-equal? (ue:type exn) ue:type:not-a-class))

(test-case
 "declaring a class twice"
 (define exn (i "
class A {
}
class A {
  static function main() {
    return 5;
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:duplicate-class))

(test-case
 "class extends itself"
 (define exn (i "
class A extends A {
  static function main() {
    return 5;
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:class-extend-self))

(test-case
 "declaring a child before parent"
 (define exn (i "
class A extends Parent {
  static function main() {
    return 5;
  }
}
class Parent {
}" "A"))
 (check-equal? (ue:type exn) ue:type:not-a-class))

(test-case
 "declaring a class with a non-existent parent"
 (define exn (i "class A extends NotAClass {}" "A"))
 (check-equal? (ue:type exn) ue:type:not-a-class))

; ; INSTANCE MEMBERS

(test-case
 "instance fields with colliding names"
 (define exn (i "
class A {
  var x;
  var y;
  var x;
}" "A"))
 (check-equal? (ue:type exn) ue:type:duplicate-field))

(test-case
 "duplicate parameters in abstract and instance methods"
 (let ([inst-exn  (i "class A { function foo(a, a) {} }" "A")]
       [abst-exn  (i "class A { function foo(a, a); }" "A")])
   (check-equal? (ue:type inst-exn) ue:type:duplicate-parameter)
   (check-equal? (ue:type abst-exn) ue:type:duplicate-parameter)))

; ; STATIC MEMBERS

(test-case
 "static local vars don't persist"
 (define exn (i "
class ClassName {
  static function foo() {
    var x = 5;
  }
  static function main() {
    foo();
    return x;
  }
}" "ClassName"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "static field declared twice"
 (define exn (i "
class ClassName {
  static var x;
  static var x = 5;
  static function main() { return x; }
}" "ClassName"))
 (check-equal? (ue:type exn) ue:type:duplicate-field))

(test-case
 "Can't access static field of unrelated class w/out dot"
 (define exn (i "
class OtherClass { static var x = 5; }
class ClassName {
  static function main() { return x; }
}" "ClassName"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "Can't access static method of unrelated class w/out dot"
 (define exn (i "
class OtherClass { static function foo() { return 5; } }
class ClassName {
  static function main() { return foo(); }
}" "ClassName"))
 (check-equal? (ue:type exn) ue:type:function-not-in-scope))

(test-case
 "Static field calls function that throws"
 (define exn (i "
class ClassName {
  static var x = foo();
  static function foo() { throw 5; }
  static function main() { return foo(); }
}" "A"))
 (check-equal? (ue:type exn) ue:type:uncaught-exception))

(test-case
 "Static method with duplicate parameter names"
 (define exn (i "class A { static function foo(a, &a) {} }" "A"))
 (check-equal? (ue:type exn) ue:type:duplicate-parameter))

; ; Overriding and Abstracts

(test-case
 "subclass doesn't override parent's abstract methods"
 (define exn (i "
class Parent { function abstractMethod(); }
class Child extends Parent { static function main() { return 0; } }" "Child"))
 (check-equal? (ue:type exn) ue:type:unoverridden-abstract))

(test-case
 "subclass overrides parent's concrete with abstract"
 (define exn (i "
class Parent { function foo(x) { } }
class Child extends Parent {
  function foo(x);
}" "Child"))
 (check-equal? (ue:type exn) ue:type:override-c-w/-abstr))


(test-case
 "subclass declares method with similar signature to parent's abstract methods, but does not override"
 (define exn (i "
class Parent { function overrideMe(x, y); }
class Child extends Parent { function overrideMe(&x) { } }" "Child"))
 (check-equal? (ue:type exn) ue:type:unoverridden-abstract))


(test-case
 "Can't invoke abstract method"
 (define exn (i "
class A {
  function overrideMe();
  static function main() {
    var a = new A();
    a.overrideMe();
    return 0;
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:invoke-abstract-method))


(test-case
 "Can't invoke abstract method of parent"
 (define exn (i "
class Parent { function overrideMe(x, y); }
class Child extends Parent {
  function overrideMe(x, y);
  static function main() {
    var b = new Child();
    var a = b.overrideMe(0, 1);
    return 0;
  }
}" "Child"))
 (check-equal? (ue:type exn) ue:type:invoke-abstract-method))


(test-case
 "static methods don't count as overriding"
 (define exn (i "
class Parent { function overrideMe(x, y, z); }
class Child extends Parent { static function overrideMe(x, y, z) { } }" "Child"))
 (check-equal? (ue:type exn) ue:type:unoverridden-abstract))


(test-case
 "static methods collide with instance methods"
 (define exn (i "
class A {
  function foo(x, y, z) { }
  static function foo(a, b, c) { }
}" "A"))
 (check-equal? (ue:type exn) ue:type:duplicate-method))

(test-case
 "Invoking non-existent function when many similar ones exist"
 (define exn (i "
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
" "C"))
 (check-equal? (ue:type exn) ue:type:function-not-in-scope))

; ; Dots, this, super

(test-case
 "this on RHS of dot"
 (define exn (i "
class A {
  static function main() { return A.this; }
}" "A"))
 (check-equal? (ue:type exn) ue:type:this/super-dot-RHS))

(test-case
 "super on RHS of dot"
 (define exn (i "
class A {
  static function main() { return A.super; }
}" "A"))
 (check-equal? (ue:type exn) ue:type:this/super-dot-RHS))

(test-case
 "this in static context"
 (define exn (i "
class A {
  static function main() { return this; }
}" "A"))
 (check-equal? (ue:type exn) ue:type:this/super-in-static))

(test-case
 "non-existent class in LHS of dot during funcall"
 (define exn (i "
class A {
  static function main() { return B.c(); }
}" "A"))
 (check-equal? (ue:type exn) ue:type:unknown-LHS-dot))

(test-case
 "non-existent class in LHS of dot during field lookup"
 (define exn (i "
class A {
  static function main() { return B.c; }
}" "A"))
 (check-equal? (ue:type exn) ue:type:unknown-LHS-dot))

(test-case
 "non-existent class in LHS of dot during assignment"
 (define exn (i "
class A {
  static function main() { B.c = 2; return B.c; }
}" "A"))
 (check-equal? (ue:type exn) ue:type:unknown-LHS-dot))

(test-case
 "Static non-instance var in LHS of dot"
 (define exn (i "
class A {
  static var x = 2;
  static function main() { return x.y; }
}" "A"))
 (check-equal? (ue:type exn) ue:type:non-instance-dot))

(test-case
 "Instance Non-instance boolean field in LHS of dot"
 (define exn (i "
class A {
  var x = true;
  static function main() {
    var a = new A();
    return a.x.y;
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:non-instance-dot))

(test-case
 "Function return value non-instance var in LHS of dot"
 (define exn (i "
class A {
  function getX() { return 2; }
  static function main() {
    var a = new A();
    return a.getX().y;
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:non-instance-dot))

(test-case
 "this in static context called from instance context"
 (define exn (i "
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
}" "A"))
 (check-equal? (ue:type exn) ue:type:this/super-in-static))

(test-case
 "super in static context called from instance context"
 (define exn (i "
class A {
  var x = 10;

  static function nowork(x) {
    return super.x;
  }

  function mightwork() {
    return x + nowork(x);
  }

  static function main() {
    var a = new A();
    return a.mightwork();
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:this/super-in-static))

(test-case
 "attempting to read from super as if var in instance function"
 (define exn (i "
class Parent { }
class A extends Parent {

  function mightwork() {
    return super;
  }

  static function main() {
    var a = new A();
    return a.mightwork();
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:keyword-as-identifier))

(test-case
 "declaring an instance field named this"
 (define exn (i "
class A {
  var this;
  static function main() {
    return 0;
  }
}
" "A"))
 (check-equal? (ue:type exn) ue:type:keyword-as-identifier))

(test-case
 "declaring an instance field named super"
 (define exn (i "
class A {
  var super;
  static function main() {
    return 0;
  }
}
" "A"))
 (check-equal? (ue:type exn) ue:type:keyword-as-identifier))

(test-case
 "declaring a static field named this"
 (define exn (i "
class A {
  static var this;
  static function main() {
    return 0;
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:keyword-as-identifier))

(test-case
 "declaring a static field named super"
 (define exn (i "
class A {
  static var super;
  static function main() {
    return 0;
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:keyword-as-identifier))

(test-case
 "declaring a local variable named this"
 (define exn (i "
class A {
  static function main() {
    var this;
    return 0;
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:keyword-as-identifier))

(test-case
 "declaring a local variable named super"
 (define exn (i "
class A {
  static function main() {
    var super;
    return 0;
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:keyword-as-identifier))

(test-case
 "assigning to this"
 (define exn (i "
class A {

  function nowork() {
    this = 2;
    return this;
  }

  static function main() {
    var a = new A();
    return a.nowork();
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:assigning-to-this/super))

(test-case
 "assigning to super"
 (define exn (i "
class A {

  function mightwork() {
    super = 2;
    return this;
  }

  static function main() {
    var a = new A();
    return a.mightwork();
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:assigning-to-this/super))

(test-case
 "passing this in a reference parameter"
 (define exn (i "
class A {
  function refFun(&inst) { throw 2;
    inst = inst;
  }

  function mightwork() {
    refFun(this);
  }

  static function main() {
    var a = new A();
    return a.mightwork();
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:this-as-ref-param))

(test-case
 "Declaring a function named `this`"
 (define exn (i "
class A {
  var x;
  function this() {
    return 5;
  }
  A() {
    x = this();
  }
  static function main() { return new A(); }
}" "A"))
 (check-equal? (ue:type exn) ue:type:keyword-as-identifier))

(test-case
 "Declaring a function named `super`"
 (define exn (i "
class A {
  var x;
  function super(x, y) {
    return 5;
  }
  A() {
    x = super();
  }
  static function main() { return new A(); }
}" "A"))
 (check-equal? (ue:type exn) ue:type:keyword-as-identifier))

; ; Constructors

(test-case
 "Cannot call constructor of undeclared class"
 (define exn (i "
class A {
  static function main() {
    return new B();
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:not-a-class))

(test-case
 "Calling super() with no parent class"
 (define exn (i "
class A {
  A() {
    super();
  }
  static function main() {
    return new A();
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:super-w/out-parent))

(test-case
 "Calling this(x) when no such constructor exists"
 (define exn (i "
class A {
  A() {
    this(2, 3, 4);
  }
  static function main() {
    return new A();
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:ctor-DNE))

(test-case
 "Calling super(x) when no such constructor exists in parent."
 (define exn (i "
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
}" "B"))
 (check-equal? (ue:type exn) ue:type:ctor-DNE))

(test-case
 "Parent doesn't have default ctor, child only has default ctor"
 (define exn (i "
class A {
  A(x, y) { }
}
class B extends A {
  static function main() {
    return new B();
  }
}" "B"))
 (check-equal? (ue:type exn) ue:type:ctor-DNE))

(test-case
 "Parent doesn't have default ctor, no explicit super in user-defined ctor."
 (define exn (i "
class A {
  A(x, y) { }
}
class B extends A {
  B() { }
  static function main() {
    return new B();
  }
}" "B"))
 (check-equal? (ue:type exn) ue:type:ctor-DNE))

(test-case
 "Calling this() after first line in constructor"
 (define exn (i "
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
}" "A"))
 (check-equal? (ue:type exn) ue:type:ctor-chain-outside-ctor))

(test-case
 "Calling this(...) twice, same ctor"
 (define exn (i "
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
}" "A"))
 (check-equal? (ue:type exn) ue:type:ctor-chain-outside-ctor))

(test-case
 "Calling this(...) twice, different ctor"
 (define exn (i "
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
}" "A"))
 (check-equal? (ue:type exn) ue:type:ctor-chain-outside-ctor))

(test-case
 "Calling super() after first line in constructor"
 (define exn (i "
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
}" "A"))
 (check-equal? (ue:type exn) ue:type:ctor-chain-outside-ctor))

(test-case
 "Calling super() then this()"
 (define exn (i "
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
}" "A"))
 (check-equal? (ue:type exn) ue:type:ctor-chain-outside-ctor))

(test-case
 "Calling this() in same constructor, 1 total"
 (define exn (i "
class A {
  var x;
  A() {
    this();
  }
  static function main() {
    return new A();
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:cyclic-ctor-chaining))

(test-case
 "Calling this() in same constructor, 2 total"
 (define exn (i "
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
}" "A"))
 (check-equal? (ue:type exn) ue:type:cyclic-ctor-chaining))

(test-case
 "Calling this() in same constructor, many"
 (define exn (i "
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
}" "A"))
 (check-equal? (ue:type exn) ue:type:cyclic-ctor-chaining))

(test-case
 "Cycle of constructors when chaining, revisits initial constructor"
 (define exn (i "
class A {
  A() { this(1); }
  A(x) { this(1, 2); }
  A(x, y) { this(); }
  static function main() { return new A(); }
}" "A"))
 (check-equal? (ue:type exn) ue:type:cyclic-ctor-chaining))

(test-case
 "Cycle of constructors when chaining, does not revisit initial constructor"
 (define exn (i "
class A {
  A() { this(1); }
  A(x) { this(1, 2); }
  A(x, y) { this(); }
  A(x, y, z) { this(); }
  static function main() { return new A(1, 2, 3); }
}" "A"))
 (check-equal? (ue:type exn) ue:type:cyclic-ctor-chaining))

(test-case
 "Colliding constructor signatures"
 (define exn (i "
class A {
  var x;
  A() {
    x = 5;
  }
  A() {
    x = 6;
  }
  static function main() { return new A().x; }
}" "A"))
 (check-equal? (ue:type exn) ue:type:duplicate-constructor))

(test-case
 "Duplicate parameters in constructor signature"
 (define exn (i "
class A {
  A(a, a) {
    x = 5;
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:duplicate-parameter))

(test-case
 "throw in ctor"
 (define exn (i "
class A {
  A() {
    throw 5;
  }
  static function main() { return new A(); }
}" "A"))
 (check-equal? (ue:type exn) ue:type:uncaught-exception))

(test-case
 "break in ctor called in loop"
 (define exn (i "
class A {
  A() {
    break;
  }
  static function main() {
    var x = 2;
    while (x != 0) {
      var a = new A();
      x = x - 1;
    }
    return 5;
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:break-outside-loop))

(test-case
 "continue in ctor called in loop"
 (define exn (i "
class A {
  A() {
    continue;
  }
  static function main() {
    var x = 2;
    while (x != 0) {
      var a = new A();
      x = x - 1;
    }
    return 5;
  }
}" "A"))
 (check-equal? (ue:type exn) ue:type:continue-outside-loop))

