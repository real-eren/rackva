#lang racket/base

(require "test-shared.rkt"
         "../src/interpreter.rkt"
         "../src/user-errors.rkt"
         rackunit)

(define (i program class)
  (i-exn-str program (mode:class class)))

(define (i-throw program class)
  (i-str program (mode:class class)))

(test-case
 "user-exns raised as user errors in normal interpret"
 (check-exn exn:fail:user?
            (λ () (i-throw "
class A {
  static function main() {
    return 5;
  }
}" "NotAClass")))
 (check-exn exn:fail:user?
            (λ () (i-throw "
class A {
  A(a, &b, c) {
    throw 5;
  }
}
class B {
  var a = f1();

  function f1() {
    var c = 2;
    return f2(0, c);
  }
  static function f2(x, &y) {
    return new A(x, y, y);
  }
}
class C {
  static function main() {
    return new B();
  }
}
" "C")))
 (check-exn exn:fail:user?
            (λ () (i-throw "
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
" "C"))))

(test-case
 "throwing instance should successfully raise user-exn"
 (define result (i "
class A {
  static function main() {
    throw new A();
  }
}" "A"))
 (check-exn-result result
                   ue:type:uncaught-exception
                   '(throw "A::main()")))

; ; Instance where boolean expected

(test-case
 "instance in if/while cond"
 (check-exn-result (i "class A { static function main() { var a = new A(); if (a) return 0; } }" "A")
                   ue:type:expected-boolean-val
                   '(if "A::main()"))
 (check-exn-result (i "class A { static function main() { if (new A()) return 0; } }" "A")
                   ue:type:expected-boolean-expr
                   '(if "A::main()")))

(test-case
 "instance in || / &&"
 (check-exn-result (i "class A { static function main() { var a = new A(); return a || true; } }" "A")
                   ue:type:expected-boolean-val
                   '(return "A::main()"))
 (check-exn-result (i "class A { static function main() { return new B() || false; } }" "A")
                   ue:type:expected-boolean-expr
                   '(return "A::main()")))

; ; CLASSES

(test-case
 "Non-existent class as entry-point"
 (define result (i "
class A {
  static function main() {
    return 5;
  }
}" "NotAClass"))
 (check-exn-result result
                   ue:type:not-a-class
                   '()))

(test-case
 "declaring a class twice"
 (define result (i "
class A {
}
class A {
  static function main() {
    return 5;
  }
}" "A"))
 (check-exn-result result
                   ue:type:duplicate-class
                   '(class)))

(test-case
 "class extends itself"
 (define result (i "
class A extends A {
  static function main() {
    return 5;
  }
}" "A"))
 (check-exn-result result
                   ue:type:class-extend-self
                   '(class)))

(test-case
 "declaring a child before parent"
 (define result (i "
class A extends Parent {
  static function main() {
    return 5;
  }
}
class Parent {
}" "A"))
 (check-exn-result result
                   ue:type:not-a-class
                   '(class)))

(test-case
 "declaring a class with a non-existent parent"
 (define result (i "class A extends NotAClass {}" "A"))
 (check-exn-result result
                   ue:type:not-a-class
                   '(class)))

; ; INSTANCE MEMBERS

(test-case
 "instance fields with colliding names"
 (define result (i "
class A {
  var x;
  var y;
  var x;
}" "A"))
 (check-exn-result result
                   ue:type:duplicate-field
                   '(var class)))

(test-case
 "duplicate parameters in abstract and instance methods"
 (let ([inst-result  (i "class A { function foo(a, a) {} }" "A")]
       [abst-result  (i "class A { function foo(a, a); }" "A")])
   (check-exn-result inst-result
                     ue:type:duplicate-parameter
                     '(function class))
   (check-exn-result abst-result
                     ue:type:duplicate-parameter
                     '(abstract-function class))))

; ; STATIC MEMBERS

(test-case
 "static local vars don't persist"
 (define result (i "
class ClassName {
  static function foo() {
    var x = 5;
  }
  static function main() {
    foo();
    return x;
  }
}" "ClassName"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(return "ClassName::main()")))

(test-case
 "static field declared twice"
 (define result (i "
class ClassName {
  static var x;
  static var x = 5;
  static function main() { return x; }
}" "ClassName"))
 (check-exn-result result
                   ue:type:duplicate-field
                   '(static-var class)))

(test-case
 "Can't access static field of unrelated class w/out dot"
 (define result (i "
class OtherClass { static var x = 5; }
class ClassName {
  static function main() { return x; }
}" "ClassName"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(return "ClassName::main()")))

(test-case
 "Can't access static method of unrelated class w/out dot"
 (define result (i "
class OtherClass { static function foo() { return 5; } }
class ClassName {
  static function main() { return foo(); }
}" "ClassName"))
 (check-exn-result result
                   ue:type:function-not-in-scope
                   '(return "ClassName::main()")))

(test-case
 "Static field calls function that throws"
 (define result (i "
class ClassName {
  static var x = foo();
  static function foo() { throw 5; }
  static function main() { return foo(); }
}" "A"))
 (check-exn-result result
                   ue:type:uncaught-exception
                   '(throw "ClassName::foo()" static-var class)))

(test-case
 "Static method with duplicate parameter names"
 (define result (i "class A { static function foo(a, &a) {} }" "A"))
 (check-exn-result result
                   ue:type:duplicate-parameter
                   '(static-function class)))


; ; Overriding and Abstracts

(test-case
 "subclass doesn't override parent's abstract methods"
 (define result (i "
class Parent { function abstractMethod(); }
class Child extends Parent { static function main() { return 0; } }" "Child"))
 (check-exn-result result
                   ue:type:unoverridden-abstract
                   '(class)))

(test-case
 "subclass overrides parent's concrete with abstract"
 (define result (i "
class Parent { function foo(x) { } }
class Child extends Parent {
  function foo(x);
}" "Child"))
 (check-exn-result result
                   ue:type:override-c-w/-abstr
                   '(abstract-function class)))


(test-case
 "subclass declares method with similar signature to parent's abstract methods,
 but does not override"
 (define result (i "
class Parent { function overrideMe(x, y); }
class Child extends Parent { function overrideMe(&x) { } }" "Child"))
 (check-exn-result result
                   ue:type:unoverridden-abstract
                   '(class)))


(test-case
 "Can't invoke abstract method"
 (define result (i "
class A {
  function overrideMe();
  static function main() {
    var a = new A();
    a.overrideMe();
    return 0;
  }
}" "A"))
 (check-exn-result result
                   ue:type:invoke-abstract-method
                   '(funcall "A::main()")))


(test-case
 "Can't invoke abstract method of parent"
 (define result (i "
class Parent { function overrideMe(x, y); }
class Child extends Parent {
  function overrideMe(x, y);
  static function main() {
    var b = new Child();
    b.overrideMe(0, 1);
    return 0;
  }
}" "Child"))
 (check-exn-result result
                   ue:type:invoke-abstract-method
                   '(funcall "Child::main()")))


(test-case
 "static methods don't count as overriding"
 (define result (i "
class Parent { function overrideMe(x, y, z); }
class Child extends Parent {
  static function overrideMe(x, y, z) { }
}" "Child"))
 (check-exn-result result
                   ue:type:unoverridden-abstract
                   '(class)))


(test-case
 "static methods collide with instance methods"
 (define result (i "
class A {
  function foo(x, y, z) { }
  static function foo(a, b, c) { }
}" "A"))
 (check-exn-result result
                   ue:type:duplicate-method
                   '(static-function class)))

(test-case
 "Invoking non-existent function when many similar ones exist"
 (define result (i "
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
 (check-exn-result result
                   ue:type:function-not-in-scope
                   '(return begin "C::main()")))

; ; Dots, this, super

(test-case
 "this on RHS of dot"
 (define result (i "
class A {
  static function main() { return A.this; }
}" "A"))
 (check-exn-result result
                   ue:type:this/super-dot-RHS
                   '(return "A::main()")))

(test-case
 "super on RHS of dot"
 (define result (i "
class A {
  static function main() { return A.super; }
}" "A"))
 (check-exn-result result
                   ue:type:this/super-dot-RHS
                   '(return "A::main()")))

(test-case
 "this in static context"
 (define result (i "
class A {
  static function main() { return this; }
}" "A"))
 (check-exn-result result
                   ue:type:this/super-in-static
                   '(return "A::main()")))

(test-case
 "non-existent class in LHS of dot during funcall"
 (define result (i "
class A {
  static function main() { return B.c(); }
}" "A"))
 (check-exn-result result
                   ue:type:unknown-LHS-dot
                   '(return "A::main()")))

(test-case
 "non-existent class in LHS of dot during field lookup"
 (define result (i "
class A {
  static function main() { return B.c; }
}" "A"))
 (check-exn-result result
                   ue:type:unknown-LHS-dot
                   '(return "A::main()")))

(test-case
 "non-existent class in LHS of dot during assignment"
 (define result (i "
class A {
  static function main() { B.c = 2; return B.c; }
}" "A"))
 (check-exn-result result
                   ue:type:unknown-LHS-dot
                   '(= "A::main()")))

(test-case
 "Static non-instance var in LHS of dot"
 (define result (i "
class A {
  static var x = 2;
  static function main() { return x.y; }
}" "A"))
 (check-exn-result result
                   ue:type:non-instance-dot
                   '(return "A::main()")))

(test-case
 "Instance Non-instance boolean field in LHS of dot"
 (define result (i "
class A {
  var x = true;
  static function main() {
    var a = new A();
    return a.x.y;
  }
}" "A"))
 (check-exn-result result
                   ue:type:non-instance-dot
                   '(return "A::main()")))

(test-case
 "Function return value non-instance var in LHS of dot"
 (define result (i "
class A {
  function getX() { return 2; }
  static function main() {
    var a = new A();
    return a.getX().y;
  }
}" "A"))
 (check-exn-result result
                   ue:type:non-instance-dot
                   '(return "A::main()")))

(test-case
 "this in static context called from instance context"
 (define result (i "
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
 (check-exn-result result
                   ue:type:this/super-in-static
                   '(return "A::nowork(x)" return "A::mightwork()" return "A::main()")))

(test-case
 "super in static context called from instance context"
 (define result (i "
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
 (check-exn-result result
                   ue:type:this/super-in-static
                   '(return "A::nowork(x)" return "A::mightwork()"
                            return "A::main()")))

(test-case
 "attempting to read from super as if var in instance function"
 (define result (i "
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
 (check-exn-result result
                   ue:type:keyword-as-identifier
                   '(return "A::mightwork()" return "A::main()")))

(test-case
 "declaring an instance field named this"
 (define result (i "
class A {
  var this;
  static function main() {
    return 0;
  }
}
" "A"))
 (check-exn-result result
                   ue:type:keyword-as-identifier
                   '(var class)))

(test-case
 "declaring an instance field named super"
 (define result (i "
class A {
  var super;
  static function main() {
    return 0;
  }
}
" "A"))
 (check-exn-result result
                   ue:type:keyword-as-identifier
                   '(var class)))

(test-case
 "declaring a static field named this"
 (define result (i "
class A {
  static var this;
  static function main() {
    return 0;
  }
}" "A"))
 (check-exn-result result
                   ue:type:keyword-as-identifier
                   '(static-var class)))

(test-case
 "declaring a static field named super"
 (define result (i "
class A {
  static var super;
  static function main() {
    return 0;
  }
}" "A"))
 (check-exn-result result
                   ue:type:keyword-as-identifier
                   '(static-var class)))

(test-case
 "declaring a local variable named this"
 (define result (i "
class A {
  static function main() {
    var this;
    return 0;
  }
}" "A"))
 (check-exn-result result
                   ue:type:keyword-as-identifier
                   '(var "A::main()")))

(test-case
 "declaring a local variable named super"
 (define result (i "
class A {
  static function main() {
    var super;
    return 0;
  }
}" "A"))
 (check-exn-result result
                   ue:type:keyword-as-identifier
                   '(var "A::main()")))

(test-case
 "assigning to this"
 (define result (i "
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
 (check-exn-result result
                   ue:type:assigning-to-this/super
                   '(= "A::nowork()" return "A::main()")))

(test-case
 "assigning to super"
 (define result (i "
class A {

  function nowork() {
    super = 2;
    return this;
  }

  static function main() {
    var a = new A();
    return a.nowork();
  }
}" "A"))
 (check-exn-result result
                   ue:type:assigning-to-this/super
                   '(= "A::nowork()" return "A::main()")))

(test-case
 "passing this in a reference parameter"
 (define result (i "
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
 (check-exn-result result
                   ue:type:this-as-ref-param
                   '(funcall "A::mightwork()" return "A::main()")))

(test-case
 "Declaring a function named `this`"
 (define result (i "
class A {
  var x;
  function this() {
    return 5;
  }
  A() {
    x = 0;
  }
  static function main() { return new A(); }
}" "A"))
 (check-exn-result result
                   ue:type:keyword-as-identifier
                   '(function class)))

(test-case
 "Declaring a function named `super`"
 (define result (i "
class A {
  var x;
  function super(x, y) {
    return 5;
  }
  A() {
    x = 0;
  }
  static function main() { return new A(); }
}" "A"))
 (check-exn-result result
                   ue:type:keyword-as-identifier
                   '(function class)))

; ; Constructors

(test-case
 "Cannot call constructor of undeclared class"
 (define result (i "
class A {
  static function main() {
    return new B();
  }
}" "A"))
 (check-exn-result result
                   ue:type:not-a-class
                   '(return "A::main()")))

(test-case
 "Calling super() with no parent class"
 (define result (i "
class A {
  A() {
    super();
  }
  static function main() {
    return new A();
  }
}" "A"))
 (check-exn-result result
                   ue:type:super-w/out-parent
                   '(funcall "A::A()" return "A::main()")))

(test-case
 "Calling this(x) when no such constructor exists"
 (define result (i "
class A {
  A() {
    this(2, 3, 4);
  }
  static function main() {
    return new A();
  }
}" "A"))
 (check-exn-result result
                   ue:type:ctor-DNE
                   '(funcall "A::A()" return "A::main()")))

(test-case
 "Calling super(x) when no such constructor exists in parent."
 (define result (i "
class A {
  A() {
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
 (check-exn-result result
                   ue:type:ctor-DNE
                   '(funcall "B::B()" return "B::main()")))

(test-case
 "Parent doesn't have default ctor, child only has default ctor"
 (define result (i "
class A {
  A(x, y) { }
}
class B extends A {
  static function main() {
    return new B();
  }
}" "B"))
 (check-exn-result result
                   ue:type:ctor-DNE
                   '(funcall "B::B()" return "B::main()")))

(test-case
 "Parent doesn't have default ctor, no explicit super in user-defined ctor."
 (define result (i "
class A {
  A(x, y) { }
}
class B extends A {
  B() { }
  static function main() {
    return new B();
  }
}" "B"))
 (check-exn-result result
                   ue:type:ctor-DNE
                   '(funcall "B::B()" return "B::main()")))

(test-case
 "Calling this() in static non-ctor function"
 (define result (i "
class A {
  static function main() {
    this();
    return 4;
  }
}" "A"))
 (check-exn-result result
                   ue:type:this/super-in-static
                   '(funcall "A::main()")))
(test-case
 "Calling this() in instance non-ctor function"
 (define result (i "
class A {
  function foo() { this(); }
  static function main() {
    var a = new A();
    a.foo();
    return 4;
  }
}" "A"))
 (check-exn-result result
                   ue:type:ctor-chain-outside-ctor
                   '(funcall "A::foo()" funcall "A::main()")))

(test-case
 "Calling this() after first line in constructor"
 (define result (i "
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
 (check-exn-result result
                   ue:type:ctor-chain-outside-ctor
                   '(funcall "A::A()" return "A::main()")))

(test-case
 "Calling this(...) twice, same ctor"
 (define result (i "
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
 (check-exn-result result
                   ue:type:ctor-chain-outside-ctor
                   '(funcall "A::A()" return "A::main()")))

(test-case
 "Calling this(...) twice, different ctor"
 ; tests that cycle detection algorithm isn't linear w.r.t # all ctors
 (define result (i "
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
 (check-exn-result result
                   ue:type:ctor-chain-outside-ctor
                   '(funcall "A::A()" return "A::main()")))

(test-case
 "Calling this(...) as value fun in ctor first line var decl"
 (define result (i "
class A {
  var x;
  A() { x = this(1); }
  A(v) { this.x = v; }
  static function main() { return new A(); }
}" "A"))
 (check-exn-result result
                   ue:type:ctor-chain-outside-ctor
                   '(= "A::A()" return "A::main()")))

(test-case
 "Calling this(...) in ctor first line if statement"
 (define result (i "
class A {
  var x;
  A() { if (true) x = this(1); }
  A(v) { this.x = v; }
  static function main() { return new A(); }
}" "A"))
 (check-exn-result result
                   ue:type:ctor-chain-outside-ctor
                   '(= if "A::A()" return "A::main()")))

(test-case
 "Calling super() after first line in constructor"
 (define result (i "
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
 (check-exn-result result
                   ue:type:ctor-chain-outside-ctor
                   '(funcall "A::A()" return "A::main()")))

(test-case
 "Calling super() then this()"
 (define result (i "
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
 (check-exn-result result
                   ue:type:ctor-chain-outside-ctor
                   '(funcall "A::A()" return "A::main()")))

(test-case
 "Calling this() in same constructor, 1 total"
 (define result (i "
class A {
  var x;
  A() {
    this();
  }
  static function main() {
    return new A();
  }
}" "A"))
 (check-exn-result result
                   ue:type:cyclic-ctor-chaining
                   '(funcall "A::A()" return "A::main()")))

(test-case
 "Calling this() in same constructor, 2 total"
 (define result (i "
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
 (check-exn-result result
                   ue:type:cyclic-ctor-chaining
                   '(funcall "A::A()" return "A::main()")))

(test-case
 "Calling this() in same constructor, many"
 (define result (i "
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
 (check-exn-result result
                   ue:type:cyclic-ctor-chaining
                   '(funcall "A::A()" return "A::main()")))

(test-case
 "Cycle of constructors when chaining, revisits initial constructor"
 (define result (i "
class A {
  A() { this(1); }
  A(x) { this(1, 2); }
  A(x, y) { this(); }
  static function main() { return new A(); }
}" "A"))
 (check-exn-result result
                   ue:type:cyclic-ctor-chaining
                   '(funcall "A::A(x, y)" funcall "A::A(x)"
                             funcall "A::A()" return "A::main()")))

(test-case
 "Cycle of constructors when chaining, does not revisit initial constructor"
 (define result (i "
class A {
  A() { this(1); }
  A(x) { this(1, 2); }
  A(x, y) { this(); }
  A(x, y, z) { this(); }
  static function main() { return new A(1, 2, 3); }
}" "A"))
 (check-exn-result result
                   ue:type:cyclic-ctor-chaining
                   '(funcall "A::A(x, y)" funcall  "A::A(x)" funcall "A::A()"
                             funcall "A::A(x, y, z)" return "A::main()")))

(test-case
 "Colliding constructor signatures"
 (define result (i "
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
 (check-exn-result result
                   ue:type:duplicate-constructor
                   '(constructor class)))

(test-case
 "Duplicate parameters in constructor signature"
 (define result (i "
class A {
  A(a, a) {
    x = 5;
  }
}" "A"))
 (check-exn-result result
                   ue:type:duplicate-parameter
                   '(constructor class)))

(test-case
 "throw in ctor"
 (define result (i "
class A {
  A() {
    throw 5;
  }
  static function main() { return new A(); }
}" "A"))
 (check-exn-result result
                   ue:type:uncaught-exception
                   '(throw "A::A()" return "A::main()")))

(test-case
 "break in ctor called in loop"
 (define result (i "
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
 (check-exn-result result
                   ue:type:break-outside-loop
                   '(break "A::A()" var begin while "A::main()")))

(test-case
 "continue in ctor called in loop"
 (define result (i "
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
 (check-exn-result result
                   ue:type:continue-outside-loop
                   '(continue "A::A()" var begin while "A::main()")))

