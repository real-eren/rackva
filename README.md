# Rava interpreter
name inspired by Jython

Interpreter for a Java-like language, written in Scheme/Racket.
Apart from variables and fields being boxed, within the intepreter, values are immutable and functions are referentially-transparent.

## Authors
v1.0.0 - v3.0.1: Duc Huy Nguyen, Eren Kahriman, Loc Nguyen

v3.0.1+ : Eren Kahriman 

### Parsers and lexer:
The 3 parsers and lexer are lightly modified copies of those given in the assignment.
They were modified to also support string form programs.

## Language Features:
- Dynamic typing
- Stack trace for user errors
### Stack trace examples:
---
```
(interpret-v3-str "
class A {
  A() { this(1); }
  A(x) { this(1, 2); }
  A(x, y) { this(); }
  A(x, y, z) { this(); }
  static function main() { return new A(1, 2, 3); }
}" "A")
```
results in
```
Cyclic constructor chaining. At least one constructor must call super(...)
stack trace: top-level -> A::main() -> new A(x, y, z) -> new A() -> new A(x) -> new A(x, y)
```
---
```
(interpret-v3-str "
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
" "C")
```
results in
```
A function `foo` with 14 parameter(s) is not in scope.
Did you mean one of the following?
----------
C::foo(a, &b, c, &d)
A::foo(a)
A::foo()
B::foo(a, b, c)
B::foo(a, b)
C::foo(a, &b, c, &d, e, f, g)
----------

stack trace: top-level -> C::main()
```
---

```
(interpret-v3-str "
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
" "C")
```
results in
```
uncaught exception: 5
stack trace: top-level -> C::main() -> new B() -> B::init() -> B::f1() -> B::f2(x, &y) -> new A(a, &b, c)
```
---

```
(interpret-v2-str "
function f(&a) { a = a + 1; }
function main() {
  return f(1);
}")
```
results in
```
Function `f(&a)` expects a reference for `a`
stack trace: top-level -> main()
```



### V1
- Int and Bool literals, expressions
- Basic Arithmetic and Logical operators
- Short-circuiting || and &&
- Variables
  - nestable assignment expressions
- If, While, Try-catch-finally, block statements

#### Sample
```js
var x = 0;
var y;
y = true;
while(x < 12) {
  try {
    x = x + 10;
    {
      var x = (1 - 2 / 3 % 4 * 5) == 1 || !((x = 1 / 0) != 0);
    }
    continue;
  } catch(e) {
    y = e;
  } finally {
     x = x + 1;
  }
  throw 2+2;
}
return x;
```
returns 22

### V2
- V1 features
- functions
  - top-level and nested
  - pass-by-value and pass-by-reference parameters
  - w/ or w/out return value
  - overloading by # parameters

#### Sample
```js
function foo() {
  return 1;
}

function foo(x, y) {
  return x + y;
}
var globalVar = foo();

function main() {
  
  function foo(a, b, c) {
    return a*b*c;
  }
  
  return globalVar + foo(100 * foo(), 1000) + foo(1, 2, 5);
}
```
returns 1111

### V3
- V2 features
- classes
  - static members
  - instance members
  - user-defined constructors
    - constructor chaining
  - polymorphism
  - abstract methods
- dot operator

#### Sample
see v3 tests

