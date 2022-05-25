# Rackva interpreter
name inspired by Jython  
Rava was taken

Interpreter for a Java-like language, written in Scheme/Racket.
Apart from variables and fields being boxed, within the intepreter, values are immutable and functions are referentially-transparent.

# Authors
### v1.0.0 - v3.0.1:
 Duc Huy Nguyen, Eren Kahriman, Loc Nguyen

### v3.0.1+:
 Eren Kahriman 


## Parsers and lexer:
The 3 parsers and lexer are lightly modified copies of those given in the assignment.
They were modified to also support string-form programs.

# Language Features:
- Dynamic typing
- Stack trace for user errors (shown later)


## V1
- Int and Bool literals, expressions
- Basic Arithmetic and Logical operators
- Short-circuiting || and &&
- Variables
- If, While, Try, block statements

### Sample
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

## V2
- V1 features
- functions
  - top-level and nested
  - pass-by-value and pass-by-reference parameters
  - w/ or w/out return value
  - overloading by # parameters

### Sample
```js
function foo() {
  return 1;
}

function foo(x, y) {
  return x + y;
}
var globalVar = foo();

function main() {
  return globalVar + foo(100 * foo(), 1000);
}
```
returns 1101

## V3
- V2 features
- classes
  - static members
  - instance members
  - user-defined constructors
    - constructor chaining
  - polymorphism
  - abstract methods
- dot operator

### Sample
```js
class A {
    var instField1;
    var instField2 = true;
    var instField3 = foo();

    static var staticField = new A();

    A() {}
    A(a, b) {}

    static function staticFun(valueParam, &refParam) {}
}

class B extends A {
    var instField1;

    B() {
      this(1, false);
    }
    B(x, y) {
      super(x, y);
      if (y) throw this.instField1;
    }

    static function main() {
        A.staticField = a.b().c.d();
        var b = new B();
        return b.instField3;
    }
}
```



## Stack trace examples:
(called in `racket` with `interpret-v3-str` and `interpret-v2-str`)

---
```js
class A {
  A() { this(1); }
  A(x) { this(1, 2); }
  A(x, y) { this(); }
  A(x, y, z) { this(); }
  static function main() { return new A(1, 2, 3); }
}
```
results in
```
Error: Cyclic constructor chaining. At least one constructor must call super(...)
Source:
----------
this();

A::A(x, y)

this(1, 2);

A::A(x)

this(1);

A::A()

this();

A::A(x, y, z)

return new A(1, 2, 3);

A::main()
----------
```

---
```js
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
```
results in
```
Error: A function `foo` with 14 parameter(s) is not in scope.
Did you mean one of the following?
----------
C::foo(a, &b, c, &d)
A::foo(a)
A::foo()
B::foo(a, b, c)
B::foo(a, b)
C::foo(a, &b, c, &d, e, f, g)
----------

Source:
----------
return foo(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

{
  function foo(a, &b, c, &d) {
    return 7;
  }
  return foo(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

C::main()
----------
```

---
```js
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
```
results in
```
Error: uncaught exception: 5
Source:
----------
throw 5;

A::A(a, &b, c)

return new A(x, y, y);

B::f2(x, &y)

return f2(0, c);

B::f1()

a = f1();

B::init()

B::B()

return new B();

C::main()
----------
```

---
```js
function f(&a) { a = a + 1; }
function main() {
  return f(1);
```
results in
```
Error: `f(&a)` expects a reference for `a`
Source:
----------
return f(1);

main()
----------
```
