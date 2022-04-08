#lang racket/base

(require rackunit
         "interpreter.rkt"
         "interpreter-extension.rkt")

; white space / newlines do not affect the parser,
; but are included for readability

(define format-test-id
  (lambda (id)
    (if (eq? id #f)
        #f
        (string-append "test ID: " (if (string? id)
                                       id
                                       (format "~s" id))))))


(define test-file
  (lambda (expected file #:id [test-id #f])
    (check-equal? (interpret file) expected (format-test-id test-id))))

(define test-str
  (lambda (expected str #:id [test-id #f])
    (check-equal? (interpret-str str) expected (format-test-id test-id))))

(define error-file
  (lambda (file #:id [test-id #f] #:catch [suppress #t])
    (if suppress
        (check-exn exn:fail? (lambda () (interpret file)) (format-test-id test-id))
        (test-file 'error file #:id test-id))))

(define error-str
  (lambda (str #:id [test-id #f] #:catch [suppress #t])
    (if suppress
        (check-exn exn:fail? (lambda () (interpret-str str)) (format-test-id test-id))
        (test-str 'error str #:id test-id))))



; ; Simple Tests

(test-str #:id "return a number literal"
          150 "
function main() {
  return 150;
}")

(test-str #:id "return a boolean literal"
          'true "
function main() {
  return true;
}")

(test-str #:id "return a nested expression"
          -4 "
function main() {
  return 6 * (8 + (5 % 3)) / 11 - 9;
}")

(test-str #:id "declare, assign a literal, return"
          10 "
function main() {
  var z;
  z = 10;
  return z;
}")

(test-str #:id "declare with expression value, return"
          16 "
function main() {
  var x = (5 * 7 - 3) / 2;
  return x;
}")

(test-str #:id "declare w/ literal, declare w/ reference to prior var, return sum"
          220 "
function main() {
  var x = 10;
  var y = 12 + x;
  return x * y;
}")

(test-str #:id "assign in if statement, <= op"
          5 "
function main() {
  var x = 5;
  var y = 6;
  var m;
  if (x <= y)
    m = x;
  else
    m = y;
  return m;
}")

(test-str #:id "assign in if statement, >= op"
          6 "
function main() {
  var x = 5;
  var y = 6;
  var m;
  if (x >= y)
    m = x;
  else
    m = y;
  return m;
}")

(test-str #:id "!= cond"
          10 "
function main() {
  var x = 5;
  var y = 6;
  if (x != y)
    x = 10;
  return x;
}")

(test-str #:id "== cond"
          5 "
function main() {
  var x = 5;
  var y = 6;
  if (x == y)
    x = 10;
  return x;
}")

(test-str #:id "Main with no globals" 
          10 "
function main() {
  var x = 10;
  var y = 20;
  var z = 30;
  var min = 0;

  if (x < y)
    min = x;
  else
    min = y;
  if (min > z)
    min = z;
  return min;
}")

; ; Main + other top-level statements

(test-str #:id "Main reads global variables."
          14 "
var x = 4;
var y = 6 + x;

function main() {
  return x + y;
}")

(test-str #:id "Main updates global variables"
          45 "
var x = 1;
var y = 10;
var r = 0;

function main() {
  while (x < y) {
     r = r + x;
     x = x + 1;
  }
  return r;
}")

(test-str #:id " A recursive function"
          55 "
function fib(a) {
  if (a == 0)
    return 0;
  else if (a == 1)
    return 1;
  else 
    return fib(a-1) + fib(a-2);
}

function main() {
  return fib(10);
}")

(test-str #:id "Functions with multiple parameters that hide global variables."
          1 "
function min(x, y, z) {
  if (x < y) {
    if (x < z)
      return x;
    else if (z < x)
      return z;
  }
  else if (y > z)
    return z;
  else
    return y;
}

var x = 10;
var y = 20;
var z = 30;

var min1 = min(x,y,z);
var min2 = min(z,y,x);

function main() {
  var min3 = min(y,z,x);

  if (min1 == min3)
    if (min1 == min2)
      if (min2 == min3)
        return 1;
  return 0;
}")

(test-str #:id "static scoping instead of dynamic scoping"
          115 "
var a = 10;
var b = 20;

function bmethod() {
  var b = 30;
  return a + b;
}

function cmethod() {
  var a = 40;
  return bmethod() + a + b;
}

function main () {
  var b = 5;
  return cmethod() + a + b;
}")

(test-str #:id "Boolean parameters and return values."
          'true "
function minmax(a, b, min) {
  if (min && a < b || !min && a > b)
    return true;
  else
    return false;
}

function main() {
  return (minmax(10, 100, true) && minmax(5, 3, false));
}")

(test-str #:id "Multiple function calls in an expression"
          20 "
function fact(n) {
  var f = 1;
  while (n > 1) {
    f = f * n;
    n = n - 1;
  }
  return f;
}

function binom(a, b) {
  var val = fact(a) / (fact(b) * fact(a-b));
  return val;
}

function main() {
  return binom(6,3);
}
")

(test-str #:id "A function call in the parameter of a function."
          24 "
function fact(n) {
  var r = 1;
  while (n > 1) {
    r = r * n;
    n = n - 1;
  }
  return r;
}

function main() {
  return fact(fact(3) - fact(2));
}")

(test-str #:id "A function call that ignores the return value."
          2 "
var count = 0;

function f(a,b) {
  count = count + 1;
  a = a + b;
  return a;
}

function main() {
  f(1, 2);
  f(3, 4);
  return count;
}
")

(test-str #:id "A function without a return statement. "
          35 "
var x = 0;
var y = 0;

function setx(a) {
  x = a;
}

function sety(b) {
  y = b;
}

function main() {
  setx(5);
  sety(7);
  return x * y;
}
")

(test-str #:id "Functions inside functions. "
          90 "
function main() {
  function h() {
    return 10;
  }

  function g() {
    return 100;
  }

  return g() - h();
}
")

(test-str #:id " Functions inside functions accessing variables outside. "
          69 "
function collatz(n) {
  var counteven = 0;
  var countodd = 0;

  function evenstep(n) {
    counteven = counteven + 1;
    return n / 2;
  }

  function oddstep(n) {
    countodd = countodd + 1;
    return 3 * n + 1;
  }

  while (n != 1) {
    if (n % 2 == 0)
      n = evenstep(n);
    else
      n = oddstep(n);
  }
  return counteven + countodd;
}

function main() {
  return collatz(111);
}")

(test-str #:id "Functions inside functions with variables of the same name"
          87 "
function f(n) {
  var a;
  var b;
  var c;

  a = 2 * n;
  b = n - 10;

  function g(x) {
    var a;
    a = x + 1;
    b = 100;
    return a;
  }

  if (b == 0)
    c = g(a);
  else
    c = a / b;
  return a + b + c;
}

function main() {
  var x = f(10);
  var y = f(20);

  return x - y;
}")

(test-str #:id "Functions inside functions inside functions. "
          64 "
function main() {
  var result;
  var base;

  function getpow(a) {
     var x;

     function setanswer(n) {
        result = n;
     }

     function recurse(m) {
       if (m > 0) {
         x = x * base;
         recurse(m-1);
       }
       else
         setanswer(x);
     }

     x = 1;
     recurse(a);
  }
  base = 2;
  getpow(6);
  return result;
}
")


(test-str #:id "try/catch finally, but no exception thrown. "
          125 "
function divide(x, y) {
  if (y == 0)
    throw y;
  return x / y;
}

function main() {
  var x;

  try {
    x = divide(10, 5) * 10;
    x = x + divide(5, 1);
  }
  catch(e) {
    x = e;
  }
  finally {
    x = x + 100;
  }
  return x;
}
")

(test-str #:id "Throwing an exception inside a function."
          100 "
function divide(x, y) {
  if (y == 0)
    throw y;
  return x / y;
}

function main() {
  var x;

  try {
    x = divide(10, 5) * 10;
    x = x + divide(5, 0);
  }
  catch(e) {
    x = e;
  }
  finally {
    x = x + 100;
  }
  return x;
}
")

(test-str #:id "Throwing an exception from a function"
          2000400 "
function divide(x, y) {
  if (y == 0)
    throw 1000000;
  return x / y;
}

 function main() {
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
        x = x + divide(e, j);
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
")

(test-str #:id "Call-by-reference"
          3421 "
function swap1(x, y) {
  var temp = x;
  x = y;
  y = temp;
}

function swap2(&x, &y) {
  var temp = x;
  x = y;
  y = temp;
}

function main() {
  var a = 1;
  var b = 2;
  swap1(a,b);
  var c = 3;
  var d = 4;
  swap2(c,d);
  return a + 10*b + 100*c + 1000*d;
}
")

(test-str #:id "Assignment side effects with function calls"
          20332 "
var x;

function f(a,b) {
  return a * 100 + b;
}

function fib(f) {
  var last = 0;
  var last1 = 1;

  while (f > 0) {
    f = f - 1;
    var temp = last1 + last;
    last = last1;
    last1 = temp;
  }
  return last;
}

function main() {
  var y;
  var z = f(x = fib(3), y = fib(4));
  return z * 100 + y * 10 + x;
}")

(test-str #:id "Mixture of call-by-value and call-by-reference."
          21 "
function gcd(a, &b) {
  if (a < b) {
    var temp = a;
    a = b;
    b = temp;
  }
  var r = a % b;
  while (r != 0) {
    a = b;
    b = r;
    r = a % b;
  }
  return b;
}
function main () {
  var x = 14;
  var y = 3 * x - 7;
  gcd(x,y);
  return x+y;
}")

(test-str #:id "global function invokes function defined later"
          2
          "
function a() {
  return b();
}

function b() {
  return 2;
}
function main() {
  return a();
}")

(test-str #:id "global function invokes later function that invokes function defined later"
          2
          "
function main() {
  return a();
}
function a() {
  return b();
}
function b() {
  return 2;
}")

(test-str #:id "function w/ return used as statement"
          0
          "
function f() {
  return 2;
}
function main() {
  f();
  return 0;
}")

(test-str #:id "param shadows global var"
          0
          "
var x = 3;
function decrement(x) {
  x = x - 1;
  return x;
}
function main() {
  return decrement(1);
}")

(test-str #:id "recursion between two global functions, matching param names"
          303
          "
function a(n, i) {
  if (i > 0) {
    return b(n + 1, i-1);
  }
  return n;
}
function b(n, i) {
  if (i > 0) {
    return a(n + 100, i-1);
  }
  return n;
}
function main() {
  return a(0, 6);
}")

(test-str #:id "nested function refers to later nested function"
          11
          "
function main() {
  function b() {
    return 10 + a();
  }
  function a() {
    return 1;
  }
  return b();
}")

(test-str #:id "global function refers to later global var"
          5
          "
function getX() {
  return x;
}
var x = 5;

function main() {
  return getX();
}")

(test-str #:id "main function refers to later global var"
          5
          "
function main() {
  return x;
}

var x = 5;")

(test-str #:id "nested function refers to later global var"
          5
          "
function main() {
  function nested() {
    return a;
  }
  return nested();
}
var a = 5;")


(test-str #:id "nested function refers to later local var"
          5
          "
function main() {
  function nested() {
    return a;
  }
  var a = 5;
  return nested();
}")


; ; Error Tests

(error-str #:id "invoking undefined function in top level var declaration"
           #:catch #t
           "
var x = f();

function f() {
  return 2;
}

function main() {
  return x;
}")

(error-str #:id "invoking nested function before its definition in the same scope"
           #:catch #t
           "
function main() {
  a = nested();
  function nested() {
    return 1;
  }
  return a;
}")

(error-str #:id "Passing too many arguments"
           #:catch #t
           "
function f(a) {
  return a*a;
}

function main() {
  return f(10, 11, 12);
}")

(error-str #:id "Passing too few arguments"
           #:catch #t
           "
function f(a, b, c) {
  return a + b + c;
}
function main() {
  return f(1);
}")

(error-str #:id "Passing expressions to a reference parameter"
           #:catch #t
           "
function f(&a) {
  a = a + 1;
}
function main() {
  return f(1);
}")

(error-str #:id "Functions inside functions accessing out of scope variables."
           #:catch #t
           "
function f(x) {
  function g(x) {
    var b;
    b = x;
    return 0;
  }

  function h(x) {
    b = x;
    return 1;
  }

  return g(x) + h(x);
}

function main() {
  return f(10);
}")

(error-str #:id "global function defined twice"
           #:catch #t
           "
function foo() {
  return 1;
}
function foo() {
  return 2;
}
function main() {
  var x = foo();
  return x;
}")

(error-str #:id "absent main function"
           #:catch #t
           "var a = 2;")

(error-str #:id "main without return"
           #:catch #t
           "
function main() {
  var a = 2;
}
")

(error-str #:id "throw in main"
           #:catch #t
           "
function main() {
  throw 1;
}")

(error-str #:id "break in main w/out while"
           #:catch #t
           "
function main() {
  break;
}")

(error-str #:id "break in function w/out while"
           #:catch #t
           "
function f() {
  break;
}
function main() {
  return f();
}")

(error-str #:id "continue in main w/out while"
           #:catch #t
           "
function main() {
  continue;
}")

(error-str #:id "continue in function w/out while"
           #:catch #t
           "
function f() {
  continue;
}
function main() {
  return f();
}")

(error-str #:id "global var initializer invokes later global function"
           #:catch #t
           "
var x = f();
function f() {
  return 5;
}
function main() {
  return x;
}")

(error-str #:id "nested fun refers to 'nephew' nested fun"
           #:catch #t
           "
function main() {
  function bar() {
    return nephew();
  }
  function sibling() {
    function newphew() {
      return 5;
    }
  }
  return bar();
}")

(error-str #:id "nested fun refers to 'cousin' nested fun. Same height different branches"
           #:catch #t
           "
function main() {
  function a1() {
    function a2() {
      function a3() {
        return b2();
      }
      return a3();
    }
    return a2();
  }
  function b1() {
    function b2() {
      return 5;
    }
    function b2b() {
      return a1();
    }
    return b2b();
  }
  return b1();
}")

; not specified whether params can be shadowed by locals
(error-str #:id "local name collides with param name"
           #:catch #t
           "
function foo(x) {
  var x = 2;
  return x;
}
function main() {
  return foo(5);
}")

(error-str #:id "reading from reference to uninitialized variable"
           #:catch #t
           "
function foo(&a) {
  return a;
}
function main() {
  var x;
  return foo(x);
}
")

(error-str #:id "stack trace unwinds after function call"
           #:catch #t ; set to #f to check the stack-trace
           "
function foo() {
  return bar();
}
function bar() {
  continue;
}
function ok() {
  function ok2() {
    return 1;
  }
  return ok2();
}
var a = ok();
var b = foo();
")

(error-str #:id "stack trace unwinds between function calls in expression"
           #:catch #t
           "
function a() {
  return 1;
}
function b() {
  return 2;
}
function c() {
  return 3;
}
var a = a() + b() + c() + d();
")

(error-str #:id "stack-trace preserved after deeply nested throw"
           #:catch #t
           "
function c() {
  throw 1;
}
function b() {
  c();
}
function a() {
  b();
}
function main() {
  a();
}")

(error-str #:id "function w/out return used as expression"
           #:catch #t
           "
function noReturn() {
  var x;
  x = 2;
}

function main() {
  return noReturn();
}")
