#lang racket/base

(require "../test-shared.rkt"
         rackunit)

(define (i prog-str)
  (i-str prog-str mode:main-func))

; ; Simple Tests

(test-equal? "return a number literal"
             (i "
function main() {
  return 150;
}")
             150)

(test-equal? "return a boolean literal"
             (i "
function main() {
  return true;
}")
             'true)

(test-equal? "return a nested expression"
             (i "
function main() {
  return 6 * (8 + (5 % 3)) / 11 - 9;
}")
             -4)

(test-equal? "declare, assign a literal, return"
             (i "
function main() {
  var z;
  z = 10;
  return z;
}")
             10)

(test-equal? "declare with expression value, return"
             (i "
function main() {
  var x = (5 * 7 - 3) / 2;
  return x;
}")
             16)

(test-equal? "declare w/ literal, declare w/ reference to prior var, return product"
             (i "
function main() {
  var x = 10;
  var y = 12 + x;
  return x * y;
}")
             220)

(test-equal? "assign in if statement, <= op"
             (i "
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
             5)

(test-equal? "assign in if statement, >= op"
             (i "
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
             6)

(test-equal? "!= cond"
             (i "
function main() {
  var x = 5;
  var y = 6;
  if (x != y)
    x = 10;
  return x;
}")
             10)

(test-equal? "== cond"
             (i "
function main() {
  var x = 5;
  var y = 6;
  if (x == y)
    x = 10;
  return x;
}")
             5)

(test-equal? "Main with no globals" 
             (i "
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
             10)

; ; Main + other top-level statements

(test-equal? "Main reads global variables."
             (i "
var x = 4;
var y = 6 + x;

function main() {
  return x + y;
}")
             14)

(test-equal? "Main updates global variables"
             (i "
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
             45)

(test-equal? " A recursive function"
             (i "
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
             55)

(test-equal? "Functions with multiple parameters that hide global variables."
             (i "
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
             1)

(test-equal? "static scoping instead of dynamic scoping"
             (i "
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
             115)

(test-equal? "Boolean parameters and return values."
             (i "
function minmax(a, b, min) {
  if (min && a < b || !min && a > b)
    return true;
  else
    return false;
}

function main() {
  return (minmax(10, 100, true) && minmax(5, 3, false));
}")
             'true)

(test-equal? "Multiple function calls in an expression"
             (i "
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
}")
             20)

(test-equal? "A function call in the parameter of a function."
             (i "
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
             24)

(test-equal? "A function call that ignores the return value."
             (i "
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
}")
             2)

(test-equal? "A function without a return statement.(i "
             (i "
var x = 0;
var y = 0;

function setx(a) { x = a; }

function sety(b) { y = b; }

function main() {
  setx(5);
  sety(7);
  return x * y;
}")
             35)

(test-equal? "Functions inside functions.(i "
             (i "
function main() {
  function h() { return 10; }

  function g() { return 100; }

  return g() - h();
}")
             90)

(test-equal? " Functions inside functions accessing variables outside.(i "
             (i "
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
             69)

(test-equal? "Functions inside functions with variables of the same name"
             (i "
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
             87)

(test-equal? "Functions inside functions inside functions.(i "
             (i "
function main() {
  var result;
  var base;

  function getpow(a) {
     var x;

     function setanswer(n) { result = n; }

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
}")
             64)


(test-equal? "try/catch finally, but no exception thrown.(i "
             (i "
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
}")
             125)

(test-equal? "Throwing an exception inside a function."
             (i "
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
}")
             100)

(test-equal? "Throwing an exception from a function"
             (i "
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
}")
             2000400)

(test-equal? "Simple call-by-reference"
             (i "
function swap(&first, &second) {
  var temp = first;
  first = second;
  second = temp;
}
function main() {
  var a = 1;
  var b = 0;
  swap(a, b);
  return b - a;
}")
             1)

(test-equal? "Call-by-value vs Call-by-reference"
             (i "
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
}")
             3421)

; todo: in distance future, after adding feature of loading multiple programs, extract
; this to test-shared
(test-equal? "state changes in left params observed while evaluating following params"
             (i "
var logger = 0;
var pos = 0;

function decimalShiftLeft(val, n) {
  if (n > 0) return decimalShiftLeft(val * 10, n - 1);
  else return val;
}
function numDigits(val) {
  if (val < 10) return 1;
  else return 1 + numDigits(val/10);
}

function log(val) {
  logger = logger + decimalShiftLeft(val, pos);
  pos = pos + numDigits(val);
}

function logAndEcho(val) {
  log(val);
  return val;
}

function foo(a, b) {
  return a + b;
}

function main() {
  var a = logAndEcho(1) + foo( logAndEcho(2), logAndEcho(3) );
  return logger;
}")
             321)

(test-equal? "Assignment side effects with function calls"
             (i "
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
             20332)

(test-equal? "Mixture of call-by-value and call-by-reference."
             (i "
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
             21)

(test-equal? "global function invokes function defined later"
             (i "
function a() { return b(); }

function b() { return 2; }
function main() {
  return a();
}")
             2)

(test-equal? "global function invokes later function that invokes function defined later"
             (i "
function main() {
  return a();
}
function a() { return b(); }
function b() { return 2; }")
             2)

(test-equal? "function w/ return used as statement"
             (i "
function f() { return 2; }
function main() {
  f();
  return 0;
}")
             0)

(test-equal? "param shadows global var"
             (i "
var x = 3;
function decrement(x) {
  x = x - 1;
  return x;
}
function main() {
  return decrement(1);
}")
             0)
; insufficient, should test whether the old values are actually preserved
(test-equal? "recursion between two global functions, matching param names"
             (i "
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
             303)

(test-equal? "nested function refers to later nested function"
             (i "
function main() {
  function b() {
    return 10 + a();
  }
  function a() {
    return 1;
  }
  return b();
}")
             11)

(test-equal? "global function refers to later global var"
             (i "
function getX() { return x; }
var x = 5;

function main() {
  return getX();
}")
             5)

(test-equal? "main function refers to later global var"
             (i "
function main() {
  return x;
}

var x = 5;")
             5)

(test-equal? "nested function refers to later global var"
             (i "
function main() {
  function nested() { return a; }
  return nested();
}
var a = 5;")
             5)

(test-equal? "nested function refers to later local var"
             (i "
function main() {
  function nested() { return a; }
  var a = 5;
  return nested();
}")
             5)

; ; Function Overloading

(test-equal? "two functions with same name, different # params"
             (i "
function foo() {
  return 1;
}
function foo(x) {
  return x;
}
function foo(x, y) {
  return x + y;
}

function main() {
  return foo() + foo(10) + foo(100, 1000);
}")
             1111)
