#lang racket/base

(require "interpreter-extension.rkt"
         "util/testing.rkt")


(define error-file (make-error-tester interpret-v2-file))
(define error-str (make-error-tester interpret-v2-str))


(error-str #:id "invoking nonexistent function in top level var declaration"
           #:catch #t
           "
var x = f();

function main() {
  return x;
}")

(error-str #:id "invoking function before definition in top level var declaration"
           #:catch #t
           "
var x = f();

function f() { return 2; }

function main() {
  return x;
}")

(error-str #:id "invoking nested function before its definition in the same scope"
           #:catch #t
           "
function main() {
  a = nested();
  function nested() { return 1; }
  return a;
}")

(error-str #:id "Passing too many arguments"
           #:catch #t
           "
function f(a) { return a*a; }

function main() {
  return f(10, 11, 12);
}")

(error-str #:id "Passing too few arguments"
           #:catch #t
           "
function f(a, b, c) { return a + b + c; }
function main() {
  return f(1);
}")

(error-str #:id "Passing expressions to a reference parameter"
           #:catch #t
           "
function f(&a) { a = a + 1; }
function main() {
  return f(1);
}")

(error-str #:id "Passing an undeclared var to a reference parameter"
           #:catch #t
           "
function f(&a) { a = a + 1; }
function main() {
  return f(x);
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
function foo() { return 1; }
function foo() { return 2; }
function main() {
  var x = foo();
  return x;
}")

(error-str #:id "function defined twice, some params change to ref"
           #:catch #t
           "
function foo(x, y) { return 1; }
function foo(&x, &y) { return 2; }
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
function f() { break; }
function main() {
  return f();
}")

(error-str #:id "break in function in while"
           #:catch #t
           "
function f() { break; }
function main() {
  var x = 5;
  while (x != 0) {
    f();
    x = x - 1;
  }
  return 0;
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
function f() { continue; }
function main() {
  return f();
}")

(error-str #:id "continue in function in while"
           #:catch #t
           "
function f() { continue; }
function main() {
  var x = 5;
  while (x != 0) {
    f();
    x = x - 1;
  }
  return 0;
}")

(error-str #:id "global var initializer invokes later global function"
           #:catch #t
           "
var x = f();
function f() { return 5; }
function main() {
  return x;
}")

(error-str #:id "nested fun refers to 'nephew' nested fun"
           #:catch #t
           "
function main() {
  function bar() { return nephew(); }
  function sibling() {
    function nephew() { return 5; }
  }
  return bar();
}")

(error-str #:id "nested fun refers to 'cousin' nested fun. Same height different branches"
           #:catch #t
           "
function main() {
  function a1() {
    function a2() {
      function a3() { return b2(); }
      return a3();
    }
    return a2();
  }
  function b1() {
    function b2() { return 5; }
    function b2b() { return a1(); }
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
function foo(&a) { return a; }
function main() {
  var x;
  return foo(x);
}
")

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
