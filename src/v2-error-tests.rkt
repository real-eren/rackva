#lang racket/base

(require "interpreter-extension.rkt"
         "user-errors.rkt"
         rackunit)


(define i
  (lambda (program)
    (define user-exn (λ (exn s) exn))
    (interpret-v2-str program
                      #:return (λ (v s) (fail-check "expected an error"))
                      #:user-exn user-exn
                      #:throw (λ (e s) (user-exn (ue:uncaught-exception e) s)))))

(test-case
 "invoking nonexistent function in top level var declaration"
 (define exn (i "
var x = f();

function main() {
  return x;
}"))
 (check-equal? (ue:type exn) ue:type:function-not-in-scope))

(test-case
 "invoking function before definition in top level var declaration"
 (define exn (i "
var x = f();

function f() { return 2; }

function main() {
  return x;
}"))
 (check-equal? (ue:type exn) ue:type:function-not-in-scope))

(test-case
 "invoking nested function before its definition in the same scope"
 (define exn (i "
function main() {
  a = nested();
  function nested() { return 1; }
  return a;
}"))
 (check-equal? (ue:type exn) ue:type:function-not-in-scope))

(test-case
 "Passing too many arguments"
 (define exn (i "
function f(a) { return a*a; }

function main() {
  return f(10, 11, 12);
}"))
 (check-equal? (ue:type exn) ue:type:function-not-in-scope))

(test-case
 "Passing too few arguments"
 (define exn (i "
function f(a, b, c) { return a + b + c; }
function main() {
  return f(1);
}"))
 (check-equal? (ue:type exn) ue:type:function-not-in-scope))

(test-case
 "Passing expressions to a reference parameter"
 (define exn (i "
function f(&a) { a = a + 1; }
function main() {
  return f(1);
}"))
 (check-equal? (ue:type exn) ue:type:non-var-in-ref-param))

(test-case
 "Passing an undeclared var to a reference parameter"
 (define exn (i "
function f(&a) { a = a + 1; }
function main() {
  return f(x);
}"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "Functions inside functions accessing out of scope variables."
 (define exn (i "
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
}"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "global function defined twice"
 (define exn (i "
function foo() { return 1; }
function foo() { return 2; }
function main() {
  var x = foo();
  return x;
}"))
 (check-equal? (ue:type exn) ue:type:duplicate-function))

(test-case
 "function defined twice, some params change to ref"
 (define exn (i "
function foo(x, y) { return 1; }
function foo(&x, &y) { return 2; }
function main() {
  var x = foo();
  return x;
}"))
 (check-equal? (ue:type exn) ue:type:duplicate-function))

(test-case
 "absent main function"
 (define exn (i "var a = 2;"))
 (check-equal? (ue:type exn) ue:type:function-not-in-scope))

(test-case
 "main without return"
 (define exn (i "
function main() {
  var a = 2;
}
"))
 (check-equal? (ue:type exn) ue:type:no-return-value-fun))

(test-case
 "throw in main"
 (define exn (i "
function main() {
  throw 1;
}"))
 (check-equal? (ue:type exn) ue:type:uncaught-exception))

(test-case
 "break in main w/out while"
 (define exn (i "
function main() {
  break;
}"))
 (check-equal? (ue:type exn) ue:type:break-outside-loop))

(test-case
 "break in function w/out while"
 (define exn (i "
function f() { break; }
function main() {
  return f();
}"))
 (check-equal? (ue:type exn) ue:type:break-outside-loop))

(test-case
 "break in function in while"
 (define exn (i "
function f() { break; }
function main() {
  var x = 5;
  while (x != 0) {
    f();
    x = x - 1;
  }
  return 0;
}"))
 (check-equal? (ue:type exn) ue:type:break-outside-loop))

(test-case
 "continue in main w/out while"
 (define exn (i "
function main() {
  continue;
}"))
 (check-equal? (ue:type exn) ue:type:continue-outside-loop))

(test-case
 "continue in function w/out while"
 (define exn (i "
function f() { continue; }
function main() {
  return f();
}"))
 (check-equal? (ue:type exn) ue:type:continue-outside-loop))

(test-case
 "continue in function in while"
 (define exn (i "
function f() { continue; }
function main() {
  var x = 5;
  while (x != 0) {
    f();
    x = x - 1;
  }
  return 0;
}"))
 (check-equal? (ue:type exn) ue:type:continue-outside-loop))

(test-case
 "global var initializer invokes later global function"
 (define exn (i "
var x = f();
function f() { return 5; }
function main() {
  return x;
}"))
 (check-equal? (ue:type exn) ue:type:function-not-in-scope))

(test-case
 "nested fun refers to 'nephew' nested fun"
 (define exn (i "
function main() {
  function bar() { return nephew(); }
  function sibling() {
    function nephew() { return 5; }
  }
  return bar();
}"))
 (check-equal? (ue:type exn) ue:type:function-not-in-scope))

(test-case
 "nested fun refers to 'cousin' nested fun. Same height different branches"
 (define exn (i "
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
}"))
 (check-equal? (ue:type exn) ue:type:function-not-in-scope))

; not specified whether params can be shadowed by locals
(test-case
 "local name collides with param name"
 (define exn (i "
function foo(x) {
  var x = 2;
  return x;
}
function main() {
  return foo(5);
}"))
 (check-equal? (ue:type exn) ue:type:duplicate-variable))

(test-case
 "reading from reference to uninitialized variable"
 (define exn (i "
function foo(&a) { return a; }
function main() {
  var x;
  return foo(x);
}
"))
 (check-equal? (ue:type exn) ue:type:access-uninitialized-var))

(test-case
 "function w/out return used as expression"
 (define exn (i "
function noReturn() {
  var x;
  x = 2;
}

function main() {
  return noReturn();
}"))
 (check-equal? (ue:type exn) ue:type:no-return-value-fun))
