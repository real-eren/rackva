#lang racket

(require "error-test-shared.rkt"
         "../src/interpreter-extension.rkt"
         "../src/user-errors.rkt"
         rackunit)

(define (i program)
  (interpret-v2-str program
                    #:return (λ (v s) (fail-check "expected an error"))
                    #:user-exn test-user-exn
                    #:throw (λ (e s)
                              (test-user-exn (ue:uncaught-exception e) s))))

(test-case
 "user-exn raised as user error in normal interpret"
 (check-exn exn:fail:user?
            (λ () (interpret-v2-str "
var x = f();

function main() {
  return x;
}"))))

; ; FUNCTIONS
(test-case
 "invoking nonexistent function in top level var declaration"
 (define result (i "
var x = f();

function main() {
  return x;
}"))
 (check-exn-result result
                   ue:type:function-not-in-scope
                   '(var)))

(test-case
 "invoking function before definition in top level var declaration"
 (define result (i "
var x = f();

function f() { return 2; }

function main() {
  return x;
}"))
 (check-exn-result result
                   ue:type:function-not-in-scope
                   '(var)))

(test-case
 "invoking nested function before its definition in the same scope"
 (define result (i "
function main() {
  var a = nested();
  function nested() { return 1; }
  return a;
}"))
 (check-exn-result result
                   ue:type:function-not-in-scope
                   '(var "main()")))

(test-case
 "Passing too many arguments"
 (define result (i "
function f(a) { return a*a; }

function main() {
  return f(10, 11, 12);
}"))
 (check-exn-result result
                   ue:type:function-not-in-scope
                   '(return "main()")))

(test-case
 "Passing too few arguments"
 (define result (i "
function f(a, b, c) { return a + b + c; }
function main() {
  return f(1);
}"))
 (check-exn-result result
                   ue:type:function-not-in-scope
                   '(return "main()")))

(test-case
 "Passing expressions to a reference parameter"
 (define result (i "
function f(&a) { a = a + 1; }
function main() {
  return f(1);
}"))
 (check-exn-result result
                   ue:type:non-var-in-ref-param
                   '(return "main()")))

(test-case
 "Passing an undeclared var to a reference parameter"
 (define result (i "
function f(&a) { a = a + 1; }
function main() {
  return f(x);
}"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(return "main()")))

(test-case
 "Functions inside functions accessing out of scope variables."
 (define result (i "
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
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(= "h(x)" return "f(x)" return "main()")))

(test-case
 "global function defined twice"
 (define result (i "
function foo() { return 1; }
function foo() { return 2; }
function main() {
  var x = foo();
  return x;
}"))
 (check-exn-result result
                   ue:type:duplicate-function
                   '(function)))

(test-case
 "function defined twice, some params change to ref"
 (define result (i "
function foo(x, y) { return 1; }
function foo(&x, &y) { return 2; }
function main() {
  var x = foo();
  return x;
}"))
 (check-exn-result result
                   ue:type:duplicate-function
                   '(function)))

(test-case
 "absent main function"
 (define result (i "var a = 2;"))
 (check-exn-result result
                   ue:type:function-not-in-scope
                   '()))

(test-case
 "main without return"
 (define result (i "
function main() {
  var a = 2;
}
"))
 (check-exn-result result
                   ue:type:no-return-value-fun
                   '()))

(test-case
 "throw in main"
 (define result (i "
function main() {
  throw 1;
}"))
 (check-exn-result result
                   ue:type:uncaught-exception
                   '(throw "main()")))

(test-case
 "break in main w/out while"
 (define result (i "
function main() {
  break;
}"))
 (check-exn-result result
                   ue:type:break-outside-loop
                   '(break "main()")))

(test-case
 "break in function w/out while"
 (define result (i "
function f() { break; }
function main() {
  return f();
}"))
 (check-exn-result result
                   ue:type:break-outside-loop
                   '(break "f()" return "main()")))

(test-case
 "break in function in while"
 (define result (i "
function f() { break; }
function main() {
  var x = 5;
  while (x != 0) {
    f();
    x = x - 1;
  }
  return 0;
}"))
 (check-exn-result result
                   ue:type:break-outside-loop
                   '(break "f()" funcall begin while "main()")))

(test-case
 "continue in main w/out while"
 (define result (i "
function main() {
  continue;
}"))
 (check-exn-result result
                   ue:type:continue-outside-loop
                   '(continue "main()")))

(test-case
 "continue in function w/out while"
 (define result (i "
function f() { continue; }
function main() {
  return f();
}"))
 (check-exn-result result
                   ue:type:continue-outside-loop
                   '(continue "f()" return "main()")))

(test-case
 "continue in function in while"
 (define result (i "
function f() { continue; }
function main() {
  var x = 5;
  while (x != 0) {
    f();
    x = x - 1;
  }
  return 0;
}"))
 (check-exn-result result
                   ue:type:continue-outside-loop
                   '(continue "f()" funcall begin while "main()")))

(test-case
 "global var initializer invokes later global function"
 (define result (i "
var x = f();
function f() { return 5; }
function main() {
  return x;
}"))
 (check-exn-result result
                   ue:type:function-not-in-scope
                   '(var)))

(test-case
 "nested fun refers to 'nephew' nested fun"
 (define result (i "
function main() {
  function bar() { return nephew(); }
  function sibling() {
    function nephew() { return 5; }
  }
  return bar();
}"))
 (check-exn-result result
                   ue:type:function-not-in-scope
                   '(return "bar()" return "main()")))

(test-case
 "nested fun refers to 'cousin' nested fun. Same height different branches"
 (define result (i "
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
 (check-exn-result result
                   ue:type:function-not-in-scope
                   '(return "a3()" return "a2()" return "a1()" return
                            "b2b()" return "b1()" return "main()")))

; not specified whether params can be shadowed by locals
(test-case
 "local name collides with param name"
 (define result (i "
function foo(x) {
  var x = 2;
  return x;
}
function main() {
  return foo(5);
}"))
 (check-exn-result result
                   ue:type:duplicate-variable
                   '(var "foo(x)" return "main()")))

(test-case
 "reading from reference to uninitialized variable"
 (define result (i "
function foo(&a) { return a; }
function main() {
  var x;
  return foo(x);
}
"))
 (check-exn-result result
                   ue:type:access-uninitialized-var
                   '(return "main()")))

(test-case
 "function w/out return used as expression"
 (define result (i "
function noReturn() {
  var x;
  x = 2;
}

function main() {
  return noReturn();
}"))
 (check-exn-result result
                   ue:type:no-return-value-fun                  
                   '(return "main()")))

(test-case
 "duplicate parameter names in function"
 (let ([result  (i "function functor(a, a) { }")])
   (check-exn-result result
                     ue:type:duplicate-parameter
                     '(function)))
 (let ([result  (i "function functor(a, b, &a) { }")])
   (check-exn-result result
                     ue:type:duplicate-parameter
                     '(function)))
 (let ([result  (i "
function functor() {
  function inner(a, a) {}
}
function main() { functor(); }")])
   (check-exn-result result
                     ue:type:duplicate-parameter
                     '(function "functor()" funcall "main()"))))
