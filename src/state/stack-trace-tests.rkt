#lang racket/base
;;;; Unit tests for the stack trace

(require rackunit
         "../interpreter.rkt"
         "../functionParser.rkt"
         "state.rkt"
         "../interpreter-extension.rkt"
         "../util/testing.rkt")

; interpret a program and return the state
(define interpret-str:stack-trace
  (lambda (str)
    (interpret-parse-tree-v2 (parser-str str)
                             (lambda (v s) (error "should not reach"))
                             (lambda (e s) (state:context-stack s)))))

(define test-str
  (lambda (expected str #:id [test-id #f])
    0));(check-equal? (interpret-str:stack-trace str) expected (format-test-id test-id))))



(test-str #:id "stack trace unwinds after function call"
          '(bar foo top-level)
           "
function foo() { return bar(); }
function bar() { throw 1; }
function ok() {
  function shouldnot() { return appear(); }
  function appear() { return instacktrace(); }
  function instacktrace() { return 100; }
  return shouldnot();
}
var a = ok();
var b = foo();
")

(test-str #:id "stack trace unwinds between function calls in expression."
          '(d top-level)
          "
function a() { return 1; }
function b() { return 2; }
function c() { return 3; }
function d() { throw 5; }
var a = a() + b() + c() + d();
")

(test-str #:id "stack-trace preserved after deeply nested throw"
          '(c b a main)
          "
function c() { throw 123; }
function b() { c(); }
function a() { b(); }
function main() {
  a();
}")

(test-str #:id "stack trace of throw discarded after being caught"
          '(g3 g2 g1 main)
          "
function f1() { f2(); }
function f2() { f3(); }
function f3() { throw 10; }

function g1() { g2(); }
function g2() { g3(); }
function g3() { throw 5; }

function main() {
  try {
    f1();
  }
  catch (e) {
    g1();
  }
}")

(test-str #:id "stack trace of uncaught throw discarded after new exception in finally. only g1-3 should appear"
          '(g3 g2 g1 main)
          "
function f1() { f2(); }
function f2() { f3(); }
function f3() { throw 10; }

function g1() { g2(); }
function g2() { g3(); }
function g3() { throw 5; }

function main() {
  try {
    f1();
  }
  finally {
    g1();
  }
}")
