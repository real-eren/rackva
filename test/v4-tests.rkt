#lang racket/base
(require "test-shared.rkt"
         "../src/interpreter.rkt"
         rackunit)

(define (im prog-str)
  (i-str prog-str mode:main-func))

(define (ic prog-str class)
  (i-str prog-str (mode:class class)))

(test-equal? "top-level main accesses classes"
             (im "
class A {
  var x = 5;
  static function double(v) { return 2*v; }
}
function main() { return A.double(new A().x); }")
             10)

(test-equal? "class main accesses top-level var"
             (ic "
var b = 6;
class A {
  A() { b = b + 1; }
  static function main() {
    var c;
    c = new A();
    c = new A();
    return b;
  }
}" "A")
             8)

(test-equal? "class main accesses top-level function"
             (ic "
function double(x) { return x * 2; }
class A {
  var v;
  A(v) { this.v = double(v); }
  static function main() {
    return new A(5).v;
  }
}" "A")
             10)

(test-equal? "class members have priority over top-level members within class"
             (ic "
var a = 5;
var b = 6;
function c() { return 7; }
function d() { return 8; }

class A {
  var a = 1;
  static var b = 2;
  function test() {
    return 1000*a + 100*b + 10*c() + d();
  }
  function c() { return 3; }
  static function d() { return 4; }
  static function main() {
    return new A().test();
  }
}" "A")
             1234)

