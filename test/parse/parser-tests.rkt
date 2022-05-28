#lang racket/base
(require rackunit
         "../../src/parse/parser.rkt")

; ; v1

(test-not-exn
 "v1, comprehensive"
 (λ () (parse-str "
var x = 0;
while(x < 12) {
  try{
    x = x + 10;
    break;
  }catch(e) {
   if (true || false) return 0;
  } finally {
     x = x+1;
  }
}
return x;
")))

; ; v2

(test-not-exn
 "v2, comprehensive"
 (λ () (parse-str "
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
}")))

; ; v3

(test-not-exn
 "v3, comprehensive"
 (λ () (parse-str "
class A extends Bclass {
  static var sfield;
  var ifield = c;
  var ifield = 1 + x.y.funcall();
  A() { super(); }
  A(a, b) { this(); }
  function abstr(&a, b);
  function inst(a, b, &c) {
    this.x();
    super.foo(this.y);
    return super.x;
  }
  static function main() {
    a();
    throw 3 + 3;
  }
}")))

; ; Mixture

(test-not-exn
 "v1+v2+v3, comprehensive"
 (λ () (parse-str "
var a = 3;
function foo() { return 3; }

while (false) {
  a = a + foo();
}
class B {}

return false;
throw 0;
try {} finally {}
if (false) {}

class A { }")))


; ; Illegal usages that should be caught
(define-check (test-parser-exn msg prog-str)
  (test-exn msg exn:fail:user? (λ () (parse-str prog-str))))

(test-case
 "control flow in class body"
 (test-parser-exn
  "`if` in class body"
  "class A { if (true) { } }")
 (test-parser-exn
  "`while` in class body"
  "class A { while (true) { } }")
 (test-parser-exn
  "`try` in class body"
  "class A { try { } catch {} }")
 (test-parser-exn
  "return in class body"
  "class A { return 0; }"))

(test-case
 "class in nested statement list"
 (test-parser-exn
  "class in `if`"
  "if (true) { class A {} }")
 (test-parser-exn
  "class in `while`"
  "while (true) { class A {} }")
 (test-parser-exn
  "class in `block`"
  "{ class A {} }")
 (test-parser-exn
  "class in class"
  "class AA { class A {} }")
 (test-parser-exn
  "class in function"
  "function foo() { class A {} }"))

(test-case
 "class members outside class"
 (test-parser-exn
  "static var in global"
  "static var a;")
 (test-parser-exn
  "static fun in global"
  "static function a(){ }")
 (test-parser-exn
  "abstract function in global"
  "function a();")
 (test-parser-exn
  "abstract function in block"
  "{ function a(); }"))

