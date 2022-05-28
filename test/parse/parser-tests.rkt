#lang racket/base
(require rackunit
         (prefix-in simple- "../../src/parse/simpleParser.rkt")
         (prefix-in function- "../../src/parse/functionParser.rkt")
         (prefix-in class- "../../src/parse/classParser.rkt"))

; ; v1

(test-not-exn
 "v1, comprehensive"
 (λ () (simple-parser-str "
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
 (λ () (function-parser-str "
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
 (λ () (class-parser-str "
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
