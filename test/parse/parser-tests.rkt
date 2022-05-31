#lang racket/base
(require rackunit
         "../../src/parse/parser.rkt")

; ; v1

(test-equal?
 "v1, comprehensive"
 (parse-str "
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
")
 '((var x 0)
   (while
    (< x 12)
    (begin
      (try
       ((= x (+ x 10)) (break))
       (catch (e) ((if (|| true false) (return 0))))
       (finally ((= x (+ x 1)))))))
   (return x)))

; ; v2

(test-equal?
 "v2, comprehensive"
 (parse-str "
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
        x = x + divide(e, j,);
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
 '((function divide (x y) ((if (== y 0) (throw 1000000)) (return (/ x y))))
   (function
    main
    ()
    ((var x 0)
     (var j 1)
     (try
      ((while
        (>= j 0)
        (begin
          (var i 10)
          (while
           (>= i 0)
           (begin
             (try
              ((= x (+ x (funcall divide (* 10 i) i))))
              (catch (e) ((= x (+ x (funcall divide e j)))))
              ())
             (= i (- i 1))))
          (= j (- j 1)))))
      (catch (e2) ((= x (* x 2))))
      ())
     (return x)))))

; ; v3

(test-equal?
 "v3, comprehensive"
 (parse-str "
class A extends Bclass {
  static var sfield;
  var ifield = c;
  var ifield = 1 + x.y.funcall();
  A() { super(); x = 3; }
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
}")
 '((class A
     (extends Bclass)
     ((static-var sfield)
      (var ifield c)
      (var ifield (+ 1 (funcall (dot (dot x y) funcall))))
      (constructor () ((funcall super) (= x 3)))
      (constructor (a b) ((funcall this)))
      (abstract-function abstr (& a b))
      (function
       inst
       (a b & c)
       ((funcall (dot this x))
        (funcall (dot super foo) (dot this y))
        (return (dot super x))))
      (static-function main () ((funcall a) (throw (+ 3 3))))))))

; ; Mixture

(test-equal?
 "v1+v2+v3, comprehensive"
 (parse-str "
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

class A { }")
 '((var a 3)
   (function foo () ((return 3)))
   (while false (begin (= a (+ a (funcall foo)))))
   (class B () ())
   (return false)
   (throw 0)
   (try () () (finally ()))
   (if false (begin))
   (class A () ())))

