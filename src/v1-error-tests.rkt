#lang racket/base

(require "interpreter-extension.rkt"
         "util/testing.rkt")


(define error-file (make-error-tester interpret-v1-file))
(define error-str (make-error-tester interpret-v1-str))

; ; Error Tests

(error-str #:id "accessing var before declaring it"
           #:catch #t
           "return x;")

(error-str #:id "assign w/out declaring"
           #:catch #t
           "
var x = 1;
y = 10 + x;
return y;")

(error-str #:id "reference w/out declare or initializing"
           #:catch #t
           "
var y;
y = x;
return y;")


(error-str #:id "reference w/out initializing"
           #:catch #t
           "
var x;
var y;
x = x + y;
return x;")

(error-str #:id "redeclare in same scope"
           #:catch #t
           "
var x = 10;
var y = 20;
var x = x + y;
return x;")

(error-str #:id "var in block not accessible from subsequent outer scope"
           #:catch #t
           "
var x = 10;
var y = 4;
if (x < y) {
  var min = x;
}
else {
  var min = y;
}
return min;")


(error-str #:id "var in block can't access undeclared var"
           #:catch #t
           "
if (true) {
  a = 0;
}
return a;")

(error-str #:id "var in block not accessible in later block (same depth in stack)"
           #:catch #t
           "
if (true) {
  var a = 0;
}
if (true) {
  a = 2;
}
return a;")

(error-str #:id "var declared in first iter of while loop doesn't persist through later iters)"
           #:catch #t
           "
var x = 0;
while (x < 5) {
  if (x != 0) {
    return a;
  }
  var a = 5;
  x = x + 1;
}
return 0;")

(error-str #:id "break outside of loop"
           #:catch #t
           "
var x = 1;
break;
return x;")

(error-str #:id "continue outside of loop"
           #:catch #t
           "
var x = 1;
continue;
return x;")

(error-str #:id "Top-level throw"
           #:catch #t
           "throw 1;")

(error-str #:id "Uncaught throw in (entered) catch block"
           #:catch #t
           "
var x = 10;
var result = 1;
try {
  while (x < 10000) {
    result = result - 1;
    x = x * 10;
    if (x > 1000)
      throw x;
  }
}
catch (ex) {
  throw 1;
}
return result;")

(error-str #:id "break exit from block pops frame"
           #:catch #t
           "
var x = 0;
while (true) {
  x = x + 1;
  var a = 5;
  if (x == 2) {
    break;
  }
}
return a;
")

(error-str #:id "continue in while body block, pop frame"
           #:catch #t
           "
var x = 1;
var y = 2;
if (x < y) {
  var z = 0;
  while (z < 100) {
    var a = 1;
    z = z + a;
    continue;
    z = 1000;
  }
  if (z != x) {
    z = a;
  }
}
return x;
")

(error-str #:id "throw in block, should pop frame. var from try not visible in catch"
           #:catch #t
           "
try {
  var a = 1;
  throw 0;
}
catch (e) {
  return a;
}
")

(error-str #:id "throw in block, should pop frame. var from try not visible in finally"
           #:catch #t
           "
try {
  var a = 1;
  throw 0;
}
catch (e) {
}
finally {
  return a;
}
")