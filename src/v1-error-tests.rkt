#lang racket/base

(require "interpreter-extension.rkt"
         "user-errors.rkt"
         rackunit)


(define i
  (lambda (program)
    (define user-exn (λ (exn s) exn))
    (interpret-v1-str program
                      #:return (λ (v s) (error "expected an error"))
                      #:user-exn user-exn
                      #:throw (λ (e s) (user-exn (ue:uncaught-exception e) s)))))

(test-case
 "accessing var before declaring it"
 (define exn (i "return x;"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "assign w/out declaring, statement"
 (define exn (i "
var x = 1;
y = 10 + x;
return y;"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "assign w/out declaring, expression"
 (define exn (i "
var x = 1;
return (y = 10 + x);"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "reference w/out declare or initializing"
 (define exn (i "
var y;
y = x;
return y;"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))


(test-case
 "reference w/out initializing"
 (define exn (i "
var x;
var y;
x = x + y;
return x;"))
 (check-equal? (ue:type exn) ue:type:access-uninitialized-var))

(test-case
 "redeclare in same scope"
 (define exn (i "
var x = 10;
var y = 20;
var x = x + y;
return x;"))
 (check-equal? (ue:type exn) ue:type:duplicate-variable))

(test-case
 "var in block not accessible from subsequent outer scope"
 (define exn (i "
var x = 10;
var y = 4;
if (x < y) {
  var min = x;
}
else {
  var min = y;
}
return min;"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))


(test-case
 "var in block can't access undeclared var"
 (define exn (i "
if (true) {
  a = 0;
}
return a;"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "var in block not accessible in later block (same depth in stack)"
 (define exn (i "
if (true) {
  var a = 0;
}
if (true) {
  a = 2;
}
return a;"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "var declared in first iter of while loop doesn't persist through later iters"
 (define exn (i "
var x = 0;
while (x < 5) {
  if (x != 0) {
    return a;
  }
  var a = 5;
  x = x + 1;
}
return 0;"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "break outside of loop"
 (define exn (i "
var x = 1;
break;
return x;"))
 (check-equal? (ue:type exn) ue:type:break-outside-loop))

(test-case
 "continue outside of loop"
 (define exn (i "
var x = 1;
continue;
return x;"))
 (check-equal? (ue:type exn) ue:type:continue-outside-loop))

(test-case
 "Top-level throw"
 (define exn (i "throw 1;"))
 (check-equal? (ue:type exn) ue:type:uncaught-exception))

(test-case
 "Uncaught throw in (entered) catch block"
 (define exn (i "
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
return result;"))
 (check-equal? (ue:type exn) ue:type:uncaught-exception))

(test-case
 "break exit from block pops frame"
 (define exn (i "
var x = 0;
while (true) {
  x = x + 1;
  var a = 5;
  if (x == 2) {
    break;
  }
}
return a;
"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "continue in while body block, pop frame"
 (define exn (i "
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
"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "throw in block, should pop frame. var from try not visible in catch"
 (define exn (i "
try {
  var a = 1;
  throw 0;
}
catch (e) {
  return a;
}
"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "throw in block, should pop frame. var from try not visible in finally"
 (define exn (i "
try {
  var a = 1;
  throw 0;
}
catch (e) {
}
finally {
  return a;
}
"))
 (check-equal? (ue:type exn) ue:type:reference-undeclared-var))

(test-case
 "program without return statement"
 (define exn (i "var x = 2;"))
 (check-equal? (ue:type exn) ue:type:did-not-return))

