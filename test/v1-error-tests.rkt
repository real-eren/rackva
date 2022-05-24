#lang racket/base

(require "error-test-shared.rkt"
         "../src/interpreter-extension.rkt"
         "../src/user-errors.rkt"
         rackunit)


(define (i program)
  (interpret-v1-str program
                    #:return (λ (v s) (fail-check "expected an error"))
                    #:user-exn test-user-exn
                    #:throw (λ (e s)
                              (test-user-exn (ue:uncaught-exception e) s))))

(test-case
 "user-exn raised as user error in normal interpret"
 (check-exn exn:fail:user?
            (λ () (interpret-v1-str "return x;"))))

; ; VARIABLES

(test-case
 "accessing var before declaring it"
 (define result (i "return x;"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(return)))

(test-case
 "assign w/out declaring, statement"
 (define result (i "
var x = 1;
y = 10 + x;
return y;"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(=)))

(test-case
 "assign w/out declaring, expression"
 (define result (i "
var x = 1;
return (y = 10 + x);"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(return)))

(test-case
 "reference w/out declare or initializing"
 (define result (i "
var y;
y = x;
return y;"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(=)))


(test-case
 "reference w/out initializing"
 (define result (i "
var x;
var y;
x = x + y;
return x;"))
 (check-exn-result result
                   ue:type:access-uninitialized-var
                   '(=)))

(test-case
 "error in LHS of assign reported before error in RHS"
 (define result (i "
x = y;
return x;"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(=))
 (check-equal? (ue:exn:property 'var-name (car result))
               'x))

(test-case
 "redeclare in same scope"
 (define result (i "
var x = 10;
var y = 20;
var x = x + y;
return x;"))
 (check-exn-result result
                   ue:type:duplicate-variable
                   '(var)))

(test-case
 "var in block not accessible from subsequent outer scope"
 (define result (i "
var x = 10;
var y = 4;
if (x < y) {
  var min = x;
}
else {
  var min = y;
}
return min;"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(return)))


(test-case
 "var in block can't access undeclared var"
 (define result (i "
if (true) {
  a = 0;
}
return a;"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(= begin if)))

(test-case
 "var in block not accessible in later block (same depth in stack)"
 (define result (i "
if (true) {
  var a = 0;
}
if (true) {
  a = 2;
}
return a;"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(= begin if)))

(test-case
 "var declared in first iter of while loop doesn't persist through later iters"
 (define result (i "
var x = 0;
while (x < 5) {
  if (x != 0) {
    return a;
  }
  var a = 5;
  x = x + 1;
}
return 0;"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(return begin if begin while)))

(test-case
 "break outside of loop"
 (define result (i "
var x = 1;
break;
return x;"))
 (check-exn-result result
                   ue:type:break-outside-loop
                   '(break)))

(test-case
 "continue outside of loop"
 (define result (i "
var x = 1;
continue;
return x;"))
 (check-exn-result result
                   ue:type:continue-outside-loop
                   '(continue)))

(test-case
 "Top-level throw"
 (define result (i "throw 1;"))
 (check-exn-result result
                   ue:type:uncaught-exception
                   '(throw)))

(test-case
 "Uncaught throw in (entered) catch block"
 (define result (i "
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
 (check-exn-result result
                   ue:type:uncaught-exception
                   '(throw try)))

(test-case
 "break exit from block pops frame"
 (define result (i "
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
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(return)))

(test-case
 "continue in while body block, pop frame"
 (define result (i "
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
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(= begin if begin if)))

(test-case
 "throw in block, should pop frame. var from try not visible in catch"
 (define result (i "
try {
  var a = 1;
  throw 0;
}
catch (e) {
  return a;
}
"))
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(return try)))

(test-case
 "throw in block, should pop frame. var from try not visible in finally"
 (define result (i "
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
 (check-exn-result result
                   ue:type:reference-undeclared-var
                   '(return try)))

(test-case
 "program without return statement"
 (define result (i "var x = 2;"))
 (check-exn-result result
                   ue:type:did-not-return
                   '()))

