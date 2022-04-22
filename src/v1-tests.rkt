#lang racket/base

(require "interpreter-extension.rkt"
         "util/testing.rkt")

; white space / newlines do not affect the parser,
; but are included for readability


(define test-str (make-tester interpret-v1-str))

; ; Literal Tests
(test-str #:id "return a positive literal"
          150 "return 150;")

(test-str #:id "return a negative literal"
          -150 "return -150;")

(test-str #:id "return a true literal"
          'true "return true;")

(test-str #:id "return a false literal"
          'false "return false;")

; ; Number Tests

(test-str #:id "return a nested expression"
          -4 "return 6 * (8 + (5 % 3)) / 11 - 9;")

(test-str #:id "declare, assign a literal, return"
          10 "
var z;
z = 10;
return z;")

(test-str #:id "declare with expression value, return"
          16 "
var x = (5 * 7 - 3) / 2;
return x;")

(test-str #:id "declare w/ literal, declare w/ reference to prior var, return sum"
          220 "
var x = 10;
var y = 12 + x;
return x * y;")

(test-str #:id "assign in if statement, <= op"
          5 "
var x = 5;
var y = 6;
var m;
if (x <= y)
  m = x;
else
  m = y;
return m;")

(test-str #:id "assign in if statement, >= op"
          6 "
var x = 5;
var y = 6;
var m;
if (x >= y)
  m = x;
else
  m = y;
return m;")

(test-str #:id "!= cond"
          10 "
var x = 5;
var y = 6;
if (x != y)
  x = 10;
return x;")

(test-str #:id "== cond"
          5 "
var x = 5;
var y = 6;
if (x == y)
  x = 10;
return x;")

(test-str #:id "negative sign on nested expression"
          -39 "return 6 * -(4 * 2) + 9;")

(test-str #:id "return immediately upon first encountered return statement"
          25 "
var x = 0;
x = x + 25;
return x;
x = x + 25;
return x;
x = x + 25;
return x;")


; ; Boolean, If, While Tests

(test-str #:id "return nested bool expr"
          'true "return (10 > 20) || (5 - 6 < 10) && true;")

(test-str #:id "return literal in if-else"
          100 "
var x = 10;
var y = 20;
if (x < y && (x % 2) == 0)
  return 100;
else
  return 200;")

(test-str #:id "if with block statements in then and else"
          100 "
var x = 10;
var y = 20;
if (x < y && (x % 2) == 0) {
  return 100;
} else {
  return 200;
}")

(test-str #:id "if w/ empty then block"
          5 "
var x = 10;
if ((x = x / 2) == 5) {}
return x;
")

(test-str #:id "if w/ empty else block"
          10 "
var x = 10;
if (false) {
  x = 0;
} else {
}
return x;
")

(test-str #:id "return var assigned in if-else"
          'false "
var x = 100 % 2 == 0;
var y = 10 >= 20;
var z;
if (x || y)
  z = y;
else
  z = x;
return z;")

(test-str #:id "return var assigned in if-else"
          'true "
var x = 10;
var y = 20;
var z = 20 >= 10;
if (!z || false)
  z = !z;
else
  z = z;
return z;")

(test-str #:id "update var in while loop, return"
          128 "
var x = 2;
while (x < 100)
  x = x * 2;
return x;")

(test-str #:id "while w/ 1 line block"
          128 "
var x = 2;
while (x < 100) {
  x = x * 2;
}
return x;")

(test-str #:id "update var in while loop, update after, return"
          12 "
var x = 20;
var y = 128;
while (x * x > 128)
  x = x - 1;
x = x + 1;
return x;")

(test-str #:id "while w/ multi-line block"
          12 "
var x = 20;
var y = 128;
while (x * x > 128) {
  x = x + 1;
  x = x - 2;
}
x = x + 1;
return x;")

(test-str #:id "while w/ empty block"
          15 "
var x = 0;
while ((x = x + 1) < 15) {}
return x;
")


; ; Advanced Tests

(test-str #:id "nested assign"
          30 "
var x;
var y;
var z = x = y = 10;
return x + y + z;")

(test-str #:id "assign in if cond"
          11 "
var x;
var y;
x = y = 10;
if ((x = x + 1) > y)
  return x;
else
  return y;")

(test-str #:id "assign x twice in same expr"
          1106 "
var x;
var y = (x = 5) + (x = 6);
return y * 100 + x;")

(test-str #:id "reference x twice in expr, update in left should carry over to right operand"
          12 "
var x = 10;
x = (x = 6) + x;
return x;")

(test-str #:id "update in right operand should not affect left operand"
          16 "
var x = 10;
x = x + (x = 6);
return x;")

(test-str #:id "nested assign"
          72 "
var x;
var y;
var z;
var w = (x = 6) + (y = z = 20);
return w + x + y + z;")

(test-str #:id "assign in while cond"
          21 "
var x = 0;
while ((x = x + 1) < 21)
  x = x;
return x;")

(test-str #:id "left-associative side-effects in while cond"
          10 "
var x = 0;
var y = 20;
while ((x = x + 1) < (y = y - 1))
  x = x;
return x;")

(test-str #:id "modulus, assign expr in left and right operands"
          164 "
var a = 31160;
var b = 1476;
var r = a % b;
while (r != 0)
  r = (a = b) % (b = r);
return b;")


(test-str #:id "assign bool to var in condition"
          5 "
var i = 0;
var x;
while (x = true) {
  i = i + 1;
  if (i == 5) break;
}
return i;
")

(test-str #:id "assign bool to var in condition"
          'true "
var i = 0;
var x;
if (x = true) {
  i = i + 1;
}
return x;
")

; ; Short-circuit side-effects

(test-str #:id "short-circuit or, should evaluate second arg"
          12 "
var x = 1;
if (false || ((x = 2) == 2)) {
  x = x + 10;
}
return x;
")

(test-str #:id "short-circuit or, should not evaluate second arg"
          11 "
var x = 1;
if (true || ((x = 2) == 2)) {
  x = x + 10;
}
return x;
")

(test-str #:id "short-circuit and, should evaluate second arg (false)"
          2 "
var x = 1;
if (true && ((x = 2) == 0)) {
  x = x + 10;
}
return x;
")

(test-str #:id "short-circuit and, should evaluate second arg (true)"
          12 "
var x = 1;
if (true && ((x = 2) == 2)) {
  x = x + 10;
}
return x;
")

(test-str #:id "short-circuit and, should not evaluate second arg"
          1 "
var x = 1;
if (false && ((x = 2) == 2)) {
  x = x + 10;
}
return x;
")

;;;; Static scoping


(test-str #:id "var in block shadows outer scope var"
          10 "
var a = 10;
if (true) {
  var a = 2;
}
return a;")

(test-str #:id "inner block can access var from outer scope"
          0 "
var a = 10;
if (true) {
  a = 0;
}
return a;")

(test-str #:id "update var from outer scope"
          20 "
var x = 10;
{
  var y = 2;
  var z = x * y;
  x = z;
}
return x;")

(test-str #:id "update var in multiple nested scopes"
          164 "
var a = 31160;
var b = 1476;
if (a < b) {
  var temp = a;
  a = b;
  b = temp;
}
var r = a % b;
while (r != 0) {
  a = b;
  b = r;
  r = a % b;
}
return b;")


(test-str #:id "nested blocks are consistent"
          3 "
var a = 0;
if (true) {
  var a = 1;
  if (true) {
    var a = 2;
    if (true) {
      var a = 3;
      if (true) {
        return a;
      }
    }
  }
}")


; ; Loop Flow Control

(test-str #:id "while loop with break, single iter"
          1 "
var x = 0;
while(true) {
  x = x + 1;
  if (x == 1) {
    break;
  }
}
return x;
")

(test-str #:id "break in while loop skips remaining statements in body"
          -1 "
var x = 0;
while (x < 10) {
  x = x - 1;
  break;
  x = x + 100;
}
return x;")

(test-str #:id "while loop with break, multiple iters"
          5 "
var x = 0;
while(true) {
  x = x + 1;
  if (x == 5) {
    break;
  }
}
return x;
")

(test-str #:id "nested whiles, break exits only the immediate loop"
          'true "
var x = 0;
var y = 0;
while (x < 5) {
  while (true) {
    y = y + 1;
    if (!(y < 5))
      break;
  }
  x = x + 1;
}
return (x == 5) && (y == 9);
")

(test-str #:id "while loop with continue"
          2 "
var y = 2;
var x = 3;
while(x > y) {
  x = x - 1;
  continue;
  x = x + 1;
}
return x;
")

(test-str #:id "while w/ continue. multiple iters"
          5 "
var accumulator = 0;
var y = 0;
while (y < 10) {
  y = y + 1;
  if (y % 2 == 0)
    continue;
  accumulator = accumulator + 1;
}
return accumulator;
")

(test-str #:id "nested whiles w/ continue, jumps to immediate loop.  multiple iters"
          50 "
var accumulator = 0;
var x = 0;
while (x < 10) {
  var y = 0;
  while (y < 10) {
    y = y + 1;
    if ((y % 2) == 0)
      continue;
    accumulator = accumulator + 1;
  }
  x = x + 1;
}
return accumulator;")

; ; Try catch finally

(test-str #:id "Try catch test #15"
          125 "
var x;
try {
  x = 20;
  if (x < 0)
    throw 10;
  x = x + 5;
}
catch (e) {
  x = e;
}
finally {
  x = x + 100;
}
return x;
")

(test-str #:id "Try catch test #16"
          110 "
var x;
try {
  x = 20;
  if (x > 10)
    throw 10;
  x = x + 5;
}
catch(e) {
  x = e;
}
finally {
  x = x + 100;
}
return x;")

(test-str #:id "Try catch test #17"
          2000400 "
var x = 0;
var j = 1;
try {
  while (j >= 0) {
    var i = 10;
    while (i >= 0) {
      try {
        if (i == 0)
          throw 1000000;
        x = x + 10*i / i;
      }
      catch(e) {
        if (j == 0)
          throw 1000000;
        x = x + e / j;
      }
      i = i - 1;
    }
    j = j - 1;
  }
}
catch (e2) {
  x = x * 2;
}
return x;")

(test-str #:id "Side effect in finally"
          101 "
var x = 10;
var result = 1;
try {
  while (x < 10000) {
     result = result - 1;
     x = x + 10;
     if (x > 1000) {
       throw x;
     }
     else if (x > 100) {
        break;
     }
  }
}
finally {
  result = result + x;
}
return result;
")

(test-str #:id "try in loop, try has break and finally block"
          505 "
var x = 0;
var i = 0;
while (i < 5) {
  try {
    x = x + 1;
    i = i + 1;
    if (i == 5) {
      break;
    }
  }
  finally {
    x = x + 100;
  }
}
return x;
")

(test-str #:id "try {throw; return} catch {} finally {return}. should return val in finally"
          2 "
try {
  throw 5;
  return 1;
}
catch (e) {
}
finally {
  return 2;
}
")

(test-str #:id "side effects of try and catch visible in finally"
          111 "
var x = 0;
try {
  x = x + 1;
  throw 10;
  return x;
}
catch (e) {
  x = x + e;
}
finally {
  return x + 100;
}
return x;
")

(test-str #:id "while loop with continue inside try and try is inside while"
          22 "
var x = 0;
while(x < 12) {
  try{
    x = x + 10;
    continue;
  }catch(e) {
  } finally {
     x = x+1;
  }
}
return x;
")

(test-str #:id "while loop with break inside try and try is inside while"
          11 "
var x = 0;
while(x < 12) {
  try{
    x = x + 10;
    break;
  }catch(e) {
   
  } finally {
     x = x+1;
  }
}
return x;
")
