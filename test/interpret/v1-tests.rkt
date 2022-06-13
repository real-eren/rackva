#lang racket/base

(require "test-shared.rkt"
         "../src/interpreter.rkt"
         rackunit)

; white space / newlines do not affect the parser,
; but are included for readability

(define (i prog-str)
  (i-str prog-str mode:script))

; ; Literal Tests
(test-equal? "return a positive literal"
             (i "return 150;")
             150)

(test-equal? "return a negative literal"
             (i "return -150;")
             -150)

(test-equal? "return a true literal"
             (i "return true;")
             'true)

(test-equal? "return a false literal"
             (i "return false;")
             'false)

; ; Number Tests

(test-equal? "return a nested expression"
             (i "return 6 * (8 + (5 % 3)) / 11 - 9;")
             -4)

(test-equal? "return a nested boolean expression"
             (i "return (true && false) || (!(1 == 0) && !(2 + 2 == 5));")
             'true)

(test-equal? "declare, assign a literal, return"
             (i "
var z;
z = 10;
return z;")
             10)

(test-equal? "declare with expression value, return"
             (i "
var x = (5 * 7 - 3) / 2;
return x;")
             16)

(test-equal? "declare w/ literal, declare w/ reference to prior var, return sum"
             (i "
var x = 10;
var y = 12 + x;
return x * y;")
             220)

(test-equal? "assign in if statement, <= op"
             (i "
var x = 5;
var y = 6;
var m;
if (x <= y)
  m = x;
else
  m = y;
return m;")
             5)

(test-equal? "assign in if statement, >= op"
             (i "
var x = 5;
var y = 6;
var m;
if (x >= y)
  m = x;
else
  m = y;
return m;")
             6)

(test-equal? "!= cond"
             (i "
var x = 5;
var y = 6;
if (x != y)
  x = 10;
return x;")
             10)

(test-equal? "== cond"
             (i "
var x = 5;
var y = 6;
if (x == y)
  x = 10;
return x;")
             5)

(test-equal? "negative sign on nested expression"
             (i "return 6 * -(4 * 2) + 9;")
             -39)

(test-equal? "return immediately upon first encountered return statement"
             (i "
var x = 0;
x = x + 25;
return x;
x = x + 25;
return x;
x = x + 25;
return x;")
             25)


; ; Boolean, If, While Tests

(test-equal? "return nested bool expr"
             (i "return (10 > 20) || (5 - 6 < 10) && true;")
             'true)

(test-equal? "return literal in if-else"
             (i "
var x = 10;
var y = 20;
if (x < y && (x % 2) == 0)
  return 100;
else
  return 200;")
             100)

(test-equal? "if with block statements in then and else"
             (i "
var x = 10;
var y = 20;
if (x < y && (x % 2) == 0) {
  return 100;
} else {
  return 200;
}")
             100)

(test-equal? "if w/ empty then block"
             (i "
var x = 10;
if ((x = x / 2) == 5) {}
return x;
")
             5)

(test-equal? "if w/ empty else block"
             (i "
var x = 10;
if (false) {
  x = 0;
} else {
}
return x;
")
             10)

(test-equal? "return var assigned in if-else"
             (i "
var x = 100 % 2 == 0;
var y = 10 >= 20;
var z;
if (x || y)
  z = y;
else
  z = x;
return z;")
             'false)

(test-equal? "return var assigned in if-else"
             (i "
var x = 10;
var y = 20;
var z = 20 >= 10;
if (!z || false)
  z = !z;
else
  z = z;
return z;")
             'true)

(test-equal? "update var in while loop, return"
             (i "
var x = 2;
while (x < 100)
  x = x * 2;
return x;")
             128)

(test-equal? "while w/ 1 line block"
             (i "
var x = 2;
while (x < 100) {
  x = x * 2;
}
return x;")
             128)

(test-equal? "update var in while loop, update after, return"
             (i "
var x = 20;
var y = 128;
while (x * x > 128)
  x = x - 1;
x = x + 1;
return x;")
             12)

(test-equal? "while w/ multi-line block"
             (i "
var x = 20;
var y = 128;
while (x * x > 128) {
  x = x + 1;
  x = x - 2;
}
x = x + 1;
return x;")
             12)

(test-equal? "while w/ empty block"
             (i "
var x = 0;
while ((x = x + 1) < 15) {}
return x;
")
             15)


; ; Advanced Tests

(test-equal? "nested assign"
             (i "
var x;
var y;
var z = x = y = 10;
return x + y + z;")
             30)

(test-equal? "assign in if cond"
             (i "
var x;
var y;
x = y = 10;
if ((x = x + 1) > y)
  return x;
else
  return y;")
             11)

(test-equal? "assign x twice in same expr"
             (i "
var x;
var y = (x = 5) + (x = 6);
return y * 100 + x;")
             1106)

(test-equal? "reference x twice in expr, update in left should carry over to right operand"
             (i "
var x = 10;
x = (x = 6) + x;
return x;")
             12)

(test-equal? "update in right operand should not affect left operand"
             (i "
var x = 10;
x = x + (x = 6);
return x;")
             16)

(test-equal? "nested assign"
             (i "
var x;
var y;
var z;
var w = (x = 6) + (y = z = 20);
return w + x + y + z;")
             72)

(test-equal? "assign in while cond"
             (i "
var x = 0;
while ((x = x + 1) < 21)
  x = x;
return x;")
             21)

(test-equal? "left-associative side-effects in while cond"
             (i "
var x = 0;
var y = 20;
while ((x = x + 1) < (y = y - 1))
  x = x;
return x;")
             10)

(test-equal? "modulus, assign expr in left and right operands"
             (i "
var a = 31160;
var b = 1476;
var r = a % b;
while (r != 0)
  r = (a = b) % (b = r);
return b;")
             164)


(test-equal? "assign bool to var in condition"
             (i "
var i = 0;
var x;
while (x = true) {
  i = i + 1;
  if (i == 5) break;
}
return i;
")
             5)

(test-equal? "assign bool to var in condition"
             (i "
var i = 0;
var x;
if (x = true) {
  i = i + 1;
}
return x;
")
             'true)

; ; Short-circuit side-effects

(test-equal? "short-circuit or, should evaluate second arg"
             (i "
var x = 1;
if (false || ((x = 2) == 2)) {
  x = x + 10;
}
return x;
")
             12)

(test-equal? "short-circuit or, should not evaluate second arg"
             (i "
var x = 1;
if (true || ((x = 2) == 2)) {
  x = x + 10;
}
return x;
")
             11)

(test-equal? "short-circuit and, should evaluate second arg (false)"
             (i "
var x = 1;
if (true && ((x = 2) == 0)) {
  x = x + 10;
}
return x;
")
             2)

(test-equal? "short-circuit and, should evaluate second arg (true)"
             (i "
var x = 1;
if (true && ((x = 2) == 2)) {
  x = x + 10;
}
return x;
")
             12)

(test-equal? "short-circuit and, should not evaluate second arg"
             (i "
var x = 1;
if (false && ((x = 2) == 2)) {
  x = x + 10;
}
return x;
")
             1)

;;;; Static scoping


(test-equal? "var in block shadows outer scope var"
             (i "
var a = 10;
if (true) {
  var a = 2;
}
return a;")
             10)

(test-equal? "inner block can access var from outer scope"
             (i "
var a = 10;
if (true) {
  a = 0;
}
return a;")
             0)

(test-equal? "update var from outer scope"
             (i "
var x = 10;
{
  var y = 2;
  var z = x * y;
  x = z;
}
return x;")
             20)

(test-equal? "update var in multiple nested scopes"
             (i "
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
             164)


(test-equal? "nested blocks are consistent"
             (i "
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
             3)


; ; Loop Flow Control

(test-equal? "while loop with break, single iter"
             (i "
var x = 0;
while(true) {
  x = x + 1;
  if (x == 1) {
    break;
  }
}
return x;
")
             1)

(test-equal? "break in while loop skips remaining statements in body"
             (i "
var x = 0;
while (x < 10) {
  x = x - 1;
  break;
  x = x + 100;
}
return x;")
             -1)

(test-equal? "while loop with break, multiple iters"
             (i "
var x = 0;
while(true) {
  x = x + 1;
  if (x == 5) {
    break;
  }
}
return x;
")
             5)

(test-equal? "nested whiles, break exits only the immediate loop"
             (i "
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
             'true)

(test-equal? "while loop with continue"
             (i "
var y = 2;
var x = 3;
while(x > y) {
  x = x - 1;
  continue;
  x = x + 1;
}
return x;
")
             2)

(test-equal? "while w/ continue. multiple iters"
             (i "
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
             5)

(test-equal? "nested whiles w/ continue, jumps to immediate loop.  multiple iters"
             (i "
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
             50)

; ; Try catch finally

(test-equal? "Try catch test #15"
             (i "
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
             125)

(test-equal? "Try catch test #16"
             (i "
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
             110)

(test-equal? "Try catch test #17"
             (i "
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
             2000400)

(test-equal? "Side effect in finally"
             (i "
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
             101)

(test-equal? "try in loop, try has break and finally block"
             (i "
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
             505)

(test-equal? "try {throw; return} catch {} finally {return}. should return val in finally"
             (i "
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
             2)

(test-equal? "side effects of try and catch visible in finally"
             (i "
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
             111)

(test-equal? "while loop with continue inside try and try is inside while"
             (i "
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
             22)

(test-equal? "while loop with break inside try and try is inside while"
             (i "
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
             11)
