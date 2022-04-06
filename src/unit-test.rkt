#lang racket/base

(require rackunit 
         "interpreter.rkt"
         "interpreter-extension.rkt")

; white space / newlines do not affect the parser,
; but are included for readability

(define format-test-id
  (lambda (id)
    (if (eq? id #f)
        #f
        (format "test ID: ~s" id))))


(define test-file
  (lambda (expected file #:id [test-id #f])
    (check-equal? (interpret file) expected (format-test-id test-id))))

(define test-str
  (lambda (expected str #:id [test-id #f])
    (check-equal? (interpret-str str) expected (format-test-id test-id))))

(define error-file
  (lambda (file #:id [test-id #f])
    (check-exn exn:fail? (lambda () (interpret file)) (format-test-id test-id))))

(define error-str
  (lambda (str #:id [test-id #f])
    ;(interpret-str str))) ;for checking the error messages
    (check-exn exn:fail? (lambda () (interpret-str str)) (format-test-id test-id))))



; ; Number Tests

(test-str #:id "return a literal"
          150 "function main() {
   return 150;
}")

(test-str #:id "return a nested expression"
          -4 "function main() {
            return 6 * (8 + (5 % 3)) / 11 - 9;
          }")

(test-str #:id "declare, assign a literal, return"
          10 "
function main() {
  var z;
z = 10;
return z;
}")

(test-str #:id "declare with expression value, return"
          16 "
function main() {
  var x = (5 * 7 - 3) / 2;
  return x;
}")

(test-str #:id "declare w/ literal, declare w/ reference to prior var, return sum"
          220 "
function main() {
  var x = 10;
  var y = 12 + x;
  return x * y;
}")

(test-str #:id "assign in if statement, <= op"
          5 "
function main() {
  var x = 5;
  var y = 6;
  var m;
  if (x <= y)
    m = x;
  else
    m = y;
  return m;
}")

(test-str #:id "assign in if statement, >= op"
          6 "
function main() {
var x = 5;
var y = 6;
var m;
if (x >= y)
  m = x;
else
  m = y;
return m;
}")

(test-str #:id "!= cond"
          10 "
function main() {
var x = 5;
var y = 6;
if (x != y)
  x = 10;
return x;
}")

(test-str #:id "== cond"
          5 "
function main() {
var x = 5;
var y = 6;
if (x == y)
  x = 10;
return x;
}")

; (test-str #:id "negative sign on nested expression"
;           -39 "return 6 * -(4 * 2) + 9;")

; (test-str #:id "return immediately upon first encountered return statement"
;           25 "
; var x = 0;
; x = x + 25;
; return x;
; x = x + 25;
; return x;
; x = x + 25;
; return x;")

; ; ; Error Tests

; (error-str #:id "accessing var before declaring it"
;            "return x;")

; (error-str #:id "assign w/out declaring"
;            "
; var x = 1;
; y = 10 + x;
; return y;")

; (error-str #:id "reference w/out declare or initializing" "
; var y;
; y = x;
; return y;")


; (error-str #:id "reference w/out initializing"
;            "
; var x;
; var y;
; x = x + y;
; return x;")

; (error-str #:id "redeclare in same scope"
;            "
; var x = 10;
; var y = 20;
; var x = x + y;
; return x;")

; (error-str #:id "var in block not accessible from subsequent outer scope"
;            "
; var x = 10;
; var y = 4;
; if (x < y) {
;   var min = x;
; }
; else {
;   var min = y;
; }
; return min;")


; (error-str #:id "var in block can't access undeclared var"
;            "
; if (true) {
;   a = 0;
; }
; return a;")

; (error-str #:id "var in block not accessible in later block (same depth in stack)"
;            "
; if (true) {
;   var a = 0;
; }
; if (true) {
;   a = 2;
; }
; return a;")

; (error-str #:id "var declared in first iter of while loop doesn't persist through later iters)"
;            "
; var x = 0;
; while (x < 5) {
;   if (x != 0) {
;     return a;
;   }
;   var a = 5;
;   x = x + 1;
; }
; return 0;")

; (error-str #:id "break outside of loop"
;            "
; var x = 1;
; break;
; return x;")

; (error-str #:id "continue outside of loop"
;            "
; var x = 1;
; continue;
; return x;")

; ; ; Boolean, If, While Tests

; (test-str #:id "return nested bool expr"
;           'true "return (10 > 20) || (5 - 6 < 10) && true;")

; (test-str #:id "return literal in if-else"
;           100 "
; var x = 10;
; var y = 20;
; if (x < y && (x % 2) == 0)
;   return 100;
; else
;   return 200;")

; (test-str #:id "if with block statements in then and else"
;           100 "
; var x = 10;
; var y = 20;
; if (x < y && (x % 2) == 0) {
;   return 100;
; } else {
;   return 200;
; }")

; (test-str #:id "if w/ empty then block"
;           5 "
; var x = 10;
; if ((x = x / 2) == 5) {}
; return x;
; ")

; (test-str #:id "if w/ empty else block"
;           10 "
; var x = 10;
; if (false) {
;   x = 0;
; } else {
; }
; return x;
; ")

; (test-str #:id "return var assigned in if-else"
;           'false "
; var x = 100 % 2 == 0;
; var y = 10 >= 20;
; var z;
; if (x || y)
;   z = y;
; else
;   z = x;
; return z;")

; (test-str #:id "return var assigned in if-else"
;           'true "
; var x = 10;
; var y = 20;
; var z = 20 >= 10;
; if (!z || false)
;   z = !z;
; else
;   z = z;
; return z;")

; (test-str #:id "update var in while loop, return"
;           128 "
; var x = 2;
; while (x < 100)
;   x = x * 2;
; return x;")

; (test-str #:id "while w/ 1 line block"
;           128 "
; var x = 2;
; while (x < 100) {
;   x = x * 2;
; }
; return x;")

; (test-str #:id "update var in while loop, update after, return"
;           12 "
; var x = 20;
; var y = 128;
; while (x * x > 128)
;   x = x - 1;
; x = x + 1;
; return x;")

; (test-str #:id "while w/ multi-line block"
;           12 "
; var x = 20;
; var y = 128;
; while (x * x > 128) {
;   x = x + 1;
;   x = x - 2;
; }
; x = x + 1;
; return x;")

; (test-str #:id "while w/ empty block"
;           15 "
; var x = 0;
; while ((x = x + 1) < 15) {}
; return x;
; ")


; ; ; Advanced Tests

; (test-str #:id "nested assign"
;           30 "
; var x;
; var y;
; var z = x = y = 10;
; return x + y + z;")

; (test-str #:id "assign in if cond"
;           11 "
; var x;
; var y;
; x = y = 10;
; if ((x = x + 1) > y)
;   return x;
; else
;   return y;")

; (test-str #:id "assign x twice in same expr"
;           1106 "
; var x;
; var y = (x = 5) + (x = 6);
; return y * 100 + x;")

; (test-str #:id "reference x twice in expr, update in left should carry over to right operand"
;           12 "
; var x = 10;
; x = (x = 6) + x;
; return x;")

; (test-str #:id "update in right operand should not affect left operand"
;           16 "
; var x = 10;
; x = x + (x = 6);
; return x;")

; (test-str #:id "nested assign"
;           72 "
; var x;
; var y;
; var z;
; var w = (x = 6) + (y = z = 20);
; return w + x + y + z;")

; (test-str #:id "assign in while cond"
;           21 "
; var x = 0;
; while ((x = x + 1) < 21)
;   x = x;
; return x;")

; (test-str #:id "left-associative side-effects in while cond"
;           10 "
; var x = 0;
; var y = 20;
; while ((x = x + 1) < (y = y - 1))
;   x = x;
; return x;")

; (test-str #:id "modulus, assign expr in left and right operands"
;           164 "
; var a = 31160;
; var b = 1476;
; var r = a % b;
; while (r != 0)
;   r = (a = b) % (b = r);
; return b;")


; (test-str #:id "assign bool to var in condition"
;           5 "
; var i = 0;
; var x;
; while (x = true) {
;   i = i + 1;
;   if (i == 5) break;
; }
; return i;
; ")

; (test-str #:id "assign bool to var in condition"
;           'true "
; var i = 0;
; var x;
; if (x = true) {
;   i = i + 1;
; }
; return x;
; ")

; ; ; Short-circuit side-effects

; (test-str #:id "short-circuit or, should evaluate second arg"
;           12 "
; var x = 1;
; if (false || ((x = 2) == 2)) {
;   x = x + 10;
; }
; return x;
; ")

; (test-str #:id "short-circuit or, should not evaluate second arg"
;           11 "
; var x = 1;
; if (true || ((x = 2) == 2)) {
;   x = x + 10;
; }
; return x;
; ")

; (test-str #:id "short-circuit and, should evaluate second arg (false)"
;           2 "
; var x = 1;
; if (true && ((x = 2) == 0)) {
;   x = x + 10;
; }
; return x;
; ")

; (test-str #:id "short-circuit and, should evaluate second arg (true)"
;           12 "
; var x = 1;
; if (true && ((x = 2) == 2)) {
;   x = x + 10;
; }
; return x;
; ")

; (test-str #:id "short-circuit and, should not evaluate second arg"
;           1 "
; var x = 1;
; if (false && ((x = 2) == 2)) {
;   x = x + 10;
; }
; return x;
; ")

; ;;;; Static scoping


; (test-str #:id "var in block shadows outer scope var"
;           10 "
; var a = 10;
; if (true) {
;   var a = 2;
; }
; return a;")

; (test-str #:id "inner block can access var from outer scope"
;           0 "
; var a = 10;
; if (true) {
;   a = 0;
; }
; return a;")

; (test-str #:id "update var from outer scope"
;           20 "
; var x = 10;
; {
;   var y = 2;
;   var z = x * y;
;   x = z;
; }
; return x;")

; (test-str #:id "update var in multiple nested scopes"
;           164 "
; var a = 31160;
; var b = 1476;
; if (a < b) {
;   var temp = a;
;   a = b;
;   b = temp;
; }
; var r = a % b;
; while (r != 0) {
;   a = b;
;   b = r;
;   r = a % b;
; }
; return b;")


; (test-str #:id "nested blocks are consistent"
;           3 "
; var a = 0;
; if (true) {
;   var a = 1;
;   if (true) {
;     var a = 2;
;     if (true) {
;       var a = 3;
;       if (true) {
;         return a;
;       }
;     }
;   }
; }")


; ; ; Loop Flow Control

; (test-str #:id "while loop with break, single iter"
;           1 "
; var x = 0;
; while(true) {
;   x = x + 1;
;   if (x == 1) {
;     break;
;   }
; }
; return x;
; ")

; (test-str #:id "break in while loop skips remaining statements in body"
;           -1 "
; var x = 0;
; while (x < 10) {
;   x = x - 1;
;   break;
;   x = x + 100;
; }
; return x;")

; (test-str #:id "while loop with break, multiple iters"
;           5 "
; var x = 0;
; while(true) {
;   x = x + 1;
;   if (x == 5) {
;     break;
;   }
; }
; return x;
; ")

; (test-str #:id "nested whiles, break exits only the immediate loop"
;           'true "
; var x = 0;
; var y = 0;
; while (x < 5) {
;   while (true) {
;     y = y + 1;
;     if (!(y < 5))
;       break;
;   }
;   x = x + 1;
; }
; return (x == 5) && (y == 9);
; ")

; (test-str #:id "while loop with continue"
;           2 "
; var y = 2;
; var x = 3;
; while(x > y) {
;   x = x - 1;
;   continue;
;   x = x + 1;
; }
; return x;
; ")

; (test-str #:id "while w/ continue. multiple iters"
;           5 "
; var accumulator = 0;
; var y = 0;
; while (y < 10) {
;   y = y + 1;
;   if (y % 2 == 0)
;     continue;
;   accumulator = accumulator + 1;
; }
; return accumulator;
; ")

; (test-str #:id "nested whiles w/ continue, jumps to immediate loop.  multiple iters"
;           50 "
; var accumulator = 0;
; var x = 0;
; while (x < 10) {
;   var y = 0;
;   while (y < 10) {
;     y = y + 1;
;     if ((y % 2) == 0)
;       continue;
;     accumulator = accumulator + 1;
;   }
;   x = x + 1;
; }
; return accumulator;")

; ; ; Try catch finally

; (test-str #:id "Try catch test #15"
;           125 "
; var x;

; try {
;   x = 20;
;   if (x < 0)
;     throw 10;
;   x = x + 5;
; }
; catch (e) {
;   x = e;
; }
; finally {
;   x = x + 100;
; }
; return x;
; ")

; (test-str #:id "Try catch test #16"
;           110 "
; var x;

; try {
;   x = 20;
;   if (x > 10)
;     throw 10;
;   x = x + 5;
; }
; catch(e) {
;   x = e;
; }
; finally {
;   x = x + 100;
; }
; return x;")

; (test-str #:id "Try catch test #17"
;           2000400 "
; var x = 0;
; var j = 1;

; try {
;   while (j >= 0) {
;     var i = 10;
;     while (i >= 0) {
;       try {
;         if (i == 0)
;           throw 1000000;
;         x = x + 10*i / i;
;       }
;       catch(e) {
;         if (j == 0)
;           throw 1000000;
;         x = x + e / j;
;       }
;       i = i - 1;
;     }
;     j = j - 1;
;   }
; }
; catch (e2) {
;   x = x * 2;
; }
; return x;")

; (test-str #:id "Side effect in finally"
;           101 "
; var x = 10;
; var result = 1;

; try {
;   while (x < 10000) {
;      result = result - 1;
;      x = x + 10;

;      if (x > 1000) {
;        throw x;
;      }
;      else if (x > 100) {
;         break;
;      }
;   }
; }
; finally {
;   result = result + x;
; }
; return result;
; ")

; (test-str #:id "try in loop, try has break and finally block"
;           505 "
; var x = 0;
; var i = 0;
; while (i < 5) {
;   try {
;     x = x + 1;
;     i = i + 1;
;     if (i == 5) {
;       break;
;     }
;   }
;   finally {
;     x = x + 100;
;   }
; }
; return x;
; ")

; (test-str #:id "try {throw; return} catch {} finally {return}. should return val in finally"
;           2 "
; try {
;   throw 5;
;   return 1;
; }
; catch (e) {
; }
; finally {
;   return 2;
; }
; ")

; (test-str #:id "side effects of try and catch visible in finally"
;           111 "
; var x = 0;
; try {
;   x = x + 1;
;   throw 10;
;   return x;
; }
; catch (e) {
;   x = x + e;
; }
; finally {
;   return x + 100;
; }
; return x;
; ")

; (error-str #:id "Top-level throw"
;            "
; throw 1;
; ")

; (error-str #:id "Uncaught throw in (entered) catch block"
;            "
; var x = 10;
; var result = 1;

; try {
;   while (x < 10000) {
;     result = result - 1;
;     x = x * 10;

;     if (x > 1000)
;       throw x;
;   }
; }
; catch (ex) {
;   throw 1;
; }
; return result;")

; (error-str #:id "break exit from block pops frame"
;            "
; var x = 0;
; while (true) {
;   x = x + 1;
;   var a = 5;
;   if (x == 2) {
;     break;
;   }
; }
; return a;
; ")

; (error-str #:id "continue in while body block, pop frame"
;            "
; var x = 1;
; var y = 2;
; if (x < y) {
;   var z = 0;
;   while (z < 100) {
;     var a = 1;
;     z = z + a;
;     continue;
;     z = 1000;
;   }
;   if (z != x) {
;     z = a;
;   }
; }
; return x;
; ")

; (error-str #:id "throw in block, should pop frame. var from try not visible in catch"
;            "
; try {
;   var a = 1;
;   throw 0;
; }
; catch (e) {
;   return a;
; }
; ")

; (error-str #:id "throw in block, should pop frame. var from try not visible in finally"
;            "
; try {
;   var a = 1;
;   throw 0;
; }
; catch (e) {
; }
; finally {
;   return a;
; }
; ")


; (test-str #:id "while loop with continue inside try and try is inside while"
;           22 "
; var x = 0;
; while(x < 12) {
;   try{
;     x = x + 10;
;     continue;
;   }catch(e) {
   
;   } finally {
;      x = x+1;
;   }
; }
; return x;
; ")

; (test-str #:id "while loop with break inside try and try is inside while"
;           11 "
; var x = 0;
; while(x < 12) {
;   try{
;     x = x + 10;
;     break;
;   }catch(e) {
   
;   } finally {
;      x = x+1;
;   }
; }
; return x;
; ")

(test-str #:id "A main with code inside" 
  10 "function main() {
  var x = 10;
  var y = 20;
  var z = 30;
  var min = 0;

  if (x < y)
    min = x;
  else
    min = y;
  if (min > z)
    min = z;
  return min;
}")

(test-str #:id "A function that uses global variables."
          14 "var x = 4;
var y = 6 + x;

function main() {
  return x + y;
}")

(test-str #:id "A function that changes global variables"
          45 "var x = 1;
var y = 10;
var r = 0;

function main() {
  while (x < y) {
     r = r + x;
     x = x + 1;
  }
  return r;
}")

(test-str #:id " A recursive function"
          55 "function fib(a) {
  if (a == 0)
    return 0;
  else if (a == 1)
    return 1;
  else 
    return fib(a-1) + fib(a-2);
}

function main() {
  return fib(10);
}")

(test-str #:id "Functions with multiple parameters that hide global variables."
          1 "function min(x, y, z) {
  if (x < y) {
    if (x < z)
      return x;
    else if (z < x)
      return z;
  }
  else if (y > z)
    return z;
  else
    return y;
}

var x = 10;
var y = 20;
var z = 30;

var min1 = min(x,y,z);
var min2 = min(z,y,x);

function main() {
  var min3 = min(y,z,x);

  if (min1 == min3)
    if (min1 == min2)
      if (min2 == min3)
        return 1;
  return 0;
}")

(test-str #:id "Verifying that your code uses static scoping instead of dynamic scoping"
          115 "var a = 10;
var b = 20;

function bmethod() {
  var b = 30;
  return a + b;
}

function cmethod() {
  var a = 40;
  return bmethod() + a + b;
}

function main () {
  var b = 5;
  return cmethod() + a + b;
}")

(test-str #:id "Boolean parameters and return values."
          #true "function minmax(a, b, min) {
  if (min && a < b || !min && a > b)
    return true;
  else
    return false;
}

function main() {
  return (minmax(10, 100, true) && minmax(5, 3, false));
}")

(test-str #:id "Multiple function calls in an expression"
          20 "function fact(n) {
  var f = 1;
  while (n > 1) {
    f = f * n;
    n = n - 1;
  }
  return f;
}

function binom(a, b) {
  var val = fact(a) / (fact(b) * fact(a-b));
  return val;
}

function main() {
  return binom(6,3);
}
")

(test-str #:id "A function call in the parameter of a function."
          24 "function fact(n) {
  var r = 1;
  while (n > 1) {
    r = r * n;
    n = n - 1;
  }
  return r;
}

function main() {
  return fact(fact(3) - fact(2));
}")

(test-str #:id "A function call that ignores the return value."
          2 "var count = 0;

function f(a,b) {
  count = count + 1;
  a = a + b;
  return a;
}

function main() {
  f(1, 2);
  f(3, 4);
  return count;
}
")

(test-str #:id "A function without a return statement. "
          35 "
var x = 0;
var y = 0;

function setx(a) {
  x = a;
}

function sety(b) {
  y = b;
}

function main() {
  setx(5);
  sety(7);
  return x * y;
}
")

(error-str #:id "Mismatched parameters and arguments"
           "
function f(a) {
  return a*a;
}

function main() {
  return f(10, 11, 12);
}")

(test-str #:id "Functions inside functions. "
          90 "
function main() {
  function h() {
    return 10;
  }

  function g() {
    return 100;
  }

  return g() - h();
}
")

(test-str #:id " Functions inside functions accessing variables outside. "
          69 "
function collatz(n) {
  var counteven = 0;
  var countodd = 0;

  function evenstep(n) {
    counteven = counteven + 1;
    return n / 2;
  }

  function oddstep(n) {
    countodd = countodd + 1;
    return 3 * n + 1;
  }

  while (n != 1) {
    if (n % 2 == 0)
      n = evenstep(n);
    else
      n = oddstep(n);
  }
  return counteven + countodd;
}

function main() {
  return collatz(111);
}")

(test-str #:id "Functions inside functions with variables of the same name"
          87 "
function f(n) {
  var a;
  var b;
  var c;

  a = 2 * n;
  b = n - 10;

  function g(x) {
    var a;
    a = x + 1;
    b = 100;
    return a;
  }

  if (b == 0)
    c = g(a);
  else
    c = a / b;
  return a + b + c;
}

function main() {
  var x = f(10);
  var y = f(20);

  return x - y;
}")

(test-str #:id "Functions inside functions inside functions. "
          64 "
function main() {
  var result;
  var base;

  function getpow(a) {
     var x;

     function setanswer(n) {
        result = n;
     }

     function recurse(m) {
       if (m > 0) {
         x = x * base;
         recurse(m-1);
       }
       else
         setanswer(x);
     }

     x = 1;
     recurse(a);
  }
  base = 2;
  getpow(6);
  return result;
}
")

(error-str #:id "Functions inside functions accessing out of scope variables."
          "
function f(x) {
  function g(x) {
    var b;
    b = x;
    return 0;
  }

  function h(x) {
    b = x;
    return 1;
  }

  return g(x) + h(x);
}

function main() {
  return f(10);
}
")

(test-str #:id "try/catch finally, but no exception thrown. "
          125 "
function divide(x, y) {
  if (y == 0)
    throw y;
  return x / y;
}

function main() {
  var x;

  try {
    x = divide(10, 5) * 10;
    x = x + divide(5, 1);
  }
  catch(e) {
    x = e;
  }
  finally {
    x = x + 100;
  }
  return x;
}
")

(test-str #:id "Throwing an exception inside a function."
          100 "
function divide(x, y) {
  if (y == 0)
    throw y;
  return x / y;
}

function main() {
  var x;

  try {
    x = divide(10, 5) * 10;
    x = x + divide(5, 0);
  }
  catch(e) {
    x = e;
  }
  finally {
    x = x + 100;
  }
  return x;
}
")

(test-str #:id "Throwing an exception from a function"
          2000400 "
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
}
")

(test-str #:id "Call-by-reference test. "
          3421 "
function swap1(x, y) {
  var temp = x;
  x = y;
  y = temp;
}

function swap2(&x, &y) {
  var temp = x;
  x = y;
  y = temp;
}

function main() {
  var a = 1;
  var b = 2;
  swap1(a,b);
  var c = 3;
  var d = 4;
  swap2(c,d);
  return a + 10*b + 100*c + 1000*d;
}
")

(test-str #:id "Assignment side effects with function calls"
          20332 "var x;

function f(a,b) {
  return a * 100 + b;
}

function fib(f) {
  var last = 0;
  var last1 = 1;

  while (f > 0) {
    f = f - 1;
    var temp = last1 + last;
    last = last1;
    last1 = temp;
  }
  return last;
}

function main() {
  var y;
  var z = f(x = fib(3), y = fib(4));
  return z * 100 + y * 10 + x;
}")

(test-str #:id "Mixture of call-by-value and call-by-reference."
          21 "function gcd(a, &b) {
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
  return b;
}
function main () {
  var x = 14;
  var y = 3 * x - 7;
  gcd(x,y);
  return x+y;
}")
