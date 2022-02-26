#lang racket/base

(require rackunit 
         "interpreter.rkt")

; white space / newlines do not affect the parser,
; but are included for readability

(define test-file
  (lambda (expected file)
    (check-equal? (interpret file) expected)))

(define test-str
  (lambda (expected str)
    (check-equal? (interpret-str str) expected)))

(define error-file
  (lambda (file)
    (check-exn exn:fail? (lambda () (interpret file)))))

(define error-str
  (lambda (str)
    (check-exn exn:fail? (lambda () (interpret-str str)))))

; ; Number Tests

; 1
(test-str 150 "return 150;")

; 2
(test-str -4 "return 6 * (8 + (5 % 3)) / 11 - 9;")

; 3
(test-str 10 "
var z;
z = 10;
return z;")

; 4
(test-str 16 "
var x = (5 * 7 - 3) / 2;
return x;")

; 5
(test-str 220 "
var x = 10;
var y = 12 + x;
return x * y;")

; 6
(test-str 5 "
var x = 5;
var y = 6;
var m;
if (x <= y)
  m = x;
else
  m = y;
return m;")

; 7
(test-str 6 "
var x = 5;
var y = 6;
var m;
if (x >= y)
  m = x;
else
  m = y;
return m;")

; 8
(test-str 10 "
var x = 5;
var y = 6;
if (x != y)
  x = 10;
return x;")

; 9
(test-str 5 "
var x = 5;
var y = 6;
if (x == y)
  x = 10;
return x;")

; 10
(test-str -39 "return 6 * -(4 * 2) + 9;")

; ; Error Tests
; ; NOTE THAT THIS DOES NOT TEST THAT YOU THROW THE CORRECT ERRORS

; should return error about accessing var before declaring it
(error-str "return x;")

; 11
(error-str "
var x = 1;
y = 10 + x;
return y;")

; 12
(error-str "
var y;
y = x;
return y;")

; should return error about accessing x before declaring it
(error-str "return x;")

; 13
(error-str "
var x;
var y;
x = x + y;
return x;")

; 14
(error-file "
var x = 10;
var y = 20;
var x = x + y;
return x;")

; ; Boolean, If, While Tests

; 15
(test-str 'true "return (10 > 20) || (5 - 6 < 10) && true;")

; 16
(test-str 100 "
var x = 10;
var y = 20;
if (x < y && (x % 2) == 0)
  return 100;
else
  return 200;")

; 17
(test-str 'false "
var x = 100 % 2 == 0;
var y = 10 >= 20;
var z;
if (x || y)
  z = y;
else
  z = x;
return z;")

; 18
(test-str 'true "
var x = 10;
var y = 20;
var z = 20 >= 10;
if (!z || false)
  z = !z;
else
  z = z;
return z;")

; 19
(test-str 128 "
var x = 2;
while (x < 100)
  x = x * 2;
return x;")

; 20
(test-str 12 "
var x = 20;
var y = 128;
while (x * x > 128)
  x = x - 1;
x = x + 1;
return x;")


; ; Advanced Tests

; 21
(test-str 30 "
var x;
var y;
var z = x = y = 10;
return x + y + z;")

; 22
(test-str 11 "
var x;
var y;
x = y = 10;
if ((x = x + 1) > y)
  return x;
else
  return y;")

; 23
(test-str 1106 "
var x;
var y = (x = 5) + (x = 6);
return y * 100 + x;")

; 24
(test-str 12 "
var x = 10;
x = (x = 6) + x;
return x;")

; 25
(test-str 16 "
var x = 10;
x = x + (x = 6);
return x;")

; 26
(test-str 72 "
var x;
var y;
var z;
var w = (x = 6) + (y = z = 20);
return w + x + y + z;")

; 27
(test-str 21 "
var x = 0;
while ((x = x + 1) < 21)
  x = x;
return x;")

; 28
(test-str 164 "
var a = 31160;
var b = 1476;
var r = a % b;
while (r != 0)
  r = (a = b) % (b = r);
return b;")

