#lang racket/base

(require rackunit 
         "interpreter.rkt")

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
    (check-exn exn:fail? (lambda () (interpret-str str)) (format-test-id test-id))))



; ; Number Tests

(test-str #:id 1
          150 "return 150;")

(test-str #:id 2
          -4 "return 6 * (8 + (5 % 3)) / 11 - 9;")

(test-str #:id 3
          10 "
var z;
z = 10;
return z;")

(test-str #:id 4
          16 "
var x = (5 * 7 - 3) / 2;
return x;")

(test-str #:id 5
          220 "
var x = 10;
var y = 12 + x;
return x * y;")

(test-str #:id 6
          5 "
var x = 5;
var y = 6;
var m;
if (x <= y)
  m = x;
else
  m = y;
return m;")

(test-str #:id 7
          6 "
var x = 5;
var y = 6;
var m;
if (x >= y)
  m = x;
else
  m = y;
return m;")

(test-str #:id 8
          10 "
var x = 5;
var y = 6;
if (x != y)
  x = 10;
return x;")

(test-str #:id 9
          5 "
var x = 5;
var y = 6;
if (x == y)
  x = 10;
return x;")

(test-str #:id 10
          -39 "return 6 * -(4 * 2) + 9;")

; ; Error Tests

(error-str #:id "accessing var before declaring it"
           "return x;")

(error-str #:id 11 "
var x = 1;
y = 10 + x;
return y;")

(error-str #:id 12 "
var y;
y = x;
return y;")

(error-str #:id "accessing x before declaring it"
           "return x;")

(error-str #:id 13 "
var x;
var y;
x = x + y;
return x;")

(error-file #:id 14 "
var x = 10;
var y = 20;
var x = x + y;
return x;")

; ; Boolean, If, While Tests

(test-str #:id 15
          'true "return (10 > 20) || (5 - 6 < 10) && true;")

(test-str #:id 16
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

(test-str #:id 17
          'false "
var x = 100 % 2 == 0;
var y = 10 >= 20;
var z;
if (x || y)
  z = y;
else
  z = x;
return z;")

(test-str #:id 18
          'true "
var x = 10;
var y = 20;
var z = 20 >= 10;
if (!z || false)
  z = !z;
else
  z = z;
return z;")

(test-str #:id 19
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

(test-str #:id 20
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

(test-str #:id 21
          30 "
var x;
var y;
var z = x = y = 10;
return x + y + z;")

(test-str #:id 22
          11 "
var x;
var y;
x = y = 10;
if ((x = x + 1) > y)
  return x;
else
  return y;")

(test-str #:id 23
          1106 "
var x;
var y = (x = 5) + (x = 6);
return y * 100 + x;")

(test-str #:id 24
          12 "
var x = 10;
x = (x = 6) + x;
return x;")

(test-str #:id 25
          16 "
var x = 10;
x = x + (x = 6);
return x;")

(test-str #:id 26
          72 "
var x;
var y;
var z;
var w = (x = 6) + (y = z = 20);
return w + x + y + z;")

(test-str #:id 27
          21 "
var x = 0;
while ((x = x + 1) < 21)
  x = x;
return x;")

(test-str #:id 28
          164 "
var a = 31160;
var b = 1476;
var r = a % b;
while (r != 0)
  r = (a = b) % (b = r);
return b;")

