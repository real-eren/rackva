#lang racket/base
(require rackunit
         "../../src/parse/parser.rkt")

; ; Illegal usages that should be caught
(define-check (test-parser-exn msg prog-str)
  (test-exn msg exn:fail:user? (λ () (parse-str prog-str))))

(test-case
 "malformed return"
 (test-parser-exn
  "missing value"
  "return;")
 (test-parser-exn
  "missing semicolon"
  "return 5
"))

(test-case
 "keyword as identifier"
 (define KWs '(class return break continue throw try catch finally
                if else while var function static extends))
 (define test-forms '("var ~a;"
                      "var ~a = 2;"
                      "var x.~a = 2;"
                      "var ~a.x = 2;"
                      "var x = ~a;"
                      "var x = ~a.x;"
                      "var x = x.~a.x;"
                      "class ~a { }"
                      "class A extends ~a { }"
                      "class A { var ~a; }"
                      "class A { static var ~a; }"
                      "class A { function ~a() { } }"
                      "class A { static function ~a() { } }"
                      "class A { function b(~a); }"
                      "class A { function b(c, ~a); }"
                      "class A { static function b(~a, c); }"))
 (ormap (λ (str)
          (ormap (λ (kw)
                   (test-parser-exn str (format str kw)))
                 KWs))
        test-forms))

(test-case
 "illegal function def"
 (test-parser-exn
  "abstract function outside class"
  "function funName(a, b);")
 (test-parser-exn
  "static method stub"
  "class A { static foo(); }")
 (test-parser-exn
  "static method outside class"
  "static foo() { }")
 (test-parser-exn
  "leading comma in formal params"
  "static foo(,a,b) { }"))

(test-case
 "control flow in class body"
 (test-parser-exn
  "`if` in class body"
  "class A { if (true) { } }")
 (test-parser-exn
  "`while` in class body"
  "class A { while (true) { } }")
 (test-parser-exn
  "`try` in class body"
  "class A { try { } catch {} }")
 (test-parser-exn
  "return in class body"
  "class A { return 0; }"))

(test-case
 "class in nested statement list"
 (test-parser-exn
  "class in `if`"
  "if (true) { class A {} }")
 (test-parser-exn
  "class in `while`"
  "while (true) { class A {} }")
 (test-parser-exn
  "class in `block`"
  "{ class A {} }")
 (test-parser-exn
  "class in class"
  "class AA { class A {} }")
 (test-parser-exn
  "class in function"
  "function foo() { class A {} }"))

(test-case
 "class members outside class"
 (test-parser-exn
  "static var in global"
  "static var a;")
 (test-parser-exn
  "static fun in global"
  "static function a(){ }")
 (test-parser-exn
  "abstract function in global"
  "function a();")
 (test-parser-exn
  "abstract function in block"
  "{ function a(); }"))

(test-case
 "block comment inside identifiers"
 (test-parser-exn
  "block comment in var keyword"
  "va/* comment */r x = 1; return 1;")
 (test-parser-exn
  "block comment in var name"
  "var lon/* comment */gName = 2; return longName;"))

(test-case
 "unclosed block comments"
 (test-parser-exn
  "unclosed block comment in beginning of prog"
  "/* this is a block comment
var x;
return x;")
 (test-parser-exn
  "unclosed block comment in end of prog"
  "var x = 1;
return x;
/* this is a block comment")
 (test-parser-exn
  "unclosed block comment in middle of prog"
  "var x;
/* this is a block comment
return x;")
 (test-parser-exn
  "start and end delimiter can't share *"
  "var a; /*/")
 (test-parser-exn
  "block comments don't nest"
  "var a; /* /* */ */"))

