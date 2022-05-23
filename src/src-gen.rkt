#lang racket
(provide AST-path->stack-trace)

;;;; Takes AST nodes from simpleParser/functionParser/classParser
;;;; and produces equivalent source code

(define ((? symbl) expr)
  (eq? (first expr) symbl))

(define default-indent-width 2)

(define (make-tab n)
  (string-append* (make-list n " ")))

(define (ind str
             [width default-indent-width])
  (string-replace str "_" (make-tab width)))

(module+ test
  (require rackunit)
  (define-binary-check (ce equal? a e)))



; number | true | false
(define (literal expr)
  (cond
    [(number? expr)       (number->string expr)]
    [(eq? 'false expr)    "false"]
    [(eq? 'true expr)     "true"]))

(define (literal? expr)
  (or (number? expr)
      (eq? 'true expr)
      (eq? 'false expr)))

(module+ test
  (let ([l  literal])
    (ce (l 1) "1")
    (ce (l 'true) "true")
    (ce (l 'false) "false")))


; [a-Z]+
(define simplename symbol->string)
(define simplename? symbol?)


; '(dot LHS simple)
(define (dot expr)
  (format "~a.~a"
          (value (second expr))
          (name (third expr))))

(define dot? (? 'dot))

(module+ test
  (ce (dot '(dot x y))
      "x.y")
  (ce (dot '(dot (dot X y) z))
      "X.y.z"))


; simple | dot
(define (name expr)
  (if (simplename? expr)
      (simplename expr)
      (dot expr)))

(define (name? expr)
  (or (simplename? expr)
      (dot? expr)))

(module+ test
  (ce (name 'name)
      "name")
  (ce (name '(dot apple banana))
      "apple.banana")
  (ce (name '(dot (dot apple banana) cherry))
      "apple.banana.cherry"))


; '(= {name} {value})
(define (assign expr)
  (format "~a = ~a"
          (name (second expr))
          (value (third expr))))

(define assign? (? '=))

(module+ test
  (let ([ae  assign])
    (ce (ae '(= z a))
        "z = a")
    (ce (ae '(= (dot x z) a))
        "x.z = a")
    (ce (ae '(= (dot x z) (= (dot a b) (dot (dot c d) e))))
        "x.z = a.b = c.d.e")))


; '({value}*)
(define (arglist lis)
  (string-join (map value lis) ", "))

(module+ test
  (ce (arglist null)
      "")
  (ce (arglist '(1 (= x y) (dot x y) (funcall f)))
      "1, x = y, x.y, f()"))


; '(new {simple} {value}*)
(define (new expr)
  (format "new ~a(~a)"
          (simplename (second expr))
          (arglist (cddr expr))))

(define new? (? 'new))

(module+ test
  (ce (new '(new A))
      "new A()")
  (ce (new '(new A 1 a (dot a b) (funcall f)))
      "new A(1, a, a.b, f())"))


; '(funcall {name} {value}+)

(define (funcall-expr expr)
  (format "~a(~a)"
          (name (second expr))
          (arglist (cddr expr))))

(define funcall? (? 'funcall))

(module+ test
  (let ([fce  funcall-expr])
    (ce (fce '(funcall foo))
        "foo()")
    (ce (fce '(funcall (dot foo bar)))
        "foo.bar()")
    (ce (fce '(funcall foo 1))
        "foo(1)")
    (ce (fce '(funcall foo 1 2))
        "foo(1, 2)")))


; '({unary} {value}) | '({binary} {value} {value})
(define (val-wrapped-if-complex expr)
  (if (ormap (λ (f) (f expr))
             (list literal? name? funcall?))
      (value expr)
      (format "(~a)" (value expr))))

(define (op expr)
  (if (= 2 (length expr))
      (format "~a~a"
              (first expr)
              (val-wrapped-if-complex (second expr)))
      (format "~a ~a ~a"
              (val-wrapped-if-complex (second expr))
              (first expr)
              (val-wrapped-if-complex (third expr)))))

(module+ test
  (ce (op '(- 1))
      "-1")
  (ce (op '(! true))
      "!true")
  (ce (op '(! false))
      "!false")
  (ce (op '(+ 1 2))
      "1 + 2")
  (ce (op '(+ (/ a b) (- c d)))
      "(a / b) + (c - d)")
  (ce (op '(+ (+ (+ a 1) b) (+ c d)))
      "((a + 1) + b) + (c + d)")
  (ce (op '(- (* a 1)))
      "-(a * 1)")
  (ce (op '(\|\| a b))
      "a || b")
  (ce (op '(\|\| a (&& b c)))
      "a || (b && c)")
  (ce (op '(! (\|\| a (&& b c))))
      "!(a || (b && c))"))


; literal | name | assign | new | funcall | op
; easier to have op as else case than to check for every possible op
(define value-preds
  (list literal?  name?  assign?  new?  funcall?      (λ (expr) #T)))
(define value-procs
  (list literal   name   assign   new   funcall-expr  op))

(define (value expr)
  (ormap (λ (pred proc) (and (pred expr) (proc expr)))
         value-preds
         value-procs))

(module+ test
  (define v value)
  (ce (v 1) "1")
  (ce (v 'varname) "varname")
  (ce (v '(dot x y)) "x.y")
  (ce (v '(= (dot x y) (= name 2)))
      "x.y = name = 2")
  (ce (v '(funcall f x y z))
      "f(x, y, z)")
  (ce (v '(+ 1 2))
      "1 + 2")
  (ce (v '(+ (dot x y) 2))
      "x.y + 2"))


; ; ; ; statements

; '(break), '(continue)
(define (break stmt [depth 0] [ind-width default-indent-width])
  "break;")
(define break? (? 'break))

(define (continue stmt [depth 0] [ind-width default-indent-width])
  "continue;")
(define continue? (? 'continue))

(module+ test
  (ce (break '(return)) "break;")
  (ce (continue '(continue)) "continue;"))


; '(return {value}), '(throw {value})
(define (return stmt [depth 0] [ind-width default-indent-width])
  (string-append "return " (value (second stmt)) ";"))
(define return? (? 'return))

(define (throw stmt [depth 0] [ind-width default-indent-width])
  (string-append "throw " (value (second stmt)) ";"))
(define throw? (? 'throw))

(module+ test
  (ce (return '(return (+ 3 x))) "return 3 + x;")
  (ce (throw '(throw (+ 3 x))) "throw 3 + x;"))

; see assign
(define (assign-stmt stmt [depth 0] [ind-width default-indent-width])
  (string-append (assign stmt) ";"))

(module+ test
  (let ([as  assign-stmt])
    (ce (as '(= z a))
        "z = a;")
    (ce (as '(= (dot x z) a))
        "x.z = a;")
    (ce (as '(= (dot x z) (= (dot a b) (dot (dot c d) e))))
        "x.z = a.b = c.d.e;")))


; '(var {simple} {value}?)
(define (var-decl stmt [depth 0] [ind-width default-indent-width])
  (format "var ~a~a;"
          (simplename (second stmt))
          (if (= 3 (length stmt))
              (string-append " = " (value (third stmt)))
              "")))

(define var-decl? (? 'var))

(module+ test
  (let ([d  var-decl])
    (ce (d '(var a))
        "var a;")
    (ce (d '(var varname 2))
        "var varname = 2;")
    (ce (d '(var varname (= x 2)))
        "var varname = x = 2;")))


; '(static-var {simple} {value}?)
(define (st-var-decl stmt [depth 0] [ind-width default-indent-width])
  (format "static var ~a~a;"
          (simplename (second stmt))
          (if (= 3 (length stmt))
              (string-append " = "(value (third stmt)))
              "")))

(define st-var-decl? (? 'static-var))

(module+ test
  (let ([d  st-var-decl])
    (ce (d '(var a))
        "static var a;")
    (ce (d '(var varname 2))
        "static var varname = 2;")
    (ce (d '(var varname (= x 2)))
        "static var varname = x = 2;")))


; shared by block, fundecl, static fundecl, class
(define (block-body stmts [depth 0] [ind-width default-indent-width])
  (if (null? stmts)
      "{}"
      (string-append "{\n"
                     (stmt-list stmts (+ depth 1) ind-width)
                     "\n" (make-tab (* ind-width depth)) "}")))


; '(begin {stmt}*)
(define (block stmt [depth 0] [ind-width default-indent-width])
  (block-body (cdr stmt) depth ind-width))

(define block? (? 'begin))

(module+ test
  (let ([b  block])
    (ce (b '(begin))
        "{}")
    (ce (b '(begin (= a 2)))
        (ind "{\n_a = 2;\n}"))
    (ce (b '(begin (= a 2) (var b c)))
        (ind "{
_a = 2;
_var b = c;
}"))
    (ce (b '(begin (begin (begin (= a 2)) (var b c))))
        (ind "{
_{
__{
___a = 2;
__}
__var b = c;
_}
}"))))


; shared by if and while
(define (block-or-simple stmt [depth 0] [ind-width default-indent-width])
  (if (block? stmt)
      (string-append " " (block stmt depth ind-width))
      (string-append "\n"
                     (make-tab (* (+ 1 depth) ind-width))
                     (statement stmt (+ 1 depth) ind-width))))


; '(while {value} {stmt})
(define (while stmt [depth 0] [ind-width default-indent-width])
  (format "while (~a)~a"
          (value (second stmt))
          (block-or-simple (third stmt) depth ind-width)))

(define while? (? 'while))

(module+ test
  (let ([w  while])
    (ce (w '(while (== a b) (= a (+ a 1))))
        (ind "while (a == b)
_a = a + 1;"))
    (ce (w '(while (== a b) (begin (= a (+ a 1)))))
        (ind "while (a == b) {
_a = a + 1;
}"))))


; '(if {value} {stmt} {stmt}?)
(define (_if stmt [depth 0] [ind-width default-indent-width])
  (string-append (format "if (~a)~a"
                         (value (second stmt))
                         (block-or-simple (third stmt) depth ind-width))
                 (if (= 4 (length stmt))
                     (string-append "\n" (make-tab (* depth ind-width)) "else"
                                    (block-or-simple (fourth stmt) depth ind-width))
                     "")))

(define if? (? 'if))

(module+ test
  (ce (_if '(if (== a b) (= a (+ a 2))))
      (ind "if (a == b)
_a = a + 2;"))
  (ce (_if '(if (== a b) (if (== c 2) (= a (+ a 2)))))
      (ind "if (a == b)
_if (c == 2)
__a = a + 2;"))
  (ce (_if '(if (== a 1)
                (begin (if (== a 2)
                           (= y 3)))
                (begin (if (== b 3)
                           (= z 4)
                           (begin (= v 5)))
                       (= z 0))))
      (ind "if (a == 1) {
_if (a == 2)
__y = 3;
}
else {
_if (b == 3)
__z = 4;
_else {
__v = 5;
_}
_z = 0;
}"))
  (ce (_if '(if (== a b)
                (begin (= a (+ a 2)))))
      (ind "if (a == b) {
_a = a + 2;
}"))
  (ce (_if '(if (== a b)
                (= a (+ a 2))
                (= a 0)))
      (ind "if (a == b)
_a = a + 2;
else
_a = 0;"))
  (ce (_if '(if (== a b)
                (= a (+ a 2))
                (begin (= a 0))))
      (ind "if (a == b)
_a = a + 2;
else {
_a = 0;
}"))
  (ce (_if '(if (== a b)
                (begin (= a (+ a 2)))
                (begin (= a 0))))
      (ind "if (a == b) {
_a = a + 2;
}
else {
_a = 0;
}")))


; '(try ({stmt}*) ((?:catch ({simplename}) {stmt}*)?) ((?:finally ({stmt}*))?))
(define (try stmt [depth 0] [ind-width default-indent-width])
  (define tab (make-tab (* depth ind-width)))
  (format "try ~a~a~a"
          (block-body (second stmt) depth ind-width)
          (if (null? (third stmt))
              ""
              (format "\n~acatch(~a) ~a"
                      tab
                      (simplename (first (second (third stmt))))
                      (block-body (third (third stmt)) depth ind-width)))
          (if (null? (fourth stmt))
              ""
              (format "\n~afinally ~a"
                      tab
                      (block-body (second (fourth stmt)) depth ind-width)))))

(define try? (? 'try))

(module+ test
  (ce (try '(try () (catch (e) ()) ()))
      (ind "try {}
catch(e) {}"))
  (ce (try '(try () () (finally ())))
      (ind "try {}
finally {}"))
  (ce (try '(try ((var a)) () (finally ())))
      (ind "try {
_var a;
}
finally {}"))
  (ce (try '(try ((var a)) (catch (a) ()) (finally ())))
      (ind "try {
_var a;
}
catch(a) {}
finally {}")))


; see funcall
(define (funcall-stmt stmt [depth 0] [ind-width default-indent-width])
  (string-append (funcall-expr stmt) ";"))

(module+ test
  (let ([fcs  funcall-stmt])
    (ce (fcs '(funcall foo 1))
        "foo(1);")
    (ce (fcs '(funcall foo 1 2))
        "foo(1, 2);")))


; '(function {simplename} ({fun-param}*) ({stmt}*))
(define (fun-params param-lis)
  (cond
    [(null? param-lis)            ""]
    [(null? (rest param-lis))     (symbol->string (first param-lis))]
    [(eq? '& (first param-lis))   (string-append "&" (fun-params (cdr param-lis)))]
    [else                         (string-append (symbol->string (first param-lis))
                                                 ", "
                                                 (fun-params (cdr param-lis)))]))

(module+ test
  (let ([f  fun-params])
    (ce (f '()) "")
    (ce (f '(a)) "a")
    (ce (f '(& a & b & c)) "&a, &b, &c")
    (ce (f '(a & b & c)) "a, &b, &c")))


(define (fun-decl stmt [depth 0] [ind-width default-indent-width])
  (format "function ~a(~a) ~a"
          (simplename (second stmt))
          (fun-params (third stmt))
          (block-body (fourth stmt) depth ind-width)))

(define fun-decl? (? 'function))

(module+ test
  (let ([fd  fun-decl])
    (ce (fd '(function foo () ()))
        "function foo() {}")
    (ce (fd '(function foo () ((var a 2))))
        (ind "function foo() {
_var a = 2;
}"))
    (ce (fd '(function foo () ((var a 2) (function bar () ((= a 2))))))
        (ind "function foo() {
_var a = 2;
_function bar() {
__a = 2;
_}
}"))))


; '(abstract-function {simplename} ({fun-param}*))
(define (ab-fun-decl stmt [depth 0] [ind-width default-indent-width])
  (format "function ~a(~a);"
          (simplename (second stmt))
          (fun-params (third stmt))))

(define ab-fun-decl? (? 'abstract-function))

(module+ test
  (let ([a  ab-fun-decl])
    (ce (a '(abstract-function fname (a b c)))
        "function fname(a, b, c);")
    (ce (a '(abstract-function fname ()))
        "function fname();")))


; '(static-function {simplename} ({fun-param}*) ({stmt}*))
(define (st-fun-decl stmt [depth 0] [ind-width default-indent-width])
  (format "static function ~a(~a) ~a"
          (simplename (second stmt))
          (fun-params (third stmt))
          (block-body (fourth stmt) depth ind-width)))

(define st-fun-decl? (? 'static-function))

(module+ test
  (let ([sfd  st-fun-decl])
    (ce (sfd '(static-function fname () ()))
        "static function fname() {}")
    (ce (sfd '(static-function funname
                               (a b c & d)
                               ((var a)
                                (begin))))
        (ind "static function funname(a, b, c, &d) {
_var a;
_{}
}"))))


; (constructor ({fun-param}*) ({stmt}*))
;(define (constructor stmt [depth 0] [ind-width default-indent-width])
(define (constructor stmt
                     [depth 0]
                     [ind-width default-indent-width]
                     #:class class)
  (format "~a(~a) ~a"
          class
          (fun-params (second stmt))
          (block-body (third stmt) depth ind-width)))

(define constructor? (? 'constructor))

(module+ test
  (let ([c  constructor])
    (ce (c '(constructor () ()) #:class 'A)
        "A() {}")
    (ce (c '(constructor (a b & c)
                         ((funcall this)
                          (var a)
                          (= a 2)))
           #:class 'LongName)
        (ind "LongName(a, b, &c) {
_this();
_var a;
_a = 2;
}"))))


; stmts within a class body
; constructor | var decl | st var decl | assign | fun decl | abst fun decl | st fun decl
(define (class-body stmts
                    [depth 0]
                    [ind-width default-indent-width]
                    #:class class)
  (define prefix (string-append "\n" (make-tab (* (+ 1 depth) ind-width))))
  (define postfix (string-append "\n" (make-tab (* depth ind-width))))
  (string-join (map (λ (stmt)
                      (if (constructor? stmt)
                          (constructor stmt (+ 1 depth) ind-width #:class class)
                          (statement stmt (+ 1 depth) ind-width)))
                    stmts)
               (string-append prefix)
               #:before-first prefix
               #:after-last postfix))


; '(class {simplename} ((?extends {simplename})?) ({stmt}*))
(define (class-decl stmt [depth 0] [ind-width default-indent-width])
  (format "class ~a ~a{~a}"
          (simplename (second stmt))
          (if (null? (third stmt))
              ""
              (format "extends ~a " (simplename (second (third stmt)))))
          (if (null? (fourth stmt))
              ""
              (class-body (fourth stmt) depth ind-width #:class (second stmt)))))
; todo: replace block-body call with specialized function
; bc need to pass class name along for ctor
  
(define class-decl? (? 'class))

(module+ test
  (let ([c  class-decl])
    (ce (c '(class A () ()))
        "class A {}")
    (ce (c '(class A (extends B) ()))
        "class A extends B {}")
    (ce (c '(class A () ((var a))))
        (ind "class A {
_var a;
}"))
    (ce (c '(class A
              (extends Base)
              ((static-var b 3)
               (var a)
               (= b a)
               (constructor () ((funcall super a b)
                                (if false (begin (throw (new Error))))))
               (function method () ((= a 2))))))
        (ind "class A extends Base {
_static var b = 3;
_var a;
_b = a;
_A() {
__super(a, b);
__if (false) {
___throw new Error();
__}
_}
_function method() {
__a = 2;
_}
}"))))


;; break | continue | return | throw
;; | assign | declare | declare-static
;; | block | while | if | try
;; | funcall | fundecl | abstract fundecl | static fundecl
;; | class
; less strict than actual CFG because the input comes from the parsers,
; which already validate the syntax
(define stmt-preds
  (list break?  continue?  return?  throw?
        assign?  var-decl?  st-var-decl?
        block?  while?  if?  try?
        funcall?  fun-decl?  ab-fun-decl?  st-fun-decl?
        class-decl?))
(define stmt-procs
  (list break  continue  return  throw
        assign-stmt  var-decl  st-var-decl
        block  while  _if  try
        funcall-stmt  fun-decl  ab-fun-decl  st-fun-decl
        class-decl))

(define (statement stmt [depth 0] [ind-width default-indent-width])
  (ormap (λ (pred proc) (and (pred stmt) (proc stmt depth ind-width)))
         stmt-preds
         stmt-procs))

(define (statement? stmt)
  (ormap (λ (pred) (pred stmt)) stmt-preds))

; should only test that the appropriate function is called,
; correctness of each construct is tested in the respective test suite
(module+ test
  (let ([s  statement])
    (ce (s '(= a b)) "a = b;")
    (ce (s '(var a)) "var a;")
    (ce (s '(static-var a)) "static var a;")
    (ce (s '(begin)) "{}")
    (ce (s '(begin (var a) (begin)))
        (ind "{
_var a;
_{}
}"))
    (ce (s '(while (== a b) (= a (+ a 1))))
        (ind "while (a == b)
_a = a + 1;"))
    (ce (s '(if (== a b) (= a (+ a 1))))
        (ind "if (a == b)
_a = a + 1;"))
    (ce (s '(funcall foo a b))
        "foo(a, b);")
    (ce (s '(class A () ()))
        "class A {}")
    ))


; '({stmt}*)
(define (stmt-list lis [depth 0] [ind-width default-indent-width])
  (define tab (make-tab (* depth ind-width)))
  (string-join (map (λ (stmt) (statement stmt depth ind-width)) lis)
               (string-append "\n" tab)
               #:before-first tab))

(module+ test
  (let ([sl  stmt-list])
    (ce (sl '())
        "")
    (ce (sl '((var a)))
        "var a;")
    (ce (sl '((var a)
              (= a 2)))
        "var a;
a = 2;")
    (ce (sl '((var a)
              (begin (= a 2))
              (= a 2)))
        (ind "var a;
{
_a = 2;
}
a = 2;"))
    (ce (sl '((var a)
              (= a 2)
              (begin (var b)
                     (begin (var c)
                            (begin (= c 2))))))
        (ind "var a;
a = 2;
{
_var b;
_{
__var c;
__{
___c = 2;
__}
_}
}"))))


;; symmetry tests - parse(original) == parse(gen(parse(original)))
; generated code should be semantically equivalent to input
; parse identity is a sufficiently strong property
(module+ test
  (require (prefix-in simple- "parse/simpleParser.rkt")
           (prefix-in function- "parse/functionParser.rkt")
           (prefix-in class- "parse/classParser.rkt"))
  
  (define ((_ce parser) src)
    (ce (parser (stmt-list (parser src)))
        (parser src)))
  
  (define sce (_ce simple-parser-str))
  (sce "var z;")
  (sce "
var z = (1 + 2) + (4 - 5);
while (false && !false)
  if (false) {
    try {
      throw x * y;
    } catch (e) {
      try {
      } catch(v) {
        break;
      }
    } finally {
      try {
      } finally {
        continue;
      }
    }
  }
return 5 + 5;
")
  
  (define fce (_ce function-parser-str))
  (fce "
var apple;
var banana = f1(f2(3), apple);
function main() {
  function inner(&a, b) {
    { }
    a = b;
  }
  while (false)
    if (false) {
      try {
        function nested() {}
        throw x * y;
      } catch (e) {
        try {
          nothing();
          nothing2();
        } catch(v) {
          cat();
          cat2();
        }
      } finally {
        try {
          nothing();
        } finally {
          fin();
          fin2();
        }
      }
    }
  return 5 + 5;
}
")
  (define cce (_ce class-parser-str))
  (cce "
class A extends Base {

  static var x;
  var ifield;

  A() {
    this(1, x);
  }

  A(a, b) {
    if (a < b && e) {
      x = x + x * 1 / 3 == 3;
    }
  }

  function abstr();

  function inst(&a, &b) {}

  static function main() {
    inst(1, 2, 3);
    while (false)
      if (false) {
        try {
          function nested() {}
          throw A.x * Base.y().yy + new C().x.foo();
        } catch (e) {
          try {
            nothing();
            nothing2();
          } catch(v) {
            cat();
            break;
          }
        } finally {
          try {
            nothing();
          } finally {
            fin();
            continue;
          }
        }
      }
    return 5 + 5;
  }
}
class Base { static var y; }
")
  )


; ; ; ; AST Path to stack trace
; todo: add support for abbreviated stack trace

(define (AST-path->stack-trace path)
  (string-join (map (λ (e)
                      (cond
                        [(string? e)      e]
                        [(statement? e)   (statement e)]
                        [else             (value e)]))
                    path)
               "\n\nfrom:\n"
               #:after-last "\n"))


; ; ; ; Abbreviated Source Gen

(define (tree-path tree . branches)
  (if (null? branches)
      (list tree)
      (cons tree (apply tree-path
                        ((car branches) tree)
                        (cdr branches)))))

(define (reverse-path pth tree . branches)
  (if (null? branches)
      (cons tree pth)
      (apply reverse-path
             (cons tree pth)
             ((car branches) tree)
             (cdr branches))))

(module+ test
  (let ([tree  '(while (< a (+ b 2))
                       (begin (= a 2)))])
    (ce (tree-path tree)
        (list tree))
    (ce (tree-path tree second third)
        (list tree (second tree) (third (second tree))))
    (ce (reverse-path null tree)
        (list tree))
    (ce (reverse-path null tree second third)
        (list (third (second tree)) (second tree) tree))))

#|
; only stmt list affected, every stmt type needs to propogate the info tho

; given a stack of items, fine to coarse, build up
; pass result to next
; next item decides how to format it
; if null, just do
(define (abrv cs)
  "")

(module+ test
  (let ([prog  '(if (true)
                    (= a 2))])
    (ce (abrv prog)
        (ind "if (true)\n_a = 2;")))
  (let ([prog  '(while (< a (+ b 2))
                       (begin (var x)
                              (var y)
                              (if (== c d)
                                  (begin (var z)
                                         (var a 4))
                                  (begin (var aa)
                                         (var bb)))
                              (var r)
                              (var s)
                              (var t)))])
    (ce (abrv prog)
        (ind "while (a < (b + 2)) {
_..2..
_if (c == d) {
__..1..
__var a = 4;
_} else {..}
_..3..
}"))))
|#