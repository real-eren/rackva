#lang racket/base
(require "../src-gen.rkt"
         "../util/map.rkt")

(provide (prefix-out ue: (except-out (all-defined-out)
                                     exn:of
                                     exn:ctor
                                     exn->string
                                     formatter)))
;;;; User Errors

; e is an error instance
; produced by calling functions here
; with parameters from call-site

; exn instance
; map {
;   type : 'type
;   field1 : x
;   field2 : y
; }
; this way, easy to check type but can also
; check the exn's fields in unit-tests

; cs - context stack
; built by intepreter as it enters Mstates and Mvalues
; head is most recent context

(define raise-exn
  (lambda (exn cs)
    (raise-user-error (format "Error: ~a~nSource:~n----------~n~a----------~n"
                              (exn->string exn)
                              (AST-path->stack-trace cs)))))

(define exn->string
  (lambda (exn)
    ((map:get 'fmt-proc exn) exn)))

(define exn:of
  (lambda (type fmt-proc keys values)
    (foldl map:put
           (map:of 'type      type
                   'fmt-proc  fmt-proc)
           keys
           values)))

(define (exn:type exn)
  (map:get 'type exn))

(define exn:property map:get)
  
(define formatter
  (lambda (fmt-str keys)
    (lambda (exn)
      (apply map:format-w/-keys fmt-str exn keys))))

(define exn:ctor
  (lambda (type fmt-str . keys)
    (lambda vals
      (if (= (length keys) (length vals))
          (exn:of type (formatter fmt-str keys) keys vals)
          (apply raise-arity-error 'exn-ctor (length keys) vals)))))

(module+ test
  (require rackunit)
  (test-case
   "fn returned by exn:ctor should validate # inputs"
   (check-exn exn:fail? (λ () ((exn:ctor type:break-outside-loop
                                         "str") 'v1)))
   (check-exn exn:fail? (λ () ((exn:ctor type:break-outside-loop
                                         "str"
                                         'k1) 'v1 'v2)))
   (check-exn exn:fail? (λ () ((exn:ctor type:break-outside-loop
                                         "str"
                                         'k1 'k2))))
   (check-exn exn:fail? (λ () ((exn:ctor type:break-outside-loop
                                         "str"
                                         'k1 'k2) 'v1)))))

;; exception types

(define type:break-outside-loop 'break-outside-loop)
(define break-outside-loop
  (exn:ctor type:break-outside-loop
            "break statement outside of loop"))

(define type:continue-outside-loop 'continue-outside-loop)
(define continue-outside-loop
  (exn:ctor type:continue-outside-loop
            "continue statement outside of loop"))

; currently just for v1. v2 and v3's main() are covered by `no-return-value-fun`
(define type:did-not-return 'did-not-return)
(define did-not-return
  (exn:ctor type:did-not-return
            "Program did not reach a return statement"))

(define type:unexpected-return 'unexpected-return)
(define unexpected-return
  (exn:ctor type:unexpected-return
            "Encountered top-level return, but not in `script` mode"))

(define type:uncaught-exception 'uncaught-exception)
(define uncaught-exception
  (exn:ctor type:uncaught-exception
            "uncaught exception: ~a"
            'exn-val))

(define type:type-mismatch 'type-mismatch)
(define type-mismatch
  (exn:ctor type:type-mismatch
            "expected type(s) `~a`, given value(s): `~a`"
            'expected-type
            'val))

(define type:expected-boolean-expr 'expected-boolean-expr)
(define expected-boolean-expr
  (exn:ctor type:expected-boolean-expr
            "expected a boolean expression, got: `~a`"
            'expr))


(define type:non-var-in-ref-param 'non-variable-in-reference-param)
(define non-var-in-ref-param
  (exn:ctor type:non-var-in-ref-param
            "`~a` expects a reference for `~a`"
            'fun-sig 'param-name))

(define type:this-as-ref-param 'this-as-ref-param)
(define this-as-ref-param
  (exn:ctor type:this-as-ref-param
            "`this` cannot be passed as a reference parameter"))

(define type:function-not-in-scope 'function-not-in-scope)
(define function-not-in-scope
  (exn:ctor type:function-not-in-scope
            "A function `~a` with ~a parameter(s) is not in scope.~a"
            'fun-name 'num-args 'suggestions))

(define type:no-return-value-fun 'no-return-value-fun)
(define no-return-value-fun
  (exn:ctor type:no-return-value-fun
            "Function call `~a` was expected to produce a return value, but did not"
            'funcall))

(define type:invoke-abstract-method 'invoke-abstract-method)
(define invoke-abstract-method
  (exn:ctor type:invoke-abstract-method
            "`~a` is abstract and cannot be invoked"
            'fun-sig))

(define type:override-c-w/-abstr 'override-concrete-method-with-abstract)
(define override-c-w/-abstr
  (exn:ctor type:override-c-w/-abstr
            "Cannot override concrete `~a` with abstract method"
            'fun-sig))

(define type:unoverridden-abstract 'unoverridden-abstract-method)
(define unoverridden-abstract
  (exn:ctor type:unoverridden-abstract
            "class `~a` does not override abstract method `~a`"
            'class-name 'fun-sig))



(define type:not-a-class 'not-a-class)
(define not-a-class
  (exn:ctor type:not-a-class
            "`~a` is not a declared class"
            'class-name))

(define type:class-extend-self 'class-extend-self)
(define class-extend-self
  (exn:ctor type:class-extend-self
            "class `~a` cannot extend itself"
            'class-name))



(define type:duplicate-class 'duplicate-class)
(define duplicate-class
  (exn:ctor type:duplicate-class
            "Attempted to re-declare class `~a`"
            'class-name))

(define type:duplicate-method 'duplicate-method)
(define duplicate-method
  (exn:ctor type:duplicate-method
            "A method with the signature `~a` is already declared in class `~a`."
            'fun-sig 'class-name))

(define type:duplicate-field 'duplicate-field)
(define duplicate-field
  (exn:ctor type:duplicate-field
            "A field named `~a` is already declared in this class"
            'field-name))

(define type:duplicate-constructor 'duplicate-constructor)
(define duplicate-constructor
  (exn:ctor type:duplicate-constructor
            "A constructor with signature `~a` has already been declared"
            'fun-sig))

(define type:duplicate-function 'duplicate-function)
(define duplicate-function
  (exn:ctor type:duplicate-function
            "Function `~a` is already declared in the current scope"
            'fun-sig))

(define type:duplicate-variable 'duplicate-variable)
(define duplicate-variable
  (exn:ctor type:duplicate-variable
            "A variable named `~a` is already declared in the current scope"
            'var-name))

(define type:duplicate-parameter 'duplicate-parameter)
(define duplicate-parameter
  (exn:ctor type:duplicate-parameter
            "A parameter named `~a` is already declared in this function's signature"
            'param-name))



(define type:reference-undeclared-var 'reference-undeclared-var)
(define reference-undeclared-var
  (exn:ctor type:reference-undeclared-var
            "A variable or field `~a` is not in scope"
            'var-name))

(define type:access-uninitialized-var 'access-uninitialized-var)
(define access-uninitialized-var
  (exn:ctor type:access-uninitialized-var
            "accessed variable `~a` before initializing it"
            'var-name))



(define type:assigning-to-this/super 'assigning-to-this/super)
(define assigning-to-this/super
  (exn:ctor type:assigning-to-this/super
            "`~a` cannot be assigned to"
            'name))

(define type:this/super-in-static 'this/super-in-static)
(define this/super-in-static
  (exn:ctor type:this/super-in-static
            "`~a` cannot be referenced in a free or static context"
            'name))

(define type:this/super-dot-RHS 'this/super-dot-RHS)
(define this/super-dot-RHS
  (exn:ctor type:this/super-dot-RHS
            "Keyword `~a` cannot appear on the RHS of a dot expression"
            'name))

(define type:non-instance-dot 'non-instance-dot)
(define non-instance-dot
  (exn:ctor type:non-instance-dot
            "`~a` is not an instance of a class, cannot apply dot operator"
            'name))

(define type:unknown-LHS-dot 'unknown-LHS-dot)
(define unknown-LHS-dot
  (exn:ctor type:unknown-LHS-dot
            "`~a` is not a reachable class name or variable" 
            'LHS))

(define type:malformed-identifier 'malformed-identifier)
(define malformed-identifier
  (exn:ctor type:malformed-identifier
            "malformed identifier `~a`"
            'name))

(define type:keyword-as-identifier 'keyword-as-identifier)
(define keyword-as-identifier
  (exn:ctor type:keyword-as-identifier
            "Keyword `~a` cannot be used as a ~a name"
            'keyword 'usage))



(define type:cyclic-ctor-chaining 'cyclic-ctor-chaining)
(define cyclic-ctor-chaining
  (exn:ctor type:cyclic-ctor-chaining
            "Cyclic constructor chaining. At least one constructor must call super(...)"))

(define type:ctor-chain-outside-ctor 'constructor-chaining-outside-constructor)
(define ctor-chain-outside-ctor
  (exn:ctor type:ctor-chain-outside-ctor
            "calls to `~a(...)` can only appear in the first line of a constructor"
            'keyword))

(define type:super-w/out-parent 'super-ctor-w/out-parent)
(define super-w/out-parent
  (exn:ctor type:super-w/out-parent
            "Class `~a` has no parent class, cannot call super constructor."
            'class-name))

(define type:ctor-DNE 'constructor-does-not-exist)
(define ctor-DNE
  (exn:ctor type:ctor-DNE
            "class `~a` does not declare a constructor that accepts arguments `~a`"
            'class-name 'args))

