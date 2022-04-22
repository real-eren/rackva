#lang racket
(require "../util/map.rkt")

(provide (prefix-out context: (except-out (all-defined-out)
                                          of)))

;;;; Context entered during execution of a program
;; A context is a map of information about the current context of the interpreter
; i.e. is it currently executing a function call, declaring a class, etc

(define of map:of)

(define $type 'type)
(define type (map:getter $type))

;;;; types of contexts:
; calling a function
(define type:fun-call 'fun-call)
; defining a class
(define type:class-def 'class-def)
; defining a class member. previous layer in a context-stack should be a `type:class-def`
(define type:class-def-member 'class-def-member)
; global statements, like var decl and fun decl
(define type:top-level 'top-level)

; (undecided) used to handle dots
(define type:scope 'scope)

;;;; type specific properties
; closure of function being called
(define fun-call:$fun 'fun)
(define fun-call:fun (map:getter fun-call:$fun))

; name of class being defined 
(define class-def:$name 'class-name)
(define class-def:name (map:getter class-def:$name))

; Where does this member belong. static | instance | abstract
(define class-def-member:$scope 'member-scope)
(define class-def-member:scope (map:getter class-def-member:$scope))
(define class-def-member:scope:static 'static)
(define class-def-member:scope:instance 'instance)
(define class-def-member:scope:abstract 'abstract)

; what kind of member is this. method | field ; may not need this
(define class-def-member:$kind 'member-kind)
(define class-def-member:kind (map:getter class-def-member:$kind))
(define class-def-member:kind:method 'method)
(define class-def-member:kind:field 'field)

;; constructors for the various kinds of contexts
(define of-fun-call
  (lambda (fun-closure)
    (of $type  type:fun-call
        fun-call:$fun  fun-closure)))

(define of-class-def
  (lambda (class-name)
    (of $type  type:class-def
        class-def:$name  class-name)))

(define of-class-member-def
  (lambda (scope kind)
    (of $type  type:class-def-member
        class-def-member:$scope  scope
        class-def-member:$kind  kind)))


(define static-field-decl (of-class-member-def class-def-member:scope:static
                                               class-def-member:kind:field))

(define instance-field-decl (of-class-member-def class-def-member:scope:instance
                                                 class-def-member:kind:field))

(define static-method-decl (of-class-member-def class-def-member:scope:static
                                                class-def-member:kind:method))

(define instance-method-decl (of-class-member-def class-def-member:scope:instance
                                                  class-def-member:kind:method))

(define abstract-method-decl (of-class-member-def class-def-member:scope:abstract
                                                  class-def-member:kind:method))

(define top-level (of $type  type:top-level))

