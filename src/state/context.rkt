#lang racket
(require "../util/map.rkt")

(provide (prefix-out context: (all-defined-out)))

;;;; Context entered during execution of a program
;; A context is a map of information about the current context of the interpreter
; i.e. is it currently executing a function call, declaring a class, etc

(define $type 'type)
(define type (map:getter $type))

(define of map:of)

;; types of contexts:
; calling a function
(define type:fun-call 'fun-call)
; defining a class
(define type:class-def 'class-def)
; global statements, like var decl and fun decl
(define type:top-level 'top-level)

; used to handle dots
(define type:scope 'scope)

; type specific properties
(define fun-call:$fun 'fun)

(define class-def:$name 'class-name)

;; constructors for the various kinds of contexts
(define of-fun-call
  (lambda (fun-closure)
    (of $type  type:fun-call
        fun-call:$fun  fun-closure)))

(define of-class-def
  (lambda (class-name)
    (of $type  type:class-def
        class-def:$name  class-name)))

(define top-level (of $type  type:top-level))

