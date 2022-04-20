#lang racket
(require "../util/map.rkt")

;;;; Context entered during execution of a program
;; A context is a map of information about the current context of the interpreter
; i.e. is it currently executing a function call, declaring a class, etc

(define $type 'type)
(define type (map:getter $type))

(define of map:of)

;; types of contexts:
(define type:fun-call 'fun-call)
(define type:class-def 'class-def)
(define type:top-level 'top-level)

; type specific properties
(define type:fun-call:$fun 'fun)

(define type:class-def:$name 'class-name)

;; constructors for the various kinds of contexts
(define of-fun-call
  (lambda (fun-closure)
    (of $type  type:fun-call
        type:fun-call:$fun  fun-closure)))

(define of-class-def
  (lambda (class-name)
    (of $type  type:class-def
        type:class-def:$name  class-name)))

(define of-top-level (of $type  type:top-level))