#lang racket
(require "../util/map.rkt")
(require "function.rkt")

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
; global statements, like var decl and fun decl
(define type:top-level 'top-level)


;;;; type specific properties
; closure of function being called
(define fun-call:$fun 'fun)
(define fun-call:fun (map:getter fun-call:$fun))

; name of class being defined 
(define class-def:$name 'class-name)
(define class-def:name (map:getter class-def:$name))

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


(define context->string
  (lambda (context)
    (cond
      ; class def
      [(eq? type:class-def (type context))      (format "Class ~a" (class-def:name context))]
      ; top-level
      [(eq? type:top-level (type context))      "top-level"]
      ; fun call
      [(function:constructor? (fun-call:fun context))  (format "new ~a" (function:formatted-signature (function:name (fun-call:fun context))
                                                                                                      (function:params (fun-call:fun context))))]
      [else                                            (function->string (fun-call:fun context))])))
      
