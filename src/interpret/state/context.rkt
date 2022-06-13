#lang racket/base
(require "function.rkt"
         "instance.rkt"
         "../util/map.rkt")

(provide (prefix-out ctxt: (all-defined-out)))

; ; ; ; Managed by `interpreter`, passed to `state` to apply scoping

;; current class. Name / #F. affects what bindings are in scope
(define $current-type 'current-type)
(define current-type (map:getter $current-type))

;; current function body. closure / #F. used for nested fun decls
(define $current-fun-call 'current-fun-call)
(define current-fun-call (map:getter $current-fun-call))

;; current instance. instance / #F
(define $this 'this)
(define this (map:getter $this))

;; flag that affects lookup. if true, limited to bindings available to instance and type
(define $dotted 'dotted)
(define dotted? (map:getter $dotted))

(define $super 'super)
(define super? (map:getter $super))


(define default
  (map:of
   $current-type     #F
   $current-fun-call #F
   $this             #F
   $dotted           #F
   $super            #F))

(define withv map:withv)

(define (in-fun-call function-closure context)
  (withv context
         $current-fun-call  function-closure
         $current-type      (function:class function-closure)
         $this              (if (or
                                 (function:instance? function-closure)
                                 (function:constructor? function-closure)
                                 (function:init? function-closure))
                                (this context)
                                #F)
         $dotted            #F))

(define (in-class class-name context)
  (withv context
         $current-type  class-name))

(define (in-static class-name context)
  (withv context
         $current-type  class-name
         $this          #F
         $super         #F
         $dotted        #T))

(define (of-instance this context)
  (withv context
         $current-type  (instance:class this)
         $this          this
         $super         #F
         $dotted        #T))

(define (w/this. context)
  (withv context
         $super         #F
         $dotted        #T))

(define (w/super. parent-class context)
  (withv context
         $current-type  parent-class
         $super         #T
         $dotted        #T))



(define (free? context)
  (not (current-type context)))

(define (of-instance? context)
  (this context))
