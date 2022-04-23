#lang racket

(require "../util/map.rkt"
         "function-table.rkt"
         "var-table.rkt")
(provide (prefix-out class: (all-defined-out)))

;;;; Class

(define of
  (lambda (#:name name
           #:parent parent-name)
    (map:of
     $name           name
     $super-class    parent-name
     $constructors   new-function-table
     $methods        new-function-table
     $s-fields       new-var-table)))

(define $name 'name)
(define $super-class 'super-class)
(define $init 'init)
(define $constructors 'constructors)
(define $methods 'methods)
(define $s-fields 's-fields)
