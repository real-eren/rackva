#lang racket

(require "../util/map.rkt"
         "function-table.rkt"
         "var-table.rkt")
(provide (prefix-out class: (all-defined-out)))

;;;; Class

(define of
  (lambda (#:name name
           #:parent parent-name
           #:ctors constructors
           #:i-fields instance-fields)
    (map:of
     $name           name
     $super-class    parent-name
     $constructors   constructors
     $i-field-inits  instance-fields
     $methods        new-function-table
     $s-fields       new-var-table)))

(define $name 'name)
(define $super-class 'super-class)
(define $constructors 'constructors)
(define $i-field-inits 'i-field-inits)
(define $methods 'methods)
(define $s-fields 's-fields)
