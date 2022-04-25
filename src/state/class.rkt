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
     $parent         (if (null? parent-name)
                         #F
                         parent-name)
     $constructors   new-function-table
     $methods        new-function-table
     $s-fields       new-var-table)))

(define $name 'name)
(define $parent 'parent)
(define $i-field-names 'i-field-names)
(define $init 'init)
(define $constructors 'constructors)
(define $methods 'methods)
(define $s-fields 's-fields)

(define parent (map:getter $parent))
(define methods (map:getter $methods))

; returns a list of the var names 
(define i-field-names (map:getter $i-field-names))