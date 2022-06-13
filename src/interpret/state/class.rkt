#lang racket/base

(require "../../util/map.rkt"
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
     $init           #F
     $i-field-names  null
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
; returns a list of the var names 
(define i-field-names (map:getter $i-field-names))

(define methods (map:getter $methods))
(define s-fields (map:getter $s-fields))

(define has-field?
  (lambda (name class)
    (or (member name (i-field-names class))
        (var-table:declared? name (s-fields class)))))


         
