#lang racket

(require "../util/map.rkt")
(provide (prefix-out class: (all-defined-out)))

;;;; Class

(define of
  (lambda (#:name name
           #:parent parent-name
           #:ctors constructors
           #:i-fields instance-fields
           #:i-methods instance-methods
           #:s-fields static-fields
           #:s-methods static-methods
           #:a-methods abstract-methods)
    (map:of
     $name           name
     $super-class    parent-name
     $constructors   constructors
     $i-field-inits  instance-fields
     $i-methods      instance-methods
     $s-fields       static-fields
     $s-methods      static-methods
     $a-methods      abstract-methods)))

(define $name 'name)
(define $super-class 'super-class)
(define $constructors 'constructors)
(define $i-field-inits 'i-field-inits)
(define $i-methods 'i-methods)
(define $s-fields 's-fields)
(define $s-methods 's-methods)
(define $a-methods 'a-methods)
