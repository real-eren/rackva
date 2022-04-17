#lang racket

(require "../util/map.rkt")

;;;; Class
;; super-class
;; constructors

(define of
  (lambda (#:parent parent-name
           #:ctors constructors
           #:i-fields instance-fields
           #:i-methods instance-methods
           #:s-fields static-fields
           #:s-methods static-methods)
    (map:of
     $super-class    parent-name
     $constructors   constructors
     $i-field-inits  instance-fields
     $i-methods      instance-methods
     $s-fields       static-fields
     $s-methods      static-methods)))

(define $super-class 'super-class)
(define $constructors 'constructors)
(define $i-field-inits 'i-field-inits)
(define $i-methods 'i-methods)
(define $s-fields 's-fields)
(define $s-methods 's-methods)
