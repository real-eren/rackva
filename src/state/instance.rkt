#lang racket

(require "../util/map.rkt")

;;;; Instance
;; class
;; fields

(define of
  (lambda (#:class class-name
           #:fields fields)
    (map:of
     $class   class-name
     $fields  fields)))

(define $class 'class)
(define $fields 'fields)

