#lang racket/base

(require "../util/map.rkt")
(provide is-instance?
         (prefix-out instance:
                     (except-out (all-defined-out)
                                 is-instance?)))

;;;; Instance
;; class- name of class
;; fields - stack of var-table

(define of
  (lambda (#:class class-name
           #:fields fields)
    (map:of
     $class   class-name
     $fields  fields)))


(define $class 'class)
(define class (map:getter $class))

(define $fields 'fields)
(define fields (map:getter $fields))

(define is-instance?
  (lambda (v)
    (and (list? v)
         (map:contains? $class v)
         (map:contains? $fields v))))
