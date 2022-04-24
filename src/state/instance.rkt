#lang racket

(require "../util/map.rkt")
(provide is-instance?
         (prefix-out instance:
                     (combine-out $class
                                  $fields)))

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
(define $fields 'fields)

(define is-instance?
  (lambda (v)
    (and (list? v)
         (map:contains? $class v)
         (map:contains? $fields v))))