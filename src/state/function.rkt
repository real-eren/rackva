#lang racket

(require "../util/map.rkt")

(provide (prefix-out function:
                     (combine-out $params
                                  $body
                                  $scoper
                                  $class
                                  $type
                                  type:static
                                  type:instance
                                  type:free
                                  type:abstract
                                  num-formal-params
                                  )))

;;;; function property keys
(define $name   'fun-name)
;; list of formal parameters
(define $params 'formal-params)
;; statement list that forms the body of the function
(define $body   'body)
;; state mapping function that filters the layers that should be visible during execution
(define $scoper 'scoper)
; what class this function is defined in, if applicable.
; Null if type == free
(define $class  'class)
;; what kind of function is this
; constants for Static | Instance | Free | Abstract
(define $type   'type)
(define type:static   'static)
(define type:instance 'instance)
(define type:free     'free)
(define type:abstract 'abstract)


(define of
  (lambda (name params body scoper type)
    (map:of
     $name    name
     $params  params
     $body    body
     $scoper  scoper
     $type    type)))

(define of-abstract
  (lambda (name params)
    (of $name  name
        $params  params
        $type  type:abstract)))

;; returns the number of formal parameters in a function
(define num-formal-params
  (lambda (closure)
    (length (filter (lambda (p) (not (eq? '& p))) (map:get $params closure)))))

