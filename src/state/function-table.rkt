#lang racket

(require "../util/map.rkt"
         "function.rkt")

(provide new-function-table
         (prefix-out function-table:
                     (combine-out has-fun?
                                  get-all
                                  get
                                  declare-fun)))


;;;; function table
;; a function table a map of function bindings
;; bindings { name : function }

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


(define function:of
  (lambda (name params body scoper type)
    (map:of
     $name    name
     $params  params
     $body    body
     $scoper  scoper
     $type    type)))

;; returns the number of formal parameters in a function
(define num-formal-params
  (lambda (closure)
    (length (filter (lambda (p) (not (eq? '& p))) (map:get $params closure)))))


(define new-function-table map:empty)

(define get-all map:get-all)
; gets the function with a matching signature
(define get
  (lambda (name arg-list table)
    (findf (lambda (f)
             (eq? (length arg-list)
                  (num-formal-params f)))
           (get-all name table))))

; get returns false if absent
(define has-fun?
  (lambda (name arg-list table)
    (map:contains? name table)))

(define declare-fun
  (lambda (name params body scoper type table)
    (map:insert name
                (function:of name
                             params
                             body
                             scoper
                             type)
                table)))
