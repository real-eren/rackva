#lang racket

(require "../util/map.rkt")

(provide new-function-table
         (prefix-out function-table:
                     (combine-out push-new-layer
                                  pop-layer
                                  has-fun?
                                  get-all-funs
                                  get-function
                                  declare-fun))
         (prefix-out function:
                     (combine-out $params
                                  $body
                                  $scoper
                                  )))


;;;; function table
;; a function table is a stack of layers
;; a layer is a map of function bindings
;; bindings { name : (params body scoper) }

;;;; function property keys
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
  (lambda (params body scoper)
    (map:of
     $params  params
     $body    body
     $scoper  scoper)))

;; layer is a map of { name : closure }

(define layer:has-fun? map:contains?)

(define layer:get-fun map:get)
(define layer:get-all map:get-all)

(define layer:put-fun map:put)

(define new-layer map:empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stack operations
(define new-function-table (list new-layer))

(define push-layer cons)

(define push-new-layer
  (lambda (table)
    (push-layer new-layer table)))

(define peek-layer car)
(define pop-layer cdr)
(define no-layers? empty?)

;; whether this table has a function with the given signature
(define has-fun?
  (lambda (name arg-list table)
    (ormap (curry layer:has-fun? name) table)))

;; list of all functions in this table with a matching name
(define get-all-funs
  (lambda (name table)
    (foldl append '() (map (curry layer:get-all name) table))))
;; assumes function with signature exists
; null if absent
; call has-fun? beforehand
(define get-function
  (lambda (name arg-list table)
    (layer:get-fun name (findf (curry layer:has-fun? name) table))))

;; adds function to table
(define declare-fun
  (lambda (name params body scoper table)
    (list-update table 0 (curry layer:put-fun name (function:of params body scoper)))))
