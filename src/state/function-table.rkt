#lang racket

(require "../util/map.rkt")

(provide new-function-table
         (prefix-out function-table:
                     (combine-out closure:$params
                                  closure:$body
                                  closure:$scoper
                                  push-new-layer
                                  pop-layer
                                  has-fun?
                                  get-closure
                                  declare-fun)))


;;;; function table
;; a function table is a stack of layers
;; a layer is a map of function bindings
;; bindings { name : (params body scoper) }

;; closures functions
(define closure:$params 'formal-params)
(define closure:$body   'body)
(define closure:$scoper 'scoper)

(define closure:of
  (lambda (params body scoper)
    (map:of
     closure:$params  params
     closure:$body    body
     closure:$scoper  scoper)))

;; layer is a map of { name : closure }

(define layer:has-fun? map:contains?)

(define layer:get-fun
  (lambda (name layer)
    (map:get name layer)))

(define layer:put-fun
  (lambda (name closure layer)
    (map:put name closure layer)))

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

;; whether this table has a function with the given name
(define has-fun?
  (lambda (name table)
    (cond
      [(no-layers? table)                        #f]
      [(layer:has-fun? name (peek-layer table))  #t]
      [else                                      (has-fun? name (pop-layer table))])))

;; assumes function exists
; call has-fun? beforehand
(define get-closure
  (lambda (name table)
    (cond
      [(no-layers? table)                        (error "'" name "' is not declared")]
      [(layer:has-fun? name (peek-layer table))  (layer:get-fun name (peek-layer table))]
      [else                                      (get-closure name (pop-layer table))])))

;; adds function to table
(define declare-fun
  (lambda (name params body scoper table)
    (push-layer (layer:put-fun name
                               (closure:of params body scoper)
                               (peek-layer table))
                (pop-layer table))))
