#lang racket

(require "../util/map.rkt")

(provide new-var-table
         (prefix-out var-table: (except-out (all-defined-out)
                                            new-var-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Var-table is a stack of frames
;; a frame is a map of var bindings
;; a var binding associates a symbol with a box
;; beware, assign-value mutates the given frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define new-var-table map:empty)

(define var-box map:get)
(define var-value (compose1 unbox var-box))

(define var-declared? map:contains?)
(define var-initialized? (compose1 not null? var-value))

(define assign-box map:put)
(define assign-value
  (lambda (var-name value frame)
    (set-box! (var-box var-name frame) value)
    frame))

(define declare-var
  (lambda (var-name frame)
    (declare-var-with-value var-name null frame)))
  
(define declare-var-with-box assign-box)
  
(define declare-var-with-value
  (lambda (var-name value frame)
    (assign-box var-name (box value) frame)))

