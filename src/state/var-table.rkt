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

; check if initialized first
; #F if absent
(define var-box map:get)
; check if initialized first
; error if absent
(define var-value (compose1 unbox var-box))

(define var-declared? map:contains?)
; check if declared first
(define var-initialized? (compose1 not null? var-value))

(define assign-box map:put)
; mutates in place!
(define assign-value
  (lambda (var-name value table)
    (begin
      (set-box! (var-box var-name table) value)
      table)))

; adds an uninitialized entry to the table
(define declare-var
  (lambda (var-name table)
    (declare-var-with-value var-name null table)))
  
(define declare-var-with-box assign-box)

(define declare-var-with-value
  (lambda (var-name value table)
    (declare-var-with-box var-name (box value) table)))

