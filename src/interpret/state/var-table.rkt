#lang racket/base

(require "../../util/map.rkt")

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
(define get-box map:get)
; check if initialized first
; error if absent
(define get-value (compose1 unbox get-box))

(define declared? map:contains?)
; check if declared first
(define initialized? (compose1 not null? get-value))

(define assign-box map:put)
; mutates in place!
(define assign-value
  (lambda (var-name value table)
    (begin
      (set-box! (get-box var-name table) value)
      table)))


(define declare-with-box assign-box)

(define declare-with-value
  (lambda (var-name value table)
    (declare-with-box var-name (box value) table)))

