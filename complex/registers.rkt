#lang racket

(require "util/structures.rkt")

(provide (combine-out new-registers
                      default-registers
                      registers-return?-lens
                      registers-value-lens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; registers containing the state of the interpreter
;;;; return?  whether to return
;;;; value    the current value (passed from expression to expression)

(define new-registers struc-2)

(define default-registers (new-registers #f null)) ; default-value? does that make more sense?

(define registers-return?-lens e1-lens)

(define registers-value-lens e2-lens)
