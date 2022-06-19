#lang racket/base
(provide format-val-for-output)
(require "state/instance.rkt")

;;;;
;; this module contains functions for the formatting of values output by the intepreter

;; takes a value and modifies it for output
; int as number
; boolean as symbol (matching source code)
; instance as JSON string (WIP)
(define format-val-for-output
  (lambda (value)
    (cond
      [(eq? #t value)         'true]
      [(eq? #f value)         'false]
      [(number? value)        value]
      [(is-instance? value)   value]
      [else                   (error "returned an unsupported type: " value)])))
